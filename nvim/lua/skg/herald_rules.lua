-- PURPOSE: Fetch the herald rule table from the server, and the
-- skgrepo label table it serves beside it. The table lives only in
-- Rust (server/heralds.rs); the client caches it here for the
-- session, and the labels with it. require('skg').init fetches it once per connect,
-- so reconnecting re-fetches it; the heralds display calls
-- ensure_rules to self-heal a missing table when the user turns
-- heralds on. The Lua port of elisp/skg-request-herald-rules.el (the
-- cache itself lived in heralds-minor-mode.el there; here the fetch
-- module owns both caches, which is also what skg.reload preserves
-- across a reload).
--
-- There is deliberately no user-facing command here. A missing table
-- is recovered automatically (see ensure_rules), and a hard failure
-- surfaces an informative message at that point.

local client = require('skg.client')
local log = require('skg.log')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---How many times ensure_rules re-requests the table. After this many
---empty replies it gives up, so a silent or wedged server cannot
---freeze the editor with fruitless retries.
M.max_attempts = 3

---Milliseconds ensure_rules waits for the reply to each attempt.
M.attempt_timeout_ms = 1000

---The cached rule table (a parsed sexp), nil until fetched.
M.rules = nil

---@return any|nil the cached rule table
function M.get_rules ()
  return M.rules
end

---Install RULES as the session's rule table.
---@param rules any
function M.install_rules (rules)
  M.rules = rules
end

---The cached skgrepo label table, a map from a skgrepo's name to its
---label, which the server sends beside the rule table as
---'(skgrepoLabels ("REPO" "LABEL")...)'. A 'repoBoundary' renders as
---"⌂:LABEL", LABEL looked up by the node's nodeRepo, or the nodeRepo
---itself when the table lacks it. Empty until fetched.
M.skgrepo_labels = {}

---@return table<string, string> the cached skgrepo label table
function M.get_skgrepo_labels ()
  return M.skgrepo_labels
end

---Install LABELS, a map from skgrepo name to label, as the session's
---skgrepo label table.
---@param labels table<string, string>
function M.set_skgrepo_labels (labels)
  M.skgrepo_labels = labels
end

---Install LABELS_FORM, '(skgrepoLabels (REPO LABEL)...)', as the
---skgrepo label table. Called with the rule table by the
---connect-time fetch, and by tests. Errors on any other form.
---@param labels_form any
function M.install_skgrepo_labels (labels_form)
  if not (sexpr.is_list(labels_form)
          and labels_form[1] == sexpr.symbol('skgrepoLabels')) then
    error('install_skgrepo_labels: not a label table: '
          .. vim.inspect(labels_form)) end
  local labels = {}
  for i = 2, #labels_form do
    local entry = labels_form[i]
    labels[sexpr.atom_text(entry[1])] = sexpr.atom_text(entry[2]) end
  M.set_skgrepo_labels(labels)
end

---The element of RESPONSE whose head is the symbol NAME, or nil.
---@param response any
---@param name string
---@return any|nil
local function response_form (response, name)
  if not sexpr.is_list(response) then return nil end
  for _, element in ipairs(response) do
    if sexpr.is_list(element) and element[1] == sexpr.symbol(name) then
      return element end
  end
  return nil
end

---Send a rule-table request and install the reply, the rule table
---and the skgrepo label table, asynchronously. A malformed reply
---leaves the caches untouched and logs why. This is
---the low-level fetch; callers that need the table should go through
---ensure_rules.
function M.request_herald_rules ()
  state.register_response_handler('herald-rules',
    function (_payload, response)
      local ok, err = pcall(function ()
        local content =
          require('skg.payload').field(response, 'content')
        local rules = sexpr.read(content)
        M.install_skgrepo_labels(response_form(response, 'skgrepoLabels'))
        M.install_rules(rules)
      end)
      if not ok then
        vim.notify('Heralds: could not parse the rule table from the'
                   .. ' server: ' .. tostring(err))
        log.log('error', 'heralds', 'rule table parse failure: %s',
                tostring(err)) end
    end, true)
  -- PITFALL: no lp_reset here, unlike most request functions. This
  -- request is sent immediately after connection_verify during init;
  -- resetting the LP machine could drop a partially received
  -- verify-connection response.
  client.send_string('((request . "herald rules"))\n')
end

---Return the herald rule table, fetching it from the server if
---absent: up to max_attempts requests, each waited on for
---attempt_timeout_ms. Returns whatever is installed at the end (nil
---if every attempt came back empty). A connection failure propagates
---on the first attempt rather than being retried.
---@return any|nil
function M.ensure_rules ()
  if M.rules then return M.rules end
  local attempt = 0
  while M.rules == nil and attempt < M.max_attempts do
    attempt = attempt + 1
    M.request_herald_rules()
    vim.wait(M.attempt_timeout_ms,
             function () return M.rules ~= nil or state.tcp == nil end,
             50)
  end
  return M.rules
end

return M
