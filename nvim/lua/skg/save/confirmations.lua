-- PURPOSE: The save's confirmations: the handlers of the terminal
-- messages the server sends instead of save-result (fork-, telescope-
-- hoist-, delete-from-, strandeds- and the save's overPrivateText
-- release confirmation), and the fork-confirmation buffer with its
-- approve and decline commands. Each but the release confirmation
-- retries the save through 'save.request_save_buffer'. The Lua port of
-- elisp/skg-request-save/skg-save-confirmations.el. 'skg.save'
-- requires this module only inside the handlers it registers, so the
-- two load in either order.

local length_prefix = require('skg.length_prefix')
local log = require('skg.log')
local messages = require('skg.messages')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local save = require('skg.save')
local sexpr = require('skg.sexpr.parse')

local M = {}

---Sentinel skgrepo the server pre-fills for a clone-to-be whose skgrepo
---the user has not specified (in the saved metadata or a prior
---round). 'fork_choose_placeholder_repos' prompts for a replacement per
---carrying clone (<localleader>ss on the headline works too);
---'approve_fork' refuses while any remains. Must match
---FORK_REPO_PLACEHOLDER in server/from_text/fork.rs.
M.fork_repo_placeholder = 'PICK-A-REPO'

---Handle a fork-confirmation message: the save edited foreign
---node(s) and was not pre-approved, so nothing was skgsave-committed. Show
---the confirmation buffer; interactively, ask whether to approve.
---Terminal like save_result_handler: removes the streaming handlers
---and the unfired save-result one-shot (decrementing its pending
---count), ends the stream, unlocks.
---@param save_buf integer
---@param response any
---@param strandeds_review table|nil the review the save carried, which the
---  approving re-save carries again
---@param delete_from string|nil the delete-from choice the save carried,
---  which the approving re-save carries again
function M.fork_confirmation_handler (save_buf, response, strandeds_review,
                                      delete_from)
  save.end_save_stream({ 'save-result' })
  local confirm_buf = nil
  local ok, err = pcall(function ()
    -- The pcall guards only response parsing/display. The interactive
    -- flow below runs outside it: a refusal from approve_fork must
    -- reach the user, not the log -- nesting it here used to swallow
    -- the "pick a repo first" refusal, so approving with a
    -- placeholder skgrepo silently did nothing.
    local content = payload.field_text(response, 'content')
    local to_minibuffer = payload.field_text(response, 'to-minibuffer')
    confirm_buf = M.show_fork_confirmation(
      messages.with_neovim_bindings(content or ''), save_buf)
    vim.b[confirm_buf].skg_fork_strandeds_review = strandeds_review
    vim.b[confirm_buf].skg_fork_delete_from = delete_from
    if to_minibuffer then
      vim.notify(messages.with_neovim_bindings(to_minibuffer)) end
  end)
  if not ok then
    log.log('error', 'save', 'fork-confirmation handler error: %s',
            tostring(err))
    length_prefix.tell_answer_not_applied(
      'fork-confirmation', err,
      'nothing was saved, and the fork confirmation was not shown')
    return
  end
  if confirm_buf and #vim.api.nvim_list_uis() > 0 then
    -- In headless runs (tests) the caller drives approve_fork /
    -- decline_fork directly; interactively, prompt for any clone
    -- skgrepo not yet specified, then ask.
    vim.api.nvim_buf_call(confirm_buf, function ()
      M.fork_choose_placeholder_repos(confirm_buf)
      if vim.fn.confirm('Fork the listed node(s)?',
                        '&Yes\n&No', 2) == 1 then
        M.approve_fork()
      else
        M.decline_fork() end
    end)
  end
end

---Handle the text-free terminal Hoist challenge. Nothing was skgsave-committed.
---An interactive approval reissues the same save with the exact PIDs; Abort
---and headless operation leave the buffer and disk untouched.
---@param save_buf integer
---@param response any
---@param approved_forks boolean|nil
---@param fork_repos table[]|nil
function M.telescope_hoist_confirmation_handler (
    save_buf, response, approved_forks, fork_skgrepos,
    strandeds_review, delete_from)
  save.end_save_stream({ 'save-result' })
  local ok, err = pcall(function ()
    local approved_pids = {}
    local telescopes = payload.field(response, 'telescopes') or {}
    if sexpr.is_list(telescopes) then
      for _, telescope in ipairs(telescopes) do
        local pid = payload.field_text(telescope, 'pid')
        if pid then table.insert(approved_pids, pid) end
      end
    end
    local prompt = payload.field_text(response, 'prompt')
      or 'Hoist lower title/body text to home?'
    if #vim.api.nvim_list_uis() == 0 then
      vim.notify('Hoist required for ' .. table.concat(approved_pids, ', ')
                 .. '; nothing was saved')
      return
    end
    if vim.fn.confirm(prompt, '&Hoist\n&Abort', 2) == 1 then
      vim.api.nvim_set_current_buf(save_buf)
      save.request_save_buffer(
        approved_forks, fork_skgrepos, approved_pids,
        strandeds_review, delete_from)
    else
      vim.notify('Hoist aborted; nothing was saved. Repair the .skg'
                 .. ' sections manually.')
    end
  end)
  if not ok then
    log.log('error', 'save',
            'telescope-hoist-confirmation handler error: %s',
            tostring(err))
    length_prefix.tell_answer_not_applied(
      'telescope-hoist-confirmation', err, 'the save stopped for a confirmation, which failed, so it may not have saved')
  end
end

---Handle a delete-from-confirmation: records the skgrepo restriction keeps
---out name a graphnode the save deletes, and nothing was skgsave-committed.
---Ask "Delete from: (c)ancel, (v)isible records only, (a)ll owned records",
---and re-issue the same save with the choice, carrying the other approvals
---along. Cancel saves nothing. Headless, only report.
---@param save_buf integer
---@param response any
function M.delete_from_confirmation_handler (
    save_buf, response, approved_forks, fork_skgrepos,
    approved_hoist_pids, strandeds_review)
  save.end_save_stream({ 'save-result' })
  local ok, err = pcall(function ()
    local count = payload.field_text(response, 'count') or '?'
    local prompt = payload.field_text(response, 'prompt')
      or 'Records the skgrepo restriction keeps out name a deleted graphnode.'
    if #vim.api.nvim_list_uis() == 0 then
      vim.notify(count .. ' restricted record(s) name a deleted graphnode;'
                 .. ' nothing was saved')
      return
    end
    local delete_from = M.delete_from_of_choice(
      vim.fn.confirm(prompt .. '\nDelete from:',
                     '&cancel\n&visible records only\n&all owned records', 1))
    if not delete_from then
      vim.notify('Nothing was saved.')
      return
    end
    vim.api.nvim_set_current_buf(save_buf)
    save.request_save_buffer(
      approved_forks, fork_skgrepos, approved_hoist_pids,
      strandeds_review, delete_from)
  end)
  if not ok then
    log.log('error', 'save', 'delete-from-confirmation handler error: %s',
            tostring(err))
    length_prefix.tell_answer_not_applied(
      'delete-from-confirmation', err, 'the save stopped for a confirmation, which failed, so it may not have saved')
  end
end

---The delete-from field's value for CHOICE, the number of the button the
---delete-from prompt returned: 'visible' for 2, 'all' for 3, nil (cancel)
---otherwise.
---@param choice integer
---@return string|nil
function M.delete_from_of_choice (choice)
  return ({ [2] = 'visible', [3] = 'all' })[choice]
end

---Handle a strandeds-confirmation: the save's home skgrepo moves would strand
---the relationships it lists, and nothing was skgsave-committed. Show
---them, ask which to reset to default, and re-issue the same save with that
---review, carrying the other approvals along. Headless, only report.
---@param save_buf integer
---@param response any
function M.strandeds_confirmation_handler (
    save_buf, response, approved_forks, fork_skgrepos,
    approved_hoist_pids, delete_from)
  save.end_save_stream({ 'save-result' })
  local ok, err = pcall(function ()
    local strandeds = M.strandeds_from_response(response)
    local prompt = payload.field_text(response, 'prompt')
      or 'Reset the stranded relationships to default?'
    if #vim.api.nvim_list_uis() == 0 then
      vim.notify(string.format(
        '%d relationship(s) would be stranded; nothing was saved', #strandeds))
      return
    end
    local lines = { prompt }
    for _, stranded in ipairs(strandeds) do
      table.insert(lines, '  ' .. M.stranded_description(stranded)) end
    local choice = ({ 'all', 'none', 'choose' })[
      vim.fn.confirm(table.concat(lines, '\n'),
                     '&All\n&None\n&Choose each\n&Quit', 4)]
    local review = M.strandeds_review_from_choice(strandeds, choice,
      function (question)
        return vim.fn.confirm(question, '&Yes\n&No', 2) == 1 end)
    if not review then
      vim.notify('Nothing was saved.')
      return
    end
    vim.api.nvim_set_current_buf(save_buf)
    save.request_save_buffer(
      approved_forks, fork_skgrepos, approved_hoist_pids,
      review, delete_from)
  end)
  if not ok then
    log.log('error', 'save', 'strandeds-confirmation handler error: %s',
            tostring(err))
    length_prefix.tell_answer_not_applied(
      'strandeds-confirmation', err, 'the save stopped for a confirmation, which failed, so it may not have saved')
  end
end

---The stranded relationships in a strandeds-confirmation RESPONSE, each
---{relation, recorder, member, relRepo, default}.
---@param response any
---@return table[]
function M.strandeds_from_response (response)
  local strandeds = {}
  local entries = payload.field(response, 'strandeds') or {}
  if not sexpr.is_list(entries) then return strandeds end
  for _, entry in ipairs(entries) do
    table.insert(strandeds, {
      relation = payload.field_text(entry, 'relation'),
      recorder = payload.field_text(entry, 'recorder'),
      member   = payload.field_text(entry, 'member'),
      relRepo  = payload.field_text(entry, 'relRepo'),
      default  = payload.field_text(entry, 'default') })
  end
  return strandeds
end

---One line describing STRANDED.
---@param stranded table
---@return string
function M.stranded_description (stranded)
  return string.format('%s %s %s: stays at %s; its default is now %s',
                       stranded.recorder, stranded.relation, stranded.member,
                       stranded.relRepo, stranded.default)
end

---The strandeds review CHOICE ('all', 'none', 'choose', or nil to save
---nothing) makes of STRANDEDS: the {relation, recorder, member} triples
---to reset to default, or nil. For 'choose', ASK is called with a question
---per relationship and resets to default those it accepts.
---@param strandeds table[]
---@param choice string|nil
---@param ask fun(question: string): boolean
---@return table|nil
function M.strandeds_review_from_choice (strandeds, choice, ask)
  local function triple (stranded)
    return { stranded.relation, stranded.recorder, stranded.member } end
  local review = {}
  if choice == 'all' then
    for _, stranded in ipairs(strandeds) do
      table.insert(review, triple(stranded)) end
  elseif choice == 'choose' then
    for _, stranded in ipairs(strandeds) do
      if ask('Reset ' .. M.stranded_description(stranded) .. ' to default?') then
        table.insert(review, triple(stranded)) end
    end
  elseif choice ~= 'none' then
    return nil end
  return review
end

---Handle a save-rerender release challenge, the terminal message of a
---save whose rerender would show text from overPrivateText telescopes
---under the skgrepo restriction. The save has been written, but the
---server has sent none of the rerendered text; its challenge names the
---views withheld, the saved one first. Approval asks the server to
---rerender those views, approving the PIDs the challenge lists
---('rerender_all_views.request_rerender_views'); the save is not sent
---again, since it would apply a second time (creating a new root of the
---view again, or refused because a container it gave a new child would
---be an edited stale graphnode).
---Declining keeps the buffers as they are.
---@param response any
function M.save_text_release_confirmation_handler (response)
  save.end_save_stream({ 'save-result' })
  local approved_pids =
    payload.string_list(payload.field(response, 'pids'))
  local withheld_view_ids =
    payload.string_list(payload.field(response, 'view-ids'))
  local prompt = payload.field_text(response, 'prompt')
    or 'Display staged text from overPrivateText telescopes?'
  if #vim.api.nvim_list_uis() == 0 then
    vim.notify('Saved, but protected rerender text was withheld for '
               .. table.concat(approved_pids, ', '))
    return
  end
  if vim.fn.confirm(prompt, '&Include\n&Keep withheld', 2) == 1 then
    require('skg.rerender_all_views').request_rerender_views(
      withheld_view_ids, approved_pids)
  else
    vim.notify('Save succeeded; protected rerender text remains withheld'
               .. ' and buffers are unchanged.')
  end
end

---Prompt for an owned skgrepo for each clone-to-be still carrying the
---placeholder, writing the choice into the buffer. The server's
---suggested skgrepo (the comment directly above the clone) is the
---default; aborting the prompt accepts it. A no-op when every clone's
---repo is already specified -- notably when the saved metadata
---itself specified it (the server then omits the placeholder): no
---redundant ask.
---@param buf integer
function M.fork_choose_placeholder_repos (buf)
  local picker = require('skg.picker')
  local owned = require('skg.config').owned_repos() or {}
  vim.api.nvim_buf_call(buf, function ()
    for line = 1, vim.api.nvim_buf_line_count(buf) do
      if metadata.outline_level(line) == 1 then
        local sexp = metadata.metadata_sexp_at_line_or_nil(line)
        if sexp and metadata.node_repo(sexp)
                    == M.fork_repo_placeholder then
          local suggested = nil
          if line > 1 then
            local above = metadata.line_text(line - 1) or ''
            suggested = above:match(
              '^# Suggested repo for the clone below: (.+)$')
          end
          local default = suggested or owned[1]
          local parts = metadata.split_as_stars_metadata_title(
            metadata.line_text(line) or '')
          local choice = picker.completing_read_with_cycle(
            string.format('Repo for the clone "%s" (default %s): ',
                          (parts and parts.title) or '?',
                          default or '?'),
            owned,
            { initial_input = default, cycle_values = owned,
              require_match = true })
          local chosen = (choice and choice ~= '') and choice or default
          if chosen then
            metadata.change_repo_at_line(line, chosen) end
        end
      end
    end
  end)
end

---Show CONTENT in the fork-confirmation buffer, recording SAVE_BUF as
---its source; returns the buffer. It is a navigable, editable content
---view (so the user can rotate each clone's skgrepo), but not an
---ordinary save target: it has no view ID (tripping the nil-view_id save
---guard) and its ':w' refuses. Only approve (<localleader>cc) and
---decline (<localleader>ck) act on it. Dismissing it without
---approving strips the source's lingering fork atom, so the next save
---does not silently re-fork.
---@param content string
---@param save_buf integer
---@return integer bufnr
function M.show_fork_confirmation (content, save_buf)
  local name = 'skg://fork-confirmation'
  local buf = nil
  for _, existing in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(existing)
       and vim.api.nvim_buf_get_name(existing) == name then
      buf = existing break end
  end
  if not buf then
    buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, name)
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                             vim.split(content, '\n'))
  vim.bo[buf].buftype = 'acwrite'
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = 'org'
  vim.b[buf].skg_view_id = nil -- not a registered view
  vim.b[buf].skg_fork_source = save_buf
  vim.b[buf].skg_fork_suppress_strip = false
  require('skg.heralds').enable(buf)
  require('skg.keymaps').attach_content_view(buf)
  local keymaps = require('skg.keymaps')
  local approve_key = '<localleader>'
    .. keymaps.fork_confirmation_bindings.approve
  local decline_key = '<localleader>'
    .. keymaps.fork_confirmation_bindings.decline
  vim.keymap.set('n', approve_key,
                 function () keymaps.call_showing_refusal(M.approve_fork) end,
                 { buffer = buf, desc = 'Approve the listed forks' })
  vim.keymap.set('n', decline_key,
                 function () keymaps.call_showing_refusal(M.decline_fork) end,
                 { buffer = buf, desc = 'Decline the forks' })
  if not vim.b[buf].skg_fork_autocmds then
    vim.b[buf].skg_fork_autocmds = true
    vim.api.nvim_create_autocmd('BufWriteCmd', {
      buffer = buf,
      callback = function ()
        vim.notify(string.format('This is the fork-confirmation buffer;'
                                 .. ' approve with %s or decline with %s',
                                 approve_key, decline_key))
      end })
    vim.api.nvim_create_autocmd({ 'BufDelete', 'BufWipeout' }, {
      buffer = buf,
      callback = function ()
        if not vim.b[buf].skg_fork_suppress_strip then
          local source = vim.b[buf].skg_fork_source
          if source and vim.api.nvim_buf_is_valid(source) then
            vim.api.nvim_buf_call(source, function ()
              metadata.strip_fork_requests_in_buffer() end)
          end
        end
      end })
  end
  vim.bo[buf].modified = false
  vim.api.nvim_set_current_buf(buf)
  return buf
end

---Walk the fork-confirmation buffer for {id, repo} pairs: each
---level-1 headline is a clone-to-be carrying (skgRepos (nodeRepo
---REPO)) and no gnid; each of its level-2 children is an original
---carrying (gnid N).
---@param buf integer
---@return table[]
function M.fork_repos_from_confirmation_buffer (buf)
  local pairs_found = {}
  vim.api.nvim_buf_call(buf, function ()
    local parent_skgrepo = nil
    for line = 1, vim.api.nvim_buf_line_count(buf) do
      local level = metadata.outline_level(line)
      if level then
        local sexp = metadata.metadata_sexp_at_line_or_nil(line)
        if level == 1 then
          -- Always rebind on a level-1 headline -- even a
          -- metadata-less one -- so it cannot leak a prior
          -- clone-to-be's repo to a later fork's child.
          parent_skgrepo = sexp and metadata.node_repo(sexp) or nil
        elseif level == 2 and sexp and parent_skgrepo then
          local gnid = metadata.node_id(sexp)
          if gnid then
            table.insert(pairs_found, { gnid, parent_skgrepo }) end
        end
      end
    end
  end)
  return pairs_found
end

---Approve the fork operations listed in the fork-confirmation buffer:
---extract each clone's chosen skgrepo, kill the confirmation buffer,
---and re-save the source buffer with the fork operations approved. Refuses
---while any clone-to-be still carries the placeholder skgrepo.
function M.approve_fork ()
  local buf = vim.api.nvim_get_current_buf()
  local source = vim.b[buf].skg_fork_source
  local strandeds_review = vim.b[buf].skg_fork_strandeds_review
  local delete_from = vim.b[buf].skg_fork_delete_from
  local fork_skgrepos = M.fork_repos_from_confirmation_buffer(buf)
  if not source or not vim.api.nvim_buf_is_valid(source) then
    error('The buffer that requested these forks is no longer open')
  end
  for _, pair in ipairs(fork_skgrepos) do
    if pair[2] == M.fork_repo_placeholder then
      error('Pick a repo for each clone first: point on a'
            .. ' clone-to-be headline, then <localleader>ss') end
  end
  -- The fork atom must survive to the re-save (which skgsave-commits the
  -- fork; the server drops it on re-render): suppress the kill-hook
  -- strip.
  vim.b[buf].skg_fork_suppress_strip = true
  vim.api.nvim_buf_delete(buf, { force = true })
  vim.api.nvim_set_current_buf(source)
  save.request_save_buffer(true, fork_skgrepos, nil, strandeds_review,
                        delete_from)
end

---Decline the forks; nothing was written. Strips any lingering
---explicit fork atom from the source buffer and leaves this
---confirmation buffer open for reference.
function M.decline_fork ()
  local buf = vim.api.nvim_get_current_buf()
  local source = vim.b[buf].skg_fork_source
  if source and vim.api.nvim_buf_is_valid(source) then
    vim.api.nvim_buf_call(source, function ()
      metadata.strip_fork_requests_in_buffer() end)
  end
  vim.notify('Fork declined; nothing was saved. This buffer is left'
             .. ' open for reference.')
end

return M
