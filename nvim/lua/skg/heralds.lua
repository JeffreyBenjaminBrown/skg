-- PURPOSE: Display skg metadata as a short list of "herald" markers.
-- Each org headline the server sends starts with '(skg ...)' metadata.
-- This module lenses that tree via skg.sexpr.lens, producing colored
-- tokens that summarize view and code information. Every piece of
-- display logic lives in the rule table, which LIVES IN RUST
-- (server/heralds.rs) and is fetched/cached by skg.herald_rules.
-- The Lua port of elisp/heralds-minor-mode.el.
--
-- DISPLAY MECHANISM. Where Emacs used an overlay with a 'display'
-- property (the metadata text stays in the buffer but is shown as the
-- herald string), here one extmark per headline conceals the metadata
-- extent and renders the heralds as inline virtual text whose chunk
-- list carries per-chunk highlight groups. The buffer TEXT is
-- untouched either way, so saves round-trip the full metadata.
-- Concealment needs window-local options (conceallevel=2), applied by
-- an autocmd to any window showing a heralded buffer; with
-- concealcursor='nc' the raw metadata reveals itself in insert mode
-- on the cursor line -- a small, arguably clearer, deviation from
-- Emacs, where the overlay never reveals.
--
-- The elisp version's orphaned-overlay guard (overlays surviving a
-- major-mode switch that killed the tracking variable) has no analog:
-- extmarks live in a namespace, and clearing the namespace is always
-- complete.

local herald_rules = require('skg.herald_rules')
local lens = require('skg.sexpr.lens')
local sexpr = require('skg.sexpr.parse')

local M = {}

M.namespace = vim.api.nvim_create_namespace('skg-heralds')

-- The placeholder token the server's `rels` rule emits
-- (RELS_SPANS_SENTINEL in server/heralds.rs). The relationship heralds
-- are per-CHARACTER styled spans -- more than the rule table's
-- atom-level coloring can express -- so the rule only POSITIONS them by
-- emitting this sentinel, which `chunks_from_metadata' replaces with the
-- spans it renders from the `(rels (COLOR "text") ...)' payload. The
-- analog of `heralds--rels-sentinel' in elisp.
M.RELS_SENTINEL = '__RELS_SPANS__'

---Map lens color keywords to highlight groups, mirroring the five
---heralds-*-face definitions.
local color_to_highlight_group = {
  RED = 'SkgHeraldRed',
  GREEN = 'SkgHeraldGreen',
  BLUE = 'SkgHeraldBlue',
  YELLOW = 'SkgHeraldYellow',
  ORANGE = 'SkgHeraldOrange' }

function M.define_highlight_groups ()
  vim.api.nvim_set_hl(0, 'SkgHeraldRed',
    { fg = 'white', bg = 'red', default = true })
  vim.api.nvim_set_hl(0, 'SkgHeraldGreen',
    { fg = 'white', bg = '#006400', default = true })
  vim.api.nvim_set_hl(0, 'SkgHeraldBlue',
    { fg = 'white', bg = 'blue', default = true })
  vim.api.nvim_set_hl(0, 'SkgHeraldYellow',
    { fg = 'black', bg = 'yellow', default = true })
  vim.api.nvim_set_hl(0, 'SkgHeraldOrange',
    { fg = 'white', bg = '#d2691e', default = true })
  vim.api.nvim_set_hl(0, 'SkgHeraldPurple',
    { fg = 'white', bg = '#8b00ff', default = true })
  vim.api.nvim_set_hl(0, 'SkgHeraldCyan',
    { fg = 'black', bg = '#00ffff', default = true })
  vim.api.nvim_set_hl(0, 'SkgHeraldBirth',
    { fg = 'black', bg = 'white', default = true })
end
M.define_highlight_groups()

---@param color table|nil a lens color keyword symbol
---@return string|nil highlight group name
function M.color_highlight_group (color)
  if color == nil then return nil end
  return color_to_highlight_group[sexpr.atom_text(color)]
end


-- ── tokens -> display chunks ───────────────────────────────────────

---Is CHARACTER in the keep-class of the structural-colon rule?
---(Alphanumerics, '+', ' ' and '-' keep an adjacent colon, since they
---often appear as label values or separators.)
---@param character string|nil one character, possibly multibyte
---@return boolean
function M.keeps_adjacent_colon (character)
  if character == nil then return false end
  if character == '+' or character == ' ' or character == '-' then
    return true end
  return vim.fn.match(character, '^[[:alnum:]]$') == 0
end

---TOKEN's characters as a flat list of {character, color} cells.
---@param token table a lens token
---@return table[]
function M.token_character_cells (token)
  local cells = {}
  for _, chunk in ipairs(token.chunks) do
    for _, character in ipairs(vim.fn.split(chunk.text, '\\zs')) do
      table.insert(cells, { character = character,
                            color = chunk.color }) end
  end
  return cells
end

---Remove structural colons from CELLS: a colon is structural when
---the character before or after it is outside the keep-class. The
---scan mirrors the elisp regex's two alternatives, left to right and
---non-overlapping ('(X):' -> X, then ':(Y)' -> Y).
---@param cells table[]
---@return table[]
function M.strip_structural_colons (cells)
  local kept = {}
  local i = 1
  while i <= #cells do
    local this = cells[i]
    local following = cells[i + 1]
    if following and following.character == ':'
       and this.character ~= ':'
       and not M.keeps_adjacent_colon(this.character) then
      table.insert(kept, this)
      i = i + 2
    elseif this.character == ':' and following
           and not M.keeps_adjacent_colon(following.character) then
      table.insert(kept, following)
      i = i + 2
    else
      table.insert(kept, this)
      i = i + 1 end
  end
  return kept
end

---Convert lens TOKENS to virtual-text chunks: tokens separated by a
---space, except tokens marked abut, which join the preceding token
---with no separator; structural colons stripped; colors mapped to
---highlight groups, with consecutive same-color characters grouped.
---Returns nil when TOKENS is empty (no herald, no extmark).
---@param tokens table[]
---@return table[]|nil chunks {{text, hlgroup_or_empty}, ...}
function M.tokens_to_chunks (tokens)
  if #tokens == 0 then return nil end
  local cells = {}
  for index, token in ipairs(tokens) do
    if index > 1 and not token.abut then
      table.insert(cells, { character = ' ', color = nil }) end
    for _, cell in ipairs(
        M.strip_structural_colons(M.token_character_cells(token))) do
      table.insert(cells, cell) end
  end
  local chunks = {}
  local current_text = nil
  local current_color = nil
  local function flush ()
    if current_text and #current_text > 0 then
      table.insert(chunks,
        { current_text,
          M.color_highlight_group(current_color) or 'Normal' }) end
  end
  for _, cell in ipairs(cells) do
    if current_text ~= nil and cell.color == current_color then
      current_text = current_text .. cell.character
    else
      flush()
      current_text = cell.character
      current_color = cell.color end
  end
  flush()
  return chunks
end

---@param chunks table[]|nil
---@return string the display text of CHUNKS
function M.chunks_text (chunks)
  local pieces = {}
  for _, chunk in ipairs(chunks or {}) do
    table.insert(pieces, chunk[1]) end
  return table.concat(pieces)
end

---Concatenate a lens TOKEN's chunk texts (to recognize the sentinel).
---@param token table
---@return string
function M.token_text (token)
  local pieces = {}
  for _, chunk in ipairs(token.chunks) do
    table.insert(pieces, chunk.text) end
  return table.concat(pieces)
end

---Find the first (rels ...) sub-list anywhere within SEXP, else nil.
---@param sexp any
---@return table|nil
function M.find_rels (sexp)
  if not sexpr.is_list(sexp) then return nil end
  if sexp[1] == sexpr.symbol('rels') then return sexp end
  for i = 1, #sexp do
    local found = M.find_rels(sexp[i])
    if found then return found end
  end
  return nil
end

-- ── relationship heralds: render the server's SEMANTIC facts ─────────
-- ALL presentation lives here (letters, colors, order, count-omission,
-- the a(b,c) link form); the server sends only facts. See
-- TODO/heralds-semantic-wire.org. Mirrors the elisp renderer
-- (heralds--render-rel-facts et al.) exactly.

local REL_ORDER = { 'contains', 'textlinksTo', 'subscribes',
                    'overrides', 'hides' }

local function rel_letter (rel)
  return ({ contains = 'C', textlinksTo = 'L', subscribes = 'S',
            overrides = 'O', hides = 'H' })[rel] or '?'
end

local function rel_base_hl (rel)
  if rel == 'contains' or rel == 'textlinksTo' then return 'SkgHeraldBlue' end
  return 'SkgHeraldPurple' -- subscribes / overrides / hides
end

---The child of SEXP (from index 2) whose head is the symbol NAME, or nil.
local function assq (sexp, name)
  if not sexpr.is_list(sexp) then return nil end
  for i = 2, #sexp do
    local child = sexp[i]
    if sexpr.is_list(child) and child[1] == sexpr.symbol(name) then
      return child end
  end
  return nil
end

---The first number among SEXP's elements (from index 2), or nil.
local function first_number (sexp)
  for i = 2, #sexp do
    if type(sexp[i]) == 'number' then return sexp[i] end
  end
  return nil
end

---Generation integers of an (ancestors ...) sub-form inside SIDE, else {}.
local function ancestors_of (side)
  local anc = assq(side, 'ancestors')
  if not anc then return {} end
  local gens = {}
  for i = 2, #anc do table.insert(gens, anc[i]) end
  return gens
end

---GENS as sorted distinct letters: 1->a, 2->b, ...
local function gen_letters (gens)
  local seen, sorted = {}, {}
  for _, g in ipairs(gens) do
    if not seen[g] then seen[g] = true; table.insert(sorted, g) end
  end
  table.sort(sorted)
  local out = {}
  for _, g in ipairs(sorted) do
    if type(g) == 'number' and g >= 1 and g <= 26 then
      table.insert(out, string.char(96 + g))
    else
      table.insert(out, '{' .. tostring(g) .. '}') end
  end
  return table.concat(out)
end

---Chunks for one side: the count then ancestor letters. Omits the count
---when it equals the number of ancestors (>=1). MULTI (contains inbound)
---makes a count > 1 orange; ancestor letters are yellow.
local function side_chunks (count, gens, base_hl, multi)
  local letters = gen_letters(gens)
  local n = #letters
  local chunks = {}
  if count > 0 or n > 0 then
    if not (n > 0 and count == n) then
      local hl = (multi and count > 1) and 'SkgHeraldOrange' or base_hl
      table.insert(chunks, { tostring(count), hl })
    end
    if n > 0 then
      table.insert(chunks, { letters, 'SkgHeraldYellow' })
    end
  end
  return chunks
end

local function rel_side (form, side)
  local s = assq(form, side)
  if not s then return nil end
  return { count = first_number(s) or 0, gens = ancestors_of(s) }
end

local function ordinary_rel_chunks (rel, form, base_hl)
  local inn = rel_side(form, 'in')
  local out = rel_side(form, 'out')
  local in_c = side_chunks(inn and inn.count or 0, inn and inn.gens or {},
                           base_hl, rel == 'contains')
  local out_c = side_chunks(out and out.count or 0, out and out.gens or {},
                            base_hl, false)
  if #in_c == 0 and #out_c == 0 then return nil end
  local chunks = {}
  for _, c in ipairs(in_c) do table.insert(chunks, c) end
  table.insert(chunks, { rel_letter(rel), base_hl })
  for _, c in ipairs(out_c) do table.insert(chunks, c) end
  return chunks
end

local function link_rel_chunks (form, base_hl)
  local inn = assq(form, 'in')
  local out = assq(form, 'out')
  local in_chunk
  if inn then
    local total = first_number(inn) or 0
    local surp = assq(inn, 'surprising')
    local wc = assq(inn, 'withContent')
    local b = surp and first_number(surp) or nil
    local c = wc and first_number(wc) or nil
    local inner
    if b == nil and c == nil then inner = ''
    elseif c == nil then inner = '(' .. b .. ')'
    elseif b == nil then inner = '(,' .. c .. ')'
    else inner = '(' .. b .. ',' .. c .. ')' end
    in_chunk = { tostring(total) .. inner, base_hl }
  end
  local out_chunk
  if out then
    local gens = ancestors_of(out)
    if #gens > 0 then out_chunk = { gen_letters(gens), 'SkgHeraldYellow' } end
  end
  if not in_chunk and not out_chunk then return nil end
  local chunks = {}
  if in_chunk then table.insert(chunks, in_chunk) end
  table.insert(chunks, { 'L', base_hl })
  if out_chunk then table.insert(chunks, out_chunk) end
  return chunks
end

---Render the semantic (rels ...) payload in SEXP to virtual-text chunks,
---or nil if there is none / it produces nothing. Coloring: group base
---(C/L blue, S/O/H purple), the reason-for-being token black-on-white,
---ancestor letters black-on-yellow, the contains inbound count>1 orange,
---A/I cyan. Tokens ordered C L S O H A I, space-separated.
---@param sexp any
---@return table[]|nil
function M.render_rel_facts (sexp)
  local rels = M.find_rels(sexp)
  if not rels then return nil end
  local birth = {}
  local birth_form = assq(rels, 'birth')
  if birth_form then
    for i = 2, #birth_form do
      birth[sexpr.atom_text(birth_form[i])] = true end
  end
  local chunks = {}
  local function add_token (tok)
    if tok then
      if #chunks > 0 then table.insert(chunks, { ' ', 'Normal' }) end
      for _, c in ipairs(tok) do table.insert(chunks, c) end
    end
  end
  for _, rel in ipairs(REL_ORDER) do
    local form = assq(rels, rel)
    if form then
      local base = birth[rel] and 'SkgHeraldBirth' or rel_base_hl(rel)
      add_token((rel == 'textlinksTo')
        and link_rel_chunks(form, base)
        or ordinary_rel_chunks(rel, form, base))
    end
  end
  local aliases = assq(rels, 'aliases')
  if aliases then
    add_token({ { 'A' .. tostring(first_number(aliases)), 'SkgHeraldCyan' } })
  end
  local extra = assq(rels, 'extraIds')
  if extra then
    add_token({ { 'I' .. tostring(first_number(extra)), 'SkgHeraldCyan' } })
  end
  if #chunks == 0 then return nil end
  return chunks
end

---Like `tokens_to_chunks', but the sentinel token is replaced by
---REL_CHUNKS (the rendered relationship-herald spans). When REL_CHUNKS
---is nil the sentinel -- which should then be absent -- is dropped.
---@param tokens table[]
---@param rel_chunks table[]|nil
---@return table[]|nil
function M.tokens_to_chunks_with_rels (tokens, rel_chunks)
  if #tokens == 0 then return nil end
  local chunks = {}
  local emitted = false
  local function sep_if_needed (abut)
    if emitted and not abut then
      table.insert(chunks, { ' ', 'Normal' }) end
  end
  for _, token in ipairs(tokens) do
    if M.token_text(token) == M.RELS_SENTINEL then
      if rel_chunks and #rel_chunks > 0 then
        sep_if_needed(token.abut)
        for _, c in ipairs(rel_chunks) do
          table.insert(chunks, c) end
        emitted = true
      end
    else
      local cells = M.strip_structural_colons(
        M.token_character_cells(token))
      if #cells > 0 then
        sep_if_needed(token.abut)
        local cur_text, cur_color = nil, nil
        local function flush ()
          if cur_text and #cur_text > 0 then
            table.insert(chunks,
              { cur_text,
                M.color_highlight_group(cur_color) or 'Normal' }) end
        end
        for _, cell in ipairs(cells) do
          if cur_text ~= nil and cell.color == cur_color then
            cur_text = cur_text .. cell.character
          else
            flush(); cur_text = cell.character
            cur_color = cell.color end
        end
        flush()
        emitted = true
      end
    end
  end
  if #chunks == 0 then return nil end
  return chunks
end

---Display chunks for METADATA_TEXT (a string beginning '(skg').
---Returns nil if it does not parse as an (skg ...) form or the rules
---produce no tokens. The analog of 'heralds-from-metadata'.
---@param metadata_text string
---@return table[]|nil
function M.chunks_from_metadata (metadata_text)
  local ok, sexp = pcall(sexpr.read, metadata_text)
  if not ok or not sexpr.is_list(sexp)
     or sexp[1] ~= sexpr.symbol('skg') then
    return nil end
  local rules = herald_rules.get_rules()
  if not rules then return nil end
  local tokens = lens.transform_sexp_flat(sexp, rules)
  return M.tokens_to_chunks_with_rels(tokens, M.render_rel_facts(sexp))
end

-- ── per-buffer application ─────────────────────────────────────────

---@param buf integer
---@return boolean
function M.enabled_p (buf)
  return vim.b[buf].skg_heralds == true
end

---Enable heralds in BUF. If the rule table is missing, first try to
---self-heal by re-fetching (bounded retries); on failure show an
---informative message and stay off, mirroring the elisp mode.
---@param buf integer
---@return boolean whether heralds are now on
function M.enable (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  if not M.ensure_rules_with_message() then return false end
  vim.b[buf].skg_heralds = true
  M.apply_to_buffer(buf)
  M.watch_buffer(buf)
  M.conceal_windows_showing(buf)
  return true
end

---@param buf integer
function M.disable (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  vim.b[buf].skg_heralds = false
  vim.api.nvim_buf_clear_namespace(buf, M.namespace, 0, -1)
end

---@param buf integer
function M.toggle (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  if M.enabled_p(buf) then M.disable(buf) else M.enable(buf) end
end

---True when the rule table is available, self-healing if needed.
---Never errors. The analog of 'heralds--ensure-rules'.
---@return boolean
function M.ensure_rules_with_message ()
  if herald_rules.get_rules() then return true end
  local ok, rules = pcall(herald_rules.ensure_rules)
  if ok and rules then return true end
  if ok then
    vim.notify('Heralds disabled: the skg server sent no herald rule'
               .. ' table after repeated attempts.')
  else
    vim.notify('Heralds disabled: could not fetch the herald rule'
               .. ' table: ' .. tostring(rules)) end
  return false
end

---@param buf integer
function M.apply_to_buffer (buf)
  vim.api.nvim_buf_clear_namespace(buf, M.namespace, 0, -1)
  local line_count = vim.api.nvim_buf_line_count(buf)
  for row = 0, line_count - 1 do M.apply_to_line(buf, row) end
end

---Lens the first (skg ...) occurrence on 0-based line ROW of BUF into
---one conceal+virtual-text extmark (at most).
---@param buf integer
---@param row integer
function M.apply_to_line (buf, row)
  local line = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)[1]
  if not line then return end
  local start_index = line:find('(skg', 1, true)
  if not start_index then return end
  local end_index = sexpr.find_sexp_end(line, start_index)
  if not end_index then return end
  local chunks =
    M.chunks_from_metadata(line:sub(start_index, end_index))
  if not chunks then return end
  vim.api.nvim_buf_set_extmark(buf, M.namespace, row, start_index - 1, {
    end_col = end_index,
    conceal = '',
    virt_text = chunks,
    virt_text_pos = 'inline',
    right_gravity = false })
end

---Refresh heralds on the lines an edit touched, via nvim_buf_attach.
---Detaches (by returning true from the callback) once heralds are
---disabled or the buffer is gone.
---@param buf integer
function M.watch_buffer (buf)
  if vim.b[buf].skg_heralds_watched then return end
  vim.b[buf].skg_heralds_watched = true
  vim.api.nvim_buf_attach(buf, false, {
    on_lines = function (_event, _buf, _tick, first, _old_last,
                         new_last)
      if not vim.api.nvim_buf_is_valid(buf)
         or not M.enabled_p(buf) then
        if vim.api.nvim_buf_is_valid(buf) then
          vim.b[buf].skg_heralds_watched = false end
        return true -- detach
      end
      vim.schedule(function ()
        if not vim.api.nvim_buf_is_valid(buf)
           or not M.enabled_p(buf) then return end
        local line_count = vim.api.nvim_buf_line_count(buf)
        local last = math.min(new_last, line_count)
        vim.api.nvim_buf_clear_namespace(
          buf, M.namespace, first, last)
        for row = first, last - 1 do M.apply_to_line(buf, row) end
      end)
    end })
end

---Set the conceal options on windows currently showing BUF, and
---arrange (once per buffer) for future windows to get them too.
---@param buf integer
function M.conceal_windows_showing (buf)
  local function apply (win)
    vim.wo[win][0].conceallevel = 2
    vim.wo[win][0].concealcursor = 'nc'
  end
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_get_buf(win) == buf then apply(win) end
  end
  if not vim.b[buf].skg_heralds_conceal_autocmd then
    vim.b[buf].skg_heralds_conceal_autocmd = true
    vim.api.nvim_create_autocmd('BufWinEnter', {
      buffer = buf,
      callback = function ()
        if M.enabled_p(buf) then
          apply(vim.api.nvim_get_current_win()) end
      end })
  end
end

return M
