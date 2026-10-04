-- PURPOSE: Display skg metadata as a short list of "herald" markers.
-- Each org headline the server sends starts with '(skg ...)' metadata.
-- This module lenses that tree via skg.sexpr.lens, producing colored
-- tokens that summarize view and code information. The served rule
-- table (server/heralds.rs) places non-relationship tokens. This client
-- renders semantic relationship facts and their per-character styles.
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
local shared = require('skg.shared')

local M = {}

M.namespace = vim.api.nvim_create_namespace('skg-heralds')

-- The placeholder token the server's `rels` rule emits
-- (RELS_SPANS_SENTINEL in server/heralds.rs). The relationship heralds
-- are per-CHARACTER styled spans -- more than the rule table's
-- atom-level coloring can express -- so the rule only POSITIONS them by
-- emitting this sentinel, which `chunks_from_metadata' replaces with the
-- spans it renders from semantic `(rels ...)' facts. The
-- analog of `heralds--rels-sentinel' in elisp.
M.RELS_SENTINEL = '__RELS_SPANS__'

---Define the highlight group SkgHeraldSTYLE for each style in
---'shared/herald-styles.json' (see shared.style_highlight_group).
function M.define_highlight_groups ()
  for name, look in pairs(shared.herald_styles.styles) do
    vim.api.nvim_set_hl(0, shared.style_highlight_group(name),
      { fg = look.foreground, bg = look.background,
        underline = look.underline, default = true })
  end
end
M.define_highlight_groups()

-- ── view faces ──────────────────────────────────────────────────────
-- Skg dictates every face in a view: the view_faces of
-- shared/herald-styles.json (which the Emacs client reads too) define
-- the highlight groups SkgViewNAME, and a view window's 'winhighlight'
-- maps the groups the view uses (Normal, the org headline levels,
-- links, ...) to them. Colors and weights only, not the typeface.

---The highlight group of view face NAME: 'headline_level_1' ->
---'SkgViewHeadlineLevel1'.
---@param name string
---@return string
function M.view_face_group (name)
  return 'SkgView' .. ('_' .. name):gsub('_(%w)', string.upper)
end

---Define the SkgViewNAME highlight groups. A face with no background
---gets the default's, so nothing of the user's theme shows through.
function M.define_view_face_groups ()
  local default_background = nil
  for _, entry in ipairs(shared.herald_styles.view_faces) do
    if entry.name == 'default' then default_background = entry.background end
  end
  for _, entry in ipairs(shared.herald_styles.view_faces) do
    vim.api.nvim_set_hl(0, M.view_face_group(entry.name),
      { fg = entry.foreground, bg = entry.background or default_background,
        bold = entry.weight == 'bold', underline = entry.underline,
        default = true })
  end
end
M.define_view_face_groups()

---The 'winhighlight' value for a view window.
M.view_winhighlight = (function ()
  local pairs_list = {}
  for _, entry in ipairs(shared.herald_styles.view_faces) do
    for _, group in ipairs(entry.nvim) do
      table.insert(pairs_list, group .. ':' .. M.view_face_group(entry.name))
    end
  end
  return table.concat(pairs_list, ',')
end)()

---@param style table|nil a lens style keyword symbol, e.g. GO
---@return string|nil highlight group name, or nil if it names no style
function M.style_highlight_group (style)
  if style == nil then return nil end
  local name = sexpr.atom_text(style):lower()
  if shared.herald_styles.styles[name] == nil then return nil end
  return shared.style_highlight_group(name)
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

---TOKEN's characters as a flat list of {character, style} cells.
---@param token table a lens token
---@return table[]
function M.token_character_cells (token)
  local cells = {}
  for _, chunk in ipairs(token.chunks) do
    for _, character in ipairs(vim.fn.split(chunk.text, '\\zs')) do
      table.insert(cells, { character = character,
                            style = chunk.style }) end
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
---with no separator; structural colons stripped; styles mapped to
---highlight groups, with consecutive same-style characters grouped.
---Returns nil when TOKENS is empty (no herald, no extmark).
---@param tokens table[]
---@return table[]|nil chunks {{text, hlgroup_or_empty}, ...}
function M.tokens_to_chunks (tokens)
  if #tokens == 0 then return nil end
  local cells = {}
  for index, token in ipairs(tokens) do
    if index > 1 and not token.abut then
      table.insert(cells, { character = ' ', style = nil }) end
    for _, cell in ipairs(
        M.strip_structural_colons(M.token_character_cells(token))) do
      table.insert(cells, cell) end
  end
  local chunks = {}
  local current_text = nil
  local current_style = nil
  local function flush ()
    if current_text and #current_text > 0 then
      table.insert(chunks,
        { current_text,
          M.style_highlight_group(current_style) or 'Normal' }) end
  end
  for _, cell in ipairs(cells) do
    if current_text ~= nil and cell.style == current_style then
      current_text = current_text .. cell.character
    else
      flush()
      current_text = cell.character
      current_style = cell.style end
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
-- ALL presentation lives here (letters, styles, order, count-omission,
-- fractions); the server sends only facts. The letters and order come
-- from shared/relations.json, and the tiers and floors from
-- shared/herald-styles.json, which the elisp renderer
-- (heralds--render-rel-facts et al.) reads too; the two mirror each
-- other. docs/heralds.org states the rules: a side's tier is shown by
-- its carrier, which is its numeral if it shows one, else its ancestor
-- flags; every other glyph shows its floor, except the slash, which
-- shows the side's tier.

local relationship_styles = shared.herald_styles.relationship_heralds

---Relation names in display order, from 'shared/relations.json'.
local REL_ORDER = {}
for _, relation in ipairs(shared.relations_in_display_order()) do
  table.insert(REL_ORDER, relation.name) end

---The display letter for relation REL, from 'shared/relations.json'.
local function rel_letter (rel)
  local relation = shared.relation(rel)
  return relation and relation.letter or '?'
end

---The greater of tiers A and B, per tier_order.
local function max_tier (a, b)
  local rank = {}
  for i, tier in ipairs(shared.herald_styles.tier_order) do rank[tier] = i end
  return rank[a] > rank[b] and a or b
end

---TEXT as a chunk in the highlight group of STYLE (e.g. 'high').
local function styled (text, style)
  return { text, shared.style_highlight_group(style) }
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

---Whether SEXP has the bare atom NAME among its elements (from index 2).
local function has_atom (sexp, name)
  if not sexpr.is_list(sexp) then return false end
  for i = 2, #sexp do
    if sexp[i] == sexpr.symbol(name) then return true end
  end
  return false
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

---GENS sorted and distinct.
local function distinct_gens (gens)
  local seen, sorted = {}, {}
  for _, g in ipairs(gens) do
    if not seen[g] then seen[g] = true; table.insert(sorted, g) end
  end
  table.sort(sorted)
  return sorted
end

---Ancestor-flag chunks for GENS (1 -> a, 2 -> b, ...). Each shows its
---floor, or the greater of its floor and CARRIER_TIER when the flags
---carry a tier.
local function ancestor_chunks (gens, carrier_tier)
  local out = {}
  for _, g in ipairs(distinct_gens(gens)) do
    local letter = (type(g) == 'number' and g >= 1 and g <= 26)
      and string.char(96 + g) or '{' .. tostring(g) .. '}'
    local floor = g == 1 and relationship_styles.floors.ancestor_a
                         or relationship_styles.floors.ancestor_b_and_higher
    table.insert(out, styled(letter, carrier_tier
                                     and max_tier(floor, carrier_tier)
                                     or floor))
  end
  return out
end

---COUNT then its ancestor flags GENS, carrying TIER. The numeral is
---omitted when it equals the number of ancestor members, MEMBERS_GENS
---(default GENS); then the flags carry the tier.
local function part_chunks (count, gens, tier, members_gens)
  local chunks = {}
  local show_numeral = count ~= #distinct_gens(members_gens or gens)
  if show_numeral then table.insert(chunks, styled(tostring(count), tier)) end
  for _, c in ipairs(ancestor_chunks(gens, not show_numeral and tier or nil)) do
    table.insert(chunks, c) end
  return chunks
end

---Chunks for a side with COUNT members and ancestor flags GENS, at TIER.
local function side_chunks (count, gens, tier)
  if count == 0 and #gens == 0 then return {} end
  return part_chunks(count, gens, tier)
end

---Chunks for a side with a subset of its own tier: NUMERATOR (with
---flags NUMERATOR_GENS, at SUBSET_TIER) of TOTAL (with flags TOTAL_GENS,
---at SIDE_TIER). The slash shows SIDE_TIER.
local function fraction_chunks (total, total_gens, numerator,
                                numerator_gens, side_tier, subset_tier)
  assert(numerator <= total, 'herald subset exceeds total')
  if total == 0 then return {} end
  if numerator == 0 then
    return side_chunks(total, total_gens, side_tier) end
  total_gens = distinct_gens(total_gens)
  numerator_gens = distinct_gens(numerator_gens)
  local numerator_set = {}
  for _, g in ipairs(numerator_gens) do numerator_set[g] = true end
  local remaining_gens = {}
  for _, g in ipairs(total_gens) do
    if not numerator_set[g] then table.insert(remaining_gens, g) end end
  local chunks = part_chunks(numerator, numerator_gens, subset_tier)
  table.insert(chunks, styled('/', side_tier))
  if numerator ~= total then
    for _, c in ipairs(part_chunks(total, remaining_gens, side_tier,
                                   total_gens)) do
      table.insert(chunks, c) end
  end
  return chunks
end

---Whether the side's only members are ancestors that BIRTH, a list of
---{relation, side, generation} facts, already accounts for.
local function birth_explained (rel, side, count, gens, birth)
  gens = distinct_gens(gens)
  if #gens == 0 or count ~= #gens then return false end
  for _, g in ipairs(gens) do
    local accounted = false
    for _, fact in ipairs(birth) do
      if fact.relation == rel and fact.side == side
         and fact.generation == g then accounted = true end
    end
    if not accounted then return false end
  end
  return true
end

---Chunks for relation REL's SIDE ('in' or 'out') from FORM.
local function rel_side_chunks (rel, side, form, birth, write_protected)
  local s = assq(form, side)
  if not s then return {} end
  local count, gens = first_number(s) or 0, ancestors_of(s)
  local tiers = relationship_styles.side_tiers[rel]
  local tier
  if birth_explained(rel, side, count, gens, birth) then
    tier = relationship_styles.birth_explained_side
  elseif rel == 'contains' and side == 'out' and write_protected then
    tier = tiers.out_write_protected
  else tier = tiers[side] end
  local subset_key, subset_tier = nil, nil
  if rel == 'contains' and side == 'out' then
    subset_key, subset_tier = 'unintegrated', tiers.out_unintegrated
  elseif rel == 'links_to' and side == 'in' then
    subset_key, subset_tier = 'substantive', tiers.in_substantive end
  local subset = subset_key and assq(s, subset_key) or nil
  if subset then
    return fraction_chunks(count, gens, first_number(subset) or 0,
                           ancestors_of(subset), tier, subset_tier) end
  return side_chunks(count, gens, tier)
end

---Chunks for relation REL's token from FORM, or nil if it shows nothing.
local function rel_chunks (rel, form, birth, write_protected, overrides_here)
  local in_c = rel_side_chunks(rel, 'in', form, birth, write_protected)
  local out_c = rel_side_chunks(rel, 'out', form, birth, write_protected)
  local here = rel == 'overrides_view_of' and overrides_here
  if #in_c == 0 and #out_c == 0 and not here then return nil end
  local born = false
  for _, fact in ipairs(birth) do
    if fact.relation == rel then born = true end end
  local chunks = {}
  for _, c in ipairs(in_c) do table.insert(chunks, c) end
  table.insert(chunks, styled(rel_letter(rel),
    born and relationship_styles.birth_letter or relationship_styles.letter))
  if here then
    table.insert(chunks, styled('ĥ', relationship_styles.overrides_here)) end
  for _, c in ipairs(out_c) do table.insert(chunks, c) end
  return chunks
end

---Render the semantic (rels ...) payload in SEXP to virtual-text chunks,
---or nil if there is none / it produces nothing. Tokens are the
---relations in display order (C L S O H), then the property counts
---A I F, space-separated.
---@param sexp any
---@return table[]|nil
function M.render_rel_facts (sexp)
  local rels = M.find_rels(sexp)
  if not rels then return nil end
  local node = assq(sexp, 'node')
  local view_stats = assq(node, 'viewStats')
  local overrides_here = assq(view_stats, 'overridesHere') ~= nil
  local write_protected = has_atom(node, 'writeProtected')
  local birth = {}
  local birth_form = assq(rels, 'birth')
  if birth_form then
    -- each birth fact is (RELATION SIDE [GEN])
    for i = 2, #birth_form do
      local fact = birth_form[i]
      table.insert(birth, { relation = sexpr.atom_text(fact[1]),
                            side = sexpr.atom_text(fact[2]),
                            generation = fact[3] }) end
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
    if form or (rel == 'overrides_view_of' and overrides_here) then
      add_token(rel_chunks(rel, form, birth, write_protected, overrides_here))
    end
  end
  for _, count in ipairs({ { 'aliases', 'A' }, { 'extraIds', 'I' },
                           { 'flags', 'F' } }) do
    local form = assq(rels, count[1])
    if form then
      add_token({ styled(count[2] .. tostring(first_number(form)),
                         relationship_styles.property_counts) })
    end
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
        local cur_text, cur_style = nil, nil
        local function flush ()
          if cur_text and #cur_text > 0 then
            table.insert(chunks,
              { cur_text,
                M.style_highlight_group(cur_style) or 'Normal' }) end
        end
        for _, cell in ipairs(cells) do
          if cur_text ~= nil and cell.style == cur_style then
            cur_text = cur_text .. cell.character
          else
            flush(); cur_text = cell.character
            cur_style = cell.style end
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

---Set the conceal options and the view faces ('winhighlight') on
---windows currently showing BUF, and arrange (once per buffer) for
---future windows to get them too.
---@param buf integer
function M.conceal_windows_showing (buf)
  local function apply (win)
    vim.wo[win][0].conceallevel = 2
    vim.wo[win][0].concealcursor = 'nc'
    vim.wo[win][0].winhighlight = M.view_winhighlight
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
