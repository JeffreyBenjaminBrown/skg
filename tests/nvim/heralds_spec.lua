-- Mirrors tests/elisp/test-heralds-minor-mode.el, with extmarks in
-- place of display-property overlays; the rendering cases both read
-- are in tests/shared/herald-rendering-cases.txt. The pinned fixture
-- tests/shared/herald-rules.sexp is installed directly (the analog of
-- skg-test-install-herald-rules), so no server is needed.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local herald_rules = require('skg.herald_rules')
local heralds = require('skg.heralds')

local install_fixture_rules = helpers.install_fixture_herald_rules

local function herald_text (metadata_text)
  return heralds.chunks_text(heralds.chunks_from_metadata(metadata_text))
end

local function scratch_buffer_with (lines)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_set_current_buf(buf)
  return buf
end

local function herald_extmarks (buf)
  return vim.api.nvim_buf_get_extmarks(
    buf, heralds.namespace, 0, -1, { details = true })
end

---{name, metadata, text, styles, context} for each case in the file,
---which the Emacs tests read too. STYLES has one style name (or '-')
---per character; CONTEXT is the text of its '---- context: ' line, or
---nil.
local function shared_cases ()
  local path = _G.skg_test_repo_root()
    .. '/tests/shared/herald-rendering-cases.txt'
  local cases, current = {}, nil
  for line in io.lines(path) do
    if line:sub(1, 5) == '==== ' then
      current = { name = line:sub(6) }
    elseif line:sub(1, 14) == '---- context: ' then
      current.context = line:sub(15)
    elseif line:sub(1, 11) == '---- text: ' then
      current.text = line:sub(12)
    elseif line:sub(1, 13) == '---- styles: ' then
      current.styles = {}
      for _, run in ipairs(vim.split(line:sub(14), ' ', { trimempty = true })) do
        local style, count = run:match('^(.*)%*(%d+)$')
        for _ = 1, tonumber(count) do table.insert(current.styles, style) end
      end
      table.insert(cases, current)
      current = nil
    elseif current and current.metadata == nil then
      current.metadata = line
    end
  end
  return cases
end

---The style of each character of CHUNKS: the style whose highlight
---group a chunk has, or '-'.
local function character_styles (chunks)
  local styles = {}
  for _, chunk in ipairs(chunks) do
    local style = chunk[2] and chunk[2]:match('^SkgHerald(.*)$')
    style = style and style:lower() or '-'
    for _ = 1, vim.fn.strchars(chunk[1]) do table.insert(styles, style) end
  end
  return styles
end

describe('skg.heralds', function ()
  before_each(install_fixture_rules)

  it('renders the cases shared with Emacs', function ()
    local cases = shared_cases()
    assert.is_true(#cases > 40)
    local folder_changes = { ['folderChange ready'] = { ready = true },
                             ['folderChange unready'] = { ready = false } }
    for _, case in ipairs(cases) do
      local chunks = heralds.chunks_from_metadata(
        case.metadata, folder_changes[case.context],
        case.context == 'pendingDetachment')
      assert.are.same({ case.name, case.text, case.styles },
                      { case.name, heralds.chunks_text(chunks),
                        character_styles(chunks) })
    end
  end)

  it('toggling adds and removes extmarks', function ()
    local buf = scratch_buffer_with({
      'Test line with (skg node (gnid 123) (view (rels (contains (out 2)))'
      .. ' cycle)) herald',
      'Another line (skg node (gnid 456) (editRequest delete)'
      .. ' (view (rels (linksTo (in 3 (substantive 3)))))) more text',
      'Plain line without heralds' })
    assert.is_true(heralds.enable(buf))
    local marks = herald_extmarks(buf)
    assert.is_true(#marks > 0)
    local has_virt_text = false
    for _, mark in ipairs(marks) do
      if mark[4].virt_text then has_virt_text = true end
    end
    assert.is_true(has_virt_text)
    heralds.disable(buf)
    assert.are.equal(0, #herald_extmarks(buf))
  end)

  it('conceals the metadata extent and renders semantic rel facts',
     function ()
    -- rels payload (contains (in 2 (ancestors 1))), birth contains
    -- renders as the C token 2aC: the in-side numeral "2" (medium), the
    -- ancestor "a" at its floor (low), and the birth letter "C" (low).
    local buf = scratch_buffer_with({
      'Line with (skg node (gnid 123) noClaim (editRequest delete)'
      .. ' (view (rels (contains (in 2 (ancestors 1))))'
      .. ' (birth (contains in 1)) noWarrant cycle)) text' })
    heralds.enable(buf)
    local marks = herald_extmarks(buf)
    assert.are.equal(1, #marks)
    local details = marks[1][4]
    assert.are.equal('', details.conceal)
    local text = ''
    local hl_of = {}
    for _, chunk in ipairs(details.virt_text) do
      text = text .. chunk[1]
      hl_of[chunk[1]] = chunk[2] end
    -- the sentinel placeholder must never leak into the display
    assert.is_falsy(text:find('__RELS_SPANS__', 1, true))
    -- ⊥ (noWarrant), the 2aC relationship token, ⟳ (cycle), delete
    assert.is_truthy(text:find('⊥', 1, true))
    assert.is_truthy(text:find('2aC', 1, true))
    assert.is_truthy(text:find('⟳', 1, true))
    assert.is_truthy(text:find('delete', 1, true))
    -- per-span highlight groups on the 2aC token
    assert.are.equal('SkgHeraldMedium', hl_of['2'])
    assert.are.equal('SkgHeraldLow', hl_of['a'])
    assert.are.equal('SkgHeraldLow', hl_of['C'])
    heralds.disable(buf)
    assert.are.equal(0, #herald_extmarks(buf))
  end)

  it('dictates the view faces in a heralded window', function ()
    local buf = scratch_buffer_with({
      '* (skg node (gnid 1) (view (rels (contains (out 2))))) a headline' })
    heralds.enable(buf)
    local winhighlight = vim.wo.winhighlight
    assert.is_truthy(winhighlight:find('Normal:SkgViewDefault', 1, true))
    assert.is_truthy(winhighlight:find(
      '@org.headline.level1:SkgViewHeadlineLevel1', 1, true))
    local default = vim.api.nvim_get_hl(0, { name = 'SkgViewDefault' })
    assert.are.equal(0xffffff, default.fg)
    assert.are.equal(0x000000, default.bg)
    local level1 = vim.api.nvim_get_hl(0, { name = 'SkgViewHeadlineLevel1' })
    assert.are.equal(0xff8888, level1.fg)
    assert.are.equal(0x000000, level1.bg) -- the default's background
    heralds.disable(buf)
  end)

  it('self-heals a missing rule table via the fetcher', function ()
    -- Mirrors test-heralds-self-heals-missing-rule-table: the stubbed
    -- fetcher stands in for the server answering the re-request.
    herald_rules.install_rules(nil)
    local original = herald_rules.request_herald_rules
    herald_rules.request_herald_rules = install_fixture_rules
    local buf = scratch_buffer_with({
      '(skg node (gnid 1) (skgRepos (nodeRepo s)) (view (rels (contains (out 2)))))' })
    local enabled = heralds.enable(buf)
    herald_rules.request_herald_rules = original
    assert.is_true(enabled)
    assert.is_truthy(herald_rules.get_rules())
    assert.is_true(#herald_extmarks(buf) > 0)
  end)

  it('gives up after bounded fetch attempts', function ()
    -- Mirrors test-heralds-gives-up-after-bounded-fetch-attempts.
    herald_rules.install_rules(nil)
    require('skg.state').tcp = nil
    local calls = 0
    local original = herald_rules.request_herald_rules
    local original_timeout = herald_rules.attempt_timeout_ms
    herald_rules.request_herald_rules = function () calls = calls + 1 end
    herald_rules.attempt_timeout_ms = 20
    local buf = scratch_buffer_with({ '(skg node (gnid 1))' })
    local enabled = heralds.enable(buf)
    herald_rules.request_herald_rules = original
    herald_rules.attempt_timeout_ms = original_timeout
    assert.is_false(enabled)
    assert.is_nil(herald_rules.get_rules())
    assert.are.equal(herald_rules.max_attempts, calls)
    assert.are.equal(0, #herald_extmarks(buf))
    install_fixture_rules()
  end)

  it('refreshes heralds on edited lines', function ()
    local buf = scratch_buffer_with({ 'plain line', 'another' })
    heralds.enable(buf)
    assert.are.equal(0, #herald_extmarks(buf))
    vim.api.nvim_buf_set_lines(buf, 0, 1, false, {
      '* (skg node (gnid 9) (view (rels (contains (out 2))))) now a headline' })
    vim.wait(200, function () return #herald_extmarks(buf) > 0 end, 10)
    assert.are.equal(1, #herald_extmarks(buf))
  end)

  it('renders a folder changed by its members, and keeps it current',
     function ()
    local buf = scratch_buffer_with({
      '* (skg node (gnid p)) p',
      '** (skg containerFolder) its containers',
      '*** (skg node (gnid r) (diff (rel added) (leaves 2 node rel) (ready rel))) r',
      '**** (skg node (gnid below)) below',
      '* (skg node (gnid after)) after' })
    heralds.enable(buf)
    local function folder_herald ()
      local marks = vim.api.nvim_buf_get_extmarks(
        buf, heralds.namespace, { 1, 0 }, { 1, -1 }, { details = true })
      return marks[1] and heralds.chunks_text(marks[1][4].virt_text)
    end
    assert.are.equal('☮ its containers changed (other files)✓',
                     folder_herald())
    -- Editing a member, or a line below it, rerenders the folder.
    vim.api.nvim_buf_set_lines(buf, 2, 3, false, {
      '*** (skg node (gnid r) (diff (rel added) (leaves 2 node rel) (ready node))) r' })
    vim.wait(200, function ()
      return folder_herald() == '☮ its containers changed (other files)' end, 10)
    assert.are.equal('☮ its containers changed (other files)',
                     folder_herald())
    vim.api.nvim_buf_set_lines(buf, 2, 3, false, {
      '*** (skg node (gnid r) (diff (node added) (ready node))) r' })
    vim.wait(200, function ()
      return folder_herald() == '☮ These contain it.' end, 10)
    assert.are.equal('☮ These contain it.', folder_herald())
    vim.api.nvim_buf_set_lines(buf, 3, 4, false, {
      '**** (skg (relRepoMove (old a) (new b)) (diff (leaves 1) (ready)))' })
    vim.wait(200, function ()
      return folder_herald() == '☮ its containers changed (other files)✓' end, 10)
    assert.are.equal('☮ its containers changed (other files)✓',
                     folder_herald())
    heralds.disable(buf)
    -- A changed folder's label stands in for its own, with ✓ when ready.
    assert.are.equal('its aliases changed (this file)✓',
      heralds.chunks_text(heralds.chunks_from_metadata(
        '(skg aliasFolder)', { ready = true })))
    assert.are.equal('aliases',
      heralds.chunks_text(heralds.chunks_from_metadata('(skg aliasFolder)')))
  end)

  it('renders relations and property counts once', function ()
    local text = herald_text(
      '(skg node (gnid x) (view (rels (contains (out 2))) (props (aliases 3))))')
    local _, relations = text:gsub('C2', '')
    local _, aliases = text:gsub('A3', '')
    assert.are.same({ 1, 1 }, { relations, aliases })
    assert.is_falsy(text:find('__RELS_SPANS__', 1, true))
  end)

  it('strips structural colons the way the elisp display does',
     function ()
    -- '☮:x' -> '☮x' ('☮' is outside the keep-class), while
    -- alphanumeric neighbors keep their colon ('req:folder'), and so
    -- does a colon after '⌂' ('⌂:public').
    local function stripped (text)
      local cells = heralds.strip_structural_colons(
        heralds.token_character_cells(
          { chunks = { { text = text, style = nil } }, abut = false }))
      local result = ''
      for _, cell in ipairs(cells) do result = result .. cell.character end
      return result
    end
    assert.are.same({ '☮x', 'req:folder', '⌂:public', '⌂:(' },
                    { stripped('☮:x'), stripped('req:folder'),
                      stripped('⌂:public'), stripped('⌂:(') })
  end)

  ---The display text of the herald on the last of LINES, with heralds
  ---on, or nil when it has none.
  local function last_line_display (lines)
    local buf = scratch_buffer_with(lines)
    heralds.enable(buf)
    local marks = vim.api.nvim_buf_get_extmarks(
      buf, heralds.namespace, { #lines - 1, 0 }, { #lines - 1, -1 },
      { details = true })
    heralds.disable(buf)
    return marks[1] and heralds.chunks_text(marks[1][4].virt_text)
  end

  it('renders a pending detachment as a request', function ()
    -- Mirrors test-heralds-a-pending-detachment-renders-as-a-request: a
    -- child that is 'noClaim' and warranted under an editable parent (a
    -- node that is not write-protected, or an editable folder whose
    -- anchor is one) shows "detach". Under a write-protected parent or
    -- folder, or unwarranted, it shows no request.
    local child = '** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim) c'
    local editable = '* (skg node (gnid r) (skgRepos (nodeRepo r))) r'
    local protected =
      '* (skg node (gnid r) (skgRepos (nodeRepo r)) writeProtected) r'
    assert.are.equal('detach', last_line_display({ editable, child }))
    assert.is_nil(last_line_display({ protected, child }))
    assert.are.equal('⊥', last_line_display({ editable,
      '** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim'
      .. ' (view noWarrant)) c' }))
    assert.is_nil(last_line_display({
      '** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim) a root' }))
    assert.are.equal('alias detach', last_line_display({
      editable, '** (skg aliasFolder) aliases',
      '*** (skg alias noClaim) an alias' }))
    assert.are.equal('alias', last_line_display({
      protected, '** (skg aliasFolder) aliases',
      '*** (skg alias noClaim) an alias' }))
    assert.are.equal('☮', last_line_display({
      editable, '** (skg containerFolder) containers',
      '*** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim'
      .. ' writeProtected) c' }))
  end)
end)
