-- Mirrors tests/elisp/test-heralds-minor-mode.el, with extmarks in
-- place of display-property overlays. The pinned fixture
-- tests/elisp/herald-rules.sexp is installed directly (the analog of
-- skg-test-install-herald-rules), so no server is needed.

local herald_rules = require('skg.herald_rules')
local heralds = require('skg.heralds')
local sexpr = require('skg.sexpr.parse')

local function install_fixture_rules ()
  local path = _G.skg_test_repo_root() .. '/tests/elisp/herald-rules.sexp'
  local handle = assert(io.open(path, 'r'))
  local text = handle:read('*a')
  handle:close()
  herald_rules.install_rules(sexpr.read(text))
end

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

describe('skg.heralds', function ()
  before_each(install_fixture_rules)

  it('toggling adds and removes extmarks', function ()
    local buf = scratch_buffer_with({
      'Test line with (skg (node (id 123) (rels (contains (out 2)))'
      .. ' (viewStats cycle))) herald',
      'Another line (skg (node (id 456) (rels (textlinksTo (in 3 (surprising 3))))'
      .. ' (editRequest delete))) more text',
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
    -- renders as the C token 2aC: the multi-contains "2" (orange), the
    -- ancestor "a" (yellow), and the birth "C" (black-on-white).
    local buf = scratch_buffer_with({
      'Line with (skg (node (id 123) (parentIs independent)'
      .. ' (rels (contains (in 2 (ancestors 1))) (birth contains))'
      .. ' (viewStats cycle) (editRequest delete))) text' })
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
    -- ⊥ (independent), the 2aC relationship token, ⟳ (cycle), delete
    assert.is_truthy(text:find('⊥', 1, true))
    assert.is_truthy(text:find('2aC', 1, true))
    assert.is_truthy(text:find('⟳', 1, true))
    assert.is_truthy(text:find('delete', 1, true))
    -- per-span highlight groups on the 2aC token
    assert.are.equal('SkgHeraldOrange', hl_of['2'])
    assert.are.equal('SkgHeraldYellow', hl_of['a'])
    assert.are.equal('SkgHeraldBirth', hl_of['C'])
    heralds.disable(buf)
    assert.are.equal(0, #herald_extmarks(buf))
  end)

  it('displays viewRequests as req:* heralds', function ()
    assert.is_truthy(
      herald_text('(skg (node (id 1) (viewRequests (col aliases))))')
      :find('req:col:?aliases'))
    assert.is_truthy(
      herald_text('(skg (node (id 2) (viewRequests (path container))))')
      :find('req:path:?container'))
    local many = herald_text(
      '(skg (node (id 4) (viewRequests (col aliases)'
      .. ' (path container) (path linkSource))))')
    assert.is_truthy(many:find('req:col:?aliases'))
    assert.is_truthy(many:find('req:path:?container'))
    assert.is_truthy(many:find('req:path:?linkSource'))
  end)

  it('displays scaffold kinds', function ()
    assert.is_truthy(herald_text('(skg aliasCol)'):find('aliases'))
    assert.is_truthy(herald_text('(skg alias)'):find('alias'))
    assert.is_truthy(
      herald_text('(skg folded aliasCol)'):find('aliases'))
    local changed = herald_text('(skg (textChanged staged unstaged))')
    assert.is_truthy(changed:find('text changed : staged', 1, true))
    assert.is_truthy(changed:find('text changed : unstaged', 1, true))
  end)

  it('displays staged/unstaged axes', function ()
    local removed =
      herald_text('(skg (node (id 1) (source s) (unstaged removedM)))')
    assert.is_truthy(removed:find('unstaged'))
    assert.is_truthy(removed:find('M'))
    local new_both =
      herald_text('(skg (node (id 2) (source s) (unstaged newX newM)))')
    assert.is_truthy(new_both:find('unstaged'))
    assert.is_truthy(new_both:find('X'))
    assert.is_truthy(new_both:find('M'))
    assert.is_truthy(
      herald_text('(skg alias (staged newM))'):find('staged:M', 1, true))
    local alias_removed = herald_text('(skg alias (unstaged removedM))')
    assert.is_truthy(alias_removed:find('unstaged'))
    assert.is_truthy(alias_removed:find('-M', 1, true))
  end)

  it('displays the inactive-node placeholder in blue', function ()
    local chunks = heralds.chunks_from_metadata('(skg inactiveNode)')
    assert.are.equal('node from inactive source',
                     heralds.chunks_text(chunks))
    assert.are.equal('SkgHeraldBlue', chunks[1][2])
  end)

  it('self-heals a missing rule table via the fetcher', function ()
    -- Mirrors test-heralds-self-heals-missing-rule-table: the stubbed
    -- fetcher stands in for the server answering the re-request.
    herald_rules.install_rules(nil)
    local original = herald_rules.request_herald_rules
    herald_rules.request_herald_rules = install_fixture_rules
    local buf = scratch_buffer_with({
      '(skg (node (id 1) (source s) (rels (contains (out 2)))))' })
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
    local buf = scratch_buffer_with({ '(skg (node (id 1)))' })
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
      '* (skg (node (id 9) (rels (contains (out 2))))) now a headline' })
    vim.wait(200, function () return #herald_extmarks(buf) > 0 end, 10)
    assert.are.equal(1, #herald_extmarks(buf))
  end)

  it('strips structural colons the way the elisp display does',
     function ()
    -- '⌂:public' -> '⌂public' ('⌂' is outside the keep-class), while
    -- alphanumeric neighbors keep their colon ('staged:M').
    local cells = heralds.strip_structural_colons(
      heralds.token_character_cells(
        { chunks = { { text = '⌂:public', color = nil } },
          abut = false }))
    local text = ''
    for _, cell in ipairs(cells) do text = text .. cell.character end
    assert.are.equal('⌂public', text)
  end)
end)
