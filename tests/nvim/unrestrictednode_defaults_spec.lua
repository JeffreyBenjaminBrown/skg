-- Mirrors tests/elisp/test-skg-unrestrictedNode-defaults.el.

local sexpr = require('skg.sexpr.parse')
local bijection = require('skg.sexpr.org_bijection')
local defaults = require('skg.sexpr.unrestrictednode_defaults')

---Find a headline with text TEXT among HEADLINES; returns its index.
local function find_text (headlines, text)
  for index, headline in ipairs(headlines) do
    if headline.text == text then return index end
  end
  return nil
end

local function expanded_headlines (sexp_text, default_skgrepo,
                                   display_title)
  local org_text = bijection.sexp_to_org(sexpr.read(sexp_text))
  local expanded = defaults.expand_defaults_in_org(
    org_text, default_skgrepo, display_title)
  return bijection.extract_headlines(expanded)
end

---Expand then strip then re-read, as the edit-buffer commit path does.
local function round_trip (sexp_text, default_skgrepo)
  local org_text = bijection.sexp_to_org(sexpr.read(sexp_text))
  local expanded = defaults.expand_defaults_in_org(
    org_text, default_skgrepo)
  local stripped = defaults.strip_defaults_from_org(expanded)
  return bijection.org_to_sexp(stripped)
end

local function strip_to_sexp (org_text)
  return bijection.org_to_sexp(
    defaults.strip_defaults_from_org(org_text))
end

describe('skg.sexpr.unrestrictednode_defaults.unrestrictedNode_sexp_p', function ()
  it('recognizes an UnrestrictedVognode sexp', function ()
    assert.is_true(defaults.unrestrictedNode_sexp_p(
      sexpr.read('(skg (node (id abc) (repo jeff)))')))
  end)

  it('rejects a non-skg sexp', function ()
    assert.is_false(defaults.unrestrictedNode_sexp_p(
      sexpr.read('(foo (node (id abc)))')))
  end)

  it('rejects a skg sexp without node', function ()
    assert.is_false(defaults.unrestrictedNode_sexp_p(
      sexpr.read('(skg (alias (id abc)))')))
  end)
end)

describe('skg.sexpr.unrestrictednode_defaults.headlines_to_org', function ()
  it('converts a headline list to org text', function ()
    assert.are.equal('* skg\n** node\n*** id\n**** abc',
      defaults.headlines_to_org({
        { level = 1, text = 'skg' }, { level = 2, text = 'node' },
        { level = 3, text = 'id' }, { level = 4, text = 'abc' } }))
  end)
end)

describe('skg.sexpr.unrestrictednode_defaults expansion', function ()
  it('inserts all default fields into a minimal UnrestrictedVognode', function ()
    local headlines =
      expanded_headlines('(skg (node (id abc) (repo jeff)))')
    -- skg, node, id/abc, skgrepo/jeff, write-protected/false, affectsParent/true,
    -- birth/unremarkable, editRequest/none, viewRequests/none:
    -- each key AND each value is a separate headline.
    assert.are.equal(16, #headlines)
    assert.is_not_nil(find_text(headlines, 'writeProtected'))
    assert.is_not_nil(find_text(headlines, 'false (default)'))
    assert.is_not_nil(find_text(headlines, 'editRequest'))
    assert.is_not_nil(find_text(headlines, 'none (default)'))
  end)

  it('shows a true child for a bare writeProtected', function ()
    local headlines =
      expanded_headlines('(skg (node (id abc) (repo jeff) writeProtected))')
    local write_protected_index = find_text(headlines, 'writeProtected')
    assert.is_not_nil(write_protected_index)
    assert.are.equal('true', headlines[write_protected_index + 1].text)
  end)

  it('inserts true (default) when affectsParent is missing', function ()
    local headlines =
      expanded_headlines('(skg (node (id abc) (repo jeff)))')
    local affectsParent_index = find_text(headlines, 'affectsParent')
    assert.is_not_nil(affectsParent_index)
    assert.are.equal('true (default)',
                     headlines[affectsParent_index + 1].text)
  end)

  it('orders fields canonically', function ()
    local headlines = expanded_headlines(
      '(skg (node (repo jeff) (graphStats 42) (id abc) writeProtected))')
    local level_3 = {}
    for _, headline in ipairs(headlines) do
      if headline.level == 3 then
        table.insert(level_3, headline.text) end
    end
    assert.are.same(
      { 'id', 'repo', 'writeProtected', 'affectsParent', 'birth',
        'editRequest', 'viewRequests', 'graphStats' },
      level_3)
  end)
end)

describe('skg.sexpr.unrestrictednode_defaults stripping', function ()
  it('is identity on an unmodified expansion', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (repo jeff)))'),
      round_trip('(skg (node (id abc) (repo jeff)))'))
  end)

  it('collapses writeProtected=true to a bare atom', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (repo jeff) writeProtected))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** repo', '**** jeff',
        '*** writeProtected', '**** true',
        '*** affectsParent', '**** true (default)',
        '*** birth', '**** unremarkable (default)',
        '*** editRequest', '**** none (default)',
        '*** viewRequests', '**** none (default)' }, '\n')))
  end)

  it('accepts bare default spellings without (default)', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (repo jeff)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** repo', '**** jeff',
        '*** writeProtected', '**** false',
        '*** affectsParent', '**** true',
        '*** birth', '**** unremarkable',
        '*** editRequest', '**** none',
        '*** viewRequests', '**** none' }, '\n')))
  end)

  it('round-trips a bare writeProtected', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (repo jeff) writeProtected))'),
      round_trip('(skg (node (id abc) (repo jeff) writeProtected))'))
  end)

  it('round-trips (editRequest delete)', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (repo jeff)'
                 .. ' (editRequest delete)))'),
      round_trip('(skg (node (id abc) (repo jeff)'
                 .. ' (editRequest delete)))'))
  end)

  it('round-trips (editRequest (merge XYZ))', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (repo jeff)'
                 .. ' (editRequest (merge XYZ))))'),
      round_trip('(skg (node (id abc) (repo jeff)'
                 .. ' (editRequest (merge XYZ))))'))
  end)

  it('extracts the id from a merge org link', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (repo jeff)'
                 .. ' (editRequest (merge XYZ))))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** repo', '**** jeff',
        '*** editRequest', '**** merge [[id:XYZ][some label]]' },
        '\n')))
  end)

  it('keeps affectsParent=false', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (repo jeff)'
                 .. ' (affectsParent false)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** repo', '**** jeff',
        '*** affectsParent', '**** false' }, '\n')))
  end)

  it('keeps populated viewRequests', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (repo jeff)'
                 .. ' (viewRequests (folder aliases) (roleTree container))))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** repo', '**** jeff',
        '*** viewRequests', '**** folder', '***** aliases',
        '**** roleTree', '***** container' }, '\n')))
  end)

  it('drops every childless editable field of the empty-node skeleton',
     function ()
    assert.are.same(sexpr.read('(skg (node (repo only)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** repo', '**** only',
        '*** writeProtected', '*** affectsParent', '*** birth',
        '*** editRequest', '*** viewRequests' }, '\n')))
  end)

  it('drops a childless viewRequests rather than keeping a bare atom',
     function ()
    assert.are.same(sexpr.read('(skg (node (repo only)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** repo', '**** only',
        '*** viewRequests' }, '\n')))
  end)
end)

describe('skg.sexpr.unrestrictednode_defaults repo defaults', function ()
  it('marks a matching repo value with (default)', function ()
    local headlines = expanded_headlines(
      '(skg (node (id abc) (repo jeff)))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'jeff (default)'))
  end)

  it('leaves a non-matching repo value bare', function ()
    local headlines = expanded_headlines(
      '(skg (node (id abc) (repo bob)))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'bob'))
    assert.is_nil(find_text(headlines, 'bob (default)'))
  end)

  it('inserts the default repo when missing', function ()
    local headlines =
      expanded_headlines('(skg (node (id abc)))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'repo'))
    assert.is_not_nil(find_text(headlines, 'jeff (default)'))
  end)

  it('strips the (default) suffix from a repo value', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (repo jeff)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** repo', '**** jeff (default)' }, '\n')))
  end)

  it('keeps a bare repo value as-is', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (repo bob)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** repo', '**** bob' }, '\n')))
  end)

  it('round-trips with a default repo', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (repo jeff)))'),
      round_trip('(skg (node (id abc) (repo jeff)))', 'jeff'))
  end)

  it('round-trips a new node with no repo', function ()
    local expanded = defaults.expand_defaults_in_org(
      '* skg\n** node', 'jeff')
    local stripped = defaults.strip_defaults_from_org(expanded)
    assert.are.same(sexpr.read('(skg (node (repo jeff)))'),
      bijection.org_to_sexp(stripped))
  end)
end)

describe('skg.sexpr.unrestrictednode_defaults real-world metadata', function ()
  it('preserves an existing repo and appends non-canonical fields',
     function ()
    local headlines = expanded_headlines(
      '(skg (node (id abc) (repo public) (rels "C5")))')
    assert.is_not_nil(find_text(headlines, 'repo'))
    assert.is_not_nil(find_text(headlines, 'public'))
    assert.is_not_nil(find_text(headlines, 'id'))
    assert.is_not_nil(find_text(headlines, 'abc'))
    assert.is_not_nil(find_text(headlines, 'rels'))
  end)

  it('round-trips id + repo', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (repo public)))'),
      round_trip('(skg (node (id abc) (repo public)))'))
  end)

  it('expands real-world metadata with a herald field', function ()
    local headlines = expanded_headlines(
      '(skg (node (id 6972d099) (repo public)'
      .. ' (rels "C5 4(1,1)L")))')
    assert.is_not_nil(find_text(headlines, 'repo'))
    assert.is_not_nil(find_text(headlines, 'public'))
    assert.is_not_nil(find_text(headlines, 'writeProtected'))
    assert.is_not_nil(find_text(headlines, 'affectsParent'))
    assert.is_not_nil(find_text(headlines, 'editRequest'))
    assert.is_not_nil(find_text(headlines, 'rels'))
  end)
end)

describe('skg.sexpr.unrestrictednode_defaults display title', function ()
  it('prepends the title group', function ()
    local headlines = expanded_headlines(
      '(skg (node (id abc) (repo jeff)))', nil, 'actual title')
    assert.are.same(
      { { level = 1, text = 'title' },
        { level = 2, text = 'actual title' },
        { level = 1, text = 'skg' },
        { level = 2, text = 'node' } },
      vim.list_slice(headlines, 1, 4))
  end)

  it('does not prepend an empty title', function ()
    local headlines = expanded_headlines(
      '(skg (node (id abc) (repo jeff)))', nil, '')
    assert.are.same({ level = 1, text = 'skg' }, headlines[1])
  end)

  it('is removed again by stripping', function ()
    local org_text = bijection.sexp_to_org(
      sexpr.read('(skg (node (id abc) (repo jeff)))'))
    local expanded = defaults.expand_defaults_in_org(
      org_text, nil, 'actual title')
    local stripped = defaults.strip_defaults_from_org(expanded)
    assert.are.same(sexpr.read('(skg (node (id abc) (repo jeff)))'),
      bijection.org_to_sexp(stripped))
  end)
end)
