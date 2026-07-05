-- Mirrors tests/elisp/test-skg-activeNode-defaults.el.

local sexpr = require('skg.sexpr.parse')
local bijection = require('skg.sexpr.org_bijection')
local defaults = require('skg.sexpr.activenode_defaults')

---Find a headline with text TEXT among HEADLINES; returns its index.
local function find_text (headlines, text)
  for index, headline in ipairs(headlines) do
    if headline.text == text then return index end
  end
  return nil
end

local function expanded_headlines (sexp_text, default_source,
                                   display_title)
  local org_text = bijection.sexp_to_org(sexpr.read(sexp_text))
  local expanded = defaults.expand_defaults_in_org(
    org_text, default_source, display_title)
  return bijection.extract_headlines(expanded)
end

---Expand then strip then re-read, as the edit-buffer commit path does.
local function round_trip (sexp_text, default_source)
  local org_text = bijection.sexp_to_org(sexpr.read(sexp_text))
  local expanded = defaults.expand_defaults_in_org(
    org_text, default_source)
  local stripped = defaults.strip_defaults_from_org(expanded)
  return bijection.org_to_sexp(stripped)
end

local function strip_to_sexp (org_text)
  return bijection.org_to_sexp(
    defaults.strip_defaults_from_org(org_text))
end

describe('skg.sexpr.activenode_defaults.activeNode_sexp_p', function ()
  it('recognizes an ActiveNode sexp', function ()
    assert.is_true(defaults.activeNode_sexp_p(
      sexpr.read('(skg (node (id abc) (source jeff)))')))
  end)

  it('rejects a non-skg sexp', function ()
    assert.is_false(defaults.activeNode_sexp_p(
      sexpr.read('(foo (node (id abc)))')))
  end)

  it('rejects a skg sexp without node', function ()
    assert.is_false(defaults.activeNode_sexp_p(
      sexpr.read('(skg (alias (id abc)))')))
  end)
end)

describe('skg.sexpr.activenode_defaults.headlines_to_org', function ()
  it('converts a headline list to org text', function ()
    assert.are.equal('* skg\n** node\n*** id\n**** abc',
      defaults.headlines_to_org({
        { level = 1, text = 'skg' }, { level = 2, text = 'node' },
        { level = 3, text = 'id' }, { level = 4, text = 'abc' } }))
  end)
end)

describe('skg.sexpr.activenode_defaults expansion', function ()
  it('inserts all default fields into a minimal ActiveNode', function ()
    local headlines =
      expanded_headlines('(skg (node (id abc) (source jeff)))')
    -- skg, node, id/abc, source/jeff, indef/false, parentIs/affected,
    -- birth/unremarkable, editRequest/none, viewRequests/none:
    -- each key AND each value is a separate headline.
    assert.are.equal(16, #headlines)
    assert.is_not_nil(find_text(headlines, 'indef'))
    assert.is_not_nil(find_text(headlines, 'false (default)'))
    assert.is_not_nil(find_text(headlines, 'editRequest'))
    assert.is_not_nil(find_text(headlines, 'none (default)'))
  end)

  it('shows a true child for a bare indef', function ()
    local headlines =
      expanded_headlines('(skg (node (id abc) (source jeff) indef))')
    local indef_index = find_text(headlines, 'indef')
    assert.is_not_nil(indef_index)
    assert.are.equal('true', headlines[indef_index + 1].text)
  end)

  it('inserts affected (default) when parentIs is missing', function ()
    local headlines =
      expanded_headlines('(skg (node (id abc) (source jeff)))')
    local parentIs_index = find_text(headlines, 'parentIs')
    assert.is_not_nil(parentIs_index)
    assert.are.equal('affected (default)',
                     headlines[parentIs_index + 1].text)
  end)

  it('orders fields canonically', function ()
    local headlines = expanded_headlines(
      '(skg (node (source jeff) (graphStats 42) (id abc) indef))')
    local level_3 = {}
    for _, headline in ipairs(headlines) do
      if headline.level == 3 then
        table.insert(level_3, headline.text) end
    end
    assert.are.same(
      { 'id', 'source', 'indef', 'parentIs', 'birth',
        'editRequest', 'viewRequests', 'graphStats' },
      level_3)
  end)
end)

describe('skg.sexpr.activenode_defaults stripping', function ()
  it('is identity on an unmodified expansion', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (source jeff)))'),
      round_trip('(skg (node (id abc) (source jeff)))'))
  end)

  it('collapses indef=true to a bare atom', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (source jeff) indef))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** source', '**** jeff',
        '*** indef', '**** true',
        '*** parentIs', '**** affected (default)',
        '*** birth', '**** unremarkable (default)',
        '*** editRequest', '**** none (default)',
        '*** viewRequests', '**** none (default)' }, '\n')))
  end)

  it('accepts bare default spellings without (default)', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (source jeff)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** source', '**** jeff',
        '*** indef', '**** false',
        '*** parentIs', '**** affected',
        '*** birth', '**** unremarkable',
        '*** editRequest', '**** none',
        '*** viewRequests', '**** none' }, '\n')))
  end)

  it('round-trips a bare indef', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (source jeff) indef))'),
      round_trip('(skg (node (id abc) (source jeff) indef))'))
  end)

  it('round-trips (editRequest delete)', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (source jeff)'
                 .. ' (editRequest delete)))'),
      round_trip('(skg (node (id abc) (source jeff)'
                 .. ' (editRequest delete)))'))
  end)

  it('round-trips (editRequest (merge XYZ))', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (source jeff)'
                 .. ' (editRequest (merge XYZ))))'),
      round_trip('(skg (node (id abc) (source jeff)'
                 .. ' (editRequest (merge XYZ))))'))
  end)

  it('extracts the id from a merge org link', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (source jeff)'
                 .. ' (editRequest (merge XYZ))))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** source', '**** jeff',
        '*** editRequest', '**** merge [[id:XYZ][some label]]' },
        '\n')))
  end)

  it('keeps parentIs=independent', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (source jeff)'
                 .. ' (parentIs independent)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** source', '**** jeff',
        '*** parentIs', '**** independent' }, '\n')))
  end)

  it('keeps populated viewRequests', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (source jeff)'
                 .. ' (viewRequests (col aliases) (path container))))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** source', '**** jeff',
        '*** viewRequests', '**** col', '***** aliases',
        '**** path', '***** container' }, '\n')))
  end)

  it('drops every childless editable field of the empty-node skeleton',
     function ()
    assert.are.same(sexpr.read('(skg (node (source only)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** source', '**** only',
        '*** indef', '*** parentIs', '*** birth',
        '*** editRequest', '*** viewRequests' }, '\n')))
  end)

  it('drops a childless viewRequests rather than keeping a bare atom',
     function ()
    assert.are.same(sexpr.read('(skg (node (source only)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** source', '**** only',
        '*** viewRequests' }, '\n')))
  end)
end)

describe('skg.sexpr.activenode_defaults source defaults', function ()
  it('marks a matching source value with (default)', function ()
    local headlines = expanded_headlines(
      '(skg (node (id abc) (source jeff)))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'jeff (default)'))
  end)

  it('leaves a non-matching source value bare', function ()
    local headlines = expanded_headlines(
      '(skg (node (id abc) (source bob)))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'bob'))
    assert.is_nil(find_text(headlines, 'bob (default)'))
  end)

  it('inserts the default source when missing', function ()
    local headlines =
      expanded_headlines('(skg (node (id abc)))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'source'))
    assert.is_not_nil(find_text(headlines, 'jeff (default)'))
  end)

  it('strips the (default) suffix from a source value', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (source jeff)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** source', '**** jeff (default)' }, '\n')))
  end)

  it('keeps a bare source value as-is', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (source bob)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** source', '**** bob' }, '\n')))
  end)

  it('round-trips with a default source', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (source jeff)))'),
      round_trip('(skg (node (id abc) (source jeff)))', 'jeff'))
  end)

  it('round-trips a new node with no source', function ()
    local expanded = defaults.expand_defaults_in_org(
      '* skg\n** node', 'jeff')
    local stripped = defaults.strip_defaults_from_org(expanded)
    assert.are.same(sexpr.read('(skg (node (source jeff)))'),
      bijection.org_to_sexp(stripped))
  end)
end)

describe('skg.sexpr.activenode_defaults real-world metadata', function ()
  it('preserves an existing source and appends non-canonical fields',
     function ()
    local headlines = expanded_headlines(
      '(skg (node (id abc) (source public) (rels "C5")))')
    assert.is_not_nil(find_text(headlines, 'source'))
    assert.is_not_nil(find_text(headlines, 'public'))
    assert.is_not_nil(find_text(headlines, 'id'))
    assert.is_not_nil(find_text(headlines, 'abc'))
    assert.is_not_nil(find_text(headlines, 'rels'))
  end)

  it('round-trips id + source', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (source public)))'),
      round_trip('(skg (node (id abc) (source public)))'))
  end)

  it('expands real-world metadata with a herald field', function ()
    local headlines = expanded_headlines(
      '(skg (node (id 6972d099) (source public)'
      .. ' (rels "C5 4(1,1)L")))')
    assert.is_not_nil(find_text(headlines, 'source'))
    assert.is_not_nil(find_text(headlines, 'public'))
    assert.is_not_nil(find_text(headlines, 'indef'))
    assert.is_not_nil(find_text(headlines, 'parentIs'))
    assert.is_not_nil(find_text(headlines, 'editRequest'))
    assert.is_not_nil(find_text(headlines, 'rels'))
  end)
end)

describe('skg.sexpr.activenode_defaults display title', function ()
  it('prepends the title group', function ()
    local headlines = expanded_headlines(
      '(skg (node (id abc) (source jeff)))', nil, 'actual title')
    assert.are.same(
      { { level = 1, text = 'title' },
        { level = 2, text = 'actual title' },
        { level = 1, text = 'skg' },
        { level = 2, text = 'node' } },
      vim.list_slice(headlines, 1, 4))
  end)

  it('does not prepend an empty title', function ()
    local headlines = expanded_headlines(
      '(skg (node (id abc) (source jeff)))', nil, '')
    assert.are.same({ level = 1, text = 'skg' }, headlines[1])
  end)

  it('is removed again by stripping', function ()
    local org_text = bijection.sexp_to_org(
      sexpr.read('(skg (node (id abc) (source jeff)))'))
    local expanded = defaults.expand_defaults_in_org(
      org_text, nil, 'actual title')
    local stripped = defaults.strip_defaults_from_org(expanded)
    assert.are.same(sexpr.read('(skg (node (id abc) (source jeff)))'),
      bijection.org_to_sexp(stripped))
  end)
end)
