-- Mirrors tests/elisp/test-skg-metadata-fields.el.

local sexpr = require('skg.sexpr.parse')
local bijection = require('skg.sexpr.org_bijection')
local fields = require('skg.sexpr.metadata_fields')
local shared = require('skg.shared')

---The unrestricted node's kind from 'shared/metadata-fields.json'.
local node_kind = shared.metadata_kind('node')

---Find a headline with text TEXT among HEADLINES; returns its index.
local function find_text (headlines, text)
  for index, headline in ipairs(headlines) do
    if headline.text == text then return index end
  end
  return nil
end

local function filledIn_headlines (sexp_text, default_skgrepo,
                                   display_title)
  local org_text = bijection.sexp_to_org(sexpr.read(sexp_text))
  local filledIn = fields.fillIn_defaults_in_org(
    org_text, node_kind, default_skgrepo, display_title)
  return bijection.extract_headlines(filledIn)
end

---Fill in then strip then re-read, as the edit-buffer apply path does.
local function round_trip (sexp_text, default_skgrepo)
  local org_text = bijection.sexp_to_org(sexpr.read(sexp_text))
  local filledIn = fields.fillIn_defaults_in_org(
    org_text, node_kind, default_skgrepo)
  local stripped = fields.strip_defaults_from_org(filledIn, node_kind)
  return bijection.org_to_sexp(stripped)
end

local function strip_to_sexp (org_text)
  return bijection.org_to_sexp(
    fields.strip_defaults_from_org(org_text, node_kind))
end

describe('skg.sexpr.metadata_fields.kind_of_sexp', function ()
  it('finds the kind of the first atom the table names', function ()
    for sexp_text, kind_name in pairs({
        ['(skg (node (id abc) (repo jeff)))'] = 'unrestricted node',
        ['(skg focused (node (id abc)))'] = 'unrestricted node',
        ['(skg (historical (id g) (repo r)))'] = 'historical phantom',
        ['(skg alias (relRepo r))'] = 'alias',
        ['(skg (flag noSearchMatching))'] = 'flag',
        ['(skg (subscribeeFolder changed))'] = 'folder',
        ['(skg folded tombstone)'] = 'tombstone' }) do
      assert.are.equal(kind_name,
        fields.kind_of_sexp(sexpr.read(sexp_text)).kind, sexp_text)
    end
    assert.is_nil(fields.kind_of_sexp(sexpr.read('(skg (relRepo r))')))
    assert.is_nil(fields.kind_of_sexp(sexpr.read('(foo)')))
  end)
end)

describe('skg.sexpr.metadata_fields.headlines_to_org', function ()
  it('converts a headline list to org text', function ()
    assert.are.equal('* skg\n** node\n*** id\n**** abc',
      fields.headlines_to_org({
        { level = 1, text = 'skg' }, { level = 2, text = 'node' },
        { level = 3, text = 'id' }, { level = 4, text = 'abc' } }))
  end)
end)

describe('skg.sexpr.metadata_fields filling in', function ()
  it('inserts all default fields into a minimal UnrestrictedVognode', function ()
    local headlines =
      filledIn_headlines('(skg (node (id abc) (repo jeff)))')
    -- skg, node, id/abc, skgrepo/jeff, write-protected/false, noClaim/false,
    -- editRequest/none, viewRequests/none:
    -- each key and each value is a separate headline.
    assert.are.equal(14, #headlines)
    assert.is_not_nil(find_text(headlines, 'writeProtected'))
    assert.is_not_nil(find_text(headlines, 'false (default)'))
    assert.is_not_nil(find_text(headlines, 'editRequest'))
    assert.is_not_nil(find_text(headlines, 'none (default)'))
  end)

  it('shows a true child for a bare writeProtected', function ()
    local headlines =
      filledIn_headlines('(skg (node (id abc) (repo jeff) writeProtected))')
    local write_protected_index = find_text(headlines, 'writeProtected')
    assert.is_not_nil(write_protected_index)
    assert.are.equal('true', headlines[write_protected_index + 1].text)
  end)

  it('offers noClaim as a bare atom', function ()
    for sexp_text, value in pairs({
        ['(skg (node (id abc) (repo jeff)))'] = 'false (default)',
        ['(skg (node (id abc) (repo jeff) noClaim))'] = 'true' }) do
      local headlines = filledIn_headlines(sexp_text)
      local noClaim_index = find_text(headlines, 'noClaim')
      assert.is_not_nil(noClaim_index)
      assert.are.equal(value, headlines[noClaim_index + 1].text)
    end
  end)

  it('orders fields canonically', function ()
    local headlines = filledIn_headlines(
      '(skg (node (repo jeff) (graphStats 42) (id abc) writeProtected))')
    local level_3 = {}
    for _, headline in ipairs(headlines) do
      if headline.level == 3 then
        table.insert(level_3, headline.text) end
    end
    assert.are.same(
      { 'id', 'repo', 'writeProtected', 'noClaim',
        'editRequest', 'viewRequests', 'graphStats' },
      level_3)
  end)
end)

describe('skg.sexpr.metadata_fields stripping', function ()
  it('is identity on unmodified filled-in text', function ()
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
        '*** noClaim', '**** false (default)',
        '*** editRequest', '**** none (default)',
        '*** viewRequests', '**** none (default)' }, '\n')))
  end)

  it('accepts bare default spellings without (default)', function ()
    assert.are.same(sexpr.read('(skg (node (id abc) (repo jeff)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** repo', '**** jeff',
        '*** writeProtected', '**** false',
        '*** noClaim', '**** false',
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

  it('collapses noClaim=true to a bare atom', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (repo jeff)'
                 .. ' noClaim))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** repo', '**** jeff',
        '*** noClaim', '**** true' }, '\n')))
  end)

  it('keeps populated viewRequests', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (repo jeff)'
                 .. ' (viewRequests (folder aliases) (folder containers))))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** repo', '**** jeff',
        '*** viewRequests', '**** folder', '***** aliases',
        '**** folder', '***** containers' }, '\n')))
  end)

  it('drops every childless editable field of the empty-node skeleton',
     function ()
    assert.are.same(sexpr.read('(skg (node (repo only)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** repo', '**** only',
        '*** writeProtected', '*** noClaim',
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

describe('skg.sexpr.metadata_fields repo defaults', function ()
  it('marks a matching repo value with (default)', function ()
    local headlines = filledIn_headlines(
      '(skg (node (id abc) (repo jeff)))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'jeff (default)'))
  end)

  it('leaves a non-matching repo value bare', function ()
    local headlines = filledIn_headlines(
      '(skg (node (id abc) (repo bob)))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'bob'))
    assert.is_nil(find_text(headlines, 'bob (default)'))
  end)

  it('inserts the default repo when missing', function ()
    local headlines =
      filledIn_headlines('(skg (node (id abc)))', 'jeff')
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
    local filledIn = fields.fillIn_defaults_in_org(
      '* skg\n** node', node_kind, 'jeff')
    local stripped = fields.strip_defaults_from_org(filledIn, node_kind)
    assert.are.same(sexpr.read('(skg (node (repo jeff)))'),
      bijection.org_to_sexp(stripped))
  end)
end)

describe('skg.sexpr.metadata_fields real-world metadata', function ()
  it('preserves an existing repo and appends non-canonical fields',
     function ()
    local headlines = filledIn_headlines(
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

  it('fills in real-world metadata with a herald field', function ()
    local headlines = filledIn_headlines(
      '(skg (node (id 6972d099) (repo public)'
      .. ' (rels "C5 4(1,1)L")))')
    assert.is_not_nil(find_text(headlines, 'repo'))
    assert.is_not_nil(find_text(headlines, 'public'))
    assert.is_not_nil(find_text(headlines, 'writeProtected'))
    assert.is_not_nil(find_text(headlines, 'noClaim'))
    assert.is_not_nil(find_text(headlines, 'editRequest'))
    assert.is_not_nil(find_text(headlines, 'rels'))
  end)
end)

describe('skg.sexpr.metadata_fields display title', function ()
  it('prepends the title group', function ()
    local headlines = filledIn_headlines(
      '(skg (node (id abc) (repo jeff)))', nil, 'actual title')
    assert.are.same(
      { { level = 1, text = 'title' },
        { level = 2, text = 'actual title' },
        { level = 1, text = 'skg' },
        { level = 2, text = 'node' } },
      vim.list_slice(headlines, 1, 4))
  end)

  it('does not prepend an empty title', function ()
    local headlines = filledIn_headlines(
      '(skg (node (id abc) (repo jeff)))', nil, '')
    assert.are.same({ level = 1, text = 'skg' }, headlines[1])
  end)

  it('is removed again by stripping', function ()
    local org_text = bijection.sexp_to_org(
      sexpr.read('(skg (node (id abc) (repo jeff)))'))
    local filledIn = fields.fillIn_defaults_in_org(
      org_text, node_kind, nil, 'actual title')
    local stripped = fields.strip_defaults_from_org(filledIn, node_kind)
    assert.are.same(sexpr.read('(skg (node (id abc) (repo jeff)))'),
      bijection.org_to_sexp(stripped))
  end)
end)

describe('skg.sexpr.metadata_fields other kinds, arguments, conditions', function ()
  it('fills in and strips a top-level field beside the atom', function ()
    local sexp = sexpr.read('(skg focused alias)')
    local kind = fields.kind_of_sexp(sexp)
    local filledIn = fields.fillIn_defaults_in_org(
      bijection.sexp_to_org(sexp), kind)
    assert.are.same(
      { { level = 1, text = 'skg' }, { level = 2, text = 'focused' },
        { level = 2, text = 'alias' }, { level = 2, text = 'editRequest' },
        { level = 3, text = 'none (default)' } },
      bijection.extract_headlines(filledIn))
    assert.are.same(sexp, bijection.org_to_sexp(
      fields.strip_defaults_from_org(filledIn, kind)))
  end)

  it('leaves a kind without fields alone', function ()
    local sexp = sexpr.read('(skg (deleted (id d) (repo r)))')
    local kind = fields.kind_of_sexp(sexp)
    local org_text = bijection.sexp_to_org(sexp)
    assert.are.equal(org_text,
      fields.fillIn_defaults_in_org(org_text, kind))
    assert.are.same(sexp, bijection.org_to_sexp(
      fields.strip_defaults_from_org(org_text, kind)))
  end)

  it('writes values with arguments as lists', function ()
    assert.are.same(
      sexpr.read('(skg (node (id abc) (editRequest (relRepo r))'
                 .. ' (viewRequests editableView (folder containers))))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '*** id', '**** abc',
        '*** editRequest', '**** relRepo r',
        '*** viewRequests', '**** editableView', '**** none (default)',
        '**** folder containers' }, '\n')))
    assert.are.same(sexpr.read('(skg (node (editRequest (merge x))))'),
      strip_to_sexp('* skg\n** node\n*** editRequest\n'
                    .. '**** merge [[id:x][a title]]'))
    assert.are.same(
      sexpr.read('(skg (node (editRequest (flag noSearchMatching false))))'),
      strip_to_sexp('* skg\n** node\n*** editRequest\n'
                    .. '**** flag noSearchMatching false'))
  end)

  it('honors the conditions on values', function ()
    assert.are.same({ 'none (default)', 'relRepo' },
      fields.cycle_values(node_kind, 'editRequest', false, false))
    assert.are.same(
      { 'none (default)', 'delete', 'merge', 'relRepo', 'flag',
        'restoreRel' },
      fields.cycle_values(node_kind, 'editRequest', true, true))
    assert.are.same({ 'false (default)', 'true' },
      fields.cycle_values(node_kind, 'writeProtected', false, false))
    assert.is_nil(fields.cycle_values(node_kind, 'repo', true, false))
  end)
end)
