-- Mirrors tests/elisp/test-skg-metadata-fields.el.

local sexpr = require('skg.sexpr.parse')
local bijection = require('skg.sexpr.org_bijection')
local fields = require('skg.sexpr.metadata_fields')
local shared = require('skg.shared')

---The occurrence's kind from 'shared/metadata-fields.json'.
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
        ['(skg node (gnid abc) (skgRepos (nodeRepo jeff)))'] = 'occurrence',
        ['(skg focused node (gnid abc))'] = 'occurrence',
        ['(skg ghost (gnid g) (skgRepos (nodeRepo r)))'] = 'ghost',
        ['(skg alias (skgRepos (relRepo r)))'] = 'alias',
        ['(skg (flag noSearchMatching))'] = 'flag',
        ['(skg subscribeeFolder)'] = 'folder',
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
    assert.are.equal('* skg\n** node\n** gnid\n*** abc',
      fields.headlines_to_org({
        { level = 1, text = 'skg' }, { level = 2, text = 'node' },
        { level = 2, text = 'gnid' }, { level = 3, text = 'abc' } }))
  end)
end)

describe('skg.sexpr.metadata_fields filling in', function ()
  it('inserts all default fields into a minimal occurrence', function ()
    local headlines =
      filledIn_headlines('(skg node (gnid abc) (skgRepos (nodeRepo jeff)))')
    -- skg, node, gnid/abc, skgRepos/nodeRepo/jeff, writeProtected/false,
    -- noClaim/false, editRequest/none, viewRequests/none:
    -- each key and each value is a separate headline.
    assert.are.equal(15, #headlines)
    assert.is_not_nil(find_text(headlines, 'writeProtected'))
    assert.is_not_nil(find_text(headlines, 'false (default)'))
    assert.is_not_nil(find_text(headlines, 'editRequest'))
    assert.is_not_nil(find_text(headlines, 'none (default)'))
  end)

  it('shows a true child for a bare writeProtected', function ()
    local headlines =
      filledIn_headlines('(skg node (gnid abc) (skgRepos (nodeRepo jeff)) writeProtected)')
    local write_protected_index = find_text(headlines, 'writeProtected')
    assert.is_not_nil(write_protected_index)
    assert.are.equal('true', headlines[write_protected_index + 1].text)
  end)

  it('offers noClaim as a bare atom', function ()
    for sexp_text, value in pairs({
        ['(skg node (gnid abc) (skgRepos (nodeRepo jeff)))'] = 'false (default)',
        ['(skg node (gnid abc) (skgRepos (nodeRepo jeff)) noClaim)'] = 'true' }) do
      local headlines = filledIn_headlines(sexp_text)
      local noClaim_index = find_text(headlines, 'noClaim')
      assert.is_not_nil(noClaim_index)
      assert.are.equal(value, headlines[noClaim_index + 1].text)
    end
  end)

  it('orders fields canonically', function ()
    local headlines = filledIn_headlines(
      '(skg node (skgRepos (nodeRepo jeff)) (view cycle) (gnid abc) writeProtected)')
    local level_2 = {}
    for _, headline in ipairs(headlines) do
      if headline.level == 2 then
        table.insert(level_2, headline.text) end
    end
    assert.are.same(
      { 'node', 'gnid', 'skgRepos', 'writeProtected', 'noClaim',
        'editRequest', 'viewRequests', 'view' },
      level_2)
  end)
end)

describe('skg.sexpr.metadata_fields stripping', function ()
  it('is identity on unmodified filled-in text', function ()
    assert.are.same(sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff)))'),
      round_trip('(skg node (gnid abc) (skgRepos (nodeRepo jeff)))'))
  end)

  it('collapses writeProtected=true to a bare atom', function ()
    assert.are.same(
      sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff)) writeProtected)'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** gnid', '*** abc',
        '** skgRepos', '*** nodeRepo', '**** jeff',
        '** writeProtected', '*** true',
        '** noClaim', '*** false (default)',
        '** editRequest', '*** none (default)',
        '** viewRequests', '*** none (default)' }, '\n')))
  end)

  it('accepts bare default spellings without (default)', function ()
    assert.are.same(sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** gnid', '*** abc',
        '** skgRepos', '*** nodeRepo', '**** jeff',
        '** writeProtected', '*** false',
        '** noClaim', '*** false',
        '** editRequest', '*** none',
        '** viewRequests', '*** none' }, '\n')))
  end)

  it('round-trips a bare writeProtected', function ()
    assert.are.same(
      sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff)) writeProtected)'),
      round_trip('(skg node (gnid abc) (skgRepos (nodeRepo jeff)) writeProtected)'))
  end)

  it('round-trips (editRequest delete)', function ()
    assert.are.same(
      sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff))'
                 .. ' (editRequest delete))'),
      round_trip('(skg node (gnid abc) (skgRepos (nodeRepo jeff))'
                 .. ' (editRequest delete))'))
  end)

  it('round-trips (editRequest (merge XYZ))', function ()
    assert.are.same(
      sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff))'
                 .. ' (editRequest (merge XYZ)))'),
      round_trip('(skg node (gnid abc) (skgRepos (nodeRepo jeff))'
                 .. ' (editRequest (merge XYZ)))'))
  end)

  it('extracts the id from a merge org link', function ()
    assert.are.same(
      sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff))'
                 .. ' (editRequest (merge XYZ)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** gnid', '*** abc',
        '** skgRepos', '*** nodeRepo', '**** jeff',
        '** editRequest', '*** merge [[id:XYZ][some label]]' },
        '\n')))
  end)

  it('collapses noClaim=true to a bare atom', function ()
    assert.are.same(
      sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff))'
                 .. ' noClaim)'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** gnid', '*** abc',
        '** skgRepos', '*** nodeRepo', '**** jeff',
        '** noClaim', '*** true' }, '\n')))
  end)

  it('keeps populated viewRequests', function ()
    assert.are.same(
      sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff))'
                 .. ' (viewRequests (folder aliases) (folder containers)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** gnid', '*** abc',
        '** skgRepos', '*** nodeRepo', '**** jeff',
        '** viewRequests', '*** folder', '**** aliases',
        '*** folder', '**** containers' }, '\n')))
  end)

  it('drops every childless editable field of the empty-node skeleton',
     function ()
    assert.are.same(sexpr.read('(skg node (skgRepos (nodeRepo only)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** skgRepos', '*** nodeRepo', '**** only',
        '** writeProtected', '** noClaim',
        '** editRequest', '** viewRequests' }, '\n')))
  end)

  it('drops a childless viewRequests rather than keeping a bare atom',
     function ()
    assert.are.same(sexpr.read('(skg node (skgRepos (nodeRepo only)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** skgRepos', '*** nodeRepo', '**** only',
        '** viewRequests' }, '\n')))
  end)
end)

describe('skg.sexpr.metadata_fields repo defaults', function ()
  it('marks a matching repo value with (default)', function ()
    local headlines = filledIn_headlines(
      '(skg node (gnid abc) (skgRepos (nodeRepo jeff)))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'jeff (default)'))
  end)

  it('leaves a non-matching repo value bare', function ()
    local headlines = filledIn_headlines(
      '(skg node (gnid abc) (skgRepos (nodeRepo bob)))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'bob'))
    assert.is_nil(find_text(headlines, 'bob (default)'))
  end)

  it('inserts the default repo when missing', function ()
    local headlines =
      filledIn_headlines('(skg node (gnid abc))', 'jeff')
    assert.is_not_nil(find_text(headlines, 'nodeRepo'))
    assert.is_not_nil(find_text(headlines, 'jeff (default)'))
  end)

  it('strips the (default) suffix from a repo value', function ()
    assert.are.same(sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** gnid', '*** abc',
        '** skgRepos', '*** nodeRepo', '**** jeff (default)' }, '\n')))
  end)

  it('keeps a bare repo value as-is', function ()
    assert.are.same(sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo bob)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** gnid', '*** abc',
        '** skgRepos', '*** nodeRepo', '**** bob' }, '\n')))
  end)

  it('round-trips with a default repo', function ()
    assert.are.same(sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff)))'),
      round_trip('(skg node (gnid abc) (skgRepos (nodeRepo jeff)))', 'jeff'))
  end)

  it('round-trips a new node with no repo', function ()
    local filledIn = fields.fillIn_defaults_in_org(
      '* skg\n** node', node_kind, 'jeff')
    local stripped = fields.strip_defaults_from_org(filledIn, node_kind)
    assert.are.same(sexpr.read('(skg node (skgRepos (nodeRepo jeff)))'),
      bijection.org_to_sexp(stripped))
  end)
end)

describe('skg.sexpr.metadata_fields real-world metadata', function ()
  it('preserves an existing repo and appends non-canonical fields',
     function ()
    local headlines = filledIn_headlines(
      '(skg node (gnid abc) (skgRepos (nodeRepo public)) (view (rels "C5")))')
    assert.is_not_nil(find_text(headlines, 'nodeRepo'))
    assert.is_not_nil(find_text(headlines, 'public'))
    assert.is_not_nil(find_text(headlines, 'gnid'))
    assert.is_not_nil(find_text(headlines, 'abc'))
    assert.is_not_nil(find_text(headlines, 'rels'))
  end)

  it('round-trips id + repo', function ()
    assert.are.same(
      sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo public)))'),
      round_trip('(skg node (gnid abc) (skgRepos (nodeRepo public)))'))
  end)

  it('fills in real-world metadata with a herald field', function ()
    local headlines = filledIn_headlines(
      '(skg node (gnid 6972d099) (skgRepos (nodeRepo public))'
      .. ' (view (rels "C5 4(1,1)L")))')
    assert.is_not_nil(find_text(headlines, 'nodeRepo'))
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
      '(skg node (gnid abc) (skgRepos (nodeRepo jeff)))', nil, 'actual title')
    assert.are.same(
      { { level = 1, text = 'title' },
        { level = 2, text = 'actual title' },
        { level = 1, text = 'skg' },
        { level = 2, text = 'node' } },
      vim.list_slice(headlines, 1, 4))
  end)

  it('does not prepend an empty title', function ()
    local headlines = filledIn_headlines(
      '(skg node (gnid abc) (skgRepos (nodeRepo jeff)))', nil, '')
    assert.are.same({ level = 1, text = 'skg' }, headlines[1])
  end)

  it('is removed again by stripping', function ()
    local org_text = bijection.sexp_to_org(
      sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff)))'))
    local filledIn = fields.fillIn_defaults_in_org(
      org_text, node_kind, nil, 'actual title')
    local stripped = fields.strip_defaults_from_org(filledIn, node_kind)
    assert.are.same(sexpr.read('(skg node (gnid abc) (skgRepos (nodeRepo jeff)))'),
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
        { level = 2, text = 'alias' }, { level = 2, text = 'noClaim' },
        { level = 3, text = 'false (default)' },
        { level = 2, text = 'editRequest' },
        { level = 3, text = 'none (default)' } },
      bijection.extract_headlines(filledIn))
    assert.are.same(sexp, bijection.org_to_sexp(
      fields.strip_defaults_from_org(filledIn, kind)))
  end)

  it('keeps a readiness request beside the other edit request',
     function ()
    -- Alone, it is shown beside the other request at its default.
    local sexp = sexpr.read('(skg unknown (gnid x) (editRequest (ready rel)))')
    local kind = fields.kind_of_sexp(sexp)
    local filledIn = fields.fillIn_defaults_in_org(
      bijection.sexp_to_org(sexp), kind)
    assert.are.same(
      { { level = 1, text = 'skg' }, { level = 2, text = 'unknown' },
        { level = 2, text = 'gnid' }, { level = 3, text = 'x' },
        { level = 2, text = 'noClaim' },
        { level = 3, text = 'false (default)' },
        { level = 2, text = 'editRequest' },
        { level = 3, text = 'none (default)' },
        { level = 3, text = 'ready' }, { level = 4, text = 'rel' } },
      bijection.extract_headlines(filledIn))
    assert.are.same(sexp, bijection.org_to_sexp(
      fields.strip_defaults_from_org(filledIn, kind)))
    -- Beside another request, and when that request is chosen.
    local both = sexpr.read('(skg node (editRequest delete (unready node)))')
    assert.are.same(both, bijection.org_to_sexp(
      fields.strip_defaults_from_org(fields.fillIn_defaults_in_org(
        bijection.sexp_to_org(both), node_kind), node_kind)))
    assert.are.same(
      sexpr.read('(skg node (editRequest (relRepo r) (ready rel)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** editRequest', '*** relRepo r',
        '*** ready', '**** rel' }, '\n')))
  end)

  it('leaves a kind without fields alone', function ()
    local sexp = sexpr.read('(skg (flag noSearchMatching))')
    local kind = fields.kind_of_sexp(sexp)
    local org_text = bijection.sexp_to_org(sexp)
    assert.are.equal(org_text,
      fields.fillIn_defaults_in_org(org_text, kind))
    assert.are.same(sexp, bijection.org_to_sexp(
      fields.strip_defaults_from_org(org_text, kind)))
  end)

  it('writes values with arguments as lists', function ()
    assert.are.same(
      sexpr.read('(skg node (gnid abc) (editRequest (relRepo r))'
                 .. ' (viewRequests editableView (folder containers)))'),
      strip_to_sexp(table.concat({
        '* skg', '** node', '** gnid', '*** abc',
        '** editRequest', '*** relRepo r',
        '** viewRequests', '*** editableView', '*** none (default)',
        '*** folder containers' }, '\n')))
    assert.are.same(sexpr.read('(skg node (editRequest (merge x)))'),
      strip_to_sexp('* skg\n** node\n** editRequest\n'
                    .. '*** merge [[id:x][a title]]'))
    assert.are.same(
      sexpr.read('(skg node (editRequest (flag noSearchMatching false)))'),
      strip_to_sexp('* skg\n** node\n** editRequest\n'
                    .. '*** flag noSearchMatching false'))
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
    assert.is_nil(fields.cycle_values(node_kind, 'nodeRepo', true, false))
  end)
end)
