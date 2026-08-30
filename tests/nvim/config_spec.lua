-- Mirrors the config-reader coverage of
-- tests/elisp/test-skg-insert-heading-source-prompt.el (the
-- interleaved-tables test and the reader helpers; the minibuffer
-- prompt tests live with the picker component instead).

local config = require('skg.config')

local interleaved_config = table.concat({
  '[[source_sets]]',
  'name = "public-set"',
  'sources = ["public"]',
  '',
  '[[sources]]',
  'name = "public"',
  'path = "owned/public-dir"',
  '',
  '[[source_sets]]',
  'name = "private-set"',
  'sources = ["private"]',
  '',
  '[[sources]]',
  'name = "private"',
  'path = "owned/private-dir"',
  '',
  '[[sources]]',
  'name = "foreign"',
  'path = "foreign-dir"',
  '',
  'port = 1741',
}, '\n')

describe('skg.config', function ()
  local config_dir, config_file

  before_each(function ()
    config_dir = vim.fn.tempname()
    vim.fn.mkdir(config_dir, 'p')
    config_file = config_dir .. '/skgconfig.toml'
    vim.fn.writefile(vim.split(interleaved_config, '\n'), config_file)
    config.config_file_path = config_file
    config.source_inventory = nil
  end)

  after_each(function ()
    config.config_file_path = nil
    config.source_inventory = nil
    vim.fn.delete(config_dir, 'rf')
  end)

  it('reads the port', function ()
    assert.are.equal(1741, config.port_from_toml(config_file))
  end)

  it('errors when the port is missing', function ()
    local portless = config_dir .. '/portless.toml'
    vim.fn.writefile({ '[[sources]]', 'name = "a"' }, portless)
    assert.has_error(function () config.port_from_toml(portless) end)
  end)

  it('does not confuse [[sources]] and [[source_sets]]', function ()
    assert.are.same({ 'public', 'private', 'foreign' },
                    config.source_names())
    assert.are.same({ 'public', 'private', 'foreign', 'all' },
                    config.source_set_names())
    local path_names = {}
    for _, entry in ipairs(config.source_paths()) do
      table.insert(path_names, entry.name) end
    assert.are.same({ 'public', 'private', 'foreign' }, path_names)
  end)

  it('defaults an unnamed source name to its path', function ()
    local unnamed = config_dir .. '/unnamed.toml'
    vim.fn.writefile({
      '[[sources]]',
      'path = "owned/unnamed"',
      '',
      '[[sources]]',
      'name = "named"',
      'path = "owned/named"',
    }, unnamed)
    assert.are.same({ 'owned/unnamed', 'named' },
                    config.source_names_from_toml(unnamed))
    local paths = config.source_paths_from_toml(unnamed)
    assert.are.equal('owned/unnamed', paths[1].name)
    assert.are.same({ 'owned/unnamed', 'named' },
                    config.owned_sources_from_toml(unnamed))
  end)

  it('uses the server inventory after connection verification', function ()
    local parse = require('skg.sexpr.parse')
    local response = parse.read(
      '((source-inventory (((name public) (abbreviation pub)'
      .. ' (owned true) (position 0) (configured-path owned/public)'
      .. ' (directory /real/public) (directory-identity /real/public))'
      .. ' ((name foreign) (abbreviation nil) (owned nil) (position 1)'
      .. ' (configured-path ../foreign) (directory /real/foreign)'
      .. ' (directory-identity /real/foreign)))))')
    config.install_source_inventory(
      require('skg.payload').field(response, 'source-inventory'))
    assert.are.same({ 'public', 'foreign' }, config.source_names())
    assert.are.same({ 'public' }, config.owned_sources())
    assert.are.same({
      { name = 'public', path = '/real/public' },
      { name = 'foreign', path = '/real/foreign' },
    }, config.source_paths())
  end)

  it('shows handshake telescope warnings in a persistent buffer', function ()
    local messages = require('skg.messages')
    local original = messages.big_nonfatal_message
    local shown = nil
    messages.big_nonfatal_message = function (name, message, content)
      shown = { name, message, content } end
    local response = require('skg.sexpr.parse').read(
      '((telescope-warnings (((pid X)'
      .. ' (kind ignored-foreign-pid-collision)'
      .. ' (message "owned telescope won")'
      .. ' (winning-paths (/data/owned/X.skg))'
      .. ' (ignored-paths (/data/foreign/X.skg))))))')
    require('skg.misc_requests').show_handshake_telescope_warnings(response)
    messages.big_nonfatal_message = original
    assert.are.equal('skg://messages/telescope-warnings', shown[1])
    assert.matches('^WARNING:', shown[2])
    assert.matches('retained owned files', shown[3])
    assert.matches('/data/foreign/X.skg', shown[3], 1, true)
  end)

  it('lists only owned sources', function ()
    assert.are.same({ 'public', 'private' }, config.owned_sources())
  end)

  it('resolves relative source paths against the config dir',
     function ()
    assert.are.equal(config_dir .. '/owned/public-dir',
                     config.source_dir('public'))
    assert.are.equal(config_dir .. '/owned/private-dir',
                     config.source_dir('private'))
  end)

  it('computes .skg paths from id and source', function ()
    assert.are.equal(config_dir .. '/owned/public-dir/abc123.skg',
                     config.abs_path_for_id_and_source(
                       'abc123', 'public'))
    assert.is_nil(config.abs_path_for_id_and_source(
                    'abc123', 'nonexistent'))
  end)

  it('returns nil wrappers when no config is active', function ()
    config.config_file_path = nil
    assert.is_nil(config.source_names())
    assert.is_nil(config.owned_sources())
    assert.is_nil(config.source_set_names())
  end)
end)
