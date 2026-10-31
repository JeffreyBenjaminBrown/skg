-- Mirrors the config-reader coverage of
-- tests/elisp/test-skg-insert-heading-repo-prompt.el (the
-- interleaved-tables test and the reader helpers; the minibuffer
-- prompt tests live with the picker component instead).

local config = require('skg.config')

local interleaved_config = table.concat({
  '[[repo_sets]]',
  'name = "public-set"',
  'repos = ["public"]',
  '',
  '[[repos]]',
  'name = "public"',
  'path = "owned/public-dir"',
  '',
  '[[repo_sets]]',
  'name = "private-set"',
  'repos = ["private"]',
  '',
  '[[repos]]',
  'name = "private"',
  'path = "owned/private-dir"',
  '',
  '[[repos]]',
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
  end)

  after_each(function ()
    config.config_file_path = nil
    vim.fn.delete(config_dir, 'rf')
  end)

  it('reads the port', function ()
    assert.are.equal(1741, config.port_from_toml(config_file))
  end)

  it('errors when the port is missing', function ()
    local portless = config_dir .. '/portless.toml'
    vim.fn.writefile({ '[[repos]]', 'name = "a"' }, portless)
    assert.has_error(function () config.port_from_toml(portless) end)
  end)

  it('does not confuse [[repos]] and [[repo_sets]]', function ()
    assert.are.same({ 'public', 'private', 'foreign' },
                    config.repo_names())
    assert.are.same({ 'public', 'private', 'foreign' },
                    config.repo_set_names())
    local path_names = {}
    for _, entry in ipairs(config.repo_paths()) do
      table.insert(path_names, entry.name) end
    assert.are.same({ 'public', 'private', 'foreign' }, path_names)
  end)

  -- Mirrors test-skgrepo-restriction-label-marks-the-most-private-repo.
  it('marks the most private repo as no restriction', function ()
    assert.are.equal('public', config.skgrepo_restriction_label('public'))
    assert.are.equal('foreign (no restriction)',
                     config.skgrepo_restriction_label('foreign'))
    assert.are.equal('elsewhere',
                     config.skgrepo_restriction_label('elsewhere'))
    local repo_sets = require('skg.repo_sets')
    local sexpr = require('skg.sexpr.parse')
    local message = function (text)
      return repo_sets.skgrepo_restriction_message((sexpr.read(text))) end
    assert.are.equal('Skgrepo restriction: foreign (no restriction)',
      message('((response-type skgrepo-restriction) (restriction "foreign"))'))
    assert.are.equal('Skgrepo restriction: public',
      message('((response-type skgrepo-restriction) (restriction "public"))'))
    assert.are.equal('refused', message(
      '((response-type skgrepo-restriction) (restriction "public")'
      .. ' (content "refused"))'))
  end)

  -- Mirrors test-repo-set-prompt-completes-configured-repo-sets.
  it('prompts for the skgrepo restriction by the repos\' labels',
     function ()
    local picker = require('skg.picker')
    local real = picker.completing_read_with_cycle
    for label, name in pairs({ ['public'] = 'public',
                               ['foreign (no restriction)'] = 'foreign' }) do
      picker.completing_read_with_cycle = function (prompt, collection, opts)
        assert.is_truthy(prompt:find('Most private repo', 1, true))
        assert.are.same({ 'public', 'private', 'foreign (no restriction)' },
                        collection)
        assert.are.same(collection, opts.cycle_values)
        assert.are.equal('foreign (no restriction)', opts.initial_input)
        assert.is_true(opts.require_match)
        return label end
      local ok, chosen = pcall(picker.prompt_for_repo_set)
      picker.completing_read_with_cycle = real
      assert.is_true(ok, chosen)
      assert.are.equal(name, chosen)
    end
  end)

  it('lists only owned repos', function ()
    assert.are.same({ 'public', 'private' }, config.owned_repos())
  end)

  it('resolves relative repo paths against the config dir',
     function ()
    assert.are.equal(config_dir .. '/owned/public-dir',
                     config.repo_dir('public'))
    assert.are.equal(config_dir .. '/owned/private-dir',
                     config.repo_dir('private'))
  end)

  it('computes .skg paths from id and repo', function ()
    assert.are.equal(config_dir .. '/owned/public-dir/abc123.skg',
                     config.abs_path_for_id_and_repo(
                       'abc123', 'public'))
    assert.is_nil(config.abs_path_for_id_and_repo(
                    'abc123', 'nonexistent'))
  end)

  it('returns nil wrappers when no config is unrestricted', function ()
    config.config_file_path = nil
    assert.is_nil(config.repo_names())
    assert.is_nil(config.owned_repos())
    assert.is_nil(config.repo_set_names())
  end)
end)
