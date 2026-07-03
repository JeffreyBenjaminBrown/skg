-- Mirrors tests/elisp/test-skg-git-add.el (the preview collects only
-- unstaged-new files, works from a body line, and touches no git
-- state) and test-skg-readable-ids.el (stale-generation dropping;
-- shortened ids without title annotations), plus goto_git's plain-diff
-- fallback against a real temporary git repo.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local config = require('skg.config')
local git_add = require('skg.git_add')
local goto_git = require('skg.goto_git')
local readable_ids = require('skg.readable_ids')

local function buffer_with (text)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, '\n'))
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  return buf
end

describe('skg.git_add', function ()
  local config_dir

  before_each(function ()
    config_dir = vim.fn.tempname()
    vim.fn.mkdir(config_dir .. '/main-dir', 'p')
    vim.fn.writefile({
      '[[sources]]', 'name = "main"', 'path = "main-dir"',
      'user_owns_it = true' }, config_dir .. '/skgconfig.toml')
    config.config_file_path = config_dir .. '/skgconfig.toml'
  end)

  after_each(function ()
    config.config_file_path = nil
    vim.fn.delete(config_dir, 'rf')
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('collects only unstaged-new files, from the body too', function ()
    buffer_with(table.concat({
      '* (skg (node (id root-id) (source main) (unstaged newX newM)))'
      .. ' root',
      'a body line',
      '** (skg (node (id staged-only) (source main) (staged newX)))'
      .. ' staged already',
      '** (skg (node (id member-only) (source main) (unstaged newM)))'
      .. ' membership only',
      '** (skg (node (id also-new) (source main) (unstaged newX)))'
      .. ' also new' }, '\n'))
    vim.api.nvim_win_set_cursor(0, { 2, 3 }) -- in the body
    local plan = git_add.git_add_new_files_recursive_plan()
    local names = {}
    for _, path in ipairs(plan.paths) do
      table.insert(names, vim.fn.fnamemodify(path, ':t')) end
    assert.are.same({ 'root-id.skg', 'also-new.skg' }, names)
    assert.is_truthy(plan.script:find('root-id.skg', 1, true))
    assert.is_truthy(plan.script:find('ls-files --error-unmatch',
                                      1, true))
  end)

  it('previews without touching git state', function ()
    buffer_with('* (skg (node (id n1) (source main)'
                .. ' (unstaged newX))) n1')
    git_add.git_add_if_new_recursive_preview()
    local preview = vim.api.nvim_get_current_buf()
    assert.are.equal('sh', vim.bo[preview].filetype)
    -- No .git directory was created anywhere by previewing.
    assert.are.equal(0,
      vim.fn.isdirectory(config_dir .. '/main-dir/.git'))
    vim.api.nvim_buf_delete(preview, { force = true })
  end)

  it('reports the empty plan honestly', function ()
    buffer_with('* (skg (node (id n1) (source main))) unchanged')
    local plan = git_add.git_add_new_files_recursive_plan()
    assert.are.same({}, plan.paths)
    assert.is_truthy(plan.script:find('No unstaged new skg files',
                                      1, true))
  end)
end)

describe('skg.readable_ids', function ()
  after_each(function ()
    helpers.reset_client_state()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  local uuid_a = '3861db2c-7f16-4814-b403-52b5e05d5e0a'
  local uuid_b = '557a869b-02ba-4c59-a5d3-5fb469a12353'

  it('conceals id tails even before titles arrive', function ()
    -- Mirrors "shorten inactive ids without title overlay": an id the
    -- server does not name still displays shortened.
    local server = helpers.connect_to_fake_server(
      function (line, respond)
        if line:find('titles by ids', 1, true) then
          respond(helpers.framed(string.format(
            '((response-type titles-by-ids)'
            .. ' (content ((%s . "known title"))))', uuid_a)))
        end
      end)
    local buf = buffer_with(uuid_a .. ' and ' .. uuid_b)
    readable_ids.enable(buf)
    vim.wait(2000, function ()
      local marks = vim.api.nvim_buf_get_extmarks(
        buf, readable_ids.namespace, 0, -1, { details = true })
      for _, mark in ipairs(marks) do
        if mark[4].virt_text then return true end
      end
      return false
    end, 10)
    local conceals, titles = 0, {}
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(
        buf, readable_ids.namespace, 0, -1, { details = true })) do
      if mark[4].conceal then conceals = conceals + 1 end
      if mark[4].virt_text then
        table.insert(titles, mark[4].virt_text[1][1]) end
    end
    assert.are.equal(2, conceals)      -- both ids shortened
    assert.are.same({ ' known title' }, titles) -- only one titled
    server.close()
  end)

  it('drops stale responses by generation', function ()
    -- Mirrors the stale-response-dropping test: bump the generation
    -- after the request; the late answer must not annotate.
    local respond_late = nil
    local server = helpers.connect_to_fake_server(
      function (line, respond)
        if line:find('titles by ids', 1, true) then
          respond_late = respond
        end
      end)
    local buf = buffer_with(uuid_a)
    readable_ids.enable(buf)
    vim.wait(1000, function () return respond_late ~= nil end, 10)
    -- A newer scan supersedes the outstanding one.
    vim.b[buf].skg_readable_ids_generation =
      vim.b[buf].skg_readable_ids_generation + 1
    respond_late(helpers.framed(string.format(
      '((response-type titles-by-ids)'
      .. ' (content ((%s . "stale title"))))', uuid_a)))
    vim.wait(300, function () return false end, 50)
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(
        buf, readable_ids.namespace, 0, -1, { details = true })) do
      assert.is_nil(mark[4].virt_text)
    end
    server.close()
  end)
end)

describe('skg.goto_git plain-diff fallback', function ()
  local repo_dir

  before_each(function ()
    repo_dir = vim.fn.tempname()
    vim.fn.mkdir(repo_dir, 'p')
    vim.fn.system({ 'git', '-C', repo_dir, 'init', '-q' })
    vim.fn.system({ 'git', '-C', repo_dir, 'config',
                    'user.email', 'test@test' })
    vim.fn.system({ 'git', '-C', repo_dir, 'config',
                    'user.name', 'test' })
    vim.fn.writefile({ 'title: parent', 'ids:', '- parent-id',
                       'contains:', '- child-one' },
                     repo_dir .. '/parent-id.skg')
    vim.fn.system({ 'git', '-C', repo_dir, 'add', '-A' })
    vim.fn.system({ 'git', '-C', repo_dir, 'commit', '-qm', 'x' })
    vim.fn.writefile({ 'title: parent', 'ids:', '- parent-id',
                       'contains:', '- child-one', '- child-two' },
                     repo_dir .. '/parent-id.skg')
  end)

  after_each(function ()
    vim.fn.delete(repo_dir, 'rf')
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_get_name(buf):find('skg://git%-diff/') then
        pcall(vim.api.nvim_buf_delete, buf, { force = true })
      end
    end
  end)

  it('opens the diff and lands on the searched id', function ()
    goto_git.open_plain_diff_at(repo_dir .. '/parent-id.skg',
                                'child-two')
    local buf = vim.api.nvim_get_current_buf()
    assert.are.equal('diff', vim.bo[buf].filetype)
    local cursor = vim.api.nvim_win_get_cursor(0)
    local line = vim.api.nvim_buf_get_lines(
      buf, cursor[1] - 1, cursor[1], false)[1]
    assert.is_truthy(line:find('child-two', 1, true))
  end)

  it('reports a changeless file rather than opening a buffer',
     function ()
    vim.fn.system({ 'git', '-C', repo_dir, 'checkout', '--', '.' })
    local notified = nil
    local original_notify = vim.notify
    vim.notify = function (msg) notified = msg end
    goto_git.open_plain_diff_at(repo_dir .. '/parent-id.skg', nil)
    vim.notify = original_notify
    assert.is_truthy(tostring(notified):find('no unstaged or staged'))
  end)
end)
