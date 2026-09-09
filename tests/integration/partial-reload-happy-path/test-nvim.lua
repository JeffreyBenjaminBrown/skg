-- Two-user fork/pull/subscribe-back/pull integration driver, Neovim client.
-- Each invocation performs one phase against the server selected by run-test.sh.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(170)

local content_view = require('skg.content_view')
local metadata = require('skg.metadata')
local pull = require('skg.pull')
local save = require('skg.save')
local state = require('skg.state')
local view_requests = require('skg.view_requests')

local work_root = assert(os.getenv('SKG_TEST_WORK_ROOT'))
local phase = assert(os.getenv('SKG_TEST_PHASE'))
local unlock_limit_seconds = 4

local function append_timing (label, seconds)
  local file = assert(io.open(assert(os.getenv('SKG_TEST_TIMINGS')), 'a'))
  file:write(string.format('%s\t%.3f seconds\n', label, seconds))
  file:close()
end

local function text_of (buf)
  return T.buffer_text(buf)
end

local function view_showing (id)
  local needle = '(id ' .. id .. ')'
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_view_uri ~= nil
       and text_of(buf):find(needle, 1, true) then
      return buf end
  end
  return nil
end

local function request_view (id, bypass_override)
  content_view.request_single_root_content_view_from_id(
    id, bypass_override, nil, nil, true)
  local buf = T.wait_for(function () return view_showing(id) end, 20)
  T.check(buf ~= nil, 'a fresh content view appeared for ' .. id)
  return buf
end

local function wait_for_response (scope)
  T.check(T.wait_for_response(30), scope .. ': server response settled')
end

local function line_containing (buf, needle)
  for index, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(needle, 1, true) then return index, line end
  end
  return nil
end

local function line_starting_with (buf, prefix)
  for index, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:sub(1, #prefix) == prefix then return index, line end
  end
  return nil
end

local function read_file (path)
  local file = assert(io.open(path, 'r'))
  local contents = file:read('*a')
  file:close()
  return contents
end

local function write_file (path, contents)
  local file = assert(io.open(path, 'w'))
  file:write(contents)
  file:close()
end

local function clone_id_path ()
  return work_root .. '/economics-of-china.id'
end

local function clone_id_from_file (path)
  for line in read_file(path):gmatch('[^\r\n]+') do
    local id = line:match('^pid:%s*(.-)%s*$')
    if id and id ~= '' then return id end
  end
  return nil
end

local function read_clone_id ()
  local path = clone_id_path()
  T.check(vim.fn.filereadable(path) == 1,
          'the clone-id handoff file exists')
  return vim.trim(read_file(path))
end

local function git (scope, repository, arguments)
  local result = vim.system(
    vim.list_extend({ 'git', '-C', repository }, arguments),
    { text = true }):wait()
  T.check(result.code == 0,
          string.format('%s: git %s succeeded: %s', scope,
                        table.concat(arguments, ' '), result.stderr or ''))
end

local function commit_and_push (scope, repository, message)
  git(scope, repository, { 'add', '-A' })
  git(scope, repository, { 'commit', '-m', message })
  git(scope, repository, { 'push' })
end

local function pull_and_measure_unlock (label, buf)
  local started = vim.uv.hrtime()
  T.check(pull.pull_all(), label .. ': pull maintenance started')
  T.check(T.wait_for(function ()
    return vim.b[buf].skg_maintenance_epoch ~= nil
  end, 20), label .. ': target view was maintenance-locked')
  T.check(T.wait_for(function ()
    return state.maintenance_client_incident == nil
      and vim.api.nvim_buf_is_valid(buf)
      and vim.b[buf].skg_maintenance_epoch == nil
  end, 100), label .. ': target view unlocked after maintenance settled')
  local elapsed = (vim.uv.hrtime() - started) / 1000000000
  append_timing(label, elapsed)
  T.check(elapsed <= unlock_limit_seconds,
          string.format('%s: pull-to-unlock was %.3f seconds (limit %.1f)',
                        label, elapsed, unlock_limit_seconds))
end

local function first_content_id (buf)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    local id = line:match('^%*%* %(skg %(node %(id ([^ )]+)')
    if id then return id end
  end
  return nil
end

local function economist_first ()
  local repository = work_root .. '/economist-client/owned/economist-public'
  local foreign_view = request_view('chinese-economics', true)
  local line_number, old_line = line_containing(foreign_view, 'Chinese Economics')
  T.check(line_number ~= nil, 'the foreign title is present before its fork')
  vim.api.nvim_set_current_buf(foreign_view)
  vim.api.nvim_buf_set_lines(foreign_view, line_number - 1, line_number, false,
                             { (old_line:gsub('Chinese Economics', 'Economics of China')) })
  save.request_save_buffer()

  local confirmation = T.wait_for_buffer('skg://fork-confirmation', 20)
  T.check(confirmation ~= nil, 'the fork confirmation appeared')
  local clone_line = line_starting_with(confirmation, '* (skg (node (source ')
  T.check(clone_line ~= nil, 'the confirmation contains a clone headline')
  vim.api.nvim_set_current_buf(confirmation)
  metadata.change_source_at_line(clone_line, 'economist-public')
  save.approve_fork()
  wait_for_response('economist fork approval')

  local clone_file = T.wait_for(function ()
    local files = vim.fn.glob(repository .. '/*.skg', false, true)
    for _, path in ipairs(files) do
      if vim.fn.fnamemodify(path, ':t') ~= 'economics.skg' then return path end
    end
    return nil
  end, 20)
  T.check(clone_file ~= nil, 'one clone file was written to Economist owned data')
  local clone_id = clone_id_from_file(clone_file)
  T.check(clone_id ~= nil, 'the clone file contains a pid')
  write_file(clone_id_path(), clone_id .. '\n')
  local clone_text = read_file(clone_file)
  T.check(clone_text:find('title: Economics of China', 1, true)
            and clone_text:find('subscribes_to:', 1, true)
            and clone_text:find('overrides_view_of:', 1, true),
          'the clone has its title, subscription, and override bytes')

  local economics_view = request_view('economics')
  vim.api.nvim_set_current_buf(economics_view)
  vim.api.nvim_buf_set_lines(economics_view, -1, -1, false, {
    string.format('** (skg (node (id %s) (source economist-public) indef)) Economics of China',
                  clone_id),
  })
  save.request_save_buffer()
  wait_for_response('economist contains clone')
  T.check(text_of(economics_view):find('(id ' .. clone_id .. ')', 1, true),
          'Economics contains the clone as its first content node')
  commit_and_push('economist push', repository,
                  'Fork Chinese Economics into Economics')
end

local function china_scholar ()
  local repository = work_root
    .. '/china-scholar-client/owned/china-scholar-public'
  local clone_id = read_clone_id()
  local china_view = request_view('china')
  T.check(text_of(china_view):find('(id chinese-economics)', 1, true),
          'China initially contains its local Chinese Economics node')
  pull_and_measure_unlock('china-scholar-pull', china_view)

  local original_view = request_view('chinese-economics', true)
  T.check(text_of(original_view):find('(subscribes (in 1', 1, true),
          'Chinese Economics reports Economist as an inbound subscriber')
  vim.api.nvim_set_current_buf(original_view)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  view_requests.show_collection_subscribes()
  wait_for_response('china-scholar show subscriptions')
  T.check(text_of(original_view):find('subscriberCol', 1, true)
            and text_of(original_view):find('(id ' .. clone_id .. ')', 1, true),
          'the subscriber collection contains the Economist clone')

  local subscribee_line = line_containing(original_view, '(skg subscribeeCol)')
  T.check(subscribee_line ~= nil, 'a writable subscribee collection appeared')
  vim.api.nvim_set_current_buf(original_view)
  vim.api.nvim_buf_set_lines(original_view, subscribee_line, subscribee_line, false, {
    string.format('*** (skg (node (id %s) (source economist-public) indef)) Economics of China',
                  clone_id),
  })
  save.request_save_buffer()
  wait_for_response('china-scholar subscribes back')
  local chinese_text = read_file(repository .. '/chinese-economics.skg')
  T.check(chinese_text:find('subscribes_to:', 1, true)
            and chinese_text:find(clone_id, 1, true),
          'Chinese Economics persisted its subscription to the clone')
  commit_and_push('china-scholar push', repository,
                  'Subscribe Chinese Economics back to Economist\'s fork')
end

local function economist_final ()
  local clone_id = read_clone_id()
  local economics_view = request_view('economics')
  T.check(first_content_id(economics_view) == clone_id,
          'Economics has the clone as its first content node before pull')
  pull_and_measure_unlock('economist-pull', economics_view)
  local clone_view = request_view(clone_id)
  T.check(text_of(clone_view):find('Economics of China', 1, true),
          'the fresh clone view has its fork title')
  T.check(text_of(clone_view):find('(subscribes (in 1', 1, true),
          'the fresh clone view reports China-Scholar\'s inbound subscription')
end

if phase == 'economist-first' then
  economist_first()
elseif phase == 'china-scholar' then
  china_scholar()
elseif phase == 'economist-final' then
  economist_final()
else
  T.fail('unknown happy-path phase: ' .. phase)
end

T.pass('PASS: Neovim phase completed: ' .. phase)
