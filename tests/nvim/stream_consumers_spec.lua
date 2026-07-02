-- Coverage for the rerender stream and its consumers (diff mode,
-- source sets), plus the report-shaped requests (diff analysis, stage
-- moves, export). Mirrors tests/elisp/test-skg-diff-analysis.el's
-- wiring cases and test-skg-warning-channel.el's rerender-done
-- channel, end-to-end through the loopback fake server.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local diff_analysis = require('skg.diff_analysis')
local diff_mode = require('skg.diff_mode')
local lock = require('skg.lock')
local rerender = require('skg.rerender')
local source_sets = require('skg.source_sets')
local stage_moves = require('skg.stage_moves')

local function wipe_named (name)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buf) == name then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end
end

describe('skg stream consumers', function ()
  local server

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  after_each(function ()
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
    lock.end_stream()
    wipe_named('skg://messages/rerender')
    wipe_named('skg://diff-analysis')
    wipe_named('skg://stage-moves')
  end)

  it('drives the rerender stream: lock, per-view update, done with'
     .. ' warnings', function ()
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('rerender all views', 1, true) then
        respond(helpers.framed(
          '((response-type rerender-lock)'
          .. ' (lock-views (uri-a)))'))
        respond(helpers.framed(
          '((response-type rerender-view) (view-uri uri-a)'
          .. ' (content "* (skg (node (id a))) a rerendered"))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ())'
          .. ' (warnings ("stale herald")))'))
      end
    end)
    local a = buffer.open_org_buffer_from_text(
      '* (skg (node (id a))) a', 'skg://a', 'uri-a')
    local b = buffer.open_org_buffer_from_text(
      '* (skg (node (id b))) b', 'skg://b', 'uri-b')
    rerender.request_rerender_all_views()
    -- Locked immediately; b unlocks on rerender-lock (not listed).
    assert.is_false(vim.bo[a].modifiable)
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10)
    assert.is_true(vim.bo[a].modifiable)
    assert.is_true(vim.bo[b].modifiable)
    assert.are.equal('* (skg (node (id a))) a rerendered',
      vim.api.nvim_buf_get_lines(a, 0, 1, false)[1])
    -- The warning channel fired (mirrors test-skg-warning-channel).
    local found = nil
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_get_name(buf)
         == 'skg://messages/rerender' then found = buf end
    end
    assert.is_truthy(found)
    assert.is_truthy(table.concat(
      vim.api.nvim_buf_get_lines(found, 0, -1, false), '\n')
      :find('stale herald', 1, true))
  end)

  it('diff-mode toggle refuses on unsaved buffers, else acks and'
     .. ' rerenders', function ()
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('git diff mode toggle', 1, true) then
        respond(helpers.framed(
          '((response-type git-diff-mode)'
          .. ' (content "Git diff mode enabled"))'))
        respond(helpers.framed(
          '((response-type rerender-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ())'
          .. ' (warnings ()))'))
      end
    end)
    local a = buffer.open_org_buffer_from_text(
      '* (skg (node (id a))) a', 'skg://a', 'uri-a')
    vim.bo[a].modified = true
    local ok, err = pcall(diff_mode.toggle)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('unsaved skg buffer'))
    vim.bo[a].modified = false
    local notified = {}
    local original_notify = vim.notify
    vim.notify = function (msg) table.insert(notified, msg) end
    diff_mode.toggle()
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10)
    vim.notify = original_notify
    local acked = false
    for _, msg in ipairs(notified) do
      if tostring(msg):find('Git diff mode enabled', 1, true) then
        acked = true end
    end
    assert.is_true(acked)
    assert.is_true(vim.bo[a].modifiable)
  end)

  it('switches the source-set and rides the rerender stream',
     function ()
    local seen = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('set active source set', 1, true) then
        seen = line
        respond(helpers.framed(
          '((response-type active-source-set) (active "public")'
          .. ' (content "Active source-set: public"))'))
        respond(helpers.framed(
          '((response-type rerender-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ())'
          .. ' (warnings ()))'))
      end
    end)
    source_sets.set_active_source_set('public')
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil and seen ~= nil end, 10)
    assert.are.equal(
      '((request . "set active source set") (name . "public"))',
      seen)
  end)

  it('shows the diff-analysis report with the navigation keymap',
     function ()
    -- Mirrors test-skg-diff-analysis.el's wiring: stage flags on the
    -- wire, report buffer, keymap attached.
    local seen = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('diff analysis', 1, true) then
        seen = line
        respond(helpers.framed(
          '((response-type diff-analysis)'
          .. ' (content "* changed nodes\\n** some node")'
          .. ' (errors ()) (warnings ("one warning")))'))
      end
    end)
    diff_analysis.diff_report(true, false)
    vim.wait(3000, function () return seen ~= nil end, 10)
    assert.is_truthy(seen:find('(include-staged . "true")', 1, true))
    assert.is_truthy(seen:find('(include-unstaged . "false")', 1,
                               true))
    local report = nil
    vim.wait(2000, function ()
      for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_get_name(buf) == 'skg://diff-analysis'
        then report = buf end
      end
      return report ~= nil
    end, 10)
    assert.is_truthy(report)
    assert.is_truthy(table.concat(
      vim.api.nvim_buf_get_lines(report, 0, -1, false), '\n')
      :find('changed nodes', 1, true))
    -- The navigation subset is attached buffer-locally.
    local has_goto = false
    for _, map in ipairs(vim.api.nvim_buf_get_keymap(report, 'n')) do
      if map.desc and map.desc:find('Open a view') then
        has_goto = true end
    end
    assert.is_true(has_goto)
  end)

  it('shows the stage-moves script as a shell buffer', function ()
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('stage moves', 1, true) then
        respond(helpers.framed(
          '((response-type stage-moves)'
          .. ' (content "# No moves detected.") (errors ())'
          .. ' (warnings ()))'))
      end
    end)
    stage_moves.stage_moves()
    local script = nil
    vim.wait(3000, function ()
      for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_get_name(buf) == 'skg://stage-moves' then
          script = buf end
      end
      return script ~= nil
    end, 10)
    assert.is_truthy(script)
    assert.are.equal('sh', vim.bo[script].filetype)
    assert.are.equal('# No moves detected.',
      vim.api.nvim_buf_get_lines(script, 0, 1, false)[1])
  end)
end)
