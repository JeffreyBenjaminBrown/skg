-- Coverage for skg.diff_view against the loopback fake server:
-- the one command requests the four buffers in order and opens only
-- the nonempty ones. Mirrors tests/elisp/test-skg-diff-view.el.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local diff_view = require('skg.diff_view')

describe('skg.diff_view', function ()
  local server

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
  end)

  after_each(function ()
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
  end)

  it('carries the buffer and the view-id', function ()
    assert.are.equal(
      '((request . "diff view") (buffer . "all changes")'
      .. ' (view-id . "view_id-1"))\n',
      diff_view.request_string('all changes', 'view_id-1'))
  end)

  it('requests every buffer in order and opens only the nonempty ones',
     function ()
    local requested = {}
    local answers = {
      ['all changes'] = '* (skg node (gnid a)) a',
      ['skgrepo changes'] = '* (skg node (gnid b)) b' }
    server = helpers.connect_to_fake_server(function (line, respond)
      local name = line:match('%(buffer %. "([^"]*)"%)')
      if name then
        table.insert(requested, name)
        if answers[name] then
          respond(helpers.framed(
            '((response-type diff-view) (content "'
            .. answers[name] .. '") (errors ()) (warnings ()))'))
        else
          respond(helpers.framed(
            '((response-type diff-view) (empty . t)'
            .. ' (errors ()) (warnings ()))')) end end
    end)
    diff_view.diffView_open()
    vim.wait(3000, function () return #requested == 4 end, 10)
    vim.wait(200, function () return false end, 10)
    assert.are.same(diff_view.buffers, requested)
    local opened = {}
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      local name = vim.api.nvim_buf_get_name(buf)
      if name:find('skg://diff-view: ', 1, true) then
        table.insert(opened, name) end end
    table.sort(opened)
    assert.are.same({ 'skg://diff-view: all changes',
                      'skg://diff-view: skgrepo changes' }, opened)
  end)
end)
