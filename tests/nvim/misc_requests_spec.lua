-- Coverage for skg.misc_requests' rebuild of the session-lifetime
-- stores, which also reloads the server's config. Mirrors
-- tests/elisp/test-skg-rebuild-session-lifetime-stores.el.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local misc_requests = require('skg.misc_requests')

describe('skg.misc_requests rebuild_session_lifetime_stores', function ()
  local server
  local real_notify

  before_each(function ()
    real_notify = vim.notify
    vim.notify = function () end
  end)

  after_each(function ()
    vim.notify = real_notify
    if server then server.close() server = nil end
    helpers.reset_client_state()
  end)

  ---Serve the rebuild with CONTENT; returns the request lines received.
  local function serve_rebuild (content)
    local requests = {}
    server = helpers.connect_to_fake_server(function (line, respond)
      table.insert(requests, line)
      if line:find('rebuild session-lifetime stores', 1, true) then
        respond(helpers.framed(
          '((response-type rebuild-session-lifetime-stores)'
          .. ' (content "' .. content .. '"))'))
      end
    end)
    return requests
  end

  it('sends its request, and after success fetches the herald rules',
     function ()
    local requests = serve_rebuild('Config reloaded; rebuilt.')
    misc_requests.rebuild_session_lifetime_stores()
    assert.is_truthy(vim.wait(2000, function ()
      return vim.tbl_contains(requests, '((request . "herald rules"))')
    end, 10))
    assert.is_truthy(vim.tbl_contains(requests,
      '((request . "rebuild session-lifetime stores"))'))
  end)

  it('after a failure fetches nothing more', function ()
    local requests = serve_rebuild('Rebuild failed: Reloading config: bad')
    misc_requests.rebuild_session_lifetime_stores()
    vim.wait(300, function () return false end, 10)
    assert.is_falsy(vim.tbl_contains(requests,
                    '((request . "herald rules"))'))
    assert.is_truthy(vim.tbl_contains(requests,
      '((request . "rebuild session-lifetime stores"))'))
  end)
end)
