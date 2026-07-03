-- Coverage for skg.content_view against the loopback fake server:
-- the normal open, the switch-to-view redirect, the server-supplied
-- view-uri override (the override-menu path), and the error/warning
-- display. Mirrors the response-handling halves of
-- tests/elisp/test-skg-override-bypass.el (the request-string cases)
-- and test-skg-warning-channel.el (the content-view channel).

package.path = package.path -- (helpers live beside the specs)
local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local content_view = require('skg.content_view')

describe('skg.content_view request strings', function ()
  it('carries id and view-uri', function ()
    assert.are.equal(
      '((request . "single root content view") (id . "abc")'
      .. ' (view-uri . "uri-1"))\n',
      content_view.request_string('abc', 'uri-1', nil))
  end)

  it('adds override-choice bypass only when asked', function ()
    -- Mirrors test-skg-override-bypass.el's request-shape cases.
    assert.are.equal(
      '((request . "single root content view") (id . "abc")'
      .. ' (view-uri . "uri-1") (override-choice . "bypass"))\n',
      content_view.request_string('abc', 'uri-1', true))
  end)
end)

describe('skg.content_view responses', function ()
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

  local function serve_content (payload)
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('single root content view', 1, true) then
        respond(helpers.framed(payload))
      end
    end)
  end

  it('opens a buffer holding the served view', function ()
    serve_content(
      '((response-type content-view)'
      .. ' (content "* (skg (node (id abc))) served title\n** child")'
      .. ' (errors ()) (warnings ()))')
    content_view.request_single_root_content_view_from_id('abc')
    local opened = nil
    vim.wait(2000, function ()
      opened = buffer.find_buffer_by_uri(
        vim.b[vim.api.nvim_get_current_buf()].skg_view_uri or '')
      return vim.b[vim.api.nvim_get_current_buf()].skg_content_view
             == true
    end, 10)
    local buf = vim.api.nvim_get_current_buf()
    assert.are.equal('skg://served title',
                     vim.api.nvim_buf_get_name(buf))
    assert.are.equal('* (skg (node (id abc))) served title',
      vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1])
    assert.is_truthy(vim.b[buf].skg_view_uri)
  end)

  it('adopts the server-supplied view uri (the override menu path)',
     function ()
    serve_content(
      '((response-type content-view)'
      .. ' (content "* (skg (node (id abc))) pick an overrider")'
      .. ' (view-uri "override-menu:abc")'
      .. ' (to-minibuffer "The requested node is overridden.'
      .. ' Choose a destination.")'
      .. ' (errors ()) (warnings ()))')
    local notified = nil
    local original_notify = vim.notify
    vim.notify = function (msg) notified = msg end
    content_view.request_single_root_content_view_from_id('abc')
    vim.wait(2000, function ()
      return buffer.find_buffer_by_uri('override-menu:abc') ~= nil
    end, 10)
    vim.notify = original_notify
    assert.is_truthy(buffer.find_buffer_by_uri('override-menu:abc'))
    assert.is_truthy(tostring(notified):find('overridden'))
  end)

  it('switches to an already-open view instead of duplicating',
     function ()
    local existing = buffer.open_org_buffer_from_text(
      '* already open', 'skg://already open', 'uri-existing')
    local scratch = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_set_current_buf(scratch)
    serve_content(
      '((response-type content-view)'
      .. ' (switch-to-view "uri-existing"))')
    content_view.request_single_root_content_view_from_id('abc')
    vim.wait(2000, function ()
      return vim.api.nvim_get_current_buf() == existing
    end, 10)
    assert.are.equal(existing, vim.api.nvim_get_current_buf())
  end)

  it('shows errors and warnings in a messages buffer', function ()
    -- Mirrors test-skg-warning-channel.el's content-view case.
    serve_content(
      '((response-type content-view) (content "")'
      .. ' (errors ("boom one" "boom two"))'
      .. ' (warnings ("careful")))')
    local notified = nil
    local original_notify = vim.notify
    vim.notify = function (msg) notified = msg end
    content_view.request_single_root_content_view_from_id('abc')
    vim.wait(2000, function () return notified ~= nil end, 10)
    vim.notify = original_notify
    assert.is_truthy(
      tostring(notified):find('errors and warnings'))
    local messages_buf = nil
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_get_name(buf)
         == 'skg://messages/content-view' then
        messages_buf = buf end
    end
    assert.is_truthy(messages_buf)
    local text = table.concat(
      vim.api.nvim_buf_get_lines(messages_buf, 0, -1, false), '\n')
    assert.is_truthy(text:find('%* errors'))
    assert.is_truthy(text:find('%*%* boom one'))
    assert.is_truthy(text:find('%* warnings'))
    assert.is_truthy(text:find('%*%* careful'))
    vim.api.nvim_buf_delete(messages_buf, { force = true })
  end)
end)
