-- Coverage for skg.search's three-message flow (search-results ->
-- request-snapshot -> search-enrichment) against the loopback fake
-- server, plus skg.search_make_link's finish paths, mirroring
-- tests/elisp/test-skg-search-make-link.el (the finish-at-link,
-- finish-at-headline and no-target cases; the label-reduction cases
-- are in id_search_spec.lua).

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local search = require('skg.search')
local search_make_link = require('skg.search_make_link')

describe('skg.search', function ()
  local server

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    search_make_link.pending = nil
  end)

  after_each(function ()
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
  end)

  it('sends the four search axes on the wire', function ()
    local seen = nil
    server = helpers.connect_to_fake_server(function (line)
      if line:find('text search', 1, true) then seen = line end
    end)
    search.request_text_search('dog cat', true, false, true)
    vim.wait(2000, function () return seen ~= nil end, 10)
    assert.are.equal(
      '((request . "text search") (terms . "dog cat")'
      .. ' (regex . "true") (body . "false")'
      .. ' (operators . "true"))', seen)
  end)

  it('opens results, snapshots on request, and applies enrichment',
     function ()
    local snapshot = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('"text search"', 1, true) then
        respond(helpers.framed(
          '((response-type search-results)'
          .. ' (content "* (skg (node (id r1))) first result'
          .. '\\n* (skg (node (id r2))) second result"))'))
        -- Immediately ask for the snapshot, as the server does.
        respond(helpers.framed(
          '((response-type request-snapshot)'
          .. ' (content "dog"))'))
      elseif line:find('snapshot response', 1, true) then
        snapshot = line
        respond(helpers.framed(
          '((response-type search-enrichment) (terms "dog")'
          .. ' (content "* (skg (node (id r1))) first result'
          .. ' enriched\\n* (skg (node (id r2))) second result'
          .. ' enriched") (warnings ()))'))
      end
    end)
    search.search('dog')
    local buf = nil
    vim.wait(3000, function ()
      buf = buffer.find_buffer_by_uri('search:dog')
      if not buf then return false end
      local first =
        vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1] or ''
      return first:find('enriched', 1, true) ~= nil
    end, 10)
    assert.is_truthy(buf)
    assert.are.equal('skg://?dog', vim.api.nvim_buf_get_name(buf))
    assert.is_truthy(snapshot)
    local text = table.concat(
      vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
    assert.is_truthy(text:find('first result enriched', 1, true))
    -- Read-only was dropped again after enrichment.
    assert.is_true(vim.bo[buf].modifiable)
  end)

  it('search_make_link finish inserts a link at the recorded spot',
     function ()
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('"text search"', 1, true) then
        respond(helpers.framed(
          '((response-type search-results)'
          .. ' (content "* (skg (node (id pick-me)))'
          .. ' chosen [[id:inner][with link]] title"))'))
      end
    end)
    local source = buffer.open_org_buffer_from_text(
      '* (skg (node (id src))) source\nbody line ',
      'skg://source', 'uri-source')
    vim.api.nvim_win_set_cursor(0, { 2, 9 })
    search_make_link.search_make_link('chosen')
    vim.wait(3000, function ()
      return buffer.find_buffer_by_uri('search:chosen') ~= nil
    end, 10)
    local search_buf = buffer.find_buffer_by_uri('search:chosen')
    vim.api.nvim_set_current_buf(search_buf)
    vim.api.nvim_win_set_cursor(0, { 1, 4 })
    search_make_link.finish()
    -- The search buffer died; we are back in the source; the link's
    -- label had its inner textlink reduced.
    assert.is_false(vim.api.nvim_buf_is_valid(search_buf))
    assert.are.equal(source, vim.api.nvim_get_current_buf())
    local text = table.concat(
      vim.api.nvim_buf_get_lines(source, 0, -1, false), '\n')
    assert.is_truthy(text:find(
      '[[id:pick-me][chosen with link title]]', 1, true))
  end)

  it('finish errors helpfully with no target', function ()
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_set_current_buf(buf)
    local ok, err = pcall(search_make_link.finish)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find(
      'Not in a link-creation search buffer', 1, true))
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  it('a plain search does not inherit link-creation state', function ()
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('"text search"', 1, true) then
        respond(helpers.framed(
          '((response-type search-results)'
          .. ' (content "* (skg (node (id r1))) result"))'))
      end
    end)
    search.search('plain')
    vim.wait(3000, function ()
      return buffer.find_buffer_by_uri('search:plain') ~= nil
    end, 10)
    local buf = buffer.find_buffer_by_uri('search:plain')
    assert.is_nil(vim.b[buf].skg_link_target_buf)
  end)
end)
