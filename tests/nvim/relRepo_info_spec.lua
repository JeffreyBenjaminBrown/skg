-- Mirrors the request and handler cases of
-- tests/elisp/test-skg-set-relRepo.el: the relRepo info request, the
-- choice its reply offers, and the alias's local default. The
-- gesture's helpers are in metadata_spec.lua.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local client = require('skg.client')
local config = require('skg.config')
local metadata = require('skg.metadata')
local picker = require('skg.picker')
local relRepo_info = require('skg.relRepo_info')
local sexpr = require('skg.sexpr.parse')

local content_child = {
  '* (skg node (gnid recorder) (skgRepos (nodeRepo public))) recorder',
  '** (skg node (gnid kid) (skgRepos (nodeRepo public))) kid' }

describe('skg.relRepo_info', function ()
  local originals = {}
  local sent, prompts, answer

  before_each(function ()
    originals.repo_names = config.repo_names
    originals.send_string = client.send_string
    originals.read = picker.completing_read_with_cycle
    originals.notify = vim.notify
    config.repo_names = function ()
      return { 'public', 'private', 'trusted' } end
    sent, prompts, answer = {}, {}, nil
    client.send_string = function (text) table.insert(sent, text) end
    picker.completing_read_with_cycle = function (prompt, choices, opts)
      table.insert(prompts, { prompt = prompt, choices = choices,
                              prefill = opts.initial_input })
      return answer end
    vim.notify = function () end
    helpers.reset_client_state()
  end)

  after_each(function ()
    config.repo_names = originals.repo_names
    client.send_string = originals.send_string
    picker.completing_read_with_cycle = originals.read
    vim.notify = originals.notify
    helpers.reset_client_state()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  ---Run the gesture on line 2 and answer the request with REPLY.
  local function run_with_reply (reply)
    relRepo_info.set_relRepo_at_line(2)
    require('skg.length_prefix').dispatch_by_type(reply)
  end

  it('names the recorder, the member and the relation', function ()
    helpers.buffer_with(content_child, 2)
    relRepo_info.set_relRepo()
    assert.are.equal(1, #sent)
    local request = sexpr.read(sent[1])
    local payload = require('skg.payload')
    assert.are.equal('relRepo info', payload.field_text(request, 'request'))
    assert.are.equal('recorder', payload.field_text(request, 'recorder'))
    assert.are.equal('kid', payload.field_text(request, 'member'))
    assert.are.equal('contains', payload.field_text(request, 'relation'))
  end)

  it('offers the ladder from the default, starting at the current',
     function ()
    -- Mirrors test-relRepo-handler-slices-and-applies.
    helpers.buffer_with(content_child, 2)
    answer = 'private'
    run_with_reply('((response-type relRepo-info) (default "private")'
                   .. ' (current "trusted"))')
    assert.are.equal(1, #prompts)
    assert.are.same({ 'private', 'trusted', metadata.relRepo_no_override },
                    prompts[1].choices)
    assert.are.equal('trusted', prompts[1].prefill)
    assert.is_truthy(metadata.line_text(2):find('(relRepo private)', 1, true))
  end)

  it('starts at the default when the current is more public', function ()
    -- Mirrors test-relRepo-handler-more-public-current-prefills-default.
    helpers.buffer_with(content_child, 2)
    answer = metadata.relRepo_no_override
    run_with_reply('((response-type relRepo-info) (default "private")'
                   .. ' (current "public"))')
    assert.are.equal('private', prompts[1].prefill)
  end)

  it('starts at a pending request', function ()
    helpers.buffer_with({
      content_child[1],
      '** (skg node (gnid kid) (skgRepos (nodeRepo public)) (editRequest (relRepo trusted))) kid' },
      2)
    run_with_reply('((response-type relRepo-info) (default "public")'
                   .. ' (current "private"))')
    assert.are.equal('trusted', prompts[1].prefill)
  end)

  it('offers every repo on an error reply', function ()
    -- Mirrors test-relRepo-handler-error-offers-full-ladder.
    helpers.buffer_with(content_child, 2)
    answer = metadata.relRepo_no_override
    run_with_reply('((response-type relRepo-info)'
                   .. " (error \"member 'kid' is not in the graph\"))")
    assert.are.same({ 'public', 'private', 'trusted',
                      metadata.relRepo_no_override }, prompts[1].choices)
    assert.is_falsy(metadata.line_text(2):find('relRepo'))
  end)

  it("takes an alias's default from its owner, with no request",
     function ()
    -- Mirrors test-alias-command-derives-default-locally.
    helpers.buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo private))) recorder',
      '** (skg aliasFolder)',
      '*** (skg alias) nickname' }, 3)
    answer = 'trusted'
    relRepo_info.set_relRepo()
    assert.are.equal(0, #sent)
    assert.are.same({ 'private', 'trusted', metadata.relRepo_no_override },
                    prompts[1].choices)
    assert.is_truthy(metadata.line_text(3):find('(relRepo trusted)', 1, true))
  end)
end)
