-- Mirrors tests/elisp/test-skg-readable-ids.el: the title cache, the
-- requests it saves, and the refresh that empties it.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local client = require('skg.client')
local length_prefix = require('skg.length_prefix')
local readable_ids = require('skg.readable_ids')

local id_a = '11111111-1111-4111-8111-111111111111'
local id_b = '22222222-2222-4222-8222-222222222222'
local id_restricted = '33333333-3333-4333-8333-333333333333'
local id_unrestricted = '44444444-4444-4444-8444-444444444444'
local id_c = '55555555-5555-4555-8555-555555555555'
local id_d = '66666666-6666-4666-8666-666666666666'

local function titles_payload (gnid, title)
  return string.format(
    '((response-type titles-by-ids) (content ((%q . %q))))', gnid, title)
end

---The title annotations in the current buffer, each its text.
local function title_annotations ()
  local result = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(
                          0, readable_ids.title_namespace, 0, -1,
                          { details = true })) do
    table.insert(result, mark[4].virt_text[1][1]) end
  return result
end

---The concealed ID tails in the current buffer.
local function concealed_tail_count ()
  return #vim.api.nvim_buf_get_extmarks(
    0, readable_ids.namespace, 0, -1, {})
end

describe('skg.readable_ids', function ()
  local sent
  local original_send_string = client.send_string

  before_each(function ()
    sent = {}
    client.send_string = function (text) table.insert(sent, text) end
    readable_ids.title_cache = {}
    readable_ids.pending_title_requests = {}
    helpers.reset_client_state()
  end)

  after_each(function ()
    client.send_string = original_send_string
    readable_ids.title_cache = {}
    readable_ids.pending_title_requests = {}
    helpers.reset_client_state()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('drops an earlier response for a buffer annotated since', function ()
    -- Mirrors test-skg-readable-ids-later-response-survives-earlier-response.
    local buf = helpers.buffer_with(id_a)
    readable_ids.annotate_buffer(buf)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { id_b })
    readable_ids.annotate_buffer(buf)
    assert.are.equal(2, #sent)
    length_prefix.dispatch_by_type(titles_payload(id_a, 'Title A'))
    length_prefix.dispatch_by_type(titles_payload(id_b, 'Title B'))
    assert.are.same({ ' Title B' }, title_annotations())
  end)

  it('shortens a restricted ID without a title', function ()
    -- Mirrors
    -- test-skg-readable-ids-shortens-restricted-ids-without-title-overlay.
    helpers.buffer_with({
      string.format(
        '* (skg node (gnid %s) (skgRepos (nodeRepo private)))',
        id_restricted),
      string.format(
        '* (skg node (gnid %s) (skgRepos (nodeRepo public))) unrestricted',
        id_unrestricted) })
    readable_ids.annotate_buffer(0)
    assert.are.equal(2, concealed_tail_count())
    length_prefix.dispatch_by_type(
      titles_payload(id_unrestricted, 'Public Title'))
    local marks = vim.api.nvim_buf_get_extmarks(
      0, readable_ids.title_namespace, 0, -1, {})
    assert.are.equal(1, #marks)
    assert.are.equal(1, marks[1][2]) -- on the second line (row 1)
  end)

  it('annotates cached titles without a request', function ()
    -- Mirrors test-skg-readable-ids-cached-titles-need-no-request.
    readable_ids.title_cache[id_c] = 'Cached Title'
    helpers.buffer_with(id_c)
    readable_ids.annotate_buffer(0)
    assert.are.equal(0, #sent)
    assert.are.same({ ' Cached Title' }, title_annotations())
  end)

  it('requests only the uncached IDs and caches the answer', function ()
    -- Mirrors test-skg-readable-ids-requests-only-uncached.
    readable_ids.title_cache[id_c] = 'Cached Title'
    helpers.buffer_with({ id_c, id_d })
    readable_ids.annotate_buffer(0)
    assert.are.equal(1, #sent)
    assert.is_truthy(sent[1]:find(id_d, 1, true))
    assert.is_falsy(sent[1]:find(id_c, 1, true))
    assert.are.equal(1, #title_annotations())
    length_prefix.dispatch_by_type(titles_payload(id_d, 'Fetched Title'))
    assert.are.equal(2, #title_annotations())
    assert.are.equal('Fetched Title', readable_ids.title_cache[id_d])
  end)

  it('caches a missing ID, and the refresh asks again', function ()
    -- Mirrors test-skg-readable-ids-missing-cached-and-refreshable.
    helpers.buffer_with(id_c)
    readable_ids.annotate_buffer(0)
    assert.are.equal(1, #sent)
    length_prefix.dispatch_by_type(
      '((response-type titles-by-ids) (content ()))')
    assert.are.equal(false, readable_ids.title_cache[id_c])
    readable_ids.annotate_buffer(0) -- like a neogit refresh
    assert.are.equal(1, #sent)
    assert.are.equal(0, #title_annotations())
    readable_ids.refresh(0)
    assert.are.equal(2, #sent)
    assert.is_truthy(sent[2]:find(id_c, 1, true))
  end)

  it('retries a challenged lookup with the approved IDs', function ()
    -- Mirrors test-skg-readable-ids-privacy-challenge-retries-exact-pids.
    helpers.buffer_with(id_c)
    readable_ids.annotate_buffer(0)
    assert.are.equal(1, #sent)
    local original_confirm = vim.fn.confirm
    vim.fn.confirm = function () return 1 end
    length_prefix.dispatch_by_type(string.format(
      '((response-type overPrivateText-telescope-confirmation)'
        .. ' (operation titles-by-ids) (pids (%q))'
        .. ' (prompt "Include lower text?"))', id_c))
    vim.fn.confirm = original_confirm
    assert.are.equal(2, #sent)
    assert.is_truthy(sent[2]:find('approved-overPrivateText-pids', 1, true))
    assert.is_truthy(sent[2]:find(id_c, 1, true))
    assert.is_nil(readable_ids.title_cache[id_c])
  end)
end)
