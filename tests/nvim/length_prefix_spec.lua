-- Mirrors tests/elisp/test-skg-length-prefix.el (the split
-- binary-chunk dispatch test), plus step-machine unit coverage that
-- elisp exercised only implicitly through production use.

local length_prefix = require('skg.length_prefix')
local state = require('skg.state')

local function reset_state ()
  state.lp_reset()
  state.clear_request_coordinator()
end

local function framed (payload)
  return string.format('Content-Length: %d\r\n\r\n%s',
                       #payload, payload)
end

local function artifact_framed (descriptor, bytes)
  return string.format(
    'Content-Length: %d\r\n'
    .. 'Content-Type: application/x-skg-artifact-bundle\r\n'
    .. 'Descriptor-Length: %d\r\n\r\n%s%s',
    #descriptor + #bytes, #descriptor, descriptor, bytes)
end

local function activate_request ()
  local record = state.take_request_record()
  state.active_request_id = record.id
  return record
end

local function response (record, frame_kind, fields, terminal_status)
  return string.format(
    '((response-type %s)%s (request-id %q) (frame-kind %s)%s)',
    frame_kind, fields or '', record.id, frame_kind,
    terminal_status
      and string.format(' (terminal-status %s)', terminal_status) or '')
end

describe('skg.length_prefix dispatch', function ()
  before_each(reset_state)

  it('reassembles a split chunk with non-ASCII payload', function ()
    -- The elisp test's exact payload: multibyte content must survive
    -- an arbitrary split point.
    local seen = nil
    state.register_response_handler('titles-by-ids',
      function (payload_text) seen = payload_text end, false)
    local record = activate_request()
    local payload = response(
      record, 'titles-by-ids', ' (content ((id . "Montoya ñó")))',
      'complete')
    local message = framed(payload)
    length_prefix.handle_generic_chunk(message:sub(1, 20))
    length_prefix.handle_generic_chunk(message:sub(21))
    assert.are.equal(payload, seen)
  end)

  it('keeps artifact bytes opaque across arbitrary chunk splits', function ()
    local seen_descriptor = nil
    local seen_artifacts = nil
    state.register_response_handler('maintenance-evidence',
      function (payload_text, _, artifact_bytes)
        seen_descriptor = payload_text
        seen_artifacts = artifact_bytes
      end, true)
    local record = activate_request()
    local descriptor = response(
      record, 'maintenance-evidence', ' (note "niño")', 'complete')
    local artifacts = string.char(0, 255, 254, 195, 40, 10)
    local message = artifact_framed(descriptor, artifacts)
    length_prefix.handle_generic_chunk(message:sub(1, 31))
    length_prefix.handle_generic_chunk(message:sub(32, -4))
    assert.is_nil(seen_descriptor)
    length_prefix.handle_generic_chunk(message:sub(-3))
    assert.are.equal(descriptor, seen_descriptor)
    assert.are.equal(artifacts, seen_artifacts)
    assert.is_nil(state.request_records[record.id])
  end)

  it('dispatches two messages arriving in one chunk', function ()
    local calls = {}
    state.register_response_handler('save-lock',
      function () table.insert(calls, 'lock') end, true)
    state.register_response_handler('save-result',
      function () table.insert(calls, 'result') end, true)
    local record = activate_request()
    local first = response(record, 'save-lock', ' (lock-views ())')
    local second = response(
      record, 'save-result', ' (content "x")', 'complete')
    length_prefix.handle_generic_chunk(framed(first) .. framed(second))
    assert.are.same({ 'lock', 'result' }, calls)
  end)

  it('removes one-shot handlers after use and decrements pending',
     function ()
    state.register_response_handler('verify-connection',
      function () end, true)
    local record = activate_request()
    assert.are.equal(1, state.lp_pending_count)
    length_prefix.handle_generic_chunk(
      framed(response(record, 'verify-connection',
                      ' (content "ok")', 'complete')))
    assert.is_nil(state.request_records[record.id])
    assert.are.equal(0, state.lp_pending_count)
  end)

  it('keeps non-one-shot handlers registered', function ()
    local count = 0
    state.register_response_handler('collateral-view',
      function () count = count + 1 end, false)
    local record = activate_request()
    local message = framed(
      response(record, 'collateral-view', ' (view-uri "u")'))
    length_prefix.handle_generic_chunk(message .. message)
    assert.are.equal(2, count)
    assert.is_not_nil(record.handlers['collateral-view'])
  end)

  it('cleans a terminal request even when its handler errors', function ()
    state.register_response_handler('save-result',
      function () error('handler boom') end, true)
    local record = activate_request()
    length_prefix.handle_generic_chunk(
      framed(response(record, 'save-result', '', 'complete')))
    assert.is_nil(state.request_records[record.id])
    assert.are.equal(0, state.lp_pending_count)
  end)

  it('tolerates a response with no response-type', function ()
    length_prefix.handle_generic_chunk(framed('((content "x"))'))
    -- Nothing to assert beyond "no error"; the warning goes to the log.
  end)

  it('accepts a quoted response-type spelling too', function ()
    local seen = false
    state.register_response_handler('git-diff-mode',
      function () seen = true end, true)
    local record = activate_request()
    length_prefix.handle_generic_chunk(
      framed(string.format(
        '(("response-type" "git-diff-mode") ("content" "on")'
        .. ' ("request-id" %q) ("frame-kind" "git-diff-mode")'
        .. ' ("terminal-status" "complete"))', record.id)))
    assert.is_true(seen)
  end)

  it('keeps like-typed queued requests separate by request ID', function ()
    local sent = {}
    local calls = {}
    local send = function (wire) table.insert(sent, wire) end
    state.register_response_handler('verify-connection',
      function () table.insert(calls, 'first') end, true)
    local first = state.take_request_record()
    state.enqueue_request(first, 'first-wire', send)
    state.register_response_handler('verify-connection',
      function () table.insert(calls, 'second') end, true)
    local second = state.take_request_record()
    state.enqueue_request(second, 'second-wire', send)
    assert.are.same({ 'first-wire' }, sent)
    length_prefix.dispatch_frame(
      response(first, 'verify-connection', '', 'complete'))
    assert.are.same({ 'first-wire', 'second-wire' }, sent)
    length_prefix.dispatch_frame(
      response(second, 'verify-connection', '', 'complete'))
    assert.are.same({ 'first', 'second' }, calls)
    assert.is_nil(state.active_request_id)
  end)

  it('dispatches a server push without a request ID', function ()
    local seen = nil
    state.register_server_push_handler('collateral-view',
      function (payload_text) seen = payload_text end)
    local pushed = '((response-type collateral-view)'
      .. ' (frame-kind collateral-view) (server-push true)'
      .. ' (operation-id background-1) (content fresh))'
    length_prefix.dispatch_frame(pushed)
    assert.are.equal(pushed, seen)
    state.remove_server_push_handler('collateral-view')
  end)

  it('errors on a malformed header and resets', function ()
    assert.has_error(function ()
      length_prefix.handle_generic_chunk(
        'No-Length-Here: 5\r\n\r\nabcde') end)
    assert.are.equal('', state.lp_buffer)
    assert.is_nil(state.lp_bytes_left)
  end)
end)

describe('skg.length_prefix step machine', function ()
  it('waits for a complete header', function ()
    assert.are.equal('need_more',
      length_prefix.step('Content-Length: 5\r\n', nil).kind)
  end)

  it('parses a header and hands back partial body bytes', function ()
    local step = length_prefix.step('Content-Length: 5\r\n\r\nab', nil)
    assert.are.equal('header', step.kind)
    assert.are.equal(5, step.length)
    assert.are.equal('ab', step.remainder)
  end)

  it('parses and validates an artifact descriptor boundary', function ()
    local step = length_prefix.step(
      'Content-Length: 9\r\n'
      .. 'Content-Type: application/x-skg-artifact-bundle\r\n'
      .. 'Descriptor-Length: 5\r\n\r\nab', nil)
    assert.are.equal('header', step.kind)
    assert.are.equal(9, step.length)
    assert.are.equal(5, step.descriptor_length)
    assert.are.equal('artifact', step.body_state.kind)
    assert.are.equal('ab', step.remainder)
  end)

  it('refuses an artifact descriptor longer than its body', function ()
    local step = length_prefix.step(
      'Content-Length: 4\r\n'
      .. 'Content-Type: application/x-skg-artifact-bundle\r\n'
      .. 'Descriptor-Length: 5\r\n\r\nbody', nil)
    assert.are.equal('error', step.kind)
    assert.are.equal('Malformed artifact-bundle header', step.message)
  end)

  it('validates descriptor UTF-8 without inspecting opaque bytes', function ()
    local invalid = length_prefix.try_consume_body(
      string.char(255) .. 'opaque',
      { kind = 'artifact', length = 7, descriptor_length = 1 })
    assert.are.equal('error', invalid.kind)
    local valid = length_prefix.try_consume_body(
      '()' .. string.char(255, 254),
      { kind = 'artifact', length = 4, descriptor_length = 2 })
    assert.are.equal('done', valid.kind)
    assert.are.equal(string.char(255, 254), valid.artifact_bytes)
  end)

  it('waits for a complete body, then splits payload and remainder',
     function ()
    assert.are.equal('need_more', length_prefix.step('abc', 5).kind)
    local done = length_prefix.step('abcdeREST', 5)
    assert.are.equal('done', done.kind)
    assert.are.equal('abcde', done.payload)
    assert.are.equal('REST', done.remainder)
  end)

  it('counts body length in bytes, not characters', function ()
    local payload = 'ñó'  -- 4 bytes, 2 characters
    local done = length_prefix.step(payload, #payload)
    assert.are.equal('done', done.kind)
    assert.are.equal(payload, done.payload)
  end)
end)
