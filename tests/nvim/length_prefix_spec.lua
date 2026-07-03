-- Mirrors tests/elisp/test-skg-length-prefix.el (the split
-- binary-chunk dispatch test), plus step-machine unit coverage that
-- elisp exercised only implicitly through production use.

local length_prefix = require('skg.length_prefix')
local state = require('skg.state')

local function reset_state ()
  state.lp_reset()
  state.response_handler_map = {}
  state.lp_pending_count = 0
end

local function framed (payload)
  return string.format('Content-Length: %d\r\n\r\n%s',
                       #payload, payload)
end

describe('skg.length_prefix dispatch', function ()
  before_each(reset_state)

  it('reassembles a split chunk with non-ASCII payload', function ()
    -- The elisp test's exact payload: multibyte content must survive
    -- an arbitrary split point.
    local payload =
      '((response-type titles-by-ids)'
      .. ' (content ((id . "Montoya ñó"))))'
    local message = framed(payload)
    local seen = nil
    state.register_response_handler('titles-by-ids',
      function (payload_text) seen = payload_text end, false)
    length_prefix.handle_generic_chunk(message:sub(1, 20))
    length_prefix.handle_generic_chunk(message:sub(21))
    assert.are.equal(payload, seen)
  end)

  it('dispatches two messages arriving in one chunk', function ()
    local first = '((response-type save-lock) (lock-views ()))'
    local second = '((response-type save-result) (content "x"))'
    local calls = {}
    state.register_response_handler('save-lock',
      function () table.insert(calls, 'lock') end, true)
    state.register_response_handler('save-result',
      function () table.insert(calls, 'result') end, true)
    length_prefix.handle_generic_chunk(framed(first) .. framed(second))
    assert.are.same({ 'lock', 'result' }, calls)
  end)

  it('removes one-shot handlers after use and decrements pending',
     function ()
    state.register_response_handler('verify-connection',
      function () end, true)
    assert.are.equal(1, state.lp_pending_count)
    length_prefix.handle_generic_chunk(
      framed('((response-type verify-connection) (content "ok"))'))
    assert.is_nil(state.response_handler_map['verify-connection'])
    assert.are.equal(0, state.lp_pending_count)
  end)

  it('keeps non-one-shot handlers registered', function ()
    local count = 0
    state.register_response_handler('collateral-view',
      function () count = count + 1 end, false)
    local message =
      framed('((response-type collateral-view) (view-uri "u"))')
    length_prefix.handle_generic_chunk(message .. message)
    assert.are.equal(2, count)
    assert.is_not_nil(state.response_handler_map['collateral-view'])
  end)

  it('keeps a one-shot handler that errored, and logs', function ()
    -- Mirrors the elisp comment: if the handler signals, the removal
    -- does not fire, so the handler is not removed.
    state.register_response_handler('save-result',
      function () error('handler boom') end, true)
    length_prefix.handle_generic_chunk(
      framed('((response-type save-result))'))
    assert.is_not_nil(state.response_handler_map['save-result'])
  end)

  it('tolerates a response with no response-type', function ()
    length_prefix.handle_generic_chunk(framed('((content "x"))'))
    -- Nothing to assert beyond "no error"; the warning goes to the log.
  end)

  it('accepts a quoted response-type spelling too', function ()
    local seen = false
    state.register_response_handler('git-diff-mode',
      function () seen = true end, true)
    length_prefix.handle_generic_chunk(
      framed('(("response-type" "git-diff-mode") ("content" "on"))'))
    assert.is_true(seen)
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
