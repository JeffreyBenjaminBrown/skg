-- PURPOSE: The rerender stream (rerender-lock -> rerender-view* ->
-- rerender-done), shared by the diff-mode toggle, the source-set
-- switch, and the explicit rerender-all request. The Lua port of
-- elisp/skg-request-rerender-all-views.el.

local client = require('skg.client')
local lock = require('skg.lock')
local log = require('skg.log')
local messages = require('skg.messages')
local payload = require('skg.payload')
local save = require('skg.save')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Ask the server to re-render every open view: lock everything, then
---drive the streaming protocol.
function M.request_rerender_all_views ()
  return M.request_rerender_all_views_with_approval(nil)
end

function M.request_rerender_all_views_with_approval (approved_pids)
  lock.begin_stream('rerender')
  lock.lock_all_skg_buffers()
  M.register_rerender_stream_handlers()
  M.register_ugly_confirmation(function (pids)
    M.request_rerender_all_views_with_approval(pids)
  end)
  local request = {
    sexpr.pair(
      sexpr.symbol('request'),
      'rerender all views') }
  if approved_pids and #approved_pids > 0 then
    local approval = { sexpr.symbol('allow-ugly-telescopes') }
    for _, pid in ipairs(approved_pids) do table.insert(approval, pid) end
    table.insert(request, approval) end
  client.submit_request(sexpr.to_string(request) .. '\n')
end

---Register the alternative privacy challenge for a rerendering request.
function M.register_ugly_confirmation (retry, unfired_response_type)
  state.register_response_handler('ugly-telescope-confirmation',
    function (_payload_text, response)
      state.remove_response_handler('ugly-telescope-confirmation')
      if unfired_response_type then
        state.remove_response_handler(unfired_response_type) end
      local prompt = payload.field_text(response, 'prompt') or
        'This rerender includes text selected below home. Include it?'
      local pids = payload.string_list(payload.field(response, 'pids'))
      lock.end_stream()
      lock.unlock_all_save_locked()
      if vim.fn.confirm(prompt, '&Include\n&Decline', 2) == 1 then
        vim.schedule(function () retry(pids) end)
      end
    end, false)
end

---Register the three handlers for streamed rerender responses.
---Shared by every rerender-carrying request.
function M.register_rerender_stream_handlers ()
  state.register_response_handler('rerender-lock',
    function (_payload_text, response)
      state.remove_response_handler('ugly-telescope-confirmation')
      local ok, err = pcall(function ()
        local lock_views = payload.field(response, 'lock-views')
        if lock_views ~= nil then
          lock.unlock_buffers_not_in_uri_list(
            payload.string_list(lock_views)) end
      end)
      if not ok then
        lock.unlock_all_save_locked()
        log.log('error', 'rerender',
                'rerender-lock handler error: %s', tostring(err))
      end
    end, true)
  state.register_response_handler('rerender-view',
    function (payload_text, response)
      save.apply_streamed_view_update(payload_text, response,
                                      'rerender', 'rerender-view')
    end, false) -- non-one-shot: fires for each streamed view
  state.register_response_handler('rerender-done',
    function (_payload_text, response)
      state.remove_response_handler('rerender-view')
      lock.end_stream()
      lock.unlock_all_save_locked() -- safety net
      local ok, err = pcall(function ()
        local errors =
          payload.string_list(payload.field(response, 'errors'))
        local warnings =
          payload.string_list(payload.field(response, 'warnings'))
        if #errors > 0 or #warnings > 0 then
          local headline
          if #errors > 0 and #warnings > 0 then
            headline = 'Rerender completed with errors and warnings'
          elseif #errors > 0 then
            headline = 'Rerender completed with errors'
          else
            headline = 'Rerender completed with warnings' end
          messages.big_nonfatal_message(
            'skg://messages/rerender', headline,
            messages.errors_and_warnings_to_org_string(
              errors, warnings))
        end
      end)
      if not ok then
        vim.notify('skg: rerender-done handler error: '
                   .. tostring(err))
      end
    end, true)
end

return M
