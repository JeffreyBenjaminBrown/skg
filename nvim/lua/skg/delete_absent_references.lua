local client = require('skg.client')
local lock = require('skg.lock')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local rerender = require('skg.rerender')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

local function remove_unfired (types)
  for _, response_type in ipairs(types) do
    if state.response_handler_map[response_type] then
      state.response_handler_map[response_type] = nil
      state.lp_pending_count = math.max(0, state.lp_pending_count - 1)
    end
  end
end

local function any_dirty_view ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and vim.bo[buf].modified
       and vim.b[buf].skg_view_id then return true end
  end
  return false
end

local function send (skgid, approved_preview)
  lock.begin_stream('delete absent references')
  lock.lock_all_skg_buffers()
  rerender.register_rerender_stream_handlers()
  state.register_response_handler('delete-references-confirmation',
    function (_text, response)
      remove_unfired({ 'delete-references-result', 'error' })
      local content = payload.field_text(response, 'content') or ''
      local approval = payload.field_text(response, 'approved-preview') or ''
      local buf = vim.api.nvim_create_buf(false, true)
      vim.api.nvim_buf_set_name(buf, 'skg://absent-reference-warning')
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(content, '\n'))
      vim.bo[buf].modifiable = false
      vim.bo[buf].filetype = 'org'
      vim.api.nvim_set_current_buf(buf)
      if vim.fn.confirm('Remove the structured references?', '&Yes\n&No', 2) == 1 then
        rerender.after_empty_stream = function () send(skgid, approval) end end
    end, true)
  state.register_response_handler('delete-references-result',
    function (_text, response)
      remove_unfired({ 'error' })
      vim.notify(payload.field_text(response, 'content') or
                 'Absent-reference cleanup complete')
    end, true)
  state.register_response_handler('error',
    function (_text, response)
      -- The dispatcher removes `error' itself.  Remove only terminal
      -- alternatives that can no longer arrive.
      remove_unfired({ 'delete-references-confirmation',
                       'delete-references-result' })
      vim.notify('skg absent-reference cleanup: ' ..
                 (payload.field_text(response, 'content') or 'server error'),
                 vim.log.levels.ERROR)
    end, true)
  state.lp_reset()
  local request = { sexpr.pair(sexpr.symbol('request'), 'delete references to absent node'),
                    sexpr.pair(sexpr.symbol('id'), skgid) }
  if approved_preview then
    table.insert(request, sexpr.pair(sexpr.symbol('approved-preview'), approved_preview)) end
  client.send_string(sexpr.to_string(request) .. '\n')
end

function M.request ()
  local skgid = metadata.unknown_id_at_point()
  if not skgid then error('skg: point must be on an Unknown headline') end
  if any_dirty_view() then error('skg: save or revert every view before global cleanup') end
  send(skgid, nil)
end

return M
