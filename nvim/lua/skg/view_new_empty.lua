-- PURPOSE: Open a blank skg content view for creating new nodes. The
-- Lua port of elisp/skg-view-new-empty.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local payload = require('skg.payload')
local state = require('skg.state')

local M = {}

local function unused_buffer_name ()
  local base = 'skg://empty'
  local occupied = {}
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then
      occupied[vim.api.nvim_buf_get_name(buf)] = true end
  end
  if not occupied[base] then return base end
  local suffix = 2
  while occupied[string.format('%s<%d>', base, suffix)] do
    suffix = suffix + 1 end
  return string.format('%s<%d>', base, suffix)
end

---Ask the server for the current runtime generation, and call
---CALLBACK with it, a string, when the answer arrives.
---@param callback fun(generation: string)
function M.request_runtime_generation (callback)
  state.register_response_handler('runtime-generation',
    function (_payload, response)
      callback(payload.field_text(response, 'content'))
    end, true)
  state.lp_reset()
  client.send_string('((request . "runtime generation"))\n')
end

---Open a blank skg content view for creating new nodes. It opens when
---the server answers the current runtime generation, which it keeps as
---its 'skg_rendered_at': its save is then refused if it would write a
---node that changed after it was opened.
function M.view_new_empty ()
  M.request_runtime_generation(function (generation)
    buffer.open_org_buffer_from_text('', unused_buffer_name(), nil,
                                     generation)
  end)
end

return M
