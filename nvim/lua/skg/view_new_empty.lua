-- PURPOSE: Open a new skg content view for a new node: prompts for an
-- owned source (every root needs one -- children inherit, roots
-- cannot) and opens a buffer with an indefinitive, id-less ActiveNode
-- and a placeholder title. The Lua port of elisp/skg-view-new-empty.el.

local buffer = require('skg.buffer')
local picker = require('skg.picker')

local M = {}

function M.view_new_empty ()
  local source = picker.prompt_for_owned_source()
  if not source then return end
  local org_text = string.format(
    '* (skg (node (source %s) indef)) life, the universe and everything\n',
    source)
  buffer.open_org_buffer_from_text(
    org_text, buffer.content_view_buffer_name(org_text))
end

return M
