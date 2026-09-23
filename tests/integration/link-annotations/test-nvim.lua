-- Live graph lookup, extmarks, and graph publication in Neovim.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(45)

local annotations = require('skg.link_annotations')
local buffer = require('skg.buffer')
local herald_rules = require('skg.herald_rules')
local misc = require('skg.misc_requests')
local save = require('skg.save')

T.client.connect()
herald_rules.request_herald_rules()
T.check(T.wait_for_response(10), 'herald rules arrived')

local view_text = '* (skg (node (id src) (source public))) '
  .. 'Source [[id:old-dest][café]]\n'
  .. '[[id:gone][gone]] [[id:private-node][private]]\n'
local buf = buffer.open_org_buffer_from_text(
  view_text, 'skg://link-integration', 'link-integration')

local function status (id, kind)
  return annotations.cache[id] and annotations.cache[id][1] == kind
end

local function suffix (fragment)
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(
      buf, annotations.namespace, 0, -1, { details = true })) do
    local text = mark[4].virt_text and mark[4].virt_text[1][1] or ''
    if text:find(fragment, 1, true) then return true end
  end
  return false
end

T.check(T.wait_for(function ()
  return status('old-dest', 'resolved') and status('gone', 'missing')
         and status('private-node', 'inactive')
end, 10), 'initial resolved, missing, and inactive statuses arrived')
T.check(buffer.text(buf) == view_text and not vim.bo[buf].modified,
        'initial annotations leave buffer text and modified state alone')
T.check(vim.b[buf].skg_link_source_suffix ~= true,
        'source suffix starts off')
annotations.toggle_source_overlay(buf)
T.check(suffix('⌂:PUB') and suffix('⌂:missing')
        and suffix('⌂:inactive') and not suffix('PRIV'),
        'suffixes distinguish visible and unavailable targets')
save.replace_buffer_with_new_content(buf, view_text)
T.check(vim.b[buf].skg_link_source_suffix == true
        and not vim.bo[buf].modified,
        'source toggle and clean state survive replacement')

vim.api.nvim_buf_set_lines(buf, -1, -1, false,
                           { '[[id:new][new]]' })
annotations.refresh(buf)
T.check(T.wait_for(function () return status('new', 'missing') end, 10),
        'unsaved link edit is looked up')
local unsaved_text = buffer.text(buf)
local data = assert(os.getenv('SKG_TEST_DATA_DIR'))
local new_file = assert(io.open(data .. '/public/new.skg', 'w'))
new_file:write('pid: new\ntitle: New target\n')
new_file:close()
misc.rebuild_ephemeral_data_stores()
T.check(T.wait_for(function () return status('new', 'resolved') end, 10),
        'newly published target stops being missing')
T.check(buffer.text(buf) == unsaved_text and vim.bo[buf].modified,
        'lookup and rebuild leave unsaved text alone')

assert(os.rename(data .. '/private/private-node.skg',
                 data .. '/public/private-node.skg'))
misc.rebuild_ephemeral_data_stores()
T.check(T.wait_for(function ()
  return status('private-node', 'resolved') end, 10),
  'source move refreshes inactive target')
assert(os.remove(data .. '/public/dest.skg'))
misc.rebuild_ephemeral_data_stores()
T.check(T.wait_for(function () return status('old-dest', 'missing') end, 10),
        'deleted target becomes missing')
T.check(suffix('⌂:missing'), 'deleted target gets unavailable suffix')

T.pass('PASS: live Neovim link annotation cycle')
