-- Live graph lookup, extmarks, and graph swap-in in Neovim.

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

local view_text = '* (skg (node (id src) (repo public))) '
  .. 'Repo [[id:old-dest][café]]\n'
  .. '[[id:gone][gone]] [[id:private-node][private]]\n'
local buf = buffer.open_org_buffer_from_text(
  view_text, 'skg://link-integration', 'link-integration')

local function status (skgid, kind)
  return annotations.cache[skgid] and annotations.cache[skgid][1] == kind
end

local function suffix (fragment)
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(
      buf, annotations.namespace, 0, -1, { details = true })) do
    local text = ''
    for _, chunk in ipairs(mark[4].virt_text or {}) do text = text .. chunk[1] end
    if text:find(fragment, 1, true) then return true end
  end
  return false
end

---How many link labels are styled as broken links.
local function broken_count ()
  local count = 0
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(
      buf, annotations.namespace, 0, -1, { details = true })) do
    if mark[4].hl_group == 'SkgBrokenLink' then count = count + 1 end
  end
  return count
end

T.check(T.wait_for(function ()
  return status('old-dest', 'resolved') and status('gone', 'missing')
         and status('private-node', 'inactive')
end, 10), 'initial resolved, missing, and inactive statuses arrived')
T.check(buffer.text(buf) == view_text and not vim.bo[buf].modified,
        'initial annotations leave buffer text and modified state alone')
T.check(vim.b[buf].skg_link_repo_suffix ~= true,
        'repo suffix starts off')
local repo_sets = require('skg.repo_sets')
local state = require('skg.state')
repo_sets.set_active_repo_set('all')
T.check(T.wait_for(function ()
  return status('private-node', 'resolved')
         and state.lp_pending_count == 0 end, 10),
  'widening the repo-set refreshes a link target without a headline')
repo_sets.set_active_repo_set('public')
T.check(T.wait_for(function ()
  return status('private-node', 'inactive')
         and state.lp_pending_count == 0 end, 10),
  'narrowing the repo-set hides its repo again')
annotations.toggle_repo_overlay(buf)
T.check(suffix('⌂:PUB') and suffix('⌂:inactive') and not suffix('PRIV')
        and not suffix('missing') and broken_count() > 0,
        'suffixes distinguish visible and unavailable targets; broken links get none')
save.replace_buffer_with_new_content(buf, view_text)
T.check(vim.b[buf].skg_link_repo_suffix == true
        and not vim.bo[buf].modified,
        'repo toggle and clean state survive replacement')

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
        'newly swapped-in target stops being missing')
T.check(buffer.text(buf) == unsaved_text and vim.bo[buf].modified,
        'lookup and rebuild leave unsaved text alone')

assert(os.rename(data .. '/private/private-node.skg',
                 data .. '/public/private-node.skg'))
misc.rebuild_ephemeral_data_stores()
T.check(T.wait_for(function ()
  return status('private-node', 'resolved') end, 10),
  'repo move refreshes inactive target')
local broken_before = broken_count()
assert(os.remove(data .. '/public/dest.skg'))
misc.rebuild_ephemeral_data_stores()
T.check(T.wait_for(function () return status('old-dest', 'missing') end, 10),
        'deleted target becomes missing')
T.check(T.wait_for(function () return broken_count() > broken_before end, 10),
        'a link to the deleted target is styled as broken')

T.pass('PASS: live Neovim link annotation cycle')
