-- Real-server import, preview confirmation, render, and save.
local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

local input_dir = assert(os.getenv('SKG_TEST_INPUT_DIR'))
local source_dir = assert(os.getenv('SKG_TEST_SOURCE_DIR'))
local host_prompts, approvals = 0, 0
local original_input, original_confirm = vim.fn.input, vim.fn.confirm
vim.fn.input = function () host_prompts = host_prompts + 1 return '' end
vim.fn.confirm = function () approvals = approvals + 1 return 1 end

require('skg.import_md_and_org').import_md_and_org(input_dir, 'main')
local result = T.wait_for_buffer('skg://messages/import-result', 15)
T.check(result and T.buffer_text(result):find('Imported %d+ nodes'),
  'import result arrived')
vim.fn.input, vim.fn.confirm = original_input, original_confirm
T.check(host_prompts == 1, 'absolute filesystem link prompted once')
T.check(approvals == 1, 'valid preview required explicit approval')
T.check(vim.fn.filereadable(source_dir .. '/import-root.skg') == 1,
  'explicit Org root ID was published')
-- The import result schedules a clean-view refresh on the same connection.
vim.wait(100, function () return false end, 100)
T.check(T.wait_for_response(15), 'import refresh settled')

require('skg.content_view')
  .request_single_root_content_view_from_id('import-root')
local view = T.wait_for_buffer('skg://Sample', 15)
T.check(view, 'imported root view opened')
T.check(T.buffer_text(view):find(',%* not a heading'),
  'literal Org-looking heading was safely encoded')
T.check(T.wait_for_response(15), 'imported view settled')

vim.api.nvim_set_current_buf(view)
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(15), 'imported view saved')
local files = vim.fn.glob(source_dir .. '/*.skg', false, true)
T.check(#files == 7, 'save did not turn the literal heading into a node')
local hard_break = 'First line' .. string.char(92, 92) .. '\n'
local found_hard_break = false
for _, file in ipairs(files) do
  local content = table.concat(vim.fn.readfile(file), '\n')
  if content:find(hard_break, 1, true) then found_hard_break = true end
end
T.check(found_hard_break, 'Markdown hard break became Org double backslash')
local root_text = table.concat(vim.fn.readfile(source_dir .. '/import-root.skg'), '\n')
T.check(root_text:find('%* not a heading'),
  'saved root retained the literal heading')
T.pass('PASS: mixed import, approval, literal body, save')
