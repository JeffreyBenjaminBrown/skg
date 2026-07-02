-- PURPOSE: Bootstrap for the skg nvim test suites. Passed as
-- minimal_init to PlenaryBustedDirectory (bash/nvim-tests.sh), so each
-- child nvim gets the repo's plugin on its runtimepath plus the baked-in
-- image plugins (plenary, orgmode, ...), and nothing else.
-- The analog of elisp/skg-test-utils.el's load-path setup.

local this_file = debug.getinfo(1, 'S').source:sub(2) -- strip leading '@'
local repo_root = vim.fn.fnamemodify(this_file, ':h:h:h')
vim.opt.runtimepath:prepend(repo_root .. '/nvim')
-- The harness starts children with --noplugin; force the packpath
-- plugins (plenary, orgmode, neogit, ...) onto the runtimepath anyway.
vim.cmd('packloadall!')

---Where the repo lives, for specs that need fixtures.
---@return string
function _G.skg_test_repo_root ()
  return repo_root
end
