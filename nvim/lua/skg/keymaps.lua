-- PURPOSE: All skg keybindings and the Ex-command surface.
-- The Lua port of elisp/skg-keymaps-and-aliases.el, translated per
-- the settled keybinding scheme (plan.org Q4):
--   ':w' saves (BufWriteCmd, wired in skg.buffer); '<localleader>'
--   plays the C-c prefix with the same mnemonic letters; where Emacs
--   ended a chord with RET the letter doubles (C-c g RET ->
--   <localleader>gg); capitals stay capital; delete (C-c backspace)
--   is <localleader>dd, recursive <localleader>dR; id-next/prev also
--   ride the ]i / [i motions; '^' toggles line start <-> title start
--   (the C-a toggle); and every command is an :Skg* Ex command.
--
-- Commands resolve their modules lazily (at keypress), so this
-- surface can be complete while modules land incrementally; invoking
-- a not-yet-ported command reports so instead of erroring. The elisp
-- alias/completion-predicate machinery has no analog here: the Ex
-- command names ARE the public names.

local M = {}

---One entry per user-facing command: the Ex name, the module and
---function that implement it, and its description.
---@type table<string, {module: string, fn: string, desc: string}>
M.commands = {
  Save = { module = 'skg.save', fn = 'request_save_buffer',
           desc = 'Save the view; the server rebuilds it' },
  Delete = { module = 'skg.metadata', fn = 'delete',
             desc = 'Mark the node at point for deletion' },
  DeleteRecursive = { module = 'skg.metadata', fn = 'delete_recursive',
                      desc = 'Mark the node and its descendants for deletion' },
  Search = { module = 'skg.search', fn = 'search',
             desc = 'Text search with conservative defaults' },
  SearchInteractive = { module = 'skg.search', fn = 'search_interactive',
                        desc = 'Text search choosing r/b/t options' },
  SearchMakeLink = { module = 'skg.search_make_link', fn = 'search_make_link',
                     desc = 'Search, pick a result, insert a link to it' },
  Goto = { module = 'skg.id_search', fn = 'goto_id_near_point',
           desc = 'Open a view of the id at point' },
  GotoAndCloseThis = { module = 'skg.id_search', fn = 'goto_and_close_this',
                       desc = 'Goto, closing the buffer left behind' },
  GotoBypassOverride = { module = 'skg.id_search', fn = 'goto_bypass_override',
                         desc = 'Goto the node itself, skipping the override menu' },
  GotoBiggestBranch = { module = 'skg.modify_graph', fn = 'goto_biggest_branch',
                        desc = 'Jump to the biggest sibling/child branch' },
  GotoById = { module = 'skg.id_search', fn = 'goto_by_id',
               desc = 'Open a view of a literal UUID' },
  GotoByIdAndCloseThis = { module = 'skg.id_search', fn = 'goto_by_id_and_close_this',
                           desc = 'Goto by id, closing the buffer left behind' },
  GotoInGit = { module = 'skg.goto_git', fn = 'goto_in_git',
                desc = "Open a git status view of the node's file" },
  GotoInGitAndCloseThis = { module = 'skg.goto_git', fn = 'goto_in_git_and_close_this',
                            desc = 'Goto in git, closing the buffer left behind' },
  GotoInGitParent = { module = 'skg.goto_git', fn = 'goto_in_git_parent',
                      desc = "Open a git view of the parent's file at this id" },
  GotoInGitParentAndCloseThis = { module = 'skg.goto_git',
                                  fn = 'goto_in_git_parent_and_close_this',
                                  desc = 'Goto in git parent, closing this buffer' },
  ShowCollectionAliases = { module = 'skg.view_requests', fn = 'show_collection_aliases',
                            desc = 'Request the alias collection (auto-saves)' },
  ShowCollectionOverrides = { module = 'skg.view_requests', fn = 'show_collection_overrides',
                              desc = 'Request the override collections (auto-saves)' },
  ShowCollectionHides = { module = 'skg.view_requests', fn = 'show_collection_hides',
                          desc = 'Request the hide collections (auto-saves)' },
  ShowCollectionSubscribes = { module = 'skg.view_requests', fn = 'show_collection_subscribes',
                               desc = 'Request the subscribe collections (auto-saves)' },
  ShowPathsThroughContainers = { module = 'skg.view_requests', fn = 'show_paths_through_containers',
                                 desc = 'Graft the containment ancestry (auto-saves)' },
  ShowPathsThroughLinkSources = { module = 'skg.view_requests', fn = 'show_paths_through_link_sources',
                                  desc = 'Graft the nodes that link here (auto-saves)' },
  ShowPathsThroughLinkDests = { module = 'skg.view_requests', fn = 'show_paths_through_link_dests',
                                desc = 'Graft the nodes this links to (auto-saves)' },
  ShowPathsThroughOverriders = { module = 'skg.view_requests', fn = 'show_paths_through_overriders',
                                 desc = 'Graft the nodes overriding this (auto-saves)' },
  ShowPathsThroughOverridden = { module = 'skg.view_requests', fn = 'show_paths_through_overridden',
                                 desc = 'Graft the nodes this overrides (auto-saves)' },
  ShowPathsThroughHiders = { module = 'skg.view_requests', fn = 'show_paths_through_hiders',
                             desc = 'Graft the nodes hiding this (auto-saves)' },
  ShowPathsThroughHidden = { module = 'skg.view_requests', fn = 'show_paths_through_hidden',
                             desc = 'Graft the nodes this hides (auto-saves)' },
  ShowPathsThroughSubscribers = { module = 'skg.view_requests', fn = 'show_paths_through_subscribers',
                                  desc = 'Graft the subscribers (auto-saves)' },
  ShowPathsThroughSubscribees = { module = 'skg.view_requests', fn = 'show_paths_through_subscribees',
                                  desc = 'Graft the subscribees (auto-saves)' },
  SetDefinitive = { module = 'skg.view_requests', fn = 'set_definitive',
                    desc = 'Make this the editable view of its node' },
  SetIndefinitive = { module = 'skg.metadata', fn = 'set_indefinitive',
                      desc = 'Make this view of its node read-only' },
  SetMergeRequest = { module = 'skg.metadata', fn = 'set_merge_request',
                      desc = 'Request merging another node into this one' },
  SetSource = { module = 'skg.metadata', fn = 'set_source',
                desc = "Change the node's source (in-buffer; save applies)" },
  SetSourceRecursive = { module = 'skg.metadata', fn = 'set_source_recursive',
                         desc = 'Change source here and in matching descendants' },
  ReplaceLinkWithContent = { module = 'skg.modify_graph', fn = 'replace_link_with_content',
                             desc = 'Turn the link at point into content (saves)' },
  Fork = { module = 'skg.view_requests', fn = 'fork_node',
           desc = 'Fork the owned node at point into a private clone' },
  ReplaceContentWithLink = { module = 'skg.modify_graph', fn = 'replace_content_with_link',
                             desc = 'Turn the branch at point into a link (saves)' },
  ViewDiffMode = { module = 'skg.diff_mode', fn = 'toggle',
                   desc = 'Toggle git diff annotations in all views' },
  ViewNewEmpty = { module = 'skg.view_new_empty', fn = 'view_new_empty',
                   desc = 'Open a fresh root node in a chosen source' },
  HeraldsToggle = { module = 'skg.heralds', fn = 'toggle',
                    desc = 'Toggle herald display of metadata', pass_buf = true },
  LimitSourceSet = { module = 'skg.source_sets', fn = 'set_active_source_set',
                     desc = 'Limit display and search to one source-set' },
  ViewMetadata = { module = 'skg.metadata_edit', fn = 'edit_metadata',
                   desc = "Edit the node's metadata as an org tree" },
  ViewOrgAncestry = { module = 'skg.org_ancestry', fn = 'view_org_ancestry',
                      desc = 'Show only the outline ancestry of point' },
  ReadableIdsToggle = { module = 'skg.readable_ids', fn = 'toggle',
                        desc = 'Toggle shortened, title-annotated UUIDs' },
  ViewIdStack = { module = 'skg.linkstack', fn = 'view_id_stack',
                  desc = 'View and edit the linkstack' },
  ViewWithoutMetadata = { module = 'skg.metadata', fn = 'view_without_metadata',
                          desc = 'Copy the selection, metadata stripped, to a new buffer' },
  IdNext = { module = 'skg.id_search', fn = 'id_next',
             desc = 'Hop to the next id (metadata or inline link)' },
  IdPrev = { module = 'skg.id_search', fn = 'id_prev',
             desc = 'Hop to the previous id' },
  IdPush = { module = 'skg.linkstack', fn = 'id_push',
             desc = 'Push a link to the node at point onto the linkstack' },
  PasteId = { module = 'skg.linkstack', fn = 'paste_id',
              desc = 'Insert the top-of-stack id at the cursor' },
  PasteLink = { module = 'skg.linkstack', fn = 'paste_link',
                desc = 'Insert a link to the top-of-stack node' },
  PasteNode = { module = 'skg.linkstack', fn = 'paste_node',
                desc = 'Insert an indefinitive view of the top-of-stack node' },
  PopId = { module = 'skg.linkstack', fn = 'pop_id',
            desc = 'Paste the top-of-stack id and pop it' },
  PopLink = { module = 'skg.linkstack', fn = 'pop_link',
              desc = 'Paste a link to the top-of-stack node and pop it' },
  PopNode = { module = 'skg.linkstack', fn = 'pop_node',
              desc = 'Paste the top-of-stack node and pop it' },
  GitAddIfNewRecursive = { module = 'skg.git_add', fn = 'git_add_if_new_recursive',
                           desc = "git add this subtree's new .skg files" },
  GitAddIfNewRecursivePreview = { module = 'skg.git_add',
                                  fn = 'git_add_if_new_recursive_preview',
                                  desc = 'Preview the git add script for this subtree' },
  StageMoves = { module = 'skg.stage_moves', fn = 'stage_moves',
                 desc = 'Script staging cross-source node moves' },
  DiffReport = { module = 'skg.diff_analysis', fn = 'diff_report',
                 desc = 'Semantic report of git-visible graph changes' },
  ExportToOrg = { module = 'skg.export', fn = 'export_some_to_org',
                  desc = 'Export a source-set to plain .org files' },
  RebuildDbs = { module = 'skg.misc_requests', fn = 'rebuild_dbs',
                 desc = 'Rebuild TypeDB and Tantivy from the .skg files' },
  CloseAllSkgBuffers = { module = 'skg.buffer', fn = 'close_all_skg_buffers',
                         desc = 'Kill every skg view buffer' },
  ListSourceSets = { module = 'skg.source_sets', fn = 'list_source_sets',
                     desc = 'List the configured source-sets' },
  ActiveSourceSet = { module = 'skg.source_sets', fn = 'active_source_set',
                      desc = 'Echo the active source-set' },
  ViewSourceList = { module = 'skg.picker', fn = 'view_source_list',
                     desc = 'List configured sources and their paths' },
  BeginningOfLine = { module = 'skg.metadata', fn = 'beginning_of_line',
                      desc = 'Toggle between line start and title start' },
}

---Invoke command NAME, resolving its module lazily.
---@param name string a key of M.commands
function M.invoke (name)
  local spec = M.commands[name]
  if not spec then error('skg: unknown command ' .. name) end
  local ok, module = pcall(require, spec.module)
  if not ok then
    vim.notify(string.format(
      'skg: %s is not available yet (%s has not landed)',
      name, spec.module))
    return end
  local fn = module[spec.fn]
  if not fn then
    vim.notify(string.format('skg: %s.%s is missing',
                             spec.module, spec.fn))
    return end
  if spec.pass_buf then return fn(0) end
  return fn()
end

---Define every :Skg* Ex command. Called once from plugin/skg.lua.
function M.define_ex_commands ()
  for name, spec in pairs(M.commands) do
    vim.api.nvim_create_user_command('Skg' .. name, function ()
      M.invoke(name)
    end, { desc = spec.desc })
  end
end

---The content-view keybindings: {lhs, command-name} pairs under
---<localleader>, plus motions. The single source of truth for the
---C-c chord translations; docs/COMMANDS-nvim.org is generated to
---match.
M.content_view_bindings = {
  { 'dd', 'Delete' },                    -- C-c <backspace>
  { 'dR', 'DeleteRecursive' },           -- C-u C-c <backspace>
  { 'ff', 'Search' },                    -- C-c f RET
  { 'fi', 'SearchInteractive' },         -- C-c f i
  { 'fl', 'SearchMakeLink' },            -- C-c f l
  { 'gg', 'Goto' },                      -- C-c g RET
  { 'GG', 'GotoAndCloseThis' },          -- C-c G RET
  { 'gb', 'GotoBiggestBranch' },         -- C-c g b
  { 'gi', 'GotoById' },                  -- C-c g i
  { 'Gi', 'GotoByIdAndCloseThis' },      -- C-c G i
  { 'gm', 'GotoInGit' },                 -- C-c g m
  { 'Gm', 'GotoInGitAndCloseThis' },     -- C-c G m
  { 'gM', 'GotoInGitParent' },           -- C-c g M
  { 'GM', 'GotoInGitParentAndCloseThis' }, -- C-c G M
  { 'ca', 'ShowCollectionAliases' },     -- C-c c a
  { 'co', 'ShowCollectionOverrides' },   -- C-c c o
  { 'ch', 'ShowCollectionHides' },       -- C-c c h
  { 'cs', 'ShowCollectionSubscribes' },  -- C-c c s
  { 'pC', 'ShowPathsThroughContainers' }, -- C-c p C
  { 'pL', 'ShowPathsThroughLinkSources' }, -- C-c p L
  { 'pl', 'ShowPathsThroughLinkDests' }, -- C-c p l
  { 'pO', 'ShowPathsThroughOverriders' }, -- C-c p O
  { 'po', 'ShowPathsThroughOverridden' }, -- C-c p o
  { 'pH', 'ShowPathsThroughHiders' },    -- C-c p H
  { 'ph', 'ShowPathsThroughHidden' },    -- C-c p h
  { 'pS', 'ShowPathsThroughSubscribers' }, -- C-c p S
  { 'ps', 'ShowPathsThroughSubscribees' }, -- C-c p s
  { 'sd', 'SetDefinitive' },             -- C-c s d
  { 'si', 'SetIndefinitive' },           -- C-c s i
  { 'sm', 'SetMergeRequest' },           -- C-c s m
  { 'ss', 'SetSource' },                 -- C-c s s
  { 'sS', 'SetSourceRecursive' },        -- C-c s S
  { 'mc', 'ReplaceLinkWithContent' },    -- C-c m c
  { 'mf', 'Fork' },                      -- C-c m f
  { 'ml', 'ReplaceContentWithLink' },    -- C-c m l
  { 'vd', 'ViewDiffMode' },              -- C-c v d
  { 've', 'ViewNewEmpty' },              -- C-c v e
  { 'vh', 'HeraldsToggle' },             -- C-c v h
  { 'vl', 'LimitSourceSet' },            -- C-c v l
  { 'vm', 'ViewMetadata' },              -- C-c v m
  { 'vo', 'ViewOrgAncestry' },           -- C-c v o
  { 'vr', 'ReadableIdsToggle' },         -- C-c v r
  { 'vs', 'ViewIdStack' },               -- C-c v s
  { 'vw', 'ViewWithoutMetadata' },       -- C-c v w
  { 'in', 'IdNext' },                    -- C-c i n
  { 'ip', 'IdPrev' },                    -- C-c i p
  { 'u', 'IdPush' },                     -- C-c u
  { 'oi', 'PasteId' },                   -- C-c o i
  { 'ol', 'PasteLink' },                 -- C-c o l
  { 'on', 'PasteNode' },                 -- C-c o n
  { 'Oi', 'PopId' },                     -- C-c O i
  { 'Ol', 'PopLink' },                   -- C-c O l
  { 'On', 'PopNode' },                   -- C-c O n
  { 'tA', 'GitAddIfNewRecursive' },      -- C-c t A
  { 'ta', 'GitAddIfNewRecursivePreview' }, -- C-c t a
  { 'tm', 'StageMoves' },                -- C-c t m
  { 'tr', 'DiffReport' },                -- C-c t r
}

---The subset used by .skg file buffers (the file-minor-mode analog).
M.file_bindings = {
  { 'gg', 'Goto' }, { 'GG', 'GotoAndCloseThis' },
  { 'in', 'IdNext' }, { 'ip', 'IdPrev' },
  { 'oi', 'PasteId' }, { 'ol', 'PasteLink' }, { 'on', 'PasteNode' },
  { 'Oi', 'PopId' }, { 'Ol', 'PopLink' }, { 'On', 'PopNode' },
}

---The subset used by diff-analysis report buffers.
M.diff_analysis_bindings = {
  { 'ff', 'Search' }, { 'fi', 'SearchInteractive' },
  { 'gg', 'Goto' }, { 'GG', 'GotoAndCloseThis' },
  { 'gi', 'GotoById' }, { 'Gi', 'GotoByIdAndCloseThis' },
  { 'gm', 'GotoInGit' }, { 'Gm', 'GotoInGitAndCloseThis' },
  { 'gM', 'GotoInGitParent' }, { 'GM', 'GotoInGitParentAndCloseThis' },
  { 've', 'ViewNewEmpty' }, { 'vl', 'LimitSourceSet' },
  { 'vs', 'ViewIdStack' },
  { 'in', 'IdNext' }, { 'ip', 'IdPrev' },
  { 'u', 'IdPush' },
  { 'oi', 'PasteId' }, { 'ol', 'PasteLink' }, { 'on', 'PasteNode' },
  { 'Oi', 'PopId' }, { 'Ol', 'PopLink' }, { 'On', 'PopNode' },
}

---Apply BINDINGS (localleader pairs) buffer-locally to BUF.
---@param buf integer
---@param bindings table[]
function M.apply_bindings (buf, bindings)
  for _, binding in ipairs(bindings) do
    local lhs, name = binding[1], binding[2]
    vim.keymap.set('n', '<localleader>' .. lhs, function ()
      M.invoke(name)
    end, { buffer = buf, desc = M.commands[name].desc })
  end
end

---Attach the full content-view surface to BUF: the localleader
---bindings, the ]i/[i motions, and the '^' title toggle.
---@param buf integer
function M.attach_content_view (buf)
  M.apply_bindings(buf, M.content_view_bindings)
  vim.keymap.set('n', ']i', function () M.invoke('IdNext') end,
                 { buffer = buf, desc = M.commands.IdNext.desc })
  vim.keymap.set('n', '[i', function () M.invoke('IdPrev') end,
                 { buffer = buf, desc = M.commands.IdPrev.desc })
  vim.keymap.set('n', '^', function () M.invoke('BeginningOfLine') end,
                 { buffer = buf,
                   desc = M.commands.BeginningOfLine.desc })
  vim.keymap.set('n', '<Tab>', 'za',
                 { buffer = buf, desc = 'Toggle the fold at point' })
  vim.keymap.set('n', '<S-Tab>', function ()
    -- Global visibility toggle: all closed <-> all open.
    if vim.wo.foldlevel > 0 then
      vim.wo.foldlevel = 0
    else
      vim.wo.foldlevel = 99 end
  end, { buffer = buf, desc = 'Toggle global fold visibility' })
  vim.keymap.set('n', 'cit', function ()
    require('skg.metadata').todo_cycle(1)
  end, { buffer = buf,
         desc = 'Cycle TODO keyword (metadata-safe)' })
  vim.keymap.set('n', 'ciT', function ()
    require('skg.metadata').todo_cycle(-1)
  end, { buffer = buf,
         desc = 'Cycle TODO keyword backward (metadata-safe)' })
end

---Attach the .skg-file subset to BUF.
---@param buf integer
function M.attach_file_buffer (buf)
  M.apply_bindings(buf, M.file_bindings)
  vim.keymap.set('n', ']i', function () M.invoke('IdNext') end,
                 { buffer = buf, desc = M.commands.IdNext.desc })
  vim.keymap.set('n', '[i', function () M.invoke('IdPrev') end,
                 { buffer = buf, desc = M.commands.IdPrev.desc })
end

---Attach the diff-analysis subset to BUF.
---@param buf integer
function M.attach_diff_analysis (buf)
  M.apply_bindings(buf, M.diff_analysis_bindings)
end

return M
