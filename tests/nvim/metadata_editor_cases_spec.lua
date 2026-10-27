-- Mirrors tests/elisp/test-skg-metadata-editor-cases.el: the cases
-- both read are in tests/shared/metadata-editor-cases.txt.

local fields = require('skg.sexpr.metadata_fields')
local metadata_edit = require('skg.metadata_edit')
local readiness = require('skg.readiness')
local server_warnings = require('skg.server_warnings')
local sexpr = require('skg.sexpr.parse')

---Each case of the file, which the Emacs tests read too: a table from
---each field's key to its value, a string; a block's lines are joined
---by newlines. The key 'name' holds the case's name.
---@return table[]
local function shared_cases ()
  local path = _G.skg_test_repo_root()
    .. '/tests/shared/metadata-editor-cases.txt'
  local cases, current, block_key = {}, nil, nil
  for line in io.lines(path) do
    if line:sub(1, 5) == '==== ' then
      current = { name = line:sub(6) }
      table.insert(cases, current)
      block_key = nil
    elseif line:sub(1, 5) == '---- ' then
      local key, value = line:match('^%-%-%-%- ([^:]+):(.*)$')
      if value == '' then
        block_key = key
        current[key] = ''
      else
        block_key = nil
        current[key] = value:sub(2) end
    elseif block_key and line ~= '' then
      current[block_key] = current[block_key] == '' and line
        or current[block_key] .. '\n' .. line
    end
  end
  return cases
end

describe('the metadata editor', function ()
  it('passes the cases shared with Emacs', function ()
    local cases = shared_cases()
    assert.is_true(#cases > 10)
    for _, case in ipairs(cases) do
      if case.metadata then
        local sexp = sexpr.read(case.metadata)
        local shown = metadata_edit.org_text_to_show(sexp, case.title)
        assert.are.same({ case.name, case['fill-in'] },
                        { case.name, shown })
        assert.are.same(
          { case.name, case.strip },
          { case.name, metadata_edit.metadata_text_to_save(
              shown, fields.kind_of_sexp(sexp)) })
        for _, writer in ipairs({
            { 'edit request', 'with the edit request',
              readiness.with_edit_request },
            { 'readiness request', 'with the readiness request',
              readiness.with_readiness_request } }) do
          local request = case[writer[1]]
          if request then
            assert.are.same(
              { case.name, case[writer[2]] },
              { case.name, sexpr.to_string(
                  writer[3](sexpr.read(case.metadata),
                            sexpr.read(request))) })
          end
        end
      end
      if case.corrections then
        assert.are.same(
          { case.name, case.display },
          { case.name, server_warnings.corrections_to_org_string(
              sexpr.read(case.corrections)) })
      end
    end
  end)
end)
