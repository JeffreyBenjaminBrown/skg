-- Mirrors the linkstack half of tests/elisp/test-skg-id-search.el.

local linkstack = require('skg.linkstack')
local state = require('skg.state')

local function buffer_with (text)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, '\n'))
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  return buf
end

local function buffer_text ()
  return table.concat(
    vim.api.nvim_buf_get_lines(0, 0, -1, false), '\n')
end

---Place the cursor on the first occurrence of NEEDLE (plus OFFSET).
local function cursor_on (needle, offset)
  for line = 1, vim.api.nvim_buf_line_count(0) do
    local text = vim.api.nvim_buf_get_lines(0, line - 1, line,
                                            false)[1]
    local start = text:find(needle, 1, true)
    if start then
      vim.api.nvim_win_set_cursor(0,
        { line, start - 1 + (offset or 0) })
      return
    end
  end
  error('needle not found: ' .. needle)
end

describe('skg.linkstack push', function ()
  before_each(function () state.id_stack = {} end)
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('pushes the metadata id from anywhere on the line', function ()
    buffer_with(table.concat({
      '* (skg (node (id 1))) [[id:2][link to 2]]',
      '* (skg (node (id 3))) [[id:2][link to 4]] hello',
      '* (skg (fake metadata]] [[id:fake-link)(]]',
      '* (skg (node (id 6))) just a title' }, '\n'))
    for _, id in ipairs({ '1', '3', '6' }) do
      cursor_on(string.format('(id %s)', id), 2)
      local before = #state.id_stack
      linkstack.id_push()
      assert.are.equal(before + 1, #state.id_stack)
      assert.are.equal(id, state.id_stack[1][1])
    end
    -- From the title area, still finds the metadata id.
    cursor_on('just a title', 3)
    linkstack.id_push()
    assert.are.same({ '6', 'just a title' }, state.id_stack[1])
    -- Fake metadata must not push.
    local before = #state.id_stack
    cursor_on('fake metadata', 2)
    linkstack.id_push()
    assert.are.equal(before, #state.id_stack)
  end)

  it('pushes the inline link when point is on one', function ()
    buffer_with('* (skg (node (id outer-id))) outer with'
                .. ' [[id:inner-id][inner label]] inside')
    cursor_on('inner label', 3)
    linkstack.id_push()
    assert.are.equal(1, #state.id_stack)
    assert.are.same({ 'inner-id', 'inner label' }, state.id_stack[1])
  end)

  it('stacks pushes most-recent-first', function ()
    buffer_with(table.concat({
      '* (skg (node (id a))) a',
      '** (skg (node (id b))) b has a [[id:a][link to a]]',
      '* (skg (node (id c))) c' }, '\n'))
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
    linkstack.id_push()
    -- From the title area of line 2, off the inline link. (The elisp
    -- test stood at end-of-line, a past-the-last-character position
    -- normal mode does not have; on the link's final bracket the
    -- inline link would win here.)
    vim.api.nvim_win_set_cursor(0, { 2, 24 })
    linkstack.id_push()
    assert.are.equal(2, #state.id_stack)
    assert.are.same({ 'b', 'b has a [[id:a][link to a]]' },
                    state.id_stack[1])
    assert.are.same({ 'a', 'a' }, state.id_stack[2])
  end)
end)

describe('skg.linkstack paste and pop', function ()
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('paste_node inserts an indefinitive node without popping',
     function ()
    state.id_stack = { { 'id-1', 'Title from stack' } }
    buffer_with('')
    linkstack.paste_node()
    assert.are.equal(
      '* (skg (node (id id-1) indef)) Title from stack\n',
      buffer_text())
    assert.are.same({ { 'id-1', 'Title from stack' } },
                    state.id_stack)
  end)

  it('pop_node inserts and pops', function ()
    state.id_stack = { { 'id-2', 'Second' }, { 'id-1', 'First' } }
    buffer_with('')
    linkstack.pop_node()
    assert.are.equal('* (skg (node (id id-2) indef)) Second\n',
                     buffer_text())
    assert.are.same({ { 'id-1', 'First' } }, state.id_stack)
  end)

  it('paste_node after typed stars inserts only metadata and title',
     function ()
    state.id_stack = { { 'id-1', 'Title from stack' } }
    buffer_with('** ')
    vim.api.nvim_win_set_cursor(0, { 1, 3 })
    linkstack.paste_node()
    assert.are.equal(
      '** (skg (node (id id-1) indef)) Title from stack',
      buffer_text())
  end)

  it('paste_id and pop_id insert the bare id', function ()
    state.id_stack = { { 'top-id', 'Top' }, { 'next-id', 'Next' } }
    buffer_with('')
    linkstack.paste_id()
    assert.are.equal('top-id', buffer_text())
    linkstack.pop_id()
    assert.are.same({ { 'next-id', 'Next' } }, state.id_stack)
  end)
end)

describe('skg.linkstack stack buffer', function ()
  after_each(function ()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_get_name(buf) == 'skg://id-stack' then
        pcall(vim.api.nvim_buf_delete, buf, { force = true })
      end
    end
  end)

  it('formats the stack as org, head first', function ()
    state.id_stack = {}
    assert.are.equal('', linkstack.format_id_stack_as_org())
    state.id_stack = { { 'id-1', 'label one' } }
    assert.are.equal('* label one\nid-1',
                     linkstack.format_id_stack_as_org())
    state.id_stack = { { 'id-2', 'second' }, { 'id-1', 'first' } }
    assert.are.equal('* second\nid-2\n* first\nid-1',
                     linkstack.format_id_stack_as_org())
  end)

  it('validates good stack buffers', function ()
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false,
      { '* the label', '  the-id', '* label 2', '  id-2' })
    local ok, result = linkstack.validate_id_stack_buffer(buf)
    assert.is_true(ok)
    assert.are.same({ { 'the-id', 'the label' },
                      { 'id-2', 'label 2' } }, result)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  it('rejects the invalid stack-buffer shapes', function ()
    local cases = {
      { 'Hello!', '* okay', '  okay-id' },          -- text before
      { '* ', '  okay-id' },                        -- empty title
      { '* okay', '  okay-id', '* label without ID',
        '* okay', '  okay-id' },                    -- no body
      { '* okay', '  okay-id', '* too much', '  an-id',
        '  extra', '* okay', '  okay-id' },         -- multi-line body
    }
    for _, lines in ipairs(cases) do
      local buf = vim.api.nvim_create_buf(true, false)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
      local ok = linkstack.validate_id_stack_buffer(buf)
      assert.is_false(ok, vim.inspect(lines))
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end)

  it('accepts empty and whitespace-only buffers as an empty stack',
     function ()
    for _, lines in ipairs({ { '' }, { ' ', '' }, { '', '' } }) do
      local buf = vim.api.nvim_create_buf(true, false)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
      local ok, result = linkstack.validate_id_stack_buffer(buf)
      assert.is_true(ok)
      assert.are.same({}, result)
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end)

  it('view_id_stack opens the editable stack; :w updates the stack',
     function ()
    state.id_stack = { { 'uuid-123', 'My Node' } }
    linkstack.view_id_stack()
    local buf = vim.api.nvim_get_current_buf()
    assert.are.equal('skg://id-stack',
                     vim.api.nvim_buf_get_name(buf))
    assert.are.equal('* My Node\nuuid-123',
      table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false),
                   '\n'))
    vim.api.nvim_buf_set_lines(buf, 0, -1, false,
      { '* new label', 'new-id', '* another', 'another-id' })
    vim.cmd('write')
    assert.are.same({ { 'new-id', 'new label' },
                      { 'another-id', 'another' } }, state.id_stack)
    assert.is_false(vim.bo[buf].modified)
  end)
end)
