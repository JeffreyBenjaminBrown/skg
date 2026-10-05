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
  before_each(function () state.linkstack = {} end)
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
      local before = #state.linkstack
      linkstack.id_push()
      assert.are.equal(before + 1, #state.linkstack)
      assert.are.equal(id, state.linkstack[1][1])
    end
    -- From the title area, still finds the metadata id.
    cursor_on('just a title', 3)
    linkstack.id_push()
    assert.are.same({ '6', 'just a title' }, state.linkstack[1])
    -- Fake metadata must not push.
    local before = #state.linkstack
    cursor_on('fake metadata', 2)
    linkstack.id_push()
    assert.are.equal(before, #state.linkstack)
  end)

  it('pushes the inline link when point is on one', function ()
    buffer_with('* (skg (node (id outer-id))) outer with'
                .. ' [[id:inner-id][inner label]] inside')
    cursor_on('inner label', 3)
    linkstack.id_push()
    assert.are.equal(1, #state.linkstack)
    assert.are.same({ 'inner-id', 'inner label' }, state.linkstack[1])
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
    assert.are.equal(2, #state.linkstack)
    assert.are.same({ 'b', 'b has a [[id:a][link to a]]' },
                    state.linkstack[1])
    assert.are.same({ 'a', 'a' }, state.linkstack[2])
  end)
end)

describe('skg.linkstack paste and pop', function ()
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('paste_node inserts a write-protected node without popping',
     function ()
    state.linkstack = { { 'id-1', 'Title from stack' } }
    buffer_with('')
    linkstack.paste_node()
    assert.are.equal(
      '* (skg (node (id id-1) writeProtected)) Title from stack\n',
      buffer_text())
    assert.are.same({ { 'id-1', 'Title from stack' } },
                    state.linkstack)
  end)

  it('pop_node inserts and pops', function ()
    state.linkstack = { { 'id-2', 'Second' }, { 'id-1', 'First' } }
    buffer_with('')
    linkstack.pop_node()
    assert.are.equal('* (skg (node (id id-2) writeProtected)) Second\n',
                     buffer_text())
    assert.are.same({ { 'id-1', 'First' } }, state.linkstack)
  end)

  ---Paste id-1 at the end of a view containing EXISTING_LINE.
  ---@return string buffer text, string|nil last notification
  local function paste_node_in_view (existing_line)
    state.linkstack = { { 'id-1', 'Title from stack' } }
    buffer_with(existing_line .. '\n')
    vim.b.skg_view_uri = 'test-view'
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    local notified = nil
    local real_notify = vim.notify
    vim.notify = function (message) notified = message end
    linkstack.paste_node()
    vim.notify = real_notify
    return buffer_text(), notified
  end

  it('in a view, paste_node requests a definitive view', function ()
    local text, notified = paste_node_in_view(
      '* (skg (node (id id-1) writeProtected)) Elsewhere')
    assert.are.equal(
      '* (skg (node (id id-1) writeProtected)) Elsewhere\n'
      .. '* (skg (node (id id-1) writeProtected'
      .. ' (viewRequests definitiveView))) Title from stack\n',
      text)
    assert.is_nil(notified)
  end)

  it('beside a writable occurrence, paste_node stays write-protected',
     function ()
    local text, notified = paste_node_in_view(
      '* (skg (node (id id-1) (repo main))) Writable')
    assert.are.equal(
      '* (skg (node (id id-1) (repo main))) Writable\n'
      .. '* (skg (node (id id-1) writeProtected)) Title from stack\n',
      text)
    assert.are.equal('NOTE: Pasting node write-protected because a writable'
                     .. ' occurrence is already present in this same buffer.',
                     notified)
  end)

  it('a pending definitive view request counts as writable', function ()
    local text, notified = paste_node_in_view(
      '* (skg (node (id id-1) writeProtected'
      .. ' (viewRequests definitiveView))) First')
    assert.is_truthy(text:find(
      '\n* (skg (node (id id-1) writeProtected)) Title from stack\n',
      1, true))
    assert.are.equal('NOTE:', notified:sub(1, 5))
  end)

  it('paste_node after typed stars inserts only metadata and title',
     function ()
    state.linkstack = { { 'id-1', 'Title from stack' } }
    buffer_with('** ')
    vim.api.nvim_win_set_cursor(0, { 1, 3 })
    linkstack.paste_node()
    assert.are.equal(
      '** (skg (node (id id-1) writeProtected)) Title from stack',
      buffer_text())
  end)

  it('paste_id and pop_id insert the bare id', function ()
    state.linkstack = { { 'top-id', 'Top' }, { 'next-id', 'Next' } }
    buffer_with('')
    linkstack.paste_id()
    assert.are.equal('top-id', buffer_text())
    linkstack.pop_id()
    assert.are.same({ { 'next-id', 'Next' } }, state.linkstack)
  end)
end)

describe('skg.linkstack stack buffer', function ()
  after_each(function ()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_get_name(buf) == 'skg://linkstack' then
        pcall(vim.api.nvim_buf_delete, buf, { force = true })
      end
    end
  end)

  it('formats the stack as org, head first', function ()
    state.linkstack = {}
    assert.are.equal('', linkstack.format_linkstack_as_org())
    state.linkstack = { { 'id-1', 'label one' } }
    assert.are.equal('* label one\nid-1',
                     linkstack.format_linkstack_as_org())
    state.linkstack = { { 'id-2', 'second' }, { 'id-1', 'first' } }
    assert.are.equal('* second\nid-2\n* first\nid-1',
                     linkstack.format_linkstack_as_org())
  end)

  it('validates good stack buffers', function ()
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false,
      { '* the label', '  the-id', '* label 2', '  id-2' })
    local ok, result = linkstack.validate_linkstack_buffer(buf)
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
      local ok = linkstack.validate_linkstack_buffer(buf)
      assert.is_false(ok, vim.inspect(lines))
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end)

  it('accepts empty and whitespace-only buffers as an empty stack',
     function ()
    for _, lines in ipairs({ { '' }, { ' ', '' }, { '', '' } }) do
      local buf = vim.api.nvim_create_buf(true, false)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
      local ok, result = linkstack.validate_linkstack_buffer(buf)
      assert.is_true(ok)
      assert.are.same({}, result)
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end)

  it('view_linkstack opens the editable stack; :w updates the stack',
     function ()
    state.linkstack = { { 'uuid-123', 'My Node' } }
    linkstack.view_linkstack()
    local buf = vim.api.nvim_get_current_buf()
    assert.are.equal('skg://linkstack',
                     vim.api.nvim_buf_get_name(buf))
    assert.are.equal('* My Node\nuuid-123',
      table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false),
                   '\n'))
    vim.api.nvim_buf_set_lines(buf, 0, -1, false,
      { '* new label', 'new-id', '* another', 'another-id' })
    vim.cmd('write')
    assert.are.same({ { 'new-id', 'new label' },
                      { 'another-id', 'another' } }, state.linkstack)
    assert.is_false(vim.bo[buf].modified)
  end)
end)
