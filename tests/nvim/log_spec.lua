-- Tests for nvim/lua/skg/log.lua (no elisp counterpart to mirror;
-- skg-log.el has no test file, but the port earns one since Lua's
-- stack-walking and JSON encoding differ from Emacs's).

local log = require('skg.log')

describe('skg.log', function ()
  local temporary_log_path

  before_each(function ()
    temporary_log_path = vim.fn.tempname()
    log.log_file = temporary_log_path
    log.log_categories = {}
    log.log_min_level = 'info'
  end)

  after_each(function ()
    log.log_file = nil
    os.remove(temporary_log_path)
  end)

  local function logged_lines ()
    local handle = io.open(temporary_log_path, 'r')
    if not handle then return {} end
    local lines = {}
    for line in handle:lines() do table.insert(lines, line) end
    handle:close()
    return lines
  end

  it('writes one decodable JSON object per entry', function ()
    log.log('info', 'tcp', 'received %d bytes', 42)
    local lines = logged_lines()
    assert.are.equal(1, #lines)
    local entry = vim.json.decode(lines[1])
    assert.are.equal('info', entry.level)
    assert.are.equal('tcp', entry.cat)
    assert.are.equal('received 42 bytes', entry.msg)
    assert.is_string(entry.ts)
    assert.is_string(entry.fn)
  end)

  it('is disabled entirely when log_file is nil', function ()
    log.log_file = nil
    log.log('error', 'tcp', 'should not appear')
    assert.are.equal(0, #logged_lines())
  end)

  it('drops entries below the minimum level', function ()
    log.log_min_level = 'warn'
    log.log('info', 'tcp', 'dropped')
    log.log('warn', 'tcp', 'kept')
    log.log('error', 'tcp', 'kept too')
    assert.are.equal(2, #logged_lines())
  end)

  it('filters by category when categories are named', function ()
    log.log_categories = { 'save' }
    log.log('info', 'tcp', 'dropped')
    log.log('info', 'save', 'kept')
    assert.are.equal(1, #logged_lines())
    assert.are.equal('kept',
      vim.json.decode(logged_lines()[1]).msg)
  end)

  it('JSON-escapes message content', function ()
    log.log('info', 'parse', 'quote " backslash \\ newline \n done')
    local entry = vim.json.decode(logged_lines()[1])
    assert.are.equal('quote " backslash \\ newline \n done', entry.msg)
  end)
end)
