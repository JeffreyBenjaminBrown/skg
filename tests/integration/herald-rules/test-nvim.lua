-- Integration test for the herald-rules fetch path, nvim client.
-- Fetches the herald rule table from a running server and checks
-- that it lands in the heralds cache and drives the lens engine.
--
-- The Lua mirror of test-emacs.el in this directory.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(15)

local herald_rules = require('skg.herald_rules')
local heralds = require('skg.heralds')
local sexpr = require('skg.sexpr.parse')

print('Starting herald-rules integration test...')
herald_rules.request_herald_rules()
print('Sent herald rules request...')

if not T.wait_for_response(10) then
  T.fail('TIMEOUT: No response received!')
end

local rules = herald_rules.get_rules()
if not (sexpr.is_list(rules) and rules[1] == sexpr.symbol('skg')) then
  T.fail('rule table not installed: ' .. vim.inspect(rules))
end

-- The fetched table should drive the lens engine end to end.
local chunks = heralds.chunks_from_metadata(
  '(skg (node (id 1) (source main) indef))')
local herald_text = chunks and heralds.chunks_text(chunks) or nil

if herald_text and herald_text:find('☮', 1, true) then
  T.pass('PASS: Integration test successful!')
else
  T.fail('fetched table did not produce the indef herald: '
         .. tostring(herald_text))
end
