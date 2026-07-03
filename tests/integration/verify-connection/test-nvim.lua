-- Integration test for skg verify-connection, nvim client.
-- The Lua mirror of test-emacs.el in this directory.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(10)

print('Starting integration test...')
require('skg.misc_requests').connection_verify()
print('Sent verify connection request...')

if T.wait_for_response(10) then
  T.pass('PASS: Integration test successful!')
else
  T.fail('TIMEOUT: No response received!')
end
