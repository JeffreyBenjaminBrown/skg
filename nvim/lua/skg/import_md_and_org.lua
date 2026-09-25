-- Interactive, additive Markdown/Org import. All paths are server-side.
local client = require('skg.client')
local messages = require('skg.messages')
local payload = require('skg.payload')
local picker = require('skg.picker')
local rerender = require('skg.rerender')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}
local response_types = {
  'import-md-and-org-host-mapping-needed',
  'import-md-and-org-preview',
  'import-md-and-org-result',
}

local function cleanup ()
  for _, kind in ipairs(response_types) do
    state.response_handler_map[kind] = nil
  end
end

local function send (fields, input_directory, destination_source)
  client.connect()
  cleanup()
  state.register_response_handler('import-md-and-org-host-mapping-needed',
    function (_text, response)
      cleanup()
      messages.big_nonfatal_message('skg://messages/import-host',
        'Import needs a host root', payload.field_text(response, 'content') or '')
      vim.schedule(function ()
        local ok, host_root = pcall(vim.fn.input,
          'Absolute host path corresponding to input directory (blank leaves links unresolved): ')
        if not ok then
          M.cancel(input_directory, destination_source)
          return end
        send({ action = 'preview', ['input-directory'] = input_directory,
          ['destination-source'] = destination_source,
          ['host-root'] = host_root }, input_directory, destination_source)
      end)
    end, false)
  state.register_response_handler('import-md-and-org-preview',
    function (_text, response)
      cleanup()
      local content = payload.field_text(response, 'content') or ''
      local token = payload.field_text(response, 'approval-token')
      messages.big_nonfatal_message('skg://messages/import-preview',
        'Import preview', content)
      if token then vim.schedule(function ()
        local ok, answer = pcall(vim.fn.confirm,
          'Import exactly this preview?', '&Import\n&Decline', 2)
        if ok and answer == 1 then
          send({ action = 'apply', ['approval-token'] = token },
            input_directory, destination_source)
        else M.cancel(input_directory, destination_source) end
      end) end
    end, false)
  state.register_response_handler('import-md-and-org-result',
    function (_text, response)
      cleanup()
      messages.big_nonfatal_message('skg://messages/import-result',
        'Import result', payload.field_text(response, 'content') or '')
      if payload.field_text(response, 'record-id') then
        vim.schedule(function () rerender.request_rerender_clean_views_after_import() end)
      end
    end, false)
  state.lp_reset()
  local request = { sexpr.pair(sexpr.symbol('request'), 'import md and org') }
  for _, name in ipairs({ 'action', 'input-directory', 'destination-source',
                         'host-root', 'approval-token' }) do
    if fields[name] ~= nil then
      table.insert(request, sexpr.pair(sexpr.symbol(name), fields[name])) end
  end
  local ok, err = pcall(client.send_string, sexpr.to_string(request) .. '\n')
  if not ok then
    cleanup()
    error(err)
  end
end

function M.cancel (input_directory, destination_source)
  send({ action = 'cancel' }, input_directory, destination_source)
end

function M.import_md_and_org (input_directory, destination_source)
  if not input_directory then
    input_directory = vim.fn.input('Input directory on server (absolute path): ') end
  if input_directory == '' then return end
  if not destination_source then
    vim.notify('Choose an owned source; it determines privacy for every imported node.')
  end
  destination_source = destination_source or picker.prompt_for_owned_source()
  if not destination_source then return end
  send({ action = 'preview', ['input-directory'] = input_directory,
    ['destination-source'] = destination_source },
    input_directory, destination_source)
end

return M
