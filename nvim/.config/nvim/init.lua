-- https://github.com/WhoIsSethDaniel/toggle-lsp-diagnostics.nvim

vim.cmd([[
  set runtimepath^=~/.vim runtimepath+=~/.vim/after
  let &packpath=&runtimepath
  source ~/.vim/vimrc

  set guicursor=i:block

  let g:vimsyn_embed= 'l'

  nnoremap <Leader>fei :e ~/.config/nvim/init.lua<CR>
  nnoremap <silent> <Leader>gh :DiffviewFileHistory<CR>
  nnoremap <silent> <Leader>tr :TroubleToggle document_diagnostics<CR>
  nnoremap - :NvimTreeFindFileToggle<CR>zz
  nnoremap <Leader>lt :LspStop<CR>
  nnoremap <Leader>ls :LspStart<CR>
  nnoremap <Leader>ch :Chat
  vnoremap <Leader>ch :Chat
]])

require("trouble").setup {
  auto_jump = {}
}

-- Last I checked this plugin was pretty broken, unfortunate.
-- require("chatgpt").setup({})

require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
    disable = { "python" },
  },
  playground = {
    enable = true,
  },
}

require("nvim-tree").setup{
  disable_netrw = false,
  hijack_netrw = false,
  view = {
    width = 50,
    mappings = {
      list = {
        { key = "<C-k>", action = "" }
      }
    }
  },
  actions = {
    open_file = {
      quit_on_open = false,
      window_picker = {
        enable = false,
      }
    }
  }
}

require("stabilize").setup()
require("diffview").setup()
require("gitlinker").setup()

local nvim_lsp = require('lspconfig')
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap=true, silent=true }

  -- buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'g', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  -- buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>fr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>ft', '<cmd>lua vim.lsp.buf.workspace_symbol()<CR>', opts)
  -- buf_set_keymap("n", "<space>=", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  buf_set_keymap('n', 'gtd', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rf', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  -- TODO Fix for python
  -- vim.keymap.set('n', '<space>=', function() vim.lsp.buf.format { async = true } end, opts)
end

local servers = {
  "jsonls",
  "tsserver",
  "pyright",
  "gopls",
  "rust_analyzer",
}

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  }
end

require'lspconfig'.gopls.setup{
  on_attach = on_attach
}

require'lspconfig'.pyright.setup{
  on_attach = on_attach,
  settings = {
    python = {
      analysis = {
        diagnosticMode = "openFilesOnly",
      }
    }
  }
}

-- require'lspconfig'.tsserver.setup{
--     on_attach = on_attach,
--     cmd = {"node", "--max-old-space-size=12288", "/opt/homebrew/bin/typescript-language-server", "--tsserver-path", "/Users/chris.laverdiere/go/src/github.com/DataDog/web-ui/tsconfig.json", "--stdio"},
-- }

-- require('lspconfig').jsonls.setup {
--   on_attach = on_attach,
--   settings = {
--     json = {
--       schemas = require('schemastore').json.schemas(),
--     },
--   },
-- }

require'lspconfig'.sumneko_lua.setup {
  on_attach = on_attach,
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}

-- require("null-ls").setup({
--     debug = true,
--     sources = {
--     require("null-ls").builtins.diagnostics.vale.with({
--       filetypes = { "text", "vimwiki", "markdown" },
--     }),
--   },
-- })

vim.diagnostic.config({virtual_text = false, signs = true, severity_sort = true})

local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.o.updatetime = 250
vim.api.nvim_create_autocmd("CursorHold", {
  buffer = bufnr,
  callback = function()
    local opts = {
      focusable = false,
      close_events = { "BufLeave", "CursorMoved", "InsertEnter", "FocusLost" },
      border = 'rounded',
      source = 'always',
      prefix = ' ',
      scope = 'cursor',
    }
    vim.diagnostic.open_float(nil, opts)
  end
})
