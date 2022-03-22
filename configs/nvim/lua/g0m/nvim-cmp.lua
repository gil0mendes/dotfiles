local utils = require 'g0m.utils'

-- nvim-cmp
-- https://github.com/hrsh7th/nvim-cmp
vim.cmd 'packadd nvim-cmp'

-- TODO: -- Recommended option settings
-- vim.o.completeopt = 'menuone,noselect'
-- vim.o.shortmess   = vim.o.shortmess .. 'c' -- don't show extra message when using completion

-- -- Use <Tab> and <S-Tab> to navigate through popup menu, <CR> to confirm.
-- utils.keymaps { mode = 'i', opts = { 'noremap', 'expr' }, maps = {
--   { '<Tab>'   , [[pumvisible() ? "\<C-n>" : "\<Tab>"]]   },
--   { '<S-Tab>' , [[pumvisible() ? "\<C-p>" : "\<S-Tab>"]] },
--   { '<CR>'    , [[compe#confirm('<CR>')]] },
-- }}

-- -- Completion settings
-- require'compe'.setup {
--   documentation = {
--     border = "rounded",
--   },
--   source = {
--     path = true,
--     buffer = true,
--     calc = true,
--     nvim_lsp = true,
--     nvim_lua = true,

--     -- External plugins
--     tabnine = true,
--   },
-- }
