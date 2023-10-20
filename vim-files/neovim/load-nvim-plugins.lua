-- ***************************
-- * Nvim Tree Configuration!
-- ***************************

-- Disable netrw at the very start of your init.lua
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- Set termguicolors to enable highlight groups
vim.opt.termguicolors = true

-- Customized setup!
require("nvim-tree").setup({
    actions =
    {
        open_file =
        {
            resize_window = false
        }
    }
})

-- ***************************
-- * Neogit Configuration!
-- ***************************

require("neogit").setup({
    disable_insert_on_commit = "auto",
    auto_refresh = false,
    kind = "vsplit",
})

