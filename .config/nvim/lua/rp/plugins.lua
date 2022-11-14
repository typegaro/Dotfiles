
local status, packer = pcall(require, "packer")
if not status then
	print("Packer is not installed")
	return
end

-- Reloads Neovim after whenever you save plugins.lua
vim.cmd([[
    augroup packer_user_config
      autocmd!
     autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup END
]])

packer.startup(function(use)
	-- Packer can manage itself
	use("wbthomason/packer.nvim")

    use 'NTBBloodbath/doom-one.nvim'
	-- Dashboard is a nice start screen for nvim
	use("glepnir/dashboard-nvim")
	
    -- Productivity
	use("nvim-lualine/lualine.nvim") -- A better statusline

	-- File management --
	use("scrooloose/nerdtree")
	use("tiagofumo/vim-nerdtree-syntax-highlight")
	use("ryanoasis/vim-devicons")


	-- Syntax Highlighting and Colors --
	use("PotatoesMaster/i3-vim-syntax")
	use("kovetskiy/sxhkd-vim")
	use("ap/vim-css-color")

	-- Junegunn Choi Plugins --
	use("junegunn/goyo.vim")
	use("junegunn/limelight.vim")
	use("junegunn/vim-emoji")

    -- Style --
    --use {'akinsho/bufferline.nvim', tag = "v3.*", 
    --    requires = 'nvim-tree/nvim-web-devicons'}
	
    -- coding  --
    use {'neoclide/coc.nvim', branch = 'release'}
	use {
        'nvim-treesitter/nvim-treesitter',
        run = function()
            local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
            ts_update()
        end,
    }
    use("axvr/org.vim")
    if packer_bootstrap then
		packer.sync()
	end
end)
require('lualine').setup{}
--require("bufferline").setup{
--    options = {
--        mode = "tabs",
--    }
--}

