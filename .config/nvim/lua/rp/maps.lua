local function map(m, k, v)
	vim.keymap.set(m, k, v, { silent = true })
end

-- Keybindings for window split
map("n", "<leader>ws", "<CMD>vsplit<CR>")
map("n", "<leader>hs", "<CMD>split<CR>")
-- Keybindings for tabs
map("n", "<leader>tt", "<CMD>tabnew<CR>")
map("n", "<leader>nn", "<CMD>tabnext<CR>")
map("n", "<leader>tc", "<CMD>tabclose<CR>")

-- latex
map('n', '<leader>mc',':!pandoc %  --pdf-engine=xelatex --highlight-style pygments -N -o %:r.pdf<CR>',opts)
map('n', '<leader>lc',':!pdflatex %:r.tex<CR>',opts)
map('n', '<leader>lr',':!rm *.aux *.log<CR>',opts)
map('n', '<leader>lp',':!zathura %:r.pdf & <CR>',opts)
map("n", "hh", "gqq",opts)
