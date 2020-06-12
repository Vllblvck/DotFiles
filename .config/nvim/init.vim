" KEYBINDS CHEAT SHEET 
" <leader>d: go to definition
" K: check documentation of class or method
" <leader>n: show the usage of a name in current file
" <leader>r: rename a name
" <leader>cc and <leader>cu
" <C-T> toggle nert tree 


call plug#begin('~/.local/share/nvim/site/autoload/')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' } "Auto completion
Plug 'scrooloose/nerdcommenter' "Auto comment
Plug 'scrooloose/nerdtree' "File tree
Plug 'morhetz/gruvbox' "Color scheme
Plug 'neomake/neomake' "Code checking
Plug 'vim-airline/vim-airline' "Status bar

"Python plugins
Plug 'zchee/deoplete-jedi', { 'for': 'python' } " Required by autocompletion
Plug 'jiangmiao/auto-pairs', { 'for': 'python' } "Automatic pairs for ( and etc.
Plug 'sbdchd/neoformat', { 'for': 'python' } "Automatic code formatting
Plug 'davidhalter/jedi-vim', { 'for': 'python' } "Used for go to

"LaTeX plugins
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }

call plug#end()

"Latex pdf preview
let g:livepreview_previewer = 'apvlv' "LLPStartPreview

"Neomake stuff
let g:neomake_python_enabled_makers = ['flake8']
let g:neomake_python_flake8_exe = '/home/vllblvck/.local/bin/flake8'
autocmd VimEnter * call neomake#configure#automake('nrwi', 500)

"Auto completion stuff
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" Disable autocompletion, cause I use deoplete for completion
let g:jedi#completions_enabled = 0
" Open the go-to function in split
let g:jedi#use_splits_not_buffers = "right"

" Moving between splits
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"Launching Nerd tree
nnoremap <C-T> :NERDTreeToggle<CR>

" Colorscheme
colorscheme gruvbox

"Always yanking to clipboard
set clipboard+=unnamedplus

"Line numbers
set number
