call plug#begin('~/.local/share/nvim/site/autoload/')

"Misc plugins
Plug 'Shougo/deoplete.nvim', { 'for': ['python', 'tex'], 'do': ':UpdateRemotePlugins' }
Plug 'scrooloose/nerdcommenter' "Auto comment <leader>cc and <leader>cu
Plug 'neomake/neomake'
Plug 'jiangmiao/auto-pairs'
Plug 'sbdchd/neoformat'
Plug 'scrooloose/nerdtree'

"Python plugins
Plug 'zchee/deoplete-jedi', { 'for': 'python' }
Plug 'tmhedberg/SimpylFold', { 'for': 'python' }

"LaTeX plugins
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }

call plug#end()

"Neomake automatic run on write
let g:neomake_python_enabled_makers = ['pylint']
call neomake#configure#automake('w')

"Auto completion stuff
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

"Latex pdf preview
let g:livepreview_previewer = 'apvlv' "LLPStartPreview

"Always yanking to clipboard
set clipboard+=unnamedplus
