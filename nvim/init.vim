" Global vim behavior

set number                                                      " Show line numbers
set autoindent                                                  " Auto indent
set mouse=a                                                     " Enable mouse functionality
set showmatch                                                   " Brace matching
set expandtab tabstop=4 shiftwidth=4 smarttab softtabstop=4     " TAB behavior
set completeopt-=preview                                        " No preview window
set title                                                       " Dynamic window title
set clipboard+=unnamedplus                                      " Set global clipboard
set autochdir                                                   " Dynamic pwd
set splitbelow                                                  " Vertical split
set splitright                                                  " Horizontal split


" Plugins

call plug#begin('~/.config/nvim/plugged')
Plug 'https://github.com/tpope/vim-surround'                    " Surrounding ysw)
Plug 'https://github.com/vim-airline/vim-airline'               " Status bar
Plug 'https://github.com/preservim/nerdtree'                    " Directory explorer
Plug 'https://github.com/tpope/vim-commentary'                  " Comments for gcc, gc
Plug 'https://github.com/tc50cal/vim-terminal'                  " Vim terminal
Plug 'https://github.com/terryma/vim-multiple-cursors'          " Multiple cursors
Plug 'https://github.com/preservim/tagbar'                      " Tagbar
Plug 'https://github.com/neoclide/coc.nvim'                     " Code autocompletion
Plug 'rubixninja314/vim-mcfunction'                             " mcfunction syntax
call plug#end()


" Syntax highlighting

filetype plugin on
filetype indent on
syntax on


" NERDTree

let g:NERDTreeMinimalMenu=1                    " Bugfix for menu
let g:NERDTreeDirArrowExpandable='+'
let g:NERDTreeDirArrowCollapsible='~'

function EnterFileManager()
        let g:NERDTreeCustomOpenArgs={'file': {'where': 't', 'reuse': 'all', 'keepopen': 1, 'stay': 0}, 'dir': {}}
        let g:NERDTreeMapOpenInTab='<Nop>'
        let g:NERDTreeMapOpenInTabSilent='<Nop>'
endfunction
function ExitFileManager()
    let g:NERDTreeCustomOpenArgs={'file': {'where': 'p', 'reuse': 'all', 'keepopen': 1, 'stay': 0}, 'dir': {}}
    let g:NERDTreeMapOpenInTab='t'
    let g:NERDTreeMapOpenInTabSilent='T'
endfunction

autocmd WinEnter,BufEnter * if count(@%, "NERD_tree_1")
                            \ | call EnterFileManager()
                            \ | endif
autocmd WinEnter,BufEnter * if !count(@%, "NERD_tree_1")
                            \ | call ExitFileManager()
                            \ | endif


" MCFUNCTION

let g:mcversion='latest'

autocmd WinEnter,BufEnter * if &filetype == "mcfunction"
                            \ | set notermguicolors
                            \ | endif
autocmd WinEnter,BufEnter * if &filetype != "mcfunction" 
                            \ | set termguicolors 
                            \ | endif
autocmd WinEnter,BufEnter *.mcmeta set syntax=json


" NBT

function NBTRead()
    let l:file=@%
    !nbted --print --input % --output %_pp
    tabnew
    execute "edit " . file . "_pp"
    tabprev
    q
endfunction

function NBTWrite()
    !nbted --reverse --input % --output $(echo % | rev | cut -c 4- | rev)
endfunction

autocmd WinEnter,BufEnter *.dat call NBTRead()
autocmd BufWritePost *.dat_pp call NBTWrite()
autocmd QuitPre *.dat_pp !rm %


" Key remappings

" Split-screen navigation
nmap <silent> <A-k> :wincmd k<CR>
nmap <silent> <A-j> :wincmd j<CR>
nmap <silent> <A-h> :wincmd h<CR>
nmap <silent> <A-l> :wincmd l<CR>
nmap <silent> <A-UP> :wincmd k<CR>
nmap <silent> <A-DOWN> :wincmd j<CR>
nmap <silent> <A-LEFT> :wincmd h<CR>
nmap <silent> <A-RIGHT> :wincmd l<CR>

" Tab navigation
nmap <TAB> :tabnext<CR>
nmap <S-TAB> :tabprev<CR>
nmap <BAR> :tabnew<CR>

" File navigation (NERDTree, ls, Tagbar)
nmap <F1> :NERDTreeToggle<CR>
imap <F1> <ESC>:NERDTreeToggle
nmap <F2> :NERDTreeFocus<CR>
nmap <F3> :ls<CR>
nmap <F4> :TagbarToggle<CR>

" Auto-completion menu
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>"
inoremap <silent><expr> <BS> pumvisible() ? "\<C-g>u" : "\<BS>"
inoremap <expr> <TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr> <S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
inoremap <expr> <UP> pumvisible() ? "\<C-n>" : "\<UP>"
inoremap <expr> <DOWN> pumvisible() ? "\<C-p>" : "\<DOWN>"

" Highlight
nmap <C-h> :noh<CR>

" Clipboard
vmap <C-c> "+y
vmap <C-x> "+c
vmap <C-v> c<ESC>"+P
imap <C-v> <C-r><C-o>+

" Select-all
nmap <C-a> gg<HOME>vG<END>
imap <C-a> <ESC><C-a>
vmap a <ESC><C-a>


" Startup hook
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in")
                    \ | cd $HOME
                    \ | NERDTree
                    \ | wincmd l
                    \ | q
                    \ | call EnterFileManager()
                    \ | endif


" Colors

" Master colorscheme
colorscheme dosbox-black

" Per-filetype operator highlighting
" ----------------------------------

" Highlight color
let g:ophigh_color = "green"

" Filetypes to ignore
let g:ophigh_filetypes_to_ignore = {}
function OperatorHighlightIgnore( file_type )
  if get( g:ophigh_filetypes_to_ignore, a:file_type, 1 )
    let g:ophigh_filetypes_to_ignore[ a:file_type ] = 1
  endif
endfunction
call OperatorHighlightIgnore('help')
call OperatorHighlightIgnore('markdown')
call OperatorHighlightIgnore('qf') " This is for the quickfix window
call OperatorHighlightIgnore('conque_term')
call OperatorHighlightIgnore('diff')
call OperatorHighlightIgnore('html')
call OperatorHighlightIgnore('css')
call OperatorHighlightIgnore('less')
call OperatorHighlightIgnore('xml')
call OperatorHighlightIgnore('sh')
call OperatorHighlightIgnore('bash')
call OperatorHighlightIgnore('notes')
call OperatorHighlightIgnore('jinja')
call OperatorHighlightIgnore('mcfunction')

" Main implementation
function HighlightOperators()
    
    if get( g:ophigh_filetypes_to_ignore, &filetype, 0 )
        return
    endif
    
    if &filetype == 'haskell'
        syntax match Operators "\^\|@\|\$\|?\|+\|-\(-\(>\)\@!\|}\)\@!\|\*\|;\|:\|,\|<\|>\|&\||\|!\|\~\|%\|=\|)\|(\|{\(-\)\@!\|}\|\.\|\[\|\]\|/\(/\|*\)\@!" 
    else
        syntax match Operators "\^\|@\|?\|+\|-\|\*\|;\|:\|,\|<\|>\|&\||\|!\|\~\|%\|=\|)\|(\|{\|}\|\.\|\[\|\]\|/\(/\|*\)\@!"
    endif

    exec "hi Operators guifg=" . g:ophigh_color . " gui=NONE"
    exec "hi Operators ctermfg=" . g:ophigh_color . " cterm=NONE"

endfunction

autocmd Syntax * call HighlightOperators()
autocmd ColorScheme * call HighlightOperators()
" ----------------------------------
