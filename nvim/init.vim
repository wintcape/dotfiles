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
Plug 'https://github.com/tpope/vim-surround'                    " Surround
Plug 'https://github.com/vim-airline/vim-airline'               " Status bar
Plug 'https://github.com/preservim/nerdtree'                    " Directory explorer
Plug 'https://github.com/tpope/vim-commentary'                  " Comments for gcc, gc
Plug 'https://github.com/tc50cal/vim-terminal'                  " Vim terminal
Plug 'https://github.com/preservim/tagbar'                      " Tagbar
Plug 'https://github.com/neoclide/coc.nvim'                     " Code autocompletion
Plug 'rubixninja314/vim-mcfunction'                             " mcfunction syntax
call plug#end()




" Syntax highlighting

filetype plugin on
filetype indent on
syntax on




" NERDTree configuration


" Global behavior

let g:NERDTreeMinimalMenu=1                    " Bugfix for menu
let g:NERDTreeDirArrowExpandable='+'
let g:NERDTreeDirArrowCollapsible='~'


" First NERDTree window (see startup hook) acts as a file browser / opener

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




" Syntax editing for Minecraft filetypes

" *.mcfunction
let g:mcversion='latest'
autocmd WinEnter,BufEnter * if &filetype == "mcfunction"
                            \ | set notermguicolors
                            \ | endif
autocmd WinEnter,BufEnter * if &filetype != "mcfunction" 
                            \ | set termguicolors 
                            \ | endif

" *.mcmeta
autocmd WinEnter,BufEnter *.mcmeta set syntax=json


" *.dat (read / write NBT data)

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




" Vim surround

" Default keybindings are left untouched:
"     ysw<symbol>  to encase a single character within <symbol>.
"     ysiw<symbol> to encase a single word within <symbol>.




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


" Auto-completion menu (coc)

" <Tab>:   completion next
" <S-Tab>: completion back
" <CR>:    confirm completion
" <BS>:    cancel completion
inoremap <silent><expr> <Tab> pumvisible() ? "\<C-N>" : "\<Tab>"
inoremap <silent><expr> <S-Tab> pumvisible() ? "\<C-P>" : "\<C-H>"
inoremap <expr> <CR> pumvisible() ? coc#_select_confirm() : "\<CR>"

" Clear highlight
nmap <C-h> :noh<CR>


" Global system clipboard (xclip)

" When in visual mode:
"     Ctrl+c: copy selection to global clipboard (single item only)
"     Ctrl+x: cut selection to global clipboard (single item only)
vmap <C-c> "+y
vmap <C-x> "+c

" When in insert mode:
"     Ctrl+v: paste from global clipboard
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
