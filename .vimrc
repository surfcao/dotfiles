set nocompatible              " be iMproved, required
filetype off                  " required

"""""""""""""""""""""""""""""""""""""""""""""""""""""
""   Plug
""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Install vim-plug if we don't already have it
if empty(glob("~/.vim/autoload/plug.vim"))
    "" Ensure all needed directories exist  (Thanks @kapadiamush)
    silent execute "!mkdir -p ~/.vim/plugged"
    silent execute "!mkdir -p ~/.vim/autoload"
    " Download the actual plugin manager
    execute "!curl -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim"
endif

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'reedes/vim-pencil'
"Plug 'ctrlpvim/ctrlp.vim'
Plug 'vim-scripts/Solarized'
"Plug 'morhetz/gruvbox'
Plug 'ervandew/supertab'
let g:SuperTabDefaultCompletionType = '<C-n>'
let g:SuperTabCrMapping = 0

Plug 'SirVer/ultisnips', {'for': ['r','tex']}
Plug 'honza/vim-snippets', {'for': ['r','tex']}
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<s-tab>'
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

"Plug 'Valloric/YouCompleteMe'
""" make YCM compatible with UltiSnips (using supertab)
"let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
"let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
"if !exists('g:ycm_semantic_triggers')
"    let g:ycm_semantic_triggers = {}
"endif
"let g:ycm_semantic_triggers.tex = [
"        \ 're!\\[A-Za-z]*cite[A-Za-z]*(\[[^]]*\]){0,2}{[^}]*',
"        \ 're!\\[A-Za-z]*ref({[^}]*|range{([^,{}]*(}{)?))',
"        \ 're!\\hyperref\[[^]]*',
"        \ 're!\\includegraphics\*?(\[[^]]*\]){0,2}{[^}]*',
"        \ 're!\\(include(only)?|input){[^}]*',
"        \ 're!\\\a*(gls|Gls|GLS)(pl)?\a*(\s*\[[^]]*\]){0,2}\s*\{[^}]*',
"        \ 're!\\includepdf(\s*\[[^]]*\])?\s*\{[^}]*',
"        \ 're!\\includestandalone(\s*\[[^]]*\])?\s*\{[^}]*',
"        \ 're!\\usepackage(\s*\[[^]]*\])?\s*\{[^}]*',
"        \ 're!\\documentclass(\s*\[[^]]*\])?\s*\{[^}]*',
"\ ]

Plug 'vim-scripts/LanguageTool', {'for': ['tex', 'markdown', 'txt']}
"Plug 'rhysd/vim-grammarous', {'for': ['tex', 'markdown', 'txt']}
Plug 'panozzaj/vim-autocorrect', {'for': ['tex', 'markdown', 'txt']}
Plug 'jalvesaq/Nvim-R', {'for': 'r'}
Plug 'lervag/vimtex', {'for': 'tex'}
let g:vimtex_latexmk_continuous=1
let g:vimtex_latexmk_build_dir='output'
if has('mac') == 0
	let g:vimtex_view_method='zathura' 
endif 
Plug 'vim-pandoc/vim-pandoc', {'for': 'markdown'}
let g:pandoc#modules#disabled = ["folding"]
let g:pandoc#command#latex_engine = 'pdflatex'
let g:pandoc#keyboard#enabled_submodules = ["lists", "references", "sections", "links"]
"let g:pandoc#filetypes#handled = ["pandoc", "markdown"]
Plug 'vim-pandoc/vim-pandoc-syntax', {'for': 'markdown'}
call plug#end()
filetype plugin indent on                   " required!
syntax on

"""""""""""""""""""""""""""""""""""""""""""""""""""""
""   General settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""

" path to search files
set path+=$HOME/Dropbox/work/ttu/gr/**
set path+=$HOME/Git/lab/papers/**
set wildignore+=**/.git/**,*.aux,*.blg,*.dvi,*.log,*.out,*.fls,*.fdb_latexmk
set wildmenu
set showcmd

" put the backup, swp and undo file into the same place
set backupdir=$HOME/.vim/.backup//,/tmp
set directory=$HOME/.vim/.swp//,/tmp
if exists('+undodir')
    set undodir=$HOME/.vim/.undo//,/tmp
    set undofile
endif

set ignorecase
set smartcase
set hlsearch
set lbr
set tabstop=8
set shiftwidth=8
set keywordprg=:help

set tw=75
"set formatoptions+=nt
"default formatoptions=tcq
set formatoptions+=n
autocmd FileType tex,markdown set formatoptions+=a
set wrapmargin=0

"map fm {v}!par -w75<CR> vmap fm !par -w75<CR>
set formatprg=par\ -w75
set incsearch
set wrap
set linebreak

"for macvim
set backspace=indent,eol,start

set spell spelllang=en_us
set spellfile=$HOME/Dropbox/.vim/spell/en.utf-8.add
hi SpellBad ctermbg=22
set spell
" Under latex, this is a workaround only works for non-comments
syntax spell toplevel
set dictionary+=/usr/share/dict/words
set complete+=k
set thesaurus+=$HOME/Dropbox/.vim/thesaurus/mthesaur.txt

set autowriteall

set history=1000
set undolevels=1000
set title "change the terminal's title
set showmatch
set hidden
set noruler "hide the status line
set number
set nonumber " No line numbering

"""""""""""""""""""""""""""""""""""""""""""""""""""""
""   Key Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""
" some shortcuts
let mapleader = "\<Space>"
let maplocalleader = ","
nmap <Leader>q :nohlsearch<CR>
nnoremap <Leader>w :w<CR>
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P
nmap <Leader>q :nohlsearch<CR>
" buffer switch
nnoremap <LocalLeader>b :ls<CR>
nnoremap <LocalLeader>p :bp<CR>
nnoremap <LocalLeader>n :bn<CR>
nnoremap <LocalLeader>d :bd<CR>
nnoremap <LocalLeader>g :e#<CR>

" Window 
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <LocalLeader>o <C-w>o

" Hardwrap, softwrap
map j gj
map k gk
" Make Y consistent with D
nnoremap Y y$
" Visually select the text that are recently edited
nmap gV `[v`]

"""""""""""""""""""""""""""""""
"""  Solarized color theme  
"""""""""""""""""""""""""""""""
"set background=dark 
set background=light 
set t_Co=256
" option name default optional
let g:solarized_termcolors= 256
if &t_Co<256 && !has('gui_running')
	let g:solarized_termcolors= 16
endif
let g:solarized_termtrans = 1 | 0
let g:solarized_degrade = 0 | 0
let g:solarized_bold = 1 | 0 
let g:solarized_underline = 1 | 0
"Most terminals (including screen) don't handle italics right, but urxvt
"can. 
let g:solarized_italic = 0 | 1 
let g:solarized_contrast = "high"| "normal" or "low" 
let g:solarized_visibility= "high"| "normal" or "low"
colorscheme solarized
"colorscheme gruvbox
highlight LineNr ctermbg=NONE

"""""""""""""""""""
"""   Markdown  
"""""""""""""""""""

" Auto detect filetype
autocmd BufRead,BufNewFile *.md,*.markdown set filetype=markdown.pandoc
 
"""""""""""""""""""
"""    Latex    
"""""""""""""""""""
let g:tex_flavor = "latex"

autocmd FileType tex setlocal iskeyword+=:,-

"inoremap { {}<ESC>i
"inoremap [ []<ESC>i
"inoremap ( ()<ESC>i

" make going to a line number easier.
"nnoremap <CR> G

"""""""""""""""""""
"""    R       
"""""""""""""""""""
" R script settings
"let maplocalleader = ","
"vmap r <Plug>RDSendSelection
"nmap r <Plug>RDSendLine
let R_in_buffer = 0
let R_applescript = 0
let R_tmux_split = 1
let R_vsplit=1
"let R_applescript=0
"
"let vimrplugin_applescript=0
"let vimrplugin_vsplit=1
"let vimrplugin_assign = 0
"
""""""""""""""""""""""
"""" Language Check 
""""""""""""""""""""""
let g:languagetool_jar='$HOME/mytools/LanguageTool-3.6/languagetool-commandline.jar'

""""""""""""""""""""""
"""" netrw 
""""""""""""""""""""""

" Tweaks for browsing using netrw
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=2  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_winsize = 25     " 25 percent of width
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

""""""""""""""""""""""""""
"""   My commands 
""""""""""""""""""""""""""
" write a file with root 
cmap w!! w !sudo tee % >/dev/null
" Create the `tags` file (may need to install ctags first)
command! MakeTags !ctags -R .
