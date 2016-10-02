"pathogen
"execute pathogen#infect()
"
set nocompatible              " be iMproved, required
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'jalvesaq/Nvim-R'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'vim-scripts/Solarized'
Plugin 'morhetz/gruvbox'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'ervandew/supertab'
let g:SuperTabDefaultCompletionType = '<C-n>'
let g:SuperTabCrMapping = 0
call vundle#end()      
filetype plugin indent on

syntax on

set ic
set hlsearch
set lbr
set tabstop=8
set shiftwidth=8

set tw=75
set formatoptions+=nt
set wrapmargin=0

"map fm {v}!par -w75<CR>
"vmap fm !par -w75<CR>
set formatprg=par\ -w75
set incsearch
set wrap
set linebreak

"for macvim
set backspace=indent,eol,start

set spell spelllang=en_us
set spellfile=$HOME/Dropbox/backup/spell/en.utf-8.add
hi SpellBad ctermbg=22
set spell
" Under latex, this is a workaround only works for non-comments
syntax spell toplevel
set dictionary+=/usr/share/dict/words
set complete+=k
set thesaurus+=/Users/guofeng/.vim/thesaurus/mthesaur.txt

set autowriteall

set history=1000
set undolevels=1000
set title "change the terminal's title
set showmatch
set hidden
set noruler "hide the status line
set number
set nonumber " No line numbering
" some shortcuts
let mapleader = "\<Space>"
nmap <Leader>q :nohlsearch<CR>
nnoremap <Leader>w :w<CR>
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P
nmap <Leader>q :nohlsearch<CR>

map j gj
map k gk

""""solarized colorscheme
"set background=dark 
set background=light 
"set t_Co=16 
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
let g:solarized_italic = 0 | 0 
let g:solarized_contrast = "high"| "normal" or "low" 
let g:solarized_visibility= "high"| "normal" or "low"
colorscheme solarized
"colorscheme gruvbox

"""""""""""""""""""
"""   Snippet   """
"""""""""""""""""""
 
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<s-tab>'

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

"""""""""""""""""""
"""    Latex    """
"""""""""""""""""""

setlocal iskeyword+=:,-
inoremap { {<CR>}<ESC>i
inoremap [ [<CR>]<ESC>i
inoremap ( (<CR>)<ESC>i

"""""""""""""""""""
"""    R        """
"""""""""""""""""""

" R script settings
let maplocalleader = ","
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
