"pathogen
execute pathogen#infect()
syntax on
filetype plugin indent on

set ic
set hlsearch
colorscheme delek
set lbr
set tabstop=8
set shiftwidth=8
set tw=75
"map fm {v}!par -w75<CR>
"vmap fm !par -w75<CR>
set formatprg=par\ -w75
set incsearch
set wrap
set linebreak

"for macvim
set backspace=indent,eol,start

"colo xterm16
set spell spelllang=en_us
set spellfile=$HOME/Dropbox/backup/spell/en.utf-8.add
hi SpellBad ctermbg=22
set spell
" Under latex, this is a workaround only works for non-comments
syntax spell toplevel
set dictionary+=/usr/share/dict/words
set complete+=k

set autowriteall

set history=1000
set undolevels=1000
set title "change the terminal's title
set showmatch
set hidden
set noruler "hide the status line
"set number
"colo blink
"set runtimepath^=~/.vim/bundle/ctrlp.vim
" vimroom configuration
set nonumber                      " No line numbering
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

""""solarized colorscheme
"set background=dark "dark|light"
set background=light "dark|light"
"set nocompatible                                           
set t_Co=16 
" option name default optional
let g:solarized_termcolors= 16 | 256 
let g:solarized_termtrans = 0 | 1 
let g:solarized_degrade = 0 | 1
let g:solarized_bold = 1 | 0 
let g:solarized_underline = 1 | 0
let g:solarized_italic = 1 | 0 
let g:solarized_contrast = "normal"| "high" or "low" 
let g:solarized_visibility= "normal"| "high" or "low"
colorscheme solarized
call togglebg#map("<F5>")

"for Goyo
function! GoyoBefore()
	if has('gui_running')
		set fullscreen
		"set background=light
		"set linespace=7
	elseif exists('$TMUX')
		silent !tmux set status off
	endif
endfunction

function! GoyoAfter()
	if has('gui_running')
		set nofullscreen
		"set background=dark
		"et linespace=0
	elseif exists('$TMUX')
		silent !tmux set status on
	endif
endfunction
let g:goyo_callbacks = [function('GoyoBefore'), function('GoyoAfter')]
let g:goyo_margin_top=2
let g:goyo_margin_bottom=2
"" Map Goyo toggle to <Leader> + spacebar
nnoremap <Leader>g :Goyo<CR>  
"

" R script settings
let maplocalleader = ","
vmap <Space> <Plug>RDSendSelection
nmap <Space> <Plug>RDSendLine
let vimrplugin_applescript=0
let vimrplugin_vsplit=1
let vimrplugin_assign = 0
