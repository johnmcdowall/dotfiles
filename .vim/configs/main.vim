"***********************************************************************************
"   __  ___         _               ____       __   __    _                   
"  /  |/  / ___ _  (_)  ___        / __/ ___  / /_ / /_  (_)  ___   ___ _  ___
" / /|_/ / / _ `/ / /  / _ \      _\ \  / -_)/ __// __/ / /  / _ \ / _ `/ (_-<
"/_/  /_/  \_,_/ /_/  /_//_/     /___/  \__/ \__/ \__/ /_/  /_//_/ \_, / /___/
"                                                                 /___/
"
"***********************************************************************************

" Set leader to space bar
let mapleader = "\<Space>"
let maplocalleader = "\<Space>"

augroup specify_filetype
    autocmd!
    autocmd BufRead,BufNewFile *.md set filetype=markdown
    autocmd BufRead,BufNewFile *.txt set filetype=text

    " Sometimes syntax highlighting can get out of sync in large JSX and TSX
    " files. This was happening too often for me so I opted to enable syntax
    " sync fromstart, which forces vim to rescan the entire buffer when
    " highlighting. 
    autocmd BufEnter *.{js,jsx,ts,tsx} :syntax sync fromstart
    autocmd BufLeave *.{js,jsx,ts,tsx} :syntax sync clear

    function! ShowDocIfNoDiagnostic(timer_id)
      if (coc#util#has_float() == 0)
        silent call CocActionAsync('doHover')
      endif
    endfunction

    function! s:show_hover_doc()
      call timer_start(500, 'ShowDocIfNoDiagnostic')
    endfunction

    autocmd CursorHoldI * :call <SID>show_hover_doc()
    autocmd CursorHold * :call <SID>show_hover_doc()

augroup END

" Backspace deletes like most programs in insert mode
set backspace=2

" Longer leader key timeout
set timeout timeoutlen=1500

" Enable spell-checking for certain files
autocmd FileType text,markdown setlocal spell

" Limit line length for text files
autocmd FileType text,markdown,tex setlocal textwidth=180

" Don't automatically collapse markdown
set conceallevel=0

" Don't display mode in command line (airline already shows it)
set noshowmode

" Automatically re-read file if a change was detected outside of vim
set autoread

" no case-sensitive search unless uppercase is present
set ignorecase
set smartcase

" Enable mouse scroll
set mouse=a

" Allow a new buffer to be opened without saving current
set hidden

" Statusline Config
set statusline+=%F
set cmdheight=1

" Tab Settings
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set smarttab
set autoindent
set smartindent
set shiftround

" Enable syntax highlighting
syntax on

" Print syntax highlighting.
set printoptions+=syntax:y

" Matching braces/tags
set showmatch

" Keep a backup file.
"set backup

" Save undo tree.
"set undofile

" Do not back up temporary files.
set backupskip=/tmp/*,/private/tmp/*"

" Store backup files in one place.
set backupdir^=$HOME/.config/nvim/storage/backups/

" Store swap files in one place.
set dir^=$HOME/.config/nvim/storage/swaps/

" Store undo files in one place.
set undodir^=$HOME/.config/nvim/storage/undos//

" Undofile shit
if exists("+undofile")
  " undofile - This allows you to use undos after exiting and restarting
  " This, like swap and backups, uses .vim-undo first, then ~/.vim/undo
  " :help undo-persistence
  " This is only present in 7.3+
  if isdirectory($HOME . '/.vimundo') == 0
    :silent !mkdir -p ~/.vimundo > /dev/null 2>&1
  endif

  set undodir=./.vim-undo//
  set undodir+=~/.vimundo//
  set undofile
endif




" Soft line wrapping
set wrap

" Turns on detection for fyletypes, indentation files and plugin files
filetype plugin indent on

" Make sure compatible mode is disabled
set nocompatible

" Share yank buffer with system clipboard
set clipboard+=unnamedplus

" Show next 3 lines while scrolling.
if !&scrolloff
    set scrolloff=3
endif

" Show next 5 columns while side-scrolling.
if !&sidescrolloff
    set sidescrolloff=5
endif

" Get off my lawn
nnoremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
nnoremap <Down> :echoe "Use j"<CR>


" Treat <li> and <p> tags like the block tags they are
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"
let g:html_indent_tags = 'li\|p'
let g:html_indent_inctags = "address,article,aside,audio,blockquote,canvas,dd,div,dl,fieldset,figcaption,figure,footer,form,h1,h2,h3,h4,h5,h6,header,hgroup,hr,main,nav,noscript,ol,output,p,pre,section,table,tfoot,ul,video,container,spacer,row,columns"

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

set hlsearch " highlight searches by default
set ignorecase
set incsearch " find the next match as we type the search
set showmatch
set mat=2 "how many tenths of a second to blink when matching brackets

" pasting
noremap <leader>y "*y
noremap <leader>p :set paste<CR>"*p<CR>:set nopaste<CR>
noremap <leader>P :set paste<CR>"*P<CR>:set nopaste<CR>"

" all the exits
:command! WQ wq
:command! Wq wq
:command! Q q
:command! W w
:command! Bd bd

" http://vimcasts.org/episodes/bubbling-text/
nmap <C-Up> ddkP
nmap <C-Down> ddp
nmap <C-Left> <<
nmap <C-Right> >>

"Horizontal bubbling
vnoremap < <gv
vnoremap > >gv
nmap gV `[v``]

"Bubble multiple lines
vmap <C-Up> xkP`[V``]
vmap <C-Down> xp`[V``]
vmap <C-Right> >gv
vmap <C-Left> <gvn

set redrawtime=10000
nnoremap U :syntax sync fromstart<cr>:redraw!<cr>

let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.jsx,*.js,*.ejs"
let g:AutoPairsMultilineClose = 0
let g:AutoPairsFlyMode = 0

" Spelling
highlight clear SpellBad
highlight SpellBad ctermfg=001 ctermbg=007 guifg=#ff0000 guibg=#000000

" Jump to the last known position when reopening a file.
if has("autocmd")
  au BufReadPost * if &filetype != "gitcommit" && line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
endif

" Relative line numbers
set number
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

let s:clip = '/mnt/c/Windows/System32/clip.exe' 
if executable(s:clip)
  augroup WSLYank
    autocmd!
    autocmd TextYankPost * call system('echo '.shellescape(join(v:event.regcontents, "\<CR>")).' | '.s:clip)
  augroup END
end

