"*****************************************************************************************
"   ___    __                _               ____       __   __    _                   
"  / _ \  / / __ __  ___ _  (_)  ___        / __/ ___  / /_ / /_  (_)  ___   ___ _  ___
" / ___/ / / / // / / _ `/ / /  / _ \      _\ \  / -_)/ __// __/ / /  / _ \ / _ `/ (_-<
"/_/    /_/  \_,_/  \_, / /_/  /_//_/     /___/  \__/ \__/ \__/ /_/  /_//_/ \_, / /___/
"                  /___/                                                   /___/       
"
"*****************************************************************************************

" Colorscheme
colorscheme nord

" CoC
let g:coc_global_extensions = ['coc-tslint-plugin', 'coc-tsserver', 'coc-emmet', 'coc-css', 'coc-html', 'coc-json', 'coc-yank', 'coc-solargraph', 'coc-tailwindcss']

if isdirectory('./node_modules') && isdirectory('./node_modules/prettier')
        "let g:coc_global_extensions += ['coc-prettier']
endif

if isdirectory('./node_modules') && isdirectory('./node_modules/eslint')
    let g:coc_global_extensions += ['coc-eslint']
endif

"""""""""""""""
" Supertab    "
"""""""""""""""
let g:SuperTabDefaultCompletionType = "<c-n>"

" Language server
let g:LanguageClient_serverCommands = {
    \ 'ruby': ['~/.asdf/shims/solargraph', 'stdio'],
    \ }

let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.erb,*.ejs,*.jsx,*.tsx,*.js"

" Treat <li> and <p> tags like the block tags they are
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"
let g:html_indent_tags = 'li\|p'
let g:html_indent_inctags = "address,article,aside,audio,blockquote,canvas,dd,div,dl,fieldset,figcaption,figure,footer,form,h1,h2,h3,h4,h5,h6,header,hgroup,hr,main,nav,noscript,ol,output,p,pre,section,table,tfoot,ul,video,container,spacer,row,columns"

" Git Gutter  "
"""""""""""""""
let g:gitgutter_enabled = 1
let g:gitgutter_grep=''


"""""""""""
" Goyo    "
"""""""""""
nmap <F6> :Goyo<CR>

""""""""""""
"NerdTree  "
""""""""""""
map <Leader>, :NERDTreeToggle<CR>

let nerdtreequitonopen = 0
let NERDTreeShowHidden=1
let nerdchristmastree=1
let g:NERDTreeMinimalUI = 1
let g:nerdtreewinsize = 25
let g:NERDTreeDirArrowExpandable = '▷'
let g:NERDTreeDirArrowCollapsible = '▼'
let NERDTreeAutoCenter=1
let g:NERDTreeGitStatusIndicatorMapCustom = {
        \ "modified"  : "✹",
        \ "staged"    : "✚",
        \ "untracked" : "✭",
        \ "renamed"   : "➜",
        \ "unmerged"  : "═",
        \ "deleted"   : "✖",
        \ "dirty"     : "✗",
        \ "clean"     : "✔︎",
        \ 'ignored'   : '☒',
        \ "unknown"   : "?"
        \ }


""""""""""""
"Airline   "
""""""""""""
"main settings
let g:airline_theme='nord'
let g:airline_powerline_fonts = 1
let g:airline_symbols = {}
let g:airline_skip_empty_sections = 1
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols_branch = ''
let g:airline_powerline_fonts = 1
let g:airline_symbols.crypt = ''
let g:airline_symbols.linenr = '☰'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = 'Ɇ'
let g:airline_symbols.whitespace = 'Ξ'
let g:airline_symbols.modified = ' '
let g:airline_section_error = '%{airline#util#wrap(airline#extensions#coc#get_error(),0)}'
let g:airline_section_warning = '%{airline#util#wrap(airline#extensions#coc#get_warning(),0)}'
"extensions
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#coc#enabled = 1
let g:airline#extensions#unicode#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#vista#enabled = 1
let g:airline#extensions#hunks#enabled = 1
"extension settings
let airline#extensions#coc#stl_format_err = '%E{[%e(#%fe)]}'
let airline#extensions#coc#stl_format_warn = '%W{[%w(#%fw)]}'
let airline#extensions#coc#warning_symbol = ':'
let airline#extensions#coc#error_symbol = ':'
let g:airline#extensions#hunks#hunk_symbols = [':', ':', ':']
let g:airline#extensions#branch#format = 2


"""""""""""""
"Devicons   "
"""""""""""""
let g:webdevicons_enable = 1
let g:webdevicons_enable_unite = 1
let g:webdevicons_enable_denite = 1
let g:webdevicons_enable_nerdtree = 1
let g:webdevicons_enable_airline_tabline = 1
let g:webdevicons_enable_vimfiler = 1
let g:WebDevIconsUnicodeDecorateFileNodes = 1
let g:WebDevIconsUnicodeDecorateFolderNodes = 1
let g:WebDevIconsUnicodeGlyphDoubleWidth = 1
let g:webdevicons_enable_airline_statusline = 1
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1
let g:WebDevIconsUnicodeGlyphDoubleWidth = 1
let g:WebDevIconsUnicodeDecorateFolderNodesDefaultSymbol = ''
let g:DevIconsDefaultFolderOpenSymbol = ''

""""""""""
" Emoji  "
""""""""""
set completefunc=emoji#complete


"""""""""""""""""
"Indent Guides  "
"""""""""""""""""
let g:indentLine_char_list = ['|', '¦', '┆', '┊']

let g:indent_guides_auto_colors = 1
let g:indentLine_fileTypeExclude = [
      \'defx',
      \'markdown',
      \'denite',
      \'startify',
      \'tagbar',
      \]

"""""""""""""
"Autopairs  "
"""""""""""""
let g:AutoPairsFlyMode = 0
let g:AutoPairsMultilineClose = 0

"""""""""""""
"Ultinsips  "
"""""""""""""
" These were interfering with coc.nvims completion keybinds
"let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsJumpForwardTrigger="<c-b>"
"let g:UltiSnipsJumpBackwardTrigger="<c-z>"
"let g:UltiSnipsListSnippets="<c-tab>"

""""""""""""
"Startify  "
""""""""""""
function! s:center(lines) abort
  let longest_line   = max(map(copy(a:lines), 'strwidth(v:val)'))
  let centered_lines = map(copy(a:lines),
        \ 'repeat(" ", (&columns / 2) - (longest_line / 2)) . v:val')
  return centered_lines
endfunction
let s:header= [
      \"▄▀▀▄ ▄▀▀▄  ▄▀▀█▀▄    ▄▀▀▄ ▄▀▄      ▄▀▀▄▀▀▀▄  ▄▀▀█▄▄▄▄  ▄▀▀█▄   ▄▀▀▄▀▀▀▄  ▄▀▀█▄▄▄▄  ▄▀▀▄▀▀▀▄ ",
      \"█   █    █ █   █  █  █  █ ▀  █     █   █   █ ▐  ▄▀   ▐ ▐ ▄▀ ▀▄ █   █   █ ▐  ▄▀   ▐ █   █   █ ",
      \"▐  █    █  ▐   █  ▐  ▐  █    █     ▐  █▀▀█▀    █▄▄▄▄▄    █▄▄▄█ ▐  █▀▀▀▀    █▄▄▄▄▄  ▐  █▀▀█▀  ",
      \"  █   ▄▀      █       █    █       ▄▀    █    █    ▌   ▄▀   █    █        █    ▌   ▄▀    █  ",
      \"   ▀▄▀     ▄▀▀▀▀▀▄  ▄▀   ▄▀       █     █    ▄▀▄▄▄▄   █   ▄▀   ▄▀        ▄▀▄▄▄▄   █     █   ",
      \"          █       █ █    █        ▐     ▐    █    ▐   ▐   ▐   █          █    ▐   ▐     ▐   ",
      \"          ▐       ▐ ▐    ▐                   ▐                ▐          ▐                  ",
      \"",
      \"",
      \"                                          ;::::;",
      \"                                        ;::::; :;",
      \"                                      ;:::::'   :;",
      \"                                     ;:::::;     ;.",
      \"                                    ,:::::'       ;           OOO\ ",
      \"                                    ::::::;       ;          OOOOO\ ",
      \"                                    ;:::::;       ;         OOOOOOOO",
      \"                                   ,;::::::;     ;'         / OOOOOOO",
      \"                                 ;:::::::::`. ,,,;.        /  / DOOOOOO",
      \"                               .';:::::::::::::::::;,     /  /     DOOOO",
      \"                              ,::::::;::::::;;;;::::;,   /  /        DOOO",
      \"                             ;`::::::`'::::::;;;::::: ,#/  /          DOOO",
      \"                             :`:::::::`;::::::;;::: ;::#  /            DOOO",
      \"                             ::`:::::::`;:::::::: ;::::# /              DOO",
      \"                             `:`:::::::`;:::::: ;::::::#/               DOO",
      \"                              :::`:::::::`;; ;:::::::::##                OO",
      \"                              ::::`:::::::`;::::::::;:::#                OO",
      \"                              `:::::`::::::::::::;'`:;::#                O",
      \"                               `:::::`::::::::;' /  / `:#",
      \"                                ::::::`:::::;'  /  /   `#",
      \]

" Do not switch the working directory to the opened file
let g:startify_change_to_dir = 0
let g:startify_custom_header = s:center(s:header)
" Optionally create and use footer
"let s:header= []
"let g:startify_custom_footer = s:center(s:footer)

" Transparent popup window
hi! Pmenu ctermbg=black
hi! PmenuSel ctermfg=2
hi! PmenuSel ctermbg=0

" Brighter line numbers
hi! LineNr ctermfg=NONE guibg=NONE

augroup MyAutoCmd
  autocmd!
augroup end

"""""""""""""""""
"Nerd Commenter "
"""""""""""""""""
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1
" Allow commenting and inverting empty lines (useful when commenting a region)

let g:rainbow_active = 1
let g:rainbow_conf = {
\   'ctermfgs': ['4', '3', '12', '8', '10', '5'],
\}

augroup vimrcEx
  autocmd!
  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if &ft != 'gitcommit' && &ft != 'gitrebase' && line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  " Set syntax highlighting for specific file types
  autocmd BufRead,BufNewFile *.md set filetype=markdown
  autocmd BufRead,BufNewFile *.inky-erb set filetype=eruby

  autocmd BufRead,BufNewFile .{jscs,jshint,eslint}rc set filetype=json

  " in makefiles, don't expand tabs to spaces, since actual tab characters are
  " needed, and have indentation at 8 chars to be sure that all indents are tabs
  " (despite the mappings later):

  autocmd FileType make set noexpandtab shiftwidth=8 softtabstop=0

  " Remove trailing whitespace before saving a file
  au FileType ruby,javascript,css,scss,sass,html,erb autocmd BufWritePre <buffer> :%s/\s\+$//e
augroup END


" Fix visual selection
set background=dark
highlight Visual ctermbg=Blue ctermfg=White

let g:fzf_layout = {'up':'~90%', 'window': { 'width': 0.8, 'height': 0.8,'yoffset':0.5,'xoffset': 0.5, 'highlight': 'Todo', 'border': 'sharp' } }

" let $FZF_DEFAULT_OPTS = '--layout=reverse --info=inline'


