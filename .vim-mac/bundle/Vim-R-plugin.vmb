" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/rcomplete.vim	[[[1
96
" Vim completion script
" Language:    R
" Maintainer:  Jakson Alves de Aquino <jalvesaq@gmail.com>
"

" Tell R to create a list of objects file listing all currently available
" objects in its environment. The file is necessary for omni completion.
function BuildROmniList()
    if string(g:SendCmdToR) == "function('SendCmdToR_fake')"
        return
    endif

    let omnilistcmd = 'vimcom:::vim.bol("' . g:rplugin_tmpdir . "/GlobalEnvList_" . $VIMINSTANCEID . '"'
    if g:vimrplugin_allnames == 1
        let omnilistcmd = omnilistcmd . ', allnames = TRUE'
    endif
    let omnilistcmd = omnilistcmd . ')'

    call delete(g:rplugin_tmpdir . "/vimbol_finished")
    call delete(g:rplugin_tmpdir . "/eval_reply")
    call SendToVimCom("\x08" . $VIMINSTANCEID . omnilistcmd)
    if g:rplugin_vimcomport == 0
        sleep 500m
        return
    endif
    let g:rplugin_lastev = ReadEvalReply()
    if g:rplugin_lastev == "R is busy." || g:rplugin_lastev == "No reply"
        call RWarningMsg(g:rplugin_lastev)
        sleep 800m
        return
    endif
    sleep 20m
    let ii = 0
    while !filereadable(g:rplugin_tmpdir . "/vimbol_finished") && ii < 5
        let ii += 1
        sleep
    endwhile
    echon "\r               "
    if ii == 5
        call RWarningMsg("No longer waiting...")
        return
    endif

    let g:rplugin_globalenvlines = readfile(g:rplugin_tmpdir . "/GlobalEnvList_" . $VIMINSTANCEID)
endfunction

fun! rcomplete#CompleteR(findstart, base)
    if &filetype == "rnoweb" && RnwIsInRCode(0) == 0 && exists("*LatexBox_Complete")
        let texbegin = LatexBox_Complete(a:findstart, a:base)
        return texbegin
    endif
    if a:findstart
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && (line[start - 1] =~ '\w' || line[start - 1] =~ '\.' || line[start - 1] =~ '\$')
            let start -= 1
        endwhile
        call BuildROmniList()
        return start
    else
        let resp = []
        if strlen(a:base) == 0
            return resp
        endif

        if len(g:rplugin_omni_lines) == 0
            call add(resp, {'word': a:base, 'menu': " [ List is empty. Did you load vimcom package? ]"})
        endif

        let flines = g:rplugin_omni_lines + g:rplugin_globalenvlines
        " The char '$' at the end of 'a:base' is treated as end of line, and
        " the pattern is never found in 'line'.
        let newbase = '^' . substitute(a:base, "\\$$", "", "")
        for line in flines
            if line =~ newbase
                " Skip cols of data frames unless the user is really looking for them.
                if a:base !~ '\$' && line =~ '\$'
                    continue
                endif
                let tmp1 = split(line, "\x06", 1)
                if g:vimrplugin_show_args
                    let info = tmp1[4]
                    let info = substitute(info, "\t", ", ", "g")
                    let info = substitute(info, "\x07", " = ", "g")
                    let tmp2 = {'word': tmp1[0], 'menu': tmp1[1] . ' ' . tmp1[3], 'info': info}
                else
                    let tmp2 = {'word': tmp1[0], 'menu': tmp1[1] . ' ' . tmp1[3]}
                endif
                call add(resp, tmp2)
            endif
        endfor

        return resp
    endif
endfun

doc/r-plugin.txt	[[[1
3267
*r-plugin.txt*                                                  *vim-r-plugin*
				 Vim-R-plugin~
			     Plugin to work with R~

Authors: Jakson A. Aquino   <jalvesaq@gmail.com>
         Jose Claudio Faria <joseclaudio.faria@gmail.com>

Version: 1.2
For Vim version 7.4

 1. Overview                                    |r-plugin-overview|
 2. Main features                               |r-plugin-features|
 3. Installation                                |r-plugin-installation|
 4. Use                                         |r-plugin-use|
 5. Known bugs and workarounds                  |r-plugin-known-bugs|
 6. Options                                     |r-plugin-options|
 7. Custom key bindings                         |r-plugin-key-bindings|
 8. License and files                           |r-plugin-files|
 9. FAQ and tips                                |r-plugin-tips|
10. News                                        |r-plugin-news|


==============================================================================
							   *r-plugin-overview*
1. Overview~

This plugin improves Vim's support for editing R code and makes it possible to
integrate Vim with R. If you want to use Neovim and R, see:

   https://github.com/jalvesaq/Nvim-R

The plugin uses some ideas and code from Johannes Ranke's (vim-r-plugin), Eric
Van Dewoestine's (screen.vim plugin), Vincent Nijs (R.vim for Mac OS X) and
some ideas from the Tinn-R (Windows only) project.

The latest stable version of this plugin is available at:

    http://www.vim.org/scripts/script.php?script_id=2628

Feedback is welcomed. Please submit bug reports to the developers. Do not like
a feature? Tell us and we may add an option to disable it. If you have any
comments or questions, please post them at:

    https://groups.google.com/forum/#!forum/vim-r-plugin

The plugin should emit useful warnings if you do things it was not programmed
to deal with. Cryptic error message are bugs... Please report them at:

    https://github.com/jcfaria/Vim-R-plugin/issues

We do not plan to take the initiative of writing code for new features, but
patches and git pull requests are welcome. If you want a feature that only few
people might be interested in, you can write a script to be sourced by the
Vim-R-plugin (see |vimrplugin_source|).


==============================================================================
							   *r-plugin-features*
2. Main features~

  * Syntax highlighting for R code, including:
      - Special characters in strings.
      - Functions of loaded packages.
      - Special highlighting for R output (.Rout files).
      - Spell check only strings and comments.
      - Fold code when foldmethod=syntax.
  * Syntax highlighting for RHelp, RMarkdown and RreStructuredText.
  * Smart indentation for R, RHelp, Rnoweb, RMarkdown and RreStructuredText.
  * Integrated communication with R:
      - Start/Close R.
      - Send lines, selection, paragraphs, functions, blocks, entire file.
      - Send commands with the object under cursor as argument: help, args,
        plot, print, str, summary, example, names.
      - Send to R the Sweave, knit and pdflatex commands.
  * Omni completion (auto-completion) for R objects (.GlobalEnv and loaded
    packages).
  * Auto-completion of function arguments.
  * Auto-completion of knitr chunk options.
  * Ability to see R's documentation in a Vim's buffer:
      - Automatic calculation of the best layout of the R documentation buffer
        (split the window either horizontally or vertically according to the
        available room).
      - Automatic formatting of the text to fit the panel width.
      - Send code and commands to R (useful to run examples).
      - Jump to another R documentation.
      - Syntax highlighting of R documentation.
  * Object Browser (.GlobalEnv and loaded packages):
      - Send commands with the object under cursor as argument.
      - Call R's `help()` with the object under cursor as argument.
      - Syntax highlighting of the Object Browser.
  * SyncTeX support.
  * Most of the plugin's behavior is customizable.

For screenshots see: http://www.lepem.ufc.br/jaa/vim-r-plugin.html

==============================================================================
						       *r-plugin-installation*
3. Installation~

The installation instructions are split in six sections:

   3.1. Preliminary system setup on Windows
   3.2. Preliminary system setup on Mac OS X
   3.3. Preliminary system setup on Unix
   3.4. Plugin installation
   3.5. Troubleshooting
   3.6. Optional steps


------------------------------------------------------------------------------
3.1. Preliminary system setup on Windows~

Before installing the plugin, you should install several external
dependencies:

    * R's version must be >= 3.0.0: http://www.r-project.org/

    * vimcom = 1.2-0: http://www.lepem.ufc.br/jaa/vimcom.html

      Note: If you cannot build vimcom yourself, you will want to
      download and install the zip file.

    * Vim's version must be >= 7.4: http://www.vim.org/download.php

    * Only if you write Rnoweb code:

       * Sumatra PDF viewer >= 3.0.

       * MikTeX or other LaTeX distribution.

       * Perl (required to run `latexmk`).

Add the following lines to your `Rprofile`:
>
   options(vimcom.verbose = 1)
   library(vimcom)
<
If you do not know where your .Rprofile is, do the following command in R
Console to open it:
>
   edit(file = "~/.Rprofile")
<

------------------------------------------------------------------------------
3.2. Preliminary system setup on Mac OS X~

Before installing the plugin, you should install its dependencies:

   Depends:~

   Cocoa MacVim >= 7.4: https://code.google.com/p/macvim

   R >= 3.0.0: http://www.r-project.org/

   vimcom = 1.2-0: http://www.lepem.ufc.br/jaa/vimcom.html

   Only if you edit Rnoweb files: MacTeX, BasicTeX or other LaTeX system.

If you want to call R.app from MacVim, put in your `~/.Rprofile`:
>
   if(interactive()){
       options(vimcom.verbose = 1) # optional
       library(vimcom)
   }
<
On Mac OS X, the plugin will use AppleScript to send commands to the R Console
application unless |vimrplugin_applescript| = 0. In this case, R will run in a
external terminal emulator. The advantage of running R in a terminal emulator
is that its output can be colorized by the R package "colorout". If you want
to try this configuration, install Tmux and the R packages "setwidth" and
"colorout" (see instructions in section 3.3) and put in your `~/.Rprofile`:
>
   if(interactive()){
       library(colorout)
       library(setwidth)
       options(vimcom.verbose = 1) # optional
       library(vimcom)
   }
<
Note: The plugin is fully functional with MacVim, but the Vim application that
comes with MacVim does not have the the server side of the 'clientserver'
feature which is required to have the plugin working properly. If you want to
run both Vim and R in the same Tmux window in a terminal emulator, you have to
follow the instructions from section 3.3.


------------------------------------------------------------------------------
3.3. Preliminary system setup on Unix~

Before installing the plugin, you should install its dependencies:

   Depends:~

   Vim, GVim >= 7.4: http://www.vim.org/download.php

      Note: The Vim-R-plugin requires Vim compiled with |+libcall|,
	    |+clientserver| and |+conceal| features. In Normal mode, do
	    `:version`  to check if your Vim has these features.

	    If you need to use the Vim-R-plugin in a Unix system without the
	    X Server running (e.g. Linux Console, and any system accessed
	    through ssh), you have to use Neovim and Nvim-R:
	    https://github.com/jalvesaq/Nvim-R
	    If you cannot use Neovim and need to use Vim in these
	    circumstances, please, read |r-plugin-nox| and |r-plugin-remote|.

   R >= 3.0.0: http://www.r-project.org/

   vimcom = 1.2-0: http://www.lepem.ufc.br/jaa/vimcom.html

   Tmux >= 1.8:   http://tmux.sourceforge.net
                  Tmux is necessary to send commands from Vim to R Console.

   wmctrl:     http://tomas.styblo.name/wmctrl/
               Required for better SyncTeX support under X Server.


   Suggests:~

   colorout:      http://www.lepem.ufc.br/jaa/colorout.html
                  Colorizes the R output.

   setwidth:      An R package that can be installed with the command
                  `install.packages("setwidth")`.
                  The library setwidth adjusts the value of `options("width")`
                  whenever the terminal is resized.

   ncurses-term:  http://invisible-island.net/ncurses
		  Might be necessary if you want support for 256 colors at the
		  terminal emulator.

   latexmk:       Automate the compilation of LaTeX documents.
                  See examples in |vimrplugin_latexcmd|.

   Note: Vim, R, Tmux, ncurses-term and latexmk are already packaged for most
   GNU/Linux distributions and other Unix variants. Unfortunately their
   installation instructions vary widely and are beyond the scope of this
   documentation.

Put the following lines in your `~/.Rprofile`:
>
   if(interactive()){
       library(colorout)
       library(setwidth)
       options(vimcom.verbose = 1) # optional
       library(vimcom)
   }
<
Put the following in your `~/.bashrc`:
>
   alias vim="vim --servername VIM"
<
Before proceeding, you have to start a new shell session to have the alias
enabled.

If you start either GVim or Vim in a terminal emulator the plugin will start R
in a external terminal emulator. If you start Vim inside of a Tmux session,
the plugin will split the Tmux window in two and start R in the other pane.

The recommended way of running the plugin on Linux is running Vim inside a
Tmux session. Please, read |r-plugin-tmux| for details.

If you run Vim in a terminal emulator, it must be started with the argument
|--servername|. Please, read the section |r-plugin-bash-setup| to know some
tips on how to configure Bash.


------------------------------------------------------------------------------
3.4. Plugin installation (instructions for all systems)~

You need to activate plugins and indentation according to 'filetype'. You
should have at least the following options at the top or at near the very top
of your |vimrc| (but below `set` `runtimepath`, if you have set it):
>
   set nocompatible
   syntax enable
   filetype plugin on
   filetype indent on
<
Download the latest version of the plugin from:

    http://www.vim.org/scripts/script.php?script_id=2628

If you either are on Windows or prefer to use a graphical interface, start the
file manager, find the file `Vim-R-plugin.vmb` and open it with GVim or
MacVim. Otherwise, start a terminal emulator, go to the directory where you
have downloaded the plugin and type:
>
   vim Vim-R-plugin.vmb
<
Then, in Vim, type:
>
   :so %
<
Press <Enter> to start the installation and press the <Space> bar as many
times as necessary to finish the installation. You should, then, quit Vim.

Note: If you need to install the plugin in a non default directory, do
`:UseVimball` `[path]`. In this case, the configuration of Vim's 'runtimepath'
must be done before the command "filetype on" in both the system and the user
|vimrc| files, otherwise, some file types might not be correctly recognized.

The plugin is installed and will be activated next time that you start to edit
an R script.

You may want to improve the configuration of your |vimrc| for a better use of
the plugin. Please, see |r-plugin-vimrc-setup|.

If you want to uninstall the plugin, do
>
   :RmVimball Vim-R-plugin
<

------------------------------------------------------------------------------
3.5. Troubleshooting (if the plugin doesn't work)~

Note: The <LocalLeader> is '\' by default.

The plugin is a |file-type| plugin. It will be active only if you are editing
a .R, .Rnw, .Rd, Rmd, or Rrst file. The menu items will not be visible and the
key bindings will not be active while editing either unnamed files or files
with name extensions other than the mentioned above. If the plugin is active,
pressing <LocalLeader>rf should start R.

Did you see warning messages but they disappeared before you have had time to
read them? Type the command |:messages| in Normal mode to see them again.

On Windows, if R does not start and you get an error message instead, you may
want to set the path to your Rgui.exe in your |vimrc|, for example (please
adapt to your installation):
>
   let vimrplugin_r_path = 'C:\\Program Files\\R\\R-3.1.2\\bin\\i386'
<
Are you using Debian, Ubuntu or other Debian based Linux distribution? If yes,
you may prefer to install the Debian package available at:

   http://www.lepem.ufc.br/jaa/vim-r-plugin.html

Did you see the message "VimCom port not found"? This means that R is not
running, the vimcom package is not installed (or is installed but is not
loaded), or R was not started by Vim.

If R takes more than 5 seconds to load, you should adjust the value of
|vimrplugin_vimcom_wait|.


------------------------------------------------------------------------------
3.6. Optional steps~

3.6.1 Customize the plugin~

Please, read the section |r-plugin-options|. Emacs/ESS users should read the
section |r-plugin-indenting| of this document.


------------------------------------------------------------------------------
3.6.2 Install additional plugins~

You may be interested in installing additional general plugins to get
functionality not provided by this file type plugin. ShowMarks and snipMate
are particularly interesting. Please read |r-plugin-tips| for details. If you
edit Rnoweb files, you may want to try LaTeX-Box for omnicompletion of LaTeX
code (see |r-plugin-latex-box| for details).


==============================================================================
								*r-plugin-use*
4. Use~

4.1. Key bindings~

Note: The <LocalLeader> is '\' by default.

Note: It is recommended the use of different keys for <Leader> and
<LocalLeader> to avoid clashes between filetype plugins and general plugins
key binds. See |filetype-plugins|, |maplocalleader| and |r-plugin-localleader|.

To use the plugin, open a .R, .Rnw, .Rd, .Rmd or .Rrst file with Vim and type
<LocalLeader>rf. Then, you will be able to use the plugin key bindings to send
commands to R.

This plugin has many key bindings, which correspond with menu entries. In the
list below, the backslash represents the <LocalLeader>. Not all menu items and
key bindings are enabled in all filetypes supported by the plugin (r, rnoweb,
rhelp, rrst, rmd).

Menu entry                                Default shortcut~
Start/Close
  . Start R (default)                                  \rf
  . Start R --vanilla                                  \rv
  . Start R (custom)                                   \rc
  --------------------------------------------------------
  . Close R (no save)                                  \rq
  . Stop R                                          :RStop
-----------------------------------------------------------

Send
  . File                                               \aa
  . File (echo)                                        \ae
  . File (open .Rout)                                  \ao
  --------------------------------------------------------
  . Block (cur)                                        \bb
  . Block (cur, echo)                                  \be
  . Block (cur, down)                                  \bd
  . Block (cur, echo and down)                         \ba
  --------------------------------------------------------
  . Chunk (cur)                                        \cc
  . Chunk (cur, echo)                                  \ce
  . Chunk (cur, down)                                  \cd
  . Chunk (cur, echo and down)                         \ca
  . Chunk (from first to here)                         \ch
  --------------------------------------------------------
  . Function (cur)                                     \ff
  . Function (cur, echo)                               \fe
  . Function (cur and down)                            \fd
  . Function (cur, echo and down)                      \fa
  --------------------------------------------------------
  . Selection                                          \ss
  . Selection (echo)                                   \se
  . Selection (and down)                               \sd
  . Selection (echo and down)                          \sa
  --------------------------------------------------------
  . Paragraph                                          \pp
  . Paragraph (echo)                                   \pe
  . Paragraph (and down)                               \pd
  . Paragraph (echo and down)                          \pa
  --------------------------------------------------------
  . Line                                                \l
  . Line (and down)                                     \d
  . Line (and new one)                                  \q
  . Left part of line (cur)                       \r<Left>
  . Right part of line (cur)                     \r<Right>
  . Line (evaluate and insert the output as comment)    \o
-----------------------------------------------------------

Command
  . List space                                         \rl
  . Clear console                                      \rr
  . Clear all                                          \rm
  --------------------------------------------------------
  . Print (cur)                                        \rp
  . Names (cur)                                        \rn
  . Structure (cur)                                    \rt
  --------------------------------------------------------
  . Arguments (cur)                                    \ra
  . Example (cur)                                      \re
  . Help (cur)                                         \rh
  --------------------------------------------------------
  . Summary (cur)                                      \rs
  . Plot (cur)                                         \rg
  . Plot and summary (cur)                             \rb
  --------------------------------------------------------
  . Set working directory (cur file path)              \rd
  --------------------------------------------------------
  . Sweave (cur file)                                  \sw
  . Sweave and PDF (cur file)                          \sp
  . Sweave, BibTeX and PDF (cur file) (Linux/Unix)     \sb
  --------------------------------------------------------
  . Knit (cur file)                                    \kn
  . Knit and PDF (cur file)                            \kp
  . Knit, BibTeX and PDF (cur file) (Linux/Unix)       \kb
  . Knit and Beamer PDF (cur file) (only .Rmd)         \kl
  . Knit and HTML (cur file, verbose) (only .Rmd)      \kh
  . Spin (cur file) (only .R)                          \ks
  --------------------------------------------------------
  . Open PDF (cur file)                                \op
  . Search forward (SyncTeX)                           \gp
  . Go to LaTeX (SyncTeX)                              \gt
  --------------------------------------------------------
  . Build tags file (cur dir)                  :RBuildTags
-----------------------------------------------------------

Edit
  . Insert "<-"                                          _
  . Complete object name                              ^X^O
  . Complete function arguments                       ^X^A
  --------------------------------------------------------
  . Indent (line)                                       ==
  . Indent (selected lines)                              =
  . Indent (whole buffer)                             gg=G
  --------------------------------------------------------
  . Toggle comment (line, sel)                         \xx
  . Comment (line, sel)                                \xc
  . Uncomment (line, sel)                              \xu
  . Add/Align right comment (line, sel)                 \;
  --------------------------------------------------------
  . Go (next R chunk)                                  \gn
  . Go (previous R chunk)                              \gN
-----------------------------------------------------------

Object Browser
  . Show/Update                                        \ro
  . Expand (all lists)                                 \r=
  . Collapse (all lists)                               \r-
  . Toggle (cur)                                     Enter
-----------------------------------------------------------

Help (plugin)
Help (R)                                            :Rhelp
-----------------------------------------------------------

Please see |r-plugin-key-bindings| to learn how to customize the key bindings
without editing the plugin directly.

The plugin commands that send code to R Console are the most commonly used. If
the code to be sent to R has a single line it is sent directly to R Console,
but if it has more than one line (a selection of lines, a block of lines
between two marks, a paragraph etc) the lines are written to a file and the
plugin sends to R the command to source the file. You should type quickly
<LocalLeader>d to send to R Console the line currently under the cursor. If
you want to see what lines are being sourced when sending a selection of
lines, you should do either <LocalLeader>se or <LocalLeader>sa instead of
<LocalLeader>ss.

If the cursor is over the header of an R chunk with the `child` option (from
Rnoweb, RMarkdown or RreStructuredText document), and you use one of the
commands that send a single line of code to R, then the plugin will send to R
the command to knit the child document.

After the commands that send, sweave or knit the current buffer, Vim will save
the current buffer if it has any pending changes before performing the tasks.
After <LocalLeader>ao, Vim will run "R CMD BATCH --no-restore --no-save" on
the current file and show the resulting .Rout file in a new tab. Please see
|vimrplugin_routnotab| if you prefer that the file is open in a new split
window. Note: The command <LocalLeader>ao, silently writes the current buffer
to its file if it was modified and deletes the .Rout file if it exists.

R syntax uses " <- " to assign values to variables which is inconvenient to
type. In insert mode, typing a single underscore, "_", will write " <- ",
unless you are typing inside a string. The replacement will always happen if
syntax highlighting is off (see |:syn-on| and |:syn-off|). If necessary, it is
possible to insert an actual underscore into your file by typing a second
underscore. This behavior is similar to the EMACS ESS mode some users may be
familiar with and is enabled by default. You have to change the value of
|vimrplugin_assign| to disable underscore replacement.

When you press <LocalLeader>rh, the plugin shows the help for the function
under the cursor. The plugin also checks the class of the object passed as
argument to the function to try to figure out whether the function is a
generic one and whether it is more appropriate to show a specific method. The
same procedure is followed with <LocalLeader>rp, that is, while printing an
object. For example, if you run the code below and, then, press
<LocalLeader>rh and <LocalLeader>rp over the two occurrences of `summary`, the
plugin will show different help documents and print different function methods
in each case:
>
   y <- rnorm(100)
   x <- rnorm(100)
   m <- lm(y ~ x)
   summary(x)
   summary(m)
<
When completing object names (CTRL-X CTRL-O) and function arguments (CTRL-X
CTRL-A) you have to press CTRL-N to go foward in the list and CTRL-P to go
backward (see |popupmenu-completion|). Note: if using Vim in a terminal
emulator, Tmux will capture the CTRL-A command. You have to do CTRL-A twice to
pass a single CTRL-A to Vim. For rnoweb, rmd and rrst file types, CTRL-X
CTRL-A can also be used to complete knitr chunk options if the cursor is
inside the chunk header.

If R is not running or if it is running but is busy the completion will be
based on information from the packages listed by |vimrplugin_start_libs|
(provided that the libraries were loaded at least once during a session of
Vim-R-plugin usage). Otherwise, the pop up menu for completion of function
arguments will include an additional line with the name of the library where
the function is (if the function name can be found in more than one library)
and the function method (if what is being shown are the arguments of a method
and not of the function itself). For library() and require(), when completing
the first argument, the popup list shows the names of installed packages, but
only if R is running.

To get help on an R topic, type in Vim (Normal mode):
>
   :Rhelp topic
<
The command may be abbreviated to  :Rh  and you can either press <Tab> to
trigger the autocompletion of R objects names or hit CTRL-D to list the
possible completions (see |cmdline-completion| for details on the various ways
of getting command-line completion). The list of objects used for
completion is the same available for omnicompletion (see
|vimrplugin_start_libs|). You may close the R documentation buffer by
simply pressing `q`.

You can source all .R files in a directory with the Normal mode command
:RSourceDir, which accepts an optional argument (the directory to be sourced).
								    *:Rinsert*
The command  :Rinsert <cmd>  inserts one or more lines with the output of the
R command sent to R. By using this command we can avoid the need of copying
and pasting the output R from its console to Vim. For example, to insert the
output of `dput(levels(var))`, where `var` is a factor vector, we could do in
Vim:
>
   :Rinsert dput(levels(var))
<
The output inserted by  :Rinsert  is limited to 5012 characters.

The command  :Rformat  calls the function `tidy_source()` of formatR package
to format either the entire buffer or the selected lines. The value of the
`width.cutoff` argument is set to the buffer's 'textwidth' if it is not
outside the range 20-180. Se R help on `tidy_source` for details on how to
control the function behavior.


------------------------------------------------------------------------------
4.2. Edition of Rnoweb files~

In Rnoweb files (.Rnw), when the cursor is over the `@` character, which
finishes an R chunk, the sending of all commands to R is suspended and the
shortcut to send the current line makes the cursor to jump to the next chunk.
While editing Rnoweb files, the following commands are available in Normal
mode:

   [count]<LocalLeader>gn : go to the next chunk of R code
   [count]<LocalLeader>gN : go to the previous chunk of R code

The commands <LocalLeader>cc, ce, cd and ca send the current chunk of R code
to R Console. The command <LocalLeader>ch sends the R code from the first
chunk up to the current line.

The commands <LocalLeader>kn builds the .tex file from the Rnoweb one using
the knitr package and <LocalLeader>kp compiles the pdf; for Sweave, the
commands are, respectively <LocalLeader>sw and <LocalLeader>sp. On Linux, if
using Evince, Okular or Zathura, you can jump from the Rnoweb file to the PDF
with the command <LocalLeader>gp. To jump from a specific location in the PDF
to the corresponding line in the Rnoweb, in either Evince or Zathura you
should press <C-LeftMouse>, and in Okular <S-LeftMouse>. No configuration is
required if you use Evince, Gnome-Terminal and the knitr package, and work
with single file Rnoweb documents. Otherwise, see |r-plugin-SyncTeX| for
configuration details.


------------------------------------------------------------------------------
4.3. Omni completion and the highlighting of functions~

The plugin adds some features to the default syntax highlight of R code. One
such feature is the highlight of R functions. However, functions are
highlighted only if their libraries are loaded by R (but see
|vimrplugin_start_libs|).

Note: If you have too many loaded packages Vim may be unable to load the list
of functions for syntax highlight.

Vim can automatically complete the names of R objects when CTRL-X CTRL-O is
pressed in insert mode (see |omni-completion| for details). Omni completion
shows in a pop up menu the name of the object, its class and its environment
(most frequently, its package name). If the object is a function, the plugin
can also show the function arguments in a separate preview window (this
feature is disabled by default: see |vimrplugin_show_args|).

If a data.frame is found, while building the list of objects, the columns in
the data.frame are added to the list. When you try to use omni completion to
complete the name of a data.frame, the columns are not shown. But when the
data.frame name is already complete, and you have inserted the '$' symbol,
omni completion will show the column names.

Only the names of objects in .GlobalEnv and in loaded libraries are completed,
and the |clientserver| feature is required to get the list of loaded libraries
automatically updated. If either R is not running or Vim is running without
the |clientserver| feature, only objects of libraries listed in
|vimrplugin_start_libs| will have their names completed. When you load a new
library in R, only the current buffer has the highlighting of function names
immediately updated. If you have other buffers open, they will be updated when
you enter them.

Vim uses one file to store the names of .GlobalEnv objects and a list of files
for all other objects. The .GlobalEnv list is stored in the `$VIMRPLUGIN_TMPDIR`
directory and is deleted when you quits Vim. The other files are stored in the
`$VIMRPLUGIN_COMPLDIR` directory and remain available until you manually delete
them.


------------------------------------------------------------------------------
4.4. The Object Browser~

You have to do <LocalLeader>ro to start the Object Browser. The Object Browser
has two views: .GlobalEnv and Libraries. If you either press <Enter> or double
click (GVim or Vim with 'mouse' set to "a") on the first line of the Object
Browser it will toggle the view between the objects in .GlobalEnv and the
currently loaded libraries. The Object Browser requires the |+clientserver|
feature to be automatically updated and the |+conceal| feature to correctly
align list items.

In the .GlobalEnv view, if an object has the attribute "label", it will also
be displayed. Please, see the R help for package vimcom for some options
to control the Object Browser behavior. In the Object Browser window, while in
Normal mode, you can either press <Enter> or double click (GVim only) over a
data.frame or list to show/hide its elements (not if viewing the content of
loaded libraries). If you are running R in an environment where the string
UTF-8 is part of either LC_MESSAGES or LC_ALL variables, unicode line drawing
characters will be used to draw lines in the Object Browser. This is the case
of most Linux distributions.

In the Libraries view, you can either double click or press <Enter> over a
library to see its objects. In the Object Browser, the libraries have the
color defined by the PreProc highlighting group, and the other objects have
their colors defined by the return value of some R functions. Each line in the
table below shows a highlighting group and the corresponding R function (if
any) used to classify the objects:

	 PreProc	libraries
	 Number		is.numeric()
	 String		is.character()
	 Special	is.factor()
	 Boolean	is.logical()
	 Type		is.list()
	 Function	is.function()
	 Statement	isS4()

One limitation is that objects made available by the command `data()` may not
have their classes recognized in the GlobalEnv view.


------------------------------------------------------------------------------
4.5. Commenting and uncommenting lines~

You can toggle the state of a line as either commented or uncommented by
typing <LocalLeader>xx. The string used to comment the line will be "# ",
"## " or "### ", depending on the values of |vimrplugin_indent_commented| and
|r_indent_ess_comments|.

You can also add the string "# " to the beginning of a line by typing
<LocalLeader>xc and remove it with <LocalLeader>xu. In this case, you can set
the value of vimrplugin_rcomment_string to control what string will be added
to the beginning of the line. Example:
>
   let vimrplugin_rcomment_string = "# "
<
Finally, you can also add comments to the right of a line with the
<LocalLeader>; shortcut. By default, the comment starts at the 40th column,
which can be changed by setting the value of r_indent_comment_column, as
below:
>
   let r_indent_comment_column = 20
<
If the line is longer than 38 characters, the comment will start two columns
after the last character in the line. If you are running <LocalLeader>; over a
selection of lines, the comments will be aligned according to the longest
line.

Note: While typing comments the leader comment string is automatically added
to new lines when you reach 'textwidth' but not when you press <Enter>.
Please, read the Vim help about 'formatoptions' and |fo-table|. For example,
you can add the following line to your |vimrc| if you want the comment string
being added after <Enter>:
>
   autocmd FileType r setlocal formatoptions-=t formatoptions+=croql
<
Tip: You can use Vim substitution command `:%s/#.*//` to delete all comments
in a buffer (see |:s| and |pattern-overview|).


------------------------------------------------------------------------------
								 *:RBuildTags*
4.6. Build a tags file to jump to function definitions~

Vim can jump to functions defined in other files if you press CTRL-] over the
name of a function, but it needs a tags file to be able to find the function
definition (see |tags-and-searches|). The command  :RBuildTags  calls the R
function `rtags()` to build the tags file for the R scripts in the current
directory. Please read |r-plugin-tagsfile| to learn how to create a tags file
referencing source code located in other directories, including the entire R
source code.


------------------------------------------------------------------------------
							       *r-plugin-tmux*
4.7. Tmux usage~

When running either GVim or Vim in a terminal emulator (Linux/Unix only), the
Vim-R-plugin will use Tmux to start R in a separate terminal emulator. R will
be running inside a Tmux session, but you will hardly notice any difference
from R running directly in the terminal emulator. The remaining of this
section refers to the case of starting R when Vim already is in a Tmux
session, that is, if you do:
>
   tmux
   vim --servername VIM filename.R
   exit
<
In this case, the terminal window is split in two regions: one for Vim and the
other for Tmux. Then, it's useful (but not required) to know some Tmux
commands. After you finished editing the file, you have to type `exit` to quit
the Tmux session.

Note: Starting GVim within a Tmux session is not supported.


------------------------------------------------------------------------------
							 *r-plugin-tmux-setup*
4.7.1 Tmux configuration~

If, as recommended, you always prefer to run Tmux before running you have to
create your `~/.tmux.conf` if it does not exist yet. You may put the lines
below in your `~/.tmux.conf` as a starting point to your own configuration
file:
>
   # Use <C-a> instead of the default <C-b> as Tmux prefix
   set-option -g prefix C-a
   unbind-key C-b
   bind-key C-a send-prefix

   # Options enable mouse support in Tmux
   set -g terminal-overrides 'xterm*:smcup@:rmcup@'
   set -g mode-mouse on
   set -g mouse-select-pane on
   set -g mouse-resize-pane on

   # Act more like vim:
   set-window-option -g mode-keys vi
   bind h select-pane -L
   bind j select-pane -D
   bind k select-pane -U
   bind l select-pane -R
   unbind p
   bind p paste-buffer
   bind -t vi-copy v begin-selection
   bind -t vi-copy y copy-selection
<

------------------------------------------------------------------------------
4.7.2 Key bindings and mouse support~

The Tmux configuration file suggested above configures Tmux to use vi key
bindings. It also configures Tmux to react to mouse clicks. You should be able
to switch the active pane by clicking on an inactive pane, to resize the panes
by clicking on the border line and dragging it, and to scroll the R Console
with the mouse wheel. When you use the mouse wheel, Tmux enters in its
copy/scroll back mode (see below).

The configuration script also sets <C-a> as the Tmux escape character (the
default is <C-b>), that is, you have to type <C-a> before typing a Tmux
command. Below are the most useful key bindings to use Tmux with the above
tmux.conf:

    <C-a>arrow keys : Move the cursor to the Tmux panel above, below, at the
                      right or at the left of the current one.

    <C-a><C-Up>     : Move the panel division upward one line, that is, resize
                      the panels. Repeat <C-Up> to move more. <C-Down> will
                      move the division downward one line. If you are using
                      the vertical split, you should use <C-Left> and
                      <C-Right> to resize the panels.

    <C-a>[          : Enter the copy/scroll back mode. You can use <PgUp>,
                      <PgDown> and vi key bindings to move the cursor around
                      the panel. Press q to quit copy mode.

    <C-a>]          : Paste the content of Tmux paste buffer.

    <C-a>z          : Hide/show all panes except the current one.
		      Note: If you mistakenly press <C-a><C-z>, you have to
		      type `fg` to get Tmux back to the foreground.

While in the copy and scroll back mode, the following key bindings are very
useful:

    q               : Quit the copy and scroll mode.
    <Space>         : Start text selection.
    v<Space>        : Start rectangular text selection.
    <Enter>         : Copy the selection to Tmux paste buffer.

Please, read the manual page of Tmux if you want to change the Tmux
configuration and learn more commands. To read the Tmux manual, type in the
terminal emulator:
>
  man tmux
<
Note: Because <C-a> was configured as the Tmux escape character, it will not
be passed to applications running under Tmux. To send <C-a> to either R or Vim
you have to type <C-a><C-a>.


------------------------------------------------------------------------------
4.7.3 Copying and pasting~

You do not need to copy code from Vim to R because you can use the plugin's
shortcuts to send the code. For pasting the output of R commands into Vim's
buffer, you can use the command |:Rinsert|. If you want to copy text from an
application running inside the Tmux to another application also running in
Tmux, as explained in the previous subsection, you can enter in Tmux
copy/scroll mode, select the text, copy it, switch to the other application
pane and, then, paste.

However, if you want to copy something from either Vim or R to another
application not running inside Tmux, Tmux may prevent the X server from
capturing the text selected by the mouse. The solution is to disable mouse
support in Tmux. You will be able to toggle mouse support on and off by typing
<C-a>m if you add the following line to your ~/.tmux.conf:
>
   bind m run-shell '( if [ "mode-mouse off" = "$(tmux show-window-option mode-mouse)" ]; then toggle=on; else toggle=off; fi; tmux display-message "mouse $toggle"; tmux set-option -w mode-mouse $toggle ; for cmd in mouse-select-pane mouse-resize-pane mouse-select-window; do tmux set-option -g $cmd $toggle ; done;) > /dev/null 2>&1'
<

------------------------------------------------------------------------------
							     *r-plugin-remote*
4.7.2 Remote access~

The Vim-R-plugin can not send commands from a local Vim to a remote R, but
you can access the remote machine through ssh and run Tmux, Vim and R in the
remote machine. Tmux should not be running in the local machine because some
environment variables could pass from the local to the remote Tmux and make
the plugin confuse.

If you need to access Vim in a remote Unix machine through ssh, and if you
want to get omnicompletion, syntax highlight of function names and the Object
Browser working properly, instead of Vim, it is recommended that you use
Neovim because it does not require the X Server to be fully functional. You if
you cannot use Neovim, then you have to:

   - Enable X11 Forwarding in the remote machine.

   - Have Vim in the remote machine compiled with |clientserver| feature.

   - Have the X Server running in the local machine.

   - Pass either the `-X` or the `-Y` argument to ssh, as in the examples
     below:
>
     ssh -X yourlogin@remote.address
     ssh -Y yourlogin@remote.address
<
   - Start Tmux and, then, Vim with the |--servername| argument:
>
     tmux
     vim --servername VIM script.R
<
With Tmux, you can detach the Vim-R session and reattach it latter (but the
connection with the XServer could be lost in the process). This is useful if
you plan to begin the use the Vim-R-plugin in a machine and latter move to
another computer and access remotely your previous Vim-R session. Below is the
step-by-step procedure to run the Vim-R remotely:

  - Start Tmux:
      tmux

  - Start Vim:
      vim script.R

  - Use Vim to start an R session:
      <LocalLeader>rf

  - Send code from Vim to R, and, then, detach Vim and R with <C-a>d
    The command will be <C-b>d if you have not set <C-a> as the escape
    character in your ~/.tmux.conf.

  - Some time latter (even if accessing the machine remotely) reattach the
    Tmux session:
      tmux attach


==============================================================================
							 *r-plugin-known-bugs*
5. Known bugs and workarounds~

The bugs that are known to exist but that will not be fixed are listed in this
section. Some of them can not be fixed because they depend on either R or Vim
missing features; others would be very time consuming to fix without breaking
anything.


------------------------------------------------------------------------------
5.1. R's source() issues~

The R's `source()` function of base package prints an extra new line between
commands if the option echo = TRUE, and error and warning messages are printed
only after the entire code is sourced, which makes it more difficult to find
errors in the code sent to R. Details:

   https://stat.ethz.ch/pipermail/r-devel/2012-December/065352.html


------------------------------------------------------------------------------
5.2. The clipboard's content is lost (Windows only)~

On Windows, the plugin copies the command that will be sent to R into the
clipboard. Thus, if you have anything in the clipboard it will be lost while
using the plugin.


------------------------------------------------------------------------------
5.3. The menu may not reflect some of your custom key bindings~

If you have created a custom key binding for the Vim-R-plugin, the menu in
GVim will not always reflect the correct key binding if it is not the same for
Normal, Visual and Insert modes.


------------------------------------------------------------------------------
5.4. Syntactically correct code may be wrongly indented~

If the Vim-R-plugin indents your code wrongly you may get the correct
indentation by adding braces and line breaks to it. For example, try to
indent the code below:
>
   # This code will be wrongly indented:

   levels(x) <- ## nl == nL or 1
       if (nl == nL) as.character(labels)
       else paste(labels, seq_along(levels), sep = "")
   class(x) <- c(if(ordered) "ordered", "factor")


   # But this one will be correctly indented:

   levels(x) <- ## nl == nL or 1
       if (nl == nL)
           as.character(labels)
       else
           paste(labels, seq_along(levels), sep = "")
   class(x) <- c(if(ordered) "ordered", "factor")
<
See also:

   https://github.com/jcfaria/Vim-R-plugin/issues/77
   https://github.com/jcfaria/Vim-R-plugin/blob/master/r-plugin/indent_test.R


------------------------------------------------------------------------------
5.5. Functions are not always correctly sent to R~

The plugin is only capable of recognizing functions defined using the `<-`
operator. See: https://github.com/jcfaria/Vim-R-plugin/issues/94


------------------------------------------------------------------------------
5.6. Objects Browser does not support knitr cache=TRUE~

When processing Rnoweb documents with the knitr package, if the chunk option
`cache` is `TRUE`, the Object Browser will not be able to detect the classes
of objects because knitr uses R's lazy load feature. The workaround for this
issue is to define the chunk option `cache.lazy=FALSE`.


------------------------------------------------------------------------------
5.7. Wrong message that "R is busy" (Windows only)~

On Windows, when code is sent from Vim to R Console, the vimcom library sets
the value of the internal variable `r_is_busy` to 1. The value is set back to
0 when any code is successfully evaluated. If you send invalid code to R,
there will be no successful evaluation of code and, thus, the value of
`r_is_busy` will remain set to 1. Then, if you try to update the object
browser, see the R documentation for any function, or do other tasks that
require the hidden evaluation of code by R, the vimcom library will refuse to
do the tasks to avoid any risk of corrupting R's memory. It will tell Vim that
"R is busy" and Vim will display this message. Everything should work as
expected again after any valid code is executed in the R Console.

The vimcom library is started with the state `busy`.

------------------------------------------------------------------------------
							*r-plugin-SyncTeX-win*
							*r-plugin-SyncTeX-mac*
5.8. SyncTeX on Windows~

On Windows, backward search with Sumatra pop ups a console window (`vim`
running the required --remote-expr command) which quickly disappears.


------------------------------------------------------------------------------
								*r-plugin-nox*
5.9. Bugs that affect Linux Console and Vim accessed through ssh~

If you run Vim in the Linux Console or start Vim in a terminal emulator
without the command line argument |--servername|, Vim will not receive remote
messages and the vimcom package will display one of the following messages:

   Vim was built without the 'clientserver' feature.
   Did you pass the --servername argument to Vim?
   There is no X Server running.
   Vim's servername is unknown.

To avoid the message, you should start vim with the |--servername| argument,
as below:
>
   vim --servername VIM script.R
<
To avoid having to type this argument every time that you start Vim, please,
look at the example in |r-plugin-bash-setup|.

If the |--servername| argument does not work in your case, you can put the
following line in your `~/.Rprofile` to suppress the message:
>
   options(vimcom.verbose = -1)
<
When Vim cannot receive messages, the result is:

   * The Object Browser is not automatically updated. You have to do
     <LocalLeader>ro to update it manually.

   * Syntax highlight of function names and the list of objects for
     omnicompletion are not immediately updated after you load a new R
     package. For a workaround, see |vimrplugin_start_libs|.

   * You cannot see R documentation in a Vim buffer.

On Linux you may find Vim binaries without the |clientserver| feature if
you install packages such as vim-nox on Debian/Ubuntu or vim-enhanced on
Fedora/Red Hat. If you want to use Vim in a terminal emulator in Fedora/Red
Hat, you may want to create a symbolic link to "gvim" named "vim". You still
have to explicitly start the server with the argument |--servername|.

The R package vimcom has support only for Windows and X11 interprocess
communication systems while MacVim is a Cocoa application. For this reason, on
Mac OS X, by default, vimcom is built without support to Vim's |clientserver|
feature. If you prefer to use Vim instead of MacVim in Mac OS X, then, you
should:

   1. Install XQuartz server (X11.app)

   2. Build vimcom with the argument `--enable-clientserver`. That is, download
      vimcom from http://www.lepem.ufc.br/jaa/vimcom.html and do in R:
>
      install.packages("vimcom_1.2-0.tar.gz", repos = NULL, type = "source",
                       configure.args = "--enable-clientserver")
<
   3. Build Vim with X11 support. A user reported success with the following
      procedure:
>
      brew install vim --with-client-server
      tmux
      /usr/local/Cellar/vim/7.4.273/bin/vim --servername VIM r_script.R
<
See also: |r-plugin-remote|.


------------------------------------------------------------------------------
5.10. R must be started by Vim~

The communication between Vim and R will work only if R was started by Vim
through the <LocalLeader>rf command because the plugin was designed to connect
each Vim instance with its own R instance.

If you start R before Vim, it will not inherit from Vim the environment
variables VIMRPLUGIN_TMPDIR, VIMRPLUGIN_COMPLDIR, VIMEDITOR_SVRNM,
VIMINSTANCEID, and VIMRPLUGIN_SECRET. The first one is the path used by the R
package vimcom to save temporary files used by the Vim-R-plugin to: perform
omnicompletion, show R documentation in a Vim buffer, and update the Object
Browser. The two last ones are used by the Vim-R-plugin and by vimcom to know
that the connections are valid. If you use Vim to start R, but then closes
Vim, some variables will become outdated. Additionally, the Vim-R-plugin sets
the value of its internal variable SendCmdToR from SendCmdToR_fake to the
appropriate value when R is successfully started. It is possible to set the
values of all those variables manually, but, as you can see below, it is not
practical to do so. If you have either started R before Vim or closed Vim and
opened it again and really want full communication between Vim and R, you can
try the following (not all procedures are necessary for all cases):

   In Normal mode Vim do:
>
   :echo g:rplugin_tmpdir
   :echo g:rplugin_compldir
   :echo $VIMINSTANCEID
   :echo $VIMRPLUGIN_SECRET
   :echo $VIMEDITOR_SVRNM
<
   In R do:
>
   detach("package:vimcom", unload = TRUE)
   Sys.setenv(VIMRPLUGIN_TMPDIR="T")    # where "T" is what Vim has echoed
   Sys.setenv(VIMINSTANCEID="I")        # where "I" is what Vim has echoed
   Sys.setenv(VIMRPLUGIN_SECRET"="S")   # where "S" is what Vim has echoed
   Sys.setenv(VIMEDITOR_SVRNM"="N")     # where "N" is what Vim has echoed
   Sys.setenv(VIMRPLUGIN_COMPLDIR"="C") # where "C" is what Vim has echoed
   library(vimcom)
<
If you are running R in a terminal emulator (Linux/Unix) Vim still needs to
know the name of Tmux session and Tmux pane where R is running.

So, in R do:
>
   Sys.getenv("TMUX_PANE")
<
   and the following Tmux command:
>
   <Ctrl-A>:display-message -p '#S'<Enter><Enter>
<
And in Normal mode Vim do:
>
   :let rplugin_rconsole_pane = "X"
   :let rplugin_tmuxsname = "Y"
<
Finally, do one of the commands below in Normal mode Vim, according to how R
is running:
>
   let SendCmdToR = function('SendCmdToR_TmuxSplit')
   let SendCmdToR = function('SendCmdToR_Term')
   let SendCmdToR = function('SendCmdToR_OSX')
   let SendCmdToR = function('SendCmdToR_Windows')
<

==============================================================================
							    *r-plugin-options*
6. Options~

|vimrplugin_term|              External terminal to be used
|vimrplugin_term_cmd|          Complete command to open an external terminal
|vimrplugin_sleeptime|         Delay while sending commands in MS Windows
|vimrplugin_assign|            Convert '_' into ' <- '
|vimrplugin_assign_map|        Choose what to convert into ' <- '
|vimrplugin_rnowebchunk|       Convert '<' into '<<>>=\n@' in Rnoweb files
|vimrplugin_objbr_place|       Placement of Object Browser
|vimrplugin_objbr_w|           Initial width of Object Browser window
|vimrplugin_tmux_ob|           Run Object Browser in Tmux pane
|vimrplugin_vimpager|          Use Vim to see R documentation
|vimrplugin_editor_w|          Minimum width of R script buffer
|vimrplugin_help_w|            Desired width of R documentation buffer
|vimrplugin_i386|              Use 32 bit version of R
|vimrplugin_r_path|            Directory where R is
|vimrplugin_r_args|            Arguments to pass to R
|vimrplugin_start_libs|        Objects for omnicompletion and syntax highlight
|vimrplugin_routnotab|         Show output of R CMD BATCH in new window
|vimrplugin_indent_commented|  Indent lines commented with the \xx command
|vimrplugin_notmuxconf|        Don't use a specially built Tmux config file
|vimrplugin_rconsole_height|   The number of lines of R Console (Tmux split)
|vimrplugin_vsplit|            Make Tmux split the window vertically
|vimrplugin_rconsole_width|    The number of columns of R Console (Tmux split)
|vimrplugin_tmux_title|        Set Tmux window title to "VimR"
|vimrplugin_applescript|       Use osascript in Mac OS X
|vimrplugin_listmethods|       Do `vim.list.args()` instead of `args()`
|vimrplugin_specialplot|       Do `vim.plot()` instead of `plot()`
|vimrplugin_maxdeparse|        Argument to R `source()` function
|vimrplugin_latexcmd|          Command to run on .tex files
|vimrplugin_latexmk|           Define wether `latexmk` should be run
|vimrplugin_sweaveargs|        Arguments do `Sweave()`
|vimrplugin_rmd_environment|   Environment in which to save evaluated rmd code
|vimrplugin_never_unmake_menu| Do not unmake the menu when switching buffers
|vimrplugin_map_r|             Use 'r' to send lines and selected text
|vimrplugin_ca_ck|             Add ^A^K to the beginning of commands
|vimrplugin_pdfviewer|         PDF application used to open PDF documents
|vimrplugin_openpdf|           Open PDF after processing rnoweb file
|vimrplugin_openhtml|          Open HTML after processing either Rrst or Rmd
|vimrplugin_strict_rst|        Code style for generated rst files
|vimrplugin_insert_mode_cmds|  Allow R commands in insert mode
|vimrplugin_allnames|          Show names which begin with a dot
|vimrplugin_rmhidden|          Remove hidden objects from R workspace
|vimrplugin_source|            Source additional scripts
|vimrplugin_restart|           Restart R if it is already running
|vimrplugin_show_args|         Show extra information during omnicompletion
|vimrplugin_vimcom_wait|       Time to wait for vimcom loading
|vimrplugin_vim_wd|            Start R in Vim's working directory
|vimrplugin_user_maps_only|    Only set user specified key bindings
|vimrplugin_tmpdir|            Where temporary files are created
|vimrplugin_compldir|          Where lists for omnicompletion are stored
|r-plugin-SyncTeX|             Options for SyncTeX


------------------------------------------------------------------------------
							     *vimrplugin_term*
6.1. Terminal emulator (Linux/Unix only)~

Note: The options of this section are ignored on Mac OS X, where the command
`open` is called to run the default application used to run shell scripts.

The plugin uses the first terminal emulator that it finds in the following
list:
    1. gnome-terminal,
    2. konsole,
    3. xfce4-terminal,
    4. iterm,
    5. Eterm,
    6. (u)rxvt,
    7. aterm,
    8. roxterm,
    9. terminator,
   10. xterm.

If Vim does not select your favorite terminal emulator, you may define it in
your |vimrc| by setting the variable vimrplugin_term, as shown below:
>
   let vimrplugin_term = "xterm"
   let vimrplugin_term = "/Applications/Utilities/Terminal.app/Contents/MacOS/Terminal"
<
							 *vimrplugin_term_cmd*
If your terminal emulator is not listed above, or if you are not satisfied
with the way your terminal emulator is called by the plugin, you may define in
your |vimrc| the variable vimrplugin_term_cmd, as in the examples below:
>
   let vimrplugin_term_cmd = "gnome-terminal --title R -e"
   let vimrplugin_term_cmd = "terminator --title R -x"
   let vimrplugin_term_cmd = "/Applications/Utilities/iTerm.app/Contents/MacOS/iTerm -t R"
<
Please, look at the manual of your terminal emulator to know how to call it.
The last argument must be the one which precedes the command to be executed.


------------------------------------------------------------------------------
							*vimrplugin_sleeptime*
6.2. Options specific to Windows~


The plugin gives to R a small amount of time to process the paste command. The
default value is 100 milliseconds, but you should experiment different values. The
example show how to adjust the value of sleeptime in your |vimrc|:
>
   let vimrplugin_sleeptime = 30
<
Note: the option vimrplugin_Rterm was disabled because the C code to send
strings from Vim to Windows PowerShell is not working.


------------------------------------------------------------------------------
						      *vimrplugin_rnowebchunk*
	                                              *vimrplugin_assign_map*
                                                      *vimrplugin_assign*
6.3. Assignment operator and Rnoweb completion of code block~

In Rnoweb files, a `<` is replaced with `<<>>=\n@`. To disable this feature,
put in your |vimrc|:
>
   let vimrplugin_rnowebchunk = 0
<
While editing R code, `_` is replaced with `<-`. If you want to bind other
keys to be replaced by `<-`, set the value of |vimrplugin_assign_map| in your
|vimrc|, as in the example below which emulates RStudio behavior (may only
work on GVim):
>
   let vimrplugin_assign_map = "<M-->"
<
Note: If you are using Vim in a terminal emulator, you have to put in your
|vimrc|:
>
   set <M-->=^[-
   let vimrplugin_assign_map = "<M-->"
<
where `^[` is obtained by pressing CTRL-V CTRL-[ in Insert mode.

Note: You can't map <C-=>, as StatET does because in Vim only alphabetic
letters can be mapped in combination with the CTRL key.

To completely disable this feature, put in your |vimrc|:
>
   let vimrplugin_assign = 0
<
If you need to type many object names with underscores, you may want to change
the value vimrplugin_assign to 2. Then, you will have to type two `_` to get
them converted into `<-`.

------------------------------------------------------------------------------
						      *vimrplugin_objbr_place*
						      *vimrplugin_objbr_w*
						      *vimrplugin_tmux_ob*
6.4. Object Browser options~

By default, the object browser will be created with 40 columns. The minimum
width of the Object Browser window is 9 columns. You can change the object
browser's default width by setting the value of |vimrplugin_objbr_w| in your
|vimrc|, as below:
>
   let vimrplugin_objbr_w = 30
<
The Object Browser is created by splitting the Vim script window, but if Vim
is running in a terminal emulator inside a Tmux session, the Object Browser
will be created in an independent Vim instance in a Tmux pane. If you prefer
the Object Browser always created as a Vim split window, put in your |vimrc|:
>
   let vimrplugin_tmux_ob = 0
<
Valid values for the Object Browser placement are "script" or "console" and
"right" or "left" separated by a comma. Examples:
>
   let vimrplugin_objbr_place = "script,right"
   let vimrplugin_objbr_place = "console,left"
<

------------------------------------------------------------------------------
							 *vimrplugin_vimpager*
							 *vimrplugin_editor_w*
							 *vimrplugin_help_w*
6.5. Vim as pager for R help~

6.5.1. Quick setup~

If you do not want to see R documentation in a Vim's buffer, put in your
|vimrc|:
>
   let vimrplugin_vimpager = "no"
<
And put in your `~/.Rprofile`:
>
   options(vimcom.vimpager = FALSE)
<
If you want to see R documentation in Vim, but are not satisfied with the way
it works, please, read the subsection 6.5.2 below.

------------------------------------------------------------------------------
6.5.2. Details and other options:~

The plugin key bindings will remain active in the documentation buffer, and,
thus, you will be able to send commands to R as you do while editing an R
script. You can, for example, use <LocalLeader>rh to jump to another R help
document.

The valid values of vimrplugin_vimpager are:

   "tab"       : Show the help document in a new tab. If there is already a
                 tab with an R help document tab, use it.
                 This is the default.
   "vertical"  : Split the window vertically if the editor width is large
                 enough; otherwise, split the window horizontally and attempt
                 to set the window height to at least 20 lines.
   "horizontal": Split the window horizontally.
   "tabnew"    : Show the help document in a new tab.
   "no"        : Do not show R documentation in Vim.

The window will be considered large enough if it has more columns than
vimrplugin_editor_w + vimrplugin_help_w. These variables control the minimum
width of the editor window and the help window, and their default values are,
respectively, 66 and 46. Thus, if you want to have more control over Vim's
behavior while opening R's documentations, you will want to set different
values to some variables in your |vimrc|, as in the example:
>
   let vimrplugin_editor_w = 80
   let vimrplugin_editor_h = 60
<

------------------------------------------------------------------------------
							     *vimrplugin_i386*
6.6. Use 32 bit version of R (Windows and Mac OS X only)~

If you are using a 64 bit Windows or a 64 bit Mac OS X, but prefer to run the
32 bit version of R, put in your |vimrc|:
>
   let vimrplugin_i386 = 1
<

------------------------------------------------------------------------------
							   *vimrplugin_r_path*
6.7. R path~

Vim will run the first R executable in the path. You can set an alternative R
path in your |vimrc| as in the examples:
>
   let vimrplugin_r_path = "/path/to/my/preferred/R/version/bin"
   let vimrplugin_r_path = "C:\\Program Files\\R\\R-3.1.2\\bin\\i386"
<
On Windows, Vim will try to find the R install path in the Windows Registry.

You can set a different R version for specific R scripts in your |vimrc|.
Example:
>
   autocmd BufReadPre ~/old* let vimrplugin_r_path='~/app/R-2.8.1/bin'
<

------------------------------------------------------------------------------
							   *vimrplugin_r_args*
6.8. Arguments to R~

Set this option in your |vimrc| if you want to pass command line arguments to
R at the startup. Example:
>
   let vimrplugin_r_args = "--sdi --no-save --quiet"
<
On Linux, there is no default value for |vimrplugin_r_args|. On Windows, the
default value is "--sdi", but you may change it to "--mdi" if you do not like
the SDI style of the graphical user interface.


------------------------------------------------------------------------------
						   *vimrplugin_start_libs*
6.9. Omnicompletion and syntax highlight of R functions~

The list of functions to be highlighted and the list of objects for
omnicompletion are built dynamically as the libraries are loaded by R.
However, you can set the value of vimrplugin_start_libs if you want that
the functions and objects of specific packages are respectively highlighted
and available for omnicompletion even if R is not running yet. By default,
only the functions of vanilla R are always highlighted. Below is the default
value of vimrplugin_start_libs:
>
   let vimrplugin_start_libs = "base,stats,graphics,grDevices,utils,methods"
<
You may also want to define the value of vimrplugin_start_libs if you are
running Vim without the |clientserver| feature.


------------------------------------------------------------------------------
							*vimrplugin_routnotab*
6.10. Options for .Rout file file type~

After the command <LocalLeader>ao, Vim will save the current buffer if it has
any pending changes, run `R CMD BATCH --no-restore --no-save` on the current
file and show the resulting .Rout file in a new tab. If you prefer that the
file is open in a new split window, put in your |vimrc|:
>
   let vimrplugin_routnotab = 1
<
By default, the R commands in .Rout files are highlighted with the color of
comments, and only the output of commands has some of its elements highlighted
(numbers, strings, index of vectors, warnings and errors).

If you prefer that R commands in the R output are highlighted as they are in R
scripts, put the following in your |vimrc|:
>
   let Rout_more_colors = 1
<

------------------------------------------------------------------------------
						 *vimrplugin_indent_commented*
                                                 *r_indent_ess_comments*
6.11. Indent commented lines~

You can type <LocalLeader>xx to comment out a line or selected lines. If the
line already starts with a comment string, it will be removed. After adding
the comment string, the line will be reindented by default. To turn off the
automatic indentation, put in your |vimrc|:
>
   let vimrplugin_indent_commented = 0
<
What string will be added to the beginning of the line depends on the values
of vimrplugin_indent_commented and r_indent_ess_comments according to the
table below (see |r-plugin-indenting|):
>
   vimrplugin_indent_commented   r_indent_ess_comments   string
                 1                        0                #
                 0                        0                #
                 1                        1                ##
                 0                        1                ###
<

------------------------------------------------------------------------------
						       *vimrplugin_notmuxconf*
6.12. Tmux configuration (Linux/Unix only)~


GVim (or Vim running R in an external terminal emulator) runs Tmux with a
specially built configuration file. If you want to use your own ~/.tmux.conf,
put in your |vimrc|:
>
   let vimrplugin_notmuxconf = 1
<
If you opted for using your own configuration file, the plugin will write a
minimum configuration which will set the value of four environment variables
required for the communication with R and then source your own configuration
file (~/.tmux.conf).


------------------------------------------------------------------------------
						  *vimrplugin_rconsole_height*
                                                  *vimrplugin_vsplit*
			                          *vimrplugin_rconsole_width*
                                                  *vimrplugin_tmux_title*
6.13. Integration with Tmux (Linux/Unix only)~

These three options are valid only when Vim is started inside a Tmux session.
In this case, when you type <LocalLeader>rf, the terminal will be split in two
regions and R will run in one of them. By default, the Vim-R-plugin will tell
Tmux to split the terminal window horizontally and you can set in your
|vimrc| the initial number of lines of the Tmux pane running R as in the
example below:
>
   let vimrplugin_rconsole_height = 15
<
If you prefer to split it vertically:
>
   let vimrplugin_vsplit = 1
<
In this case, you can choose the initial number of columns of R Console:
>
   let vimrplugin_rconsole_width = 15
<
Tmux automatically renames window titles to the command currently running. The
Vim-R-plugin sets the title of the window where Vim and R are running to
"VimR". This title will be visible only if Tmux status bar is "on", and it is
useful only if you have created new windows with the <C-a>c command. You
can change the value of vimrplugin_tmux_title to either set a different title
or let Tmux set the title automatically. Examples:
>
   let vimrplugin_tmux_title = "Vim-R"
   let vimrplugin_tmux_title = "automatic"
<

------------------------------------------------------------------------------
                                                      *vimrplugin_applescript*
6.14. Integration with AppleScript (OS X only)~

In Mac OS X, the plugin will try to send commands to R gui using AppleScript.
If you prefer to run R in an external terminal emulator, put in your |vimrc|:
>
   let vimrplugin_applescript = 0
<
If Vim is running inside Tmux, the terminal will be split in two regions.
Otherwise, R will start in an external terminal emulator.


------------------------------------------------------------------------------
						      *vimrplugin_listmethods*
                                                      *vimrplugin_specialplot*
6.15. Special R functions~

The R function `args()` lists the arguments of a function, but not the arguments
of its methods. If you want that the plugin calls the function
`vim.list.args()` after <LocalLeader>ra, you have to add to your |vimrc|:
>
   let vimrplugin_listmethods = 1
<
By default, R makes a scatterplot of numeric vectors. The function `vim.plot()`
do both a histogram and a boxplot. The function can be called by the plugin
after <LocalLeader>rg if you put the following line in your |vimrc|:
>
   let vimrplugin_specialplot = 1
<

------------------------------------------------------------------------------
						       *vimrplugin_maxdeparse*
6.16. maxdeparse~

You can set the argument maxdeparse to be passed to R's `source()` function.
Example:
>
   let vimrplugin_maxdeparse = 300
<

------------------------------------------------------------------------------
						       *vimrplugin_sweaveargs*
						       *vimrplugin_latexcmd*
						       *vimrplugin_latexmk*
6.17. LaTeX command~

To produce a pdf document from the .tex file generated by either `Sweave()` or
`knit()` command, if vimrplugin_latexmk = 1 and both `latexmk` and `perl`
(which is required to run `latexmk`) are installed and in the path, the vimcom
package calls:
>
   latexmk -pdflatex="pdflatex -file-line-error -synctex=1" -pdf
<
Otherwise, it calls:
>
   pdflatex -file-line-error -synctex=1
<
You can use the options vimrplugin_latexcmd and vimrplugin_latexmk to change
this behavior. Examples:
>
   let vimrplugin_latexmk = 0
   let vimrplugin_latexcmd = "latex"
   let vimrplugin_latexcmd = 'latexmk -pdf -pdflatex="xelatex %O -synctex=1 %S"'
<
By default, vimrplugin_latexmk is 0 on Windows and 1 on other systems.
If you want to pass arguments do the `Sweave()` function, set the value of the
vimrplugin_sweaveargs variable.


------------------------------------------------------------------------------
						  *vimrplugin_rmd_environment*
6.18. Rmd environment~

When rendering an Rmd file, the code can be evaluated (and saved) in a
specified environment.  The default value is `.GlobalEnv` which makes the
objects stored in the Rmd file available on the R console.  If you do not want
the  objects stored in the Rmd file to be available in the global environment,
you can set
>
    let vimrplugin_rmd_environment = "new.env()"
<

------------------------------------------------------------------------------
						*vimrplugin_never_unmake_menu*
6.19. Never unmake the R menu~

Use this option if you want that the "R" menu item in GVim is not deleted when
you change from one buffer to another, for example, when going from an .R file
to a .txt one:
>
   let vimrplugin_never_unmake_menu = 1
<
When this option is enabled all menu items are created regardless of the file
type. If you have added R related tool bar buttons (see |r-plugin-toolbar|)
the buttons also are created at the plugin startup and kept while you go to
different file type buffers.


------------------------------------------------------------------------------
							    *vimrplugin_map_r*
6.20. Map 'r'~

If the variable |vimrplugin_map_r| exists, the plugin will map the letter 'r'
to send lines to R when there are visually selected lines, for compatibility
with the original plugin. To activate this option, insert the following into
|vimrc|:
>
   let vimrplugin_map_r = 1
<
You may want to add the following three lines to your |vimrc| which were in
Johannes Ranke's plugin and will increase compatibility with code edited with
Emacs:
>
   set expandtab
   set shiftwidth=4
   set tabstop=8
<

------------------------------------------------------------------------------
							    *vimrplugin_ca_ck*
6.21. Add ^A^K to the beginning of commands~

When one types <C-a> in the R Console the cursor goes to the beginning of the
line and when one types <C-k> the characters to the right of the cursor are
deleted. This is useful to avoid characters left on the R Console being mixed
with commands sent by Vim. However, sending <C-a> may be problematic if using
Tmux. The Vim-R-plugin will add <C-a><C-k> to every command if you put
in your |vimrc|:
>
   let vimrplugin_ca_ck = 1
<

------------------------------------------------------------------------------
	                                                *vimrplugin_pdfviewer*
                                                        *vimrplugin_openpdf*
                                                        *vimrplugin_openhtml*
6.22. Open PDF after processing rnoweb, rmd or rrst files~

The plugin can automatically open the pdf file generated by pdflatex, after
either `Sweave()` or `knit()`. This behavior is controlled by the variable
|vimrplugin_openpdf| whose value may be 0 (do not open the pdf), 1 (open only
the first time that pdflatex is called) or a number higher than 1 (always
open the pdf). For example, if you want that the pdf application is started
automatically but do not want the terminal (or GVim) losing focus every time
that you generate the pdf, you should put in put in your |vimrc|:
>
   let vimrplugin_openpdf = 1
<
If you use Linux or other Unix and eventually use the system console (without
the X server) you may want to put in your |vimrc|:
>
   if $DISPLAY != ""
       let vimrplugin_openpdf = 1
   endif
<
Note: If the pdf is already open, some pdf readers will automatically update
the pdf; others will lock the pdf file and prevent R from successfully
compiling it again. You can change the value of vimrplugin_pdfviewer in your
|vimrc| to define what PDF viewer will be called. Example:
>
   let vimrplugin_pdfviewer = "zathura"
<
If editing an Rmd file, you can produce the html result with <LocalLeader>kh.
The html file will be automatically opened if you put the following in your
|vimrc|:
>
   let vimrplugin_openhtml = 1
<

------------------------------------------------------------------------------
                                                     *vimrplugin_rrstcompiler*
			                             *vimrplugin_strict_rst*
                                                     *vimrplugin_rst2pdfpath*
			                             *vimrplugin_rst2pdfargs*
6.23. Support to RreStructuredText file~

By default, the Vim-R-plugin sends the command `render_rst(strict=TRUE)` to R
before using R's `knit()` function to convert an Rrst file into an rst one. If
you prefer the non strict rst code, put the following in your |vimrc|:
>
   let vimrplugin_strict_rst = 0
<
You can also set the value of vimrplugin_rst2pdfpath (the path to rst2pdf
application), vimrplugin_rrstcompiler (the compiler argument to be passed to R
function knit2pdf), and vimrplugin_rst2pdfargs (further arguments to be passed
to R function knit2pdf).


------------------------------------------------------------------------------
						 *vimrplugin_insert_mode_cmds*
6.24. Allow R commands in insert mode~

Vim-R commands are designed to work in insert mode as well as normal mode.
However, depending on your <LocalLeader>, this can make it very difficult to
write R packages or Sweave files.  For example, if <LocalLeader> is set to the
`\` character, typing `\dQuote` in a .Rd file tries to send the command!

The option vimrplugin_insert_mode_cmds disables commands in insert mode.  To
use it, add the following to your |vimrc|:
>
   let g:vimrplugin_insert_mode_cmds = 0
<
The default value is 1, for consistency with earlier versions.

See also: |r-plugin-localleader|.


------------------------------------------------------------------------------
							 *vimrplugin_allnames*
							 *vimrplugin_rmhidden*
6.25. Show/remove hidden objects~

Hidden objects are not included in the list of objects for omni completion. If
you prefer to include them, put in your |vimrc|:
>
   let g:vimrplugin_allnames = 1
<
Hidden objects are removed from R workspace when you do <LocalLeader>rm. If
you prefer to remove only visible objects, put in your |vimrc|:
>
   let g:vimrplugin_rmhidden = 0
<

------------------------------------------------------------------------------
							   *vimrplugin_source*
6.26. Source additional scripts~

This variable should contain a comma separated list of Vim scripts to be
sourced by the Vim-R-plugin. These scripts may provide additional
functionality and/or change the behavior of the Vim-R-plugin. If you have such
scripts, put in your |vimrc|:
>
   let vimrplugin_source = "~/path/to/MyScript.vim,/path/to/AnotherScript.vim"
<
Currently, there are only two scripts known to extend the Vim-R-plugin
features:

   Support to the devtools R package~
   https://github.com/mllg/vim-devtools-plugin

   Basic integration with GNU screen~
   https://github.com/jalvesaq/screenR


------------------------------------------------------------------------------
							  *vimrplugin_restart*
6.27. Restart R if it is already running (Linux/Unix only)~

When R is already running and you type one of the commands to start R before
you have done <LocalLeader>rq, the Vim-R-plugin does one of the following:
(a) If R is in an external terminal emulator, the terminal is closed, a new
one is opened with the same R session running in it. (b) If both Vim and R are
running in different Tmux regions of the same terminal emulator, the plugin
warns that R is already running.

If instead of the default behavior, you prefer to quit and restart R when you
do <LocalLeader>rf, <LocalLeader>rv or <LocalLeader>rc, then, put in your
|vimrc|:
>
   let vimrplugin_restart = 1
<

------------------------------------------------------------------------------
							*vimrplugin_show_args*
6.28. Show extra information during omnicompletion~

If you want that Vim shows a preview window with the function arguments as you
do omnicompletion, put in your |vimrc|:
>
   let vimrplugin_show_args = 1
<
The preview window is not shown by default because it is more convenient to
run <Ctrl-X><Ctrl-A> to complete the function arguments. The preview window
will be shown only if "preview" is also included in your 'completeopt'.


------------------------------------------------------------------------------
						      *vimrplugin_vimcom_wait*
6.29. Time to wait for vimcom loading~

The Vim-R-plugin waits 5000 milliseconds for vimcom package to be loaded
during R startup. It then checks whether you are using the correct version of
vimcom. If 5000 milliseconds is not enough to your R startup, then set a
higher value for the variable in your |vimrc|. Example:
>
   let vimrplugin_vimcom_wait = 10000
<
Note: You should have the line  `library(vimcom)`  in your `~/.Rprofile`.


------------------------------------------------------------------------------
							   *vimrplugin_vim_wd*
6.30 Start R in working directory of Vim~

The Vim-R-plugin starts R in the directory where the current buffer is. If you
want R's working directory to be the same as Vim's working directory, put in
your |vimrc|:
>
    let vimrplugin_vim_wd = 1
<
This option is useful only for those who did not enable 'autochdir'.


------------------------------------------------------------------------------
						   *vimrplugin_user_maps_only*
6.31 Only set key bindings that are user specified~

The Vim-R-plugin sets many default key bindings.  The user can set custom
key bindings (|r-plugin-key-bindings|).  If you wish the Vim-R-plugin to only
set those key-bindings specified by the user, put in your vimrc:
>
    let vimrplugin_user_maps_only = 1
<

------------------------------------------------------------------------------
							   *vimrplugin_tmpdir*
							 *vimrplugin_compldir*
6.32 Temporary files directories~

You can change the directories where temporary files are created and
stored by setting in your |vimrc| the values of vimrplugin_tmpdir and
vimrplugin_compldir, as in the example below:
>
   let vimrplugin_tmpdir = "/dev/shm/R_tmp_dir"
   let vimrplugin_compldir = "~/.cache/Vim-R-plugin"
<
The default paths of these directories depend on the operating system. If you
want to know they are, while editing an R file, do in Normal mode:
>
   :echo g:rplugin_tmpdir
   :echo g:rplugin_compldir
<

------------------------------------------------------------------------------
							    *r-plugin-SyncTeX*
6.33 SyncTeX support (Linux only)~

SyncTeX is a set of communication systems used by some PDF viewers and by some
text editors which allow users to jump from a specific line in the text editor
to the corresponding line in the PDF viewer and vice-versa. The Vim-R-plugin
has support for Evince, Zathura and Okular SyncTeX systems.

The application `wmctrl` is required to raise both the PDF viewer and Vim
windows. Limitation: the Vim-R-plugin may raise the wrong terminal window or
GVim window if there is more than one open.

No configuration is required if you use Evince, Gnome-Terminal, the knitr
package, and single file Rnoweb documents. Otherwise, keep reading...

If you use `Sweave()` rather than `knit()`, you must put in your Rnoweb
document:
>
   \SweaveOpts{concordance=TRUE}
<
If you work with a master document and child subdocuments, each child
subdocument (TeX and Rnoweb alike) should include the following line:
>
   % !Rnw root = master.Rnw
<
where `master.Rnw` must be replaced with the name of the actual master
document.

Note: The current knitr package (version 1.7) has at least two limitations:

   - It has no SyncTeX support for child documents. The correspondence data
     point to lines right below child chunks in the master document and not to
     somewhere in the child documents themselves. See:
     https://github.com/yihui/knitr/issues/225

   - It only starts registering the concordance after the first chunk. So, it
     is recommended that you put the first chunk of R code just after the
     `\begin{document}` command.


------------------------------------------------------------------------------
6.33.1 Evince configuration~

If you have Evince installed, the Vim-R-plugin assumes that you are using it
to view PDF documents and that you run either GVim or Vim in Gnome-Terminal.
If you run Vim in another terminal emulator, you have to set the value of
|vimrplugin_vim_window|, as explained below.

Note: If Evince is not started yet when you try to jump to the PDF document
for the first time, it will start, but will not jump to desired line; you have
to press <LocalLeader>gp again.


------------------------------------------------------------------------------
6.33.2 Okular configuration~

You have to configure Okular to call Vim during backward searches.
In Okular, click in:
>
   Settings
   Configure Okular
   Editor
   Dropdown menu: Custom Text Editor
         Command: vim --remote-expr "SyncTeX_backward('%f', %l)"

In the command above, replace `vim` with `gvim` if you use GVim.

If Evince is not installed in your system, the Vim-R-plugin will automatically
use Okular as the PDF viewer. Otherwise, you have to set the value of
|vimrplugin_pdfviewer| to "okular".

If the Vim-R-plugin is set to use Okular as the PDF viewer, it assumes that
you run either GVim or Vim in Konsole. If you run Vim in another terminal
emulator, you have to set the value of |vimrplugin_vim_window|, as explained
below.

Note: If the PDF document is already open the first time that you jump to it,
and if Okular was not started with the `--unique` argument, another instance
of Okular will be started.


------------------------------------------------------------------------------
6.33.3 Zathura configuration~

Zathura (version >= 0.3.1) is also supported. If the terminal emulator is not
raised during backward search (from PDF to Vim), you have to set the value of
|vimrplugin_vim_window|, as explained below.

Note: If Zathura is not started yet when you try to jump to the PDF document
for the first time, it will start, but will not jump to desired line; you have
to press <LocalLeader>gp again.


------------------------------------------------------------------------------
6.33.4 Skim with MacVim configuration~

You have to configure Skim to call MacVim during backward searches.
In Skim click in the drop down menu and fill the fields:
>
   Skim
   Settings
   Sync
   Preset: Custom
   Command: /path/to/MacVim/mvim
   Arguments: --remote-expr "SyncTeX_backward('%file', %line)"
<

------------------------------------------------------------------------------
                                                       *vimrplugin_vim_window*
				                       *vimrplugin_synctex*
6.33.4 Configuring the PDF editor and Vim's window title~

If SyncTeX does not work (see above), you may try to set in your |vimrc| the
values of |vimrplugin_pdfviewer| and vimrplugin_vim_window.

The valid values for |vimrplugin_pdfviewer| are "evince", "okular" and
"zathura".

To know the correct value of vimrplugin_vim_window, type in the terminal
emulator:
>
   wmctrl -xl
<
This command will list the windows currently running under the X Window
system. You must set the value of vimrplugin_vim_window as a string that is
part of the title of the window where Vim is running ("GVim", "Terminal",
"Konsole", "yourlogin", "vim", etc).

Example of |vimrc| configuration:
>
   let vimrplugin_pdfviewer = "okular"
   let vimrplugin_vim_window = "XTerm"
<
To completely disable SyncTeX support, put in your |vimrc|:
>
   let vimrplugin_synctex = 0
<

There is no support for Windows and Mac OS X. See |r-plugin-SyncTeX-win| and
|r-plugin-SyncTeX-mac|.


==============================================================================
						       *r-plugin-key-bindings*
7. Custom key bindings~

When creating custom key bindings for the Vim-R-plugin, it is necessary to
create three maps for most functions because the way the function is called is
different in each Vim mode. Thus, key bindings must be made for Normal mode,
Insert mode, and Visual mode.

To customize a key binding you should put in your |vimrc| something like:
>
   nmap <LocalLeader>sr <Plug>RStart
   imap <LocalLeader>sr <Plug>RStart
   vmap <LocalLeader>sr <Plug>RStart
<
The above example shows how to change key binding used to start R from
<LocalLeader>rf to <LocalLeader>sr.

Only the custom key bindings for Normal mode are shown in Vim's menu, but you
can type |:map| to see the complete list of current mappings, and below is the
list of the names for custom key bindings (the prefix RE means "echo";
RD, "cursor down"; RED, both "echo" and "down"):

   Star/Close R~
   RStart
   RVanillaStart
   RCustomStart
   RClose
   RSaveClose

   Clear R console~
   RClearAll
   RClearConsole

   Edit R code~
   RSimpleComment
   RSimpleUnComment
   RToggleComment
   RRightComment
   RCompleteArgs
   RIndent
   RNextRChunk
   RPreviousRChunk

   Send line or part of it to R~
   RSendLine
   RDSendLine
   RSendLAndOpenNewOne
   RNLeftPart
   RNRightPart
   RILeftPart
   RIRightPart
   RDSendLineAndInsertOutput

   Send code to R console~
   RSendSelection
   RESendSelection
   RDSendSelection
   REDSendSelection
   RSendMBlock
   RESendMBlock
   RDSendMBlock
   REDSendMBlock
   RSendParagraph
   RESendParagraph
   RDSendParagraph
   REDSendParagraph
   RSendFunction
   RESendFunction
   RDSendFunction
   REDSendFunction
   RSendFile
   RESendFile

   Send command to R~
   RHelp
   RPlot
   RSPlot
   RShowArgs
   RShowEx
   RShowRout
   RObjectNames
   RObjectPr
   RObjectStr
   RSetwd
   RSummary
   RListSpace

   Support to Sweave and knitr~
   RSendChunk
   RDSendChunk
   RESendChunk
   REDSendChunk
   RSendChunkFH (from the first chunk to here)
   RBibTeX    (Sweave)
   RBibTeXK   (Knitr)
   RSweave
   RKnit
   RMakeHTML
   RMakeODT
   RMakePDF   (Sweave)
   RMakePDFK  (Knitr)
   RMakePDFKb (.Rmd, beamer)
   RMakeRmd   (rmarkdown default)
   RMakeAll   (rmarkdown all in yaml)
   ROpenPDF
   RSyncFor   (SyncTeX search forward)
   RGoToTeX   (Got to LaTeX output)
   RSpinFile
   RNextRChunk
   RPreviousRChunk

   Object browser~
   RUpdateObjBrowser
   ROpenLists
   RCloseLists

The completion of function arguments only happens in Insert mode. To customize
its keybind you should put in your |vimrc| something as in the example:
>
   imap <C-A> <Plug>RCompleteArgs
<
The plugin also contains a function called RAction which allows you to build
ad-hoc commands to R. This function takes the name of an R function such as
"levels" or "table" and the word under the cursor, and passes them to R as a
command.

For example, if your cursor is sitting on top of the object called gender and
you call the RAction function, with an argument such as levels, Vim will pass
the command `levels(gender)` to R, which will show you the levels of the
object gender. To make it even easier to use this and other functions, you
could write custom key bindings in your |vimrc|, as in the examples below:
>
   map <silent> <LocalLeader>rk :call RAction("levels")<CR>
   map <silent> <LocalLeader>t :call RAction("tail")<CR>
   map <silent> <LocalLeader>h :call RAction("head")<CR>
<
If the command that you want to send does not require an R object as argument,
you can create a shortcut to it by following the example:
>
   map <silent> <LocalLeader>s :call g:SendCmdToR("search()")
<
See also: |vimrplugin_source|.


==============================================================================
							      *r-plugin-files*
8. License and files~

The Vim-R-plugin is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option) any
later version.

The Vim-R-plugin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available at
http://www.r-project.org/Licenses/

The files released with Vim runtime files are distributed under the Vim
Charityware license.

The following files are part of the plugin and should be in your ~/.vim
directory after the installation:

   autoload/rcomplete.vim
   doc/r-plugin.txt
   ftdetect/r.vim
   ftplugin/r.vim
   ftplugin/r_rplugin.vim
   ftplugin/rbrowser.vim
   ftplugin/rdoc.vim
   ftplugin/rhelp.vim
   ftplugin/rhelp_rplugin.vim
   ftplugin/rmd.vim
   ftplugin/rmd_rplugin.vim
   ftplugin/rnoweb.vim
   ftplugin/rnoweb_rplugin.vim
   ftplugin/rrst.vim
   ftplugin/rrst_rplugin.vim
   indent/r.vim
   indent/rhelp.vim
   indent/rmd.vim
   indent/rnoweb.vim
   indent/rrst.vim
   r-plugin/common_buffer.vim
   r-plugin/common_global.vim
   r-plugin/functions.vim
   r-plugin/global_r_plugin.vim
   r-plugin/gui_running.vim
   r-plugin/osx.vim
   r-plugin/r.snippets
   r-plugin/rmd.snippets
   r-plugin/setcompldir.vim
   r-plugin/synctex_evince_backward.py
   r-plugin/synctex_evince_forward.py
   r-plugin/synctex_okular_backward.sh
   r-plugin/windows.vim
   syntax/r.vim
   syntax/rbrowser.vim
   syntax/rdoc.vim
   syntax/rhelp.vim
   syntax/rmd.vim
   syntax/rout.vim
   syntax/rrst.vim


==============================================================================
							       *r-plugin-tips*
9. FAQ and tips~

9.1. Is it possible to stop R from within Vim?~

Yes. In Normal mode do `:RStop` and Vim will send SIGINT to R which is the
same signal sent when you press CTRL-C into R's Console.


------------------------------------------------------------------------------
9.2. Html help and custom pager~

If you prefer to see help pages in an html browser, put in your `~/.Rprofile`:
>
   options(help_type = "html")
<
and in your |vimrc| (see |vimrplugin_vimpager|):
>
   let vimrplugin_vimpager = "no"
<

------------------------------------------------------------------------------
							  *r-plugin-showmarks*
9.3. How do marked blocks work?~

Vim allows you to put several marks (bookmarks) in buffers. The most commonly
used marks are the lowercase alphabet letters. If the cursor is between any
two marks, the plugin will send the lines between them to R if you press
<LocalLeader>bb. If the cursor is above the first mark, the plugin will send
from the beginning of the file to the mark. If the cursor is below the last
mark, the plugin will send from the mark to the end of the file. The mark
above the cursor is included and the mark below is excluded from the block to
be sent to R. To create a mark, press m<letter> in Normal mode.

We recommended the use of ShowMarks plugin which show what lines have marks
defined. The plugin is available at:

   http://www.vim.org/scripts/script.php?script_id=152

You may want to add the following two lines to your |vimrc| to customize
ShowMarks behavior:
>
   let marksCloseWhenSelected = 0
   let showmarks_include = "abcdefghijklmnopqrstuvwxyz"
<

------------------------------------------------------------------------------
							   *r-plugin-snippets*
9.4. Use snipMate~

You probably will want to use the snipMate plugin to insert snippets of code
in your R script. The plugin may be downloaded from:

   http://www.vim.org/scripts/script.php?script_id=2540

The snipMate plugin does not come with snippets for R, but you can copy the
files r.snippets and rmd.snippets that ship with the Vim-R-plugin (look at the
r-plugin directory) to the snippets directory. The files have only a few
snippets, but they will help you to get started. If you usually edit rnoweb
files, you may also want to create an rnoweb.snippets by concatenating both
tex.snippets and r.snippets. If you edit R documentation, you may want to
create an rhelp.snippets


------------------------------------------------------------------------------
							   *r-plugin-bindings*
9.5. Easier key bindings for most used commands~

The most used commands from Vim-R-plugin probably are "Send line" and "Send
selection". You may find it a good idea to map them to the space bar in your
|vimrc| (suggestion made by Iago Mosqueira):
>
   vmap <Space> <Plug>RDSendSelection
   nmap <Space> <Plug>RDSendLine
<
You may also want to remap <C-x><C-o>:

   http://stackoverflow.com/questions/2269005/how-can-i-change-the-keybinding-used-to-autocomplete-in-vim

Note: Not all mappings work in all versions of Vim. Some mappings may not work
on GVim on Windows, and others may not work on Vim running in a terminal
emulator or in Linux Console. The use of <Shift>, <Alt> and <Fn> keys in
mappings are particularly problematic. See:

   https://github.com/jcfaria/Vim-R-plugin/issues/111

You can also use Vim's 'clientserver' feature to send remote messages to Vim.
For example, if you want a keyboard shortcut that sends to Vim the command to
send the current line to R and go down to the next line of code, then use your
desktop configuration tools to create a global shortcut to the following
command:
>
   vim --remote-expr 'SendLineToR("down")'
<
The command above will work properly only if there is a single Vim instance
running. Otherwise, the |--servername| argument would have to be used to send
the message to the correct Vim instance. If the command works properly, you
will be able to send lines of code from Vim to R even if the window
currently focused is not the Vim's window (either the terminal emulator where
Vim is running or GVim).


------------------------------------------------------------------------------
							*r-plugin-localleader*
9.6. Remap the <LocalLeader>~

People writing Rnoweb documents may find it better to use a comma or other key
as the <LocalLeader> instead of the default backslash (see |maplocalleader|).
For example, to change the <LocalLeader> to a comma, put at the beginning of
your |vimrc| (before any mapping command):
>
   let maplocalleader = ","
<

------------------------------------------------------------------------------
							   *r-plugin-tagsfile*
9.7. Use a tags file to jump to function definitions~

Vim can jump to a function definition if it finds a "tags" file with the
information about the place where the function is defined. To generate the
tags file, use the R function `rtags()`, which will build an Emacs tags file.
If Vim was compiled with the feature |emacs_tags|, it will be able to read the
tags file. Otherwise, you can use the vimcom function `etags2ctags()` to
convert the Emacs tags file into a Vim's one. To jump to a function
definition, put the cursor over the function name and hit CTRL-]. Please, read
|tagsrch.txt| for details on how to use tags files, specially the section
|tags-option|.

You could, for example, download and unpack R's source code, start R inside
the ~/.vim directory and do the following commands:
>
   rtags(path = "/path/to/R/source/code", recursive = TRUE, ofile = "RTAGS")
   etags2ctags("RTAGS", "Rtags")
<
Then, you would quit R and do the following command in the terminal emulator:
>
   ctags --languages=C,Fortran,Java,Tcl -R -f RsrcTags /path/to/R/source/code
<
Finally, you would put the following in your |vimrc|, optionally inside an
|autocmd-group|:
>
   autocmd FileType r set tags+=~/.vim/Rtags,~/.vim/RsrcTags
   autocmd FileType rnoweb set tags+=~/.vim/Rtags,~/.vim/RsrcTags
<
Note: While defining the autocmd, the Rtags path must be put before RsrcTags.

Example on how to test whether your setup is ok:

   1. Type `mapply()` in an R script and save the buffer.
   2. Press CTRL-] over "mapply" (Vim should jump to "mapply.R").
   3. Locate the string "do_mapply", which is the name of a C function.
   4. Press CTRL-] over "do_mapply" (Vim sould jump to "mapply.c").


------------------------------------------------------------------------------
							  *r-plugin-indenting*
9.8. Indenting setup~

Note: In Normal mode, type |==| to indent the current line and gg=G to format
the entire buffer (see |gg|, |=| and |G| for details). These are Vim commands;
they are not specific to R code.

The Vim-R-plugin includes a script to automatically indent R files. By
default, the script aligns function arguments if they span for multiple lines.
If you prefer do not have the arguments of functions aligned, put in your
|vimrc|:
>
   let r_indent_align_args = 0
<
By default, all lines beginning with a comment character, `#`, get the same
indentation level of the normal R code. Users of Emacs/ESS may be used to have
lines beginning with a single `#` indented in the 40th column, `##` indented as R
code, and `###` not indented. If you prefer that lines beginning with comment
characters are aligned as they are by Emacs/ESS, put in your |vimrc|:
>
   let r_indent_ess_comments = 1
<
If you prefer that lines beginning with a single # are aligned at a column
different from the 40th one, you should set a new value to the variable
r_indent_comment_column, as in the example below:
>
   let r_indent_comment_column = 30
<
By default any code after a line that ends with "<-" is indented. Emacs/ESS
does not indent the code if it is a top level function. If you prefer that the
Vim-R-plugin behaves like Emacs/ESS in this regard, put in your |vimrc|:
>
   let r_indent_ess_compatible = 1
<
Below is an example of indentation with and without this option enabled:
>
   ### r_indent_ess_compatible = 1           ### r_indent_ess_compatible = 0
   foo <-                                    foo <-
       function(x)                               function(x)
   {                                             {
       paste(x)                                      paste(x)
   }                                             }
<
Notes: (1) Not all code indented by Emacs/ESS will be indented by the
           Vim-R-plugin in the same way, and, in some circumstances it may be
           necessary to make changes in the code to get it properly indented
           by Vim (you may have to either put or remove braces and line
           breaks).
       (2) Indenting is not a file type plugin option. It is a feature defined
           in indent/r.vim. That is why it is documented in this section.


------------------------------------------------------------------------------
							    *r-plugin-folding*
9.9. Folding setup~

Vim has several methods of folding text (see |fold-methods| and
|fold-commands|). To enable the syntax method of folding for R files, put in
your |vimrc|:
>
   let r_syntax_folding = 1
<
With the above option, Vim will load R files with all folds closed. If you
prefer to start editing files with all folds open, put in your |vimrc|:
>
   set nofoldenable
<
Notes: (1) Enabling folding may slow down Vim. (2) Folding is not a file type
plugin option. It is a feature defined in syntax/r.vim.

Note: Indentation of R code is very slow because the indentation algorithm
sometimes goes backwards looking for an opening parenthesis or brace or for
the beginning of a `for`, `if` or `while` statement. This is necessary because
the indentation level of a given line depends on the indentation level of the
previous line, but the previous line is not always the line above. It's the
line where the statement immediately above started. Of course someone may
develop a better algorithm in the future.


------------------------------------------------------------------------------
9.10. Highlight chunk header as R code~

By default, Vim will highlight chunk headers of RMarkdown and
RreStructuredText with a single color. When the code is processed by knitr,
chunk headers should contain valid R code and, thus, you may want to highlight
them as such. You can do this by putting in your |vimrc|:
>
   let rrst_syn_hl_chunk = 1
   let rmd_syn_hl_chunk = 1
<

------------------------------------------------------------------------------
9.11. Automatically close parenthesis~

Some people want Vim automatically inserting a closing parenthesis, bracket or
brace when an open one has being typed. The page below explains how to achieve
this goal:

   http://vim.wikia.com/wiki/Automatically_append_closing_characters


------------------------------------------------------------------------------
9.12. Automatic line breaks~

By default, while editing R code, Vim does not break lines when you are typing
if you reach the column defined by the 'textwidth' option. If you prefer that
Vim breaks the R code automatically put in your |vimrc|:
>
   autocmd FileType r setlocal formatoptions+=t
<

------------------------------------------------------------------------------
9.13. Vim with 256 colors in a terminal emulator (Linux/Unix only)~

If you want 256 colors support in Vim, install the package ncurses-term. Then
put in your `~/.bashrc` the lines suggested at |r-plugin-bash-setup|.
Finally, put in your |vimrc|:
>
   if &term =~ "xterm" || &term =~ "256" || $DISPLAY != ""
       set t_Co=256
   endif
   colorscheme your_preferred_color_scheme
<
You have to search the internet for color schemes supporting 256 colors,
download and copy them to ~/.vim/colors. You may use the command
|:colorscheme| to try them one by one before setting your preference in your
|vimrc|.


------------------------------------------------------------------------------
9.14. Run your Makefile from within R~

Do you have many Rnoweb files included in a master tex or Rnoweb file and use
a Makefile to build the pdf? You may consider it useful to put the following
line in your |vimrc|:
>
   nmap <LocalLeader>sm :update<CR>:call g:SendCmdToR('system("make")')<CR>
<

------------------------------------------------------------------------------
							   *r-plugin-Rprofile*
9.15. Edit your ~/.Rprofile~

You may want to edit your `~/.Rprofile` in addition to considering the
suggestions of |r-plugin-R-setup| you may also want to put the following
lines in your `~/.Rprofile` if you are using Linux:
>
   grDevices::X11.options(width = 4.5, height = 4, ypos = 0,
                          xpos = 1000, pointsize = 10)
<
The `X11.options()` is used to choose the position and dimensions of the X11
graphical device. You can also install the application wmctrl and create
shortcuts in your desktop environment to the commands
>
   wmctrl -r "R Graphics" -b add,above
   wmctrl -r "R Graphics" -b remove,above
<
which will toggle the "always on top" state of the X11 device window.
Alternatively, you can right click on the X11 device window title bar and
choose "Always on top". This is useful to emulate a feature present in R IDEs
which can display R plots in a separate panel. Although we can not embed an R
graphical device in Vim, we can at least make it always visible over the
terminal emulator or the GVim window.


------------------------------------------------------------------------------
9.16. Debugging R functions~

The Vim-R-Plugin does not have debugging facilities, but you may want to use
the R package "debug":
>
   install.packages("debug")
   library(debug)
   mtrace(function_name)
<
Once the library is installed and loaded, you should use `mtrace(function_name)`
to enable the debugging of a function. Then, the next time that the function
is called it will enter in debugging mode. Once debugging a function, you can
hit <Enter> to evaluate the current line, `go(n)` to go to line `n` in the
function and `qqq()` to quit the function (See debug's help for details). A
useful tip is to click on the title bar of the debug window and choose "Always
on top" or a similar option provided by your desktop manager.


------------------------------------------------------------------------------
							     *r-plugin-global*
9.17. Turn the R-plugin into a global plugin (Linux/Unix only)~

The Vim-R-plugin is a file type plugin. If you want its functionality
available for all file types, then put in your |vimrc|:
>
   runtime r-plugin/global_r_plugin.vim
<
You will then be able to type <LocalLeader>rf to start and <LocalLeader>rq to
quit different command line interpreters and <LocalLeader>d, l, ss, bb, etc to
send code to the interpreter. If you want support for a language other than
Julia, Python, Haskell, Ruby and Lisp, you have to set the values of
vimrplugin_exe and vimrplugin_quit in two Vim auto commands in your |vimrc|,
BEFORE the `runtime` command above. Example:
>
   autocmd FileType matlab let vimrplugin_exe = "octave" | let vimrplugin_quit = "quit"
   autocmd BufNewFile,BufRead *.m let vimrplugin_exe = "octave" | let vimrplugin_quit = "quit"
<
If you use GVim, you may also want to set |vimrplugin_never_unmake_menu| in
your |vimrc|:
>
   let vimrplugin_never_unmake_menu = 1
<

------------------------------------------------------------------------------
9.18. Disable syntax highlight of R functions~

If you want to disable the syntax highlight of R functions put in your
|vimrc|:
>
   autocmd Syntax * syntax clear rFunction
<

------------------------------------------------------------------------------
							      *r-plugin-knitr*
9.19. Tips for knitr users~

If you are using knitr with option cache=TRUE, you may want from time to time
to delete all objects in R workspace and all files in the cache directory. If
you want to use <LocalLeader>kr in Normal mode for this, put in your |vimrc|:
>
   nmap <LocalLeader>kr :call g:SendCmdToR('rm(list=ls(all.names=TRUE)); unlink("cache/*")')<CR>
<
When generating pdfs out of Rmd-files, you can send options to pandoc. State
them in your vimrc. For example

   let vimrplugin_pandoc_args = "--toc -V lang=german"

will produce a german document with a table of contents.


------------------------------------------------------------------------------
							  *r-plugin-latex-box*
9.20. Integration with LaTeX-Box~

LaTeX-Box does not automatically recognize Rnoweb files as a valid LaTeX file.
You have to tell LaTeX-BoX that the .tex file compiled by either `knitr()` or
`Sweave()` is the main LaTeX file. You can do this in two ways. Suppose that
your Rnoweb file is called report.Rnw... You can:

    (1) Create an empty file called "report.tex.latexmain".

    or

    (2) Put in the first 5 lines of report.Rnw:

        % For LaTeX-Box: root = report.tex

Of course you must run either `knitr()` or `Sweave()` before trying LaTeX-Box
omnicompletion. Please, read LaTeX-Box documentation for more information.

See also: |vimrplugin_latexcmd|.


------------------------------------------------------------------------------
							*r-plugin-quick-setup*
9.21. Suggestion of setup for the Vim-R-plugin~

Please, look at section |r-plugin-options| if you want information about the
Vim-r-plugin customization.

Here are some suggestions of configuration of Vim, Bash, Tmux and R. To
understand what you are doing, and change the configuration to your taste,
please read this document from the beginning.

							*r-plugin-vimrc-setup*
   ~/.vimrc~
>
   " Minimum required configuration:
   set nocompatible
   syntax on
   filetype plugin on
   filetype indent on

   " Change Leader and LocalLeader keys:
   let maplocalleader = ","
   let mapleader = ";"

   " Use Ctrl+Space to do omnicompletion:
   if has("gui_running")
       inoremap <C-Space> <C-x><C-o>
   else
       inoremap <Nul> <C-x><C-o>
   endif

   " Press the space bar to send lines and selection to R:
   vmap <Space> <Plug>RDSendSelection
   nmap <Space> <Plug>RDSendLine

   " The lines below are suggestions for Vim in general and are not
   " specific to the improvement of the Vim-R-plugin.

   " Highlight the last searched pattern:
   set hlsearch

   " Show where the next pattern is as you type it:
   set incsearch

   " By default, Vim indents code by 8 spaces. Most people prefer 4
   " spaces:
   set sw=4

   " Search "Vim colorscheme 256" in the internet and download color
   " schemes that supports 256 colors in the terminal emulator. Then,
   " uncomment the code below to set you color scheme:
   "colorscheme not_defined

   " Use 256 colors even if in a terminal emulator:
   if &term =~ "xterm" || &term =~ "256" || $DISPLAY != ""
       set t_Co=256
   endif
<

							    *r-plugin-R-setup*
   ~/.Rprofile~
>
   if(interactive()){
       options(vimcom.verbose = 1)
       # Load the required libraries:
       library(colorout)
       library(setwidth)
       library(vimcom)

       # Use the text based web browser w3m to navigate through R docs
       # in Linux Console after help.start():
       if(Sys.getenv("TMUX") != "" && Sys.getenv("DISPLAY") == "")
	   options(browser = function(u) system(paste0("tmux new-window 'w3m ", u, "'")))
   }
<

							 *r-plugin-bash-setup*
   ~/.bashrc for Vim (Unix):~
>
   # Change the TERM environment variable (to get 256 colors) and make Vim
   # connecting to X Server even if running in a terminal emulator (many of
   # the plugin features depend on this).
   if [ "x$DISPLAY" != "x" ]
   then
       if [ "screen" = "$TERM" ]
       then
           export TERM=screen-256color
       else
           export TERM=xterm-256color
       fi
       alias vim='vim --servername VIM'
   fi
<


Finally, if you want to use vi key bindings in Bash:

   ~/.inputrc~
>
   set editing-mode vi
   set keymap vi
<

------------------------------------------------------------------------------

9.22. Integration with GNU Screen, screen plugin, Conque Shell or VimShell~

The plugin used to be able to use GNU Screen (through screen plugin), Conque
Shell or VimShell to send commands to R. This integration was removed on
August 20, 2013. People wanting this integration back into the plugin may want
to use the old Vim-R-plugin code as a starting point to create scripts to be
sourced by the Vim-R-plugin. Please look at |vimrplugin_source| for details.


==============================================================================
							       *r-plugin-news*
10. News~

1.2 (2015-01-18)

 * Remove support for Neovim. See: https://github.com/jalvesaq/Nvim-R

 * Remove command :RpluginConfig.

 * Remove option vimrplugin_Rterm.

 * Change commands gn and gN to <LocalLeader>gn and <LocalLeader>gN.

 * Change default value of vimrplugin_openpdf to 2.

 * Options vimrplugin_sleeptime now should be in miliseconds.

 * Replace option vimrplugin_external_ob with vimrplugin_tmux_ob.

 * Rename vimrplugin_permanent_libs to vimrplugin_start_libs.

 * Rename vimrplugin_routmorecolors to Rout_more_colors.

 * New command: :RStop.

 * No longer require +python feature; require +libcall instead.

 * Support for SyncTeX on Windows and Mac OS X.

 * New option: vimrplugin_latexmk

1.1 (2014-11-13)

 * Version update for Linux/Unix only. May not work on Windows or Mac.

 * Minor bug fixes.

 * The option vimrplugin_assign now accepts the values 0, 1 and 2.

 * SyncTeX support (Evince, Okular and Zathura):
   - New options: vimrplugin_synctex and vimrplugin_vim_window.
   - Deprecated option: vimrplugin_openpdf_quietly

1.0 (2014-07-02)

 * The package now depends on vimcom (which is fully featured and is no longer
   on CRAN).

 * Neovim support.

 * vimrplugin_openpdf now accepts three values: 0, 1 and 2.

 * New command \o evaluates current line in R and inserts the output in the
   script.

 * New options: vimrplugin_vimcom_wait, vimrplugin_vim_wd, and
   vimrplugin_tmux_title.

 * Minor bug fixes.

0.9.9.9 (2014-02-01)

 * Minor bug fixes.

 * Delete temporary files on VimLeave event.

 * Support to R package slidify (thanks to Michael Lerch).

 * New option: vimrplugin_rcomment_string.

0.9.9.8 (2013-11-30)

 * The list of objects for omnicompletion and the list of functions for syntax
   highlight now are built dynamically. Deprecated commands and options:
   :RUpdateObjList, :RAddLibToList, vimrplugin_buildwait. New option:
   vimrplugin_permanent_libs.

 * New options: vimrplugin_show_args.

 * New command \ch: send to R Console all R code from the first chunk up to
   this line.

 * Remove toolbar icons (they still may be added back manually by interested
   users).

 * If latexmk is installed, use it by default to compile the pdf.

0.9.9.7 (2013-11-06)

 * Minor bug fixes.

0.9.9.6 (2013-10-31)

 * Minor bug fixes.

0.9.9.5 (2013-10-12)

 * Minor bug fixes.

0.9.9.4 (2013-09-24)

 * Minor bug fixes.
 * The package now depends on vimcom.plus.
 * The support to GNU Screen, VimShell and Conque Shell was dropped. The
   screen plugin no longer is used.
 * The delete command was removed from the Object Browser.
 * New options: vimrplugin_vsplit, vimrplugin_rconsole_height and
   vimrplugin_rconsole_width.
 * New option: vimrplugin_restart.
 * Show elements of S4 objects in the Object Browser.

0.9.9.3 (2013-04-11)

 * Minor bug fixes.
 * New option: vimrplugin_source.

0.9.9.2 (2013-02-01)

 * Update vimcom version requirement to 0.9-7 (fix incompatibility with tcltk
   package on Unix).
 * Change the default value of vimrplugin_rmhidden to 0.
 * New option for Windows: vimrplugin_Rterm.
 * New simpler un/comment commands: <LocalLeader>xc and <LocalLeader>xu.
 * Remove options vimrplugin_nosingler and vimrplugin_by_vim_instance.

0.9.9.1 (2012-12-11)

 * Enable mouse on Tmux again.

0.9.9 (2012-12-03)

 * New commands:  :Rinsert  and  :Rformat.
 * Automatically update the Object Browser in GVim.
 * On MS Windows, don't raise the R Console before sending CTRL-V to it.
 * Search for vimcom in both IPv4 and IPv6 ports (thanks to Zé Loff for
   writing the patch).

0.9.8 (2012-10-13)

 * Open PDF automatically after processing Rnoweb file if
   vimrplugin_openpdf = 1 (thanks to Tomaz Ficko for suggesting the feature).
   Open it quietly if vimrplugin_openpdf_quietly = 1.
   Open it manually with \op.
 * Open HTML automatically after processing either Rmd or Rrst file if
   vimrplugin_openhtml = 1. Generate strict rst code if
   vimrplugin_strict_rst = 1.
 * Remove option vimrplugin_knitargs.
 * Start last R if there is more than one installed on Windows (thanks to Alex
   Zvoleff for reporting the bug and writing the patch).
 * Alex Zvoleff added support to Rrst file type.
 * michelk added support to Rmd file type.
 * For Rnoweb, Rmd and Rrst file types, CTRL-X CTRL-A completes knitr chunk
   options if the cursor is inside the chunk header.
 * New option: vimrplugin_rmhidden.
 * New option: vimrplugin_insert_mode_cmds (thanks to Charles R. Hogg III).
 * New command  :RAddLibToList  to add the objects of specific libraries to
   omnicompletion.
 * Thanks to genrich and NagatoPain for other bug fixes and code improvements.
 * New option: vimrplugin_assign_map. The option vimrplugin_underscore was
   renamed to vimrplugin_assign

0.9.7 (2012-05-04)

 * Use the R package vimcom:
     - Automatic update of the Object Browser when running R in a Tmux
       session.
     - The following options are now set on the vimcom R package and no longer
       in the Vim-R-plugin: allnames, open_df, and open_list.
     - New command in normal and visual modes when on the Object Browser: "d"
       deletes objects and detach libraries. New option: vimrplugin_ob_sleep.
 * New option, vimrplugin_external_ob, to open the Object Browser in a Tmux
   pane in the external terminal running R.
 * New command  :Rhelp (thanks for Nir Atias for suggesting the new feature).
 * Remove the command  :RUpdateObjListAll  because Vim may not load the
   syntax file if it is too big.
 * Add support to knitr package.
 * New command  :RSourceDir.
 * New key bindings \r<Left> and \r<Right>.
 * Correctly send selected blocks.

0.9.6 (2011-12-13)

 * Fix path to R source() command on Windows.
 * New default value of vimrplugin_vimpager = "tab".
 * New default value of vimrplugin_objbr_place = "editor,right"
 * Autocompletion of function arguments with <C-X><C-A>.

0.9.5 (2011-12-07)

 * Changed the way that blocks are sent to R.
 * Added "terminal" to the list of known terminal emulators (thanks for "i5m"
   for the patch).
 * Use Tmux to start the Object Browser beside the R console if
   vimrplugin_objbr_place =~ "console".
 * The file r-plugin/omniList was renamed to r-plugin/omnils because its
   field separator changed.

111114 (2011-11-14)
 * Changed key binding for commenting/uncommenting code from \cc to \xx.
 * Added function SendChunkToR() and its corresponding key bindings:
   \cc, \ce, \cd and \ca (thanks to Xavier Fernández i Marín for suggesting
   the feature).
 * New option (vimrplugin_ca_ck) was created to fix bug reported by Xavier
   Fernández i Marín: spurious ^A^K being added to lines sent to R.
 * Don't blink the menu and toolbar buttons when doing omni completion.
 * Use Tmux to run R in an external terminal emulator.

111014 (2011-10-14)
 * Fixed spell check bug in R documentation files (.Rd).
 * Fixed beep bug when sending commands to R.
 * New option: vimrplugin_notmuxconf.
 * Fixed bug when starting tmux before vim: the environment variable
   VIMRPLUGIN_TMPDIR was not being set. Thanks to Michel Lang for reporting
   the bug and helping to track its source, and thanks to Eric Dewoestine for
   explaining how to fix the bug.
 * Fixed bug in code indentation after unbalanced brackets and parenthesis
   when r_indent_align_args = 0 (thanks to Chris Neff and Peng Yu for
   reporting the bugs).
 * Really make the use of AppleScript the default on OS X (thanks for Jason
   for reporting the bug).

110805 (2011-08-05)
 * New option: vimrplugin_tmux.
 * Set Tmux as the default instead of either GNU Screen or Conque Shell.
 * Document Tmux as the preferred way of running the plugin on Linux.
 * Vim-LaTeX-suite plugin can be used with Rnoweb files without any additional
   configuration. The necessary code was added to the ftplugin/rnoweb.vim.
 * Added count argument to normal mode commands gn and gN (thanks to Ivan
   Bezerra for the suggestion).

110614 (2011-06-14)
 * When doing the command \rh, the plugin tries to show the help for the
   method corresponding to the class of the object passed as argument to the
   function. The same with \rp (thanks to Thomas Scheike for suggesting the
   feature).
 * Removed script rpager.sh.
 * Added script global_r_plugin.vim to allow the use of the plugin with any
   file type.

110222 (2011-02-22)
 * Added syntax/rhelp.vim.
 * New command for rnoweb files: BibTeX current file (\sb).
 * New commands for the object browser: open visible lists (\r=) and close
   visible lists (\r-).
 * Reorganization of the GUI menu.

110208 (2011-02-08)
 * Fixed bug in "else if" constructions (thanks to Dan Kelley for reporting
   the bug).
 * Support for commenting/uncommenting lines.

110203 (2011-02-03)
 * Fixed bug in  :RUpdateObjList  when the function arguments included S4
   objects (thanks to Gerhard Schoefl for reporting the bug).
 * Improvements in indentation of R code (thanks to Dan Kelley for finding and
   reporting indentation bugs and testing many versions of indent/r.vim).
 * New indentation options: r_indent_align_args, r_indent_ess_comments,
   r_indent_comment_column, and r_indent_ess_compatible.
 * New file: indent/rhelp.vim.

110117 (2011-01-17)
 * Fixed indentation bug in Rnoweb files (thanks to Dan Kelley for reporting
   the bug).

101217 (2010-12-17)
 * Renamed the function SendCmdToScreen to SendCmdToR.
 * Clear the current line in the R console before sending a new line.
 * Always starts R on the script's directory.
 * Don't send "^@$" as part of a paragraph in rnoweb files (thanks to Fabio
   Correa for reporting the bug).
 * More useful warning message when PyWin32 isn't installed.
 * Initial support to AppleScript on Mac OS X (thanks to Vincent Nijs for
   writing and testing the code).

101121 (2010-11-21)
 * Fix for when whoami returns domain name on Windows (thanks to "Si" for
   fixing the bug).

101118 (2010-11-18)
 * New command:  :RUpdateObjListAll.
 * New option: vimrplugin_allnames.
 * Allow the use of Python 3.1 on Windows.
 * Minor improvements in indentation of R code.
 * The file r-plugin/omni_list was renamed to r-plugin/omniList because its
   field separator changed to ";".
 * Fixed bug that was causing ^H to be exhibited in the R documentation in
   some systems. (Thanks to Helge Liebert for reporting the problem).

101107 (2010-11-07)
 * New feature: complete chunk block when '<' is pressed in rnoweb files.
 * New option: vimrplugin_rnowebchunk.
 * New key bindings in Normal mode for Rnoweb files: gn (go to next R code
   chunk) and gN (go to previous R code chunk).
 * New command:  :RBuildTags.
 * Added fold capability to syntax/r.vim.
 * Improved indentation of rnoweb files: thanks to Johannes Tanzler for
   writing the tex.vim indent script and for permitting its inclusion in the
   Vim-R-plugin.
 * R CMD BATCH now is called with --no-restore --no-save (key binding \ro).
 * The file r-plugin/omnilist now has an additional field and was renamed as
   omni_list.
 * Use 64 bit version of R by default on Windows if the directory bin/x64
   exists.
 * New Windows only option: vimrplugin_i386.

101025 (2010-10-25)
 * New option: vimrplugin_routmorecolors.
 * Fixed bug in the Object Browser when a data.frame or list had just one
   element (thanks to Jan Larres for reporting the bug).
 * Do not copy omnilist and functions.vim to ~/.vim/r-plugin if the directory
   where the plugin is installed is writable (thanks to Jan Larres for the
   suggestion).

101023 (2010-10-23)
 * New options: vimrplugin_objbr_place and vimrplugin_objbr_w.
 * New default value: vimrplugin_vimpager = "vertical"
 * The R help can now be seen in a Vim buffer on MS Windows.
 * Fix width of help text when R version >= 2.12.0.
 * Implemented actions in the Object Browser: summary, print, etc...
 * Browse libraries objects in Object Browser.

101016 (2010-10-16)
 * Minor bug fixes in the Object Browser.

101015 (2010-10-15)
 * New feature: Object Browser.
 * Conque Shell will be used if installed unless explicitly told otherwise in
   the vimrc.
 * New valid value for vimrplugin_vimpager: "tabnew"

100923 (2010-09-23)
 * New option: vimrplugin_vimpager.
 * Do not let Vim translate "File" in R menu.
 * Fixed bug when the option vimrplugin_r_path was used (thanks to Asis Hallab
   for reporting the bug),
 * Fixed bug (E687) when user created custom key binding (thanks to Philippe
   Glaziou for reporting the bug).

100917 (2010-09-17)
 * Changed the use of vimrplugin_r_path: now the option includes only the
   directory part of the path.
 * Initial support to Conque Shell plugin. Thanks to "il_principe orange" for
   suggesting the use of Conque Shell, "ZyX-I" for writing the initial code to
   use Conque Shell, and Nico Raffo for writing the plugin and additional code
   to integrate both plugins.
 * New options: vimrplugin_conqueplugin and vimrplugin_conquevsplit.
 * New option: vimrplugin_r_args.
 * Fixed bug when the plugin was installed in a directory other than ~/.vim
   (thanks to Tom Link).
 * Initial support for Vim-R communication on Windows using Python.

100825 (2010-08-25)
 * Minor improvements in syntax highlighting.
 * New option: vimrplugin_buildwait.
 * New option: vimrplugin_r_path (thanks to Asis Hallab).

100803 (2010-08-03)
 * Fixed bug in .Rsource name making in some systems.

100801 (2010-08-01)
 * Dropped options vimrplugin_hstart and vimrplugin_browser_time.
 * If ~/.vim/r-plugin/functions.vim is not found, try to copy it from
   /usr/share/vim/addons/r-plugin/functions.vim.
 * Minor bug fixes.

100730 (2010-07-30)
 * Added menu item and key binding for run "R CMD BATCH" and open the
   resulting ".Rout" file.
 * Fixed bug when more than one Vim instance used the same file to send
   multiple lines of code to R (thanks to Bart for reporting the bug).

100728 (2010-07-28)
 * Adapted the plugin to allow the creation of a Debian package.

100719 (2010-07-19)
 * Added options vimrplugin_listmethods and vimrplugin_specialplot.
 * Improved syntax highlight of R batch output (.Rout files).
 * No longer uses the external programs grep, awk and sed to build the
   additional syntax file containing the list of functions.

100710 (2010-07-10)
 * Fixed :RUpdateObjList bug when list had length 0.

100707 (2010-07-07)
 * Fixed 'E329: No menu "R"' when more than one file were loaded simultaneously
   by calling vim with either -p or -o parameters. Thanks to Peng Yu for
   reporting the bug.
 * Correctly recognize a newly created file with extension ".R" as an R script
   file.

100521 (2010-05-12)
 * Replaced "-t" with "--title" to make xfce4-terminal work again.

100512 (2010-05-12)
 * Thanks to Tortonesi Mauro who wrote a patch to make the plugin work with
   pathogen.vim.
 * Added simple syntax highlight for .Rout files.
 * Increased the time limit of RUpdateObjList to two minutes.
 * Improvement in the syntax highlight based on code written by Zhuojun Chen.
 * Thanks to Scott Kostyshak who helped to improve the documentation.
 * Iago Mosqueira suggested that the plugin should be able to run one R process
   for each Vim instance, and his suggestion was implemented with the option
   vimrplugin_by_vim_instance.

091223 (2009-12-23)
 * Syntax highlight for R functions.
 * Added "info" field to omni completion (thanks to Ben Kujala for writing the
   original code).

091016 (2009-10-16)
 * The plugin now can run together with screen.vim, thanks to Eric Van
   Dewoestine, the author of screen.vim, who added script integration to
   screen.vim.
 * Andy Choens has made many improvements on the documentation.
 * Added the possibility of custom key binding creation to call any R function
   with the word under cursor as argument.
 * The key bindings related with Sweave are activated even if the file type is
   not rnoweb.
 * Replaced <Leader> with <LocalLeader> in the key bindings.
 * Added "Send Paragraph" commands.

091004 (2009-10-04)
 * Jose Claudio Faria has begun to work in the project as co-author.
 * Some ideas from Tinn-R project were ported to the plugin.
 * The main menu has new items and the toolbar new icons.
 * Documentation improvements.

090828 (2009-08-28)
 * Faster startup.
 * Better support for Rnoweb files: the cursor goes to '^<<' if the sent line
   is '^@$'.

090811 (2009-08-12)
 * Now use screen instead of funnel.pl. The bugs and limitations related with
   funnel.pl are solved.
 * Deleted key binding for R-devel.
 * Automatically detect available terminal emulators and choose one of them.
 * By default, no longer calls help.start() the first time that CTRL-H is
   pressed.

090810 (2009-08-10)
 * Added R icons for some terminal emulators.
 * Removed the script open-gvim-here. You may use Vim's option autochdir.
 * Added option vimrplugin_term.
 * Improved indentation script.
 * Changed key binding from Shift-Enter, which doesn't work in any terminal, to
   Alt-Enter, which at least works in xterm.

090610 (2009-06-11)
 * The options expandtab, shiftwidth and tabstop are no longer set by the plugin.
 * Better word detection before calling R's help().
 * Fixed bug in underscore replacement.
 * Fixed small bug in code indentation.
 * Added script rpager.sh.
 * Added two new plugin options: no underscore replacement and fixed name for
   the pipe file instead of random one.

090523 (2009-05-23)
 * Key bindings now are customizable.
 * Default key binding for calling R's args() changed to Shift-F1.
 * New R script rargs.R gives better results for generic functions than R's
   args() called directly.

090519 (2009-05-20)
 * Don't send large blocks of code to R to avoid xterm freezing.
 * Automatically call help.start() after CTRL-H is pressed for the first time,
   and wait 4 seconds for the browser start before calling R's help(). These
   features are customizable.
 * Fixed tags file script.

090516 (2009-05-16)
 * Added documentation.
 * Added ability to send function to R, revert the automatic conversion of "_"
   into "<-" and call R's help().
 * Added archive with some files to ease desktop integration, if desired.

090507 (2009-05-08)
 * Initial upload

vim:tw=78:ts=8:ft=help:norl
ftdetect/r.vim	[[[1
21

if exists("disable_r_ftplugin")
  finish
endif

autocmd BufNewFile,BufRead *.Rprofile set ft=r
autocmd BufRead *.Rhistory set ft=r
autocmd BufNewFile,BufRead *.r set ft=r
autocmd BufNewFile,BufRead *.R set ft=r
autocmd BufNewFile,BufRead *.s set ft=r
autocmd BufNewFile,BufRead *.S set ft=r

autocmd BufNewFile,BufRead *.Rout set ft=rout
autocmd BufNewFile,BufRead *.Rout.save set ft=rout
autocmd BufNewFile,BufRead *.Rout.fail set ft=rout

autocmd BufNewFile,BufRead *.Rrst set ft=rrst
autocmd BufNewFile,BufRead *.rrst set ft=rrst

autocmd BufNewFile,BufRead *.Rmd set ft=rmd
autocmd BufNewFile,BufRead *.rmd set ft=rmd
ftplugin/r.vim	[[[1
31
" Vim filetype plugin file
" Language: R
" Maintainer: Jakson Alves de Aquino <jalvesaq@gmail.com>
" Last Change:	Sun Feb 23, 2014  04:07PM

" Only do this when not yet done for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

setlocal iskeyword=@,48-57,_,.
setlocal formatoptions-=t
setlocal commentstring=#\ %s
setlocal comments=:#',:###,:##,:#

if has("gui_win32") && !exists("b:browsefilter")
  let b:browsefilter = "R Source Files (*.R)\t*.R\n" .
        \ "Files that include R (*.Rnw *.Rd *.Rmd *.Rrst)\t*.Rnw;*.Rd;*.Rmd;*.Rrst\n" .
        \ "All Files (*.*)\t*.*\n"
endif

let b:undo_ftplugin = "setl cms< com< fo< isk< | unlet! b:browsefilter"

let &cpo = s:cpo_save
unlet s:cpo_save
ftplugin/r_rplugin.vim	[[[1
106

if exists("g:disable_r_ftplugin") || has("nvim")
    finish
endif

" Source scripts common to R, Rnoweb, Rhelp, Rmd, Rrst and rdoc files:
runtime r-plugin/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rnoweb, Rhelp, Rmd, Rrst and rdoc files
" need be defined after the global ones:
runtime r-plugin/common_buffer.vim

" Run R CMD BATCH on current file and load the resulting .Rout in a split
" window
function! ShowRout()
    let b:routfile = expand("%:r") . ".Rout"
    if bufloaded(b:routfile)
        exe "bunload " . b:routfile
        call delete(b:routfile)
    endif

    if !exists("b:rplugin_R")
        call SetRPath()
    endif

    " if not silent, the user will have to type <Enter>
    silent update

    if has("win32") || has("win64")
        let rcmd = 'Rcmd.exe BATCH --no-restore --no-save "' . expand("%") . '" "' . b:routfile . '"'
    else
        let rcmd = b:rplugin_R . " CMD BATCH --no-restore --no-save '" . expand("%") . "' '" . b:routfile . "'"
    endif

    echon "Please wait for: " . rcmd
    redraw
    let rlog = system(rcmd)
    if v:shell_error && rlog != ""
        call RWarningMsg('Error: "' . rlog . '"')
        sleep 1
    endif
    if filereadable(b:routfile)
        if g:vimrplugin_routnotab == 1
            exe "split " . b:routfile
        else
            exe "tabnew " . b:routfile
        endif
        set filetype=rout
    else
        call RWarningMsg("The file '" . b:routfile . "' is not readable.")
    endif
endfunction

" Convert R script into Rmd, md and, then, html.
function! RSpin()
    update
    call g:SendCmdToR('require(knitr); .vim_oldwd <- getwd(); setwd("' . expand("%:p:h") . '"); spin("' . expand("%:t") . '"); setwd(.vim_oldwd); rm(.vim_oldwd)')
endfunction

" Default IsInRCode function when the plugin is used as a global plugin
function! DefaultIsInRCode(vrb)
    return 1
endfunction

let b:IsInRCode = function("DefaultIsInRCode")

" Pointer to function that must be different if the plugin is used as a
" global one:
let b:SourceLines = function("RSourceLines")

"==========================================================================
" Key bindings and menu items

call RCreateStartMaps()
call RCreateEditMaps()

" Only .R files are sent to R
call RCreateMaps("ni", '<Plug>RSendFile',     'aa', ':call SendFileToR("silent")')
call RCreateMaps("ni", '<Plug>RESendFile',    'ae', ':call SendFileToR("echo")')
call RCreateMaps("ni", '<Plug>RShowRout',     'ao', ':call ShowRout()')

" Knitr::spin
" -------------------------------------
call RCreateMaps("ni", '<Plug>RSpinFile',     'ks', ':call RSpin()')

call RCreateSendMaps()
call RControlMaps()
call RCreateMaps("nvi", '<Plug>RSetwd',        'rd', ':call RSetWD()')


" Menu R
if has("gui_running")
    runtime r-plugin/gui_running.vim
    call MakeRMenu()
endif

call RSourceOtherScripts()

if exists("b:undo_ftplugin")
    let b:undo_ftplugin .= " | unlet! b:IsInRCode b:SourceLines"
else
    let b:undo_ftplugin = "unlet! b:IsInRCode b:SourceLines"   
endif
ftplugin/rbrowser.vim	[[[1
365
" Vim filetype plugin file
" Language: R Browser (generated by the Vim-R-plugin)
" Maintainer: Jakson Alves de Aquino <jalvesaq@gmail.com>


" Only do this when not yet done for this buffer
if exists("b:did_ftplugin") || has("nvim")
    finish
endif

let g:rplugin_upobcnt = 0

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

" Source scripts common to R, Rnoweb, Rhelp and rdoc files:
runtime r-plugin/common_global.vim

" Some buffer variables common to R, Rnoweb, Rhelp and rdoc file need be
" defined after the global ones:
runtime r-plugin/common_buffer.vim

setlocal noswapfile
setlocal buftype=nofile
setlocal nowrap
setlocal iskeyword=@,48-57,_,.

if !exists("g:rplugin_hasmenu")
    let g:rplugin_hasmenu = 0
endif

" Popup menu
if !exists("g:rplugin_hasbrowsermenu")
    let g:rplugin_hasbrowsermenu = 0
endif

" Current view of the object browser: .GlobalEnv X loaded libraries
let g:rplugin_curview = "GlobalEnv"

function! UpdateOB(what)
    if a:what == "both"
        let wht = g:rplugin_curview
    else
        let wht = a:what
    endif
    if g:rplugin_curview != wht
        return "curview != what"
    endif
    if g:rplugin_upobcnt
        echoerr "OB called twice"
        return "OB called twice"
    endif
    let g:rplugin_upobcnt = 1

    let rplugin_switchedbuf = 0
    if g:vimrplugin_tmux_ob == 0
        redir => s:bufl
        silent buffers
        redir END
        if s:bufl !~ "Object_Browser"
            let g:rplugin_upobcnt = 0
            return "Object_Browser not listed"
        endif
        if exists("g:rplugin_curbuf") && g:rplugin_curbuf != "Object_Browser"
            let savesb = &switchbuf
            set switchbuf=useopen,usetab
            sil noautocmd sb Object_Browser
            let rplugin_switchedbuf = 1
        endif
    endif

    setlocal modifiable
    let curline = line(".")
    let curcol = col(".")
    if !exists("curline")
        let curline = 3
    endif
    if !exists("curcol")
        let curcol = 1
    endif
    let save_unnamed_reg = @@
    sil normal! ggdG
    let @@ = save_unnamed_reg 
    if wht == "GlobalEnv"
        let fcntt = readfile(g:rplugin_tmpdir . "/globenv_" . $VIMINSTANCEID)
    else
        let fcntt = readfile(g:rplugin_tmpdir . "/liblist_" . $VIMINSTANCEID)
    endif
    call setline(1, fcntt)
    call cursor(curline, curcol)
    if bufname("%") =~ "Object_Browser" || b:rplugin_extern_ob
        setlocal nomodifiable
    endif
    redraw
    if rplugin_switchedbuf
        exe "sil noautocmd sb " . g:rplugin_curbuf
        exe "set switchbuf=" . savesb
    endif
    let g:rplugin_upobcnt = 0
    return "End of UpdateOB()"
endfunction

function! RBrowserDoubleClick()
    " Toggle view: Objects in the workspace X List of libraries
    if line(".") == 1
        if g:rplugin_curview == "libraries"
            let g:rplugin_curview = "GlobalEnv"
            call UpdateOB("GlobalEnv")
        else
            let g:rplugin_curview = "libraries"
            call UpdateOB("libraries")
        endif
        return
    endif

    " Toggle state of list or data.frame: open X closed
    let key = RBrowserGetName(0, 1)
    if g:rplugin_curview == "GlobalEnv"
        call SendToVimCom("\006" . key)
    else
        let key = substitute(key, '`', '', "g") 
        if key !~ "^package:"
            let key = "package:" . RBGetPkgName() . '-' . key
        endif
        call SendToVimCom("\006" . key)
    endif
endfunction

function! RBrowserRightClick()
    if line(".") == 1
        return
    endif

    let key = RBrowserGetName(1, 0)
    if key == ""
        return
    endif

    let line = getline(".")
    if line =~ "^   ##"
        return
    endif
    let isfunction = 0
    if line =~ "(#.*\t"
        let isfunction = 1
    endif

    if g:rplugin_hasbrowsermenu == 1
        aunmenu ]RBrowser
    endif
    let key = substitute(key, '\.', '\\.', "g")
    let key = substitute(key, ' ', '\\ ', "g")

    exe 'amenu ]RBrowser.summary('. key . ') :call RAction("summary")<CR>'
    exe 'amenu ]RBrowser.str('. key . ') :call RAction("str")<CR>'
    exe 'amenu ]RBrowser.names('. key . ') :call RAction("names")<CR>'
    exe 'amenu ]RBrowser.plot('. key . ') :call RAction("plot")<CR>'
    exe 'amenu ]RBrowser.print(' . key . ') :call RAction("print")<CR>'
    amenu ]RBrowser.-sep01- <nul>
    exe 'amenu ]RBrowser.example('. key . ') :call RAction("example")<CR>'
    exe 'amenu ]RBrowser.help('. key . ') :call RAction("help")<CR>'
    if isfunction
        exe 'amenu ]RBrowser.args('. key . ') :call RAction("args")<CR>'
    endif
    popup ]RBrowser
    let g:rplugin_hasbrowsermenu = 1
endfunction

function! RBGetPkgName()
    let lnum = line(".")
    while lnum > 0
        let line = getline(lnum)
        if line =~ '.*##[0-9a-zA-Z\.]*\t'
            let line = substitute(line, '.*##\(.*\)\t', '\1', "")
            return line
        endif
        let lnum -= 1
    endwhile
    return ""
endfunction

function! RBrowserFindParent(word, curline, curpos)
    let curline = a:curline
    let curpos = a:curpos
    while curline > 1 && curpos >= a:curpos
        let curline -= 1
        let line = substitute(getline(curline), "	.*", "", "")
        let curpos = stridx(line, '[#')
        if curpos == -1
            let curpos = stridx(line, '<#')
            if curpos == -1
                let curpos = a:curpos
            endif
        endif
    endwhile

    if g:rplugin_curview == "GlobalEnv"
        let spacelimit = 3
    else
        if s:isutf8
            let spacelimit = 10
        else
            let spacelimit = 6
        endif
    endif
    if curline > 1
        let line = substitute(line, '^.\{-}\(.\)#', '\1#', "")
        let line = substitute(line, '^ *', '', "")
        if line =~ " " || line =~ '^.#[0-9]'
            let line = substitute(line, '\(.\)#\(.*\)$', '\1#`\2`', "")
        endif
        if line =~ '<#'
            let word = substitute(line, '.*<#', "", "") . '@' . a:word
        else
            let word = substitute(line, '.*\[#', "", "") . '$' . a:word
        endif
        if curpos != spacelimit
            let word = RBrowserFindParent(word, line("."), curpos)
        endif
        return word
    else
        " Didn't find the parent: should never happen.
        let msg = "R-plugin Error: " . a:word . ":" . curline
        echoerr msg
    endif
    return ""
endfunction

function! RBrowserCleanTailTick(word, cleantail, cleantick)
    let nword = a:word
    if a:cleantick
        let nword = substitute(nword, "`", "", "g")
    endif
    if a:cleantail
        let nword = substitute(nword, '[\$@]$', '', '')
        let nword = substitute(nword, '[\$@]`$', '`', '')
    endif
    return nword
endfunction

function! RBrowserGetName(cleantail, cleantick)
    let line = getline(".")
    if line =~ "^$"
        return ""
    endif

    let curpos = stridx(line, "#")
    let word = substitute(line, '.\{-}\(.#\)\(.\{-}\)\t.*', '\2\1', '')
    let word = substitute(word, '\[#$', '$', '')
    let word = substitute(word, '<#$', '@', '')
    let word = substitute(word, '.#$', '', '')

    if word =~ ' ' || word =~ '^[0-9]'
        let word = '`' . word . '`'
    endif

    if (g:rplugin_curview == "GlobalEnv" && curpos == 4) || (g:rplugin_curview == "libraries" && curpos == 3)
        " top level object
        let word = substitute(word, '\$\[\[', '[[', "g")
        let word = RBrowserCleanTailTick(word, a:cleantail, a:cleantick)
        if g:rplugin_curview == "libraries"
            return "package:" . substitute(word, "#", "", "")
        else
            return word
        endif
    else
        if g:rplugin_curview == "libraries"
            if s:isutf8
                if curpos == 11
                    let word = RBrowserCleanTailTick(word, a:cleantail, a:cleantick)
                    let word = substitute(word, '\$\[\[', '[[', "g")
                    return word
                endif
            elseif curpos == 7
                let word = RBrowserCleanTailTick(word, a:cleantail, a:cleantick)
                let word = substitute(word, '\$\[\[', '[[', "g")
                return word
            endif
        endif
        if curpos > 4
            " Find the parent data.frame or list
            let word = RBrowserFindParent(word, line("."), curpos - 1)
            let word = RBrowserCleanTailTick(word, a:cleantail, a:cleantick)
            let word = substitute(word, '\$\[\[', '[[', "g")
            return word
        else
            " Wrong object name delimiter: should never happen.
            let msg = "R-plugin Error: (curpos = " . curpos . ") " . word
            echoerr msg
            return ""
        endif
    endif
endfunction

function! MakeRBrowserMenu()
    let g:rplugin_curbuf = bufname("%")
    if g:rplugin_hasmenu == 1
        return
    endif
    menutranslate clear
    call RControlMenu()
    call RBrowserMenu()
endfunction

function! ObBrBufUnload()
    if exists("g:rplugin_editor_sname")
        call system("tmux select-pane -t " . g:rplugin_vim_pane)
    endif
endfunction

function! SourceObjBrLines()
    exe "source " . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g') . "/objbrowserInit"
endfunction

nmap <buffer><silent> <CR> :call RBrowserDoubleClick()<CR>
nmap <buffer><silent> <2-LeftMouse> :call RBrowserDoubleClick()<CR>
nmap <buffer><silent> <RightMouse> :call RBrowserRightClick()<CR>

call RControlMaps()

setlocal winfixwidth
setlocal bufhidden=wipe

if has("gui_running")
    runtime r-plugin/gui_running.vim
    call RControlMenu()
    call RBrowserMenu()
endif

au BufEnter <buffer> stopinsert

if g:vimrplugin_tmux_ob
    au BufUnload <buffer> call ObBrBufUnload()
    " Fix problems caused by some plugins
    if exists("g:loaded_surround") && mapcheck("ds", "n") != ""
        nunmap ds
    endif
    if exists("g:loaded_showmarks ")
        autocmd! ShowMarks
    endif
else
    au BufUnload <buffer> call SendToVimCom("\004Stop updating info [OB BufUnload].")
endif

let s:envstring = tolower($LC_MESSAGES . $LC_ALL . $LANG)
if s:envstring =~ "utf-8" || s:envstring =~ "utf8"
    let s:isutf8 = 1
else
    let s:isutf8 = 0
endif
unlet s:envstring

call setline(1, ".GlobalEnv | Libraries")

let b:SourceLines = function("RSourceLines")

call RSourceOtherScripts()

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=4
ftplugin/rdoc.vim	[[[1
143
" Vim filetype plugin file
" Language: R Documentation (generated by the Vim-R-plugin)
" Maintainer: Jakson Alves de Aquino <jalvesaq@gmail.com>


" Only do this when not yet done for this buffer
if exists("b:did_rdoc_ftplugin") || has("nvim")
    finish
endif

" Don't load another plugin for this buffer
let b:did_rdoc_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

" Source scripts common to R, Rnoweb, Rhelp and rdoc files:
runtime r-plugin/common_global.vim

" Some buffer variables common to R, Rnoweb, Rhelp and rdoc file need be
" defined after the global ones:
runtime r-plugin/common_buffer.vim

setlocal iskeyword=@,48-57,_,.

" Prepare R documentation output to be displayed by Vim
function! FixRdoc()
    let lnr = line("$")
    for ii in range(1, lnr)
        call setline(ii, substitute(getline(ii), "_\010", "", "g"))
    endfor

    " Mark the end of Examples
    let ii = search("^Examples:$", "nw")
    if ii
        if getline("$") !~ "^###$"
            let lnr = line("$") + 1
            call setline(lnr, '###')
        endif
    endif

    " Add a tab character at the end of the Arguments section to mark its end.
    let ii = search("^Arguments:$", "nw")
    if ii
        " A space after 'Arguments:' is necessary for correct syntax highlight
        " of the first argument
        call setline(ii, "Arguments: ")
        let doclength = line("$")
        let ii += 2
        let lin = getline(ii)
        while lin !~ "^[A-Z].*:$" && ii < doclength
            let ii += 1
            let lin = getline(ii)
        endwhile
        if ii < doclength
            let ii -= 1
            if getline(ii) =~ "^$"
                call setline(ii, "\t")
            endif
        endif
    endif

    " Add a tab character at the end of the Usage section to mark its end.
    let ii = search("^Usage:$", "nw")
    if ii
        let doclength = line("$")
        let ii += 2
        let lin = getline(ii)
        while lin !~ "^[A-Z].*:" && ii < doclength
            let ii += 1
            let lin = getline(ii)
        endwhile
        if ii < doclength
            let ii -= 1
            if getline(ii) =~ "^ *$"
                call setline(ii, "\t")
            endif
        endif
    endif

    normal! gg

    " Clear undo history
    let old_undolevels = &undolevels
    set undolevels=-1
    exe "normal a \<BS>\<Esc>"
    let &undolevels = old_undolevels
    unlet old_undolevels
endfunction

function! RdocIsInRCode(vrb)
    let exline = search("^Examples:$", "bncW")
    if exline > 0 && line(".") > exline
        return 1
    else
        if a:vrb
            call RWarningMsg('Not in the "Examples" section.')
        endif
        return 0
    endif
endfunction

let b:IsInRCode = function("RdocIsInRCode")
let b:SourceLines = function("RSourceLines")

"==========================================================================
" Key bindings and menu items

call RCreateSendMaps()
call RControlMaps()

" Menu R
if has("gui_running")
    runtime r-plugin/gui_running.vim
    call MakeRMenu()
endif

call RSourceOtherScripts()

function! RDocExSection()
    let ii = search("^Examples:$", "nW")
    if ii == 0
        call RWarningMsg("No example section below.")
        return
    else
        call cursor(ii+1, 1)
    endif
endfunction

nmap <buffer><silent> ge :call RDocExSection()<CR>
nmap <buffer><silent> q :q<CR>

setlocal bufhidden=wipe
setlocal noswapfile
set buftype=nofile
autocmd VimResized <buffer> let g:vimrplugin_newsize = 1
call FixRdoc()
autocmd FileType rdoc call FixRdoc()

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=4
ftplugin/rhelp.vim	[[[1
29
" Vim filetype plugin file
" Language: R help file
" Maintainer: Jakson Alves de Aquino <jalvesaq@gmail.com>
" Last Change:	Wed Jul 09, 2014  06:23PM

" Only do this when not yet done for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

setlocal iskeyword=@,48-57,_,.

if has("gui_win32") && !exists("b:browsefilter")
  let b:browsefilter = "R Source Files (*.R *.Rnw *.Rd *.Rmd *.Rrst)\t*.R;*.Rnw;*.Rd;*.Rmd;*.Rrst\n" .
        \ "All Files (*.*)\t*.*\n"
endif

let b:undo_ftplugin = "setl isk< | unlet! b:browsefilter"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2
ftplugin/rhelp_rplugin.vim	[[[1
49

if exists("g:disable_r_ftplugin") || has("nvim")
    finish
endif

" Source scripts common to R, Rnoweb, Rhelp and rdoc files:
runtime r-plugin/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rnoweb, Rhelp and rdoc file need be
" defined after the global ones:
runtime r-plugin/common_buffer.vim

function! RhelpIsInRCode(vrb)
    let lastsec = search('^\\[a-z][a-z]*{', "bncW")
    let secname = getline(lastsec)
    if line(".") > lastsec && (secname =~ '^\\usage{' || secname =~ '^\\examples{' || secname =~ '^\\dontshow{' || secname =~ '^\\dontrun{' || secname =~ '^\\donttest{' || secname =~ '^\\testonly{')
        return 1
    else
        if a:vrb
            call RWarningMsg("Not inside an R section.")
        endif
        return 0
    endif
endfunction

let b:IsInRCode = function("RhelpIsInRCode")
let b:SourceLines = function("RSourceLines")

"==========================================================================
" Key bindings and menu items

call RCreateStartMaps()
call RCreateEditMaps()
call RCreateSendMaps()
call RControlMaps()
call RCreateMaps("nvi", '<Plug>RSetwd',        'rd', ':call RSetWD()')

" Menu R
if has("gui_running")
    runtime r-plugin/gui_running.vim
    call MakeRMenu()
endif

call RSourceOtherScripts()

let b:undo_ftplugin .= " | unlet! b:IsInRCode b:SourceLines"
ftplugin/rmd.vim	[[[1
43
" Vim filetype plugin file
" Language: R help file
" Maintainer: Jakson Alves de Aquino <jalvesaq@gmail.com>
" Last Change:	Wed Jul 09, 2014  06:23PM
" Original work by Alex Zvoleff (adjusted for rmd by Michel Kuhlmann)

" Only do this when not yet done for this buffer
if exists("b:did_ftplugin")
  finish
endif

runtime! ftplugin/html.vim ftplugin/html_*.vim ftplugin/html/*.vim

setlocal comments=fb:*,fb:-,fb:+,n:> commentstring=>\ %s
setlocal formatoptions+=tcqln
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+
setlocal iskeyword=@,48-57,_,.

let s:cpo_save = &cpo
set cpo&vim

" Enables pandoc if it is installed
unlet! b:did_ftplugin
runtime ftplugin/pandoc.vim

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

if has("gui_win32") && !exists("b:browsefilter")
  let b:browsefilter = "R Source Files (*.R *.Rnw *.Rd *.Rmd *.Rrst)\t*.R;*.Rnw;*.Rd;*.Rmd;*.Rrst\n" .
        \ "All Files (*.*)\t*.*\n"
endif

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= " | setl cms< com< fo< flp< isk< | unlet! b:browsefilter"
else
  let b:undo_ftplugin = "setl cms< com< fo< flp< isk< | unlet! b:browsefilter"
endif

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2
ftplugin/rmd_rplugin.vim	[[[1
159

if exists("g:disable_r_ftplugin") || has("nvim")
    finish
endif


" Source scripts common to R, Rrst, Rnoweb, Rhelp and Rdoc:
runtime r-plugin/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rmd, Rrst, Rnoweb, Rhelp and Rdoc need to
" be defined after the global ones:
runtime r-plugin/common_buffer.vim

function! RmdIsInRCode(vrb)
    let chunkline = search("^[ \t]*```[ ]*{r", "bncW")
    let docline = search("^[ \t]*```$", "bncW")
    if chunkline > docline && chunkline != line(".")
        return 1
    else
        if a:vrb
            call RWarningMsg("Not inside an R code chunk.")
        endif
        return 0
    endif
endfunction

function! RmdPreviousChunk() range
    let rg = range(a:firstline, a:lastline)
    let chunk = len(rg)
    for var in range(1, chunk)
        let curline = line(".")
        if RmdIsInRCode(0)
            let i = search("^[ \t]*```[ ]*{r", "bnW")
            if i != 0
                call cursor(i-1, 1)
            endif
        endif
        let i = search("^[ \t]*```[ ]*{r", "bnW")
        if i == 0
            call cursor(curline, 1)
            call RWarningMsg("There is no previous R code chunk to go.")
            return
        else
            call cursor(i+1, 1)
        endif
    endfor
    return
endfunction

function! RmdNextChunk() range
    let rg = range(a:firstline, a:lastline)
    let chunk = len(rg)
    for var in range(1, chunk)
        let i = search("^[ \t]*```[ ]*{r", "nW")
        if i == 0
            call RWarningMsg("There is no next R code chunk to go.")
            return
        else
            call cursor(i+1, 1)
        endif
    endfor
    return
endfunction

function! RMakeRmd(t)
    update

    if a:t == "odt"
        if has("win32") || has("win64")
            let g:rplugin_soffbin = "soffice.exe"
        else
            let g:rplugin_soffbin = "soffice"
        endif
        if !executable(g:rplugin_soffbin)
            call RWarningMsg("Is Libre Office installed? Cannot convert into ODT: '" . g:rplugin_soffbin . "' not found.")
            return
        endif
    endif

    let rmddir = expand("%:p:h")
    if has("win32") || has("win64")
        let rmddir = substitute(rnwdir, '\\', '/', 'g')
    endif
    if a:t == "default"
        let rcmd = 'vim.interlace.rmd("' . expand("%:t") . '", rmddir = "' . rmddir . '"'
    else
        let rcmd = 'vim.interlace.rmd("' . expand("%:t") . '", outform = "' . a:t .'", rmddir = "' . rmddir . '"'
    endif
    if (g:vimrplugin_openhtml  == 0 && a:t == "html_document") || (g:vimrplugin_openpdf == 0 && (a:t == "pdf_document" || a:t == "beamer_presentation"))
        let rcmd .= ", view = FALSE"
    endif
    let rcmd = rcmd . ', envir = ' . g:vimrplugin_rmd_environment . ')'
    call g:SendCmdToR(rcmd)
endfunction

" Send Rmd chunk to R
function! SendRmdChunkToR(e, m)
    if RmdIsInRCode(0) == 0
        call RWarningMsg("Not inside an R code chunk.")
        return
    endif
    let chunkline = search("^[ \t]*```[ ]*{r", "bncW") + 1
    let docline = search("^[ \t]*```", "ncW") - 1
    let lines = getline(chunkline, docline)
    let ok = RSourceLines(lines, a:e)
    if ok == 0
        return
    endif
    if a:m == "down"
        call RmdNextChunk()
    endif
endfunction

let b:IsInRCode = function("RmdIsInRCode")
let b:PreviousRChunk = function("RmdPreviousChunk")
let b:NextRChunk = function("RmdNextChunk")
let b:SendChunkToR = function("SendRmdChunkToR")
let b:SourceLines = function("RSourceLines")

"==========================================================================
" Key bindings and menu items

call RCreateStartMaps()
call RCreateEditMaps()
call RCreateSendMaps()
call RControlMaps()
call RCreateMaps("nvi", '<Plug>RSetwd',        'rd', ':call RSetWD()')

" Only .Rmd files use these functions:
call RCreateMaps("nvi", '<Plug>RKnit',          'kn', ':call RKnit()')
call RCreateMaps("nvi", '<Plug>RMakeRmd',       'kr', ':call RMakeRmd("default")')
call RCreateMaps("nvi", '<Plug>RMakePDFK',      'kp', ':call RMakeRmd("pdf_document")')
call RCreateMaps("nvi", '<Plug>RMakePDFKb',     'kl', ':call RMakeRmd("beamer_presentation")')
call RCreateMaps("nvi", '<Plug>RMakeHTML',      'kh', ':call RMakeRmd("html_document")')
call RCreateMaps("nvi", '<Plug>RMakeODT',       'ko', ':call RMakeRmd("odt")')
call RCreateMaps("ni",  '<Plug>RSendChunk',     'cc', ':call b:SendChunkToR("silent", "stay")')
call RCreateMaps("ni",  '<Plug>RESendChunk',    'ce', ':call b:SendChunkToR("echo", "stay")')
call RCreateMaps("ni",  '<Plug>RDSendChunk',    'cd', ':call b:SendChunkToR("silent", "down")')
call RCreateMaps("ni",  '<Plug>REDSendChunk',   'ca', ':call b:SendChunkToR("echo", "down")')
call RCreateMaps("n",  '<Plug>RNextRChunk',     'gn', ':call b:NextRChunk()')
call RCreateMaps("n",  '<Plug>RPreviousRChunk', 'gN', ':call b:PreviousRChunk()')

" Menu R
if has("gui_running")
    runtime r-plugin/gui_running.vim
    call MakeRMenu()
endif

let g:rplugin_has_pandoc = 0
let g:rplugin_has_soffice = 0

call RSetPDFViewer()

call RSourceOtherScripts()

let b:undo_ftplugin .= " | unlet! b:IsInRCode b:SourceLines b:PreviousRChunk b:NextRChunk b:SendChunkToR"
ftplugin/rnoweb.vim	[[[1
40
" Vim filetype plugin file
" Language: Rnoweb
" Maintainer: Jakson Alves de Aquino <jalvesaq@gmail.com>
" Last Change:	Wed Jul 09, 2014  06:23PM

" Only do this when not yet done for this buffer
if exists("b:did_ftplugin")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

runtime! ftplugin/tex.vim

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

" Enables Vim-Latex-Suite, LaTeX-Box if installed
runtime ftplugin/tex_*.vim

setlocal iskeyword=@,48-57,_,.
setlocal suffixesadd=.bib,.tex
setlocal comments=b:%,b:#,b:##,b:###,b:#'

if has("gui_win32") && !exists("b:browsefilter")
  let b:browsefilter = "R Source Files (*.R *.Rnw *.Rd *.Rmd *.Rrst)\t*.R;*.Rnw;*.Rd;*.Rmd;*.Rrst\n" .
        \ "All Files (*.*)\t*.*\n"
endif

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= " | setl isk< sua< com< | unlet! b:browsefilter"
else
  let b:undo_ftplugin = "setl isk< sua< com< | unlet! b:browsefilter"
endif

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2
ftplugin/rnoweb_rplugin.vim	[[[1
647

if exists("g:disable_r_ftplugin") || has("nvim")
    finish
endif

" Source scripts common to R, Rnoweb, Rhelp and Rdoc:
runtime r-plugin/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rnoweb, Rhelp and Rdoc need to be defined
" after the global ones:
runtime r-plugin/common_buffer.vim

if has("win32") || has("win64")
    call RSetDefaultValue("g:vimrplugin_latexmk", 0)
else
    call RSetDefaultValue("g:vimrplugin_latexmk", 1)
endif
if !exists("g:rplugin_has_latexmk")
    if g:vimrplugin_latexmk && executable("latexmk") && executable("perl")
	let g:rplugin_has_latexmk = 1
    else
	let g:rplugin_has_latexmk = 0
    endif
endif

function! RWriteChunk()
    if getline(".") =~ "^\\s*$" && RnwIsInRCode(0) == 0
        call setline(line("."), "<<>>=")
        exe "normal! o@"
        exe "normal! 0kl"
    else
        exe "normal! a<"
    endif
endfunction

function! RnwIsInRCode(vrb)
    let chunkline = search("^<<", "bncW")
    let docline = search("^@", "bncW")
    if chunkline > docline && chunkline != line(".")
        return 1
    else
        if a:vrb
            call RWarningMsg("Not inside an R code chunk.")
        endif
        return 0
    endif
endfunction

function! RnwPreviousChunk() range
    let rg = range(a:firstline, a:lastline)
    let chunk = len(rg)
    for var in range(1, chunk)
        let curline = line(".")
        if RnwIsInRCode(0)
            let i = search("^<<.*$", "bnW")
            if i != 0
                call cursor(i-1, 1)
            endif
        endif
        let i = search("^<<.*$", "bnW")
        if i == 0
            call cursor(curline, 1)
            call RWarningMsg("There is no previous R code chunk to go.")
            return
        else
            call cursor(i+1, 1)
        endif
    endfor
    return
endfunction

function! RnwNextChunk() range
    let rg = range(a:firstline, a:lastline)
    let chunk = len(rg)
    for var in range(1, chunk)
        let i = search("^<<.*$", "nW")
        if i == 0
            call RWarningMsg("There is no next R code chunk to go.")
            return
        else
            call cursor(i+1, 1)
        endif
    endfor
    return
endfunction


" Because this function delete files, it will not be documented.
" If you want to try it, put in your vimrc:
"
" let vimrplugin_rm_knit_cache = 1
"
" If don't want to answer the question about deleting files, and
" if you trust this code more than I do, put in your vimrc:
"
" let vimrplugin_ask_rm_knitr_cache = 0
"
" Note that if you have the string "cache.path=" in more than one place only
" the first one above the cursor position will be found. The path must be
" surrounded by quotes; if it's an R object, it will not be recognized.
function! RKnitRmCache()
    let lnum = search('\<cache\.path\>\s*=', 'bnwc')
    if lnum == 0
        let pathdir = "cache/"
    else
        let pathregexpr = '.*\<cache\.path\>\s*=\s*[' . "'" . '"]\(.\{-}\)[' . "'" . '"].*'
        let pathdir = substitute(getline(lnum), pathregexpr, '\1', '')
        if pathdir !~ '/$'
            let pathdir .= '/'
        endif
    endif
    if exists("g:vimrplugin_ask_rm_knitr_cache") && g:vimrplugin_ask_rm_knitr_cache == 0
        let cleandir = 1
    else
        call inputsave()
        let answer = input('Delete all files from "' . pathdir . '"? [y/n]: ')
        call inputrestore()
        if answer == "y"
            let cleandir = 1
        else
            let cleandir = 0
        endif
    endif
    normal! :<Esc>
    if cleandir
        call g:SendCmdToR('rm(list=ls(all.names=TRUE)); unlink("' . pathdir . '*")')
    endif
endfunction

" knit the current buffer content
function! RKnitRnw()
    update
    let rnwdir = expand("%:p:h")
    if has("win32") || has("win64")
        let rnwdir = substitute(rnwdir, '\\', '/', 'g')
    endif
    if g:vimrplugin_synctex == 0
        call g:SendCmdToR('vim.interlace.rnoweb("' . expand("%:t") . '", rnwdir = "' . rnwdir . '", buildpdf = FALSE, synctex = FALSE)')
    else
        call g:SendCmdToR('vim.interlace.rnoweb("' . expand("%:t") . '", rnwdir = "' . rnwdir . '", buildpdf = FALSE)')
    endif
endfunction

" Sweave and compile the current buffer content
function! RMakePDF(bibtex, knit)
    if g:rplugin_vimcomport == 0
        call RWarningMsg("The vimcom package is required to make and open the PDF.")
    endif
    update
    let rnwdir = expand("%:p:h")
    if has("win32") || has("win64")
        let rnwdir = substitute(rnwdir, '\\', '/', 'g')
    endif
    let pdfcmd = 'vim.interlace.rnoweb("' . expand("%:t") . '", rnwdir = "' . rnwdir . '"'

    if a:knit == 0
        let pdfcmd = pdfcmd . ', knit = FALSE'
    endif

    if g:rplugin_has_latexmk == 0
        let pdfcmd = pdfcmd . ', latexmk = FALSE'
    endif

    if g:vimrplugin_latexcmd != "default"
        let pdfcmd = pdfcmd . ", latexcmd = '" . g:vimrplugin_latexcmd . "'"
    endif

    if g:vimrplugin_synctex == 0
        let pdfcmd = pdfcmd . ", synctex = FALSE"
    endif

    if a:bibtex == "bibtex"
        let pdfcmd = pdfcmd . ", bibtex = TRUE"
    endif

    if g:vimrplugin_openpdf == 0
        let pdfcmd = pdfcmd . ", view = FALSE"
    else
        if g:vimrplugin_openpdf == 1
            if b:pdf_opened == 0
                let b:pdf_opened = 1
            else
                let pdfcmd = pdfcmd . ", view = FALSE"
            endif
        endif
    endif

    if a:knit == 0 && exists("g:vimrplugin_sweaveargs")
        let pdfcmd = pdfcmd . ", " . g:vimrplugin_sweaveargs
    endif

    let pdfcmd = pdfcmd . ")"
    let ok = g:SendCmdToR(pdfcmd)
    if ok == 0
        return
    endif
endfunction

" Send Sweave chunk to R
function! RnwSendChunkToR(e, m)
    if RnwIsInRCode(0) == 0
        call RWarningMsg("Not inside an R code chunk.")
        return
    endif
    let chunkline = search("^<<", "bncW") + 1
    let docline = search("^@", "ncW") - 1
    let lines = getline(chunkline, docline)
    let ok = RSourceLines(lines, a:e)
    if ok == 0
        return
    endif
    if a:m == "down"
        call RnwNextChunk()
    endif
endfunction

" Sweave the current buffer content
function! RSweave()
    update
    let rnwdir = expand("%:p:h")
    if has("win32") || has("win64")
        let rnwdir = substitute(rnwdir, '\\', '/', 'g')
    endif
    let scmd = 'vim.interlace.rnoweb("' . expand("%:t") . '", rnwdir = "' . rnwdir . '", knit = FALSE, buildpdf = FALSE'
    if exists("g:vimrplugin_sweaveargs")
        let scmd .= ', ' . g:vimrplugin_sweaveargs
    endif
    if g:vimrplugin_synctex == 0
        let scmd .= ", synctex = FALSE"
    endif
    call g:SendCmdToR(scmd . ')')
endfunction

if g:vimrplugin_rnowebchunk == 1
    " Write code chunk in rnoweb files
    imap <buffer><silent> < <Esc>:call RWriteChunk()<CR>a
endif

" Pointers to functions whose purposes are the same in rnoweb, rrst, rmd,
" rhelp and rdoc and which are called at common_global.vim
let b:IsInRCode = function("RnwIsInRCode")
let b:PreviousRChunk = function("RnwPreviousChunk")
let b:NextRChunk = function("RnwNextChunk")
let b:SendChunkToR = function("RnwSendChunkToR")

" Pointers to functions that must be different if the plugin is used as a
" global one:
let b:SourceLines = function("RSourceLines")

let b:pdf_opened = 0


"==========================================================================
" Key bindings and menu items

call RCreateStartMaps()
call RCreateEditMaps()
call RCreateSendMaps()
call RControlMaps()
call RCreateMaps("nvi", '<Plug>RSetwd',        'rd', ':call RSetWD()')

" Only .Rnw files use these functions:
call RCreateMaps("nvi", '<Plug>RSweave',      'sw', ':call RSweave()')
call RCreateMaps("nvi", '<Plug>RMakePDF',     'sp', ':call RMakePDF("nobib", 0)')
call RCreateMaps("nvi", '<Plug>RBibTeX',      'sb', ':call RMakePDF("bibtex", 0)')
if exists("g:vimrplugin_rm_knit_cache") && g:vimrplugin_rm_knit_cache == 1
    call RCreateMaps("nvi", '<Plug>RKnitRmCache', 'kr', ':call RKnitRmCache()')
endif
call RCreateMaps("nvi", '<Plug>RKnit',        'kn', ':call RKnitRnw()')
call RCreateMaps("nvi", '<Plug>RMakePDFK',    'kp', ':call RMakePDF("nobib", 1)')
call RCreateMaps("nvi", '<Plug>RBibTeXK',     'kb', ':call RMakePDF("bibtex", 1)')
call RCreateMaps("nvi", '<Plug>RIndent',      'si', ':call RnwToggleIndentSty()')
call RCreateMaps("ni",  '<Plug>RSendChunk',   'cc', ':call b:SendChunkToR("silent", "stay")')
call RCreateMaps("ni",  '<Plug>RESendChunk',  'ce', ':call b:SendChunkToR("echo", "stay")')
call RCreateMaps("ni",  '<Plug>RDSendChunk',  'cd', ':call b:SendChunkToR("silent", "down")')
call RCreateMaps("ni",  '<Plug>REDSendChunk', 'ca', ':call b:SendChunkToR("echo", "down")')
call RCreateMaps("nvi", '<Plug>ROpenPDF',     'op', ':call ROpenPDF("Get Master")')
if g:vimrplugin_synctex
    call RCreateMaps("ni",  '<Plug>RSyncFor',     'gp', ':call SyncTeX_forward()')
    call RCreateMaps("ni",  '<Plug>RGoToTeX',     'gt', ':call SyncTeX_forward(1)')
endif
call RCreateMaps("n",  '<Plug>RNextRChunk',     'gn', ':call b:NextRChunk()')
call RCreateMaps("n",  '<Plug>RPreviousRChunk', 'gN', ':call b:PreviousRChunk()')

" Menu R
if has("gui_running")
    runtime r-plugin/gui_running.vim
    call MakeRMenu()
endif

"==========================================================================
" SyncTeX support:

function! SyncTeX_GetMaster()
    if filereadable(expand("%:t:r") . "-concordance.tex")
        return [expand("%:t:r"), "."]
    endif

    let ischild = search('% *!Rnw *root *=', 'bwn')
    if ischild
        let mfile = substitute(getline(ischild), '.*% *!Rnw *root *= *\(.*\) *', '\1', '')
        if mfile =~ "/"
            let mdir = substitute(mfile, '\(.*\)/.*', '\1', '')
            let mfile = substitute(mfile, '.*/', '', '')
            if mdir == '..'
                let mdir = expand("%:p:h:h")
            endif
        else
            let mdir = "."
        endif
        let basenm = substitute(mfile, '\....$', '', '')
        return [basenm, mdir]
    endif

    " Maybe this buffer is a master Rnoweb not compiled yet.
    return [expand("%:t:r"), "."]
endfunction

" See http://www.stats.uwo.ca/faculty/murdoch/9864/Sweave.pdf page 25
function! SyncTeX_readconc(basenm)
    let texidx = 0
    let rnwidx = 0
    let ntexln = len(readfile(a:basenm . ".tex"))
    let lstexln = range(1, ntexln)
    let lsrnwf = range(1, ntexln)
    let lsrnwl = range(1, ntexln)
    let conc = readfile(a:basenm . "-concordance.tex")
    let idx = 0
    let maxidx = len(conc)
    while idx < maxidx && texidx < ntexln && conc[idx] =~ "Sconcordance"
        let texf = substitute(conc[idx], '\\Sconcordance{concordance:\(.\{-}\):.*', '\1', "g")
        let rnwf = substitute(conc[idx], '\\Sconcordance{concordance:.\{-}:\(.\{-}\):.*', '\1', "g")
        let idx += 1
        let concnum = ""
        while idx < maxidx && conc[idx] !~ "Sconcordance"
            let concnum = concnum . conc[idx]
            let idx += 1
        endwhile
        let concnum = substitute(concnum, '%', '', 'g')
        let concnum = substitute(concnum, '}', '', '')
        let concl = split(concnum)
        let ii = 0
        let maxii = len(concl) - 2
        let rnwl = str2nr(concl[0])
        let lsrnwl[texidx] = rnwl
        let lsrnwf[texidx] = rnwf
        let texidx += 1
        while ii < maxii && texidx < ntexln
            let ii += 1
            let lnrange = range(1, concl[ii])
            let ii += 1
            for iii in lnrange
                if  texidx >= ntexln
                    break
                endif
                let rnwl += concl[ii]
                let lsrnwl[texidx] = rnwl
                let lsrnwf[texidx] = rnwf
                let texidx += 1
            endfor
        endwhile
    endwhile
    return {"texlnum": lstexln, "rnwfile": lsrnwf, "rnwline": lsrnwl}
endfunction

function! GoToBuf(rnwbn, rnwf, basedir, rnwln)
    if bufname("%") != a:rnwbn
        if bufloaded(a:basedir . '/' . a:rnwf)
            let savesb = &switchbuf
            set switchbuf=useopen,usetab
            exe "sb " . substitute(a:basedir . '/' . a:rnwf, ' ', '\\ ', 'g')
            exe "set switchbuf=" . savesb
        elseif bufloaded(a:rnwf)
            let savesb = &switchbuf
            set switchbuf=useopen,usetab
            exe "sb " . substitute(a:rnwf, ' ', '\\ ', 'g')
            exe "set switchbuf=" . savesb
        else
            if filereadable(a:basedir . '/' . a:rnwf)
                exe "tabnew " . substitute(a:basedir . '/' . a:rnwf, ' ', '\\ ', 'g')
            elseif filereadable(a:rnwf)
                exe "tabnew " . substitute(a:rnwf, ' ', '\\ ', 'g')
            else
                call RWarningMsg('Could not find either "' . a:rnwbn . ' or "' . a:rnwf . '" in "' . a:basedir . '".')
                return 0
            endif
        endif
    endif
    exe a:rnwln
    redraw
    return 1
endfunction

function! SyncTeX_backward(fname, ln)
    let flnm = substitute(a:fname, 'file://', '', '') " Evince
    let flnm = substitute(flnm, '/\./', '/', '')      " Okular
    let basenm = substitute(flnm, "\....$", "", "")   " Delete extension
    if basenm =~ "/"
        let basedir = substitute(basenm, '\(.*\)/.*', '\1', '')
    else
        let basedir = '.'
    endif
    if filereadable(basenm . "-concordance.tex")
        if !filereadable(basenm . ".tex")
            call RWarningMsg('SyncTeX [Vim-R-plugin]: "' . basenm . '.tex" not found.')
            return
        endif
        let concdata = SyncTeX_readconc(basenm)
        let texlnum = concdata["texlnum"]
        let rnwfile = concdata["rnwfile"]
        let rnwline = concdata["rnwline"]
        let rnwln = 0
        for ii in range(len(texlnum))
            if texlnum[ii] >= a:ln
                let rnwf = rnwfile[ii]
                let rnwln = rnwline[ii]
                break
            endif
        endfor
        if rnwln == 0
            call RWarningMsg("Could not find Rnoweb source line.")
            return
        endif
    else
        if filereadable(basenm . ".Rnw") || filereadable(basenm . ".rnw")
            call RWarningMsg('SyncTeX [Vim-R-plugin]: "' . basenm . '-concordance.tex" not found.')
            return
        elseif filereadable(flnm)
            let rnwf = flnm
            let rnwln = a:ln
        else
            call RWarningMsg("Could not find '" . basenm . ".Rnw'.")
        endif
    endif

    let rnwbn = substitute(rnwf, '.*/', '', '')
    let rnwf = substitute(rnwf, '^\./', '', '')

    if GoToBuf(rnwbn, rnwf, basedir, rnwln)
	if g:rplugin_has_wmctrl
        call system("wmctrl -xa " . g:vimrplugin_vim_window)
	elseif has("gui_running")
	    call foreground()
    endif
    endif

endfunction

function! SyncTeX_forward(...)
    let basenm = expand("%:t:r")
    let lnum = 0
    let rnwf = expand("%:t")

    if g:rplugin_pdfviewer == "evince" && expand("%:p") =~ " "
        call RWarningMsg('SyncTeX may not work because there is space in the file path "' . expand("%:p") . '".')
    endif

    let olddir = getcwd()
    if olddir != expand("%:p:h")
        exe "cd " . substitute(expand("%:p:h"), ' ', '\\ ', 'g')
    endif

    if filereadable(basenm . "-concordance.tex")
        let lnum = line(".")
    else
        let ischild = search('% *!Rnw *root *=', 'bwn')
        if ischild
            let mfile = substitute(getline(ischild), '.*% *!Rnw *root *= *\(.*\) *', '\1', '')
            let basenm = substitute(mfile, '\....$', '', '')
            if filereadable(basenm . "-concordance.tex")
                let mlines = readfile(mfile)
                for ii in range(len(mlines))
                    " Sweave has detailed child information
                    if mlines[ii] =~ 'SweaveInput.*' . bufname("%")
                        let lnum = line(".")
                        break
                    endif
                    " Knitr does not include detailed child information
                    if mlines[ii] =~ '<<.*child *=.*' . bufname("%") . '["' . "']"
                        let lnum = ii + 1
                        let rnwf = substitute(mfile, '.*/', '', '')
                        break
                    endif
                endfor
                if lnum == 0
                    call RWarningMsg('Could not find "child=' . bufname("%") . '" in ' . mfile . '.')
                    exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
                    return
                endif
            else
                call RWarningMsg('Vim-R-plugin [SyncTeX]: "' . basenm . '-concordance.tex" not found.')
                exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
                return
            endif
        else
            call RWarningMsg('SyncTeX [Vim-R-plugin]: "' . basenm . '-concordance.tex" not found.')
            exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
            return
        endif
    endif

    if !filereadable(basenm . ".tex")
        call RWarningMsg('"' . basenm . '.tex" not found.')
        exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
        return
    endif
    let concdata = SyncTeX_readconc(basenm)
    let texlnum = concdata["texlnum"]
    let rnwfile = concdata["rnwfile"]
    let rnwline = concdata["rnwline"]
    let texln = 0
    for ii in range(len(texlnum))
        if rnwfile[ii] =~ rnwf && rnwline[ii] >= lnum
            let texln = texlnum[ii]
            break
        endif
    endfor

    if texln == 0
        call RWarningMsg("Error: did not find LaTeX line.")
        exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
        return
    endif
    if basenm =~ '/'
        let basedir = substitute(basenm, '\(.*\)/.*', '\1', '')
        let basenm = substitute(basenm, '.*/', '', '')
        exe "cd " . substitute(basedir, ' ', '\\ ', 'g')
    else
        let basedir = ''
    endif

    if a:0 && a:1
        call GoToBuf(basenm . ".tex", basenm . ".tex", basedir, texln)
        exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
        return
    endif

    if !filereadable(basenm . ".pdf")
        call RWarningMsg('SyncTeX forward cannot be done because the file "' . basenm . '.pdf" is missing.')
        exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
        return
    endif
    if !filereadable(basenm . ".synctex.gz")
        call RWarningMsg('SyncTeX forward cannot be done because the file "' . basenm . '.synctex.gz" is missing.')
        if g:vimrplugin_latexcmd != "default" && g:vimrplugin_latexcmd !~ "synctex"
            call RWarningMsg('Note: The string "-synctex=1" is not in your vimrplugin_latexcmd. Please check your vimrc.')
        endif
        exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
        return
    endif

    if g:rplugin_pdfviewer == "okular"
        call system("okular --unique " . basenm . ".pdf#src:" . texln . substitute(expand("%:p:h"), ' ', '\\ ', 'g') . "/./" . substitute(basenm, ' ', '\\ ', 'g') . ".tex 2> /dev/null >/dev/null &")
    elseif g:rplugin_pdfviewer == "evince"
        call system("python " . g:rplugin_home . "/r-plugin/synctex_evince_forward.py '" . basenm . ".pdf' " . texln . " '" . basenm . ".tex' 2> /dev/null >/dev/null &")
        if g:rplugin_has_wmctrl
            call system("wmctrl -a '" . basenm . ".pdf'")
        endif
    elseif g:rplugin_pdfviewer == "zathura"
        if system("wmctrl -xl") =~ 'Zathura.*' . basenm . '.pdf' && g:rplugin_zathura_pid[basenm] != 0
            if g:rplugin_has_dbussend
                let result = system('dbus-send --print-reply --session --dest=org.pwmt.zathura.PID-' . g:rplugin_zathura_pid[basenm] . ' /org/pwmt/zathura org.pwmt.zathura.SynctexView string:"' . basenm . '.tex' . '" uint32:' . texln . ' uint32:1')
            else
                let result = system("zathura --synctex-forward=" . texln . ":1:" . basenm . ".tex --synctex-pid=" . g:rplugin_zathura_pid[basenm] . " " . basenm . ".pdf")
            endif
            if v:shell_error
                let g:rplugin_zathura_pid[basenm] = 0
                call RWarningMsg(result)
            endif
        else
            let g:rplugin_zathura_pid[basenm] = 0
            call RStart_Zathura(basenm)
        endif
        call system("wmctrl -a '" . basenm . ".pdf'")
    elseif g:rplugin_pdfviewer == "sumatra"
        if g:rplugin_sumatra_path != "" || FindSumatra()
            silent exe '!start "' . g:rplugin_sumatra_path . '" -reuse-instance -forward-search ' . basenm . '.tex ' . texln . ' -inverse-search "vim --servername ' . v:servername . " --remote-expr SyncTeX_backward('\\%f',\\%l)" . '" "' . basenm . '.pdf"'
        endif
    elseif g:rplugin_pdfviewer == "skim"
        " This command is based on macvim-skim
        call system(g:macvim_skim_app_path . '/Contents/SharedSupport/displayline -r ' . texln . ' "' . basenm . '.pdf" "' . basenm . '.tex" 2> /dev/null >/dev/null &')
    else
        call RWarningMsg('SyncTeX support for "' . g:rplugin_pdfviewer . '" not implemented.')
    endif
    exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
endfunction

function! SyncTeX_SetPID(spid)
    exe 'autocmd VimLeave * call system("kill ' . a:spid . '")'
endfunction

function! Run_SyncTeX()
    if $DISPLAY == "" || g:rplugin_pdfviewer == "none" || exists("b:did_synctex")
        return
    endif
    let b:did_synctex = 1

    let olddir = getcwd()
    if olddir != expand("%:p:h")
        exe "cd " . substitute(expand("%:p:h"), ' ', '\\ ', 'g')
    endif

    if g:rplugin_pdfviewer == "evince"
        let [basenm, basedir] = SyncTeX_GetMaster()
        if basedir != '.'
            exe "cd " . substitute(basedir, ' ', '\\ ', 'g')
        endif
        if v:servername != ""
            call system("python " . g:rplugin_home . "/r-plugin/synctex_evince_backward.py '" . basenm . ".pdf' " . v:servername . " &")
        endif
        if basedir != '.'
            cd -
        endif
    endif
    exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
endfunction

call RSetPDFViewer()
if g:rplugin_pdfviewer != "none"
    if g:rplugin_pdfviewer == "zathura"
        let s:this_master = SyncTeX_GetMaster()[0]
        let s:key_list = keys(g:rplugin_zathura_pid)
        let s:has_key = 0
        for kk in s:key_list
            if kk == s:this_master
                let s:has_key = 1
                break
            endif
        endfor
        if s:has_key == 0
            let g:rplugin_zathura_pid[s:this_master] = 0
        endif
        unlet s:this_master
        unlet s:key_list
        unlet s:has_key
    endif
    if g:vimrplugin_synctex
        call Run_SyncTeX()
    endif
endif

call RSourceOtherScripts()

let b:undo_ftplugin .= " | unlet! b:IsInRCode b:SourceLines b:PreviousRChunk b:NextRChunk b:SendChunkToR"
ftplugin/rrst.vim	[[[1
37
" Vim filetype plugin file
" Language: reStructuredText documentation format with R code
" Maintainer: Jakson Alves de Aquino <jalvesaq@gmail.com>
" Last Change:	Wed Jul 09, 2014  06:23PM
" Original work by Alex Zvoleff

" Only do this when not yet done for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

setlocal comments=fb:*,fb:-,fb:+,n:> commentstring=>\ %s
setlocal formatoptions+=tcqln
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+
setlocal iskeyword=@,48-57,_,.

if has("gui_win32") && !exists("b:browsefilter")
  let b:browsefilter = "R Source Files (*.R *.Rnw *.Rd *.Rmd *.Rrst)\t*.R;*.Rnw;*.Rd;*.Rmd;*.Rrst\n" .
        \ "All Files (*.*)\t*.*\n"
endif

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= " | setl cms< com< fo< flp< isk< | unlet! b:browsefilter"
else
  let b:undo_ftplugin = "setl cms< com< fo< flp< isk< | unlet! b:browsefilter"
endif

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2
ftplugin/rrst_rplugin.vim	[[[1
194

if exists("g:disable_r_ftplugin") || has("nvim")
    finish
endif

" Source scripts common to R, Rrst, Rnoweb, Rhelp and Rdoc:
runtime r-plugin/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rrst, Rnoweb, Rhelp and Rdoc need to be
" defined after the global ones:
runtime r-plugin/common_buffer.vim

function! RrstIsInRCode(vrb)
    let chunkline = search("^\\.\\. {r", "bncW")
    let docline = search("^\\.\\. \\.\\.", "bncW")
    if chunkline > docline && chunkline != line(".")
        return 1
    else
        if a:vrb
            call RWarningMsg("Not inside an R code chunk.")
        endif
        return 0
    endif
endfunction

function! RrstPreviousChunk() range
    let rg = range(a:firstline, a:lastline)
    let chunk = len(rg)
    for var in range(1, chunk)
        let curline = line(".")
        if RrstIsInRCode(0)
            let i = search("^\\.\\. {r", "bnW")
            if i != 0
                call cursor(i-1, 1)
            endif
        endif
        let i = search("^\\.\\. {r", "bnW")
        if i == 0
            call cursor(curline, 1)
            call RWarningMsg("There is no previous R code chunk to go.")
            return
        else
            call cursor(i+1, 1)
        endif
    endfor
    return
endfunction

function! RrstNextChunk() range
    let rg = range(a:firstline, a:lastline)
    let chunk = len(rg)
    for var in range(1, chunk)
        let i = search("^\\.\\. {r", "nW")
        if i == 0
            call RWarningMsg("There is no next R code chunk to go.")
            return
        else
            call cursor(i+1, 1)
        endif
    endfor
    return
endfunction

function! RMakeHTMLrrst(t)
    call RSetWD()
    update
    if g:rplugin_has_rst2pdf == 0
        if executable("rst2pdf")
            let g:rplugin_has_rst2pdf = 1
        else
            call RWarningMsg("Is 'rst2pdf' application installed? Cannot convert into HTML/ODT: 'rst2pdf' executable not found.")
            return
        endif
    endif

    let rcmd = 'require(knitr)'
    if g:vimrplugin_strict_rst
        let rcmd = rcmd . '; render_rst(strict=TRUE)'
    endif
    let rcmd = rcmd . '; knit("' . expand("%:t") . '")'

    if a:t == "odt"
        let rcmd = rcmd . '; system("rst2odt ' . expand("%:r:t") . ".rst " . expand("%:r:t") . '.odt")'
    else
        let rcmd = rcmd . '; system("rst2html ' . expand("%:r:t") . ".rst " . expand("%:r:t") . '.html")'
    endif

    if g:vimrplugin_openhtml && a:t == "html"
        let rcmd = rcmd . '; browseURL("' . expand("%:r:t") . '.html")'
    endif
    call g:SendCmdToR(rcmd)
endfunction

function! RMakePDFrrst()
    if g:rplugin_vimcomport == 0
        call RWarningMsg("The vimcom package is required to make and open the PDF.")
    endif
    update
    call RSetWD()
    if g:rplugin_has_rst2pdf == 0
        if exists("g:vimrplugin_rst2pdfpath") && executable(g:vimrplugin_rst2pdfpath)
            let g:rplugin_has_rst2pdf = 1
        elseif executable("rst2pdf")
            let g:rplugin_has_rst2pdf = 1
        else
            call RWarningMsg("Is 'rst2pdf' application installed? Cannot convert into PDF: 'rst2pdf' executable not found.")
            return
        endif
    endif

    let rrstdir = expand("%:p:h")
    if has("win32") || has("win64")
        let rrstdir = substitute(rrstdir, '\\', '/', 'g')
    endif
    let pdfcmd = 'vim.interlace.rrst("' . expand("%:t") . '", rrstdir = "' . rrstdir . '"'
    if exists("g:vimrplugin_rrstcompiler")
        let pdfcmd = pdfcmd . ", compiler='" . g:vimrplugin_rrstcompiler . "'"
    endif
    if exists("g:vimrplugin_knitargs")
        let pdfcmd = pdfcmd . ", " . g:vimrplugin_knitargs
    endif
    if exists("g:vimrplugin_rst2pdfpath")
        let pdfcmd = pdfcmd . ", rst2pdfpath='" . g:vimrplugin_rst2pdfpath . "'"
    endif
    if exists("g:vimrplugin_rst2pdfargs")
        let pdfcmd = pdfcmd . ", " . g:vimrplugin_rst2pdfargs
    endif
    let pdfcmd = pdfcmd . ")"
    let ok = g:SendCmdToR(pdfcmd)
    if ok == 0
        return
    endif
endfunction

" Send Rrst chunk to R
function! SendRrstChunkToR(e, m)
    if RrstIsInRCode(0) == 0
        call RWarningMsg("Not inside an R code chunk.")
        return
    endif
    let chunkline = search("^\\.\\. {r", "bncW") + 1
    let docline = search("^\\.\\. \\.\\.", "ncW") - 1
    let lines = getline(chunkline, docline)
    let ok = RSourceLines(lines, a:e)
    if ok == 0
        return
    endif
    if a:m == "down"
        call RrstNextChunk()
    endif
endfunction

let b:IsInRCode = function("RrstIsInRCode")
let b:PreviousRChunk = function("RrstPreviousChunk")
let b:NextRChunk = function("RrstNextChunk")
let b:SendChunkToR = function("SendRrstChunkToR")
let b:SourceLines = function("RSourceLines")

"==========================================================================
" Key bindings and menu items

call RCreateStartMaps()
call RCreateEditMaps()
call RCreateSendMaps()
call RControlMaps()
call RCreateMaps("nvi", '<Plug>RSetwd',        'rd', ':call RSetWD()')

" Only .Rrst files use these functions:
call RCreateMaps("nvi", '<Plug>RKnit',          'kn', ':call RKnit()')
call RCreateMaps("nvi", '<Plug>RMakePDFK',      'kp', ':call RMakePDFrrst()')
call RCreateMaps("nvi", '<Plug>RMakeHTML',      'kh', ':call RMakeHTMLrrst("html")')
call RCreateMaps("nvi", '<Plug>RMakeODT',       'ko', ':call RMakeHTMLrrst("odt")')
call RCreateMaps("nvi", '<Plug>RIndent',        'si', ':call RrstToggleIndentSty()')
call RCreateMaps("ni",  '<Plug>RSendChunk',     'cc', ':call b:SendChunkToR("silent", "stay")')
call RCreateMaps("ni",  '<Plug>RESendChunk',    'ce', ':call b:SendChunkToR("echo", "stay")')
call RCreateMaps("ni",  '<Plug>RDSendChunk',    'cd', ':call b:SendChunkToR("silent", "down")')
call RCreateMaps("ni",  '<Plug>REDSendChunk',   'ca', ':call b:SendChunkToR("echo", "down")')
call RCreateMaps("n",  '<Plug>RNextRChunk',     'gn', ':call b:NextRChunk()')
call RCreateMaps("n",  '<Plug>RPreviousRChunk', 'gN', ':call b:PreviousRChunk()')

" Menu R
if has("gui_running")
    runtime r-plugin/gui_running.vim
    call MakeRMenu()
endif

let g:rplugin_has_rst2pdf = 0

call RSourceOtherScripts()

let b:undo_ftplugin .= " | unlet! b:IsInRCode b:SourceLines b:PreviousRChunk b:NextRChunk b:SendChunkToR"
indent/r.vim	[[[1
495
" Vim indent file
" Language:	R
" Author:	Jakson Alves de Aquino <jalvesaq@gmail.com>
" Last Change:	Mon Nov 03, 2014  11:29AM


" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentkeys=0{,0},:,!^F,o,O,e
setlocal indentexpr=GetRIndent()

" Only define the function once.
if exists("*GetRIndent")
  finish
endif

" Options to make the indentation more similar to Emacs/ESS:
if !exists("g:r_indent_align_args")
  let g:r_indent_align_args = 1
endif
if !exists("g:r_indent_ess_comments")
  let g:r_indent_ess_comments = 0
endif
if !exists("g:r_indent_comment_column")
  let g:r_indent_comment_column = 40
endif
if ! exists("g:r_indent_ess_compatible")
  let g:r_indent_ess_compatible = 0
endif

function s:RDelete_quotes(line)
  let i = 0
  let j = 0
  let line1 = ""
  let llen = strlen(a:line)
  while i < llen
    if a:line[i] == '"'
      let i += 1
      let line1 = line1 . 's'
      while !(a:line[i] == '"' && ((i > 1 && a:line[i-1] == '\' && a:line[i-2] == '\') || a:line[i-1] != '\')) && i < llen
        let i += 1
      endwhile
      if a:line[i] == '"'
        let i += 1
      endif
    else
      if a:line[i] == "'"
        let i += 1
        let line1 = line1 . 's'
        while !(a:line[i] == "'" && ((i > 1 && a:line[i-1] == '\' && a:line[i-2] == '\') || a:line[i-1] != '\')) && i < llen
          let i += 1
        endwhile
        if a:line[i] == "'"
          let i += 1
        endif
      else
        if a:line[i] == "`"
          let i += 1
          let line1 = line1 . 's'
          while a:line[i] != "`" && i < llen
            let i += 1
          endwhile
          if a:line[i] == "`"
            let i += 1
          endif
        endif
      endif
    endif
    if i == llen
      break
    endif
    let line1 = line1 . a:line[i]
    let j += 1
    let i += 1
  endwhile
  return line1
endfunction

" Convert foo(bar()) int foo()
function s:RDelete_parens(line)
  if s:Get_paren_balance(a:line, "(", ")") != 0
    return a:line
  endif
  let i = 0
  let j = 0
  let line1 = ""
  let llen = strlen(a:line)
  while i < llen
    let line1 = line1 . a:line[i]
    if a:line[i] == '('
      let nop = 1
      while nop > 0 && i < llen
        let i += 1
        if a:line[i] == ')'
          let nop -= 1
        else
          if a:line[i] == '('
            let nop += 1
          endif
        endif
      endwhile
      let line1 = line1 . a:line[i]
    endif
    let i += 1
  endwhile
  return line1
endfunction

function! s:Get_paren_balance(line, o, c)
  let line2 = substitute(a:line, a:o, "", "g")
  let openp = strlen(a:line) - strlen(line2)
  let line3 = substitute(line2, a:c, "", "g")
  let closep = strlen(line2) - strlen(line3)
  return openp - closep
endfunction

function! s:Get_matching_brace(linenr, o, c, delbrace)
  let line = SanitizeRLine(getline(a:linenr))
  if a:delbrace == 1
    let line = substitute(line, '{$', "", "")
  endif
  let pb = s:Get_paren_balance(line, a:o, a:c)
  let i = a:linenr
  while pb != 0 && i > 1
    let i -= 1
    let pb += s:Get_paren_balance(SanitizeRLine(getline(i)), a:o, a:c)
  endwhile
  return i
endfunction

" This function is buggy because there 'if's without 'else'
" It must be rewritten relying more on indentation
function! s:Get_matching_if(linenr, delif)
  let line = SanitizeRLine(getline(a:linenr))
  if a:delif
    let line = substitute(line, "if", "", "g")
  endif
  let elsenr = 0
  let i = a:linenr
  let ifhere = 0
  while i > 0
    let line2 = substitute(line, '\<else\>', "xxx", "g")
    let elsenr += strlen(line) - strlen(line2)
    if line =~ '.*\s*if\s*()' || line =~ '.*\s*if\s*()'
      let elsenr -= 1
      if elsenr == 0
        let ifhere = i
        break
      endif
    endif
    let i -= 1
    let line = SanitizeRLine(getline(i))
  endwhile
  if ifhere
    return ifhere
  else
    return a:linenr
  endif
endfunction

function! s:Get_last_paren_idx(line, o, c, pb)
  let blc = a:pb
  let line = substitute(a:line, '\t', s:curtabstop, "g")
  let theidx = -1
  let llen = strlen(line)
  let idx = 0
  while idx < llen
    if line[idx] == a:o
      let blc -= 1
      if blc == 0
        let theidx = idx
      endif
    else
      if line[idx] == a:c
        let blc += 1
      endif
    endif
    let idx += 1
  endwhile
  return theidx + 1
endfunction

" Get previous relevant line. Search back until getting a line that isn't
" comment or blank
function s:Get_prev_line(lineno)
  let lnum = a:lineno - 1
  let data = getline( lnum )
  while lnum > 0 && (data =~ '^\s*#' || data =~ '^\s*$')
    let lnum = lnum - 1
    let data = getline( lnum )
  endwhile
  return lnum
endfunction

" This function is also used by r-plugin/common_global.vim
" Delete from '#' to the end of the line, unless the '#' is inside a string.
function SanitizeRLine(line)
  let newline = s:RDelete_quotes(a:line)
  let newline = s:RDelete_parens(newline)
  let newline = substitute(newline, '#.*', "", "")
  let newline = substitute(newline, '\s*$', "", "")
  if &filetype == "rhelp" && newline =~ '^\\method{.*}{.*}(.*'
    let newline = substitute(newline, '^\\method{\(.*\)}{.*}', '\1', "")
  endif
  return newline
endfunction

function GetRIndent()

  let clnum = line(".")    " current line

  let cline = getline(clnum)
  if cline =~ '^\s*#'
    if g:r_indent_ess_comments == 1
      if cline =~ '^\s*###'
        return 0
      endif
      if cline !~ '^\s*##'
        return g:r_indent_comment_column
      endif
    endif
  endif

  let cline = SanitizeRLine(cline)

  if cline =~ '^\s*}' || cline =~ '^\s*}\s*)$'
    let indline = s:Get_matching_brace(clnum, '{', '}', 1)
    if indline > 0 && indline != clnum
      let iline = SanitizeRLine(getline(indline))
      if s:Get_paren_balance(iline, "(", ")") == 0 || iline =~ '(\s*{$'
        return indent(indline)
      else
        let indline = s:Get_matching_brace(indline, '(', ')', 1)
        return indent(indline)
      endif
    endif
  endif

  " Find the first non blank line above the current line
  let lnum = s:Get_prev_line(clnum)
  " Hit the start of the file, use zero indent.
  if lnum == 0
    return 0
  endif

  let line = SanitizeRLine(getline(lnum))

  if &filetype == "rhelp"
    if cline =~ '^\\dontshow{' || cline =~ '^\\dontrun{' || cline =~ '^\\donttest{' || cline =~ '^\\testonly{'
      return 0
    endif
    if line =~ '^\\examples{' || line =~ '^\\usage{' || line =~ '^\\dontshow{' || line =~ '^\\dontrun{' || line =~ '^\\donttest{' || line =~ '^\\testonly{'
      return 0
    endif
  endif

  if cline =~ '^\s*{'
    if g:r_indent_ess_compatible && line =~ ')$'
      let nlnum = lnum
      let nline = line
      while s:Get_paren_balance(nline, '(', ')') < 0
        let nlnum = s:Get_prev_line(nlnum)
        let nline = SanitizeRLine(getline(nlnum)) . nline
      endwhile
      if nline =~ '^\s*function\s*(' && indent(nlnum) == &sw
        return 0
      endif
    endif
    if s:Get_paren_balance(line, "(", ")") == 0
      return indent(lnum)
    endif
  endif

  " line is an incomplete command:
  if line =~ '\<\(if\|while\|for\|function\)\s*()$' || line =~ '\<else$' || line =~ '<-$'
    return indent(lnum) + &sw
  endif

  " Deal with () and []

  let pb = s:Get_paren_balance(line, '(', ')')

  if line =~ '^\s*{$' || line =~ '(\s*{' || (pb == 0 && (line =~ '{$' || line =~ '(\s*{$'))
    return indent(lnum) + &sw
  endif

  let s:curtabstop = repeat(' ', &tabstop)

  if g:r_indent_align_args == 1
    if pb > 0 && line =~ '{$'
      return s:Get_last_paren_idx(line, '(', ')', pb) + &sw
    endif

    let bb = s:Get_paren_balance(line, '[', ']')

    if pb == 0 && bb == 0 && (line =~ '.*[,&|\-\*+<>]$' || cline =~ '^\s*[,&|\-\*+<>]')
      return indent(lnum)
    endif

    if pb > 0
      if &filetype == "rhelp"
        let ind = s:Get_last_paren_idx(line, '(', ')', pb)
      else
        let ind = s:Get_last_paren_idx(getline(lnum), '(', ')', pb)
      endif
      return ind
    endif

    if pb < 0 && line =~ '.*[,&|\-\*+<>]$'
      let lnum = s:Get_prev_line(lnum)
      while pb < 1 && lnum > 0
        let line = SanitizeRLine(getline(lnum))
        let line = substitute(line, '\t', s:curtabstop, "g")
        let ind = strlen(line)
        while ind > 0
          if line[ind] == ')'
            let pb -= 1
          else
            if line[ind] == '('
              let pb += 1
            endif
          endif
          if pb == 1
            return ind + 1
          endif
          let ind -= 1
        endwhile
        let lnum -= 1
      endwhile
      return 0
    endif

    if bb > 0
      let ind = s:Get_last_paren_idx(getline(lnum), '[', ']', bb)
      return ind
    endif
  endif

  let post_block = 0
  if line =~ '}$'
    let lnum = s:Get_matching_brace(lnum, '{', '}', 0)
    let line = SanitizeRLine(getline(lnum))
    if lnum > 0 && line =~ '^\s*{'
      let lnum = s:Get_prev_line(lnum)
      let line = SanitizeRLine(getline(lnum))
    endif
    let pb = s:Get_paren_balance(line, '(', ')')
    let post_block = 1
  endif

  let post_fun = 0
  if pb < 0 && line !~ ')\s*[,&|\-\*+<>]$'
    let post_fun = 1
    while pb < 0 && lnum > 0
      let lnum -= 1
      let linepiece = SanitizeRLine(getline(lnum))
      let pb += s:Get_paren_balance(linepiece, "(", ")")
      let line = linepiece . line
    endwhile
    if line =~ '{$' && post_block == 0
      return indent(lnum) + &sw
    endif

    " Now we can do some tests again
    if cline =~ '^\s*{'
      return indent(lnum)
    endif
    if post_block == 0
      let newl = SanitizeRLine(line)
      if newl =~ '\<\(if\|while\|for\|function\)\s*()$' || newl =~ '\<else$' || newl =~ '<-$'
        return indent(lnum) + &sw
      endif
    endif
  endif

  if cline =~ '^\s*else'
    if line =~ '<-\s*if\s*()'
      return indent(lnum) + &sw
    else
      if line =~ '\<if\s*()'
        return indent(lnum)
      else
        return indent(lnum) - &sw
      endif
    endif
  endif

  let bb = s:Get_paren_balance(line, '[', ']')
  if bb < 0 && line =~ '.*]'
    while bb < 0 && lnum > 0
      let lnum -= 1
      let linepiece = SanitizeRLine(getline(lnum))
      let bb += s:Get_paren_balance(linepiece, "[", "]")
      let line = linepiece . line
    endwhile
    let line = s:RDelete_parens(line)
  endif

  let plnum = s:Get_prev_line(lnum)
  let ppost_else = 0
  if plnum > 0
    let pline = SanitizeRLine(getline(plnum))
    let ppost_block = 0
    if pline =~ '}$'
      let ppost_block = 1
      let plnum = s:Get_matching_brace(plnum, '{', '}', 0)
      let pline = SanitizeRLine(getline(plnum))
      if pline =~ '^\s*{$' && plnum > 0
        let plnum = s:Get_prev_line(plnum)
        let pline = SanitizeRLine(getline(plnum))
      endif
    endif

    if pline =~ 'else$'
      let ppost_else = 1
      let plnum = s:Get_matching_if(plnum, 0)
      let pline = SanitizeRLine(getline(plnum))
    endif

    if pline =~ '^\s*else\s*if\s*('
      let pplnum = s:Get_prev_line(plnum)
      let ppline = SanitizeRLine(getline(pplnum))
      while ppline =~ '^\s*else\s*if\s*(' || ppline =~ '^\s*if\s*()\s*\S$'
        let plnum = pplnum
        let pline = ppline
        let pplnum = s:Get_prev_line(plnum)
        let ppline = SanitizeRLine(getline(pplnum))
      endwhile
      while ppline =~ '\<\(if\|while\|for\|function\)\s*()$' || ppline =~ '\<else$' || ppline =~ '<-$'
        let plnum = pplnum
        let pline = ppline
        let pplnum = s:Get_prev_line(plnum)
        let ppline = SanitizeRLine(getline(pplnum))
      endwhile
    endif

    let ppb = s:Get_paren_balance(pline, '(', ')')
    if ppb < 0 && (pline =~ ')\s*{$' || pline =~ ')$')
      while ppb < 0 && plnum > 0
        let plnum -= 1
        let linepiece = SanitizeRLine(getline(plnum))
        let ppb += s:Get_paren_balance(linepiece, "(", ")")
        let pline = linepiece . pline
      endwhile
      let pline = s:RDelete_parens(pline)
    endif
  endif

  let ind = indent(lnum)
  let pind = indent(plnum)

  if g:r_indent_align_args == 0 && pb != 0
    let ind += pb * &sw
    return ind
  endif

  if g:r_indent_align_args == 0 && bb != 0
    let ind += bb * &sw
    return ind
  endif

  if ind == pind || (ind == (pind  + &sw) && pline =~ '{$' && ppost_else == 0)
    return ind
  endif

  let pline = getline(plnum)
  let pbb = s:Get_paren_balance(pline, '[', ']')

  while pind < ind && plnum > 0 && ppb == 0 && pbb == 0
    let ind = pind
    let plnum = s:Get_prev_line(plnum)
    let pline = getline(plnum)
    let ppb = s:Get_paren_balance(pline, '(', ')')
    let pbb = s:Get_paren_balance(pline, '[', ']')
    while pline =~ '^\s*else'
      let plnum = s:Get_matching_if(plnum, 1)
      let pline = getline(plnum)
      let ppb = s:Get_paren_balance(pline, '(', ')')
      let pbb = s:Get_paren_balance(pline, '[', ']')
    endwhile
    let pind = indent(plnum)
    if ind == (pind  + &sw) && pline =~ '{$'
      return ind
    endif
  endwhile

  return ind

endfunction

" vim: sw=2
indent/rhelp.vim	[[[1
107
" Vim indent file
" Language:	R Documentation (Help), *.Rd
" Author:	Jakson Alves de Aquino <jalvesaq@gmail.com>
" Last Change:	Thu Oct 16, 2014  07:07AM


" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
runtime indent/r.vim
let s:RIndent = function(substitute(&indentexpr, "()", "", ""))
let b:did_indent = 1

setlocal noautoindent
setlocal nocindent
setlocal nosmartindent
setlocal nolisp
setlocal indentkeys=0{,0},:,!^F,o,O,e
setlocal indentexpr=GetCorrectRHelpIndent()

" Only define the functions once.
if exists("*GetRHelpIndent")
  finish
endif

function s:SanitizeRHelpLine(line)
  let newline = substitute(a:line, '\\\\', "x", "g")
  let newline = substitute(newline, '\\{', "x", "g")
  let newline = substitute(newline, '\\}', "x", "g")
  let newline = substitute(newline, '\\%', "x", "g")
  let newline = substitute(newline, '%.*', "", "")
  let newline = substitute(newline, '\s*$', "", "")
  return newline
endfunction

function GetRHelpIndent()

  let clnum = line(".")    " current line
  if clnum == 1
    return 0
  endif
  let cline = getline(clnum)

  if cline =~ '^\s*}\s*$'
    let i = clnum
    let bb = -1
    while bb != 0 && i > 1
      let i -= 1
      let line = s:SanitizeRHelpLine(getline(i))
      let line2 = substitute(line, "{", "", "g")
      let openb = strlen(line) - strlen(line2)
      let line3 = substitute(line2, "}", "", "g")
      let closeb = strlen(line2) - strlen(line3)
      let bb += openb - closeb
    endwhile
    return indent(i)
  endif

  if cline =~ '^\s*#ifdef\>' || cline =~ '^\s*#endif\>'
    return 0
  endif

  let lnum = clnum - 1
  let line = getline(lnum)
  if line =~ '^\s*#ifdef\>' || line =~ '^\s*#endif\>'
    let lnum -= 1
    let line = getline(lnum)
  endif
  while lnum > 1 && (line =~ '^\s*$' || line =~ '^#ifdef' || line =~ '^#endif')
    let lnum -= 1
    let line = getline(lnum)
  endwhile
  if lnum == 1
    return 0
  endif
  let line = s:SanitizeRHelpLine(line)
  let line2 = substitute(line, "{", "", "g")
  let openb = strlen(line) - strlen(line2)
  let line3 = substitute(line2, "}", "", "g")
  let closeb = strlen(line2) - strlen(line3)
  let bb = openb - closeb

  let ind = indent(lnum) + (bb * &sw)

  if line =~ '^\s*}\s*$'
    let ind = indent(lnum)
  endif

  if ind < 0
    return 0
  endif

  return ind
endfunction

function GetCorrectRHelpIndent()
  let lastsection = search('^\\[a-z]*{', "bncW")
  let secname = getline(lastsection)
  if secname =~ '^\\usage{' || secname =~ '^\\examples{' || secname =~ '^\\dontshow{' || secname =~ '^\\dontrun{' || secname =~ '^\\donttest{' || secname =~ '^\\testonly{' || secname =~ '^\\method{.*}{.*}('
    return s:RIndent()
  else
    return GetRHelpIndent()
  endif
endfunction

" vim: sw=2
indent/rmd.vim	[[[1
46
" Vim indent file
" Language:	Rmd
" Author:	Jakson Alves de Aquino <jalvesaq@gmail.com>
" Last Change:	Thu Jul 10, 2014  07:11PM


" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
runtime indent/r.vim
let s:RIndent = function(substitute(&indentexpr, "()", "", ""))
let b:did_indent = 1

setlocal indentkeys=0{,0},:,!^F,o,O,e
setlocal indentexpr=GetRmdIndent()

if exists("*GetRmdIndent")
  finish
endif

function GetMdIndent()
  let pline = getline(v:lnum - 1)
  let cline = getline(v:lnum)
  if prevnonblank(v:lnum - 1) < v:lnum - 1 || cline =~ '^\s*[-\+\*]\s' || cline =~ '^\s*\d\+\.\s\+'
    return indent(v:lnum)
  elseif pline =~ '^\s*[-\+\*]\s'
    return indent(v:lnum - 1) + 2
  elseif pline =~ '^\s*\d\+\.\s\+'
    return indent(v:lnum - 1) + 3
  endif
  return indent(prevnonblank(v:lnum - 1))
endfunction

function GetRmdIndent()
  if getline(".") =~ '^[ \t]*```{r .*}$' || getline(".") =~ '^[ \t]*```$'
    return 0
  endif
  if search('^[ \t]*```{r', "bncW") > search('^[ \t]*```$', "bncW")
    return s:RIndent()
  else
    return GetMdIndent()
  endif
endfunction

" vim: sw=2
indent/rnoweb.vim	[[[1
35
" Vim indent file
" Language:	Rnoweb
" Author:	Jakson Alves de Aquino <jalvesaq@gmail.com>
" Last Change:	Thu Jul 10, 2014  07:11PM


" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
runtime indent/tex.vim
let s:TeXIndent = function(substitute(&indentexpr, "()", "", ""))
unlet b:did_indent
runtime indent/r.vim
let s:RIndent = function(substitute(&indentexpr, "()", "", ""))
let b:did_indent = 1

setlocal indentkeys=0{,0},!^F,o,O,e,},=\bibitem,=\item
setlocal indentexpr=GetRnowebIndent()

if exists("*GetRnowebIndent")
  finish
endif

function GetRnowebIndent()
  if getline(".") =~ "^<<.*>>=$"
    return 0
  endif
  if search("^<<", "bncW") > search("^@", "bncW")
    return s:RIndent()
  endif
  return s:TeXIndent()
endfunction

" vim: sw=2
indent/rrst.vim	[[[1
46
" Vim indent file
" Language:	Rrst
" Author:	Jakson Alves de Aquino <jalvesaq@gmail.com>
" Last Change:	Thu Jul 10, 2014  07:11PM


" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
runtime indent/r.vim
let s:RIndent = function(substitute(&indentexpr, "()", "", ""))
let b:did_indent = 1

setlocal indentkeys=0{,0},:,!^F,o,O,e
setlocal indentexpr=GetRrstIndent()

if exists("*GetRrstIndent")
  finish
endif

function GetRstIndent()
  let pline = getline(v:lnum - 1)
  let cline = getline(v:lnum)
  if prevnonblank(v:lnum - 1) < v:lnum - 1 || cline =~ '^\s*[-\+\*]\s' || cline =~ '^\s*\d\+\.\s\+'
    return indent(v:lnum)
  elseif pline =~ '^\s*[-\+\*]\s'
    return indent(v:lnum - 1) + 2
  elseif pline =~ '^\s*\d\+\.\s\+'
    return indent(v:lnum - 1) + 3
  endif
  return indent(prevnonblank(v:lnum - 1))
endfunction

function GetRrstIndent()
  if getline(".") =~ '^\.\. {r .*}$' || getline(".") =~ '^\.\. \.\.$'
    return 0
  endif
  if search('^\.\. {r', "bncW") > search('^\.\. \.\.$', "bncW")
    return s:RIndent()
  else
    return GetRstIndent()
  endif
endfunction

" vim: sw=2
r-plugin/common_buffer.vim	[[[1
83
"  This program is free software; you can redistribute it and/or modify
"  it under the terms of the GNU General Public License as published by
"  the Free Software Foundation; either version 2 of the License, or
"  (at your option) any later version.
"
"  This program is distributed in the hope that it will be useful,
"  but WITHOUT ANY WARRANTY; without even the implied warranty of
"  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"  GNU General Public License for more details.
"
"  A copy of the GNU General Public License is available at
"  http://www.r-project.org/Licenses/

"==========================================================================
" ftplugin for R files
"
" Authors: Jakson Alves de Aquino <jalvesaq@gmail.com>
"          Jose Claudio Faria
"          
"          Based on previous work by Johannes Ranke
"
" Please see doc/r-plugin.txt for usage details.
"==========================================================================


" Set completion with CTRL-X CTRL-O to autoloaded function.
if exists('&ofu')
    if &filetype == "r" || &filetype == "rnoweb" || &filetype == "rdoc" || &filetype == "rhelp" || &filetype == "rrst" || &filetype == "rmd"
        setlocal omnifunc=rcomplete#CompleteR
    endif
endif

" This isn't the Object Browser running externally
let b:rplugin_extern_ob = 0

" Set the name of the Object Browser caption if not set yet
let s:tnr = tabpagenr()
if !exists("b:objbrtitle")
    if s:tnr == 1
        let b:objbrtitle = "Object_Browser"
    else
        let b:objbrtitle = "Object_Browser" . s:tnr
    endif
    unlet s:tnr
endif


" Make the file name of files to be sourced
let b:bname = expand("%:t")
let b:bname = substitute(b:bname, " ", "",  "g")
if exists("*getpid") " getpid() was introduced in Vim 7.1.142
    let b:rsource = g:rplugin_tmpdir . "/Rsource-" . getpid() . "-" . b:bname
else
    let b:randnbr = system("echo $RANDOM")
    let b:randnbr = substitute(b:randnbr, "\n", "", "")
    if strlen(b:randnbr) == 0
        let b:randnbr = "NoRandom"
    endif
    let b:rsource = g:rplugin_tmpdir . "/Rsource-" . b:randnbr . "-" . b:bname
    unlet b:randnbr
endif
unlet b:bname

if exists("g:rplugin_firstbuffer") && g:rplugin_firstbuffer == ""
    " The file global_r_plugin.vim was copied to ~/.vim/plugin
    let g:rplugin_firstbuffer = expand("%:p")
endif

let g:rplugin_lastft = &filetype

if !exists("g:SendCmdToR")
    let g:SendCmdToR = function('SendCmdToR_fake')
endif

" Were new libraries loaded by R?
if !exists("b:rplugin_new_libs")
    let b:rplugin_new_libs = 0
endif
" When using as a global plugin for non R files, RCheckLibList will not exist
if exists("*RCheckLibList")
    autocmd BufEnter <buffer> call RCheckLibList()
endif

r-plugin/common_global.vim	[[[1
3281
"  This program is free software; you can redistribute it and/or modify
"  it under the terms of the GNU General Public License as published by
"  the Free Software Foundation; either version 2 of the License, or
"  (at your option) any later version.
"
"  This program is distributed in the hope that it will be useful,
"  but WITHOUT ANY WARRANTY; without even the implied warranty of
"  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"  GNU General Public License for more details.
"
"  A copy of the GNU General Public License is available at
"  http://www.r-project.org/Licenses/

"==========================================================================
" Authors: Jakson Alves de Aquino <jalvesaq@gmail.com>
"          Jose Claudio Faria
"
" Purposes of this file: Create all functions and commands and set the
" value of all global variables and some buffer variables.for r,
" rnoweb, rhelp, rdoc, and rbrowser files
"
" Why not an autoload script? Because autoload was designed to store
" functions that are only occasionally used. The Vim-R-plugin has
" global variables and functions that are common to five file types
" and most of these functions will be used every time the plugin is
" used.
"==========================================================================


" Do this only once
if exists("g:rplugin_did_global_stuff")
    finish
endif
let g:rplugin_did_global_stuff = 1

"==========================================================================
" Functions that are common to r, rnoweb, rhelp and rdoc
"==========================================================================

function RWarningMsg(wmsg)
    echohl WarningMsg
    echomsg a:wmsg
    echohl Normal
endfunction

function RWarningMsgInp(wmsg)
    let savedlz = &lazyredraw
    if savedlz == 0
        set lazyredraw
    endif
    let savedsm = &shortmess
    set shortmess-=T
    echohl WarningMsg
    call input(a:wmsg . " [Press <Enter> to continue] ")
    echohl Normal
    if savedlz == 0
        set nolazyredraw
    endif
    let &shortmess = savedsm
endfunction

if v:version < 704
    call RWarningMsgInp("The Vim-R-plugin requires Vim >= 7.4.")
    let g:rplugin_failed = 1
    finish
endif

" Set default value of some variables:
function RSetDefaultValue(var, val)
    if !exists(a:var)
        exe "let " . a:var . " = " . a:val
    endif
endfunction

function ReplaceUnderS()
    if &filetype != "r" && b:IsInRCode(0) == 0
        let isString = 1
    else
        let save_unnamed_reg = @@
        let j = col(".")
        let s = getline(".")
        if g:vimrplugin_assign == 1 && g:vimrplugin_assign_map == "_" && j > 3 && s[j-3] == "<" && s[j-2] == "-" && s[j-1] == " "
            exe "normal! 3h3xr_"
            let @@ = save_unnamed_reg
            return
        endif
        let isString = 0
        let synName = synIDattr(synID(line("."), j, 1), "name")
        if synName == "rSpecial"
            let isString = 1
        else
            if synName == "rString"
                let isString = 1
                if s[j-1] == '"' || s[j-1] == "'" && g:vimrplugin_assign == 1
                    let synName = synIDattr(synID(line("."), j-2, 1), "name")
                    if synName == "rString" || synName == "rSpecial"
                        let isString = 0
                    endif
                endif
            else
                if g:vimrplugin_assign == 2
                    if s[j-1] != "_" && !(j > 3 && s[j-3] == "<" && s[j-2] == "-" && s[j-1] == " ")
                        let isString = 1
                    elseif j > 3 && s[j-3] == "<" && s[j-2] == "-" && s[j-1] == " "
                        exe "normal! 3h3xr_a_"
                        let @@ = save_unnamed_reg
                        return
                    else
                        if j == len(s)
                            exe "normal! 1x"
                            let @@ = save_unnamed_reg
                        else
                            exe "normal! 1xi <- "
                            let @@ = save_unnamed_reg
                            return
                        endif
                    endif
                endif
            endif
        endif
    endif
    if isString
        exe "normal! a" . g:vimrplugin_assign_map
    else
        exe "normal! a <- "
    endif
endfunction

function! ReadEvalReply()
    let reply = "No reply"
    let haswaitwarn = 0
    let ii = 0
    while ii < 20
        sleep 100m
        if filereadable($VIMRPLUGIN_TMPDIR . "/eval_reply")
            let tmp = readfile($VIMRPLUGIN_TMPDIR . "/eval_reply")
            if len(tmp) > 0
                let reply = tmp[0]
                break
            endif
        endif
        let ii += 1
        if ii == 2
            echon "\rWaiting for reply"
            let haswaitwarn = 1
        endif
    endwhile
    if haswaitwarn
        echon "\r                 "
        redraw
    endif
    return reply
endfunction

function! CompleteChunkOptions()
    let cline = getline(".")
    let cpos = getpos(".")
    let idx1 = cpos[2] - 2
    let idx2 = cpos[2] - 1
    while cline[idx1] =~ '\w' || cline[idx1] == '.' || cline[idx1] == '_'
        let idx1 -= 1
    endwhile
    let idx1 += 1
    let base = strpart(cline, idx1, idx2 - idx1)
    let rr = []
    if strlen(base) == 0
        let newbase = '.'
    else
        let newbase = '^' . substitute(base, "\\$$", "", "")
    endif

    let ktopt = ["eval=;TRUE", "echo=;TRUE", "results=;'markup|asis|hold|hide'",
                \ "warning=;TRUE", "error=;TRUE", "message=;TRUE", "split=;FALSE",
                \ "include=;TRUE", "strip.white=;TRUE", "tidy=;FALSE", "tidy.opts=; ",
                \ "prompt=;FALSE", "comment=;'##'", "highlight=;TRUE", "background=;'#F7F7F7'",
                \ "cache=;FALSE", "cache.path=;'cache/'", "cache.vars=; ",
                \ "cache.lazy=;TRUE", "cache.comments=; ", "dependson=;''",
                \ "autodep=;FALSE", "fig.path=; ", "fig.keep=;'high|none|all|first|last'",
                \ "fig.show=;'asis|hold|animate|hide'", "dev=; ", "dev.args=; ",
                \ "fig.ext=; ", "dpi=;72", "fig.width=;7", "fig.height=;7",
                \ "out.width=;'7in'", "out.height=;'7in'", "out.extra=; ",
                \ "resize.width=; ", "resize.height=; ", "fig.align=;'left|right|center'",
                \ "fig.env=;'figure'", "fig.cap=;''", "fig.scap=;''", "fig.lp=;'fig:'",
                \ "fig.pos=;''", "fig.subcap=; ", "fig.process=; ", "interval=;1",
                \ "aniopts=;'controls.loop'", "code=; ", "ref.label=; ",
                \ "child=; ", "engine=;'R'", "opts.label=;''", "purl=;TRUE",
                \ 'R.options=; ']
    if &filetype == "rnoweb"
        let ktopt += ["external=;TRUE", "sanitize=;FALSE", "size=;'normalsize'"]
    endif
    if &filetype == "rmd" || &filetype == "rrst"
        let ktopt += ["fig.retina=;1"]
        if &filetype == "rmd"
            let ktopt += ["collapse=;FALSE"]
        endif
    endif
    call sort(ktopt)

    for kopt in ktopt
      if kopt =~ newbase
        let tmp1 = split(kopt, ";")
        let tmp2 = {'word': tmp1[0], 'menu': tmp1[1]}
        call add(rr, tmp2)
      endif
    endfor
    call complete(idx1 + 1, rr)
endfunction

function IsFirstRArg(lnum, cpos)
    let line = getline(a:lnum)
    let ii = a:cpos[2] - 2
    let cchar = line[ii]
    while ii > 0 && cchar != '('
        let cchar = line[ii]
        if cchar == ','
            return 0
        endif
        let ii -= 1
    endwhile
    return 1
endfunction

function RCompleteArgs()
    let line = getline(".")
    if (&filetype == "rnoweb" && line =~ "^<<.*>>=$") || (&filetype == "rmd" && line =~ "^``` *{r.*}$") || (&filetype == "rrst" && line =~ "^.. {r.*}$") || (&filetype == "r" && line =~ "^#\+")
        call CompleteChunkOptions()
        return ''
    endif

    let lnum = line(".")
    let cpos = getpos(".")
    let idx = cpos[2] - 2
    let idx2 = cpos[2] - 2
    call cursor(lnum, cpos[2] - 1)
    if line[idx2] == ' ' || line[idx2] == ',' || line[idx2] == '('
        let idx2 = cpos[2]
        let argkey = ''
    else
        let idx1 = idx2
        while line[idx1] =~ '\w' || line[idx1] == '.' || line[idx1] == '_'
            let idx1 -= 1
        endwhile
        let idx1 += 1
        let argkey = strpart(line, idx1, idx2 - idx1 + 1)
        let idx2 = cpos[2] - strlen(argkey)
    endif

    let np = 1
    let nl = 0

    while np != 0 && nl < 10
        if line[idx] == '('
            let np -= 1
        elseif line[idx] == ')'
            let np += 1
        endif
        if np == 0
            call cursor(lnum, idx)
            let rkeyword0 = RGetKeyWord()
            let classfor = RGetClassFor(rkeyword0)
            let classfor = substitute(classfor, '\\', "", "g")
            let classfor = substitute(classfor, '\(.\)"\(.\)', '\1\\"\2', "g")
            let rkeyword = '^' . rkeyword0 . "\x06"
            call cursor(cpos[1], cpos[2])

            " If R is running, use it
            if string(g:SendCmdToR) != "function('SendCmdToR_fake')"
                call delete(g:rplugin_tmpdir . "/eval_reply")
                let msg = 'vimcom:::vim.args("'
                if classfor == ""
                    let msg = msg . rkeyword0 . '", "' . argkey . '"'
                else
                    let msg = msg . rkeyword0 . '", "' . argkey . '", classfor = ' . classfor
                endif
                if rkeyword0 == "library" || rkeyword0 == "require"
                    let isfirst = IsFirstRArg(lnum, cpos)
                else
                    let isfirst = 0
                endif
                if isfirst
                    let msg = msg . ', firstLibArg = TRUE)'
                else
                    let msg = msg . ')'
                endif
                call SendToVimCom("\x08" . $VIMINSTANCEID . msg)

                if g:rplugin_vimcomport > 0
                    let g:rplugin_lastev = ReadEvalReply()
                    if g:rplugin_lastev != "NOT_EXISTS" && g:rplugin_lastev != "NO_ARGS" && g:rplugin_lastev != "R is busy." && g:rplugin_lastev != "NOANSWER" && g:rplugin_lastev != "INVALID" && g:rplugin_lastev != "" && g:rplugin_lastev != "No reply"
                        let args = []
                        if g:rplugin_lastev[0] == "\x04" && len(split(g:rplugin_lastev, "\x04")) == 1
                            return ''
                        endif
                        let tmp0 = split(g:rplugin_lastev, "\x04")
                        let tmp = split(tmp0[0], "\x09")
                        if(len(tmp) > 0)
                            for id in range(len(tmp))
                                let tmp2 = split(tmp[id], "\x07")
                                if tmp2[0] == '...' || isfirst
                                    let tmp3 = tmp2[0]
                                else
                                    let tmp3 = tmp2[0] . " = "
                                endif
                                if len(tmp2) > 1
                                    call add(args,  {'word': tmp3, 'menu': tmp2[1]})
                                else
                                    call add(args,  {'word': tmp3, 'menu': ' '})
                                endif
                            endfor
                            if len(args) > 0 && len(tmp0) > 1
                                call add(args, {'word': ' ', 'menu': tmp0[1]})
                            endif
                            call complete(idx2, args)
                        endif
                        return ''
                    endif
                endif
            endif

            " If R isn't running, use the prebuilt list of objects
            let flines = g:rplugin_globalenvlines + g:rplugin_omni_lines
            for omniL in flines
                if omniL =~ rkeyword && omniL =~ "\x06function\x06function\x06"
                    let tmp1 = split(omniL, "\x06")
                    if len(tmp1) < 5
                        return ''
                    endif
                    let info = tmp1[4]
                    let argsL = split(info, "\x09")
                    let args = []
                    for id in range(len(argsL))
                        let newkey = '^' . argkey
                        let tmp2 = split(argsL[id], "\x07")
                        if (argkey == '' || tmp2[0] =~ newkey) && tmp2[0] !~ "No arguments"
                            if tmp2[0] != '...'
                                let tmp2[0] = tmp2[0] . " = "
                            endif
                            if len(tmp2) == 2
                                let tmp3 = {'word': tmp2[0], 'menu': tmp2[1]}
                            else
                                let tmp3 = {'word': tmp2[0], 'menu': ''}
                            endif
                            call add(args, tmp3)
                        endif
                    endfor
                    call complete(idx2, args)
                    return ''
                endif
            endfor
            break
        endif
        let idx -= 1
        if idx <= 0
            let lnum -= 1
            if lnum == 0
                break
            endif
            let line = getline(lnum)
            let idx = strlen(line)
            let nl +=1
        endif
    endwhile
    call cursor(cpos[1], cpos[2])
    return ''
endfunction

function RGetFL(mode)
    if a:mode == "normal"
        let fline = line(".")
        let lline = line(".")
    else
        let fline = line("'<")
        let lline = line("'>")
    endif
    if fline > lline
        let tmp = lline
        let lline = fline
        let fline = tmp
    endif
    return [fline, lline]
endfunction

function IsLineInRCode(vrb, line)
    let save_cursor = getpos(".")
    call setpos(".", [0, a:line, 1, 0])
    let isR = b:IsInRCode(a:vrb)
    call setpos('.', save_cursor)
    return isR
endfunction

function RSimpleCommentLine(mode, what)
    let [fline, lline] = RGetFL(a:mode)
    let cstr = g:vimrplugin_rcomment_string
    if (&filetype == "rnoweb"|| &filetype == "rhelp") && IsLineInRCode(0, fline) == 0
        let cstr = "%"
    elseif (&filetype == "rmd" || &filetype == "rrst") && IsLineInRCode(0, fline) == 0
        return
    endif

    if a:what == "c"
        for ii in range(fline, lline)
            call setline(ii, cstr . getline(ii))
        endfor
    else
        for ii in range(fline, lline)
            call setline(ii, substitute(getline(ii), "^" . cstr, "", ""))
        endfor
    endif
endfunction

function RCommentLine(lnum, ind, cmt)
    let line = getline(a:lnum)
    call cursor(a:lnum, 0)

    if line =~ '^\s*' . a:cmt
        let line = substitute(line, '^\s*' . a:cmt . '*', '', '')
        call setline(a:lnum, line)
        normal! ==
    else
        if g:vimrplugin_indent_commented
            while line =~ '^\s*\t'
                let line = substitute(line, '^\(\s*\)\t', '\1' . s:curtabstop, "")
            endwhile
            let line = strpart(line, a:ind)
        endif
        let line = a:cmt . ' ' . line
        call setline(a:lnum, line)
        if g:vimrplugin_indent_commented
            normal! ==
        endif
    endif
endfunction

function RComment(mode)
    let cpos = getpos(".")
    let [fline, lline] = RGetFL(a:mode)

    " What comment string to use?
    if g:r_indent_ess_comments
        if g:vimrplugin_indent_commented
            let cmt = '##'
        else
            let cmt = '###'
        endif
    else
        let cmt = '#'
    endif
    if (&filetype == "rnoweb" || &filetype == "rhelp") && IsLineInRCode(0, fline) == 0
        let cmt = "%"
    elseif (&filetype == "rmd" || &filetype == "rrst") && IsLineInRCode(0, fline) == 0
        return
    endif

    let lnum = fline
    let ind = &tw
    while lnum <= lline
        let idx = indent(lnum)
        if idx < ind
            let ind = idx
        endif
        let lnum += 1
    endwhile

    let lnum = fline
    let s:curtabstop = repeat(' ', &tabstop)
    while lnum <= lline
        call RCommentLine(lnum, ind, cmt)
        let lnum += 1
    endwhile
    call cursor(cpos[1], cpos[2])
endfunction

function MovePosRCodeComment(mode)
    if a:mode == "selection"
        let fline = line("'<")
        let lline = line("'>")
    else
        let fline = line(".")
        let lline = fline
    endif

    let cpos = g:r_indent_comment_column
    let lnum = fline
    while lnum <= lline
        let line = getline(lnum)
        let cleanl = substitute(line, '\s*#.*', "", "")
        let llen = strlen(cleanl)
        if llen > (cpos - 2)
            let cpos = llen + 2
        endif
        let lnum += 1
    endwhile

    let lnum = fline
    while lnum <= lline
        call MovePosRLineComment(lnum, cpos)
        let lnum += 1
    endwhile
    call cursor(fline, cpos + 1)
    if a:mode == "insert"
        startinsert!
    endif
endfunction

function MovePosRLineComment(lnum, cpos)
    let line = getline(a:lnum)

    let ok = 1

    if &filetype == "rnoweb"
        if search("^<<", "bncW") > search("^@", "bncW")
            let ok = 1
        else
            let ok = 0
        endif
        if line =~ "^<<.*>>=$"
            let ok = 0
        endif
        if ok == 0
            call RWarningMsg("Not inside an R code chunk.")
            return
        endif
    endif

    if &filetype == "rhelp"
        let lastsection = search('^\\[a-z]*{', "bncW")
        let secname = getline(lastsection)
        if secname =~ '^\\usage{' || secname =~ '^\\examples{' || secname =~ '^\\dontshow{' || secname =~ '^\\dontrun{' || secname =~ '^\\donttest{' || secname =~ '^\\testonly{' || secname =~ '^\\method{.*}{.*}('
            let ok = 1
        else
            let ok = 0
        endif
        if ok == 0
            call RWarningMsg("Not inside an R code section.")
            return
        endif
    endif

    if line !~ '#'
        " Write the comment character
        let line = line . repeat(' ', a:cpos)
        let cmd = "let line = substitute(line, '^\\(.\\{" . (a:cpos - 1) . "}\\).*', '\\1# ', '')"
        exe cmd
        call setline(a:lnum, line)
    else
        " Align the comment character(s)
        let line = substitute(line, '\s*#', '#', "")
        let idx = stridx(line, '#')
        let str1 = strpart(line, 0, idx)
        let str2 = strpart(line, idx)
        let line = str1 . repeat(' ', a:cpos - idx - 1) . str2
        call setline(a:lnum, line)
    endif
endfunction

" Count braces
function CountBraces(line)
    let line2 = substitute(a:line, "{", "", "g")
    let line3 = substitute(a:line, "}", "", "g")
    let result = strlen(line3) - strlen(line2)
    return result
endfunction

" Skip empty lines and lines whose first non blank char is '#'
function GoDown()
    if &filetype == "rnoweb"
        let curline = getline(".")
        let fc = curline[0]
        if fc == '@'
            call RnwNextChunk()
            return
        endif
    elseif &filetype == "rmd"
        let curline = getline(".")
        if curline =~ '^```$'
            call RmdNextChunk()
            return
        endif
    elseif &filetype == "rrst"
        let curline = getline(".")
        if curline =~ '^\.\. \.\.$'
            call RrstNextChunk()
            return
        endif
    endif

    let i = line(".") + 1
    call cursor(i, 1)
    let curline = substitute(getline("."), '^\s*', "", "")
    let fc = curline[0]
    let lastLine = line("$")
    while i < lastLine && (fc == '#' || strlen(curline) == 0)
        let i = i + 1
        call cursor(i, 1)
        let curline = substitute(getline("."), '^\s*', "", "")
        let fc = curline[0]
    endwhile
endfunction

" Adapted from screen plugin:
function TmuxActivePane()
  let line = system("tmux list-panes | grep \'(active)$'")
  let paneid = matchstr(line, '\v\%\d+ \(active\)')
  if !empty(paneid)
    return matchstr(paneid, '\v^\%\d+')
  else
    return matchstr(line, '\v^\d+')
  endif
endfunction

function StartR_TmuxSplit(rcmd)
    let g:rplugin_vim_pane = TmuxActivePane()
    let tmuxconf = ['set-environment VIMRPLUGIN_TMPDIR "' . g:rplugin_tmpdir . '"',
                \ 'set-environment VIMRPLUGIN_COMPLDIR "' . substitute(g:rplugin_compldir, ' ', '\\ ', "g") . '"',
                \ 'set-environment VIM_PANE ' . g:rplugin_vim_pane ,
                \ 'set-environment VIMEDITOR_SVRNM ' . $VIMEDITOR_SVRNM ,
                \ 'set-environment VIMINSTANCEID ' . $VIMINSTANCEID ,
                \ 'set-environment VIMRPLUGIN_SECRET ' . $VIMRPLUGIN_SECRET ]
    if &t_Co == 256
        call extend(tmuxconf, ['set default-terminal "' . $TERM . '"'])
    endif
    call writefile(tmuxconf, g:rplugin_tmpdir . "/tmux" . $VIMINSTANCEID . ".conf")
    call system("tmux source-file '" . g:rplugin_tmpdir . "/tmux" . $VIMINSTANCEID . ".conf" . "'")
    call delete(g:rplugin_tmpdir . "/tmux" . $VIMINSTANCEID . ".conf")
    let tcmd = "tmux split-window "
    if g:vimrplugin_vsplit
        if g:vimrplugin_rconsole_width == -1
            let tcmd .= "-h"
        else
            let tcmd .= "-h -l " . g:vimrplugin_rconsole_width
        endif
    else
        let tcmd .= "-l " . g:vimrplugin_rconsole_height
    endif
    if !g:vimrplugin_restart
        " Let Tmux automatically kill the panel when R quits.
        let tcmd .= " '" . a:rcmd . "'"
    endif
    let rlog = system(tcmd)
    if v:shell_error
        call RWarningMsg(rlog)
        return
    endif
    let g:rplugin_rconsole_pane = TmuxActivePane()
    let rlog = system("tmux select-pane -t " . g:rplugin_vim_pane)
    if v:shell_error
        call RWarningMsg(rlog)
        return
    endif
    let g:SendCmdToR = function('SendCmdToR_TmuxSplit')
    if g:vimrplugin_restart
        sleep 200m
        let ca_ck = g:vimrplugin_ca_ck
        let g:vimrplugin_ca_ck = 0
        call g:SendCmdToR(a:rcmd)
        let g:vimrplugin_ca_ck = ca_ck
    endif
    let g:rplugin_last_rcmd = a:rcmd
    if g:vimrplugin_tmux_title != "automatic" && g:vimrplugin_tmux_title != ""
        call system("tmux rename-window " . g:vimrplugin_tmux_title)
    endif
    if WaitVimComStart()
        call SendToVimCom("\005B Update OB [StartR]")
    endif
endfunction


function StartR_ExternalTerm(rcmd)
    if $DISPLAY == "" && !g:rplugin_is_darwin
        call RWarningMsg("Start 'tmux' before Vim. The X Window system is required to run R in an external terminal.")
        return
    endif

    if g:vimrplugin_notmuxconf
        let tmuxcnf = ' '
    else
        " Create a custom tmux.conf
        let cnflines = ['set-option -g prefix C-a',
                    \ 'unbind-key C-b',
                    \ 'bind-key C-a send-prefix',
                    \ 'set-window-option -g mode-keys vi',
                    \ 'set -g status off',
                    \ 'set -g default-terminal "screen-256color"',
                    \ "set -g terminal-overrides 'xterm*:smcup@:rmcup@'" ]

        if g:vimrplugin_term == "rxvt" || g:vimrplugin_term == "urxvt"
            let cnflines = cnflines + [
                    \ "set terminal-overrides 'rxvt*:smcup@:rmcup@'" ]
        endif

        if g:vimrplugin_tmux_ob || !has("gui_running")
            call extend(cnflines, ['set -g mode-mouse on', 'set -g mouse-select-pane on', 'set -g mouse-resize-pane on'])
        endif
        call writefile(cnflines, g:rplugin_tmpdir . "/tmux.conf")
        let tmuxcnf = '-f "' . g:rplugin_tmpdir . "/tmux.conf" . '"'
    endif

    let rcmd = 'VIMRPLUGIN_TMPDIR=' . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g') . ' VIMRPLUGIN_COMPLDIR=' . substitute(g:rplugin_compldir, ' ', '\\ ', 'g') . ' VIMINSTANCEID=' . $VIMINSTANCEID . ' VIMRPLUGIN_SECRET=' . $VIMRPLUGIN_SECRET . ' VIMEDITOR_SVRNM=' . $VIMEDITOR_SVRNM . ' ' . a:rcmd

    call system("tmux has-session -t " . g:rplugin_tmuxsname)
    if v:shell_error
        if g:rplugin_is_darwin
            let $VIM_BINARY_PATH = substitute($VIMRUNTIME, "/MacVim.app/Contents/.*", "", "") . "/MacVim.app/Contents/MacOS/Vim"
            let rcmd = "VIM_BINARY_PATH=" . substitute($VIM_BINARY_PATH, ' ', '\\ ', 'g') . ' TERM=screen-256color ' . rcmd
            let opencmd = printf("tmux -2 %s new-session -s %s '%s'", tmuxcnf, g:rplugin_tmuxsname, rcmd)
            call writefile(["#!/bin/sh", opencmd], $VIMRPLUGIN_TMPDIR . "/openR")
            call system("chmod +x '" . $VIMRPLUGIN_TMPDIR . "/openR'")
            let opencmd = "open '" . $VIMRPLUGIN_TMPDIR . "/openR'"
        else
            if g:rplugin_termcmd =~ "gnome-terminal" || g:rplugin_termcmd =~ "xfce4-terminal" || g:rplugin_termcmd =~ "terminal" || g:rplugin_termcmd =~ "iterm"
                let opencmd = printf("%s 'tmux -2 %s new-session -s %s \"%s\"' &", g:rplugin_termcmd, tmuxcnf, g:rplugin_tmuxsname, rcmd)
            else
                let opencmd = printf("%s tmux -2 %s new-session -s %s \"%s\" &", g:rplugin_termcmd, tmuxcnf, g:rplugin_tmuxsname, rcmd)
            endif
        endif
    else
        if g:rplugin_is_darwin
            call RWarningMsg("Tmux session with R is already running")
            return
        endif
        if g:rplugin_termcmd =~ "gnome-terminal" || g:rplugin_termcmd =~ "xfce4-terminal" || g:rplugin_termcmd =~ "terminal" || g:rplugin_termcmd =~ "iterm"
            let opencmd = printf("%s 'tmux -2 %s attach-session -d -t %s' &", g:rplugin_termcmd, tmuxcnf, g:rplugin_tmuxsname)
        else
            let opencmd = printf("%s tmux -2 %s attach-session -d -t %s &", g:rplugin_termcmd, tmuxcnf, g:rplugin_tmuxsname)
        endif
    endif

    let rlog = system(opencmd)
    if v:shell_error
        call RWarningMsg(rlog)
        return
    endif
    let g:SendCmdToR = function('SendCmdToR_Term')
    if WaitVimComStart()
        call SendToVimCom("\005B Update OB [StartR]")
    endif
endfunction

function IsSendCmdToRFake()
    if string(g:SendCmdToR) != "function('SendCmdToR_fake')"
        if exists("g:maplocalleader")
            call RWarningMsg("As far as I know, R is already running. Did you quit it from within Vim (" . g:maplocalleader . "rq if not remapped)?")
        else
            call RWarningMsg("As far as I know, R is already running. Did you quit it from within Vim (\\rq if not remapped)?")
        endif
        return 1
    endif
    return 0
endfunction

" Start R
function StartR(whatr)
    if has("gui_macvim") && v:servername != ""
        let $VIMEDITOR_SVRNM = "MacVim_" . v:servername
        let $VIM_BINARY_PATH = substitute($VIMRUNTIME, "/MacVim.app/Contents/.*", "", "") . "/MacVim.app/Contents/MacOS/Vim"
    elseif !has("clientserver")
        let $VIMEDITOR_SVRNM = "NoClientServer"
    elseif v:servername == ""
        let $VIMEDITOR_SVRNM = "NoServerName"
    else
        let $VIMEDITOR_SVRNM = v:servername
    endif

    call writefile([], g:rplugin_tmpdir . "/globenv_" . $VIMINSTANCEID)
    call writefile([], g:rplugin_tmpdir . "/liblist_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/libnames_" . $VIMINSTANCEID)

    if !exists("b:rplugin_R")
        call SetRPath()
    endif

    " Change to buffer's directory before starting R
    if g:vimrplugin_vim_wd == 0
        lcd %:p:h
    endif

    if a:whatr =~ "vanilla"
        let b:rplugin_r_args = "--vanilla"
    else
        if a:whatr =~ "custom"
            call inputsave()
            let b:rplugin_r_args = input('Enter parameters for R: ')
            call inputrestore()
        endif
    endif

    if g:vimrplugin_applescript
        call StartR_OSX()
        return
    endif

    if has("win32") || has("win64")
        call StartR_Windows()
        return
    endif

    if g:vimrplugin_only_in_tmux && $TMUX_PANE == ""
        call RWarningMsg("Not inside Tmux.")
        if g:vimrplugin_vim_wd == 0
            lcd -
        endif
        return
    endif

    " R was already started. Should restart it or warn?
    if string(g:SendCmdToR) != "function('SendCmdToR_fake')"
        if g:rplugin_do_tmux_split
            if g:vimrplugin_restart
                call g:SendCmdToR('quit(save = "no")')
                sleep 100m
                call delete(g:rplugin_tmpdir . "/vimcom_running_" . $VIMINSTANCEID)
                let ca_ck = g:vimrplugin_ca_ck
                let g:vimrplugin_ca_ck = 0
                call g:SendCmdToR(g:rplugin_last_rcmd)
                let g:vimrplugin_ca_ck = ca_ck
                if IsExternalOBRunning()
                    if WaitVimComStart()
                        sleep 100m
                        call g:SendCmdToR("\014")
                    endif
                    call VimExprToOB('ResetVimComPort()')
                    call SendToVimCom("\002" . g:rplugin_obsname)
                    call SendToVimCom("\005G .GlobalEnv [Restarting R]")
                    call SendToVimCom("\005L Libraries [Restarting()]")
                    " vimcom automatically update the libraries view, but not
                    " the GlobalEnv one because vimcom_count_objects() returns 0.
                    call VimExprToOB('UpdateOB("GlobalEnv")')
                endif
                return
            elseif IsSendCmdToRFake()
                return
            endif
        else
            if g:vimrplugin_restart
                call RQuit("restartR")
                call ResetVimComPort()
            endif
        endif
    endif

    if b:rplugin_r_args == " "
        let rcmd = b:rplugin_R
    else
        let rcmd = b:rplugin_R . " " . b:rplugin_r_args
    endif

    if g:rplugin_do_tmux_split
        call StartR_TmuxSplit(rcmd)
    else
        if g:vimrplugin_restart && bufloaded(b:objbrtitle)
            call delete(g:rplugin_tmpdir . "/vimcom_running_" . $VIMINSTANCEID)
        endif
        call StartR_ExternalTerm(rcmd)
        if g:vimrplugin_restart && bufloaded(b:objbrtitle)
            call WaitVimComStart()
            call SendToVimCom("\002" . v:servername)
            call SendToVimCom("\005G .GlobalEnv [Restarting R]")
            call SendToVimCom("\005L Libraries [Restarting()]")
            if exists("*UpdateOB")
                call UpdateOB("GlobalEnv")
            endif
        endif
    endif

    " Go back to original directory:
    if g:vimrplugin_vim_wd == 0
        lcd -
    endif
    echon
endfunction

" To be called by edit() in R running in Neovim buffer.
function ShowRObject(fname)
    call RWarningMsg("ShowRObject not implemented yet: '" . a:fname . "'")
    let fcont = readfile(a:fname)
    let s:finalA = g:rplugin_tmpdir . "/vimcom_edit_" . $VIMINSTANCEID . "_A"
    let finalB = g:rplugin_tmpdir . "/vimcom_edit_" . $VIMINSTANCEID . "_B"
    let finalB = substitute(finalB, ' ', '\\ ', 'g')
    exe "tabnew " . finalB
    call setline(".", fcont)
    set ft=r
    stopinsert
    autocmd BufUnload <buffer> call delete(s:finalA) | unlet s:finalA | startinsert
endfunction

" Send SIGINT to R
function StopR()
    if g:rplugin_r_pid
        call system("kill -s SIGINT " . g:rplugin_r_pid)
    endif
endfunction

function OpenRScratch()
    below 6split R_Scratch
    set filetype=r
    setlocal noswapfile
    set buftype=nofile
    nmap <buffer><silent> <Esc> :quit<CR>
    nmap <buffer><silent> q :quit<CR>
    startinsert
endfunction

function WaitVimComStart()
    if g:vimrplugin_vimcom_wait < 300
        g:vimrplugin_vimcom_wait = 300
    endif
    redraw
    echo "Waiting vimcom loading..."
    sleep 300m
    let ii = 300
    let waitmsg = 0
    while !filereadable(g:rplugin_tmpdir . "/vimcom_running_" . $VIMINSTANCEID) && ii < g:vimrplugin_vimcom_wait
        let ii = ii + 200
        sleep 200m
    endwhile
    echon "\r                              "
    redraw
    sleep 100m
    if filereadable(g:rplugin_tmpdir . "/vimcom_running_" . $VIMINSTANCEID)
        let vr = readfile(g:rplugin_tmpdir . "/vimcom_running_" . $VIMINSTANCEID)
        let g:rplugin_vimcom_version = vr[0]
        let g:rplugin_vimcom_home = vr[1]
        let g:rplugin_vimcomport = vr[2]
        let g:rplugin_r_pid = vr[3]
        if g:rplugin_vimcom_version != "1.2.0"
            call RWarningMsg('This version of Vim-R-plugin requires vimcom 1.2.0.')
            sleep 1
        endif
        if has("win32")
            let g:rplugin_vimcom_lib = g:rplugin_vimcom_home . "/bin/i386/libVimR.dll"
        elseif has("win64")
            let g:rplugin_vimcom_lib = g:rplugin_vimcom_home . "/bin/x64/libVimR.dll"
        else
            let g:rplugin_vimcom_lib = g:rplugin_vimcom_home . "/bin/libVimR.so"
        endif
        if !filereadable(g:rplugin_vimcom_lib)
            call RWarningMsgInp('Could not find "' . g:rplugin_vimcom_lib . '".')
        endif
        call delete(g:rplugin_tmpdir . "/vimcom_running_" . $VIMINSTANCEID)

        if !has("libcall")
            call RWarningMsgInp("+libcall feature is missing: communication with R will be flawed.")
        endif

        if g:rplugin_do_tmux_split
            " Environment variables persists across Tmux windows.
            " Leave a hint (to vimcom) that R was not started by Vim:
            call system("tmux set-environment VIMRPLUGIN_TMPDIR None")
        endif
        return 1
    else
        call RWarningMsg("The package vimcom wasn't loaded yet.")
        sleep 500m
        return 0
    endif
endfunction

function IsExternalOBRunning()
    if exists("g:rplugin_ob_pane")
        let plst = system("tmux list-panes | cat")
        if plst =~ g:rplugin_ob_pane
            return 1
        endif
    endif
    return 0
endfunction

function ResetVimComPort()
    let g:rplugin_vimcomport = 0
endfunction

function StartObjBrowser_Tmux()
    if b:rplugin_extern_ob
        " This is the Object Browser
        echoerr "StartObjBrowser_Tmux() called."
        return
    endif

    let g:RBrOpenCloseLs = function("RBrOpenCloseLs_TmuxVim")

    call SendToVimCom("\005G GlobalEnv [OB StartObjBrowser_Tmux]")
    sleep 50m
    call SendToVimCom("\005L Libraries [OB StartObjBrowser_Tmux]")
    sleep 50m

    " Don't start the Object Browser if it already exists
    if IsExternalOBRunning()
        return
    endif

    let objbrowserfile = g:rplugin_tmpdir . "/objbrowserInit"
    let tmxs = " "

    if v:servername == ""
        let myservername = '""'
    else
        let myservername = '"' . v:servername . '"'
    endif

    call writefile([
                \ 'let g:rplugin_editor_sname = ' . myservername,
                \ 'let g:rplugin_vim_pane = "' . g:rplugin_vim_pane . '"',
                \ 'let g:rplugin_rconsole_pane = "' . g:rplugin_rconsole_pane . '"',
                \ 'let $VIMINSTANCEID = "' . $VIMINSTANCEID . '"',
                \ 'let showmarks_enable = 0',
                \ 'let g:rplugin_tmuxsname = "' . g:rplugin_tmuxsname . '"',
                \ 'let b:rscript_buffer = "' . bufname("%") . '"',
                \ 'set filetype=rbrowser',
                \ 'let g:rplugin_vimcom_home = "' . g:rplugin_vimcom_home . '"',
                \ 'let g:rplugin_vimcom_lib = "' . g:rplugin_vimcom_lib . '"',
                \ 'let b:objbrtitle = "' . b:objbrtitle . '"',
                \ 'let b:rplugin_extern_ob = 1',
                \ 'set shortmess=atI',
                \ 'set rulerformat=%3(%l%)',
                \ 'set laststatus=0',
                \ 'set noruler',
                \ 'let g:SendCmdToR = function("SendCmdToR_TmuxSplit")',
                \ 'let g:RBrOpenCloseLs = function("RBrOpenCloseLs_TmuxOB")',
                \ 'if has("clientserver") && v:servername != ""',
                \ '    let g:rplugin_vimcomport = ' . g:rplugin_vimcomport,
                \ '    call SendToVimCom("\002" . v:servername)',
                \ 'endif',
                \ 'sleep 150m',
                \ 'call UpdateOB("GlobalEnv")', ], objbrowserfile)

    if g:vimrplugin_objbr_place =~ "left"
        let panw = system("tmux list-panes | cat")
        if g:vimrplugin_objbr_place =~ "console"
            " Get the R Console width:
            let panw = substitute(panw, '.*[0-9]: \[\([0-9]*\)x[0-9]*.\{-}' . g:rplugin_rconsole_pane . '\>.*', '\1', "")
        else
            " Get the Vim width
            let panw = substitute(panw, '.*[0-9]: \[\([0-9]*\)x[0-9]*.\{-}' . g:rplugin_vim_pane . '\>.*', '\1', "")
        endif
        let panewidth = panw - g:vimrplugin_objbr_w
        " Just to be safe: If the above code doesn't work as expected
        " and we get a spurious value:
        if panewidth < 30 || panewidth > 180
            let panewidth = 80
        endif
    else
        let panewidth = g:vimrplugin_objbr_w
    endif
    if g:vimrplugin_objbr_place =~ "console"
        let obpane = g:rplugin_rconsole_pane
    else
        let obpane = g:rplugin_vim_pane
    endif

    if has("clientserver")
        let obsname = "--servername " . g:rplugin_obsname
    else
        let obsname = " "
    endif

    if g:rplugin_is_darwin && has("gui_macvim")
        let vimexec = substitute($VIMRUNTIME, "/MacVim.app/Contents/.*", "", "") . "/MacVim.app/Contents/MacOS/Vim"
        let vimexec = substitute(vimexec, ' ', '\\ ', 'g')
    else
        let vimexec = "vim"
    endif

    let cmd = "tmux split-window -h -l " . panewidth . " -t " . obpane . ' "' . vimexec . ' ' . obsname . " -c 'source " . substitute(objbrowserfile, ' ', '\\ ', 'g') . "'" . '"'
    let rlog = system(cmd)
    if v:shell_error
        let rlog = substitute(rlog, '\n', ' ', 'g')
        let rlog = substitute(rlog, '\r', ' ', 'g')
        call RWarningMsg(rlog)
        let g:rplugin_running_objbr = 0
        return 0
    endif

    let g:rplugin_ob_pane = TmuxActivePane()
    let rlog = system("tmux select-pane -t " . g:rplugin_vim_pane)
    if v:shell_error
        call RWarningMsg(rlog)
        return 0
    endif

    if g:vimrplugin_objbr_place =~ "left"
        if g:vimrplugin_objbr_place =~ "console"
            call system("tmux swap-pane -d -s " . g:rplugin_rconsole_pane . " -t " . g:rplugin_ob_pane)
        else
            call system("tmux swap-pane -d -s " . g:rplugin_vim_pane . " -t " . g:rplugin_ob_pane)
        endif
    endif
    if g:rplugin_ob_warn_shown == 0
        if !has("clientserver")
            call RWarningMsg("The +clientserver feature is required to automatically update the Object Browser.")
            sleep 200m
        else
            if $DISPLAY == ""
                call RWarningMsg("The X Window system is required to automatically update the Object Browser.")
                sleep 200m
            endif
        endif
        let g:rplugin_ob_warn_shown = 1
    endif
    return
endfunction

function StartObjBrowser_Vim()
    let g:RBrOpenCloseLs = function("RBrOpenCloseLs_Vim")
    let wmsg = ""
    if v:servername == ""
        if g:rplugin_ob_warn_shown == 0
            if !has("clientserver")
                let wmsg = "The +clientserver feature is required to automatically update the Object Browser."
            else
                if $DISPLAY == "" && !(has("win32") || has("win64"))
                    let wmsg = "The X Window system is required to automatically update the Object Browser."
                else
                    let wmsg ="The Object Browser will not be automatically updated because Vim's client/server was not started."
                endif
            endif
        endif
        let g:rplugin_ob_warn_shown = 1
    else
        call SendToVimCom("\002" . v:servername)
    endif

    " Either load or reload the Object Browser
    let savesb = &switchbuf
    set switchbuf=useopen,usetab
    if bufloaded(b:objbrtitle)
        exe "sb " . b:objbrtitle
        let wmsg = ""
    else
        " Copy the values of some local variables that will be inherited
        let g:tmp_objbrtitle = b:objbrtitle
        let g:tmp_tmuxsname = g:rplugin_tmuxsname
        let g:tmp_curbufname = bufname("%")

        let l:sr = &splitright
        if g:vimrplugin_objbr_place =~ "left"
            set nosplitright
        else
            set splitright
        endif
        if g:vimrplugin_objbr_place =~ "console"
            sb R_Output
        endif
        sil exe "vsplit " . b:objbrtitle
        let &splitright = l:sr
        sil exe "vertical resize " . g:vimrplugin_objbr_w
        sil set filetype=rbrowser

        " Inheritance of some local variables
        let g:rplugin_tmuxsname = g:tmp_tmuxsname
        let b:objbrtitle = g:tmp_objbrtitle
        let b:rscript_buffer = g:tmp_curbufname
        unlet g:tmp_objbrtitle
        unlet g:tmp_tmuxsname
        unlet g:tmp_curbufname
        call SendToVimCom("\005B Update OB [OB init GVIM]")
        sleep 50m
    endif
    if wmsg != ""
        call RWarningMsg(wmsg)
        sleep 200m
    endif
endfunction

" Open an Object Browser window
function RObjBrowser()
    " Only opens the Object Browser if R is running
    if string(g:SendCmdToR) == "function('SendCmdToR_fake')"
        call RWarningMsg("The Object Browser can be opened only if R is running.")
        return
    endif

    if g:rplugin_running_objbr == 1
        " Called twice due to BufEnter event
        return
    endif

    let g:rplugin_running_objbr = 1

    if !b:rplugin_extern_ob
        if g:vimrplugin_tmux_ob
            call StartObjBrowser_Tmux()
        else
            call StartObjBrowser_Vim()
        endif
    endif
    let g:rplugin_running_objbr = 0
    return
endfunction

function VimExprToOB(msg)
    if serverlist() =~ "\\<" . g:rplugin_obsname . "\n"
        return remote_expr(g:rplugin_obsname, a:msg)
    else
        if IsExternalOBRunning()
            let slog = system("tmux set-buffer ':call " . a:msg . "\<C-M>' && tmux paste-buffer -t " . g:rplugin_ob_pane)
            if v:shell_error
                call RWarningMsg(slog)
            endif
            return "NoVimServer"
        endif
        return "NoOBRunning"
    endif
endfunction

function RBrOpenCloseLs_Vim(status)
    if a:status == 1
        if exists("g:rplugin_curview")
            let curview = g:rplugin_curview
        else
            let curview = "GlobalEnv"
        endif
        if curview == "libraries"
            echohl WarningMsg
            echon "GlobalEnv command only."
            sleep 1
            echohl Normal
            normal! :<Esc>
            return
        endif
    endif

    " Avoid possibly freezing cross messages between Vim and R
    if exists("g:rplugin_curview") && v:servername != ""
        let stt = a:status
    else
        let stt = a:status + 2
    endif

    let switchedbuf = 0
    if buflisted("Object_Browser") && g:rplugin_curbuf != "Object_Browser"
        let savesb = &switchbuf
        set switchbuf=useopen,usetab
        sil noautocmd sb Object_Browser
        let switchedbuf = 1
    endif

    call SendToVimCom("\007" . stt)

    if switchedbuf
        exe "sil noautocmd sb " . g:rplugin_curbuf
        exe "set switchbuf=" . savesb
    endif
endfunction

function RBrOpenCloseLs_TmuxVim(status)
    if a:status == 1
        if IsExternalOBRunning() && has("clientserver") && $DISPLAY != "" && serverlist() =~ "\\<" . g:rplugin_obsname . "\n"
            let curview = remote_expr(g:rplugin_obsname, 'g:rplugin_curview')
            if curview == "libraries"
                echohl WarningMsg
                echon "GlobalEnv command only."
                sleep 1
                echohl Normal
                normal! :<Esc>
                return
            endif
        endif
    endif

    call SendToVimCom("\007" . a:status)
endfunction

function RBrOpenCloseLs_TmuxOB(status)
    if a:status == 1 && g:rplugin_curview == "libraries"
        echohl WarningMsg
        echon "GlobalEnv command only."
        sleep 1
        echohl Normal
        normal! :<Esc>
        return
    endif
    call SendToVimCom("\007" . a:status)
    if v:servername == ""
        call UpdateOB("both")
    endif
endfunction

function RFormatCode() range
    if g:rplugin_vimcomport == 0
        return
    endif

    let lns = getline(a:firstline, a:lastline)
    call writefile(lns, g:rplugin_tmpdir . "/unformatted_code")
    let wco = &textwidth
    if wco == 0
        let wco = 78
    elseif wco < 20
        let wco = 20
    elseif wco > 180
        let wco = 180
    endif
    call delete(g:rplugin_tmpdir . "/eval_reply")
    call SendToVimCom("\x08" . $VIMINSTANCEID . 'formatR::tidy_source("' . g:rplugin_tmpdir . '/unformatted_code", file = "' . g:rplugin_tmpdir . '/formatted_code", width.cutoff = ' . wco . ')')
    let g:rplugin_lastev = ReadEvalReply()
    if g:rplugin_lastev == "R is busy." || g:rplugin_lastev == "UNKNOWN" || g:rplugin_lastev =~ "^Error" || g:rplugin_lastev == "INVALID" || g:rplugin_lastev == "ERROR" || g:rplugin_lastev == "EMPTY" || g:rplugin_lastev == "No reply"
        call RWarningMsg(g:rplugin_lastev)
        return
    endif
    let lns = readfile(g:rplugin_tmpdir . "/formatted_code")
    silent exe a:firstline . "," . a:lastline . "delete"
    call append(a:firstline - 1, lns)
    echo (a:lastline - a:firstline + 1) . " lines formatted."
endfunction

function RInsert(cmd)
    if g:rplugin_vimcomport == 0
        return
    endif

    call delete(g:rplugin_tmpdir . "/eval_reply")
    call delete(g:rplugin_tmpdir . "/Rinsert")
    call SendToVimCom("\x08" . $VIMINSTANCEID . 'capture.output(' . a:cmd . ', file = "' . g:rplugin_tmpdir . '/Rinsert")')
    let g:rplugin_lastev = ReadEvalReply()
    if g:rplugin_lastev == "R is busy." || g:rplugin_lastev == "UNKNOWN" || g:rplugin_lastev =~ "^Error" || g:rplugin_lastev == "INVALID" || g:rplugin_lastev == "ERROR" || g:rplugin_lastev == "EMPTY" || g:rplugin_lastev == "No reply"
        call RWarningMsg(g:rplugin_lastev)
        return 0
    else
        silent exe "read " . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g') . "/Rinsert"
        return 1
    endif
endfunction

function SendLineToRAndInsertOutput()
    let lin = getline(".")
    if RInsert("print(" . lin . ")")
        let curpos = getpos(".")
        " comment the output
        let ilines = readfile(substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g') . "/Rinsert")
        for iln in ilines
            call RSimpleCommentLine("normal", "c")
            normal! j
        endfor
        call setpos(".", curpos)
    endif
endfunction

" Function to send commands
" return 0 on failure and 1 on success
function SendCmdToR_fake(cmd)
    call RWarningMsg("Did you already start R?")
    return 0
endfunction

function SendCmdToR_TmuxSplit(cmd)
    if g:vimrplugin_ca_ck
        let cmd = "\001" . "\013" . a:cmd
    else
        let cmd = a:cmd
    endif

    if !exists("g:rplugin_rconsole_pane")
        " Should never happen
        call RWarningMsg("Missing internal variable: g:rplugin_rconsole_pane")
    endif
    let str = substitute(cmd, "'", "'\\\\''", "g")
    let scmd = "tmux set-buffer '" . str . "\<C-M>' && tmux paste-buffer -t " . g:rplugin_rconsole_pane
    let rlog = system(scmd)
    if v:shell_error
        let rlog = substitute(rlog, "\n", " ", "g")
        let rlog = substitute(rlog, "\r", " ", "g")
        call RWarningMsg(rlog)
        call ClearRInfo()
        return 0
    endif
    return 1
endfunction

function SendCmdToR_Term(cmd)
    if g:vimrplugin_ca_ck
        let cmd = "\001" . "\013" . a:cmd
    else
        let cmd = a:cmd
    endif

    " Send the command to R running in an external terminal emulator
    let str = substitute(cmd, "'", "'\\\\''", "g")
    let scmd = "tmux set-buffer '" . str . "\<C-M>' && tmux paste-buffer -t " . g:rplugin_tmuxsname . '.0'
    let rlog = system(scmd)
    if v:shell_error
        let rlog = substitute(rlog, '\n', ' ', 'g')
        let rlog = substitute(rlog, '\r', ' ', 'g')
        call RWarningMsg(rlog)
        call ClearRInfo()
        return 0
    endif
    return 1
endfunction

" Get the word either under or after the cursor.
" Works for word(| where | is the cursor position.
function RGetKeyWord()
    " Go back some columns if character under cursor is not valid
    let save_cursor = getpos(".")
    let curline = line(".")
    let line = getline(curline)
    if strlen(line) == 0
        return ""
    endif
    " line index starts in 0; cursor index starts in 1:
    let i = col(".") - 1
    while i > 0 && "({[ " =~ line[i]
        call setpos(".", [0, line("."), i])
        let i -= 1
    endwhile
    let save_keyword = &iskeyword
    setlocal iskeyword=@,48-57,_,.,$,@-@
    let rkeyword = expand("<cword>")
    exe "setlocal iskeyword=" . save_keyword
    call setpos(".", save_cursor)
    return rkeyword
endfunction

" Send sources to R
function RSourceLines(lines, e)
    let lines = a:lines
    if &filetype == "rrst"
        let lines = map(copy(lines), 'substitute(v:val, "^\\.\\. \\?", "", "")')
    endif
    if &filetype == "rmd"
        let lines = map(copy(lines), 'substitute(v:val, "^\\`\\`\\?", "", "")')
    endif
    call writefile(lines, b:rsource)
    if a:e == "echo"
        if exists("g:vimrplugin_maxdeparse")
            let rcmd = 'base::source("' . b:rsource . '", echo=TRUE, max.deparse=' . g:vimrplugin_maxdeparse . ')'
        else
            let rcmd = 'base::source("' . b:rsource . '", echo=TRUE)'
        endif
    else
        let rcmd = 'base::source("' . b:rsource . '")'
    endif
    let ok = g:SendCmdToR(rcmd)
    return ok
endfunction

" Send file to R
function SendFileToR(e)
    update
    let fpath = expand("%:p")
    if has("win32") || has("win64")
        let fpath = substitute(fpath, "\\", "/", "g")
    endif
    if a:e == "echo"
        call g:SendCmdToR('base::source("' . fpath . '", echo=TRUE)')
    else
        call g:SendCmdToR('base::source("' . fpath . '")')
    endif
endfunction

" Send block to R
" Adapted from marksbrowser plugin
" Function to get the marks which the cursor is between
function SendMBlockToR(e, m)
    if &filetype != "r" && b:IsInRCode(1) == 0
        return
    endif

    let curline = line(".")
    let lineA = 1
    let lineB = line("$")
    let maxmarks = strlen(s:all_marks)
    let n = 0
    while n < maxmarks
        let c = strpart(s:all_marks, n, 1)
        let lnum = line("'" . c)
        if lnum != 0
            if lnum <= curline && lnum > lineA
                let lineA = lnum
            elseif lnum > curline && lnum < lineB
                let lineB = lnum
            endif
        endif
        let n = n + 1
    endwhile
    if lineA == 1 && lineB == (line("$"))
        call RWarningMsg("The file has no mark!")
        return
    endif
    if lineB < line("$")
        let lineB -= 1
    endif
    let lines = getline(lineA, lineB)
    let ok = b:SourceLines(lines, a:e)
    if ok == 0
        return
    endif
    if a:m == "down" && lineB != line("$")
        call cursor(lineB, 1)
        call GoDown()
    endif
endfunction

" Send functions to R
function SendFunctionToR(e, m)
    if &filetype != "r" && b:IsInRCode(1) == 0
        return
    endif

    let startline = line(".")
    let save_cursor = getpos(".")
    let line = SanitizeRLine(getline("."))
    let i = line(".")
    while i > 0 && line !~ "function"
        let i -= 1
        let line = SanitizeRLine(getline(i))
    endwhile
    if i == 0
        call RWarningMsg("Begin of function not found.")
        return
    endif
    let functionline = i
    while i > 0 && line !~ "<-"
        let i -= 1
        let line = SanitizeRLine(getline(i))
    endwhile
    if i == 0
        call RWarningMsg("The function assign operator  <-  was not found.")
        return
    endif
    let firstline = i
    let i = functionline
    let line = SanitizeRLine(getline(i))
    let tt = line("$")
    while i < tt && line !~ "{"
        let i += 1
        let line = SanitizeRLine(getline(i))
    endwhile
    if i == tt
        call RWarningMsg("The function opening brace was not found.")
        return
    endif
    let nb = CountBraces(line)
    while i < tt && nb > 0
        let i += 1
        let line = SanitizeRLine(getline(i))
        let nb += CountBraces(line)
    endwhile
    if nb != 0
        call RWarningMsg("The function closing brace was not found.")
        return
    endif
    let lastline = i

    if startline > lastline
        call setpos(".", [0, firstline - 1, 1])
        call SendFunctionToR(a:e, a:m)
        call setpos(".", save_cursor)
        return
    endif

    let lines = getline(firstline, lastline)
    let ok = b:SourceLines(lines, a:e)
    if  ok == 0
        return
    endif
    if a:m == "down"
        call cursor(lastline, 1)
        call GoDown()
    endif
endfunction

" Send selection to R
function SendSelectionToR(e, m)
    if &filetype != "r" && b:IsInRCode(1) == 0
        if !(&filetype == "rnoweb" && getline(".") =~ "\\Sexpr{")
            return
        endif
    endif

    if line("'<") == line("'>")
        let i = col("'<") - 1
        let j = col("'>") - i
        let l = getline("'<")
        let line = strpart(l, i, j)
        let ok = g:SendCmdToR(line)
        if ok && a:m =~ "down"
            call GoDown()
        endif
        return
    endif

    let lines = getline("'<", "'>")

    if visualmode() == "\<C-V>"
        let lj = line("'<")
        let cj = col("'<")
        let lk = line("'>")
        let ck = col("'>")
        if cj > ck
            let bb = ck - 1
            let ee = cj - ck + 1
        else
            let bb = cj - 1
            let ee = ck - cj + 1
        endif
        if cj > len(getline(lj)) || ck > len(getline(lk))
            for idx in range(0, len(lines) - 1)
                let lines[idx] = strpart(lines[idx], bb)
            endfor
        else
            for idx in range(0, len(lines) - 1)
                let lines[idx] = strpart(lines[idx], bb, ee)
            endfor
        endif
    else
        let i = col("'<") - 1
        let j = col("'>")
        let lines[0] = strpart(lines[0], i)
        let llen = len(lines) - 1
        let lines[llen] = strpart(lines[llen], 0, j)
    endif

    let ok = b:SourceLines(lines, a:e)
    if ok == 0
        return
    endif

    if a:m == "down"
        call GoDown()
    else
        normal! gv
    endif
endfunction

" Send paragraph to R
function SendParagraphToR(e, m)
    if &filetype != "r" && b:IsInRCode(1) == 0
        return
    endif

    let i = line(".")
    let c = col(".")
    let max = line("$")
    let j = i
    let gotempty = 0
    while j < max
        let j += 1
        let line = getline(j)
        if &filetype == "rnoweb" && line =~ "^@$"
            let j -= 1
            break
        elseif &filetype == "rmd" && line =~ "^[ \t]*```$"
            let j -= 1
            break
        endif
        if line =~ '^\s*$'
            break
        endif
    endwhile
    let lines = getline(i, j)
    let ok = b:SourceLines(lines, a:e)
    if ok == 0
        return
    endif
    if j < max
        call cursor(j, 1)
    else
        call cursor(max, 1)
    endif
    if a:m == "down"
        call GoDown()
    else
        call cursor(i, c)
    endif
endfunction

" Send R code from the first chunk up to current line
function SendFHChunkToR()
    if &filetype == "rnoweb"
        let begchk = "^<<.*>>=\$"
        let endchk = "^@"
        let chdchk = "^<<.*child *= *"
    elseif &filetype == "rmd"
        let begchk = "^[ \t]*```[ ]*{r"
        let endchk = "^[ \t]*```$"
        let chdchk = "^```.*child *= *"
    elseif &filetype == "rrst"
        let begchk = "^\\.\\. {r"
        let endchk = "^\\.\\. \\.\\."
        let chdchk = "^\.\. {r.*child *= *"
    else
        " Should never happen
        call RWarningMsg('Strange filetype (SendFHChunkToR): "' . &filetype '"')
    endif

    let codelines = []
    let here = line(".")
    let curbuf = getline(1, "$")
    let idx = 0
    while idx < here
        if curbuf[idx] =~ begchk
            " Child R chunk
            if curbuf[idx] =~ chdchk
                " First run everything up to child chunk and reset buffer
                call b:SourceLines(codelines, "silent")
                let codelines = []

                " Next run child chunk and continue
                call KnitChild(curbuf[idx], 'stay')
                let idx += 1
            " Regular R chunk
            else
                let idx += 1
                while curbuf[idx] !~ endchk && idx < here
                    let codelines += [curbuf[idx]]
                    let idx += 1
                endwhile
            endif
        else
            let idx += 1
        endif
    endwhile
    call b:SourceLines(codelines, "silent")
endfunction

function KnitChild(line, godown)
    let nline = substitute(a:line, '.*child *= *', "", "")
    let cfile = substitute(nline, nline[0], "", "")
    let cfile = substitute(cfile, nline[0] . '.*', "", "")
    if filereadable(cfile)
        let ok = g:SendCmdToR("require(knitr); knit('" . cfile . "', output=" . g:rplugin_null . ")")
        if a:godown =~ "down"
            call cursor(line(".")+1, 1)
            call GoDown()
        endif
    else
        call RWarningMsg("File not found: '" . cfile . "'")
    endif
endfunction

" Send current line to R.
function SendLineToR(godown)
    let line = getline(".")
    if strlen(line) == 0
        if a:godown =~ "down"
            call GoDown()
        endif
        return
    endif

    if &filetype == "rnoweb"
        if line =~ "^@$"
            if a:godown =~ "down"
                call GoDown()
            endif
            return
        endif
        if line =~ "^<<.*child *= *"
            call KnitChild(line, a:godown)
            return
        endif
        if RnwIsInRCode(1) == 0
            return
        endif
    endif

    if &filetype == "rmd"
        if line =~ "^```$"
            if a:godown =~ "down"
                call GoDown()
            endif
            return
        endif
        if line =~ "^```.*child *= *"
            call KnitChild(line, a:godown)
            return
        endif
        let line = substitute(line, "^\\`\\`\\?", "", "")
        if RmdIsInRCode(1) == 0
            return
        endif
    endif

    if &filetype == "rrst"
        if line =~ "^\.\. \.\.$"
            if a:godown =~ "down"
                call GoDown()
            endif
            return
        endif
        if line =~ "^\.\. {r.*child *= *"
            call KnitChild(line, a:godown)
            return
        endif
        let line = substitute(line, "^\\.\\. \\?", "", "")
        if RrstIsInRCode(1) == 0
            return
        endif
    endif

    if &filetype == "rdoc"
        if getline(1) =~ '^The topic'
            let topic = substitute(line, '.*::', '', "")
            let package = substitute(line, '::.*', '', "")
            call AskRDoc(topic, package, 1)
            return
        endif
        if RdocIsInRCode(1) == 0
            return
        endif
    endif

    if &filetype == "rhelp" && RhelpIsInRCode(1) == 0
        return
    endif

    let ok = g:SendCmdToR(line)
    if ok
        if a:godown =~ "down"
            call GoDown()
        else
            if a:godown == "newline"
                normal! o
            endif
        endif
    endif
endfunction

function RSendPartOfLine(direction, correctpos)
    let lin = getline(".")
    let idx = col(".") - 1
    if a:correctpos
        call cursor(line("."), idx)
    endif
    if a:direction == "right"
        let rcmd = strpart(lin, idx)
    else
        let rcmd = strpart(lin, 0, idx + 1)
    endif
    call g:SendCmdToR(rcmd)
endfunction

" Clear the console screen
function RClearConsole(...)
    if has("win32") || has("win64")
        if g:vimrplugin_Rterm
            let repl = libcall(g:rplugin_vimcom_lib, "RClearConsole", "Term")
        else
            let repl = libcall(g:rplugin_vimcom_lib, "RClearConsole", "Rgui")
        endif
        exe "sleep " . g:rplugin_sleeptime
        call foreground()
    elseif !g:vimrplugin_applescript
        call g:SendCmdToR("\014")
    endif
endfunction

" Remove all objects
function RClearAll()
    if g:vimrplugin_rmhidden
        call g:SendCmdToR("rm(list=ls(all.names = TRUE))")
    else
        call g:SendCmdToR("rm(list=ls())")
    endif
    sleep 500m
    call RClearConsole()
endfunction

"Set working directory to the path of current buffer
function RSetWD()
    let wdcmd = 'setwd("' . expand("%:p:h") . '")'
    if has("win32") || has("win64")
        let wdcmd = substitute(wdcmd, "\\", "/", "g")
    endif
    call g:SendCmdToR(wdcmd)
    sleep 100m
endfunction

function CloseExternalOB()
    if IsExternalOBRunning()
        call system("tmux kill-pane -t " . g:rplugin_ob_pane)
        unlet g:rplugin_ob_pane
        sleep 250m
    endif
endfunction

function ClearRInfo()
    if exists("g:rplugin_rconsole_pane")
        unlet g:rplugin_rconsole_pane
    endif

    call delete(g:rplugin_tmpdir . "/globenv_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/liblist_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/libnames_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/GlobalEnvList_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/vimcom_running_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/rconsole_hwnd_" . $VIMRPLUGIN_SECRET)
    let g:SendCmdToR = function('SendCmdToR_fake')
    let g:rplugin_r_pid = 0
    let g:rplugin_vimcomport = 0

    if g:rplugin_do_tmux_split && g:vimrplugin_tmux_title != "automatic" && g:vimrplugin_tmux_title != ""
        call system("tmux set automatic-rename on")
    endif
endfunction

" Quit R
function RQuit(how)
    if a:how != "restartR"
        if bufloaded(b:objbrtitle)
            exe "bunload! " . b:objbrtitle
            sleep 30m
        endif
    endif

    if exists("b:quit_command")
        let qcmd = b:quit_command
    else
        if a:how == "save"
            let qcmd = 'quit(save = "yes")'
        else
            let qcmd = 'quit(save = "no")'
        endif
    endif

    if has("win32") || has("win64")
        let repl = libcall(g:rplugin_vimcom_lib, "SendQuitMsg", qcmd . "\n")
    else
        call g:SendCmdToR(qcmd)
        if g:rplugin_do_tmux_split
            if a:how == "save"
                sleep 200m
            endif
            if g:vimrplugin_restart
                let ca_ck = g:vimrplugin_ca_ck
                let g:vimrplugin_ca_ck = 0
                call g:SendCmdToR("exit")
                let g:vimrplugin_ca_ck = ca_ck
            endif
        endif
    endif

    sleep 50m
    call CloseExternalOB()
    call ClearRInfo()
endfunction

" knit the current buffer content
function! RKnit()
    update
    if has("win32") || has("win64")
        call g:SendCmdToR('require(knitr); .vim_oldwd <- getwd(); setwd("' . substitute(expand("%:p:h"), '\\', '/', 'g') . '"); knit("' . expand("%:t") . '"); setwd(.vim_oldwd); rm(.vim_oldwd)')
    else
        call g:SendCmdToR('require(knitr); .vim_oldwd <- getwd(); setwd("' . expand("%:p:h") . '"); knit("' . expand("%:t") . '"); setwd(.vim_oldwd); rm(.vim_oldwd)')
    endif
endfunction

function SetRTextWidth(rkeyword)
    if g:vimrplugin_vimpager == "tabnew"
        let s:rdoctitle = a:rkeyword . "\\ (help)"
    else
        let s:tnr = tabpagenr()
        if g:vimrplugin_vimpager != "tab" && s:tnr > 1
            let s:rdoctitle = "R_doc" . s:tnr
        else
            let s:rdoctitle = "R_doc"
        endif
        unlet s:tnr
    endif
    if !bufloaded(s:rdoctitle) || g:vimrplugin_newsize == 1
        let g:vimrplugin_newsize = 0

        " s:vimpager is used to calculate the width of the R help documentation
        " and to decide whether to obey vimrplugin_vimpager = 'vertical'
        let s:vimpager = g:vimrplugin_vimpager

        let wwidth = winwidth(0)

        " Not enough room to split vertically
        if g:vimrplugin_vimpager == "vertical" && wwidth <= (g:vimrplugin_help_w + g:vimrplugin_editor_w)
            let s:vimpager = "horizontal"
        endif

        if s:vimpager == "horizontal"
            " Use the window width (at most 80 columns)
            let htwf = (wwidth > 80) ? 88.1 : ((wwidth - 1) / 0.9)
        elseif g:vimrplugin_vimpager == "tab" || g:vimrplugin_vimpager == "tabnew"
            let wwidth = &columns
            let htwf = (wwidth > 80) ? 88.1 : ((wwidth - 1) / 0.9)
        else
            let min_e = (g:vimrplugin_editor_w > 80) ? g:vimrplugin_editor_w : 80
            let min_h = (g:vimrplugin_help_w > 73) ? g:vimrplugin_help_w : 73

            if wwidth > (min_e + min_h)
                " The editor window is large enough to be split
                let s:hwidth = min_h
            elseif wwidth > (min_e + g:vimrplugin_help_w)
                " The help window must have less than min_h columns
                let s:hwidth = wwidth - min_e
            else
                " The help window must have the minimum value
                let s:hwidth = g:vimrplugin_help_w
            endif
            let htwf = (s:hwidth - 1) / 0.9
        endif
        let htw = printf("%f", htwf)
        let g:rplugin_htw = substitute(htw, "\\..*", "", "")
        let g:rplugin_htw = g:rplugin_htw - (&number || &relativenumber) * &numberwidth
    endif
endfunction

function RGetClassFor(rkeyword)
    let classfor = ""
    let line = substitute(getline("."), '#.*', '', "")
    let begin = col(".")
    if strlen(line) > begin
        let piece = strpart(line, begin)
        while piece !~ '^' . a:rkeyword && begin >= 0
            let begin -= 1
            let piece = strpart(line, begin)
        endwhile
        let line = piece
        if line !~ '^\k*\s*('
            return classfor
        endif
        let begin = 1
        let linelen = strlen(line)
        while line[begin] != '(' && begin < linelen
            let begin += 1
        endwhile
        let begin += 1
        let line = strpart(line, begin)
        let line = substitute(line, '^\s*', '', "")
        if (line =~ '^\k*\s*(' || line =~ '^\k*\s*=\s*\k*\s*(') && line !~ '[.*('
            let idx = 0
            while line[idx] != '('
                let idx += 1
            endwhile
            let idx += 1
            let nparen = 1
            let len = strlen(line)
            let lnum = line(".")
            while nparen != 0
                if line[idx] == '('
                    let nparen += 1
                else
                    if line[idx] == ')'
                        let nparen -= 1
                    endif
                endif
                let idx += 1
                if idx == len
                    let lnum += 1
                    let line = line . substitute(getline(lnum), '#.*', '', "")
                    let len = strlen(line)
                endif
            endwhile
            let classfor = strpart(line, 0, idx)
        elseif line =~ '^\(\k\|\$\)*\s*[' || line =~ '^\(k\|\$\)*\s*=\s*\(\k\|\$\)*\s*[.*('
            let idx = 0
            while line[idx] != '['
                let idx += 1
            endwhile
            let idx += 1
            let nparen = 1
            let len = strlen(line)
            let lnum = line(".")
            while nparen != 0
                if line[idx] == '['
                    let nparen += 1
                else
                    if line[idx] == ']'
                        let nparen -= 1
                    endif
                endif
                let idx += 1
                if idx == len
                    let lnum += 1
                    let line = line . substitute(getline(lnum), '#.*', '', "")
                    let len = strlen(line)
                endif
            endwhile
            let classfor = strpart(line, 0, idx)
        else
            let classfor = substitute(line, ').*', '', "")
            let classfor = substitute(classfor, ',.*', '', "")
            let classfor = substitute(classfor, ' .*', '', "")
        endif
    endif
    if classfor =~ "^'" && classfor =~ "'$"
        let classfor = substitute(classfor, "^'", '"', "")
        let classfor = substitute(classfor, "'$", '"', "")
    endif
    return classfor
endfunction

" Show R's help doc in Vim's buffer
" (based  on pydoc plugin)
function AskRDoc(rkeyword, package, getclass)
    if filewritable(g:rplugin_docfile)
        call delete(g:rplugin_docfile)
    endif

    let classfor = ""
    if bufname("%") =~ "Object_Browser" || bufname("%") == "R_Output"
        let savesb = &switchbuf
        set switchbuf=useopen,usetab
        exe "sb " . b:rscript_buffer
        exe "set switchbuf=" . savesb
    else
        if a:getclass
            let classfor = RGetClassFor(a:rkeyword)
        endif
    endif

    if classfor =~ '='
        let classfor = "eval(expression(" . classfor . "))"
    endif

    call SetRTextWidth(a:rkeyword)

    if classfor == "" && a:package == ""
        let rcmd = 'vimcom:::vim.help("' . a:rkeyword . '", ' . g:rplugin_htw . 'L)'
    elseif a:package != ""
        let rcmd = 'vimcom:::vim.help("' . a:rkeyword . '", ' . g:rplugin_htw . 'L, package="' . a:package  . '")'
    else
        let classfor = substitute(classfor, '\\', "", "g")
        let classfor = substitute(classfor, '\(.\)"\(.\)', '\1\\"\2', "g")
        let rcmd = 'vimcom:::vim.help("' . a:rkeyword . '", ' . g:rplugin_htw . 'L, ' . classfor . ')'
    endif

    call SendToVimCom("\x08" . $VIMINSTANCEID . rcmd)
endfunction

" This function is called by vimcom
function ShowRDoc(rkeyword)
    let rkeyw = a:rkeyword
    if a:rkeyword =~ "^MULTILIB"
        let msgs = split(a:rkeyword)
        " Vim cannot receive message from vimcom before replying to this message
        let flines = ['',
                    \ 'The topic "' . msgs[-1] . '" was found in more than one library.',
                    \ 'Press <Enter> over one of them to see the R documentation:',
                    \ '']
        for idx in range(1, len(msgs) - 2)
            let flines += [ '   ' . msgs[idx] ]
        endfor
        call writefile(flines, g:rplugin_docfile)
        let rkeyw = msgs[-1]
    endif

    if bufname("%") =~ "Object_Browser" || bufname("%") == "R_Output"
        let savesb = &switchbuf
        set switchbuf=useopen,usetab
        exe "sb " . b:rscript_buffer
        exe "set switchbuf=" . savesb
    endif
    call SetRTextWidth(rkeyw)

    " Local variables that must be inherited by the rdoc buffer
    let g:tmp_tmuxsname = g:rplugin_tmuxsname
    let g:tmp_objbrtitle = b:objbrtitle

    let rdoccaption = substitute(s:rdoctitle, '\', '', "g")
    if a:rkeyword =~ "R History"
        let rdoccaption = "R_History"
        let s:rdoctitle = "R_History"
    endif
    if bufloaded(rdoccaption)
        let curtabnr = tabpagenr()
        let savesb = &switchbuf
        set switchbuf=useopen,usetab
        exe "sb ". s:rdoctitle
        exe "set switchbuf=" . savesb
        if g:vimrplugin_vimpager == "tabnew"
            exe "tabmove " . curtabnr
        endif
    else
        if g:vimrplugin_vimpager == "tab" || g:vimrplugin_vimpager == "tabnew"
            exe 'tabnew ' . s:rdoctitle
        elseif s:vimpager == "vertical"
            let l:sr = &splitright
            set splitright
            exe s:hwidth . 'vsplit ' . s:rdoctitle
            let &splitright = l:sr
        elseif s:vimpager == "horizontal"
            exe 'split ' . s:rdoctitle
            if winheight(0) < 20
                resize 20
            endif
        else
            echohl WarningMsg
            echomsg 'Invalid vimrplugin_vimpager value: "' . g:vimrplugin_vimpager . '". Valid values are: "tab", "vertical", "horizontal", "tabnew" and "no".'
            echohl Normal
            return
        endif
    endif

    setlocal modifiable
    let g:rplugin_curbuf = bufname("%")

    " Inheritance of local variables from the script buffer
    let b:objbrtitle = g:tmp_objbrtitle
    let g:rplugin_tmuxsname = g:tmp_tmuxsname
    unlet g:tmp_objbrtitle
    unlet g:tmp_tmuxsname

    let save_unnamed_reg = @@
    sil normal! ggdG
    let fcntt = readfile(g:rplugin_docfile)
    call setline(1, fcntt)
    if a:rkeyword =~ "R History"
        set filetype=r
        call cursor(1, 1)
    elseif a:rkeyword =~ "^MULTILIB"
        syn match Special '<Enter>'
        exe 'syn match String /"' . rkeyw . '"/'
        for idx in range(1, len(msgs) - 2)
            exe "syn match PreProc '^   " . msgs[idx] . "'"
        endfor
        exe 'nmap <buffer><silent> <CR> :call AskRDoc("' . rkeyw . '", expand("<cword>"), 0)<CR>'
        redraw
        call cursor(5, 4)
    else
        set filetype=rdoc
        call cursor(1, 1)
    endif
    let @@ = save_unnamed_reg
    setlocal nomodified
    redraw
    stopinsert
endfunction

function RSetPDFViewer()
    if exists("g:vimrplugin_pdfviewer") && g:vimrplugin_pdfviewer != "none"
        let g:rplugin_pdfviewer = tolower(g:vimrplugin_pdfviewer)
    else
        " Try to guess what PDF viewer is used:
        if has("win32") || has("win64")
            let g:rplugin_pdfviewer = "sumatra"
        elseif g:rplugin_is_darwin
            let g:rplugin_pdfviewer = "skim"
        elseif executable("evince")
            let g:rplugin_pdfviewer = "evince"
        elseif executable("okular")
            let g:rplugin_pdfviewer = "okular"
        else
            let g:rplugin_pdfviewer = "none"
            if $R_PDFVIEWER == ""
                let pdfvl = ["xdg-open"]
            else
                let pdfvl = [$R_PDFVIEWER, "xdg-open"]
            endif
            " List from R configure script:
            let pdfvl += ["evince", "okular", "zathura", "xpdf", "gv", "gnome-gv", "ggv", "kpdf", "gpdf", "kghostview,", "acroread", "acroread4"]
            for prog in pdfvl
                if executable(prog)
                    let g:rplugin_pdfviewer = prog
                    break
                endif
            endfor
        endif
    endif

    if executable("wmctrl")
        let g:rplugin_has_wmctrl = 1
    else
        let g:rplugin_has_wmctrl = 0
    endif

    if g:rplugin_pdfviewer == "zathura"
        if g:rplugin_has_wmctrl == 0
            let g:rplugin_pdfviewer = "none"
            call RWarningMsgInp("The application wmctrl must be installed to use Zathura as PDF viewer.")
        else
            if executable("dbus-send")
                let g:rplugin_has_dbussend = 1
            else
                let g:rplugin_has_dbussend = 0
            endif
        endif
    endif

    " Try to guess the title of the window where Vim is running:
    if has("gui_running")
        call RSetDefaultValue("g:vimrplugin_vim_window", "'GVim'")
    elseif g:rplugin_pdfviewer == "evince"
        call RSetDefaultValue("g:vimrplugin_vim_window", "'Terminal'")
    elseif g:rplugin_pdfviewer == "okular"
        call RSetDefaultValue("g:vimrplugin_vim_window", "'Konsole'")
    else
        call RSetDefaultValue("g:vimrplugin_vim_window", "'term'")
    endif

endfunction

function RStart_Zathura(basenm)
    let a2 = 'a2 = "vim --servername ' . v:servername . " --remote-expr \\\"SyncTeX_backward('%{input}',%{line})\\\"" . '"'
    let pycode = ["import subprocess",
                \ "import os",
                \ "import sys",
                \ "FNULL = open(os.devnull, 'w')",
                \ "a1 = '--synctex-editor-command'",
                \ a2,
                \ "a3 = '" . a:basenm . ".pdf'",
                \ "zpid = subprocess.Popen(['zathura', a1, a2, a3], stdout = FNULL, stderr = FNULL).pid",
                \ "sys.stdout.write(str(zpid))" ]
    call writefile(pycode, g:rplugin_tmpdir . "/start_zathura.py")
    let pid = system("python '" . g:rplugin_tmpdir . "/start_zathura.py" . "'")
    let g:rplugin_zathura_pid[a:basenm] = pid
    call delete(g:rplugin_tmpdir . "/start_zathura.py")
endfunction

function ROpenPDF(path)
    if a:path == "Get Master"
        let tmpvar = SyncTeX_GetMaster()
        let pdfpath = tmpvar[1] . '/' . tmpvar[0] . '.pdf'
    else
        let pdfpath = a:path
    endif
    let basenm = substitute(substitute(pdfpath, '.*/', '', ''), '\.pdf$', '', '')

    let olddir = getcwd()
    if olddir != expand("%:p:h")
        exe "cd " . substitute(expand("%:p:h"), ' ', '\\ ', 'g')
    endif

    if !filereadable(basenm . ".pdf")
        call RWarningMsg('File not found: "' . basenm . '.pdf".')
        exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
        return
    endif
    if g:rplugin_pdfviewer == "none"
        call RWarningMsg("Could not find a PDF viewer, and vimrplugin_pdfviewer is not defined.")
    else
        if g:rplugin_pdfviewer == "okular"
            let pcmd = "okular --unique '" .  pdfpath . "' 2>/dev/null >/dev/null &"
        elseif g:rplugin_pdfviewer == "zathura"
            if system("wmctrl -xl") =~ 'Zathura.*' . basenm . '.pdf' && g:rplugin_zathura_pid[basenm] != 0
                call system("wmctrl -a '" . basenm . ".pdf'")
            else
                let g:rplugin_zathura_pid[basenm] = 0
                call RStart_Zathura(basenm)
            endif
            exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
            return
        elseif g:rplugin_pdfviewer == "sumatra" && (g:rplugin_sumatra_path != "" || FindSumatra())
            silent exe '!start "' . g:rplugin_sumatra_path . '" -reuse-instance -inverse-search "vim --servername ' . v:servername . " --remote-expr SyncTeX_backward('\\%f',\\%l)" . '" "' . basenm . '.pdf"'
            exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
            return
        elseif g:rplugin_pdfviewer == "skim"
            call system(g:macvim_skim_app_path . '/Contents/MacOS/Skim "' . basenm . '.pdf" 2> /dev/null >/dev/null &')
        else
            let pcmd = g:rplugin_pdfviewer . " '" . pdfpath . "' 2>/dev/null >/dev/null &"
        call system(pcmd)
        endif
        if g:rplugin_has_wmctrl
            call system("wmctrl -a '" . basenm . ".pdf'")
        endif
    endif
    exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
endfunction


function RSourceDirectory(...)
    if has("win32") || has("win64")
        let dir = substitute(a:1, '\\', '/', "g")
    else
        let dir = a:1
    endif
    if dir == ""
        call g:SendCmdToR("vim.srcdir()")
    else
        call g:SendCmdToR("vim.srcdir('" . dir . "')")
    endif
endfunction

function RAskHelp(...)
    if a:1 == ""
        call g:SendCmdToR("help.start()")
        return
    endif
    if g:vimrplugin_vimpager != "no"
        call AskRDoc(a:1, "", 0)
    else
        call g:SendCmdToR("help(" . a:1. ")")
    endif
endfunction

function PrintRObject(rkeyword)
    if bufname("%") =~ "Object_Browser"
        let classfor = ""
    else
        let classfor = RGetClassFor(a:rkeyword)
    endif
    if classfor == ""
        call g:SendCmdToR("print(" . a:rkeyword . ")")
    else
        call g:SendCmdToR('vim.print("' . a:rkeyword . '", ' . classfor . ")")
    endif
endfunction

" Call R functions for the word under cursor
function RAction(rcmd)
    if &filetype == "rbrowser"
        let rkeyword = RBrowserGetName(1, 0)
    else
        let rkeyword = RGetKeyWord()
    endif
    if strlen(rkeyword) > 0
        if a:rcmd == "help"
            if g:vimrplugin_vimpager == "no"
                call g:SendCmdToR("help(" . rkeyword . ")")
            else
                if bufname("%") =~ "Object_Browser" || b:rplugin_extern_ob
                    if g:rplugin_curview == "libraries"
                        let pkg = RBGetPkgName()
                    else
                        let pkg = ""
                    endif
                    if b:rplugin_extern_ob
                        if g:rplugin_vim_pane == "none"
                            call RWarningMsg("Cmd not available.")
                        else
                            if g:rplugin_editor_sname == ""
                                let slog = system("tmux set-buffer '" . "\<C-\>\<C-N>" . ':call AskRDoc("' . rkeyword . '", "' . pkg . '", 0)' . "\<C-M>' && tmux paste-buffer -t " . g:rplugin_vim_pane . " && tmux select-pane -t " . g:rplugin_vim_pane)
                                if v:shell_error
                                    call RWarningMsg(slog)
                                endif
                            else
                                silent exe 'call remote_expr("' . g:rplugin_editor_sname . '", ' . "'AskRDoc(" . '"' . rkeyword . '", "' . pkg . '", 0)' . "')"
                            endif
                        endif
                    else
                        call AskRDoc(rkeyword, pkg, 0)
                    endif
                    return
                endif
                call AskRDoc(rkeyword, "", 1)
            endif
            return
        endif
        if a:rcmd == "print"
            call PrintRObject(rkeyword)
            return
        endif
        let rfun = a:rcmd
        if a:rcmd == "args" && g:vimrplugin_listmethods == 1
            let rfun = "vim.list.args"
        endif
        if a:rcmd == "plot" && g:vimrplugin_specialplot == 1
            let rfun = "vim.plot"
        endif
        if a:rcmd == "plotsumm"
            if g:vimrplugin_specialplot == 1
                let raction = "vim.plot(" . rkeyword . "); summary(" . rkeyword . ")"
            else
                let raction = "plot(" . rkeyword . "); summary(" . rkeyword . ")"
            endif
            call g:SendCmdToR(raction)
            return
        endif

        let raction = rfun . "(" . rkeyword . ")"
        call g:SendCmdToR(raction)
    endif
endfunction

if exists('g:maplocalleader')
    let s:tll = '<Tab>' . g:maplocalleader
else
    let s:tll = '<Tab>\\'
endif

redir => s:ikblist
silent imap
redir END
redir => s:nkblist
silent nmap
redir END
redir => s:vkblist
silent vmap
redir END
let s:iskblist = split(s:ikblist, "\n")
let s:nskblist = split(s:nkblist, "\n")
let s:vskblist = split(s:vkblist, "\n")
let s:imaplist = []
let s:vmaplist = []
let s:nmaplist = []
for i in s:iskblist
    let si = split(i)
    if len(si) == 3 && si[2] =~ "<Plug>R"
        call add(s:imaplist, [si[1], si[2]])
    endif
endfor
for i in s:nskblist
    let si = split(i)
    if len(si) == 3 && si[2] =~ "<Plug>R"
        call add(s:nmaplist, [si[1], si[2]])
    endif
endfor
for i in s:vskblist
    let si = split(i)
    if len(si) == 3 && si[2] =~ "<Plug>R"
        call add(s:vmaplist, [si[1], si[2]])
    endif
endfor
unlet s:ikblist
unlet s:nkblist
unlet s:vkblist
unlet s:iskblist
unlet s:nskblist
unlet s:vskblist
unlet i
unlet si

function RNMapCmd(plug)
    for [el1, el2] in s:nmaplist
        if el2 == a:plug
            return el1
        endif
    endfor
endfunction

function RIMapCmd(plug)
    for [el1, el2] in s:imaplist
        if el2 == a:plug
            return el1
        endif
    endfor
endfunction

function RVMapCmd(plug)
    for [el1, el2] in s:vmaplist
        if el2 == a:plug
            return el1
        endif
    endfor
endfunction

function RCreateMenuItem(type, label, plug, combo, target)
    if a:type =~ '0'
        let tg = a:target . '<CR>0'
        let il = 'i'
    else
        let tg = a:target . '<CR>'
        let il = 'a'
    endif
    if a:type =~ "n"
        if hasmapto(a:plug, "n")
            let boundkey = RNMapCmd(a:plug)
            exec 'nmenu <silent> &R.' . a:label . '<Tab>' . boundkey . ' ' . tg
        else
            exec 'nmenu <silent> &R.' . a:label . s:tll . a:combo . ' ' . tg
        endif
    endif
    if a:type =~ "v"
        if hasmapto(a:plug, "v")
            let boundkey = RVMapCmd(a:plug)
            exec 'vmenu <silent> &R.' . a:label . '<Tab>' . boundkey . ' ' . '<Esc>' . tg
        else
            exec 'vmenu <silent> &R.' . a:label . s:tll . a:combo . ' ' . '<Esc>' . tg
        endif
    endif
    if a:type =~ "i"
        if hasmapto(a:plug, "i")
            let boundkey = RIMapCmd(a:plug)
            exec 'imenu <silent> &R.' . a:label . '<Tab>' . boundkey . ' ' . '<Esc>' . tg . il
        else
            exec 'imenu <silent> &R.' . a:label . s:tll . a:combo . ' ' . '<Esc>' . tg . il
        endif
    endif
endfunction

function RBrowserMenu()
    call RCreateMenuItem("nvi", 'Object\ browser.Show/Update', '<Plug>RUpdateObjBrowser', 'ro', ':call RObjBrowser()')
    call RCreateMenuItem("nvi", 'Object\ browser.Expand\ (all\ lists)', '<Plug>ROpenLists', 'r=', ':call g:RBrOpenCloseLs(1)')
    call RCreateMenuItem("nvi", 'Object\ browser.Collapse\ (all\ lists)', '<Plug>RCloseLists', 'r-', ':call g:RBrOpenCloseLs(0)')
    if &filetype == "rbrowser"
        imenu <silent> R.Object\ browser.Toggle\ (cur)<Tab>Enter <Esc>:call RBrowserDoubleClick()<CR>
        nmenu <silent> R.Object\ browser.Toggle\ (cur)<Tab>Enter :call RBrowserDoubleClick()<CR>
    endif
    let g:rplugin_hasmenu = 1
endfunction

function RControlMenu()
    call RCreateMenuItem("nvi", 'Command.List\ space', '<Plug>RListSpace', 'rl', ':call g:SendCmdToR("ls()")')
    call RCreateMenuItem("nvi", 'Command.Clear\ console\ screen', '<Plug>RClearConsole', 'rr', ':call RClearConsole()')
    call RCreateMenuItem("nvi", 'Command.Clear\ all', '<Plug>RClearAll', 'rm', ':call RClearAll()')
    "-------------------------------
    menu R.Command.-Sep1- <nul>
    call RCreateMenuItem("nvi", 'Command.Print\ (cur)', '<Plug>RObjectPr', 'rp', ':call RAction("print")')
    call RCreateMenuItem("nvi", 'Command.Names\ (cur)', '<Plug>RObjectNames', 'rn', ':call RAction("vim.names")')
    call RCreateMenuItem("nvi", 'Command.Structure\ (cur)', '<Plug>RObjectStr', 'rt', ':call RAction("str")')
    "-------------------------------
    menu R.Command.-Sep2- <nul>
    call RCreateMenuItem("nvi", 'Command.Arguments\ (cur)', '<Plug>RShowArgs', 'ra', ':call RAction("args")')
    call RCreateMenuItem("nvi", 'Command.Example\ (cur)', '<Plug>RShowEx', 're', ':call RAction("example")')
    call RCreateMenuItem("nvi", 'Command.Help\ (cur)', '<Plug>RHelp', 'rh', ':call RAction("help")')
    "-------------------------------
    menu R.Command.-Sep3- <nul>
    call RCreateMenuItem("nvi", 'Command.Summary\ (cur)', '<Plug>RSummary', 'rs', ':call RAction("summary")')
    call RCreateMenuItem("nvi", 'Command.Plot\ (cur)', '<Plug>RPlot', 'rg', ':call RAction("plot")')
    call RCreateMenuItem("nvi", 'Command.Plot\ and\ summary\ (cur)', '<Plug>RSPlot', 'rb', ':call RAction("plotsumm")')
    let g:rplugin_hasmenu = 1
endfunction

function RControlMaps()
    " List space, clear console, clear all
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RListSpace',    'rl', ':call g:SendCmdToR("ls()")')
    call RCreateMaps("nvi", '<Plug>RClearConsole', 'rr', ':call RClearConsole()')
    call RCreateMaps("nvi", '<Plug>RClearAll',     'rm', ':call RClearAll()')

    " Print, names, structure
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RObjectPr',     'rp', ':call RAction("print")')
    call RCreateMaps("nvi", '<Plug>RObjectNames',  'rn', ':call RAction("vim.names")')
    call RCreateMaps("nvi", '<Plug>RObjectStr',    'rt', ':call RAction("str")')

    " Arguments, example, help
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RShowArgs',     'ra', ':call RAction("args")')
    call RCreateMaps("nvi", '<Plug>RShowEx',       're', ':call RAction("example")')
    call RCreateMaps("nvi", '<Plug>RHelp',         'rh', ':call RAction("help")')

    " Summary, plot, both
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RSummary',      'rs', ':call RAction("summary")')
    call RCreateMaps("nvi", '<Plug>RPlot',         'rg', ':call RAction("plot")')
    call RCreateMaps("nvi", '<Plug>RSPlot',        'rb', ':call RAction("plotsumm")')

    " Build list of objects for omni completion
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RUpdateObjBrowser', 'ro', ':call RObjBrowser()')
    call RCreateMaps("nvi", '<Plug>ROpenLists',        'r=', ':call g:RBrOpenCloseLs(1)')
    call RCreateMaps("nvi", '<Plug>RCloseLists',       'r-', ':call g:RBrOpenCloseLs(0)')
endfunction


" For each noremap we need a vnoremap including <Esc> before the :call,
" otherwise vim will call the function as many times as the number of selected
" lines. If we put the <Esc> in the noremap, vim will bell.
" RCreateMaps Args:
"   type : modes to which create maps (normal, visual and insert) and whether
"          the cursor have to go the beginning of the line
"   plug : the <Plug>Name
"   combo: the combination of letter that make the shortcut
"   target: the command or function to be called
function RCreateMaps(type, plug, combo, target)
    if a:type =~ '0'
        let tg = a:target . '<CR>0'
        let il = 'i'
    else
        let tg = a:target . '<CR>'
        let il = 'a'
    endif
    if a:type =~ "n"
        if hasmapto(a:plug, "n")
            exec 'noremap <buffer><silent> ' . a:plug . ' ' . tg
        elseif g:vimrplugin_user_maps_only == 0
            exec 'noremap <buffer><silent> <LocalLeader>' . a:combo . ' ' . tg
        endif
    endif
    if a:type =~ "v"
        if hasmapto(a:plug, "v")
            exec 'vnoremap <buffer><silent> ' . a:plug . ' <Esc>' . tg
        elseif g:vimrplugin_user_maps_only == 0
            exec 'vnoremap <buffer><silent> <LocalLeader>' . a:combo . ' <Esc>' . tg
        endif
    endif
    if g:vimrplugin_insert_mode_cmds == 1 && a:type =~ "i"
        if hasmapto(a:plug, "i")
            exec 'inoremap <buffer><silent> ' . a:plug . ' <Esc>' . tg . il
        elseif g:vimrplugin_user_maps_only == 0
            exec 'inoremap <buffer><silent> <LocalLeader>' . a:combo . ' <Esc>' . tg . il
        endif
    endif
endfunction


function SpaceForRGrDevice()
    let savesb = &switchbuf
    set switchbuf=useopen,usetab
    let l:sr = &splitright
    set splitright
    37vsplit Space_for_Graphics
    setlocal nomodifiable
    setlocal noswapfile
    set buftype=nofile
    set nowrap
    set winfixwidth
    exe "sb " . g:rplugin_curbuf
    let &splitright = l:sr
    exe "set switchbuf=" . savesb
endfunction

function RCreateStartMaps()
    " Start
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RStart',        'rf', ':call StartR("R")')
    call RCreateMaps("nvi", '<Plug>RVanillaStart', 'rv', ':call StartR("vanilla")')
    call RCreateMaps("nvi", '<Plug>RCustomStart',  'rc', ':call StartR("custom")')

    " Close
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RClose',        'rq', ":call RQuit('nosave')")
    call RCreateMaps("nvi", '<Plug>RSaveClose',    'rw', ":call RQuit('save')")

endfunction

function RCreateEditMaps()
    " Edit
    "-------------------------------------
    call RCreateMaps("ni", '<Plug>RToggleComment',   'xx', ':call RComment("normal")')
    call RCreateMaps("v", '<Plug>RToggleComment',   'xx', ':call RComment("selection")')
    call RCreateMaps("ni", '<Plug>RSimpleComment',   'xc', ':call RSimpleCommentLine("normal", "c")')
    call RCreateMaps("v", '<Plug>RSimpleComment',   'xc', ':call RSimpleCommentLine("selection", "c")')
    call RCreateMaps("ni", '<Plug>RSimpleUnComment',   'xu', ':call RSimpleCommentLine("normal", "u")')
    call RCreateMaps("v", '<Plug>RSimpleUnComment',   'xu', ':call RSimpleCommentLine("selection", "u")')
    call RCreateMaps("ni", '<Plug>RRightComment',   ';', ':call MovePosRCodeComment("normal")')
    call RCreateMaps("v", '<Plug>RRightComment',    ';', ':call MovePosRCodeComment("selection")')
    " Replace 'underline' with '<-'
    if g:vimrplugin_assign == 1 || g:vimrplugin_assign == 2
        silent exe 'imap <buffer><silent> ' . g:vimrplugin_assign_map . ' <Esc>:call ReplaceUnderS()<CR>a'
    endif
    if hasmapto("<Plug>RCompleteArgs", "i")
        imap <buffer><silent> <Plug>RCompleteArgs <C-R>=RCompleteArgs()<CR>
    else
        imap <buffer><silent> <C-X><C-A> <C-R>=RCompleteArgs()<CR>
    endif
endfunction

function RCreateSendMaps()
    " Block
    "-------------------------------------
    call RCreateMaps("ni", '<Plug>RSendMBlock',     'bb', ':call SendMBlockToR("silent", "stay")')
    call RCreateMaps("ni", '<Plug>RESendMBlock',    'be', ':call SendMBlockToR("echo", "stay")')
    call RCreateMaps("ni", '<Plug>RDSendMBlock',    'bd', ':call SendMBlockToR("silent", "down")')
    call RCreateMaps("ni", '<Plug>REDSendMBlock',   'ba', ':call SendMBlockToR("echo", "down")')

    " Function
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RSendFunction',  'ff', ':call SendFunctionToR("silent", "stay")')
    call RCreateMaps("nvi", '<Plug>RDSendFunction', 'fe', ':call SendFunctionToR("echo", "stay")')
    call RCreateMaps("nvi", '<Plug>RDSendFunction', 'fd', ':call SendFunctionToR("silent", "down")')
    call RCreateMaps("nvi", '<Plug>RDSendFunction', 'fa', ':call SendFunctionToR("echo", "down")')

    " Selection
    "-------------------------------------
    call RCreateMaps("v", '<Plug>RSendSelection',   'ss', ':call SendSelectionToR("silent", "stay")')
    call RCreateMaps("v", '<Plug>RESendSelection',  'se', ':call SendSelectionToR("echo", "stay")')
    call RCreateMaps("v", '<Plug>RDSendSelection',  'sd', ':call SendSelectionToR("silent", "down")')
    call RCreateMaps("v", '<Plug>REDSendSelection', 'sa', ':call SendSelectionToR("echo", "down")')

    " Paragraph
    "-------------------------------------
    call RCreateMaps("ni", '<Plug>RSendParagraph',   'pp', ':call SendParagraphToR("silent", "stay")')
    call RCreateMaps("ni", '<Plug>RESendParagraph',  'pe', ':call SendParagraphToR("echo", "stay")')
    call RCreateMaps("ni", '<Plug>RDSendParagraph',  'pd', ':call SendParagraphToR("silent", "down")')
    call RCreateMaps("ni", '<Plug>REDSendParagraph', 'pa', ':call SendParagraphToR("echo", "down")')

    if &filetype == "rnoweb" || &filetype == "rmd" || &filetype == "rrst"
        call RCreateMaps("ni", '<Plug>RSendChunkFH', 'ch', ':call SendFHChunkToR()')
    endif

    " *Line*
    "-------------------------------------
    call RCreateMaps("ni", '<Plug>RSendLine', 'l', ':call SendLineToR("stay")')
    call RCreateMaps('ni0', '<Plug>RDSendLine', 'd', ':call SendLineToR("down")')
    call RCreateMaps('ni0', '<Plug>RDSendLineAndInsertOutput', 'o', ':call SendLineToRAndInsertOutput()')
    call RCreateMaps('i', '<Plug>RSendLAndOpenNewOne', 'q', ':call SendLineToR("newline")')
    call RCreateMaps('n', '<Plug>RNLeftPart', 'r<left>', ':call RSendPartOfLine("left", 0)')
    call RCreateMaps('n', '<Plug>RNRightPart', 'r<right>', ':call RSendPartOfLine("right", 0)')
    call RCreateMaps('i', '<Plug>RILeftPart', 'r<left>', 'l:call RSendPartOfLine("left", 1)')
    call RCreateMaps('i', '<Plug>RIRightPart', 'r<right>', 'l:call RSendPartOfLine("right", 1)')

    " For compatibility with Johannes Ranke's plugin
    if g:vimrplugin_map_r == 1
        vnoremap <buffer><silent> r <Esc>:call SendSelectionToR("silent", "down")<CR>
    endif
endfunction

function RBufEnter()
    let g:rplugin_curbuf = bufname("%")
    if has("gui_running")
        if &filetype != g:rplugin_lastft
            call UnMakeRMenu()
            if &filetype == "r" || &filetype == "rnoweb" || &filetype == "rmd" || &filetype == "rrst" || &filetype == "rdoc" || &filetype == "rbrowser" || &filetype == "rhelp"
                if &filetype == "rbrowser"
                    call MakeRBrowserMenu()
                else
                    call MakeRMenu()
                endif
            endif
        endif
        if &buftype != "nofile" || (&buftype == "nofile" && &filetype == "rbrowser")
            let g:rplugin_lastft = &filetype
        endif
    endif
endfunction

function RVimLeave()
    if exists("b:rsource")
        " b:rsource only exists if the filetype of the last buffer is .R*
        call delete(b:rsource)
    endif
    call delete(g:rplugin_tmpdir . "/eval_reply")
    call delete(g:rplugin_tmpdir . "/formatted_code")
    call delete(g:rplugin_tmpdir . "/GlobalEnvList_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/globenv_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/liblist_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/libnames_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/objbrowserInit")
    call delete(g:rplugin_tmpdir . "/Rdoc")
    call delete(g:rplugin_tmpdir . "/Rinsert")
    call delete(g:rplugin_tmpdir . "/tmux.conf")
    call delete(g:rplugin_tmpdir . "/unformatted_code")
    call delete(g:rplugin_tmpdir . "/vimbol_finished")
    call delete(g:rplugin_tmpdir . "/vimcom_running_" . $VIMINSTANCEID)
    call delete(g:rplugin_tmpdir . "/rconsole_hwnd_" . $VIMRPLUGIN_SECRET)
    call delete(g:rplugin_tmpdir . "/openR'")
endfunction

function SetRPath()
    if exists("g:vimrplugin_r_path")
        let b:rplugin_R = expand(g:vimrplugin_r_path)
        if isdirectory(b:rplugin_R)
            let b:rplugin_R = b:rplugin_R . "/R"
        endif
    else
        let b:rplugin_R = "R"
    endif
    if !executable(b:rplugin_R)
        call RWarningMsgInp("R executable not found: '" . b:rplugin_R . "'")
    endif
    if !exists("g:vimrplugin_r_args")
        let b:rplugin_r_args = " "
    else
        let b:rplugin_r_args = g:vimrplugin_r_args
    endif
endfunction

function RSourceOtherScripts()
    if exists("g:vimrplugin_source")
        let flist = split(g:vimrplugin_source, ",")
        for fl in flist
            if fl =~ " "
                call RWarningMsgInp("Invalid file name (empty spaces are not allowed): '" . fl . "'")
            else
                exe "source " . escape(fl, ' \')
            endif
        endfor
    endif
endfunction

command -nargs=1 -complete=customlist,RLisObjs Rinsert :call RInsert(<q-args>)
command -range=% Rformat <line1>,<line2>:call RFormatCode()
command RBuildTags :call g:SendCmdToR('rtags(ofile = "TAGS")')
command -nargs=? -complete=customlist,RLisObjs Rhelp :call RAskHelp(<q-args>)
command -nargs=? -complete=dir RSourceDir :call RSourceDirectory(<q-args>)
command RStop :call StopR()
command Rhistory :call ShowRhistory()


"==========================================================================
" Global variables
" Convention: vimrplugin_ for user options
"             rplugin_    for internal parameters
"==========================================================================

if !exists("g:rplugin_compldir")
    runtime r-plugin/setcompldir.vim
endif


if exists("g:vimrplugin_tmpdir")
    let g:rplugin_tmpdir = expand(g:vimrplugin_tmpdir)
else
    if has("win32") || has("win64")
        if isdirectory($TMP)
            let g:rplugin_tmpdir = $TMP . "/r-plugin-" . g:rplugin_userlogin
        elseif isdirectory($TEMP)
            let g:rplugin_tmpdir = $TEMP . "/r-plugin-" . g:rplugin_userlogin
        else
            let g:rplugin_tmpdir = g:rplugin_uservimfiles . "/r-plugin/tmp"
        endif
        let g:rplugin_tmpdir = substitute(g:rplugin_tmpdir, "\\", "/", "g")
    else
        if isdirectory($TMPDIR)
            if $TMPDIR =~ "/$"
                let g:rplugin_tmpdir = $TMPDIR . "r-plugin-" . g:rplugin_userlogin
            else
                let g:rplugin_tmpdir = $TMPDIR . "/r-plugin-" . g:rplugin_userlogin
            endif
        elseif isdirectory("/tmp")
            let g:rplugin_tmpdir = "/tmp/r-plugin-" . g:rplugin_userlogin
        else
            let g:rplugin_tmpdir = g:rplugin_uservimfiles . "/r-plugin/tmp"
        endif
    endif
endif

let $VIMRPLUGIN_TMPDIR = g:rplugin_tmpdir
if !isdirectory(g:rplugin_tmpdir)
    call mkdir(g:rplugin_tmpdir, "p", 0700)
endif

let g:rplugin_is_darwin = system("uname") =~ "Darwin"

" Variables whose default value is fixed
call RSetDefaultValue("g:vimrplugin_map_r",             0)
call RSetDefaultValue("g:vimrplugin_allnames",          0)
call RSetDefaultValue("g:vimrplugin_rmhidden",          0)
call RSetDefaultValue("g:vimrplugin_assign",            1)
call RSetDefaultValue("g:vimrplugin_assign_map",    "'_'")
call RSetDefaultValue("g:vimrplugin_rnowebchunk",       1)
call RSetDefaultValue("g:vimrplugin_strict_rst",        1)
call RSetDefaultValue("g:vimrplugin_openpdf",           2)
call RSetDefaultValue("g:vimrplugin_synctex",           1)
call RSetDefaultValue("g:vimrplugin_openhtml",          0)
call RSetDefaultValue("g:vimrplugin_Rterm",             0)
call RSetDefaultValue("g:vimrplugin_vim_wd",            0)
call RSetDefaultValue("g:vimrplugin_restart",           0)
call RSetDefaultValue("g:vimrplugin_vsplit",            0)
call RSetDefaultValue("g:vimrplugin_rconsole_width",   -1)
call RSetDefaultValue("g:vimrplugin_rconsole_height",  15)
call RSetDefaultValue("g:vimrplugin_tmux_title", "'VimR'")
call RSetDefaultValue("g:vimrplugin_listmethods",       0)
call RSetDefaultValue("g:vimrplugin_specialplot",       0)
call RSetDefaultValue("g:vimrplugin_notmuxconf",        0)
call RSetDefaultValue("g:vimrplugin_only_in_tmux",      0)
call RSetDefaultValue("g:vimrplugin_routnotab",         0)
call RSetDefaultValue("g:vimrplugin_editor_w",         66)
call RSetDefaultValue("g:vimrplugin_help_w",           46)
call RSetDefaultValue("g:vimrplugin_objbr_w",          40)
call RSetDefaultValue("g:vimrplugin_i386",              0)
call RSetDefaultValue("g:vimrplugin_vimcom_wait",    5000)
call RSetDefaultValue("g:vimrplugin_show_args",         0)
call RSetDefaultValue("g:vimrplugin_never_unmake_menu", 0)
call RSetDefaultValue("g:vimrplugin_insert_mode_cmds",  1)
call RSetDefaultValue("g:vimrplugin_indent_commented",  1)
call RSetDefaultValue("g:vimrplugin_source",         "''")
call RSetDefaultValue("g:vimrplugin_rcomment_string", "'# '")
call RSetDefaultValue("g:vimrplugin_vimpager",      "'tab'")
call RSetDefaultValue("g:vimrplugin_objbr_place",     "'script,right'")
call RSetDefaultValue("g:vimrplugin_user_maps_only", 0)
call RSetDefaultValue("g:vimrplugin_latexcmd", "'default'")
call RSetDefaultValue("g:vimrplugin_rmd_environment", "'.GlobalEnv'")

" The C code in VimCom/src/apps/vimr.c to send strings to RTerm is not working:
let g:vimrplugin_Rterm = 0

" Look for invalid options
let objbrplace = split(g:vimrplugin_objbr_place, ",")
let obpllen = len(objbrplace) - 1
if obpllen > 1
    call RWarningMsgInp("Too many options for vimrplugin_objbr_place.")
    let g:rplugin_failed = 1
    finish
endif
for idx in range(0, obpllen)
    if objbrplace[idx] != "console" && objbrplace[idx] != "script" && objbrplace[idx] != "left" && objbrplace[idx] != "right"
        call RWarningMsgInp('Invalid option for vimrplugin_objbr_place: "' . objbrplace[idx] . '". Valid options are: console or script and right or left."')
        let g:rplugin_failed = 1
        finish
    endif
endfor
unlet objbrplace
unlet obpllen

function RSetMyPort(p)
    let g:rplugin_myport = a:p
    if &filetype == "rbrowser"
        call SendToVimCom("\002" . a:p)
        call SendToVimCom("\005B Update OB [RSetMyPort]")
    endif
endfunction

function SendObjPortToVimCom(p)
    call SendToVimCom("\002" . a:p)
endfunction

function ROnJobActivity()
    if v:job_data[1] == 'stdout'
        for cmd in v:job_data[2]
            if cmd == ""
                continue
            endif
            if cmd =~ "^call " || cmd  =~ "^let "
                exe cmd
            else
                call RWarningMsg("[JobActivity] Unknown command: " . cmd)
            endif
        endfor
    elseif v:job_data[1] == 'stderr'
        call RWarningMsg('JobActivity error: ' . join(v:job_data[2]))
    endif
endfunction

function SendToVimCom(...)
    if g:rplugin_vimcomport == 0
        call RWarningMsg("VimCom port is unknown.")
        return
    endif
    let repl = libcall(g:rplugin_vimcom_lib, "SendToVimCom", g:rplugin_vimcomport . " " . a:1)
    if repl != "OK"
        call RWarningMsg(repl)
    endif
endfunction


" ^K (\013) cleans from cursor to the right and ^U (\025) cleans from cursor
" to the left. However, ^U causes a beep if there is nothing to clean. The
" solution is to use ^A (\001) to move the cursor to the beginning of the line
" before sending ^K. But the control characters may cause problems in some
" circumstances.
call RSetDefaultValue("g:vimrplugin_ca_ck", 0)

" ========================================================================
" Set default mean of communication with R

if has('gui_running')
    let g:rplugin_do_tmux_split = 0
endif

if g:rplugin_is_darwin
    let g:rplugin_r64app = 0
    if isdirectory("/Applications/R64.app")
        call RSetDefaultValue("g:vimrplugin_applescript", 1)
        let g:rplugin_r64app = 1
    elseif isdirectory("/Applications/R.app")
        call RSetDefaultValue("g:vimrplugin_applescript", 1)
    else
        call RSetDefaultValue("g:vimrplugin_applescript", 0)
    endif
    if !exists("g:macvim_skim_app_path")
        let g:macvim_skim_app_path = '/Applications/Skim.app'
    endif
else
    let g:vimrplugin_applescript = 0
endif

if has("gui_running") || g:vimrplugin_applescript
    let vimrplugin_only_in_tmux = 0
endif

if has("gui_running") || has("win32") || g:vimrplugin_applescript
    let g:vimrplugin_tmux_ob = 0
    if g:vimrplugin_objbr_place =~ "console"
        let g:vimrplugin_objbr_place = substitute(g:vimrplugin_objbr_place, "console", "script", "")
    endif
endif

if $TMUX == ""
    let g:rplugin_do_tmux_split = 0
    call RSetDefaultValue("g:vimrplugin_tmux_ob", 0)
else
    let g:rplugin_do_tmux_split = 1
    let g:vimrplugin_applescript = 0
    call RSetDefaultValue("g:vimrplugin_tmux_ob", 1)
endif
if g:vimrplugin_objbr_place =~ "console"
    let g:vimrplugin_tmux_ob = 1
endif


" ========================================================================

" Check whether Tmux is OK
if !has("win32") && !has("win64") && !has("gui_win32") && !has("gui_win64") && !g:vimrplugin_applescript
    if !executable('tmux') && g:vimrplugin_source !~ "screenR"
        call RWarningMsgInp("Please, install the 'Tmux' application to enable the Vim-R-plugin.")
        let g:rplugin_failed = 1
        finish
    endif

    let s:tmuxversion = system("tmux -V")
    let s:tmuxversion = substitute(s:tmuxversion, '.*tmux \([0-9]\.[0-9]\).*', '\1', '')
    if strlen(s:tmuxversion) != 3
        let s:tmuxversion = "1.0"
    endif
    if s:tmuxversion < "1.5" && g:vimrplugin_source !~ "screenR"
        call RWarningMsgInp("Vim-R-plugin requires Tmux >= 1.5")
        let g:rplugin_failed = 1
        finish
    endif
    unlet s:tmuxversion
endif

" Start with an empty list of objects in the workspace
let g:rplugin_globalenvlines = []

" Minimum width for the Object Browser
if g:vimrplugin_objbr_w < 10
    let g:vimrplugin_objbr_w = 10
endif

" Control the menu 'R' and the tool bar buttons
if !exists("g:rplugin_hasmenu")
    let g:rplugin_hasmenu = 0
endif

" List of marks that the plugin seeks to find the block to be sent to R
let s:all_marks = "abcdefghijklmnopqrstuvwxyz"


" Choose a terminal (code adapted from screen.vim)
if exists("g:vimrplugin_term")
    if !executable(g:vimrplugin_term)
        call RWarningMsgInp("'" . g:vimrplugin_term . "' not found. Please change the value of 'vimrplugin_term' in your vimrc.")
        let g:vimrplugin_term = "xterm"
    endif
endif
if has("win32") || has("win64") || g:rplugin_is_darwin || g:rplugin_do_tmux_split
    " No external terminal emulator will be called, so any value is good
    let g:vimrplugin_term = "xterm"
endif
if !exists("g:vimrplugin_term")
    let s:terminals = ['gnome-terminal', 'konsole', 'xfce4-terminal', 'terminal', 'Eterm',
                \ 'rxvt', 'urxvt', 'aterm', 'roxterm', 'terminator', 'lxterminal', 'xterm']
    for s:term in s:terminals
        if executable(s:term)
            let g:vimrplugin_term = s:term
            break
        endif
    endfor
    unlet s:term
    unlet s:terminals
endif

if !exists("g:vimrplugin_term") && !exists("g:vimrplugin_term_cmd")
    call RWarningMsgInp("Please, set the variable 'g:vimrplugin_term_cmd' in your .vimrc. Read the plugin documentation for details.")
    let g:rplugin_failed = 1
    finish
endif

let g:rplugin_termcmd = g:vimrplugin_term . " -e"

if g:vimrplugin_term == "gnome-terminal" || g:vimrplugin_term == "xfce4-terminal" || g:vimrplugin_term == "terminal" || g:vimrplugin_term == "lxterminal"
    " Cannot set gnome-terminal icon: http://bugzilla.gnome.org/show_bug.cgi?id=126081
    if g:vimrplugin_vim_wd
        let g:rplugin_termcmd = g:vimrplugin_term . " --title R -e"
    else
        let g:rplugin_termcmd = g:vimrplugin_term . " --working-directory='" . expand("%:p:h") . "' --title R -e"
    endif
endif

if g:vimrplugin_term == "terminator"
    if g:vimrplugin_vim_wd
        let g:rplugin_termcmd = "terminator --title R -x"
    else
        let g:rplugin_termcmd = "terminator --working-directory='" . expand("%:p:h") . "' --title R -x"
    endif
endif

if g:vimrplugin_term == "konsole"
    if g:vimrplugin_vim_wd
        let g:rplugin_termcmd = "konsole --icon " . g:rplugin_home . "/bitmaps/ricon.png -e"
    else
        let g:rplugin_termcmd = "konsole --workdir '" . expand("%:p:h") . "' --icon " . g:rplugin_home . "/bitmaps/ricon.png -e"
    endif
endif

if g:vimrplugin_term == "Eterm"
    let g:rplugin_termcmd = "Eterm --icon " . g:rplugin_home . "/bitmaps/ricon.png -e"
endif

if g:vimrplugin_term == "roxterm"
    " Cannot set icon: http://bugzilla.gnome.org/show_bug.cgi?id=126081
    if g:vimrplugin_vim_wd
        let g:rplugin_termcmd = "roxterm --title R -e"
    else
        let g:rplugin_termcmd = "roxterm --directory='" . expand("%:p:h") . "' --title R -e"
    endif
endif

if g:vimrplugin_term == "xterm" || g:vimrplugin_term == "uxterm"
    let g:rplugin_termcmd = g:vimrplugin_term . " -xrm '*iconPixmap: " . g:rplugin_home . "/bitmaps/ricon.xbm' -e"
endif

if g:vimrplugin_term == "rxvt" || g:vimrplugin_term == "urxvt"
    let g:rplugin_termcmd = g:vimrplugin_term . " -cd '" . expand("%:p:h") . "' -title R -xrm '*iconPixmap: " . g:rplugin_home . "/bitmaps/ricon.xbm' -e"
endif

" Override default settings:
if exists("g:vimrplugin_term_cmd")
    let g:rplugin_termcmd = g:vimrplugin_term_cmd
endif

if filewritable('/dev/null')
    let g:rplugin_null = "'/dev/null'"
elseif has("win32") && filewritable('NUL')
    let g:rplugin_null = "'NUL'"
else
    let g:rplugin_null = 'tempfile()'
endif

autocmd BufEnter * call RBufEnter()
if &filetype != "rbrowser"
    autocmd VimLeave * call RVimLeave()
endif

let g:rplugin_firstbuffer = expand("%:p")
let g:rplugin_running_objbr = 0
let g:rplugin_newliblist = 0
let g:rplugin_ob_warn_shown = 0
let g:rplugin_clt_job = 0
let g:rplugin_r_pid = 0
let g:rplugin_myport = 0
let g:rplugin_vimcomport = 0
let g:rplugin_vimcom_home = ""
let g:rplugin_vimcom_version = 0
let g:rplugin_lastev = ""
let g:rplugin_last_r_prompt = ""
let g:rplugin_hasRSFbutton = 0
let g:rplugin_tmuxsname = "VimR-" . substitute(localtime(), '.*\(...\)', '\1', '')

" SyncTeX options
let g:rplugin_has_wmctrl = 0
let g:rplugin_synctexpid = 0
let g:rplugin_zathura_pid = {}

let g:rplugin_py_exec = "none"
if executable("python3")
    let g:rplugin_py_exec = "python3"
elseif executable("python")
    let g:rplugin_py_exec = "python"
endif

function GetRandomNumber(width)
    if g:rplugin_py_exec != "none"
        let pycode = ["import os, sys, base64",
                    \ "sys.stdout.write(base64.b64encode(os.urandom(" . a:width . ")).decode())" ]
        call writefile(pycode, g:rplugin_tmpdir . "/getRandomNumber.py")
        let randnum = system(g:rplugin_py_exec . ' "' . g:rplugin_tmpdir . '/getRandomNumber.py"')
        call delete(g:rplugin_tmpdir . "/getRandomNumber.py")
    elseif !has("win32") && !has("win64") && !has("gui_win32") && !has("gui_win64")
        let randnum = system("echo $RANDOM")
    else
        let randnum = localtime()
    endif
    return substitute(randnum, '\W', '', 'g')
endfunction

" If this is the Object Browser running in a Tmux pane, $VIMINSTANCEID is
" already defined and shouldn't be changed
if &filetype == "rbrowser"
    if $VIMINSTANCEID == ""
        call RWarningMsgInp("VIMINSTANCEID is undefined")
    endif
else
    let $VIMRPLUGIN_SECRET = GetRandomNumber(16)
    let $VIMINSTANCEID = substitute(g:rplugin_firstbuffer . GetRandomNumber(16), '\W', '', 'g')
    if strlen($VIMINSTANCEID) > 64
        let $VIMINSTANCEID = substitute($VIMINSTANCEID, '.*\(...............................................................\)', '\1', '')
    endif
endif

let g:rplugin_obsname = toupper(substitute(substitute(expand("%:r"), '\W', '', 'g'), "_", "", "g"))

let g:rplugin_docfile = g:rplugin_tmpdir . "/Rdoc"

" Create an empty file to avoid errors if the user do Ctrl-X Ctrl-O before
" starting R:
if &filetype != "rbrowser"
    call writefile([], g:rplugin_tmpdir . "/GlobalEnvList_" . $VIMINSTANCEID)
endif

if has("win32") || has("win64")
    runtime r-plugin/windows.vim
    let g:rplugin_has_icons = len(globpath(&rtp, "bitmaps/RStart.bmp")) > 0
else
    call SetRPath()
    let g:rplugin_has_icons = len(globpath(&rtp, "bitmaps/RStart.png")) > 0
endif
if has("gui_running")
    runtime r-plugin/gui_running.vim
endif
if g:vimrplugin_applescript
    runtime r-plugin/osx.vim
endif

if exists("g:vimrplugin_permanent_libs")
    call RWarningMsgInp("The option 'vimrplugin_permanent_libs' was renamed to 'vimrplugin_start_libs'. Please, rename it in your vimrc too.")
endif

if exists("g:vimrplugin_routmorecolors")
    call RWarningMsgInp("The option 'vimrplugin_routmorecolors' was renamed to 'Rout_more_colors'. Please, rename it in your vimrc too.")
endif
r-plugin/global_r_plugin.vim	[[[1
86

runtime r-plugin/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Default IsInRCode function when the plugin is used as a global plugin
function! DefaultIsInRCode(vrb)
    return 1
endfunction

let b:IsInRCode = function("DefaultIsInRCode")

call RCreateStartMaps()
call RCreateEditMaps()
call RCreateSendMaps()
call RControlMaps()

" Menu R
if g:vimrplugin_never_unmake_menu && has("gui_running")
    call MakeRMenu()
endif

call RSourceOtherScripts()

if exists("g:rplugin_did_sourcelines")
    finish
endif
let g:rplugin_did_sourcelines = 1

function SourceNotDefined(lines, e)
    echohl WarningMsg
    echo 'The function to source "' . &filetype . '" lines is not defined.'
    echohl Normal
endfunction

function JuliaSourceLines(lines, e)
    call writefile(a:lines, b:rsource)
    let jcmd = 'include("' . b:rsource . '")'
    let ok = g:SendCmdToR(jcmd)
    return ok
endfunction

function SetExeCmd()
    runtime r-plugin/common_buffer.vim
    if exists("g:vimrplugin_exe") && exists("g:vimrplugin_quit")
        let b:rplugin_R = g:vimrplugin_exe
        if exists("g:vimrplugin_args")
            let b:rplugin_r_args = g:vimrplugin_args
        else
            let b:rplugin_r_args = " "
        endif
        let b:quit_command = g:vimrplugin_quit
        let b:SourceLines = function("SourceNotDefined")
    elseif &filetype == "julia"
        let b:rplugin_R = "julia"
        let b:rplugin_r_args = " "
        let b:quit_command = "quit()"
        let b:SourceLines = function("JuliaSourceLines")
        call RCreateMaps("ni", '<Plug>RSendFile',     'aa', ':call JuliaSourceLines(getline(1, "$"), "silent")')
    elseif &filetype == "python"
        let b:rplugin_R = "python"
        let b:rplugin_r_args = " "
        let b:quit_command = "quit()"
        let b:SourceLines = function("SourceNotDefined")
    elseif &filetype == "haskell"
        let b:rplugin_R = "ghci"
        let b:rplugin_r_args = " "
        let b:quit_command = ":quit"
        let b:SourceLines = function("SourceNotDefined")
    elseif &filetype == "ruby"
        let b:rplugin_R = "irb"
        let b:rplugin_r_args = " "
        let b:quit_command = "quit"
        let b:SourceLines = function("SourceNotDefined")
    elseif &filetype == "lisp"
        let b:rplugin_R = "clisp"
        let b:rplugin_r_args = " "
        let b:quit_command = "(quit)"
        let b:SourceLines = function("SourceNotDefined")
    endif
endfunction

autocmd FileType * call SetExeCmd()
call SetExeCmd()

r-plugin/r.snippets	[[[1
33
# library()
snippet li
	library(${1:})
# If Condition
snippet if
	if(${1:condition}){
	    ${2:}
	}
snippet el
	else {
	    ${1:}
	}
snippet wh
	while(${1:condition}){
	    ${2:}
	}
# For Loop
snippet for
	for(${1:i} in ${2:range}){
	    ${3:}
	}
# Function
snippet fun
	${1:funname} <- function(${2:})
	{
	    ${3:}
	}
# repeat
snippet re
	repeat{
	    ${2:}
	    if(${1:condition}) break
	}
r-plugin/rmd.snippets	[[[1
205
#
# Snipmate Snippets for Pandoc Markdown
#
# Many snippets have starred versions, i.e., versions
# that end with an asterisk (`*`). These snippets use
# vim's `"*` register---i.e., the contents of the 
# system clipboard---to insert text.

# Insert Title Block
snippet %%
	% ${1:`Filename('', 'title')`}
	% ${2:`g:snips_author`}
	% ${3:`strftime("%d %B %Y")`}

	${4}
snippet %%*
	% ${1:`Filename('', @*)`}
	% ${2:`g:snips_author`}
	% ${3:`strftime("%d %b %Y")`}

	${4}

# Insert Definition List
snippet ::
	${1:term}
	  ~  ${2:definition}

# Underline with `=`s or `-`s
snippet ===
	`repeat('=', strlen(getline(line(".") - 1)))`
	
	${1}
snippet ---
	`repeat('-', strlen(getline(line(".") - 1)))`
	
	${1}

# Links and their kin
# -------------------
#
# (These don't play very well with delimitMate)
#

snippet [
	[${1:link}](http://${2:url} "${3:title}")${4}
snippet [*
	[${1:link}](${2:`@*`} "${3:title}")${4}

snippet [:
	[${1:id}]: http://${2:url} "${3:title}"
snippet [:*
	[${1:id}]: ${2:`@*`} "${3:title}"

snippet [@
	[${1:link}](mailto:${2:email})${3}
snippet [@*
	[${1:link}](mailto:${2:`@*`})${3}

snippet [:@
	[${1:id}]: mailto:${2:email} "${3:title}"
snippet [:@*
	[${1:id}]: mailto:${2:`@*`} "${3:title}"

snippet ![
	![${1:alt}](${2:url} "${3:title}")${4}
snippet ![*
	![${1:alt}](${2:`@*`} "${3:title}")${4}

snippet ![:
	![${1:id}]: ${2:url} "${3:title}"
snippet ![:*
	![${1:id}]: ${2:`@*`} "${3:title}"

snippet [^:
	[^${1:id}]: ${2:note}
snippet [^:*
	[^${1:id}]: ${2:`@*`}

# 

# library()
snippet req
	require(${1:}, quietly = TRUE)
# If Condition
snippet if
	if ( ${1:condition} ) 
	{ 
		${2:} 
	}
snippet el
	else 
	{ 
		${1:} 
	}

# Function
snippet fun
	${1:funname} <- 			# ${2:}
		function
	(
	 	${3:}
	) 
	{
	  ${4:}
	}
# repeat
snippet re
	repeat{
	  ${2:}
	  if(${1:condition}) break
	}

# matrix
snippet ma
	matrix(NA, nrow = ${1:}, ncol = ${2:})

# data frame
snippet df
	data.frame(${1:}, header = TRUE)

snippet cmdarg
	args <- commandArgs(TRUE)
	if (length(args) == 0)
	    stop("Please give ${1:}!")
	if (!all(file.exists(args)))
	     stop("Couln't find input files!") 

snippet getopt
	require('getopt', quietly = TRUE)
	opt_spec <- matrix(c(
					'help',     'h', 0, "logical", 	"Getting help",
					'file',     'f', 1, "character","File to process" 
	                ), ncol = 5, byrow = TRUE)
	opt <- getopt(spec = opt_spec)
	if ( !is.null(opt$help) || is.null(commandArgs()) )   {    
	    cat(getopt(spec = opt_spec, usage = TRUE, command = "yourCmd"))
	    q(status=0)
	}
	# some inital value
	if ( is.null(opt$???) )    { opt$??? <- ??? }

snippet optparse
	require("optparse", quietly = TRUE)
	option_list <- 
	    list(make_option(c("-n", "--add_numbers"), action="store_true", default=FALSE,
	                     help="Print line number at the beginning of each line [default]")
	         )
	parser <- OptionParser(usage = "%prog [options] file", option_list=option_list)
	arguments <- parse_args(parser, positional_arguments = TRUE)
	opt <- arguments$options
	
	if(length(arguments$args) != 1) {
	    cat("Incorrect number of required positional arguments\n\n")
	    print_help(parser)
	    stop()
	} else {
	    file <- arguments$args
	}
	
	if( file.access(file) == -1) {
	    stop(sprintf("Specified file ( %s ) does not exist", file))
	} else {
	    file_text <- readLines(file)
	}

snippet #!
	#!/usr/bin/env Rscript

snippet debug
	# Development & Debugging, don't forget to uncomment afterwards!
	#--------------------------------------------------------------------------------
	#setwd("~/Projekte/${1:}")
	#opt <- list(${2:}
	#            )
	#--------------------------------------------------------------------------------


# Took from pandoc-plugin <<<<
# Underline with `=`s or `-`s
snippet #===
	#`repeat('=', strlen(getline(line(".") - 1)))`
	${1}
snippet #---
	#`repeat('-', strlen(getline(line(".") - 1)))`
	${1}

# >>>>

snippet r
	\`\`\`{r ${1:chung_tag}, echo = FALSE ${2:options}}
	${3:}
	\`\`\`
snippet ri
	\`{r ${1:}}\`

snippet copt
	\`\`\` {r setup, echo = FALSE}
		opts_chunk$set(fig.path='../figures/${1:}', cache.path='../cache/-'
		, fig.align='center', fig.show='hold', par=TRUE)	
		#opts_knit$set(upload.fun = imgur_upload) # upload images
	\`\`\`

	
# End of File ===================================================================
# vim: set noexpandtab:
r-plugin/synctex_evince_backward.py	[[[1
137

# The code in this files is borrowed from Gedit Synctex plugin.
#
# Copyright (C) 2010 Jose Aliste
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public Licence as published by the Free Software
# Foundation; either version 2 of the Licence, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public Licence for more
# details.
#
# You should have received a copy of the GNU General Public Licence along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA  02110-1301, USA

# Modified to Vim-R-plugin by Jakson Aquino

import dbus, subprocess, time
import dbus.mainloop.glib, sys, os, signal
from gi.repository import GObject

RUNNING, CLOSED = range(2)

EV_DAEMON_PATH = "/org/gnome/evince/Daemon"
EV_DAEMON_NAME = "org.gnome.evince.Daemon"
EV_DAEMON_IFACE = "org.gnome.evince.Daemon"

EVINCE_PATH = "/org/gnome/evince/Evince"
EVINCE_IFACE = "org.gnome.evince.Application"

EV_WINDOW_IFACE = "org.gnome.evince.Window"

class EvinceWindowProxy:
    """A DBUS proxy for an Evince Window."""
    daemon = None
    bus = None

    def __init__(self, uri, spawn = False):
        self.uri = uri
        self.spawn = spawn
        self.status = CLOSED
        self.dbus_name = ''
        self._handler = None
        try:
            if EvinceWindowProxy.bus is None:
                EvinceWindowProxy.bus = dbus.SessionBus()

            if EvinceWindowProxy.daemon is None:
                EvinceWindowProxy.daemon = EvinceWindowProxy.bus.get_object(EV_DAEMON_NAME,
                                                EV_DAEMON_PATH,
                                                follow_name_owner_changes=True)
            EvinceWindowProxy.bus.add_signal_receiver(self._on_doc_loaded, signal_name="DocumentLoaded", 
                                                      dbus_interface = EV_WINDOW_IFACE, 
                                                      sender_keyword='sender')
            self._get_dbus_name(False)

        except dbus.DBusException:
            sys.stderr.write("Could not connect to the Evince Daemon")
            sys.stderr.flush()
            loop.quit()

    def _on_doc_loaded(self, uri, **keyargs):
        if uri == self.uri and self._handler is None:
            self.handle_find_document_reply(keyargs['sender'])
        
    def _get_dbus_name(self, spawn):
        EvinceWindowProxy.daemon.FindDocument(self.uri,spawn,
                     reply_handler=self.handle_find_document_reply,
                     error_handler=self.handle_find_document_error,
                     dbus_interface = EV_DAEMON_IFACE)

    def handle_find_document_error(self, error):
        sys.stderr.write("FindDocument DBus call has failed")
        sys.stderr.flush()

    def handle_find_document_reply(self, evince_name):
        if self._handler is not None:
            handler = self._handler
        else:
            handler = self.handle_get_window_list_reply
        if evince_name != '':
            self.dbus_name = evince_name
            self.status = RUNNING
            self.evince = EvinceWindowProxy.bus.get_object(self.dbus_name, EVINCE_PATH)
            self.evince.GetWindowList(dbus_interface = EVINCE_IFACE,
                          reply_handler = handler,
                          error_handler = self.handle_get_window_list_error)

    def handle_get_window_list_error (self, e):
        sys.stderr.write("GetWindowList DBus call has failed")
        sys.stderr.flush()

    def handle_get_window_list_reply (self, window_list):
        if len(window_list) > 0:
            window_obj = EvinceWindowProxy.bus.get_object(self.dbus_name, window_list[0])
            self.window = dbus.Interface(window_obj,EV_WINDOW_IFACE)
            self.window.connect_to_signal("Closed", self.on_window_close)
            self.window.connect_to_signal("SyncSource", self.on_sync_source)
        else:
            #That should never happen. 
            sys.stderr.write("GetWindowList returned empty list")
            sys.stderr.flush()

    def on_window_close(self):
        self.window = None
        self.status = CLOSED

    def on_sync_source(self, input_file, source_link, timestamp):
        if vimnm == "nvim":
            sys.stdout.write("call SyncTeX_backward('" + input_file + "', " + str(source_link[0]) + ")\n")
            sys.stdout.flush()
        else:
            os.system(vimexec + ' --servername ' + vimnm + ' --remote-expr "' + "SyncTeX_backward('" + input_file + "', " + str(source_link[0]) + ')"')

path_output = os.getcwd() + '/' + sys.argv[1]

vimnm = sys.argv[2]
if vimnm != "nvim":
    if vimnm.find("GVIM") == 0:
        vimexec = "gvim"
    else:
        vimexec = "vim"
    time.sleep(1)
    os.system(vimexec + ' --servername ' + vimnm + ' --remote-expr "SyncTeX_SetPID(' + str(os.getpid()) + ')"')


dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

a = EvinceWindowProxy('file://' + path_output, True)

loop = GObject.MainLoop()
loop.run() 

r-plugin/synctex_evince_forward.py	[[[1
143

# The code in this files is borrowed from Gedit Synctex plugin.
#
# Copyright (C) 2010 Jose Aliste
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public Licence as published by the Free Software
# Foundation; either version 2 of the Licence, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public Licence for more
# details.
#
# You should have received a copy of the GNU General Public Licence along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA  02110-1301, USA

# Modified to Vim-R-plugin by Jakson Aquino

import dbus, subprocess, time
import dbus.mainloop.glib, sys, os
from gi.repository import GObject

RUNNING, CLOSED = range(2)

EV_DAEMON_PATH = "/org/gnome/evince/Daemon"
EV_DAEMON_NAME = "org.gnome.evince.Daemon"
EV_DAEMON_IFACE = "org.gnome.evince.Daemon"

EVINCE_PATH = "/org/gnome/evince/Evince"
EVINCE_IFACE = "org.gnome.evince.Application"

EV_WINDOW_IFACE = "org.gnome.evince.Window"



class EvinceWindowProxy:
    """A DBUS proxy for an Evince Window."""
    daemon = None
    bus = None

    def __init__(self, uri, spawn = False):
        self.uri = uri
        self.spawn = spawn
        self.status = CLOSED
        self.source_handler = None
        self.dbus_name = ''
        self._handler = None
        try:
            if EvinceWindowProxy.bus is None:
                EvinceWindowProxy.bus = dbus.SessionBus()

            if EvinceWindowProxy.daemon is None:
                EvinceWindowProxy.daemon = EvinceWindowProxy.bus.get_object(EV_DAEMON_NAME,
                                                EV_DAEMON_PATH,
                                                follow_name_owner_changes=True)
            EvinceWindowProxy.bus.add_signal_receiver(self._on_doc_loaded, signal_name="DocumentLoaded", 
                                                      dbus_interface = EV_WINDOW_IFACE, 
                                                      sender_keyword='sender')
            self._get_dbus_name(False)

        except dbus.DBusException:
            sys.stderr.write("Could not connect to the Evince Daemon")
            sys.stderr.flush()

    def _on_doc_loaded(self, uri, **keyargs):
        if uri == self.uri and self._handler is None:
            self.handle_find_document_reply(keyargs['sender'])
        
    def _get_dbus_name(self, spawn):
        EvinceWindowProxy.daemon.FindDocument(self.uri,spawn,
                     reply_handler=self.handle_find_document_reply,
                     error_handler=self.handle_find_document_error,
                     dbus_interface = EV_DAEMON_IFACE)

    def handle_find_document_error(self, error):
        sys.stderr.write("FindDocument DBus call has failed")
        sys.stderr.flush()

    def handle_find_document_reply(self, evince_name):
        if self._handler is not None:
            handler = self._handler
        else:
            handler = self.handle_get_window_list_reply
        if evince_name != '':
            self.dbus_name = evince_name
            self.status = RUNNING
            self.evince = EvinceWindowProxy.bus.get_object(self.dbus_name, EVINCE_PATH)
            self.evince.GetWindowList(dbus_interface = EVINCE_IFACE,
                          reply_handler = handler,
                          error_handler = self.handle_get_window_list_error)

    def handle_get_window_list_error (self, e):
        sys.stderr.write("GetWindowList DBus call has failed")
        sys.stderr.flush()

    def handle_get_window_list_reply (self, window_list):
        if len(window_list) > 0:
            window_obj = EvinceWindowProxy.bus.get_object(self.dbus_name, window_list[0])
            self.window = dbus.Interface(window_obj,EV_WINDOW_IFACE)
        else:
            #That should never happen. 
            sys.stderr.write("GetWindowList returned empty list")
            sys.stderr.flush()


    def SyncView(self, input_file, data, time):
        if self.status == CLOSED:
            if self.spawn:
                self._tmp_syncview = [input_file, data, time];
                self._handler = self._syncview_handler
                self._get_dbus_name(True)
        else:
            self.window.SyncView(input_file, data, time,  dbus_interface = "org.gnome.evince.Window")

    def _syncview_handler(self, window_list):
        self.handle_get_window_list_reply(window_list)

        if self.status == CLOSED: 
            return False
        self.window.SyncView(self._tmp_syncview[0],self._tmp_syncview[1], self._tmp_syncview[2], dbus_interface="org.gnome.evince.Window")
        del self._tmp_syncview
        self._handler = None
        return True

path_output  = os.getcwd() + '/' + sys.argv[1]
line_number = int(sys.argv[2])
path_input   = os.getcwd() + '/' + sys.argv[3]

dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

a = EvinceWindowProxy('file://' + path_output, True)

def sync_view(ev_window, path_input, line_number):
    ev_window.SyncView(path_input, (line_number, 1), 0)
    loop.quit()

GObject.timeout_add(400, sync_view, a, path_input, line_number)
loop = GObject.MainLoop()
loop.run() 

r-plugin/synctex_okular_backward.sh	[[[1
6
#!/bin/sh

# This script is required to do backward search from Okular to Neovim

echo "call SyncTeX_backward('$1', $2)" >> "$VIMRPLUGIN_TMPDIR/okular_search"

r-plugin/functions.vim	[[[1
176

if has("nvim")
    finish
endif

" Only source this once
if exists("*RmFromRLibList")
    if len(g:rplugin_lists_to_load) > 0
        for s:lib in g:rplugin_lists_to_load
            call SourceRFunList(s:lib)
        endfor
        unlet s:lib
    endif
    finish
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set global variables when this script is called for the first time
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Users may define the value of g:vimrplugin_start_libs
if !exists("g:vimrplugin_start_libs")
    let g:vimrplugin_start_libs = "base,stats,graphics,grDevices,utils,methods"
endif

let g:rplugin_lists_to_load = split(g:vimrplugin_start_libs, ",")
let g:rplugin_debug_lists = []
let g:rplugin_loaded_lists = []
let g:rplugin_Rhelp_list = []
let g:rplugin_omni_lines = []
let g:rplugin_new_libs = 0

" syntax/r.vim may have being called before ftplugin/r.vim
if !exists("g:rplugin_compldir")
    runtime r-plugin/setcompldir.vim
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Function for highlighting rFunction
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Must be run for each buffer
function SourceRFunList(lib)
    if isdirectory(g:rplugin_compldir)
        let fnf = split(globpath(g:rplugin_compldir, 'fun_' . a:lib . '_*'), "\n")
        if len(fnf) == 1
            " Highlight R functions
            exe "source " . substitute(fnf[0], ' ', '\\ ', 'g')
        elseif len(fnf) == 0
            let g:rplugin_debug_lists += ['Function list for "' . a:lib . '" not found.']
        elseif len(fnf) > 1
            let g:rplugin_debug_lists += ['There is more than one function list for "' . a:lib . '".']
            for obl in fnf
                let g:rplugin_debug_lists += [obl]
            endfor
        endif
    endif
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Omnicompletion functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function RLisObjs(arglead, cmdline, curpos)
    let lob = []
    let rkeyword = '^' . a:arglead
    for xx in g:rplugin_Rhelp_list
        if xx =~ rkeyword
            call add(lob, xx)
        endif
    endfor
    return lob
endfunction

function RmFromRLibList(lib)
    for idx in range(len(g:rplugin_loaded_lists))
        if g:rplugin_loaded_lists[idx] == a:lib
            call remove(g:rplugin_loaded_lists, idx)
            break
        endif
    endfor
    for idx in range(len(g:rplugin_lists_to_load))
        if g:rplugin_lists_to_load[idx] == a:lib
            call remove(g:rplugin_lists_to_load, idx)
            break
        endif
    endfor
endfunction

function AddToRLibList(lib)
    if isdirectory(g:rplugin_compldir)
        let omf = split(globpath(g:rplugin_compldir, 'omnils_' . a:lib . '_*'), "\n")
        if len(omf) == 1
            let g:rplugin_loaded_lists += [a:lib]

            " List of objects for omni completion
            let olist = readfile(omf[0])
            let g:rplugin_omni_lines += olist

            " List of objects for :Rhelp completion
            for xx in olist
                let xxx = split(xx, "\x06")
                if len(xxx) > 0 && xxx[0] !~ '\$'
                    call add(g:rplugin_Rhelp_list, xxx[0])
                endif
            endfor
        elseif len(omf) == 0
            let g:rplugin_debug_lists += ['Omnils list for "' . a:lib . '" not found.']
            call RmFromRLibList(a:lib)
            return
        elseif len(omf) > 1
            let g:rplugin_debug_lists += ['There is more than one omnils and function list for "' . a:lib . '".']
            for obl in omf
                let g:rplugin_debug_lists += [obl]
            endfor
            call RmFromRLibList(a:lib)
            return
        endif
    endif
endfunction


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Function called by vimcom
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function FillRLibList()
    " Update the list of objects for omnicompletion
    if filereadable(g:rplugin_tmpdir . "/libnames_" . $VIMINSTANCEID)
        let g:rplugin_lists_to_load = readfile(g:rplugin_tmpdir . "/libnames_" . $VIMINSTANCEID)
        for lib in g:rplugin_lists_to_load
            let isloaded = 0
            for olib in g:rplugin_loaded_lists
                if lib == olib
                    let isloaded = 1
                    break
                endif
            endfor
            if isloaded == 0
                call AddToRLibList(lib)
            endif
        endfor
    endif
    " Now we need to update the syntax in all R files. There should be a
    " better solution than setting a flag to let other buffers know that they
    " also need to update the syntax on CursorMoved event:
    " https://github.com/neovim/neovim/issues/901
    let g:rplugin_new_libs = len(g:rplugin_loaded_lists)
    silent exe 'set filetype=' . &filetype
    let b:rplugin_new_libs = g:rplugin_new_libs
endfunction


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Update the buffer syntax if necessary
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function RCheckLibList()
    if b:rplugin_new_libs == g:rplugin_new_libs
        return
    endif
    silent exe 'set filetype=' . &filetype
    let b:rplugin_new_libs = g:rplugin_new_libs
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Source the Syntax scripts for the first time and Load omnilists
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

for s:lib in g:rplugin_lists_to_load
    call SourceRFunList(s:lib)
    call AddToRLibList(s:lib)
endfor

unlet s:lib
r-plugin/gui_running.vim	[[[1
302
" This file contains code used only if has("gui_running")

if exists("g:did_r_plugin_gui_running")
    finish
endif
let g:did_r_plugin_gui_running = 1

function MakeRMenu()
    if g:rplugin_hasmenu == 1
        return
    endif

    " Do not translate "File":
    menutranslate clear

    "----------------------------------------------------------------------------
    " Start/Close
    "----------------------------------------------------------------------------
    call RCreateMenuItem("nvi", 'Start/Close.Start\ R\ (default)', '<Plug>RStart', 'rf', ':call StartR("R")')
    call RCreateMenuItem("nvi", 'Start/Close.Start\ R\ --vanilla', '<Plug>RVanillaStart', 'rv', ':call StartR("vanilla")')
    call RCreateMenuItem("nvi", 'Start/Close.Start\ R\ (custom)', '<Plug>RCustomStart', 'rc', ':call StartR("custom")')
    "-------------------------------
    menu R.Start/Close.-Sep1- <nul>
    call RCreateMenuItem("nvi", 'Start/Close.Close\ R\ (no\ save)', '<Plug>RClose', 'rq', ":call RQuit('no')")
    menu R.Start/Close.-Sep2- <nul>

    nmenu <silent> R.Start/Close.Stop\ R<Tab>:RStop :RStop<CR>

    "----------------------------------------------------------------------------
    " Send
    "----------------------------------------------------------------------------
    if &filetype == "r" || g:vimrplugin_never_unmake_menu
        call RCreateMenuItem("ni", 'Send.File', '<Plug>RSendFile', 'aa', ':call SendFileToR("silent")')
        call RCreateMenuItem("ni", 'Send.File\ (echo)', '<Plug>RESendFile', 'ae', ':call SendFileToR("echo")')
        call RCreateMenuItem("ni", 'Send.File\ (open\ \.Rout)', '<Plug>RShowRout', 'ao', ':call ShowRout()')
    endif
    "-------------------------------
    menu R.Send.-Sep1- <nul>
    call RCreateMenuItem("ni", 'Send.Block\ (cur)', '<Plug>RSendMBlock', 'bb', ':call SendMBlockToR("silent", "stay")')
    call RCreateMenuItem("ni", 'Send.Block\ (cur,\ echo)', '<Plug>RESendMBlock', 'be', ':call SendMBlockToR("echo", "stay")')
    call RCreateMenuItem("ni", 'Send.Block\ (cur,\ down)', '<Plug>RDSendMBlock', 'bd', ':call SendMBlockToR("silent", "down")')
    call RCreateMenuItem("ni", 'Send.Block\ (cur,\ echo\ and\ down)', '<Plug>REDSendMBlock', 'ba', ':call SendMBlockToR("echo", "down")')
    "-------------------------------
    if &filetype == "rnoweb" || &filetype == "rmd" || &filetype == "rrst" || g:vimrplugin_never_unmake_menu
        menu R.Send.-Sep2- <nul>
        call RCreateMenuItem("ni", 'Send.Chunk\ (cur)', '<Plug>RSendChunk', 'cc', ':call b:SendChunkToR("silent", "stay")')
        call RCreateMenuItem("ni", 'Send.Chunk\ (cur,\ echo)', '<Plug>RESendChunk', 'ce', ':call b:SendChunkToR("echo", "stay")')
        call RCreateMenuItem("ni", 'Send.Chunk\ (cur,\ down)', '<Plug>RDSendChunk', 'cd', ':call b:SendChunkToR("silent", "down")')
        call RCreateMenuItem("ni", 'Send.Chunk\ (cur,\ echo\ and\ down)', '<Plug>REDSendChunk', 'ca', ':call b:SendChunkToR("echo", "down")')
        call RCreateMenuItem("ni", 'Send.Chunk\ (from\ first\ to\ here)', '<Plug>RSendChunkFH', 'ch', ':call SendFHChunkToR()')
    endif
    "-------------------------------
    menu R.Send.-Sep3- <nul>
    call RCreateMenuItem("ni", 'Send.Function\ (cur)', '<Plug>RSendFunction', 'ff', ':call SendFunctionToR("silent", "stay")')
    call RCreateMenuItem("ni", 'Send.Function\ (cur,\ echo)', '<Plug>RESendFunction', 'fe', ':call SendFunctionToR("echo", "stay")')
    call RCreateMenuItem("ni", 'Send.Function\ (cur\ and\ down)', '<Plug>RDSendFunction', 'fd', ':call SendFunctionToR("silent", "down")')
    call RCreateMenuItem("ni", 'Send.Function\ (cur,\ echo\ and\ down)', '<Plug>REDSendFunction', 'fa', ':call SendFunctionToR("echo", "down")')
    "-------------------------------
    menu R.Send.-Sep4- <nul>
    call RCreateMenuItem("v", 'Send.Selection', '<Plug>RSendSelection', 'ss', ':call SendSelectionToR("silent", "stay")')
    call RCreateMenuItem("v", 'Send.Selection\ (echo)', '<Plug>RESendSelection', 'se', ':call SendSelectionToR("echo", "stay")')
    call RCreateMenuItem("v", 'Send.Selection\ (and\ down)', '<Plug>RDSendSelection', 'sd', ':call SendSelectionToR("silent", "down")')
    call RCreateMenuItem("v", 'Send.Selection\ (echo\ and\ down)', '<Plug>REDSendSelection', 'sa', ':call SendSelectionToR("echo", "down")')
    "-------------------------------
    menu R.Send.-Sep5- <nul>
    call RCreateMenuItem("ni", 'Send.Paragraph', '<Plug>RSendParagraph', 'pp', ':call SendParagraphToR("silent", "stay")')
    call RCreateMenuItem("ni", 'Send.Paragraph\ (echo)', '<Plug>RESendParagraph', 'pe', ':call SendParagraphToR("echo", "stay")')
    call RCreateMenuItem("ni", 'Send.Paragraph\ (and\ down)', '<Plug>RDSendParagraph', 'pd', ':call SendParagraphToR("silent", "down")')
    call RCreateMenuItem("ni", 'Send.Paragraph\ (echo\ and\ down)', '<Plug>REDSendParagraph', 'pa', ':call SendParagraphToR("echo", "down")')
    "-------------------------------
    menu R.Send.-Sep6- <nul>
    call RCreateMenuItem("ni0", 'Send.Line', '<Plug>RSendLine', 'l', ':call SendLineToR("stay")')
    call RCreateMenuItem("ni0", 'Send.Line\ (and\ down)', '<Plug>RDSendLine', 'd', ':call SendLineToR("down")')
    call RCreateMenuItem("ni0", 'Send.Line\ (and\ insert\ output)', '<Plug>RDSendLineAndInsertOutput', 'o', ':call SendLineToRAndInsertOutput()')
    call RCreateMenuItem("i", 'Send.Line\ (and\ new\ one)', '<Plug>RSendLAndOpenNewOne', 'q', ':call SendLineToR("newline")')
    call RCreateMenuItem("n", 'Send.Left\ part\ of\ line\ (cur)', '<Plug>RNLeftPart', 'r<Left>', ':call RSendPartOfLine("left", 0)')
    call RCreateMenuItem("n", 'Send.Right\ part\ of\ line\ (cur)', '<Plug>RNRightPart', 'r<Right>', ':call RSendPartOfLine("right", 0)')
    call RCreateMenuItem("i", 'Send.Left\ part\ of\ line\ (cur)', '<Plug>RILeftPart', 'r<Left>', 'l:call RSendPartOfLine("left", 1)')
    call RCreateMenuItem("i", 'Send.Right\ part\ of\ line\ (cur)', '<Plug>RIRightPart', 'r<Right>', 'l:call RSendPartOfLine("right", 1)')

    "----------------------------------------------------------------------------
    " Control
    "----------------------------------------------------------------------------
    call RControlMenu()
    "-------------------------------
    menu R.Command.-Sep4- <nul>
    if &filetype != "rdoc"
        call RCreateMenuItem("nvi", 'Command.Set\ working\ directory\ (cur\ file\ path)', '<Plug>RSetwd', 'rd', ':call RSetWD()')
    endif
    "-------------------------------
    if &filetype == "rnoweb" || &filetype == "rmd" || &filetype == "rrst" || g:vimrplugin_never_unmake_menu
        if &filetype == "rnoweb" || g:vimrplugin_never_unmake_menu
            menu R.Command.-Sep5- <nul>
            call RCreateMenuItem("nvi", 'Command.Sweave\ (cur\ file)', '<Plug>RSweave', 'sw', ':call RSweave()')
            call RCreateMenuItem("nvi", 'Command.Sweave\ and\ PDF\ (cur\ file)', '<Plug>RMakePDF', 'sp', ':call RMakePDF("nobib", 0)')
            call RCreateMenuItem("nvi", 'Command.Sweave,\ BibTeX\ and\ PDF\ (cur\ file)', '<Plug>RBibTeX', 'sb', ':call RMakePDF("bibtex", 0)')
        endif
        menu R.Command.-Sep6- <nul>
        if &filetype == "rnoweb"
            call RCreateMenuItem("nvi", 'Command.Knit\ (cur\ file)', '<Plug>RKnit', 'kn', ':call RKnitRnw()')
        else
            call RCreateMenuItem("nvi", 'Command.Knit\ (cur\ file)', '<Plug>RKnit', 'kn', ':call RKnit()')
        endif
        if &filetype == "rnoweb" || g:vimrplugin_never_unmake_menu
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ PDF\ (cur\ file)', '<Plug>RMakePDFK', 'kp', ':call RMakePDF("nobib", 1)')
            call RCreateMenuItem("nvi", 'Command.Knit,\ BibTeX\ and\ PDF\ (cur\ file)', '<Plug>RBibTeXK', 'kb', ':call RMakePDF("bibtex", 1)')
        endif
        if &filetype == "rmd" || g:vimrplugin_never_unmake_menu
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ PDF\ (cur\ file)', '<Plug>RMakePDFK', 'kp', ':call RMakeRmd("pdf_document")')
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ Beamer\ PDF\ (cur\ file)', '<Plug>RMakePDFKb', 'kl', ':call RMakeRmd("beamer_presentation")')
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ HTML\ (cur\ file)', '<Plug>RMakeHTML', 'kh', ':call RMakeRmd("html_document")')
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ ODT\ (cur\ file)', '<Plug>RMakeODT', 'ko', ':call RMakeRmd("odt")')
        endif
        if &filetype == "rrst" || g:vimrplugin_never_unmake_menu
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ PDF\ (cur\ file)', '<Plug>RMakePDFK', 'kp', ':call RMakePDFrrst()')
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ HTML\ (cur\ file)', '<Plug>RMakeHTML', 'kh', ':call RMakeHTMLrrst("html")')
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ ODT\ (cur\ file)', '<Plug>RMakeODT', 'ko', ':call RMakeHTMLrrst("odt")')
        endif
        menu R.Command.-Sep61- <nul>
        call RCreateMenuItem("nvi", 'Command.Open\ PDF\ (cur\ file)', '<Plug>ROpenPDF', 'op', ':call ROpenPDF("Get Master")')
        if ($DISPLAY != "" && g:vimrplugin_synctex && &filetype == "rnoweb") || g:vimrplugin_never_unmake_menu
            call RCreateMenuItem("nvi", 'Command.Search\ forward\ (SyncTeX)', '<Plug>RSyncFor', 'gp', ':call SyncTeX_forward()')
            call RCreateMenuItem("nvi", 'Command.Go\ to\ LaTeX\ (SyncTeX)', '<Plug>RSyncTex', 'gt', ':call SyncTeX_forward(1)')
        endif
    endif
    "-------------------------------
    if &filetype == "r" || g:vimrplugin_never_unmake_menu
        menu R.Command.-Sep71- <nul>
        call RCreateMenuItem("nvi", 'Command.Spin\ (cur\ file)', '<Plug>RSpinFile', 'ks', ':call RSpin()')
    endif
    menu R.Command.-Sep72- <nul>
    if &filetype == "r" || &filetype == "rnoweb" || g:vimrplugin_never_unmake_menu
        nmenu <silent> R.Command.Build\ tags\ file\ (cur\ dir)<Tab>:RBuildTags :call g:SendCmdToR('rtags(ofile = "TAGS")')<CR>
        imenu <silent> R.Command.Build\ tags\ file\ (cur\ dir)<Tab>:RBuildTags <Esc>:call g:SendCmdToR('rtags(ofile = "TAGS")')<CR>a
    endif

    menu R.-Sep7- <nul>

    "----------------------------------------------------------------------------
    " Edit
    "----------------------------------------------------------------------------
    if &filetype == "r" || &filetype == "rnoweb" || &filetype == "rrst" || &filetype == "rhelp" || g:vimrplugin_never_unmake_menu
        if g:vimrplugin_assign == 1 || g:vimrplugin_assign == 2
            silent exe 'imenu <silent> R.Edit.Insert\ \"\ <-\ \"<Tab>' . g:vimrplugin_assign_map . ' <Esc>:call ReplaceUnderS()<CR>a'
        endif
        imenu <silent> R.Edit.Complete\ object\ name<Tab>^X^O <C-X><C-O>
        if hasmapto("<Plug>RCompleteArgs", "i")
            let boundkey = RIMapCmd("<Plug>RCompleteArgs")
            exe "imenu <silent> R.Edit.Complete\\ function\\ arguments<Tab>" . boundkey . " " . boundkey
        else
            imenu <silent> R.Edit.Complete\ function\ arguments<Tab>^X^A <C-X><C-A>
        endif
        menu R.Edit.-Sep71- <nul>
        nmenu <silent> R.Edit.Indent\ (line)<Tab>== ==
        vmenu <silent> R.Edit.Indent\ (selected\ lines)<Tab>= =
        nmenu <silent> R.Edit.Indent\ (whole\ buffer)<Tab>gg=G gg=G
        menu R.Edit.-Sep72- <nul>
        call RCreateMenuItem("ni", 'Edit.Toggle\ comment\ (line/sel)', '<Plug>RToggleComment', 'xx', ':call RComment("normal")')
        call RCreateMenuItem("v", 'Edit.Toggle\ comment\ (line/sel)', '<Plug>RToggleComment', 'xx', ':call RComment("selection")')
        call RCreateMenuItem("ni", 'Edit.Comment\ (line/sel)', '<Plug>RSimpleComment', 'xc', ':call RSimpleCommentLine("normal", "c")')
        call RCreateMenuItem("v", 'Edit.Comment\ (line/sel)', '<Plug>RSimpleComment', 'xc', ':call RSimpleCommentLine("selection", "c")')
        call RCreateMenuItem("ni", 'Edit.Uncomment\ (line/sel)', '<Plug>RSimpleUnComment', 'xu', ':call RSimpleCommentLine("normal", "u")')
        call RCreateMenuItem("v", 'Edit.Uncomment\ (line/sel)', '<Plug>RSimpleUnComment', 'xu', ':call RSimpleCommentLine("selection", "u")')
        call RCreateMenuItem("ni", 'Edit.Add/Align\ right\ comment\ (line,\ sel)', '<Plug>RRightComment', ';', ':call MovePosRCodeComment("normal")')
        call RCreateMenuItem("v", 'Edit.Add/Align\ right\ comment\ (line,\ sel)', '<Plug>RRightComment', ';', ':call MovePosRCodeComment("selection")')
        if &filetype == "rnoweb" || &filetype == "rrst" || &filetype == "rmd" || g:vimrplugin_never_unmake_menu
            menu R.Edit.-Sep73- <nul>
            call RCreateMenuItem("n", 'Edit.Go\ (next\ R\ chunk)', '<Plug>RNextRChunk', 'gn', ':call b:NextRChunk()')
            call RCreateMenuItem("n", 'Edit.Go\ (previous\ R\ chunk)', '<Plug>', 'gN', ':call b:PreviousRChunk()')
        endif
    endif

    "----------------------------------------------------------------------------
    " Object Browser
    "----------------------------------------------------------------------------
    call RBrowserMenu()

    "----------------------------------------------------------------------------
    " Help
    "----------------------------------------------------------------------------
    menu R.-Sep8- <nul>
    amenu R.Help\ (plugin).Overview :help r-plugin-overview<CR>
    amenu R.Help\ (plugin).Main\ features :help r-plugin-features<CR>
    amenu R.Help\ (plugin).Installation :help r-plugin-installation<CR>
    amenu R.Help\ (plugin).Use :help r-plugin-use<CR>
    amenu R.Help\ (plugin).Known\ bugs\ and\ workarounds :help r-plugin-known-bugs<CR>

    amenu R.Help\ (plugin).Options.Assignment\ operator\ and\ Rnoweb\ code :help vimrplugin_assign<CR>
    amenu R.Help\ (plugin).Options.Object\ Browser :help vimrplugin_objbr_place<CR>
    amenu R.Help\ (plugin).Options.Vim\ as\ pager\ for\ R\ help :help vimrplugin_vimpager<CR>
    if !(has("gui_win32") || has("gui_win64"))
        amenu R.Help\ (plugin).Options.Terminal\ emulator :help vimrplugin_term<CR>
    endif
    if g:rplugin_is_darwin
        amenu R.Help\ (plugin).Options.Integration\ with\ Apple\ Script :help vimrplugin_applescript<CR>
    endif
    if has("gui_win32") || has("gui_win64")
        amenu R.Help\ (plugin).Options.Use\ 32\ bit\ version\ of\ R :help vimrplugin_i386<CR>
    endif
    amenu R.Help\ (plugin).Options.R\ path :help vimrplugin_r_path<CR>
    amenu R.Help\ (plugin).Options.Arguments\ to\ R :help vimrplugin_r_args<CR>
    amenu R.Help\ (plugin).Options.Omni\ completion\ when\ R\ not\ running :help vimrplugin_start_libs<CR>
    amenu R.Help\ (plugin).Options.Syntax\ highlighting\ of\ \.Rout\ files :help vimrplugin_routmorecolors<CR>
    amenu R.Help\ (plugin).Options.Automatically\ open\ the\ \.Rout\ file :help vimrplugin_routnotab<CR>
    amenu R.Help\ (plugin).Options.Special\ R\ functions :help vimrplugin_listmethods<CR>
    amenu R.Help\ (plugin).Options.Indent\ commented\ lines :help vimrplugin_indent_commented<CR>
    amenu R.Help\ (plugin).Options.LaTeX\ command :help vimrplugin_latexcmd<CR>
    amenu R.Help\ (plugin).Options.Never\ unmake\ the\ R\ menu :help vimrplugin_never_unmake_menu<CR>

    amenu R.Help\ (plugin).Custom\ key\ bindings :help r-plugin-key-bindings<CR>
    amenu R.Help\ (plugin).Files :help r-plugin-files<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.All\ tips :help r-plugin-tips<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Indenting\ setup :help r-plugin-indenting<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Folding\ setup :help r-plugin-folding<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Remap\ LocalLeader :help r-plugin-localleader<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Customize\ key\ bindings :help r-plugin-bindings<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.ShowMarks :help r-plugin-showmarks<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.SnipMate :help r-plugin-snippets<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.LaTeX-Box :help r-plugin-latex-box<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Highlight\ marks :help r-plugin-showmarks<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Global\ plugin :help r-plugin-global<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Jump\ to\ function\ definitions :help r-plugin-tagsfile<CR>
    amenu R.Help\ (plugin).News :help r-plugin-news<CR>

    amenu R.Help\ (R)<Tab>:Rhelp :call g:SendCmdToR("help.start()")<CR>
    let g:rplugin_hasmenu = 1

    "----------------------------------------------------------------------------
    " ToolBar
    "----------------------------------------------------------------------------
    if g:rplugin_has_icons
        " Buttons
        amenu <silent> ToolBar.RStart :call StartR("R")<CR>
        amenu <silent> ToolBar.RClose :call RQuit('no')<CR>
        "---------------------------
        if &filetype == "r" || g:vimrplugin_never_unmake_menu
            nmenu <silent> ToolBar.RSendFile :call SendFileToR("echo")<CR>
            imenu <silent> ToolBar.RSendFile <Esc>:call SendFileToR("echo")<CR>
            let g:rplugin_hasRSFbutton = 1
        endif
        nmenu <silent> ToolBar.RSendBlock :call SendMBlockToR("echo", "down")<CR>
        imenu <silent> ToolBar.RSendBlock <Esc>:call SendMBlockToR("echo", "down")<CR>
        nmenu <silent> ToolBar.RSendFunction :call SendFunctionToR("echo", "down")<CR>
        imenu <silent> ToolBar.RSendFunction <Esc>:call SendFunctionToR("echo", "down")<CR>
        vmenu <silent> ToolBar.RSendSelection <ESC>:call SendSelectionToR("echo", "down")<CR>
        nmenu <silent> ToolBar.RSendParagraph :call SendParagraphToR("echo", "down")<CR>
        imenu <silent> ToolBar.RSendParagraph <Esc>:call SendParagraphToR("echo", "down")<CR>
        nmenu <silent> ToolBar.RSendLine :call SendLineToR("down")<CR>
        imenu <silent> ToolBar.RSendLine <Esc>:call SendLineToR("down")<CR>
        "---------------------------
        nmenu <silent> ToolBar.RListSpace :call g:SendCmdToR("ls()")<CR>
        imenu <silent> ToolBar.RListSpace <Esc>:call g:SendCmdToR("ls()")<CR>
        nmenu <silent> ToolBar.RClear :call RClearConsole()<CR>
        imenu <silent> ToolBar.RClear <Esc>:call RClearConsole()<CR>
        nmenu <silent> ToolBar.RClearAll :call RClearAll()<CR>
        imenu <silent> ToolBar.RClearAll <Esc>:call RClearAll()<CR>

        " Hints
        tmenu ToolBar.RStart Start R (default)
        tmenu ToolBar.RClose Close R (no save)
        if &filetype == "r" || g:vimrplugin_never_unmake_menu
            tmenu ToolBar.RSendFile Send file (echo)
        endif
        tmenu ToolBar.RSendBlock Send block (cur, echo and down)
        tmenu ToolBar.RSendFunction Send function (cur, echo and down)
        tmenu ToolBar.RSendSelection Send selection (cur, echo and down)
        tmenu ToolBar.RSendParagraph Send paragraph (cur, echo and down)
        tmenu ToolBar.RSendLine Send line (cur and down)
        tmenu ToolBar.RListSpace List objects
        tmenu ToolBar.RClear Clear the console screen
        tmenu ToolBar.RClearAll Remove objects from workspace and clear the console screen
        let g:rplugin_hasbuttons = 1
    else
        let g:rplugin_hasbuttons = 0
    endif
endfunction

function UnMakeRMenu()
    if g:rplugin_hasmenu == 0 || g:vimrplugin_never_unmake_menu == 1 || &previewwindow || (&buftype == "nofile" && &filetype != "rbrowser")
        return
    endif
    aunmenu R
    let g:rplugin_hasmenu = 0
    if g:rplugin_hasbuttons
        aunmenu ToolBar.RClearAll
        aunmenu ToolBar.RClear
        aunmenu ToolBar.RListSpace
        aunmenu ToolBar.RSendLine
        aunmenu ToolBar.RSendSelection
        aunmenu ToolBar.RSendParagraph
        aunmenu ToolBar.RSendFunction
        aunmenu ToolBar.RSendBlock
        if g:rplugin_hasRSFbutton
            aunmenu ToolBar.RSendFile
            let g:rplugin_hasRSFbutton = 0
        endif
        aunmenu ToolBar.RClose
        aunmenu ToolBar.RStart
        let g:rplugin_hasbuttons = 0
    endif
endfunction

r-plugin/osx.vim	[[[1
52
" This file contains code used only on OS X

function StartR_OSX()
    if IsSendCmdToRFake()
        return
    endif
    if g:rplugin_r64app && g:vimrplugin_i386 == 0
        let rcmd = "/Applications/R64.app"
    else
        let rcmd = "/Applications/R.app"
    endif

    if b:rplugin_r_args != " "
        " https://github.com/jcfaria/Vim-R-plugin/issues/63
        " https://stat.ethz.ch/pipermail/r-sig-mac/2013-February/009978.html
        call RWarningMsg('R.app does not support command line arguments. To pass "' . b:rplugin_r_args . '" to R, you must run it in a console. Set "vimrplugin_applescript = 0" (you may need to install XQuartz)')
    endif
    let rlog = system("open " . rcmd)
    if v:shell_error
        call RWarningMsg(rlog)
    endif
    if g:vimrplugin_vim_wd == 0
        lcd -
    endif
    let g:SendCmdToR = function('SendCmdToR_OSX')
    if WaitVimComStart()
        call SendToVimCom("\005B Update OB [StartR]")
    endif
endfunction

function SendCmdToR_OSX(cmd)
    if g:vimrplugin_ca_ck
        let cmd = "\001" . "\013" . a:cmd
    else
        let cmd = a:cmd
    endif

    if g:rplugin_r64app && g:vimrplugin_i386 == 0
        let rcmd = "R64"
    else
        let rcmd = "R"
    endif

    " for some reason it doesn't like "\025"
    let cmd = a:cmd
    let cmd = substitute(cmd, "\\", '\\\', 'g')
    let cmd = substitute(cmd, '"', '\\"', "g")
    let cmd = substitute(cmd, "'", "'\\\\''", "g")
    call system("osascript -e 'tell application \"".rcmd."\" to cmd \"" . cmd . "\"'")
    return 1
endfunction

r-plugin/setcompldir.vim	[[[1
75

" g:rplugin_home should be the directory where the r-plugin files are.  For
" users following the installation instructions it will be at ~/.vim or
" ~/vimfiles, that is, the same value of g:rplugin_uservimfiles. However the
" variables will have different values if the plugin is installed somewhere
" else in the runtimepath.
let g:rplugin_home = expand("<sfile>:h:h")

" g:rplugin_uservimfiles must be a writable directory. It will be g:rplugin_home
" unless it's not writable. Then it wil be ~/.vim or ~/vimfiles.
if filewritable(g:rplugin_home) == 2
    let g:rplugin_uservimfiles = g:rplugin_home
else
    let g:rplugin_uservimfiles = split(&runtimepath, ",")[0]
endif

" From changelog.vim, with bug fixed by "Si" ("i5ivem")
" Windows logins can include domain, e.g: 'DOMAIN\Username', need to remove
" the backslash from this as otherwise cause file path problems.
if executable("whoami")
    let g:rplugin_userlogin = substitute(system('whoami'), "\\", "-", "")
elseif $USER != ""
    let g:rplugin_userlogin = $USER
else
    call RWarningMsgInp("Could not determine user name.")
    let g:rplugin_failed = 1
    finish
endif

if v:shell_error
    let g:rplugin_userlogin = 'unknown'
else
    let newuline = stridx(g:rplugin_userlogin, "\n")
    if newuline != -1
        let g:rplugin_userlogin = strpart(g:rplugin_userlogin, 0, newuline)
    endif
    unlet newuline
endif

if has("win32") || has("win64")
    let g:rplugin_home = substitute(g:rplugin_home, "\\", "/", "g")
    let g:rplugin_uservimfiles = substitute(g:rplugin_uservimfiles, "\\", "/", "g")
    if $USERNAME != ""
        let g:rplugin_userlogin = substitute($USERNAME, " ", "", "g")
    endif
endif

if exists("g:vimrplugin_compldir")
    let g:rplugin_compldir = expand(g:vimrplugin_compldir)
elseif (has("win32") || has("win64")) && $AppData != "" && isdirectory($AppData)
    let g:rplugin_compldir = $AppData . "\\Vim-R-plugin"
elseif isdirectory(expand("~/.cache"))
    let g:rplugin_compldir = expand("~/.cache/Vim-R-plugin")
elseif isdirectory(expand("~/Library/Caches"))
    let g:rplugin_compldir = expand("~/Library/Caches/Vim-R-plugin")
else
    let g:rplugin_compldir = g:rplugin_uservimfiles . "/r-plugin/objlist/"
endif

" Create the directory if it doesn't exist yet
if !isdirectory(g:rplugin_compldir)
    call mkdir(g:rplugin_compldir, "p")
    if !filereadable(g:rplugin_compldir . "/README")
        let readme = ['The omnils_ and fun_ files in this directory are generated by Vim-R-plugin',
                    \ 'and vimcom and are used for omni completion and syntax highlight.',
                    \ '',
                    \ 'When you load a new version of a library, their files are replaced.',
                    \ '',
                    \ 'You should manually delete files corresponding to libraries that you no',
                    \ 'longer use.']
        call writefile(readme, g:rplugin_compldir . "/README")
    endif
endif
let $VIMRPLUGIN_COMPLDIR = g:rplugin_compldir

r-plugin/windows.vim	[[[1
183
" This file contains code used only on Windows

let g:rplugin_sumatra_path = ""
let g:rplugin_python_initialized = 0

call RSetDefaultValue("g:vimrplugin_sleeptime", 100)

" Avoid invalid values defined by the user
exe "let s:sleeptimestr = " . '"' . g:vimrplugin_sleeptime . '"'
let s:sleeptime = str2nr(s:sleeptimestr)
if s:sleeptime < 1 || s:sleeptime > 1000
    let g:vimrplugin_sleeptime = 100
endif
unlet s:sleeptimestr
unlet s:sleeptime

let g:rplugin_sleeptime = g:vimrplugin_sleeptime . 'm'
exe 'let $VIM_SLEEPTIME = ' . '"' . g:vimrplugin_sleeptime . '"'

if g:vimrplugin_Rterm
    let b:rplugin_R = "Rgui.exe"
else
    let b:rplugin_R = "Rterm.exe"
endif
if !exists("g:rplugin_rpathadded")
    if exists("g:vimrplugin_r_path")
        if !isdirectory(g:vimrplugin_r_path)
            call RWarningMsgInp("vimrplugin_r_path must be a directory (check your vimrc)")
            let g:rplugin_failed = 1
            finish
        endif
        if !filereadable(g:vimrplugin_r_path . "\\Rgui.exe")
            call RWarningMsgInp('File "' . g:vimrplugin_r_path . '\Rgui.exe" is unreadable (check vimrplugin_r_path in your vimrc).')
            let g:rplugin_failed = 1
            finish
        endif
        let $PATH = g:vimrplugin_r_path . ";" . $PATH
        let g:rplugin_Rgui = g:vimrplugin_r_path . "\\Rgui.exe"
    else
        let rip = filter(split(system('reg.exe QUERY "HKLM\SOFTWARE\R-core\R" /s'), "\n"), 'v:val =~ ".*InstallPath.*REG_SZ"')
        let g:rdebug_reg_rpath_1 = rip
        if len(rip) > 0
            let s:rinstallpath = substitute(rip[0], '.*InstallPath.*REG_SZ\s*', '', '')
            let s:rinstallpath = substitute(s:rinstallpath, '\n', '', 'g')
            let s:rinstallpath = substitute(s:rinstallpath, '\s*$', '', 'g')
            let g:rdebug_reg_rpath_2 = s:rinstallpath
        endif

        if !exists("s:rinstallpath")
            call RWarningMsgInp("Could not find R path in Windows Registry. If you have already installed R, please, set the value of 'vimrplugin_r_path'.")
            let g:rplugin_failed = 1
            finish
        endif
        if isdirectory(s:rinstallpath . '\bin\i386')
            if !isdirectory(s:rinstallpath . '\bin\x64')
                let g:vimrplugin_i386 = 1
            endif
            if g:vimrplugin_i386
                let $PATH = s:rinstallpath . '\bin\i386;' . $PATH
                let g:rplugin_Rgui = s:rinstallpath . '\bin\i386\Rgui.exe'
            else
                let $PATH = s:rinstallpath . '\bin\x64;' . $PATH
                let g:rplugin_Rgui = s:rinstallpath . '\bin\x64\Rgui.exe'
            endif
        else
            let $PATH = s:rinstallpath . '\bin;' . $PATH
            let g:rplugin_Rgui = s:rinstallpath . '\bin\Rgui.exe'
        endif
        unlet s:rinstallpath
    endif
    let g:rplugin_rpathadded = 1
endif
let g:vimrplugin_term_cmd = "none"
let g:vimrplugin_term = "none"
if !exists("g:vimrplugin_r_args")
    let g:vimrplugin_r_args = "--sdi"
endif
if g:vimrplugin_Rterm
    let g:rplugin_Rgui = substitute(g:rplugin_Rgui, "Rgui", "Rterm", "")
endif

if !exists("g:vimrplugin_R_window_title")
    if g:vimrplugin_Rterm
        let g:vimrplugin_R_window_title = "Rterm"
    else
        let g:vimrplugin_R_window_title = "R Console"
    endif
endif

function FindSumatra()
    if executable($ProgramFiles . "\\SumatraPDF\\SumatraPDF.exe")
        let g:rplugin_sumatra_path = $ProgramFiles . "\\SumatraPDF\\SumatraPDF.exe"
        return 1
    endif
    let smtr = system('reg.exe QUERY "HKLM\Software\Microsoft\Windows\CurrentVersion\App Paths" /v "SumatraPDF.exe"')
    if len(smtr) > 0
        let g:rdebug_reg_personal = smtr
        let smtr = substitute(smtr, '.*REG_SZ\s*', '', '')
        let smtr = substitute(smtr, '\n', '', 'g')
        let smtr = substitute(smtr, '\s*$', '', 'g')
        if executable(smtr)
            let g:rplugin_sumatra_path = smtr
            return 1
        else
            call RWarningMsg('Sumatra not found: "' . smtr . '"')
        endif
    else
        call RWarningMsg("SumatraPDF not found in Windows registry.")
    endif
    return 0
endfunction

function StartR_Windows()
    if string(g:SendCmdToR) != "function('SendCmdToR_fake')"
        let repl = libcall(g:rplugin_vimcom_lib, "IsRRunning", 'No argument')
        if repl =~ "^Yes"
            call RWarningMsg('R is already running.')
            return
        else
            let g:SendCmdToR = function('SendCmdToR_fake')
            let g:rplugin_r_pid = 0
        endif
    endif

    if !executable(g:rplugin_Rgui)
        call RWarningMsg('R executable "' . g:rplugin_Rgui . '" not found.')
        if exists("g:rdebug_reg_rpath_1")
            call RWarningMsg('DEBUG message 1: >>' . g:rdebug_reg_rpath_1 . '<<')
        endif
        if exists("g:rdebug_reg_rpath_1")
            call RWarningMsg('DEBUG message 2: >>' . g:rdebug_reg_rpath_2 . '<<')
        endif
        return
    endif

    " R and Vim use different values for the $HOME variable.
    let saved_home = $HOME
    let prs = system('reg.exe QUERY "HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders" /v "Personal"')
    if len(prs) > 0
        let g:rdebug_reg_personal = prs
        let prs = substitute(prs, '.*REG_SZ\s*', '', '')
        let prs = substitute(prs, '\n', '', 'g')
        let prs = substitute(prs, '\s*$', '', 'g')
        let $HOME = prs
    endif

    let rcmd = g:rplugin_Rgui
    if g:vimrplugin_Rterm
        let rcmd = substitute(rcmd, "Rgui", "Rterm", "")
    endif
    let rcmd = '"' . rcmd . '" ' . g:vimrplugin_r_args

    silent exe "!start " . rcmd

    let $HOME = saved_home

    if g:vimrplugin_vim_wd == 0
        lcd -
    endif
    let g:SendCmdToR = function('SendCmdToR_Windows')
    call WaitVimComStart()
endfunction

function SendCmdToR_Windows(cmd)
    if g:vimrplugin_ca_ck
        let cmd = "\001" . "\013" . a:cmd . "\n"
    else
        let cmd = a:cmd . "\n"
    endif
    if g:vimrplugin_Rterm
        let repl = libcall(g:rplugin_vimcom_lib, "SendToRTerm", cmd)
    else
        let repl = libcall(g:rplugin_vimcom_lib, "SendToRConsole", cmd)
    endif
    if repl != "OK"
        call RWarningMsg(repl)
        call ClearRInfo()
    endif
    exe "sleep " . g:rplugin_sleeptime
    call foreground()
    return 1
endfunction

syntax/r.vim	[[[1
236
" Vim syntax file
" Language:	      R (GNU S)
" Maintainer:	      Jakson Aquino <jalvesaq@gmail.com>
" Former Maintainers: Vaidotas Zemlys <zemlys@gmail.com>
" 		      Tom Payne <tom@tompayne.org>
" Last Change:	      Wed Dec 31, 2014  12:36AM
" Filenames:	      *.R *.r *.Rhistory *.Rt
"
" NOTE: The highlighting of R functions is defined in
" runtime files created by a filetype plugin, if installed.
"
" CONFIGURATION:
"   syntax folding can be turned on by
"
"      let r_syntax_folding = 1
"
" Some lines of code were borrowed from Zhuojun Chen.

if exists("b:current_syntax")
  finish
endif

setlocal iskeyword=@,48-57,_,.

if exists("g:r_syntax_folding")
  setlocal foldmethod=syntax
endif

syn case match

" Comment
syn match rCommentTodo contained "\(BUG\|FIXME\|NOTE\|TODO\):"
syn match rComment contains=@Spell,rCommentTodo "#.*"

" Roxygen
syn match rOKeyword contained "@\(param\|return\|name\|rdname\|examples\|include\|docType\)"
syn match rOKeyword contained "@\(S3method\|TODO\|aliases\|alias\|assignee\|author\|callGraphDepth\|callGraph\)"
syn match rOKeyword contained "@\(callGraphPrimitives\|concept\|exportClass\|exportMethod\|exportPattern\|export\|formals\)"
syn match rOKeyword contained "@\(format\|importClassesFrom\|importFrom\|importMethodsFrom\|import\|keywords\|useDynLib\)"
syn match rOKeyword contained "@\(method\|noRd\|note\|references\|seealso\|setClass\|slot\|source\|title\|usage\)"
syn match rOKeyword contained "@\(family\|template\|templateVar\|description\|details\|inheritParams\)"
syn match rOComment contains=@Spell,rOKeyword "#'.*"


if &filetype == "rhelp"
  " string enclosed in double quotes
  syn region rString contains=rSpecial,@Spell start=/"/ skip=/\\\\\|\\"/ end=/"/
  " string enclosed in single quotes
  syn region rString contains=rSpecial,@Spell start=/'/ skip=/\\\\\|\\'/ end=/'/
else
  " string enclosed in double quotes
  syn region rString contains=rSpecial,rStrError,@Spell start=/"/ skip=/\\\\\|\\"/ end=/"/
  " string enclosed in single quotes
  syn region rString contains=rSpecial,rStrError,@Spell start=/'/ skip=/\\\\\|\\'/ end=/'/
endif

syn match rStrError display contained "\\."


" New line, carriage return, tab, backspace, bell, feed, vertical tab, backslash
syn match rSpecial display contained "\\\(n\|r\|t\|b\|a\|f\|v\|'\|\"\)\|\\\\"

" Hexadecimal and Octal digits
syn match rSpecial display contained "\\\(x\x\{1,2}\|[0-8]\{1,3}\)"

" Unicode characters
syn match rSpecial display contained "\\u\x\{1,4}"
syn match rSpecial display contained "\\U\x\{1,8}"
syn match rSpecial display contained "\\u{\x\{1,4}}"
syn match rSpecial display contained "\\U{\x\{1,8}}"

" Statement
syn keyword rStatement   break next return
syn keyword rConditional if else
syn keyword rRepeat      for in repeat while

" Constant (not really)
syn keyword rConstant T F LETTERS letters month.abb month.name pi
syn keyword rConstant R.version.string

syn keyword rNumber   NA_integer_ NA_real_ NA_complex_ NA_character_

" Constants
syn keyword rConstant NULL
syn keyword rBoolean  FALSE TRUE
syn keyword rNumber   NA Inf NaN

" integer
syn match rInteger "\<\d\+L"
syn match rInteger "\<0x\([0-9]\|[a-f]\|[A-F]\)\+L"
syn match rInteger "\<\d\+[Ee]+\=\d\+L"

" number with no fractional part or exponent
syn match rNumber "\<\d\+\>"
" hexadecimal number
syn match rNumber "\<0x\([0-9]\|[a-f]\|[A-F]\)\+"

" floating point number with integer and fractional parts and optional exponent
syn match rFloat "\<\d\+\.\d*\([Ee][-+]\=\d\+\)\="
" floating point number with no integer part and optional exponent
syn match rFloat "\<\.\d\+\([Ee][-+]\=\d\+\)\="
" floating point number with no fractional part and optional exponent
syn match rFloat "\<\d\+[Ee][-+]\=\d\+"

" complex number
syn match rComplex "\<\d\+i"
syn match rComplex "\<\d\++\d\+i"
syn match rComplex "\<0x\([0-9]\|[a-f]\|[A-F]\)\+i"
syn match rComplex "\<\d\+\.\d*\([Ee][-+]\=\d\+\)\=i"
syn match rComplex "\<\.\d\+\([Ee][-+]\=\d\+\)\=i"
syn match rComplex "\<\d\+[Ee][-+]\=\d\+i"

syn match rAssign    '='
syn match rOperator    "&"
syn match rOperator    '-'
syn match rOperator    '\*'
syn match rOperator    '+'
if &filetype != "rmd" && &filetype != "rrst"
  syn match rOperator    "[|!<>^~/:]"
else
  syn match rOperator    "[|!<>^~`/:]"
endif
syn match rOperator    "%\{2}\|%\S\{-}%"
syn match rOperator '\([!><]\)\@<=='
syn match rOperator '=='
syn match rOpError  '\*\{3}'
syn match rOpError  '//'
syn match rOpError  '&&&'
syn match rOpError  '|||'
syn match rOpError  '<<'
syn match rOpError  '>>'

syn match rAssign "<\{1,2}-"
syn match rAssign "->\{1,2}"

" Special
syn match rDelimiter "[,;:]"

" Error
if exists("g:r_syntax_folding")
  syn region rRegion matchgroup=Delimiter start=/(/ matchgroup=Delimiter end=/)/ transparent contains=ALLBUT,rError,rBraceError,rCurlyError fold
  syn region rRegion matchgroup=Delimiter start=/{/ matchgroup=Delimiter end=/}/ transparent contains=ALLBUT,rError,rBraceError,rParenError fold
  syn region rRegion matchgroup=Delimiter start=/\[/ matchgroup=Delimiter end=/]/ transparent contains=ALLBUT,rError,rCurlyError,rParenError fold
else
  syn region rRegion matchgroup=Delimiter start=/(/ matchgroup=Delimiter end=/)/ transparent contains=ALLBUT,rError,rBraceError,rCurlyError
  syn region rRegion matchgroup=Delimiter start=/{/ matchgroup=Delimiter end=/}/ transparent contains=ALLBUT,rError,rBraceError,rParenError
  syn region rRegion matchgroup=Delimiter start=/\[/ matchgroup=Delimiter end=/]/ transparent contains=ALLBUT,rError,rCurlyError,rParenError
endif

syn match rError      "[)\]}]"
syn match rBraceError "[)}]" contained
syn match rCurlyError "[)\]]" contained
syn match rParenError "[\]}]" contained

" Source list of R functions produced by a filetype plugin (if installed)
if has("nvim")
  " Nvim-R
  runtime R/functions.vim
else
  " Vim-R-plugin
  runtime r-plugin/functions.vim
endif

syn match rDollar display contained "\$"
syn match rDollar display contained "@"

" List elements will not be highlighted as functions:
syn match rLstElmt "\$[a-zA-Z0-9\\._]*" contains=rDollar
syn match rLstElmt "@[a-zA-Z0-9\\._]*" contains=rDollar

" Functions that may add new objects
syn keyword rPreProc     library require attach detach source

if &filetype == "rhelp"
  syn match rHelpIdent '\\method'
  syn match rHelpIdent '\\S4method'
endif

" Type
syn keyword rType array category character complex double function integer list logical matrix numeric vector data.frame

" Name of object with spaces
if &filetype != "rmd" && &filetype != "rrst"
  syn region rNameWSpace start="`" end="`"
endif

if &filetype == "rhelp"
  syn match rhPreProc "^#ifdef.*"
  syn match rhPreProc "^#endif.*"
  syn match rhSection "\\dontrun\>"
endif

if exists("r_syn_minlines")
  exe "syn sync minlines=" . r_syn_minlines
else
  syn sync minlines=40
endif

" Define the default highlighting.
hi def link rAssign      Statement
hi def link rBoolean     Boolean
hi def link rBraceError  Error
hi def link rComment     Comment
hi def link rCommentTodo Todo
hi def link rOComment    Comment
hi def link rComplex     Number
hi def link rConditional Conditional
hi def link rConstant    Constant
hi def link rCurlyError  Error
hi def link rDelimiter   Delimiter
hi def link rDollar      SpecialChar
hi def link rError       Error
hi def link rFloat       Float
hi def link rFunction    Function
hi def link rHelpIdent   Identifier
hi def link rhPreProc    PreProc
hi def link rhSection    PreCondit
hi def link rInteger     Number
hi def link rLstElmt     Normal
hi def link rNameWSpace  Normal
hi def link rNumber      Number
hi def link rOperator    Operator
hi def link rOpError     Error
hi def link rParenError  Error
hi def link rPreProc     PreProc
hi def link rRepeat      Repeat
hi def link rSpecial     SpecialChar
hi def link rStatement   Statement
hi def link rString      String
hi def link rStrError    Error
hi def link rType        Type
hi def link rOKeyword    Title

let b:current_syntax="r"

" vim: ts=8 sw=2
syntax/rbrowser.vim	[[[1
73
" Vim syntax file
" Language:	Object browser of R Workspace
" Maintainer:	Jakson Alves de Aquino (jalvesaq@gmail.com)

if exists("b:current_syntax")
    finish
endif
scriptencoding utf-8

setlocal iskeyword=@,48-57,_,.

if has("conceal")
    setlocal conceallevel=2
    setlocal concealcursor=nvc
    syn match rbrowserNumeric	"{#.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserCharacter	/"#.*\t/ contains=rbrowserDelim,rbrowserTab
    syn match rbrowserFactor	"'#.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserFunction	"(#.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserList	"\[#.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserLogical	"%#.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserLibrary	"##.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserS4  	"<#.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserUnknown	"=#.*\t" contains=rbrowserDelim,rbrowserTab
else
    syn match rbrowserNumeric	"{.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserCharacter	/".*\t/ contains=rbrowserDelim,rbrowserTab
    syn match rbrowserFactor	"'.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserFunction	"(.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserList	"\[.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserLogical	"%.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserLibrary	"#.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserS4	        "<.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserUnknown	"=.*\t" contains=rbrowserDelim,rbrowserTab
endif
syn match rbrowserEnv		"^.GlobalEnv "
syn match rbrowserEnv		"^Libraries "
syn match rbrowserLink		" Libraries$"
syn match rbrowserLink		" .GlobalEnv$"
syn match rbrowserTreePart	"├─"
syn match rbrowserTreePart	"└─"
syn match rbrowserTreePart	"│" 
if &encoding != "utf-8"
    syn match rbrowserTreePart	"|" 
    syn match rbrowserTreePart	"`-"
    syn match rbrowserTreePart	"|-"
endif

syn match rbrowserTab contained "\t"
syn match rbrowserErr /Error: label isn't "character"./
if has("conceal")
    syn match rbrowserDelim contained /'#\|"#\|(#\|\[#\|{#\|%#\|##\|<#\|=#/ conceal
else
    syn match rbrowserDelim contained /'\|"\|(\|\[\|{\|%\|#\|<\|=/
endif

hi def link rbrowserEnv		Statement
hi def link rbrowserNumeric	Number
hi def link rbrowserCharacter	String
hi def link rbrowserFactor	Special
hi def link rbrowserList	Type
hi def link rbrowserLibrary	PreProc
hi def link rbrowserLink	Comment
hi def link rbrowserLogical	Boolean
hi def link rbrowserFunction	Function
hi def link rbrowserS4  	Statement
hi def link rbrowserUnknown	Normal
hi def link rbrowserWarn	WarningMsg
hi def link rbrowserErr 	ErrorMsg
hi def link rbrowserTreePart	Comment
hi def link rbrowserDelim	Ignore
hi def link rbrowserTab		Ignore

" vim: ts=8 sw=4
syntax/rdoc.vim	[[[1
65
" Vim syntax file
" Language:	R documentation
" Maintainer:	Jakson A. Aquino <jalvesaq@gmail.com>

if exists("b:current_syntax")
    finish
endif

setlocal iskeyword=@,48-57,_,.

if !exists("rdoc_minlines")
    let rdoc_minlines = 200
endif
if !exists("rdoc_maxlines")
    let rdoc_maxlines = 2 * rdoc_minlines
endif
exec "syn sync minlines=" . rdoc_minlines . " maxlines=" . rdoc_maxlines


syn match  rdocTitle	      "^[A-Z].*:$"
syn match  rdocTitle "^\S.*R Documentation$"
syn match rdocFunction "\([A-Z]\|[a-z]\|\.\|_\)\([A-Z]\|[a-z]\|[0-9]\|\.\|_\)*" contained
syn region rdocStringS  start="\%u2018" end="\%u2019" contains=rdocFunction transparent keepend
syn region rdocStringS  start="\%x91" end="\%x92" contains=rdocFunction transparent keepend
syn region rdocStringD  start='"' skip='\\"' end='"'
syn match rdocURL `\v<(((https?|ftp|gopher)://|(mailto|file|news):)[^'	<>"]+|(www|web|w3)[a-z0-9_-]*\.[a-z0-9._-]+\.[^'  <>"]+)[a-zA-Z0-9/]`
syn keyword rdocNote		note Note NOTE note: Note: NOTE: Notes Notes:

" When using vim as R pager to see the output of help.search():
syn region rdocPackage start="^[A-Za-z]\S*::" end="[\s\r]" contains=rdocPackName,rdocFuncName transparent
syn match rdocPackName "^[A-Za-z][A-Za-z0-9\.]*" contained
syn match rdocFuncName "::[A-Za-z0-9\.\-_]*" contained

syn region rdocArgReg matchgroup=rdocArgTitle start="^Arguments:" matchgroup=NONE end="^\t" contains=rdocArgItems,rdocArgTitle,rdocPackage,rdocFuncName,rdocStringS keepend transparent
syn match rdocArgItems "\n\n\s*\([A-Z]\|[a-z]\|[0-9]\|\.\|_\)*:" contains=rdocArg contained transparent
syn match rdocArg "\([A-Z]\|[a-z]\|[0-9]\|\.\|_\)*" contained

syn include @rdocR syntax/r.vim
syn region rdocExample matchgroup=rdocExTitle start="^Examples:$" matchgroup=rdocExEnd end='^###$' contains=@rdocR keepend
syn region rdocUsage matchgroup=rdocTitle start="^Usage:$" matchgroup=NONE end='^\t' contains=@rdocR

syn sync match rdocSyncExample grouphere rdocExample "^Examples:$"
syn sync match rdocSyncUsage grouphere rdocUsage "^Usage:$"
syn sync match rdocSyncArg grouphere rdocArgReg "^Arguments:"
syn sync match rdocSyncNONE grouphere NONE "^\t$"


" Define the default highlighting.
"hi def link rdocArgReg Statement
hi def link rdocTitle	    Title
hi def link rdocArgTitle    Title
hi def link rdocExTitle   Title
hi def link rdocExEnd   Comment
hi def link rdocFunction    Function
hi def link rdocStringD     String
hi def link rdocURL    HtmlLink
hi def link rdocArg         Special
hi def link rdocNote  Todo

hi def link rdocPackName Title
hi def link rdocFuncName Function

let b:current_syntax = "rdoc"

" vim: ts=8 sw=4
syntax/rhelp.vim	[[[1
249
" Vim syntax file
" Language:    R Help File
" Maintainer: Jakson Aquino <jalvesaq@gmail.com>
" Former Maintainer: Johannes Ranke <jranke@uni-bremen.de>
" Last Change: Wed Jul 09, 2014  10:28PM
" Remarks:     - Includes R syntax highlighting in the appropriate
"                sections if an r.vim file is in the same directory or in the
"                default debian location.
"              - There is no Latex markup in equations
"              - Thanks to Will Gray for finding and fixing a bug
"              - No support for \if, \ifelse and \out as I don't understand
"                them and have no examples at hand (help welcome).
"              - No support for \var tag within quoted string (dito)

" Version Clears: {{{1
" For version 5.x: Clear all syntax items
" For version 6.x and 7.x: Quit when a syntax file was already loaded
if version < 600 
  syntax clear
elseif exists("b:current_syntax")
  finish
endif 

setlocal iskeyword=@,48-57,_,.

syn case match

" R help identifiers {{{1
syn region rhelpIdentifier matchgroup=rhelpSection	start="\\name{" end="}" 
syn region rhelpIdentifier matchgroup=rhelpSection	start="\\alias{" end="}" 
syn region rhelpIdentifier matchgroup=rhelpSection	start="\\pkg{" end="}" contains=rhelpLink
syn region rhelpIdentifier matchgroup=rhelpSection start="\\method{" end="}" contained
syn region rhelpIdentifier matchgroup=rhelpSection start="\\Rdversion{" end="}"

" Highlighting of R code using an existing r.vim syntax file if available {{{1
syn include @R syntax/r.vim

" Strings {{{1
syn region rhelpString start=/"/ skip=/\\"/ end=/"/ contains=rhelpSpecialChar,rhelpCodeSpecial,rhelpLink contained

" Special characters in R strings
syn match rhelpCodeSpecial display contained "\\\\\(n\|r\|t\|b\|a\|f\|v\|'\|\"\)\|\\\\"

" Special characters  ( \$ \& \% \# \{ \} \_)
syn match rhelpSpecialChar        "\\[$&%#{}_]"


" R code {{{1
syn match rhelpDots		"\\dots" containedin=@R
syn region rhelpRcode matchgroup=Delimiter start="\\examples{" matchgroup=Delimiter transparent end="}" contains=@R,rhelpLink,rhelpIdentifier,rhelpString,rhelpSpecialChar,rhelpSection
syn region rhelpRcode matchgroup=Delimiter start="\\usage{" matchgroup=Delimiter transparent end="}" contains=@R,rhelpIdentifier,rhelpS4method
syn region rhelpRcode matchgroup=Delimiter start="\\synopsis{" matchgroup=Delimiter transparent end="}" contains=@R
syn region rhelpRcode matchgroup=Delimiter start="\\special{" matchgroup=Delimiter transparent end="}" contains=@R

if v:version > 703
  syn region rhelpRcode matchgroup=Delimiter start="\\code{" skip='\\\@1<!{.\{-}\\\@1<!}' transparent end="}" contains=@R,rhelpDots,rhelpString,rhelpSpecialChar,rhelpLink keepend
else
  syn region rhelpRcode matchgroup=Delimiter start="\\code{" skip='\\\@<!{.\{-}\\\@<!}' transparent end="}" contains=@R,rhelpDots,rhelpString,rhelpSpecialChar,rhelpLink keepend
endif
syn region rhelpS4method matchgroup=Delimiter start="\\S4method{.*}(" matchgroup=Delimiter transparent end=")" contains=@R,rhelpDots
syn region rhelpSexpr matchgroup=Delimiter start="\\Sexpr{" matchgroup=Delimiter transparent end="}" contains=@R

" PreProc {{{1
syn match rhelpPreProc "^#ifdef.*" 
syn match rhelpPreProc "^#endif.*" 

" Special Delimiters {{{1
syn match rhelpDelimiter		"\\cr"
syn match rhelpDelimiter		"\\tab "

" Keywords {{{1
syn match rhelpKeyword	"\\R"
syn match rhelpKeyword	"\\ldots"
syn match rhelpKeyword  "--"
syn match rhelpKeyword  "---"
syn match rhelpKeyword  "<"
syn match rhelpKeyword  ">"
syn match rhelpKeyword	"\\ge"
syn match rhelpKeyword	"\\le"
syn match rhelpKeyword	"\\alpha"
syn match rhelpKeyword	"\\beta"
syn match rhelpKeyword	"\\gamma"
syn match rhelpKeyword	"\\delta"
syn match rhelpKeyword	"\\epsilon"
syn match rhelpKeyword	"\\zeta"
syn match rhelpKeyword	"\\eta"
syn match rhelpKeyword	"\\theta"
syn match rhelpKeyword	"\\iota"
syn match rhelpKeyword	"\\kappa"
syn match rhelpKeyword	"\\lambda"
syn match rhelpKeyword	"\\mu"
syn match rhelpKeyword	"\\nu"
syn match rhelpKeyword	"\\xi"
syn match rhelpKeyword	"\\omicron"
syn match rhelpKeyword	"\\pi"
syn match rhelpKeyword	"\\rho"
syn match rhelpKeyword	"\\sigma"
syn match rhelpKeyword	"\\tau"
syn match rhelpKeyword	"\\upsilon"
syn match rhelpKeyword	"\\phi"
syn match rhelpKeyword	"\\chi"
syn match rhelpKeyword	"\\psi"
syn match rhelpKeyword	"\\omega"
syn match rhelpKeyword	"\\Alpha"
syn match rhelpKeyword	"\\Beta"
syn match rhelpKeyword	"\\Gamma"
syn match rhelpKeyword	"\\Delta"
syn match rhelpKeyword	"\\Epsilon"
syn match rhelpKeyword	"\\Zeta"
syn match rhelpKeyword	"\\Eta"
syn match rhelpKeyword	"\\Theta"
syn match rhelpKeyword	"\\Iota"
syn match rhelpKeyword	"\\Kappa"
syn match rhelpKeyword	"\\Lambda"
syn match rhelpKeyword	"\\Mu"
syn match rhelpKeyword	"\\Nu"
syn match rhelpKeyword	"\\Xi"
syn match rhelpKeyword	"\\Omicron"
syn match rhelpKeyword	"\\Pi"
syn match rhelpKeyword	"\\Rho"
syn match rhelpKeyword	"\\Sigma"
syn match rhelpKeyword	"\\Tau"
syn match rhelpKeyword	"\\Upsilon"
syn match rhelpKeyword	"\\Phi"
syn match rhelpKeyword	"\\Chi"
syn match rhelpKeyword	"\\Psi"
syn match rhelpKeyword	"\\Omega"

" Links {{{1
syn region rhelpLink matchgroup=rhelpSection start="\\link{" end="}" contained keepend extend
syn region rhelpLink matchgroup=rhelpSection start="\\link\[.\{-}\]{" end="}" contained keepend extend
syn region rhelpLink matchgroup=rhelpSection start="\\linkS4class{" end="}" contained keepend extend

" Verbatim like {{{1
if v:version > 703
  syn region rhelpVerbatim matchgroup=rhelpType start="\\samp{" skip='\\\@1<!{.\{-}\\\@1<!}' end="}" contains=rhelpSpecialChar,rhelpComment
  syn region rhelpVerbatim matchgroup=rhelpType start="\\verb{" skip='\\\@1<!{.\{-}\\\@1<!}' end="}" contains=rhelpSpecialChar,rhelpComment
else
  syn region rhelpVerbatim matchgroup=rhelpType start="\\samp{" skip='\\\@<!{.\{-}\\\@<!}' end="}" contains=rhelpSpecialChar,rhelpComment
  syn region rhelpVerbatim matchgroup=rhelpType start="\\verb{" skip='\\\@<!{.\{-}\\\@<!}' end="}" contains=rhelpSpecialChar,rhelpComment
endif

" Type Styles {{{1
syn match rhelpType		"\\emph\>"
syn match rhelpType		"\\strong\>"
syn match rhelpType		"\\bold\>"
syn match rhelpType		"\\sQuote\>"
syn match rhelpType		"\\dQuote\>"
syn match rhelpType		"\\preformatted\>"
syn match rhelpType		"\\kbd\>"
syn match rhelpType		"\\eqn\>"
syn match rhelpType		"\\deqn\>"
syn match rhelpType		"\\file\>"
syn match rhelpType		"\\email\>"
syn match rhelpType		"\\url\>"
syn match rhelpType		"\\href\>"
syn match rhelpType		"\\var\>"
syn match rhelpType		"\\env\>"
syn match rhelpType		"\\option\>"
syn match rhelpType		"\\command\>"
syn match rhelpType		"\\newcommand\>"
syn match rhelpType		"\\renewcommand\>"
syn match rhelpType		"\\dfn\>"
syn match rhelpType		"\\cite\>"
syn match rhelpType		"\\acronym\>"

" rhelp sections {{{1
syn match rhelpSection		"\\encoding\>"
syn match rhelpSection		"\\title\>"
syn match rhelpSection		"\\item\>"
syn match rhelpSection		"\\description\>"
syn match rhelpSection		"\\concept\>"
syn match rhelpSection		"\\arguments\>"
syn match rhelpSection		"\\details\>"
syn match rhelpSection		"\\value\>"
syn match rhelpSection		"\\references\>"
syn match rhelpSection		"\\note\>"
syn match rhelpSection		"\\author\>"
syn match rhelpSection		"\\seealso\>"
syn match rhelpSection		"\\keyword\>"
syn match rhelpSection		"\\docType\>"
syn match rhelpSection		"\\format\>"
syn match rhelpSection		"\\source\>"
syn match rhelpSection    "\\itemize\>"
syn match rhelpSection    "\\describe\>"
syn match rhelpSection    "\\enumerate\>"
syn match rhelpSection    "\\item "
syn match rhelpSection    "\\item$"
syn match rhelpSection		"\\tabular{[lcr]*}"
syn match rhelpSection		"\\dontrun\>"
syn match rhelpSection		"\\dontshow\>"
syn match rhelpSection		"\\testonly\>"
syn match rhelpSection		"\\donttest\>"

" Freely named Sections {{{1
syn region rhelpFreesec matchgroup=Delimiter start="\\section{" matchgroup=Delimiter transparent end="}"
syn region rhelpFreesubsec matchgroup=Delimiter start="\\subsection{" matchgroup=Delimiter transparent end="}" 

syn match rhelpDelimiter "{\|\[\|(\|)\|\]\|}"

" R help file comments {{{1
syn match rhelpComment /%.*$/

" Error {{{1
syn region rhelpRegion matchgroup=Delimiter start=/(/ matchgroup=Delimiter end=/)/ contains=@Spell,rhelpCodeSpecial,rhelpComment,rhelpDelimiter,rhelpDots,rhelpFreesec,rhelpFreesubsec,rhelpIdentifier,rhelpKeyword,rhelpLink,rhelpPreProc,rhelpRComment,rhelpRcode,rhelpRegion,rhelpS4method,rhelpSection,rhelpSexpr,rhelpSpecialChar,rhelpString,rhelpType,rhelpVerbatim
syn region rhelpRegion matchgroup=Delimiter start=/{/ matchgroup=Delimiter end=/}/ contains=@Spell,rhelpCodeSpecial,rhelpComment,rhelpDelimiter,rhelpDots,rhelpFreesec,rhelpFreesubsec,rhelpIdentifier,rhelpKeyword,rhelpLink,rhelpPreProc,rhelpRComment,rhelpRcode,rhelpRegion,rhelpS4method,rhelpSection,rhelpSexpr,rhelpSpecialChar,rhelpString,rhelpType,rhelpVerbatim
syn region rhelpRegion matchgroup=Delimiter start=/\[/ matchgroup=Delimiter end=/]/ contains=@Spell,rhelpCodeSpecial,rhelpComment,rhelpDelimiter,rhelpDots,rhelpFreesec,rhelpFreesubsec,rhelpIdentifier,rhelpKeyword,rhelpLink,rhelpPreProc,rhelpRComment,rhelpRcode,rhelpRegion,rhelpS4method,rhelpSection,rhelpSexpr,rhelpSpecialChar,rhelpString,rhelpType,rhelpVerbatim
syn match rhelpError      /[)\]}]/
syn match rhelpBraceError /[)}]/ contained
syn match rhelpCurlyError /[)\]]/ contained
syn match rhelpParenError /[\]}]/ contained

syntax sync match rhelpSyncRcode grouphere rhelpRcode "\\examples{"

" Define the default highlighting {{{1
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_rhelp_syntax_inits")
  if version < 508
    let did_rhelp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink rhelpVerbatim    String
  HiLink rhelpDelimiter   Delimiter
  HiLink rhelpIdentifier  Identifier
  HiLink rhelpString      String
  HiLink rhelpCodeSpecial Special
  HiLink rhelpKeyword     Keyword
  HiLink rhelpDots        Keyword
  HiLink rhelpLink        Underlined
  HiLink rhelpType        Type
  HiLink rhelpSection     PreCondit
  HiLink rhelpError       Error
  HiLink rhelpBraceError  Error
  HiLink rhelpCurlyError  Error
  HiLink rhelpParenError  Error
  HiLink rhelpPreProc     PreProc
  HiLink rhelpDelimiter   Delimiter
  HiLink rhelpComment     Comment
  HiLink rhelpRComment    Comment
  HiLink rhelpSpecialChar SpecialChar
  delcommand HiLink
endif 

let   b:current_syntax = "rhelp"

" vim: foldmethod=marker sw=2
syntax/rmd.vim	[[[1
87
" markdown Text with R statements
" Language: markdown with R code chunks
" Last Change: Wed Jul 09, 2014  10:29PM
"
" CONFIGURATION:
"   To highlight chunk headers as R code, put in your vimrc:
"   let rmd_syn_hl_chunk = 1

" for portability
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" load all of pandoc info
runtime syntax/pandoc.vim
if exists("b:current_syntax")
  let rmdIsPandoc = 1
  unlet b:current_syntax
else
  let rmdIsPandoc = 0
  runtime syntax/markdown.vim
  if exists("b:current_syntax")
    unlet b:current_syntax
  endif
endif

" load all of the r syntax highlighting rules into @R
syntax include @R syntax/r.vim
if exists("b:current_syntax")
  unlet b:current_syntax
endif

if exists("g:rmd_syn_hl_chunk")
  " highlight R code inside chunk header
  syntax match rmdChunkDelim "^[ \t]*```{r" contained
  syntax match rmdChunkDelim "}$" contained
else
  syntax match rmdChunkDelim "^[ \t]*```{r.*}$" contained
endif
syntax match rmdChunkDelim "^[ \t]*```$" contained
syntax region rmdChunk start="^[ \t]*``` *{r.*}$" end="^[ \t]*```$" contains=@R,rmdChunkDelim keepend fold

" also match and syntax highlight in-line R code
syntax match rmdEndInline "`" contained
syntax match rmdBeginInline "`r " contained
syntax region rmdrInline start="`r "  end="`" contains=@R,rmdBeginInline,rmdEndInline keepend

" match slidify special marker
syntax match rmdSlidifySpecial "\*\*\*"


if rmdIsPandoc == 0
  syn match rmdBlockQuote /^\s*>.*\n\(.*\n\@<!\n\)*/ skipnl
  " LaTeX
  syntax include @LaTeX syntax/tex.vim
  if exists("b:current_syntax")
    unlet b:current_syntax
  endif
  " Inline
  syntax match rmdLaTeXInlDelim "\$"
  syntax match rmdLaTeXInlDelim "\\\$"
  syn region texMathZoneX	matchgroup=Delimiter start="\$" skip="\\\\\|\\\$"	matchgroup=Delimiter end="\$" end="%stopzone\>"	contains=@texMathZoneGroup
  " Region
  syntax match rmdLaTeXRegDelim "\$\$" contained
  syntax match rmdLaTeXRegDelim "\$\$latex$" contained
  syntax region rmdLaTeXRegion start="^\$\$" skip="\\\$" end="\$\$$" contains=@LaTeX,rmdLaTeXSt,rmdLaTeXRegDelim keepend
  syntax region rmdLaTeXRegion2 start="^\\\[" end="\\\]" contains=@LaTeX,rmdLaTeXSt,rmdLaTeXRegDelim keepend
  hi def link rmdLaTeXSt Statement
  hi def link rmdLaTeXInlDelim Special
  hi def link rmdLaTeXRegDelim Special
endif

setlocal iskeyword=@,48-57,_,.

syn sync match rmdSyncChunk grouphere rmdChunk "^[ \t]*``` *{r"

hi def link rmdChunkDelim Special
hi def link rmdBeginInline Special
hi def link rmdEndInline Special
hi def link rmdBlockQuote Comment
hi def link rmdSlidifySpecial Special

let b:current_syntax = "rmd"

" vim: ts=8 sw=2
syntax/rout.vim	[[[1
275
" Vim syntax file
" Language:    R output Files
" Maintainer:  Jakson Aquino <jalvesaq@gmail.com>


if exists("b:current_syntax")
    finish
endif 

setlocal iskeyword=@,48-57,_,.

syn case match

" Normal text
syn match routNormal "."

" Strings
syn region routString start=/"/ skip=/\\\\\|\\"/ end=/"/ end=/$/

" Constants
syn keyword routConst  NULL NA NaN
syn keyword routTrue   TRUE
syn keyword routFalse  FALSE
syn match routConst "\<Na's\>"
syn match routInf "-Inf\>"
syn match routInf "\<Inf\>"

" integer
syn match routInteger "\<\d\+L"
syn match routInteger "\<0x\([0-9]\|[a-f]\|[A-F]\)\+L"
syn match routInteger "\<\d\+[Ee]+\=\d\+L"

" number with no fractional part or exponent
syn match routNumber "\<\d\+\>"
syn match routNegNum "-\<\d\+\>"
" hexadecimal number 
syn match routNumber "\<0x\([0-9]\|[a-f]\|[A-F]\)\+"

" floating point number with integer and fractional parts and optional exponent
syn match routFloat "\<\d\+\.\d*\([Ee][-+]\=\d\+\)\="
syn match routNegFlt "-\<\d\+\.\d*\([Ee][-+]\=\d\+\)\="
" floating point number with no integer part and optional exponent
syn match routFloat "\<\.\d\+\([Ee][-+]\=\d\+\)\="
syn match routNegFlt "-\<\.\d\+\([Ee][-+]\=\d\+\)\="
" floating point number with no fractional part and optional exponent
syn match routFloat "\<\d\+[Ee][-+]\=\d\+"
syn match routNegFlt "-\<\d\+[Ee][-+]\=\d\+"

" complex number
syn match routComplex "\<\d\+i"
syn match routComplex "\<\d\++\d\+i"
syn match routComplex "\<0x\([0-9]\|[a-f]\|[A-F]\)\+i"
syn match routComplex "\<\d\+\.\d*\([Ee][-+]\=\d\+\)\=i"
syn match routComplex "\<\.\d\+\([Ee][-+]\=\d\+\)\=i"
syn match routComplex "\<\d\+[Ee][-+]\=\d\+i"

" dates and times
syn match routDate "[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][-0-9]"
syn match routDate "[0-9][0-9][-/][0-9][0-9][-/][0-9][0-9][0-9][-0-9]"
syn match routDate "[0-9][0-9]:[0-9][0-9]:[0-9][-0-9]"

if !exists("g:Rout_more_colors")
    let g:Rout_more_colors = 0
endif

if g:Rout_more_colors
    syn include @routR syntax/r.vim
    syn region routColoredR start="^> " end='$' contains=@routR keepend
    syn region routColoredR start="^+ " end='$' contains=@routR keepend
else
    " Input
    syn match routInput /^> .*/
    syn match routInput /^+ .*/
endif

if has("conceal")
    setlocal conceallevel=3
    set concealcursor=n
    syn match routConceal '^: ' conceal contained
    syn region routStdErr start='^: ' end="$" contains=routConceal
    syn region routError start='^: .*Error.*' end='\n:\@!' contains=routConceal
    syn region routWarn start='^: .*Warn.*' end='\n:\@!' contains=routConceal
else
    syn region routStdErr start='^: ' end="$"
    syn region routError start='^: .*Error.*' end='\n:\@!'
    syn region routWarn start='^: .*Warn.*' end='\n:\@!'
endif

" Index of vectors
syn match routIndex /^\s*\[\d\+\]/

" Errors and warnings
syn match routError "^Error.*"
syn match routWarn "^Warning.*"

if v:lang =~ "^da"
    syn match routError	"^Fejl.*"
    syn match routWarn	"^Advarsel.*"
endif

if v:lang =~ "^de"
    syn match routError	"^Fehler.*"
    syn match routWarn	"^Warnung.*"
endif

if v:lang =~ "^es"
    syn match routWarn	"^Aviso.*"
endif

if v:lang =~ "^fr"
    syn match routError	"^Erreur.*"
    syn match routWarn	"^Avis.*"
endif

if v:lang =~ "^it"
    syn match routError	"^Errore.*"
    syn match routWarn	"^Avviso.*"
endif

if v:lang =~ "^nn"
    syn match routError	"^Feil.*"
    syn match routWarn	"^Åtvaring.*"
endif

if v:lang =~ "^pl"
    syn match routError	"^BŁĄD.*"
    syn match routError	"^Błąd.*"
    syn match routWarn	"^Ostrzeżenie.*"
endif

if v:lang =~ "^pt_BR"
    syn match routError	"^Erro.*"
    syn match routWarn	"^Aviso.*"
endif

if v:lang =~ "^ru"
    syn match routError	"^Ошибка.*"
    syn match routWarn	"^Предупреждение.*"
endif

if v:lang =~ "^tr"
    syn match routError	"^Hata.*"
    syn match routWarn	"^Uyarı.*"
endif

" Define the default highlighting.
if g:Rout_more_colors == 0
    hi def link routInput	Comment
endif

hi def link routConceal	Ignore
if exists("g:rout_follow_colorscheme") && g:rout_follow_colorscheme
    " Default when following :colorscheme
    hi def link routNormal	Normal
    hi def link routNumber	Number
    hi def link routInteger	Number
    hi def link routFloat	Float
    hi def link routComplex	Number
    hi def link routNegNum	Number
    hi def link routNegFlt	Float
    hi def link routDate	Number
    hi def link routTrue	Boolean
    hi def link routFalse	Boolean
    hi def link routInf  	Number
    hi def link routConst	Constant
    hi def link routString	String
    hi def link routError	Error
    hi def link routWarn	WarningMsg
    hi def link routIndex	Special
    hi def link routStdErr	Function
    hi def link routError	ErrorMsg
    hi def link routWarn	WarningMsg
else
    if &t_Co == 256
        " Defalt 256 colors scheme for R output:
        hi routInput	ctermfg=247
        hi routNormal	ctermfg=40
        hi routNumber	ctermfg=214
        hi routInteger	ctermfg=214
        hi routFloat	ctermfg=214
        hi routComplex	ctermfg=214
        hi routNegNum	ctermfg=209
        hi routNegFlt	ctermfg=209
        hi routDate	ctermfg=179
        hi routFalse	ctermfg=203
        hi routTrue	ctermfg=78
        hi routInf      ctermfg=39
        hi routConst	ctermfg=35
        hi routString	ctermfg=85
        hi routStdErr	ctermfg=117
        hi routError	ctermfg=15 ctermbg=1
        hi routWarn	ctermfg=1
        hi routIndex	ctermfg=109
    else
        " Defalt 16 colors scheme for R output:
        hi routInput	ctermfg=gray
        hi routNormal	ctermfg=darkgreen
        hi routNumber	ctermfg=darkyellow
        hi routInteger	ctermfg=darkyellow
        hi routFloat	ctermfg=darkyellow
        hi routComplex	ctermfg=darkyellow
        hi routNegNum	ctermfg=darkyellow
        hi routNegFlt	ctermfg=darkyellow
        hi routDate	ctermfg=darkyellow
        hi routInf	ctermfg=darkyellow
        hi routFalse	ctermfg=magenta
        hi routTrue	ctermfg=darkgreen
        hi routConst	ctermfg=magenta
        hi routString	ctermfg=darkcyan
        hi routStdErr	ctermfg=cyan
        hi routError	ctermfg=white ctermbg=red
        hi routWarn	ctermfg=red
        hi routIndex	ctermfg=darkgreen
    endif

    " Change colors under user request:
    if exists("g:rout_color_input")
        exe "hi routInput ctermfg=" . g:rout_color_input
    endif
    if exists("g:rout_color_normal")
        exe "hi routNormal ctermfg=" . g:rout_color_normal
    endif
    if exists("g:rout_color_number")
        exe "hi routNumber ctermfg=" . g:rout_color_number
    endif
    if exists("g:rout_color_integer")
        exe "hi routInteger ctermfg=" . g:rout_color_integer
    endif
    if exists("g:rout_color_float")
        exe "hi routFloat ctermfg=" . g:rout_color_float
    endif
    if exists("g:rout_color_complex")
        exe "hi routComplex ctermfg=" . g:rout_color_complex
    endif
    if exists("g:rout_color_negnum")
        exe "hi routNegNum ctermfg=" . g:rout_color_negnum
    endif
    if exists("g:rout_color_negfloat")
        exe "hi routNegFlt ctermfg=" . g:rout_color_negfloat
    endif
    if exists("g:rout_color_date")
        exe "hi routDate ctermfg=" . g:rout_color_date
    endif
    if exists("g:rout_color_false")
        exe "hi routFalse ctermfg=" . g:rout_color_false
    endif
    if exists("g:rout_color_true")
        exe "hi routTrue ctermfg=" . g:rout_color_true
    endif
    if exists("g:rout_color_inf")
        exe "hi routInf ctermfg=" . g:rout_color_inf
    endif
    if exists("g:rout_color_constant")
        exe "hi routConst ctermfg=" . g:rout_color_constant
    endif
    if exists("g:rout_color_string")
        exe "hi routString ctermfg=" . g:rout_color_string
    endif
    if exists("g:rout_color_stderr")
        exe "hi routStdErr ctermfg=" . g:rout_color_stderr
    endif
    if exists("g:rout_color_error")
        exe "hi routError ctermfg=" . g:rout_color_error
    endif
    if exists("g:rout_color_warn")
        exe "hi routWarn ctermfg=" . g:rout_color_warn
    endif
    if exists("g:rout_color_index")
        exe "hi routIndex ctermfg=" . g:rout_color_index
    endif
endif

let   b:current_syntax = "rout"

" vim: ts=8 sw=4
syntax/rrst.vim	[[[1
47
" reStructured Text with R statements
" Language: reST with R code chunks
" Maintainer: Alex Zvoleff, azvoleff@mail.sdsu.edu
" Last Change: Wed Jul 09, 2014  10:29PM
"
" CONFIGURATION:
"   To highlight chunk headers as R code, put in your vimrc:
"   let rrst_syn_hl_chunk = 1

" for portability
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" load all of the rst info
runtime syntax/rst.vim
unlet b:current_syntax

" load all of the r syntax highlighting rules into @R
syntax include @R syntax/r.vim

setlocal iskeyword=@,48-57,_,.

" highlight R chunks
if exists("g:rrst_syn_hl_chunk")
  " highlight R code inside chunk header
  syntax match rrstChunkDelim "^\.\. {r" contained
  syntax match rrstChunkDelim "}$" contained
else
  syntax match rrstChunkDelim "^\.\. {r .*}$" contained
endif
syntax match rrstChunkDelim "^\.\. \.\.$" contained
syntax region rrstChunk start="^\.\. {r.*}$" end="^\.\. \.\.$" contains=@R,rrstChunkDelim keepend transparent fold

" also highlight in-line R code
syntax match rrstInlineDelim "`" contained
syntax match rrstInlineDelim ":r:" contained
syntax region rrstInline start=":r: *`" skip=/\\\\\|\\`/ end="`" contains=@R,rrstInlineDelim keepend

hi def link rrstChunkDelim Special
hi def link rrstInlineDelim Special

let b:current_syntax = "rrst"

" vim: ts=8 sw=2
