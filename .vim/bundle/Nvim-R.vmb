" Vimball Archiver by Charles E. Campbell
UseVimball
finish
doc/Nvim-R.txt	[[[1
2744
*Nvim-R.txt*                                                  *Nvim-R*
				  Nvim-R~
			  Plugin to work with R~

Authors: Jakson A. Aquino   <jalvesaq@gmail.com>
         Jose Claudio Faria <joseclaudio.faria@gmail.com>

Version: 0.9.6
For Neovim >= 0.1.4 and Vim >= 7.4.1829

 1. Overview                                    |Nvim-R-overview|
 2. Main features                               |Nvim-R-features|
 3. Installation                                |Nvim-R-installation|
 4. Use                                         |Nvim-R-use|
 5. Known bugs and workarounds                  |Nvim-R-known-bugs|
 6. Options                                     |Nvim-R-options|
 7. Custom key bindings                         |Nvim-R-key-bindings|
 8. License and files                           |Nvim-R-files|
 9. FAQ and tips                                |Nvim-R-tips|
10. News                                        |Nvim-R-news|


==============================================================================
							     *Nvim-R-overview*
1. Overview~

This plugin improves the support for editing R code with both Vim and Neovim,
integrating them with R. It is the successor of Vim-R-plugin which may be
discontinued in the future. Stable versions of this plugin are available
available at:

    https://github.com/jalvesaq/Nvim-R/releases

Feedback is welcomed. Please submit bug reports to the developers. Do not like
a feature? Tell us and we may add an option to disable it. If you have any
comments or questions, please post them at:

    https://groups.google.com/forum/#!forum/vim-r-plugin

The plugin should emit useful warnings if you do things it was not programmed
to deal with. Cryptic error message are bugs... Please report them at:

    https://github.com/jalvesaq/Nvim-R/issues

Patches and git pull requests are welcome. If you want a feature that only few
people might be interested in, you can write a script to be sourced by the
Nvim-R (see |R_source|).


==============================================================================
							     *Nvim-R-features*
2. Main features~

  * Syntax highlighting for R code, including:
      - Special characters in strings.
      - Functions of loaded packages.
      - Special highlighting for R output (.Rout files).
      - Spell check only strings and comments.
      - Fold code when foldmethod=syntax.
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
  * Ability to see R's documentation in an editor buffer:
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


==============================================================================
							 *Nvim-R-installation*
3. Installation~

The installation process is described in four sections:

   3.1. Installation of dependencies
   3.2. Installation of the plugin
   3.3. Troubleshooting
   3.4. Optional steps

------------------------------------------------------------------------------
3.1. Installation of dependencies~

Before installing the plugin, you should install its dependencies:

Main dependencies:~

   Neovim >= 0.1.4:
      https://github.com/neovim/neovim/releases
      See also: https://github.com/neovim/neovim/wiki/Installing-Neovim

   or Vim >= 7.4.1829:
      http://www.vim.org/download.php
      Vim must be compiled with |+channel|, |+job| and |+conceal| features.
      In Normal mode, do `:version` to check if your Vim has these features.


   R >= 3.0.0:
      http://www.r-project.org/

   Notes about the R package `nvimcom`:

     - The R package `nvimcom` is included in Nvim-R and is automatically
       installed and updated whenever necessary. The package requires
       compilation by a C compiler (e.g. `gcc` or `clang`).

     - You do not need to load nvimcom in your .Rprofile because the Nvim-R
       plugin sets the environment variable `R_DEFAULT_PACKAGES`, including
       `nvimcom` in the list of packages to be loaded on R startup. However,
       if you set the option `defaultPackages` in a .Rprofile, you should
       include "nvimcom" among the packages to be loaded (see
       |nvimcom-not-loaded|).

     - On Windows, you have to install Rtools to be able to build the package:
       https://cran.r-project.org/bin/windows/Rtools/

       Note: I used to release the Windows binary `nvimcom.zip` along with
       Nvim-R, but I am no longer releasing it because my Windows system is
       not being well maintained.


Additional dependencies for editing Rnoweb documents:~

   latexmk:       Automate the compilation of LaTeX documents.
                  See examples in |R_latexcmd|.

   PDF viewer with SyncTeX support and capable of automatically reloading
   documents. This is required only if you edit Rnoweb files.
      On Linux and other Unixes systems: Zathura (recommended), Evince or Okular.
      On OS X: Skim.
      On Windows: SumatraPDF.

   wmctrl:       http://tomas.styblo.name/wmctrl/
                 Required for better SyncTeX support under X Server.
		 (Not required on either Windows or OS X)


Additional suggestions for Unix (Linux, OS X, etc.):~

   ncurses-term: http://invisible-island.net/ncurses
		 Might be necessary if you want support for 256 colors at the
		 terminal emulator.
		 (Not required if using R.app on OS X)

   setwidth:     An R package that can be installed with the command
                 `install.packages("setwidth")`.
                 The library setwidth adjusts the value of `options("width")`
                 whenever the terminal is resized.
		 (Optional if |R_in_buffer| and not required if using R.app)
		 You should put in your Rprofile: `library(setwidth)`

   colorout:     https://github.com/jalvesaq/colorout/releases
                 Colorizes the R output in terminal emulators.
		 (Optional if |R_in_buffer| and not required if using R.app)
		 You should put in your Rprofile: `library(colorout)`

   Tmux >= 2.0:  http://tmux.sourceforge.net
		 Tmux is required only if you want to run R in an external
		 terminal emulator (see |R_in_buffer|) or if you are using
		 Vim.

You may want to improve the configuration of your |vimrc| for a better use of
the plugin. You need to activate plugins and indentation according to
'filetype'. You should have at least the following options at the top or at
near the very top of your |vimrc| (but below `set` `runtimepath`, if you have
set it):
>
   syntax enable
   filetype plugin on
   filetype indent on
<
Please, see |Nvim-R-vimrc-setup| for more suggestions of configuration.


------------------------------------------------------------------------------
3.2. Installation of the plugin~

Now, install Nvim-R. You have two options: Vimball and Vim Package.


Vimball~

If you want to install from the vimball, download the file Nvim-R.vmb from:

   http://www.vim.org/scripts/script.php?script_id=2628

Then, open the file with either Vim or Neovim and do:
>
   :packadd vimball
   :so %
<
Finally, press the space bar a few time to ensure the installation of all
files.


Vim Package~

If you have a previous Vimball installation, you should uninstall it first:
>
   :packadd vimball
   :RmVimball Nvim-R
<
The Vim package is a zip file released at:

    https://github.com/jalvesaq/Nvim-R/releases

If, for instance, it was saved in the `/tmp` directory, to install it on an
Unix system, you should do for Neovim:
>
   mkdir -p ~/.local/share/nvim/site/pack/R
   cd ~/.local/share/nvim/site/pack/R
   unzip /tmp/Nvim-R_0.9.6.zip
<
The directory for Vim on Unix is `~/.vim/pack/R`.
For Neovim on Windows, it is `~/AppData/Local/nvim/pack/R`.
And, for Vim on Windows, it is `~/vimfiles/pack/R`.
The name of the last subdirectory does not need to be `R`; it might be
anything.

Finally, in Vim (or Neovim) run `:helptags` (adjust the path according to your
system):
>
   :helptags ~/.local/share/nvim/site/pack/R/start/Nvim-R/doc
<
See |packages| for details.


------------------------------------------------------------------------------
3.3. Troubleshooting (if the plugin doesn't work)~

Note: The <LocalLeader> is '\' by default.

The plugin is a |file-type| plugin. It will be active only if you are editing
a .R, .Rnw, .Rd, Rmd, or Rrst file. The menu items will not be visible and the
key bindings will not be active while editing either unnamed files or files
with name extensions other than the mentioned above. If the plugin is active,
pressing <LocalLeader>rf should start R.

Did you see warning messages but they disappeared before you have had time to
read them? Type the command |:messages| in Normal mode to see them again.

If R does not start with the <LocalLeader>rf command and you get an error
message instead, you may want to set the path to the R executable (see
|R_path|).
							  *nvimcom-not-loaded*
If you see the message "The package nvimcom wasn't loaded yet" after starting
R, then Nvim-R could not induce R to load nvimcom. Nvim-R sets the environment
variable `R_DEFAULT_PACKAGES` before starting R. If the variable already
exists, the string ",nvimcom" is appended to it. However, if you set the
option `defaultPackages` in your .Rprofile or in a .Rprofile in the current
directory, the environment variable will be overridden. Thus, if you have to
set the option `defaultPackages`, you should include "nvimcom" among the
packages to be loaded. You might want to include "nvimcom" only if R was
started by Nvim-R, as in the example below:
>
   if(Sys.getenv("NVIMR_TMPDIR") == ""){
       options(defaultPackages = c("utils", "grDevices", "graphics", "stats", "methods"))
   } else {
       options(defaultPackages = c("utils", "grDevices", "graphics", "stats", "methods", "nvimcom"))
   }
<
Did you see the message "nvimcom port not found"? This means that R is not
running, the nvimcom package is not installed (or is installed but is not
loaded), or R was not started by Vim/Neovim.


------------------------------------------------------------------------------
3.4. Optional steps~

Customize the plugin~

Please, read the section |Nvim-R-options|. Emacs/ESS users should also read
|ft-r-indent|.


Install additional plugins~

You may be interested in installing additional general plugins to get
functionality not provided by this file type plugin. Particularly interesting
are vim-signature, csv.vim and snipMate. Please read |Nvim-R-tips| for
details. If you edit Rnoweb files, you may want to try LaTeX-Box for
omnicompletion of LaTeX code (see |Nvim-R-latex-box| for details).


==============================================================================
								  *Nvim-R-use*
4. Use~

On Windows, by default, Nvim-R will run Rgui. On other systems, by default, it
will run R in a Neovim's built in terminal.

It is also possible to run R either in an external terminal emulator (see
|R_in_buffer|).


4.1. Key bindings~

Note: The <LocalLeader> is '\' by default.

Note: It is recommended the use of different keys for <Leader> and
<LocalLeader> to avoid clashes between filetype plugins and general plugins
key binds. See |filetype-plugins|, |maplocalleader| and |Nvim-R-localleader|.

To use the plugin, open a .R, .Rnw, .Rd, .Rmd or .Rrst file with Vim and
type <LocalLeader>rf. Then, you will be able to use the plugin key bindings to
send commands to R.

This plugin has many key bindings, which correspond with menu entries. In the
list below, the backslash represents the <LocalLeader>. Not all menu items and
key bindings are enabled in all filetypes supported by the plugin (r, rnoweb,
rhelp, rrst, rmd).

Menu entry                                Default shortcut~
Start/Close
  . Start R (default)                                  \rf
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
  . Selection (evaluate and insert output in new tab)  \so
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
  . View data.frame (cur)                              \rv
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
  . Knit, BibTeX and PDF (cur file) (Linux/Unix)       \kb
  . Knit and PDF (cur file)                            \kp
  . Knit and Beamer PDF (cur file)                     \kl
  . Knit and HTML (cur file, verbose)                  \kh
  . Knit and ODT (cur file)                            \ko
  . Knit and Word Document (cur file)                  \kw
  . Markdown render (cur file)                         \kr
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

Please see |Nvim-R-key-bindings| to learn how to customize the key bindings
without editing the plugin directly.

The plugin commands that send code to R Console are the most commonly used. If
the code to be sent to R has a single line it is sent directly to R Console,
but if it has more than one line (a selection of lines, a block of lines
between two marks, a paragraph etc) the lines are written to a file and the
plugin sends to R the command to source the file. To send to R Console the
line currently under the cursor you should type <LocalLeader>d. If you want to
see what lines are being sourced when sending a selection of lines, you can
use either <LocalLeader>se or <LocalLeader>sa instead of <LocalLeader>ss.

The command <LocalLeader>o runs in the background the R command `print(line)`,
where `line` is the line under cursor, and adds the output as commented lines
to the source code.

If the cursor is over the header of an R chunk with the `child` option (from
Rnoweb, RMarkdown or RreStructuredText document), and you use one of the
commands that send a single line of code to R, then the plugin will send to R
the command to knit the child document.

After the commands that send, sweave or knit the current buffer, Vim will
save the current buffer if it has any pending changes before performing the
tasks. After <LocalLeader>ao, Vim will run "R CMD BATCH --no-restore
--no-save" on the current file and show the resulting .Rout file in a new tab.
Please see |R_routnotab| if you prefer that the file is open in a new split
window. Note: The command <LocalLeader>ao, silently writes the current buffer
to its file if it was modified and deletes the .Rout file if it exists.

R syntax uses " <- " to assign values to variables which is inconvenient to
type. In insert mode, typing a single underscore, "_", will write " <- ",
unless you are typing inside a string. The replacement will always happen if
syntax highlighting is off (see |:syn-on| and |:syn-off|). If necessary, it is
possible to insert an actual underscore into your file by typing a second
underscore. This behavior is similar to the EMACS ESS mode some users may be
familiar with and is enabled by default. You have to change the value of
|R_assign| to disable underscore replacement.

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
To get help on an R topic, you can also type in Vim (Normal mode):
>
   :Rhelp topic
<
The command may be abbreviated to `:Rh` and you can either press <Tab> to
trigger the autocompletion of R objects names or hit CTRL-D to list the
possible completions (see |cmdline-completion| for details on the various ways
of getting command-line completion). The list of objects used for completion
is the same available for omnicompletion (see |R_start_libs|). You may close
the R documentation buffer by simply pressing `q`.

If the object under the cursor is a data.frame or a matrix, <LocalLeader>rv
will show it in a new tab. If the csv.vim plugin is not installed, Nvim-R
will warn you about that (see |Nvim-R-df-view|). Specially useful commands
from the csv.vim plugin are |:CSVHeader|, |:ArrangeColumn| and |:CSVHiColumn|.
It can be installed from:

   http://www.vim.org/scripts/script.php?script_id=2830

When completing object names (CTRL-X CTRL-O) and function arguments (CTRL-X
CTRL-A) you have to press CTRL-N to go foward in the list and CTRL-P to go
backward (see |popupmenu-completion|). For rnoweb, rmd and rrst file types,
CTRL-X CTRL-A can also be used to complete knitr chunk options if the cursor
is inside the chunk header.

Note: If you are using Vim in a Tmux session, and if you configured CTRL-A
to be the Tmux prefix key, Tmux will capture the CTRL-A command. You have to
do CTRL-A twice to pass a single CTRL-A to Vim.

If R is not running or if it is running but is busy the completion will be
based on information from the packages listed by |R_start_libs|
(provided that the libraries were loaded at least once during a session of
Nvim-R usage). Otherwise, the pop up menu for completion of function
arguments will include an additional line with the name of the library where
the function is (if the function name can be found in more than one library)
and the function method (if what is being shown are the arguments of a method
and not of the function itself). For library() and require(), when completing
the first argument, the popup list shows the names of installed packages, but
only if R is running.

You can source all .R files in a directory with the Normal mode command
:RSourceDir, which accepts an optional argument (the directory to be sourced).
								     *Rinsert*
The command  `:Rinsert` <cmd>  inserts one or more lines with the output of the
R command sent to R. By using this command we can avoid the need of copying
and pasting the output R from its console to Vim. For example, to insert
the output of `dput(levels(var))`, where `var` is a factor vector, we could do
in Vim:
>
   :Rinsert dput(levels(var))
<
The output inserted by `:Rinsert` is limited to 5012 characters.

The command `:Rformat` calls the function `tidy_source()` of formatR package
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

You can also press <LocalLeader>gt to go the corresponding line in the
generated .tex file (if SyncTeX is enabled).

The commands <LocalLeader>cc, ce, cd and ca send the current chunk of R code
to R Console. The command <LocalLeader>ch sends the R code from the first
chunk up to the current line.

The commands <LocalLeader>kn builds the .tex file from the Rnoweb one using
the knitr package and <LocalLeader>kp compiles the pdf; for Sweave, the
commands are, respectively <LocalLeader>sw and <LocalLeader>sp. You can jump
from the Rnoweb file to the PDF (forward search) with the command
<LocalLeader>gp. The command to jump from a specific location in the PDF to
the corresponding line in the Rnoweb (backward search) is specific to each pdf
viewer:

   Zathura: <C-LeftMouse>
   Evince:  <C-LeftMouse>
   Okular:  <S-LeftMouse>
   Skim:    <S-Cmd-Click>
   Sumatra: <Double-click>

In any case, the pdf viewer must be started by the Nvim-R plugin. See
|Nvim-R-SyncTeX| for details.


------------------------------------------------------------------------------
4.3. Omni completion and the highlighting of functions~

The plugin adds some features to the default syntax highlight of R code. One
such feature is the highlight of R functions. However, functions are
highlighted only if their libraries are loaded by R (but see
|R_start_libs|).

Note: If you have too many loaded packages Vim may be unable to load the
list of functions for syntax highlight.

Vim can automatically complete the names of R objects when CTRL-X CTRL-O is
pressed in insert mode (see |omni-completion| for details). Omni completion
shows in a pop up menu the name of the object, its class and its environment
(most frequently, its package name). If the object is a function, the plugin
can also show the function arguments in a separate preview window (this
feature is disabled by default: see |R_show_args|).

If a data.frame is found, while building the list of objects, the columns in
the data.frame are added to the list. When you try to use omni completion to
complete the name of a data.frame, the columns are not shown. But when the
data.frame name is already complete, and you have inserted the '$' symbol,
omni completion will show the column names.

Only the names of objects in .GlobalEnv and in loaded libraries are completed.
If R is not running, only objects of libraries listed in |R_start_libs| will
have their names completed. When you load a new library in R, only the current
buffer has the highlighting of function names immediately updated. If you have
other buffers open, they will be updated when you enter them.

Vim uses one file to store the names of .GlobalEnv objects and a list of
files for all other objects. The .GlobalEnv list is stored in the
`$NVIMR_TMPDIR` directory and is deleted when you quit Vim. The other files
are stored in the `$NVIMR_COMPLDIR` directory and remain available until you
manually delete them.


------------------------------------------------------------------------------
4.4. The Object Browser~

You have to use <LocalLeader>ro to start the Object Browser. The Object Browser
has two views: .GlobalEnv and Libraries. If you press <Enter> on the first
line of the Object Browser it will toggle the view between the objects in
.GlobalEnv and the currently loaded libraries.

In the .GlobalEnv view, if an object has the attribute "label", it will also
be displayed. For instance, the code below would make the Object Browser
display the variable labels of an imported SPSS dataset:
>
   library(foreign)
   d <- read.spss("/path/to/spss/dataset.sav", to.data.frame = TRUE)
   vlabs <- attr(d, "variable.labels")
   for(n in names(vlabs))
       attr(d[[n]], "label") <- vlabs[[n]]
<
In the Object Browser window, while in Normal mode, you can either press
<Enter> or double click (GVim only) over a data.frame or list to show/hide its
elements (not if viewing the content of loaded libraries). If you are running
R in an environment where the string UTF-8 is part of either LC_MESSAGES or
LC_ALL variables, unicode line drawing characters will be used to draw lines
in the Object Browser. This is the case of most Linux distributions.

In the Libraries view, you can either double click or press <Enter> on a
library name to see its objects. In the Object Browser, the libraries have the
color defined by the PreProc highlighting group. The other objects have
their colors defined by the return value of some R functions. Each line in the
table below shows a highlighting group and the corresponding type of R object:

	 PreProc	libraries
	 Number		numeric
	 String		character
	 Special	factor
	 Boolean	logical
	 Type		list
	 Function	function
	 Statement	s4
	 Comment	promise (lazy load object)

One limitation of the Object Browser is that objects made available by the
command `data()` are only links to the actual objects (promises of lazily
loading the object when needed) and their real classes are not recognized in
the GlobalEnv view. The same problem happens when the `knitr` option
`cache.lazy=TRUE`. However, if you press <Enter> over the name of the object
in the Object Browser, it will be actually loaded by the command (ran in the
background):
>
   obj <- obj
<

------------------------------------------------------------------------------
4.5. Commenting and uncommenting lines~

You can toggle the state of a line as either commented or uncommented by
typing <LocalLeader>xx. The string used to comment the line will be "# ", "##
" or "### ", depending on the values of R_indent_commented and
r_indent_ess_comments (see |R_rcomment_string|).

You can also add the string "# " to the beginning of a line by typing
<LocalLeader>xc and remove it with <LocalLeader>xu. In this case, you can set
the value of R_rcomment_string to control what string will be added
to the beginning of the line. Example:
>
   let R_rcomment_string = "# "
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
								  *RBuildTags*
4.6. Build a tags file to jump to function definitions~

Vim can jump to functions defined in other files if you press CTRL-] over the
name of a function, but it needs a tags file to be able to find the function
definition (see |tags-and-searches|). The command `:RBuildTags` calls the R
functions `rtags()` and `etags2ctags` to build the tags file for the R scripts
in the current directory. Please read |Nvim-R-tagsfile| to learn how to create
a tags file referencing source code located in other directories, including
the entire R source code.


==============================================================================
							   *Nvim-R-known-bugs*
5. Known bugs and workarounds~

Known bugs that will not be fixed are listed in this section. Some of them can
not be fixed because they depend on missing features in either R or Vim;
others would be very time consuming to fix without breaking anything.


------------------------------------------------------------------------------
5.1. R's source() issues~

The R's `source()` function of the base package prints an extra new line
between commands if the option echo = TRUE, and error and warning messages are
printed only after the entire code is sourced. This makes it more difficult to
find errors in the code sent to R. Details:

   https://stat.ethz.ch/pipermail/r-devel/2012-December/065352.html


------------------------------------------------------------------------------
5.2. The menu may not reflect some of your custom key bindings~

If you have created a custom key binding for Nvim-R, the menu in
GVim will not always reflect the correct key binding if it is not the same for
Normal, Visual and Insert modes.


------------------------------------------------------------------------------
5.3. Functions are not always correctly sent to R~

The plugin is only capable of recognizing functions defined using the `<-`
operator. Also, only current function scope is sent to R. See:

   https://github.com/jcfaria/Vim-R-plugin/issues/94
   https://github.com/jalvesaq/Nvim-R/issues/34


------------------------------------------------------------------------------
5.4. Wrong message that "R is busy" (Windows only)~

On Windows, when code is sent from Vim to R Console, the nvimcom library
sets the value of the internal variable `r_is_busy` to 1. The value is set
back to 0 when any code is successfully evaluated. If you send invalid code to
R, there will be no successful evaluation of code and, thus, the value of
`r_is_busy` will remain set to 1. Then, if you try to update the object
browser, see the R documentation for any function, or do other tasks that
require the hidden evaluation of code by R, the nvimcom library will refuse to
do the tasks to avoid any risk of corrupting R's memory. It will tell Vim
that "R is busy" and Vim will display this message. Everything should work
as expected again after any valid code is executed in the R Console.

The nvimcom library is started with the state `busy`.


------------------------------------------------------------------------------
5.5. R must be started by Vim, and both must be in the same machine~

The communication between Vim and R will work only if R was started by
Vim through the <LocalLeader>rf command because the plugin was designed to
connect each Vim instance with its own R instance.

If you start R before Vim, it will not inherit from Vim the environment
variables NVIMR_TMPDIR, NVIMR_COMPLDIR, NVIMR_ID, and NVIMR_SECRET. The first
one is the path used by the R package nvimcom to save temporary files used by
Nvim-R to: perform omnicompletion, show R documentation in a Vim buffer, and
update the Object Browser. The two last ones are used by Nvim-R and by nvimcom
to know that the connections are valid. Unless you are running R in a Neovim's
built-in terminal (which is the default on Unix), if you use Vim to start R,
but then close Vim, some variables will become outdated. Additionally, Nvim-R
sets the value of its internal variable SendCmdToR from SendCmdToR_fake to the
appropriate value when R is successfully started. It is possible to set the
values of all those variables and start the TCP client-server manually, but it
is not practical to do so.

Moreover, both R and Vim must be in the same machine because the TCP
communication between them is limited to localhost and the communication
through cached and temporary files requires that both R and Vim have access to
the same directories. Please, see the explanation on the communication between
Vim and R at the end of

   https://github.com/jalvesaq/Nvim-R/blob/master/README.md


==============================================================================
							      *Nvim-R-options*
6. Options~

|R_in_buffer|         Run R in Neovim buffer
|R_esc_term|          Map <Esc> to go to Normal mode in the terminal buffer
|R_close_term|        Close terminal buffer after R quited
|R_hl_term|           Syntax highlight terminal as rout file type
|R_term|              External terminal to be used
|R_term_cmd|          Complete command to open an external terminal
|R_silent_term|       Do not show terminal errors
|R_set_home_env|      Set the value of $HOME for R (Windows only)
|R_save_win_pos|      Save positions of R and GVim windows (Windows only)
|R_arrange_windows|   Restore positions of R and GVim windows (Windows only)
|R_assign|            Convert '_' into ' <- '
|R_assign_map|        Choose what to convert into ' <- '
|R_rnowebchunk|       Convert '<' into '<<>>=\n@' in Rnoweb files
|R_objbr_place|       Placement of Object Browser
|R_objbr_w|           Initial width of Object Browser window
|R_objbr_opendf|      Display data.frames open in the Object Browser
|R_objbr_openlist|    Display lists open in the Object Browser
|R_objbr_allnames|    Display hidden objects in the Object Browser
|R_objbr_labelerr|    Show error if "label" attribute is invalid
|R_nvimpager|         Use Vim to see R documentation
|R_open_example|      Use Vim to display R examples
|R_editor_w|          Minimum width of R script buffer
|R_help_w|            Desired width of R documentation buffer
|R_path|              Directory where R is
|R_app|, |R_cmd|        Names of R applications
|R_args|              Arguments to pass to R
|R_start_libs|        Objects for omnicompletion and syntax highlight
|Rout_more_colors|    More syntax highlighting in R output
|R_hi_fun|            Highlight R functions
|R_routnotab|         Show output of R CMD BATCH in new window
|R_indent_commented|  Indent lines commented with the \xx command
|R_rcomment_string|   String to comment code with \xx and \o
|R_notmuxconf|        Don't use a specially built Tmux config file
|R_rconsole_height|   The number of lines of R Console
|R_vsplit|            Split the window vertically when starting R
|R_rconsole_width|    The number of columns of R Console
|R_applescript|       Use osascript in Mac OS X to run R.app
|R_listmethods|       Do `nvim.list.args()` instead of `args()`
|R_specialplot|       Do `nvim.plot()` instead of `plot()`
|R_source_args|       Arguments to R `source()` function
|R_commented_lines|   Include commented lines in code sent to `source()`
|R_latexcmd|          Command to run on .tex files
|R_latexmk|           Define wether `latexmk` should be run
|R_texerr|            Show a summary of LaTeX errors after compilation
|R_sweaveargs|        Arguments do `Sweave()`
|R_rmd_environment|   Environment in which to save evaluated rmd code
|R_never_unmake_menu| Do not unmake the menu when switching buffers
|R_ca_ck|             Add ^A^K to the beginning of commands
|R_pdfviewer|         PDF application used to open PDF documents
|R_openpdf|           Open PDF after processing rnoweb file
|R_openhtml|          Open HTML after processing either Rrst or Rmd
|R_strict_rst|        Code style for generated rst files
|R_insert_mode_cmds|  Allow R commands in insert mode
|R_allnames|          Show names which begin with a dot
|R_rmhidden|          Remove hidden objects from R workspace
|R_source|            Source additional scripts
|R_show_args|         Show extra information during omnicompletion
|R_args_in_stline|    Set 'statusline' to function arguments
|R_nvimcom_wait|      Time to wait for nvimcom loading
|R_nvim_wd|           Start R in Vim's working directory
|R_after_start|       System command to execute after R startup
|R_user_maps_only|    Only set user specified key bindings
|R_tmpdir|            Where temporary files are created
|R_compldir|          Where lists for omnicompletion are stored
|Nvim-R-df-view|      Options for visualizing a data.frame or matrix
|Nvim-R-SyncTeX|      Options for SyncTeX


------------------------------------------------------------------------------
								*R_esc_term*
								*R_close_term*
								*R_hl_term*
								*R_setwidth*
								*R_in_buffer*
6.1. R in Neovim buffer~

By default, R runs in a Neovim buffer created with the command |:term|, and
the <Esc> key is mapped to stop the Terminal mode and go to Normal mode.
Nvim-R sets the option "editor" to a function that makes the object to be
edited in a new tab when `R_esc_term` = 1 (the default value).

If you want to set you own external editor, set the option "editor" after
loading nvimcom in your Rprofile. Example:
>
   library(nvimcom)
   options(editor = "leafpad")
<
If you want to use `vi`, `vim` or `nvim` in Neovim's terminal, put in your
|vimrc|:
>
   let R_esc_term = 0
<
Then, you will have to press the default <C-\><C-N> to go from Terminal to
Normal mode.

Neovim does not close its built-in terminal emulator when the application
running in it quits, but Nvim-R does close it for you. If you rather prefer
that the terminal remains open after R quits, put in your |vimrc|:
>
   let R_close_term = 0
>
You may either use the package colorout (Unix only) to colorize R output or
let Neovim highlight the terminal contents as it was a .Rout file type. Two
advantages of colorout are the possibility of highlighting numbers close to
zero in a different color and the distinction between stdout and stderr. The
value of R_hl_term (0 or 1) determines whether Neovim should syntax highlight
the R output, and its default value will be set to 1 if the package colorout
is not loaded at the moment that you send the first command to R console.
If you prefer do not rely on the auto detection of colorout, you should set
the value of R_hl_term in your |vimrc|. Example:
>
   let R_hl_term = 0
<
Unless the R package setwidth is loaded, before sending a command to R
Console, if the terminal width has changed, Neovim will send to nvimcom the
command `options(width=X)`, where X is the new terminal width. You can either
set the value of R_setwidth to 1 to force the width to be set even if setwidth
is loaded or set it to 0 to avoid the width being set even if setwidth is not
loaded. Example:
>
   let R_setwidth = 0
<
Tips:

  - Use the commands CTRL-W H and CTRL-W K to switch the editor and the R
    console orientation (vertical and horizontal). See |window-moving| for a
    complete description of commands.

  - If you want to press `gz` in Normal mode to emulate the <C-a>z Tmux
    command (zoom Window), put the following in your |vimrc|:
>
    " Emulate Tmux ^az
    function ZoomWindow()
        let cpos = getpos(".")
        tabnew %
        redraw
        call cursor(cpos[1], cpos[2])
        normal! zz
    endfunction
    nmap gz :call ZoomWindow()<CR>
<
    Then, you can open the current buffer in a new tab to get it occupying the
    whole screen.

If you do not want to run R in Neovim's built in terminal emulator, you have
to install Tmux >= 1.8, and then put in your |vimrc|:
>
   let R_in_buffer = 0
<
Then, R will start in an external terminal emulator (useful if you use two
monitors and want Neovim and R separate from each other), and Tmux will be
used to send commands from Neovim to R.


------------------------------------------------------------------------------
								      *R_term*
								  *R_term_cmd*
							       *R_silent_term*
6.2. Terminal emulator (Linux/Unix only)~

Note: The options of this section are ignored on Mac OS X, where the command
`open` is called to run the default application used to run shell scripts.

If |R_in_buffer| = 0 and the X Window System is running and Tmux is installed,
then R will run in an external terminal emulator. The plugin uses the first
terminal emulator that it finds in the following list:

    1. gnome-terminal,
    2. konsole,
    3. xfce4-terminal,
    4. Eterm,
    5. (u)rxvt,
    6. aterm,
    7. roxterm,
    8. terminator,
    9. lxterminal
   10. xterm.

If Vim does not select your favorite terminal emulator, you may define it in
your |vimrc| by setting the variable R_term, as shown below:
>
   let R_term = "xterm"
<
If your terminal emulator is not listed above, or if you are not satisfied
with the way your terminal emulator is called by the plugin, you may define in
your |vimrc| the variable R_term_cmd, as in the examples below:
>
   let R_term_cmd = "xterm -title R -e"
   let R_term_cmd = "xfce4-terminal --icon=/path/to/icons/R.png --title=R -x"
<
Please, look at the manual of your terminal emulator to know how to call it.
The last argument must be the one which precedes the command to be executed.

Note: Terminal emulators that require the command to be executed to be quoted
(such as `termit`) are not supported.

The terminal error messages, if any, are shown as warning messages, unless you
put in your |vimrc|:
>
   let R_silent_term = 1
<

------------------------------------------------------------------------------
							      *R_save_win_pos*
							   *R_arrange_windows*
							      *R_set_home_env*
6.3. Windows specific options~

By default, Nvim-R will save the positions of R Console and Vim windows
when you quit R with the <LocalLeader>rq command, and it will restore the
positions of the windows when you start R. If you do not like this behavior,
you can put in your |vimrc|:
>
   let R_save_win_pos = 0
   let R_arrange_windows = 0
<
If you want R and GVim windows always in a specific arrangement, regardless of
their state when you have quited R for the last time, you should arrange them
in the way you want, quit R, change in your |vimrc| only the value of
R_save_win_pos and, finally, quit Vim.

The plugin sets `$HOME` as the Windows register value for "Personal" "Shell
Folders" which is the same value set by R. However, if you have set `$HOME`
yourself with the intention of changing the default value of `$HOME` assumed
by R, you will want to put in your |vimrc|:
>
   let R_set_home_env = 0
<

------------------------------------------------------------------------------
							       *R_rnowebchunk*
							       *R_assign_map*
							       *R_assign*
6.4. Assignment operator and Rnoweb completion of code block~

In Rnoweb files, a `<` is replaced with `<<>>=\n@`. To disable this feature,
put in your |vimrc|:
>
   let R_rnowebchunk = 0
<
While editing R code, `_` is replaced with `<-`. If you want to bind other
keys to be replaced by `<-`, set the value of |R_assign_map| in your
|vimrc|, as in the example below which emulates RStudio behavior (may only
work on GVim):
>
   let R_assign_map = "<M-->"
<
Note: If you are using Vim in a terminal emulator, you have to put in your
|vimrc|:
>
   set <M-->=^[-
   let R_assign_map = "<M-->"
<
where `^[` is obtained by pressing CTRL-V CTRL-[ in Insert mode.

Note: You can't map <C-=>, as StatET does because in Vim only alphabetic
letters can be mapped in combination with the CTRL key.

To completely disable this feature, put in your |vimrc|:
>
   let R_assign = 0
<
If you need to type many object names with underscores, you may want to change
the value R_assign to 2. Then, you will have to type two `_` to get
them converted into `<-`. Alternatively, if the value of R_assign is 3, the
plugin will run the following command in each buffer containing R code (R,
Rnoweb, Rhelp, Rrst, and Rmd):
>
   iabb <buffer> _ <-
<
That is, the underscore will be replaced with the assign operator only if it
is preceded by a space and followed by a non-word character.

------------------------------------------------------------------------------
							    *R_objbr_w*
							    *R_objbr_place*
							    *R_objbr_opendf*
							    *R_objbr_openlist*
							    *R_objbr_allnames*
							    *R_objbr_labelerr*
6.5. Object Browser options~

By default, the Object Browser will be created with 40 columns. The minimum
width of the Object Browser window is 9 columns. You can change the object
browser's default width by setting the value of |R_objbr_w| in your
|vimrc|, as below:
>
   let R_objbr_w = 30
<
Valid values for the Object Browser placement are "script" or "console" and
"right" or "left" separated by a comma. Examples:
>
   let R_objbr_place = "script,right"
   let R_objbr_place = "console,left"
<
Below is an example of setup of some other options in the |vimrc| that
control the behavior of the Object Browser:
>
   let R_objbr_opendf = 1    " Show data.frames elements
   let R_objbr_openlist = 0  " Show lists elements
   let R_objbr_allnames = 0  " Show .GlobalEnv hidden objects
   let R_objbr_labelerr = 1  " Warn if label is not a valid text
<
Objects whose names start with a "." are hidden by default. If you want them
displayed in the Object Browser, set the value of `R_objbr_allnames` to `1`.

When a `data.frame` appears in the Object Browser for the first time, its
elements are immediately displayed, but the elements of a `list` are displayed
only if it is explicitly opened. The options `R_objbr_opendf` and
`R_objbr_openlist` control the initial status (either opened or closed) of,
respectively, `data.frames` and `lists`. The options are ignored for
`data.frames` and `lists` of libraries which are always started closed.

If an object R's workspace has the attribute `"label"`, it is displayed in
Vim's Object Browser. If the `"label"` attribute is not of class
`"character"`, and if  `R_objbr_labelerr` is `1`, an error message is printed
in the Object Browser.


------------------------------------------------------------------------------
							      *R_open_example*
							      *R_nvimpager*
							      *R_editor_w*
							      *R_help_w*
6.6. Vim as pager for R help~

6.6.1. Quick setup~

If you do not want to see R examples in a Vim buffer, put in your |vimrc|:
>
   let R_open_example = 0
<
If you do not want to see R documentation in a Vim buffer, put in your
|vimrc|:
>
   let R_nvimpager = "no"
<
This option can only be set to "no" in the |vimrc|. It will be automatically
changed to a suitable value if it is set to "no" after Vim startup.

If you want to see R documentation in Vim, but are not satisfied with the
way it works, please, read the subsection 6.6.2 below.

------------------------------------------------------------------------------
6.6.2. Details and other options:~

The plugin key bindings will remain active in the documentation buffer, and,
thus, you will be able to send commands to R as you do while editing an R
script. You can, for example, use <LocalLeader>rh to jump to another R help
document.

The valid values of R_nvimpager are:

   "tab"       : Show the help document in a new tab. If there is already a
                 tab with an R help document tab, use it.
                 This is the default if R_in_buffer = 0.
   "vertical"  : Split the window vertically if the editor width is large
                 enough; otherwise, split the window horizontally and attempt
                 to set the window height to at least 20 lines.
                 This is the default if R_in_buffer = 1.
   "horizontal": Split the window horizontally.
   "tabnew"    : Show the help document in a new tab.
   "no"        : Do not show R documentation in Vim.

The window will be considered large enough if it has more columns than
R_editor_w + R_help_w. These variables control the minimum
width of the editor window and the help window, and their default values are,
respectively, 66 and 46. Thus, if you want to have more control over Vim's
behavior while opening R's documentations, you will want to set different
values to some variables in your |vimrc|, as in the example:
>
   let R_editor_w = 80
   let R_editor_h = 60
<

------------------------------------------------------------------------------
								      *R_path*
								      *R_app*
								      *R_cmd*
6.7. R path and application names~

Vim will run the first R executable in the path. You can set an alternative
path to R in your |vimrc| as in the examples:
>
   let R_path = "/path/to/my/preferred/R/version/bin"
   let R_path = "C:\\Program Files\\R\\R-3.3.1\\bin\\i386"
<
On Windows, Vim will try to find the R install path in the Windows Registry.

You can set the path to a different R version for specific R scripts in your
|vimrc|. Example:
>
   autocmd BufReadPre ~/old* let R_path='~/app/R-2.8.1/bin'
<
By default the value of `R_app` is `"R"` on Unix systems (such as Linux and
Mac OS X). On Windows, it is `Rterm.exe` if R is going to run in a Neovim
buffer and `"Rgui.exe"` otherwise. If your R binary has a different name, you
should set the value of `R_app` in your |vimrc|.

By default the plugin will call the application `R` to run the following
commands:
>
   R CMD build nvimcom
   R CMD install nvimcom
   R CMD BATCH current_script.R
<
If it is necessary to call a different application to run the above commands
in your system, you should set the value of `R_cmd` in your |vimrc|.


------------------------------------------------------------------------------
								      *R_args*
6.8. Arguments to R~

Set this option in your |vimrc| if you want to pass command line arguments to
R at the startup. The value of this variable must be a |List|. Example:
>
   let R_args = ['--no-save', '--quiet']
<
On Linux, there is no default value for |R_args|. On Windows, the default
value is ['--sdi'], but you may change it to ['--mdi'] if you do not like the
SDI style of the graphical user interface.


------------------------------------------------------------------------------
								*R_start_libs*
6.9. Omnicompletion and syntax highlight of R functions~

The list of functions to be highlighted and the list of objects for
omnicompletion are built dynamically as the libraries are loaded by R.
However, you can set the value of R_start_libs if you want that
the functions and objects of specific packages are respectively highlighted
and available for omnicompletion even if R is not running yet. By default,
only the functions of vanilla R are always highlighted. Below is the default
value of R_start_libs:
>
   let R_start_libs = "base,stats,graphics,grDevices,utils,methods"
<

------------------------------------------------------------------------------
							    *Rout_more_colors*
							    *R_hi_fun*
6.10. Syntax highlight: functions and .Rout files~

By default, the R commands in .Rout files are highlighted with the color of
comments, and only the output of commands has some of its elements highlighted
(numbers, strings, index of vectors, warnings and errors).

If you prefer that R commands in the R output are highlighted as they are in R
scripts, put the following in your |vimrc|:
>
   let Rout_more_colors = 1
<
If you want to disable the syntax highlight of R functions put in your
|vimrc|:
>
   let R_hi_fun = 0
<

------------------------------------------------------------------------------
								 *R_routnotab*
6.11. How to automatically open the .Rout file~

After the command <LocalLeader>ao, Vim will save the current buffer if it
has any pending changes, run `R CMD BATCH --no-restore --no-save` on the
current file and show the resulting .Rout file in a new tab. If you prefer
that the file is open in a new split window, put in your |vimrc|:
>
   let R_routnotab = 1
<

------------------------------------------------------------------------------
							  *R_indent_commented*
						          *R_rcomment_string*
6.12. Indent commented lines~

You can type <LocalLeader>xx to comment out a line or selected lines. If the
line already starts with a comment string, it will be removed. After adding
the comment string, the line will be reindented by default. To turn off the
automatic indentation, put in your |vimrc|:
>
   let R_indent_commented = 0
<
The string used to comment text with <LocalLeader>xc, <LocalLeader>xu,
<LocalLeader>xx and <LocalLeader>o is defined by R_rcomment_string.
Example:
>
   let R_rcomment_string = "# "
<
If the value of `r_indent_ess_comments` is 1, `R_rcomment_string` will be
overridden and the string used to comment the line will change according to
the value of `R_indent_commented` ("## " if 0 and "### " if 1; see
|ft-r-indent|).


------------------------------------------------------------------------------
								*R_notmuxconf*
6.13. Tmux configuration (Linux/Unix only)~


If Vim is running R in an external terminal emulator, R will run in a Tmux
session with a specially built Tmux configuration file. If you want to use
your own ~/.tmux.conf, put in your |vimrc|:
>
   let R_notmuxconf = 1
<
If you opted for using your own configuration file, the plugin will write a
minimum configuration which will set the value of four environment variables
required for the communication with R and then source your own configuration
file (~/.tmux.conf).


------------------------------------------------------------------------------
							   *R_vsplit*
							   *R_rconsole_height*
							   *R_rconsole_width*
6.14. Control of R window~

By default, Neovim's window is split horizontally. If you prefer to split it
vertically, put in your |vimrc|:
>
   let R_vsplit = 1
<

You can also set the initial number of lines and columns of the R window:
>
   let R_rconsole_height = 15
   let R_rconsole_width = 40
<
Note: If running R in a Neovim buffer, the number of lines and columns will
automatically change if you switch between horizontal and vertical splits (see
|CTRL-W_K| and |CTRL-W_H|). You may request Neovim to try to keep the minimum
width and height of a specific window by setting the options 'winwidth' and
'winheight'. So, if the window is split horizontally and you want a small R
Console window, you should set a large value for 'winheight' in the script
window.


------------------------------------------------------------------------------
							       *R_applescript*
6.15. Integration with AppleScript (OS X only)~

If you are on Mac OS X and want to use the R.app graphical application, put in
your |vimrc|:
>
   let R_in_buffer = 0
   let R_applescript = 1
<

------------------------------------------------------------------------------
							       *R_listmethods*
							       *R_specialplot*
6.16. Special R functions~

The R function `args()` lists the arguments of a function, but not the
arguments of its methods. If you want that the plugin calls the function
`nvim.list.args()` after <LocalLeader>ra, you have to add to your |vimrc|:
>
   let R_listmethods = 1
<
By default, R makes a scatterplot of numeric vectors. The function
`nvim.plot()` do both a histogram and a boxplot. The function can be called by
the plugin after <LocalLeader>rg if you put the following line in your
|vimrc|:
>
   let R_specialplot = 1
<

------------------------------------------------------------------------------
							       *R_source_args*
							   *R_commented_lines*
6.17. Arguments to R source() function~

When you send multiple lines of code to R (a selection of lines, a paragraph,
code between two marks or an R chunk of code), Nvim-R saves the lines in a
temporary file and, then, sends to R the command `source()` to read the
temporary file.

By default, R's `source()` is called with the argument `print.eval=TRUE`, but
you can change the arguments passed to it. Example:
 >
   let R_source_args = "print.eval = TRUE, max.deparse.length = 300"
<
If you want that commented lines are included in the code to be sourced, put
in your |vimrc|:
>
   let R_commented_lines = 1
<

------------------------------------------------------------------------------
								*R_sweaveargs*
								*R_latexmk*
								*R_latexcmd*
								*R_texerr*
6.18. LaTeX options~

To produce a pdf document from the .tex file generated by either `Sweave()` or
`knit()` command, if R_latexmk = 1 and both `latexmk` and `perl` (which is
required to run `latexmk`) are installed and in the path, the nvimcom package
calls:
>
   latexmk -pdflatex="pdflatex -file-line-error -synctex=1" -pdf
<
Otherwise, it calls:
>
   pdflatex -file-line-error -synctex=1
<
You can use the options R_latexcmd and R_latexmk to change
this behavior. Examples:
>
   let R_latexmk = 0
   let R_latexcmd = "latex"
   let R_latexcmd = 'latexmk -pdf -pdflatex="xelatex %O -synctex=1 %S"'
<
By default, R_latexmk is 1. If you want to pass arguments to the `Sweave()`
function, set the value of the R_sweaveargs variable.

If the value of `R_texerr` is `1`, LaTeX errors and warnings produced by the
compilation of the .tex document into .pdf file will be output to R Console at
the end of the compilation. So, you do not have to scroll the R Console
seeking for these messages.


------------------------------------------------------------------------------
							   *R_rmd_environment*
6.19. Rmd environment~

When rendering an Rmd file, the code can be evaluated (and saved) in a
specified environment.  The default value is `.GlobalEnv` which makes the
objects stored in the Rmd file available on the R console.  If you do not want
the  objects stored in the Rmd file to be available in the global environment,
you can set
>
    let R_rmd_environment = "new.env()"
<

------------------------------------------------------------------------------
							 *R_never_unmake_menu*
6.20. Never unmake the R menu~

Use this option if you want that the "R" menu item in GVim is not deleted when
you change from one buffer to another, for example, when going from a .R file
to a .txt one:
>
   let R_never_unmake_menu = 1
<
When this option is enabled all menu items are created regardless of the file
type.


------------------------------------------------------------------------------
								     *R_ca_ck*
6.21. Add ^A^K to the beginning of commands~

When one types <C-a> in the R Console the cursor goes to the beginning of the
line and when one types <C-k> the characters to the right of the cursor are
deleted. This is useful to avoid characters left on the R Console being mixed
with commands sent by Vim. However, sending <C-a> may be problematic if
using Tmux. The Nvim-R will add <C-a><C-k> to every command if you put in your
|vimrc|:
>
   let R_ca_ck = 1
<

------------------------------------------------------------------------------
								 *R_pdfviewer*
								 *R_openpdf*
								 *R_openhtml*
6.22. Open PDF after processing rnoweb, rmd or rrst files~

The plugin can automatically open the pdf file generated by pdflatex, after
either `Sweave()` or `knit()`. This behavior is controlled by the variable
|R_openpdf| whose value may be 0 (do not open the pdf), 1 (open only
the first time that pdflatex is called) or a number higher than 1 (always
open the pdf). For example, if you want that the pdf application is started
automatically but do not want the terminal (or GVim) losing focus every time
that you generate the pdf, you should put in put in your |vimrc|:
>
   let R_openpdf = 1
<
If you use Linux or other Unix and eventually use the system console (without
the X server) you may want to put in your |vimrc|:
>
   if $DISPLAY != ""
       let R_openpdf = 1
   endif
<
The default value of `R_openpdf` is 1 on Mac OS X and 2 on other systems.

On Windows, Nvim-R will call Sumatra to open the PDF, and, on Mac OS X, it
will call Skim. On Linux, you can change the value of R_pdfviewer in your
|vimrc| to define what PDF viewer will be called. Valid values are "zathura"
(default, and the one best integrated with Nvim-R), "evince" and "okular".

If editing an Rmd file, you can produce the html result with <LocalLeader>kh
(it may also work with other file types). The html file will be automatically
opened if you put the following in your |vimrc|:
>
   let R_openhtml = 1
<

------------------------------------------------------------------------------
							      *R_rrstcompiler*
							      *R_strict_rst*
							      *R_rst2pdfpath*
							      *R_rst2pdfargs*
6.23. Support to RreStructuredText file~

By default, Nvim-R sends the command `render_rst(strict=TRUE)` to R
before using R's `knit()` function to convert an Rrst file into an rst one. If
you prefer the non strict rst code, put the following in your |vimrc|:
>
   let R_strict_rst = 0
<
You can also set the value of R_rst2pdfpath (the path to rst2pdf
application), R_rrstcompiler (the compiler argument to be passed to R
function knit2pdf), and R_rst2pdfargs (further arguments to be passed
to R function knit2pdf).


------------------------------------------------------------------------------
							  *R_insert_mode_cmds*
6.24. Allow R commands in insert mode~

Nvim-R commands are enabled for Normal mode, but most of them can also be
enabled in Insert mode. However, depending on your <LocalLeader>, this can
make it very difficult to write R packages or Sweave files.  For example, if
<LocalLeader> is set to the `\` character, typing `\dQuote` in a .Rd file
tries to send the command!

If you want to enable commands in Insert mode, add the following to your
|vimrc|:
>
   let R_insert_mode_cmds = 1
<
See also: |Nvim-R-localleader|.


------------------------------------------------------------------------------
								  *R_allnames*
								  *R_rmhidden*
6.25. Show/remove hidden objects~

Hidden objects are not included in the list of objects for omni completion. If
you prefer to include them, put in your |vimrc|:
>
   let g:R_allnames = 1
<
Hidden objects are removed from R workspace when you do <LocalLeader>rm. If
you prefer to remove only visible objects, put in your |vimrc|:
>
   let g:R_rmhidden = 0
<

------------------------------------------------------------------------------
								    *R_source*
6.26. Source additional scripts~

This variable should contain a comma separated list of Vim scripts to be
sourced by Nvim-R. These scripts may provide additional
functionality and/or change the behavior of Nvim-R. If you have such
scripts, put in your |vimrc|:
>
   let R_source = "~/path/to/MyScript.vim,/path/to/AnotherScript.vim"
<
Currently, there is only one script known to extend Nvim-R features:

   Support to the devtools R package~
   https://github.com/mllg/vim-devtools-plugin


------------------------------------------------------------------------------
								 *R_show_args*
							    *R_args_in_stline*
6.28. Function arguments~

If you want that Vim shows a preview window with the function arguments as
you do omnicompletion, put in your |vimrc|:
>
   let R_show_args = 1
<
The preview window is not shown by default because it is more convenient to
run <Ctrl-X><Ctrl-A> to complete the function arguments. The preview window
will be shown only if "preview" is also included in your 'completeopt'.

If you want that function arguments are displayed in Vim's status line when
you insert `(`, put in your |vimrc|:
>
   let R_args_in_stline = 1
<
The status line is restored when you either type `)` or leave Insert mode.
This option is useful only if the window has a status line. See |laststatus|.
If the string with the list of arguments is longer than the status line width,
the list is not displayed completely. This argument is incompatible with any
plugin that changes the status line because it is always restored to the value
that it had at Vim's startup. It is also incompatible with any plugin that
automatically closes parentheses. Functions of .GlobalEnv do not have their
arguments displayed.


------------------------------------------------------------------------------
							      *R_nvimcom_wait*
6.29. Time to wait for nvimcom loading~

The Nvim-R waits 5000 milliseconds for nvimcom package to be loaded during R
startup. It then checks whether you are using the correct version of nvimcom.
If 5000 milliseconds is not enough to your R startup, then set a higher value
for the variable in your |vimrc|. Example:
>
   let R_nvimcom_wait = 10000
<

------------------------------------------------------------------------------
								   *R_nvim_wd*
6.30 Start R in working directory of Vim~

When you are editing an R file (.R, .Rnw, .Rd, .Rmd, .Rrst) and start R, the R
package nvimcom runs the command `setwd()` with the directory of the file
being edited as argument, that is, the R working directory becomes the same
directory of the R file. If you want R's working directory to be the same as
Vim's working directory, put in your |vimrc|:
>
   let R_nvim_wd = 1
<
This option is useful only for those who did not enable 'autochdir'.

If you prefer that the Vim-R-plugin does not set the working directory in any
way, put in |vimrc|:
>
   let R_nvim_wd = -1
<

------------------------------------------------------------------------------
							       *R_after_start*
6.31 System command to be executed after R startup~

If you want that Vim executes a external command right after R startup, set
the value of R_after_start in your |vimrc|.

Nvim-R stores the he environment variable $WINDOWID of the terminal where R is
running as $RCONSOLE. Thus, if you are running R in a external terminal
emulator on Linux, `~/bin` is in your path, and you want to resize and change
the positions of the terminals containing Vim and R, you may create a
script `~/bin/after_R_start` similar to the following:
>
   #!/bin/sh
   wmctrl -i -r $RCONSOLE -e 0,0,200,1200,800
   wmctrl -i -r $WINDOWID -e 0,300,40,1200,800
   wmctrl -i -r $WINDOWID -b remove,maximized_vert,maximized_horz

<
Then, make the script executable, and put in your |vimrc|:
>
   let R_in_buffer = 0
   let R_after_start = "after_R_start"
<
Similarly, on Mac OS X, the script below (developed by songcai) will bring the
focus to Neovim terminal window:
>
   #!/usr/bin/env osascript

   --Raise the Terminal window containing name ".R".
   tell application "Terminal"
     set index of window 1 where name contains ".R" to 1
     delay 0.05
     activate window 1
   end tell

   --Click the title bar of the raised window to bring focus to it.
   tell application "System Events"
     tell process "Terminal"
       click menu item (name of window 1) of menu of menu bar item "Window" of menu bar 1
     end tell
   end tell
<

------------------------------------------------------------------------------
							    *R_user_maps_only*
6.32 Only set key bindings that are user specified~

The Nvim-R sets many default key bindings.  The user can set custom
key bindings (|Nvim-R-key-bindings|).  If you wish Nvim-R to only
set those key-bindings specified by the user, put in your |vimrc|:
>
    let R_user_maps_only = 1
<

------------------------------------------------------------------------------
								  *R_tmpdir*
								  *R_compldir*
6.33 Temporary files directories~

You can change the directories where temporary files are created and
stored by setting in your |vimrc| the values of R_tmpdir and
R_compldir, as in the example below:
>
   let R_tmpdir = "/dev/shm/R_tmp_dir"
   let R_compldir = "~/.cache/Nvim-R"
<
The default paths of these directories depend on the operating system. If you
want to know they are, while editing an R file, do in Normal mode:
>
   :echo g:rplugin_tmpdir
   :echo g:rplugin_compldir
<

------------------------------------------------------------------------------
							      *Nvim-R-df-view*
6.34 View a data.frame or matrix~

The csv.vim plugin helps to visualize and edit csv files, and if it is not
installed, Nvim-R will warn you about that when you do <LocalLeader>rv.
If you do not want to install the csv.vim plugin, put in your |vimrc|:
>
   let R_csv_warn = 0
<
If you rather prefer to see the table in a graphical viewer, you should set
the value of R_csv_app in your |vimrc|. Examples:
>
   let R_csv_app = "localc"
   let R_csv_app = "c:/Program Files (x86)/LibreOffice 4/program/scalc.exe"
<
There is also the option of configuring Nvim-R to run an R command to
display the data. Example:
>
   let R_df_viewer = "relimp::showData(%s, font = 'Courier 14')"
<
The value of R_df_viewer is a string and the substring `%s` is replaced by the
name of the object under the cursor.


------------------------------------------------------------------------------
							      *Nvim-R-SyncTeX*
6.35 SyncTeX support~

SyncTeX is a set of communication systems used by some PDF viewers and by some
text editors which allow users to jump from a specific line in the text editor
to the corresponding line in the PDF viewer and vice-versa. The Nvim-R
has support for the SyncTeX systems of five applications:

   Linux:   Zathura, Evince and Okular
   OS X:    Skim
   Windows: SumatraPDF

On Linux, the application `wmctrl` is required to raise both the PDF viewer
and Vim windows.

To completely disable SyncTeX support, put in your |vimrc|:
>
   let R_synctex = 0
<
You do not have to do anything special in your Rnoweb document if you are
using the knitr package, and are editing single file Rnoweb documents.
Otherwise, keep reading...

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
6.35.1 Okular configuration~

In Okular, do the mouse clicks described below and fill the field "Command"
from "Custom Text Editor" as exemplified:
>
     Settings
     Configure Okular
     Editor
     Dropdown menu: Custom Text Editor
           Command: nclientserver '%f' %l
<
Note: If the PDF document is already open the first time that you jump to it,
and if Okular was not started with the `--unique` argument, another instance
of Okular will be started.


------------------------------------------------------------------------------
6.35.1 Skim configuration~

In Skim, click in the drop down menu and fill the fields:
>
   Skim
   Settings
   Sync
   Preset: Custom
   Command: nclientserver
   Arguments: '%file' %line
<

==============================================================================
							 *Nvim-R-key-bindings*
7. Custom key bindings~

When creating custom key bindings for Nvim-R, it is necessary to
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

Only the custom key bindings for Normal mode are shown in Vim's menu, but
you can type |:map| to see the complete list of current mappings, and below is
the list of the names for custom key bindings (the prefix RE means "echo"; RD,
"cursor down"; RED, both "echo" and "down"):

   Star/Close R~
   RStart
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
   RSendSelAndInsertOutput
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
   RViewDF
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
   RMakePDF   (Sweave)
   RMakePDFK  (Knitr)
   RMakePDFKb (.Rmd, beamer)
   RMakeODT   (.Rmd, Open document)
   RMakeWord  (.Rmd, Word document)
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
you call the RAction function, with an argument such as levels, Vim will
pass the command `levels(gender)` to R, which will show you the levels of the
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
See also: |R_source|.


==============================================================================
								*Nvim-R-files*
8. License and files~

The Nvim-R is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option) any
later version.

The Nvim-R is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available at
http://www.r-project.org/Licenses/

The files released with Nvim-R runtime files are distributed under either Vim
or Neovim license.

The following files are part of the plugin and should be in your ~/.vim or
~/.config/nvim directory after the installation:

doc/Nvim-R.txt
ftdetect/r.vim
ftplugin/r_nvimr.vim
ftplugin/rbrowser.vim
ftplugin/rdoc.vim
ftplugin/rhelp_nvimr.vim
ftplugin/rmd_nvimr.vim
ftplugin/rnoweb_nvimr.vim
ftplugin/rrst_nvimr.vim
syntax/rbrowser.vim
syntax/rdoc.vim
syntax/rout.vim
R/common_buffer.vim
R/common_global.vim
R/flag.vim
R/functions.vim
R/gui_running.vim
R/nvimbuffer.vim
R/osx.vim
R/r.snippets
R/rmd.snippets
R/setcompldir.vim
R/synctex_evince_backward.py
R/synctex_evince_forward.py
R/tmux.vim
R/tmux_split.vim
R/extern_term.vim
R/windows.vim
R/nvimrcom.vim
R/vimrcom.vim
R/nvimcom/DESCRIPTION
R/nvimcom/NAMESPACE
R/nvimcom/NEWS
R/nvimcom/README.md
R/nvimcom/man/etags2ctags.Rd
R/nvimcom/man/nvimcom-package.Rd
R/nvimcom/man/nvim.interlace.rmd.Rd
R/nvimcom/man/nvim.interlace.rnoweb.Rd
R/nvimcom/man/nvim.interlace.rrst.Rd
R/nvimcom/man/nvim.list.args.Rd
R/nvimcom/man/nvim.names.Rd
R/nvimcom/man/nvim.plot.Rd
R/nvimcom/man/nvim.print.Rd
R/nvimcom/man/nvim.srcdir.Rd
R/nvimcom/R/etags2ctags.R
R/nvimcom/R/nvim.bol.R
R/nvimcom/R/nvimcom.R
R/nvimcom/R/nvim.help.R
R/nvimcom/R/nvim.interlace.R
R/nvimcom/R/nvim.print.R
R/nvimcom/R/nvim.srcdir.R
R/nvimcom/R/specialfuns.R
R/nvimcom/src/apps
R/nvimcom/src/install.libs.R
R/nvimcom/src/Makevars
R/nvimcom/src/Makevars.win
R/nvimcom/src/nvimcom.c
R/nvimcom/src/apps/Makefile
R/nvimcom/src/apps/Makefile.win
R/nvimcom/src/apps/nclientserver.c


==============================================================================
								 *Nvim-R-tips*
9. FAQ and tips~

9.1. How to start and quit R automatically?~

If you want that R is started automatically when you start editing an R
script, you should create in |vimrc| auto commands for each R file type that
you usually edit. However, you should try to start R only if it is not already
started. Examples for R and RMarkdown:
>
   autocmd FileType r if string(g:SendCmdToR) == "function('SendCmdToR_fake')" | call StartR("R") | endif
   autocmd FileType rmd if string(g:SendCmdToR) == "function('SendCmdToR_fake')" | call StartR("R") | endif
<
And if you want to quit automatically when quiting Vim, put in your |vimrc|
(replace `"nosave"` with `"save"` if you want to save R's workspace):
>
   autocmd VimLeave * if exists("g:SendCmdToR") && string(g:SendCmdToR) != "function('SendCmdToR_fake')" | call RQuit("nosave") | endif
<

9.2. Is it possible to stop R from within Vim?~

Yes. In Normal mode do `:RStop` and Vim will send SIGINT to R which is the
same signal sent when you press CTRL-C into R's Console.


------------------------------------------------------------------------------
9.3. Html help and custom pager~

If you prefer to see help pages in an html browser, put in your `~/.Rprofile`:
>
   options(help_type = "html")
<
and in your |vimrc| (see |R_nvimpager|):
>
   let R_nvimpager = "no"
<

------------------------------------------------------------------------------
							    *Nvim-R-showmarks*
9.4. How do marked blocks work?~

Vim allows you to put several marks (bookmarks) in buffers (see |mark|). The
most commonly used marks are the lowercase alphabet letters. If the cursor is
between any two marks, the plugin will send the lines between them to R if you
press <LocalLeader>bb. If the cursor is above the first mark, the plugin will
send from the beginning of the file to the mark. If the cursor is below the
last mark, the plugin will send from the mark to the end of the file. The mark
above the cursor is included and the mark below is excluded from the block to
be sent to R. To create a mark, press m<letter> in Normal mode.

We recommended the use of either ShowMarks or vim-signature which show what
lines have marks defined. The plugins are available at:

   http://www.vim.org/scripts/script.php?script_id=152
   https://github.com/kshenoy/vim-signature

------------------------------------------------------------------------------
							     *Nvim-R-snippets*
9.5. Use snipMate~

You probably will want to use the snipMate plugin to insert snippets of code
in your R script. The plugin may be downloaded from:

   http://www.vim.org/scripts/script.php?script_id=2540

The snipMate plugin does not come with snippets for R, but you can copy the
files r.snippets and rmd.snippets that ship with Nvim-R (look at the R
directory) to the snippets directory. The files have only a few snippets, but
they will help you to get started. If you usually edit rnoweb files, you may
also want to create an rnoweb.snippets by concatenating both tex.snippets and
r.snippets. If you edit R documentation, you may want to create an
rhelp.snippets


------------------------------------------------------------------------------
							     *Nvim-R-bindings*
9.6. Easier key bindings for most used commands~

The most used commands from Nvim-R probably are "Send line" and "Send
selection". You may find it a good idea to map them to the space bar in your
|vimrc| (suggestion made by Iago Mosqueira):
>
   vmap <Space> <Plug>RDSendSelection
   nmap <Space> <Plug>RDSendLine
<
If you want to press <C-Enter> to send lines to R, see:

   https://github.com/jalvesaq/Nvim-R/issues/64

You may also want to remap <C-x><C-o>:

   http://stackoverflow.com/questions/2269005/how-can-i-change-the-keybinding-used-to-autocomplete-in-vim

Note: Not all mappings work in all versions of Vim. Some mappings may not
work on GVim on Windows, and others may not work on Vim running in a
terminal emulator or in Linux Console. The use of <Shift>, <Alt> and <Fn> keys
in mappings are particularly problematic. See:

   https://github.com/jcfaria/Vim-R-plugin/issues/111


------------------------------------------------------------------------------
							  *Nvim-R-localleader*
9.7. Remap the <LocalLeader>~

People writing Rnoweb documents may find it better to use a comma or other key
as the <LocalLeader> instead of the default backslash (see |maplocalleader|).
For example, to change the <LocalLeader> to a comma, put at the beginning of
your |vimrc| (before any mapping command):
>
   let maplocalleader = ","
<

------------------------------------------------------------------------------
							     *Nvim-R-tagsfile*
9.8. Use a tags file to jump to function definitions~

Vim can jump to a function definition if it finds a "tags" file with the
information about the place where the function is defined. To generate the
tags file, use the R function `rtags()`, which will build an Emacs tags file.
You can use the nvimcom function `etags2ctags()` to convert the Emacs tags
file into a Vim one. To jump to a function definition, put the cursor over
the function name and hit CTRL-]. Please, read |tagsrch.txt| for details on
how to use tags files, specially the section |tags-option|.

You could, for example, download and unpack R's source code, start R inside
the ~/.cache/Nvim-R directory and do the following commands:
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
   autocmd FileType r set tags+=~/.cache/Nvim-R/Rtags,~/.cache/Nvim-R/RsrcTags
   autocmd FileType rnoweb set tags+=~/.cache/Nvim-R/Rtags,~/.cache/Nvim-R/RsrcTags
<
Note: While defining the autocmd, the Rtags path must be put before RsrcTags.

Example on how to test whether your setup is ok:

   1. Type `mapply()` in an R script and save the buffer.
   2. Press CTRL-] over "mapply" (Vim should jump to "mapply.R").
   3. Locate the string "do_mapply", which is the name of a C function.
   4. Press CTRL-] over "do_mapply" (Vim sould jump to "mapply.c").


------------------------------------------------------------------------------
							      *Nvim-R-folding*
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
Notes: (1) Enabling folding may slow down Vim. (2) Folding is not a file
type plugin option. It is a feature defined in syntax/r.vim.

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

Some people want Vim automatically inserting a closing parenthesis, bracket
or brace when an open one has being typed. The page below explains how to
achieve this goal:

   http://vim.wikia.com/wiki/Automatically_append_closing_characters


------------------------------------------------------------------------------
9.12. Automatic line breaks~

By default, while editing R code, Vim does not break lines when you are
typing if you reach the column defined by the 'textwidth' option. If you
prefer that Vim breaks the R code automatically put in your |vimrc|:
>
   autocmd FileType r setlocal formatoptions+=t
<

------------------------------------------------------------------------------
9.13. Vim with 256 colors in a terminal emulator (Linux/Unix only)~

If you want 256 colors support in Vim, install the package ncurses-term.
Then put in your `~/.bashrc` the lines suggested at |Nvim-R-bash-setup|.

You have to search the internet for color schemes supporting 256 colors,
download and copy them to either ~/.config/nvim/colors or ~/.vim. You may use
the command |:colorscheme| to try them one by one before setting your
preference in your |vimrc|.


------------------------------------------------------------------------------
9.14. Run your Makefile from within R~

Do you have many Rnoweb files included in a master tex or Rnoweb file and use
a Makefile to build the pdf? You may consider it useful to put the following
line in your |vimrc|:
>
   nmap <LocalLeader>sm :update<CR>:call g:SendCmdToR('system("make")')<CR>
<

------------------------------------------------------------------------------
							     *Nvim-R-Rprofile*
9.15. Edit your ~/.Rprofile~

You may want to edit your `~/.Rprofile` in addition to considering the
suggestions of |Nvim-R-R-setup| you may also want to put the following
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

The Nvim-R-Plugin does not have debugging facilities, but you may want to use
the R package "debug":
>
   install.packages("debug")
   library(debug)
   mtrace(function_name)
<
Once the library is installed and loaded, you should use
`mtrace(function_name)` to enable the debugging of a function. Then, the next
time that the function is called it will enter in debugging mode. Once
debugging a function, you can hit <Enter> to evaluate the current line,
`go(n)` to go to line `n` in the function and `qqq()` to quit the function
(See debug's help for details). A useful tip is to click on the title bar of
the debug window and choose "Always on top" or a similar option provided by
your desktop manager.


------------------------------------------------------------------------------
							    *Nvim-R-latex-box*
9.17. Integration with LaTeX-Box~

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

See also: |R_latexcmd|.


------------------------------------------------------------------------------
							  *Nvim-R-quick-setup*
9.18. Suggested setup for Nvim-R~

Please, look at section |Nvim-R-options| if you want information about the
Nvim-R customization.

Here are some suggestions of configuration for Vim/Neovim, Bash and R. To
understand what you are doing, and change the configuration to your taste,
please read this document from the beginning.

							 *Nvim-R-vimrc-setup*
   ~/.vimrc or ~/.config/nvim/init.vim or ~\AppData\Roaming\nvim\init.vim~
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
   " specific to the improvement of Nvim-R.

   " Highlight the last searched pattern:
   set hlsearch

   " Show where the next pattern is as you type it:
   set incsearch

   " By default, Vim indents code by 8 spaces. Most people prefer 4
   " spaces:
   set sw=4

   " Search "vim colorscheme 256" in the internet and download color
   " schemes that supports 256 colors in the terminal emulator. Then,
   " uncomment the code below to set you color scheme:
   "colorscheme not_defined
<

							      *Nvim-R-R-setup*
   ~/.Rprofile~
>
   if(interactive()){
       # Suggested libraries:
       library(colorout)
       library(setwidth)

       # Use text based web browser to navigate through R docs after help.start()
       # if you cannot run a graphical web browser (e.g. when you are in the
       # Linux console):
        if(Sys.getenv("DISPLAY") == ""){
            if(Sys.getenv("TMUX") != "")
                options(browser = function(u) system(paste0("tmux new-window 'w3m ", u, "'")))
            else if(Sys.getenv("NVIMR_TMPDIR") != "")
                options(browser = function(u) .C("nvimcom_msg_to_nvim",
                                                 paste0('StartTxtBrowser("w3m", "', u, '")')))
        }
   }
<

							   *Nvim-R-bash-setup*
   ~/.bashrc (Linux, Mac OS X, and other Unix systems):~
>
   # Change the TERM environment variable (to get 256 colors) even if you are
   # accessing your system through ssh and using either Tmux or GNU Screen:
   if [ "$TERM" = "xterm" ] || [ "$TERM" = "xterm-256color" ]
   then
       export TERM=xterm-256color
       export HAS_256_COLORS=yes
   fi
   if [ "$TERM" = "screen" ] && [ "$HAS_256_COLORS" = "yes" ]
   then
       export TERM=screen-256color
   fi
<


Finally, if you want to use vi key bindings in Bash or other shell:

   ~/.inputrc~
>
   set editing-mode vi
   set keymap vi
<

------------------------------------------------------------------------------
								 *rout_colors*
9.19. Syntax highlight of .Rout files~

You can set the both foreground background colors of R output in your |vimrc|.
The example below is for either a gui version of Vim or a terminal
interface of Neovim with `$NVIM_TUI_ENABLE_TRUE_COLOR=1` (see |:highlight|):
>
   if has("gui_running") || $NVIM_TUI_ENABLE_TRUE_COLOR == 1
     let rout_color_input    = 'guifg=#9e9e9e'
     let rout_color_normal   = 'guifg=#ff5f00'
     let rout_color_number   = 'guifg=#ffaf00'
     let rout_color_integer  = 'guifg=#feaf00'
     let rout_color_float    = 'guifg=#fdaf00'
     let rout_color_complex  = 'guifg=#fcaf00'
     let rout_color_negnum   = 'guifg=#d7afff'
     let rout_color_negfloat = 'guifg=#d6afff'
     let rout_color_date     = 'guifg=#00d7af'
     let rout_color_true     = 'guifg=#5dd685'
     let rout_color_false    = 'guifg=#ff5d5e'
     let rout_color_inf      = 'guifg=#10aed7'
     let rout_color_constant = 'guifg=#5fafcf'
     let rout_color_string   = 'guifg=#5fd7af'
     let rout_color_error    = 'guifg=#ffffff guibg=#c40000'
     let rout_color_warn     = 'guifg=#d00000'
     let rout_color_index    = 'guifg=#d0d080'
   endif
<
If you are running Vim with 256 colors, you could use lines like these:
>
   if &t_Co == 256
     let rout_color_input    = 'ctermfg=247'
     let rout_color_normal   = 'ctermfg=39'
     " etc.
   endif
<
You can also set both true colors and 256 colors at the same time:
>
   let rout_color_input    = 'ctermfg=247 guifg=#9e9e9e'
   let rout_color_normal   = 'ctermfg=39 guifg=#ff5f00'
   " etc.
<
To know what number corresponds to your preferred color (among the 256
possibilities), hover you mouse pointer over the table of colors built by:
>
   library("colorout")
   show256Colors()
<
If you prefer that R output is highlighted using you current |:colorscheme|,
put in your |vimrc|:
>
   let rout_follow_colorscheme = 1
<

------------------------------------------------------------------------------
								 *Nvim-R-Tmux*
9.20 Integration with Tmux~

Before Neovim's built in terminal emulator was developed, the best way of
running R was inside a Tmux session. It is still possible to do this, as
explained in this section.

Tmux >= 1.8 is required, and you have to put in your |vimrc|:
>
   let R_in_buffer = 0
   let R_applescript = 0
   let R_tmux_split = 1
<
Then, start Tmux before starting Vim:
>
   tmux
   vim filename.R
   exit
<
In this case, when you start R, the terminal window is split in two regions:
one for Vim and the other for Tmux. Then, it's useful (but not required) to
know some Tmux commands. After you finished editing the file, you have to type
`exit` to quit the Tmux session.


Tmux configuration~

You have to create your `~/.tmux.conf` if it does not exist yet. You may put
the lines below in your `~/.tmux.conf` as a starting point to your own
configuration file:
>
   # Use <C-a> instead of the default <C-b> as Tmux prefix
   set-option -g prefix C-a
   unbind-key C-b
   bind-key C-a send-prefix

   # Options to enable mouse support in Tmux
   set -g terminal-overrides 'xterm*:smcup@:rmcup@'
   # For Tmux < 2.1
   set -g mode-mouse on
   set -g mouse-select-pane on
   set -g mouse-resize-pane on
   # For Tmux >= 2.1
   set -g mouse on

   # Escape time for libtermkey
   # (see https://github.com/neovim/neovim/issues/2035):
   set -sg escape-time 10

   # Act more like vi:
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

Tmux automatically renames window titles to the command currently running.
Nvim-R sets the title of the window where Vim and R are running to "NvimR".
This title will be visible only if Tmux status bar is "on", and it is useful
only if you have created new windows with the <C-a>c command. You can change
the value of R_tmux_title to either set a different title or let Tmux set the
title automatically. Examples:
>
   let R_tmux_title = "Nvim-R"
   let R_tmux_title = "automatic"
<

Key bindings and mouse support~

The Tmux configuration file suggested above configures Tmux to use vi key
bindings. It also configures Tmux to react to mouse clicks. You should be able
to switch the active pane by clicking on an inactive pane, to resize the panes
by clicking on the border line and dragging it, and to scroll the R Console
with the mouse wheel. When you use the mouse wheel, Tmux enters in its
copy/scroll back mode (see below).

The configuration script also sets <C-a> as the Tmux escape character (the
default is <C-b>), that is, you have to type <C-a> before typing a Tmux
command. Below are the most useful key bindings for Tmux with the tmux.conf
shown above:

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
be passed to applications running under Tmux. To send <C-a> to either R or
Vim you have to type <C-a><C-a>.


Copying and pasting~

You do not need to copy code from Vim to R because you can use the plugin's
shortcuts to send the code. For pasting the output of R commands into Vim's
buffer, you can use the command :|Rinsert|. If you want to copy text from an
application running inside the Tmux to another application also running in
Tmux, as explained in the previous subsection, you can enter in Tmux
copy/scroll mode, select the text, copy it, switch to the other application
pane and, then, paste.

However, if you want to copy something from either Vim or R to another
application not running inside Tmux, Tmux may prevent the X server from
capturing the text selected by the mouse. This can be prevented by
pressing the <Shift> key, as it suspends the capturing of mouse events
by tmux. If you keep <Shift> pressed while selecting text with the mouse,
it will be available in the X server clipboard and can be inserted
into other windows using the middle mouse button. It can of course also be
inserted into a tmux window using <Shift> and the middle mouse button.


Remote access~

The Nvim-R can not send commands from a local Vim to a remote R, but
you can access the remote machine through ssh and run Tmux, Vim and R in the
remote machine. Tmux should not be running in the local machine because some
environment variables could pass from the local to the remote Tmux and make
the plugin confuse.

With Tmux, you can detach the Nvim-R session and reattach it later (but the
connection with the XServer could be lost in the process). This is useful if
you plan to begin the use of Nvim-R in a machine and later move to another
computer and access remotely your previous Nvim-R session. Below is the
step-by-step procedure to run Nvim-R remotely:

  - Start Tmux:
      tmux

  - Start Vim:
      vim script.R

  - Use Vim to start an R session:
      <LocalLeader>rf

  - Send code from Vim to R, and, then, detach Vim and R with <C-a>d
    The command will be <C-b>d if you have not set <C-a> as the escape
    character in your ~/.tmux.conf.

  - Some time later (even if accessing the machine remotely) reattach the
    Tmux session:
      tmux attach


==============================================================================
								 *Nvim-R-news*
10. News~

0.9.6 (2016-08-10)

 * New option: R_open_example.
 * Change default value of R_source_args to "print.eval=TRUE".
 * Change in \aa and \ae: do not save the buffer before sending the whole file
   to R.
 * Minor bug fixes.

0.9.5 (2016-05-18)

 * Ask whether R_LIBS_USER directory should be created.

0.9.4 (2016-05-16)

 * Delete option R_tmux_ob. The Object Browser will always start in a Vim
   split window, not in a Tmux split pane.
 * New option: R_cmd
 * Minor bug fixes.
 * Require Neovim >= 0.1.4 or Vim >= 7.4.1829.

0.9.3 (2016-03-26)

 * Build nvimcom even when Nvim-R directory in non-writable.

0.9.2 (2016-03-19)

 * Support Vim.
 * New option: R_app.

0.9.1 (2016-02-28)

 * New option: R_close_term.
 * Delete option: R_restart.

0.9.0 (2015-11-03)

 * Initial release of Nvim-R for Neovim 0.1.0.

vim:tw=78:ts=8:ft=help:norl
ftdetect/r.vim	[[[1
3
autocmd BufNewFile,BufRead *.Rout set ft=rout
autocmd BufNewFile,BufRead *.Rout.save set ft=rout
autocmd BufNewFile,BufRead *.Rout.fail set ft=rout
ftplugin/r_nvimr.vim	[[[1
103

if exists("g:disable_r_ftplugin")
    finish
endif

" Source scripts common to R, Rnoweb, Rhelp, Rmd, Rrst and rdoc files:
runtime R/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rnoweb, Rhelp, Rmd, Rrst and rdoc files
" need be defined after the global ones:
runtime R/common_buffer.vim

function! GetRCmdBatchOutput(...)
    if filereadable(s:routfile)
        let curpos = getpos(".")
        if g:R_routnotab == 1
            exe "split " . s:routfile
            set filetype=rout
            exe "normal! \<c-w>\<c-p>"
        else
            exe "tabnew " . s:routfile
            set filetype=rout
            normal! gT
        endif
    else
        call RWarningMsg("The file '" . s:routfile . "' either does not exist or not readable.")
    endif
endfunction

" Run R CMD BATCH on current file and load the resulting .Rout in a split
" window
function! ShowRout()
    let s:routfile = expand("%:r") . ".Rout"
    if bufloaded(s:routfile)
        exe "bunload " . s:routfile
        call delete(s:routfile)
    endif

    " if not silent, the user will have to type <Enter>
    silent update

    if has("win32")
        let rcmd = g:rplugin_Rcmd . ' CMD BATCH --no-restore --no-save "' . expand("%") . '" "' . s:routfile . '"'
    else
        let rcmd = [g:rplugin_Rcmd, "CMD", "BATCH", "--no-restore", "--no-save", expand("%"),  s:routfile]
    endif
    if has("nvim")
        let g:rplugin_jobs["R_CMD"] = jobstart(rcmd, {'on_exit': function('GetRCmdBatchOutput')})
    else
        let rjob = job_start(rcmd, {'close_cb': function('GetRCmdBatchOutput')})
        let g:rplugin_jobs["R_CMD"] = job_getchannel(rjob)
    endif
endfunction

" Convert R script into Rmd, md and, then, html -- using knitr::spin()
function! RSpin()
    update
    call g:SendCmdToR('require(knitr); .vim_oldwd <- getwd(); setwd("' . expand("%:p:h") . '"); spin("' . expand("%:t") . '"); setwd(.vim_oldwd); rm(.vim_oldwd)')
endfunction

" Default IsInRCode function when the plugin is used as a global plugin
function! DefaultIsInRCode(vrb)
    return 1
endfunction

let b:IsInRCode = function("DefaultIsInRCode")

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
    runtime R/gui_running.vim
    call MakeRMenu()
endif

call RSourceOtherScripts()

if exists("b:undo_ftplugin")
    let b:undo_ftplugin .= " | unlet! b:IsInRCode"
else
    let b:undo_ftplugin = "unlet! b:IsInRCode"
endif
ftplugin/rbrowser.vim	[[[1
339
" Vim filetype plugin file
" Language: R Browser (generated by the Nvim-R)
" Maintainer: Jakson Alves de Aquino <jalvesaq@gmail.com>


" Only do this when not yet done for this buffer
if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

let g:rplugin_upobcnt = 0

let s:cpo_save = &cpo
set cpo&vim

" Source scripts common to R, Rnoweb, Rhelp and rdoc files:
runtime R/common_global.vim

" Some buffer variables common to R, Rnoweb, Rhelp and rdoc file need be
" defined after the global ones:
runtime R/common_buffer.vim

setlocal noswapfile
setlocal buftype=nofile
setlocal nowrap
setlocal iskeyword=@,48-57,_,.
setlocal nolist

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
        let fcntt = readfile(g:rplugin_tmpdir . "/globenv_" . $NVIMR_ID)
    else
        let fcntt = readfile(g:rplugin_tmpdir . "/liblist_" . $NVIMR_ID)
    endif
    call setline(1, fcntt)
    call cursor(curline, curcol)
    if bufname("%") =~ "Object_Browser"
        setlocal nomodifiable
    endif
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
            call SendToNvimcom("\004G RBrowserDoubleClick")
        else
            let g:rplugin_curview = "libraries"
            call SendToNvimcom("\004L RBrowserDoubleClick")
        endif
        return
    endif

    " Toggle state of list or data.frame: open X closed
    let key = RBrowserGetName(0, 1)
    if g:rplugin_curview == "GlobalEnv"
        if getline(".") =~ "&#.*\t"
            call SendToNvimcom("\006&" . key)
        else
            call SendToNvimcom("\006" . key)
        endif
    else
        let key = substitute(key, '`', '', "g")
        if key !~ "^package:"
            let key = "package:" . RBGetPkgName() . '-' . key
        endif
        call SendToNvimcom("\006" . key)
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
        if line =~ " " || line =~ '^.#[0-9]' || line =~ '-'
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

    if word =~ ' ' || word =~ '^[0-9]' || word =~ '-'
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

function! SourceObjBrLines()
    exe "source " . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g') . "/objbrowserInit"
endfunction

function! OnOBBufUnload()
    call SendToNvimcom("\004Stop updating info [OB BufUnload].")
endfunction

nmap <buffer><silent> <CR> :call RBrowserDoubleClick()<CR>
nmap <buffer><silent> <2-LeftMouse> :call RBrowserDoubleClick()<CR>
nmap <buffer><silent> <RightMouse> :call RBrowserRightClick()<CR>

call RControlMaps()

setlocal winfixwidth
setlocal bufhidden=wipe

if has("gui_running")
    runtime R/gui_running.vim
    call RControlMenu()
    call RBrowserMenu()
endif

au BufEnter <buffer> stopinsert
au BufUnload <buffer> call OnOBBufUnload()

let s:envstring = tolower($LC_MESSAGES . $LC_ALL . $LANG)
if s:envstring =~ "utf-8" || s:envstring =~ "utf8"
    let s:isutf8 = 1
else
    let s:isutf8 = 0
endif
unlet s:envstring

call setline(1, ".GlobalEnv | Libraries")

call RSourceOtherScripts()

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=4
ftplugin/rdoc.vim	[[[1
150
" Vim filetype plugin file
" Language: R Documentation (generated by the Nvim-R)
" Maintainer: Jakson Alves de Aquino <jalvesaq@gmail.com>


" Only do this when not yet done for this buffer
if exists("b:did_rdoc_ftplugin")
    finish
endif

" Don't load another plugin for this buffer
let b:did_rdoc_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

" Source scripts common to R, Rnoweb, Rhelp and rdoc files:
runtime R/common_global.vim

" Some buffer variables common to R, Rnoweb, Rhelp and rdoc file need be
" defined after the global ones:
runtime R/common_buffer.vim

setlocal iskeyword=@,48-57,_,.

" Prepare R documentation output to be displayed by Nvim
function! FixRdoc()
    let lnr = line("$")
    for ii in range(1, lnr)
        let lii = getline(ii)
        let lii = substitute(lii, "_\010", "", "g")
        let lii = substitute(lii, '<URL: \(.\{-}\)>', ' |\1|', 'g')
        let lii = substitute(lii, '<email: \(.\{-}\)>', ' |\1|', 'g')
        if &encoding == "utf-8"
            let lii = substitute(lii, "\x91", "‘", 'g')
            let lii = substitute(lii, "\x92", "’", 'g')
        endif
        call setline(ii, lii)
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
                call setline(ii, " \t")
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

"==========================================================================
" Key bindings and menu items

call RCreateSendMaps()
call RControlMaps()

" Menu R
if has("gui_running")
    runtime R/gui_running.vim
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
autocmd VimResized <buffer> let g:R_newsize = 1
call FixRdoc()
autocmd FileType rdoc call FixRdoc()

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=4
ftplugin/rhelp_nvimr.vim	[[[1
91

if exists("g:disable_r_ftplugin")
    finish
endif

" Source scripts common to R, Rnoweb, Rhelp and rdoc files:
runtime R/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rnoweb, Rhelp and rdoc file need be
" defined after the global ones:
runtime R/common_buffer.vim

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

function! RhelpComplete(findstart, base)
    if a:findstart
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && (line[start - 1] =~ '\w' || line[start - 1] == '\')
            let start -= 1
        endwhile
        return start
    else
        let resp = []
        let hwords = ['\Alpha', '\Beta', '\Chi', '\Delta', '\Epsilon',
                    \ '\Eta', '\Gamma', '\Iota', '\Kappa', '\Lambda', '\Mu', '\Nu',
                    \ '\Omega', '\Omicron', '\Phi', '\Pi', '\Psi', '\R', '\Rdversion',
                    \ '\Rho', '\S4method', '\Sexpr', '\Sigma', '\Tau', '\Theta', '\Upsilon',
                    \ '\Xi', '\Zeta', '\acronym', '\alias', '\alpha', '\arguments',
                    \ '\author', '\beta', '\bold', '\chi', '\cite', '\code', '\command',
                    \ '\concept', '\cr', '\dQuote', '\delta', '\deqn', '\describe',
                    \ '\description', '\details', '\dfn', '\docType', '\dontrun', '\dontshow',
                    \ '\donttest', '\dots', '\email', '\emph', '\encoding', '\enumerate',
                    \ '\env', '\epsilon', '\eqn', '\eta', '\examples', '\file', '\format',
                    \ '\gamma', '\ge', '\href', '\iota', '\item', '\itemize', '\kappa',
                    \ '\kbd', '\keyword', '\lambda', '\ldots', '\le',
                    \ '\link', '\linkS4class', '\method', '\mu', '\name', '\newcommand',
                    \ '\note', '\nu', '\omega', '\omicron', '\option', '\phi', '\pi',
                    \ '\pkg', '\preformatted', '\psi', '\references', '\renewcommand', '\rho',
                    \ '\sQuote', '\samp', '\section', '\seealso', '\sigma', '\source',
                    \ '\special', '\strong', '\subsection', '\synopsis', '\tab', '\tabular',
                    \ '\tau', '\testonly', '\theta', '\title', '\upsilon', '\url', '\usage',
                    \ '\value', '\var', '\verb', '\xi', '\zeta']
        for word in hwords
            if word =~ '^' . escape(a:base, '\')
                call add(resp, {'word': word})
            endif
        endfor
        return resp
    endif
endfunction

let b:IsInRCode = function("RhelpIsInRCode")
let b:rplugin_nonr_omnifunc = "RhelpComplete"

"==========================================================================
" Key bindings and menu items

call RCreateStartMaps()
call RCreateEditMaps()
call RCreateSendMaps()
call RControlMaps()
call RCreateMaps("nvi", '<Plug>RSetwd',        'rd', ':call RSetWD()')

" Menu R
if has("gui_running")
    runtime R/gui_running.vim
    call MakeRMenu()
endif

call RSourceOtherScripts()

if exists("b:undo_ftplugin")
    let b:undo_ftplugin .= " | unlet! b:IsInRCode"
else
    let b:undo_ftplugin = "unlet! b:IsInRCode"
endif
ftplugin/rmd_nvimr.vim	[[[1
124

if exists("g:disable_r_ftplugin")
    finish
endif

" Source scripts common to R, Rrst, Rnoweb, Rhelp and Rdoc:
runtime R/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rmd, Rrst, Rnoweb, Rhelp and Rdoc need to
" be defined after the global ones:
runtime R/common_buffer.vim

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

"==========================================================================
" Key bindings and menu items

call RCreateStartMaps()
call RCreateEditMaps()
call RCreateSendMaps()
call RControlMaps()
call RCreateMaps("nvi", '<Plug>RSetwd',        'rd', ':call RSetWD()')

" Only .Rmd files use these functions:
call RCreateMaps("nvi", '<Plug>RKnit',          'kn', ':call RKnit()')
call RCreateMaps("ni",  '<Plug>RSendChunk',     'cc', ':call b:SendChunkToR("silent", "stay")')
call RCreateMaps("ni",  '<Plug>RESendChunk',    'ce', ':call b:SendChunkToR("echo", "stay")')
call RCreateMaps("ni",  '<Plug>RDSendChunk',    'cd', ':call b:SendChunkToR("silent", "down")')
call RCreateMaps("ni",  '<Plug>REDSendChunk',   'ca', ':call b:SendChunkToR("echo", "down")')
call RCreateMaps("n",  '<Plug>RNextRChunk',     'gn', ':call b:NextRChunk()')
call RCreateMaps("n",  '<Plug>RPreviousRChunk', 'gN', ':call b:PreviousRChunk()')

" Menu R
if has("gui_running")
    runtime R/gui_running.vim
    call MakeRMenu()
endif

let g:rplugin_has_pandoc = 0
let g:rplugin_has_soffice = 0

call RSourceOtherScripts()

if exists("b:undo_ftplugin")
    let b:undo_ftplugin .= " | unlet! b:IsInRCode b:PreviousRChunk b:NextRChunk b:SendChunkToR"
else
    let b:undo_ftplugin = "unlet! b:IsInRCode b:PreviousRChunk b:NextRChunk b:SendChunkToR"
endif
ftplugin/rnoweb_nvimr.vim	[[[1
662

if exists("g:disable_r_ftplugin")
    finish
endif

" Source scripts common to R, Rnoweb, Rhelp and Rdoc:
runtime R/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rnoweb, Rhelp and Rdoc need to be defined
" after the global ones:
runtime R/common_buffer.vim

call RSetDefaultValue("g:R_latexmk", 1)
if !exists("g:rplugin_has_latexmk")
    if g:R_latexmk && executable("latexmk") && executable("perl")
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
" let R_rm_knit_cache = 1
"
" If don't want to answer the question about deleting files, and
" if you trust this code more than I do, put in your vimrc:
"
" let R_ask_rm_knitr_cache = 0
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
    if exists("g:R_ask_rm_knitr_cache") && g:R_ask_rm_knitr_cache == 0
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

" Weave and compile the current buffer content
function! RWeave(bibtex, knit, pdf)
    if g:rplugin_nvimcom_port == 0
        call RWarningMsg("The nvimcom package is required to make and open the PDF.")
    endif
    update
    let rnwdir = expand("%:p:h")
    if has("win32")
        let rnwdir = substitute(rnwdir, '\\', '/', 'g')
    endif
    let pdfcmd = 'nvim.interlace.rnoweb("' . expand("%:t") . '", rnwdir = "' . rnwdir . '"'

    if a:knit == 0
        let pdfcmd = pdfcmd . ', knit = FALSE'
    endif

    if a:pdf == 0
        let pdfcmd = pdfcmd . ', buildpdf = FALSE'
    endif

    if g:rplugin_has_latexmk == 0
        let pdfcmd = pdfcmd . ', latexmk = FALSE'
    endif

    if g:R_latexcmd != "default"
        let pdfcmd = pdfcmd . ", latexcmd = '" . g:R_latexcmd . "'"
    endif

    if g:R_synctex == 0
        let pdfcmd = pdfcmd . ", synctex = FALSE"
    endif

    if a:bibtex == "bibtex"
        let pdfcmd = pdfcmd . ", bibtex = TRUE"
    endif

    if a:pdf == 0 || g:R_openpdf == 0 || b:pdf_is_open
        let pdfcmd = pdfcmd . ", view = FALSE"
    endif

    if a:pdf && g:R_openpdf == 1
        let b:pdf_is_open = 1
    endif

    if a:knit == 0 && exists("g:R_sweaveargs")
        let pdfcmd = pdfcmd . ", " . g:R_sweaveargs
    endif

    let pdfcmd = pdfcmd . ")"
    call g:SendCmdToR(pdfcmd)
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

if g:R_rnowebchunk == 1
    " Write code chunk in rnoweb files
    inoremap <buffer><silent> < <Esc>:call RWriteChunk()<CR>a
endif

" Pointers to functions whose purposes are the same in rnoweb, rrst, rmd,
" rhelp and rdoc and which are called at common_global.vim
let b:IsInRCode = function("RnwIsInRCode")
let b:PreviousRChunk = function("RnwPreviousChunk")
let b:NextRChunk = function("RnwNextChunk")
let b:SendChunkToR = function("RnwSendChunkToR")


"==========================================================================
" Key bindings and menu items

call RCreateStartMaps()
call RCreateEditMaps()
call RCreateSendMaps()
call RControlMaps()
call RCreateMaps("nvi", '<Plug>RSetwd',        'rd', ':call RSetWD()')

" Only .Rnw files use these functions:
call RCreateMaps("nvi", '<Plug>RSweave',      'sw', ':call RWeave("nobib", 0, 0)')
call RCreateMaps("nvi", '<Plug>RMakePDF',     'sp', ':call RWeave("nobib", 0, 1)')
call RCreateMaps("nvi", '<Plug>RBibTeX',      'sb', ':call RWeave("bibtex", 0, 1)')
if exists("g:R_rm_knit_cache") && g:R_rm_knit_cache == 1
    call RCreateMaps("nvi", '<Plug>RKnitRmCache', 'kr', ':call RKnitRmCache()')
endif
call RCreateMaps("nvi", '<Plug>RKnit',        'kn', ':call RWeave("nobib", 1, 0)')
call RCreateMaps("nvi", '<Plug>RMakePDFK',    'kp', ':call RWeave("nobib", 1, 1)')
call RCreateMaps("nvi", '<Plug>RBibTeXK',     'kb', ':call RWeave("bibtex", 1, 1)')
call RCreateMaps("nvi", '<Plug>RIndent',      'si', ':call RnwToggleIndentSty()')
call RCreateMaps("ni",  '<Plug>RSendChunk',   'cc', ':call b:SendChunkToR("silent", "stay")')
call RCreateMaps("ni",  '<Plug>RESendChunk',  'ce', ':call b:SendChunkToR("echo", "stay")')
call RCreateMaps("ni",  '<Plug>RDSendChunk',  'cd', ':call b:SendChunkToR("silent", "down")')
call RCreateMaps("ni",  '<Plug>REDSendChunk', 'ca', ':call b:SendChunkToR("echo", "down")')
call RCreateMaps("nvi", '<Plug>ROpenPDF',     'op', ':call ROpenPDF("Get Master")')
if g:R_synctex
    call RCreateMaps("ni",  '<Plug>RSyncFor',     'gp', ':call SyncTeX_forward()')
    call RCreateMaps("ni",  '<Plug>RGoToTeX',     'gt', ':call SyncTeX_forward(1)')
endif
call RCreateMaps("n",  '<Plug>RNextRChunk',     'gn', ':call b:NextRChunk()')
call RCreateMaps("n",  '<Plug>RPreviousRChunk', 'gN', ':call b:PreviousRChunk()')

" Menu R
if has("gui_running")
    runtime R/gui_running.vim
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
    let flnm = substitute(a:fname, '/\./', '/', '')   " Okular
    let basenm = substitute(flnm, "\....$", "", "")   " Delete extension
    if basenm =~ "/"
        let basedir = substitute(basenm, '\(.*\)/.*', '\1', '')
    else
        let basedir = '.'
    endif
    if filereadable(basenm . "-concordance.tex")
        if !filereadable(basenm . ".tex")
            call RWarningMsg('SyncTeX [Nvim-R]: "' . basenm . '.tex" not found.')
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
            call RWarningMsg('SyncTeX [Nvim-R]: "' . basenm . '-concordance.tex" not found.')
            return
        elseif filereadable(flnm)
            let rnwf = flnm
            let rnwln = a:ln
        else
            call RWarningMsg("Could not find '" . basenm . ".Rnw'.")
            return
        endif
    endif

    let rnwbn = substitute(rnwf, '.*/', '', '')
    let rnwf = substitute(rnwf, '^\./', '', '')

    if GoToBuf(rnwbn, rnwf, basedir, rnwln)
        if g:rplugin_has_wmctrl
            if v:windowid != 0
                call system("wmctrl -ia " . v:windowid)
            elseif $WINDOWID != ""
                call system("wmctrl -ia " . $WINDOWID)
            endif
        elseif has("gui_running")
            if has("win32")
                " Attempt 1
                call JobStdin(g:rplugin_jobs["ClientServer"], "\007\n")

                " Attempt 2
                " if has("nvim")
                "     call rpcnotify(0, 'Gui', 'Foreground')
                " else
                "     call foreground()
            else
                call foreground()
            endif
        endif
    endif
endfunction

function! SyncTeX_forward_Zathura(basenm, texln, vrbs)
    if g:rplugin_zathura_pid[a:basenm] == 0
        return 0
    endif
    let result = system("zathura --synctex-forward=" . a:texln . ":1:" . a:basenm . ".tex --synctex-pid=" . g:rplugin_zathura_pid[a:basenm] . " " . a:basenm . ".pdf")
    if v:shell_error
        let g:rplugin_zathura_pid[a:basenm] = 0
        if a:vrbs
            call RWarningMsg(substitute(result, "\n", " ", "g"))
        endif
        return 0
    endif
    return 1
endfunction

" Avoid possible infinite loop if Evince cannot open the document and
" synctex_evince_forward.py keeps sending the message to Neovim run
" SyncTeX_forward() again.
function! Evince_Again()
    let g:rplugin_evince_loop += 1
    call SyncTeX_forward()
endfunction

function! SyncTeX_forward(...)
    let basenm = expand("%:t:r")
    let lnum = 0
    let rnwf = expand("%:t")

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
                call RWarningMsg('Nvim-R [SyncTeX]: "' . basenm . '-concordance.tex" not found.')
                exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
                return
            endif
        else
            call RWarningMsg('SyncTeX [Nvim-R]: "' . basenm . '-concordance.tex" not found.')
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
        if g:R_latexcmd != "default" && g:R_latexcmd !~ "synctex"
            call RWarningMsg('Note: The string "-synctex=1" is not in your R_latexcmd. Please check your vimrc.')
        endif
        exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
        return
    endif

    if g:rplugin_pdfviewer == "zathura"
        if system("wmctrl -xl") =~ 'Zathura.*' . basenm . '.pdf' && g:rplugin_zathura_pid[basenm] != 0
            if SyncTeX_forward_Zathura(basenm, texln, 0) == 0
                " The user closed Zathura and later opened the pdf manually
                call RStart_Zathura(basenm)
                sleep 900m
                call SyncTeX_forward_Zathura(basenm, texln, 1)
            endif
        else
            let g:rplugin_zathura_pid[basenm] = 0
            call RStart_Zathura(basenm)
            sleep 900m
            call SyncTeX_forward_Zathura(basenm, texln, 1)
        endif
        call system("wmctrl -a '" . basenm . ".pdf'")
    elseif g:rplugin_pdfviewer == "okular"
        call system("NVIMR_PORT=" . g:rplugin_myport . " okular --unique " . basenm . ".pdf#src:" . texln . substitute(expand("%:p:h"), ' ', '\\ ', 'g') . "/./" . substitute(basenm, ' ', '\\ ', 'g') . ".tex 2> /dev/null >/dev/null &")
    elseif g:rplugin_pdfviewer == "evince"
        if g:rplugin_evince_loop < 2
            let g:rplugin_jobs["Python (Evince forward)"] = StartJob(["python", g:rplugin_home . "/R/synctex_evince_forward.py",  basenm . ".pdf", string(texln), basenm . ".tex"], g:rplugin_job_handlers)
        else
            let g:rplugin_evince_loop = 0
        endif
        if g:rplugin_has_wmctrl
            call system("wmctrl -a '" . basenm . ".pdf'")
        endif
    elseif g:rplugin_pdfviewer == "sumatra"
        if basenm =~ ' '
            call RWarningMsg('You must remove the empty spaces from the rnoweb file name ("' . basenm .'") to get SyncTeX support with SumatraPDF.')
        endif
        if SumatraInPath()
            let $NVIMR_PORT = g:rplugin_myport
            call writefile(['start SumatraPDF.exe -reuse-instance -forward-search ' . basenm . '.tex ' . texln . ' -inverse-search "nclientserver.exe %%f %%l" ' . basenm . '.pdf'], g:rplugin_tmpdir . "/run_cmd.bat")
            call system(g:rplugin_tmpdir . "/run_cmd.bat")
        endif
    elseif g:rplugin_pdfviewer == "skim"
        " This command is based on macvim-skim
        call system("NVIMR_PORT=" . g:rplugin_myport . " " . g:macvim_skim_app_path . '/Contents/SharedSupport/displayline -r ' . texln . ' "' . basenm . '.pdf" "' . basenm . '.tex" 2> /dev/null >/dev/null &')
    else
        call RWarningMsg('SyncTeX support for "' . g:rplugin_pdfviewer . '" not implemented.')
    endif
    exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
endfunction

function! Run_EvinceBackward()
    let olddir = getcwd()
    if olddir != expand("%:p:h")
        try
            exe "cd " . substitute(expand("%:p:h"), ' ', '\\ ', 'g')
        catch /.*/
            return
        endtry
    endif
    let [basenm, basedir] = SyncTeX_GetMaster()
    if basedir != '.'
        exe "cd " . substitute(basedir, ' ', '\\ ', 'g')
    endif
    let did_evince = 0
    if !exists("g:rplugin_evince_list")
        let g:rplugin_evince_list = []
    else
        for bb in g:rplugin_evince_list
            if bb == basenm
                let did_evince = 1
                break
            endif
        endfor
    endif
    if !did_evince
        call add(g:rplugin_evince_list, basenm)
        let g:rplugin_jobs["Python (Evince backward)"] = StartJob(["python", g:rplugin_home . "/R/synctex_evince_backward.py", basenm . ".pdf"], g:rplugin_job_handlers)
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
    if g:R_synctex
        if $DISPLAY != "" && g:rplugin_pdfviewer == "evince"
            let g:rplugin_evince_loop = 0
            call Run_EvinceBackward()
        elseif g:rplugin_nvimcom_bin_dir != "" && IsJobRunning("ClientServer") == 0 && !($DISPLAY == "" && (g:rplugin_pdfviewer == "zathura" || g:rplugin_pdfviewer == "okular"))
            if !has("win32")
                if $PATH !~ g:rplugin_nvimcom_bin_dir
                    let $PATH = g:rplugin_nvimcom_bin_dir . ':' . $PATH
                endif
                if v:windowid != 0 && $WINDOWID == ""
                    let $WINDOWID = v:windowid
                endif
                let g:rplugin_jobs["ClientServer"] = StartJob("nclientserver", g:rplugin_job_handlers)
            endif
        endif
    endif
endif

call RSourceOtherScripts()

if exists("b:undo_ftplugin")
    let b:undo_ftplugin .= " | unlet! b:IsInRCode b:PreviousRChunk b:NextRChunk b:SendChunkToR"
else
    let b:undo_ftplugin = "unlet! b:IsInRCode b:PreviousRChunk b:NextRChunk b:SendChunkToR"
endif
ftplugin/rrst_nvimr.vim	[[[1
201

if exists("g:disable_r_ftplugin")
    finish
endif

" Source scripts common to R, Rrst, Rnoweb, Rhelp and Rdoc:
runtime R/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rrst, Rnoweb, Rhelp and Rdoc need to be
" defined after the global ones:
runtime R/common_buffer.vim

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
    if g:R_strict_rst
        let rcmd = rcmd . '; render_rst(strict=TRUE)'
    endif
    let rcmd = rcmd . '; knit("' . expand("%:t") . '")'

    if a:t == "odt"
        let rcmd = rcmd . '; system("rst2odt ' . expand("%:r:t") . ".rst " . expand("%:r:t") . '.odt")'
    else
        let rcmd = rcmd . '; system("rst2html ' . expand("%:r:t") . ".rst " . expand("%:r:t") . '.html")'
    endif

    if g:R_openhtml && a:t == "html"
        let rcmd = rcmd . '; browseURL("' . expand("%:r:t") . '.html")'
    endif
    call g:SendCmdToR(rcmd)
endfunction

function! RMakePDFrrst()
    if !exists("g:rplugin_pdfviewer")
        call RSetPDFViewer()
    endif

    if g:rplugin_nvimcom_port == 0
        call RWarningMsg("The nvimcom package is required to make and open the PDF.")
    endif
    update
    call RSetWD()
    if g:rplugin_has_rst2pdf == 0
        if exists("g:R_rst2pdfpath") && executable(g:R_rst2pdfpath)
            let g:rplugin_has_rst2pdf = 1
        elseif executable("rst2pdf")
            let g:rplugin_has_rst2pdf = 1
        else
            call RWarningMsg("Is 'rst2pdf' application installed? Cannot convert into PDF: 'rst2pdf' executable not found.")
            return
        endif
    endif

    let rrstdir = expand("%:p:h")
    if has("win32")
        let rrstdir = substitute(rrstdir, '\\', '/', 'g')
    endif
    let pdfcmd = 'nvim.interlace.rrst("' . expand("%:t") . '", rrstdir = "' . rrstdir . '"'
    if exists("g:R_rrstcompiler")
        let pdfcmd = pdfcmd . ", compiler='" . g:R_rrstcompiler . "'"
    endif
    if exists("g:R_knitargs")
        let pdfcmd = pdfcmd . ", " . g:R_knitargs
    endif
    if exists("g:R_rst2pdfpath")
        let pdfcmd = pdfcmd . ", rst2pdfpath='" . g:R_rst2pdfpath . "'"
    endif
    if exists("g:R_rst2pdfargs")
        let pdfcmd = pdfcmd . ", " . g:R_rst2pdfargs
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
    runtime R/gui_running.vim
    call MakeRMenu()
endif

let g:rplugin_has_rst2pdf = 0

call RSourceOtherScripts()

if exists("b:undo_ftplugin")
    let b:undo_ftplugin .= " | unlet! b:IsInRCode b:PreviousRChunk b:NextRChunk b:SendChunkToR"
else
    let b:undo_ftplugin = "unlet! b:IsInRCode b:PreviousRChunk b:NextRChunk b:SendChunkToR"
endif
syntax/rbrowser.vim	[[[1
76
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
    syn match rbrowserS4	"<#.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserLazy	"&#.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserUnknown	"=#.*\t" contains=rbrowserDelim,rbrowserTab
else
    syn match rbrowserNumeric	"{.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserCharacter	/".*\t/ contains=rbrowserDelim,rbrowserTab
    syn match rbrowserFactor	"'.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserFunction	"(.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserList	"\[.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserLogical	"%.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserLibrary	"#.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserS4	"<.*\t" contains=rbrowserDelim,rbrowserTab
    syn match rbrowserLazy	"&.*\t" contains=rbrowserDelim,rbrowserTab
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
    syn match rbrowserDelim contained /'#\|"#\|(#\|\[#\|{#\|%#\|##\|<#\|&#\|=#/ conceal
else
    syn match rbrowserDelim contained /'\|"\|(\|\[\|{\|%\|#\|<\|&\|=/
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
hi def link rbrowserS4		Statement
hi def link rbrowserLazy	Comment
hi def link rbrowserUnknown	Normal
hi def link rbrowserWarn	WarningMsg
hi def link rbrowserErr 	ErrorMsg
hi def link rbrowserTreePart	Comment
hi def link rbrowserDelim	Ignore
hi def link rbrowserTab		Ignore

" vim: ts=8 sw=4
syntax/rdoc.vim	[[[1
70
" Vim syntax file
" Language:	R documentation
" Maintainer:	Jakson A. Aquino <jalvesaq@gmail.com>

if exists("b:current_syntax")
    finish
endif

setlocal iskeyword=@,48-57,_,.
setlocal conceallevel=2
setlocal concealcursor=nvc

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
syn match rdocURL " |.\{-}|" contains=rdocURLBar
syn match rdocURLBar contained " " conceal
syn match rdocURLBar contained "|" conceal
syn keyword rdocNote		note Note NOTE note: Note: NOTE: Notes Notes:

" When using vim as R pager to see the output of help.search():
syn region rdocPackage start="^[A-Za-z]\S*::" end="[\s\r]" contains=rdocPackName,rdocFuncName transparent
syn match rdocPackName "^[A-Za-z][A-Za-z0-9\.]*" contained
syn match rdocFuncName "::[A-Za-z0-9\.\-_]*" contained

syn region rdocArgReg matchgroup=rdocArgTitle start="^Arguments:" matchgroup=NONE end="^ \t" contains=rdocArgItems,rdocArgTitle,rdocPackage,rdocFuncName,rdocStringS keepend transparent
syn region rdocArgItems start="\n\n" end=":" contains=rdocArg contained transparent
syn match rdocArg "\([A-Z]\|[a-z]\|[0-9]\|\.\|_\)*" contained extend

syn include @rdocR syntax/r.vim
syn region rdocExample matchgroup=rdocExTitle start="^Examples:$" matchgroup=rdocExEnd end='^###$' contains=@rdocR keepend
syn region rdocUsage matchgroup=rdocTitle start="^Usage:$" matchgroup=NONE end='^\t' contains=@rdocR

syn sync match rdocSyncExample grouphere rdocExample "^Examples:$"
syn sync match rdocSyncUsage grouphere rdocUsage "^Usage:$"
syn sync match rdocSyncArg grouphere rdocArgReg "^Arguments:"
syn sync match rdocSyncNONE grouphere NONE "^\t$"
syn sync match rdocSyncNONE grouphere NONE "^ \t$"


" Define the default highlighting.
"hi def link rdocArgReg Statement
hi def link rdocTitle	    Title
hi def link rdocArgTitle    Title
hi def link rdocExTitle   Title
hi def link rdocExEnd   Comment
hi def link rdocFunction    Function
hi def link rdocStringD     String
hi def link rdocURL    Underlined
hi def link rdocArg         Special
hi def link rdocNote  Todo

hi def link rdocPackName Title
hi def link rdocFuncName Function

let b:current_syntax = "rdoc"

" vim: ts=8 sw=4
syntax/rout.vim	[[[1
188
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
syn match routNegFloat "-\<\d\+\.\d*\([Ee][-+]\=\d\+\)\="
" floating point number with no integer part and optional exponent
syn match routFloat "\<\.\d\+\([Ee][-+]\=\d\+\)\="
syn match routNegFloat "-\<\.\d\+\([Ee][-+]\=\d\+\)\="
" floating point number with no fractional part and optional exponent
syn match routFloat "\<\d\+[Ee][-+]\=\d\+"
syn match routNegFloat "-\<\d\+[Ee][-+]\=\d\+"

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
    syn region routColoredR start="^>" end='$' contains=@routR keepend
    syn region routColoredR start="^+" end='$' contains=@routR keepend
else
    " Input
    syn match routInput /^>.*/
    syn match routInput /^+.*/
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

if exists("g:rout_follow_colorscheme") && g:rout_follow_colorscheme
    " Default when following :colorscheme
    hi def link routNormal	Normal
    hi def link routNumber	Number
    hi def link routInteger	Number
    hi def link routFloat	Float
    hi def link routComplex	Number
    hi def link routNegNum	Number
    hi def link routNegFloat	Float
    hi def link routDate	Number
    hi def link routTrue	Boolean
    hi def link routFalse	Boolean
    hi def link routInf  	Number
    hi def link routConst	Constant
    hi def link routString	String
    hi def link routIndex	Special
    hi def link routError	ErrorMsg
    hi def link routWarn	WarningMsg
else
    function s:SetGroupColor(group, cgui, c256, c16)
        if exists("g:rout_color_" . tolower(a:group))
            exe "hi rout" . a:group . eval("g:rout_color_" . tolower(a:group))
        elseif &t_Co == 256
            exe "hi rout" . a:group . "ctermfg=" . a:c256 . " guifg=" . a:cgui
        else
            exe "hi rout" . a:group . "ctermfg=" . a:c16 . " guifg=" . a:cgui
        endif
    endfunction
    call s:SetGroupColor("Input ",    "#9e9e9e",               "247",          "gray")
    call s:SetGroupColor("Normal ",   "#00d700",               "40",           "darkgreen")
    call s:SetGroupColor("Number ",   "#ffaf00",               "214",          "darkyellow")
    call s:SetGroupColor("Integer ",  "#ffaf00",               "214",          "darkyellow")
    call s:SetGroupColor("Float ",    "#ffaf00",               "214",          "darkyellow")
    call s:SetGroupColor("Complex ",  "#ffaf00",               "214",          "darkyellow")
    call s:SetGroupColor("NegNum ",   "#ff875f",               "209",          "darkyellow")
    call s:SetGroupColor("NegFloat ", "#ff875f",               "209",          "darkyellow")
    call s:SetGroupColor("Date ",     "#d7af5f",               "179",          "darkyellow")
    call s:SetGroupColor("False ",    "#ff5f5f",               "203",          "darkyellow")
    call s:SetGroupColor("True ",     "#5fd787",               "78",           "magenta")
    call s:SetGroupColor("Inf ",      "#00afff",               "39",           "darkgreen")
    call s:SetGroupColor("Const ",    "#00af5f",               "35",           "magenta")
    call s:SetGroupColor("String ",   "#5fffaf",               "85",           "darkcyan")
    call s:SetGroupColor("Error ",    "#ffffff guibg=#c00000", "15 ctermbg=1", "white ctermbg=red")
    call s:SetGroupColor("Warn ",     "#c00000",               "1",            "red")
    call s:SetGroupColor("Index ",    "#87afaf",               "109",          "darkgreen")
    delfunction s:SetGroupColor
endif

let   b:current_syntax = "rout"

" vim: ts=8 sw=4
R/common_buffer.vim	[[[1
78
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
" Please see doc/Nvim-R.txt for usage details.
"==========================================================================


" Set completion with CTRL-X CTRL-O to autoloaded function.
if exists('&ofu')
    if &filetype == "rnoweb" || &filetype == "rrst" || &filetype == "rmd"
        if &omnifunc == "CompleteR"
            let b:rplugin_nonr_omnifunc = ""
        else
            let b:rplugin_nonr_omnifunc = &omnifunc
        endif
    endif
    if &filetype == "r" || &filetype == "rnoweb" || &filetype == "rdoc" || &filetype == "rhelp" || &filetype == "rrst" || &filetype == "rmd"
        setlocal omnifunc=CompleteR
    endif
endif

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

let g:rplugin_lastft = &filetype

" Check if b:pdf_is_open already exists because this script is called when
" FillRLibList() is called
if !exists("b:pdf_is_open")
    let b:pdf_is_open = 0
endif

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

if &filetype != "rnoweb"
    let g:rplugin_zathura_pid[expand("%:t:r")] = 0
endif

if g:R_assign == 3
    iabb <buffer> _ <-
endif
R/common_global.vim	[[[1
3348
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
" functions that are only occasionally used. The Nvim-R has
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

if !has("nvim")
    if !exists("*job_getchannel") || !has("patch-7.4.1829")
        call RWarningMsgInp("Nvim-R requires either Neovim >= 0.1.4 or Vim >= 7.4.1829.\nIf using Vim, it must have been compiled with both +channel and +job features.\n")
        let g:rplugin_failed = 1
        finish
    endif
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
        if g:R_assign == 1 && g:R_assign_map == "_" && j > 3 && s[j-3] == "<" && s[j-2] == "-" && s[j-1] == " "
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
                if s[j-1] == '"' || s[j-1] == "'" && g:R_assign == 1
                    let synName = synIDattr(synID(line("."), j-2, 1), "name")
                    if synName == "rString" || synName == "rSpecial"
                        let isString = 0
                    endif
                endif
            else
                if g:R_assign == 2
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
        exe "normal! a" . g:R_assign_map
    else
        exe "normal! a <- "
    endif
endfunction

function ReadEvalReply()
    let reply = "No reply"
    let haswaitwarn = 0
    let ii = 0
    while ii < 20
        sleep 100m
        if filereadable($NVIMR_TMPDIR . "/eval_reply")
            let tmp = readfile($NVIMR_TMPDIR . "/eval_reply")
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
    if reply == "No reply" || reply =~ "^Error" || reply == "INVALID" || reply == "ERROR" || reply == "EMPTY" || reply == "NO_ARGS" || reply == "NOT_EXISTS"
        return "R error: " . reply
    else
        return reply
    endif
endfunction

function CompleteChunkOptions()
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
            let rkeyword0 = RGetKeyword(1)
            if rkeyword0 =~ "::"
                let pkg = '"' . substitute(rkeyword0, "::.*", "", "") . '"'
                let rkeyword0 = substitute(rkeyword0, ".*::", "", "")
                let objclass = ""
            else
                let objclass = RGetFirstObjClass(rkeyword0)
                let pkg = ""
            endif
            let rkeyword = '^' . rkeyword0 . "\x06"
            call cursor(cpos[1], cpos[2])

            " If R is running, use it
            if string(g:SendCmdToR) != "function('SendCmdToR_fake')"
                call delete(g:rplugin_tmpdir . "/eval_reply")
                let msg = 'nvimcom:::nvim.args("' . rkeyword0 . '", "' . argkey . '"'
                if objclass != ""
                    let msg = msg . ', objclass = ' . objclass
                elseif pkg != ""
                    let msg = msg . ', pkg = ' . pkg
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
                call SendToNvimcom("\x08" . $NVIMR_ID . msg)

                if g:rplugin_nvimcom_port > 0
                    let g:rplugin_lastev = ReadEvalReply()
                    if g:rplugin_lastev !~ "^R error: "
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
            let flines = g:rplugin_omni_lines + g:rplugin_globalenvlines
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
    let cstr = g:R_rcomment_string
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

    if line =~ '^\s*' . a:cmt || line =~ '^\s*#'
        let line = substitute(line, '^\s*' . a:cmt, '', '')
        let line = substitute(line, '^\s*#*', '', '')
        call setline(a:lnum, line)
        normal! ==
    else
        if g:R_indent_commented
            while line =~ '^\s*\t'
                let line = substitute(line, '^\(\s*\)\t', '\1' . s:curtabstop, "")
            endwhile
            let line = strpart(line, a:ind)
        endif
        let line = a:cmt . line
        call setline(a:lnum, line)
        if g:R_indent_commented
            normal! ==
        endif
    endif
endfunction

function RComment(mode)
    let cpos = getpos(".")
    let [fline, lline] = RGetFL(a:mode)

    " What comment string to use?
    if g:r_indent_ess_comments
        if g:R_indent_commented
            let cmt = '## '
        else
            let cmt = '### '
        endif
    else
        let cmt = g:R_rcomment_string
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

function CleanOxygenLine(line)
    let cline = a:line
    if cline =~ "^\s*#\\{1,2}'"
        let synName = synIDattr(synID(line("."), col("."), 1), "name")
        if synName == "rOExamples"
            let cline = substitute(cline, "^\s*#\\{1,2}'", "", "")
        endif
    endif
    return cline
endfunction

function CleanCurrentLine()
    let curline = substitute(getline("."), '^\s*', "", "")
    if &filetype == "r"
        let curline = CleanOxygenLine(curline)
    endif
    return curline
endfunction

" Skip empty lines and lines whose first non blank char is '#'
function GoDown()
    if &filetype == "rnoweb"
        let curline = getline(".")
        if curline[0] == '@'
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
    let curline = CleanCurrentLine()
    let lastLine = line("$")
    while i < lastLine && (curline[0] == '#' || strlen(curline) == 0)
        let i = i + 1
        call cursor(i, 1)
        let curline = CleanCurrentLine()
    endwhile
endfunction

function IsSendCmdToRFake()
    if string(g:SendCmdToR) != "function('SendCmdToR_fake')"
        redir => nkblist
        silent nmap
        redir END
        let nkbls = split(nkblist, "\n")
        let qcmd = "\\rq"
        for nkb in nkbls
            if stridx(nkb, "RQuit('nosave')") > 0
                let qls = split(nkb, " ")
                let qcmd = qls[1]
                break
            endif
        endfor
        call RWarningMsg("As far as I know, R is already running. If it is not running, did you quit it from within ". v:progname . " (command " . qcmd . ")?")
        return 1
    endif
    return 0
endfunction

function ShowRSysLog(slog, fname, msg)
    let logl = split(a:slog, "\n")
    exe len(logl) . "split " . a:fname
    call setline(1, logl)
    set nomodified
    redraw
    call RWarningMsg(a:msg)
    if has("win32")
        call UnsetRHome()
    endif
    sleep 1
endfunction

function CheckRtools()
    let Rtpath = substitute($PATH, '.*;\(.*Rtools\)\\.*', '\1', '')
    if Rtpath =~ "Rtools"
        let Rtpath = substitute(Rtpath, "\\", "/", "g") . "/VERSION.txt"
        if filereadable(Rtpath)
            let Rtvrsn = readfile(Rtpath)
            if Rtvrsn[0] =~ "version 3.4"
                call RWarningMsg("Nvim-R is incompatible with Rtools 3.4 (August 2016). Please, try Rtools 3.3.")
            endif
        endif
    endif
endfunction

function CheckNvimcomVersion()
    let neednew = 0
    if g:rplugin_nvimcom_home == ""
        let neednew = 1
    else
        if !filereadable(g:rplugin_nvimcom_home . "/DESCRIPTION")
            let neednew = 1
        else
            let ndesc = readfile(g:rplugin_nvimcom_home . "/DESCRIPTION")
            let nvers = substitute(ndesc[1], "Version: ", "", "")
            if nvers != s:required_nvimcom
                let neednew = 1
            endif
        endif
    endif

    " Nvim-R might have been installed as root in a non writable directory.
    " We have to build nvimcom in a writable directory before installing it.
    if neednew
        exe "cd " . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g')
        if has("win32")
            call SetRHome()
        endif

        " The user libs directory may not exist yet if R was just upgraded
        let rcode = [ 'sink("' . g:rplugin_tmpdir . '/libpaths")',
                    \ 'cat(.libPaths()[1L],',
                    \ '    unlist(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep))[1L],',
                    \ '    sep = "\n")',
                    \ 'sink()']
        let slog = system('R --no-save', rcode)
        if v:shell_error
            call RWarningMsg(slog)
            return 0
        endif
        let libpaths = readfile(g:rplugin_tmpdir . "/libpaths")
        if !(isdirectory(expand(libpaths[0])) && filewritable(expand(libpaths[0])) == 2)
            if !isdirectory(expand(libpaths[1]))
                let resp = input('"' . libpaths[0] . '" is not writable. Should "' . libpaths[1] . '" be created now? [y/n] ')
                if resp[0] == "y" || resp[0] == "Y"
                    call mkdir(expand(libpaths[1]), "p")
                endif
                echo " "
            endif
        endif
        call delete(g:rplugin_tmpdir . "/libpaths")

        echo "Updating nvimcom... "
        let slog = system(g:rplugin_Rcmd . ' CMD build "' . g:rplugin_home . '/R/nvimcom"')
        if v:shell_error
            call ShowRSysLog(slog, "Error_building_nvimcom", "Failed to build nvimcom")
            return 0
        else
            let slog = system(g:rplugin_Rcmd . " CMD INSTALL nvimcom_" . s:required_nvimcom . ".tar.gz")
            if v:shell_error
                call ShowRSysLog(slog, "Error_installing_nvimcom", "Failed to install nvimcom")
                if has("win32")
                    call CheckRtools()
                endif
                return 0
            else
                echon "OK!"
            endif
        endif
        if has("win32")
            call UnsetRHome()
        endif
        call delete("nvimcom_" . s:required_nvimcom . ".tar.gz")
        silent cd -
    endif
    return 1
endfunction

" Start R
function StartR(whatr)
    if !isdirectory(g:rplugin_tmpdir)
        call mkdir(g:rplugin_tmpdir, "p", 0700)
    endif
    call writefile([], g:rplugin_tmpdir . "/globenv_" . $NVIMR_ID)
    call writefile([], g:rplugin_tmpdir . "/liblist_" . $NVIMR_ID)
    call delete(g:rplugin_tmpdir . "/libnames_" . $NVIMR_ID)

    if !CheckNvimcomVersion()
        return
    endif

    if $R_DEFAULT_PACKAGES == ""
        let $R_DEFAULT_PACKAGES = "datasets,utils,grDevices,graphics,stats,methods,nvimcom"
    elseif $R_DEFAULT_PACKAGES !~ "nvimcom"
        let $R_DEFAULT_PACKAGES .= ",nvimcom"
    endif

    if a:whatr =~ "custom"
        call inputsave()
        let r_args = input('Enter parameters for R: ')
        call inputrestore()
        let g:rplugin_r_args = split(r_args)
    else
        if exists("g:R_args")
            let g:rplugin_r_args = g:R_args
        else
            let g:rplugin_r_args = []
        endif
    endif

    if g:R_objbr_opendf
        let start_options = ['options(nvimcom.opendf = TRUE)']
    else
        let start_options = ['options(nvimcom.opendf = FALSE)']
    endif
    if g:R_objbr_openlist
        let start_options += ['options(nvimcom.openlist = TRUE)']
    else
        let start_options += ['options(nvimcom.openlist = FALSE)']
    endif
    if g:R_objbr_allnames
        let start_options += ['options(nvimcom.allnames = TRUE)']
    else
        let start_options += ['options(nvimcom.allnames = FALSE)']
    endif
    if g:R_texerr
        let start_options += ['options(nvimcom.texerrs = TRUE)']
    else
        let start_options += ['options(nvimcom.texerrs = FALSE)']
    endif
    if g:R_objbr_labelerr
        let start_options += ['options(nvimcom.labelerr = TRUE)']
    else
        let start_options += ['options(nvimcom.labelerr = FALSE)']
    endif
    if g:R_nvimpager == "no"
        let start_options += ['options(nvimcom.nvimpager = FALSE)']
    else
        let start_options += ['options(nvimcom.nvimpager = TRUE)']
    endif
    if g:R_in_buffer && g:R_esc_term
        let start_options += ['options(editor = nvimcom:::nvim.edit)']
    endif

    let rwd = ""
    if g:R_nvim_wd == 0
        let rwd = expand("%:p:h")
    elseif g:R_nvim_wd == 1
        let rwd = getcwd()
    endif
    if rwd != ""
        if has("win32")
            let rwd = substitute(rwd, '\\', '/', 'g')
        endif

        " `rwd` will not be a real directory if editing a file on the internet
        " with netrw plugin
        if isdirectory(rwd)
            if has("win32") && &encoding == "utf-8"
                let start_options += ['.nvim.rwd <- "' . rwd . '"']
                let start_options += ['Encoding(.nvim.rwd) <- "UTF-8"']
                let start_options += ['setwd(.nvim.rwd)']
                let start_options += ['rm(.nvim.rwd)']
            else
                let start_options += ['setwd("' . rwd . '")']
            endif
        endif
    endif
    let start_options += ['if(utils::packageVersion("nvimcom") != "' .
                \ s:required_nvimcom_dot .
                \ '") warning("Your version of Nvim-R requires nvimcom-' .
                \ s:required_nvimcom .
                \ '.", call. = FALSE)']
    call writefile(start_options, g:rplugin_tmpdir . "/start_options.R")

    if g:R_in_buffer
        call StartR_Neovim()
        return
    endif

    if g:R_applescript
        call StartR_OSX()
        return
    endif

    if has("win32")
        call StartR_Windows()
        return
    endif

    if IsSendCmdToRFake()
        return
    endif

    let args_str = join(g:rplugin_r_args)
    if args_str == ""
        let rcmd = g:rplugin_R
    else
        let rcmd = g:rplugin_R . " " . args_str
    endif

    if g:R_tmux_split
        call StartR_TmuxSplit(rcmd)
    else
        call StartR_ExternalTerm(rcmd)
    endif
endfunction

" Send SIGINT to R
function StopR()
    if g:rplugin_r_pid
        call system("kill -s SIGINT " . g:rplugin_r_pid)
    endif
endfunction

function WaitNvimcomStart()
    let args_str = join(g:rplugin_r_args)
    if args_str =~ "vanilla"
        return 0
    endif
    if g:R_nvimcom_wait < 0
        return 0
    endif
    if g:R_nvimcom_wait < 300
        g:R_nvimcom_wait = 300
    endif
    redraw
    echo "Waiting nvimcom loading..."
    sleep 300m
    let ii = 300
    let waitmsg = 0
    while !filereadable(g:rplugin_tmpdir . "/nvimcom_running_" . $NVIMR_ID) && ii < g:R_nvimcom_wait
        let ii = ii + 200
        sleep 200m
    endwhile
    echon "\r                              "
    redraw
    sleep 300m
    if filereadable(g:rplugin_tmpdir . "/nvimcom_running_" . $NVIMR_ID)
        let vr = readfile(g:rplugin_tmpdir . "/nvimcom_running_" . $NVIMR_ID)
        let g:rplugin_nvimcom_version = vr[0]
        let g:rplugin_nvimcom_home = vr[1]
        let g:rplugin_nvimcom_port = vr[2]
        let g:rplugin_r_pid = vr[3]
        let $RCONSOLE = vr[4]
        let search_list = vr[5]
        call delete(g:rplugin_tmpdir . "/nvimcom_running_" . $NVIMR_ID)
        if g:rplugin_nvimcom_version != s:required_nvimcom_dot
            call RWarningMsg('This version of Nvim-R requires nvimcom ' .
                        \ s:required_nvimcom . '.')
            sleep 1
        endif

        if search_list =~ "package:colorout" && !exists("g:R_hl_term")
            let g:R_hl_term = 0
        endif
        if search_list =~ "package:setwidth" && !exists("g:R_setwidth")
            let g:R_setwidth = 0
        endif

        if isdirectory(g:rplugin_nvimcom_home . "/bin/x64")
            let g:rplugin_nvimcom_bin_dir = g:rplugin_nvimcom_home . "/bin/x64"
        elseif isdirectory(g:rplugin_nvimcom_home . "/bin/i386")
            let g:rplugin_nvimcom_bin_dir = g:rplugin_nvimcom_home . "/bin/i386"
        else
            let g:rplugin_nvimcom_bin_dir = g:rplugin_nvimcom_home . "/bin"
        endif

        call writefile([g:rplugin_nvimcom_version, g:rplugin_nvimcom_home, g:rplugin_nvimcom_bin_dir], g:rplugin_compldir . "/nvimcom_info")

        if has("win32")
            let nvc = "nclientserver.exe"
            if $PATH !~ g:rplugin_nvimcom_bin_dir
                let $PATH = g:rplugin_nvimcom_bin_dir . ';' . $PATH
            endif
        else
            let nvc = "nclientserver"
            if $PATH !~ g:rplugin_nvimcom_bin_dir
                let $PATH = g:rplugin_nvimcom_bin_dir . ':' . $PATH
            endif
        endif
        if filereadable(g:rplugin_nvimcom_bin_dir . '/' . nvc)
            " Set nvimcom port in the nclientserver
            let $NVIMCOMPORT = g:rplugin_nvimcom_port
            " Set RConsole window ID in nclientserver to ArrangeWindows()
            if has("win32")
                if $RCONSOLE == "0"
                    call RWarningMsg("nvimcom did not save R window ID")
                endif
                if v:windowid != 0 && $WINDOWID == ""
                    let $WINDOWID = v:windowid
                endif
            endif
            if !IsJobRunning("ClientServer")
                let g:rplugin_jobs["ClientServer"] = StartJob(nvc, g:rplugin_job_handlers)
            else
                " ClientServer already started
                if has("win32")
                    call JobStdin(g:rplugin_jobs["ClientServer"], "\001" . g:rplugin_nvimcom_port . " " . $RCONSOLE . "\n")
                else
                    call JobStdin(g:rplugin_jobs["ClientServer"], "\001" . g:rplugin_nvimcom_port . "\n")
                endif
                call SendToNvimcom("\001" . g:rplugin_myport)
            endif
        else
            call RWarningMsg('Application "' . nvc . '" not found.')
        endif

        if g:R_tmux_split
            " Environment variables persist across Tmux windows.
            " Unset NVIMR_TMPDIR to avoid nvimcom loading its C library
            " when R was not started by Neovim:
            call system("tmux set-environment -u NVIMR_TMPDIR")
        endif
        return 1
    else
        if filereadable(g:rplugin_compldir . "/nvimcom_info")
            " The information on nvimcom home might be invalid if R was upgraded
            call delete(g:rplugin_compldir . "/nvimcom_info")
            let g:rplugin_nvimcom_version = "0"
            let g:rplugin_nvimcom_home = ""
            let g:rplugin_nvimcom_bin_dir = ""
            let msg = "The package nvimcom wasn't loaded yet. Please, quit R and try again."
        else
            let msg = "The package nvimcom wasn't loaded yet. Please, see  :h nvimcom-not-loaded"
        endif
        if g:R_tmux_split
            call RWarningMsgInp(msg)
        else
            call RWarningMsg(msg)
            sleep 500m
        endif
        return 0
    endif
endfunction

function StartObjBrowser()
    " Either load or reload the Object Browser
    let savesb = &switchbuf
    set switchbuf=useopen,usetab
    if bufloaded(b:objbrtitle)
        exe "sb " . b:objbrtitle
    else
        " Copy the values of some local variables that will be inherited
        let g:tmp_objbrtitle = b:objbrtitle
        let g:tmp_curbufname = bufname("%")

        let l:sr = &splitright
        if g:R_objbr_place =~ "left"
            set nosplitright
        else
            set splitright
        endif
        if g:R_objbr_place =~ "console"
            exe 'sb ' . g:rplugin_R_bufname
        endif
        sil exe "vsplit " . b:objbrtitle
        let &splitright = l:sr
        sil exe "vertical resize " . g:R_objbr_w
        sil set filetype=rbrowser

        " Inheritance of some local variables
        let b:objbrtitle = g:tmp_objbrtitle
        let b:rscript_buffer = g:tmp_curbufname
        unlet g:tmp_objbrtitle
        unlet g:tmp_curbufname
        call SendToNvimcom("\002" . g:rplugin_myport)
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

    call StartObjBrowser()
    let g:rplugin_running_objbr = 0
    return
endfunction

function RBrOpenCloseLs(stt)
    call SendToNvimcom("\007" . a:stt)
endfunction

function SendToNvimcom(cmd)
    if !IsJobRunning("ClientServer")
        call RWarningMsg("ClientServer not running.")
        return
    endif
    call JobStdin(g:rplugin_jobs["ClientServer"], "\002" . a:cmd . "\n")
endfunction

function RSetMyPort(p)
    let g:rplugin_myport = a:p
    let $NVIMR_PORT = a:p
    if IsJobRunning("ClientServer")
        if g:rplugin_nvimcom_port
            call SendToNvimcom("\001" . g:rplugin_myport)
        endif
    endif
endfunction

function RFormatCode() range
    if g:rplugin_nvimcom_port == 0
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
    call SendToNvimcom("\x08" . $NVIMR_ID . 'formatR::tidy_source("' . g:rplugin_tmpdir . '/unformatted_code", file = "' . g:rplugin_tmpdir . '/formatted_code", width.cutoff = ' . wco . ')')
    let g:rplugin_lastev = ReadEvalReply()
    if g:rplugin_lastev =~ "^R error: "
        call RWarningMsg(g:rplugin_lastev)
        return
    endif
    let lns = readfile(g:rplugin_tmpdir . "/formatted_code")
    silent exe a:firstline . "," . a:lastline . "delete"
    call append(a:firstline - 1, lns)
    echo (a:lastline - a:firstline + 1) . " lines formatted."
endfunction

function RInsert(...)
    if g:rplugin_nvimcom_port == 0
        return
    endif

    call delete(g:rplugin_tmpdir . "/eval_reply")
    call delete(g:rplugin_tmpdir . "/Rinsert")
    call SendToNvimcom("\x08" . $NVIMR_ID . 'capture.output(' . a:1 . ', file = "' . g:rplugin_tmpdir . '/Rinsert")')
    let g:rplugin_lastev = ReadEvalReply()
    if g:rplugin_lastev =~ "^R error: "
        call RWarningMsg(g:rplugin_lastev)
    else
        if a:0 == 2 && a:2 == "newtab"
            tabnew
            set ft=rout
        endif
        silent exe "read " . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g') . "/Rinsert"
    endif
endfunction

function SendLineToRAndInsertOutput()
    let lin = getline(".")
    call RInsert("print(" . lin . ")")
    let curpos = getpos(".")
    " comment the output
    let ilines = readfile(g:rplugin_tmpdir . "/Rinsert")
    for iln in ilines
        call RSimpleCommentLine("normal", "c")
        normal! j
    endfor
    call setpos(".", curpos)
endfunction

" Function to send commands
" return 0 on failure and 1 on success
function SendCmdToR_fake(...)
    call RWarningMsg("Did you already start R?")
    return 0
endfunction

" Get the word either under or after the cursor.
" Works for word(| where | is the cursor position.
function RGetKeyword(colon)
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
    if a:colon
        setlocal iskeyword=@,48-57,_,.,:,$,@-@
    else
        setlocal iskeyword=@,48-57,_,.,$,@-@
    endif
    let rkeyword = expand("<cword>")
    exe "setlocal iskeyword=" . save_keyword
    call setpos(".", save_cursor)
    return rkeyword
endfunction

function GetROutput(outf)
    if a:outf =~ g:rplugin_tmpdir
        let tnum = 1
        while bufexists("so" . tnum)
            let tnum += 1
        endwhile
        exe 'tabnew so' . tnum
        exe 'read ' . substitute(a:outf, " ", '\\ ', 'g')
        set filetype=rout
        setlocal buftype=nofile
        setlocal noswapfile
    else
        exe 'tabnew ' . substitute(a:outf, " ", '\\ ', 'g')
    endif
    normal! gT
    redraw
endfunction

function RViewDF(oname)
    if exists("g:R_csv_app")
        if !executable(g:R_csv_app)
            call RWarningMsg('R_csv_app ("' . g:R_csv_app . '") is not executable')
            return
        endif
        normal! :<Esc>
        call system('cp "' . g:rplugin_tmpdir . '/Rinsert" "' . a:oname . '.csv"')
        if has("win32")
            silent exe '!start "' . g:R_csv_app . '" "' . a:oname . '.csv"'
        else
            call system(g:R_csv_app . ' "' . a:oname . '.csv" >/dev/null 2>/dev/null &')
        endif
        return
    endif
    echo 'Opening "' . a:oname . '.csv"'
    silent exe 'tabnew ' . a:oname . '.csv'
    silent 1,$d
    silent exe 'read ' . substitute(g:rplugin_tmpdir, " ", '\\ ', 'g') . '/Rinsert'
    silent 1d
    set filetype=csv
    set nomodified
    redraw
    if !exists(":CSVTable") && g:R_csv_warn
        call RWarningMsg("csv.vim is not installed (http://www.vim.org/scripts/script.php?script_id=2830)")
    endif
endfunction

function GetSourceArgs(e)
    let sargs = ""
    if g:R_source_args != ""
        let sargs = ", " . g:R_source_args
    endif
    if a:e == "echo"
        let sargs .= ', echo=TRUE'
    endif
    return sargs
endfunction

" Send sources to R
function RSourceLines(...)
    let lines = a:1
    if &filetype == "rrst"
        let lines = map(copy(lines), 'substitute(v:val, "^\\.\\. \\?", "", "")')
    endif
    if &filetype == "rmd"
        let lines = map(copy(lines), 'substitute(v:val, "^(\\`\\`)\\?", "", "")')
    endif
    if !g:R_commented_lines
        let newlines = []
        for line in lines
            if line !~ '^\s*#'
                call add(newlines, line)
            endif
        endfor
        let lines = newlines
    endif
    call writefile(lines, g:rplugin_rsource)

    if a:0 == 3 && a:3 == "NewtabInsert"
        call SendToNvimcom("\x08" . $NVIMR_ID . 'nvimcom:::nvim_capture_source_output("' . g:rplugin_rsource . '", "' . g:rplugin_tmpdir . '/Rinsert")')
        return 1
    endif

    let sargs = GetSourceArgs(a:2)
    let rcmd = 'base::source("' . g:rplugin_rsource . '"' . sargs . ')'
    let ok = g:SendCmdToR(rcmd)
    return ok
endfunction

" Send file to R
function SendFileToR(e)
    let flines = getline(1, "$")
    call RSourceLines(flines, a:e)
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
    let ok = RSourceLines(lines, a:e)
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
    let ok = RSourceLines(lines, a:e)
    if  ok == 0
        return
    endif
    if a:m == "down"
        call cursor(lastline, 1)
        call GoDown()
    endif
endfunction

" Send selection to R
function SendSelectionToR(...)
    if &filetype != "r"
        if b:IsInRCode(0) == 0
            if (&filetype == "rnoweb" && getline(".") !~ "\\Sexpr{") || (&filetype == "rmd" && getline(".") !~ "`r ") || (&filetype == "rrst" && getline(".") !~ ":r:`")
                call RWarningMsg("Not inside an R code chunk.")
                return
            endif
        endif
    endif

    if line("'<") == line("'>")
        let i = col("'<") - 1
        let j = col("'>") - i
        let l = getline("'<")
        let line = strpart(l, i, j)
        let line = CleanOxygenLine(line)
        let ok = g:SendCmdToR(line)
        if ok && a:2 =~ "down"
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

    let curpos = getpos(".")
    let curline = line("'<")
    for idx in range(0, len(lines) - 1)
        call setpos(".", [0, curline, 1, 0])
        let lines[idx] = CleanOxygenLine(lines[idx])
        let curline += 1
    endfor
    call setpos(".", curpos)

    if a:0 == 3 && a:3 == "NewtabInsert"
        let ok = RSourceLines(lines, a:1, "NewtabInsert")
    else
        let ok = RSourceLines(lines, a:1)
    endif

    if ok == 0
        return
    endif

    if a:2 == "down"
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
    let ok = RSourceLines(lines, a:e)
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
                call RSourceLines(codelines, "silent")
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
    call RSourceLines(codelines, "silent")
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
        let line = substitute(line, "^(\\`\\`)\\?", "", "")
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

    if &filetype == "r"
        let line = CleanOxygenLine(line)
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
function RClearConsole()
    if has("win32") && !g:R_in_buffer
        call JobStdin(g:rplugin_jobs["ClientServer"], "\006\n")
        sleep 50m
        call JobStdin(g:rplugin_jobs["ClientServer"], "\007\n")
    else
        call g:SendCmdToR("\014", 0)
    endif
endfunction

" Remove all objects
function RClearAll()
    if g:R_rmhidden
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
    if has("win32")
        let wdcmd = substitute(wdcmd, "\\", "/", "g")
    endif
    call g:SendCmdToR(wdcmd)
    sleep 100m
endfunction

function ClearRInfo()
    if exists("g:rplugin_rconsole_pane")
        unlet g:rplugin_rconsole_pane
    endif

    call delete(g:rplugin_tmpdir . "/globenv_" . $NVIMR_ID)
    call delete(g:rplugin_tmpdir . "/liblist_" . $NVIMR_ID)
    call delete(g:rplugin_tmpdir . "/libnames_" . $NVIMR_ID)
    call delete(g:rplugin_tmpdir . "/GlobalEnvList_" . $NVIMR_ID)
    call delete(g:rplugin_tmpdir . "/nvimcom_running_" . $NVIMR_ID)
    let g:SendCmdToR = function('SendCmdToR_fake')
    let g:rplugin_r_pid = 0
    let g:rplugin_nvimcom_port = 0

    if g:R_tmux_split && g:R_tmux_title != "automatic" && g:R_tmux_title != ""
        call system("tmux set automatic-rename on")
    endif

    if bufloaded(b:objbrtitle)
        exe "bunload! " . b:objbrtitle
        sleep 30m
    endif
endfunction

" Quit R
function RQuit(how)
    if g:R_in_buffer && has("win32")
        call RWarningMsg("RQuit not implemented yet for R_in_buffer on Windows.")
        return
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

    if g:R_save_win_pos
        " SaveWinPos
        call JobStdin(g:rplugin_jobs["ClientServer"], "\004" . $NVIMR_COMPLDIR . "\n")
    endif

    " Must be in term buffer to get TermClose event triggered
    if g:R_in_buffer && exists("g:rplugin_R_bufname")
        exe "sbuffer " . g:rplugin_R_bufname
        startinsert
    endif

    if bufloaded(b:objbrtitle)
        exe "bunload! " . b:objbrtitle
        sleep 30m
    endif

    call g:SendCmdToR(qcmd)

    if g:R_tmux_split && a:how == "save"
        sleep 200m
    endif

    sleep 50m
    call ClearRInfo()
endfunction

" knit the current buffer content
function RKnit()
    update
    if has("win32")
        call g:SendCmdToR('require(knitr); .nvim_oldwd <- getwd(); setwd("' . substitute(expand("%:p:h"), '\\', '/', 'g') . '"); knit("' . expand("%:t") . '"); setwd(.nvim_oldwd); rm(.nvim_oldwd)')
    else
        call g:SendCmdToR('require(knitr); .nvim_oldwd <- getwd(); setwd("' . expand("%:p:h") . '"); knit("' . expand("%:t") . '"); setwd(.nvim_oldwd); rm(.nvim_oldwd)')
    endif
endfunction

function SetRTextWidth(rkeyword)
    if g:R_nvimpager == "tabnew"
        let s:rdoctitle = a:rkeyword . "\\ (help)"
    else
        let s:tnr = tabpagenr()
        if g:R_nvimpager != "tab" && s:tnr > 1
            let s:rdoctitle = "R_doc" . s:tnr
        else
            let s:rdoctitle = "R_doc"
        endif
        unlet s:tnr
    endif
    if !bufloaded(s:rdoctitle) || g:R_newsize == 1
        let g:R_newsize = 0

        " s:vimpager is used to calculate the width of the R help documentation
        " and to decide whether to obey R_nvimpager = 'vertical'
        let s:vimpager = g:R_nvimpager

        let wwidth = winwidth(0)

        " Not enough room to split vertically
        if g:R_nvimpager == "vertical" && wwidth <= (g:R_help_w + g:R_editor_w)
            let s:vimpager = "horizontal"
        endif

        if s:vimpager == "horizontal"
            " Use the window width (at most 80 columns)
            let htwf = (wwidth > 80) ? 88.1 : ((wwidth - 1) / 0.9)
        elseif g:R_nvimpager == "tab" || g:R_nvimpager == "tabnew"
            let wwidth = &columns
            let htwf = (wwidth > 80) ? 88.1 : ((wwidth - 1) / 0.9)
        else
            let min_e = (g:R_editor_w > 80) ? g:R_editor_w : 80
            let min_h = (g:R_help_w > 73) ? g:R_help_w : 73

            if wwidth > (min_e + min_h)
                " The editor window is large enough to be split
                let s:hwidth = min_h
            elseif wwidth > (min_e + g:R_help_w)
                " The help window must have less than min_h columns
                let s:hwidth = wwidth - min_e
            else
                " The help window must have the minimum value
                let s:hwidth = g:R_help_w
            endif
            let htwf = (s:hwidth - 1) / 0.9
        endif
        let htw = printf("%f", htwf)
        let g:rplugin_htw = substitute(htw, "\\..*", "", "")
        let g:rplugin_htw = g:rplugin_htw - (&number || &relativenumber) * &numberwidth
    endif
endfunction

function RGetFirstObjClass(rkeyword)
    let firstobj = ""
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
            return firstobj
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
            let firstobj = strpart(line, 0, idx)
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
            let firstobj = strpart(line, 0, idx)
        else
            let firstobj = substitute(line, ').*', '', "")
            let firstobj = substitute(firstobj, ',.*', '', "")
            let firstobj = substitute(firstobj, ' .*', '', "")
        endif
    endif

    " Fix some problems
    if firstobj =~ '^"' && firstobj !~ '"$'
        let firstobj = firstobj . '"'
    elseif firstobj =~ "^'" && firstobj !~ "'$"
        let firstobj = firstobj . "'"
    endif
    if firstobj =~ "^'" && firstobj =~ "'$"
        let firstobj = substitute(firstobj, "^'", '"', "")
        let firstobj = substitute(firstobj, "'$", '"', "")
    endif
    if firstobj =~ '='
        let firstobj = "eval(expression(" . firstobj . "))"
    endif

    let objclass = ""
    call SendToNvimcom("\x08" . $NVIMR_ID . "nvimcom:::nvim.getclass(" . firstobj . ")")
    if g:rplugin_nvimcom_port > 0
        let g:rplugin_lastev = ReadEvalReply()
        if g:rplugin_lastev !~ "^R error: "
            let objclass = '"' . g:rplugin_lastev . '"'
        endif
    endif

    return objclass
endfunction

" Show R's help doc in Nvim's buffer
" (based  on pydoc plugin)
function AskRDoc(rkeyword, package, getclass)
    if filewritable(g:rplugin_docfile)
        call delete(g:rplugin_docfile)
    endif

    let objclass = ""
    if bufname("%") =~ "Object_Browser" || (exists("g:rplugin_R_bufname") && bufname("%") == g:rplugin_R_bufname)
        let savesb = &switchbuf
        set switchbuf=useopen,usetab
        exe "sb " . b:rscript_buffer
        exe "set switchbuf=" . savesb
    else
        if a:getclass
            let objclass = RGetFirstObjClass(a:rkeyword)
        endif
    endif

    call SetRTextWidth(a:rkeyword)

    if objclass == "" && a:package == ""
        let rcmd = 'nvimcom:::nvim.help("' . a:rkeyword . '", ' . g:rplugin_htw . 'L)'
    elseif a:package != ""
        let rcmd = 'nvimcom:::nvim.help("' . a:rkeyword . '", ' . g:rplugin_htw . 'L, package="' . a:package  . '")'
    else
        let rcmd = 'nvimcom:::nvim.help("' . a:rkeyword . '", ' . g:rplugin_htw . 'L, ' . objclass . ')'
    endif

    call SendToNvimcom("\x08" . $NVIMR_ID . rcmd)
endfunction

function StartTxtBrowser(brwsr, url)
    if exists("*termopen")
        tabnew
        call termopen(a:brwsr . " " . a:url)
        startinsert
    elseif $TMUX != ""
        call system("tmux new-window '" . a:brwsr . " " . a:url . "'")
    else
        call RWarningMsg('Cannot run "' . a:brwsr . '".')
    endif
endfunction

" This function is called by nvimcom
function ShowRDoc(rkeyword)
    let rkeyw = a:rkeyword
    if a:rkeyword =~ "^MULTILIB"
        let msgs = split(a:rkeyword)
        let msg = "The topic '" . msgs[-1] . "' was found in more than one library:\n"
        for idx in range(1, len(msgs) - 2)
            let msg .= idx . " : " . msgs[idx] . "\n"
        endfor
        redraw
        let chn = input(msg . "Please, select one of them: ")
        if chn > 0 && chn < (len(msgs) - 1)
            call delete(g:rplugin_tmpdir . "/eval_reply")
            call SendToNvimcom("\x08" . $NVIMR_ID . 'nvimcom:::nvim.help("' . msgs[-1] . '", ' . g:rplugin_htw . 'L, package="' . msgs[chn] . '")')
        endif
        return
    endif

    if exists("g:rplugin_R_bufname") && bufname("%") == g:rplugin_R_bufname
        " Exit Terminal mode and go to Normal mode
        stopinsert
    endif

    " If the help command was triggered in the R Console, jump to Vim pane
    if g:R_tmux_split && !g:rplugin_running_rhelp
        let slog = system("tmux select-pane -t " . g:rplugin_editor_pane)
        if v:shell_error
            call RWarningMsg(slog)
        endif
    endif
    let g:rplugin_running_rhelp = 0

    if bufname("%") =~ "Object_Browser" || (exists("g:rplugin_R_bufname") && bufname("%") == g:rplugin_R_bufname)
        let savesb = &switchbuf
        set switchbuf=useopen,usetab
        exe "sb " . b:rscript_buffer
        exe "set switchbuf=" . savesb
    endif
    call SetRTextWidth(rkeyw)

    " Local variables that must be inherited by the rdoc buffer
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
        if g:R_nvimpager == "tabnew"
            exe "tabmove " . curtabnr
        endif
    else
        if g:R_nvimpager == "tab" || g:R_nvimpager == "tabnew"
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
        elseif s:vimpager == "no"
            " The only way of ShowRDoc() being called when R_nvimpager=="no"
            " is the user setting the value of R_nvimpager to 'no' after
            " Neovim startup. It should be set in the vimrc.
            if g:R_in_buffer
                let g:R_nvimpager = "vertical"
            else
                let g:R_nvimpager = "tab"
            endif
            call ShowRDoc(a:rkeyword)
            return
        else
            echohl WarningMsg
            echomsg 'Invalid R_nvimpager value: "' . g:R_nvimpager . '". Valid values are: "tab", "vertical", "horizontal", "tabnew" and "no".'
            echohl Normal
            return
        endif
    endif

    setlocal modifiable
    let g:rplugin_curbuf = bufname("%")

    " Inheritance of local variables from the script buffer
    let b:objbrtitle = g:tmp_objbrtitle
    unlet g:tmp_objbrtitle

    let save_unnamed_reg = @@
    set modifiable
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
    stopinsert
    redraw
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
        try
            " The :cd command will fail if the cursor is in the term buffer
            silent exe "cd " . substitute(expand("%:p:h"), ' ', '\\ ', 'g')
        catch /.*/
        endtry
    endif

    if !filereadable(basenm . ".pdf")
        call RWarningMsg('File not found: "' . basenm . '.pdf".')
        exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
        return
    endif
    if g:rplugin_pdfviewer == "zathura"
        if system("wmctrl -xl") =~ 'Zathura.*' . basenm . '.pdf' && g:rplugin_zathura_pid[basenm] != 0
            call system("wmctrl -a '" . basenm . ".pdf'")
        else
            let g:rplugin_zathura_pid[basenm] = 0
            call RStart_Zathura(basenm)
        endif
        exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
        return
    elseif g:rplugin_pdfviewer == "okular"
        let pcmd = "NVIMR_PORT=" . g:rplugin_myport . " okular --unique '" .  pdfpath . "' 2>/dev/null >/dev/null &"
        call system(pcmd)
    elseif g:rplugin_pdfviewer == "evince"
        let pcmd = "evince '" . pdfpath . "' 2>/dev/null >/dev/null &"
        call system(pcmd)
    elseif g:rplugin_pdfviewer == "sumatra"
        if basenm =~ ' '
            call RWarningMsg('You must remove the empty spaces from the rnoweb file name ("' . basenm .'") to get SyncTeX support with SumatraPDF.')
        endif
        if SumatraInPath()
            let $NVIMR_PORT = g:rplugin_myport
            call writefile(['start SumatraPDF.exe -reuse-instance -inverse-search "nclientserver.exe %%f %%l" ' . basenm . '.pdf'], g:rplugin_tmpdir . "/run_cmd.bat")
            call system(g:rplugin_tmpdir . "/run_cmd.bat")
            exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
        endif
    elseif g:rplugin_pdfviewer == "skim"
        call system("NVIMR_PORT=" . g:rplugin_myport . " " . g:macvim_skim_app_path . '/Contents/MacOS/Skim "' . basenm . '.pdf" 2> /dev/null >/dev/null &')
    endif
    if g:rplugin_has_wmctrl
        call system("wmctrl -a '" . basenm . ".pdf'")
    endif
    exe "cd " . substitute(olddir, ' ', '\\ ', 'g')
endfunction

function StartZathuraNeovim(basenm)
    let g:rplugin_jobs["Zathura"] = jobstart(["zathura",
                \ "--synctex-editor-command",
                \ "nclientserver %{input} %{line}", a:basenm . ".pdf"],
                \ {"detach": 1, "on_stderr": function('ROnJobStderr')})
    if g:rplugin_jobs["Zathura"] < 1
        call RWarningMsg("Failed to run Zathura...")
    else
        let g:rplugin_zathura_pid[a:basenm] = jobpid(g:rplugin_jobs["Zathura"])
    endif
endfunction

function StartZathuraVim(basenm)
    let pycode = ["# -*- coding: " . &encoding . " -*-",
                \ "import subprocess",
                \ "import os",
                \ "import sys",
                \ "FNULL = open(os.devnull, 'w')",
                \ "a3 = '" . a:basenm . ".pdf'",
                \ "zpid = subprocess.Popen(['zathura', '--synctex-editor-command', 'nclientserver %{input} %{line}', a3], stdout = FNULL, stderr = FNULL).pid",
                \ "sys.stdout.write(str(zpid))" ]
    call writefile(pycode, g:rplugin_tmpdir . "/start_zathura.py")
    let pid = system("python '" . g:rplugin_tmpdir . "/start_zathura.py" . "'")
    if pid == 0
        call RWarningMsg("Failed to run Zathura: " . substitute(pid, "\n", " ", "g"))
    else
        let g:rplugin_zathura_pid[a:basenm] = pid
    endif
    call delete(g:rplugin_tmpdir . "/start_zathura.py")
endfunction

function RStart_Zathura(basenm)
    " Use wmctrl to check if the pdf is already open and get Zathura's PID to
    " close the document and kill Zathura.
    if g:rplugin_has_wmctrl && g:rplugin_has_dbussend && filereadable("/proc/sys/kernel/pid_max")
        let info = filter(split(system("wmctrl -xpl"), "\n"), 'v:val =~ "Zathura.*' . a:basenm . '"')
        if len(info) > 0
            let pid = split(info[0])[2] + 0     " + 0 to convert into number
            let max_pid = readfile("/proc/sys/kernel/pid_max")[0] + 0
            if pid > 0 && pid <= max_pid
                " Instead of killing, it would be better to reset the backward
                " command, but Zathura does not have a Dbus message for this,
                " and we would have to change nclientserver to receive NVIMR_PORT
                " and NVIMR_SECRET as part of argv[].
                call system('dbus-send --print-reply --session --dest=org.pwmt.zathura.PID-' . pid . ' /org/pwmt/zathura org.pwmt.zathura.CloseDocument')
                sleep 5m
                call system('kill ' . pid)
                sleep 5m
            endif
        endif
    endif

    let $NVIMR_PORT = g:rplugin_myport
    if exists("*jobpid")
        call StartZathuraNeovim(a:basenm)
    else
        call StartZathuraVim(a:basenm)
    endif
endfunction

function RSetPDFViewer()
    let g:rplugin_pdfviewer = tolower(g:R_pdfviewer)

    if !has("win32") && !g:rplugin_is_darwin
        if g:rplugin_pdfviewer == "zathura"
            if executable("zathura")
                let vv = split(system("zathura --version 2>/dev/null"))[1]
                if vv < '0.3.1'
                    call RWarningMsgInp("Zathura version must be >= 0.3.1")
                endif
            else
                call RWarningMsgInp('Please, either install "zathura" or set the value of R_pdfviewer.')
            endif
            if executable("dbus-send")
                let g:rplugin_has_dbussend = 1
            else
                let g:rplugin_has_dbussend = 0
            endif
        endif

        if g:rplugin_pdfviewer != "zathura" && g:rplugin_pdfviewer != "okular" && g:rplugin_pdfviewer != "evince"
            call RWarningMsgInp('Invalid value for R_pdfviewer: "' . g:R_pdfviewer . '"')
        endif

        if executable("wmctrl")
            let g:rplugin_has_wmctrl = 1
        else
            let g:rplugin_has_wmctrl = 0
            if &filetype == "rnoweb"
                call RWarningMsgInp("The application wmctrl must be installed to edit Rnoweb effectively.")
            endif
        endif
    endif
endfunction

function RSourceDirectory(...)
    if has("win32")
        let dir = substitute(a:1, '\\', '/', "g")
    else
        let dir = a:1
    endif
    if dir == ""
        call g:SendCmdToR("nvim.srcdir()")
    else
        call g:SendCmdToR("nvim.srcdir('" . dir . "')")
    endif
endfunction

function RAskHelp(...)
    if a:1 == ""
        call g:SendCmdToR("help.start()")
        return
    endif
    if g:R_nvimpager == "no"
        call g:SendCmdToR("help(" . a:1. ")")
    else
        call AskRDoc(a:1, "", 0)
    endif
endfunction

function DisplayArgs()
    let s:displaying_args = 1
    if &filetype == "r" || b:IsInRCode(0)
        let rkeyword = RGetKeyword(0)
        let s:sttl_str = g:rplugin_status_line
        let fargs = "Not a function"
        for omniL in g:rplugin_omni_lines
            if omniL =~ '^' . rkeyword . "\x06"
                let tmp = split(omniL, "\x06")
                if len(tmp) < 5
                    break
                else
                    let fargs = rkeyword . '(' . tmp[4] . ')'
                endif
            endif
        endfor
        if fargs !~ "Not a function"
            let fargs = substitute(fargs, "NO_ARGS", '', 'g')
            let fargs = substitute(fargs, "\x07", '=', 'g')
            let s:sttl_str = substitute(fargs, "\x09", ', ', 'g')
            silent set statusline=%!RArgsStatusLine()
        endif
    endif
    exe "normal! a("
    let s:displaying_args = 0
endfunction

function RArgsStatusLine()
    return s:sttl_str
endfunction

function RestoreStatusLine(p)
    if s:displaying_args
        return
    endif
    exe 'set statusline=' . substitute(g:rplugin_status_line, ' ', '\\ ', 'g')
    if a:p
        normal! a)
    endif
endfunction

function PrintRObject(rkeyword)
    if bufname("%") =~ "Object_Browser"
        let objclass = ""
    else
        let objclass = RGetFirstObjClass(a:rkeyword)
    endif
    if objclass == ""
        call g:SendCmdToR("print(" . a:rkeyword . ")")
    else
        call g:SendCmdToR('nvim.print("' . a:rkeyword . '", ' . objclass . ")")
    endif
endfunction

function OpenRExample()
    if bufloaded(g:rplugin_tmpdir . "/example.R")
        exe "bunload! " . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g')
    endif
    if g:R_nvimpager == "tabnew" || g:R_nvimpager == "tab"
        exe "tabnew " . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g') . "/example.R"
    else
        let nvimpager = g:R_nvimpager
        if g:R_nvimpager == "vertical"
            let wwidth = winwidth(0)
            let min_e = (g:R_editor_w > 78) ? g:R_editor_w : 78
            let min_h = (g:R_help_w > 78) ? g:R_help_w : 78
            if wwidth < (min_e + min_h)
                let nvimpager = "horizontal"
            endif
        endif
        if nvimpager == "vertical"
            exe "belowright vsplit " . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g') . "/example.R"
        else
            exe "belowright split " . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g') . "/example.R"
        endif
    endif
    nmap <buffer><silent> q :q<CR>
    setlocal bufhidden=wipe
    setlocal noswapfile
    set buftype=nofile
    call delete(g:rplugin_tmpdir . "/example.R")
endfunction

" Call R functions for the word under cursor
function RAction(rcmd)
    if &filetype == "rbrowser"
        let rkeyword = RBrowserGetName(1, 0)
    else
        if a:rcmd == "help" || (a:rcmd == "args" && g:R_listmethods) || a:rcmd == "viewdf"
            let rkeyword = RGetKeyword(0)
        else
            let rkeyword = RGetKeyword(1)
        endif
    endif
    if strlen(rkeyword) > 0
        if a:rcmd == "help"
            let g:rplugin_running_rhelp = 1
            if g:R_nvimpager == "no"
                call g:SendCmdToR("help(" . rkeyword . ")")
            else
                if bufname("%") =~ "Object_Browser"
                    if g:rplugin_curview == "libraries"
                        let pkg = RBGetPkgName()
                    else
                        let pkg = ""
                    endif
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
        if a:rcmd == "args"
            if g:R_listmethods == 1
                call g:SendCmdToR('nvim.list.args("' . rkeyword . '")')
            else
                call g:SendCmdToR('args("' . rkeyword . '")')
            endif
            return
        endif
        if a:rcmd == "plot" && g:R_specialplot == 1
            let rfun = "nvim.plot"
        endif
        if a:rcmd == "plotsumm"
            if g:R_specialplot == 1
                let raction = "nvim.plot(" . rkeyword . "); summary(" . rkeyword . ")"
            else
                let raction = "plot(" . rkeyword . "); summary(" . rkeyword . ")"
            endif
            call g:SendCmdToR(raction)
            return
        endif
        if a:rcmd == "viewdf"
            if exists("g:R_df_viewer")
                call g:SendCmdToR(printf(g:R_df_viewer, rkeyword))
            else
                echo "Wait..."
                call delete(g:rplugin_tmpdir . "/Rinsert")
                call SendToNvimcom("\x08" . $NVIMR_ID . 'nvimcom:::nvim_viewdf("' . rkeyword . '")')
            endif
            return
        endif
        if g:R_open_example && a:rcmd == "example"
            call SendToNvimcom("\x08" . $NVIMR_ID . 'nvimcom:::nvim.example("' . rkeyword . '")')
            return
        endif

        let raction = rfun . "(" . rkeyword . ")"
        call g:SendCmdToR(raction)
    endif
endfunction

" render a document with rmarkdown
function! RMakeRmd(t)
    if !exists("g:rplugin_pdfviewer")
        call RSetPDFViewer()
    endif

    update

    if a:t == "odt"
        if has("win32")
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
    if has("win32")
        let rmddir = substitute(rmddir, '\\', '/', 'g')
    endif
    if a:t == "default"
        let rcmd = 'nvim.interlace.rmd("' . expand("%:t") . '", rmddir = "' . rmddir . '"'
    else
        let rcmd = 'nvim.interlace.rmd("' . expand("%:t") . '", outform = "' . a:t .'", rmddir = "' . rmddir . '"'
    endif
    if b:pdf_is_open || (g:R_openhtml  == 0 && a:t == "html_document") ||
                \ (g:R_openpdf == 0 && (a:t == "pdf_document" || a:t == "beamer_presentation" || a:t == "word_document"))
        let rcmd .= ", view = FALSE"
    elseif g:R_openpdf == 1 && (a:t == "pdf_document" || a:t == "beamer_presentation")
        let b:pdf_is_open = 1
    endif
    let rcmd = rcmd . ', envir = ' . g:R_rmd_environment . ')'
    call g:SendCmdToR(rcmd)
endfunction

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

function RControlMaps()
    " List space, clear console, clear all
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RListSpace',    'rl', ':call g:SendCmdToR("ls()")')
    call RCreateMaps("nvi", '<Plug>RClearConsole', 'rr', ':call RClearConsole()')
    call RCreateMaps("nvi", '<Plug>RClearAll',     'rm', ':call RClearAll()')

    " Print, names, structure
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RObjectPr',     'rp', ':call RAction("print")')
    call RCreateMaps("nvi", '<Plug>RObjectNames',  'rn', ':call RAction("nvim.names")')
    call RCreateMaps("nvi", '<Plug>RObjectStr',    'rt', ':call RAction("str")')
    call RCreateMaps("nvi", '<Plug>RViewDF',       'rv', ':call RAction("viewdf")')

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
    call RCreateMaps("nvi", '<Plug>ROpenLists',        'r=', ':call RBrOpenCloseLs(1)')
    call RCreateMaps("nvi", '<Plug>RCloseLists',       'r-', ':call RBrOpenCloseLs(0)')

    " Render script with rmarkdown
    "-------------------------------------
    call RCreateMaps("nvi", '<Plug>RMakeRmd',       'kr', ':call RMakeRmd("default")')
    call RCreateMaps("nvi", '<Plug>RMakePDFK',      'kp', ':call RMakeRmd("pdf_document")')
    call RCreateMaps("nvi", '<Plug>RMakePDFKb',     'kl', ':call RMakeRmd("beamer_presentation")')
    call RCreateMaps("nvi", '<Plug>RMakeWord',      'kw', ':call RMakeRmd("word_document")')
    call RCreateMaps("nvi", '<Plug>RMakeHTML',      'kh', ':call RMakeRmd("html_document")')
    call RCreateMaps("nvi", '<Plug>RMakeODT',       'ko', ':call RMakeRmd("odt")')
endfunction


" For each noremap we need a vnoremap including <Esc> before the :call,
" otherwise nvim will call the function as many times as the number of selected
" lines. If we put <Esc> in the noremap, nvim will bell.
" RCreateMaps Args:
"   type : modes to which create maps (normal, visual and insert) and whether
"          the cursor have to go the beginning of the line
"   plug : the <Plug>Name
"   combo: combination of letters that make the shortcut
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
        elseif g:R_user_maps_only == 0
            exec 'noremap <buffer><silent> <LocalLeader>' . a:combo . ' ' . tg
        endif
    endif
    if a:type =~ "v"
        if hasmapto(a:plug, "v")
            exec 'vnoremap <buffer><silent> ' . a:plug . ' <Esc>' . tg
        elseif g:R_user_maps_only == 0
            exec 'vnoremap <buffer><silent> <LocalLeader>' . a:combo . ' <Esc>' . tg
        endif
    endif
    if g:R_insert_mode_cmds == 1 && a:type =~ "i"
        if hasmapto(a:plug, "i")
            exec 'inoremap <buffer><silent> ' . a:plug . ' <Esc>' . tg . il
        elseif g:R_user_maps_only == 0
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
    if g:R_assign == 1 || g:R_assign == 2
        silent exe 'inoremap <buffer><silent> ' . g:R_assign_map . ' <Esc>:call ReplaceUnderS()<CR>a'
    endif
    if g:R_args_in_stline
        inoremap <buffer><silent> ( <Esc>:call DisplayArgs()<CR>a
        inoremap <buffer><silent> ) <Esc>:call RestoreStatusLine(1)<CR>a
        autocmd InsertLeave <buffer> call RestoreStatusLine(0)
    endif
    if hasmapto("<Plug>RCompleteArgs", "i")
        inoremap <buffer><silent> <Plug>RCompleteArgs <C-R>=RCompleteArgs()<CR>
    else
        inoremap <buffer><silent> <C-X><C-A> <C-R>=RCompleteArgs()<CR>
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
    call RCreateMaps('v', '<Plug>RSendSelAndInsertOutput', 'so', ':call SendSelectionToR("echo", "stay", "NewtabInsert")')

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
    call RCreateMaps('v', '<Plug>RDSendLineAndInsertOutput', 'o', ':call RWarningMsg("This command does not work over a selection of lines.")')
    call RCreateMaps('i', '<Plug>RSendLAndOpenNewOne', 'q', ':call SendLineToR("newline")')
    call RCreateMaps('n', '<Plug>RNLeftPart', 'r<left>', ':call RSendPartOfLine("left", 0)')
    call RCreateMaps('n', '<Plug>RNRightPart', 'r<right>', ':call RSendPartOfLine("right", 0)')
    call RCreateMaps('i', '<Plug>RILeftPart', 'r<left>', 'l:call RSendPartOfLine("left", 1)')
    call RCreateMaps('i', '<Plug>RIRightPart', 'r<right>', 'l:call RSendPartOfLine("right", 1)')
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
    if IsJobRunning("ClientServer")
        call JobStdin(g:rplugin_jobs["ClientServer"], "\x08Quit\n")
    endif
    call delete(g:rplugin_rsource)
    call delete(g:rplugin_tmpdir . "/start_options.R")
    call delete(g:rplugin_tmpdir . "/eval_reply")
    call delete(g:rplugin_tmpdir . "/formatted_code")
    call delete(g:rplugin_tmpdir . "/GlobalEnvList_" . $NVIMR_ID)
    call delete(g:rplugin_tmpdir . "/globenv_" . $NVIMR_ID)
    call delete(g:rplugin_tmpdir . "/liblist_" . $NVIMR_ID)
    call delete(g:rplugin_tmpdir . "/libnames_" . $NVIMR_ID)
    call delete(g:rplugin_tmpdir . "/objbrowserInit")
    call delete(g:rplugin_tmpdir . "/Rdoc")
    call delete(g:rplugin_tmpdir . "/Rinsert")
    call delete(g:rplugin_tmpdir . "/tmux.conf")
    call delete(g:rplugin_tmpdir . "/unformatted_code")
    call delete(g:rplugin_tmpdir . "/nvimbol_finished")
    call delete(g:rplugin_tmpdir . "/nvimcom_running_" . $NVIMR_ID)
    call delete(g:rplugin_tmpdir . "/run_cmd.bat")
    if executable("rmdir")
        call system("rmdir '" . g:rplugin_tmpdir . "'")
    endif
endfunction

" Tell R to create a list of objects file listing all currently available
" objects in its environment. The file is necessary for omni completion.
function BuildROmniList(pattern)
    if string(g:SendCmdToR) == "function('SendCmdToR_fake')"
        return
    endif

    let omnilistcmd = 'nvimcom:::nvim.bol("' . g:rplugin_tmpdir . "/GlobalEnvList_" . $NVIMR_ID . '"'
    if g:R_allnames == 1
        let omnilistcmd = omnilistcmd . ', allnames = TRUE'
    endif
    let omnilistcmd = omnilistcmd . ', pattern = "' . a:pattern . '")'

    call delete(g:rplugin_tmpdir . "/nvimbol_finished")
    call delete(g:rplugin_tmpdir . "/eval_reply")
    call SendToNvimcom("\x08" . $NVIMR_ID . omnilistcmd)
    if g:rplugin_nvimcom_port == 0
        sleep 500m
        return
    endif
    let g:rplugin_lastev = ReadEvalReply()
    if g:rplugin_lastev =~ "^R error: "
        call RWarningMsg(g:rplugin_lastev)
        sleep 800m
        return
    endif
    sleep 20m
    let ii = 0
    while !filereadable(g:rplugin_tmpdir . "/nvimbol_finished") && ii < 5
        let ii += 1
        sleep
    endwhile
    echon "\r               "
    if ii == 5
        call RWarningMsg("No longer waiting...")
        return
    endif

    let g:rplugin_globalenvlines = readfile(g:rplugin_tmpdir . "/GlobalEnvList_" . $NVIMR_ID)
endfunction

function RFillOmniMenu(base, newbase, prefix, pkg, olines, toplev)
    let resp = []
    for line in a:olines
        if line =~ a:newbase
            " Skip elements of lists unless the user is really looking for them.
            " Skip lists if the user is looking for one of its elements.
            if (a:base !~ '\$' && line =~ '\$') || (a:base =~ '\$' && line !~ '\$')
                continue
            endif
            " Idem with S4 objects
            if (a:base !~ '@' && line =~ '@') || (a:base =~ '@' && line !~ '@')
                continue
            endif
            let sln = split(line, "\x06", 1)
            if a:pkg != "" && sln[3] != a:pkg
                continue
            endif
            if len(a:toplev)
                " Do not show an object from a package if it was masked by a
                " toplevel object in .GlobalEnv
                let masked = 0
                let pkgobj = substitute(sln[0], "\\$.*", "", "")
                let pkgobj = substitute(pkgobj, "@.*", "", "")
                for tplv in a:toplev
                    if tplv == pkgobj
                        let masked = 1
                        continue
                    endif
                endfor
                if masked
                    continue
                endif
            endif
            if g:R_show_args
                let info = sln[4]
                let info = substitute(info, "\t", ", ", "g")
                let info = substitute(info, "\x07", " = ", "g")
                call add(resp, {'word': a:prefix . sln[0], 'menu': sln[1] . ' ' . sln[3], 'info': info})
            else
                call add(resp, {'word': a:prefix . sln[0], 'menu': sln[1] . ' ' . sln[3]})
            endif
        endif
    endfor
    return resp
endfunction

function CompleteR(findstart, base)
    if (&filetype == "rnoweb" || &filetype == "rmd" || &filetype == "rrst" || &filetype == "rhelp") && b:IsInRCode(0) == 0 && b:rplugin_nonr_omnifunc != ""
        let Ofun = function(b:rplugin_nonr_omnifunc)
        let thebegin = Ofun(a:findstart, a:base)
        return thebegin
    endif
    if a:findstart
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && (line[start - 1] =~ '\w' || line[start - 1] =~ '\.' || line[start - 1] =~ '\$' || line[start - 1] =~ '@' || line[start - 1] =~ ':')
            let start -= 1
        endwhile
        return start
    else
        let resp = []

        if strlen(a:base) == 0
            return resp
        endif

        if len(g:rplugin_omni_lines) == 0
            call add(resp, {'word': a:base, 'menu': " [ List is empty. Was nvimcom library ever loaded? ]"})
        endif

        if a:base =~ ":::"
            return resp
        elseif a:base =~ "::"
            let newbase = substitute(a:base, ".*::", "", "")
            let prefix = substitute(a:base, "::.*", "::", "")
            let pkg = substitute(a:base, "::.*", "", "")
        else
            let newbase = a:base
            let prefix = ""
            let pkg = ""
        endif

        " The char '$' at the end of 'a:base' is treated as end of line, and
        " the pattern is never found in 'line'.
        let newbase = '^' . substitute(newbase, "\\$$", "", "")

        if pkg == ""
            call BuildROmniList(a:base)
            let resp = RFillOmniMenu(a:base, newbase, prefix, pkg, g:rplugin_globalenvlines, [])
            if filereadable(g:rplugin_tmpdir . "/nvimbol_finished")
                let toplev = readfile(g:rplugin_tmpdir . "/nvimbol_finished")
            else
                let toplev = []
            endif
            let resp += RFillOmniMenu(a:base, newbase, prefix, pkg, g:rplugin_omni_lines, toplev)
        else
            let omf = split(globpath(g:rplugin_compldir, 'omnils_' . pkg . '_*'), "\n")
            if len(omf) == 1
                let olines = readfile(omf[0])
                if len(olines) == 0 || (len(olines) == 1 && len(olines[0]) < 3)
                    return resp
                endif
                let resp = RFillOmniMenu(a:base, newbase, prefix, pkg, olines, [])
            else
                call add(resp, {'word': a:base, 'menu': ' [ List is empty. Was "' . pkg . '" library ever loaded? ]'})
            endif
        endif

        return resp
    endif
endfun

function RSourceOtherScripts()
    if exists("g:R_source")
        let flist = split(g:R_source, ",")
        for fl in flist
            if fl =~ " "
                call RWarningMsgInp("Invalid file name (empty spaces are not allowed): '" . fl . "'")
            else
                exe "source " . escape(fl, ' \')
            endif
        endfor
    endif
endfunction

function RBuildTags()
    if filereadable("TAGS")
        call RWarningMsg('The file "TAGS" exists. Please, delete it and try again.')
        return
    endif
    call g:SendCmdToR('rtags(ofile = "TAGS"); etags2ctags("TAGS", "tags"); unlink("TAGS")')
endfunction

command -nargs=1 -complete=customlist,RLisObjs Rinsert :call RInsert(<q-args>)
command -range=% Rformat <line1>,<line2>:call RFormatCode()
command RBuildTags :call RBuildTags()
command -nargs=? -complete=customlist,RLisObjs Rhelp :call RAskHelp(<q-args>)
command -nargs=? -complete=dir RSourceDir :call RSourceDirectory(<q-args>)
command RStop :call StopR()


"==========================================================================
" Global variables
" Convention: R_        for user options
"             rplugin_  for internal parameters
"==========================================================================

if !exists("g:rplugin_compldir")
    runtime R/setcompldir.vim
endif


if exists("g:R_tmpdir")
    let g:rplugin_tmpdir = expand(g:R_tmpdir)
else
    if has("win32")
        if isdirectory($TMP)
            let g:rplugin_tmpdir = $TMP . "/NvimR-" . g:rplugin_userlogin
        elseif isdirectory($TEMP)
            let g:rplugin_tmpdir = $TEMP . "/Nvim-R-" . g:rplugin_userlogin
        else
            let g:rplugin_tmpdir = g:rplugin_uservimfiles . "/R/tmp"
        endif
        let g:rplugin_tmpdir = substitute(g:rplugin_tmpdir, "\\", "/", "g")
    else
        if isdirectory($TMPDIR)
            if $TMPDIR =~ "/$"
                let g:rplugin_tmpdir = $TMPDIR . "Nvim-R-" . g:rplugin_userlogin
            else
                let g:rplugin_tmpdir = $TMPDIR . "/Nvim-R-" . g:rplugin_userlogin
            endif
        elseif isdirectory("/tmp")
            let g:rplugin_tmpdir = "/tmp/Nvim-R-" . g:rplugin_userlogin
        else
            let g:rplugin_tmpdir = g:rplugin_uservimfiles . "/R/tmp"
        endif
    endif
endif

let $NVIMR_TMPDIR = g:rplugin_tmpdir
if !isdirectory(g:rplugin_tmpdir)
    call mkdir(g:rplugin_tmpdir, "p", 0700)
endif

" Make the file name of files to be sourced
let g:rplugin_rsource = g:rplugin_tmpdir . "/Rsource-" . getpid()

let g:rplugin_is_darwin = system("uname") =~ "Darwin"

" Variables whose default value is fixed
call RSetDefaultValue("g:R_allnames",          0)
call RSetDefaultValue("g:R_rmhidden",          0)
call RSetDefaultValue("g:R_assign",            1)
call RSetDefaultValue("g:R_assign_map",    "'_'")
call RSetDefaultValue("g:R_args_in_stline",    0)
call RSetDefaultValue("g:R_rnowebchunk",       1)
call RSetDefaultValue("g:R_strict_rst",        1)
call RSetDefaultValue("g:R_synctex",           1)
call RSetDefaultValue("g:R_openhtml",          0)
call RSetDefaultValue("g:R_nvim_wd",           0)
call RSetDefaultValue("g:R_commented_lines",   0)
call RSetDefaultValue("g:R_after_start",    "''")
call RSetDefaultValue("g:R_vsplit",            0)
call RSetDefaultValue("g:R_csv_warn",          1)
call RSetDefaultValue("g:R_rconsole_width",   -1)
call RSetDefaultValue("g:R_rconsole_height",  15)
call RSetDefaultValue("g:R_tmux_title","'NvimR'")
call RSetDefaultValue("g:R_listmethods",       0)
call RSetDefaultValue("g:R_specialplot",       0)
call RSetDefaultValue("g:R_notmuxconf",        0)
call RSetDefaultValue("g:R_routnotab",         0)
call RSetDefaultValue("g:R_editor_w",         66)
call RSetDefaultValue("g:R_help_w",           46)
call RSetDefaultValue("g:R_objbr_w",          40)
call RSetDefaultValue("g:R_objbr_opendf",      1)
call RSetDefaultValue("g:R_objbr_openlist",    0)
call RSetDefaultValue("g:R_objbr_allnames",    0)
call RSetDefaultValue("g:R_objbr_labelerr",    1)
call RSetDefaultValue("g:R_applescript",       0)
call RSetDefaultValue("g:R_tmux_split",        0)
call RSetDefaultValue("g:R_esc_term",          1)
call RSetDefaultValue("g:R_close_term",        1)
call RSetDefaultValue("g:R_nvimcom_wait",   5000)
call RSetDefaultValue("g:R_show_args",         0)
call RSetDefaultValue("g:R_never_unmake_menu", 0)
call RSetDefaultValue("g:R_insert_mode_cmds",  0)
call RSetDefaultValue("g:R_source",         "''")
call RSetDefaultValue("g:R_in_buffer",         1)
call RSetDefaultValue("g:R_open_example",      1)
if !exists("*termopen")
    let g:R_in_buffer = 0
endif
if g:R_in_buffer
    call RSetDefaultValue("g:R_nvimpager", "'vertical'")
else
    call RSetDefaultValue("g:R_nvimpager",      "'tab'")
endif
call RSetDefaultValue("g:R_objbr_place",     "'script,right'")
call RSetDefaultValue("g:R_source_args",  "'print.eval=TRUE'")
call RSetDefaultValue("g:R_user_maps_only", 0)
call RSetDefaultValue("g:R_latexcmd", "'default'")
call RSetDefaultValue("g:R_texerr",             1)
call RSetDefaultValue("g:R_rmd_environment", "'.GlobalEnv'")
call RSetDefaultValue("g:R_indent_commented",  1)

call RSetDefaultValue("g:R_hi_fun", 1)
if !g:R_hi_fun
    " Declare empty function to be called by nvimcom
    function FillRLibList()
    endfunction
endif

if g:rplugin_is_darwin
    call RSetDefaultValue("g:R_openpdf", 1)
    let g:R_pdfviewer = "skim"
else
    call RSetDefaultValue("g:R_openpdf", 2)
    if has("win32")
        let g:R_pdfviewer = "sumatra"
    else
        call RSetDefaultValue("g:R_pdfviewer", "'zathura'")
    endif
endif

if !exists("g:r_indent_ess_comments")
    let g:r_indent_ess_comments = 0
endif
if g:r_indent_ess_comments
    if g:R_indent_commented
        call RSetDefaultValue("g:R_rcomment_string", "'## '")
    else
        call RSetDefaultValue("g:R_rcomment_string", "'### '")
    endif
else
    call RSetDefaultValue("g:R_rcomment_string", "'# '")
endif

if g:R_in_buffer
    let g:R_save_win_pos = 0
    let g:R_arrange_windows  = 0
endif
if has("win32")
    call RSetDefaultValue("g:R_save_win_pos",    1)
    call RSetDefaultValue("g:R_arrange_windows", 1)
else
    call RSetDefaultValue("g:R_save_win_pos",    0)
    call RSetDefaultValue("g:R_arrange_windows", 0)
endif

" Look for invalid options
let objbrplace = split(g:R_objbr_place, ",")
let obpllen = len(objbrplace) - 1
if obpllen > 1
    call RWarningMsgInp("Too many options for R_objbr_place.")
    let g:rplugin_failed = 1
    finish
endif
for idx in range(0, obpllen)
    if objbrplace[idx] != "console" && objbrplace[idx] != "script" && objbrplace[idx] != "left" && objbrplace[idx] != "right"
        call RWarningMsgInp('Invalid option for R_objbr_place: "' . objbrplace[idx] . '". Valid options are: console or script and right or left."')
        let g:rplugin_failed = 1
        finish
    endif
endfor
unlet objbrplace
unlet obpllen


" ^K (\013) cleans from cursor to the right and ^U (\025) cleans from cursor
" to the left. However, ^U causes a beep if there is nothing to clean. The
" solution is to use ^A (\001) to move the cursor to the beginning of the line
" before sending ^K. But the control characters may cause problems in some
" circumstances.
call RSetDefaultValue("g:R_ca_ck", 0)

" ========================================================================
" Check if default mean of communication with R is OK

if g:rplugin_is_darwin
    if !exists("g:macvim_skim_app_path")
        let g:macvim_skim_app_path = '/Applications/Skim.app'
    endif
else
    let g:R_applescript = 0
endif

if has("gui_running") || g:R_applescript || g:R_in_buffer || $TMUX == ""
    let g:R_tmux_split = 0
endif

if !g:R_in_buffer && !g:R_tmux_split
    let g:R_objbr_place = substitute(g:R_objbr_place, "console", "script", "")
endif


" ========================================================================

" Start with an empty list of objects in the workspace
let g:rplugin_globalenvlines = []

" Minimum width for the Object Browser
if g:R_objbr_w < 10
    let g:R_objbr_w = 10
endif

" Control the menu 'R' and the tool bar buttons
if !exists("g:rplugin_hasmenu")
    let g:rplugin_hasmenu = 0
endif

" List of marks that the plugin seeks to find the block to be sent to R
let s:all_marks = "abcdefghijklmnopqrstuvwxyz"

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
let g:rplugin_status_line = &statusline
let s:displaying_args = 0
let g:rplugin_running_objbr = 0
let g:rplugin_running_rhelp = 0
let g:rplugin_r_pid = 0
let g:rplugin_myport = 0
let g:rplugin_ob_port = 0
let g:rplugin_nvimcom_port = 0
let g:rplugin_lastev = ""

let s:filelines = readfile(g:rplugin_home . "/R/nvimcom/DESCRIPTION")
let s:required_nvimcom = substitute(s:filelines[1], "Version: ", "", "")
let s:required_nvimcom_dot = substitute(s:required_nvimcom, "-", ".", "")
unlet s:filelines

let g:rplugin_nvimcom_version = "0"
let g:rplugin_nvimcom_home = ""
let g:rplugin_nvimcom_bin_dir = ""
if filereadable(g:rplugin_compldir . "/nvimcom_info")
    let s:filelines = readfile(g:rplugin_compldir . "/nvimcom_info")
    if len(s:filelines) == 3
        let g:rplugin_nvimcom_version = s:filelines[0]
        let g:rplugin_nvimcom_home = s:filelines[1]
        let g:rplugin_nvimcom_bin_dir = s:filelines[2]
    endif
    unlet s:filelines
endif

if has("nvim")
    runtime R/nvimrcom.vim
else
    runtime R/vimrcom.vim
endif

" SyncTeX options
let g:rplugin_has_wmctrl = 0
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
    elseif !has("win32")
        let randnum = system("echo $RANDOM")
    else
        let randnum = localtime()
    endif
    return substitute(randnum, '\W', '', 'g')
endfunction

" If this is the Object Browser running in a Tmux pane, $NVIMR_ID is
" already defined and shouldn't be changed
if &filetype == "rbrowser"
    if $NVIMR_ID == ""
        call RWarningMsgInp("NVIMR_ID is undefined")
    endif
else
    let $NVIMR_SECRET = GetRandomNumber(16)
    let $NVIMR_ID = GetRandomNumber(16)
endif

let g:rplugin_docfile = g:rplugin_tmpdir . "/Rdoc"

" Create an empty file to avoid errors if the user do Ctrl-X Ctrl-O before
" starting R:
if &filetype != "rbrowser"
    call writefile([], g:rplugin_tmpdir . "/GlobalEnvList_" . $NVIMR_ID)
endif

" Set the name of R executable
if exists("g:R_app")
    let g:rplugin_R = g:R_app
    if !has("win32") && !exists("g:R_cmd")
        let g:R_cmd = g:R_app
    endif
else
    if has("win32")
        if g:R_in_buffer
            let g:rplugin_R = "Rterm.exe"
        else
            let g:rplugin_R = "Rgui.exe"
        endif
    else
        let g:rplugin_R = "R"
    endif
endif

" Set the name of R executable to be used in `R CMD`
if exists("g:R_cmd")
    let g:rplugin_Rcmd = g:R_cmd
else
    let g:rplugin_Rcmd = "R"
endif

" Add R directory to the $PATH
if exists("g:R_path")
    let g:rplugin_R_path = expand(g:R_path)
    if !isdirectory(g:rplugin_R_path)
        call RWarningMsgInp('"' . g:R_path . '" is not a directory. Fix the value of R_path in your vimrc.')
        let g:rplugin_failed = 1
        finish
    endif
    if $PATH !~ g:rplugin_R_path
        if has("win32")
            let $PATH = g:rplugin_R_path . ';' . $PATH
        else
            let $PATH = g:rplugin_R_path . ':' . $PATH
        endif
    endif
    if !executable(g:rplugin_R)
        call RWarningMsgInp('"' . g:rplugin_R . '" not found. Fix the value of either R_path or R_app in your vimrc.')
        let g:rplugin_failed = 1
        finish
    endif
endif

if has("win32")
    runtime R/windows.vim
endif

if g:R_applescript
    runtime R/osx.vim
endif

if !has("win32") && !g:R_applescript && !g:R_in_buffer
    runtime R/tmux.vim
endif

if g:R_in_buffer
    runtime R/nvimbuffer.vim
endif

if has("gui_running")
    runtime R/gui_running.vim
endif

if !executable(g:rplugin_R)
    call RWarningMsgInp("R executable not found: '" . g:rplugin_R . "'")
endif

" Check if there is more than one copy of Nvim-R
" (e.g. from the Vimballl and from a plugin manager)
let s:ff = split(substitute(globpath(&rtp, "R/functions.vim"), "functions.vim", "", "g"), "\n")
let s:ft = split(globpath(&rtp, "ftplugin/r*_nvimr.vim"), "\n")
if len(s:ff) > 1 || len(s:ft) > 5
    call RWarningMsgInp("It seems that Nvim-R is installed in more than one place.\n" .
                \ "Please, remove one of them to avoid conflicts.\n" .
                \ "Below is a list of some of the possibly duplicated directories and files:\n" . join(s:ff, "\n") . "\n" . join(s:ft, "\n") . "\n")
endif
unlet s:ff
unlet s:ft
R/extern_term.vim	[[[1
174

function StartR_ExternalTerm(rcmd)
    if g:R_notmuxconf
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

        if g:R_term == "rxvt" || g:R_term == "urxvt"
            let cnflines = cnflines + [
                        \ "set terminal-overrides 'rxvt*:smcup@:rmcup@'" ]
        endif

        call writefile(cnflines, g:rplugin_tmpdir . "/tmux.conf")
        let tmuxcnf = '-f "' . g:rplugin_tmpdir . "/tmux.conf" . '"'
    endif

    let rcmd = 'NVIMR_TMPDIR=' . substitute(g:rplugin_tmpdir, ' ', '\\ ', 'g') .
                \ ' NVIMR_COMPLDIR=' . substitute(g:rplugin_compldir, ' ', '\\ ', 'g') .
                \ ' NVIMR_ID=' . $NVIMR_ID .
                \ ' NVIMR_SECRET=' . $NVIMR_SECRET .
                \ ' R_DEFAULT_PACKAGES=' . $R_DEFAULT_PACKAGES . ' ' . a:rcmd

    call system("tmux -L NvimR has-session -t " . g:rplugin_tmuxsname)
    if v:shell_error
        if g:rplugin_is_darwin
            let rcmd = 'TERM=screen-256color ' . rcmd
            let opencmd = printf("tmux -L NvimR -2 %s new-session -s %s '%s'",
                        \ tmuxcnf, g:rplugin_tmuxsname, rcmd)
            call writefile(["#!/bin/sh", opencmd], $NVIMR_TMPDIR . "/openR")
            call system("chmod +x '" . $NVIMR_TMPDIR . "/openR'")
            let opencmd = "open '" . $NVIMR_TMPDIR . "/openR'"
        else
            let opencmd = printf("%s tmux -L NvimR -2 %s new-session -s %s \"%s\"",
                        \ g:rplugin_termcmd, tmuxcnf, g:rplugin_tmuxsname, rcmd)
        endif
    else
        if g:rplugin_is_darwin
            call RWarningMsg("Tmux session with R is already running")
            return
        endif
        let opencmd = printf("%s tmux -L NvimR -2 %s attach-session -d -t %s",
                    \ g:rplugin_termcmd, tmuxcnf, g:rplugin_tmuxsname)
    endif

    if g:R_silent_term
        let opencmd .= " &"
        let rlog = system(opencmd)
        if v:shell_error
            call RWarningMsg(rlog)
            return
        endif
    else
        let initterm = ['cd "' . getcwd() . '"',
                    \ opencmd ]
        call writefile(initterm, g:rplugin_tmpdir . "/initterm_" . $NVIMR_ID . ".sh")
        if has("nvim")
            let g:rplugin_jobs["Terminal emulator"] = StartJob("sh '" . g:rplugin_tmpdir . "/initterm_" . $NVIMR_ID . ".sh'",
                        \ {'on_stderr': function('ROnJobStderr'), 'on_exit': function('ROnJobExit'), 'detach': 1})
        else
            let g:rplugin_jobs["Terminal emulator"] = StartJob(["sh", g:rplugin_tmpdir . "/initterm_" . $NVIMR_ID . ".sh"],
                        \ {'err_cb': 'ROnJobStderr', 'exit_cb': 'ROnJobExit'})
        endif
    endif

    let g:SendCmdToR = function('SendCmdToR_Term')
    if WaitNvimcomStart()
        if g:R_after_start != ''
            call system(g:R_after_start)
        endif
    endif
    call delete(g:rplugin_tmpdir . "/initterm_" . $NVIMR_ID . ".sh")
    call delete(g:rplugin_tmpdir . "/openR")
endfunction

function SendCmdToR_Term(...)
    if g:R_ca_ck
        let cmd = "\001" . "\013" . a:1
    else
        let cmd = a:1
    endif

    " Send the command to R running in an external terminal emulator
    let str = substitute(cmd, "'", "'\\\\''", "g")
    if str =~ '^-'
        let str = ' ' . str
    endif
    if a:0 == 2 && a:2 == 0
        let scmd = "tmux -L NvimR set-buffer '" . str . "' && tmux -L NvimR paste-buffer -t " . g:rplugin_tmuxsname . '.0'
    else
        let scmd = "tmux -L NvimR set-buffer '" . str . "\<C-M>' && tmux -L NvimR paste-buffer -t " . g:rplugin_tmuxsname . '.0'
    endif
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

" The Object Browser can run in a Tmux pane only if Neovim is inside a Tmux session
let g:R_objbr_place = substitute(g:R_objbr_place, "console", "script", "")

call RSetDefaultValue("g:R_silent_term", 0)

if g:rplugin_is_darwin
    let g:R_term = "xterm"
    finish
endif

" Choose a terminal (code adapted from screen.vim)
if exists("g:R_term")
    if !executable(g:R_term)
        call RWarningMsgInp("'" . g:R_term . "' not found. Please change the value of 'R_term' in your vimrc.")
        let g:R_term = "xterm"
    endif
endif

if !exists("g:R_term")
    let s:terminals = ['gnome-terminal', 'konsole', 'xfce4-terminal', 'Eterm',
                \ 'rxvt', 'urxvt', 'aterm', 'roxterm', 'terminator', 'lxterminal', 'xterm']
    for s:term in s:terminals
        if executable(s:term)
            let g:R_term = s:term
            break
        endif
    endfor
    unlet s:term
    unlet s:terminals
endif

if !exists("g:R_term") && !exists("g:R_term_cmd")
    call RWarningMsgInp("Please, set the variable 'g:R_term_cmd' in your vimrc. Read the plugin documentation for details.")
    let g:rplugin_failed = 1
    finish
endif

let g:rplugin_termcmd = g:R_term

if g:R_term =~ '^\(gnome-terminal\|xfce4-terminal\|roxterm\|terminator\|Eterm\|aterm\|lxterminal\|rxvt\|urxvt\)$'
    let g:rplugin_termcmd = g:rplugin_termcmd . " --title R"
elseif g:R_term == '^\(xterm\|uxterm\|lxterm\)$'
    let g:rplugin_termcmd = g:rplugin_termcmd . " -title R"
endif

if !g:R_nvim_wd
    if g:R_term =~ '^\(gnome-terminal\|xfce4-terminal\|terminator\|lxterminal\)$'
        let g:rplugin_termcmd = g:R_term . " --working-directory='" . expand("%:p:h") . "'"
    elseif g:R_term == "konsole"
        let g:rplugin_termcmd = "konsole --workdir '" . expand("%:p:h") . "'"
    elseif g:R_term == "roxterm"
        let g:rplugin_termcmd = "roxterm --directory='" . expand("%:p:h") . "'"
    endif
endif

if g:R_term == "gnome-terminal" || g:R_term == "xfce4-terminal"
    let g:rplugin_termcmd = g:rplugin_termcmd . " -x"
else
    let g:rplugin_termcmd = g:rplugin_termcmd . " -e"
endif

" Override default settings:
if exists("g:R_term_cmd")
    let g:rplugin_termcmd = g:R_term_cmd
endif
R/flag.vim	[[[1
1
let g:NvimR_installed = 1
R/functions.vim	[[[1
181

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

" Users may define the value of g:R_start_libs
if !exists("g:R_start_libs")
    let g:R_start_libs = "base,stats,graphics,grDevices,utils,methods"
endif

let g:rplugin_lists_to_load = split(g:R_start_libs, ",")
let g:rplugin_debug_lists = []
let g:rplugin_loaded_lists = []
let g:rplugin_Rhelp_list = []
let g:rplugin_omni_lines = []
let g:rplugin_new_libs = 0

" syntax/r.vim may have being called before ftplugin/r.vim
if !exists("g:rplugin_compldir")
    runtime R/setcompldir.vim
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

            " Library setwidth has no functions
            if len(olist) == 0 || (len(olist) == 1 && len(olist[0]) < 3)
                return
            endif

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
" Function called by nvimcom
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function FillRLibList()
    " Update the list of objects for omnicompletion
    if filereadable(g:rplugin_tmpdir . "/libnames_" . $NVIMR_ID)
        let g:rplugin_lists_to_load = readfile(g:rplugin_tmpdir . "/libnames_" . $NVIMR_ID)
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
        call delete(g:rplugin_tmpdir . "/libnames_" . $NVIMR_ID)
    endif
    " Now we need to update the syntax in all R files. There should be a
    " better solution than setting a flag to let other buffers know that they
    " also need to update the syntax on CursorMoved event:
    " https://github.com/neovim/neovim/issues/901
    let g:rplugin_new_libs = len(g:rplugin_loaded_lists)
    silent exe 'set syntax=' . &syntax
    redraw
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
    redraw
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
R/gui_running.vim	[[[1
306
" This file contains code used only if has("gui_running")

if exists("g:did_nvimr_gui_running")
    finish
endif
let g:did_nvimr_gui_running = 1

if exists('g:maplocalleader')
    let s:tll = '<Tab>' . g:maplocalleader
else
    let s:tll = '<Tab>\\'
endif

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
    call RCreateMenuItem("nvi", 'Object\ browser.Expand\ (all\ lists)', '<Plug>ROpenLists', 'r=', ':call RBrOpenCloseLs(1)')
    call RCreateMenuItem("nvi", 'Object\ browser.Collapse\ (all\ lists)', '<Plug>RCloseLists', 'r-', ':call RBrOpenCloseLs(0)')
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
    call RCreateMenuItem("nvi", 'Command.Names\ (cur)', '<Plug>RObjectNames', 'rn', ':call RAction("nvim.names")')
    call RCreateMenuItem("nvi", 'Command.Structure\ (cur)', '<Plug>RObjectStr', 'rt', ':call RAction("str")')
    call RCreateMenuItem("nvi", 'Command.View\ data\.frame\ (cur)', '<Plug>RViewDF', 'rv', ':call RAction("viewdf")')
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
    call RCreateMenuItem("nvi", 'Start/Close.Start\ R\ (custom)', '<Plug>RCustomStart', 'rc', ':call StartR("custom")')
    "-------------------------------
    menu R.Start/Close.-Sep1- <nul>
    call RCreateMenuItem("nvi", 'Start/Close.Close\ R\ (no\ save)', '<Plug>RClose', 'rq', ":call RQuit('no')")
    menu R.Start/Close.-Sep2- <nul>

    nmenu <silent> R.Start/Close.Stop\ R<Tab>:RStop :RStop<CR>

    "----------------------------------------------------------------------------
    " Send
    "----------------------------------------------------------------------------
    if &filetype == "r" || g:R_never_unmake_menu
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
    if &filetype == "rnoweb" || &filetype == "rmd" || &filetype == "rrst" || g:R_never_unmake_menu
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
    call RCreateMenuItem("v", 'Send.Selection\ (and\ insert\ output)', '<Plug>RSendSelAndInsertOutput', 'so', ':call SendSelectionToR("echo", "stay", "NewtabInsert")')
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
    if &filetype == "rnoweb" || g:R_never_unmake_menu
        menu R.Command.-Sep5- <nul>
        call RCreateMenuItem("nvi", 'Command.Sweave\ (cur\ file)', '<Plug>RSweave', 'sw', ':call RWeave("nobib", 0, 0)')
        call RCreateMenuItem("nvi", 'Command.Sweave\ and\ PDF\ (cur\ file)', '<Plug>RMakePDF', 'sp', ':call RMakePDF("nobib", 0)')
        call RCreateMenuItem("nvi", 'Command.Sweave,\ BibTeX\ and\ PDF\ (cur\ file)', '<Plug>RBibTeX', 'sb', ':call RMakePDF("bibtex", 0)')
    endif
    menu R.Command.-Sep6- <nul>
    if &filetype == "rnoweb"
        call RCreateMenuItem("nvi", 'Command.Knit\ (cur\ file)', '<Plug>RKnit', 'kn', ':call RWeave("nobib", 1, 0)')
        call RCreateMenuItem("nvi", 'Command.Knit,\ BibTeX\ and\ PDF\ (cur\ file)', '<Plug>RBibTeXK', 'kb', ':call RMakePDFK("bibtex", 1)')
    else
        call RCreateMenuItem("nvi", 'Command.Knit\ (cur\ file)', '<Plug>RKnit', 'kn', ':call RKnit()')
    endif
    call RCreateMenuItem("nvi", 'Command.Knit\ and\ PDF\ (cur\ file)', '<Plug>RMakePDFK', 'kp', ':call RMakeRmd("pdf_document")')
    call RCreateMenuItem("nvi", 'Command.Knit\ and\ Beamer\ PDF\ (cur\ file)', '<Plug>RMakePDFKb', 'kl', ':call RMakeRmd("beamer_presentation")')
    call RCreateMenuItem("nvi", 'Command.Knit\ and\ HTML\ (cur\ file)', '<Plug>RMakeHTML', 'kh', ':call RMakeRmd("html_document")')
    call RCreateMenuItem("nvi", 'Command.Knit\ and\ ODT\ (cur\ file)', '<Plug>RMakeODT', 'ko', ':call RMakeRmd("odt")')
    call RCreateMenuItem("nvi", 'Command.Knit\ and\ Word\ Document\ (cur\ file)', '<Plug>RMakeWord', 'kw', ':call RMakeRmd("word_document")')
    call RCreateMenuItem("nvi", 'Command.Markdown\ render\ (cur\ file)', '<Plug>RMakeRmd', 'kr', ':call RMakeRmd("default")')
    if &filetype == "r" || g:R_never_unmake_menu
        call RCreateMenuItem("nvi", 'Command.Spin\ (cur\ file)', '<Plug>RSpin', 'ks', ':call RSpin()')
    endif
    if ($DISPLAY != "" && g:R_synctex && &filetype == "rnoweb") || g:R_never_unmake_menu
        menu R.Command.-Sep61- <nul>
        call RCreateMenuItem("nvi", 'Command.Open\ PDF\ (cur\ file)', '<Plug>ROpenPDF', 'op', ':call ROpenPDF("Get Master")')
        call RCreateMenuItem("nvi", 'Command.Search\ forward\ (SyncTeX)', '<Plug>RSyncFor', 'gp', ':call SyncTeX_forward()')
        call RCreateMenuItem("nvi", 'Command.Go\ to\ LaTeX\ (SyncTeX)', '<Plug>RSyncTex', 'gt', ':call SyncTeX_forward(1)')
    endif
    "-------------------------------
    if &filetype == "r" || &filetype == "rnoweb" || g:R_never_unmake_menu
        menu R.Command.-Sep72- <nul>
        nmenu <silent> R.Command.Build\ tags\ file\ (cur\ dir)<Tab>:RBuildTags :call RBuildTags()<CR>
        imenu <silent> R.Command.Build\ tags\ file\ (cur\ dir)<Tab>:RBuildTags <Esc>:call RBuildTags()<CR>a
    endif

    menu R.-Sep7- <nul>

    "----------------------------------------------------------------------------
    " Edit
    "----------------------------------------------------------------------------
    if &filetype == "r" || &filetype == "rnoweb" || &filetype == "rrst" || &filetype == "rhelp" || g:R_never_unmake_menu
        if g:R_assign == 1 || g:R_assign == 2
            silent exe 'imenu <silent> R.Edit.Insert\ \"\ <-\ \"<Tab>' . g:R_assign_map . ' <Esc>:call ReplaceUnderS()<CR>a'
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
        if &filetype == "rnoweb" || &filetype == "rrst" || &filetype == "rmd" || g:R_never_unmake_menu
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
    amenu R.Help\ (plugin).Overview :help Nvim-R-overview<CR>
    amenu R.Help\ (plugin).Main\ features :help Nvim-R-features<CR>
    amenu R.Help\ (plugin).Installation :help Nvim-R-installation<CR>
    amenu R.Help\ (plugin).Use :help Nvim-R-use<CR>
    amenu R.Help\ (plugin).Known\ bugs\ and\ workarounds :help Nvim-R-known-bugs<CR>

    amenu R.Help\ (plugin).Options.Assignment\ operator\ and\ Rnoweb\ code :help R_assign<CR>
    amenu R.Help\ (plugin).Options.Object\ Browser :help R_objbr_place<CR>
    amenu R.Help\ (plugin).Options.Vim\ as\ pager\ for\ R\ help :help R_nvimpager<CR>
    if !has("win32")
        amenu R.Help\ (plugin).Options.Terminal\ emulator :help R_term<CR>
    endif
    if g:rplugin_is_darwin
        amenu R.Help\ (plugin).Options.Integration\ with\ Apple\ Script :help R_applescript<CR>
    endif
    amenu R.Help\ (plugin).Options.R\ path :help R_path<CR>
    amenu R.Help\ (plugin).Options.Arguments\ to\ R :help R_args<CR>
    amenu R.Help\ (plugin).Options.Omni\ completion\ when\ R\ not\ running :help R_start_libs<CR>
    amenu R.Help\ (plugin).Options.Syntax\ highlighting\ of\ \.Rout\ files :help R_routmorecolors<CR>
    amenu R.Help\ (plugin).Options.Automatically\ open\ the\ \.Rout\ file :help R_routnotab<CR>
    amenu R.Help\ (plugin).Options.Special\ R\ functions :help R_listmethods<CR>
    amenu R.Help\ (plugin).Options.Indent\ commented\ lines :help R_indent_commented<CR>
    amenu R.Help\ (plugin).Options.LaTeX\ command :help R_latexcmd<CR>
    amenu R.Help\ (plugin).Options.Never\ unmake\ the\ R\ menu :help R_never_unmake_menu<CR>

    amenu R.Help\ (plugin).Custom\ key\ bindings :help Nvim-R-key-bindings<CR>
    amenu R.Help\ (plugin).Files :help Nvim-R-files<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.All\ tips :help Nvim-R-tips<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Indenting\ setup :help Nvim-R-indenting<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Folding\ setup :help Nvim-R-folding<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Remap\ LocalLeader :help Nvim-R-localleader<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Customize\ key\ bindings :help Nvim-R-bindings<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.ShowMarks :help Nvim-R-showmarks<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.SnipMate :help Nvim-R-snippets<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.LaTeX-Box :help Nvim-R-latex-box<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Highlight\ marks :help Nvim-R-showmarks<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Global\ plugin :help Nvim-R-global<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Jump\ to\ function\ definitions :help Nvim-R-tagsfile<CR>
    amenu R.Help\ (plugin).News :help Nvim-R-news<CR>

    amenu R.Help\ (R)<Tab>:Rhelp :call g:SendCmdToR("help.start()")<CR>
    let g:rplugin_hasmenu = 1
endfunction

function UnMakeRMenu()
    if g:rplugin_hasmenu == 0 || g:R_never_unmake_menu == 1 || &previewwindow || (&buftype == "nofile" && &filetype != "rbrowser")
        return
    endif
    aunmenu R
    let g:rplugin_hasmenu = 0
endfunction

function MakeRBrowserMenu()
    let g:rplugin_curbuf = bufname("%")
    if g:rplugin_hasmenu == 1
        return
    endif
    menutranslate clear
    call RControlMenu()
    call RBrowserMenu()
endfunction

R/nvimbuffer.vim	[[[1
153
" This file contains code used only when R run in Neovim buffer

function ExeOnRTerm(cmd)
    let curwin = winnr()
    exe 'sb ' . g:rplugin_R_bufname
    exe a:cmd
    call cursor("$", 1)
    exe curwin . 'wincmd w'
endfunction

function SendCmdToR_Neovim(...)
    if g:rplugin_jobs["R"]
        if g:R_ca_ck
            let cmd = "\001" . "\013" . a:1
        else
            let cmd = a:1
        endif

        if !exists("g:R_hl_term") || !exists("g:R_setwidth")
            call SendToNvimcom("\x08" . $NVIMR_ID . 'paste(search(), collapse=" ")')
            let g:rplugin_lastev = ReadEvalReply()
            if !exists("g:R_hl_term")
                if g:rplugin_lastev =~ "colorout"
                    let g:R_hl_term = 0
                else
                    let g:R_hl_term = 1
                endif
            endif
            if !exists("g:R_setwidth")
                if g:rplugin_lastev =~ "setwidth"
                    let g:R_setwidth = 0
                else
                    let g:R_setwidth = 1
                endif
            endif
        endif

        if !exists("g:rplugin_hl_term")
            let g:rplugin_hl_term = g:R_hl_term
            if g:rplugin_hl_term
                call ExeOnRTerm('set filetype=rout')
            endif
        endif

        " Update the width, if necessary
        if g:R_setwidth && len(filter(tabpagebuflist(), "v:val =~ bufnr(g:rplugin_R_bufname)")) >= 1
            call ExeOnRTerm("let s:rwnwdth = winwidth(0)")
            if s:rwnwdth != g:rplugin_R_width && s:rwnwdth != -1 && s:rwnwdth > 10 && s:rwnwdth < 999
                let g:rplugin_R_width = s:rwnwdth
                call SendToNvimcom("\x08" . $NVIMR_ID . "options(width=" . g:rplugin_R_width. ")")
                sleep 10m
            endif
        endif

        if a:0 == 2 && a:2 == 0
            call jobsend(g:rplugin_jobs["R"], cmd)
        else
            call jobsend(g:rplugin_jobs["R"], cmd . "\n")
        endif
        return 1
    else
        call RWarningMsg("Is R running?")
        return 0
    endif
endfunction

function OnTermClose()
    if exists("g:rplugin_R_bufname")
        if g:rplugin_R_bufname == bufname("%")
            if g:R_close_term
                call feedkeys('<cr>')
            endif
        endif
        unlet g:rplugin_R_bufname
    endif

    " Set nvimcom port to 0 in nclientserver
    if g:rplugin_jobs["ClientServer"]
        call jobsend(g:rplugin_jobs["ClientServer"], "\001R0\n")
    endif
endfunction

function SendCmdToR_NotYet(...)
    call RWarningMsg("Not ready yet")
endfunction

function StartR_Neovim()
    if string(g:SendCmdToR) != "function('SendCmdToR_fake')"
        return
    endif
    let g:R_tmux_split = 0

    let g:SendCmdToR = function('SendCmdToR_NotYet')

    let edbuf = bufname("%")
    let objbrttl = b:objbrtitle
    let curbufnm = bufname("%")
    set switchbuf=useopen
    if g:R_vsplit
        if g:R_rconsole_width > 16 && g:R_rconsole_width < (winwidth(0) - 17)
            silent exe "belowright " . g:R_rconsole_width . "vnew"
        else
            silent belowright vnew
        endif
    else
        if g:R_rconsole_height > 0 && g:R_rconsole_height < (winheight(0) - 1)
            silent exe "belowright " . g:R_rconsole_height . "new"
        else
            silent belowright new
        endif
    endif
    if has("win32")
        call SetRHome()
    endif
    let g:rplugin_jobs["R"] = termopen(g:rplugin_R . " " . join(g:rplugin_r_args), {'on_exit': function('ROnJobExit')})
    if has("win32")
        redraw
        call jobsend(g:rplugin_jobs["R"], g:rplugin_R . " " . join(g:rplugin_r_args) . " && exit\r\n")
        call UnsetRHome()
    endif
    let g:rplugin_R_bufname = bufname("%")
    let g:rplugin_R_width = 0
    let b:objbrtitle = objbrttl
    let b:rscript_buffer = curbufnm
    if exists("g:R_hl_term") && g:R_hl_term
        set filetype=rout
        let g:rplugin_hl_term = g:R_hl_term
    endif
    if g:R_esc_term
        tnoremap <buffer> <Esc> <C-\><C-n>
    endif
    autocmd TermClose <buffer> call OnTermClose()
    exe "sbuffer " . edbuf
    stopinsert
    if WaitNvimcomStart()
        let g:SendCmdToR = function('SendCmdToR_Neovim')
    endif
endfunction

" To be called by edit() in R running in Neovim buffer.
function ShowRObject(fname)
    let fcont = readfile(a:fname)
    exe "tabnew " . substitute($NVIMR_TMPDIR . "/edit_" . $NVIMR_ID, ' ', '\\ ', 'g')
    call setline(".", fcont)
    set filetype=r
    stopinsert
    autocmd BufUnload <buffer> call delete($NVIMR_TMPDIR . "/edit_" . $NVIMR_ID . "_wait") | startinsert
endfunction

if has("win32")
    " The R package colorout only works on Unix systems
    call RSetDefaultValue("g:R_hl_term", 1)
endif
R/osx.vim	[[[1
59
" This file contains code used only on OS X

if isdirectory("/Applications/R64.app")
    let g:rplugin_r64app = 1
else
    let g:rplugin_r64app = 0
endif

function StartR_OSX()
    if IsSendCmdToRFake()
        return
    endif
    if g:rplugin_r64app
        let rcmd = "/Applications/R64.app"
    else
        let rcmd = "/Applications/R.app"
    endif

    let args_str = join(g:rplugin_r_args)
    if args_str != " "
        " https://github.com/jcfaria/Vim-R-plugin/issues/63
        " https://stat.ethz.ch/pipermail/r-sig-mac/2013-February/009978.html
        call RWarningMsg('R.app does not support command line arguments. To pass "' . args_str . '" to R, you must put "let R_applescript = 0" in your vimrc to run R in a terminal emulator.')
    endif
    let rlog = system("open " . rcmd)
    if v:shell_error
        call RWarningMsg(rlog)
    endif
    let g:SendCmdToR = function('SendCmdToR_OSX')
    if WaitNvimcomStart()
        call foreground()
        sleep 200m
        if g:R_after_start != ''
            call system(g:R_after_start)
        endif
    endif
endfunction

function SendCmdToR_OSX(...)
    if g:R_ca_ck
        let cmd = "\001" . "\013" . a:1
    else
        let cmd = a:1
    endif

    if g:rplugin_r64app
        let rcmd = "R64"
    else
        let rcmd = "R"
    endif

    " for some reason it doesn't like "\025"
    let cmd = substitute(cmd, "\\", '\\\', 'g')
    let cmd = substitute(cmd, '"', '\\"', "g")
    let cmd = substitute(cmd, "'", "'\\\\''", "g")
    call system("osascript -e 'tell application \"".rcmd."\" to cmd \"" . cmd . "\"'")
    return 1
endfunction

R/r.snippets	[[[1
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
R/rmd.snippets	[[[1
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
R/setcompldir.vim	[[[1
75

" g:rplugin_home should be the directory where the plugin files are.  For
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
    let g:rplugin_userlogin = substitute(system('whoami'), '\W', '-', 'g')
elseif $USERNAME != ""
    let g:rplugin_userlogin = $USERNAME
elseif $USER != ""
    let g:rplugin_userlogin = $USER
else
    call RWarningMsgInp("Could not determine user name.")
    let g:rplugin_failed = 1
    finish
endif
let g:rplugin_userlogin = substitute(substitute(g:rplugin_userlogin, '.*\\', '', ''), '\W', '', 'g')
if g:rplugin_userlogin == ""
    call RWarningMsgInp("Could not determine user name.")
    let g:rplugin_failed = 1
    finish
endif

if has("win32")
    let g:rplugin_home = substitute(g:rplugin_home, "\\", "/", "g")
    let g:rplugin_uservimfiles = substitute(g:rplugin_uservimfiles, "\\", "/", "g")
    if $USERNAME != ""
        let g:rplugin_userlogin = substitute($USERNAME, '\W', '', 'g')
    endif
endif

if exists("g:R_compldir")
    let g:rplugin_compldir = expand(g:R_compldir)
elseif has("win32") && $APPDATA != "" && isdirectory($APPDATA)
    let g:rplugin_compldir = $APPDATA . "\\Nvim-R"
elseif $XDG_CACHE_HOME != "" && isdirectory($XDG_CACHE_HOME)
    let g:rplugin_compldir = $XDG_CACHE_HOME . "/Nvim-R"
elseif isdirectory(expand("~/.cache"))
    let g:rplugin_compldir = expand("~/.cache/Nvim-R")
elseif isdirectory(expand("~/Library/Caches"))
    let g:rplugin_compldir = expand("~/Library/Caches/Nvim-R")
else
    let g:rplugin_compldir = g:rplugin_uservimfiles . "/R/objlist/"
endif

" Create the directory if it doesn't exist yet
if !isdirectory(g:rplugin_compldir)
    call mkdir(g:rplugin_compldir, "p")
    let s:readme = ['The omnils_ and fun_ files in this directory are generated by Nvim-R',
                \ 'and nvimcom and are used for omni completion and syntax highlight.',
                \ 'If you delete them, they will be regenerated.',
                \ '',
                \ 'When you load a new version of a library, their files are replaced.',
                \ '',
                \ 'Files corresponding to uninstalled libraries are not automatically deleted.',
                \ 'You should manually delete them if you want to save disc space.']
    call writefile(s:readme, g:rplugin_compldir . "/README")
    unlet s:readme
endif
let $NVIMR_COMPLDIR = g:rplugin_compldir

R/synctex_evince_backward.py	[[[1
127

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

# Modified to Nvim-R by Jakson Aquino

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
        input_file = input_file.replace("file://", "")
        input_file = input_file.replace("%20", " ")
        sys.stdout.write("call SyncTeX_backward('" + input_file + "', " + str(source_link[0]) + ")\n")
        sys.stdout.flush()

path_output = os.getcwd() + '/' + sys.argv[1]
path_output = path_output.replace(" ", "%20")

dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

a = EvinceWindowProxy('file://' + path_output, True)

loop = GObject.MainLoop()
loop.run()

R/synctex_evince_forward.py	[[[1
146

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

# Modified to Nvim-R by Jakson Aquino

import dbus
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


    def SyncView(self, input_file, data):
        if self.status == CLOSED:
            if self.spawn:
                self._tmp_syncview = [input_file, data, 0];
                self._handler = self._syncview_handler
                self._get_dbus_name(True)
                sys.stdout.write("call Evince_Again()")
                sys.stdout.flush()
        else:
            self.window.SyncView(input_file, data, 0,  dbus_interface = "org.gnome.evince.Window")
            sys.stdout.write("let g:rplugin_evince_loop = 0")
            sys.stdout.flush()

    def _syncview_handler(self, window_list):
        self.handle_get_window_list_reply(window_list)

        if self.status == CLOSED:
            return False
        self.window.SyncView(self._tmp_syncview[0],self._tmp_syncview[1], self._tmp_syncview[2], dbus_interface="org.gnome.evince.Window")
        del self._tmp_syncview
        self._handler = None
        return True

path_output = os.getcwd() + '/' + sys.argv[1]
path_output = path_output.replace(" ", "%20")
line_number = int(sys.argv[2])
path_input = os.getcwd() + '/' + sys.argv[3]

dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

a = EvinceWindowProxy('file://' + path_output, True)

def sync_view(ev_window, path_input, line_number):
    ev_window.SyncView(path_input, (line_number, 1))
    loop.quit()

GObject.timeout_add(400, sync_view, a, path_input, line_number)
loop = GObject.MainLoop()
loop.run()
R/tmux.vim	[[[1
35
" Check whether Tmux is OK
if !executable('tmux')
    if exists("*termopen")
        let g:R_in_buffer = 1
    else
        call RWarningMsgInp("tmux executable not found")
    endif
    finish
endif

if system("uname") =~ "OpenBSD"
    " Tmux does not have -V option on OpenBSD: https://github.com/jcfaria/Vim-R-plugin/issues/200
    let g:rplugin_tmux_version = "2.1"
else
    let s:tmuxversion = system("tmux -V")
    let s:tmuxversion = substitute(s:tmuxversion, '.*tmux \([0-9]\.[0-9]\).*', '\1', '')
    if strlen(s:tmuxversion) != 3
        let s:tmuxversion = "1.0"
    endif
    if s:tmuxversion < "1.8"
        call RWarningMsgInp("Nvim-R requires Tmux >= 1.8")
        let g:rplugin_failed = 1
        finish
    endif
    unlet s:tmuxversion
endif

let g:rplugin_tmuxsname = "NvimR-" . substitute(localtime(), '.*\(...\)', '\1', '')

if g:R_tmux_split
    runtime R/tmux_split.vim
else
    runtime R/extern_term.vim
endif

R/tmux_split.vim	[[[1
96

if exists("*TmuxActivePane")
    finish
endif

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
    let g:rplugin_editor_pane = $TMUX_PANE
    let tmuxconf = ['set-environment NVIMR_TMPDIR "' . g:rplugin_tmpdir . '"',
                \ 'set-environment NVIMR_COMPLDIR "' . substitute(g:rplugin_compldir, ' ', '\\ ', "g") . '"',
                \ 'set-environment NVIMR_ID ' . $NVIMR_ID ,
                \ 'set-environment NVIMR_SECRET ' . $NVIMR_SECRET ,
                \ 'set-environment R_DEFAULT_PACKAGES ' . $R_DEFAULT_PACKAGES ]
    if &t_Co == 256
        call extend(tmuxconf, ['set default-terminal "' . $TERM . '"'])
    endif
    call writefile(tmuxconf, g:rplugin_tmpdir . "/tmux" . $NVIMR_ID . ".conf")
    call system("tmux source-file '" . g:rplugin_tmpdir . "/tmux" . $NVIMR_ID . ".conf" . "'")
    call delete(g:rplugin_tmpdir . "/tmux" . $NVIMR_ID . ".conf")
    let tcmd = "tmux split-window "
    if g:R_vsplit
        if g:R_rconsole_width == -1
            let tcmd .= "-h"
        else
            let tcmd .= "-h -l " . g:R_rconsole_width
        endif
    else
        let tcmd .= "-l " . g:R_rconsole_height
    endif

    " Let Tmux automatically kill the panel when R quits.
    let tcmd .= " '" . a:rcmd . "'"

    let rlog = system(tcmd)
    if v:shell_error
        call RWarningMsg(rlog)
        return
    endif
    let g:rplugin_rconsole_pane = TmuxActivePane()
    let rlog = system("tmux select-pane -t " . g:rplugin_editor_pane)
    if v:shell_error
        call RWarningMsg(rlog)
        return
    endif
    let g:SendCmdToR = function('SendCmdToR_TmuxSplit')
    let g:rplugin_last_rcmd = a:rcmd
    if g:R_tmux_title != "automatic" && g:R_tmux_title != ""
        call system("tmux rename-window " . g:R_tmux_title)
    endif
    if WaitNvimcomStart()
        if g:R_after_start != ''
            call system(g:R_after_start)
        endif
    endif
endfunction

function SendCmdToR_TmuxSplit(...)
    if g:R_ca_ck
        let cmd = "\001" . "\013" . a:1
    else
        let cmd = a:1
    endif

    if !exists("g:rplugin_rconsole_pane")
        " Should never happen
        call RWarningMsg("Missing internal variable: g:rplugin_rconsole_pane")
    endif
    let str = substitute(cmd, "'", "'\\\\''", "g")
    if str =~ '^-'
        let str = ' ' . str
    endif
    if a:0 == 2 && a:2 == 0
        let scmd = "tmux set-buffer '" . str . "' && tmux paste-buffer -t " . g:rplugin_rconsole_pane
    else
        let scmd = "tmux set-buffer '" . str . "\<C-M>' && tmux paste-buffer -t " . g:rplugin_rconsole_pane
    endif
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
R/windows.vim	[[[1
141
" This file contains code used only on Windows

let g:rplugin_sumatra_in_path = 0

call RSetDefaultValue("g:R_set_home_env", 1)
call RSetDefaultValue("g:R_i386", 0)

if !exists("g:rplugin_R_path")
    call writefile(['reg.exe QUERY "HKLM\SOFTWARE\R-core\R" /s'], g:rplugin_tmpdir . "/run_cmd.bat")
    let ripl = system(g:rplugin_tmpdir . "/run_cmd.bat")
    let rip = filter(split(ripl, "\n"), 'v:val =~ ".*InstallPath.*REG_SZ"')
    if len(rip) == 0
        " Normally, 32 bit applications access only 32 bit registry and...
        " We have to try again if the user has installed R only in the other architecture.
        if has("win64")
            call writefile(['reg.exe QUERY "HKLM\SOFTWARE\R-core\R" /s /reg:32'], g:rplugin_tmpdir . "/run_cmd.bat")
        else
            call writefile(['reg.exe QUERY "HKLM\SOFTWARE\R-core\R" /s /reg:64'], g:rplugin_tmpdir . "/run_cmd.bat")
        endif
        let ripl = system(g:rplugin_tmpdir . "/run_cmd.bat")
        let rip = filter(split(ripl, "\n"), 'v:val =~ ".*InstallPath.*REG_SZ"')
    endif
    if len(rip) > 0
        let s:rinstallpath = substitute(rip[0], '.*InstallPath.*REG_SZ\s*', '', '')
        let s:rinstallpath = substitute(s:rinstallpath, '\n', '', 'g')
        let s:rinstallpath = substitute(s:rinstallpath, '\s*$', '', 'g')
    endif
    if !exists("s:rinstallpath")
        call RWarningMsgInp("Could not find R path in Windows Registry. If you have already installed R, please, set the value of 'R_path'.")
        let g:rplugin_failed = 1
        finish
    endif
    let hasR32 = isdirectory(s:rinstallpath . '\bin\i386')
    let hasR64 = isdirectory(s:rinstallpath . '\bin\x64')
    if hasR32 && !hasR64
        let g:R_i386 = 1
    endif
    if hasR64 && !hasR32
        let g:R_i386 = 0
    endif
    if hasR32 && g:R_i386
        let $PATH = s:rinstallpath . '\bin\i386;' . $PATH
    elseif hasR64 && g:R_i386 == 0
        let $PATH = s:rinstallpath . '\bin\x64;' . $PATH
    else
        let $PATH = s:rinstallpath . '\bin;' . $PATH
    endif
    unlet s:rinstallpath
endif

if !exists("g:R_args")
    if g:R_in_buffer
        let g:R_args = ["--no-save"]
    else
        let g:R_args = ["--sdi", "--no-save"]
    endif
endif

let g:R_R_window_title = "R Console"

function SumatraInPath()
    if g:rplugin_sumatra_in_path
        return 1
    endif
    if $PATH =~ "SumatraPDF"
        let g:rplugin_sumatra_in_path = 1
        return 1
    endif

    " $ProgramFiles has different values for win32 and win64
    if executable($ProgramFiles . "\\SumatraPDF\\SumatraPDF.exe")
        let $PATH = $ProgramFiles . "\\SumatraPDF;" . $PATH
        let g:rplugin_sumatra_in_path = 1
        return 1
    endif
    if executable($ProgramFiles . " (x86)\\SumatraPDF\\SumatraPDF.exe")
        let $PATH = $ProgramFiles . " (x86)\\SumatraPDF;" . $PATH
        let g:rplugin_sumatra_in_path = 1
        return 1
    endif
    return 0
endfunction

function SetRHome()
    " R and Vim use different values for the $HOME variable.
    if g:R_set_home_env
        let s:saved_home = $HOME
        call writefile(['reg.exe QUERY "HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders" /v "Personal"'], g:rplugin_tmpdir . "/run_cmd.bat")
        let prs = system(g:rplugin_tmpdir . "/run_cmd.bat")
        if len(prs) > 0
            let prs = substitute(prs, '.*REG_SZ\s*', '', '')
            let prs = substitute(prs, '\n', '', 'g')
            let prs = substitute(prs, '\r', '', 'g')
            let prs = substitute(prs, '\s*$', '', 'g')
            let $HOME = prs
        endif
    endif
endfunction

function UnsetRHome()
    if exists("s:saved_home")
        let $HOME = s:saved_home
        unlet s:saved_home
    endif
endfunction

function StartR_Windows()
    if string(g:SendCmdToR) != "function('SendCmdToR_fake')"
        call RWarningMsg('R was already started.')
        return
    endif

    call SetRHome()
    if has("nvim")
        call system("start " . g:rplugin_R . ' ' . join(g:R_args))
    else
        silent exe "!start " . g:rplugin_R . ' ' . join(g:R_args)
    endif
    call UnsetRHome()

    let g:SendCmdToR = function('SendCmdToR_Windows')
    if WaitNvimcomStart()
        if g:R_arrange_windows && filereadable(g:rplugin_compldir . "/win_pos")
            " ArrangeWindows
            call JobStdin(g:rplugin_jobs["ClientServer"], "\005" . g:rplugin_compldir . "\n")
        endif
        if g:R_after_start != ''
            call system(g:R_after_start)
        endif
    endif
endfunction

function SendCmdToR_Windows(...)
    if g:R_ca_ck
        let cmd = "\001" . "\013" . a:1 . "\n"
    else
        let cmd = a:1 . "\n"
    endif
    call JobStdin(g:rplugin_jobs["ClientServer"], "\003" . cmd)
    return 1
endfunction
R/vimrcom.vim	[[[1
73
function JobStdin(ch, cmd)
    call ch_sendraw(a:ch, a:cmd)
endfunction

function StartJob(cmd, opt)
    let jobid = job_start(a:cmd, a:opt)
    return job_getchannel(jobid)
endfunction

function GetJobTitle(job_id)
    for key in keys(g:rplugin_jobs)
        if g:rplugin_jobs[key] == a:job_id
            return key
        endif
    endfor
    return "Job"
endfunction

function ROnJobStdout(job_id, msg)
    let cmd = substitute(a:msg, '\n', '', 'g')
    let cmd = substitute(cmd, '\r', '', 'g')
    if cmd =~ "^call " || cmd  =~ "^let " || cmd =~ "^unlet "
        exe cmd
    else
        call RWarningMsg("[" . GetJobTitle(a:job_id) . "] Unknown command: " . cmd)
    endif
endfunction

function ROnJobStderr(job_id, msg)
    let msg = substitute(a:msg, '\n', '', 'g')
    call RWarningMsg("[" . GetJobTitle(a:job_id) . "] " . msg)
endfunction

function ROnJobExit(job_id, stts)
    let key = GetJobTitle(a:job_id)
    if key != "Job"
        let g:rplugin_jobs[key] = "no"
    endif
    if a:stts != 0
        call RWarningMsg('"' . key . '"' . ' exited with status ' . a:stts)
    endif
endfunction

function IsJobRunning(key)
    try
        let chstt =  ch_status(g:rplugin_jobs[a:key])
    catch
        let chstt = "no"
    endtry
    if chstt == "open"
        return 1
    else
        return 0
    endif
endfunction

let g:rplugin_jobs = {"ClientServer": "no", "R": "no", "Terminal emulator": "no"}
let g:rplugin_job_handlers = {
            \ 'out_cb':  'ROnJobStdout',
            \ 'err_cb':  'ROnJobStderr',
            \ 'exit_cb': 'ROnJobExit'}


" Check if Vim-R-plugin is installed
let s:ff = globpath(&rtp, "r-plugin/functions.vim")
let s:ft = globpath(&rtp, "ftplugin/r*_rplugin.vim")
if s:ff != "" || s:ft != ""
    let s:ff = substitute(s:ff, "functions.vim", "", "g")
    call RWarningMsgInp("Nvim-R conflicts with Vim-R-plugin. Please, uninstall Vim-R-plugin.\n" .
                \ "At least the following directories and files are from a Vim-R-plugin installation:\n" . s:ff . "\n" . s:ft . "\n")
endif
unlet s:ff
unlet s:ft
R/nvimrcom.vim	[[[1
55
function JobStdin(job, cmd)
    call jobsend(a:job, a:cmd)
endfunction

function StartJob(cmd, opt)
    let jobid = jobstart(a:cmd, a:opt)
    return jobid
endfunction

function GetJobTitle(job_id)
    for key in keys(g:rplugin_jobs)
        if g:rplugin_jobs[key] == a:job_id
            return key
        endif
    endfor
    return "Job"
endfunction

function ROnJobStdout(job_id, data)
    for cmd in a:data
        let cmd = substitute(cmd, '\r', '', 'g')
        if cmd == ""
            continue
        endif
        if cmd =~ "^call " || cmd  =~ "^let " || cmd =~ "^unlet "
            exe cmd
        else
            call RWarningMsg("[" . GetJobTitle(a:job_id) . "] Unknown command: " . cmd)
        endif
    endfor
endfunction

function ROnJobStderr(job_id, data)
    call RWarningMsg("[" . GetJobTitle(a:job_id) . "] " . substitute(join(a:data), '\r', '', 'g'))
endfunction

function ROnJobExit(job_id, data)
    let key = GetJobTitle(a:job_id)
    if key != "Job"
        let g:rplugin_jobs[key] = 0
    endif
    if a:data != 0
        call RWarningMsg('"' . key . '"' . ' exited with status ' . a:data)
    endif
endfunction

function IsJobRunning(key)
    return g:rplugin_jobs[a:key]
endfunction

let g:rplugin_jobs = {"ClientServer": 0, "R": 0, "Terminal emulator": 0}
let g:rplugin_job_handlers = {
            \ 'on_stdout': function('ROnJobStdout'),
            \ 'on_stderr': function('ROnJobStderr'),
            \ 'on_exit':   function('ROnJobExit')}
R/nvimcom/DESCRIPTION	[[[1
12
Package: nvimcom
Version: 0.9-21
Date: 2016-08-06
Title: Intermediate the communication between R and either Neovim or Vim
Author: Jakson Aquino
Maintainer: Jakson Alves de Aquino <jalvesaq@gmail.com>
Depends: R (>= 3.0.0)
Suggests: knitr, rmarkdown
Imports: tools
Description: Provide a service to intermediate the communication between R and either Neovim or Vim.
License: GPL (>= 2)
URL: https://github.com/jalvesaq/nvimcom
R/nvimcom/NAMESPACE	[[[1
4
export(nvim.list.args, nvim.srcdir, nvim.plot, nvim.interlace.rnoweb,
nvim.interlace.rrst, nvim.print, nvim.names, etags2ctags,
nvim.interlace.rmd,vi)
importFrom(tools, Rd2txt_options, texi2dvi)
R/nvimcom/NEWS	[[[1
15
Version 0.9-12 (2016-03-12)

  - Add vi() function.

Version 0.9-11 (2016-03-10)

  - Add support to Vim.

Version 0.9-10 (2016-02-28)

  - Join nvimclient and nvimserver into nclientserver.

Version 0.9-8 (2015-11-03)

  - Initial release.
R/nvimcom/README.md	[[[1
60
# nvimcom

This is the development version of the R package *nvimcom*, which creates a
server on R to allow the communication with either
[Neovim](https://github.com/neovim/neovim) or [Vim](http://www.vim.org) through the
[Nvim-R](https://github.com/jalvesaq/Nvim-R) plugin. The package is necessary
to update the highlighting of the names of R functions, open R documentation
in editor's buffer, run the Object Browser, run either `Sweave()` or `knit()`
on the document being edited. It also has some functions called by editor such
as `nvim.plot()`, `nvim.print()`, and `nvim.bol()`. This last one is required
to build the data base used in omnicompletion. The nvimcom code necessary to
automatically update both the Object Browser and the list of functions for
syntax highlight calls non-API entry points and cannot be on CRAN.

## How to install

### Development version

You need to install the development version of nvimcom if you are using the
development version of Nvim-R. In this case, the easiest way to install
nvimcom is to use the
[devtools](http://cran.r-project.org/web/packages/devtools/index.html)
package.

```s
devtools::install_github("jalvesaq/nvimcom")
```

To manually download and install nvimcom, do the following in a terminal
emulator:

```sh
git clone https://github.com/jalvesaq/nvimcom.git
R CMD INSTALL nvimcom
```

### Released version

You can also download a released version and install it as in the example
below:

```r
install.packages("nvimcom_0.9-8.tar.gz", type = "source", repos = NULL)
```

**Note**: On Windows, you need
[Rtools](http://cran.r-project.org/bin/windows/Rtools/) (and, perhaps, either
[MiKTeX](http://miktex.org/) or [TexLive](http://www.tug.org/texlive/))
installed and in your path (see [qfin](http://statmath.wu.ac.at/software/R/qfin/)
and [Rwinpack](http://www.biostat.wisc.edu/~kbroman/Rintro/Rwinpack.html) for
further instructions).

If you are using Windows and cannot build the package yourself, you may want
to try the binary package built on R-3.2.2 (md5sum
84c9b32e3c707e65d5eaa4a06b50570a):

```r
detach("package:nvimcom", unload = TRUE)
install.packages("nvimcom_0.9-8.zip", type = "win.binary", repos = NULL)
```
R/nvimcom/man/etags2ctags.Rd	[[[1
22
\name{etags2ctags}
\alias{etags2ctags}
\title{
Convert an Emacs tags file into the CTags file format.
}
\description{
Convert an Emacs tags file into the CTags file format.
}
\usage{
etags2ctags(etagsfile, ctagsfile)
}
\arguments{
  \item{etagsfile}{Path to Emacs tags file.}
  \item{ctagsfile}{Path to the CTags file to be created.}
}
\author{Jakson Alves de Aquino <jalvesaq@gmail.com>}
\examples{
\dontrun{
rtags(path = "/path/to/R/source/code", recursive = TRUE, ofile = "RTAGS")
etags2ctags("RTAGS", "Rtags")
}
}
R/nvimcom/man/nvimcom-package.Rd	[[[1
36
\name{nvimcom-package}
\alias{nvimcom-package}
\alias{nvimcom}
\docType{package}
\title{Allow the communication between Nvim and R }
\description{This package provides a TCP/IP server to allow the communication
  between R and Nvim-R. It also has some functions called by the
  plugin.

  The \samp{nvimcom.verbose} option controls the amount of debugging
  information printed on R Console. Its default value is 0. If the value is 1,
  the package version will be output on startup. If the value is 2, the time
  required to update the Object Browser will be printed. This is useful if you
  suspect that R is noticeably slower when the Object Browser is open in the
  Nvim-R plugin. Higher values, up to 4, will make the package print
  information verbosely which is useful only if you want to either fix a bug
  or understand how nvimcom works.

  Below is an example of how to load \pkg{nvimcom} in your \samp{~/.Rprofile}:

  \preformatted{
    if(interactive()){
        if(Sys.getenv("NVIMR_TMPDIR") != ""){
            options(nvimcom.verbose = 1)
            library(nvimcom)
        }
    }

  }
}

\seealso{\link[base]{Startup}}
\author{
Jakson Alves de Aquino \email{jalvesaq@gmail.com}
}
\keyword{ package }
R/nvimcom/man/nvim.interlace.rmd.Rd	[[[1
17
\name{nvim.interlace.rmd}
\alias{nvim.interlace.rmd}
\title{Convert a rmd file to PDF}
\description{
  Run the \pkg{knitr} function \code{knit()} to convert a rmd file to
  PDF.
}
\usage{
nvim.interlace.rmd(Rmdfile, outform = NULL, rmddir, view = TRUE, ...)
}
\arguments{
  \item{Rmdfile}{The Rmd file to be processed.}
  \item{outform}{R Markdown output format to convert to.}
  \item{rmddir}{The directory of the Rnoweb file.}
  \item{view}{Logical value indicating whether to show the generated output.}
  \item{\dots}{Further arguments to be passed to \code{render()}.}
}
R/nvimcom/man/nvim.interlace.rnoweb.Rd	[[[1
24
\name{nvim.interlace.rnoweb}
\alias{nvim.interlace.rnoweb}
\title{Run either Sweave or knit and, then, pdflatex on a Rnoweb file}
\description{
  Run the R function Sweave() or knit() and, then, the application pdflatex.
}
\usage{
nvim.interlace.rnoweb(rnowebfile, rnwdir, latexcmd,
                     latexmk = TRUE, synctex = TRUE,
                     bibtex = FALSE, knit = TRUE,
                     buildpdf = TRUE, view = TRUE, ...)
}
\arguments{
  \item{rnowebfile}{The Rnoweb file to be processed.}
  \item{rnwdir}{The directory of the Rnoweb file.}
  \item{latexcmd}{The command to run on the generated .tex file.}
  \item{latexmk}{Whether to run latexmk.}
  \item{synctex}{Whether to compile the PDF with support to SyncTeX.}
  \item{bibtex}{Whether to run bibtex.}
  \item{knit}{Whether to use knitr instead of Sweave.}
  \item{buildpdf}{Whether to compile the PDF.}
  \item{view}{Logical value indicating whether to show the generated PDF document.}
  \item{\dots}{Further arguments to be passed to \code{Sweave}.}
}
R/nvimcom/man/nvim.interlace.rrst.Rd	[[[1
19
\name{nvim.interlace.rrst}
\alias{nvim.interlace.rrst}
\title{Run knit2pdf to convert a Rrst file to to PDF using pdflatex or rst2pdf}
\description{
  Run the \pkg{knitr} function \code{knit2pdf()} to convert a Rrst (reStructuredText) file to
  PDF. If desired, the user can specify to use \samp{rst2pdf} to compile to
  PDF if a LaTeX installation is not present.
}
\usage{
nvim.interlace.rrst(Rrstfile, rrstdir, view = TRUE,
                   compiler = "rst2pdf", ...)
}
\arguments{
  \item{Rrstfile}{The Rrst file to be processed.}
  \item{rrstdir}{The directory of the Rrst file.}
  \item{view}{Logical value indicating whether to show the generated PDF document.}
  \item{compiler}{The compiler program to use for the rst to PDF conversion.}
  \item{\dots}{Further arguments to be passed to compiler (usually \code{rst2pdf} or \code{pdflatex}).}
}
R/nvimcom/man/nvim.list.args.Rd	[[[1
12
\name{nvim.list.args}
\alias{nvim.list.args}
\title{List function arguments}
\description{
  List function arguments and the function methods arguments.
}
\usage{
nvim.list.args(ff)
}
\arguments{
  \item{ff}{A function.}
}
R/nvimcom/man/nvim.names.Rd	[[[1
12
\name{nvim.names}
\alias{nvim.names}
\title{List function arguments}
\description{
  List the names of either the object elements or its slots.
}
\usage{
nvim.names(x)
}
\arguments{
  \item{x}{An R object.}
}
R/nvimcom/man/nvim.plot.Rd	[[[1
14
\name{nvim.plot}
\alias{nvim.plot}
\title{Plot an object}
\description{
  Plot an object. If the object is numeric, plot histogram and boxplot instead
  of default scatter plot.
}
\usage{
nvim.plot(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The object.}
}
R/nvimcom/man/nvim.print.Rd	[[[1
16
\name{nvim.print}
\alias{nvim.print}
\title{
  Print an object.
}
\description{
  Print an object. If the object is a function, search for a method for the
  \code{classfor} expression. The function is supposed to be called by Nvim-R.
}
\usage{
nvim.print(object, objclass)
}
\arguments{
  \item{object}{An R object.}
  \item{objclass}{The class of the R object following the parenthesis, if any.}
}
R/nvimcom/man/nvim.srcdir.Rd	[[[1
14
\name{nvim.srcdir}
\alias{nvim.srcdir}
\title{
  Source all .R file of a given directory.
}
\description{
  Source all .R file of a given directory.
}
\usage{
nvim.srcdir(dr = ".")
}
\arguments{
  \item{dr}{The directory path.}
}
R/nvimcom/R/etags2ctags.R	[[[1
71
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### Jakson Alves de Aquino
### Sat, July 17, 2010

# Note: Emacs TAGS file can be read by Vim if the feature
# +emacs_tags was enabled while compiling Vim. If your Vim does
# not have this feature, you can try the function below:

# Function to convert from Emacs' TAGS file into Vim's tags file.
# See http://en.wikipedia.org/wiki/Ctags on the web and ":help
# ctags" in Vim for details on the two file formats.
# Note: This function only works with a tags file created by the R
# function rtags().
# Arguments:
#   etagsfile = character string with path to original TAGS
#   ctagsfile = character string with path to destination tags
# Example:
#   setwd("/path/to/R-2.11.1/src/library/base/R")
#   rtags(ofile = "TAGS")
#   etags2ctags("TAGS", "tags")
# After the above commands you should be able to jump from on file
# to another with Vim by hitting CTRL-] over function names.
etags2ctags <- function(etagsfile, ctagsfile){
  elines <- readLines(etagsfile)
  filelen <- length(elines)
  nfread <- sum(elines == "\x0c")
  nnames <- filelen - (2 * nfread)
  clines <- vector(mode = "character", length = nnames)
  i <- 1
  k <- 1
  while (i < filelen) {
    if(elines[i] == "\x0c"){
      i <- i + 1
      curfile <- sub(",.*", "", elines[i])
      i <- i + 1
      curflines <- readLines(curfile)
      while(elines[i] != "\x0c" && i <= filelen){
	curname <- sub(".\x7f(.*)\x01.*", "\\1", elines[i])
	curlnum <- as.numeric(sub(".*\x01(.*),.*", "\\1", elines[i]))
	curaddr <- curflines[as.numeric(curlnum)]
	curaddr <- gsub("\\\\", "\\\\\\\\", curaddr)
	curaddr <- gsub("\t", "\\\\t", curaddr)
	curaddr <- gsub("/", "\\\\/", curaddr)
	curaddr <- paste("/^", curaddr, "$", sep = "")
	clines[k] <- paste(curname, curfile, curaddr, sep = "\t")
	i <- i + 1
	k <- k + 1
      }
    } else {
      stop("Error while trying to interpret line ", i, " of '", etagsfile, "'.\n")
    }
  }
  curcollate <- Sys.getlocale(category = "LC_COLLATE")
  invisible(Sys.setlocale(category = "LC_COLLATE", locale = "C"))
  clines <- sort(clines)
  invisible(Sys.setlocale(category = "LC_COLLATE", locale = curcollate))
  writeLines(clines, ctagsfile)
}

R/nvimcom/R/nvim.bol.R	[[[1
253
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### Jakson Alves de Aquino
### Tue, January 18, 2011

# This function writes two files: one with the names of all functions in all
# packages (either loaded or installed); the other file lists all objects,
# including function arguments. These files are used by Vim to highlight
# functions and to complete the names of objects and the arguments of
# functions.

nvim.grepl <- function(pattern, x) {
    res <- grep(pattern, x)
    if(length(res) == 0){
        return(FALSE)
    } else {
        return(TRUE)
    }
}

nvim.omni.line <- function(x, envir, printenv, curlevel, maxlevel = 0) {
    if(curlevel == 0){
        xx <- try(get(x, envir), silent = TRUE)
        if(inherits(xx, "try-error"))
            return(invisible(NULL))
    } else {
        x.clean <- gsub("$", "", x, fixed = TRUE)
        x.clean <- gsub("_", "", x.clean, fixed = TRUE)
        haspunct <- nvim.grepl("[[:punct:]]", x.clean)
        if(haspunct[1]){
            ok <- nvim.grepl("[[:alnum:]]\\.[[:alnum:]]", x.clean)
            if(ok[1]){
                haspunct  <- FALSE
                haspp <- nvim.grepl("[[:punct:]][[:punct:]]", x.clean)
                if(haspp[1]) haspunct <- TRUE
            }
        }

        # No support for names with spaces
        if(nvim.grepl(" ", x)){
            haspunct <- TRUE
        }

        if(haspunct[1]){
            xx <- NULL
        } else {
            xx <- try(eval(parse(text=x)), silent = TRUE)
            if(class(xx)[1] == "try-error"){
                xx <- NULL
            }
        }
    }

    if(is.null(xx)){
        x.group <- " "
        x.class <- "unknown"
    } else {
        if(x == "break" || x == "next" || x == "for" || x == "if" || x == "repeat" || x == "while"){
            x.group <- "flow-control"
            x.class <- "flow-control"
        } else {
            if(is.function(xx)) x.group <- "function"
            else if(is.numeric(xx)) x.group <- "numeric"
            else if(is.factor(xx)) x.group <- "factor"
            else if(is.character(xx)) x.group <- "character"
            else if(is.logical(xx)) x.group <- "logical"
            else if(is.data.frame(xx)) x.group <- "data.frame"
            else if(is.list(xx)) x.group <- "list"
            else x.group <- " "
            x.class <- class(xx)[1]
        }
    }

    if(curlevel == maxlevel || maxlevel == 0){
        if(x.group == "function"){
            if(curlevel == 0){
                if(nvim.grepl("GlobalEnv", printenv)){
                    cat(x, "\x06function\x06function\x06", printenv, "\x06", nvim.args(x, txt = ""), "\n", sep="")
                } else {
                    cat(x, "\x06function\x06function\x06", printenv, "\x06", nvim.args(x, txt = "", pkg = printenv), "\n", sep="")
                }
            } else {
                # some libraries have functions as list elements
                cat(x, "\x06function\x06function\x06", printenv, "\x06Unknown arguments", "\n", sep="")
            }
        } else {
            if(is.list(xx)){
                if(curlevel == 0){
                    cat(x, "\x06", x.class, "\x06", x.group, "\x06", printenv, "\x06Not a function", "\n", sep="")
                } else {
                    cat(x, "\x06", x.class, "\x06", " ", "\x06", printenv, "\x06Not a function", "\n", sep="")
                }
            } else {
                cat(x, "\x06", x.class, "\x06", x.group, "\x06", printenv, "\x06Not a function", "\n", sep="")
            }
        }
    }

    if(is.list(xx) && curlevel <= maxlevel){
        obj.names <- names(xx)
        curlevel <- curlevel + 1
        if(length(xx) > 0){
            for(k in obj.names){
                nvim.omni.line(paste(x, "$", k, sep=""), envir, printenv, curlevel, maxlevel)
            }
        }
    } else if(isS4(xx) && curlevel <= maxlevel){
        obj.names <- slotNames(xx)
        curlevel <- curlevel + 1
        if(length(xx) > 0){
            for(k in obj.names){
                nvim.omni.line(paste(x, "@", k, sep=""), envir, printenv, curlevel, maxlevel)
            }
        }
    }
}

# Build Omni List
nvim.bol <- function(omnilist, packlist, allnames = FALSE, pattern = "") {
    nvim.OutDec <- options("OutDec")
    on.exit(options(nvim.OutDec))
    options(OutDec = ".")

    if(nvim.grepl("GlobalEnvList_", omnilist)){
        sink(omnilist, append = FALSE)
        obj.list <- objects(".GlobalEnv", all.names = allnames)
        l <- length(obj.list)
        maxlevel <- nchar(pattern) - nchar(gsub("@", "", gsub("\\$", "", pattern)))
        pattern <- sub("\\$.*", "", pattern)
        pattern <- sub("@.*", "", pattern)
        if(l > 0)
            for(obj in obj.list)
                if(length(grep(paste0("^", pattern), obj)) > 0)
                    nvim.omni.line(obj, ".GlobalEnv", ".GlobalEnv", 0, maxlevel)
        sink()
        writeLines(text = paste(obj.list, collapse = "\n"),
                   con = paste(Sys.getenv("NVIMR_TMPDIR"), "/nvimbol_finished", sep = ""))
        return(invisible(NULL))
    }

    if(getOption("nvimcom.verbose") > 3)
        cat("Building files with lists of objects in loaded packages for",
            "omni completion and Object Browser...\n")

    loadpack <- search()
    if(missing(packlist))
        listpack <- loadpack[grep("^package:", loadpack)]
    else
        listpack <- paste("package:", packlist, sep = "")

    needunload <- FALSE
    for(curpack in listpack){
        curlib <- sub("^package:", "", curpack)
        if(nvim.grepl(curlib, loadpack) == FALSE){
            cat("Loading   '", curlib, "'...\n", sep = "")
            needunload <- try(require(curlib, character.only = TRUE))
            if(needunload != TRUE){
                needunload <- FALSE
                next
            }
        }
        obj.list <- objects(curpack, all.names = allnames)
        l <- length(obj.list)
        if(l > 0){
            sink(omnilist, append = FALSE)
            for(obj in obj.list)
                nvim.omni.line(obj, curpack, curlib, 0)
            sink()
            # Build list of functions for syntax highlight
            fl <- readLines(omnilist)
            fl <- fl[grep("\x06function\x06function", fl)]
            fl <- sub("\x06.*", "", fl)
            fl <- fl[!grepl("[<%\\[\\+\\*&=\\$:{|@\\(\\^>/~!]", fl)]
            fl <- fl[!grepl("-", fl)]
            if(curlib == "base"){
                fl <- fl[!grepl("^array$", fl)]
                fl <- fl[!grepl("^attach$", fl)]
                fl <- fl[!grepl("^character$", fl)]
                fl <- fl[!grepl("^complex$", fl)]
                fl <- fl[!grepl("^data.frame$", fl)]
                fl <- fl[!grepl("^detach$", fl)]
                fl <- fl[!grepl("^double$", fl)]
                fl <- fl[!grepl("^function$", fl)]
                fl <- fl[!grepl("^integer$", fl)]
                fl <- fl[!grepl("^library$", fl)]
                fl <- fl[!grepl("^list$", fl)]
                fl <- fl[!grepl("^logical$", fl)]
                fl <- fl[!grepl("^matrix$", fl)]
                fl <- fl[!grepl("^numeric$", fl)]
                fl <- fl[!grepl("^require$", fl)]
                fl <- fl[!grepl("^source$", fl)]
                fl <- fl[!grepl("^vector$", fl)]
            }
            if(length(fl) > 0){
                fl <- paste("syn keyword rFunction", fl)
                writeLines(text = fl, con = sub("omnils_", "fun_", omnilist))
            } else {
                writeLines(text = '" No functions found.', con = sub("omnils_", "fun_", omnilist))
            }
        } else {
            writeLines(text = '', con = omnilist)
            writeLines(text = '" No functions found.', con = sub("omnils_", "fun_", omnilist))
        }
        if(needunload){
            cat("Detaching '", curlib, "'...\n", sep = "")
            try(detach(curpack, unload = TRUE, character.only = TRUE), silent = TRUE)
            needunload <- FALSE
        }
    }
    writeLines(text = "Finished",
               con = paste(Sys.getenv("NVIMR_TMPDIR"), "/nvimbol_finished", sep = ""))
    return(invisible(NULL))
}

nvim.buildomnils <- function(p){
    pvi <- utils::packageDescription(p)$Version
    bdir <- paste0(Sys.getenv("NVIMR_COMPLDIR"), "/")
    odir <- dir(bdir)
    pbuilt <- odir[grep(paste0("omnils_", p, "_"), odir)]
    fbuilt <- odir[grep(paste0("fun_", p, "_"), odir)]
    # if(length(pbuilt) > 1 || length(fbuilt) > 1 || length(fbuilt) == 0){
    #     unlink(paste0(bdir, c(pbuilt, fbuilt)))
    #     pbuilt <- character()
    #     fbuilt <- character()
    # }
    if(length(pbuilt) > 0){
        pvb <- sub(".*_.*_", "", pbuilt)
        if(pvb == pvi){
            if(getOption("nvimcom.verbose") > 3)
                cat("nvimcom R: No need to build omnils:", p, pvi, "\n")
        } else {
            if(getOption("nvimcom.verbose") > 3)
                cat("nvimcom R: omnils is outdated: ", p, " (", pvb, " x ", pvi, ")\n", sep = "")
            unlink(c(paste0(bdir, pbuilt), paste0(bdir, fbuilt)))
            nvim.bol(paste0(bdir, "omnils_", p, "_", pvi), p, getOption("nvimcom.allnames"))
        }
    } else {
        if(getOption("nvimcom.verbose") > 3)
            cat("nvimcom R: omnils does not exist:", p, "\n")
        nvim.bol(paste0(bdir, "omnils_", p, "_", pvi), p, getOption("nvimcom.allnames"))
    }
}
R/nvimcom/R/nvimcom.R	[[[1
131
# This file is part of nvimcom R package
#
# It is distributed under the GNU General Public License.
# See the file ../LICENSE for details.
#
# (c) 2011 Jakson Aquino: jalvesaq@gmail.com
#
###############################################################

.onLoad <- function(libname, pkgname) {
    if(Sys.getenv("NVIMR_TMPDIR") == "")
        return(invisible(NULL))
    library.dynam("nvimcom", pkgname, libname, local = FALSE)

    if(is.null(getOption("nvimcom.verbose")))
        options(nvimcom.verbose = 0)

    # The remaining options are set by Neovim. Don't try to set them in your
    # ~/.Rprofile because they will be overridden here:
    if(file.exists(paste0(Sys.getenv("NVIMR_TMPDIR"), "/start_options.R"))){
        source(paste0(Sys.getenv("NVIMR_TMPDIR"), "/start_options.R"))
    } else {
        options(nvimcom.opendf = TRUE)
        options(nvimcom.openlist = FALSE)
        options(nvimcom.allnames = FALSE)
        options(nvimcom.texerrs = TRUE)
        options(nvimcom.labelerr = TRUE)
        options(nvimcom.nvimpager = TRUE)
    }
    if(getOption("nvimcom.nvimpager"))
        options(pager = nvim.hmsg)
}

.onAttach <- function(libname, pkgname) {
    if(Sys.getenv("NVIMR_TMPDIR") == "")
        return(invisible(NULL))
    if(version$os == "mingw32")
        termenv <- "MinGW"
    else
        termenv <- Sys.getenv("TERM")

    if(interactive() && termenv != "" && termenv != "dumb" && Sys.getenv("NVIMR_COMPLDIR") != ""){
        dir.create(Sys.getenv("NVIMR_COMPLDIR"), showWarnings = FALSE)
        .C("nvimcom_Start",
           as.integer(getOption("nvimcom.verbose")),
           as.integer(getOption("nvimcom.opendf")),
           as.integer(getOption("nvimcom.openlist")),
           as.integer(getOption("nvimcom.allnames")),
           as.integer(getOption("nvimcom.labelerr")),
           path.package("nvimcom"),
           as.character(utils::packageVersion("nvimcom")),
           paste(search(), collapse = " "),
           PACKAGE="nvimcom")
    }
}

.onUnload <- function(libpath) {
    if(is.loaded("nvimcom_Stop", PACKAGE = "nvimcom")){
        .C("nvimcom_Stop", PACKAGE="nvimcom")
        if(Sys.getenv("NVIMR_TMPDIR") != ""){
            unlink(paste0(Sys.getenv("NVIMR_TMPDIR"), "/nvimcom_running_",
                          Sys.getenv("NVIMR_ID")))
            if(.Platform$OS.type == "windows")
                unlink(paste0(Sys.getenv("NVIMR_TMPDIR"), "/rconsole_hwnd_",
                              Sys.getenv("NVIMR_SECRET")))
        }
        Sys.sleep(0.2)
        library.dynam.unload("nvimcom", libpath)
    }
}

nvim.edit <- function(name, file, title)
{
    if(file != "")
        stop("Feature not implemented. Use nvim to edit files.")
    if(is.null(name))
        stop("Feature not implemented. Use nvim to create R objects from scratch.")

    waitf <- paste0(Sys.getenv("NVIMR_TMPDIR"), "/edit_", Sys.getenv("NVIMR_ID"), "_wait")
    editf <- paste0(Sys.getenv("NVIMR_TMPDIR"), "/edit_", Sys.getenv("NVIMR_ID"))
    unlink(editf)
    writeLines(text = "Waiting...", con = waitf)

    initial = paste0(Sys.getenv("NVIMR_TMPDIR"), "/nvimcom_edit_", round(runif(1, min = 100, max = 999)))
    sink(initial)
    dput(name)
    sink()

    .C("nvimcom_msg_to_nvim",
       paste0("ShowRObject('", initial, "')"),
       PACKAGE="nvimcom")

    while(file.exists(waitf))
        Sys.sleep(1)
    x <- eval(parse(editf))
    unlink(initial)
    unlink(editf)
    x
}

vi <- function(name = NULL, file = "")
{
    nvim.edit(name, file)
}

nvim_capture_source_output <- function(s, o)
{
    capture.output(base::source(s, echo = TRUE), file = o)
    .C("nvimcom_msg_to_nvim", paste0("GetROutput('", o, "')"), PACKAGE="nvimcom")
}

nvim_viewdf <- function(oname)
{
    ok <- try(o <- get(oname, envir = .GlobalEnv), silent = TRUE)
    if(inherits(ok, "try-error")){
        .C("nvimcom_msg_to_nvim",
           paste0("RWarningMsg('", '"', oname, '"', " not found in .GlobalEnv')"),
           PACKAGE="nvimcom")
        return(invisible(NULL))
    }
    if(is.data.frame(o) || is.matrix(o)){
        write.table(o, sep = "\t", row.names = FALSE, quote = FALSE,
                    file = paste0(Sys.getenv("NVIMR_TMPDIR"), "/Rinsert"))
        .C("nvimcom_msg_to_nvim", paste0("RViewDF('", oname, "')"), PACKAGE="nvimcom")
    } else {
        .C("nvimcom_msg_to_nvim",
           paste0("RWarningMsg('", '"', oname, '"', " is not a data.frame or matrix')"),
           PACKAGE="nvimcom")
    }
    return(invisible(NULL))
}
R/nvimcom/R/nvim.help.R	[[[1
114

nvim.hmsg <- function(files, header, title, delete.file)
{
    if(Sys.getenv("NVIMR_TMPDIR") == "")
        stop("NVIMR_TMPDIR not set.")
    dest <- paste0(Sys.getenv("NVIMR_TMPDIR"), "/Rdoc")
    file.copy(files[1], dest, overwrite = TRUE)
    keyword <- sub(".* '", "", title)
    keyword <- sub(".* \u2018", "", keyword)
    keyword <- sub("'", "", keyword)
    keyword <- sub("\u2019", "", keyword)
    .C("nvimcom_msg_to_nvim", paste0("ShowRDoc('", keyword, "')"), PACKAGE="nvimcom")
    return(invisible(NULL))
}

nvim.help <- function(topic, w, objclass, package)
{
    if(!missing(objclass) && objclass != ""){
        mlen <- try(length(methods(topic)), silent = TRUE)
        if(class(mlen) == "integer" && mlen > 0){
            for(i in 1:length(objclass)){
                newtopic <- paste(topic, ".", objclass[i], sep = "")
                if(length(utils::help(newtopic))){
                    topic <- newtopic
                    break
                }
            }
        }
    }

    oldRdOp <- tools::Rd2txt_options()
    on.exit(tools::Rd2txt_options(oldRdOp))
    tools::Rd2txt_options(width = w)

    oldpager <- getOption("pager")
    on.exit(options(pager = oldpager), add = TRUE)
    options(pager = nvim.hmsg)

    # try devtools first (if loaded)
    if ("devtools" %in% loadedNamespaces()) {
        if (missing(package)) {
            if (!is.null(devtools:::find_topic(topic))) {
                devtools::dev_help(topic)
                return(invisible(NULL))
            }
        } else {
            if (package %in% devtools::dev_packages()) {
                ret = try(devtools::dev_help(topic), silent = TRUE)
                if (inherits(ret, "try-error"))
                    .C("nvimcom_msg_to_nvim", paste0("RWarningMsg('", as.character(ret), "')"), PACKAGE="nvimcom")
                return(invisible(NULL))
            }
        }
    }

    if(missing(package))
        h <- utils::help(topic, help_type = "text")
    else
        h <- utils::help(topic, package = as.character(package), help_type = "text")

    if(length(h) == 0){
        msg <- paste('No documentation for "', topic, '" in loaded packages and libraries.', sep = "")
        .C("nvimcom_msg_to_nvim", paste0("RWarningMsg('", msg, "')"), PACKAGE="nvimcom")
        return(invisible(NULL))
    }
    if(length(h) > 1){
        if(missing(package)){
            h <- sub("/help/.*", "", h)
            h <- sub(".*/", "", h)
            msg <- paste("MULTILIB", paste(h, collapse = " "), topic)
            .C("nvimcom_msg_to_nvim", paste0("ShowRDoc('", msg, "')"), PACKAGE="nvimcom")
            return(invisible(NULL))
        } else {
            h <- h[grep(paste("/", package, "/", sep = ""), h)]
            if(length(h) == 0){
                msg <- paste("Package '", package, "' has no documentation for '", topic, "'", sep = "")
                .C("nvimcom_msg_to_nvim", paste0("RWarningMsg('", msg, "')"), PACKAGE="nvimcom")
                return(invisible(NULL))
            }
        }
    }
    print(h)

    return(invisible(NULL))
}

nvim.example <- function(topic)
{
    saved.warn <- getOption("warn")
    options(warn = -1)
    on.exit(options(warn = saved.warn))
    ret <- try(example(topic, give.lines = TRUE, character.only = TRUE,
                       package = NULL), silent = TRUE)
    if (inherits(ret, "try-error")){
        .C("nvimcom_msg_to_nvim",
           paste0("RWarningMsg('", as.character(ret), "')"), PACKAGE="nvimcom")
    } else {
        if(is.character(ret)){
            if(length(ret) > 0){
                writeLines(ret, paste0(Sys.getenv("NVIMR_TMPDIR"), "/example.R"))
                .C("nvimcom_msg_to_nvim", "OpenRExample()", PACKAGE="nvimcom")
            } else {
                .C("nvimcom_msg_to_nvim",
                   paste0("RWarningMsg('There is no example for \"", topic, "\"')"),
                   PACKAGE="nvimcom")
            }
        } else {
            .C("nvimcom_msg_to_nvim",
               paste0("RWarningMsg('There is no help for \"", topic, "\".')"),
               PACKAGE="nvimcom")
        }
    }
    return(invisible(NULL))
}
R/nvimcom/R/nvim.interlace.R	[[[1
286
SyncTeX_readconc <- function(basenm)
{
    texidx <- 1
    rnwidx <- 1
    ntexln <- length(readLines(paste0(basenm, ".tex")))
    lstexln <- 1:ntexln
    lsrnwf <- lsrnwl <- rep(NA, ntexln)
    conc <- readLines(paste0(basenm, "-concordance.tex"))
    idx <- 1
    maxidx <- length(conc) + 1
    while(idx < maxidx && texidx < ntexln && length(grep("Sconcordance", conc[idx])) > 0){
        curline <- sub("\\\\Sconcordance\\{concordance:", "", conc[idx])
        texf <- sub('([^:]*):.*', '\\1', curline)
        rnwf <- sub('[^:]*:([^:]*):.*', '\\1', curline)
        idx <- idx + 1
        concnum <- ""
        while(idx < maxidx && length(grep("Sconcordance", conc[idx])) == 0){
            concnum <- paste0(concnum, conc[idx])
            idx <- idx + 1
        }
        concnum <- gsub('%', '', concnum)
        concnum <- sub('\\}', '', concnum)
        concl <- strsplit(concnum, " ")
        concl <- as.numeric(concl[[1]])
        ii <- 1
        maxii <- length(concl) - 1
        rnwl <- concl[1]
        lsrnwl[texidx] <- rnwl
        lsrnwf[texidx] <- rnwf
        texidx <- texidx + 1
        while(ii < maxii && texidx < ntexln){
            ii <- ii + 1
            lnrange <- 1:concl[ii]
            ii <- ii + 1
            for(iii in lnrange){
                if(texidx >= ntexln)
                    break
                rnwl <- rnwl + concl[ii]
                lsrnwl[texidx] <- rnwl
                lsrnwf[texidx] <- rnwf
                texidx <- texidx + 1
            }
        }
    }
    return(data.frame(texlnum = lstexln, rnwfile = lsrnwf, rnwline = lsrnwl, stringsAsFactors = FALSE))
}

GetRnwLines <- function(x, l)
{
    if(file.exists(sub(".log$", "-concordance.tex", x))){
        conc <- SyncTeX_readconc(sub(".log$", "", x))
        for(ii in 1:length(l)){
            if(length(grep("line [0-9]", l[ii])) > 0){
                texln <- as.numeric(sub(".*line ([0-9]*)", "\\1", l[ii]))
                idx <- 1
                while(idx < nrow(conc) && texln > conc$texlnum[idx]){
                    idx <- idx + 1
                    if(conc$texlnum[idx] >= texln){
                        l[ii] <- sub("(.*) line ([0-9]*)",
                                     paste0("\\1 line \\2 [",
                                            conc$rnwfile[idx], ": ",
                                            conc$rnwline[idx], "]"), l[ii])
                        break
                    }
                }
            } else if(length(grep("lines [0-9]*--[0-9]*", l[ii])) > 0){
                texln1 <- as.numeric(sub(".*lines ([0-9]*)--.*", "\\1", l[ii]))
                texln2 <- as.numeric(sub(".*lines [0-9]*--([0-9]*).*", "\\1", l[ii]))
                rnwIdx1 <- NA
                rnwIdx2 <- NA
                idx <- 1
                while(idx < nrow(conc) && texln1 > conc$texlnum[idx]){
                    idx <- idx + 1
                    if(conc$texlnum[idx] >= texln1){
                        rnwIdx1 <- idx
                        break
                    }
                }
                idx <- 1
                while(idx < nrow(conc) && texln2 > conc$texlnum[idx]){
                    idx <- idx + 1
                    if(conc$texlnum[idx] >= texln2){
                        rnwIdx2 <- idx
                        break
                    }
                }
                if(!is.na(rnwIdx1) && !is.na(rnwIdx2)){
                        l[ii] <- sub("(.*) lines ([0-9]*)--([0-9]*)",
                                     paste0("\\1 lines \\2--\\3 [",
                                            conc$rnwfile[rnwIdx1], ": ",
                                            conc$rnwline[rnwIdx1], "--",
                                            conc$rnwline[rnwIdx2], "]"), l[ii])
                }
            }
        }
    }
    l
}

ShowTexErrors <- function(x)
{
    l <- readLines(x, encoding = "latin1")
    if(length(grep(sub("log$", "tex", x), l)) == 0){
        # XeLaTeX uses UTF-8
        l8 <- readLines(x, encoding = "utf-8")
        if(length(grep(sub("log$", "tex", x), l8)) > 0){
            l <- l8
        }
    }

    llen <- length(l)
    lf <- character(llen)
    lev <- 1
    levfile <- "Unknown"
    fname <- NA
    idx <- 1
    while(idx < llen){
        if(grepl("^(Over|Under)full \\\\(h|v)box ", l[idx])){
            while(l[idx] != "" && idx < llen){
                lf[idx] <- levfile[lev]
                idx <- idx + 1
            }
        } else {
            pb <- length(grep(28, charToRaw(l[idx]))) - length(grep(29, charToRaw(l[idx])))
            if(pb > 0){
                lev <- lev + pb
                fname <- sub(".*\\(", "", l[idx])
                levfile[lev] <- fname
            } else if(pb < 0){
                lev <- lev + pb
            }
        }
        lf[idx] <- levfile[lev]
        idx <- idx + 1
    }

    idx <- rep(FALSE, length(l))
    idx[grepl("^(Over|Under)full \\\\(h|v)box ", l, useBytes = TRUE)] <- TRUE
    idx[grepl("^(Package|Class) \\w+ (Error|Warning):", l, useBytes = TRUE)] <- TRUE
    idx[grepl("^LaTeX (Error|Warning):", l, useBytes = TRUE)] <- TRUE
    idx[grepl("^No pages of output", l, useBytes = TRUE)] <- TRUE
    if(sum(grepl("pdfTeX (error|warning)", l, useBytes = TRUE)) > 0)
        has.pdfTeX.errors <- TRUE
    else
        has.pdfTeX.errors <- FALSE

    if(sum(idx) > 0){
        l <- l[idx]
        lf <- lf[idx]
        ismaster <- grep(paste0("./", sub("\\.log$", ".tex", x)), lf)
        if(length(ismaster) > 0)
            l[ismaster] <- GetRnwLines(x, l[ismaster])
        msg <- paste0('\nSelected lines of "', x, '":\n\n', paste(lf, l, sep = ": ", collapse = "\n"), "\n")
        if(has.pdfTeX.errors)
            msg <- paste0(msg, 'There are pdfTeX errors or warnings. See "', x, '" for details.\n')
        cat(msg)
    }
}

OpenPDF <- function(x)
{
    path <- sub("\\.tex$", ".pdf", x)
    .C("nvimcom_msg_to_nvim", paste0("ROpenPDF('", getwd(), "/", path, "')"), PACKAGE="nvimcom")
    return(invisible(NULL))
}

nvim.interlace.rnoweb <- function(rnowebfile, rnwdir, latexcmd, latexmk = TRUE, synctex = TRUE, bibtex = FALSE,
                                  knit = TRUE, buildpdf = TRUE, view = TRUE, ...)
{
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(rnwdir)

    Sres <- NA

    # Check whether the .tex was already compiled
    twofiles <- c(rnowebfile, sub("\\....$", ".tex", rnowebfile))
    if(sum(file.exists(twofiles)) == 2){
        fi <- file.info(twofiles)$mtime
        if(fi[1] < fi[2])
            Sres <- twofiles[2]
    }

    # Compile the .tex file
    if(is.na(Sres) || !buildpdf){
        if(knit){
            if(!require(knitr))
                stop("Please, install the 'knitr' package.")
            if(synctex)
                knitr::opts_knit$set(concordance = TRUE)
            Sres <- knit(rnowebfile, envir = globalenv())
        } else {
            Sres <- Sweave(rnowebfile, ...)
        }
    }

    if(!buildpdf)
        return(invisible(NULL))

    # Compile the .pdf
    if(exists('Sres')){
        # From RStudio: Check for spaces in path (Sweave chokes on these)
        if(length(grep(" ", Sres)) > 0)
            stop(paste("Invalid filename: '", Sres, "' (TeX does not understand paths with spaces).", sep=""))
        if(missing(latexcmd)){
            if(latexmk){
                if(synctex)
                    latexcmd = 'latexmk -pdflatex="pdflatex -file-line-error -synctex=1" -pdf'
                else
                    latexcmd = 'latexmk -pdflatex="pdflatex -file-line-error" -pdf'
            } else {
                if(synctex)
                    latexcmd = "pdflatex -file-line-error -synctex=1"
                else
                    latexcmd = "pdflatex -file-line-error"
            }
        }
        haserror <- system(paste(latexcmd, Sres))
        if(!haserror && bibtex){
            haserror <- system(paste("bibtex", sub("\\.tex$", ".aux", Sres)))
            if(!haserror){
                haserror <- system(paste(latexcmd, Sres))
                if(!haserror)
                    haserror <- system(paste(latexcmd, Sres))
            }
        }
        if(!haserror){
            if(view)
                OpenPDF(Sres)
            if(getOption("nvimcom.texerrs"))
                ShowTexErrors(sub("\\.tex$", ".log", Sres))
        }
    }
    return(invisible(NULL))
}

nvim.interlace.rrst <- function(Rrstfile, rrstdir, view = TRUE,
                               compiler = "rst2pdf", ...)
{
    if(!require(knitr))
        stop("Please, install the 'knitr' package.")

    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(rrstdir)

    knitr::knit2pdf(Rrstfile, compiler = compiler, ...)
    if (view) {
        Sys.sleep(0.2)
        pdffile = sub('\\.Rrst$', ".pdf", Rrstfile, ignore.case = TRUE)
        OpenPDF(pdffile)
    }
}

nvim.interlace.rmd <- function(Rmdfile, outform = NULL, rmddir, view = TRUE, ...)
{
    if(!require(rmarkdown))
        stop("Please, install the 'rmarkdown' package.")

    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(rmddir)

    if(!is.null(outform)){
        if(outform == "odt"){
            res <- rmarkdown::render(Rmdfile, "html_document", ...)
            system(paste('soffice --invisible --convert-to odt', res))
        } else {
            res <- rmarkdown::render(Rmdfile, outform, ...)
        }

        if(view){
            if(outform == "html_document")
                browseURL(res)
            else
                if(outform == "pdf_document" || outform == "beamer_presentation")
                    OpenPDF(sub(".*/", "", res))
                else
                    if(outform == "odt")
                        system(paste0("lowriter '", sub("\\.html$", ".odt'", res)))
        }
    } else {
        res <- rmarkdown::render(Rmdfile, ...)
    }
}

R/nvimcom/R/nvim.print.R	[[[1
22

nvim.print <- function(object, objclass)
{
    if(!exists(object))
        stop("object '", object, "' not found")
    if(!missing(objclass)){
        mlen <- try(length(methods(object)), silent = TRUE)
        if(class(mlen) == "integer" && mlen > 0){
            for(cls in objclass){
                if(exists(paste(object, ".", objclass, sep = ""))){
                    .newobj <- get(paste(object, ".", objclass, sep = ""))
                    message(paste0("Note: Printing ", object, ".", objclass))
                    break
                }
            }
        }
    }
    if(!exists(".newobj"))
        .newobj <- get(object)
    print(.newobj)
}

R/nvimcom/R/nvim.srcdir.R	[[[1
8

nvim.srcdir <- function(dr = "."){
    for(f in list.files(path = dr, pattern = "\\.[RrSsQq]$")){
        cat(f, "\n")
        source(paste(dr, "/", f, sep = ""))
    }
    return(invisible(NULL))
}
R/nvimcom/R/specialfuns.R	[[[1
185
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### Jakson Alves de Aquino

nvim.primitive.args <- function(x)
{
    fun <- get(x)
    f <- capture.output(args(x))
    f <- sub(") $", "", sub("^function \\(", "", f[1]))
    f <- strsplit(f, ",")[[1]]
    f <- sub("^ ", "", f)
    f <- sub(" = ", "\x07", f)
    paste(f, collapse = "\x09")
}


# Adapted from: https://stat.ethz.ch/pipermail/ess-help/2011-March/006791.html
nvim.args <- function(funcname, txt, pkg = NULL, objclass, firstLibArg = FALSE)
{
    # First argument of either library() or require():
    if(firstLibArg){
        p <- dir(.libPaths())
        p <- p[grep(paste0("^", txt), p)]
        return(paste0(p, collapse = "\x09"))
    }

    frm <- NA
    funcmeth <- NA
    if(!missing(objclass) && nvim.grepl("[[:punct:]]", funcname) == FALSE){
        mlen <- try(length(methods(funcname)), silent = TRUE)
        if(class(mlen) == "integer" && mlen > 0){
            for(i in 1:length(objclass)){
                funcmeth <- paste(funcname, ".", objclass[i], sep = "")
                if(existsFunction(funcmeth)){
                    funcname <- funcmeth
                    frm <- formals(funcmeth)
                    break
                }
            }
        }
    }

    if(is.na(frm[1])){
        if(is.null(pkg)){
            deffun <- paste(funcname, ".default", sep = "")
            if (existsFunction(deffun)) {
                funcname <- deffun
                funcmeth <- deffun
            } else if(!existsFunction(funcname)) {
                return("NOT_EXISTS")
            }
            if(is.primitive(get(funcname)))
                return(nvim.primitive.args(funcname))
            else
                frm <- formals(funcname)
        } else {
            idx <- grep(paste0(":", pkg, "$"), search())
            if(length(idx)){
                ff <- "NULL"
                tr <- try(ff <- get(paste(funcname, ".default", sep = ""), pos = idx), silent = TRUE)
                if(class(tr)[1] == "try-error")
                    ff <- get(funcname, pos = idx)
                frm <- formals(ff)
            } else {
                if(!isNamespaceLoaded(pkg))
                    loadNamespace(pkg)
                ff <- getAnywhere(funcname)
                idx <- grep(pkg, ff$where)
                if(length(idx))
                    frm <- formals(ff$objs[[idx]])
            }
        }
    }

    res <- NULL
    for (field in names(frm)) {
        type <- typeof(frm[[field]])
        if (type == 'symbol') {
            res <- append(res, paste('\x09', field, sep = ''))
        } else if (type == 'character') {
            res <- append(res, paste('\x09', field, '\x07"', frm[[field]], '"', sep = ''))
        } else if (type == 'logical') {
            res <- append(res, paste('\x09', field, '\x07', as.character(frm[[field]]), sep = ''))
        } else if (type == 'double') {
            res <- append(res, paste('\x09', field, '\x07', as.character(frm[[field]]), sep = ''))
        } else if (type == 'NULL') {
            res <- append(res, paste('\x09', field, '\x07', 'NULL', sep = ''))
        } else if (type == 'language') {
            res <- append(res, paste('\x09', field, '\x07', deparse(frm[[field]]), sep = ''))
        }
    }
    idx <- grep(paste("^\x09", txt, sep = ""), res)
    res <- res[idx]
    res <- paste(res, sep = '', collapse='')
    res <- sub("^\x09", "", res)
    res <- gsub("\n", "\\\\n", res)

    if(length(res) == 0 || res == ""){
        res <- "NO_ARGS"
    } else {
        if(is.null(pkg)){
            info <- ""
            pkgname <- find(funcname, mode = "function")
            if(length(pkgname) > 1)
                info <- pkgname[1]
            if(!is.na(funcmeth)){
                if(info != "")
                    info <- paste(info, ", ", sep = "")
                info <- paste(info, "function:", funcmeth, "()", sep = "")
            }
            if(info != "")
                res <- paste(res, "\x04", info, sep = "")
        }
    }

    return(res)
}


nvim.list.args <- function(ff){
    mm <- try(methods(ff), silent = TRUE)
    if(class(mm) == "MethodsFunction" && length(mm) > 0){
        for(i in 1:length(mm)){
            if(exists(mm[i])){
                cat(ff, "[method ", mm[i], "]:\n", sep="")
                print(args(mm[i]))
                cat("\n")
            }
        }
        return(invisible(NULL))
    }
    print(args(ff))
}


nvim.plot <- function(x)
{
    xname <- deparse(substitute(x))
    if(length(grep("numeric", class(x))) > 0 || length(grep("integer", class(x))) > 0){
        oldpar <- par(no.readonly = TRUE)
        par(mfrow = c(2, 1))
        hist(x, col = "lightgray", main = paste("Histogram of", xname), xlab = xname)
        boxplot(x, main = paste("Boxplot of", xname),
                col = "lightgray", horizontal = TRUE)
        par(oldpar)
    } else {
        plot(x)
    }
}

nvim.names <- function(x)
{
    if(isS4(x))
        slotNames(x)
    else
        names(x)
}

nvim.getclass <- function(x)
{
    if(getOption("nvimcom.verbose") < 3){
        saved.warn <- getOption("warn")
        options(warn = -1)
        on.exit(options(warn = saved.warn))
        tr <- try(obj <- eval(expression(x)), silent = TRUE)
    } else {
        tr <- try(obj <- eval(expression(x)))
    }
    if(class(tr)[1] == "try-error"){
        return("Error evaluating the object")
    } else {
        return(class(obj)[1])
    }
}
R/nvimcom/src/install.libs.R	[[[1
16
files <- Sys.glob(paste0("*", SHLIB_EXT))
dest <- file.path(R_PACKAGE_DIR, paste0('libs', R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
file.copy(files, dest, overwrite = TRUE)
if(file.exists("symbols.rds"))
    file.copy("symbols.rds", dest, overwrite = TRUE)

exec <- "apps/nclientserver"
if(WINDOWS)
    exec <- "apps/nclientserver.exe"
if(any(file.exists(exec))){
    dest <- file.path(R_PACKAGE_DIR,  paste0('bin', R_ARCH))
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    file.copy(exec, dest, overwrite = TRUE)
}

R/nvimcom/src/Makevars	[[[1
10

.PHONY: all nvimapps

all: $(SHLIB) nvimapps

$(SHLIB):

nvimapps:
	(cd apps; make)

R/nvimcom/src/Makevars.win	[[[1
12
PKG_CFLAGS=-D`${R_HOME}/bin/R --slave -e 'cat(Sys.info()[[1]])'` -DLWS_NO_FORK  -DWIN32
PKG_LIBS=-lWs2_32

.PHONY: all nvimapps

all: $(SHLIB) nvimapps

$(SHLIB):

nvimapps:
	(cd apps; make -f Makefile.win)

R/nvimcom/src/nvimcom.c	[[[1
1363
#include <R.h>  /* to include Rconfig.h */
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <R_ext/Callbacks.h>
#ifndef WIN32
#include <R_ext/eventloop.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>

#ifdef WIN32
#include <winsock2.h>
#include <process.h>
#ifdef _WIN64
#include <inttypes.h>
#endif
#else
#include <stdint.h>
#include <sys/socket.h>
#include <netdb.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>
#endif

static char nvimcom_version[32];

static pid_t R_PID;

static int nvimcom_initialized = 0;
static int verbose = 0;
static int opendf = 1;
static int openls = 0;
static int allnames = 0;
static int labelerr = 1;
static int nvimcom_is_utf8;
static int nvimcom_failure = 0;
static int nlibs = 0;
static int needsfillmsg = 0;
static int openclosel = 0;
static char edsrvr[128];
static char nvimsecr[128];
static char liblist[512];
static char globenv[512];
static char *obbrbuf1;
static char *obbrbuf2;
static int obbrbufzise = 4096;
static char strL[16];
static char strT[16];
static char tmpdir[512];
static char nvimcom_home[1024];
static char search_list[1024];
static int objbr_auto = 0; // 0 = Nothing; 1 = .GlobalEnv; 2 = Libraries

#ifdef WIN32
static int r_is_busy = 1;
static int tcltkerr = 0;
#else
static int fired = 0;
static char flag_eval[512];
static int flag_lsenv = 0;
static int flag_lslibs = 0;
static int ifd, ofd;
static InputHandler *ih;
#endif

typedef struct liststatus_ {
    char *key;
    int status;
    struct liststatus_ *next;
} ListStatus;

static int nvimcom_checklibs();

static ListStatus *firstList = NULL;

static char *loadedlibs[64];
static char *builtlibs[64];

#ifdef WIN32
SOCKET sfd;
static int tid;
extern void Rconsolecmd(char *cmd); // Defined in R: src/gnuwin32/rui.c
#else
static int sfd = -1;
static pthread_t tid;
#endif

char *nvimcom_strcat(char* dest, const char* src)
{
    while(*dest) dest++;
    while((*dest++ = *src++));
    return --dest;
}

char *nvimcom_grow_obbrbuf()
{
    obbrbufzise += 4096;
    char *tmp = (char*)calloc(obbrbufzise, sizeof(char));
    strcpy(tmp, obbrbuf1);
    free(obbrbuf1);
    obbrbuf1 = tmp;
    tmp = (char*)calloc(obbrbufzise, sizeof(char));
    strcpy(tmp, obbrbuf2);
    free(obbrbuf2);
    obbrbuf2 = tmp;
    return(obbrbuf2 + strlen(obbrbuf2));
}

static void nvimcom_del_newline(char *buf)
{
    for(int i = 0; i < strlen(buf); i++)
        if(buf[i] == '\n'){
            buf[i] = 0;
            break;
        }
}

#ifndef WIN32
static void nvimcom_nvimclient(const char *msg, char *port)
{
    struct addrinfo hints;
    struct addrinfo *result, *rp;
    char portstr[16];
    int s, a;
    size_t len;
    int srvport = atoi(port);

    if(verbose > 2)
        Rprintf("nvimcom_nvimclient(%s): '%s' (%d)\n", msg, port, srvport);
    if(port[0] == 0){
        if(verbose > 3)
            REprintf("nvimcom_nvimclient() called although Neovim server port is undefined\n");
        return;
    }

    /* Obtain address(es) matching host/port */

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_flags = 0;
    hints.ai_protocol = 0;

    sprintf(portstr, "%d", srvport);
    a = getaddrinfo("127.0.0.1", portstr, &hints, &result);
    if (a != 0) {
        REprintf("Error: getaddrinfo: %s\n", gai_strerror(a));
        objbr_auto = 0;
        return;
    }

    for (rp = result; rp != NULL; rp = rp->ai_next) {
        s = socket(rp->ai_family, rp->ai_socktype,
                rp->ai_protocol);
        if (s == -1)
            continue;

        if (connect(s, rp->ai_addr, rp->ai_addrlen) != -1)
            break;		   /* Success */

        close(s);
    }

    if (rp == NULL) {		   /* No address succeeded */
        REprintf("Error: Could not connect\n");
        objbr_auto = 0;
        return;
    }

    freeaddrinfo(result);	   /* No longer needed */

    /* Prefix NVIMR_SECRET to msg to increase security.
     * The nvimclient does not need this because it is protect by the X server. */
    char finalmsg[256];
    strncpy(finalmsg, nvimsecr, 255);
    strncat(finalmsg, "call ", 255);
    strncat(finalmsg, msg, 255);
    len = strlen(finalmsg);
    if (write(s, finalmsg, len) != len) {
        REprintf("Error: partial/failed write\n");
        objbr_auto = 0;
        return;
    }
}
#endif

#ifdef WIN32
static void nvimcom_nvimclient(const char *msg, char *port)
{
    WSADATA wsaData;
    struct sockaddr_in peer_addr;
    SOCKET sfd;

    if(verbose > 2)
        Rprintf("nvimcom_nvimclient(%s): '%s'\n", msg, port);
    if(port[0] == 0){
        if(verbose > 3)
            REprintf("nvimcom_nvimclient() called although Neovim server port is undefined\n");
        return;
    }

    WSAStartup(MAKEWORD(2, 2), &wsaData);
    sfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);

    if(sfd < 0){
        REprintf("nvimcom_nvimclient socket failed.\n");
        return;
    }

    peer_addr.sin_family = AF_INET;
    peer_addr.sin_port = htons(atoi(port));
    peer_addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if(connect(sfd, (struct sockaddr *)&peer_addr, sizeof(peer_addr)) < 0){
        REprintf("nvimcom_nvimclient could not connect.\n");
        return;
    }

    /* Prefix NVIMR_SECRET to msg to increase security.
     * The nvimclient does not need this because it is protect by the X server. */
    char finalmsg[256];
    strncpy(finalmsg, nvimsecr, 255);
    strncat(finalmsg, "call ", 255);
    strncat(finalmsg, msg, 255);
    int len = strlen(finalmsg);
    if (send(sfd, finalmsg, len+1, 0) < 0) {
        REprintf("nvimcom_nvimclient failed sending message.\n");
        return;
    }

    if(closesocket(sfd) < 0)
        REprintf("nvimcom_nvimclient error closing socket.\n");
    return;
}
#endif

void nvimcom_msg_to_nvim(char **cmd)
{
    nvimcom_nvimclient(*cmd, edsrvr);
}

static void nvimcom_toggle_list_status(const char *x)
{
    ListStatus *tmp = firstList;
    while(tmp){
        if(strcmp(tmp->key, x) == 0){
            tmp->status = !tmp->status;
            break;
        }
        tmp = tmp->next;
    }
}

static void nvimcom_add_list(const char *x, int s)
{
    ListStatus *tmp = firstList;
    while(tmp->next)
        tmp = tmp->next;
    tmp->next = (ListStatus*)calloc(1, sizeof(ListStatus));
    tmp->next->key = (char*)malloc((strlen(x) + 1) * sizeof(char));
    strcpy(tmp->next->key, x);
    tmp->next->status = s;
}

static int nvimcom_get_list_status(const char *x, const char *xclass)
{
    ListStatus *tmp = firstList;
    while(tmp){
        if(strcmp(tmp->key, x) == 0)
            return(tmp->status);
        tmp = tmp->next;
    }
    if(strcmp(xclass, "data.frame") == 0){
        nvimcom_add_list(x, opendf);
        return(opendf);
    } else if(strcmp(xclass, "list") == 0){
        nvimcom_add_list(x, openls);
        return(openls);
    } else {
        nvimcom_add_list(x, 0);
        return(0);
    }
}

char *nvimcom_browser_line(SEXP *x, const char *xname, const char *curenv, const char *prefix, char *p)
{
    char xclass[64];
    char newenv[512];
    char curenvB[512];
    char ebuf[64];
    char pre[128];
    char newpre[128];
    int len;
    const char *ename;
    SEXP listNames, label, lablab, eexp, elmt = R_NilValue;
    SEXP cmdSexp, cmdexpr, ans, cmdSexp2, cmdexpr2;
    ParseStatus status, status2;
    int er = 0;
    char buf[128];

    if(strlen(xname) > 64)
        return p;

    if(obbrbufzise < strlen(obbrbuf2) + 1024)
        p = nvimcom_grow_obbrbuf();

    p = nvimcom_strcat(p, prefix);
    if(Rf_isLogical(*x)){
        p = nvimcom_strcat(p, "%#");
        strcpy(xclass, "logical");
    } else if(Rf_isNumeric(*x)){
        p = nvimcom_strcat(p, "{#");
        strcpy(xclass, "numeric");
    } else if(Rf_isFactor(*x)){
        p = nvimcom_strcat(p, "'#");
        strcpy(xclass, "factor");
    } else if(Rf_isValidString(*x)){
        p = nvimcom_strcat(p, "\"#");
        strcpy(xclass, "character");
    } else if(Rf_isFunction(*x)){
        p = nvimcom_strcat(p, "(#");
        strcpy(xclass, "function");
    } else if(Rf_isFrame(*x)){
        p = nvimcom_strcat(p, "[#");
        strcpy(xclass, "data.frame");
    } else if(Rf_isNewList(*x)){
        p = nvimcom_strcat(p, "[#");
        strcpy(xclass, "list");
    } else if(Rf_isS4(*x)){
        p = nvimcom_strcat(p, "<#");
        strcpy(xclass, "s4");
    } else if(TYPEOF(*x) == PROMSXP){
        p = nvimcom_strcat(p, "&#");
        strcpy(xclass, "lazy");
    } else {
        p = nvimcom_strcat(p, "=#");
        strcpy(xclass, "other");
    }

    PROTECT(lablab = allocVector(STRSXP, 1));
    SET_STRING_ELT(lablab, 0, mkChar("label"));
    PROTECT(label = getAttrib(*x, lablab));
    p = nvimcom_strcat(p, xname);
    p = nvimcom_strcat(p, "\t");
    if(length(label) > 0){
        if(Rf_isValidString(label)){
            snprintf(buf, 127, "%s", CHAR(STRING_ELT(label, 0)));
            p = nvimcom_strcat(p, buf);
        } else {
            if(labelerr)
                p = nvimcom_strcat(p, "Error: label isn't \"character\".");
        }
    }
    p = nvimcom_strcat(p, "\n");
    UNPROTECT(2);

    if(strcmp(xclass, "list") == 0 || strcmp(xclass, "data.frame") == 0 || strcmp(xclass, "s4") == 0){
        strncpy(curenvB, curenv, 500);
        if(xname[0] == '[' && xname[1] == '['){
            curenvB[strlen(curenvB) - 1] = 0;
        }
        if(strcmp(xclass, "s4") == 0)
            snprintf(newenv, 500, "%s%s@", curenvB, xname);
        else
            snprintf(newenv, 500, "%s%s$", curenvB, xname);
        if((nvimcom_get_list_status(newenv, xclass) == 1)){
            len = strlen(prefix);
            if(nvimcom_is_utf8){
                int j = 0, i = 0;
                while(i < len){
                    if(prefix[i] == '\xe2'){
                        i += 3;
                        if(prefix[i-1] == '\x80' || prefix[i-1] == '\x94'){
                            pre[j] = ' '; j++;
                        } else {
                            pre[j] = '\xe2'; j++;
                            pre[j] = '\x94'; j++;
                            pre[j] = '\x82'; j++;
                        }
                    } else {
                        pre[j] = prefix[i];
                        i++, j++;
                    }
                }
                pre[j] = 0;
            } else {
                for(int i = 0; i < len; i++){
                    if(prefix[i] == '-' || prefix[i] == '`')
                        pre[i] = ' ';
                    else
                        pre[i] = prefix[i];
                }
                pre[len] = 0;
            }
            sprintf(newpre, "%s%s", pre, strT);

            if(strcmp(xclass, "s4") == 0){
                snprintf(buf, 127, "slotNames(%s%s)", curenvB, xname);
                PROTECT(cmdSexp = allocVector(STRSXP, 1));
                SET_STRING_ELT(cmdSexp, 0, mkChar(buf));
                PROTECT(cmdexpr = R_ParseVector(cmdSexp, -1, &status, R_NilValue));

                if (status != PARSE_OK) {
                    p = nvimcom_strcat(p, "nvimcom error: invalid value in slotNames(");
                    p = nvimcom_strcat(p, xname);
                    p = nvimcom_strcat(p, ")\n");
                } else {
                    PROTECT(ans = R_tryEval(VECTOR_ELT(cmdexpr, 0), R_GlobalEnv, &er));
                    if(er){
                        p = nvimcom_strcat(p, "nvimcom error: ");
                        p = nvimcom_strcat(p, xname);
                        p = nvimcom_strcat(p, "\n");
                    } else {
                        len = length(ans);
                        if(len > 0){
                            int len1 = len - 1;
                            for(int i = 0; i < len; i++){
                                ename = CHAR(STRING_ELT(ans, i));
                                snprintf(buf, 127, "%s%s@%s", curenvB, xname, ename);
                                PROTECT(cmdSexp2 = allocVector(STRSXP, 1));
                                SET_STRING_ELT(cmdSexp2, 0, mkChar(buf));
                                PROTECT(cmdexpr2 = R_ParseVector(cmdSexp2, -1, &status2, R_NilValue));
                                if (status2 != PARSE_OK) {
                                    p = nvimcom_strcat(p, "nvimcom error: invalid code \"");
                                    p = nvimcom_strcat(p, xname);
                                    p = nvimcom_strcat(p, "@");
                                    p = nvimcom_strcat(p, ename);
                                    p = nvimcom_strcat(p, "\"\n");
                                } else {
                                    PROTECT(elmt = R_tryEval(VECTOR_ELT(cmdexpr2, 0), R_GlobalEnv, &er));
                                    if(i == len1)
                                        sprintf(newpre, "%s%s", pre, strL);
                                    p = nvimcom_browser_line(&elmt, ename, newenv, newpre, p);
                                    UNPROTECT(1);
                                }
                                UNPROTECT(2);
                            }
                        }
                    }
                    UNPROTECT(1);
                }
                UNPROTECT(2);
            } else {
                PROTECT(listNames = getAttrib(*x, R_NamesSymbol));
                len = length(listNames);
                if(len == 0){ /* Empty list? */
                    int len1 = length(*x);
                    if(len1 > 0){ /* List without names */
                        len1 -= 1;
                        for(int i = 0; i < len1; i++){
                            sprintf(ebuf, "[[%d]]", i + 1);
                            elmt = VECTOR_ELT(*x, i);
                            p = nvimcom_browser_line(&elmt, ebuf, newenv, newpre, p);
                        }
                        sprintf(newpre, "%s%s", pre, strL);
                        sprintf(ebuf, "[[%d]]", len1 + 1);
                        PROTECT(elmt = VECTOR_ELT(*x, len));
                        p = nvimcom_browser_line(&elmt, ebuf, newenv, newpre, p);
                        UNPROTECT(1);
                    }
                } else { /* Named list */
                    len -= 1;
                    for(int i = 0; i < len; i++){
                        PROTECT(eexp = STRING_ELT(listNames, i));
                        ename = CHAR(eexp);
                        UNPROTECT(1);
                        if(ename[0] == 0){
                            sprintf(ebuf, "[[%d]]", i + 1);
                            ename = ebuf;
                        }
                        PROTECT(elmt = VECTOR_ELT(*x, i));
                        p = nvimcom_browser_line(&elmt, ename, newenv, newpre, p);
                        UNPROTECT(1);
                    }
                    sprintf(newpre, "%s%s", pre, strL);
                    ename = CHAR(STRING_ELT(listNames, len));
                    if(ename[0] == 0){
                        sprintf(ebuf, "[[%d]]", len + 1);
                        ename = ebuf;
                    }
                    PROTECT(elmt = VECTOR_ELT(*x, len));
                    p = nvimcom_browser_line(&elmt, ename, newenv, newpre, p);
                    UNPROTECT(1);
                }
                UNPROTECT(1); /* listNames */
            }
        }
    }
    return p;
}

static void nvimcom_write_obbr()
{
    strcpy(obbrbuf1, obbrbuf2);
    FILE *f = fopen(globenv, "w");
    if(f == NULL){
        REprintf("Error: Could not write to '%s'. [nvimcom]\n", globenv);
        return;
    }
    fprintf(f, "%s", obbrbuf1);
    fclose(f);
    nvimcom_nvimclient("UpdateOB('GlobalEnv')", edsrvr);
}

static void nvimcom_list_env()
{
    const char *varName;
    SEXP envVarsSEXP, varSEXP;

    if(tmpdir[0] == 0)
        return;

    if(objbr_auto != 1)
        return;

#ifndef WIN32
    struct timeval begin, middle, end, tdiff1, tdiff2;
    if(verbose > 1)
        gettimeofday(&begin, NULL);
#endif

    memset(obbrbuf2, 0, obbrbufzise);
    char *p = nvimcom_strcat(obbrbuf2, ".GlobalEnv | Libraries\n\n");

    PROTECT(envVarsSEXP = R_lsInternal(R_GlobalEnv, allnames));
    for(int i = 0; i < Rf_length(envVarsSEXP); i++){
        varName = CHAR(STRING_ELT(envVarsSEXP, i));
        PROTECT(varSEXP = Rf_findVar(Rf_install(varName), R_GlobalEnv));
        if (varSEXP != R_UnboundValue) // should never be unbound
        {
            p = nvimcom_browser_line(&varSEXP, varName, "", "   ", p);
        } else {
            REprintf("Unexpected R_UnboundValue returned from R_lsInternal.\n");
        }
        UNPROTECT(1);
    }
    UNPROTECT(1);

#ifndef WIN32
    if(verbose > 1)
        gettimeofday(&middle, NULL);
#endif

    int len1 = strlen(obbrbuf1);
    int len2 = strlen(obbrbuf2);
    if(len1 != len2){
        nvimcom_write_obbr();
    } else {
        for(int i = 0; i < len1; i++){
            if(obbrbuf1[i] != obbrbuf2[i]){
                nvimcom_write_obbr();
                break;
            }
        }
    }
#ifndef WIN32
    if(verbose > 1){
        gettimeofday(&end, NULL);
        timersub(&middle, &begin, &tdiff1);
        timersub(&end, &middle, &tdiff2);
        Rprintf("Time to Update the Object Browser: %ld.%06ld + %ld.%06ld\n",
                (long int)tdiff1.tv_sec, (long int)tdiff1.tv_usec,
                (long int)tdiff2.tv_sec, (long int)tdiff2.tv_usec);
    }
#endif
}

static void nvimcom_eval_expr(const char *buf)
{
    char fn[512];
    snprintf(fn, 510, "%s/eval_reply", tmpdir);

    if(verbose > 3)
        Rprintf("nvimcom_eval_expr: '%s'\n", buf);

    FILE *rep = fopen(fn, "w");
    if(rep == NULL){
        REprintf("Error: Could not write to '%s'. [nvimcom]\n", fn);
        return;
    }

#ifdef WIN32
    if(tcltkerr){
        fprintf(rep, "Error: \"nvimcom\" and \"tcltk\" packages are incompatible!\n");
        fclose(rep);
        return;
    } else {
        if(objbr_auto == 0)
            nvimcom_checklibs();
        if(tcltkerr){
            fprintf(rep, "Error: \"nvimcom\" and \"tcltk\" packages are incompatible!\n");
            fclose(rep);
            return;
        }
    }
#endif

    SEXP cmdSexp, cmdexpr, ans;
    ParseStatus status;
    int er = 0;

    PROTECT(cmdSexp = allocVector(STRSXP, 1));
    SET_STRING_ELT(cmdSexp, 0, mkChar(buf));
    PROTECT(cmdexpr = R_ParseVector(cmdSexp, -1, &status, R_NilValue));

    if (status != PARSE_OK) {
        fprintf(rep, "INVALID\n");
    } else {
        /* Only the first command will be executed if the expression includes
         * a semicolon. */
        PROTECT(ans = R_tryEval(VECTOR_ELT(cmdexpr, 0), R_GlobalEnv, &er));
        if(er){
            fprintf(rep, "ERROR\n");
        } else {
            switch(TYPEOF(ans)) {
                case REALSXP:
                    fprintf(rep, "%f\n", REAL(ans)[0]);
                    break;
                case LGLSXP:
                case INTSXP:
                    fprintf(rep, "%d\n", INTEGER(ans)[0]);
                    break;
                case STRSXP:
                    if(length(ans) > 0)
                        fprintf(rep, "%s\n", CHAR(STRING_ELT(ans, 0)));
                    else
                        fprintf(rep, "EMPTY\n");
                    break;
                default:
                    fprintf(rep, "RTYPE\n");
            }
        }
        UNPROTECT(1);
    }
    UNPROTECT(2);
    fclose(rep);
}

static int nvimcom_checklibs()
{
    const char *libname;
    char buf[256];
    char *libn;
    SEXP a, l;

    PROTECT(a = eval(lang1(install("search")), R_GlobalEnv));

    int newnlibs = Rf_length(a);
    if(nlibs == newnlibs)
        return(nlibs);

    int k = 0;
    for(int i = 0; i < newnlibs; i++){
        if(i == 62)
            break;
        PROTECT(l = STRING_ELT(a, i));
        libname = CHAR(l);
        libn = strstr(libname, "package:");
        if(libn != NULL){
            strncpy(loadedlibs[k], libname, 63);
            loadedlibs[k+1][0] = 0;
#ifdef WIN32
            if(tcltkerr == 0){
                if(strstr(libn, "tcltk") != NULL){
                    REprintf("Error: \"nvimcom\" and \"tcltk\" packages are incompatible!\n");
                    tcltkerr = 1;
                }
            }
#endif
            k++;
        }
        UNPROTECT(1);
    }
    UNPROTECT(1);
    for(int i = 0; i < 64; i++){
        if(loadedlibs[i][0] == 0)
            break;
        for(int j = 0; j < 64; j++){
            libn = strstr(loadedlibs[i], ":");
            libn++;
            if(strcmp(builtlibs[j], libn) == 0)
                break;
            if(builtlibs[j][0] == 0){
                strcpy(builtlibs[j], libn);
                sprintf(buf, "nvimcom:::nvim.buildomnils('%s')", libn);
                nvimcom_eval_expr(buf);
                needsfillmsg = 1;
                break;
            }
        }
    }

    char fn[512];
    snprintf(fn, 510, "%s/libnames_%s", tmpdir, getenv("NVIMR_ID"));
    FILE *f = fopen(fn, "w");
    if(f == NULL){
        REprintf("Error: Could not write to '%s'. [nvimcom]\n", fn);
        return(newnlibs);
    }
    for(int i = 0; i < 64; i++){
        if(builtlibs[i][0] == 0)
            break;
        fprintf(f, "%s\n", builtlibs[i]);
    }
    fclose(f);

    return(newnlibs);
}

static void nvimcom_list_libs()
{
    int newnlibs;

    if(tmpdir[0] == 0)
        return;

    newnlibs = nvimcom_checklibs();

    if(newnlibs == nlibs && openclosel == 0)
        return;

    nlibs = newnlibs;
    openclosel = 0;

    if(objbr_auto != 2)
        return;

    int len, len1;
    char *libn;
    char prefixT[64];
    char prefixL[64];
    char libasenv[64];
    SEXP x, oblist, obj;

    memset(obbrbuf2, 0, obbrbufzise);
    char *p = nvimcom_strcat(obbrbuf2, "Libraries | .GlobalEnv\n\n");

    strcpy(prefixT, "   ");
    strcpy(prefixL, "   ");
    strcat(prefixT, strT);
    strcat(prefixL, strL);

    int save_opendf = opendf;
    int save_openls = openls;
    opendf = 0;
    openls = 0;
    int i = 0;
    while(loadedlibs[i][0] != 0){
        libn = loadedlibs[i] + 8;
        p = nvimcom_strcat(p, "   ##");
        p = nvimcom_strcat(p, libn);
        p = nvimcom_strcat(p, "\t\n");
        if(nvimcom_get_list_status(loadedlibs[i], "library") == 1){
#ifdef WIN32
            if(tcltkerr){
                REprintf("Error: Cannot open libraries due to conflict between \"nvimcom\" and \"tcltk\" packages.\n");
                i++;
                continue;
            }
#endif
            PROTECT(x = allocVector(STRSXP, 1));
            SET_STRING_ELT(x, 0, mkChar(loadedlibs[i]));
            PROTECT(oblist = eval(lang2(install("objects"), x), R_GlobalEnv));
            len = Rf_length(oblist);
            len1 = len - 1;
            for(int j = 0; j < len; j++){
                PROTECT(obj = eval(lang3(install("get"), ScalarString(STRING_ELT(oblist, j)), x), R_GlobalEnv));
                snprintf(libasenv, 63, "%s-", loadedlibs[i]);
                if(j == len1)
                    p = nvimcom_browser_line(&obj, CHAR(STRING_ELT(oblist, j)), libasenv, prefixL, p);
                else
                    p = nvimcom_browser_line(&obj, CHAR(STRING_ELT(oblist, j)), libasenv, prefixT, p);
                UNPROTECT(1);
            }
            UNPROTECT(2);
        }
        i++;
    }

    FILE *f = fopen(liblist, "w");
    if(f == NULL){
        REprintf("Error: Could not write to '%s'. [nvimcom]\n", liblist);
        return;
    }
    fprintf(f, "%s", obbrbuf2);
    fclose(f);
    opendf = save_opendf;
    openls = save_openls;
    nvimcom_nvimclient("UpdateOB('libraries')", edsrvr);
}

Rboolean nvimcom_task(SEXP expr, SEXP value, Rboolean succeeded,
        Rboolean visible, void *userData)
{
    nvimcom_list_libs();
    nvimcom_list_env();
#ifdef WIN32
    r_is_busy = 0;
#endif
    if(edsrvr[0] != 0 && needsfillmsg){
        needsfillmsg = 0;
        nvimcom_nvimclient("FillRLibList()", edsrvr);
    }
    return(TRUE);
}

#ifndef WIN32
static void nvimcom_exec(){
    if(*flag_eval){
        nvimcom_eval_expr(flag_eval);
        *flag_eval = 0;
    }
    if(flag_lsenv)
        nvimcom_list_env();
    if(flag_lslibs)
        nvimcom_list_libs();
    flag_lsenv = 0;
    flag_lslibs = 0;
}

/* Code adapted from CarbonEL.
 * Thanks to Simon Urbanek for the suggestion on r-devel mailing list. */
static void nvimcom_uih(void *data) {
    char buf[16];
    if(read(ifd, buf, 1) < 1)
        REprintf("nvimcom error: read < 1\n");
    R_ToplevelExec(nvimcom_exec, NULL);
    fired = 0;
}

static void nvimcom_fire()
{
    if(fired)
        return;
    fired = 1;
    char buf[16];
    *buf = 0;
    if(write(ofd, buf, 1) <= 0)
        REprintf("nvimcom error: write <= 0\n");
}
#endif

static void nvimcom_save_running_info(int bindportn)
{
    char fn[512];
    snprintf(fn, 510, "%s/nvimcom_running_%s", tmpdir, getenv("NVIMR_ID"));
    FILE *f = fopen(fn, "w");
    if(f == NULL){
        REprintf("Error: Could not write to '%s'. [nvimcom]\n", fn);
    } else {
#ifdef WIN32
#ifdef _WIN64
        fprintf(f, "%s\n%s\n%d\n%" PRId64 "\n%" PRId64 "\n%s\n",
                nvimcom_version, nvimcom_home, bindportn, R_PID,
                (long long)GetForegroundWindow(), search_list);
#else
        fprintf(f, "%s\n%s\n%d\n%d\n%ld\n%s\n",
                nvimcom_version, nvimcom_home, bindportn, R_PID,
                (long)GetForegroundWindow(), search_list);
#endif
#else
        if(getenv("WINDOWID"))
            fprintf(f, "%s\n%s\n%d\n%d\n%s\n%s\n",
                    nvimcom_version, nvimcom_home, bindportn, R_PID,
                    getenv("WINDOWID"), search_list);
        else
            fprintf(f, "%s\n%s\n%d\n%d\n0\n%s\n",
                    nvimcom_version, nvimcom_home, bindportn, R_PID, search_list);
#endif
        fclose(f);
    }
}

static void nvimcom_parse_received_msg(char *buf)
{
    int status;
    char *bbuf;

    if(verbose > 2){
        bbuf = buf;
        if(buf[0] < 30)
            bbuf++;
        REprintf("nvimcom Received: [%d] %s\n", buf[0], bbuf);
    }

    switch(buf[0]){
        case 1: // Set Editor server port number
            bbuf = buf;
            bbuf++;
            strcpy(edsrvr, bbuf);
            nvimcom_del_newline(edsrvr);
            break;
        case 2: // Start updating the Object Browser
            objbr_auto = 1;
#ifdef WIN32
            if(!r_is_busy)
                nvimcom_list_env();
#else
            flag_lsenv = 1;
            flag_lslibs = 1;
            nvimcom_fire();
#endif
            break;
        case 4: // Change value of objbr_auto
            if(buf[1] == 'G'){
                objbr_auto = 1;
                memset(obbrbuf1, 0, obbrbufzise);
            } else if(buf[1] == 'L'){
                nlibs = 0;
                objbr_auto = 2;
            } else {
                objbr_auto = 0;
            }
#ifdef WIN32
            if(!r_is_busy){
                if(objbr_auto == 1)
                    nvimcom_list_env();
                else
                    if(objbr_auto == 2)
                        nvimcom_list_libs();
            }
#else
            if(objbr_auto == 1)
                flag_lsenv = 1;
            else
                if(objbr_auto == 2)
                    flag_lslibs = 1;
            if(objbr_auto != 0)
                nvimcom_fire();
#endif
            break;
#ifdef WIN32
        case 5:
            bbuf = buf;
            bbuf++;
            if(strstr(bbuf, getenv("NVIMR_ID")) == bbuf){
                bbuf += strlen(getenv("NVIMR_ID"));
                r_is_busy = 1;
                Rconsolecmd(bbuf);
            }
            break;
#endif
        case 6: // Toggle list status
#ifdef WIN32
            if(r_is_busy)
                break;
#endif
            bbuf = buf;
            bbuf++;
            if(*bbuf == '&'){
                bbuf++;
#ifdef WIN32
                char flag_eval[512];
                snprintf(flag_eval, 510, "%s <- %s", bbuf, bbuf);
                nvimcom_eval_expr(flag_eval);
                *flag_eval = 0;
                nvimcom_list_env();
#else
                snprintf(flag_eval, 510, "%s <- %s", bbuf, bbuf);
                flag_lsenv = 1;
                nvimcom_fire();
#endif
                break;
            }
            nvimcom_toggle_list_status(bbuf);
            if(strstr(bbuf, "package:") == bbuf){
                openclosel = 1;
#ifdef WIN32
                nvimcom_list_libs();
#else
                flag_lslibs = 1;
#endif
            } else {
#ifdef WIN32
                nvimcom_list_env();
#else
                flag_lsenv = 1;
#endif
            }
#ifndef WIN32
            nvimcom_fire();
#endif
            break;
        case 7: // Close/open all lists
#ifdef WIN32
            if(r_is_busy)
                break;
#endif
            bbuf = buf;
            bbuf++;
            status = atoi(bbuf);
            ListStatus *tmp = firstList;
            if(status){
                while(tmp){
                    if(strstr(tmp->key, "package:") != tmp->key)
                        tmp->status = 1;
                    tmp = tmp->next;
                }
#ifdef WIN32
                nvimcom_list_env();
#else
                flag_lsenv = 1;
#endif
            } else {
                while(tmp){
                    tmp->status = 0;
                    tmp = tmp->next;
                }
                openclosel = 1;
#ifdef WIN32
                nvimcom_list_libs();
                nvimcom_list_env();
#else
                flag_lsenv = 1;
                flag_lslibs = 1;
#endif
            }
#ifndef WIN32
            nvimcom_fire();
#endif
            break;
        case 8: // eval expression
            bbuf = buf;
            bbuf++;
            if(strstr(bbuf, getenv("NVIMR_ID")) == bbuf){
                bbuf += strlen(getenv("NVIMR_ID"));
#ifdef WIN32
                if(!r_is_busy)
                    nvimcom_eval_expr(bbuf);
#else
                strncpy(flag_eval, bbuf, 510);
                nvimcom_fire();
#endif
            } else {
                REprintf("\nvimcom: received invalid NVIMR_ID.\n");
            }
            break;
        default: // do nothing
            REprintf("\nError [nvimcom]: Invalid message received: %s\n", buf);
            break;
    }
}

#ifndef WIN32
static void *nvimcom_server_thread(void *arg)
{
    unsigned short bindportn = 10000;
    ssize_t nread;
    int bsize = 5012;
    char buf[bsize];
    int result;

    struct addrinfo hints;
    struct addrinfo *rp;
    struct addrinfo *res;
    struct sockaddr_storage peer_addr;
    char bindport[16];
    socklen_t peer_addr_len = sizeof(struct sockaddr_storage);

#ifndef __APPLE__
    // block SIGINT
    {
        sigset_t set;
        sigemptyset(&set);
        sigaddset(&set, SIGINT);
        sigprocmask(SIG_BLOCK, &set, NULL);
    }
#endif

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;    /* Allow IPv4 or IPv6 */
    hints.ai_socktype = SOCK_DGRAM; /* Datagram socket */
    hints.ai_flags = AI_PASSIVE;    /* For wildcard IP address */
    hints.ai_protocol = 0;          /* Any protocol */
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;
    rp = NULL;
    result = 1;
    while(rp == NULL && bindportn < 10049){
        bindportn++;
        sprintf(bindport, "%d", bindportn);
        result = getaddrinfo("127.0.0.1", bindport, &hints, &res);
        if(result != 0){
            REprintf("Error at getaddrinfo: %s [nvimcom]\n", gai_strerror(result));
            nvimcom_failure = 1;
            return(NULL);
        }

        for (rp = res; rp != NULL; rp = rp->ai_next) {
            sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
            if (sfd == -1)
                continue;
            if (bind(sfd, rp->ai_addr, rp->ai_addrlen) == 0)
                break;       /* Success */
            close(sfd);
        }
        freeaddrinfo(res);   /* No longer needed */
    }

    if (rp == NULL) {        /* No address succeeded */
        REprintf("Error: Could not bind. [nvimcom]\n");
        nvimcom_failure = 1;
        return(NULL);
    }

    if(verbose > 1)
        REprintf("nvimcom port: %d\n", bindportn);

    flag_lslibs = 1;
    nvimcom_fire();

    // Save a file to indicate that nvimcom is running
    nvimcom_save_running_info(bindportn);

    /* Read datagrams and reply to sender */
    for (;;) {
        memset(buf, 0, bsize);

        nread = recvfrom(sfd, buf, bsize, 0,
                (struct sockaddr *) &peer_addr, &peer_addr_len);
        if (nread == -1){
            if(verbose > 1)
                REprintf("nvimcom: recvfrom failed\n");
            continue;     /* Ignore failed request */
        }
        nvimcom_parse_received_msg(buf);
    }
    return(NULL);
}
#endif

#ifdef WIN32
static void nvimcom_server_thread(void *arg)
{
    unsigned short bindportn = 10000;
    ssize_t nread;
    int bsize = 5012;
    char buf[bsize];
    int result;

    WSADATA wsaData;
    SOCKADDR_IN RecvAddr;
    SOCKADDR_IN peer_addr;
    int peer_addr_len = sizeof (peer_addr);
    int nattp = 0;
    int nfail = 0;
    int lastfail = 0;

    result = WSAStartup(MAKEWORD(2, 2), &wsaData);
    if (result != NO_ERROR) {
        REprintf("WSAStartup failed with error %d\n", result);
        return;
    }

    while(bindportn < 10049){
        bindportn++;
        sfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
        if (sfd == INVALID_SOCKET) {
            REprintf("Error: socket failed with error %d [nvimcom]\n", WSAGetLastError());
            return;
        }

        RecvAddr.sin_family = AF_INET;
        RecvAddr.sin_port = htons(bindportn);
        RecvAddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

        nattp++;
        if(bind(sfd, (SOCKADDR *) & RecvAddr, sizeof (RecvAddr)) == 0)
            break;
        lastfail = WSAGetLastError();
        nfail++;
        if(verbose > 1)
            REprintf("nvimcom: Could not bind to port %d [error  %d].\n", bindportn, lastfail);
    }
    if(nfail > 0 && verbose > 1){
        if(nattp > nfail)
            REprintf("nvimcom: finally, bind to port %d was successful.\n", bindportn);
    }
    if(nattp == nfail){
        if(nfail == 1)
            REprintf("nvimcom: bind failed once with error %d.\n", lastfail);
        else
            REprintf("nvimcom: bind failed %d times and the last error was \"%d\".\n", nfail, lastfail);
        nvimcom_failure = 1;
        return;
    }

    if(verbose > 1)
        REprintf("nvimcom port: %d\n", bindportn);

    // Save a file to indicate that nvimcom is running
    nvimcom_save_running_info(bindportn);

    /* Read datagrams and reply to sender */
    for (;;) {
        memset(buf, 0, bsize);

        nread = recvfrom(sfd, buf, bsize, 0, (SOCKADDR *) &peer_addr, &peer_addr_len);
        if (nread == SOCKET_ERROR) {
            REprintf("nvimcom: recvfrom failed with error %d\n", WSAGetLastError());
            return;
        }
        nvimcom_parse_received_msg(buf);
    }

    REprintf("nvimcom: Finished receiving. Closing socket.\n");
    result = closesocket(sfd);
    if (result == SOCKET_ERROR) {
        REprintf("closesocket failed with error %d\n", WSAGetLastError());
        return;
    }
    WSACleanup();
    return;
}
#endif

void nvimcom_Start(int *vrb, int *odf, int *ols, int *anm, int *lbe, char **pth, char **vcv, char **srchls)
{
    verbose = *vrb;
    opendf = *odf;
    openls = *ols;
    allnames = *anm;
    labelerr = *lbe;

    R_PID = getpid();
    strncpy(nvimcom_version, *vcv, 31);

    if(getenv("NVIMR_TMPDIR")){
        strncpy(nvimcom_home, *pth, 1023);
        strncpy(search_list, *srchls, 1023);
        strncpy(tmpdir, getenv("NVIMR_TMPDIR"), 500);
        if(getenv("NVIMR_SECRET"))
            strncpy(nvimsecr, getenv("NVIMR_SECRET"), 127);
        else
            REprintf("nvimcom: Environment variable NVIMR_SECRET is missing.\n");
    } else {
        if(verbose)
            REprintf("nvimcom: It seems that R was not started by Neovim. The communication with Nvim-R will not work.\n");
        tmpdir[0] = 0;
        return;
    }

    snprintf(liblist, 510, "%s/liblist_%s", tmpdir, getenv("NVIMR_ID"));
    snprintf(globenv, 510, "%s/globenv_%s", tmpdir, getenv("NVIMR_ID"));

    char envstr[1024];
    envstr[0] = 0;
    if(getenv("LC_MESSAGES"))
        strcat(envstr, getenv("LC_MESSAGES"));
    if(getenv("LC_ALL"))
        strcat(envstr, getenv("LC_ALL"));
    if(getenv("LANG"))
        strcat(envstr, getenv("LANG"));
    int len = strlen(envstr);
    for(int i = 0; i < len; i++)
        envstr[i] = toupper(envstr[i]);
    if(strstr(envstr, "UTF-8") != NULL || strstr(envstr, "UTF8") != NULL){
        nvimcom_is_utf8 = 1;
        strcpy(strL, "\xe2\x94\x94\xe2\x94\x80 ");
        strcpy(strT, "\xe2\x94\x9c\xe2\x94\x80 ");
    } else {
        nvimcom_is_utf8 = 0;
        strcpy(strL, "`- ");
        strcpy(strT, "|- ");
    }

#ifndef WIN32
    *flag_eval = 0;
    int fds[2];
    if(pipe(fds) == 0){
        ifd = fds[0];
        ofd = fds[1];
        ih = addInputHandler(R_InputHandlers, ifd, &nvimcom_uih, 32);
    } else {
        REprintf("setwidth error: pipe != 0\n");
        ih = NULL;
    }
#endif

#ifdef WIN32
    tid = _beginthread(nvimcom_server_thread, 0, NULL);
#else
    pthread_create(&tid, NULL, nvimcom_server_thread, NULL);
#endif

    if(nvimcom_failure == 0){
        // Linked list sentinel
        firstList = calloc(1, sizeof(ListStatus));
        firstList->key = (char*)malloc(13 * sizeof(char));
        strcpy(firstList->key, "package:base");

        for(int i = 0; i < 64; i++){
            loadedlibs[i] = (char*)malloc(64 * sizeof(char));
            loadedlibs[i][0] = 0;
        }
        for(int i = 0; i < 64; i++){
            builtlibs[i] = (char*)malloc(64 * sizeof(char));
            builtlibs[i][0] = 0;
        }

        obbrbuf1 = (char*)calloc(obbrbufzise, sizeof(char));
        obbrbuf2 = (char*)calloc(obbrbufzise, sizeof(char));
        if(!obbrbuf1 || !obbrbuf2)
            REprintf("nvimcom: Error allocating memory.\n");

        Rf_addTaskCallback(nvimcom_task, NULL, free, "NVimComHandler", NULL);

        nvimcom_initialized = 1;
        if(verbose > 0)
            // TODO: use packageStartupMessage()
            REprintf("nvimcom %s loaded\n", nvimcom_version);
        if(verbose > 1)
            REprintf("    NVIMR_TMPDIR = %s\n    NVIMR_ID = %s\n",
                    tmpdir, getenv("NVIMR_ID"));
    }
}

void nvimcom_Stop()
{
#ifndef WIN32
    if(ih){
        removeInputHandler(&R_InputHandlers, ih);
        close(ifd);
        close(ofd);
    }
#endif

    if(nvimcom_initialized){
        Rf_removeTaskCallbackByName("NVimComHandler");
#ifdef WIN32
        closesocket(sfd);
        WSACleanup();
#else
        close(sfd);
        pthread_cancel(tid);
        pthread_join(tid, NULL);
#endif
        ListStatus *tmp = firstList;
        while(tmp){
            firstList = tmp->next;
            free(tmp->key);
            free(tmp);
            tmp = firstList;
        }
        for(int i = 0; i < 64; i++){
            free(loadedlibs[i]);
            loadedlibs[i] = NULL;
        }
        if(obbrbuf1)
            free(obbrbuf1);
        if(obbrbuf2)
            free(obbrbuf2);
        if(verbose)
            REprintf("nvimcom stopped\n");
    }
    nvimcom_initialized = 0;
}
R/nvimcom/src/apps/Makefile	[[[1
8
CC?=gcc
CFLAGS+=-pthread -std=gnu99 -O2 -Wall

nclientserver: nclientserver.c
	$(CC) $(CFLAGS) nclientserver.c -o nclientserver

clean:

R/nvimcom/src/apps/Makefile.win	[[[1
13
CC?=gcc
ifeq "$(WIN)" "64"
    CFLAGS+=-m64 -mwindows -std=gnu99 -O3 -Wall -DWIN32
else
    CFLAGS+=-m32 -mwindows -std=gnu99 -O3 -Wall -DWIN32
endif

# Note: -lWs2_32 must be after the object files
nclientserver.exe: nclientserver.c
	$(CC) $(CFLAGS) nclientserver.c -o nclientserver.exe -lWs2_32

clean:

R/nvimcom/src/apps/nclientserver.c	[[[1
712
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#ifdef WIN32
#include <winsock2.h>
#include <process.h>
#include <windows.h>
HWND NvimHwnd = NULL;
HWND RConsole = NULL;
#else
#include <stdint.h>
#include <sys/socket.h>
#include <netdb.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>
#endif

static char NvimcomPort[16];
static char VimSecret[128];
static int VimSecretLen;
FILE *df = NULL;

#ifdef WIN32
static SOCKET Sfd;
static int Tid;
#else
static int Sfd = -1;
static pthread_t Tid;
#endif

static void HandleSigTerm(int s)
{
    if(df){
        fprintf(df, "HandleSigTerm called\n");
        fflush(df);
    }
    exit(0);
}

static void RegisterPort(int bindportn)
{
    // Register the port:
    printf("call RSetMyPort('%d')\n", bindportn);
    fflush(stdout);
}

static void ParseMsg(char *buf)
{
    char *bbuf = buf;
    if(df){
        fprintf(df, "tcp: %s\n", bbuf);
        fflush(df);
    }

    if(strstr(bbuf, VimSecret) == bbuf){
        bbuf += VimSecretLen;
        printf("%s\n", bbuf);
        fflush(stdout);
    } else {
        fprintf(stderr, "Strange string received: \"%s\"\n", bbuf);
        fflush(stderr);
    }
}

#ifndef WIN32
static void *NeovimServer(void *arg)
{
    unsigned short bindportn = 10100;
    ssize_t nread;
    int bsize = 5012;
    char buf[bsize];
    int result;

    struct addrinfo hints;
    struct addrinfo *rp;
    struct addrinfo *res;
    struct sockaddr_storage peer_addr;
    int Sfd = -1;
    char bindport[16];
    socklen_t peer_addr_len = sizeof(struct sockaddr_storage);

    // block SIGINT
    {
        sigset_t set;
        sigemptyset(&set);
        sigaddset(&set, SIGINT);
        sigprocmask(SIG_BLOCK, &set, NULL);
    }

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;    /* Allow IPv4 or IPv6 */
    hints.ai_socktype = SOCK_DGRAM; /* Datagram socket */
    hints.ai_flags = AI_PASSIVE;    /* For wildcard IP address */
    hints.ai_protocol = 0;          /* Any protocol */
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;
    rp = NULL;
    result = 1;
    while(rp == NULL && bindportn < 10149){
        bindportn++;
        sprintf(bindport, "%d", bindportn);
        result = getaddrinfo("127.0.0.1", bindport, &hints, &res);
        if(result != 0){
            fprintf(stderr, "Error at getaddrinfo (%s)\n", gai_strerror(result));
            fflush(stderr);
            return NULL;
        }

        for (rp = res; rp != NULL; rp = rp->ai_next) {
            Sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
            if (Sfd == -1)
                continue;
            if (bind(Sfd, rp->ai_addr, rp->ai_addrlen) == 0)
                break;       /* Success */
            close(Sfd);
        }
        freeaddrinfo(res);   /* No longer needed */
    }

    if (rp == NULL) {        /* No address succeeded */
        fprintf(stderr, "Could not bind\n");
        fflush(stderr);
        return NULL;
    }

    RegisterPort(bindportn);

    /* Read datagrams and reply to sender */
    for (;;) {
        memset(buf, 0, bsize);

        nread = recvfrom(Sfd, buf, bsize, 0,
                (struct sockaddr *) &peer_addr, &peer_addr_len);
        if (nread == -1){
            fprintf(stderr, "recvfrom failed [port %d]\n", bindportn);
            fflush(stderr);
            continue;     /* Ignore failed request */
        }
        if(strstr(buf, "QUIT_NVINSERVER_NOW"))
            break;

        ParseMsg(buf);
    }
    if(df){
        fclose(df);
        df = NULL;
    }
    return NULL;
}
#endif

#ifdef WIN32
static void NeovimServer(void *arg)
{
    unsigned short bindportn = 10100;
    ssize_t nread;
    int bsize = 5012;
    char buf[bsize];
    int result;

    WSADATA wsaData;
    SOCKADDR_IN RecvAddr;
    SOCKADDR_IN peer_addr;
    SOCKET Sfd;
    int peer_addr_len = sizeof (peer_addr);
    int nattp = 0;
    int nfail = 0;

    result = WSAStartup(MAKEWORD(2, 2), &wsaData);
    if (result != NO_ERROR) {
        fprintf(stderr, "WSAStartup failed with error %d.\n", result);
        fflush(stderr);
        return;
    }

    while(bindportn < 10149){
        bindportn++;
        Sfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
        if (Sfd == INVALID_SOCKET) {
            fprintf(stderr, "socket failed with error %d\n", WSAGetLastError());
            fflush(stderr);
            return;
        }

        RecvAddr.sin_family = AF_INET;
        RecvAddr.sin_port = htons(bindportn);
        RecvAddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

        nattp++;
        if(bind(Sfd, (SOCKADDR *) & RecvAddr, sizeof (RecvAddr)) == 0)
            break;
        nfail++;
    }
    if(nattp == nfail){
        fprintf(stderr, "Could not bind\n");
        fflush(stderr);
        return;
    }

    RegisterPort(bindportn);

    /* Read datagrams and reply to sender */
    for (;;) {
        memset(buf, 0, bsize);

        nread = recvfrom(Sfd, buf, bsize, 0,
                (SOCKADDR *) &peer_addr, &peer_addr_len);
        if (nread == SOCKET_ERROR) {
            fprintf(stderr, "recvfrom failed with error %d [port %d]\n",
                    bindportn, WSAGetLastError());
            fflush(stderr);
            return;
        }
        if(strstr(buf, "QUIT_NVINSERVER_NOW"))
            break;

        ParseMsg(buf);
    }
    if(df){
        fprintf(df, "Neovim server: Finished receiving. Closing socket.\n");
        fflush(df);
    }
    result = closesocket(Sfd);
    if (result == SOCKET_ERROR) {
        fprintf(stderr, "closesocket failed with error %d\n", WSAGetLastError());
        fflush(stderr);
        return;
    }
    WSACleanup();
    if(df){
        fclose(df);
        df = NULL;
    }
    return;
}
#endif

#ifndef WIN32
static void SendToServer(const char *port, const char *msg)
{
    struct addrinfo hints;
    struct addrinfo *result, *rp;
    int s, a;
    size_t len;

    /* Obtain address(es) matching host/port */
    if(strncmp(port, "0", 15) == 0){
        fprintf(stderr, "Port is 0\n");
        fflush(stderr);
        return;
    }

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_flags = 0;
    hints.ai_protocol = 0;

    a = getaddrinfo("127.0.0.1", port, &hints, &result);
    if (a != 0) {
        fprintf(stderr, "Error in getaddrinfo [port = '%s'] [msg = '%s']: %s\n", port, msg, gai_strerror(a));
        fflush(stderr);
        return;
    }

    for (rp = result; rp != NULL; rp = rp->ai_next) {
        s = socket(rp->ai_family, rp->ai_socktype,
                rp->ai_protocol);
        if (s == -1)
            continue;

        if (connect(s, rp->ai_addr, rp->ai_addrlen) != -1)
            break;		   /* Success */

        close(s);
    }

    if (rp == NULL) {		   /* No address succeeded */
        fprintf(stderr, "Could not connect.\n");
        fflush(stderr);
        return;
    }

    freeaddrinfo(result);	   /* No longer needed */

    len = strlen(msg);
    if (write(s, msg, len) != (ssize_t)len) {
        fprintf(stderr, "Partial/failed write.\n");
        fflush(stderr);
        return;
    }
}
#endif

#ifdef WIN32
static void SendToServer(const char *port, const char *msg)
{
    WSADATA wsaData;
    struct sockaddr_in peer_addr;
    SOCKET sfd;

    if(strncmp(port, "0", 15) == 0){
        fprintf(stderr, "Port is 0\n");
        fflush(stderr);
        return;
    }

    WSAStartup(MAKEWORD(2, 2), &wsaData);
    sfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);

    if(sfd < 0){
        fprintf(stderr, "Socket failed\n");
        fflush(stderr);
        return;
    }

    peer_addr.sin_family = AF_INET;
    peer_addr.sin_port = htons(atoi(port));
    peer_addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if(connect(sfd, (struct sockaddr *)&peer_addr, sizeof(peer_addr)) < 0){
        fprintf(stderr, "Could not connect\n");
        fflush(stderr);
        return;
    }

    int len = strlen(msg);
    if (send(sfd, msg, len+1, 0) < 0) {
        fprintf(stderr, "Failed sending message\n");
        fflush(stderr);
        return;
    }

    if(closesocket(sfd) < 0){
        fprintf(stderr, "Error closing socket\n");
        fflush(stderr);
    }
}

static void SendToRConsole(char *aString){
    if(!RConsole){
        fprintf(stderr, "R Console window ID not defined [SendToRConsole]\n");
        fflush(stderr);
        return;
    }

    // FIXME: Delete this code when $WINDOWID is implemented in NeovimQt
    if(!NvimHwnd)
        NvimHwnd = GetForegroundWindow();

    char msg[512];
    snprintf(msg, 510, "\005%s%s", getenv("NVIMR_ID"), aString);
    SendToServer(NvimcomPort, msg);
    Sleep(0.02);

    // Necessary to force RConsole to actually process the line
    PostMessage(RConsole, WM_NULL, 0, 0);
}

static void RClearConsole(){
    if(!RConsole){
        fprintf(stderr, "R Console window ID not defined [RClearConsole]\n");
        fflush(stderr);
        return;
    }

    SetForegroundWindow(RConsole);
    keybd_event(VK_CONTROL, 0, 0, 0);
    keybd_event(VkKeyScan('L'), 0, KEYEVENTF_EXTENDEDKEY | 0, 0);
    Sleep(0.05);
    keybd_event(VkKeyScan('L'), 0, KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
    keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);
    Sleep(0.05);
    PostMessage(RConsole, WM_NULL, 0, 0);
}

static void SaveWinPos(char *cachedir){
    if(!RConsole){
        fprintf(stderr, "R Console window ID not defined [SaveWinPos]\n");
        fflush(stderr);
        return;
    }

    RECT rcR, rcV;
    if(!GetWindowRect(RConsole, &rcR)){
        fprintf(stderr, "Could not get R Console position\n");
        fflush(stderr);
        return;
    }

    if(!GetWindowRect(NvimHwnd, &rcV)){
        fprintf(stderr, "Could not get Neovim position\n");
        fflush(stderr);
        return;
    }

    rcR.right = rcR.right - rcR.left;
    rcR.bottom = rcR.bottom - rcR.top;
    rcV.right = rcV.right - rcV.left;
    rcV.bottom = rcV.bottom - rcV.top;

    char fname[512];
    snprintf(fname, 511, "%s/win_pos", cachedir);
    FILE *f = fopen(fname, "w");
    if(f == NULL){
        fprintf(stderr, "Could not write to '%s'\n", fname);
        fflush(stderr);
        return;
    }
    fprintf(f, "%ld\n%ld\n%ld\n%ld\n%ld\n%ld\n%ld\n%ld\n",
            rcR.left, rcR.top, rcR.right, rcR.bottom,
            rcV.left, rcV.top, rcV.right, rcV.bottom);
    fclose(f);
}

static void ArrangeWindows(char *cachedir){
    if(!RConsole){
        fprintf(stderr, "R Console window ID not defined [ArrangeWindows]\n");
        fflush(stderr);
        return;
    }

    char fname[512];
    snprintf(fname, 511, "%s/win_pos", cachedir);
    FILE *f = fopen(fname, "r");
    if(f == NULL){
        fprintf(stderr, "Could not read '%s'\n", fname);
        fflush(stderr);
        return;
    }

    RECT rcR, rcV;
    char b[32];
    if((fgets(b, 31, f))){
        rcR.left = atol(b);
    } else {
        fprintf(stderr, "Error reading R left position\n");
        fflush(stderr);
        fclose(f);
        return;
    }
    if((fgets(b, 31, f))){
        rcR.top = atol(b);
    } else {
        fprintf(stderr, "Error reading R top position\n");
        fflush(stderr);
        fclose(f);
        return;
    }
    if((fgets(b, 31, f))){
        rcR.right = atol(b);
    } else {
        fprintf(stderr, "Error reading R right position\n");
        fflush(stderr);
        fclose(f);
        return;
    }
    if((fgets(b, 31, f))){
        rcR.bottom = atol(b);
    } else {
        fprintf(stderr, "Error reading R bottom position\n");
        fflush(stderr);
        fclose(f);
        return;
    }
    if((fgets(b, 31, f))){
        rcV.left = atol(b);
    } else {
        fprintf(stderr, "Error reading Neovim left position\n");
        fflush(stderr);
        fclose(f);
        return;
    }
    if((fgets(b, 31, f))){
        rcV.top = atol(b);
    } else {
        fprintf(stderr, "Error reading Neovim top position\n");
        fflush(stderr);
        fclose(f);
        return;
    }
    if((fgets(b, 31, f))){
        rcV.right = atol(b);
    } else {
        fprintf(stderr, "Error reading Neovim right position\n");
        fflush(stderr);
        fclose(f);
        return;
    }
    if((fgets(b, 31, f))){
        rcV.bottom = atol(b);
    } else {
        fprintf(stderr, "Error reading Neovim bottom position\n");
        fflush(stderr);
        fclose(f);
        return;
    }

    if(rcR.left > 0 && rcR.top > 0 && rcR.right > 0 && rcR.bottom > 0 &&
            rcR.right > rcR.left && rcR.bottom > rcR.top){
        if(!SetWindowPos(RConsole, HWND_TOP,
                    rcR.left, rcR.top, rcR.right, rcR.bottom, 0)){
            fprintf(stderr, "Error positioning RConsole window\n");
            fflush(stderr);
            fclose(f);
            return;
        }
    }

    if(rcV.left > 0 && rcV.top > 0 && rcV.right > 0 && rcV.bottom > 0 &&
            rcV.right > rcV.left && rcV.bottom > rcV.top){
        if(!SetWindowPos(NvimHwnd, HWND_TOP,
                    rcV.left, rcV.top, rcV.right, rcV.bottom, 0)){
            fprintf(stderr, "Error positioning Neovim window\n");
            fflush(stderr);
        }
    }

    SetForegroundWindow(NvimHwnd);
    fclose(f);
}
#endif

int main(int argc, char **argv){
    char line[1024];
    char *msg;
    int keep_running = 1;
    memset(line, 0, 1024);
    strcpy(NvimcomPort, "0");

    if(argc == 3 && getenv("NVIMR_PORT") && getenv("NVIMR_SECRET")){
        snprintf(line, 1023, "%scall SyncTeX_backward('%s', %s)", getenv("NVIMR_SECRET"), argv[1], argv[2]);
        SendToServer(getenv("NVIMR_PORT"), line);

        if(getenv("DEBUG_NVIMR")){
            FILE *df1 = fopen("/tmp/nclientserver_1_debug", "a");
            if(df1 != NULL){
                fprintf(df1, "%s %s %s %s\n", getenv("NVIMR_PORT"), getenv("NVIMR_SECRET"), argv[1], argv[2]);
                fclose(df1);
            }
        }
        return 0;
    }

    if(getenv("DEBUG_NVIMR")){
        df = fopen("/tmp/nclientserver_debug", "w");
        if(df){
            fprintf(df, "NVIMR_SECRET=%s NVIMCOMPORT=%s\n",
                    getenv("NVIMR_SECRET"), getenv("NVIMCOMPORT"));
            fflush(df);
        } else {
            fprintf(stderr, "Error opening \"nclientserver_debug\" for writing\n");
            fflush(stderr);
        }
    }

    // Set nvimcom port
    if(getenv("NVIMCOMPORT"))
        strcpy(NvimcomPort, getenv("NVIMCOMPORT"));

#ifdef WIN32
    // Set the value of RConsole
    if(getenv("RCONSOLE")){
#ifdef _WIN64
        RConsole = (HWND)atoll(getenv("RCONSOLE"));
#else
        RConsole = (HWND)atol(getenv("RCONSOLE"));
#endif
    } else {
        fprintf(stderr, "$RCONSOLE not defined\n");
        fflush(stderr);

        RConsole = FindWindow(NULL, "R Console (64-bit)");
        if(!RConsole){
            RConsole = FindWindow(NULL, "R Console (32-bit)");
            if(!RConsole)
                RConsole = FindWindow(NULL, "R Console");
        }
        if(!RConsole){
            fprintf(stderr, "\"R Console\" window not found\n");
            fflush(stderr);
        }
    }

    // Set the value of NvimHwnd
    if(getenv("WINDOWID")){
#ifdef _WIN64
        NvimHwnd = (HWND)atoll(getenv("WINDOWID"));
#else
        NvimHwnd = (HWND)atol(getenv("WINDOWID"));
#endif
    } else {
        //fprintf(stderr, "$WINDOWID not defined\n");
        //fflush(stderr);
        // FIXME: Delete this code when $WINDOWID is implemented in NeovimQt
        NvimHwnd = FindWindow(NULL, "Neovim");
        if(!NvimHwnd){
            fprintf(stderr, "\"Neovim\" window not found\n");
            fflush(stderr);
        }
    }
#endif

    // Start the server
    if(!getenv("NVIMR_SECRET")){
        fprintf(stderr, "NVIMR_SECRET not found\n");
        fflush(stderr);
        exit(1);
    }
    strncpy(VimSecret, getenv("NVIMR_SECRET"), 127);
    VimSecretLen = strlen(VimSecret);

    // Finish immediately with SIGTERM
    signal(SIGTERM, HandleSigTerm);

#ifdef WIN32
    Sleep(1000);
#else
    sleep(1);
#endif

#ifdef WIN32
    Tid = _beginthread(NeovimServer, 0, NULL);
#else
    pthread_create(&Tid, NULL, NeovimServer, NULL);
#endif

    while(fgets(line, 1023, stdin) && keep_running){
        if(df){
            msg = line;
            msg++;
            fprintf(df, "stdin: [%d] %s", (unsigned int)*line, msg);
            fflush(df);
        }

        for(unsigned int i = 0; i < strlen(line); i++)
            if(line[i] == '\n' || line[i] == '\r')
                line[i] = 0;
        msg = line;
        switch(*msg){
            case 1: // SetPort
                msg++;
#ifdef WIN32
                char *p = msg;
                while(*p != ' ')
                    p++;
                *p = 0;
                p++;
                strncpy(NvimcomPort, msg, 15);
#ifdef _WIN64
                RConsole = (HWND)atoll(p);
#else
                RConsole = (HWND)atol(p);
#endif
                if(msg[0] == '0')
                    RConsole = NULL;
#else
                strncpy(NvimcomPort, msg, 15);
#endif
                break;
            case 2: // Send message
                msg++;
                SendToServer(NvimcomPort, msg);
                break;
#ifdef WIN32
            case 3: // SendToRConsole
                msg++;
                SendToRConsole(msg);
                break;
            case 4: // SaveWinPos
                msg++;
                SaveWinPos(msg);
                break;
            case 5: // ArrangeWindows
                msg++;
                ArrangeWindows(msg);
                break;
            case 6:
                RClearConsole();
                break;
            case 7: // RaiseNvimWindow
                if(NvimHwnd)
                    SetForegroundWindow(NvimHwnd);
                break;
#endif
            case 8: // Quit now
                keep_running = 0;
                break;
            default:
                fprintf(stderr, "Unknown command received: [%d] %s\n", line[0], msg);
                fflush(stderr);
                break;
        }
        memset(line, 0, 1024);
    }
#ifdef WIN32
        closesocket(Sfd);
        WSACleanup();
#else
        close(Sfd);
        pthread_cancel(Tid);
        pthread_join(Tid, NULL);
#endif
    if(df)
        fclose(df);
    return 0;
}
