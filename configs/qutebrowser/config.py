#import setproctitle
#setproctitle.setproctitle("qutebrowser")

#config.unbind("+")

#config.unbind("<Ctrl-e>")

c.bindings.commands = {
    'insert': {
        '<Ctrl-w>'        : 'fake-key <Alt-Backspace>',
        '<Ctrl-u>'        : 'fake-key <Ctrl-Backspace>',
        '<Ctrl-f>'        : 'fake-key <Right>',
        '<Ctrl-b>'        : 'fake-key <Left>',
        '<Ctrl-a>'        : 'fake-key <Home>',
        '<Ctrl-e>'        : 'fake-key <End>',
        '<Ctrl-j>'        : 'fake-key <Down>',
        '<Ctrl-k>'        : 'fake-key <Up>',
        #'<Alt-v>'         : 'fake-key <PgUp>',
        #'<Ctrl-v>'        : 'fake-key <PgDown>',
        '<Alt-f>'         : 'fake-key <Ctrl-Right>',
        '<Alt-b>'         : 'fake-key <Ctrl-Left>',
        '<Ctrl-d>'        : 'fake-key <Delete>',
        '<Ctrl-y>'        : 'insert-text {primary}',
    }
}
#c.bindings.commands = {
#    'passthrough': {
#        '<Ctrl-w>'        : 'fake-key <Alt-Backspace>',
#        '<Ctrl-u>'        : 'fake-key <Ctrl-Backspace>',
#        '<Ctrl-n>'        : 'tab-next',
#        '<Ctrl-p>'        : 'tab-prev',
#    }
#}

config.unbind('<Ctrl-v>')
config.bind("<Ctrl-i>", "enter-mode 'passthrough'")
config.bind('<Ctrl-i>', 'leave-mode', mode='passthrough')

# Bindings
config.bind("gi", "hint inputs")
config.bind("<f12>", "inspector")

#config.unbind("+")
#config.unbind("-")
#config.unbind("=")
config.bind("zi", "zoom-in")
config.bind("zo", "zoom-out")
config.bind("zz", "zoom")

config.unbind("O")
config.unbind("T")
config.unbind("th")
config.unbind("tl")
config.bind("O", "set-cmd-text :open {url:pretty}")
config.bind("T", "set-cmd-text :open -t {url:pretty}")
config.bind("t", "set-cmd-text -s :open -t")

config.unbind("<ctrl+tab>")
config.bind("<ctrl+n>", "tab-next")
config.bind("<ctrl+p>", "tab-prev")

config.unbind("ZQ")
config.unbind("ZZ")
config.unbind("<ctrl+q>")
config.bind("<ctrl+q>", "wq")


# Aliases for commands. The keys of the given dictionary are the
# aliases, while the values are the commands they map to.
c.aliases = {
        "w": "session-save",
        "wq": "quit --save",
        "mpv": "spawn -d mpv --force-window=immediate {url}",
        "nicehash": "spawn --userscript nicehash",
        "pass": "spawn -d pass -c",
        "i": "jseval javascript:function iprl5(){var d=document,z=d.createElement('scr'+'ipt'),b=d.body,l=d.location;try{if(!b)throw(0);d.title='(Saving...) '+d.title;z.setAttribute('src',l.protocol+'//www.instapaper.com/j/Uv0833HMBT1g?a=read-later&u='+encodeURIComponent(l.href)+'&t='+(new Date().getTime()));b.appendChild(z);}catch(e){alert('Please wait until the page has loaded.');}}iprl5();void(0)", 
        "m": "jseval javascript: document.getElementsByTagName('body')[0].appendChild(document.createElement('script')).setAttribute('src','https://www.mendeley.com/minified/bookmarklet.js');",
        }

# Always restore open sites when qutebrowser is reopened.
c.auto_save.session = True

# Foreground color of the URL in the statusbar on successful load
# (https).
c.colors.statusbar.url.success.https.fg = "white"

# Background color of unselected tabs.
c.colors.tabs.even.bg = "silver"
c.colors.tabs.odd.bg = "gainsboro"

# Foreground color of unselected tabs.
c.colors.tabs.even.fg = "#666666"
c.colors.tabs.odd.fg = c.colors.tabs.even.fg

#c.colors.statusbar.passthrough.bg = c.colors.tabs.selected.even.bg
#c.colors.statusbar.passthrough.fg = c.colors.tabs.selected.even.fg

#c.statusbar.hide = True

## avoid the popup gmail confirmation everytime
config.set('content.register_protocol_handler', True, '*://mail.google.com/*')
config.set('content.host_blocking.whitelist', ['*://statcounter.com/*'])

# The height of the completion, in px or as percentage of the window.
c.completion.height = "20%"

# Move on to the next part when there's only one possible completion
# left.
c.completion.quick = False

# When to show the autocompletion window.
# Valid values:
#   - always: Whenever a completion is available.
#   - auto: Whenever a completion is requested.
#   - never: Never.
c.completion.show = "auto"

# Whether quitting the application requires a confirmation.
# Valid values:
#   - always: Always show a confirmation.
#   - multiple-tabs: Show a confirmation if multiple tabs are opened.
#   - downloads: Show a confirmation if downloads are running
#   - never: Never show a confirmation.
c.confirm_quit = ["downloads"]

# Value to send in the `Accept-Language` header.
c.content.headers.accept_language = "en-US,en;q=0.8,fi;q=0.6"

# The proxy to use. In addition to the listed values, you can use a
# `socks://...` or `http://...` URL.
# Valid values:
#   - system: Use the system wide proxy.
#   - none: Don"t use any proxy
c.content.proxy = "none"

# Validate SSL handshakes.
# Valid values:
#   - true
#   - false
#   - ask
c.content.ssl_strict = True

# A list of user stylesheet filenames to use.
c.content.user_stylesheets = "user.css"

# The directory to save downloads to. If unset, a sensible os-specific
# default is used.
c.downloads.location.directory = "/Users/guofeng/Downloads"

# Prompt the user for the download location. If set to false,
# `downloads.location.directory` will be used.
c.downloads.location.prompt = False

# The editor (and arguments) to use for the `open-editor` command. `{}`
# gets replaced by the filename of the file to be edited.
c.editor.command = ["iTerm2", "-e", "vim '{}'"]

monospace = "14px 'Bok MonteCarlo'"

# Font used in the completion categories.
c.fonts.completion.category = f"bold {monospace}"

# Font used in the completion widget.
c.fonts.completion.entry = monospace

# Font used for the debugging console.
c.fonts.debug_console = monospace

# Font used for the downloadbar.
c.fonts.downloads = monospace

# Font used in the keyhint widget.
c.fonts.keyhint = monospace

# Font used for error messages.
c.fonts.messages.error = monospace

# Font used for info messages.
c.fonts.messages.info = monospace

# Font used for warning messages.
c.fonts.messages.warning = monospace

# Font used for prompts.
c.fonts.prompts = monospace

monospace2 = "10px 'Bok MonteCarlo'"

# Font used in the statusbar.
c.fonts.statusbar = monospace2

# Font used in the tab bar.
c.fonts.tabs = monospace

# Font used for the hints.
c.fonts.hints = "bold 13px 'DejaVu Sans Mono'"

# Chars used for hint strings.
c.hints.chars = "asdfghjklie"

# Leave insert mode if a non-editable element is clicked.
c.input.insert_mode.auto_leave = True

# Automatically enter insert mode if an editable element is focused
# after loading the page.
c.input.insert_mode.auto_load = True

# Show a scrollbar.
# c.scrolling.bar = True

# Enable smooth scrolling for web pages. Note smooth scrolling does not
# work with the `:scroll-px` command.
c.scrolling.smooth = False

# Open new tabs (middleclick/ctrl+click) in the background.
c.tabs.background = True

# Behavior when the last tab is closed.
# Valid values:
#   - ignore: Don't do anything.
#   - blank: Load a blank page.
#   - startpage: Load the start page.
#   - default-page: Load the default page.
#   - close: Close the window.
c.tabs.last_close = "close"

# Padding around text for tabs
c.tabs.padding = {
    "left": 5,
    "right": 5,
    "top": 0,
    "bottom": 1,
}

# Which tab to select when the focused tab is removed.
# Valid values:
#   - prev: Select the tab which came before the closed one (left in horizontal, above in vertical).
#   - next: Select the tab which came after the closed one (right in horizontal, below in vertical).
#   - last-used: Select the previously selected tab.
c.tabs.select_on_remove = "prev"

# Width of the progress indicator (0 to disable).
c.tabs.indicator.width= 0

c.scrolling.bar='always'

# The page to open if :open -t/-b/-w is used without URL. Use
# `about:blank` for a blank page.
c.url.default_page = "https://www.google.com"

# Definitions of search engines which can be used via the address bar.
# Maps a searchengine name (such as `DEFAULT`, or `ddg`) to a URL with a
# `{}` placeholder. The placeholder will be replaced by the search term,
# use `{{` and `}}` for literal `{`/`}` signs. The searchengine named
# `DEFAULT` is used when `url.auto_search` is turned on and something
# else than a URL was entered to be opened. Other search engines can be
# used by prepending the search engine name to the search term, e.g.
# `:open google qutebrowser`.
c.url.searchengines = {"DEFAULT": "https://www.google.com/search?q={}", "c": "https://scholar.google.com/scholar?q={}"}

# The page(s) to open at the start.
c.url.start_pages = "https://www.google.com"

# The format to use for the window title. The following placeholders are
# defined:
#   * `{perc}`: The percentage as a string like `[10%]`.
#   * `{perc_raw}`: The raw percentage, e.g. `10`
#   * `{title}`: The title of the current web page
#   * `{title_sep}`: The string ` - ` if a title is set, empty otherwise.
#   * `{id}`: The internal window ID of this window.
#   * `{scroll_pos}`: The page scroll position.
#   * `{host}`: The host of the current web page.
#   * `{backend}`: Either ''webkit'' or ''webengine''
#   * `{private}` : Indicates when private mode is enabled.
c.window.title_format = "{private}{perc}{title}{title_sep}qutebrowser"
