vim = hs.loadSpoon('VimMode')

-- Basic key binding to ctrl+;
-- You can choose any key binding you want here, see:
--   https://www.hammerspoon.org/docs/hs.hotkey.html#bind

hs.hotkey.bind({'ctrl'}, ';', function()
  vim:enter()
end)

vim:enableKeySequence('j', 'k')

vim:disableForApp('iTerm2')
vim:disableForApp('MacVim')
vim:disableForApp('Terminal')
vim:disableForApp('VIM')
vim:disableForApp('Vivaldi')
vim:disableForApp('Firefox')
vim:disableForApp('Emacs')
