# Preconfigured stuff
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://accounts.google.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36', 'https://*.slack.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://docs.google.com/*')
config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

# Auto save of config, cookies etc
c.auto_save.interval = 15000

# Auto save opened tabs
c.auto_save.session = False

# Which categories to show (in which order) in the :open completion
c.completion.open_categories = ['quickmarks', 'bookmarks', 'history', 'searchengines']

# Start pages
c.url.start_pages = ['https://www.google.com']

# Search engine
c.url.searchengines = {
    'DEFAULT': 'https://google.com/search?q={}',
    'duck': 'https://duckduckgo.com/?q={}'
}

# Aliases
c.aliases = {
    'ab': 'adblock-update',
    'o': 'open'
}

#Color scheme
nord = {
    'base03': '#3b4252',
    'base02': '#434c5e',
    'base01': '#e5e9f0',
    'base00': '#d8dee9',
    'base0': '#4c566a',
    'base1': '#5e81ac',
    'base2': '#eee8d5',
    'base3': '#eceff4',
    'yellow': '#ebcb8b',
    'orange': '#ebcb8b',
    'red': '#bf616a',
    'magenta': '#b48ead',
    'violet': '#8fbcbb',
    'blue': '#5e81ac',
    'cyan': '#88c0d0',
    'green': '#a3be8c'
}

## Background color of the completion widget category headers.
c.colors.completion.category.bg = nord['base03']
## Bottom border color of the completion widget category headers.
c.colors.completion.category.border.bottom = nord['base03']
## Top border color of the completion widget category headers.
c.colors.completion.category.border.top = nord['base03']
## Foreground color of completion widget category headers.
c.colors.completion.category.fg = nord['base3']
## Background color of the completion widget for even rows.
c.colors.completion.even.bg = nord['base02']
## Text color of the completion widget.
c.colors.completion.fg = nord['base3']
## Background color of the selected completion item.
c.colors.completion.item.selected.bg = nord['violet']
## Bottom border color of the selected completion item.
c.colors.completion.item.selected.border.bottom = nord['violet']
## Top border color of the completion widget category headers.
c.colors.completion.item.selected.border.top = nord['violet']
## Foreground color of the selected completion item.
c.colors.completion.item.selected.fg = nord['base3']
## Foreground color of the matched text in the completion.
c.colors.completion.match.fg = nord['base2']
## Background color of the completion widget for odd rows.
c.colors.completion.odd.bg = nord['base02']
## Color of the scrollbar in completion view
c.colors.completion.scrollbar.bg = nord['base0']
## Color of the scrollbar handle in completion view.
c.colors.completion.scrollbar.fg = nord['base2']
## Background color for the download bar.
c.colors.downloads.bar.bg = nord['base03']
## Background color for downloads with errors.
c.colors.downloads.error.bg = nord['red']
## Foreground color for downloads with errors.
c.colors.downloads.error.fg = nord['base3']
## Color gradient start for download text.
c.colors.downloads.start.fg = nord['base3']
## Background color for hints. Note that you can use a `rgba(...)` value
## for transparency.
c.colors.hints.bg = nord['violet']
## Font color for hints.
c.colors.hints.fg = nord['base3']
## Font color for the matched part of hints.
c.colors.hints.match.fg = nord['base2']
## Background color of the keyhint widget.
# c.colors.keyhint.bg = 'rgba(0, 0, 0, 80%)'
## Text color for the keyhint widget.
c.colors.keyhint.fg = nord['base3']
## Highlight color for keys to complete the current keychain.
c.colors.keyhint.suffix.fg = nord['yellow']
## Background color of an error message.
c.colors.messages.error.bg = nord['red']
## Border color of an error message.
c.colors.messages.error.border = nord['red']
## Foreground color of an error message.
c.colors.messages.error.fg = nord['base3']
## Background color of an info message.
c.colors.messages.info.bg = nord['base03']
## Border color of an info message.
c.colors.messages.info.border = nord['base03']
## Foreground color an info message.
c.colors.messages.info.fg = nord['base3']
## Background color of a warning message.
c.colors.messages.warning.bg = nord['orange']
## Border color of a warning message.
c.colors.messages.warning.border = nord['orange']
## Foreground color a warning message.
c.colors.messages.warning.fg = nord['base3']
## Background color for prompts.
c.colors.prompts.bg = nord['base02']
## Border used around UI elements in prompts.
c.colors.prompts.border = '1px solid ' + nord['base3']
## Foreground color for prompts.
c.colors.prompts.fg = nord['base3']
## Background color for the selected item in filename prompts.
c.colors.prompts.selected.bg = nord['base01']
## Background color of the statusbar in caret mode.
c.colors.statusbar.caret.bg = nord['blue']
## Foreground color of the statusbar in caret mode.
c.colors.statusbar.caret.fg = nord['base3']
## Background color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.bg = nord['violet']
## Foreground color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.fg = nord['base3']
## Background color of the statusbar in command mode.
c.colors.statusbar.command.bg = nord['base03']
## Foreground color of the statusbar in command mode.
c.colors.statusbar.command.fg = nord['base3']
## Background color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.bg = nord['base01']
## Foreground color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.fg = nord['base3']
## Background color of the statusbar in insert mode.
c.colors.statusbar.insert.bg = nord['green']
## Foreground color of the statusbar in insert mode.
c.colors.statusbar.insert.fg = nord['base3']
## Background color of the statusbar.
c.colors.statusbar.normal.bg = nord['base03']
## Foreground color of the statusbar.
c.colors.statusbar.normal.fg = nord['base3']
## Background color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.bg = nord['magenta']
## Foreground color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.fg = nord['base3']
## Background color of the statusbar in private browsing mode.
c.colors.statusbar.private.bg = nord['base01']
## Foreground color of the statusbar in private browsing mode.
c.colors.statusbar.private.fg = nord['base3']
## Background color of the progress bar.
c.colors.statusbar.progress.bg = nord['base3']
## Foreground color of the URL in the statusbar on error.
c.colors.statusbar.url.error.fg = nord['red']
## Default foreground color of the URL in the statusbar.
c.colors.statusbar.url.fg = nord['base3']
## Foreground color of the URL in the statusbar for hovered links.
c.colors.statusbar.url.hover.fg = nord['base2']
## Foreground color of the URL in the statusbar on successful load
## (http).
c.colors.statusbar.url.success.http.fg = nord['base3']
## Foreground color of the URL in the statusbar on successful load
## (https).
c.colors.statusbar.url.success.https.fg = nord['base3']
## Foreground color of the URL in the statusbar when there's a warning.
c.colors.statusbar.url.warn.fg = nord['yellow']
## Background color of the tab bar.
# c.colors.tabs.bar.bg = '#555555'
## Background color of unselected even tabs.
c.colors.tabs.even.bg = nord['base01']
## Foreground color of unselected even tabs.
c.colors.tabs.even.fg = nord['base2']
## Color for the tab indicator on errors.
c.colors.tabs.indicator.error = nord['red']
## Color gradient start for the tab indicator.
c.colors.tabs.indicator.start = nord['violet']
## Color gradient end for the tab indicator.
c.colors.tabs.indicator.stop = nord['orange']
## Background color of unselected odd tabs.
c.colors.tabs.odd.bg = nord['base01']
## Foreground color of unselected odd tabs.
c.colors.tabs.odd.fg = nord['base2']
## Background color of selected even tabs.
c.colors.tabs.selected.even.bg = nord['base03']
## Foreground color of selected even tabs.
c.colors.tabs.selected.even.fg = nord['base3']
## Background color of selected odd tabs.
c.colors.tabs.selected.odd.bg = nord['base03']
## Foreground color of selected odd tabs.
c.colors.tabs.selected.odd.fg = nord['base3']
