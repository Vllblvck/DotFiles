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

# Key binds
config.bind(',v', 'hint links spawn mpv {hint-url}')

# Color schemes
nord = {
    # Polar Night
    'nord0': '#2e3440',
    'nord1': '#3b4252',
    'nord2': '#434c5e',
    'nord3': '#4c566a',
    # Snow Storm
    'nord4': '#d8dee9',
    'nord5': '#e5e9f0',
    'nord6': '#eceff4',
    # Frost
    'nord7': '#81A1C1',
    'nord8': '#88c0d0',
    'nord9': '#81a1c1',
    'nord10': '#5e81ac',
    # Aurora
    'nord11': '#bf616a',
    'nord12': '#d08770',
    'nord13': '#ebcb8b',
    'nord14': '#a3be8c',
    'nord15': '#b48ead',
}

gruvbox = {
    'base00': "#1d2021",
    'base01': "#3c3836",
    'base02': "#504945",
    'base03': "#665c54",
    'base04': "#bdae93",
    'base05': "#d5c4a1",
    'base06': "#ebdbb2",
    'base07': "#fbf1c7",
    'base08': "#fb4934",
    'base09': "#fe8019",
    'base0A': "#fabd2f",
    'base0B': "#b8bb26",
    'base0C': "#8ec07c",
    'base0D': "#83a598",
    'base0E': "#d3869b",
    'base0F': "#d65d0e",
}

#Set colors

# Nordic
c.colors.completion.category.bg = nord['nord0']
c.colors.completion.category.border.bottom = nord['nord0']
c.colors.completion.category.border.top = nord['nord0']
c.colors.completion.category.fg = nord['nord5']
c.colors.completion.even.bg = nord['nord1']
c.colors.completion.odd.bg = nord['nord1']
c.colors.completion.fg = nord['nord4']
c.colors.completion.item.selected.bg = nord['nord7']
c.colors.completion.item.selected.border.bottom = nord['nord3']
c.colors.completion.item.selected.border.top = nord['nord3']
c.colors.completion.item.selected.fg = nord['nord6']
c.colors.completion.match.fg = nord['nord13']
c.colors.completion.scrollbar.bg = nord['nord1']
c.colors.completion.scrollbar.fg = nord['nord5']
c.colors.downloads.bar.bg = nord['nord0']
c.colors.downloads.error.bg = nord['nord11']
c.colors.downloads.error.fg = nord['nord5']
c.colors.downloads.stop.bg = nord['nord15']
c.colors.downloads.system.bg = 'none'
c.colors.hints.bg = nord['nord13']
c.colors.hints.fg = nord['nord0']
c.colors.hints.match.fg = nord['nord10']
c.colors.keyhint.bg = nord['nord1']
c.colors.keyhint.fg = nord['nord5']
c.colors.keyhint.suffix.fg = nord['nord13']
c.colors.messages.error.bg = nord['nord11']
c.colors.messages.error.border = nord['nord11']
c.colors.messages.error.fg = nord['nord5']
c.colors.messages.info.bg = nord['nord8']
c.colors.messages.info.border = nord['nord8']
c.colors.messages.info.fg = nord['nord5']
c.colors.messages.warning.bg = nord['nord12']
c.colors.messages.warning.border = nord['nord12']
c.colors.messages.warning.fg = nord['nord5']
c.colors.prompts.bg = nord['nord2']
c.colors.prompts.border = '1px solid ' + nord['nord0']
c.colors.prompts.fg = nord['nord5']
c.colors.prompts.selected.bg = nord['nord3']
c.colors.statusbar.caret.bg = nord['nord15']
c.colors.statusbar.caret.fg = nord['nord5']
c.colors.statusbar.caret.selection.bg = nord['nord15']
c.colors.statusbar.caret.selection.fg = nord['nord5']
c.colors.statusbar.command.bg = nord['nord2']
c.colors.statusbar.command.fg = nord['nord5']
c.colors.statusbar.command.private.bg = nord['nord2']
c.colors.statusbar.command.private.fg = nord['nord5']
c.colors.statusbar.insert.bg = nord['nord14']
c.colors.statusbar.insert.fg = nord['nord1']
c.colors.statusbar.normal.bg = nord['nord0']
c.colors.statusbar.normal.fg = nord['nord5']
c.colors.statusbar.passthrough.bg = nord['nord10']
c.colors.statusbar.passthrough.fg = nord['nord5']
c.colors.statusbar.private.bg = nord['nord3']
c.colors.statusbar.private.fg = nord['nord5']
c.colors.statusbar.progress.bg = nord['nord5']
c.colors.statusbar.url.error.fg = nord['nord11']
c.colors.statusbar.url.fg = nord['nord5']
c.colors.statusbar.url.hover.fg = nord['nord8']
c.colors.statusbar.url.success.http.fg = nord['nord5']
c.colors.statusbar.url.success.https.fg = nord['nord14']
c.colors.statusbar.url.warn.fg = nord['nord12']
c.colors.tabs.bar.bg = nord['nord3']
c.colors.tabs.even.bg = nord['nord3']
c.colors.tabs.even.fg = nord['nord5']
c.colors.tabs.indicator.error = nord['nord11']
c.colors.tabs.indicator.system = 'none'
c.colors.tabs.odd.bg = nord['nord3']
c.colors.tabs.odd.fg = nord['nord5']
c.colors.tabs.selected.even.bg = nord['nord0']
c.colors.tabs.selected.even.fg = nord['nord5']
c.colors.tabs.selected.odd.bg = nord['nord0']
c.colors.tabs.selected.odd.fg = nord['nord5']

# #Gruvbox
# c.colors.completion.fg = gruvbox['base05']
# c.colors.completion.odd.bg = gruvbox['base03']
# c.colors.completion.even.bg = gruvbox['base00']
# c.colors.completion.category.fg = gruvbox['base0A']
# c.colors.completion.category.bg = gruvbox['base00']
# c.colors.completion.category.border.top = gruvbox['base00']
# c.colors.completion.category.border.bottom = gruvbox['base00']
# c.colors.completion.item.selected.fg = gruvbox['base01']
# c.colors.completion.item.selected.bg = gruvbox['base0A']
# c.colors.completion.item.selected.border.top = gruvbox['base0A']
# c.colors.completion.item.selected.border.bottom = gruvbox['base0A']
# c.colors.completion.item.selected.match.fg = gruvbox['base08']
# c.colors.completion.match.fg = gruvbox['base0B']
# c.colors.completion.scrollbar.fg = gruvbox['base05']
# c.colors.completion.scrollbar.bg = gruvbox['base00']
# c.colors.contextmenu.menu.bg = gruvbox['base00']
# c.colors.contextmenu.menu.fg =  gruvbox['base05']
# c.colors.contextmenu.selected.bg = gruvbox['base0A']
# c.colors.contextmenu.selected.fg = gruvbox['base01']
# c.colors.downloads.bar.bg = gruvbox['base00']
# c.colors.downloads.start.fg = gruvbox['base00']
# c.colors.downloads.start.bg = gruvbox['base0D']
# c.colors.downloads.stop.fg = gruvbox['base00']
# c.colors.downloads.stop.bg = gruvbox['base0C']
# c.colors.downloads.error.fg = gruvbox['base08']
# c.colors.hints.fg = gruvbox['base00']
# c.colors.hints.bg = gruvbox['base0A']
# c.colors.hints.match.fg = gruvbox['base05']
# c.colors.keyhint.fg = gruvbox['base05']
# c.colors.keyhint.suffix.fg = gruvbox['base05']
# c.colors.keyhint.bg = gruvbox['base00']
# c.colors.messages.error.fg = gruvbox['base00']
# c.colors.messages.error.bg = gruvbox['base08'] 
# c.colors.messages.error.border = gruvbox['base08'] 
# c.colors.messages.warning.fg = gruvbox['base00']
# c.colors.messages.warning.bg = gruvbox['base0E'] 
# c.colors.messages.warning.border = gruvbox['base0E'] 
# c.colors.messages.info.fg = gruvbox['base05']
# c.colors.messages.info.bg = gruvbox['base00']
# c.colors.messages.info.border = gruvbox['base00']
# c.colors.prompts.fg = gruvbox['base05']
# c.colors.prompts.border = gruvbox['base00']
# c.colors.prompts.bg = gruvbox['base00']
# c.colors.prompts.selected.bg = gruvbox['base0A']
# c.colors.statusbar.normal.fg = gruvbox['base0B']
# c.colors.statusbar.normal.bg = gruvbox['base00']
# c.colors.statusbar.insert.fg = gruvbox['base00']
# c.colors.statusbar.insert.bg = gruvbox['base0D']
# c.colors.statusbar.passthrough.fg = gruvbox['base00']
# c.colors.statusbar.passthrough.bg = gruvbox['base0C']
# c.colors.statusbar.private.fg = gruvbox['base00']
# c.colors.statusbar.private.bg = gruvbox['base03']
# c.colors.statusbar.command.fg = gruvbox['base05']
# c.colors.statusbar.command.bg = gruvbox['base00']
# c.colors.statusbar.command.private.fg = gruvbox['base05']
# c.colors.statusbar.command.private.bg = gruvbox['base00']
# c.colors.statusbar.caret.fg = gruvbox['base00']
# c.colors.statusbar.caret.bg = gruvbox['base0E']
# c.colors.statusbar.caret.selection.fg = gruvbox['base00']
# c.colors.statusbar.caret.selection.bg = gruvbox['base0D']
# c.colors.statusbar.progress.bg = gruvbox['base0D']
# c.colors.statusbar.url.fg = gruvbox['base05']
# c.colors.statusbar.url.error.fg = gruvbox['base08']
# c.colors.statusbar.url.hover.fg = gruvbox['base05']
# c.colors.statusbar.url.success.http.fg = gruvbox['base0C']
# c.colors.statusbar.url.success.https.fg = gruvbox['base0B']
# c.colors.statusbar.url.warn.fg = gruvbox['base0E']
# c.colors.tabs.bar.bg = gruvbox['base00']
# c.colors.tabs.indicator.start = gruvbox['base0D']
# c.colors.tabs.indicator.stop = gruvbox['base0C']
# c.colors.tabs.indicator.error = gruvbox['base08']
# c.colors.tabs.odd.fg = gruvbox['base05']
# c.colors.tabs.odd.bg = gruvbox['base03']
# c.colors.tabs.even.fg = gruvbox['base05']
# c.colors.tabs.even.bg = gruvbox['base00']
# c.colors.tabs.pinned.even.bg = gruvbox['base0C']
# c.colors.tabs.pinned.even.fg = gruvbox['base07']
# c.colors.tabs.pinned.odd.bg = gruvbox['base0B']
# c.colors.tabs.pinned.odd.fg = gruvbox['base07']
# c.colors.tabs.pinned.selected.even.bg = gruvbox['base05']
# c.colors.tabs.pinned.selected.even.fg = gruvbox['base00']
# c.colors.tabs.pinned.selected.odd.bg = gruvbox['base05']
# c.colors.tabs.pinned.selected.odd.fg = gruvbox['base0E']
# c.colors.tabs.selected.odd.fg = gruvbox['base00']
# c.colors.tabs.selected.odd.bg = gruvbox['base05']
# c.colors.tabs.selected.even.fg = gruvbox['base00']
# c.colors.tabs.selected.even.bg = gruvbox['base05']
# Background color for webpages if unset (or empty to use the theme's
# color).
# c.colors.webpage.bg = gruvbox['base00']
