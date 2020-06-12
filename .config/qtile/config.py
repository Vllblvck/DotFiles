from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.lazy import lazy
from libqtile import layout, bar, widget

from typing import List


# General settings
GAPS = 10
MOD = "mod4"
GROUPS = ["WEB", "DEV", "FUN", "MUS", "CHAT", "MAIL", "TRNT"]

# Default apps
TERMINAL = "alacritty"
APP_LAUNCHER = "rofi -show drun -theme ~/.config/rofi/launcher.rasi"
BROWSER = "qutebrowser"
MUSIC_PLAYER = TERMINAL + " -e mocp"

# Key bindings
keys = [
   
    # Move between windows  
    Key([MOD], "k", lazy.layout.down()),
    Key([MOD], "j", lazy.layout.up()),
    Key([MOD], "h", lazy.layout.left()),
    Key([MOD], "l", lazy.layout.right()),

    # Move windows
    Key([MOD, "shift"], "k", lazy.layout.shuffle_down()),
    Key([MOD, "shift"], "j", lazy.layout.shuffle_up()),

    # Doing things with windows
    Key([MOD], "f", lazy.window.toggle_fullscreen()),
    Key([MOD, "shift"], "q", lazy.window.kill()),
    Key([MOD], "space", lazy.window.toggle_floating()),

    # Qtile specific
    Key([MOD, "shift"], "r", lazy.restart()),
    Key([MOD, "control"], "q", lazy.shutdown()),

    # Layouts movement
    Key([MOD], "Tab", lazy.next_layout()),
    Key([MOD, "shift"], "Tab", lazy.prev_layout()),
    
    # Apps launching
    Key([MOD], "Return", lazy.spawn(TERMINAL)),
    Key([MOD], "d", lazy.spawn(APP_LAUNCHER)),
    Key([MOD], "w", lazy.spawn(BROWSER)),
    Key([MOD], "m", lazy.spawn(MUSIC_PLAYER)),
]

# Groups
groups = [Group(name) for name in GROUPS]
for idx, name in enumerate(GROUPS, start=1):
    keys.extend([
        Key([MOD], str(idx), lazy.group[name].toscreen()),
        Key([MOD, "shift"], str(idx), lazy.window.togroup(name)),
    ])

# Layouts
layouts = [ 
    layout.MonadTall(margin=GAPS), # Main on the left and rest on the right
    layout.MonadWide(margin=GAPS), # Main on the top and rest on the bottom
    layout.Max(),
]


widget_defaults = dict(
    font='sans',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.CurrentLayout(),
                widget.GroupBox(),
                widget.Prompt(),
                widget.WindowName(),
                widget.TextBox("default config", name="default"),
                widget.Systray(),
                widget.Clock(format='%Y-%m-%d %a %I:%M %p'),
                widget.QuickExit(),
            ],
            24,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([MOD], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([MOD], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([MOD], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
