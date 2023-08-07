# RMOO 1.3

The following documents how to get RMOO up and running for the complete beginner. If you're unfamiliar with Emacs, the conventions used may be a bit odd. Here's a quick rundown of how commands are presented:

When prefixed with a C-, that means hold down CTRL and the letter following C-. e.g. `C-w` means hold down CTRL while pressing w.

When prefixed with an M-, that means your meta key. Typically this is your Windows or Option key. e.g. `M-x` means to hold down meta while pressing x. (This will often be followed by a full string. M-x allows you to run commands by typing them in.)

### Installation
#### Pre-Emacs 29.1
The simplest way to install, in my opinion:
1. `git clone https://github.com/lisdude/rmoo.git ~/.emacs.d/rmoo`
2. `git clone https://github.com/atomontage/xterm-color.git ~/.emacs.d/xterm-color`
3. Add the following to your configuration file:
```
(add-to-list 'load-path "~/.emacs.d/xterm-color")
(add-to-list 'load-path "~/.emacs.d/rmoo")
(require 'rmoo-autoload)
(require 'moocode-mode)
(require 'coldc-mode)
(global-set-key (kbd "C-c C-r") 'rmoo)
(add-to-list 'auto-mode-alist '("\\.moo$" . moocode-mode))
(add-hook 'rmoo-interactive-mode-hooks (lambda ()
(linum-mode -1)                  ;; ... no line numbers
(goto-address-mode t)))          ;; ... clickable links
```

#### Post-Emacs 29.1
The latest version of Emacs bundles the very useful `use-package`,
and also deprecates `linum-mode`, so some changes seem like a good idea.

1. `git clone https://github.com/lisdude/rmoo.git ~/lib/emacs/rmoo`
2. (In Emacs) M-: `(package-install "xterm-color")`
3. Add the following to your configuration file:
```
(use-package rmoo-autoload
  :bind ("C-c C-r" . rmoo)
  :commands rmoo-worlds-add-new-moo
  :load-path "~/lib/emacs/rmoo"
  :init
  (add-hook 'rmoo-interactive-mode-hooks (lambda ()
                                           (display-line-numbers-mode -1) ;; ... no line numbers
                                           (goto-address-mode t)))) ;; ... clickable links
(use-package moocode-mode
  :mode "\\.moo$")
```

### World Management
#### Adding World
To add a world, type: `M-x rmoo-worlds-add-new-moo` (or press `C-c C-w C-a`)

You will then be prompted for the following:
- __World Name__ - The name of the MOO you're connecting to. e.g. `Miriani`
- __Site__ - The address of the MOO you're connecting to. e.g. `toastsoft.net`
- __Port__ - The port of the MOO you're connecting to. e.g. `1443`
- __TLS/SSL__ - If the MOO you're connecting to supports secure connections over TLS / SSL, say yes here.
- __Log File Path__ - The path of the file where all of the MOO's output will be saved.

Once the world is added, you'll probably want to save it for future connections. To save your world file, type: `M-x rmoo-worlds-save-worlds-to-file` (or `C-c C-w C-s`)

#### Connecting and Disconnecting
To connect to a world, type `M-x rmoo` (or `C-c C-r`)

To disconnect from a world, type `M-x rmoo-quit` (or `C-c C-q`)

### Editing Code
First, enable local editing inside your MOO: `@edito-o +local`

Once you @edit a verb, the screen will split in half with your verb code on one side and the MOO output on the other. For basic commands to use to manipulate windows, see the [Window Management](#Window-Management) section below. Here are some commands that will come in handy in the editor:

| Command   | Effect                                               |
| --------- | ---------------------------------------------------- |
| `C-c s`   | Send your code to the MOO.                           |
| `C-c c`   | Send your code to the MOO and close the editor pane. |
| `C-j`     | Add a newline and indent.                            |
| `C-c C-c` | Comment out the selection.                           |
| `C-c C-u` | Uncomment the selection.                             |
| `C-c ]`   | Kill editor buffer and close the frame.              |

### Commands and Keybindings
| Command                | Keybinding    | Effect                                                                                                                                                                                                              |
| ---------------------- | ------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `rmoo`                 | `C-c C-r`     | Open the world list to select a world to connect to.                                                                                                                                                                |
| `rmoo-quit`            | `C-c C-q`     | Disconnect from the current world.                                                                                                                                                                                  |
| `rmoo-scratch`         | `C-c C-s`     | Open a scratch buffer. Anything you enter in this buffer will get sent directly to the MOO. This is useful for pasting in long scripts or using the @paste command. Like the code editor, you can send with `C-c s` |
| `rmoo-@paste-kill`     | `C-c C-p`     | @paste whatever is in the 'kill ring'.                                                                                                                                                                              |
| `rmoo-set-linelength`  | `C-c C-l`     | Automatically set @linelength based on the size of your Emacs window.                                                                                                                                               |
| `rmoo-clear-input`     | `M-backspace` | Delete the contents of the command line and, if scrolled up, jump back to the command line.                                                                                                                         |
| `rmoo-jump-to-last-input` | `M-space` | Jump to the spot in the buffer where you last sent a command. |
| `rmoo-up-command`      | `Up Arrow`    | Recall command history. Can also be summoned with `Esc-p`                                                                                                                                                           |
| `rmoo-down-command`    | `Down Arrow`  | Same as up arrow, only opposite direction. Can also be summoned with `Esc-n`                                                                                                                                        |
| `rmoo-extras-get-verb` | `C-c C-v`     | Prompts for a verb name to edit in the local editor.                                                                                                                                                                |
| `rmoo-extras-get-prop` | `C-c C-p`     | Prompts for a property name to edit in the local editor.                                                                                                                                                            |
|                        |               |                                                                                                                                                                                                                     |

### Miscellaneous Settings
These are some settings you can put in your Emacs init file to enhance your rmoo experience. RMOO-specific settings can be configured from within Emacs by typing `M-x customize-group`, `rmoo`.

| Setting                                                   | Effect                                                                                                       |
| --------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| `(setq write-region-inhibit-fsync t)`                     | Disable fsync, which vastly speeds up writing to log files at the expense of safety in the event of a crash. |
| `(evil-set-initial-state 'rmoo-interactive-mode 'insert)` | Start MOOcode mode in insert mode when using evil.                                                           |
| `(evil-set-initial-state 'rmoo-scratch-mode 'insert)`     | Start MOO scratch buffers in insert mode.                                                                    |
| `(setq rmoo-connect-function 'socks-open-network-stream)` | Use a SOCKS proxy for connecting to MOOs.                                                                    |
| `(setq rmoo-mcp-record-unknown t)`                        | Create a buffer to record unrecognizable MCP data instead of just ignoring it.                               |
| `(setq rmoo-input-history-size 50)`                       | Set the number of lines of input history that RMOO will remember.                                            |
| `(setq rmoo-worlds-max-worlds 100)`                       | The maximum number of MOO worlds that can exist.                                                             |
| `(setq rmoo-send-always-goto-end t)`                      | When true, RMOO will always jump to the end of the buffer after sending a line.                              |
| `(setq rmoo-send-require-last-line t)`                    | When true, RMOO will only send lines on the last line of the buffer.                                         |
| `(setq rmoo-clear-local f)`                               | When false, RMOO will not clear any existing command history when reconnecting from an existing buffer.      |

### Window Management
| Command | Effect                                                                                         |
| ------- | ---------------------------------------------------------------------------------------------- |
| `C-x o` | Switches between open panes.                                                                   |
| `C-x 0` | Close the current pane.                                                                        |
| `C-x b` | Display a list of buffers to open in current pane. This is useful when editing multiple verbs. |
| `C-x 3` | Split the screen vertically. Typically followed by `C-x b` to open a buffer.                   |
| `C-x 2` | Split the screen horizontally.                                                                 |
| `C-x k` | Select a buffer to close entirely.                                                             |

### Changelog
__Version 1.3 (In Progress)__
- Add UTF-8 support.
- Add `M-space` to jump to your last command in the buffer.
- Add an option to retain command history across connections in the same buffer.
- Fix an issue where reconnecting from an existing buffer would duplicate the contents of that buffer, eventually leading to a crash after several reconnections.
- Add minimal support for ColdC worlds. (At the moment, this only affects which mode LambdaMOO-style local editing will invoke.)
- Update moocode-mode to add new types and built-in functions from ToastStunt.

__Version 1.2 (November 13, 2018)__
- Add 256-color ANSI support.
- Add support for SSL/TLS connections.
- Prevent backspace from deleting the prompt.
- Add logging capabilities.
- Add a scratch buffer, allowing you to execute arbitrary commands from a separate buffer. Useful for pasting @dump output, @paste, etc.
- Add a new keybinding to automatically set @linelength based on window size with `C-c C-l`
- Added more documentation.
- Replaced MCP 1.0 support with MCP 2.1 support. Implemented packages include:
    - mcp-negotiate
    - dns-org-mud-moo-simpleedit
    - dns-com-awns-status
    - dns-com-vmoo-client
- Made settings visible by adding an 'rmoo' group to `M-x customize-group`.

__Earlier Versions__
- See [CREDITS.txt](CREDITS.txt) for original changelogs and version control notes.

### Authors
[Ron Tapia](http://www.nmia.com/~tapia/) <[tapia@nmia.com](mailto:tapia@nmia.com)>, [Matthew Campbell](http://www.pobox.com/~mattcampbell/) <[mattcampbell@pobox.com](mailto:mattcampbell@pobox.com)>, [Todd Sundsted](https://github.com/toddsundsted), [lisdude](https://www.lisdude.com) <[lisdude@lisdude.com](mailto:lisdude@lisdude.com)>
