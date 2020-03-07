# Pullover
Texting while driving (other apps) is dangerous. Let's pull over there to edit the text in Emacs instead!

**Note**: This currently only works on macOS.

## Installation and Setup
- Check that Emacs was built with module support: `(functionp 'module-load)`.
- Add pullover ELPA to `package-archives` (remember to run `package-refresh-contents` afterwards):
    ```emacs-lisp
    (add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))
    ```
- Install `pullover`.
- Load the package and start the Emacs server.
    ```emacs-lisp
    (require 'pullover)
    (server-start)
    ```

If you want to build from source:
- Install `cargo` and `cask`.
- Run `./bin/package`.
- Look for the built package under `dist/`.

### Global Shortcut Configuration
- Assign a global shortcut to the wrapper script [pullover-start-or-finish](./pullover-start-or-finish), using an app like [Karabiner Elements](https://github.com/tekezo/Karabiner-Elements).
- Add the shortcut-handling app to the list in `System Preferences > Security & Privacy > Privacy > Accessibility`. Usually the macOS will ask for this on first use, but sometimes it won't. It's a good idea to add the app to the list in advance.

#### Karabiner Elements
My `karabiner.json` config looks similar to this:

```json
"profiles": [{
  "complex_modifications": {
    "rules": [
      {
        "manipulators": [
          {
            "type": "basic",
            "from": {"modifiers": {"mandatory": ["command", "control"]}, "key_code": "e"},
            "to": [{"shell_command": "~/.emacs.d/lib/pullover/pullover-start-or-finish"}]
          }
        ]
      }
    ]
  }
}]
```

The app to give Accessibility permissions is `karabiner_console_user_server`, not the main `Karabiner Elements` app itself. It is usually located at `/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_console_user_server`.

## Usage
- Press the global shortcut while inside another app to pull text from the current editing area over into Emacs.
- Edit the text as usual.
- After you are done, press the same shortcut (while inside Emacs) to end the editing session, sending the text back to the original app.
- To discard the edited text and return to the original app, use the command `pullover-cancel`, which overrides `kill-buffer` in pullover buffers.

<p align="center">
<kbd>
  <img src="./pullover.gif" alt="pullover illustration" />
</kbd>
</p>

## Customization
You can specify the major mode to use for editing.
```emacs-lisp
(use-package pullover
  :custom (pullover-major-mode 'gfm-mode)
  :config (server-start))
```

If your `Emacs.app` is installed in a non-standard location, you should customize `pullover-emacsclient-command`.
```emacs-lisp
(use-package pullover
  :custom
  (pullover-emacsclient-command "/Applications/MacPorts/EmacsMac.app/Contents/MacOS/bin/emacsclient"))
```

If the original app takes too long to copy text to the clipboard, the pullover buffer may not be populated correctly. You can give the app more time by configuring `pullover-clipboard-timeout`.
```emacs-lisp
(use-package pullover
  :custom (pullover-clipboard-timeout 200))
```
