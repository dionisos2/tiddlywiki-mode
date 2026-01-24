# tiddlywiki-mode

A major mode for Emacs to edit TiddlyWiki `.tid` files.

## Features

- **Syntax highlighting** for TiddlyWiki wikitext markup:
  - Headings (`!`, `!!`, `!!!`, etc.)
  - Text formatting: bold (`''text''`), italic (`//text//`), underline (`__text__`), strikethrough (`~~text~~`), superscript (`^^text^^`), subscript (`,,text,,`)
  - Inline code (`` `code` ``) and code blocks (` ``` `)
  - Links (`[[link]]`, `[[display|link]]`)
  - Transclusions (`{{tiddler}}`)
  - Macros (`<<macro>>`)
  - Widgets (`<$widget>`)
  - Lists (`*`, `#`, `-`)
  - Definition lists (`;`, `:`)

- **Header management**:
  - Parse and modify `.tid` file headers
  - Automatic `modified` timestamp update on save
  - Toggle header visibility (narrowing)

- **Multi-wiki support**:
  - Configure multiple wikis
  - Switch between wikis easily

- **Navigation**:
  - Open tiddlers with completion
  - Search in tiddlers with `consult-ripgrep`
  - Create new tiddlers

## Installation

### Manual

Clone this repository and add to your load path:

```elisp
(add-to-list 'load-path "/path/to/tiddlywiki-mode")
(require 'tiddlywiki-mode)
```

### use-package

```elisp
(use-package tiddlywiki-mode
  :load-path "/path/to/tiddlywiki-mode"
  :mode "\\.tid\\'"
  :bind (:map tiddlywiki-mode-map
         ("C-c C-o" . tiddlywiki-open-tiddler)
         ("C-c C-s" . tiddlywiki-grep)
         ("C-c C-n" . tiddlywiki-new-tiddler)
         ("C-c C-w" . tiddlywiki-select-wiki)
         ("C-c C-h" . tiddlywiki-toggle-header)
         ("C-c C-t" . tiddlywiki-add-tag)
         ("C-c C-r" . tiddlywiki-remove-tag))
  :custom
  (tiddlywiki-wiki-alist
   '(("my-wiki" . "~/path/to/wiki/tiddlers")
     ("other-wiki" . "~/path/to/other/tiddlers")))
  (tiddlywiki-default-wiki "my-wiki"))
```

## Configuration

### Wiki list

Configure your wikis in `tiddlywiki-wiki-alist`:

```elisp
(setq tiddlywiki-wiki-alist
      '(("personal" . "~/wikis/personal/tiddlers")
        ("work" . "~/wikis/work/tiddlers")
        ("notes" . "~/wikis/notes/tiddlers")))
```

### Default wiki

Set a default wiki:

```elisp
(setq tiddlywiki-default-wiki "personal")
```

### Disable automatic timestamp update

```elisp
(setq tiddlywiki-update-modified-on-save nil)
```

## Suggested Key Bindings

The mode does not define default keybindings. Here are the suggested bindings (included in the use-package example above):

| Key       | Command                     | Description                          |
|-----------|-----------------------------|--------------------------------------|
| `C-c C-o` | `tiddlywiki-open-tiddler`   | Open a tiddler with completion       |
| `C-c C-s` | `tiddlywiki-grep`           | Search in tiddlers (requires consult)|
| `C-c C-n` | `tiddlywiki-new-tiddler`    | Create a new tiddler                 |
| `C-c C-w` | `tiddlywiki-select-wiki`    | Select active wiki                   |
| `C-c C-h` | `tiddlywiki-toggle-header`  | Toggle header visibility             |
| `C-c C-t` | `tiddlywiki-add-tag`        | Add a tag to current tiddler         |
| `C-c C-r` | `tiddlywiki-remove-tag`     | Remove a tag from current tiddler    |

Use `C-u` prefix with navigation commands to select a different wiki.

## Code Block Support (Polymode)

For proper syntax highlighting and indentation inside code blocks, you can use the polymode integration:

```elisp
(use-package tiddlywiki-polymode
  :load-path "/path/to/tiddlywiki-mode"
  :mode ("\\.tid\\'" . poly-tiddlywiki-mode))
```

This enables:
- Syntax highlighting using the language's major mode
- Proper indentation inside code blocks
- All editing features of the associated mode

By default, mode hooks are **not** run in code blocks to avoid starting heavy processes like LSP. If you want full mode functionality:

```elisp
(setq tiddlywiki-code-block-run-hooks t)
```

### Language Mapping

Languages are automatically mapped to modes (e.g., `python` → `python-mode`). You can customize this:

```elisp
(add-to-list 'tiddlywiki-language-mode-alist '("my-lang" . my-lang-mode))
```

## Dependencies

- Emacs 27.1 or later
- [consult](https://github.com/minad/consult) (optional, for `tiddlywiki-grep`)
- [polymode](https://github.com/polymode/polymode) (optional, for code block support)

## Development

### Running tests

```bash
make test
```

Or with Eldev:

```bash
eldev test
```

### Byte-compilation

```bash
make compile
```

## License

GPL-3.0-or-later

## Contributing

Contributions are welcome! Please open an issue or pull request.
