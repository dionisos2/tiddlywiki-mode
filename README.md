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

### With straight.el and use-package

```elisp
(use-package tiddlywiki-mode
  :straight (:host github :repo "dionisos2/tiddlywiki-mode")
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

### Manual

Clone this repository and add to your load path:

```elisp
(add-to-list 'load-path "/path/to/tiddlywiki-mode")
(require 'tiddlywiki-mode)
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
	:demand
	:straight nil
	:mode ("\\.tid\\'" . poly-tiddlywiki-mode))
```

This enables:
- Syntax highlighting using the language's major mode
- Proper indentation inside code blocks
- All editing features of the associated mode

**Note:** Code blocks must specify a language (e.g., ` ```python `) to activate the inner mode. Blocks without a language specifier will use the host mode's highlighting.

### Hook Filtering

By default, heavy mode hooks (LSP, eglot, flycheck, etc.) are filtered out to avoid starting heavy processes for each code block. Lightweight hooks (show-paren-mode, electric-pair-mode, etc.) run normally.

The default blacklist includes:
- `lsp`, `lsp-deferred`, `lsp-mode`
- `eglot-ensure`
- `flycheck-mode`, `flymake-mode`
- `company-mode`, `corfu-mode`
- `tree-sitter-mode`, `copilot-mode`
- And others

You can customize the blacklist:

```elisp
(setq tiddlywiki-code-block-hooks-blacklist
      '(lsp lsp-deferred eglot-ensure my-heavy-hook))
```

To run ALL hooks (including LSP), disable hook filtering entirely:

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
