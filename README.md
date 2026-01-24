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
  :config
  (setq tiddlywiki-wiki-alist
        '(("my-wiki" . "~/path/to/wiki/tiddlers")
          ("other-wiki" . "~/path/to/other/tiddlers")))
  (setq tiddlywiki-default-wiki "my-wiki"))
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

## Key Bindings

| Key       | Command                     | Description                          |
|-----------|-----------------------------|--------------------------------------|
| `C-c C-o` | `tiddlywiki-open-tiddler`   | Open a tiddler with completion       |
| `C-c C-s` | `tiddlywiki-grep`           | Search in tiddlers (requires consult)|
| `C-c C-n` | `tiddlywiki-new-tiddler`    | Create a new tiddler                 |
| `C-c C-w` | `tiddlywiki-select-wiki`    | Select active wiki                   |
| `C-c C-h` | `tiddlywiki-toggle-header`  | Toggle header visibility             |
| `C-c C-t` | `tiddlywiki-add-tag`        | Add a tag to current tiddler         |

Use `C-u` prefix with navigation commands to select a different wiki.

## Dependencies

- Emacs 27.1 or later
- [consult](https://github.com/minad/consult) (optional, for `tiddlywiki-grep`)

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
