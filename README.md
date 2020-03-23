# evil-ruby-text-objects 

[![Build Status](https://github.com/porras/evil-ruby-text-objects/workflows/CI/badge.svg)](https://github.com/porras/evil-ruby-text-objects/actions?query=workflow%3ACI)
[![MELPA](https://melpa.org/packages/evil-ruby-text-objects-badge.svg)](https://melpa.org/#/evil-ruby-text-objects)
[![MELPA Stable](https://stable.melpa.org/packages/evil-ruby-text-objects-badge.svg)](https://stable.melpa.org/#/evil-ruby-text-objects)

[Emacs](https://www.gnu.org/software/emacs/) package that adds some text objects and keybindings to work with Ruby code with [Evil](https://github.com/emacs-evil/evil).

It's inspired by [vim-ruby](https://github.com/vim-ruby/vim-ruby/blob/96d5db458f868255393fdc2732d6bef21a45c68f/doc/ft-ruby-plugin.txt#L56-L76)'s `m` text objects and also [vim-textobj-ruby](https://github.com/rhysd/vim-textobj-ruby)'s `r`/`r*`, although it doesn't work exactly the same.

## Installation

You can install `evil-ruby-text-objects` from [MELPA](https://melpa.org/) with the following command:

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `evil-ruby-text-objects` <kbd>[RET]</kbd>

## Usage

`M-x evil-ruby-text-objects-mode` to enable. You can automatically enable it for all ruby files by adding this line to your config:

```elisp
(add-hook 'ruby-mode-hook 'evil-ruby-text-objects-mode)
```

Or, if you use `enh-ruby-mode`:

```elisp
(add-hook 'enh-ruby-mode-hook 'evil-ruby-text-objects-mode)
```

## Keybindings

| Keybinding | Object                                                   |
|------------|----------------------------------------------------------|
| `rm` `m`   | Method                                                   |
| `rc`       | Class                                                    |
| `rM`       | Module                                                   |
| `rn`       | Namespace (either a class or a module)                   |
| `rg`       | `begin`...`end` block                                    |
| `ri`       | Conditional statement (either `if`, `unless`, or `case`) |
| `rb`       | `do`...`end` block, with or without arguments            |

All these objects support both _outer_ (`a`) and _inner_ (`i`) variants, and also an optional _count_ argument (this doesn't make sense for the method, but it does for all others).

## Examples

Assuming this code (`#_` represents the position of the cursor):

```ruby
module A
  class B
    def m(args)
      something#_
    end
  end
end
```

* `dam` deletes the whole method.
* `cim` deletes the method implementation (just one line in this case) and sets you in insert mode to type a new one.
* `yarc` copies the whole class `B`.
* `v2arn` switches to visual mode selecting the whole namespace 2 levels up (that is, the `A` module).

All other objects work similarly.

## Note about Ruby modes

This package supports both the built-in `ruby-mode` and [`enh-ruby-mode`](https://github.com/zenspider/enhanced-ruby-mode). The implementation differs slightly so if you notice some unexpected behaviour, please file an issue with an example, mentioning which of both Ruby modes you are using.

## Crystal support

This mode can be used to some extent with [Crystal](https://crystal-lang.org/) code, and possibly any other language with syntax similar enough to Ruby, although it can act funny in places where the syntax differs from Ruby, like for example `abstract class`/`def`. Complete support is not a goal of this project, but the basic stuff should work. Please file an issue if you find basic behavior that is not working.

In order to activate it automatically, add this to your config:

```elisp
(add-hook 'crystal-mode-hook 'evil-ruby-text-objects-mode)
```
