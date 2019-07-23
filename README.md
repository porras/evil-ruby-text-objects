# evil-ruby-text-objects 

[![Build Status](https://travis-ci.com/porras/evil-ruby-text-objects.svg?branch=master)](https://travis-ci.com/porras/evil-ruby-text-objects)

[Emacs](https://www.gnu.org/software/emacs/) package that adds some text objects and keybindings to work with Ruby code with [Evil](https://github.com/emacs-evil/evil).

It's inspired by [vim-ruby](https://github.com/vim-ruby/vim-ruby/blob/96d5db458f868255393fdc2732d6bef21a45c68f/doc/ft-ruby-plugin.txt#L56-L76)'s `m` text objects and also [vim-textobj-ruby](https://github.com/rhysd/vim-textobj-ruby)'s `r`/`r*`, although it doesn't work exactly the same.

## Usage

`M-x evil-ruby-text-objects-mode` to enable. You can automatically enable it for `enh-ruby-mode` adding this line to your config:

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

## Note

This package use [`enh-ruby-mode`](https://github.com/zenspider/enhanced-ruby-mode) functionality to do all the hard work, that is, navigating the Ruby code. This means, a) that it is quite accurate when doing it, since it uses Ruby's parser¹, b) but **it won't work if you're using regular `ruby-mode`.**

¹ There is some _hacky_ code of my own for the inner variants and one line methods/classes/etc., _that_ might be not so accurate, please file an issue with an example if you see unexpected behaviour.
