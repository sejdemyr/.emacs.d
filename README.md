# My personal Emacs configuration #

This is a light-weight emacs configuration that includes ergonomic
key bindings and support for R, Python, JavaScript, SQL, LaTeX, and
Markdown (among others). Drawing on
[use-package](https://github.com/jwiegley/use-package), most
dependencies will be installed automatically (so no need to `M-x
package-install` anything from melpa).

## Setup ##

You need to install Emacs (currently v. 24). Homebrew provides [an
easy way to do this](http://wikemacs.org/wiki/Installing_Emacs_on_OS_X)
if you're on a mac.

All packages are installed automatically except the following, which
are easier to install directly from github:

1. [Emacs Speaks Statistics](http://ess.r-project.org/) (ESS). See the
site for download instructions; I install it into `emacs.d/ess/` like
so: `git clone https://github.com/emacs-ess/ESS.git ~/emacs.d/`.

2. [Color themes](https://github.com/owainlewis/emacs-color-themes). I
`git clone` this directory and run `install.sh`, which places the
themes in `.emacs.d/themes`. You can set the theme in
`custom-lisp/appearance.el`.


## Layout - Where do you find things? ##

The `init.el` file contains (mostly) `use-package` declarations for all
the built-in and 3rd party packages I use.  These declarations have all the
configuration and setup for the specific packages.

There’s also `custom-lisp/` which has my own custom functions, etc.
These libraries are loaded like normal packages with `use-package` in
`init.el`.

Thanks to [lunaryorn](https://github.com/lunaryorn/.emacs.d) for a
similar setup.


## License ##

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with GNU
Emacs; see the file COPYING.  If not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.