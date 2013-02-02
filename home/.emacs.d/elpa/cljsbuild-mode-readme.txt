An Emacs minor mode for the ClojureScript 'lein cljsbuild' command
that will automatically watch the compilation buffer, pops it when the
compilation failed and (optionally) hides it when the compilation
succeed.

Installation:

Packages are available in the Marmalade and MELPA repositories.
Install the mode with "M-x package-install RET cljsbuild-mode".

Usage:

1. M-x cljsbuild-auto
2. Enjoy!

Alternatively, if you prefer to work from a terminal:

1. Start a terminal with M-x term or M-x multi-term
2. Run 'lein cljsbuild auto' in it
3. Start cljsbuild-mode in the terminal buffer with M-x cljsbuild-mode
