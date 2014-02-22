(defun init ()
  "Perform steps defined for emacs configuration"
  (when (first-run-p)
      (first-run-install))
  (autosave-to-home)
  (extend-load-path)
  (elpa-set-repos)
  (use-utf-8)
  (configure-behaviour)
  (set-frame-look)
  (unclutter-emacs-window)
  (general-programming)
  (clojure-mode-configuration)
  (python-mode-configuration)
  (c-mode-configuration)
  (emacs-lisp-mode-configuration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specify ELPA repositories

(defun elpa-set-repos ()
  "Set ELPA repositories available for use"
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("elpa" . "http://tromey.com/elpa/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ELPA packages

(defun first-run-p ()
  "Checks if this is a fresh install"
  (not (file-exists-p "~/.emacs.d/elpa")))

(defun first-run-install ()
  "Install non-standard ELPA packages"
  (package-initialize)
  (package-refresh-contents)
  (dolist (p '(evil
               paredit
               rainbow-delimiters
               fill-column-indicator
               column-enforce-mode
               git-commit
               markdown-mode
               rust-mode
               cider
               clojurescript-mode
               cljsbuild-mode
               google-c-style
               auto-complete
               auto-complete-clang
               ac-nrepl
               yasnippet-bundle
               xlicense
               zlc))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path in .emacs.d

(defun extend-load-path ()
  "Put additional directories into load path"
  (make-directory "~/.emacs.d/site-lisp/" t)
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (let ((default-directory  "~/.emacs.d/site-lisp"))
    (normal-top-level-add-subdirs-to-load-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general programming

(defun general-programming ()
  "Enable things suitable for any programmning task"
  (add-hook 'prog-mode-hook 'configure-autocomplete)
  (add-hook 'prog-mode-hook 'configure-programming-look)
  (add-hook 'prog-mode-hook 'guess-indentation-type))

(defun configure-autocomplete ()
  "Enable AC mode"
  (require 'auto-complete-config)
  (yas/minor-mode-off)
  (add-completion-source ac-source-yasnippet)
  (setq ac-auto-show-menu t)
  (setq ac-quick-help-delay 0.0)
  (setq completion-at-point-functions '(auto-complete))
  (auto-complete-mode +1))

(defun add-completion-source (completion-source)
  (setq ac-sources (append (list completion-source) ac-sources)))

(defun configure-programming-look ()
  "Add colours and behaviour for programming mode"
  (rainbow-delimiters-mode +1)
  (progn
    (setq fci-rule-column 80)
    (setq fci-rule-color "gray30")
    (fci-mode +1))
  (progn
    (column-enforce-mode)
    (80-column-rule)))

(defun guess-indentation-type ()
  "Tries to guess whether tabs should be used in a file"
  (setq indent-tabs-mode (> (how-many "^\t+.*$") (how-many "^ .*$")))
  (add-hook 'before-save-hook (lambda ()
                                (when indent-tabs-mode
                                  (untabify (point-min) (point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure

(defun clojure-mode-configuration ()
  (add-hook 'clojure-mode-hook 'clojure-mode-utils)
  (add-hook 'clojurescript-mode-hook 'clojure-mode-utils)
  (clojure-nrepl-utils))

(defun clojure-mode-utils ()
  "Enable several utilities useful in clojure-mode"
  (rainbow-delimiters-mode +1)
  (paredit-mode +1))

(defun clojure-nrepl-utils ()
  "Power nREPL mode up"
  (add-hook 'cider-mode-hook 'ac-nrepl-setup)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
  (add-hook 'cider-repl-mode-hook 'clojure-mode-utils)
  (add-hook 'cider-repl-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-repl-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-lisp

(defun emacs-lisp-mode-configuration ()
  "Enable utils useful for messing with elisp"
  (add-hook 'emacs-lisp-mode-hook 'configure-elisp-look)
  (add-hook 'emacs-lisp-mode-hook 'configure-elisp-autocomplete))

(defun configure-elisp-look ()
  "Enable paredit and eldoc"
  (rainbow-delimiters-mode +1)
  (paredit-mode +1)
  (eldoc-mode +1))

(defun configure-elisp-autocomplete ()
  "Enable AC for elisp"
  (ac-emacs-lisp-mode-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python mode
;; source: http://www.emacswiki.org/emacs/PythonProgrammingInEmacs#toc5

(defun python-mode-configuration ()
  "Configure python mode"
  (add-hook 'python-mode-hook 'python-ipython))

(defun python-ipython ()
  "Use ipython3 in python-shell"
  (setq
     python-shell-interpreter "ipython3"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang

(defun c-mode-configuration ()
  (add-hook 'c-mode-hook 'linux-c-mode)
  (add-hook 'c++-mode-hook 'google-c++-mode)
  (add-hook 'c-mode-common-hook 'clang-completion))

(defun linux-c-mode ()
  "linux coding style for pure C"
  (c-set-style "linux"))

(defun google-c++-mode ()
  "C++ mode with Goole coding style imposed"
  (require 'google-c-style)
  (google-set-c-style)
  (google-make-newline-indent))

(defun clang-completion ()
  "C/C++ code completion using clang compiler"
  (require 'auto-complete-clang)
  (setq ac-sources '(ac-source-yasnippet ac-source-clang)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autosave
;; trick - http://snarfed.org/gnu_emacs_backup_files

(defun autosave-to-home ()
  "Make emacs put autosave files (ie #foo#) and backup files (ie foo~) in
   ~/.emacs.d/ and create the autosave dir if necessary, since emacs won't"
  (custom-set-variables
   '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
   '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
  (make-directory "~/.emacs.d/autosaves/" t)
  (make-directory "~/.emacs.d/backups/" t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utf-8
;; trick - http://gentoo-wiki.com/HOWTO_Make_your_system_use_unicode/utf-8#Editors

(defun use-utf-8 ()
  "Make emacs use UTF-8"
  (setq locale-coding-system 'utf-8)
  (setq terminal-coding-system 'utf-8)
  (setq keyboard-coding-system 'utf-8)
  (setq selection-coding-system 'utf-8)
  (setq prefer-coding-system 'utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; look

(defun unclutter-emacs-window ()
  "Remove scrollbars, toolbars etc."
  (custom-set-variables
   '(color-theme-is-global nil)
   '(inhibit-startup-screen t)
   '(tool-bar-mode nil)
   '(menu-bar-mode nil)
   '(scroll-bar-mode nil)
   '(show-paren-mode t)
   '(column-number-mode t)
   '(blink-cursor-mode nil))
  (global-hl-line-mode t))

(defun set-frame-look ()
  "Set font and theme for emacs frame"
  (add-to-list 'default-frame-alist '(font . "Source Code Pro Semibold-10"))
  (load-theme 'wombat)
  (set-cursor-color "red")
  (set-face-underline-p 'highlight nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behaviour

(defun configure-behaviour ()
  "Configure certain aspects of emacs' behaviour"
  (setq scroll-step 1)
  (setq default-tab-width 8)
  (setq ident-tabs-mode nil)
  (setq require-final-newline t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'after-init-hook 'enable-zlc)
  (add-hook 'after-init-hook 'reuse-buffer-in-dired)
  (add-hook 'after-init-hook 'side-with-evil))

(defun always-use-horizontal-split ()
  "Make emacs always split horizontally"
  (setq split-height-threshold nil)
  (setq split-width-threshold 0))

(defun enable-zlc ()
  "Enable zsh-like menu completion"
  (zlc-mode t))

(defun reuse-buffer-in-dired ()
  "Reuse current `dired' buffer when entering new directory with `a' key"
  (put 'dired-find-alternate-file 'disabled nil))

(defun side-with-evil ()
  "Introduce some VI-like behaviour to emacs"
  (setq evil-default-cursor t)
  (evil-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go!

(init)
