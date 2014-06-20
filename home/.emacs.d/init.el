(defun init ()
  "Perform steps defined for emacs configuration"
  (init-basic)
  (init-coding)
  (init-clojure)
  (init-python)
  (init-c)
  (init-elisp)
  (init-rust))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic config

(defun init-basic ()
  "Setup some very basic functionality"
  (elpa-set-repos)
  (expect-packages '(evil evil-paredit expand-region markdown-mode zlc))
  (autosave-to-home)
  (extend-load-path)
  (use-utf-8)
  (configure-behaviour)
  (set-frame-look)
  (unclutter-emacs-window))

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

(defun expect-packages (expected)
  (package-initialize)
  (when (first-run-p)
    (package-refresh-contents))
  (dolist (package expected)
    (when (not (package-installed-p package))
      (package-install package))))

(defun first-run-p ()
  "Checks if this is a fresh install"
  (not (file-exists-p "~/.emacs.d/elpa")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path in .emacs.d

(defun extend-load-path ()
  "Put additional directories into load path"
  (make-directory "~/.emacs.d/site-lisp/" t)
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (let ((default-directory  "~/.emacs.d/site-lisp"))
    (normal-top-level-add-subdirs-to-load-path)))

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
  (setq locale-coding-system 'utf-8-unix)
  (setq terminal-coding-system 'utf-8-unix)
  (setq keyboard-coding-system 'utf-8-unix)
  (setq selection-coding-system 'utf-8-unix)
  (setq prefer-coding-system 'utf-8-unix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general look

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
  "Set font and theme for emacs frames"
  (load-theme 'wombat)
  (let ((font-name (first-available-font '("Source Code Pro Semibold"
                                           "Inconsolata"
                                           "DejaVu Sans Mono"
                                           "Monaco"
                                           "Consolas"
                                           "Courier New"))))
    (when font-name
      (let* ((font-size 10)
             (font (format "%s-%i" font-name font-size)))
        (add-to-list 'default-frame-alist `(font . ,font)))))
  (add-to-list 'default-frame-alist '(cursor-color . "red"))
  (when window-system
    (set-face-background 'highlight "#300000")
    (set-face-foreground 'highlight nil)
    (set-face-underline-p 'highlight nil)))

(defun first-available-font (font-name-list)
  "Returns first available font from the list"
  (let ((font-available-p (lambda (name)
                            (if (find-font (font-spec :name name)) name))))
    (car (delq nil (mapcar font-available-p font-name-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behaviour

(defun configure-behaviour ()
  "Configure certain aspects of emacs' behaviour"
  (require 'recentf)
  (setq scroll-step 1)
  (setq-default tab-width 8)
  (setq-default ident-tabs-mode nil)
  (setq-default require-final-newline t)
  (setq-default truncate-lines t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'after-init-hook 'recentf-mode)
  (add-hook 'after-init-hook 'enable-zlc)
  (add-hook 'after-init-hook 'reuse-buffer-in-dired)
  (add-hook 'after-init-hook 'side-with-evil)
  (configure-key-bindings))

(defun configure-key-bindings ()
  "Assign additional key bindings"
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C-x r C-f") 'recentf-open-files)
  (global-set-key (kbd "C-x n C-f") 'find-file-other-frame))

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
;; general programming

(defun init-coding ()
  "Enable things suitable for any programmning task"
  (expect-packages '(git-commit fill-column-indicator column-enforce-mode
                     yasnippet-bundle auto-complete xlicense))
  (require 'auto-complete-config)
  (add-hook 'prog-mode-hook 'configure-autocomplete)
  (add-hook 'prog-mode-hook 'configure-programming-look)
  (add-hook 'prog-mode-hook 'guess-indentation-type))

(defun configure-autocomplete ()
  "Enable AC mode"
  (yas/minor-mode-off)
  (add-completion-source ac-source-yasnippet)
  (setq ac-auto-show-menu t)
  (setq ac-quick-help-delay 0.5)
  (setq completion-at-point-functions '(auto-complete))
  (auto-complete-mode +1))

(defun add-completion-source (completion-source)
  (setq ac-sources (append (list completion-source) ac-sources)))

(defun configure-programming-look ()
  "Add colours and behaviour for programming mode"
  (progn
    (setq fci-rule-column 80)
    (setq fci-rule-color "gray30")
    (fci-mode -1))
  (column-enforce-mode))

(defun guess-indentation-type ()
  "Tries to guess whether tabs should be used in a file"
  (setq indent-tabs-mode (> (how-many "^\t+.*$") (how-many "^ .*$")))
  (add-hook 'before-save-hook (lambda ()
                                (if indent-tabs-mode
                                  (tabify (point-min) (point-max))
                                  (untabify (point-min) (point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure

(defun init-clojure ()
  (expect-packages '(paredit evil-paredit expand-region rainbow-delimiters
                     cider cljsbuild-mode ac-nrepl))
  (require 'rainbow-delimiters)
  (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-repl-mode))
  (eval-after-load "paredit" '(require 'evil-paredit))
  (add-hook 'paredit-mode-hook (lambda () (evil-paredit-mode +1)))
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojurescript-mode-hook (lambda () (run-hooks 'clojure-mode-hook)))
  (add-hook 'cider-mode-hook 'ac-nrepl-setup)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
  (add-hook 'cider-repl-mode-hook (lambda () (run-hooks 'clojure-mode-hook)))
  (add-hook 'cider-repl-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-lisp

(defun init-elisp ()
  (expect-packages '(paredit evil-paredit expand-region rainbow-delimiters))
  (require 'rainbow-delimiters)
  (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-repl-mode))
  (eval-after-load "paredit" '(require 'evil-paredit))
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python mode
;; source: http://www.emacswiki.org/emacs/PythonProgrammingInEmacs#toc5

(defun init-python ()
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

(defun init-c ()
  (expect-packages '(google-c-style auto-complete-clang))
  (require 'auto-complete-clang)
  (require 'google-c-style)
  (add-hook 'c-mode-hook (lambda () (c-set-style "linux")))
  (add-hook 'c++-mode-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-make-newline-indent)
  (add-hook 'c-mode-common-hook 'clang-completion))

(defun clang-completion ()
  "C/C++ code completion using clang compiler"
  (defun clang-completion-add-to-include-path (path)
    "Add `path' as a clang's `-I<path>' parameter"
    (interactive "DPath: ")
    (setq ac-clang-flags (cons (concat "-I" path) ac-clang-flags)))
  (setq ac-sources '(ac-source-yasnippet ac-source-clang)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust

(defun init-rust ()
  (expect-packages '(rust-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go!

(init)
