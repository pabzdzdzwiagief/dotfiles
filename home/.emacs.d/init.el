(defun init ()
  "Perform steps defined for emacs configuration"
  (autosave-to-home)
  (extend-load-path)
  (elpa-set-repos)
  (use-utf-8)
  (configure-behaviour)
  (set-theme)
  (unclutter-emacs-window)
  (clojure-mode-configuration)
  (c-mode-configuration)
  (emacs-lisp-mode-configuration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specify ELPA repositories

(defun elpa-set-repos ()
  "Set ELPA repositories available for use"
  (setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
			   ("elpa"      . "http://tromey.com/elpa/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")
			   ("melpa"     . "http://melpa.milkbox.net/packages/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path in .emacs.d

(defun extend-load-path ()
  "Put additional directories into load path"
  (make-directory "~/.emacs.d/site-lisp/" t)
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (let ((default-directory  "~/.emacs.d/site-lisp"))
    (normal-top-level-add-subdirs-to-load-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general programming

(defun general-programming ()
  "Enable things suitable for any programmning task"
  (require-programming-modes)
  (yas/minor-mode-off)
  (add-completion-source ac-source-yasnippet)
  (setq ac-auto-show-menu t)
  (setq ac-quick-help-delay 0.0)
  (auto-complete-mode +1)
  (rainbow-delimiters-mode +1))

(defun require-programming-modes ()
  "Require programming modes and autocompletion"
  (require 'auto-complete-config))

(defun add-completion-source (completion-source)
  (setq ac-sources (append (list completion-source) ac-sources)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure

(defun clojure-mode-configuration ()
  (add-hook 'clojure-mode-hook 'clojure-mode-utils)
  (add-hook 'clojure-mode-hook 'general-programming)
  (clojure-nrepl-utils))

(defun clojure-mode-utils ()
  "Enable several utilities useful in clojure-mode"
  (paredit-mode +1))

(defun clojure-nrepl-utils ()
  "Power up nREPL mode"
  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
  (add-hook 'nrepl-mode-hook 'clojure-mode-utils)
  (add-hook 'nrepl-mode-hook 'general-programming)
  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
  (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-lisp

(defun emacs-lisp-mode-configuration ()
  "Enable utils useful for messing with elisp"
  (add-hook 'emacs-lisp-mode-hook '(lambda ()
				     (paredit-mode +1)
				     (eldoc-mode +1)
				     (ac-emacs-lisp-mode-setup)))
  (add-hook 'emacs-lisp-mode-hook 'general-programming))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang

(defun c-mode-configuration()
  (add-hook 'c-mode-common-hook 'linux-c-mode)
  (add-hook 'c-mode-common-hook 'google-c++-mode)
  (add-hook 'c-mode-common-hook 'clang-completion)
  (add-hook 'c-mode-common-hook 'general-programming))

(defun linux-c-mode ()
  "linux coding style to complement google style"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utf-8
;; trick - http://gentoo-wiki.com/HOWTO_Make_your_system_use_unicode/utf-8#Editors

(defun use-utf-8 ()
  "Make emacs use UTF-8"
  (setq locale-coding-system 'utf-8)
  (setq terminal-coding-system 'utf-8)
  (setq keyboard-coding-system 'utf-8)
  (setq selection-coding-system 'utf-8)
  (setq prefer-coding-system 'utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; look
;; faces trick - http://superuser.com/questions/292880/emacs-as-daemon-on-os-x-with-window-system

(defun unclutter-emacs-window ()
  "Remove scrollbars, toolbars and such"
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

(defun set-theme ()
  "Set theme for window or tty frame"
  (add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)
  (add-hook 'after-init-hook (lambda ()
			       (run-after-make-frame-hooks (selected-frame))))
  (add-hook 'after-make-window-system-frame-hooks 'window-theme)
  (add-hook 'after-make-console-frame-hooks 'tty-theme))

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
   `after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
	       'after-make-window-system-frame-hooks
	       'after-make-console-frame-hooks)))

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun tty-theme ()
  "Hook for a new TTY frame theme"
  (load-theme 'wombat))

(defun window-theme ()
  "Hook for a new window-system-theme"
  (load-theme 'wombat)
  (set-face-font 'default "Source Code Pro Semibold-10"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behaviour

(defun configure-behaviour ()
  "Configure certain aspects of emacs' behaviour"
  (setq scroll-step 1)
  (setq default-tab-width 8)
  (setq ident-tabs-mode nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun always-use-horizontal-split ()
  "Make emacs always split horizontally"
  (setq split-height-threshold nil)
  (setq split-width-threshold 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go!

(init)
