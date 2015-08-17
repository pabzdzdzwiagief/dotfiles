(defun init ()
  "Perform steps defined for emacs configuration"
  (init-packages)
  (init-text)
  (init-keys)
  (init-files)
  (init-looks)
  (init-coding))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELPA

(defun init-packages ()
  (packages-elpa-set-repos)
  (packages-expect-packages
   '(expand-region vlf
     markdown-mode fill-column-indicator
     ido-yes-or-no
     flycheck projectile flx-ido helm-projectile
     paredit rainbow-delimiters rainbow-blocks
     company company-statistics
     cider clj-refactor cljr-helm
     jedi company-jedi)))

(defun packages-elpa-set-repos ()
  "Set ELPA repositories available for use"
  (setq package-archives
	'(("gnu" . "http://elpa.gnu.org/packages/")
	  ("melpa-stable" . "http://stable.melpa.org/packages/"))))

(defun packages-expect-packages (expected)
  "Installs packages from the list"
  (package-initialize)
  (when (packages-first-run-p)
    (setq async-bytecomp-allowed-packages nil)
    (package-refresh-contents)
    (dolist (package expected)
      (when (not (package-installed-p package))
	(package-install package)))))

(defun packages-first-run-p ()
  "Returns true if this is a fresh install"
  (not (file-exists-p "~/.emacs.d/elpa")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text

(defun init-text ()
  (text-use-utf-8)
  (text-whitespace)
  (text-checkers)
  (text-encryption))

(defun text-use-utf-8 ()
  "Always use UTF-8
   see http://gentoo-wiki.com/HOWTO_Make_your_system_use_unicode/utf-8#Editors"
  (setq-default locale-coding-system 'utf-8-unix)
  (setq-default terminal-coding-system 'utf-8-unix)
  (setq-default keyboard-coding-system 'utf-8-unix)
  (setq-default selection-coding-system 'utf-8-unix)
  (setq-default prefer-coding-system 'utf-8-unix))

(defun text-whitespace ()
  "How whitespace in files should be handled"
  (setq-default tab-width 8)
  (setq-default ident-tabs-mode nil)
  (setq-default require-final-newline t)
  (setq-default truncate-lines t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun text-checkers ()
  (require 'fill-column-indicator)
  (setq fci-rule-column 80
	fci-rule-color "gray30")
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'prog-mode-hook 'fci-mode))

(defun text-encryption ()
  (setq epa-armor t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys

(defun init-keys ()
  (require 'ido)
  (require 'recentf)
  (require 'expand-region)
  (require 'helm)
  (require 'projectile)
  (add-hook 'after-init-hook 'ido-mode)
  (add-hook 'ido-setup-hook 'ido-yes-or-no-mode)
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C-x r C-f") 'recentf-open-files)
  (global-set-key (kbd "C-x n C-f") 'find-file-other-frame)
  (global-set-key (kbd "C-c C-h") 'helm-projectile)
  (global-set-key (kbd "C-c N") 'projectile-find-file)
  (global-set-key (kbd "C-c D") 'projectile-dired)
  (global-set-key (kbd "C-c F") 'projectile-grep)
  (global-set-key (kbd "C-c H") 'projectile-replace)
  (global-set-key (kbd "C-c S") 'projectile-save-project-buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

(defun init-files ()
  (require 'vlf)
  (require 'savehist)
  (require 'saveplace)
  (custom-set-variables
   '(vlf-application 'dont-ask))
  (custom-set-variables
   '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
   '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
  (make-directory "~/.emacs.d/autosaves/" t)
  (make-directory "~/.emacs.d/backups/" t)
  (setq make-backup-files nil)
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saveplace")
  (when (not (file-exists-p save-place-file))
    (write-region "" nil save-place-file))
  (setq savehist-file "~/.emacs.d/savehist")
  (setq recentf-save-file "~/.emacs.d/recentf"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Looks

(defun init-looks ()
  (looks-font)
  (looks-colors)
  (looks-window))

(defun looks-font ()
  (let ((font-name (looks-first-available-font '("Source Code Pro Semibold"
						 "Inconsolata"
						 "DejaVu Sans Mono"
						 "Monaco"
						 "Consolas"
						 "Courier New"))))
    (when font-name
      (let* ((font-size 10)
             (font (format "%s-%i" font-name font-size)))
        (add-to-list 'default-frame-alist `(font . ,font))))))

(defun looks-first-available-font (font-name-list)
  "Returns first available font from the list"
  (if (font-family-list)
      (let ((font-available-p (lambda (name)
				(if (find-font (font-spec :name name))
				    name))))
	(car (delq nil (mapcar font-available-p font-name-list))))
    ; `font-family-list' tends to be empty in daemon mode
    ; apparently there is no sane way to handle this in 24.4
    (car font-name-list)))

(defun looks-colors ()
  "Set font and theme for emacs frames"
  (load-theme 'wombat)
  (add-to-list 'default-frame-alist '(cursor-color . "red"))
  (set-face-background 'highlight "#305000")
  (set-face-foreground 'highlight nil)
  (set-face-underline 'highlight nil)
  (global-hl-line-mode t)
  (custom-set-variables
   '(color-theme-is-global nil))
  (add-hook 'prog-mode-hook 'looks-programming))

(defun looks-programming ()
  "Colors and behavior for programming mode"
  (require 'rainbow-delimiters)
  (rainbow-delimiters-mode +1))

(defun looks-window ()
  "Remove scrollbars, toolbars etc."
  (custom-set-variables
   '(inhibit-startup-screen t)
   '(tool-bar-mode nil)
   '(menu-bar-mode nil)
   '(scroll-bar-mode nil)
   '(show-paren-mode t)
   '(column-number-mode t)
   '(blink-cursor-mode nil))
  (set-fringe-mode '(0 . 0))
  (setq-default mode-line-format nil)
  (setq scroll-step 1
        scroll-conservatively 10000
        auto-window-vscroll nil)
  (add-hook 'after-init-hook 'looks-reuse-buffer-in-dired))

(defun looks-reuse-buffer-in-dired ()
  "Reuse current `dired' buffer when entering new directory with `a' key"
  (put 'dired-find-alternate-file 'disabled nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding

(defun init-coding ()
  (coding-generic)
  (coding-autocomplete)
  (coding-elisp)
  (coding-clojure)
  (coding-python)
  (coding-ipython)
  (coding-c))

(defun coding-generic ()
  "Enable things suitable for any programming task"
  (require 'flx-ido)
  (require 'projectile)
  (add-hook 'prog-mode-hook 'flx-ido-mode)
  (add-hook 'prog-mode-hook 'projectile-mode)
  (add-hook 'prog-mode-hook 'coding-autocomplete))

(defun coding-autocomplete ()
  "Enable autocompletion"
  (require 'company)
  (add-hook 'prog-mode-hook 'company-mode))

(defun coding-elisp ()
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(defun coding-clojure ()
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clojurescript-mode-hook (lambda () (run-hooks 'clojure-mode-hook)))
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook (lambda () (run-hooks 'clojure-mode-hook)))
  (add-hook 'cider-repl-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(defun coding-python ()
  (autoload 'jedi:setup "jedi" nil t)
  (add-hook 'python-mode-hook 'python-ipython)
  (add-hook 'python-mode-hook 'whitespace-mode)
  (setq jedi:complete-on-dot t))

(defun coding-ipython ()
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

(defun coding-c ()
  (add-hook 'c-mode-hook (lambda () (c-set-style "linux"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go!

(init)
