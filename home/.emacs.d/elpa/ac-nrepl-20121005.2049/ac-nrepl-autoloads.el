;;; ac-nrepl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ac-nrepl-setup) "ac-nrepl" "ac-nrepl.el" (20591
;;;;;;  18310))
;;; Generated autoloads from ac-nrepl.el

(defface ac-nrepl-candidate-face '((t (:inherit ac-candidate-face))) "\
Face for nrepl candidates." :group (quote auto-complete))

(defface ac-nrepl-selection-face '((t (:inherit ac-selection-face))) "\
Face for the nrepl selected candidate." :group (quote auto-complete))

(defvar ac-source-nrepl-ns '((candidates . ac-nrepl-candidates-ns) (available . ac-nrepl-available-p) (candidate-face . ac-nrepl-candidate-face) (selection-face . ac-nrepl-selection-face) (prefix . ac-nrepl-symbol-start-pos) (symbol . "n") (document . ac-nrepl-documentation)) "\
Auto-complete source for nrepl ns completion.")

(defvar ac-source-nrepl-vars '((candidates . ac-nrepl-candidates-vars) (available . ac-nrepl-available-p) (candidate-face . ac-nrepl-candidate-face) (selection-face . ac-nrepl-selection-face) (prefix . ac-nrepl-symbol-start-pos) (symbol . "v") (document . ac-nrepl-documentation)) "\
Auto-complete source for nrepl var completion.")

(defvar ac-source-nrepl-ns-classes '((candidates . ac-nrepl-candidates-ns-classes) (available . ac-nrepl-available-p) (candidate-face . ac-nrepl-candidate-face) (selection-face . ac-nrepl-selection-face) (prefix . ac-nrepl-symbol-start-pos) (symbol . "c") (document . ac-nrepl-documentation)) "\
Auto-complete source for nrepl ns-specific class completion.")

(defvar ac-source-nrepl-all-classes '((candidates . ac-nrepl-candidates-all-classes) (available . ac-nrepl-available-p) (candidate-face . ac-nrepl-candidate-face) (selection-face . ac-nrepl-selection-face) (prefix . ac-nrepl-symbol-start-pos) (symbol . "c") (document . ac-nrepl-documentation)) "\
Auto-complete source for nrepl all class completion.")

(defvar ac-source-nrepl-java-methods '((candidates . ac-nrepl-candidates-java-methods) (available . ac-nrepl-available-p) (candidate-face . ac-nrepl-candidate-face) (selection-face . ac-nrepl-selection-face) (prefix . ac-nrepl-symbol-start-pos) (symbol . "m") (document . ac-nrepl-documentation) (action . ac-nrepl-delete-java-class-hint)) "\
Auto-complete source for nrepl java method completion.")

(defvar ac-source-nrepl-static-methods '((candidates . ac-nrepl-candidates-static-methods) (available . ac-nrepl-available-p) (candidate-face . ac-nrepl-candidate-face) (selection-face . ac-nrepl-selection-face) (prefix . ac-nrepl-symbol-start-pos) (symbol . "s") (document . ac-nrepl-documentation)) "\
Auto-complete source for nrepl java static method completion.")

(autoload 'ac-nrepl-setup "ac-nrepl" "\
Add the nrepl completion source to the front of `ac-sources'.
This affects only the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ac-nrepl-pkg.el") (20591 18311 63107))

;;;***

(provide 'ac-nrepl-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-nrepl-autoloads.el ends here
