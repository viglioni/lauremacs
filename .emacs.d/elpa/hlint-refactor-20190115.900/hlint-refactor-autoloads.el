;;; hlint-refactor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hlint-refactor" "hlint-refactor.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from hlint-refactor.el

(autoload 'hlint-refactor-refactor-buffer "hlint-refactor" "\
Apply all hlint suggestions in the current buffer.
ARGS specifies additional arguments that are passed to hlint.

\(fn &optional ARGS)" t nil)

(autoload 'hlint-refactor-refactor-at-point "hlint-refactor" "\
Apply the hlint suggestion at point." t nil)

(autoload 'hlint-refactor-mode "hlint-refactor" "\
Automatically apply hlint suggestions

If called interactively, enable Hlint-Refactor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hlint-refactor" '("hlint-refactor-call-process-region-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hlint-refactor-autoloads.el ends here
