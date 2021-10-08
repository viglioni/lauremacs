;;; prettier-js-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "prettier-js" "prettier-js.el" (0 0 0 0))
;;; Generated autoloads from prettier-js.el

(autoload 'prettier-js-mode "prettier-js" "\
Runs prettier on file save when this mode is turned on

If called interactively, enable Prettier-Js mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "prettier-js" '("prettier-js")))

;;;***

;;;### (autoloads nil nil ("prettier-js-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prettier-js-autoloads.el ends here
