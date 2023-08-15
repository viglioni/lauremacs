;;; ts-repl.el --- Ts Repl  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Laura Viglioni

;; Author: Laura Viglioni <viglionilaura@gmail.com>
;; Maintainer: Laura Viglioni <viglionilaura@gmail.com>
;; Created: 04 Jul 2021
;; Keywords: keywods
;; URL: https://github.com/Viglioni/laurisp/tree/main/personal-libs/ts-repl
;; Version:  0.0.1
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary:
;; Not a repl per se.
;; Executes the file and outputs the result, i.e. console.logs with `ts-repl-exec-ts-buffer'
;; Executes the buffer and consoles the last sexp `ts-repl-send-last-sexp'
;; External requires: node with npx command, ts-node

;;; Code:

(require 'laurisp-core)
(require 'functional)

(defvar ts-repl--buffer-name "*TS-result*"
  "Output buffer of the TS execution.")

(defvar ts-repl--error-buffer-name "*TS-erros*"
  "Output buffer of the TS execution errors.")


;;;###autoload
(defun ts-repl--is-ts? (filename)
  "Check if FILENAME is a typescript file: *.ts or .tsx."
  (bool (string-match "\\.tsx?$" filename)))

;;;###autoload
(define-derived-mode ts-repl-mode typescript-mode "TS Repl"
  "A major mode for visualizing the output of a TS file")


;;;###autoload
(defun ts-repl--wrap-console (beginning end)
  "Wraps a console.log around BEGINNING and END region."
  (concat "\nconsole.log("
          (replace-regexp-in-string
           ";$" ""
           (buffer-substring beginning end))
          ")\n"))

;;;###autoload
(defun ts-repl--pulse (&optional sexp-beg sexp-end)
  "Pulses region: sexp region or marked SEXP-BEG SEXP-END region or the whole buffer."
  (pulse-momentary-highlight-region
   (or sexp-beg (and (use-region-p) (region-beginning)) (point-min))
   (or sexp-end (and (use-region-p) (region-end)) (point-max))))

;;;###autoload
(defun ts-repl--content (&optional sexp-beg sexp-end)
  "Gets the content of the buffer where the function is executed.
If a SEXP-BEG SEXP-END region is passed as arg or if a region is selected,
it is wrapped by a console.log statement and concatenated to the end"
  (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (sexp? (bool (and sexp-beg sexp-end)))
         (region-to-be-wrapped? (bool (or sexp? (use-region-p))))
         (beginning (if sexp? sexp-beg (region-beginning)))
         (end (if sexp? sexp-end (region-end)))
         (console-wrapped-content (if region-to-be-wrapped?
                                      (ts-repl--wrap-console beginning end)
                                    "")))
    (concat buffer-content console-wrapped-content)))

;;;###autoload
(defun ts-repl--run-ts (tmp-file)
  "Run the command that will execute the typescript content on a TMP-FILE."
  (projectile-run-async-shell-command-in-root
   (concat "echo \"-*-TS-Repl-start-*-\n\n\""
           " && npx ts-node -T " (join-path (lpwd) tmp-file)
           " && echo \"\n\""
           " || echo \"\n\n-*-An error has occurred-*-\""
           " && echo \"-*-TS-Repl-end-*-\"")
   ts-repl--buffer-name
   ts-repl--error-buffer-name))

;;;###autoload
(defun ts-repl--run-after-ts (tmp-file)
  "Commands that will run after the TS execution:
adds `ts-repl-mode' and deletes the TMP-FILE."
  (let* ((result-buff (get-buffer ts-repl--buffer-name))
         (proc (get-buffer-process result-buff)))
    (when (process-live-p proc)
      (set-process-sentinel
       proc
       #'(lambda (process signal)
           (with-current-buffer result-buff (funcall 'ts-repl-mode))
           (shell-command (concat "rm " tmp-file))
           (shell-command-sentinel process signal))))))

;;;###autoload
(defun ts-repl-send-last-sexp ()
  "Execute buffer without type verification for efficiency reasons.
Also consoles the last sexp around the cursor"
  (interactive)
  (let ((beginning (save-excursion
                     (backward-sexp)
                     (move-beginning-of-line nil)
                     (point)))
        (end (point)))
    (ts-repl-exec-ts-buffer beginning end)))

;;;###autoload
(defun ts-repl-exec-ts-buffer (&optional sexp-beg sexp-end)
  "Execute buffer without type verification for efficiency reasons.
Passes to `ts-repl--pulse' SEXP-BEG and SEXP-END to be colour pulsed."
  (interactive)
  (let* ((result-buff (get-buffer-create ts-repl--buffer-name))
         (ts-content (ts-repl--content sexp-beg sexp-end))
         (tmp-file (concat (make-temp-name "ts-repl") ".ts")))
    (throw-unless (ts-repl--is-ts? (buffer-file-name))
                  "this is not a typescript file!")
    (write-region ts-content nil tmp-file)
    (ts-repl--pulse sexp-beg sexp-end)
    (ts-repl--run-ts tmp-file)
    (ts-repl--run-after-ts tmp-file)))


(provide 'ts-repl)
;;; ts-repl.el ends here






