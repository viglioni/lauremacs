;;; -*- lexical-binding: t; l-syntax: t -*-
;;
;; Emacs Lisp
;;

(require 'eval-sexp-fu)
(add-hook 'emacs-lisp-mode-hook 'eval-sexp-fu-flash-mode)

;;;###autoload
(defun lauremacs-ielm-eval ()
  "Run last sexp on IELM."
  (interactive)
  (let* ((beginning (save-excursion
                     (backward-sexp)
                     (move-beginning-of-line nil)
                     (point)))
         (end (point))
         (cmd (buffer-substring-no-properties beginning end))
         (current-window (car (window-list))))
    (funcall (lauremacs-pop-shell 'ielm))
    (with-current-buffer "*ielm*"
      (insert cmd)
      (ielm-send-input))
    (select-window current-window)))

(lauremacs-major-mode-leader
  :keymaps 'emacs-lisp-mode-map
  "="  '(nil :which-key "format")
  ;; "=." '(lauremacs-align-assoc-list   :which-key "align assoc list")
  ;; "==" '(lauremacs/buffer-indent :which-key "indent buffer")
  "=d" '(checkdoc :which-key "checkdoc")
;;  "=g" '(lauremacs-align-general-sexp	:which-key "format codeblocks of general.el functions")
  "=r" '(indent-region :which-key "indent region")
  "c"  '(nil :which-key "compile")
  "cc" '(emacs-lisp-byte-compile :which-key "byte compile")
  "d"  '(nil :which-key "documentation")
  ;; "dC" '(make-box-comment-region			:which-key "make box comment region")
  ;; "dc" '(make-box-comment :which-key "make box comment")
  ;; "dd" '(make-divider :which-key "make divider")
  ;; "dh" '(make-header :which-key "make lib header")
  ;; "dr" '(make-revision :which-key "make revision")
  ;; "du" '(update-file-header :which-key "update lib header")
  "e"  '(nil :which-key "eval")
  "eb" '(eval-buffer :which-key "eval buffer")
  "er" '(eval-region :which-key "eval region")
  "ei" '(lauremacs-ielm-eval :which-key "run on ielm")
  )


(config-package paredit
  :hook
  ((emacs-lisp-mode                  . enable-paredit-mode)
   (eval-expression-minibuffer-setup . enable-paredit-mode)
   (lisp-interaction-mode            . enable-paredit-mode)
   (lisp-mode                        . enable-paredit-mode)
   ;; (minibuffer-exit                  . my/restore-paredit-key)
   (minibuffer-setup                 . disable-paredit-mode)
   ;; (minibuffer-setup                 . my/conditionally-enable-paredit-mode)
   (eshell-mode                      . (lambda ()
                                         (define-key eshell-mode-map
                                                     (kbd "<return>")
                                                     'eshell-send-input))))
  :config
  (defvar my/paredit-minibuffer-commands '(eval-expression
                                           ielm-eval-input
                                           pp-eval-expression
                                           eval-expression-with-eldoc
                                           ibuffer-do-eval
                                           ibuffer-do-view-and-eval
                                           org-ql-sparse-tree
                                           org-ql-search)
    "Interactive commands for which paredit should be enabled in the minibuffer.")

  ;; (defun my/conditionally-enable-paredit-mode ()
  ;;   "Enable paredit during lisp-related minibuffer commands."
  ;;   (when (memq this-command my/paredit-minibuffer-commands)
  ;;     (enable-paredit-mode)
  ;;     (unbind-key (kbd "RET") paredit-mode-map)))

  ;; (defun my/restore-paredit-key ()
  ;;   "Restore the RET binding that was disabled by
  ;; my/conditionally-enable-paredit-mode."
  ;;   (bind-key (kbd "RET") #'paredit-newline paredit-mode-map))
  )
