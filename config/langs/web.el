;;; -*- lexical-binding: t; l-syntax: t -*-
(config-package typescript-mode
  :mode ( "\\.ts\\'" "\\.js\\'")
  :hook ((typescript-mode . lsp-deferred)
				 (typescript-mode . prettier-js-mode)         
				 (tsx-mode        . lsp-deferred)
				 (tsx-mode        . prettier-js-mode)
         ;; (typescript-mode . (lambda () (add-multiple-into-list 'prettify-symbols-alist
				 ;;  																						    '((">="  . "≥")
				 ;;  																							    ("<="  . "≤")
				 ;;  																							    ("!==" . "≠")
         ;;                                                    ("=>"  . "⇒")))
         )
  :custom
  (typescript-indent-level 2)
  
	:init
	(define-derived-mode tsx-mode typescript-mode "tsx")
	(add-hook 'tsx-mode #'subword-mode)
	(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))
  )

(config-package prettier-js
  :after (typescript-mode))

(config-package tree-sitter
  :ensure t
  :hook ((tsx-mode . tree-sitter-hl-mode)))

(config-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx)))

(config-package vue-mode
  :mode "\\.vue\\'"
  :hook ((vue-mode . lsp-deferred)
         (vue-mode . prettier-js-mode))
  :custom
  (vue-html-tab-width 2)
  (js-indent-level 2)
  (mmm-submode-decoration-level 0)
  (css-indent-offset 2)
  :config
  ;; Custom origami parser for Vue files
  (with-eval-after-load 'origami
    (defun origami-vue-parser (create)
      "Origami parser for Vue single-file components."
      (lambda (content)
        (let ((positions nil))
          (with-temp-buffer
            (insert content)
            (goto-char (point-min))
            ;; Find all <script>, <template>, and <style> blocks
            (while (re-search-forward "^<\\(script\\|template\\|style\\)[^>]*>" nil t)
              (let* ((tag (match-string 1))
                     (start (match-beginning 0))
                     (tag-end (point)))
                (when (re-search-forward (format "</%s>" tag) nil t)
                  (let ((end (match-end 0)))
                    (push (funcall create start end 0 nil) positions)))))
            (reverse positions)))))

    ;; Register the parser for vue-mode
    (add-to-list 'origami-parser-alist '(vue-mode . origami-vue-parser))))

(config-package web-mode
  :mode "\\.html$")
