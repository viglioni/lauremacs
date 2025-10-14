;;; packages.el --- Package manifest for Lauremacs -*- lexical-binding: t; l-syntax: t -*-
;;
;; Package versioning documentation:
;;
;; Packages are organized by category (runtime-deps, dev-deps) with version constraints.
;;
;; Version specification formats:
;;   (:latest
;;   (:latest)                 - Always use latest available version
;;   (:git-repo "user/repo"    - Git repository packages with branch/tag
;;    :branch "branch-name")
;;
;; Version constraints use semantic versioning operators:
;;   ">= version"  - Minimum version required
;;   "= version"   - Exact version match
;;   "latest"      - Latest available version
;;
;; MELPA versions use date format YYYYMMDD representing package build date.
;;

;;; Code:

(defun lauremacs-packages ()
  "Package dependencies organized by category with version constraints."
  '((runtime-deps
     (highlight-indentation :latest)
     (lsp-volar :type git :host github :repo "jadestrong/lsp-volar")
     (elixir-mode :latest)
     (vterm :latest)
     (claude-code :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                  :files ("*.el" (:exclude "images/*")))
     (web-mode :latest)
     (s :latest)
     (general :latest)
     (which-key :latest)
     (python-mode :latest)
     (envrc :latest)
     (typescript-mode :latest)
     (prettier-js :latest)
     (tree-sitter :latest)
     (tree-sitter-langs :latest)
     (vue-mode :latest)
     (l :repo "viglioni/l-el"
        :branch "latest-release")
     (org :built-in)
     (lsp-mode :latest)
     (lsp-ui :latest)
     (helm-lsp :latest)
     (lsp-treemacs :latest)
     (lsp-origami :latest)
     (lsp-haskell :latest)
     (lsp-tailwindcss :latest)
     (magit :source melpa)
     (org-roam :latest)
     (helm-posframe :repo "tumashu/helm-posframe")
     (avy :latest)
     (eval-sexp-fu :latest)
     (eat :latest)
     (smartparens :latest)
     (doom-modeline :latest)
     (nyan-mode :latest)
     (rainbow-delimiters :latest)
     (paren :latest)
     (yasnippet :latest)
     (yasnippet-snippets :latest)
     (iedit :latest)
     (flycheck :latest)
     (flyspell :latest)
     (company :latest)
     (expand-region :latest)
     (ace-window :latest)
     (neotree :latest)
     (exec-path-from-shell :latest)
     (helm :latest)
     (helm-swoop :latest)
     (helm-flx :latest)
     (helm-ag :latest)
     (multiple-cursors :latest)
     (helm-projectile :latest)
     (all-the-icons :latest)
     (gptel :latest)
     (lsp-pyright :latest)
     (spacemacs-theme :latest)
     (company-box :latest)
     (org-bullets :latest)
     (olivetti :latest)
     (org-download :latest)
     (valign :latest)
     (company-prescient :latest)
     (evil :latest)
     (projectile   :latest)
     (paredit :latest)
     )
    
    (dev-deps
     (buttercup    :latest)
     (package-lint :latest))))

;;; packages.el ends here
