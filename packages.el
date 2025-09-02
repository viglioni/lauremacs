;;; packages.el --- Package manifest for Lauremacs
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

(defconst lauremacs-packages
  '((runtime-deps
     (general      :latest)
     (which-key    :latest)
     (l            :repo "viglioni/l-el"
                   :branch "latest-release")
     (helm         :latest)
     (solarized-theme :latest)
     (projectile   :latest))
    
    (dev-deps
     (buttercup    :latest)
     (package-lint :latest)))
  "Package dependencies organized by category with version constraints.")

;;; packages.el ends here
