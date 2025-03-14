;;; release.el --- Script for releasing new versions -*- lexical-binding: t; -*-

(require 'subr-x)

(defun release-version (increment-type)
  "Release a new version, incrementing by INCREMENT-TYPE (major, minor, or patch)."
  (interactive (list (completing-read "Increment type: " '("major" "minor" "patch"))))
  
  ;; Get current version from git tag
  (let* ((current-version (string-trim (shell-command-to-string "git describe --tags --abbrev=0")))
         (version-parts (mapcar #'string-to-number (split-string current-version "\\.")))
         (major (nth 0 version-parts))
         (minor (nth 1 version-parts))
         (patch (nth 2 version-parts))
         new-version)
    
    ;; Calculate new version
    (cond
     ((string= increment-type "major")
      (setq new-version (format "%d.0.0" (1+ major))))
     ((string= increment-type "minor")
      (setq new-version (format "%d.%d.0" major (1+ minor))))
     ((string= increment-type "patch")
      (setq new-version (format "%d.%d.%d" major minor (1+ patch))))
     (t (error "Unknown increment type: %s" increment-type)))
    
    ;; Confirm versions
    (unless (yes-or-no-p (format "Confirm current version: %s?" current-version))
      (user-error "Release aborted"))
    
    (unless (yes-or-no-p (format "Confirm new version: %s?" new-version))
      (user-error "Release aborted"))
    
    (message "Preparing release: %s -> %s" current-version new-version)
    
    ;; Update "since NEXT" to new version in all .el files
    (dolist (file (directory-files-recursively default-directory "\\.el$"))
      (with-temp-buffer
        (insert-file-contents file)
        (when (search-forward ";; since NEXT" nil t)
          (replace-match (format ";; since %s" new-version))
          (write-region (point-min) (point-max) file)
          (message "Updated %s" file))))
    
    ;; Update changelog
    (let ((changelog-file (if (file-exists-p "CHANGELOG.org") 
                              "CHANGELOG.org"
                            "CHANGELOG.md"))
          (today (format-time-string "%Y-%m-%d"))
          (unreleased-header (if (file-exists-p "CHANGELOG.org") 
                                "* Unreleased"
                              "## [Unreleased]")))
      
      (with-temp-buffer
        (insert-file-contents changelog-file)
        
        ;; Replace Unreleased with new version
        (goto-char (point-min))
        (when (re-search-forward (if (file-exists-p "CHANGELOG.org") 
                                    "\\* Unreleased"
                                  "## \\[Unreleased\\]") nil t)
          (replace-match (if (file-exists-p "CHANGELOG.org")
                             (format "* %s - %s" new-version today)
                           (format "## [%s] - %s" new-version today))))
        
        ;; Add new Unreleased section
        (goto-char (point-min))
        (forward-line 4) ; Move past title and initial description
        (insert unreleased-header "\n\n")
        
        (write-region (point-min) (point-max) changelog-file)
        (message "Updated %s" changelog-file)))
    
    ;; Commit the changes
    (when (yes-or-no-p "Commit changelog and version updates?")
      (shell-command (format "git add -A && git commit -m \"Release version %s\"" new-version))
      (message "Changes committed"))
    
    ;; Create git tag
    (when (yes-or-no-p (format "Create tag %s?" new-version))
      (shell-command (format "git tag -a %s -m \"Release %s\"" new-version new-version))
      (message "Tagged version %s" new-version))
    
    ;; Push changes to remote
    (when (yes-or-no-p "Push changes to remote?")
      (shell-command "git push")
      (message "Changes pushed to remote"))
    
    ;; Push tag to remote
    (when (yes-or-no-p (format "Push tag %s to remote?" new-version))
      (shell-command (format "git push origin %s" new-version))
      (message "Tag %s pushed to remote" new-version))
    
    (message "Release %s completed!" new-version)))

(provide 'release)
