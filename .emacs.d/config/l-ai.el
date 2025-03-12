
;;
;; Laura Viglioni
;; 2025
;;

(use-package buttercup)

(use-package gptel
  :ensure t
  :config
  (setq gptel-default-mode 'org-mode)
  :init
  (define-key gptel-mode-map (kbd "C-c C-c") 'gptel-send))

(require 'gptel)
(require 'gptel-rewrite)
(require 'fp)

(defun lauremacs/add-files-to-gptel (suffix-regexp &optional directory)
  "Add all files matching SUFFIX-REGEXP to gptel context.
The function scans the DIRECTORY or current projectile project root directory and adds
files that match the given regular expression to gptel's knowledge base.

SUFFIX-REGEXP is a regular expression string that matches file suffixes/extensions.
For example, \"\\.org$\" will match all org files.

DIRECTORY is an optional parameter specifying the root directory to scan.
If not provided, uses the current projectile project root.

This function requires projectile and gptel to be properly configured.
Interactively prompts for the suffix regexp when called interactively."
  (interactive "sInsert suffix regexp: ")
  (let* ((root-dir (or directory (projectile-project-root)))
         (all-files (projectile-dir-files root-dir))
         (matching-files (seq-filter
                         (lambda (file) (string-match-p suffix-regexp file))
                         all-files)))
    (print root-dir)
    (print matching-files)
    (dolist (file matching-files)
      (gptel-add-file (expand-file-name file root-dir)))))


(defun lauremacs/add-files-under-dir-to-gptel (&optional suffix-regexp)
  (interactive "sInsert suffix regexp: ")
  (lauremacs/add-files-to-gptel suffix-regexp default-directory))

(defun lauremacs/gptel-rewrite-function-at-point (directive)
  "Use gptel-rewrite on the function definition at point with DIRECTIVE.
Does not show transient menu and executes immediately with clean code output."
  (interactive "sRewrite directive: ")
  (save-excursion
    (beginning-of-defun)
    (push-mark)
    (end-of-defun)
    (activate-mark)
    (let* ((gptel--rewrite-message
            (format "Follow these rules strictly:
1. Only output the exact code with NO markdown, NO backticks, NO explanations
2. Keep indentation and formatting consistent

Here is your task: %s" directive))
           (gptel-rewrite-default-action 'accept))
      (gptel--suffix-rewrite gptel--rewrite-message))))

(defun lauremacs/gptel-add-documentation-to-function (directive)
  "Use GPT to generate documentation for the function at point.
Uses gptel to analyze the function definition and generate
appropriate documentation based on the code.

DIRECTIVE is a string containing custom instructions for GPT when
generating the documentation. By default it will be asked to
'Write documentation for this function.'

When called interactively, prompts for a custom directive. 
The directive helps guide GPT in how to document the function."
  (interactive (list (read-string "Enter directive: " "Write documentation for this function.")))
  (lauremacs/gptel-rewrite-function-at-point directive))


(defun lauremacs/gptel-generate-changelog-entry ()
  "Generate a changelog entry using GPT based on staged git changes.

This function uses the gptel package to analyze the current git diff of
staged changes and creates a well-formatted changelog entry. The entry is
automatically inserted into CHANGELOG.md file under the Unreleased section.

If no staged changes are found, it will ask GPT to review recent commits instead.

The generated entry will be in past tense with bullet points, focusing on
user-facing changes and improvements.

Requirements:
- gptel package must be installed
- The current directory must be within a git repository
- CHANGELOG.md file should exist (will be created otherwise)"
  (interactive)
  (if (not (featurep 'gptel))
      (error "This function requires gptel. Please install it first")
    
    ;; Get the git diff for staged changes
    (let* ((git-diff (shell-command-to-string "git diff --staged"))
           (changelog-file (expand-file-name "CHANGELOG.md"))
           (prompt (format "Based on the following git diff, write a clear, concise changelog entry 
in past tense with bullet points. Focus on user-facing changes and improvements:

%s" git-diff)))
      
      ;; If no changes found
      (when (string-empty-p git-diff)
        (setq prompt "Please review my recent commits and suggest a changelog entry for them."))
      
      ;; Use gptel to generate the changelog entry
      (gptel prompt 
             :callback (lambda (response)
                         (with-current-buffer (find-file-noselect changelog-file)
                           (goto-char (point-min))
                           ;; Look for a section to add the entry
                           (if (re-search-forward "^## \\[Unreleased\\]" nil t)
                               (progn
                                 (forward-line 1)
                                 (insert "\n" response "\n")
                                 (save-buffer)
                                 (message "Changelog entry added to %s" changelog-file))
                             ;; If no Unreleased section, just add at the beginning
                             (goto-char (point-min))
                             (insert "## [Unreleased]\n\n" response "\n\n")
                             (save-buffer)
                             (message "Changelog entry added to %s" changelog-file))))))))


(defun lauremacs/gptel-rewrite-region-or-buffer (directive)
  "Use gptel-rewrite on the active region or whole buffer with DIRECTIVE.
If region is active, rewrites just that region. Otherwise, rewrites the entire buffer.
Does not show transient menu and executes immediately with clean code output."
  (interactive "sRewrite directive: ")
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (let* ((gptel--rewrite-message
            (format "Follow these rules strictly:
1. Only output the exact code with NO markdown, NO backticks, NO explanations
2. Keep indentation and formatting consistent

Here is your task: %s" directive))
           (gptel-rewrite-default-action 'accept))
      (gptel--suffix-rewrite gptel--rewrite-message))))


(defmacro lauremacs/bind-key-conditionally (key function condition mode-map)
  "Bind KEY to FUNCTION if CONDITION is true, otherwise call the existing binding.

KEY is the key to bind.
FUNCTION is the function to call if CONDITION evaluates to true.
CONDITION is a form that is evaluated to determine which function to call.

If CONDITION evaluates to true, FUNCTION is called.
If CONDITION evaluates to false, the existing binding of the key is called."
  `(let ((existing-binding (key-binding (kbd ,key))))

     (define-key ,mode-map
                 (kbd ,key)
                 (lambda () (interactive)
                   (if (funcall ,condition)
                       (funcall ,function)
                     (funcall-interactively existing-binding))))))
 

(use-package copilot
  :ensure t
  :hook ((prog-mode . copilot-mode))
  :init 
  (lauremacs/bind-key-conditionally "<backtab>" 'copilot-accept-completion 'copilot--overlay-visible copilot-mode-map)
  (lauremacs/bind-key-conditionally "s-," 'copilot-next-completion 'copilot--overlay-visible copilot-mode-map)
  (lauremacs/bind-key-conditionally "s-." 'copilot-previous-completion 'copilot--overlay-visible copilot-mode-map)
  (lauremacs/bind-key-conditionally "TAB" (lambda () (interactive) (copilot-accept-completion-by-word) (copilot-complete)) 'copilot--overlay-visible copilot-mode-map)
)



(use-package copilot-chat
  ;;	:straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :ensure t
  :hook ((copilot-chat . visual-line-mode)
         (git-commit-setup . copilot-chat-insert-commit-message))
  :config
  (setq copilot-chat-frontend 'org)
  (setq copilot-chat-follow nil))


