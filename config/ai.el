;;; -*- lexical-binding: t; l-syntax: t -*-
(use-package claude-code
   ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  ;; optional IDE integration with Monet
 ; (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
;  (monet-mode 1)

  (claude-code-mode))


(config-package gptel
  :config
  (setq gptel-backend
        (gptel-make-anthropic "Claudio"
          :key (lambda () (getenv "ANTHROPIC_API_KEY"))
          :stream t
          :models '(claude-sonnet-4-20250514)
          :header (lambda () (when-let* ((key (gptel--get-api-key)))
                          `(("x-api-key" . ,key)
                            ("anthropic-version" . "2023-06-01")
                            ("anthropic-beta" . "pdfs-2024-09-25")
                            ("anthropic-beta" . "output-128k-2025-02-19")
                            ("anthropic-beta" . "prompt-caching-2024-07-31"))))
          :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                                      :max_tokens 4096)))
  (setq gptel-default-mode 'org-mode)
  (setq gptel-prompt-prefix-alist
        '((org-mode . "* ")))           ; H1 for prompts

  :config
  (define-key gptel-mode-map (kbd "C-c C-c")     'gptel-send)
  (define-key gptel-mode-map (kbd "C-u C-c C-c") 'org-ctrl-c-ctrl-c)
  (mapcar
   (lambda (item) (add-to-list 'gptel-directives item))
   '((emacs . "You are an expert in Emacs, emacs-lisp and common lisp.
You are the creator of emacs distribution with custom libraries.
You must help a junior engineer to make good deliverables in this project, on senior level.")
     (ruby . "You are an expert in Ruby On Rails/ReactJS.
You are the tech lead of this project.
You must help a junior engineer to make good deliverables in this project, on senior level.")
     (python . "You are an expert in Django python and VueJS.
You are the tech lead of this project.
You must help a junior engineer to make good deliverables in this project, on senior level.")
     (elixir . "You are an expert in Elixir, Phoenix and Phoenix LiveView.
You are the tech lead of this project.
You must help a junior engineer to make good deliverables in this project, on senior level."))))

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
  (message "Rewriting function at point with directive: %s" directive)
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
      (message "Querying GPT for function rewrite...")
      (gptel--suffix-rewrite gptel--rewrite-message)
      (message "Function rewrite completed successfully"))))

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
  (message "Adding documentation to function at point...")
  (lauremacs/gptel-rewrite-function-at-point directive)
  (message "Documentation added successfully"))


(defun lauremacs/gptel-add-type-to-function (directive)
  "Use GPT to add type annotations to the function at point.
Uses gptel to analyze the function definition and add appropriate
type annotations based on the code.

DIRECTIVE is a string containing custom instructions for GPT when
generating the type annotations. By default it will ask to
'Add proper type annotations to this function.'

When called interactively, prompts for a custom directive.
The directive helps guide GPT in how to type the function."
  (interactive (list (read-string "Enter directive: " "Add proper type annotations to this function.")))
  (message "Adding type annotations to function at point...")
  (save-excursion
    (beginning-of-defun)
    (push-mark)
    (end-of-defun)
    (activate-mark)
    (let* ((gptel--rewrite-message
            (format "Follow these rules strictly:
1. Only output the exact code with NO markdown, NO backticks, NO explanations
2. Keep indentation and formatting consistent
3. Add appropriate type annotations/signatures based on the function's implementation
4. Preserve all existing functionality and logic

Here is your task: %s" directive))
           (gptel-rewrite-default-action 'accept))
      (message "Querying GPT for type annotations...")
      (gptel--suffix-rewrite gptel--rewrite-message)
      (message "Type annotations added successfully"))))


(defun lauremacs/gptel-generate-changelog-entry ()
  "Generate a changelog entry using GPT based on staged git changes.

This function uses the gptel package to analyze the current git diff of
staged changes and creates a well-formatted changelog entry. The entry is
automatically inserted into CHANGELOG.md or CHANGELOG.org file under the Unreleased section.

If no staged changes are found, it will ask GPT to review recent commits instead.

The generated entry will be in past tense with bullet points, focusing on
user-facing changes and improvements.

Requirements:
- gptel package must be installed
- The current directory must be within a git repository
- CHANGELOG.md or CHANGELOG.org file should exist (will be created otherwise)"
  (interactive)
  (if (not (featurep 'gptel))
      (error "This function requires gptel. Please install it first")
    
    ;; Get the git diff for staged changes
    (let* ((git-diff (shell-command-to-string "git diff --staged"))
           (org-changelog-file (expand-file-name "CHANGELOG.org"))
           (md-changelog-file (expand-file-name "CHANGELOG.md"))
           (use-org (file-exists-p org-changelog-file))
           (changelog-file (if use-org org-changelog-file md-changelog-file))
           ;; Format-specific variables
           (section-regex (if use-org "^\\*\\* \\(.*\\)" "^### \\(.*\\)"))
           (unreleased-regex (if use-org "^\\* Unreleased" "^## \\[Unreleased\\]"))
           (next-section-regex (if use-org "^\\* " "^## "))
           (section-format (if use-org "\n** %s\n\n%s" "\n### %s\n\n%s"))
           (unreleased-format (if use-org "* Unreleased\n\n%s\n\n" "## [Unreleased]\n\n%s\n\n"))
           ;; Project info
           (project-root (or (projectile-project-root) default-directory))
           (project-name (file-name-nondirectory (directory-file-name project-root)))
           (git-branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
           (git-info (string-trim (shell-command-to-string "git log -1 --pretty=format:'%s' HEAD")))
           (prompt (format "Based on the following git diff, write a clear, concise changelog entry 
in past tense with bullet points. Focus on user-facing changes and improvements:

%s" git-diff)))
      
      ;; If no changes found
      (when (string-empty-p git-diff)
        (setq prompt "Please review my recent commits and suggest a changelog entry for them."))
      
      ;; Open file and add to context
      (when (not (find-buffer-visiting changelog-file))
        (find-file-noselect changelog-file))
      
      ;; Add file to gptel context if not already added
      (gptel-add-file changelog-file)
      
      ;; Format the instruction
      (let* ((final-changelog-file changelog-file)
             (final-section-regex section-regex)
             (final-unreleased-regex unreleased-regex)
             (final-next-section-regex next-section-regex)
             (final-section-format section-format)
             (final-unreleased-format unreleased-format)
             (instruction (format "Write a changelog entry for the project '%s' (branch: %s) based on the following changes.
Focus ONLY on changes related to THIS project, not external dependencies.
Follow the format of the existing changelog entries exactly.
Group changes under appropriate header (Added/Changed/Fixed/Removed).
Sort section headers alphabetically (Added comes before Changed comes before Fixed comes before Removed).
Sort items alphabetically within each section when order is not important.
Only output the raw changelog entry with no explanations, no code blocks, and no backticks.
Use concise, user-focused language:

Current commit message: %s

Changes:
%s" 
                                  project-name git-branch git-info prompt)))
        
        ;; Make the GPT request
        (gptel-request
            instruction
          :callback (lambda (response info)
                      (when response
                        (with-current-buffer (find-file final-changelog-file)
                          (goto-char (point-min))
                          ;; Look for Unreleased section
                          (if (re-search-forward final-unreleased-regex nil t)
                              (progn
                                ;; Define the unreleased section boundary
                                (let* ((unreleased-start (point))
                                       (unreleased-end (save-excursion
                                                         (if (re-search-forward final-next-section-regex nil t)
                                                             (match-beginning 0)
                                                           (point-max))))
                                       (lines (split-string response "\n"))
                                       (current-section nil)
                                       (sections (make-hash-table :test 'equal)))
                                 
                                  ;; Group content by section headers
                                  (dolist (line lines)
                                    (if (string-match final-section-regex line)
                                        (setq current-section (match-string 1 line))
                                      (when current-section
                                        (puthash current-section 
                                                 (concat (gethash current-section sections "") line "\n")
                                                 sections))))
                                 
                                  ;; Insert content into appropriate sections within the Unreleased section
                                  (let* ((all-section-names (hash-table-keys sections))
                                         (section-names (sort all-section-names 'string<)))
                                    (dolist (section-name section-names)
                                      ;; Look for existing section within the Unreleased boundaries
                                      (save-excursion
                                        (goto-char unreleased-start)
                                        (if (and (re-search-forward (format final-section-regex (regexp-quote section-name)) unreleased-end t))
                                            ;; Section exists, append content
                                            (progn
                                              (forward-line 1)
                                              (insert (gethash section-name sections)))
                                          ;; Section doesn't exist, create it at the end of Unreleased section
                                          (goto-char unreleased-start)
                                          (insert (format final-section-format section-name (gethash section-name sections)))))))
                                  (save-buffer)
                                  (message "Changelog entry added to %s" final-changelog-file)))
                            ;; If no Unreleased section, just add at the beginning
                            (goto-char (point-min))
                            (insert (format final-unreleased-format response))
                            (save-buffer)
                            (message "Changelog entry added to %s" final-changelog-file))))))))))

(defun lauremacs/gptel-insert-at-point (directive)
  "Ask GPT with DIRECTIVE and insert response at current point.
The response will be clean text without explanations or code fences."
  (interactive "sEnter directive: ")
  (let* ((buffer (current-buffer)))
    (message "Connecting to LLM...")
    (gptel-request
     directive
     :system "You are a helpful assistant that provides direct answers.
Follow these instructions strictly:
- Generate ONLY plain text as output, without any explanation or markdown formatting
- Do not use code fences, backticks, or any other formatting
- Start immediately with the content
- End immediately after the content"
     :callback (lambda (response info)
                 (cond
                  ((plist-get info :error)
                   (message "Error: Something went wrong with the request - %s" 
                            (plist-get info :error)))
                  (response
                   (message "Writing response...")
                   (with-current-buffer (current-buffer)
                     (save-excursion
                       (insert response)
                       (message "GPT response inserted at point"))))
                  (t
                   (message "No response received from LLM")))))))


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


(defmacro lauremacs/bind-key-conditionally (key function condition)
  "Bind KEY to FUNCTION if CONDITION is true, otherwise call the existing binding.

KEY is the key to bind.
FUNCTION is the function to call if CONDITION evaluates to true.
CONDITION is a form that is evaluated to determine which function to call."
  `(let ((existing-binding (key-binding (kbd ,key))))
     (print existing-binding)
     (global-set-key
      (kbd ,key)
      (lambda () (interactive)
        (if (funcall ,condition)
            (call-interactively ,function)
          (when existing-binding
            (call-interactively existing-binding)))))))

;;
;; GitHub PR Creation with AI
;;

(defun lauremacs/ai--extract-issue-code-from-branch ()
  "Extract issue code from current git branch.
Branch format: {gh-user}/{issue-code}-issue-title-in-kebab-case
Returns: issue code in uppercase (e.g., AD-6120)"
  (let* ((branch (magit-get-current-branch)))
    (when branch
      (when (string-match "/\\([a-z]+\\)-\\([0-9]+\\)-" branch)
        (upcase (format "%s-%s"
                       (match-string 1 branch)
                       (match-string 2 branch)))))))

(defun lauremacs/ai--get-repo ()
  "Get Organization/Repo from git remote."
  (replace-regexp-in-string
   "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
   (magit-get "remote"
              (magit-get-push-remote)
              "url")))

(defun lauremacs/ai-create-pr-with-claude ()
  "Create a GitHub PR using Claude Code to generate description.
Extracts the Linear issue code from branch name, asks Claude Code to:
1. Get issue details from Linear
2. Generate PR title: [ISSUE-CODE] - Title
3. Generate PR description with summary of changes
4. Open GitHub PR creation page with pre-filled data"
  (interactive)
  (let* ((issue-code (lauremacs/ai--extract-issue-code-from-branch))
         (repo (lauremacs/ai--get-repo))
         (branch (magit-get-current-branch)))

    (unless issue-code
      (error "Could not extract issue code from branch name. Expected format: {user}/{code}-title"))

    (message "Generating PR with Claude Code for issue %s..." issue-code)

    ;; Call claude-code CLI to generate PR
    (let* ((claude-prompt (format "Create a pull request for Linear issue %s.

Instructions:
1. Get the issue details from Linear using the issue code %s
2. Generate a PR title in format: [%s] - {Issue Title from Linear}
3. Create a comprehensive PR description that includes:
   - Summary of changes made
   - Link to Linear issue
   - Any relevant context from the issue

Then use the GitHub CLI (gh) to create the PR with this information, assigning it to the current user.

Current branch: %s
Repository: %s"
                                   issue-code
                                   issue-code
                                   issue-code
                                   branch
                                   repo))
           (claude-buffer (get-buffer-create "*Claude PR Creation*")))

      ;; Display Claude buffer
      (with-current-buffer claude-buffer
        (erase-buffer)
        (insert (format "Creating PR for %s...\n\n" issue-code))
        (insert "Calling Claude Code CLI...\n\n"))

      (pop-to-buffer claude-buffer)

      ;; Call claude-code CLI
      (let ((default-directory (projectile-project-root)))
        (make-process
         :name "claude-pr-creation"
         :buffer claude-buffer
         :command (list "claude-code" "task" claude-prompt)
         :sentinel (lambda (proc event)
                    (when (string-match-p "finished" event)
                      (message "PR creation completed! Check *Claude PR Creation* buffer for details"))))))))
