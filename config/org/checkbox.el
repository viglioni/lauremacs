(defun lauremacs/org-update-parent-todo (n-done n-not-done)
  "Update parent TODO state based on the state of its children.
When all children are done, mark parent as DONE.
When no children are done, mark parent as TODO.
When some children are done, mark parent as DOING.

N-DONE is the number of children marked as done.
N-NOT-DONE is the number of children not marked as done.

This function assumes the valid TODO states are TODO, DOING, and DONE,
and will only update if these are the exact TODO keywords configured."
  (let ((todo-state (org-get-todo-state))
        (valid-states '("TODO" "DOING" "DONE")))
    (when (and todo-state (equal valid-states org-todo-keywords-1))
      (cond
       ((= n-done 0) (org-todo "TODO"))
       ((= n-not-done 0) (org-todo "DONE"))
       (t (org-todo "DOING"))))))

;; todo auto update
(add-hook 'org-after-todo-statistics-hook
          (lambda (n-done n-not-done)
            (let ((todo-state (org-get-todo-state))
                  (valid-states '("TODO" "DOING" "DONE")))
              (when (and todo-state
                         (equal valid-states org-todo-keywords-1))
                (cond
                 ((= n-done 0) (org-todo "TODO"))
                 ((= n-not-done 0) (org-todo "DONE"))
                 (t (org-todo "DOING")))))))

;;;###autoload
(defun lauremacs//org-get-heading-statistics-at-point (heading)
  "Extract completion statistics from an org heading containing [X/Y] format.
HEADING is the org heading string to parse.

Return a cons cell (X . Y) if the heading contains statistics in [X/Y] format,
where X and Y are numbers.  Return nil if the heading doesn't contain statistics."
	(when (and heading (string-match "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]" heading))
		(cons (string-to-number (match-string 1 heading))
					(string-to-number (match-string 2 heading)))))

;;;###autoload
(defun lauremacs/org-checkbox-update-parent ()
  "Update parent heading's TODO state based on its checkbox statistics.
When all checkboxes are unchecked, set state to TODO.
When all checkboxes are checked, set state to DONE.
When some checkboxes are checked, set state to DOING.

The function only works when the todo-keywords are exactly:
'(\"TODO\" \"DOING\" \"DONE\").

Assumes presence of checkbox statistics in the heading,
formatted as [n/m] where n is number of completed items
and m is total number of items."
  (save-excursion
    (org-previous-visible-heading 1)
    (let* ((heading (org-get-heading t t t t)) ; Get clean heading text
           (stats (lauremacs//org-get-heading-statistics-at-point heading))  
           (todo-state (org-get-todo-state))
           (valid-states '("TODO" "DOING" "DONE")))
      (when (and stats todo-state
                 (equal valid-states org-todo-keywords-1))
        (let ((n-done (car stats))      ; n in [n/m]
              (n-total (cdr stats)))    ; m in [n/m]
          (cond
           ((= n-done 0) (org-todo "TODO"))
           ((= n-done n-total) (org-todo "DONE"))
           ((and (> n-done 0) (< n-done n-total)) (org-todo "DOING"))))))))

(add-hook 'org-checkbox-statistics-hook 'lauremacs/org-checkbox-update-parent)
