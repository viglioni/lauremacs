;;
;; Laura Viglioni
;; 2025
;;


(use-package gptel
  :ensure t
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'claude-3-5-sonnet-20241022)
  
  :bind (("C-c m m" . gptel-send)
         ("C-c m o" . gptel-org-set-topic)
         ("C-c m r" . gptel-rewrite)))

(defmacro lauremacs/bind-key-conditionally (key function condition)
  "Bind KEY to FUNCTION if CONDITION is true, otherwise call the existing binding.

KEY is the key to bind.
FUNCTION is the function to call if CONDITION evaluates to true.
CONDITION is a form that is evaluated to determine which function to call.

If CONDITION evaluates to true, FUNCTION is called.
If CONDITION evaluates to false, the existing binding of the key is called."
  (let ((existing-binding (key-binding (kbd key))))
    (local-set-key
     (kbd key)
     `(lambda () (interactive)
        (if (funcall ,condition)
            (funcall ,function)
          (funcall-interactively (quote ,existing-binding)))))))
 

(use-package copilot
  :ensure t
  :hook ((prog-mode . copilot-mode))
  :init
  (lauremacs/bind-key-conditionally "<backtab>" 'copilot-accept-completion 'copilot--overlay-visible)
  (lauremacs/bind-key-conditionally "s-," 'copilot-next-completion 'copilot--overlay-visible)
  (lauremacs/bind-key-conditionally "s-." 'copilot-previous-completion 'copilot--overlay-visible)
  (lauremacs/bind-key-conditionally "TAB" (lambda () (interactive) (copilot-accept-completion-by-word) (copilot-complete)) 'copilot--overlay-visible)
)


(use-package copilot-chat
  :ensure t
  :hook ((copilot-chat . visual-line-mode)
         (git-commit-setup . copilot-chat-insert-commit-message))
  :config
  (setq copilot-chat-frontend 'org)
  (setq copilot-chat-follow nil))


