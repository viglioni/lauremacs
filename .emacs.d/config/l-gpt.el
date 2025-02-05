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
,
