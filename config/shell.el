;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

(require 'window-purpose)
(require 'ansi-color)

;;;###autoload
(defun lauremacs-pop-shell (shell-fn &rest shell-args)
	"Pop a shell in a bottom window.
SHELL-FN is the shell to be used, e.g. `shell', `ansi-term' etc.
SHELL-ARGS are the args passed to the SHELL-FN, e.g. \"/bin/zsh\"."
	(lexical-let ((shell-fn shell-fn) (shell-args shell-args))
		(lambda ()
			(interactive)
			(display-buffer-in-side-window
			 (save-window-excursion
				 (apply shell-fn shell-args)
				 (current-buffer))
			 '((side . bottom)))
			(select-window (purpose-get-bottom-window)))))

;;;###autoload
(defun lauremacs-pop-shell-cmd (cmd buff-name)
	"Pop a shell with some cmd executed.
CMD some shell command, e.g \"lein repl\".
BUFF-NAME the name of the buffer the where shell will be executed."
	(lexical-let ((cmd cmd) (buff-name buff-name))
		(lambda ()
			(interactive)
			(let ((buff (get-buffer-create buff-name)))
				(display-buffer-in-side-window buff '((side . bottom)))
				(projectile-with-default-dir (or (projectile-project-root) ".")
					(async-shell-command cmd buff-name (concat "error-" buff-name)))
				(set-window-dedicated-p (get-buffer-window buff) t)
				(select-window (purpose-get-bottom-window))))))

;;;###autoload
(defun lauremacs-shell-cmd-other-window (cmd buff-name)
	"Pop a shell with some cmd executed.
CMD some shell command, e.g \"lein repl\".
BUFF-NAME the name of the buffer the where shell will be executed."
	(lexical-let ((cmd cmd) (buff-name buff-name))
		(lambda ()
			(interactive)
			(let ((buff (get-buffer-create buff-name)))
				(switch-to-buffer buff)
				(projectile-with-default-dir (or (projectile-project-root) ".")
					(async-shell-command cmd buff-name (concat "error-" buff-name)))
				(set-window-dedicated-p (get-buffer-window buff) t)
				(select-window (purpose-get-bottom-window))))))



;;
;; eshell config
;;


(setq eshell-history-size              10000
      eshell-buffer-maximum-lines      10000
      eshell-hist-ignoredups           t
      eshell-scroll-to-bottom-on-input t)

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :bind (("C-<return>" . eshell-send-input))
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  (eshell-git-prompt-use-theme 'multiline)
	:init
	;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history))

(add-to-list 'display-buffer-alist
             '("*Async Shell Command*" (display-buffer-in-side-window) (side . bottom)))

;;
;; binds
;;





(with-eval-after-load "term"
  (define-key term-raw-map (kbd "C-c C-y") 'term-paste)
	(define-key term-raw-map (kbd "s-v") 'term-paste))


;;
;; Colour configs
;;

(defun lauremacs/colorize-compilation-buffer ()
  (read-only-mode nil)
  (ansi-color-apply-on-region 1 (point))
  (read-only-mode 1))

(add-hook 'compilation-filter-hook 'lauremacs/colorize-compilation-buffer)

