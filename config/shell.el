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

(lauremacs-leader
	"!"   '(shell-command                                                   :which-key "shell command")
	"&"   '(async-shell-command                                             :which-key "async shell command")
  "ab"  '(eww                                                             :which-key "browser (eww)")
  ;; translate
  "at"  '(nil                                                             :which-key "translate")
  "att" '(lauremacs-translate-transient                                   :which-key "translate")
  "atp" '(lauremacs-translate-to-brazilian-at-point                       :which-key "translate to brazilian word at point")
  "atP" '(lauremacs-translate-from-brazilian-at-point                     :which-key "translate from brazilian at point")
	;; pop shell
  "as"  '(nil                                                             :which-key "shell")
	"asH" (list (lauremacs-pop-shell-cmd "stack ghci" "haskell-stack-ghci") :which-key "haskell stack ghci")
	"asN" (list (lauremacs-pop-shell-cmd "node" "node")                     :which-key "node")
	"asc" (list (lauremacs-pop-shell-cmd "iex" "elixir-repl")               :which-key "elixir repl")
	"asc" (list (lauremacs-pop-shell-cmd "lein repl" "clojure-lein-repl")   :which-key "clojure lein repl")
  "ase" (list (lauremacs-pop-shell 'eshell)                               :which-key "eshell")
	"ash" (list (lauremacs-pop-shell-cmd "ghci" "haskell-ghci")             :which-key "haskell ghci")
	"asi" (list (lauremacs-pop-shell 'ielm)                                 :which-key "ielm")
	"asn" (list (lauremacs-pop-shell-cmd "npx ts-node" "ts-node")           :which-key "ts node")
  "asp" (list (lauremacs-pop-shell-cmd "python3" "python")                :which-key "python")
	"ast" (list (lauremacs-pop-shell 'ansi-term (getenv "SHELL"))           :which-key "ansi-term")
  "ass" (list (lauremacs-pop-shell 'shell)                                :which-key "shell")
  "asS" (list (lauremacs-pop-shell-cmd "sage" "sage")                     :which-key "sage")
  "asx" (list (lauremacs-pop-shell-cmd "iex" "elixir-repl")               :which-key "elixir repl")
	;; full buffer shell
  "asf" '(nil                                                             :which-key "full buffer shell")
	"asfH" `(,(fp/const-fn-interactive "stack ghci" "haskell-stack-ghci")   :which-key "haskell stack ghci")
	"asfN" `(,(fp/const-fn-interactive "node" "node")                       :which-key "node")
	"asfc" `(,(fp/const-fn-interactive "iex" "elixir-repl")                 :which-key "elixir repl")
	"asfc" `(,(fp/const-fn-interactive "lein repl" "clojure-lein-repl")     :which-key "clojure lein repl")
  "asfe" `(eshell                                                         :which-key "eshell")
	"asfh" `(,(fp/const-fn-interactive "ghci" "haskell-ghci")               :which-key "haskell ghci")
	"asfi" `(ielm                                                           :which-key "ielm")
	"asfn" `(,(fp/const-fn-interactive "npx ts-node" "ts-node")             :which-key "ts node")
  "asfp" `(,(fp/const-fn-interactive "python3" "python")                  :which-key "python")
	"asft" `(,(fp/const-fn-interactive 'ansi-term "/bin/zsh")               :which-key "ansi-term")
  "asfs" `(shell                                                          :which-key "shell")
  "asfS" `(,(fp/const-fn-interactive 'shell-command "sage" "sage")                       :which-key "sage")
  "asfx" `(,(fp/const-fn-interactive "iex" "elixir-repl")                 :which-key "elixir repl"))



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

