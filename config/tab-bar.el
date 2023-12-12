(require 'projectile)
(require 'functional)

;;
;; Re/Naming tabs
;;


(defun lauremacs-tab--crop-name (name)
  "Crop NAME and add ..."
  (let ((len (- lauremacs-tab-width 3)))
    (fp/pipe name
      (lambda (str) (substring str 0 len))
      (fp/partial 'format "%s..."))))

(defun lauremacs-tab--fill-with-spaces (name)
  "Fill NAME with spaces."
  (let* ((spaces       (- lauremacs-tab-width (length name)))
         (right-spaces (/ spaces 2))
         (left-spaces  (- spaces right-spaces))
         (get-spaces   (fp/compose 'string-join
                          (fp/map (fp/const " "))
                          'fp/range)))
    (concat
     (funcall get-spaces left-spaces)
     name
     (funcall get-spaces right-spaces))))


(defun lauremacs-tab--format-tab-name (name)
  "Format NAME to have exactly `lauremacs-tab-width'."
  (let ((len (length name)))
    (cond
     ((= len lauremacs-tab-width) name)
     ((> len lauremacs-tab-width) (lauremacs-tab--crop-name name))
     ((< len lauremacs-tab-width) (lauremacs-tab--fill-with-spaces name)))))

(defun lauremacs-tabs-rename-tab (&optional name)
  "Rename tab with projectile project name.
If NAME is passed, NAME will be used.
Else it will be named `Default'."
	(interactive)
  (let ((tab-name (or name
                      (and (projectile-project-p) (projectile-project-name))
			                "default")))
    (fp/pipe tab-name
      'lauremacs-tab--format-tab-name
	    'tab-bar-rename-tab)))

(advice-add 'helm-projectile-switch-project
            :after 'lauremacs-tabs-rename-tab)

(defun lauremacs-tabs-get-tab-name ()
  "Return current tab name."
  (alist-get 'name (tab-bar--current-tab)))

(defun lauremacs-tabs--get-all-tab-names ()
  (mapcar (fp/partial 'alist-get 'name) (tab-bar-tabs)))

(defun lauremacs-tabs--tab-exists? (name)
  (let ((tab-name (lauremacs-tab--format-tab-name name))
        (tabs (lauremacs-tabs--get-all-tab-names)))
    (contains? tabs tab-name)))


;;
;; Killing tabs / buffers
;;

(defun lauremacs-tabs--kill-all-buffers (&optional tab-index)
  (interactive)
  (when tab-index (tab-bar-select-tab tab-index) (projectile-project-p))
  (mapcar 'kill-buffer (projectile-project-buffers)))

(advice-add 'tab-bar-close-tab :before 'lauremacs-tabs--kill-all-buffers)

(defconst lauremacs-tab-width 17)

(defun lauremacs-tab-new-project-tab ()
  "Open a new tab and select a project."
  (interactive)
  (tab-bar-new-tab)
  (revert-buffer)
  (helm-projectile-switch-project))


;;
;; Opening org roam files in tab
;;

(defconst lauremacs-tabs-org-roam-tab
  (lauremacs-tab--format-tab-name "org roam"))

;;;###autoload
(defun lauremacs-tabs-find-org-roam-node ()
  "Create/go to org roam tab and find node."
  (interactive)
  (unless (lauremacs-tabs--tab-exists? lauremacs-tabs-org-roam-tab)
    (tab-bar-new-tab)
    (lauremacs-tabs-rename-tab lauremacs-tabs-org-roam-tab))
  (tab-bar-select-tab-by-name lauremacs-tabs-org-roam-tab)
  (org-roam-node-find))


(use-package tab-bar
  :init
  (setq tab-bar-mode t)
  (setq tab-bar-tab-hints t)
  (setq tab-bar-new-tab-choice "*lauremacs*")
  (setq tab-bar-new-tab-to 'rightmost))



