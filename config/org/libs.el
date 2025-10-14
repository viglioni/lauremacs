;;; -*- lexical-binding: t; l-syntax: t -*-
(config-package org-download
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "./pics")
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-org-width 600)
  :init
  (lauremacs-major-mode-leader
    :keymaps 'org-mode-map
    "d"  '(nil                           :which-key "org download")
    "dd" '(org-download-delete           :which-key "delete")
    "ds" '(org-download-screenshot       :which-key "screenshot")
    "dr" '(org-download-rename-at-point  :which-key "rename at point")
    "dR" '(org-download-rename-last-file :which-key "rename last file")
    "de" '(org-download-edit             :which-key "edit")
    "du" '(org-download-image            :which-key "image from url")
    "dy" '(org-download-clipboard        :which-key "paste image from clipboard"))
  :bind
  (("C-c C-d d" . org-download-delete)
   ("C-c C-d s" . org-download-screenshot)
   ("C-c C-d r" . org-download-rename-at-point)
   ("C-c C-d R" . org-download-rename-last-file)
   ("C-c C-d e" . org-download-edit)          
   ("C-c C-d u" . org-download-image)
   ("C-c C-d y" . org-download-yank)))

(config-package valign)

