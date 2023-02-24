;;; lauremacs-translate.el --- Translate functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Laura Viglioni

;; Author: Laura Viglioni
;; Maintainer: Laura Viglioni
;; Created: 24 Feb 2023
;; Keywords: translation
;; URL: https://github.com/viglioni/lauremacs

;; This file is not part of GNU Emacs.

;; This file is free software

;; along with this file.  If not, see <https://www.gnu.org/licenses/>.



;;; Commentary:
;; Uses go-translate and transient to handle translations


(require 'transient)

;;; Code:

(use-package go-translate
  :init
  (setq gts-translate-list '())
  (setq gts-default-translator nil)

  (lauremacs-leader
    "at" '(lauremacs-translate-transient :which-key "translate")))


(transient-define-prefix lauremacs-translate-transient ()
  [;; brazilian to others
   ("be" "brazilian to english"    (lambda () (interactive) (lauremacs-translate "pt" "en")))
   ("bi" "brazilian to italian"    (lambda () (interactive) (lauremacs-translate "pt" "it")))
   ("bn" "brazilian to nederlands" (lambda () (interactive) (lauremacs-translate "pt" "nl")))
   ("bs" "brazilian to español"    (lambda () (interactive) (lauremacs-translate "pt" "es")))
   ("br" "brazilian to russian"    (lambda () (interactive) (lauremacs-translate "pt" "ru")))
   ;; others to brazilian
   ("eb" "english to brazilian"    (lambda () (interactive) (lauremacs-translate "en" "pt")))
   ("ib" "italian to brazilian"    (lambda () (interactive) (lauremacs-translate "it" "pt")))
   ("nb" "nederlands to brazilian" (lambda () (interactive) (lauremacs-translate "nl" "pt")))
   ("sb" "español to brazilian"    (lambda () (interactive) (lauremacs-translate "es" "pt")))
   ("rb" "russian to brazilian"    (lambda () (interactive) (lauremacs-translate "ru" "pt")))
   ;; english to others
   ("ei" "english to italian"      (lambda () (interactive) (lauremacs-translate "en" "it")))
   ("en" "english to nederlands"   (lambda () (interactive) (lauremacs-translate "en" "nl")))
   ("er" "english to russian"      (lambda () (interactive) (lauremacs-translate "en" "ru")))
   ;; others to english
   ("ie" "italian to english"      (lambda () (interactive) (lauremacs-translate "it" "en")))
   ("ne" "nederlands to english"   (lambda () (interactive) (lauremacs-translate "nl" "en")))
   ("re" "russian to english"      (lambda () (interactive) (lauremacs-translate "ru" "en")))
   ])


;;;###autoload
(defun lauremacs-translate (from to)
  "Translate words FROM language TO language."
  (let ((gts-translate-list  (list (list from to)))
				(gts-default-translator
				 (gts-translator
					:picker (gts-prompt-picker :single t)
					:engines (list (gts-bing-engine) (gts-google-engine))
					:render (gts-posframe-pin-render))))
    (print gts-translate-list)
		(gts-translate gts-default-translator)))

(provide 'lauremacs-translate)

;;; lauremacs-translate.el ends here
