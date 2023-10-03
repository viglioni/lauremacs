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
(require 'general)

;;; Code:

(use-package go-translate
  :init
  (setq gts-translate-list '())
  (setq gts-default-translator nil))


(transient-define-prefix lauremacs-translate-transient ()
  [ ;; brazilian to others
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
(cl-defun lauremacs-translate (from to &key
                                    (picker (gts-prompt-picker :single t))
                                    (render (gts-posframe-pin-render)))
  "Translate words FROM language TO language."
  (message (format "translating from %s to %s" from to))
  (let ((gts-translate-list  `((,from ,to)))
				(gts-default-translator
				 (gts-translator
					:picker picker
					:engines (list (gts-bing-engine) (gts-google-engine))
					:render render)))
		(gts-translate gts-default-translator)))


(defun lauremacs-translate-to-brazilian-at-point ()
  (interactive)
  (let ((from (seq-take ispell-local-dictionary 2)))
    (lauremacs-translate
     from "pt"
     :picker (gts-noprompt-picker)
     :render (gts-posframe-pop-render :width 40))))

(defun lauremacs-translate-from-brazilian-at-point ()
  (interactive)
  (let ((to (seq-take ispell-local-dictionary 2)))
    (lauremacs-translate
     "pt" to
     :picker (gts-noprompt-picker)
     :render (gts-posframe-pop-render :width 40))))


(provide 'lauremacs-translate)

;;; lauremacs-translate.el ends here

