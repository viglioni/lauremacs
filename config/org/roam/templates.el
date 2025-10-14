;;; -*- lexical-binding: t; l-syntax: t -*-
(defmacro lauremacs/roam-lang-template (keybind name lang-code)
  `'(,keybind
     ,name
     plain
     "\n\n%?"
     :if-new
     (file+head
      "%<%Y%m%d%H%M%S>-${slug}.org"
      ,(concat "# -*- ispell-local-dictionary: \"" lang-code "\"; -*-" "\n"
               "#+title: ${title}" "\n"
               "#+filetags: :" name ":" "\n"))
     :unnarrowed t))

(defconst default-org-roam-template
  '("d" "default" plain
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t))

(defconst org-roam-math-template
  '("m" "math" plain
    "\n\n%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                       "
# -*- eval: (org-math-mode 1); ispell-local-dictionary: \"en\"; -*-
#+title: ${title}
#+options: tex:t
#+startup: latexpreview
#+filetags: :math:")
    :unnarrowed t))

(defconst org-roam-private-template
  '("p" "private" plain
    "\n\n%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                       "
# -*- ispell-local-dictionary: \"pt_BR\"; -*-
#+title: ${title}
#+options: tex:t
#+startup: latexpreview
#+filetags: :privado:")
    :unnarrowed t))

(defconst org-roam-recipees
  '("r" "recipees" plain
    "\n\n%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                       "
# -*- ispell-local-dictionary: \"pt_BR\"; -*-
#+title: ${title}
#+filetags: :receitas:")
    :unnarrowed t))

(defun lauremacs/org-roam-templates ()
  (setq org-roam-capture-templates
        (list
         default-org-roam-template
         org-roam-math-template
         org-roam-private-template
         org-roam-recipees
         (lauremacs/roam-lang-template "b" "brazilian" "pt_BR")
         (lauremacs/roam-lang-template "i" "italiano" "it")
         (lauremacs/roam-lang-template "n" "nederlands" "dutch")
         (lauremacs/roam-lang-template "e" "español" "es")
         (lauremacs/roam-lang-template "R" "ruskij" "ru"))))
