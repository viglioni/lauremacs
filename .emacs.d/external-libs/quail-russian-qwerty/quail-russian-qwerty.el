;;; quail-russian-qwerty.el --- Russian (QWERTY) layout for Quail  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Gosha Tcherednitchenko

;; Author: Gosha Tcherednitchenko <mail@gosha.net>
;; Keywords: languages, extensions, convenience, quail, russian
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;; 
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;; 
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'quail)

(quail-define-package
 "russian-qwerty" "Russian" "RU" nil
 "ЯШЕРТЫ Russian QWERTY-based phonetic layout"
 nil t t t t nil nil nil nil nil t)

;;  1! 2@ 3# 4$ 5% 6^ 7& 8* 9( 0) Ь Ъ /? щЩ
;;   Я  Ш  Е  Р  Т  Ы  У  И  О  П  Ю  Ж
;;    А  С  Д  Ф  Г  Ч  Й  К  Л  ;:  '"
;;     З  Х  Ц  В  Б  Н  М  ,<  .>  Э

(quail-define-rules
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?ь)
 ("=" ?ъ)
 ("|" ?э)
 ("`" ?щ)
 ("q" ?я)
 ("w" ?ш)
 ("e" ?е)
 ("r" ?р)
 ("t" ?т)
 ("y" ?ы)
 ("u" ?у)
 ("i" ?и)
 ("o" ?о)
 ("p" ?п)
 ("[" ?ю)
 ("]" ?ж)
 ("a" ?а)
 ("s" ?с)
 ("d" ?д)
 ("f" ?ф)
 ("g" ?г)
 ("h" ?ч)
 ("j" ?й)
 ("k" ?к)
 ("l" ?л)
 (";" ?\;)
 ("'" ?')
 ("\\" ?э)
 ("z" ?з)
 ("x" ?х)
 ("c" ?ц)
 ("v" ?в)
 ("b" ?б)
 ("n" ?н)
 ("m" ?м)
 ("," ?,)
 ("." ?.)
 ("/" ?/)
 ("!" ?!)
 ("@" ?@)
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?^)
 ("&" ?&)
 ("*" ?*)
 ("(" ?\()
 (")" ?\))
 ("_" ?Ь)
 ("+" ?Ъ)
 ("~" ?Щ)
 ("Q" ?Я)
 ("W" ?Ш)
 ("E" ?Е)
 ("R" ?Р)
 ("T" ?Т)
 ("Y" ?Ы)
 ("U" ?У)
 ("I" ?И)
 ("O" ?О)
 ("P" ?П)
 ("{" ?Ю)
 ("}" ?Ж)
 ("A" ?А)
 ("S" ?С)
 ("D" ?Д)
 ("F" ?Ф)
 ("G" ?Г)
 ("H" ?Ч)
 ("J" ?Й)
 ("K" ?К)
 ("L" ?Л)
 (":" ?:)
 ("\"" ?э)
 ("|" ?Э)
 ("Z" ?З)
 ("X" ?Х)
 ("C" ?Ц)
 ("V" ?В)
 ("B" ?Б)
 ("N" ?Н)
 ("M" ?М)
 ("<" ?<)
 (">" ?>)
 ("?" ??))

(provide 'quail-russian-qwerty)
;;; quail-russian-qwerty.el ends here
