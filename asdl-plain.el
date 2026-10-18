;;; asdl-plain.el --- ASDL-only fontification  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bo Lin

;; Author: Bo Lin <bo@dreamsphere.org>
;; Keywords: languages, asdl

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for Zephyr ASDL files.  It fontifies ASDL constructs only, so
;; field lists and comments stay plain.
;;
;; Imenu lists the type definitions of the buffer, with the constructors of
;; each definition as sub-entries; the imenu index also serves xref through
;; the backend that `basic' installs.

;;; Code:

(require 'generic)
(require 'basic)

(defun asdl-plain--imenu-create-index ()
  "Return an imenu index for the current ASDL buffer.

Type definitions (sums and products) become top-level entries;
the constructors of each definition become sub-entries; the
definition's own position heads the sub-list, so the type name is
selectable directly.
Matches inside comments are ignored.
Positions are markers when `imenu-use-markers' is non-nil."
  (save-excursion
    (goto-char (point-min))
    (let ((def-re "^[ \t]*\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t]*=")
          (ctor-re "\\(?:=\\||\\)[ \t]*\\([A-Za-z_][A-Za-z0-9_]*\\)")
          defs)
      (while (re-search-forward def-re nil t)
        (let ((name (match-string-no-properties 1))
              (def-beg (match-beginning 0)))
          ;; `syntax-ppss' moves point to POS; keep point at match end.
          (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 1))))
            (push (list name (if imenu-use-markers
                                 (copy-marker def-beg t)
                               def-beg))
                  defs))))
      (setq defs (nreverse defs))
      (let ((rest defs)
            index)
        (while rest
          (let* ((name (nth 0 (car rest)))
                 (beg (nth 1 (car rest)))
                 (end (if (cdr rest) (nth 1 (cadr rest)) (point-max)))
                 ctors)
            (goto-char beg)
            (while (re-search-forward ctor-re end t)
              (let ((name (match-string-no-properties 1))
                    (name-beg (match-beginning 1)))
                (unless (save-excursion (nth 8 (syntax-ppss name-beg)))
                (push (cons name (if imenu-use-markers
                                   (copy-marker name-beg t)
                                 name-beg))
                      ctors))))
            (push (if ctors
                      (cons name (cons (cons name beg)
                                       (nreverse ctors)))
                    (cons name beg))
                  index)
            (setq rest (cdr rest))))
        (nreverse index)))))

;;;###autoload
(define-generic-mode 'asdl-mode
  '("--")                                      ; Line comment: --
  nil                                          ; Keywords are in font-lock list
  '(;; Section headers
    ("^[ \t]*\\(module\\|attributes\\)\\_>" . 'font-lock-keyword-face)
    ;; Module name:  module Name
    ("\\_<module[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)" (1 'font-lock-constant-face))
    ;; Type definition:  TypeName =
    ("^[ \t]*\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t]*=" (1 'font-lock-type-face))
    ;; Constructor: first identifier after '=' or '|'
    ("\\(?:=\\||\\)[ \t]*\\([A-Za-z_][A-Za-z0-9_]*\\)" (1 'font-lock-function-name-face))
    ;; Field types:  Con(type name, user* names, builtin? opt), also
    ;; when the fields sit on their own continuation lines.
    ("\\(?:[,(][ \t\r\n]*\\|^[ \t]+\\)\\([A-Za-z_][A-Za-z0-9_]*\\)[?*]?"
     (1 (if (member (match-string 1)
                    '("identifier" "int" "string" "bytes" "object" "constant" "bool"))
            'font-lock-builtin-face
          'font-lock-type-face)))
    ;; Field cardinality modifiers (* and ?)
    ("[?*]" . 'font-lock-keyword-face))
  nil                                          ; Files: see `auto-mode-alist'
  '((lambda ()
      (modify-syntax-entry ?* ".")          ; `*' is punctuation, not a word char
      (setq-local imenu-create-index-function
                  #'asdl-plain--imenu-create-index)
      (add-hook 'after-save-hook #'imenu-flush-cache nil t)
      (add-hook 'after-revert-hook #'imenu-flush-cache nil t)
      (add-hook 'xref-backend-functions #'basic/generic-mode-imenu-xref-backend nil t)))
  "A lightweight major mode for Zephyr ASDL files.")

(provide 'asdl-plain)
;;; asdl-plain.el ends here
