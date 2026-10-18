;;; stringtemplate-plain.el --- StringTemplate-only fontification  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bo Lin

;; Author: Bo Lin <bo@dreamsphere.org>
;; Keywords: languages, stringtemplate

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

;; Major mode for StringTemplate group files.  It fontifies StringTemplate
;; constructs only, so template bodies stay plain.
;;
;; Imenu lists the template definitions, including dictionaries, aliases and
;; region overrides like @enclosing.region; the imenu index also serves xref
;; through the backend that `basic' installs.

;;; Code:

(require 'generic)
(require 'basic)

(defun stringtemplate-plain--imenu-create-index ()
  "Return an imenu index for the current StringTemplate group buffer.

Template definitions (including dictionaries, aliases, and region
overrides like @enclosing.region) become index entries.  Formal
arguments are shown for context, as in \"name(arg1, arg2)\".
Matches inside comments are ignored.  Positions are markers when
`imenu-use-markers' is non-nil."
  (save-excursion
    (goto-char (point-min))
    (let ((def-re (concat "^[ \t]*"
                          "\\(@?[A-Za-z_][A-Za-z0-9_-]*"
                          "\\(?:\\.[A-Za-z_][A-Za-z0-9_-]*\\)?\\)"
                          "[ \t]*\\(([^)]*)\\)?"
                          "[ \t\n]*::="))
          index)
      (while (re-search-forward def-re nil t)
        (let ((name (match-string-no-properties 1))
              (args (match-string-no-properties 2))
              (beg (match-beginning 1)))
          ;; `syntax-ppss' moves point to POS; keep point at match end.
          (unless (save-excursion (nth 8 (syntax-ppss beg)))
            (when args
              (setq args
                    (mapconcat
                     (lambda (s)
                       (replace-regexp-in-string
                        "\\`[ \t\r\n]+\\|[ \t\r\n]+\\'" "" s))
                     (split-string (substring args 1 -1) ",")
                     ", "))
              (setq name (concat name "(" args ")")))
            (push (cons name (if imenu-use-markers
                                 (copy-marker beg t)
                               beg))
                  index))))
      (nreverse index))))

;;;###autoload
(define-generic-mode 'stringtemplate-mode
  '("//" ("/*" . "*/") ("<!" . "!>"))         ; Comment delimiters
  '("default" "delimiters" "group" "import")  ; StringTemplate keywords
  '(;; Dictionary definitions:  name ::= [ ... ]
    ("\\_<\\([A-Za-z_][A-Za-z0-9_.-]*\\)\\_>[ \t]*::=[ \t]*\\["
     (1 'font-lock-constant-face))
    ;; Definition names (templates, aliases, region overrides like
    ;; @tmpl.region): the name contrasts with the body that follows.
    ("^[ \t]*\\(@?[A-Za-z_][A-Za-z0-9_.-]*\\)[ \t]*\\((\\([^)]*\\))\\)?[ \t]*\\(::=\\)"
     (1 'font-lock-function-name-face)
     (3 'font-lock-variable-name-face nil t)
     (4 'font-lock-keyword-face))
    ;; Header group name:  group Name;
    ("\\_<group\\_>\\s-+\\([A-Za-z_][A-Za-z0-9_.-]*\\)"
     (1 'font-lock-constant-face))
    ;; Conditional subtemplates:  <if(x)> ... <elseif(y)> ... <endif>
    ("[^\\]<\\s-*\\(if\\|elseif\\|else\\|endif\\)\\_>" (1 'font-lock-keyword-face))
    ;; Attribute expressions:  <name>, <a:b()>, <x; separator=", ">.  Comes
    ;; before strings so expressions with embedded quotes stay intact.
    ("[^\\]<\\([^<>%>\" \t\n][^>\n]*\\)>" (1 'font-lock-variable-name-face))
    ;; String literals, including expression-free single-line bodies.  Text
    ;; stays on one line so an unpaired quote cannot swallow the next line.
    ("\"[^\"\\\n]*\\(?:\\\\.[^\"\\\n]*\\)*\"" . 'font-lock-string-face))
  nil                                          ; Files: see `auto-mode-alist'
  '((lambda ()
      (setq-local font-lock-multiline t)  ; multi-line argument lists
      ;; `"' must not open a string: bodies in <<...>> may hold lone quotes.
      (modify-syntax-entry ?\" ".")
      (modify-syntax-entry ?< ".")
      (modify-syntax-entry ?> ".")
      (setq-local imenu-create-index-function
                  #'stringtemplate-plain--imenu-create-index)
      (add-hook 'after-save-hook #'imenu-flush-cache nil t)
      (add-hook 'after-revert-hook #'imenu-flush-cache nil t)
      (add-hook 'xref-backend-functions #'basic/generic-mode-imenu-xref-backend nil t)))
  "A lightweight major mode for StringTemplate files.")

(provide 'stringtemplate-plain)
;;; stringtemplate-plain.el ends here
