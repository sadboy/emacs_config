;;; jinja2-plain.el --- Jinja2-only fontification for templates  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bo Lin

;; Author: Bo Lin <bo@dreamsphere.org>
;; Keywords: languages, jinja2

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

;; Major mode for Jinja2 templates.  It fontifies Jinja2 constructs
;; only, so text outside Jinja2 tags stays plain and HTML is not
;; highlighted.
;;
;; Imenu lists the {% macro %} and {% block %} definitions, grouped as
;; "Macros" and "Blocks", so a definition can be jumped to by name.
;; `beginning-of-defun' and `end-of-defun' move over those definitions.
;;
;; Limitations: {% raw %} blocks are not special-cased, and numbers
;; inside a tag are not fontified.

;;; Code:

(defgroup jinja2-plain nil
  "Jinja2-only highlighting."
  :group 'languages
  :prefix "jinja2-plain-")

(defconst jinja2-plain--tag-names
  '("if" "elif" "else" "endif"
    "for" "endfor" "break" "continue" "recursive"
    "block" "endblock" "scoped" "required"
    "extends" "include" "import" "from" "as"
    "macro" "endmacro" "call" "endcall"
    "filter" "endfilter" "set" "endset"
    "with" "endwith" "without" "context"
    "autoescape" "endautoescape" "raw" "endraw"
    "trans" "endtrans" "pluralize" "do"
    "and" "or" "not" "in" "is" "loop" "self"
    "true" "false" "none" "True" "False" "None"))

(defconst jinja2-plain--keyword-re
  (concat "\\_<\\(?:" (regexp-opt jinja2-plain--tag-names) "\\)\\_>"))

(defconst jinja2-plain--filter-re
  "|\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)")

(defconst jinja2-plain--name-re
  "\\([[:alpha:]_][[:alnum:]_]*\\(?:\\.[[:alpha:]_][[:alnum:]_]*\\)*\\)")

(defconst jinja2-plain--opener-re "{[{%]-?")

(defconst jinja2-plain--macro-re
  "{%-?[[:space:]]*macro[[:space:]]+\\([[:alpha:]_][[:alnum:]_]*\\)"
  "Regexp matching a Jinja2 macro definition; group 1 is the macro name.")

(defconst jinja2-plain--block-re
  "{%-?[[:space:]]*block[[:space:]]+\\([[:alpha:]_][[:alnum:]_]*\\)"
  "Regexp matching a Jinja2 block definition; group 1 is the block name.")

(defconst jinja2-plain--defun-re
  (concat "{%-?[[:space:]]*"
          "\\(?:\\(macro\\|block\\)[[:space:]]+[[:alpha:]_][[:alnum:]_]*"
          "\\(?:[^%]\\|%[^}]\\)*?"
          "\\|end\\(macro\\|block\\)[[:space:]]*\\)"
          "[-]?%}")
  "Regexp matching a Jinja2 macro or block tag.
Group 1 is the kind of an opening tag, group 2 the kind of a closing
tag.")

(defconst jinja2-plain--string-re "\\(\"[^\"]*\"\\|'[^']*'\\)")

(defun jinja2-plain--put-face (regexp subexp face beg end)
  "Put FACE on SUBEXP of every REGEXP match between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward regexp end t)
      (put-text-property (match-beginning subexp) (match-end subexp) 'face face))))

(defun jinja2-plain--in-comment-p (pos)
  "Return non-nil if POS is inside a Jinja2 comment.
Point and the match data are preserved, because `syntax-ppss' can move
point and can run `syntax-propertize-function'."
  (save-excursion
    (save-match-data
      (nth 4 (syntax-ppss pos)))))

(defun jinja2-plain--fontify-tag (limit)
  "Fontify the next Jinja2 tag before LIMIT and return non-nil if found.
Fontify the tag body and the closing delimiter, leave point after the
tag, and set match data to the opening delimiter."
  (catch 'found
    (while (re-search-forward jinja2-plain--opener-re limit t)
      (let ((open (match-beginning 0)))
        (unless (jinja2-plain--in-comment-p (1+ open))
          (let* ((dash (eq (char-after (+ open 2)) ?-))
                 (body-beg (if dash (+ open 3) (+ open 2)))
                 (close (if (eq (char-after (1+ open)) ?%) "%}" "}}"))
                 (end (save-excursion
                        (goto-char body-beg)
                        (if (re-search-forward close nil t) (point) body-beg))))
            (when (> end body-beg)
              (let ((body-end (max body-beg (- end 2))))
                (jinja2-plain--put-face jinja2-plain--name-re 1
                                        'font-lock-variable-name-face body-beg body-end)
                (jinja2-plain--put-face jinja2-plain--filter-re 1
                                        'font-lock-function-name-face body-beg body-end)
                (jinja2-plain--put-face jinja2-plain--keyword-re 0
                                        'font-lock-keyword-face body-beg body-end)
                ;; A Jinja2 string hides the names and keywords it contains.
                (jinja2-plain--put-face jinja2-plain--string-re 1
                                        'font-lock-string-face body-beg body-end)
                (put-text-property body-end end 'face 'font-lock-preprocessor-face))
              (when (>= end (line-beginning-position 2))
                (put-text-property open end 'font-lock-multiline t)))
            (goto-char end)
            (set-match-data (list open body-beg))
            (throw 'found t)))))
    nil))

(defun jinja2-plain--imenu-items (regexp)
  "Return an alist of (NAME . POSITION) for every REGEXP definition.
Definitions inside comments are skipped."
  (let ((items nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((name (match-string-no-properties 1))
              (pos (match-beginning 0)))
          (unless (jinja2-plain--in-comment-p pos)
            (push (cons name pos) items)))))
    (nreverse items)))

(defun jinja2-plain--imenu-create-index ()
  "Return a grouped Imenu index of the Jinja2 definitions in the buffer.
Macros and blocks form the groups; a group with no definitions is left
out."
  (let ((index nil))
    (dolist (group (list (cons "Macros" jinja2-plain--macro-re)
                         (cons "Blocks" jinja2-plain--block-re)))
      (let ((items (jinja2-plain--imenu-items (cdr group))))
        (when items
          (push (cons (car group) items) index))))
    (nreverse index)))

(defun jinja2-plain--defun-tag-p ()
  "Return non-nil when the last regexp match is a definition tag.
A tag inside a Jinja2 comment does not count."
  (not (jinja2-plain--in-comment-p (match-beginning 0))))

(defun jinja2-plain--defun-opening-at-point ()
  "Return the position of the definition opener that starts at point.
Return nil when point does not start a definition.  Point and the match
data are preserved."
  (save-excursion
    (when (and (looking-at jinja2-plain--defun-re)
               (match-string 1)
               (jinja2-plain--defun-tag-p))
      (point))))

(defun jinja2-plain--defun-end-position (pos)
  "Return the position just past the definition opened at POS.
Return nil when POS does not open a definition or when the closing tag
is missing.  Definitions of the same kind nest, so their tags count."
  (save-excursion
    (goto-char pos)
    (when (re-search-forward jinja2-plain--defun-re nil t)
      (let ((kind (match-string 1))
            (depth 1)
            (end nil))
        (while (and kind (not end) (re-search-forward jinja2-plain--defun-re nil t))
          (when (jinja2-plain--defun-tag-p)
            (cond ((equal (match-string 1) kind) (setq depth (1+ depth)))
                  ((equal (match-string 2) kind)
                   (setq depth (1- depth))
                   (when (zerop depth) (setq end (point)))))))
        end))))

(defun jinja2-plain--enclosing-defun-opening (pos)
  "Return the position of the definition opener that contains POS.
Return nil when POS is not inside a definition."
  (save-excursion
    (goto-char pos)
    (let ((depth 0) (open nil))
      (while (and (not open) (re-search-backward jinja2-plain--defun-re nil t))
        (when (jinja2-plain--defun-tag-p)
          (cond ((match-string 2) (setq depth (1+ depth)))
                ((match-string 1)
                 (if (> depth 0)
                     (setq depth (1- depth))
                   (setq open (match-beginning 0)))))))
      open)))

(defun jinja2-plain--previous-defun-opening ()
  "Move to the start of the nearest definition opener before point.
Return non-nil when point moved."
  (let ((start (point)))
    (catch 'found
      (while (re-search-backward jinja2-plain--defun-re nil t)
        (when (and (jinja2-plain--defun-tag-p) (match-string 1))
          (goto-char (match-beginning 0))
          (throw 'found t)))
      (goto-char start)
      nil)))

(defun jinja2-plain--next-defun-opening ()
  "Move to the start of the next definition opener after point.
Return non-nil when point moved."
  (let ((start (point))
        (found nil))
    (while (and (not found) (re-search-forward jinja2-plain--defun-re nil t))
      (when (and (jinja2-plain--defun-tag-p)
                 (match-string 1)
                 (> (match-beginning 0) start))
        (goto-char (match-beginning 0))
        (setq found t)))
    (unless found (goto-char start))
    found))

(defun jinja2-plain--beginning-of-defun (&optional arg)
  "Move point to the beginning of the ARGth previous Jinja2 definition.
A definition starts at a {% macro %} or a {% block %} tag.  A negative
ARG moves forward to the ARGth following definition instead.  Return
non-nil if point moved."
  (let ((count (or arg 1))
        (moved nil))
    (if (< count 0)
        (while (< count 0)
          (setq count (1+ count))
          (when (jinja2-plain--next-defun-opening) (setq moved t)))
      (while (> count 0)
        (setq count (1- count))
        (when (jinja2-plain--previous-defun-opening) (setq moved t))))
    moved))

(defun jinja2-plain--end-of-defun (&optional _arg)
  "Move point to the end of the Jinja2 definition at point.
Use the definition that contains point, or else the next one that
starts at or after point.  Return non-nil if a definition was found."
  (let* ((start (point))
         (open (or (jinja2-plain--defun-opening-at-point)
                   (jinja2-plain--enclosing-defun-opening start)
                   (and (jinja2-plain--next-defun-opening) (point))))
         (end (and open (jinja2-plain--defun-end-position open))))
    (if (not end)
        (progn (goto-char start)
               nil)
      (goto-char end)
      ;; Stand at the start of the next line when the rest of the
      ;; closing tag's line is blank.
      (skip-chars-forward " \t")
      (when (eolp) (forward-line 1))
      t)))

(defconst jinja2-plain-font-lock-keywords
  '((jinja2-plain--fontify-tag (0 'font-lock-preprocessor-face))))

;;;###autoload
(define-derived-mode jinja2-plain-mode prog-mode "J2"
  "Major mode for Jinja2 templates with Jinja2-only highlighting."
  :syntax-table (let ((st (make-syntax-table prog-mode-syntax-table)))
                  (modify-syntax-entry ?' "." st)
                  (modify-syntax-entry ?\" "." st)
                  (modify-syntax-entry ?- "." st)
                  st)
  (setq-local font-lock-defaults '(jinja2-plain-font-lock-keywords))
  (setq-local font-lock-multiline t)
  (setq-local imenu-create-index-function #'jinja2-plain--imenu-create-index)
  (setq-local beginning-of-defun-function #'jinja2-plain--beginning-of-defun)
  (setq-local end-of-defun-function #'jinja2-plain--end-of-defun)
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               ("\\({\\)#" (1 "<"))
               ("#\\(}\\)" (1 ">")))))

(provide 'jinja2-plain)
;;; jinja2-plain.el ends here
