;;; basic.el --- My own convenience commands and helper functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Bo Lin

;; Author: Bo Lin <bo@dreamsphere.org>
;; Keywords: convenience, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These are basically a bunch of functions that can be autoloaded.

;;; Code:

(eval-when-compile (require 'tramp))

(require 'xref)
(require 'imenu)


;;; Xref backend driven by the imenu index
;;;
;;; Enabled globally, after `etags': etags keeps priority in buffers with
;;; a TAGS table; wherever etags declines, the imenu index steps in.
;;; The plain modes for StringTemplate and ASDL add the same function
;;; buffer-locally, so there the imenu index wins even over etags.

(defun basic/imenu-xref-backend ()
  "Return the imenu-based xref backend when the buffer has imenu support."
  (and (or imenu-generic-expression
           imenu-extract-index-name-function
           (not (eq imenu-create-index-function
                    #'imenu-default-create-index-function)))
       'basic/imenu))

;; Appended, so a configured TAGS table keeps priority over the imenu
;; index; buffer-local backends (eglot, elisp-mode) still run first via
;; their local hook value.
(add-hook 'xref-backend-functions #'basic/imenu-xref-backend 'append)

(defun basic/generic-mode-imenu-xref-backend ()
  "Like `basic/imenu-xref-backend', as a distinct function symbol.

The StringTemplate and ASDL plain modes install this buffer-locally.
A function present in both the local and the global hook value runs
only at its global position, after `etags'; the separate symbol avoids
that and keeps the imenu index first in these modes even when a TAGS
table is configured."
  (basic/imenu-xref-backend))

(defun basic/imenu--flatten-index (alist &optional prefix)
  "Flatten imenu ALIST into a list of (PATH LEAF POSITION) entries.

PATH is the fully qualified entry name (dotted with PREFIX for
sub-entries), LEAF is the entry's own name, and POSITION is the
marker or integer recorded in the index.  A subalist may start
with its own (NAME . POSITION) entry; it keeps the unqualified
path."
  (apply #'append
         (mapcar
          (lambda (item)
            (let ((name (car item))
                  (rest (cdr item)))
              (if (imenu--subalist-p item)
                  (let* ((base (if prefix (concat prefix "." name) name))
                        (self (and (consp (car rest)) (atom (cdar rest))
                                   (equal (caar rest) name)
                                   (list (list base name (cdar rest))))))
                    (append self
                            (basic/imenu--flatten-index
                             (if self (cdr rest) rest) base)))
                (list (list (if prefix (concat prefix "." name) name)
                            name
                            rest)))))
          alist)))

(defun basic/imenu--entry-match-p (path leaf identifier)
  "Return non-nil if PATH or LEAF is a definition of IDENTIFIER."
  (let* ((base (if (string-match "\\s-*(\\([^)]*\\))\\'" leaf)
                   (substring leaf 0 (match-beginning 0))
                 leaf))
         (bare (if (string-prefix-p "@" base) (substring base 1) base)))
    (or (string= identifier leaf)
        (string= identifier path)
        (string= identifier base)
        (string= identifier bare)
        ;; Region override: @expr.primary matches `primary'.
        (string-suffix-p (concat "." identifier) bare))))

(defun basic/imenu--index-entries ()
  "Return the flattened entries of the current buffer's imenu index."
  (basic/imenu--flatten-index
   (assoc-delete-all "*Rescan*" (imenu--make-index-alist))))
(cl-defmethod xref-backend-definitions ((_backend (eql basic/imenu)) identifier)
  "Find IDENTIFIER among the entries of the current buffer's imenu index."
  (when (buffer-file-name)
    (let ((xrefs nil))
      (dolist (entry (basic/imenu--index-entries))
        (let ((path (nth 0 entry))
              (leaf (nth 1 entry))
              (pos (nth 2 entry)))
          (when (basic/imenu--entry-match-p path leaf identifier)
            (push (xref-make path
                             (xref-make-file-location
                              (buffer-file-name)
                              (line-number-at-pos pos)
                              (save-excursion (goto-char pos)
                                              (- pos (line-beginning-position)))))
                  xrefs))))
      (nreverse xrefs))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql basic/imenu)))
  "Complete against all entry names in the current buffer's imenu index."
  (let ((entries (basic/imenu--index-entries)))
    (delete-dups (append (mapcar #'car entries) (mapcar #'cadr entries)))))
;;;###autoload
(defun bo-add-dir-local-variable ()
  (interactive)
  (let ((dir (ido-read-directory-name "Set var at:")))
    (with-temp-buffer
      (find-file dir)
      (call-interactively 'add-dir-local-variable))))

;;;###autoload
(defun set-tab-width (arg)
  (interactive
   (list (or current-prefix-arg
             (read-number "Set tab-width to: " tab-width))))
  (if (consp arg)
      (setq arg tab-width))
  (if (not (integerp arg))
      (error "set-tab-width requires an explicit argument")
    (message "Tab width set to %d (was %d)" arg tab-width)
    (setq tab-width arg)))

(defun basic/ff--host-path (path remote)
  "Return PATH as seen from the host with tramp prefix REMOTE.
REMOTE is nil for the local host."
  (if (and remote
           (file-name-absolute-p path)
           (not (string-prefix-p remote path)))
      (concat remote path)
    path))

(defun basic/read-stdin-to-buffer (tmpfile)
  "Return a new buffer with the contents of TMPFILE.
TMPFILE names a file on the host that runs Emacs; delete it after
reading it."
  (let ((buffer (create-file-buffer "<stdin>")))
    (unwind-protect
        (condition-case err
            (with-current-buffer buffer
              (insert-file-contents tmpfile)
              (set-buffer-modified-p nil))
          (error
           (kill-buffer buffer)
           (signal (car err) (cdr err))))
      (ignore-errors (delete-file tmpfile)))
    buffer))

;;;###autoload
(defun basic/read-stdin-to-buffer-cmd (tmpfile)
  "Emacs-side function for reading stdin to a buffer.
Meant to be called from e.g. emacsclient or vterm"
  (switch-to-buffer
   (basic/read-stdin-to-buffer
    (basic/ff--host-path tmpfile (file-remote-p default-directory)))))

;;;###autoload
(defun basic/find-file-on-host-cmd (file)
  "Like `find-file', but scoped to the tramp remote prefix of `default-directory'
Meant to be called from e.g. emacsclient or vterm"
  (find-file (basic/ff--host-path file (file-remote-p default-directory))))

;;; Waiting for the `ff' shell script
;;
;; `emacs_cmd' and `vterm_cmd' are one-way, so the shell cannot learn
;; when the editing is done.  In wait mode the shell therefore passes a
;; private directory: we drop `started' into it at once, and `done',
;; holding the exit status, when every buffer opened for the request is
;; finished.

(defvar basic/ff--sessions (make-hash-table :test #'equal)
  "Pending `ff' wait requests, keyed by their done file.
Each value is a list (REMAINING STATUS BUFFERS).")

(defvar-local basic/ff--session-keys nil
  "Done files of the `ff' wait requests that wait for this buffer.")

(defvar basic/ff-verbose t
  "When non-nil, say how to finish a buffer opened by `ff --wait'.")

(defun basic/ff--write (file text)
  "Write TEXT to FILE, atomically when possible.  Never signal an error."
  (condition-case nil
      (let ((tmp (concat file ".tmp")))
        (with-temp-file tmp
          (insert text))
        (rename-file tmp file t))
    (error
     (condition-case err
         (with-temp-file file
           (insert text))
       (error (message "ff: cannot write %s: %s"
                       file (error-message-string err)))))))

(defun basic/ff--finish (status)
  "Finish the `ff' wait requests that wait for the current buffer.
STATUS 1 marks them as failed."
  (let ((keys basic/ff--session-keys))
    (setq basic/ff--session-keys nil)
    (dolist (done keys)
      (let ((session (gethash done basic/ff--sessions)))
        (when session
          (setcar session (1- (car session)))
          (when (= status 1)
            (setcar (cdr session) 1))
          (when (<= (car session) 0)
            (remhash done basic/ff--sessions)
            (basic/ff--write done (format "%d\n" (cadr session)))))))))

(defun basic/ff--kill-hook ()
  "Finish the `ff' wait requests when the current buffer is killed."
  (basic/ff--finish (if (buffer-modified-p) 1 0)))

(defun basic/ff-finish ()
  "Save the buffer and finish the `ff' wait request."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (basic/ff--finish 0)
  (kill-buffer))

(defun basic/ff-cancel ()
  "Cancel the `ff' wait request without saving the buffer."
  (interactive)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard changes and cancel? "))
    (basic/ff--finish 1)
    (set-buffer-modified-p nil)
    (kill-buffer)))

(define-minor-mode basic/ff-mode
  "Minor mode for buffers opened by `ff --wait'.
Finish with \\[basic/ff-finish], which saves and kills the buffer, or
by killing the buffer with \\[kill-buffer]."
  :lighter " ff"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x #") #'basic/ff-finish)
            map))

(defun basic/ff--open (file kind)
  "Open FILE and return its buffer.
FILE names a file on the host that runs Emacs.  KIND is \"file\" for
an ordinary file, or \"stdin\" for a temporary file that holds
standard input."
  (if (equal kind "stdin")
      (basic/read-stdin-to-buffer file)
    (find-file file)))

;;;###autoload
(defun basic/ff-cmd (donedir kind &rest files)
  "Open FILES in this Emacs; the Emacs side of the `ff' shell script.
DONEDIR is a directory on the host where the shell runs.  When it is
not empty, create DONEDIR/started at once and DONEDIR/done when all
buffers opened here are finished; `done' holds \"0\" on success, or
\"1\" on failure.  KIND is \"file\", or \"stdin\" for temporary files
that hold standard input."
  (let* ((remote (file-remote-p default-directory))
         (host (lambda (path) (basic/ff--host-path path remote)))
         (donedir (unless (equal donedir "") (funcall host donedir)))
         (started (and donedir (concat donedir "/started")))
         (done (and donedir (concat donedir "/done")))
         (failed nil)
         buffers)
    (when started
      (basic/ff--write started ""))
    (dolist (file files)
      (let ((buffer (condition-case err
                        (basic/ff--open (funcall host file) kind)
                      (error
                       (setq failed t)
                       (message "ff: %s: %s" file (error-message-string err))
                       nil))))
        (when (and (bufferp buffer) (buffer-live-p buffer))
          (unless (memq buffer buffers)
            (push buffer buffers))
          (when (equal kind "stdin")
            (switch-to-buffer buffer)))))
    (setq buffers (nreverse buffers))
    (cond
     ((null donedir))
     ((null buffers)
      (basic/ff--write done "1\n"))
     (t
      (puthash done (list (length buffers) (if failed 1 0) buffers)
               basic/ff--sessions)
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (unless (member done basic/ff--session-keys)
            (push done basic/ff--session-keys))
          (add-hook 'kill-buffer-hook #'basic/ff--kill-hook nil t)
          (basic/ff-mode 1)))
      (when basic/ff-verbose
        (message "ff: finish with C-x # or by killing the buffer"))))))

(defun basic/ff--flush-sessions ()
  "Give up on all pending `ff' wait requests, e.g. when Emacs exits."
  (maphash (lambda (done _session)
             (basic/ff--write done "1\n"))
           basic/ff--sessions)
  (setq basic/ff--sessions (make-hash-table :test #'equal)))

(add-hook 'kill-emacs-hook #'basic/ff--flush-sessions)

(defun basic/action-from-isearch (action)
  "Invoke `action' on the current isearch query."
  (let ((case-fold-search isearch-case-fold-search)
        (query (cond
                (isearch-regexp isearch-string)
                ((eq isearch-regexp-function 'isearch-symbol-regexp)
                 (format "\\_<%s\\_>" (regexp-quote isearch-string)))
                ((eq isearch-regexp-function 'word-search-regexp)
                 (format "\\<%s\\>" (regexp-quote isearch-string)))
                (t
                 (regexp-quote isearch-string)))))
    (isearch-exit)
    (deactivate-mark)
    (funcall action query)))

;; {{{ highlighting
(defface basic-highlight
  '((((min-colors 88) (background dark)) (:background "tomato4" :extend t))
    (t (:background "red" :extend t)))
  "Face for highlighting region."
  :group 'basic-hi-lock-faces)

;;;###autoload
(defun basic-highlight-region (beg end)
  (let ((ov (make-overlay beg end))
        (face 'basic-highlight))
    (overlay-put ov 'face face)
    (overlay-put ov 'priority -20)
    (overlay-put ov 'basic-highlight t)))

(defun basic-unhighlight-region ()
  (let ((ovs (overlays-at (point)))
        found)
    (dolist (ov ovs)
      (when (overlay-get ov 'basic-highlight)
        (delete-overlay ov)
        (setq found t)))
    found))

;; }}}

;;{{{ goto-char & zap-to-char
;;;###autoload
(defun my-zap-or-goto-char (n char)
  "Forward to and including Nth occurence of CHAR.
Typing `\\[my-zap-to-char-key]' again will forwad to the next Nth
occurence of CHAR, highlighting the region between old and new
point position. Typing `\\[killregion]' will kill the highlighted
region. Typing `\\[keyboard-quit]' will quit. Typing any other
key will continue editing from current point position, leaving
mark behind."
  (interactive "p\ncZap to char: ")
  (let* ((start (point))
         read
         (reverse nil)
         (str (char-to-string char))
         (inhibit-quit t))
    (push-mark start)
    (activate-mark)
    (while
        (progn
          (if reverse
              (search-forward str nil t (- n))
            (search-forward str nil t n))
          (setq read (read-char (concat "Go to '" str "'")))
          (setq reverse nil)
          (if (eq read 127)             ; <backspace>
              (setq reverse t)
            (or (and (characterp read)
                     (char-equal read char))
                (eq (key-binding (vector last-input-event))
                    'my-zap-or-goto-char)))))
    (deactivate-mark)
    (cond
     ((eq (key-binding (vector last-input-event))
          'keyboard-quit) ;C-g
      (pop-mark)
      (goto-char start))
     (t
      (unless (and (characterp read) (char-equal read 13))      ; <return>
        (setq unread-command-events (list last-input-event)))))))
;;}}}

;; {{{ really-toggle-read-only

;;;###autoload
(defun really-toggle-read-only ()
  "Change whether this buffer is visiting its file read-only by really
trying to acquire the rights with sudo (and tramp)"
  (interactive)
  (cond
   ((buffer-modified-p)
    ;; Buffer has pending changes, so don't do anything special:
    (read-only-mode 'toggle))

   ((file-writable-p (buffer-file-name))
    ;; We have write permission to this file, so just go through the regular
    ;; `toggle-read-only':
    (read-only-mode 'toggle))

   (buffer-read-only
    ;; Otherwise, we must add a "sudo" hop:
    (call-interactively 'tramp-revert-buffer-with-sudo))

  (t
   (read-only-mode 'toggle))))

;; }}}


;; {{{ Window nav
;;* Helpers
(require 'windmove)

(defvar basic/min-frame-width-for-right-panel 160
  "Minimum frame width for the right panel to be used.")
(defvar basic/min-frame-height-for-bottom-panel 36
  "Minimum frame height for the bottom panel to be used.")
(defvar basic/min-frame-width-for-left-panel 220
  "Minimum frame width for the left panel to be used.")

(defconst basic--side-window-additional-parameters
  '((window-parameters
     (no-delete-other-windows . t)
     (delete-window . basic--maybe-save-state-then-delete))))

(defun basic--main-window-mru-child ()
  "Return the most recently used child window of the main window, or nil if
there is none."
  (let (mru-win)
    (let ((max-time -1))
      (walk-window-subtree
       (lambda (w)
         (let ((time (window-use-time w)))
           (when (> time max-time)
             (setq max-time time
                   mru-win w))))
       (window-main-window)))
    mru-win))

(defun display-buffer--maybe-right-panel (buffer _alist)
  "Display BUFFER in the main slot of the right panel of the selected frame
if the frame is at least `basic/min-frame-width-for-right-panel' columns
 wide. Otherwise, return nil."
  (and (or (window-live-p (window-with-parameter 'window-side 'right))
           (when (>= (frame-width) basic/min-frame-width-for-right-panel)
             (basic--maybe-restore-saved-state-on 'right)
             t))
       (display-buffer-in-side-window
        buffer
        `((side . right)
          (slot . 0)
          (window . root)
          (window-width . 80)
          ,@basic--side-window-additional-parameters))))

(defun display-buffer--maybe-bottom-panel (buffer _alist)
  "Display BUFFER in the bottom panel of the selected frame if the frame is
at least `basic/min-frame-height-for-bottom-panel' lines tall.
Otherwise, return nil."
  (and (or (window-live-p (window-with-parameter 'window-side 'bottom))
           (when (>= (frame-height) basic/min-frame-height-for-bottom-panel)
             (basic--maybe-restore-saved-state-on 'bottom)
             t))
       (display-buffer-in-side-window
        buffer
        `((side . bottom)
          (window . root)
          (window-height . 0.25)
          ,@basic--side-window-additional-parameters))))

(defun display-buffer--maybe-left-panel (buffer _alist)
  "Display BUFFER in the main slot of the left panel of the selected frame
if the frame is at least `basic/min-frame-width-for-left-panel' columns
wide. Otherwise, return nil."
  (and (or (window-live-p (window-with-parameter 'window-side 'left))
           (when (>= (frame-width) basic/min-frame-width-for-left-panel)
             (basic--maybe-restore-saved-state-on 'left)
             t))
       (display-buffer-in-side-window
        buffer
        `((side . left)
          (slot . 0)
          (window . root)
          (window-width . 60)
          ,@basic--side-window-additional-parameters))))

(defun display-buffer--maybe-left-panel-lower (buffer _alist)
  "Display BUFFER in the top slot of the left panel of the selected frame
if the frame is at least `basic/min-frame-width-for-right-panel' columns
wide. Otherwise, return nil."
  (and (or (window-live-p (window-with-parameter 'window-side 'left))
           (when (> (frame-width) basic/min-frame-width-for-left-panel)
             (basic--maybe-restore-saved-state-on 'left)
             t))
       (display-buffer-in-side-window
        buffer
        `((side . left)
          (slot . 1)
          (window . root)
          (window-width . 60)
          ,@basic--side-window-additional-parameters))))

(defvar basic-buffer-move-behavior 'swap
  "How `basic/buf-move-to' moves a buffer to another window.
The value is `swap' (the default), `move', `combine' or `dup'.
`swap' exchanges the buffers of the two windows.  `move' switches the
current window back to the previously displayed buffer.  `combine'
deletes the current window after the move.  `dup' leaves the source
window unchanged, so the buffer appears in both windows.")

(defun basic--direction-to-side (direction)
  "Convert a direction (`up', `down', `left' or `right') to the
   corresponding window side (`top', `bottom', `left' or `right')."
  (cond
   ((eq direction 'up) 'top)
   ((eq direction 'down) 'bottom)
   ((eq direction 'left) 'left)
   ((eq direction 'right) 'right)
   (t (error "Invalid direction %s specified" direction))))

(defun basic--side-to-state-key (side)
  "Convert a window side (`top', `bottom', `left' or `right') to the
   corresponding frame parameter key for saving the window state."
  (cond
   ((eq side 'top) 'top-window-state)
   ((eq side 'bottom) 'bottom-window-state)
   ((eq side 'left) 'left-window-state)
   ((eq side 'right) 'right-window-state)
   (t (error "Invalid side %s specified" side))))

(defun basic--find-other-window (direction)
  "Like `windmove-find-other-window', but understands the difference
between side panels and main window area."
  (let ((cur-side (window-parameter (selected-window) 'window-side)))
    (cond
     ;; if we're moving into the main window area from a side panel, then select
     ;; the mru child of the main window
     ((or
       (and (eq direction 'up) (eq cur-side 'bottom))
       (and (eq direction 'down) (eq cur-side 'top))
       (and (eq direction 'left) (eq cur-side 'right))
       (and (eq direction 'right) (eq cur-side 'left)))
      (basic--main-window-mru-child))
     ;; otherwise, just delegate to the regular `windmove-find-other-window':
     (t (windmove-find-other-window direction)))))

(defun basic--do-window-select (direction)
  "Select the window in the given direction"
  (let ((other-window (basic--find-other-window direction)))
    (cond
     ((null other-window)
      (user-error "No window in this direction"))
     ((and (window-minibuffer-p other-window)
           (not (minibuffer-window-active-p other-window)))
      (user-error "Can not focus an inactive minibuffer"))
     (t
      (select-window other-window)))))

(defun basic/buf-move-to (direction)
  "Helper function to move the current buffer to the window in the given
   direction (must be \='up, \='down, \='left or \='right). An error is
   thrown, if no window exists in this direction."
  (cl-flet ((window-settings (window)
              (list (window-buffer window)
                    (window-start window)
                    (window-hscroll window)
                    (window-point window)))
            (set-window-settings (window settings)
              (cl-destructuring-bind (buffer start hscroll point)
                  settings
                (set-window-buffer window buffer)
                (set-window-start window start)
                (set-window-hscroll window hscroll)
                (set-window-point window point))))
    (let* ((this-window (selected-window))
           (this-window-settings (window-settings this-window))
           (other-window (basic--find-other-window direction))
           (other-window-settings (window-settings other-window)))
      (cond
       ((or (null other-window) (window-minibuffer-p other-window))
        (if (or (window-parameter this-window 'window-side)
                (eq basic-buffer-move-behavior 'combine))
            (user-error "No window in this direction")
          (setq other-window
                (cond
                 ((eq direction 'up)
                  (split-window this-window nil 'above))
                 ((eq direction 'down)
                  (split-window this-window nil 'below))
                 ((eq direction 'left)
                  (split-window this-window nil 'left))
                 ((eq direction 'right)
                  (split-window this-window nil 'right))
                 (t (user-error "Invalid direction %s specified" direction)))

                other-window-settings (window-settings other-window))
          (when (null other-window)
            (user-error "Can not create new window in this direction"))))
       ((not (memq (window-dedicated-p other-window) '(nil side)))
        (user-error "The window in this direction is dedicated")))

      (set-window-settings other-window this-window-settings)

      (cond
       ((eq basic-buffer-move-behavior 'move)
        (switch-to-prev-buffer this-window)
        (select-window other-window))
       ((eq basic-buffer-move-behavior 'swap)
        (set-window-settings this-window other-window-settings)
        (select-window other-window))
       ((eq basic-buffer-move-behavior 'combine)
        (switch-to-prev-buffer this-window)
        (if (eq (window-dedicated-p this-window) 'side)
            (basic/toggle-window-on-side
             (window-parameter this-window 'window-side))
          (delete-window this-window))
        (select-window other-window))
       ;; fall-through: 'dup
       )

      other-window
      )))

(defun basic--top-level-window-on (side)
  "Return the top-level window on the given side, or nil if there is no
such window."
  (let ((window (window-with-parameter 'window-side side)))
    (while (and window (window-parent window)
                (window-parameter (window-parent window) 'window-side))
      (setq window (window-parent window)))
    window
    ))

(defun basic--maybe-restore-saved-state-on (side)
  "If there is a saved state for the side window on `side', and there is
sufficient space in the current frame layout, restore it and return the
window. Otherwise, return nil."
  (let ((ignore-window-parameters t)
        (window-combination-resize nil)
        (window--sides-inhibit-check t)
        (saved-state
         (frame-parameter (window-normalize-frame nil)
                          (basic--side-to-state-key side)))
        window window-state height width)
    (and saved-state
         (setq window-state (nth 0 saved-state))
         (setq height (nth 1 saved-state))
         (setq width (nth 2 saved-state))
         (cond
          ((memq side '(top bottom))
           (>= (window-total-height (window-main-window))
               (+ height window-min-height)))
          ((memq side '(left right))
           (>= (window-total-width (window-main-window))
               (+ width window-min-width)))
          (t (error "Invalid side %s specified" side)))
         (setq window
               (display-buffer-in-side-window
                (current-buffer)
                `((side . ,side)
                  (window . root)
                  (window-parameters . ((no-delete-other-windows . t))))))
         (window-live-p window)
         (let ()
           (cond
            ((memq side '(top bottom))
             (window-resize window (- height (window-total-height window)) nil))
            ((memq side '(left right))
             (window-resize window (- width (window-total-width window)) t)))
           (let ((window-restore-killed-buffer-windows t))
             (window-state-put window-state window))
           (unless (window-live-p window)
             (set-frame-parameter (window-normalize-frame nil)
                                  (basic--side-to-state-key side) nil)
             (error "%s side window state corrupted." side))
           (set-window-parameter window 'delete-window
                                 'basic--maybe-save-state-then-delete)
           (set-window-dedicated-p window 'side)
           window))))

(defun basic--save-side-window-state (side)
  "Save the state of the side window on `side' in the frame parameters."
  (let ((window (basic--top-level-window-on side)))
    (when window
      (let* ((window-state (window-state-get window))
             (height (window-total-height window))
             (width (window-total-width window))
             (state-key (basic--side-to-state-key side))
             (ignore-window-parameters t))
        (set-frame-parameter
         (window-normalize-frame nil) state-key
         (list window-state height width)))
      window)))

(defun basic--maybe-save-state-then-delete (window)
  "If WINDOW is a side window, save its state in the frame parameters.
Either way, delete WINDOW."
  (let ((side (window-parameter window 'window-side))
        (window--sides-inhibit-check t)
        (ignore-window-parameters t))
    (when side
      (basic--save-side-window-state side))
    (delete-window window)))

;; Like `window-toggle-side-windows', but only toggles the window on the
;; specified side.
;;;###autoload
(defun basic/toggle-window-on-side (side &optional no-select)
  "Toggle the current frame's side window on SIDE."
  (interactive)
  (let ((window--sides-inhibit-check t)
        (ignore-window-parameters t)
        window)
    (cond
     ((setq window (basic--save-side-window-state side))
      (delete-window window))
     (t
      (let ((window (basic--maybe-restore-saved-state-on side)))
        (if window
            (unless no-select
              (select-window window))
          (error "No previous state for %s panel, \
or not enough space to restore it" side))))
     )))

(defun basic/reset-side-window-states ()
  "Reset the saved states for all side windows in the current frame."
  (interactive)
  (set-frame-parameter (window-normalize-frame nil) 'top-window-state nil)
  (set-frame-parameter (window-normalize-frame nil) 'bottom-window-state nil)
  (set-frame-parameter (window-normalize-frame nil) 'left-window-state nil)
  (set-frame-parameter (window-normalize-frame nil) 'right-window-state nil))

(defun basic/focus-side-window (side)
  (let ((window (window-with-parameter 'window-side side)))
    (if (and (window-live-p window)
             (not (eq window (selected-window))))
        (select-window window)
      (basic/toggle-window-on-side side))))

;;;###autoload
(defun basic/focus-left-side-window ()
  (interactive)
  (basic/focus-side-window 'left))

;;;###autoload
(defun basic/focus-right-side-window ()
  (interactive)
  (basic/focus-side-window 'right))

;;;###autoload
(defun basic/focus-top-side-window ()
  (interactive)
  (basic/focus-side-window 'top))

;;;###autoload
(defun basic/focus-bottom-side-window ()
  (interactive)
  (basic/focus-side-window 'bottom))

;;;###autoload
(defun basic/focus-window-left ()
  (interactive)
  (basic--do-window-select 'left))

;;;###autoload
(defun basic/focus-window-right ()
  (interactive)
  (basic--do-window-select 'right))

;;;###autoload
(defun basic/focus-window-up ()
  (interactive)
  (basic--do-window-select 'up))

;;;###autoload
(defun basic/focus-window-down ()
  (interactive)
  (basic--do-window-select 'down))

;;;###autoload
(defun basic/buf-move-up ()
  (interactive)
  (let ((basic-buffer-move-behavior 'move)) (basic/buf-move-to 'up)))

;;;###autoload
(defun basic/buf-move-down ()
  (interactive)
  (let ((basic-buffer-move-behavior 'move)) (basic/buf-move-to 'down)))

;;;###autoload
(defun basic/buf-move-left ()
  (interactive)
  (let ((basic-buffer-move-behavior 'move)) (basic/buf-move-to 'left)))

;;;###autoload
(defun basic/buf-move-right ()
  (interactive)
  (let ((basic-buffer-move-behavior 'move)) (basic/buf-move-to 'right)))

;;;###autoload
(defun basic/buf-combine-up ()
  (interactive)
  (let ((basic-buffer-move-behavior 'combine)) (basic/buf-move-to 'up)))

;;;###autoload
(defun basic/buf-combine-down ()
  (interactive)
  (let ((basic-buffer-move-behavior 'combine)) (basic/buf-move-to 'down)))

;;;###autoload
(defun basic/buf-combine-left ()
  (interactive)
  (let ((basic-buffer-move-behavior 'combine)) (basic/buf-move-to 'left)))

;;;###autoload
(defun basic/buf-combine-right ()
  (interactive)
  (let ((basic-buffer-move-behavior 'combine)) (basic/buf-move-to 'right)))

;;;###autoload
(defun basic/buf-dup-up ()
  (interactive)
  (let ((basic-buffer-move-behavior 'dup)) (basic/buf-move-to 'up)))

;;;###autoload
(defun basic/buf-dup-down ()
  (interactive)
  (let ((basic-buffer-move-behavior 'dup)) (basic/buf-move-to 'down)))

;;;###autoload
(defun basic/buf-dup-left ()
  (interactive)
  (let ((basic-buffer-move-behavior 'dup)) (basic/buf-move-to 'left)))

;;;###autoload
(defun basic/buf-dup-right ()
  (interactive)
  (let ((basic-buffer-move-behavior 'dup)) (basic/buf-move-to 'right)))

;;;###autoload
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "P")
  (when (null arg)
    (setq arg 8))
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

;;;###autoload
(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "P")
  (when (null arg)
    (setq arg 8))
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

;;;###autoload
(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "P")
  (when (null arg)
    (setq arg 8))
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

;;;###autoload
(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "P")
  (when (null arg)
    (setq arg 8))
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(require 'rect)

;;;###autoload
(defun hydra-ex-point-mark ()
  "Exchange point and mark."
  (interactive)
  (if rectangle-mark-mode
      (rectangle-exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

(defvar my-window-control-map (make-sparse-keymap))
(define-key my-window-control-map (kbd "o") 'other-window)
(define-key my-window-control-map (kbd "[?\\t]") 'keymap-command-mode)
(define-key my-window-control-map (kbd "w") (lambda () (interactive) t))
(define-key my-window-control-map (kbd "h") 'fm-left-window-or-frame)
(define-key my-window-control-map (kbd "j") 'fm-down-window-or-frame)
(define-key my-window-control-map (kbd "k") 'fm-up-window-or-frame)
(define-key my-window-control-map (kbd "l") 'fm-right-window-or-frame)
(define-key my-window-control-map (kbd "H") 'buf-move-left)
(define-key my-window-control-map (kbd "J") 'buf-move-down)
(define-key my-window-control-map (kbd "K") 'buf-move-up)
(define-key my-window-control-map (kbd "L") 'buf-move-right)
(define-key my-window-control-map (kbd "^") 'enlarge-window)
(define-key my-window-control-map (kbd "_") 'shrink-window)
(define-key my-window-control-map (kbd "-") 'fit-window-to-buffer)
(define-key my-window-control-map (kbd "=") 'balance-windows)
(define-key my-window-control-map "{" 'shrink-window-horizontally)
(define-key my-window-control-map "}" 'enlarge-window-horizontally)
(define-key my-window-control-map "<" 'scroll-left)
(define-key my-window-control-map ">" 'scroll-right)

;; }}}

(provide 'basic)
;;; basic.el ends here
