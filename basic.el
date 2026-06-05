;;; basic.el --- My own convenience commands and helper functions

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

(require 'tramp)

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

(defun basic/action-from-isearch (action)
  "Invoke `action' on the current isearch query."
  (let ((case-fold-search isearch-case-fold-search)
        (query (if isearch-regexp
                   isearch-string
                 (if (or (eq isearch-regexp-function 'isearch-symbol-regexp)
                         (eq isearch-regexp-function 'word-search-regexp))
                     (format "\\b%s\\b" (regexp-quote isearch-string))
                   (regexp-quote isearch-string)))))
    (isearch-exit)
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

(defun basic--is-sudo (filename)
  (and (tramp-tramp-file-p filename)
       (with-parsed-tramp-file-name filename fn
         (and (string= "sudo" fn-method)
              (string= "root" fn-user)
              fn-localname))))

(defun basic--toggle-buffer-to-name (target)
  (let ((oldbuf (current-buffer))
        (currentpoint (point))
        (bufname (buffer-name))
        (hasclients (and (boundp 'server-buffer-clients)
                         server-buffer-clients))
        (newbuf (find-file-noselect target)))
    (rename-buffer (concat bufname ".toggle") t)
    (with-current-buffer newbuf
      (rename-buffer bufname)
      (goto-char currentpoint)
      (and buffer-read-only
           view-read-only
           (view-mode t)))
    (switch-to-buffer newbuf)
    (recenter)
    (unless hasclients
      (kill-buffer oldbuf))
    (message "Toggled to new filename: %s" target)))

;;;###autoload
(defun really-toggle-read-only ()
  "Change whether this buffer is visiting its file read-only by really
trying to acquire the rights with sudo (and tramp)"
  (interactive)
  (cond
   ((buffer-modified-p)
    ;; Buffer has pending changes, so don't do anything special:
    (call-interactively 'read-only-mode))

   ((basic--is-sudo (buffer-file-name))
    ;; We're in a "sudo:root" buffer, so toggle it off:
    (let ((tramp-name (tramp-dissect-file-name (buffer-file-name))))
      (basic--toggle-buffer-to-name (tramp-file-name-localname tramp-name))))

   ((file-writable-p (buffer-file-name))
    ;; We have write permission to this file, so just go through the regular
    ;; `toggle-read-only':
    (call-interactively 'read-only-mode))

   (t
    ;; Otherwise, we must add a "sudo" hop:
    (let* ((currentfilename (buffer-file-name))
           (host (if (tramp-tramp-file-p currentfilename)
                     (with-parsed-tramp-file-name currentfilename fn
                       fn-host)
                   "localhost"))
           (tramp-name (make-tramp-file-name
                        :method "sudo"
                        :user "root"
                        :host host
                        :localname currentfilename)))
      (basic--toggle-buffer-to-name
       (tramp-make-tramp-file-name
        (tramp-file-name-method tramp-name)
        (tramp-file-name-user tramp-name)
        (tramp-file-name-domain tramp-name)
        (tramp-file-name-host tramp-name)
        (tramp-file-name-port tramp-name)
        (tramp-file-name-localname tramp-name)
        (tramp-file-name-hop tramp-name)))))))

;; }}}


;; {{{ Window nav
;;* Helpers
(require 'windmove)

(defvar basic-buffer-move-behavior 'swap
  "If set to 'swap (default), the buffers will be exchanged
  (i.e. swapped), if set to 'move, the current window is switch back to the
  previously displayed buffer (i.e. the buffer is moved)."
)

(defun basic--direction-to-side (direction)
  "Convert a direction (must be 'up, 'down, 'left or 'right) to the
   corresponding window side (must be 'top, 'bottom, 'left or 'right)."
  (cond
   ((eq direction 'up) 'top)
   ((eq direction 'down) 'bottom)
   ((eq direction 'left) 'left)
   ((eq direction 'right) 'right)
   (t (error "Invalid direction %s specified" direction))))

(defun basic/buf-move-to (direction)
  "Helper function to move the current buffer to the window in the given
   direction (must be 'up, 'down, 'left or 'right). An error is thrown,
   if no window exists in this direction."
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
           (other-window (windmove-find-other-window direction))
           (other-window-settings (window-settings other-window)))
      (cond
       ((or (null other-window) (window-minibuffer-p other-window))
        (if (window-parameter this-window 'window-side)
            (error "No window in this direction")
          (basic/toggle-window-on-side
           (basic--direction-to-side direction))
          (select-window this-window)
          (setq other-window (windmove-find-other-window direction)
                other-window-settings (window-settings other-window))
          (when (null other-window)
            (error "Can not create new window in this direction"))))
       ((not (memq (window-dedicated-p other-window) '(nil side)))
        (error "The window in this direction is dedicated")))

      (set-window-settings other-window this-window-settings)
      (select-window other-window)

      (cond
       ((eq basic-buffer-move-behavior 'move)
        (switch-to-prev-buffer this-window))
       ((eq basic-buffer-move-behavior 'swap)
        (set-window-settings this-window other-window-settings))
       ((eq basic-buffer-move-behavior 'combine)
        (if (eq (window-dedicated-p this-window) 'side)
            (basic/toggle-window-on-side
             (window-parameter this-window 'window-side))
          (delete-window this-window)))
       ;; fall-through: 'dup
       )

      other-window
      )))

(defun basic/focus-side-window (side)
  "Focus the side window on the given side (must be 'top, 'bottom, 'left or
'right)."
  (let ((window (window-with-parameter 'window-side side)))
    (if window
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

(defun basic--top-level-window-on-side (side)
  "Return the top-level window on the given side (must be 'top, 'bottom,)
'left or 'right), or nil if there is no such window."
  (let ((window (window-with-parameter 'window-side side)))
    (while (and window (window-parent window)
                (window-parameter (window-parent window) 'window-side))
      (setq window (window-parent window)))
    window
    ))

;; Like `window-toggle-side-windows', but only toggles the window on the
;; specified side.
;;;###autoload
(defun basic/toggle-window-on-side (side &optional no-select)
  "Toggle the bottom side window."
  (interactive)
  (let* ((frame (window-normalize-frame nil))
         (state-key
          (cond
           ((eq side 'top) 'top-window-state)
           ((eq side 'bottom) 'bottom-window-state)
           ((eq side 'left) 'left-window-state)
           ((eq side 'right) 'right-window-state)
           (t (error "Invalid side %s specified" side))))
         (window--sides-inhibit-check t)
         (window (basic--top-level-window-on-side side))
         (saved-state (frame-parameter frame state-key)))

    (cond
     (window
      (let* ((window-state (window-state-get window))
             (height (window-total-height window))
             (width (window-total-width window))
             (saved-state (list window-state height width))
             (ignore-window-parameters t))
        (set-frame-parameter frame state-key saved-state)
        (delete-window window)))
     (saved-state
      (let ((ignore-window-parameters t)
            (window-combination-resize t)
            (window-state (nth 0 saved-state))
            (height (nth 1 saved-state))
            (width (nth 2 saved-state))
            (window
             (display-buffer-in-side-window
              (current-buffer)
              `((side . ,side)
                (window . root)
                (window-parameters . ((no-delete-other-windows . t)))))))
        (window-state-put window-state window)
        ;; (set-window-dedicated-p window 'side)
        (cond
         ((memq side '(top bottom))
          (window-resize window (- height (window-total-height window)) nil))
         ((memq side '(left right))
          (window-resize window (- width (window-total-width window)) t)))
        (unless no-select
          (select-window window))))
     (t
      (let ((new-win (display-buffer-in-side-window
                      (other-buffer (current-buffer))
                      `((side . ,side)
                        (slot . 0)
                        (window . root)
                        (window-width . 80)
                        (window-height . 0.25)
                        (window-parameters . ((no-delete-other-windows . t)))))))
        ;; (unless (null new-win)
        ;;   (set-window-dedicated-p new-win 'side))
        new-win))
     )))

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

(eval-when-compile
  (require 'rect)
  (defvar rectangle-mark-mode))

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

;; {{{ Buffer Nav
(defvar window-buffer-need-record 'prev)
(defun record-window-buffer (&optional window)
  "Record WINDOW's buffer.
WINDOW must be a live window and defaults to the selected one."
  (let ((get-buffers 'window-prev-buffers)
        (set-buffers 'set-window-prev-buffers))
    (when (eq window-buffer-need-record 'next)
      (setq get-buffers 'window-next-buffers)
      (setq set-buffers 'set-window-next-buffers))

    (let* ((window (window-normalize-window window t))
           (buffer (window-buffer window))
           (entry (assq buffer (funcall get-buffers window))))
      (when entry
        ;; Remove all entries for BUFFER from WINDOW's previous buffers.
        (funcall set-buffers
                 window (assq-delete-all buffer
                                         (funcall get-buffers window))))

      ;; Don't record insignificant buffers.
      (unless (eq (aref (buffer-name buffer) 0) ?\s)
        (with-current-buffer buffer
          (let ((start (window-start window))
                (point (window-point window)))
            (setq entry
                  (cons buffer
                        (if entry
                            ;; We have an entry, update marker positions.
                            (list (set-marker (nth 1 entry) start)
                                  (set-marker (nth 2 entry) point))
                          ;; Make new markers.
                          (list (copy-marker start)
                                (copy-marker
                                 ;; Preserve window-point-insertion-type
                                 ;; (Bug#12588).
                                 point window-point-insertion-type)))))
            (funcall set-buffers
                     window (cons entry (funcall get-buffers window)))))))))

;; }}}

;; {{{ Help buffer nav
;;;###autoload
(defun help-nav-forward ()
  (interactive)
  (let ((buf (get-buffer "*Help*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (call-interactively 'help-go-forward)))))
;;;###autoload
(defun help-nav-backward ()
  (interactive)
  (let ((buf (get-buffer "*Help*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (call-interactively 'help-go-back)))))
;; }}}

(provide 'basic)
;;; basic.el ends here
