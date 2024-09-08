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
(defun my-shell (&optional buffer)
  "Use `default-directory' of current buffer as working dir of `*shell*' buffer.
Also, switch to `*shell*' buffer.
Optional arg BUFFER has same meaning as for command `shell'."
  (interactive ; code taken from `shell'
   (list
    (if current-prefix-arg
        (read-buffer "Shell buffer: "
                     (generate-new-buffer-name "*shell*")))))
  (unless default-directory
    (error "default-directory of current buffer is nil"))
  (let ((edir (expand-file-name default-directory))
        (cdir
         (concat "cd "
                 (shell-quote-argument (expand-file-name default-directory)))))
    (shell (get-buffer-create (or buffer "*shell*")))
    ;; change directory, unless we are already where we want to be
    (unless (string= edir (expand-file-name default-directory))
      (goto-char (point-max))
      (if (looking-back comint-prompt-regexp nil)
          (progn (insert cdir)
                 (comint-send-input))
        (message "shell `%s' seems to be busy, cannot change directory"
                 (buffer-name))
        (comint-add-to-input-history cdir)))))

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

;;;###autoload
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

;;;###autoload
(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

;;;###autoload
(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

;;;###autoload
(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defvar rectangle-mark-mode)
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
