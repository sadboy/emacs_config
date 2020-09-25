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
    (setq fill-column arg)))

;; {{{ highlighting
(eval-when-compile
  (require 'hi-lock)
  (require 'ido))

;;;###autoload
(defun my-ido-unhighlight-regexp ()
  "Unhighlight a regexp using `ido-completing-read'."
  (interactive)
  (let ((regexp
         (ido-completing-read "Regexp to unhighlight: "
                              (mapcar 'car hi-lock-interactive-patterns)
                              t t)))
    (hi-lock-unface-buffer regexp)))

;; (defvar highlight-faces (list 'hi-gold
;;                               'hi-purple
;;                               'hi-pink
;;                               'hi-green
;;                               'hi-skyblue
;;                               'hi-red)
;;   "List of available faces for highlighting.")
;; (make-variable-buffer-local 'highlight-faces)
;; ;;;###autoload
;; (defun next-highlight-face ()
;;   (if current-prefix-arg
;;       (intern (ido-completing-read "Highlight face: "
;;                                    (mapcar 'symbol-name highlight-faces)
;;                                    t t))
;;     (let ((face (car highlight-faces))
;;           (rest (cdr highlight-faces)))
;;       (setq highlight-faces (nconc rest (cons face nil)))
;;       face)))
;; (eval-after-load "hi-lock"
;;   '(defalias 'hi-lock-read-face-name 'next-highlight-face))
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
         (overlay (make-overlay start start))
         read
         (reverse nil)
         (str (char-to-string char))
         (inhibit-quit t))
    (overlay-put overlay 'face 'highlight)
    (while
        (progn
          (if reverse
              (search-forward str nil t (- n))
            (search-forward str nil t n))
          (move-overlay overlay start (point))
          (setq read (read-char (concat "Go to '" str "'")))
          (setq reverse nil)
          (if (eq read 127)             ; <backspace>
              (setq reverse t)
            (or (and (characterp read)
                     (char-equal read char))
                (eq (key-binding (vector last-input-event))
                    'my-zap-or-goto-char)))))
    (delete-overlay overlay)
    (cond
     ((eq (key-binding (vector last-input-event))
          'keyboard-quit) ;C-g
      (goto-char start))
     (t
      (unless (eq start (point))
        (push-mark start))
      (unless (char-equal read 13)      ; <return>
        (setq unread-command-events (list last-input-event)))))))
;;}}}

;; {{{ really-toggle-read-only

;;;###autoload
(defun really-toggle-read-only (&optional arg)
  "Change whether this buffer is visiting its file read-only by really
trying to acquire the rights with sudo (and tramp)"
  (interactive "P")
  (let*
      ((currentfilename (buffer-file-name))
       (currentpoint (point))
       (newfilename
        (and currentfilename
             (not (buffer-modified-p))
             (cond
              ((string-match tramp-file-name-regexp currentfilename)
               (with-parsed-tramp-file-name currentfilename fn
                 (and (not buffer-read-only)
                      (string= "sudo" fn-method)
                      (string= "localhost" fn-host)
                      fn-localname)))
              (t
               (and buffer-read-only
                    (not (file-writable-p currentfilename))
                    (make-tramp-file-name :method "sudo"
                                          :user "root"
                                          :host "localhost"
                                          :localname currentfilename)))))))

    (if (not newfilename)
        (call-interactively 'toggle-read-only)
      (let ((oldbuf (current-buffer))
            (bufname (buffer-name))
            (hasclients (if (boundp 'server-buffer-clients)
                            server-buffer-clients
                          nil))
            (newbuf (find-file-noselect newfilename)))
        (rename-buffer (concat bufname ".toggle") t)
        (switch-to-buffer newbuf)
        (rename-buffer bufname)
        (goto-char currentpoint)
        (recenter)
        (and buffer-read-only
             view-read-only
             (view-mode t))
        (unless hasclients
          (kill-buffer oldbuf))
        (message "Toggled to new filename: %s" newfilename)))))
;; }}}

;; {{{ Auxiliary functions
;;;###autoload
(defun keymap-command-mode (keymap &optional dolast prompt)
  "Makes commands in a keymap repeatable without pressing prefixes."
  (when dolast
    (let ((cmd (lookup-key keymap (vector last-command-event))))
      (if cmd
          (call-interactively cmd)
        (error "Unable to locate %s in keymap" last-command-event))))
  (and prompt (message prompt))
  (while
      (let ((cmd (lookup-key keymap (vector (read-event)))))
        (if (commandp cmd)
            (or (condition-case err
                    (call-interactively cmd)
                  (error (message (cadr err)))) t)
          (unless (eq cmd 'keymap-command-mode)
            (setq unread-command-events (list last-input-event)))
          nil))))
;; }}}

(defun my-server-visit (files &optional mode))

;; {{{ Window nav
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

;;;###autoload
(defun my-window-control-cmd ()
  (interactive)
  (keymap-command-mode my-window-control-map t "Window Control"))

;;;###autoload
(defun my-other-window ()
  (interactive)
  (other-window 1 t))
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

(defun my-display-buffer-find-some-window (buf alist))

;;;###autoload
(defun window-history-back (&optional win)
  (interactive)
  (let* ((prev-bufs (window-prev-buffers win))
         (target (car prev-bufs)))
    (if target
        (let ((buffer (car target))
              (winpos (cadr target))
              (ptpos (car (cddr target))))
          (when (buffer-live-p buffer)
            (let ((window-buffer-need-record 'next))
              (set-window-buffer win buffer))
            (set-window-start win winpos)
            (set-window-point win ptpos))
          (set-window-prev-buffers win (cdr prev-bufs)))
      (message "Backward history empty."))))

;;;###autoload
(defun window-history-forward (&optional win)
  (interactive)
  (let* ((next-bufs (window-next-buffers win))
         (target (car next-bufs)))
    (if target
        (let ((buffer (car target))
              (winpos (cadr target))
              (ptpos (car (cddr target))))
          (when (buffer-live-p buffer)
            (set-window-buffer win buffer)
            (set-window-start win winpos)
            (set-window-point win ptpos))
          (set-window-next-buffers win (cdr next-bufs)))
      (message "Forward history empty."))))

;;;###autoload
(defun other-window-history-back ()
  (interactive)
  (let ((win (next-window nil nil 'visible)))
    (when win
      (window-history-back win))))

;;;###autoload
(defun other-window-history-forward ()
  (interactive)
  (let ((win (next-window nil nil 'visible)))
    (when win
      (window-history-forward win))))

;;;###autoload
(defvar window-history-navigation-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "<" 'window-history-back)
    (define-key keymap ">" 'window-history-forward)
    (define-key keymap "M-<" 'other-window-history-back)
    (define-key keymap "M->" 'other-window-history-forward)
    keymap))

;;;###autoload
(defun window-history-navigation ()
  (interactive)
  (keymap-command-mode window-history-navigation-map t "Buffer history:"))
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
