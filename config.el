(eval-when-compile
  (straight-use-package 'use-package)
  (straight-use-package 'hydra)
  (straight-use-package 'project)
  (require 'cl-lib)
  (require 'use-package)
  (require 'hydra))

(setq-default abbrev-mode t
              bidi-paragraph-direction 'left-to-right
              major-mode 'text-mode
              indent-tabs-mode nil
              tab-width 8
              fill-column 76
              default-directory "~/")

(setq display-buffer-base-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer-use-some-window
         display-buffer--maybe-pop-up-frame-or-window
         display-buffer-pop-up-window)
        . ((reusable-frames . visible)))

      ;; same-window-regexps '("magit:.*")
      )
(setq package-enable-at-startup nil
      ;; Don't change default temp directory on Mac:
      ;; temporary-file-directory (concat "/tmp/emacs_" (getenv "USER") "/")
      gc-cons-threshold 10000000        ; Increase garbage collection threshold
      sentence-end-without-space "。，？！；……"
      sentence-end-double-space nil
      default-input-method "TeX"
      gnus-init-file "~/emacs/lisp/basic/gnus-settings"

      view-read-only t                  ; Open read-only files in view mode
      visible-bell nil
      scroll-conservatively 10000
      isearch-allow-scroll t
      inhibit-startup-message t
      frame-title-format (concat "Emacs" emacs-version "@%b")
      column-number-mode t
      mouse-yank-at-point t
      mouse-wheel-progressive-speed nil
      show-paren-style 'parentheses
      show-paren-delay 0

      enable-local-variables :safe

      delete-auto-save-files t
      make-backup-files nil             ; Don't create ~ files

      truncate-partial-width-windows nil
      even-window-heights nil
      message-log-max 999
      parens-require-spaces nil
      transient-mark-mode nil
      set-mark-command-repeat-pop t
      tramp-default-method "sshx"

      window-min-height 10

      ;; abbrev-file-name "~/emacs/abbrev_defs"
      save-abbrevs t
      vc-follow-symlinks t

      ispell-process-directory (expand-file-name "~/")
      flyspell-issue-message-flag nil
      flymake-start-syntax-check-on-newline nil

      compilation-scroll-output 'first-error

      ;; eldoc-mode
      eldoc-idle-delay 0.2

      ;; minimap-mode:
      minimap-window-location 'right
      minimap-automatically-delete-window nil

      ;; Dired-mode:
      dired-isearch-filenames 'dwim

      ;; recentf-mode:
      recentf-max-saved-items 50

      ;; tls verify:
      tls-checktrust t
      )
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(global-set-key "\C-xQ" 'save-buffers-kill-emacs)
(global-set-key "\C-z" 'undo)

(when (not (file-exists-p temporary-file-directory))
  (make-directory temporary-file-directory t))

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)

(quietly-read-abbrev-file)
(show-paren-mode t)

;; {{{ Global key bindings
(global-set-key "\C-xQ" 'save-buffers-kill-emacs)
(global-set-key "\C-x\C-b" (lambda () (interactive) (ibuffer t nil nil nil t)))
(global-set-key "\C-xg" 'rgrep)
(global-set-key (kbd "M-s M-s") 'isearch-forward-symbol-at-point)

(defvar bo-insert-map (make-sparse-keymap))
(define-key bo-insert-map "f" 'add-file-local-variable)
(define-key bo-insert-map "l" 'add-file-local-variable-prop-line)
(define-key bo-insert-map "a" 'auto-insert)
(define-key bo-insert-map "i" 'insert-file)
(define-key bo-insert-map "d" 'bo-add-dir-local-variable)

(defvar ctrl-x-f-map (make-sparse-keymap))
(define-key ctrl-x-f-map "f" 'ffap)
(define-key ctrl-x-f-map "c" 'set-fill-column)
(define-key ctrl-x-f-map "t" 'set-tab-width)
(define-key ctrl-x-f-map "h" 'hexl-mode)
(define-key ctrl-x-f-map "r" 'revert-buffer)
(define-key ctrl-x-f-map "s" 'magit-status)
(define-key ctrl-x-f-map "b" 'magit-blame)
;; (define-key ctrl-x-f-map "l" 'calendar)
(global-set-key (kbd "C-S-s") 'magit-status)

(defvar ctrl-x-comma-map (make-sparse-keymap))
(define-key ctrl-x-comma-map "c" 'calculator)
(define-key ctrl-x-comma-map "U" 'customize-option)
(define-key ctrl-x-comma-map "a" 'apropos)

(global-set-key "\C-xi" bo-insert-map)
(global-set-key "\C-xf" ctrl-x-f-map)
(global-set-key (kbd "C-x ,") ctrl-x-comma-map)
(global-set-key (kbd "C-S-r") 'revert-buffer)

(global-set-key "\M-z" 'my-zap-or-goto-char)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(global-set-key (kbd "C-<") 'previous-buffer)
(global-set-key (kbd "C->") 'next-buffer)
(global-set-key (kbd "C-M-<") 'other-window-history-back)
(global-set-key (kbd "C-M->") 'other-window-history-forward)
;; For terminal:
(global-set-key (kbd "<f6>") 'revert-buffer)
(global-set-key (kbd "<f7>") 'previous-buffer)
(global-set-key (kbd "<f8>") 'next-buffer)
;; }}}

;; {{{ Builtin packages:
(use-package ffap
  ;; :init
  ;; (ffap-bindings)
  :bind
  (:map ctrl-x-f-map
        ("l" . find-file-literally-at-point)))
(use-package view
  :bind
  (:map view-mode-map
        ("S-SPC" . View-scroll-page-backward)))
(use-package isearch
  :bind
  (:map isearch-mode-map
        ("M-<" . isearch-beginning-of-buffer)
        ("M->" . isearch-end-of-buffer)
        ("C-h" . isearch-highlight-regexp)))
(use-package hi-lock
  :config
  ;; (setq hi-lock-face-defaults
  ;;       '("hi-gold" "hi-red" "hi-purple" "hi-skyblue"
  ;;         "hi-pink" "hi-green" "hi-blue" "hi-black-b"
  ;;         "hi-blue-b" "hi-red-b" "hi-green-b" "hi-black-hb"))
  (setq hi-lock-auto-select-face t)
  :bind
  (:map ctrl-x-comma-map
        ("u" . unhighlight-regexp)))
(use-package grep
  :bind
  (:map grep-mode-map
        ("n" . compilation-next-error)
        ("p" . compilation-previous-error)
        ("M-n" . next-error-no-select)
        ("M-p" . previous-error-no-select)))
(use-package info
  :bind
  (:map Info-mode-map
        ("S-SPC" . Info-scroll-down))
  :config
  (setq Info-directory-list nil
        Info-default-directory-list
        (cons (expand-file-name "~/.local/share/info/")
              Info-default-directory-list))
  )
(use-package whitespace
  :after (doom-themes)
  :config
  (setq whitespace-style '(face trailing indentation))
  (set-face-attribute 'whitespace-trailing nil :background "dim gray")
  (set-face-attribute 'whitespace-indentation nil :background "grey20")
  )
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))
(use-package linum
  :bind
  (:map ctrl-x-comma-map
        ("l" . linum-mode)))
(use-package ediff
  :init
  (defvar my-ediff-map (make-sparse-keymap))
  :bind-keymap
  ("C-x e" . my-ediff-map)
  :bind
  (:map my-ediff-map
        ("e" . kmacro-end-and-call-macro) ; Old binding for \C-xe
        ("r" . ediff-revision)
        ("b" . ediff-buffers)
        ("f" . ediff-files)
        ("B" . ediff-buffers3)
        ("F" . ediff-files3)
        ("=" . ediff-backup)
        ("p" . ediff-patch-file)
        ("u" . ediff-patch-buffer)
        ("j" . dired-jump))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  (defun my-ediff-max-frame () nil)
  (defun my-ediff-restore-frame() nil)

  (defvar my-ediff-bwin-config nil "Window configuration before ediff.")

  (defvar my-ediff-bwin-reg ?b
    "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")
  (defvar my-ediff-awin-config nil "Window configuration after ediff.")

  (defvar my-ediff-awin-reg ?e
    "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

  (defun my-ediff-bsh ()
    "Function to be called before any buffers or window setup for
    ediff."
    (setq my-ediff-bwin-config (current-window-configuration))
    (my-ediff-max-frame)
    (when (symbolp my-ediff-bwin-reg)
      (set-register my-ediff-bwin-reg
                    (list my-ediff-bwin-config (point-marker)))))

  (defun my-ediff-ash ()
    "Function to be called after buffers and window setup for ediff."
    (setq my-ediff-awin-config (current-window-configuration))
    (when (symbolp my-ediff-awin-reg)
      (set-register my-ediff-awin-reg
                    (list my-ediff-awin-config (point-marker)))))

  (defun my-ediff-qh ()
    "Function to be called when ediff quits."
    (my-ediff-restore-frame)
    (when my-ediff-bwin-config
      (set-window-configuration my-ediff-bwin-config)))

  (add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
  (add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
  (add-hook 'ediff-quit-hook 'my-ediff-qh 'append)
  )
(use-package wdired
  :bind
  (:map dired-mode-map
        ("C-x C-q" . wdired-change-to-wdired-mode))
  :commands (wdired-change-to-wdired-mode))
(use-package winner
  :demand t
  :config
  (setq winner-dont-bind-my-keys t)
  (winner-mode t)
  (setq winner-ring-size 12))
;; }}}

(use-package flx :straight t)
(use-package dired-narrow
  :straight t
  :bind
  (:map dired-mode-map
        ("C-x N" . dired-narrow)
        ("F" . dired-narrow)
        ("N" . dired-narrow))
  :hook
  (dired-mode . dired-narrow-mode))

(use-package multiple-cursors
  :straight t
  :bind
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)
  ;; ("M-N" . mc/mark-next-like-this)
  ;; ("M-P" . mc/mark-previous-like-this)
  :config
  ;; (setq mc/list-file "~/.emacs.d/mc.list.el")
  ;; mc/always-run-for-all t
  )

(use-package ivy
  :straight t
  :init
  (ivy-mode t)
  :config
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex)
          (counsel-rg . ivy--regex)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 25)
  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-alt-done))
  (:map ctrl-x-comma-map
        ("," . ivy-resume)
        ("r" . ivy-resume)))
(use-package counsel
  :straight t
  :bind
  ("M-x". counsel-M-x)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-c i" . counsel-semantic-or-imenu)
  (:map ctrl-x-f-map
        ("p" . counsel-projectile)
        ("g" . counsel-rg))
  (:map ctrl-x-comma-map
        ("SPC" . counsel-mark-ring)
        ("y" . counsel-yank-pop))
  ;; (:map minibuffer-local-map
  ;;       ("C-r" . counsel-minibuffer-history))
  :config
  (assoc-delete-all 'counsel-yank-pop ivy-height-alist)

  (use-package isearch
    :config
    (defun do-rg-from-isearch ()
      "Invoke `counsel-rg' from isearch."
      (interactive)
      (let ((query (if isearch-regexp
                       isearch-string
                     (if (or (eq isearch-regexp-function 'isearch-symbol-regexp)
                             (eq isearch-regexp-function 'word-search-regexp))
                         (format "\\b%s\\b" (regexp-quote isearch-string))
                       (regexp-quote isearch-string)))))
        (isearch-exit)
        (counsel-rg query)))
    :bind
    (:map isearch-mode-map
          ("M-o" . do-rg-from-isearch))
    )
  )
(use-package counsel-projectile
  :straight t
  :config
  (setq counsel-projectile-rg-initial-input '(projectile-symbol-or-selection-at-point)))
(use-package swiper
  :straight t
  :bind
  (:map isearch-mode-map
        ("C-o" . swiper-from-isearch))
  :config

  (defun swiper--fix-from-isearch ()
    "Invoke `swiper' from isearch."
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (if isearch-regexp-function
                       (funcall isearch-regexp-function isearch-string)
                     (regexp-quote isearch-string)))))
      (isearch-exit)
      (swiper query)))
  (advice-add 'swiper-from-isearch :override 'swiper--fix-from-isearch)

  (global-set-key (kbd "M-s M-o") 'swiper-thing-at-point)
  (use-package multiple-cursors
    :config
    (cl-pushnew 'swiper-mc mc--default-cmds-to-run-once)
    )
  )
(use-package ivy-rich
  :straight t
  :ensure t
  :after (ivy counsel)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-parse-remote-file-path nil)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode t))

(use-package company
  :straight (company :type git
                     :host github
                     :repo "sadboy/company-mode"
                     :branch "own-master")
  :bind
  ("M-/" . company-other-backend)
  :config
  (setq company-idle-delay nil
        company-tooltip-align-annotations t
        company-show-numbers t
        company-require-match nil
        company-auto-commit t
        company-tooltip-idle-delay .2
        company-dabbrev-char-regexp "[[:word:]-_]"
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-dabbrev-code-ignore-case t
        company-dabbrev-code-other-buffers 'all
        company-dabbrev-code-everywhere t
        company-dabbrev-time-limit .2
        company-dabbrev-code-time-limit .2)
  (global-company-mode t))

(use-package ace-window
  :straight t)

(use-package buffer-move
  :straight t
  :config
  (setq buffer-move-stay-after-swap nil
        buffer-move-behavior 'move))

(use-package rotate
  :straight t)

(use-package basic
  :after hydra
  :demand t
  :config
  (defhydra hydra-window ()
;;     "
;; Movement^^        ^Split^         ^Switch^		^Resize^
;; ----------------------------------------------------------------
;; _h_ ←       	_v_ertical    	_b_uffer		_H_ X←
;; _j_ ↓        	_x_ horizontal	_f_ind files	_J_ X↓
;; _k_ ↑        	_z_ undo      			_K_ X↑
;; _l_ →        	_Z_ reset      			_L_ X→
;; _F_ollow	   	_S_ave		max_i_mize
;; _SPC_ cancel	_1_ only this   	_d_elete	
;; "
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("H" (buf-move-left))
    ("J" (buf-move-down))
    ("K" (buf-move-up))
    ("L" (buf-move-right))
    ("{" hydra-move-splitter-left)
    ("_" hydra-move-splitter-down)
    ("^" hydra-move-splitter-up)
    ("}" hydra-move-splitter-right)
    ("<left>" hydra-move-splitter-left)
    ("<down>" hydra-move-splitter-down)
    ("<up>" hydra-move-splitter-up)
    ("<right>" hydra-move-splitter-right)
    ("-" shrink-window-if-larger-than-buffer)
    ("=" balance-windows)
    ("[" (lambda () (interactive) (scroll-left 8)))
    ("]" (lambda () (interactive) (scroll-right 8)))
    ("C--" text-scale-decrease)
    ("C-=" text-scale-increase)

    ("o" other-window)
    ("w" rotate-layout)

    ("F" follow-mode)
    ("b" ivy-switch-buffer)
    ("M-k" kill-current-buffer)
    ("f" counsel-find-file)
    ("<" previous-buffer)
    (">" next-buffer)

    ;; ("a" (lambda ()
    ;;        (interactive)
    ;;        (ace-window 1)
    ;;        (add-hook 'ace-window-end-once-hook
    ;;                  'hydra-window/body)))

    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right))
     )
    ("x" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
     )
    ;; ("s" (lambda ()
    ;;        (interactive)
    ;;        (ace-window 4)
    ;;        (add-hook 'ace-window-end-once-hook
    ;;                  'hydra-window/body)))
    ("S" save-buffer)
    ("1" delete-other-windows)
    ("2" split-window-below)
    ("3" split-window-right)
    ("0" delete-window)

    ;; ("D" (lambda ()
    ;;        (interactive)
    ;;        (ace-window 16)
    ;;        (add-hook 'ace-window-end-once-hook
    ;;                  'hydra-window/body)))

    ("i" ace-maximize-window)
    ("/" (progn
           (winner-undo)
           (setq this-command 'winner-undo))
     )
    ("y" winner-redo)
    )

  (defhydra hydra-next-error (global-map "M-g")
    "next-error"
    ("n" next-error "next")
    ("p" previous-error "previous"))

  (defhydra hydra-rectangle (:body-pre (progn
                                         (rectangle-mark-mode 1)
                                         (activate-mark))
                             :color pink
                             :post (deactivate-mark))
    "
  ^_k_^     _d_elete    s_t_ring
_h_   _l_   _o_k        _y_ank
  ^_j_^     ne_w_-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        _q_uit      _x_kill
"
    ("h" rectangle-backward-char nil)
    ("l" rectangle-forward-char nil)
    ("k" rectangle-previous-line nil)
    ("j" rectangle-next-line nil)
    ("e" hydra-ex-point-mark nil)
    ("w" copy-rectangle-as-kill nil)
    ("d" delete-rectangle nil)
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)) nil)
    ("y" yank-rectangle nil)
    ("u" undo nil)
    ("t" string-rectangle nil)
    ("x" kill-rectangle nil)
    ("o" nil nil)
    ("q" nil nil))

  (global-set-key (kbd "s-h") 'windmove-left)
  (global-set-key (kbd "s-j") 'windmove-down)
  (global-set-key (kbd "s-k") 'windmove-up)
  (global-set-key (kbd "s-l") 'windmove-right)
  (global-set-key (kbd "s-H") 'buf-move-left)
  (global-set-key (kbd "s-J") 'buf-move-down)
  (global-set-key (kbd "s-K") 'buf-move-up)
  (global-set-key (kbd "s-L") 'buf-move-right)
  (global-set-key (kbd "C-s-h") 'hydra-move-splitter-left)
  (global-set-key (kbd "C-s-j") 'hydra-move-splitter-down)
  (global-set-key (kbd "C-s-k") 'hydra-move-splitter-up)
  (global-set-key (kbd "C-s-l") 'hydra-move-splitter-right)
  ;; For terminal:
  (global-set-key (kbd "<f9>") 'windmove-left)
  (global-set-key (kbd "<f10>") 'windmove-down)
  (global-set-key (kbd "<f11>") 'windmove-up)
  (global-set-key (kbd "<f12>") 'windmove-right)
  (global-set-key (kbd "S-<f9>") 'buf-move-left)
  (global-set-key (kbd "S-<f10>") 'buf-move-down)
  (global-set-key (kbd "S-<f11>") 'buf-move-up)
  (global-set-key (kbd "S-<f12>") 'buf-move-right)
  (global-set-key (kbd "M-<f9>") 'hydra-move-splitter-left)
  (global-set-key (kbd "M-<f10>") 'hydra-move-splitter-down)
  (global-set-key (kbd "M-<f11>") 'hydra-move-splitter-up)
  (global-set-key (kbd "M-<f12>") 'hydra-move-splitter-right)

  (global-set-key (kbd "C-x w") 'hydra-window/body)
  (global-set-key (kbd "C-S-w") 'hydra-window/body)
  (global-set-key (kbd "C-x 1") 'hydra-window/delete-other-windows)
  (global-set-key (kbd "C-x 2") 'hydra-window/split-window-below)
  (global-set-key (kbd "C-x 3") 'hydra-window/split-window-right)
  (global-set-key (kbd "C-x 0") 'hydra-window/delete-window)
  (global-set-key (kbd "C-x o") 'hydra-window/other-window)
  (global-set-key (kbd "C-x <") 'hydra-window/previous-buffer)
  (global-set-key (kbd "C-x >") 'hydra-window/next-buffer)
  (global-set-key (kbd "C-x C--") 'hydra-window/text-scale-decrease)
  (global-set-key (kbd "C-x C-=") 'hydra-window/text-scale-increase)
  (global-set-key (kbd "C-x {") 'hydra-window/hydra-move-splitter-left)
  (global-set-key (kbd "C-x }") 'hydra-window/hydra-move-splitter-right)

  (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

  (global-set-key "\C-x\C-q" 'really-toggle-read-only)
  )

(use-package grep-context
  :straight (grep-context
             :fork (:host github
                          :repo "sadboy/grep-context"))
  :hook
  (grep-mode . grep-context-mode)
  (compilation-mode . grep-context-mode))

(use-package w3m
  :straight t
  :bind
  ("C-x W" . w3m))

(use-package wgrep
  :straight t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package ivy-hydra
  :straight t)

(use-package treemacs
  :straight t
  :bind
  ("C-S-t" . treemacs)
  (:map ctrl-x-comma-map
        ("t" . treemacs)))

(use-package minimap
  :straight t
  :bind
  ("C-S-m" . minimap-mode))

(use-package all-the-icons :straight t)
(use-package all-the-icons-ivy :straight t)
(use-package all-the-icons-gnus :straight t)

(use-package project
  :straight t)

(use-package projectile
  :straight t
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :bind
  (:map ctrl-x-f-map
        ("o" . projectile-find-other-file )))

(use-package treemacs-projectile
  :straight t)

(use-package magit
  :straight t
  :config
  (setq magit-auto-revert-mode nil
        magit-last-seen-setup-instructions "1.4.0"))

(use-package org
  :straight t
  :config
  (use-package ox-md)
  (setq org-latex-listings 'minted
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-ctrl-k-protect-subtree t)
  (setq org-hide-emphasis-markers t
        org-startup-indented t
        ;; org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
        org-ellipsis "  " ;; folding symbol
        org-pretty-entities t
        org-hide-emphasis-markers t
        ;; show actually italicized text instead of /italicized text/
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org-bullets
  :straight t
  :hook
  (org-mode . org-bullets-mode))

(use-package markdown-mode :straight t)

(use-package which-key
  :straight t
  :init
  (which-key-mode t))

;; (use-package ace-jump-mode
;;   :straight t
;;   :bind ("C-." . ace-jump-mode))

;; {{{ code utils
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "M-g")

  :config
  (when (boundp 'read-process-output-max)
    (setq-default read-process-output-max (* 1024 1024)))

  (defun enable-lsp ()
    (lsp)
    (lsp-completion-mode t))

  (setq lsp-rust-analyzer-cargo-watch-command "clippy")

  :hook
  (python-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c-mode . lsp-deferred)

  :commands (lsp lsp-deferred)
)

(use-package lsp-ui
  :straight (lsp-ui
             :fork (:host github
                          :repo "sadboy/lsp-ui"))
  :bind
  (:map lsp-ui-peek-mode-map
        ("<left>" . lsp-ui-peek-scroll-left)
        ("<right>" . lsp-ui-peek-scroll-right)
        ("<down" . lsp-ui-peek-scroll-down)
        ("<up>" . lsp-ui-peek-scroll-up))
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-update-mode 'line)

  (setq lsp-ui-doc-use-webkit nil)
  (setq lsp-ui-peek-always-show nil)

  (setq lsp-ui-imenu-auto-refresh t)

  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "M-R") #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "M-?") #'lsp-ui-doc-focus-frame)
  (define-key lsp-ui-mode-map (kbd "M-I") #'lsp-ui-imenu)
  )
(use-package lsp-ivy
  :straight t
  :bind
  ("C-c o" . lsp-ivy-workspace-symbol)
  ("C-c O" . lsp-ivy-global-workspace-symbol))

(use-package dap-mode
  :straight t)

(use-package dash-docs
  :straight t
  :config
  (setq dash-docs-common-docsets '("Python 3"))
  )

(use-package lsp-python-ms
  :straight t
  :config
  (require 'lsp-python-ms)
  )

(use-package lsp-pyright
  :straight t)

(use-package tree-sitter
  :straight t
)
(use-package tree-sitter-langs
  :straight t
  :config
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'c-mode-common-hook #'tree-sitter-hl-mode)
  (add-hook 'java-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'shell-mode-hook #'tree-sitter-hl-mode)
  )

(use-package pyvenv
  :straight t
  :init
  (setenv "WORKON_HOME" (getenv "PYTHON_VENVS"))
  :hook
  (python-mode . (lambda () (pyvenv-mode t))))

(use-package symbol-overlay
  :straight (symbol-overlay
             :fork (:host github
                          :repo "sadboy/symbol-overlay"))
  :init
  ;; (put 'symbol-overlay-default-face 'face-alias 'secondary-selection)
  (setq symbol-overlay-idle-time 0.2)
  (setq symbol-overlay-temp-highlight-single t)
  :config
  ;; Fix temp highlighting face:
  (set-face-attribute 'symbol-overlay-default-face nil
                      :inherit 'secondary-selection
                      :background "gray3")


  (setq symbol-overlay-inhibit-map t)
  (defhydra symbol-hydra (global-map "M-s")
    ("i" symbol-overlay-put)
    ("h" symbol-overlay-map-help)
    ("p" symbol-overlay-jump-prev)
    ("n" symbol-overlay-jump-next)
    ("<" symbol-overlay-jump-first)
    (">" symbol-overlay-jump-last)
    ("t" symbol-overlay-toggle-in-scope)
    ("e" symbol-overlay-echo-mark)
    ("M-." symbol-overlay-jump-to-definition)
    ("%" symbol-overlay-query-replace)
    ("C-l" recenter-top-bottom)
    )

  (defhydra symbol-mc-hydra (global-map "M-s")
    ("M-n" mc/mark-next-like-this-symbol)
    ("M-N" mc/unmark-next-like-this)
    ("M-p" mc/mark-previous-like-this-symbol)
    ("M-P" mc/unmark-previous-like-this)
    )

  (use-package multiple-cursors
    :config
    (let ((once-hydras '(symbol-mc-hydra/mc/mark-previous-like-this-symbol
                         symbol-mc-hydra/mc/unmark-previous-like-this
                         symbol-mc-hydra/mc/mark-next-like-this-symbol
                         symbol-mc-hydra/mc/unmark-next-like-this)))
      (mapcar
       (lambda (x) (cl-pushnew x mc--default-cmds-to-run-once))
       once-hydras)))

  (defvar symbol-overlay-nav-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\M-n" 'symbol-overlay-jump-next)
      (define-key map "\M-p" 'symbol-overlay-jump-prev)
      map)
    "Keymap for `symbol-overlay-nav-mode'.")

  (define-minor-mode symbol-overlay-nav-mode
    "Navigate occurrences of the symbol at point.

When called interactively, toggle `symbol-overlay-nav-mode'.
With prefix ARG, enable `symbol-overlay-nav-mode' if ARG is
positive, otherwise disable it.

When called from Lisp, enable `symbol-overlay-nav-mode' if ARG
is omitted, nil or positive.  If ARG is `toggle', toggle
`symbol-overlay-nav-mode'.  Otherwise behave as if called
interactively.

In `symbol-overlay-nav-mode' provide the following key bindings
to navigate between occurrences of the symbol at point in the
current buffer.

\\{symbol-overlay-nav-mode-map}")

  (defun my-highlight-region-or-symbol ()
    (interactive)
    (if (use-region-p)
        (progn
          (deactivate-mark)
          (basic-highlight-region (region-beginning) (region-end)))
      (unless (basic-unhighlight-region)
        (symbol-overlay-put)
        (symbol-hydra/body))))

  :bind
  ("M-s M-w" . symbol-overlay-save-symbol)
  ("M-s M-r" . symbol-overlay-rename)
  ("M-H" . my-highlight-region-or-symbol)
  :hook
  (prog-mode . symbol-overlay-mode)
  (text-mode . symbol-overlay-mode)
  (prog-mode . symbol-overlay-nav-mode)
  (text-mode . symbol-overlay-nav-mode)
  )

;; }}}

;; {{{ modes
(defun my-prog-mode-hook ()
  ;; (outline-minor-mode t)
  (hs-minor-mode t)
  (whitespace-mode t)
  (electric-pair-mode t)
  (flyspell-prog-mode)
  (setq fill-column 80)
  (auto-fill-mode -1)
  (visual-line-mode t)
  (linum-mode t)
  )

(use-package prog-mode
  :bind
  (:map prog-mode-map
        ("M-N" . flymake-goto-next-error)
        ("M-P" . flymake-goto-prev-error))
  :config
  (add-hook 'prog-mode-hook 'my-prog-mode-hook)
)

(use-package elisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-b" . eval-current-buffer)
        ("C-c C-c" . emacs-lisp-byte-compile)
        ("C-c C-l" . emacs-lisp-byte-compile-and-load)
        ("C-c C-r" . eval-region)
        )
  :config
  ;; (setq lisp-indent-function 'common-lisp-indent-function)
  (defun my-emacs-lisp-mode-hook ()
    (make-local-variable 'company-backends)
    (setq company-backends
          '(company-capf company-files company-dabbrev-code company-dabbrev))
    (make-local-variable 'parens-require-spaces)
    (local-set-key "\C-xiu" (lambda () (interactive) (insert ";;;###autoload")))
    (setq parens-require-spaces t
          tab-width 8))
  (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
  )

(use-package cc-mode
  :bind
  (:map c-mode-map
        ("RET" . c-context-line-break)
        ("C-c C-c" . compile))
  (:map c++-mode-map
        ("C-c C-c" . compile))

  :config
  (defun my-c-common-hook ()
    (c-set-style "stroustrup")
    (c-set-offset 'statement-case-open '+)
    (c-toggle-hungry-state 1)
    (setq c-basic-offset 4
          tab-width 8
          indent-tabs-mode nil
          indicate-empty-lines t
          hs-isearch-open nil)
    (setq company-backends '(company-capf company-dabbrev-code company-dabbrev))

    (let ((output-name (if buffer-file-name
                           (shell-quote-argument
                            (file-name-base buffer-file-name))
                         (buffer-name)))
          (input-name (if buffer-file-name
                          (file-name-nondirectory buffer-file-name)
                        (buffer-name))))
      (set (make-local-variable 'compile-command)
           (concat "make -k "
                   output-name
                   " && ./" output-name))))

  (defun my-cpp-hook ()
    (subword-mode t))

  (add-hook 'c-mode-common-hook 'my-c-common-hook)
  (add-hook 'c++-mode-hook 'my-cpp-hook))

(use-package python
  :mode
  ("/TARGETS\\'" . python-mode)
  :config
  (setq python-indent-offset 4)

  (defun my-python-mode-hook ()
    (setq python-skeleton-autoinsert nil)
    (kill-local-variable 'completion-at-point-functions)
    (make-local-variable 'company-backends)
    (setq company-backends '(company-capf company-dabbrev-code company-dabbrev))
    (subword-mode t)
    )
  (add-hook 'python-mode-hook 'my-python-mode-hook)

  (defhydra hydra-python (python-mode-map "C-c")
    "Shift indentation"
    ("<" python-indent-shift-left)
    (">" python-indent-shift-right))
  )

(use-package antlr-mode
  :mode
  ("\\.g4\\'" . antlr-mode))

(use-package ahk-mode
  :straight t
  :config
  (add-hook 'ahk-mode-hook
            (lambda ()
              (my-prog-mode-hook)
              (when (fboundp 'symbol-overlay-mode)
                (symbol-overlay-mode 1)
                (symbol-overlay-nav-mode 1)))))
(use-package cython-mode
  :straight t)
(use-package cmake-mode
  :straight t)
;; (use-package rust-mode
;;   :straight t)
;; (use-package cargo
;;   :straight t
;;   :hook
;;   (rust-mode . cargo-minor-mode))
(use-package rustic
  :straight t)
(use-package toml-mode
  :straight t)
(use-package hack-mode
  :straight t
  :config
  (add-hook 'hack-mode-hook 'lsp))

(use-package typescript-mode
  :straight t
  :mode
  ("\\.tsx\\'" . typescript-mode))

(use-package thrift-mode
  :straight t
  :mode
  ("\\.thrift\\'" . thrift-mode))

(use-package text-mode
  :config
  (defun my-text-mode-hook ()
    (auto-fill-mode 1)
    (flyspell-mode 1)
    (setq require-final-newline nil)
    (modify-syntax-entry ?' ".")
    (make-local-variable 'company-backends)
    (setq company-backends
          (if (fboundp 'company-math-symbols-unicode)
              '(company-dabbrev-code company-dabbrev
                                     company-math-symbols-unicode)
            '(company-dabbrev-code company-dabbrev))))
  (add-hook 'text-mode-hook 'my-text-mode-hook)
)
;; }}}

;; {{{ Theming
(use-package doom-themes
  ;; :if window-system
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (setq doom-one-brighter-comments t
        doom-one-brighter-modeline nil)
  ;; (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Fixup linum mode
  (use-package linum
    :config
    (set-face-foreground 'linum "dark gray")
    (set-face-background 'linum "dim gray"))
  ;; Fixup company popup
  (use-package company
    :config
  ;; (set-face-background 'company-tooltip "dim gray")
  ;; (set-face-foreground 'company-tooltip "dark gray")
  ;; (set-face-background 'company-tooltip-selection "light blue")
    (set-face-background 'company-scrollbar-bg "wheat"))

  ;; Fixup isearch highlighting
  (set-face-bold 'lazy-highlight nil)
  )
;; }}}

(provide 'config)
