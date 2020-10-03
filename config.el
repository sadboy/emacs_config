(eval-when-compile (require 'use-package))

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
         display-buffer--maybe-pop-up-frame-or-window
         display-buffer-use-some-window
         display-buffer-pop-up-window)
        . ((reusable-frames . visible)))
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
      same-window-regexps '("magit:.*")

      abbrev-file-name "~/emacs/abbrev_defs"
      save-abbrevs t

      ispell-process-directory (expand-file-name "~/")
      flyspell-issue-message-flag nil
      flymake-start-syntax-check-on-newline nil

      ;; eldoc-mode
      eldoc-idle-delay 0.1

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

(global-set-key "\C-xQ" 'save-buffers-kill-emacs)

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
(global-set-key "\C-xaa" 'apropos)
(global-set-key "\C-xQ" 'save-buffers-kill-emacs)
(global-set-key "\C-x\C-b" (lambda () (interactive) (ibuffer t nil nil nil t)))
(global-set-key "\C-x\C-q" 'really-toggle-read-only)
(global-set-key "\C-xW" 'w3m)
(global-set-key "\C-xg" 'rgrep)

(defvar bo-insert-map (make-sparse-keymap))
(define-key bo-insert-map "f" 'add-file-local-variable)
(define-key bo-insert-map "l" 'add-file-local-variable-prop-line)
(define-key bo-insert-map "a" 'auto-insert)
(define-key bo-insert-map "i" 'insert-file)
(define-key bo-insert-map "d" 'bo-add-dir-local-variable)
(defvar ctrl-x-f-map (make-sparse-keymap))
(define-key ctrl-x-f-map "F" 'ffap)
(define-key ctrl-x-f-map "c" 'set-fill-column)
(define-key ctrl-x-f-map "t" 'set-tab-width)
(define-key ctrl-x-f-map "h" 'hexl-mode)
(define-key ctrl-x-f-map "r" 'revert-buffer)
(define-key ctrl-x-f-map "s" 'magit-status)
(define-key ctrl-x-f-map "b" 'magit-blame)
;; (define-key ctrl-x-f-map "l" 'calendar)
(defvar ctrl-x-comma-map (make-sparse-keymap))
(define-key ctrl-x-comma-map "c" 'calculator)
(define-key ctrl-x-comma-map "u" 'customize-option)
(global-set-key "\C-xi" bo-insert-map)
(global-set-key "\C-xf" ctrl-x-f-map)
(global-set-key (kbd "C-x ,") ctrl-x-comma-map)

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
(global-set-key (kbd "<f7>") 'previous-buffer)
(global-set-key (kbd "<f8>") 'next-buffer)
;; }}}

;; {{{ Builtin packages:
(use-package ffap
  :init
  (ffap-bindings)
  :bind
  (:map ctrl-x-f-map
        ("l" . find-file-literally-at-point)))
(use-package view
  :bind
  (:map view-mode-map
        ("S-SPC" . View-scroll-page-backward)))
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
        ("C-x C-q" . wdired-change-to-wdired-mode)))
(use-package winner
  :demand t
  :config
  (setq winner-dont-bind-my-keys t)
  (winner-mode t)
  (setq winner-ring-size 12))
;; }}}

(use-package flx :straight t)

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
  )
(use-package counsel-projectile :straight t)
(use-package swiper
  :straight t
  :bind
  (:map isearch-mode-map
        ("C-o" . swiper-from-isearch))
  :config
  (use-package multiple-cursors
    :straight t
    :config
    (add-to-list 'mc/cmds-to-run-once 'swiper-mc)
    ))
(use-package ivy-rich
  :straight t
  :init
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
  (setq buffer-move-stay-after-swap t))

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
    ("H" buf-move-left)
    ("J" buf-move-down)
    ("K" buf-move-up)
    ("L" buf-move-right)
    ("C-h" hydra-move-splitter-left)
    ("C-j" hydra-move-splitter-down)
    ("C-k" hydra-move-splitter-up)
    ("C-l" hydra-move-splitter-right)
    ("{" hydra-move-splitter-left)
    ("_" hydra-move-splitter-down)
    ("^" hydra-move-splitter-up)
    ("}" hydra-move-splitter-right)
    ("-" shrink-window-if-larger-than-buffer)
    ("=" balance-windows)
    ("[" (lambda () (interactive) (scroll-left 8)))
    ("]" (lambda () (interactive) (scroll-right 8)))
    ("C--" text-scale-decrease)
    ("C-=" text-scale-increase)

    ("o" other-window)

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
  (global-set-key (kbd "C-<f9>") 'hydra-move-splitter-left)
  (global-set-key (kbd "C-<f10>") 'hydra-move-splitter-down)
  (global-set-key (kbd "C-<f11>") 'hydra-move-splitter-up)
  (global-set-key (kbd "C-<f12>") 'hydra-move-splitter-right)

  (global-set-key (kbd "C-x w") 'hydra-window/body)
  (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)
  (global-set-key (kbd "C-x o") 'hydra-window/other-window)
  (global-set-key (kbd "C-x <") 'hydra-window/previous-buffer)
  (global-set-key (kbd "C-x >") 'hydra-window/next-buffer)
  (global-set-key (kbd "C-x C--") 'hydra-window/text-scale-decrease)
  (global-set-key (kbd "C-x C-=") 'hydra-window/text-scale-increase)
  (global-set-key (kbd "C-x {") 'hydra-window/hydra-move-splitter-left)
  (global-set-key (kbd "C-x }") 'hydra-window/hydra-move-splitter-right)
  )

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

(use-package doom-themes
  ;; :if window-system
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (setq doom-one-brighter-comments t
        doom-one-brighter-modeline nil)
  (load-theme 'doom-one t)

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

(use-package projectile
  :straight t
  :bind-keymap
  ("C-x p" . projectile-command-map))

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
        org-ctrl-k-protect-subtree t))

(use-package markdown-mode :straight t)

(use-package multiple-cursors
  :straight t
  :bind
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)
  ("M-N" . mc/mark-next-like-this)
  ("M-P" . mc/mark-previous-like-this)
  :config
  ;; (setq mc/list-file "~/.emacs.d/mc.list.el")
  ;; mc/always-run-for-all t
  )

(use-package which-key
  :straight t
  :init
  (which-key-mode t))

;; (use-package ace-jump-mode
;;   :straight t
;;   :bind ("C-." . ace-jump-mode))

;; {{{ modes
(use-package prog-mode
  :config
  (defun my-prog-mode-hook ()
    ;; (outline-minor-mode t)
    (hs-minor-mode t)
    (whitespace-mode t)
    (electric-pair-mode t)
    (flyspell-prog-mode)
    (setq fill-column 80)
    (visual-line-mode t)
    (linum-mode t)
    )
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
  (setq lisp-indent-function 'common-lisp-indent-function)
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
        ("C-c C-c" . compile)))

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
    (auto-fill-mode nil)
    )
  (add-hook 'python-mode-hook 'my-python-mode-hook)

  (defhydra hydra-python (python-mode-map "C-c")
    "Shift indentation"
    ("<" python-indent-shift-left)
    (">" python-indent-shift-right))
  )

(use-package cython-mode
  :straight t)

(use-package rust-mode
  :straight t)

(use-package cargo
  :straight t
  :hook
  (rust-mode . cargo-minor-mode))

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

  :hook
  (python-mode . enable-lsp)
  (rust-mode . enable-lsp)
)

(use-package lsp-ui
  :straight t
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-update-mode 'line)

  (setq lsp-ui-doc-use-webkit t)

  (setq lsp-ui-peek-always-show t)

  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

(use-package dap-mode
  :straight t)

(use-package lsp-python-ms
  :straight t
  :config
  (require 'lsp-python-ms)
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
    (put 'symbol-overlay-default-face 'face-alias 'secondary-selection)
    (setq symbol-overlay-idle-time 0.1)
    (setq symbol-overlay-temp-highlight-single t)
    :config
    (setq symbol-overlay-inhibit-map t)
    (defhydra symbol-hydra (global-map "M-s")
      ("i" symbol-overlay-put)
      ("h" symbol-overlay-map-help)
      ("p" symbol-overlay-jump-prev)
      ("n" symbol-overlay-jump-next)
      ("<" symbol-overlay-jump-first)
      (">" symbol-overlay-jump-last)
      ("w" symbol-overlay-save-symbol)
      ("t" symbol-overlay-toggle-in-scope)
      ("e" symbol-overlay-echo-mark)
      ("d" symbol-overlay-jump-to-definition)
      ("s" symbol-overlay-isearch-literally)
      ("q" symbol-overlay-query-replace)
      ("r" symbol-overlay-rename)
      )
    :bind
    ("M-H" . (lambda ()
               (interactive)
               (symbol-overlay-put)
               (symbol-hydra/body)))
    :hook
    (prog-mode . symbol-overlay-mode)
    (text-mode . symbol-overlay-mode)
    )

;; }}}

(provide 'config)
