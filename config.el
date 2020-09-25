(eval-when-compile
  (require 'use-package))

(setq-default abbrev-mode t
              major-mode 'text-mode
              indent-tabs-mode nil
              tab-width 8
              fill-column 76
              default-directory "~/")

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

      display-buffer-base-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer--maybe-pop-up-frame-or-window
         display-buffer-use-some-window
         display-buffer-pop-up-window)
        . ((reusable-frames . visible)
           (window-height . fit-window-to-buffer)))
      same-window-regexps '("\\*magit:.*")

      abbrev-file-name "~/emacs/abbrev_defs"
      save-abbrevs t
      custom-theme-directory "~/emacs/lisp/themes/"

      ispell-process-directory (expand-file-name "~/")
      flyspell-issue-message-flag nil
      flymake-start-syntax-check-on-newline nil

      ;; eldoc-mode
      eldoc-idle-delay 0.1

      ;; minimap-mode:
      minimap-window-location 'right
      minimap-automatically-delete-window nil

      ;; mc-mode:
      mc/list-file "~/emacs/mc.list.el"
      mc/always-run-for-all t

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
(define-key bo-insert-map "i" 'ido-insert-file)
(define-key bo-insert-map "d" 'bo-add-dir-local-variable)
(defvar ctrl-x-f-map (make-sparse-keymap))
(define-key ctrl-x-f-map "F" 'ffap)
(define-key ctrl-x-f-map "c" 'set-fill-column)
(define-key ctrl-x-f-map "t" 'set-tab-width)
(define-key ctrl-x-f-map "h" 'hexl-mode)
(define-key ctrl-x-f-map "o" 'helm-occur)
(define-key ctrl-x-f-map "r" 'revert-buffer)
(define-key ctrl-x-f-map "d" 'ido-dired)
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

(global-set-key "\C-xo" 'my-window-control-cmd)
(global-set-key "\C-xw" 'my-window-control-cmd)
(global-set-key "\C-x^" 'my-window-control-cmd)
(global-set-key "\C-x{" 'my-window-control-cmd)
(global-set-key "\C-x}" 'my-window-control-cmd)
;; (global-set-key "\C-x<" 'my-window-control-cmd)
;; (global-set-key "\C-x>" 'my-window-control-cmd)

(global-set-key (kbd "C-<") 'previous-buffer)
(global-set-key (kbd "C->") 'next-buffer)
(global-set-key (kbd "C-M-<") 'other-window-history-back)
(global-set-key (kbd "C-M->") 'other-window-history-forward)
(global-set-key "\C-x<" 'window-history-navigation)
(global-set-key "\C-x>" 'window-history-navigation)

;; }}}

(use-package view
  :bind
  (:map view-mode-map
        ("S-SPC" . View-scroll-page-backward)))
(use-package info
  :bind
  (:map Info-mode-map
        ("S-SPC" . Info-scroll-down)))
(use-package whitespace
  :config
  (setq whitespace-style '(face trailing indentation))
  (set-face-attribute 'whitespace-trailing nil :background "dim gray")
  (set-face-attribute 'whitespace-indentation nil :background "grey20")
  )
(use-package ediff
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


(use-package flx :straight t)

(use-package ivy
  :straight t
  :init
  (ivy-mode t)
  :config
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-alt-done)
        ("C-o" . ivy-occur))
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
        ("g" . counsel-rg))
  ;; (:map minibuffer-local-map
  ;;       ("C-r" . counsel-minibuffer-history))
  )
(use-package swiper
  :straight t
  :bind
  (:map isearch-mode-map
        ("M-o" . swiper-from-isearch)))
(use-package ivy-rich
  :straight t
  :init
  (ivy-rich-mode t))

(use-package counsel-projectile :straight t)

(use-package treemacs :straight t)

(use-package all-the-icons :straight t)
(use-package all-the-icons-ivy :straight t)
(use-package all-the-icons-gnus :straight t)

(use-package doom-themes
  :if window-system
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package projectile
  :straight t
  :bind-keymap
  ("C-x p" . projectile-command-map))

(use-package magit
  :straight t
  :config
  (setq magit-auto-revert-mode nil
        magit-last-seen-setup-instructions "1.4.0"
        ))

(use-package org
  :straight t
  :config
  (use-package ox-md)
  (setq org-latex-listings 'minted
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-ctrl-k-protect-subtree t))

(use-package markdown-mode :straight t)

(use-package multiple-cursors :straight t)

(use-package ace-jump-mode
  :straight t
  :bind ("C-." . ace-jump-mode))

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

;; }}}

(provide 'config)
