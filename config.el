
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

      magit-auto-revert-mode nil
      magit-last-seen-setup-instructions "1.4.0"

      display-buffer-base-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer--maybe-pop-up-frame-or-window
         display-buffer-use-some-window
         display-buffer-pop-up-window)
        . ((reusable-frames . visible)
           (window-height . fit-window-to-buffer)))
      same-window-regexps '("\\*magit:.*")

      skeleton-pair t
      abbrev-file-name "~/emacs/abbrev_defs"
      save-abbrevs t
      custom-theme-directory "~/emacs/lisp/themes/"

      ispell-process-directory (expand-file-name "~/")
      flyspell-issue-message-flag nil
      flymake-start-syntax-check-on-newline nil

      ;; eldoc-mode
      eldoc-idle-delay 0.1

      ;; org-mode:
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-ctrl-k-protect-subtree t

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

(use-package ivy
  :init
  (ivy-mode t))

(use-package doom-themes
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

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
