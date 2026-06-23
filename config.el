;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'cl-lib)
  (require 'use-package)
  )

(setq-default abbrev-mode t
              bidi-paragraph-direction 'left-to-right
              major-mode 'text-mode
              indent-tabs-mode nil
              tab-width 8
              fill-column 76
              default-directory "~/")

(setq switch-to-buffer-obey-display-actions t
      ;; Make left and right side windows take full height:
      window-sides-vertical t
      split-width-threshold 160
      split-height-threshold 80
      )
(setopt display-buffer-base-actions
        '((display-buffer-reuse-window
           display-buffer-in-previous-window
           display-buffer-use-some-window
           display-buffer--maybe-pop-up-window
           display-buffer-same-window)
          (reusable-frames . nil)))
(add-to-list 'window-persistent-parameters '(no-delete-other-windows . writable))

(setq package-enable-at-startup nil
      ;; Don't change default temp directory on Mac:
      ;; temporary-file-directory (concat "/tmp/emacs_" (getenv "USER") "/")
      gc-cons-threshold 10000000        ; Increase garbage collection threshold
      sentence-end-without-space "。，？！；……"
      sentence-end-double-space nil
      default-input-method "TeX"

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

      window-min-width 24
      window-min-height 8

      ;; abbrev-file-name "~/emacs/abbrev_defs"
      save-abbrevs t
      vc-follow-symlinks t
      vc-handled-backends '(Git Hg)

      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t

      ispell-process-directory (expand-file-name "~/")
      flyspell-issue-message-flag nil
      flymake-start-syntax-check-on-newline nil

      compilation-scroll-output 'first-error

      ;; Dired-mode:
      dired-isearch-filenames 'dwim

      ;; tls verify:
      tls-checktrust t

      browse-url-browser-function 'eww
      )
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(global-set-key "\C-xQ" 'save-buffers-kill-emacs)
(global-set-key "\C-z" 'undo)

(when (not (file-exists-p temporary-file-directory))
  (make-directory temporary-file-directory t))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'delete-selection-mode)
  (delete-selection-mode t))

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)

(quietly-read-abbrev-file)
(show-paren-mode t)
(global-display-fill-column-indicator-mode t)
;; (global-visual-line-mode t)

;; {{{ Global key bindings
(global-set-key "\C-xQ" 'save-buffers-kill-emacs)
(global-set-key (kbd "C-S-K") 'kill-current-buffer)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

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
(define-key ctrl-x-f-map "l" 'calendar)

(defvar ctrl-x-comma-map (make-sparse-keymap))
(define-key ctrl-x-comma-map "c" 'calculator)
(define-key ctrl-x-comma-map "U" 'customize-option)
(define-key ctrl-x-comma-map "a" 'apropos)
(define-key ctrl-x-comma-map "l" 'display-line-numbers-mode)

(global-set-key "\C-xi" bo-insert-map)
(global-set-key "\C-xf" ctrl-x-f-map)
(global-set-key (kbd "C-x ,") ctrl-x-comma-map)
(global-set-key (kbd "C-S-r") 'revert-buffer)

(global-set-key "\M-z" 'my-zap-or-goto-char)

(global-set-key (kbd "<C-S-up>")     'basic/buf-move-up)
(global-set-key (kbd "<C-S-down>")   'basic/buf-move-down)
(global-set-key (kbd "<C-S-left>")   'basic/buf-move-left)
(global-set-key (kbd "<C-S-right>")  'basic/buf-move-right)

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
(use-package tramp
  :init
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profile-variables
   'remote-vterm-profile
   '((shell-file-name . "/bin/bash")))

  :config
  (setq tramp-default-method "ssh")
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
        tramp-verbose 2)
  (setq enable-remote-dir-locals t)

  ;; Not sure if this makes things better or worse:
  ;; (setq magit-tramp-pipe-stty-settings 'pty)

  ;; Note: vc is required by project.el to find the project root, so can not be
  ;; disabled: (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
  ;; vc-ignore-dir-regexp tramp-file-name-regexp))

  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (add-to-list 'tramp-connection-properties (list "/\\(ssh\\|scp\\):" "direct-async" t))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )
;; Note: this actually makes things worse (and breaks project.el as well):
;; (use-package tramp-hlo
;;     :ensure t
;;     :config
;;     (tramp-hlo-setup)
;; )
(use-package tramp-rpc
  :after tramp
  :vc (:url "https://github.com/ArthurHeymans/emacs-tramp-rpc"
       :rev :newest
       :lisp-dir "lisp")
  :init
  (connection-local-set-profile-variables
   'remote-path-with-cargo
   '((tramp-remote-path . ("~/.cargo/bin" tramp-own-remote-path tramp-default-remote-path))))

  :config
  (setq tramp-default-method "rpc")

  (connection-local-set-profiles '(:application tramp :protocol "rpc")
   'remote-path-with-cargo)
  )

(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 15
        recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "/su:" "/gpg:" "/docker:"))
  )
(use-package eldoc
  :config
  (setq
   ;; eldoc-idle-delay 0.2
   eldoc-echo-area-use-multiline-p nil
   eldoc-echo-area-display-truncation-message nil
   eldoc-echo-area-prefer-doc-buffer 'maybe
   ))
(use-package project
  :bind
  (:map ctrl-x-f-map
        ("p" . project-find-file))
  )
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
  ;; Note: This should now be managed by (package-initialize)
  ;; (setq Info-directory-list nil
  ;;       Info-default-directory-list
  ;;       (cons (expand-file-name "~/.local/share/info/")
  ;;             Info-default-directory-list))
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
(use-package ediff
  :preface
  (defvar my-ediff-map (make-sparse-keymap))
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
(use-package sql
  :mode
  ("\\.sql\\'" . sql-mode))
;; }}}

(use-package flx :ensure t)
(use-package dired-narrow
  :ensure t
  :bind
  (:map dired-mode-map
        ("C-x N" . dired-narrow)
        ("F" . dired-narrow-fuzzy)
        ("N" . dired-narrow))
  :hook
  (dired-mode . dired-narrow-mode))

(use-package multiple-cursors
  :ensure t
  :preface
  (defun my/mc/mark-all-in-region-regexp (beg end regexp)
    "Find and mark all the parts in the region matching the given regexp."
    (if (string= regexp "")
        (message "Mark aborted")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (let ((lastmatch))
          (while (and (< (point) end) ; can happen because of (forward-char)
                      (search-forward-regexp regexp end t))
            (push-mark (match-beginning 0))
            (mc/create-fake-cursor-at-point)
            (setq lastmatch (point))
            (when (= (point) (match-beginning 0))
              (forward-char)))
          (unless lastmatch
            (error "Search failed for %S" regexp)))
        (goto-char (match-end 0))
        (if (< (mc/num-cursors) 3)
            (mc/disable-multiple-cursors-mode)
          (mc/pop-state-from-overlay (mc/furthest-cursor-before-point))
          (multiple-cursors-mode 1)))))

  (cl-defun my/mc/cycle-delete (next-cursor fallback-cursor loop-message)
    (when (null next-cursor)
      (when (eql 'stop (mc/handle-loop-condition loop-message))
        (cond
         ((fboundp 'cl-return-from)
          (cl-return-from mc/cycle nil))
         ((fboundp 'return-from)
          (cl-return-from mc/cycle nil))))
      (setf next-cursor fallback-cursor))
    (mc/pop-state-from-overlay next-cursor)
    (recenter))

  (defun my/mc/unmark-forward ()
    (interactive)
    (my/mc/cycle-delete (mc/next-fake-cursor-after-point)
                        (mc/first-fake-cursor-after (point-min))
                        "We're already at the last cursor."))

  (defun my/mc/unmark-backward ()
    (interactive)
    (my/mc/cycle-delete (mc/prev-fake-cursor-before-point)
                        (mc/last-fake-cursor-before (point-max))
                        "We're already at the last cursor"))

  :hook
  (multiple-cursors-mode-disabled
   . (lambda ()
       ;; Make sure `transient-mark-mode' is disabled whenever we get out of
       ;; `multiple-cursors-mode' -- mc sometimes has a knack for leaving this
       ;; enabled for whatever reason:
       (transient-mark-mode -1))
   )

  :bind
  (("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("M-N" . mc/mark-next-like-this-symbol)
   ("M-P" . mc/mark-previous-like-this-symbol)
   ;; ("C-S-l" . mc/mark-all-like-this-dwim)
   :map mc/keymap
   ("C-S-v" . my/mc/unmark-forward)
   ("M-S-v" . my/mc/unmark-backward)
   :map isearch-mode-map
   ("C-S-l" . (lambda ()
                (interactive)
                (basic/action-from-isearch
                 (lambda (query)
                   (my/mc/mark-all-in-region-regexp
                    (point-min) (point-max) query)))))
   )
  :config
  ;; (setq mc/list-file "~/.emacs.d/mc.list.el")
  ;; mc/always-run-for-all t
  ;; (setq mc/cursor-specific-vars
  ;;       (remove 'transient-mark-mode mc/cursor-specific-vars))
  (setq mc/match-cursor-style nil)
  )

(use-package avy
  :ensure t
  :bind
  ("C-'" . avy-goto-char)
  ;;("M-z" . avy-goto-char-2)
  ("C-\"" . avy-goto-line)
  (:map isearch-mode-map
        ("C-'" . avy-isearch)))

;; Here is a modern Vertico, Orderless, Marginalia, and Consult (often called the "McClim" or "Minad" stack) configuration that mimics your Ivy/Counsel/Swiper setup.
;; ### Key Differences & Equivalents:
;; * **`ivy-mode`** $\rightarrow$ **`vertico-mode`**
;; * **`ivy-rich`** $\rightarrow$ **`marginalia-mode`** (provides rich metadata next to commands/files).
;; * **`swiper`** $\rightarrow$ **`consult-line`** (performs live buffer searching; automatically picks up active isearch queries).
;; * **`counsel-rg`** $\rightarrow$ **`consult-ripgrep`**.
;; * **`ivy-call` / `ivy-hydra`** $\rightarrow$ **`embark-act`** (bound to `C-M-m` to perform context-aware actions on candidates without closing the minibuffer).

;; Recursive minibuffers (mimics Ivy setting)
(setq enable-recursive-minibuffers t)

;; 1. VERTICO: The completion UI (mimics ivy-mode)
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  (vertico-multiform-mode t)
  :config
  (setq vertico-count 22)
  :bind
  (:map vertico-map
        ("C-k" . #'kill-line)
        ("RET" . #'vertico-directory-enter) ; Mimics ivy-alt-done for directories
        ("DEL" . #'vertico-directory-delete-char)))
;; (use-package vertico-posframe
;;   :after vertico
;;   :config
;;   (setq vertico-posframe-border-width 6
;;         vertico-posframe-poshandler 'posframe-poshandler-frame-top-center
;;         vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
;;   (setq vertico-multiform-commands
;;         '(
;;           ;; (consult-imenu (:not posframe))
;;           (consult-yank-pop (:not posframe))
;;           ;; (consult-mark (:not posframe))
;;           (t posframe
;;              ;; NOTE: This is useful when emacs is used in both in X and
;;              ;; terminal, for posframe do not work well in terminal, so
;;              ;; vertico-buffer-mode will be used as fallback at the
;;              ;; moment.
;;              (vertico-posframe-fallback-mode . vertico-buffer-mode))))
;;   ;; Automatically enabled by vertico-multiform-mode for commands in
;;   ;; vertico-multiform-commands, so no need to enable globally:
;;   ;; (vertico-posframe-mode 1)
;; )

;; 2. ORDERLESS: Completion style (mimics ivy-re-builders-alist)
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-ignore-case t
        completion-category-defaults nil
        ;; orderless-component-separator #'ignore
        completion-category-overrides '((file (styles partial-completion))))
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  )
(use-package hotfuzz
  :ensure t
  :config
  (setq completion-styles '(hotfuzz orderless basic)
        ;; completion-ignore-case t
        ;; hotfuzz-max-needle-len 5
        ;; hotfuzz-max-highlighted-completions 10
        ))

;; 3. MARGINALIA: Rich annotations in minibuffer (mimics ivy-rich)
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

(use-package consult-eglot)
;; 4. CONSULT: Search & Navigation (mimics counsel & swiper)
(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("C-S-g" . consult-ripgrep)
   ("C-c o" . consult-outline)
   ("C-c i" . consult-imenu)
   ("C-c C-m" . consult-flymake)               ;; Alternative: consult-flycheck

   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g r" . consult-grep-match)
   ("M-g f" . consult-find)
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)
   ("M-g M-o" . consult-outline)
   ("M-g SPC" . consult-mark)
   ("M-g M-SPC" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g M-i" . consult-imenu-multi)
   ("M-g I" . consult-eglot-symbols)

   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find)                  ;; Alternative: consult-fd
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s M-l" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("M-s M-o" . consult-line-thing-at-point)

   ;; Isearch integration
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line-thing-at-point)                  ;; needed by consult-line to detect isearch
   ("M-s M-l" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ("C-o" . my/do-consult-line-from-isearch)
   ("C-S-o" . my/do-consult-line-multi-from-isearch)
   ("M-o" . my/do-consult-rg-from-isearch)

   ;; Minibuffer history
   :map minibuffer-local-map
   ("C-k" . kill-line)
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
   ;; Keymaps mimicking your custom bindings

   :map ctrl-x-f-map
   ("g" . #'consult-ripgrep)
   ("p" . #'consult-git-fzf)

   :map ctrl-x-comma-map
   ("," . vertico-repeat)                 ; ivy-resume
   ("r" . vertico-repeat)
   ("SPC" . consult-mark)
   ("y" . consult-yank-pop))

  :preface
  (defun my/do-consult-rg-from-isearch ()
    "Invoke `consult-ripgrep' from isearch."
    (interactive)
    (basic/action-from-isearch (lambda (query)
                                 (consult-ripgrep nil query))))

  (defun my/do-consult-line-from-isearch ()
    "Invoke `consult-ripgrep' from isearch."
    (interactive)
    (basic/action-from-isearch 'consult-line))
  (defun my/do-consult-line-multi-from-isearch ()
    "Invoke `consult-ripgrep' from isearch."
    (interactive)
    (basic/action-from-isearch (lambda (query)
                                 (consult-line-multi nil query))))

  (defun consult-line-thing-at-point ()
    "Search for the symbol at point using `consult-line'."
    (interactive)
    (consult-line (thing-at-point 'symbol t)))

  (defun my/consult-git-fzf-builder (query)
    "Constructs a `git ls-files | fzf -f' command with the given query."
      (list
       "sh" "-c"
       (format
        "git ls-files --full-name \":/\" 2>/dev/null | fzf -f %s | head -n 100"
        (shell-quote-argument query))))

  (defun consult-git-fzf (initial)
    "Fuzzy search for files in the current git repository using fzf."
    (interactive "P")
    (let ((default-directory (project-root (project-current t)))
          ;; We want this to be fast, so don't annotate:
          marginalia-annotators)
      (find-file (consult--read
                  (consult--process-collection #'my/consult-git-fzf-builder
                    :file-handler t)
                  :prompt "Git FZF: "
                  :sort nil
                  :require-match t
                  :initial initial
                  :add-history (thing-at-point 'filename)
                  :category 'file
                  :history '(:input consult--find-history)
                  ))))

  :config
  ;; Use Consult to select xref targets (e.g., multi-matches)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --hidden")
  (consult-customize
   consult-buffer consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  )
(use-package consult-xref-stack
  :vc
  (:url "https://github.com/brett-lempereur/consult-xref-stack" :branch "main")
  :bind
  (("M-g ," . consult-xref-stack-backward)
   ("M-g ." . consult-xref-stack-forward))
  )

;; 5. EMBARK: Actions and Hydras (mimics ivy-call and ivy-hydra)
(use-package embark
  :ensure t
  :bind
  (
   ;; ("C-M-m" . #'embark-act)                     ; Mimics ivy-call-and-recenter
   ;; ("C-." . embark-dwim)                     ; Context-aware action
   ("C-h B" . embark-bindings)              ; Alternative to describe-bindings
   :map vertico-map
   ("C-." . #'embark-act)
   ("M-." . #'embark-become)
   :map isearch-mode-map                ; Make it available in isearch
   ("C-.". #'embark-act)
   )
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package vundo
  :ensure t
  :bind
  (("C-c C-/" . vundo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-window-side 'top))

;; (use-package company
;;   :ensure t
;;   :vc (:url "https://github.com/sadboy/company-mode.git"
;;             :branch "own-master")
;;   :bind
;;   ("M-/" . company-other-backend)
;;   :config
;;   (setq company-idle-delay nil
;;         company-tooltip-align-annotations t
;;         company-show-numbers t
;;         company-require-match nil
;;         company-auto-commit t
;;         company-tooltip-idle-delay .2
;;         company-dabbrev-char-regexp "[[:word:]-_]"
;;         company-dabbrev-ignore-case t
;;         company-dabbrev-downcase nil
;;         company-dabbrev-code-ignore-case t
;;         company-dabbrev-code-other-buffers 'all
;;         company-dabbrev-code-everywhere t
;;         company-dabbrev-time-limit .2
;;         company-dabbrev-code-time-limit .2)
;;   (global-company-mode t))


;; Here is the equivalent setup using **Corfu** and **Cape** (which provides the
;; Dabbrev backend and Capf extensions).

;; ### Key Mapping Notes:
;; 1. **`company-idle-delay nil`**: Mapped to `(corfu-auto nil)`. Completion is manually triggered via `M-/` (`completion-at-point`).
;; 2. **`company-show-numbers`**: Achieved by enabling the built-in `corfu-indexed-mode`.
;; 3. **Dabbrev settings**: Corfu uses Emacs's native `dabbrev` library under the hood (via `cape-dabbrev`), so `company-dabbrev-*` variables are mapped to their native Emacs `dabbrev-*` equivalents.

(use-package corfu
  :ensure t
  :bind
  (("M-/" . #'completion-at-point)    ; Manual completion trigger
   )
  (:map corfu-map
        ("M-/" . #'corfu-scroll-down))

  :preface
  (defconst my/corfu-accept-and-insert-list '(" " "." "," "/" ";" ":" "?" "!" "-" "="
                                              "[" "]" "(" ")" "{" "}" "|" "\\" "'" "`"
                                              "<" ">" "\M-(" "\"" "_" "+" "~"
                                              ;; "\M-f" "\M-b"
                                              "\M-a" "\M-e"
                                              ))

  (defun my/corfu-pass-through ()
    (let* ((event-vec (vector last-input-event))
           (local-map (current-local-map))
           (command
            (or (and local-map (lookup-key local-map event-vec))
                (lookup-key (current-global-map) event-vec))))
      (when (commandp command) (call-interactively command))))

  (defun my/corfu-accept-and-passthrough ()
    "Perform the completion, then the same command again."
    (interactive)
    (corfu-insert)
    (my/corfu-pass-through))

  :init
  (global-corfu-mode t)
  :config
  (keymap-unset corfu-map "<tab>")      ; Reserve for copilot
  (keymap-unset corfu-map "C-<tab>")      ; Reserve for copilot

  (dolist (key my/corfu-accept-and-insert-list)
    (define-key corfu-map key #'my/corfu-accept-and-passthrough))

  (setq corfu-auto nil)                 ; company-idle-delay nil (manual trigger)
  (setq corfu-align-annotations t)      ; company-tooltip-align-annotations t
  (setq corfu-preselect 'valid)
  (setq corfu-preview-current nil)
  (setq corfu-on-exact-match 'show)
  (setq corfu-cycle t)
  (setq corfu-quit-no-match nil)
  (setq corfu-border-width 3)
  ;; Enable corfu-indexed to show numbers (company-show-numbers)
  (corfu-indexed-mode t)
  )

;; Emacs built-in dabbrev configuration (used by Cape/Corfu)
(use-package dabbrev
  :custom
  (dabbrev-case-fold-search t)      ; company-dabbrev-ignore-case t
  (dabbrev-case-replace nil)        ; company-dabbrev-downcase nil
  (dabbrev-check-other-buffers t)   ; company-dabbrev-code-other-buffers 'all
  (dabbrev-friend-buffer-function (lambda (buf) t)) ; Search all buffers
  )

;; Cape provides the Dabbrev Capf and other backends
(use-package cape
  :ensure t
  :init
  ;; Add dabbrev to the default completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


(use-package ace-window
  :ensure t)

;; (use-package golden-ratio
;;   :ensure t
;;   :config
;;   (setq golden-ratio-exclude-buffer-names '("*Help*" "*info*"))
;;   (golden-ratio-mode t))

(use-package rotate
  :ensure t)

(use-package hydra
  :ensure t
  :config
  (setq hydra-is-helpful t
        hydra-hint-display-type 'message)
  )

(use-package basic
  :after hydra transient
  :demand t
  :load-path "~/emacs/config"
  :config
  (setq display-buffer-alist
        (list
         '("^\\*scratch.*\\*"
           (display-buffer-reuse-window
            display-buffer-same-window))
         '("^\\*forge: .*\\*"
           (display-buffer-reuse-window
            display-buffer-same-window))

         ;; transient popup:
         `(,(format "^%s$" (regexp-quote transient--buffer-name))
           (display-buffer-reuse-window
            display-buffer-in-side-window
            ;; display-buffer--maybe-top-panel
            ;; display-buffer--maybe-bottom-panel
            display-buffer--maybe-pop-up-window
            display-buffer-use-some-window)
           (side . top)
           (dedicated . t)
           (inhibit-same-window . t))
         ;; vundo popup:
         `("^ *\\*vundo tree\\*"
           (display-buffer-in-side-window
            display-buffer--maybe-pop-up-window
            display-buffer-use-some-window)
           (side . top)
           (dedicated . t)
           (inhibit-same-window . t))

         '("^ ?\\*Treemacs-Buffer-"
           (display-buffer-reuse-window
            display-buffer--maybe-left-panel
            display-buffer--maybe-right-panel
            display-buffer-same-window))

         '("^\\(\\*\\(Ilist\\)\\*\\)$"
           (display-buffer-reuse-window
            display-buffer--maybe-left-panel-lower
            display-buffer--maybe-right-panel
            display-buffer-same-window))

         '("^\\(\\*\\([Hh]elp.*\\|eldoc.*\\|info.*\\|Command History\\)\\*\\)\\|^magit:.*\\|^COMMIT_EDITMSG"
           (display-buffer-reuse-window
            display-buffer--maybe-right-panel
            display-buffer--maybe-bottom-panel
            display-buffer-same-window))

         '("^ *\\*.*\\*\\(<[[:digit:]]+>\\)?$\\|^magit-log\\((.+)\\)?:.*"
           (display-buffer-reuse-window
            display-buffer--maybe-bottom-panel
            display-buffer-same-window))
         ))

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
    ("h" basic/focus-window-left)
    ("j" basic/focus-window-down)
    ("k" basic/focus-window-up)
    ("l" basic/focus-window-right)
    ("H" basic/buf-move-left)
    ("J" basic/buf-move-down)
    ("K" basic/buf-move-up)
    ("L" basic/buf-move-right)
    ("C-H" basic/buf-dup-left)
    ("C-J" basic/buf-dup-down)
    ("C-K" basic/buf-dup-up)
    ("C-L" basic/buf-dup-right)
    ("{" hydra-move-splitter-left)
    ("_" hydra-move-splitter-down)
    ("^" hydra-move-splitter-up)
    ("}" hydra-move-splitter-right)
    ("<left>" windmove-left)
    ("<down>" windmove-down)
    ("<up>" windmove-up)
    ("<right>" windmove-right)
    ("S-<left>" hydra-move-splitter-left)
    ("S-<down>" hydra-move-splitter-down)
    ("S-<up>" hydra-move-splitter-up)
    ("S-<right>" hydra-move-splitter-right)
    ("-" shrink-window-if-larger-than-buffer)
    ("=" (lambda () (interactive)
           (balance-windows (window-main-window))))

    ("[" (lambda () (interactive) (scroll-left 8)))
    ("]" (lambda () (interactive) (scroll-right 8)))
    ("C--" text-scale-decrease)
    ("C-=" text-scale-increase)

    ("o" other-window)
    ("w" rotate-layout)

    ("F" follow-mode)
    ("b" pop-to-buffer)
    ("f" consult-find)
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
    ("?" winner-redo)
    )

  (defhydra hydra-next-error (global-map "M-g")
    "next-error"
    ("n" next-error "next")
    ("p" previous-error "previous"))

  (defhydra hydra-rectangle
    (:body-pre (progn
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

  :bind
  (
   ("s-1" . #'basic/focus-left-side-window)
   ("s-2" . (lambda () (interactive) (select-window (basic--main-window-mru-child))))
   ("s-3" . #'basic/focus-right-side-window)
   ("s-4" . #'basic/focus-bottom-side-window)

   ("s-h" . #'basic/focus-window-left)
   ("s-j" . #'basic/focus-window-down)
   ("s-k" . #'basic/focus-window-up)
   ("s-l" . #'basic/focus-window-right)
   ("s-H" . #'basic/buf-move-left)
   ("s-J" . #'basic/buf-move-down)
   ("s-K" . #'basic/buf-move-up)
   ("s-L" . #'basic/buf-move-right)
   ("C-s-h" . #'basic/buf-combine-left)
   ("C-s-j" . #'basic/buf-combine-down)
   ("C-s-k" . #'basic/buf-combine-up)
   ("C-s-l" . #'basic/buf-combine-right)
   ;; For terminal:
   ("<f9>" . #'basic/focus-window-left)
   ("<f10>" . #'basic/focus-window-down)
   ("<f11>" . #'basic/focus-window-up)
   ("<f12>" . #'basic/focus-window-right)
   ("S-<f9>" . #'basic/buf-move-left)
   ("S-<f10>" . #'basic/buf-move-down)
   ("S-<f11>" . #'basic/buf-move-up)
   ("S-<f12>" . #'basic/buf-move-right)
   ("M-<f9>" . #'basic/buf-dup-left)
   ("M-<f10>" . #'basic/buf-dup-down)
   ("M-<f11>" . #'basic/buf-dup-up)
   ("M-<f12>" . #'basic/buf-dup-right)

   ("C-x w" . #'hydra-window/body)
   ("C-S-w" . #'hydra-window/body)
   ("C-x 1" . #'hydra-window/delete-other-windows)
   ("C-x 2" . #'hydra-window/split-window-below)
   ("C-x 3" . #'hydra-window/split-window-right)
   ("C-x 0" . #'hydra-window/delete-window)
   ("C-x o" . #'hydra-window/other-window)
   ("C-x <" . #'hydra-window/previous-buffer)
   ("C-x >" . #'hydra-window/next-buffer)
   ("C-x C--" . #'hydra-window/text-scale-decrease)
   ("C-x C-=" . #'hydra-window/text-scale-increase)
   ("C-x {" . #'hydra-window/hydra-move-splitter-left)
   ("C-x }" . #'hydra-window/hydra-move-splitter-right)

   ("C-x SPC" . #'hydra-rectangle/body)
   ("\C-x\C-q" . #'really-toggle-read-only)

   ("s-m" . (lambda ()
              (interactive)
              (basic/toggle-window-on-side 'bottom t)))
   ("s-s" . (lambda ()
              (interactive)
              (basic/toggle-window-on-side 'right t)))
   ("s-b" . (lambda ()
              (interactive)
              (basic/toggle-window-on-side 'left t)))

   )
  )

(use-package grep-context
  :ensure t
  :vc (:url "https://github.com/sadboy/grep-context.git")
  :hook
  (grep-mode . grep-context-mode)
  (compilation-mode . grep-context-mode))

(use-package w3m
  :bind
  (("C-x W" . w3m)))

(use-package vterm
  :bind
  (("C-S-t" . (lambda ()
              (interactive)
              (my/vterm t)))
   ("s-t" . #'my/vterm)
   :map project-prefix-map
   ("t" . my/project-vterm)
   :map vterm-mode-map
   ("C-:" . #'vterm-copy-mode)
   :map vterm-copy-mode-map
   ("C-:" . #'vterm-copy-mode)
   ("q" . #'vterm-copy-mode))

  :preface
  (defun my/cycle-vterm-buffers ()
    "Cycle through active vterm buffers."
    )

  (defun my/vterm (&optional arg)
    "Wrapper around `vterm' that creates a new ssh connection for remote
buffers, instead of going through the tramp-managed connection."
    (interactive "P")
    (cond
     ((and (stringp arg) (get-buffer arg))
      (pop-to-buffer arg))
     ((and default-directory (file-remote-p default-directory))
      (with-parsed-tramp-file-name default-directory nil
        (require 'vterm)
        (let* ((remote-shell (or (vterm--tramp-get-shell method) "/bin/bash"))
               (destination (if user (format "%s@%s" user host) host))
               (maybe-port-arg (if port (format "-p %s" port) ""))
               (ssh-command
                (format
                 "ssh -t -o SetEnv=\"LC_INSIDE_EMACS=$INSIDE_EMACS\" %s %s 'cd %s; exec %s'"
                 maybe-port-arg destination localname remote-shell))
               (real-pwd default-directory)
               (default-directory "~/")
               (vterm-shell ssh-command)
               (vterm-buffer-name (format "*vterm@%s*" host)))
          (with-current-buffer (vterm arg)
            (setq-local default-directory real-pwd)))))
     (t
      (vterm arg))))

  (defun my/project-vterm ()
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root (project-current t)))
           (buffer-name (project-prefixed-buffer-name "vterm")))
      (my/vterm buffer-name)))

  (defun my/buffer-vterm ()
    (interactive)
    (let* ((vterm-buffer-name "*vterm*")
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if vterm-buffer
          (pop-to-buffer vterm-buffer)
        (my/vterm))))

  :init
  (add-to-list 'project-switch-commands     '(my/project-vterm "Vterm") t)
  (add-to-list 'project-kill-buffer-conditions  '(major-mode . vterm-mode))
  :config
  (push '("read-stdin" basic/read-stdin-to-buffer-cmd) vterm-eval-cmds)
  (push '("basic-find-file" basic/find-file-on-host-cmd) vterm-eval-cmds)
  (setq vterm-copy-exclude-prompt t)
  (setq vterm-max-scrollback 100000)
  (setq vterm-shell (format "%s -l" shell-file-name))
  (setq vterm-tramp-shells
        '(("ssh" "/usr/bin/bash")
          ("sshx" "/usr/bin/bash")
          ("podman" "/bin/bash"))))

(use-package wgrep
  :ensure t
  :bind
  (:map wgrep-mode-map
        ("C-c +" . #'grep-context-more-around-point)
        ("C-c -" . #'grep-context-less-around-point))

  :config
  (setq wgrep-auto-save-buffer t)

  (advice-add 'wgrep-change-to-wgrep-mode
              :after (lambda ()
                       (when (fboundp 'grep-context-mode)
                         (grep-context-mode -1))))
  (advice-add 'wgrep-to-original-mode
              :after (lambda ()
                       (when (fboundp 'grep-context-mode)
                         (grep-context-mode t)))))

(use-package treemacs
  :ensure t
  :bind
  (:map ctrl-x-comma-map
        ("t" . treemacs))
  :config
  (treemacs-project-follow-mode t)
  )

(use-package minimap
  :ensure t
  :config
  (setq
   minimap-window-location 'right
   minimap-automatically-delete-window 'visible)
  :bind
  ("M-M" . minimap-mode))

(use-package all-the-icons :ensure t)
(use-package all-the-icons-gnus :ensure t)

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode t))

(use-package transient
  :config
  (setq transient-show-during-minibuffer-read t
        transient-mode-line-format 3
        ;; Note: `display-buffer-alist' takes precedence over this:
        transient-display-buffer-action
        '(display-buffer-in-side-window
          (side . top)
          (dedicated . t)
          (inhibit-same-window . t))
        ))

(use-package magit
  :ensure t
  ;; :hook
  ;; (eshell-mode . #'with-editor-export-editor)

  :bind
  (
   ("C-c f" . #'magit-file-dispatch)
   ("C-x l". #'magit-log-buffer-file)

   :map ctrl-x-f-map
   ("s" . #'magit-status)
   ("b" . #'magit-blame)
   ;; ("l" . #'magit-log-buffer-file)
   )

  :config
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)
  (setq magit-auto-revert-mode nil
        magit-last-seen-setup-instructions "1.4.0"))
(use-package forge
  :after magit)

(use-package git-link
  :ensure t
  :bind
  (:map ctrl-x-f-map
        ("l" . #'my/git-link))
  :config
  (defun my/git-link (arg)
    "Call `git-link' with a prefix argument to toggle `git-link-use-commit'."
    (interactive "P")
    (let ((git-link-use-commit (if arg
                                   (not git-link-use-commit)
                                 git-link-use-commit)))
      (call-interactively #'git-link))
    )
  (setq git-link-use-commit nil)
  (setq git-link-open-in-browser nil)
  ;; (setq git-link-default-branch "main")
  )

(use-package org
  :ensure t
  :bind
  (("C-c l" . #'org-store-link)
  ("C-c a" . #'org-agenda)
  ("C-c c" . #'org-capture)

  :map org-mode-map
  ("C-c J" . #'hydra-org-motion/org-next-visible-heading)
  ("C-c K" . #'hydra-org-motion/org-previous-visible-heading)
  ("C-c U" . #'hydra-org-motion/org-up-heading-safe)
  ("C-c F" . #'hydra-org-motion/org-forward-heading-same-level)
  ("C-c B" . #'hydra-org-motion/org-backward-heading-same-level)
  ("C-c *" . #'hydra-org-motion/org-ctrl-c-star)
  ("C-c -" . #'hydra-org-motion/org-ctrl-c-minus)
  ("C-c M-j" . #'hydra-org-motion/org-metadown)
  ("C-c M-k" . #'hydra-org-motion/org-metaup)
  ("C-c M-h" . #'hydra-org-motion/org-shiftmetaleft)
  ("C-c M-l" . #'hydra-org-motion/org-shiftmetaright))

  :preface
  (defhydra hydra-org-motion ()
    "org motion commands"
    ("J" org-next-visible-heading "next")
    ("K" org-previous-visible-heading "previous")
    ("U" org-up-heading-safe "up")
    ("F" org-forward-heading-same-level "forward")
    ("B" org-backward-heading-same-level "backward")
    ("*" org-ctrl-c-star "toggle heading")
    ("-" org-ctrl-c-minus "cycle bullet")
    ("M-j" org-metadown "metadown")
    ("M-k" org-metaup "metaup")
    ("M-h" org-shiftmetaleft "shiftmetaleft")
    ("M-l" org-shiftmetaright "shiftmetaright"))

  :config
  (visual-line-mode t)
  (use-package ox-md)
  (setq org-latex-listings 'minted
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-ctrl-k-protect-subtree t
        org-list-use-circular-motion t)
  (setq org-hide-emphasis-markers t
        org-startup-indented t
        ;; org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
        org-ellipsis " ↴" ; Down arrow with corner
        org-pretty-entities t
        org-hide-emphasis-markers t
        ;; show actually italicized text instead of /italicized text/
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)
  ;; (font-lock-add-keywords
  ;;  'org-mode
  ;;  '(("^ +\\([-*]\\) "
  ;;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
)

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/Notes/roam"))
  (org-roam-db-autosync-mode t)
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry
           "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n\n")))))

(use-package org-bullets
  :ensure t
  :hook
  (org-mode . org-bullets-mode))

(use-package markdown-mode
  :ensure t
  :mode
  ("\\.mdx\\'". markdown-mode)
  :bind
  (:map markdown-mode-map
        ("C-c C-v" . markdown-preview))
  )

(use-package which-key
  :init
  (which-key-mode t))

(use-package eglot
  :after tree-sitter
  :preface
  (defface eglot-semantic-mutable
    '((t :underline t))
    "Face for mutable variables.")

  :bind
  (:map eglot-mode-map
        ("C-." . eglot-code-actions)
        ;; ("C-X" . eglot-momentary-inlay-hints)
        ("C-:" . eglot-inlay-hints-mode)
        ("M-R" . xref-find-references)
        ("M-I" . eglot-find-implementation)
        ("M-?" . eldoc-doc-buffer)
        ("M-g M-r" . eglot-rename)
        ("M-g M-f" . eglot-format)
        ("M-g M-p" . flymake-goto-prev-error)
        ("M-g M-n" . flymake-goto-next-error)
        ("C-c C-m" . flymake-show-project-diagnostics)
        )

  :config
  ;; This is required for eglot to receive `mut' variable markers from
  ;; rust-analyzer:
  (add-to-list 'eglot-semantic-token-modifiers "mutable")

  (setq-default eglot-workspace-configuration
                '(:rust-analyzer
                  (:cargo (:targetDir "target/rust-analyzer"
                                      :allFeatures t))))
  (setq eglot-autoshutdown t)
  (setq eglot-connect-timeout 300)
  ;; Use a pipe, not a pty, for communication with the language server. This can
  ;; help with performance and stability:
  (setq process-connection-type nil)
  ;; (setq eglot-extend-to-xref t)
  (setq eglot-code-action-indications '(eldoc-hint))
  )

(use-package imenu
  :config
  (setq imenu-flatten 'annotation))

(use-package imenu-list
  :bind
  (:map ctrl-x-comma-map
        ("o" . imenu-list-smart-toggle))
  :config
  ;; Remove the imenu-installed entry from `display-buffer-alist', as we don't
  ;; want it to mess with our panel layout:
  (setq display-buffer-alist
        (cl-delete 'imenu-list-display-buffer display-buffer-alist
                   :key #'cadr :test #'equal))

  (setq imenu-list-focus-after-activation t)
  ;; (setq imenu-list-size 0.25)
  ;; (setq imenu-list-position 'right)
  ;; (setq imenu-list-auto-resize t)
  )
(use-package breadcrumb
  :config
  ;; Turn it on globally for the headerline
  (breadcrumb-mode 1))

;; (use-package jarchive
;;   ;; :ensure t
;;   :after eglot
;;   :config
;;   (jarchive-setup))

(use-package copilot
  ;; :vc (copilot :url "https://github.com/sadboy/copilot.el.git"
  ;;              ;; Note: move back to official repo once the tramp support PR is
  ;;              ;; merged:
  ;;              :branch "feat/tramp")
  :vc t
  :load-path "~/emacs/copilot.el/"

  :hook
  ;; (prog-mode . (lambda ()
  ;;                (when (not (file-remote-p (buffer-file-name)))
  ;;                  (copilot-mode t))))
  (prog-mode . copilot-mode)
  (yaml-ts-mode . copilot-mode)
  (toml-ts-mode . copilot-mode)

  :bind
  ("C-c p" . #'copilot-mode)

  (:map copilot-mode-map
        ("M-]" . #'copilot-complete))
  (:map ctrl-x-comma-map
        ("k" . #'copilot-clear-overlay))
  (:map copilot-completion-map
        ("<tab>" . #'copilot-accept-completion-by-line)
        ("C-<tab>" . #'copilot-accept-completion)
        ("C-M-f" . #'copilot-accept-completion-by-word)
        ("M-]" . #'copilot-next-completion)
        ("M-[" . #'copilot-previous-completion))

  :config
  (setq
   copilot-indent-offset-warning-disable t
   copilot-enable-parentheses-balancer nil)
  )

(use-package gptel
  :ensure t
  :config
  (setq my/gptel/gemini-backend
        (gptel-make-gemini "Gemini"
          ;; :key (lambda () (getenv "GEMINI_API_KEY")) ; Evaluated at runtime
          :key (gptel-api-key-from-auth-source "api.google.com")
          :stream t
          :models '(gemini-flash-latest
                    gemini-3.5-flash gemini-3.1-flash-lite)))
  (setq my/gptel/gemini-search-backend
        (gptel-make-gemini "Gemini-Search"
          :key (gptel-api-key-from-auth-source "api.google.com")
          :stream t
          :request-params '(:tools [(:googleSearch  ())])
          :models '(gemini-flash-latest
                    gemini-3.5-flash gemini-3.1-flash-lite)))
  (setq my/gptel/anthropic-backend
        (gptel-make-anthropic "Claude"
          :key (gptel-api-key-from-auth-source "api.anthropic.com")
          :stream t
          :models '(claude-sonnet-4-6 claude-opus-4-8)))
  (setq my/gptel/deepseek-backend
        (gptel-make-deepseek "DeepSeek"
          :key (gptel-api-key-from-auth-source "api.deepseek.com")
          :stream t
          :models '(deepseek-v4-pro deepseek-v4-flash)))
  (setq my/gptel/qwen-local-backend
        (gptel-make-openai "vllm-local"
          :host "athena:8888"
          :protocol "http"
          :key "nokey"
          :models '(qwen3.6)))
  (setq gptel-backend my/gptel/gemini-search-backend
        gptel-model 'gemini-3.5-flash)
  (setq gptel-default-mode 'org-mode)

  :bind
  (("s-i" . #'gptel-menu)
   ("s-I". #'gptel-rewrite)
   ("s-?" . (lambda ()
              (interactive)
              (switch-to-buffer (call-interactively #'gptel))))
   :map gptel-mode-map
   ("s-<return>" . gptel-send))
 )

;; (use-package dap-mode
;;   :ensure t)

(use-package dash-docs
  :ensure t
  :config
  (setq dash-docs-common-docsets '("Python 3"))
  )

;; Tree sitter is native in Emacs 29
(use-package tree-sitter
  :ensure t
  ;; :config
  ;; (require 'tree-sitter-langs)
  ;; (global-tree-sitter-mode)
  ;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
)
(use-package tree-sitter-langs
  :preface
  (defun my/copy-grammars-to-emacs-tree-sitter-dir ()
    "Copy tree-sitter grammar files to native Emacs dir."
    (interactive)
    (let* ((files (directory-files (tree-sitter-langs--bin-dir) nil "\\.dylib$")))
      (dolist (grammar-file files)
        (copy-file
         (concat (tree-sitter-langs--bin-dir) grammar-file)
         (concat (expand-file-name user-emacs-directory) "tree-sitter/" "libtree-sitter-" grammar-file) t)
        (message "%s grammar files copied" (length files)))))

  :config
  ;; (add-hook 'python-mode-hook #'tree-sitter-hl-mode)
  ;; (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)
  ;; (add-hook 'c-mode-hook #'tree-sitter-hl-mode)
  ;; (add-hook 'c++-mode-hook #'tree-sitter-hl-mode)
  ;; (add-hook 'java-mode-hook #'tree-sitter-hl-mode)
  ;; (add-hook 'shell-mode-hook #'tree-sitter-hl-mode)
  )

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" (getenv "PYTHON_VENVS"))
  :hook
  (python-mode . (lambda () (pyvenv-mode t))))

(use-package symbol-overlay
  :ensure t
  ;; :straight (symbol-overlay
  ;;            :fork (:host github
  ;;                         :repo "sadboy/symbol-overlay"))
  :vc (:url "https://github.com/sadboy/symbol-overlay.git")

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

(use-package prog-mode
  :bind
  ;; (:map prog-mode-map
  ;;       ("M-N" . flymake-goto-next-error)
  ;;       ("M-P" . flymake-goto-prev-error))

  :preface
  (defun my-prog-mode-hook ()
    ;; (outline-minor-mode t)
    (hs-minor-mode t)
    (whitespace-mode t)
    (electric-pair-mode t)
    (flyspell-prog-mode)
    (setq fill-column 80)
    (auto-fill-mode -1)
    (visual-line-mode t)
    (display-line-numbers-mode t)
    )

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
        ("M-?" . describe-symbol)
        ("M-R" . xref-find-references)
        )
  :preface
  (defun my-emacs-lisp-mode-hook ()
    (make-local-variable 'parens-require-spaces)
    (local-set-key "\C-xiu" (lambda () (interactive) (insert ";;;###autoload")))
    (setq parens-require-spaces t
          tab-width 8))
  :config
  ;; (setq lisp-indent-function 'common-lisp-indent-function)
  (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
  )

(use-package cc-mode
  :bind
  (:map c-mode-map
        ("RET" . c-context-line-break)
        ("C-c C-c" . compile))
  (:map c++-mode-map
        ("C-c C-c" . compile))

  :preface
  (defun my-c-common-hook ()
    (c-set-style "stroustrup")
    (c-set-offset 'statement-case-open '+)
    (c-toggle-hungry-state 1)
    (setq c-basic-offset 4
          tab-width 8
          indent-tabs-mode nil
          indicate-empty-lines t
          hs-isearch-open nil)

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

  :config
  (add-hook 'c-mode-common-hook 'my-c-common-hook)
  (add-hook 'c++-mode-hook 'my-cpp-hook))

(use-package python
  :mode
  ("/TARGETS\\'" . python-mode)

  :preface
  (defun my-python-mode-hook ()
    (setq python-skeleton-autoinsert nil)
    (subword-mode t))

  :config
  (defhydra hydra-python (python-mode-map "C-c")
    "Shift indentation"
    ("<" python-indent-shift-left)
    (">" python-indent-shift-right))

  (setq python-indent-offset 4)
  (add-hook 'python-mode-hook 'my-python-mode-hook)
  )

(use-package java-ts-mode
  :mode "\\.java\\'"
  :config
  (add-hook 'java-ts-mode-hook 'eglot-ensure))

(use-package antlr-mode
  :mode
  ("\\.g4\\'" . antlr-mode))

(use-package ahk-mode
  :ensure t
  :config
  (add-hook 'ahk-mode-hook
            (lambda ()
              (my-prog-mode-hook)
              (when (fboundp 'symbol-overlay-mode)
                (symbol-overlay-mode 1)
                (symbol-overlay-nav-mode 1)))))

(use-package cython-mode)
(use-package cmake-mode)

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t)
)
;; (use-package cargo
;;   :ensure t
;;   :hook
;;   (rust-mode . cargo-minor-mode))
(use-package rustic
  :ensure t
  :after (rust-mode)
  :init
  (setq rustic-lsp-client 'eglot)
)
(use-package toml-ts-mode
  :mode "\\.toml\\'")
(use-package hack-mode
  :ensure t
  ;; :config
  ;; (add-hook 'hack-mode-hook 'lsp)
  )
(use-package d2-mode
  :ensure t
  :mode
  ("\\.d2\\'". d2-mode))

(use-package typescript-ts-mode
  :ensure t
  :mode
  ("\\.tsx\\'" . typescript-ts-mode))

(use-package thrift
  :mode
  ("\\.thrift\\'" . thrift-mode))

(use-package protobuf-mode
  :mode
  ("\\.proto\\'" . protobuf-mode))

(use-package dockerfile-ts-mode
  :mode
  (("\\Dockerfile\\'" . dockerfile-ts-mode)
   ("\\.dockerignore\\'" . dockerfile-ts-mode)))

(use-package sql
  :mode
  ("\\.sql\\'" . sql-mode)
  ("\\.slt\\'" . slt-mode)

  :config
  (defvar slt-mode-syntax-table
    (let ((table (make-syntax-table sql-mode-syntax-table)))
      ;; Redefine '#' as the single-line comment character instead of standard SQL '--'
      (modify-syntax-entry ?# "<" table)
      (modify-syntax-entry ?\n ">" table)
      table)
    "Syntax table for `slt-mode`.")

  (define-derived-mode slt-mode sql-mode "SQL Logic Test"
    "Major mode for editing SQLLogicTest (.slt) files."
    :syntax-table slt-mode-syntax-table

    ;; Configure comments for comment-dwim (M-;)
    (setq-local comment-start "# ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "#+ *"))

  ;; Highlight SLT-specific runner directives
  (font-lock-add-keywords
   'slt-mode
   '(;; Highlighting the directive type: statement, query, skipif, etc.
     ("^\\(statement\\|query\\|skipif\\|onlyif\\|hash-threshold\\|halt\\)\\>" 1 font-lock-keyword-face)
     ;; Highlighting expected status: ok, error
     ("^statement[ \t]+\\(ok\\|error\\)\\>" 1 font-lock-warning-face)
     ;; Highlight the results block separator (----)
     ("^----$" . font-lock-preprocessor-face)))
  )

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'eglot-ensure))

(use-package text-mode
  :preface
  (defun my-text-mode-hook ()
    ;; (auto-fill-mode 1)
    (flyspell-mode 1)
    (visual-line-mode t)
    (setq require-final-newline nil)
    (modify-syntax-entry ?' ".")
    ;; (setq completion-at-point-functions
    ;;       (append '(cape-dabbrev) completion-at-point-functions))
    )
  :config
  (add-hook 'text-mode-hook 'my-text-mode-hook)
  )

(use-package flyspell
  :config
  (define-key flyspell-mode-map (kbd "C-;") nil t)
  (define-key flyspell-mode-map (kbd "C-.") nil t)
  (define-key flyspell-mode-map (kbd "C-,") nil t)
  )

(use-package yaml-ts-mode
  :ensure t
  :mode "\\.ya?ml\\'")
(use-package json-ts-mode
  :ensure t
  :mode "\\.json\\'")
;; }}}

;; {{{ Theming
(use-package doom-themes
  ;; :if window-system
  :ensure t
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

  ;; Fixup isearch highlighting
  (set-face-bold 'lazy-highlight nil)
  )
;; }}}

;; {{{ Github integration
;; required dependencies
;; (straight-use-package 'org-ql)
;; (straight-use-package 's)
;; (straight-use-package 'ts)

;; ;; optional
;; (straight-use-package
;;  '(el-csv
;;   :type git
;;   :host github
;;   :repo "mrc/el-csv"
;;   :branch "master"
;;   :files ("parse-csv.el")))

;; ;; om-dash
;; (straight-use-package
;;  '(om-dash
;;   :type git
;;   :host github
;;   :repo "gavv/om-dash"
;;   :branch "main"
;;   :files ("om-dash.el")))

;; }}}

(provide 'config)
