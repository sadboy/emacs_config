(setq user-mail-address "bo@dreamsphere.org"
      user-full-name "Bo Lin")

;; Workaround for Emacs bug on mac
;;(add-to-list 'image-types 'svg)

;; Bootstrap straight.el:
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))


;; (straight-use-package 'use-package)
;; (straight-use-package 'hydra)
;; (straight-use-package 'exec-path-from-shell)

(require 'package)

;; Add your preferred archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; (package-initialize)

;; Install use-package if it isn't built-in (built-in on Emacs 29+)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'hydra)
)

;; Tell use-package to always download standard packages automatically
;; (setq use-package-always-ensure t)

;; CRITICAL: Mimic straight.el by pulling the latest commits instead of release tags
(setq use-package-vc-prefer-newest t)

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t)
  (dolist (var '("SSH_AUTH_SOCK"
		 "SSH_AGENT_PID"
		 "GPG_AGENT_INFO"
		 "LANG"
		 "LC_CTYPE"
		 "NIX_SSL_CERT_FILE"
		 "NIX_PATH"))
  (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package config
  :vc t
  :load-path "~/emacs/config"
)

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(condition-case err
    (server-start)
  (error
   (message "Warning: Could not start Emacs server: %s" (error-message-string err))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "720838034f1dd3b3da66f6bd4d053ee67c93a747b219d1c546c41c4e425daf93"
     "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
     "3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9"
     "7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc"
     "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad"
     "83550d0386203f010fa42ad1af064a766cfec06fc2f42eb4f2d89ab646f3ac01" default))
 '(package-selected-packages
   '(affe ahk-mode all-the-icons-dired all-the-icons-gnus all-the-icons-ivy
          breadcrumb cape cmake-mode config consult-eglot consult-ls-git
          consult-xref-stack copilot corfu cython-mode d2-mode dash-docs
          dired-narrow doom-themes embark-consult exec-path-from-shell flx forge
          git-link go-mode golden-ratio gptel grep-context hack-mode hotfuzz
          imenu-list jarchive marginalia minimap minuet multiple-cursors
          nerd-icons orderless org-bullets org-modern org-roam protobuf-mode
          pyvenv rotate rustic solaire-mode spacious-padding symbol-overlay
          thrift toml-mode tramp-rpc tree-sitter-langs treemacs typescript-mode
          vc-msg vertico-posframe vterm vundo w3m wgrep yaml-mode))
 '(package-vc-selected-packages
   '((tramp-rpc :url "https://github.com/ArthurHeymans/emacs-tramp-rpc" :lisp-dir
                "lisp")
     (copilot :url "https://github.com/sadboy/copilot.el.git" :branch
              "tramp-support")
     (consult-xref-stack :url
                         "https://github.com/brett-lempereur/consult-xref-stack"
                         :branch "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 120 :width normal :foundry "nil" :family "Monaco"))))
 '(eldoc-highlight-function-argument ((t (:inherit underline)))))

