;; -*- lexical-binding: t; -*-
;; Loaded by Emacs before init.el.  Keep this file small and dependency-free.

;; Save the file name handlers, then disable them while the init files load.
;; Handlers such as Tramp and jka-compr make every file lookup slower.
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Do not run the garbage collector while the init files allocate heavily.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

;; Load one cached file instead of scanning every package directory.
(setq package-quickstart t)

(add-hook 'after-init-hook
          (lambda ()
            ;; Restore the normal garbage collector settings.
            (setq gc-cons-threshold 10000000
                  gc-cons-percentage 0.1)
            ;; Restore the file name handlers, and keep any handler that a
            ;; package added while they were disabled.
            (setq file-name-handler-alist
                  (seq-uniq (append my/file-name-handler-alist
                                    file-name-handler-alist)))))

;;; early-init.el ends here
