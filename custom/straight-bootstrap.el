;; -*- lexical-binding: t -*-

;;; straight-bootstrap.el --- Bootstrap straight.el

;; This file bootstraps straight.el before any other package managers are loaded.
;; It must be loaded early in early-init.el or init.el.

 (defvar bootstrap-version)
  (let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-register-package '(project :type built-in))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(provide 'straight-bootstrap)
;;; straight-bootstrap.el ends here
