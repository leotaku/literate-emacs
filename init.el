;; my emacs configuration

;;; package management
;;;; DEPR package.el

;; (require 'package)

;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; (setq package-archive-priorities
;;       '(("melpa"        . 10)
;;         ("melpa-stable" . 5)
;;         ("gnu"          . 7)
;;         ("org"          . 8)))

;; (package-initialize)
                                        ; 'noactivate
;; (eval-when-compile
;;   (require 'use-package))

;;;; straight.el

(setq straight-check-for-modifications nil)
(setq straight-recipe-repositories
      '(org-elpa melpa emacsmirror))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;;;; recipes
;;;;;; require for recipes 

(require 'subr-x)
(use-package git
  :straight t
  :defer t)

;;;;;; org-mode 

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;;;; local

(defun load-config-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (mapc
   (lambda (path)
     (let ((full (expand-file-name file path)))
       (if (file-exists-p full)
           (load-file (expand-file-name full)))))
   '("~/.emacs.d/config"
     "~/.emacs.d")))
;; (message (format "Loading %s...done" full)))))))

(defun load-configuration (list)
  (mapcar 'load-config-file list))

(add-to-list 'load-path "~/.emacs.d/external/")

;;; basic commands

(defun reload ()
  "Reload init.el with optional straight integration"
  (interactive)
  (if (featurep 'straight)
      ;; straight.el
      (straight-transaction
        (straight-mark-transaction-as-init)
        (load user-init-file))
    ;; vanilla
    (load user-init-file)))

(defun nuke-buffers ()
  (interactive)
  (mapc 'kill-buffer
        (-difference
         (buffer-list)
         `(,(get-buffer "*dashboard*") ,(current-buffer)))))

(defun nuke-emacs ()
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (kill-emacs))

;;; early-load packages

(use-package no-littering
  :straight t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))
  
(recentf-mode 1)

(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 40)
(add-to-list 'recentf-exclude "/\\.emacs\\.d/elpa")
(add-to-list 'recentf-exclude "/nix/store")
(add-to-list 'recentf-exclude "\\.orhc-bibtex-cache")

(use-package bug-hunter
  :straight t
  :defer t)

(use-package general
  :straight t
  :defer t)

;;; imports

(load-configuration
 '("dashboard.el" "perf.el" "evil.el"
   "languages.el" "workflow.el" "notext.el"
   "flyspell.el" "org.el" "company.el"
   "keys.el" "look.el" "garbage.el"))

;;; custom

(setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file)

;;; end
(provide 'init)

