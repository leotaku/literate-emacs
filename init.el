;;;; my emacs configuration

;; package management

(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-archive-priorities
      '(("melpa"        . 10)
        ("melpa-stable" . 5)
        ("gnu"          . 7)
        ("org"          . 8)))

(package-initialize) ; 'noactivate
(eval-when-compile
  (require 'use-package))

(use-package dash
  :ensure t)

(defun load-config-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (-each
      '("~/.emacs.d/config"
        "~/.emacs.d")
    (lambda (path)
      (let ((full (expand-file-name file path)))
       (if (file-exists-p full)
         (load-file (expand-file-name full)))))))
;; (message (format "Loading %s...done" full)))))))

(defun load-configuration (list)
  (-each list 'load-config-file))

(defun nuke-buffers ()
  (interactive)
  (-each (-difference (buffer-list) `(,(get-buffer "*dashboard*") ,(current-buffer))) 'kill-buffer))

(defun nuke-emacs ()
  (interactive)
  (-each (buffer-list) 'kill-buffer)
  (kill-emacs))

(add-to-list 'load-path "~/.emacs.d/external/")

;; basic commands

(defun reload ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; important packages

(use-package no-littering
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))
  
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(use-package bug-hunter
  :ensure t
  :defer t)

(use-package general
  :ensure t)

;; imports

(load-configuration
 '("dashboard.el" "perf.el" "evil.el"
   "languages.el" "workflow.el" "notext.el"
   "flyspell.el" "org.el" "company.el"
   "keys.el" "look.el" "garbage.el"))

;; custom

(setq custom-file "~/.emacs.d/custom.el")
;;(load custom-file)

;; general configuration

(add-hook 'recentf-mode-hook
    '(lambda () (add-to-list 'recentf-exclude "/\\.emacs\\.d/elpa")))

(load-theme 'emacs-sexy-day t)

(provide 'init)

;;; init.el ends here
