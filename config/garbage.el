;; the place where all my uncategorized settings go
;; should be kept as small as possible
;; whoo fun

(savehist-mode 1)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(use-package smart-backspace
  :ensure t
  :config
  (general-define-key
   :states '(insert)
   "DEL" 'smart-backspace))

(setq inhibit-message nil)

(setq custom-safe-themes t)

(setq enable-recursive-minibuffers t)

(setq browse-url-generic-program "firefox")

(global-auto-revert-mode -1)

(setq help-window-select t)

(show-paren-mode 1)
(setq show-paren-delay 0)

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

(use-package openwith
  :ensure t
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "zathura" (file)))))

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (require 'smartparens-rust)
  ;; (require 'smartparens-elisp)
  :config
  (smartparens-global-strict-mode)
  (smartparens-global-mode))

(setq-default truncate-lines t)
(require 'mode-local)
(add-hook 'custom-mode-hook '(lambda () (setq-local truncate-lines nil)))

(setq make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/var/backups")))
(setq backup-by-copying t)

(use-package undo-tree
  :ensure t
  :defer t
  :init
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo")))
  (global-undo-tree-mode 1))

(setq desktop-dirname             "~/.emacs.d/var/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   0
      desktop-restore-frames      nil
      desktop-restore-eager       20
      
      desktop-restore-in-current-display nil)
;; 
(desktop-save-mode 1)

;; add before `desktop-read' hook
(defvar desktop-before-read-hook '() "Hooks run before `desktop-read'.")
(defadvice desktop-read (before my:desktop-read-run-before-hooks activate)
  (run-hooks 'desktop-before-read-hook))

(setq undo-tree-auto-save-history t)
(defun my:enable-global-undo-tree()
  (global-undo-tree-mode 1))
(add-hook 'desktop-before-read-hook 'my:enable-global-undo-tree)

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

(setq recentf-max-saved-items 40)

(add-to-list 'recentf-exclude "/\\.emacs\\.d/elpa")
(add-to-list 'recentf-exclude "/nix/store")
;; (add-to-list 'recentf-exclude "[(jpg)(jpeg)(png)(gif)]$")

;; (add-hook 'desktop-before-read-hook
;;           (lambda () (recentf-cleanup)))

;; (add-hook 'desktop-save-hook
;;           (lambda ()
;;             (recentf-save-list)))

;; (desktop-save-mode 1)
;; (add-to-list 'desktop-locals-to-save 'buffer-undo-list)

;;(use-package flycheck
;;  :ensure t
;;  :defer t
;;  :config
;;  (global-flycheck-mode 1)
;;  (setq flycheck-indication-mode 'left-fringe)
;;  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
;;    (vector #b00000000
;;            #b00000000
;;            #b00000000
;;            #b00000000
;;            #b00000000
;;            #b00111000
;;            #b01111100
;;            #b11111110
;;            #b11111110
;;            #b01111100
;;            #b00111000
;;            #b00000000
;;            #b00000000
;;            #b00000000
;;            #b00000000
;;            #b00000000
;;            #b00000000)
;;
;;  (flycheck-define-error-level 'info
;;    :severity 100
;;    :compilation-level 2
;;    :overlay-category 'flycheck-error-overlay
;;    :fringe-bitmap 'flycheck-fringe-bitmap-ball
;;    :fringe-face 'flycheck-fringe-error
;;    :error-list-face 'flycheck-error-list-error))

;;(require 'speck)
;;(setq speck-hunspell-default-dictionary-name "en_US")
;;(setq speck-hunspell-program (executable-find "hunspell"))
;;(setq speck-hunspell-library-directory "")
;;(setq speck-hunspell-coding-system (quote utf-8))
;;(setq speck-hunspell-dictionary-alist
;;      (quote (("en" . "en_US")
;;              ("ru" . "ru_RU"))
;;(setq speck-hunspell-default-dictionary-name "en")
