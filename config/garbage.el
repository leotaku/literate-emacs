;; the place where all my uncategorized settings go
;; should be kept as small as possible

;;; Testing
;;;; Outlines

(add-hook 'prog-mode-hook 'outline-minor-mode)

(use-package outshine
  :straight t
  :defer t
  :hook (outline-minor-mode . outshine-mode))

(with-eval-after-load 'outshine
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  
  ;; Enables outline-minor-mode for *ALL* programming buffers
  (general-define-key
   :keymaps 'outline-minor-mode-map
   
   "<tab>" 'outshine-smart-tab
   "<backtab>" 'outshine-cycle-buffer)
  
  (defun outshine-smart-tab ()
    (interactive)
    (if (outline-on-heading-p)
        (outshine-cycle)
      (indent-for-tab-command)))

  (set-face-attribute 'outshine-level-1 nil :inherit 'org-level-1 :height 100)
  (set-face-attribute 'outshine-level-2 nil :inherit 'org-level-2 :height 100)
  (set-face-attribute 'outshine-level-3 nil :inherit 'org-level-3 :height 100)
  (set-face-attribute 'outshine-level-4 nil :inherit 'org-level-4)
  (set-face-attribute 'outshine-level-5 nil :inherit 'org-level-5)
  (set-face-attribute 'outshine-level-6 nil :inherit 'org-level-6)
  (set-face-attribute 'outshine-level-7 nil :inherit 'org-level-7)
  (set-face-attribute 'outshine-level-8 nil :inherit 'org-level-8))

;;; Should-be defaults

(savehist-mode 1)
(global-auto-revert-mode -1)
(setq inhibit-message nil)
(setq custom-safe-themes t)
(setq enable-recursive-minibuffers t)
(setq help-window-select t)
(setq find-file-visit-truename t)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(show-paren-mode 1)
(setq show-paren-delay 0)

(setq make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/var/backups")))
(setq backup-by-copying t)

(setq-default truncate-lines t)
(require 'mode-local)
(add-hook 'custom-mode-hook '(lambda () (setq-local truncate-lines nil)))

;;; Settings for my system

(setq browse-url-generic-program "firefox")

;;; Undo + desktop (mostly broken)

(setq desktop-dirname             "~/.emacs.d/var/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$"  ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   0
      desktop-restore-frames      nil
      desktop-restore-eager       20
      
      desktop-restore-in-current-display nil)

(desktop-save-mode 1)

(use-package undo-tree
  :straight t
  :defer t
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo")))
  (global-undo-tree-mode 1))

;; ;; add before `desktop-read' hook
;; (defvar desktop-before-read-hook '() "Hooks run before `desktop-read'.")
;; (defadvice desktop-read (before my:desktop-read-run-before-hooks activate)
;; ;;   (run-hooks 'desktop-before-read-hook))
;; 
;; (setq undo-tree-auto-save-history t)
;; (defun my:enable-global-undo-tree()
;; ;;   (global-undo-tree-mode 1))
;; (add-hook 'desktop-before-read-hook 'my:enable-global-undo-tree)

;;; Packages that make stupid behavior less stupid (should  be moved out)

(use-package smart-backspace
  :straight t
  :defer t
  :config
  (general-define-key
   :states '(insert)
   "DEL" 'smart-backspace))

(use-package persistent-scratch
  :straight t
  :defer t
  :config
  (persistent-scratch-setup-default))

(use-package openwith
  :straight t
  :defer t
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "zathura" (file)))))

(use-package smartparens
  :straight t
  :defer t
  :init
  (require 'smartparens-config)
  (require 'smartparens-rust)
  ;; (require 'smartparens-elisp)
  :config
  ;; (smartparens-global-strict-mode)
  (smartparens-global-mode))

(use-package page-break-lines
  :straight t
  :defer t
  :config
  (global-page-break-lines-mode))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
