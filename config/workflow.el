;; packages that improve my top-level emacs workflow

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

;; (use-package popwin
;;   :straight t
;;   :defer nil
;;   :config
;;   (require 'popwin)
;;   (popwin-mode 1)
;;   (push '("*helm M-x*" :height 20) popwin:special-display-config))

;; projectile

(use-package projectile
  :straight t
  :defer t
  :config
  (projectile-mode +1)
  (setq projectile-project-root-files-functions '(projectile-root-top-down))
  (setq projectile-project-root-files
        '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile"))
  (setq projectile-completion-system 'ivy))

(use-package helm-projectile
  :straight t
  :defer t
  :after (projectile helm))

(use-package counsel-projectile
  :straight t
  :after (projectile counsel)
  :config
  (counsel-projectile-mode))

(general-define-key
 :prefix "C-x"
 "p" 'projectile-command-map)

;; magit

(use-package magit
  :straight t
  :defer t
  :hook (magit-mode-hook . turn-off-evil-snipe-override-mode)
  :config)

(use-package evil-magit
  :straight t
  :defer t
  :after (magit))

(general-define-key
 :prefix "C-x"
 "g" 'magit-status)

;; narrowing

(use-package helm
  :straight t
  :defer t
  :config (helm-mode 0))

(use-package hydra
  :straight t
  :defer t)

(use-package ivy
  :straight t
  :after (evil)
  :config
  (setq ivy-do-completion-in-region nil)
  (ivy-mode 1)
  (setq ivy-sort-matches-functions-alist
        '((t)
          (ivy-switch-buffer . ivy-sort-function-buffer)
          (org-insert-link . ivy--sort-by-length)
          (counsel-find-file . ivy--sort-by-length)
          (counsel-projectile-find-file . ivy--sort-by-length))))

(defun ivy--sort-by-length (_name candidates)
  (cl-sort (copy-sequence candidates)
           (lambda (f1 f2)
             (< (length f1) (length f2)))))

(use-package ivy-hydra
  :straight t
  :after (hydra ivy)
  :config
  (setq ivy-use-selectable-prompt t)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   :states '(insert normal)
   "C-o" 'hydra-ivy/body
   "<escape>" (lambda () (interactive) (hydra-ivy/keyboard-escape-quit-and-exit))))

(use-package counsel
  :straight t
  :after (ivy))

(use-package dired
  :config
  (setq dired-listing-switches "-alhv")
  (setq dired-recursive-copies 'always)

  ;; (require 'dired-details)
  ;; ;; (setq dired-details-hidden-string "[...] ")
  ;; ;; (dired-details-install)
  ;; 
  ;; (general-define-key
  ;;  :keymaps 'dired-mode-map
  ;;  ;; "<tab>" 'dired-details-toggle
  ;;  "C-c w" 'dired-toggle-read-only)
  )

(use-package diredfl
  :straight t
  :after (dired)
  :config
  (diredfl-global-mode))

(use-package dired-hide-dotfiles
  :straight t
  :after (dired evil-collection)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "." 'dired-hide-dotfiles))

(use-package peep-dired
  :straight t
  :after (dired evil-collection)
  :config
  (evil-define-key 'normal peep-dired-mode-map
    (kbd "<SPC>") 'peep-dired-scroll-page-down
    (kbd "C-<SPC>") 'peep-dired-scroll-page-up
    (kbd "<backspace>") 'peep-dired-scroll-page-up
    (kbd "j") 'peep-dired-next-file
    (kbd "k") 'peep-dired-prev-file)
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps))

;; (use-package direnv
;;   :straight t
;;   :defer t
;;   :config
;;   (direnv-mode))

(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode)
  (setq eyebrowse-wrap-around t))

(use-package shell-pop
  :straight t
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "zsh")
  (setq shell-pop-universal-key "C-t")
  (setq shell-pop-window-size 30)
  (setq shell-pop-full-span t)
  (setq shell-pop-window-position "bottom")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(general-define-key
 :keymaps 'override
 :states '(normal insert visual motion emacs)
 "C-x t" 'shell-pop
 "C-a t" 'shell-pop)

;; (use-package dired-details-s
;;   :after (dired)
;;   :config
;;   (setq dired-details-s-types
;;    '((size-time  . (size time))
;;      (all        . (perms links user group size time))
;;      (no-details . ()))))

;; (use-package dired-details-r
;;   :after (dired)
;;   :config
;;   (setq dired-details-r-combinations
;;    '((all        . (size time perms links user group))
;;      (size-time  . (size time))
;;      (no-details . ())))
;;   (dired-details-r-activate))
