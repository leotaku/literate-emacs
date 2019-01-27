;; all the evil-mode specific configurations

;; (use-package evil
;;   :straight t
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode t)
;;   (setq evil-want-minibuffer t)
;;   (evil-select-search-module 'evil-search-module 'evil-search)
;;   (setq evil-search-wrap t
;;         evil-regexp-search t))

;; (use-package evil-collection
;;   :straight t
;;   :init
;;   (setq evil-collection-setup-minibuffer t)
;;   :config
;;   ;;(general-define-key
;;   ;; :keymaps 'helm-map
;;   ;; :states 'normal
;;   ;; ":" 'evil-delete-whole-line
;;   ;; (mapc 'evil-collection-init
;;   ;;       '(minibuffer
;;   ;;         package-menu
;;   ;;         custom
;;   ;;         ivy
;;   ;;         helm
;;   ;;         help
;;   ;;         term
;;   ;;         dired
;;   ;;         image
;;   ;;         ediff))

;;   (evil-ex-define-cmd "ls" 'counsel-ibuffer)
;;   (evil-ex-define-cmd "ll" 'helm-mini)

;;   ;; (evil-collection-define-key 'insert 'evil-ex-completion-map (kbd "C-o") 'evil-ex-normal)
;;   ;; (evil-collection-define-key 'insert 'evil-ex-completion-map (kbd "<escape>") (lambda () (interactive) (top-level)))
;;   ;; (evil-collection-define-key 'normal 'evil-ex-completion-map (kbd "<escape>") 'abort-recursive-edit)

;;   ;; (evil-collection-define-key 'insert 'evil-ex-search-keymap (kbd "C-o") 'evil-ex-normal)
;;   ;; (evil-collection-define-key 'insert 'evil-ex-search-keymap (kbd "<escape>") (lambda () (interactive) (abort-recursive-edit)))
;;   ;; (evil-collection-define-key 'normal 'evil-ex-search-keymap (kbd "<escape>") 'abort-recursive-edit)

;;   ;; (evil-collection-define-key 'normal 'evil-ex-completion-map
;;   ;;   (kbd ":") 'evil-delete-whole-line
;;   ;;   (kbd "k") 'previous-history-element
;;   ;;   (kbd "j") 'next-history-element)
  
;;   ;; (evil-collection-define-key 'insert 'helm-map
;;   ;;   (kbd "<escape>") (lambda () (interactive) (abort-recursive-edit)))

;;   ;; (general-define-key
;;   ;;  :keymaps 'dired-mode-map
;;   ;;  :states 'normal
;;   ;;  "." 'dired-hide-dotfiles-mode
;;   ;;  "p" 'peep-dired
;;   ;;  "h" 'dired-up-directory
;;   ;;  "l" 'dired-find-file)
;;   )

;; (use-package evil-terminal-cursor-changer
;;   :straight t
;;   :unless (display-graphic-p)
;;   :init
;;   (setq evil-motion-state-cursor 'box)  ; █
;;   (setq evil-visual-state-cursor 'box)  ; █
;;   (setq evil-normal-state-cursor 'box)  ; █
;;   (setq evil-insert-state-cursor 'bar)  ; ⎸
;;   (setq evil-emacs-state-cursor  'hbar) ; _

;;   (evil-terminal-cursor-changer-activate))

(use-package evil-snipe
  :straight t
  :after evil
  :config
  (setq evil-snipe-spillover-scope 'visible)
  (setq evil-snipe-repeat-scope 'buffer)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-surround
  :straight t
  :after evil-snipe
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-multiedit
  :straight t
  :after (evil)
  :config
  (evil-multiedit-default-keybinds))
  (use-package evil-mc
  :straight t
  :after evil)

;; :config
  ;; (global-evil-mc-mode))

;; (defun evil-visual-line-I ()
;;   (interactive)
;;   (evil-first-non-blank-of-visual-line)
;;   (evil-insert 1))

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'visual-line-mode-map
;;   "j" 'evil-next-visual-line
;;   "k" 'evil-previous-visual-line
;;   "^" 'evil-first-non-blank-of-visual-line
;;   "$" 'evil-end-of-visual-line
;;   "I" 'evil-visual-line-I
;;   "A" "$a"
;;   "D" "d$"
;;   "C" "c$"
;;   "<down>" "j"
;;   "<up>" "k")

;; (general-define-key
;;  :states '(normal insert)
;;  :keymaps 'visual-line-mode-map
;;  "<down>" 'evil-next-visual-line
;;  "<up>" 'evil-previous-visual-line)

;; (general-define-key
;;  :keymaps 'override
;;  :states '(normal visual)
;;  "/" 'swiper)
