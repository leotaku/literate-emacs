;; my emacs configuration

(advice-add 'evil-write :before-while 'advice-evil-write)

(defun advice-evil-write (&rest ignore)
  (if (org-src-edit-buffer-p)
      (progn 
        (org-edit-src-save)
        nil)
    t))

;;; package management
;;;; load-path

(add-to-list 'load-path "~/.emacs.d/external/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;;;; straight.el

(setq straight-check-for-modifications
      '(find-when-checking check-on-save))
(setq straight-recipe-repositories
      '(org-elpa melpa emacsmirror gnu-elpa))

;;;;; bootstrap

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

;;;;; org-mode hack

(require 'subr-x)
(straight-use-package 'git)

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

(straight-use-package 'org-plus-contrib)

;;;; use-package

(straight-use-package 'use-package)

;;;;; use-config macro 

(defmacro use-config (after &rest body)
  "use-package like wrapper for configurations"
  (macroexp-progn
   (use-package-require-after-load after body)))

;; `(with-eval-after-load ',after ,@body)

(put 'use-config 'lisp-indent-function 'defun)

;;;; other

(use-package no-littering
  :straight t)

(use-config (no-littering recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;; configurations
;;;; away 
(use-config (straight hydra)
  (defhydra hydra-straight-helper (:hint nil)
    "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+----_q_uit----|
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
    ("c" straight-check-all)
    ("C" straight-check-package)
    ("r" straight-rebuild-all)
    ("R" straight-rebuild-package)
    ("f" straight-fetch-all)
    ("F" straight-fetch-package)
    ("p" straight-pull-all)
    ("P" straight-pull-package)
    ("m" straight-merge-all)
    ("M" straight-merge-package)
    ("n" straight-normalize-all)
    ("N" straight-normalize-package)
    ("u" straight-push-all)
    ("U" straight-push-package)
    ("v" straight-freeze-versions)
    ("V" straight-thaw-versions)
    ("w" straight-watcher-start)
    ("W" straight-watcher-quit)
    ("g" straight-get-recipe)
    ("e" straight-prune-build)
    ("q" nil)))

;;;; early-load
;; packages that need to be loaded early

(use-package bug-hunter
  :straight t
  :defer t)

(use-package general
  :straight t
  :defer t)

(use-package dash
  :straight t
  :config
  (dash-enable-font-lock))

;;;; vanilla
;;;;; commands

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
  "Kill all emacs buffers but *dashboard* and current"
  (interactive)
  (mapc 'kill-buffer
        (-difference
         (buffer-list)
         `(,(get-buffer "*dashboard*") ,(current-buffer)))))

(defun nuke-emacs ()
  "Kill all buffers and emacs"
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (kill-emacs))

;;;;; idiosyncrasies

(savehist-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq gc-cons-threshold 50000000)

(setq inhibit-message nil)
(setq enable-recursive-minibuffers nil)
(setq help-window-select t)
(setq find-file-visit-truename nil)
(setq custom-safe-themes t)

(setq max-specpdl-size 1200)
(setq max-lisp-eval-depth 800)

(use-package page-break-lines
  :straight t
  :defer t
  :config
  (global-page-break-lines-mode))

;;;;; programming

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(show-paren-mode 1)
(setq show-paren-delay 0)

(setq-default truncate-lines t)
(add-hook 'custom-mode-hook (lambda () (setq-local truncate-lines nil)))

;;;;; saving
;;;;;; recentf

(use-package recentf
  :straight nil
  :config
  (recentf-mode 1)

  (setq recentf-max-menu-items 20)
  (setq recentf-max-saved-items 50)
  (add-to-list 'recentf-exclude "/\\.emacs\\.d/elpa")
  ;; (add-to-list 'recentf-exclude "/\\.emacs\\.d/straight")
  (add-to-list 'recentf-exclude "/nix/store")
  (add-to-list 'recentf-exclude "\\.orhc-bibtex-cache"))

;;;;;; backup

(setq make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/var/backup")))
(setq version-control t)
(setq delete-old-versions t)
(setq backup-by-copying t)

;;;;;; autosave

(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save/files")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/var/auto-save/files" t)))

;;;;;; desktop

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

;; only allow the first server instance
;; to enable desktop-mode

(if (file-exists-p
     (concat (file-name-as-directory desktop-dirname)
             desktop-base-lock-name))
    nil
  (progn
    (desktop-save-mode 1)
    (message "Desktop save is on!")))

;;;;;; undo-tree

(use-package undo-tree
  :straight t
  :defer t
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo")))
  (global-undo-tree-mode 1))

;;;;; system

(use-package openwith
  :straight t
  :config
  (openwith-mode t)
  (setq openwith-associations nil))

(setq browse-url-generic-program "firefox")
(add-to-list 'openwith-associations '("\\.pdf\\'" "zathura" (file)))

;;;; window-management/navigation
;;;;;; shackle

(use-package shackle
  :straight t)

(use-config shackle
  (setq shackle-default-rule '()
        shackle-rules '(("\\*helm.*?\\*" :regexp t :align t :size 0.4)
                        ("\\*Completions\\*" :regexp t :select nil)
                        (image-mode :select nil :popup t)
                        (dired-mode :custom (lambda (buf alist plist)
                                              (if (bound-and-true-p dired-filter-mode)
                                                  (switch-to-buffer buf)
                                                (display-buffer-pop-up-window buf nil))))))

  (shackle-mode 1))

;;;;;; eyebrowse
;;;;;;; use-package

(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode))

;;;;;;; commands

(defun eyebrowse-dashboard ()
  (interactive)
  (eyebrowse-create-window-config)
  (delete-other-windows)
  (switch-to-buffer (get-or-create-dashboard)))

;;;;;;; configure

(use-config eyebrowse
  (setq eyebrowse-wrap-around t))

;;;;;; ace-window
;;;;;;; use-package

(use-package ace-window
  :straight t
  :defer t)

;;;;;;; customize

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;;;;; commands

(defun this-or-next-real-buffer (&optional allow-dired)
  (interactive)
  (let ((start-buffer (buffer-name)))
    (while
        (not (or
              (buffer-file-name)
              (string-match-p "\*dashboard\*" (buffer-name))
              (string-match-p "\*scratch\*" (buffer-name))
              (and allow-dired (bound-and-true-p dired-filter-mode))))
      (next-buffer))))

(defun this-or-previous-real-buffer (&optional allow-dired)
  (interactive)
  (let ((start-buffer (buffer-name)))
    (while
        (not (or
              (buffer-file-name)
              (string-match-p "\*dashboard\*" (buffer-name))
              (string-match-p "\*scratch\*" (buffer-name))
              (and allow-dired (bound-and-true-p dired-filter-mode))))
      (previous-buffer))))

(defun next-real-buffer ()
  (interactive)
  (let ((dired-p (bound-and-true-p dired-filter-mode)))
    (next-buffer)
    (this-or-next-real-buffer dired-p)))

(defun previous-real-buffer ()
  (interactive)
  (let ((dired-p (bound-and-true-p dired-filter-mode)))
    (previous-buffer)
    (this-or-previous-real-buffer dired-p)))

(defun close-window-or-eyebrowse ()
  (interactive)
  (if (condition-case nil (evil-window-delete) (error t))
      (eyebrowse-close-window-config)
    ()))

(defun switch-to-dashboard ()
  (interactive)
  (switch-to-buffer "*dashboard*"))

;;;;;; keys
;;;;;;; misc

(general-define-key
 :keymaps 'override
 "<XF86Back>" 'previous-real-buffer
 "<XF86Forward>" 'next-real-buffer
 "C-q" (lambda () (interactive) (quit-window t)))

;;;;;;; C-a

(general-define-key
 :keymaps 'override
 :states '(normal insert visual motion emacs)
 :prefix "C-a"
 "s" 'ace-window
 "o" 'other-window
 "O" (lambda () (interactive) (other-window -1))
 "j" 'ace-window
 "d" 'switch-to-dashboard
 "c" 'eyebrowse-dashboard
 "n" 'eyebrowse-next-window-config
 "p" 'eyebrowse-prev-window-config
 "a" 'delete-other-windows
 "w" 'split-window-vertically
 "q" 'split-window-horizontally
 "X" 'eyebrowse-close-window-config
 "x" 'close-window-or-eyebrowse
 "k" 'evil-window-up
 "l" 'evil-window-right
 "h" 'evil-window-left
 "j" 'evil-window-down
 "C-k" (lambda () (interactive) (evil-window-move-very-top))
 "C-j" (lambda () (interactive) (evil-window-move-very-down))
 "C-h" (lambda () (interactive) (evil-window-move-far-left))
 "C-l" (lambda () (interactive) (evil-window-move-far-right)))

;;;;;;; M-*

(general-define-key
 :keymaps 'override
 "M-:" 'eval-expression
 "M-x" 'helm-M-x)

(general-define-key
 :prefix "C-x"
 "f" 'counsel-find-file
 "C-f" 'counsel-locate
 "F" 'counsel-fzf)

;;;; look
;;;;; dashboard
;; Set up the dashboard page which is shown after emacs is first started.
;;;;;; use-package

(use-package dashboard
  :straight t
  :after evil)

;;;;;; lib

(defun get-or-create-dashboard (&optional concise)
  ;; DONE: Prevent recursive calls from server-process-filter.
  (let ((buffer (get-buffer "*dashboard*")))
    (recentf-cleanup)
    (if buffer
        buffer
      (progn
        (dashboard-insert-startupify-lists)
        (get-buffer "*dashboard*")))))

;;;;;; configure

(use-config dashboard
  (setq dashboard-startup-banner "~/.emacs.d/resources/icon.png")
  (setq dashboard-banner-logo-title "Emacs is sexy!")
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)
                          (agenda . 5)))

  (setq inhibit-splash-screen nil)
  (setq initial-buffer-choice (lambda () (get-or-create-dashboard))))

;;;;;; keys 

(general-define-key
 :keymaps 'dashboard-mode-map
 :states 'normal
 "SPC" 'dashboard-next-section
 "S-SPC" 'dashboard-previous-section
 "r" 'dashboard-refresh-buffer)

;;;;; defaults

(scroll-bar-mode 0)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;;;;; theming
;;;;;; use-package

(use-package doom-themes
  :straight t
  :defer t)

;;;;;; lib

(if (boundp 'FONT) () (setq FONT "Fira Mono"))
(if (boundp 'FONT-SIZE) () (setq FONT-SIZE 100))

(defun get-frame-to-variables ()
  (setq FRAME-WIDTH (- (frame-native-width) 16))
  (setq FRAME-HEIGHT (- (frame-native-height) 0)))

(defadvice disable-theme (after disable-theme-after activate) (set-visual-conf))
(defadvice disable-theme (before disable-theme-before activate) (get-frame-to-variables))
(defadvice load-theme (after load-theme-after activate) (set-visual-conf))
(defadvice load-theme (before load-theme-before activate) (get-frame-to-variables))

(add-hook 'after-make-frame-functions (lambda (x) (set-visual-conf)))

(get-frame-to-variables)

;;;;;; commands

(defun set-font (font size)
  (interactive)
  (get-frame-to-variables)
  (setq FONT font)
  (setq FONT-SIZE size)
  (set-visual-conf))

(defun set-visual-conf (&optional ignore)
  (interactive)
  (set-frame-font (format "%s-%d" FONT (/ FONT-SIZE 10)) nil t)
  (set-face-attribute 'default nil :height FONT-SIZE)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  (set-frame-height (selected-frame) FRAME-HEIGHT nil t)
  (set-frame-width (selected-frame) FRAME-WIDTH nil t))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;;;; modeline
;;;;;; use-package

(use-package doom-modeline
  :straight t
  :defer t
  :hook (after-init . doom-modeline-init))

(use-package all-the-icons
  :straight t
  :defer t)

;; (use-package moody
;;   :straight t
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (when (not (member '(vc-mode moody-vc-mode) mode-line-format))
;;     (moody-replace-mode-line-buffer-identification)
;;     (moody-replace-vc-mode)))

;;;;;; configure

(use-config doom-modeline
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 5)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-major-mode-color-icon nil)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-version t))

;;;;; yascroll
;;;;;; use-package

(use-package yascroll
  :straight t
  :defer t)

;;;;;; configure

(use-config yascroll
  (require 'cl)
  (setq yascroll:delay-to-hide 0.5))

(global-yascroll-bar-mode 1)

;;;; integration packages
;;;;; evil
;;;;;; use-package

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-minibuffer t))

(use-package evil-collection
  :straight t
  :init
  (setq evil-collection-setup-minibuffer t))

(use-package evil-terminal-cursor-changer
  :straight t)

;; TODO fix with dired
;; (use-package evil-snipe
;;   :straight t
;;   :after evil
;;   :config
;;   (setq evil-snipe-spillover-scope 'visible)
;;   (setq evil-snipe-repeat-scope 'buffer)
;;   (evil-snipe-mode +1)
;;   (evil-snipe-override-mode +1))

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

;; (use-package evil-multiedit
;;   :straight t
;;   :after (evil)
;;   :config
;;   (evil-multiedit-default-keybinds))

;; (use-package evil-mc
;;   :straight t
;;   :after evil)

;;;;;; lib

(defun evil-visual-line-I ()
  (interactive)
  (evil-first-non-blank-of-visual-line)
  (evil-insert 1))

(defun evil-visual-line-A ()
  (interactive)
  (evil-end-of-visual-line)
  (evil-insert 1))

;;;;;; configure
;;;;;;; evil + collection

(use-config evil
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands t)
  (setq evil-cross-lines nil
        evil-move-beyond-eol nil
        evil-want-fine-undo t
        evil-symbol-word-search t))

(use-config evil-collection
  (mapc 'evil-collection-init
        '(minibuffer
          package-menu
          custom
          ivy
          helm
          help
          term
          dired
          image
          ediff
          notmuch
          mu4e))
  
  ;; TODO Update to general
  ;; (evil-collection-define-key 'insert 'evil-ex-completion-map (kbd "C-o") 'evil-ex-normal)
  ;; (evil-collection-define-key 'insert 'evil-ex-completion-map (kbd "<escape>") (lambda () (interactive) (top-level)))
  ;; (evil-collection-define-key 'normal
  ;;   'evil-ex-completion-map (kbd "<escape>") 'abort-recursive-edit)

  ;; (evil-collection-define-key 'insert 'evil-ex-search-keymap (kbd "C-o") 'evil-ex-normal)
  ;; (evil-collection-define-key 'insert 'evil-ex-search-keymap (kbd "<escape>") (lambda () (interactive) (abort-recursive-edit)))
  ;; (evil-collection-define-key 'normal 'evil-ex-search-keymap (kbd "<escape>") 'abort-recursive-edit)

  ;; (evil-collection-define-key 'normal 'evil-ex-completion-map
  ;;   (kbd ":") 'evil-delete-whole-line
  ;;   (kbd "k") 'previous-history-element
  ;;   (kbd "j") 'next-history-element)

  ;; (evil-collection-define-key 'insert 'helm-map
  ;;   (kbd "<escape>") (lambda () (interactive) (abort-recursive-edit)))
  )

;;;;;;; terminal-cursor

(setq evil-motion-state-cursor 'box)  ; █
(setq evil-visual-state-cursor 'box)  ; █
(setq evil-normal-state-cursor 'box)  ; █
(setq evil-insert-state-cursor 'bar)  ; ⎸
(setq evil-emacs-state-cursor  'hbar) ; _


;;;;;; commands

(use-config evil
  (evil-ex-define-cmd "ls" 'counsel-ibuffer)
  (evil-ex-define-cmd "ll" 'helm-mini))

;;;;;; keys
;;;;;;; mouse fix

(general-define-key
 :keymaps 'evil-motion-state-map
 [down-mouse-1] 'mouse-set-point)

;;;;;;; visual-line

(general-define-key
 :states 'normal
 :keymaps 'visual-line-mode-map
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line
 "^" 'evil-first-non-blank-of-visual-line
 "$" 'evil-end-of-visual-line
 "I" 'evil-visual-line-I
 "A" "$a"
 "D" "d$"
 "C" "c$"
 "<down>" "j"
 "<up>" "k")

(general-define-key
 :states '(normal insert)
 :keymaps 'visual-line-mode-map
 "<down>" 'evil-next-visual-line
 "<up>" 'evil-previous-visual-line)

;;;;;;; C-x

(general-unbind
  "C-x ESC")

(general-define-key
 :keymaps 'override
 "C-x l" 'counsel-recentf)

;;;;;;; misc

(general-define-key
 :states '(normal insert)
 "C-y" 'hippie-expand)

;; (general-define-key
;;  :states '(normal visual)
;;  "]" 'evil-forward-paragraph
;;  "[" 'evil-backward-paragraph)

;;;;; ivy
;;;;;; use-package

(use-package ivy
  :straight t
  :init
  (setq ivy-do-completion-in-region nil)
  :config
  (ivy-mode 1))

(use-package counsel
  :straight t
  :after (ivy))

;;;;;; lib

(defun ivy--sort-by-length (_name candidates)
  (cl-sort (copy-sequence candidates)
           (lambda (f1 f2)
             (< (length f1) (length f2)))))

(defun ivy--matcher-desc ()
  "Return description of `ivy--regex-function'."
  (let ((cell (assq ivy--regex-function ivy-preferred-re-builders)))
    (if cell
        (cdr cell)
      "other")))

;;;;;; configure

(use-config ivy
  (setq ivy-use-selectable-prompt t)
  ;; (setq ivy-sort-matches-functions-alist
  ;;       '((t)
  ;;         (ivy-switch-buffer . ivy-sort-function-buffer)
  ;;         (org-insert-link . ivy--sort-by-length)
  ;;         (counsel-find-file . ivy--sort-by-length)
  ;;         (counsel-projectile-find-file . ivy--sort-by-length)))
  )

;;;;;; hydras

;; TODO demand
(with-eval-after-load 'hydra
  (defhydra better-ivy (:hint nil :color pink)
    "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
 _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
    ;; arrows
    ("j" ivy-next-line)
    ("k" ivy-previous-line)
    ("l" ivy-alt-done)
    ("h" ivy-backward-delete-char)
    ("g" ivy-beginning-of-buffer)
    ("G" ivy-end-of-buffer)
    ("d" ivy-scroll-up-command)
    ("u" ivy-scroll-down-command)
    ("e" ivy-scroll-down-command)
    ;; actions
    ("q" keyboard-escape-quit :exit t)
    ("C-g" keyboard-escape-quit :exit t)
    ("<escape>" keyboard-escape-quit :exit t)
    ("C-o" nil)
    ("i" nil)
    ("TAB" ivy-alt-done :exit nil)
    ("C-j" ivy-alt-done :exit nil)
    ;; ("d" ivy-done :exit t)
    ("RET" ivy-done :exit t)
    ("C-m" ivy-done :exit t)
    ("f" ivy-call)
    ("c" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    (">" ivy-minibuffer-grow)
    ("<" ivy-minibuffer-shrink)
    ("w" ivy-prev-action)
    ("s" ivy-next-action)
    ("a" ivy-read-action)
    ("t" (setq truncate-lines (not truncate-lines)))
    ("C" ivy-toggle-case-fold)
    ("o" ivy-occur :exit t)))

(general-define-key
 :keymaps 'ivy-minibuffer-map
 :states '(insert normal)
 "C-o" 'better-ivy/body
 "<escape>" 'better-ivy/keyboard-escape-quit-and-exit)

;;;;;; keys

(general-define-key
 :keymaps '(minibuffer-local-map ivy-minibuffer-map evil-ex-completion-map)
 :states 'insert
 "[" 'lispy-brackets
 "]" 'self-insert-command
 ";" 'self-insert-command
 ":" 'self-insert-command)

(general-define-key
 :keymaps '(minibuffer-local-map ivy-minibuffer-map evil-ex-completion-map)
 :states 'insert
 "<RET>" 'exit-minibuffer)


(general-define-key
 :keymaps 'ivy-minibuffer-map
 :states 'insert
 "<RET>" 'ivy-done
 "<tab>" 'ivy-partial-or-done)

;; swiper
(general-define-key
 :keymaps 'override
 :states '(normal visual)
 "/" 'swiper)

;;;;; helm
;;;;;; use-package

(use-package helm
  :straight t
  :defer t)

;; configure

(use-config helm
  (setq helm-display-function 'pop-to-buffer)) ; make helm play nice

;;;;;; hydras

(general-define-key
 :keymaps 'helm-map
 :states '(insert normal emacs visual)
 "C-o" 'hydra-helm/body)

(use-config (hydra helm)
  (defhydra hydra-helm (:hint nil :color pink)
    "
                                                                          ╭──────┐
   Navigation   Other  Sources     Mark             Do             Help   │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
        ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
        ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
        ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
  --------------------------------------------------------------------------------
        "
    ("<tab>" helm-keyboard-quit "back" :exit t)
    ("<escape>" nil "quit")
    ("\\" (insert "\\") "\\" :color blue)
    ("h" helm-beginning-of-buffer)
    ("j" helm-next-line)
    ("k" helm-previous-line)
    ("l" helm-end-of-buffer)
    ("g" helm-beginning-of-buffer)
    ("G" helm-end-of-buffer)
    ("n" helm-next-source)
    ("p" helm-previous-source)
    ("K" helm-scroll-other-window-down)
    ("J" helm-scroll-other-window)
    ("c" helm-recenter-top-bottom-other-window)
    ("m" helm-toggle-visible-mark)
    ("t" helm-toggle-all-marks)
    ("u" helm-unmark-all)
    ("H" helm-help)
    ("s" helm-buffer-help)
    ("v" helm-execute-persistent-action)
    ("d" helm-persistent-delete-marked)
    ("y" helm-yank-selection)
    ("w" helm-toggle-resplit-and-swap-windows)
    ("f" helm-follow-mode)))

;;;;; hydra
;;;;;; use-package

(use-package hydra
  :straight t
  :defer t)

;;;;; projectile
;;;;;; use-package

(use-package projectile
  :straight t
  :config
  (projectile-mode 1))

(use-package helm-projectile
  :straight t
  :defer t
  :after (projectile helm))

(use-package counsel-projectile
  :straight t
  :after (projectile counsel)
  :config
  (counsel-projectile-mode))
;;;;;; configure

(setq projectile-project-root-files-functions '(projectile-root-top-down))
(setq projectile-project-root-files
      '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile"))

(setq projectile-completion-system 'ivy)

;;;;;; keys

(general-define-key
 :prefix "C-x"
 "p" 'projectile-command-map)

;;;;; MOVE? which-key
;;;;;; use-package 

(use-package which-key
  :straight t
  :init
  (setq which-key-allow-evil-operators t)
  :config
  (which-key-mode 1))

;;;; coding and writing
;;;;; lsp
;;;;;; use-package

(use-package lsp-mode
  :straight t
  :after 'yasnippet
  :commands lsp)

(use-package lsp-ui
  :straight t
  :after lsp)

(use-package company-lsp
  :straight t
  :after lsp)

;;;;;; configure

(use-config lsp
  (set-face-attribute 'lsp-face-highlight-textual nil :background (doom-color 'base1)))

;;;;; dap
;;;;;; use-package

(use-package dap-mode
  :straight t
  :defer t)

;;;;; yasnippet
;;;;;; use-package

(use-package yasnippet
  :straight t
  :defer t)

;;;;; company
;;;;;; use-package

(use-package company
  :straight t
  :defer t
  :hook ((prog-mode . company-mode)
         (company-mode . yas-minor-mode)))

;;;;;; configure

(use-config company
  (setq company-tooltip-align-annotations t)
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-tng-frontend
                            company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.2))

;;;;;; keys

;; TODO evaluate how to make this better
(general-define-key
 :keymaps 'company-active-map
 "<RET>" 'company-complete
 "<tab>" 'company-select-next
 "<backtab>" 'company-select-previous)

;;;;; fly/ispell

(setq ispell-dictionary "en_US")
(setq flyspell-default-dictionary "en_US")

;;;;;; use-package

(use-package flyspell
  :straight t
  :defer t)

(use-package flyspell-correct
  :straight t
  :defer t
  :after flyspell)

;;;;;; config
;;;;;;; ispell
(cond
 ;; try hunspell at first
 ;; if hunspell does NOT exist, use aspell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US"
	       "[[:alpha:]]" "[^[:alpha:]]" "[']"
	       nil ("-d" "en_US")
	       nil utf-8)
	      ("de_AT"
	       "[[:alpha:]]" "[^[:alpha:]]" "[']"
	       nil ("-d" "de_AT")
	       nil utf-8))))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

;;;;;;; flyspell

(use-config flyspell
  (setq flyspell-duplicate-distance 0)
  (setq flyspell-issue-message-flag nil))

;;;;;; commands

(defun flyspell-buffer-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'buffer nil (car word) current-location (cadr word) (caddr word) current-location))))

(defun flyspell-global-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(defun ispell-better ()
  (interactive)
  (ispell)
  (deactivate-mark))

;;;;;; keys

(general-define-key
 :keymaps 'flyspell-mode-map
 :states 'normal
 :prefix "z"
 "g" 'flyspell-buffer-word
 "G" 'flyspell-global-word
 "n" 'flyspell-goto-next-error
 "p" 'flyspell-correct-previous
 "=" 'flyspell-correct-at-point)

;;;;;; buffers

(put 'ispell-skip-region-alist 'safe-local-variable (lambda (_) t))

;;;;; flycheck
;;;;;; use-package

(use-package flycheck
  :straight t
  :defer t)

;;;;; outshine
;;;;;; use-package

(use-package outshine
  :straight t
  :after 'org)

(add-hook 'emacs-lisp-mode-hook 'outshine-mode)

;;;;;; lib

(defun outshine-smart-tab ()
  (interactive)
  (if (outline-on-heading-p)
      (outshine-cycle)
    (indent-for-tab-command)))

;;;;;; configure

;; terrible hack
(use-config outshine
  (ignore-errors
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (if (bound-and-true-p outshine-mode)
                  (font-lock-add-keywords nil (outshine-fontify-headlines (outshine-calc-outline-regexp)))
                (outshine-font-lock-flush))))
          (buffer-list))))

(use-config outshine
  (set-face-attribute 'outshine-level-1 nil :inherit 'org-level-1 :height 100)
  (set-face-attribute 'outshine-level-2 nil :inherit 'org-level-2 :height 100)
  (set-face-attribute 'outshine-level-3 nil :inherit 'org-level-3 :height 100)
  (set-face-attribute 'outshine-level-4 nil :inherit 'org-level-4)
  (set-face-attribute 'outshine-level-5 nil :inherit 'org-level-5)
  (set-face-attribute 'outshine-level-6 nil :inherit 'org-level-6)
  (set-face-attribute 'outshine-level-7 nil :inherit 'org-level-7)
  (set-face-attribute 'outshine-level-8 nil :inherit 'org-level-8))

;;;;;; keys

(general-unbind
  :keymaps 'lispy-mode-map
  "<backtab>")

;; TODO fix this to be less intrusive
(general-define-key
 :keymaps 'outline-minor-mode-map
 :states 'normal
 "<tab>" 'outshine-smart-tab
 "<S-iso-lefttab>" 'outshine-cycle-buffer)

;;;;; smartparens
;;;;;; use-package

(use-package smartparens
  :straight t
  :defer t
  :init
  (require 'smartparens-config))

(smartparens-global-mode)

;;;;;; lib

(defun create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;;;;;; keys

(general-define-key
 :keymaps 'org-mode-map
 :states 'normal
 ">" 'sp-forward-slurp-sexp
 "<" 'sp-forward-barf-sexp)

;;;; languages
;;;;; nix
;;;;;; use-package

(use-package nix-mode
  :straight t
  :defer t
  :mode "\\.nix\\'")   

(use-package helm-nixos-options
  :straight t
  :defer t)

;;;;; rust
;;;;;; use-package

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'") ;; this is already done by rust-mode

(use-package flycheck-rust
  :straight t
  :after (rust-mode)
  :hook (rust-mode . flycheck-rust-setup))

(use-package cargo
  :straight t
  :after (rust-mode)
  :hook (rust-mode . cargo-minor-mode))

;;;;;; lib

(defun setup-rust-env ()
  "RLS requires some environment variables to be setup. We use rustup to get the values."
  
  (when (executable-find "rustup")
    (require 's)
    (require 'dash)
    (setq rust-default-toolchain
          (car (s-split " " (-first
                             (lambda (line) (s-match "default" line)) 
                             (s-lines (shell-command-to-string "rustup toolchain list"))))))
    ;; tell racer to use the rustup-managed rust-src
    ;; rustup component add rust-src
    (setq rust-src-path (concat (getenv "HOME") "/.multirust/toolchains/" rust-default-toolchain "/lib/rustlib/src/rust/src"))
    (setq rust-bin-path (concat (getenv "HOME") "/.multirust/toolchains/" rust-default-toolchain "/bin"))
    (setq racer-rust-src-path rust-src-path)
    (setenv "RUST_SRC_PATH" rust-src-path)
    (setenv "RUSTC" rust-bin-path)))

;;;;;; configure

(with-eval-after-load 'smartparens
  (sp-local-pair 'rust-mode "{" nil :post-handlers
                 '((create-newline-and-enter-sexp "RET")))

  (sp-local-pair 'rust-mode "(" nil :post-handlers
                 '((create-newline-and-enter-sexp "RET"))))

(use-config cargo
  (setq rust-format-on-save t))

;;;;; haskell
;;;;; lisp
;;;;;; use-package

(use-package aggressive-indent
  :straight t
  :defer t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package lispy
  :straight t
  :defer t
  :hook ((emacs-lisp-mode . lispy-mode)
         ;;  (minibuffer-setup . lispy-mode)
         ))

(use-package lispyville
  :straight t
  :hook (lispy-mode . lispyville-mode)
  :config
  (general-define-key
   :keymaps 'lispyville-mode-map
   :states 'normal
   "M-O" nil
   "M-[" nil)
  :init
  (setq lispyville-key-theme
        '(additional-movement
          wrap
          additional
          additional-insert
          slurp/barf-cp
          operators)))

;;;;;; configure

;;;;;; keys

(general-define-key
 :keymaps 'lispy-mode-map
 :states '(normal visual)
 "[" 'lispy-backward
 "]" 'lispy-forward)

;;;;; TeX
;;;;;; use-package

(use-package tex
  :straight auctex
  :defer t
  :hook
  (TeX-mode . visual-line-mode)
  (LaTeX-mode . visual-line-mode))

(use-package auctex-latexmk
  :straight t
  :after tex
  :init
  (auctex-latexmk-setup))

;;;;;; configure

(use-config auctex
  (TeX-source-correlate-mode)
  (TeX-PDF-mode)
  
  (setq TeX-auto-save t)
  (setq preview-default-option-list 
        '("titlesec" "pagestyles" "displaymath" "floats" "graphics" "textmath" "sections" "footnotes")))

(use-config auctex
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura"))
  (add-to-list 'TeX-expand-list
               '("%sn" (lambda () server-name)))
  (add-to-list 'TeX-view-program-list
               '("Zathura"
                 ("zathura %o"
                  (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
                 "zathura")))

;;;;; markdown
;;;;;; use-package

(use-package markdown-mode
  :straight t
  :defer t
  :hook (markdown-mode . visual-line-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

;;;; org
;;;; misc
;;;;; MOVE? magit
;;;;;; use-package

(use-package magit
  :straight t
  :defer t
  :hook (magit-mode-hook . turn-off-evil-snipe-override-mode))

(use-package evil-magit
  :straight t
  :after (magit)
  :config
  (evil-magit-init))

;;;;;; keys

(general-define-key
 :prefix "C-x"
 "g" 'magit-status)

;;;;; dired
;;;;;; use-package

(use-package dired)

(use-package diredfl
  :straight t
  :after (dired)
  :config
  (diredfl-global-mode))

(use-package dired-filter
  :straight t
  :after (dired)
  :hook (dired-mode . dired-filter-mode))

(use-package dired-ranger
  :straight t
  :after (dired))

(use-package dired+
  :straight (dired+
             :type git :host github
             :repo "emacsmirror/emacswiki.org"
             :files ("dired+.el"))
  :init
  :after (dired))

(use-package dired-quick-sort
  :straight t)

(use-package peep-dired
  :straight t
  :after (dired))

(use-package org-download
  :straight t
  :after (dired org)
  :hook (dired-mode . org-download-enable))

;;;;;; configure

(use-config dired
  (setq dired-listing-switches "-alhv")
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t))

(use-config peep-dired
  (setq peep-dired-cleanup-eagerly t)
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-enable-on-directories nil))

;; (use-config dired-filter
;;   (setq dired-filter-group
;;         '(("default" (dot-files))
;;           ("dotfiles"
;;            (not (dot-files))))))

;;;;;; keys

(general-define-key
 :keymaps 'dired-mode-map
 :states '(normal visual)
 "h" 'backward-char
 "j" 'next-line
 "k" 'previous-line
 "l" 'forward-char

 "<backspace>" 'dired-up-directory
 "<RET>" 'dired-find-file

 "q" (lambda ()
       (interactive)
       (if (bound-and-true-p peep-dired)
           (peep-dired-disable))
       (quit-window)
       (peep-dired-kill-buffers-without-window)))

(general-define-key
 :keymaps 'dired-mode-map
 :states '(normal visual)
 "f" dired-filter-map
 "M" dired-filter-mark-map
 "o" 'hydra-dired-quick-sort/body
 "H" 'dired-hide-details-mode
 "W" (lambda ()
       (interactive)
       (wdired-change-to-wdired-mode)
       (evil-normal-state) (forward-char))
 "U" (lambda ()
       (interactive)
       (dired-unmark-all-marks)
       (ring-remove dired-ranger-copy-ring 0)))

(general-define-key
 :keymaps 'dired-mode-map
 :states '(normal visual)
 "v" 'evil-visual-line
 "y" 'dired-ranger-copy
 "a" (lambda () (interactive) (dired-ranger-copy 1))
 "p" 'dired-ranger-paste
 "P" 'dired-ranger-move)

(general-define-key
 :keymaps 'dired-mode-map
 :states 'visual
 "u" 'diredp-unmark-region-files
 "y" (lambda ()
       (interactive)
       (call-interactively 'dired-mark)
       (dired-ranger-copy nil))
 "a" (lambda ()
       (interactive)
       (call-interactively 'dired-mark)
       (dired-ranger-copy t)))

(evil-collection-define-key '(normal visual) 'dired-mode-map
  "S" 'peep-dired
  "s" (lambda () (interactive)
        (if (bound-and-true-p peep-dired)
            (peep-dired-display-file-other-window)
          (peep-dired))))


(general-define-key
 :keymaps 'dired-mode-map
 :states 'insert
 "<backspace>" 'backward-delete-char)

(use-config dired-filter)

;;;;; nov.el
;;;;;; use-package

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode))

;;;;;; configure 

;; (add-hook 'nov-mode-hook
;;           (lambda ()
;;             (face-remap-add-relative 'variable-pitch
;;                                      :family "Liberation Serif"
;;                                      :height 1.5)))

;;;;; mail 
;;;;; erc
;;;;; rss
;;;;; mpdel
;;;;; shell-pop
;;;;;; use-package

(use-package shell-pop
  :straight t
  :defer t)

;;;;;; configure

(use-config shell-pop
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "zsh")
  (setq shell-pop-universal-key "C-t")
  (setq shell-pop-window-size 30)
  (setq shell-pop-full-span t)
  (setq shell-pop-window-position "bottom")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;;;;;; keys

(general-define-key
 :keymaps 'override
 :states '(normal insert visual motion emacs)
 "C-x t" 'shell-pop
 "C-a t" 'shell-pop)

;;; configurations2
;;;; emacs
;;;;; prelude
;;;;; look
;;;;; integration
;;;;; navigation
;;;; editing
;;;;; expand-region

(use-package expand-region
  :straight t)

(general-define-key
 :keymaps 'override
 "C-0" 'er/expand-region)

;;;; programming
;;;; languages
;;;; misc
;;;;; mail

(setq mail-host-address "brg-feldkirchen.at")
(setq user-full-name "Leo Gaskin")
(setq user-mail-address "leo.gaskin@brg-feldkirchen.at")

;;;;;; smtp/sendmail

;; (setq smtpmail-queue-mail nil
;;       smtpmail-queue-dir "~/.maildir/queue/cur")

(setq message-send-mail-function 'message-send-mail-with-sendmail
      send-mail-function 'sendmail-send-it)

;; substitute sendmail with msmtp
(setq sendmail-program "msmtp")

;; allow setting account through email header
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil t)

(add-hook 'message-send-hook
          (lambda ()
            (unless (yes-or-no-p "Sure you want to send this?")
              (signal 'quit nil))))

;; (use-package NetworkManager
;;   :straight (NetworkManager
;;              :type git :host github
;;              :repo "tromey/emacs-network-manager")
;;   :config
;;   (NetworkManager-add-listener
;;    (lambda (state)
;;      (mu4e~main-toggle-mail-sending-mode)
;;      (when (eq major-mode 'mu4e-main-mode)
;;        (let ((pos (point)))
;;          (mu4e~main-view-real nil nil)
;;          (goto-char pos))))))

;;;;;; mu4e

;; (add-to-list 'load-path "~/result/share/emacs/site-lisp/mu4e")

(use-package mu4e)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(require 'mu4e)
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook 'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook (lambda () (auto-save-mode -1)))

(use-package org-mu4e)

(general-define-key
 :keymaps 'mu4e-compose-mode-map
 "C-c C-x C-o" 'org-mu4e-compose-org-mode)

(setq mail-user-agent 'mu4e-user-agent)

(use-config mu4e
  (setq mu4e-compose-signature "Leo Gaskin\nleo.gaskin@brg-feldkirchen.at"
        mu4e-compose-signature-auto-include nil)
  
  (setq mu4e-maildir (format "%s/.maildir"
                             (getenv "HOME"))
        mu4e-sent-folder "/outlook/Sent Items"
        mu4e-drafts-folder "/outlook/Drafts"
        mu4e-trash-folder "/outlook/Trash")

  (setq mu4e-attachment-dir "~/Downloads/Mail")
  (setq mu4e-get-mail-command "systemctl --user start mbsync.service"))

(use-config mu4e
  (setq mu4e-view-use-gnus nil)
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-html2text-command "elinks -dump")
  ;; (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq mu4e-use-fancy-chars nil)
  (setq mu4e-view-show-images t)

  (setq mu4e-headers-show-threads t)
  (setq mu4e-headers-include-related nil)
  (setq mu4e-headers-results-limit 400)

  (setq mu4e-confirm-quit nil))

(use-config (mu4e dired)
  (require 'gnus-dired)
  
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
     	  (set-buffer buffer)
     	  (when (and (derived-mode-p 'message-mode)
     		         (null message-sent-message-via))
     	    (push (buffer-name buffer) buffers))))
      (nreverse buffers))))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(general-define-key
 :keymaps 'override
 "C-x m" 'hydra-mu4e-entry/body)

;;;;;;; hydra

(defvar my/mu4e-persist-searches nil)

(advice-add 'mu4e~headers-search-execute :around 'my/mu4e-persist-searches-advice)

(defun my/mu4e-persist-searches-advice (fun expr ignore-history)
  (when (not ignore-history)
    (setq my/mu4e-persist-searches (delete expr my/mu4e-persist-searches))
    (add-to-list 'my/mu4e-persist-searches expr))
  (apply fun expr ignore-history nil))

(add-to-list 'desktop-globals-to-save 'my/mu4e-persist-searches)

(defhydra hydra-mu4e-entry (:color amaranth :hint nil :foreign-keys run)
  "
 ^Actions^        ^^|                 %-s(org-add-props \"mu4e - mu for emacs v1.0 C\" nil 'face 'hydra-face-blue)   
-^^---------------^^+-----------------------------------------------------------------
_c_/_C_: compose    |  ^ ^                                           
_s_: search       ^^| [_1_] %-30s(nth 0 my/mu4e-persist-searches) [_5_] %-20s(nth 4 my/mu4e-persist-searches)
_S_: edit search  ^^| [_2_] %-30s(nth 1 my/mu4e-persist-searches) [_6_] %-20s(nth 5 my/mu4e-persist-searches)                                       
_b_: search bkmk  ^^| [_3_] %-30s(nth 2 my/mu4e-persist-searches) [_7_] %-20s(nth 6 my/mu4e-persist-searches)                              
_B_: edit bkmk    ^^| [_4_] %-30s(nth 3 my/mu4e-persist-searches) [_8_] %-20s(nth 7 my/mu4e-persist-searches)
                ^^^^|
                ^^^^+----------------------------------------------------------------- 
_h_/_H_: help       | _u_: reindex          _;_: context: %s(or (mu4e-context-current) \"None\")
_q_/_e_: quit/enter | _U_: fetch + reindex  _,_: switch to maildir "
  ;;_*_: queue: %s(if smtpmail-queue-mail \"queued\" \"direct\")

  ;; actions
  ("R" mu4e-compose-reply)
  ("c" mu4e-compose-new :exit t)
  ("C" mu4e-compose-new :exit t)
  ("s" mu4e-headers-search)
  ("S" mu4e-headers-search-edit)
  ("b" mu4e-headers-search-bookmark)
  ("B" mu4e-headers-search-bookmark-edit)
  ;; ("RET" nil :color blue)
  ("q" mu4e-quit :exit t)
  ("e" nil :exit t)
  ("RET" nil :exit t)
  ;; ("x" mu4e-quit)
  ("h" mu4e-display-manual :exit t)
  ("H" mu4e-display-manual :exit t)
  
  ;; miscellany
  ("u" mu4e-update-index :color amaranth)
  ("U" mu4e-update-mail-and-index :color amaranth)
  (";" mu4e-context-switch :color amaranth)
  ("," mu4e~headers-jump-to-maildir :color amaranth)
  ("*" mu4e~main-toggle-mail-sending-mode)
  ;; searches
  ("1" (lambda () (interactive) (mu4e~headers-search-execute (nth 0 my/mu4e-persist-searches) t)))
  ("2" (lambda () (interactive) (mu4e~headers-search-execute (nth 1 my/mu4e-persist-searches) t)))
  ("3" (lambda () (interactive) (mu4e~headers-search-execute (nth 2 my/mu4e-persist-searches) t)))
  ("4" (lambda () (interactive) (mu4e~headers-search-execute (nth 3 my/mu4e-persist-searches) t)))
  ("5" (lambda () (interactive) (mu4e~headers-search-execute (nth 4 my/mu4e-persist-searches) t)))
  ("6" (lambda () (interactive) (mu4e~headers-search-execute (nth 5 my/mu4e-persist-searches) t)))
  ("7" (lambda () (interactive) (mu4e~headers-search-execute (nth 6 my/mu4e-persist-searches) t)))
  ("8" (lambda () (interactive) (mu4e~headers-search-execute (nth 7 my/mu4e-persist-searches) t)))

  ("." nil))

(defhydra hydra-mu4e-headers (:color blue :hint nil)
  "
 ^General^   | ^Search^           | _!_: read    | _#_: deferred  | ^Switches^
-^^----------+-^^-----------------| _?_: unread  | _%_: pattern   |-^^------------------
_n_: next    | _s_: search        | _r_: refile  | _&_: custom    | _O_: sorting
_p_: prev    | _S_: edit prev qry | _u_: unmk    | _+_: flag      | _P_: threading
_]_: n unred | _/_: narrow search | _U_: unmk *  | _-_: unflag    | _Q_: full-search
_[_: p unred | _b_: search bkmk   | _d_: trash   | _T_: thr       | _V_: skip dups 
_y_: sw view | _B_: edit bkmk     | _D_: delete  | _t_: subthr    | _W_: include-related
_R_: reply   | _{_: previous qry  | _m_: move    |-^^-------------+-^^------------------ 
_C_: compose | _}_: next query    | _a_: action  | _|_: thru shl  | _`_: update, reindex
_F_: forward | _C-+_: show more   | _A_: mk4actn | _H_: help      | _;_: context-switch
_o_: org-cap | _C--_: show less   | _*_: *thing  | _q_: quit hdrs | _j_: jump2maildir "

  ;; general
  ("n" mu4e-headers-next)
  ("p" mu4e-headers-previous)
  ("[" mu4e-select-next-unread)
  ("]" mu4e-select-previous-unread)
  ("y" mu4e-select-other-view)
  ("R" mu4e-compose-reply)
  ("C" mu4e-compose-new)
  ("F" mu4e-compose-forward)
  ("o" my/org-capture-mu4e)             ; differs from built-in

  ;; search
  ("s" mu4e-headers-search)
  ("S" mu4e-headers-search-edit)
  ("/" mu4e-headers-search-narrow)
  ("b" mu4e-headers-search-bookmark)
  ("B" mu4e-headers-search-bookmark-edit)
  ("{" mu4e-headers-query-prev)         ; differs from built-in
  ("}" mu4e-headers-query-next)         ; differs from built-in
  ("C-+" mu4e-headers-split-view-grow)
  ("C--" mu4e-headers-split-view-shrink)

  ;; mark stuff 
  ("!" mu4e-headers-mark-for-read)
  ("?" mu4e-headers-mark-for-unread)
  ("r" mu4e-headers-mark-for-refile)
  ("u" mu4e-headers-mark-for-unmark)
  ("U" mu4e-mark-unmark-all)
  ("d" mu4e-headers-mark-for-trash)
  ("D" mu4e-headers-mark-for-delete)
  ("m" mu4e-headers-mark-for-move)
  ("a" mu4e-headers-action)             ; not really a mark per-se
  ("A" mu4e-headers-mark-for-action)    ; differs from built-in
  ("*" mu4e-headers-mark-for-something)

  ("#" mu4e-mark-resolve-deferred-marks)
  ("%" mu4e-headers-mark-pattern)
  ("&" mu4e-headers-mark-custom)
  ("+" mu4e-headers-mark-for-flag)
  ("-" mu4e-headers-mark-for-unflag)
  ("t" mu4e-headers-mark-subthread)
  ("T" mu4e-headers-mark-thread)

  ;; miscellany
  ("q" mu4e~headers-quit-buffer)
  ("H" mu4e-display-manual)
  ("|" mu4e-view-pipe)             ; does not seem built-in any longer

  ;; switches
  ("O" mu4e-headers-change-sorting)
  ("P" mu4e-headers-toggle-threading)
  ("Q" mu4e-headers-toggle-full-search)
  ("V" mu4e-headers-toggle-skip-duplicates)
  ("W" mu4e-headers-toggle-include-related)

  ;; more miscellany
  ("`" mu4e-update-mail-and-index)      ; differs from built-in
  (";" mu4e-context-switch)  
  ("j" mu4e~headers-jump-to-maildir)

  ("." nil))

(general-define-key
 :keymaps 'mu4e-headers-mode-map
 :states 'normal
 "SPC" 'hydra-mu4e-headers/body)

;;;;;; notmuch

(use-package notmuch
  :straight t)

(add-hook 'notmuch-hello-mode-hook 'evil-emacs-state)

;;;;; irc
;;;;; git
;;; import

(load-file "~/.emacs.d/config/org.el")

;;; custom

(setq custom-file "~/.emacs.d/etc/custom.el")
;; (load custom-file)

(if (not custom-enabled-themes)
    (load-theme 'emacs-sexy-day t))

;;; end

(provide 'init)
