
(setq straight-check-for-modifications
      '(find-when-checking check-on-save))
      
(setq straight-recipe-repositories
      '(org-elpa melpa emacsmirror gnu-elpa-mirror))
      


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
  

(require 'straight-x)


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


;; immediately load the correct org package
(straight-use-package 'org-plus-contrib)



(with-eval-after-load 'straight
  (with-eval-after-load 'hydra
    (defhydra hydra-straight-helper (:hint nil)
      "
_c_heck package   |_f_etch package     |_m_erge package  |_n_ormalize pack  |p_u_sh package
_C_heck all       |_F_etch all (async) |_M_erge all      |_N_ormlize all    |p_U_sh all    
----------------^^+------------------^^+---------------^^+----------------^^+----_q_uit----|
_r_ebuild package |_p_ull package      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild all     |_P_ull all          |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
      ("c" straight-check-package)
      ("C" straight-check-all)
      ("r" straight-rebuild-package)
      ("R" straight-rebuild-all)
      ("f" straight-fetch-package)
      ("F" straight-x-fetch-all)
      ("p" straight-pull-package)
      ("P" straight-pull-all)
      ("m" straight-merge-package)
      ("M" straight-merge-all)
      ("n" straight-normalize-package)
      ("N" straight-normalize-all)
      ("u" straight-push-package)
      ("U" straight-push-all)
      ("v" straight-freeze-versions)
      ("V" straight-thaw-versions)
      ("w" straight-watcher-start)
      ("W" straight-watcher-quit)
      ("g" straight-get-recipe)
      ("e" straight-prune-build)
      ;; For now, automatically close the fetcher window when this hydra exits
      ("q" (lambda ()
             (interactive)
             (ignore-errors
               (let ((buffer "*straight*"))
                 (delete-window (get-buffer-window buffer))
                 (kill-buffer buffer))))
       :color blue))))

(with-eval-after-load 'general
  (general-define-key
    "C-x s" 'hydra-straight-helper/body))


(straight-use-package 'use-package)


(defmacro use-config (after &rest body)
  "use-package like wrapper for configurations"
  (macroexp-progn
   (use-package-require-after-load after body)))

(put 'use-config 'lisp-indent-function 'defun)
   


(use-package no-littering
  :straight t)

(use-config (no-littering recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


(use-package bug-hunter
  :straight t
  :defer t)


(use-package general
  :straight t
  :defer t)

;; TODO: test this
(put 'general-define-key 'lisp-indent-function 'defun)



(defmacro key-fn (action &rest args)
  (let* ((args `(,@args))
         (desc (concat (symbol-name action) "-" (mapconcat (lambda (it) (format "%s" it)) args "-")))
         (fn `(lambda (&optional _)
                (interactive)
                (apply ',action ',args))))
    `(list ,fn :wk ,desc)))


(use-package key-chord
 :straight t
 :config
 (when (not (bound-and-true-p key-chord-mode))
   (key-chord-mode 1)))

(use-package async
  :straight t)

(use-package dash
  :straight t
  :config
  (dash-enable-font-lock))

(use-package f
  :straight t)

(use-package s
  :straight t)

(use-package subr-x)

(setq gc-cons-threshold 50000000)
(setq max-specpdl-size 1200)
(setq max-lisp-eval-depth 800)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-message nil)

(setq find-file-visit-truename nil)


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
  (-each (-difference
          (buffer-list)
          `(,(get-buffer "*dashboard*") ,(current-buffer)))
    'kill-buffer))

(defun nuke-emacs ()
  "Kill all buffers and emacs"
  (interactive)
  (-each (buffer-list) 'kill-buffer)
  (kill-emacs))



(general-define-key
 "C-c r" 'revert-buffer
 "C-c R" 'auto-revert-mode)

(general-define-key
 "C-=" (key-fn text-scale-set 0)
 "C-+" 'text-scale-increase
 "C--" 'text-scale-decrease)



(scroll-bar-mode 0)
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)



(use-package doom-themes
  :straight t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :defer t)

(use-package color-theme-sanityinc-solarized
  :straight t
  :defer t)

(use-package poet-theme
  :straight t
  :defer t)
  

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq custom-safe-themes t)

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; [TODO: maybe use-config]
(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'custom-enabled-themes)
  (add-hook 'desktop-after-read-hook (lambda ()
                                       (mapc 'load-theme
                                             custom-enabled-themes))))


(use-package doom-modeline
  :straight t
  :defer t
  :hook (after-init . doom-modeline-init))

(use-package all-the-icons
  :straight t
  :defer t)

;; (use-package moody
;;   :straight t)

;; (use-config moody
;;   (setq x-underline-at-descent-line t)
;;   (when (not (member '(vc-mode moody-vc-mode) mode-line-format))
;;     (moody-replace-mode-line-buffer-identification)
;;     (moody-replace-vc-mode)))

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


(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line (face-foreground 'org-level-1))
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; [TODO: maybe use-config]
(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'my/current-font)
  (add-to-list 'desktop-globals-to-save 'my/current-font-size))

(add-hook 'after-make-frame-functions 'my/restore-font)
(advice-add 'load-theme :after 'my/restore-font)

(defun set-font (font size)
  (setq my/current-font font)
  (setq my/current-font-size size)
  (set-frame-font (format "%s-%d" font size) nil t))

(defun my/restore-font (&rest _)
  (set-frame-font (format "%s-%d" my/current-font my/current-font-size) nil t))

(use-package dashboard
  :straight t
  :after (evil projectile))

(use-config dashboard
  (setq dashboard-startup-banner "~/.emacs.d/resources/icon.png")
  (setq dashboard-banner-logo-title "Emacs is sexy!")
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)
                          (agenda . 5))))

(general-define-key
 :keymaps 'dashboard-mode-map
 :states 'normal
 "SPC" 'dashboard-next-section
 "S-SPC" 'dashboard-previous-section
 "r" 'dashboard-refresh-buffer)


(defun my/get-or-create-dashboard (&optional concise)
  ;; DONE: Prevent recursive calls from server-process-filter.
  (let ((buffer (get-buffer "*dashboard*")))
    (recentf-cleanup)
    (if buffer
        buffer
      (progn
        (dashboard-insert-startupify-lists)
        (get-buffer "*dashboard*")))))

(setq inhibit-splash-screen nil)
(setq initial-buffer-choice (lambda () (my/get-or-create-dashboard)))


;; (use-package yascroll
;;   :straight t
;;   :defer t
;;   :config
;;   (require 'cl)
;;   (setq yascroll:delay-to-hide 0.5))

;; (global-yascroll-bar-mode 1)


(use-package page-break-lines
  :straight t
  :config
  (global-page-break-lines-mode))

(use-package shackle
  :straight t)


(setq shackle-default-rule '()
      shackle-rules '())

;; TODO: add shackle rules

(setq help-window-select t)



(defun my/prev-buffer-filtered (include-current blacklist)
  (let ((this-buffer (current-buffer)))
    (when (not include-current) (previous-buffer))
    (setq count 0)
    (while
        (not (or (--all? (not (eval it)) blacklist)
                 (and
                  (eq this-buffer (current-buffer))
                  (> count 0))))
      ;; (message (format "%s" count))
      (previous-buffer)
      (setq count (1+ count)))))

(defun my/next-buffer-filtered (include-current blacklist)
  (let ((this-buffer (current-buffer)))
    (when (not include-current) (next-buffer))
    (setq count 0)
    (while
        (not (or (--all? (not (eval it)) blacklist)
                 (and
                  (eq this-buffer (current-buffer))
                  (> count 0))))
      ;; (message (format "%s" count))
      (next-buffer)
      (setq count (1+ count)))))



(defun my/prev-buffer (&optional include-current)
  (interactive)
  (my/prev-buffer-filtered include-current
                           my/default-buffer-blacklist))

(defun my/next-buffer (&optional include-current)
  (interactive)
  (my/next-buffer-filtered include-current
                           my/default-buffer-blacklist))

(defun my/quit-window-kill-buffer ()
  (interactive)
  (quit-window t)
  (my/prev-buffer t))

(defconst my/default-buffer-blacklist
  '((string-match-p "\\*helm.*?\\*" (buffer-name))
    (string-match-p "\\*Completions\\*" (buffer-name))
    (string-match-p "\\*Calculator\\*" (buffer-name))
    (string-match-p "\\*Calc Trail\\*" (buffer-name))
    (string-match-p "\\*info\\*" (buffer-name))
    (string-match-p "\\*straight-process\\*" (buffer-name))
    (string-match-p "\\*Help\\*" (buffer-name))
    (string-match-p "\\*mpd\\*" (buffer-name))
    (string-match-p "\\*MPDEL.*?\\*" (buffer-name))
    (string-match-p "\\*Backtrace\\*" (buffer-name))))

(general-define-key
 :states '(insert normal visual motion emacs)
 "<XF86Back>" 'my/prev-buffer
 "<XF86Forward>" 'my/next-buffer
 "C-q" 'my/quit-window-kill-buffer)



(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode 1))

(use-config eyebrowse
  (setq eyebrowse-wrap-around t))



(general-define-key
  :keymaps 'override
  :states '(normal insert visual motion emacs)
  :prefix "C-a"
  "s" 'ace-window
  "o" 'other-window
  "O" (key-fn other-window -1)

  "d" (key-fn switch-to-buffer "*dashboard*")
  "c" 'my/eyebrowse-projectile-project
  
  "n" 'eyebrowse-next-window-config
  "p" 'eyebrowse-prev-window-config
  "a" 'delete-other-windows
  "x" 'my/close-window-or-eyebrowse
  "X" 'eyebrowse-close-window-config

  "w" 'split-window-vertically
  "q" 'split-window-horizontally
  
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "C-k" (key-fn evil-window-move-very-top)
  "C-j" (key-fn evil-window-move-very-bottom)
  "C-h" (key-fn evil-window-move-far-left)
  "C-l" (key-fn evil-window-move-far-right))

;; [TODO: define functions]

(defun my/eyebrowse-projectile-project ()
  (interactive)
  (let ((projectile-switch-project-action
         (lambda ()
           (eyebrowse-create-window-config)
           (delete-other-windows)
           (projectile-find-file))))
    (projectile-switch-project)))

(defun my/close-window-or-eyebrowse ()
  (interactive)
  (when (condition-case nil (evil-window-delete) (error t))
    (eyebrowse-close-window-config)))



(general-define-key
 :prefix "C-x"
 :keymaps 'override
 "b" 'ivy-switch-buffer
 "k" 'kill-buffer

 "f" 'counsel-find-file
 "C-f" 'counsel-locate
 "F" 'counsel-fzf)


(setq browse-url-generic-program "firefox")


(use-package openwith
  :straight t
  :config
  (openwith-mode t)
  (setq openwith-associations nil))

(use-config openwith
   (setq openwith-associations '(("\\.pdf\\'" "zathura" (file)))))



(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-minibuffer t)
  :config
  (evil-mode 1))

(use-config evil
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands t)
  
  (setq evil-cross-lines nil
        evil-move-beyond-eol nil
        evil-want-fine-undo t
        evil-symbol-word-search nil)
  
  (setq evil-motion-state-cursor 'box   ; █
        evil-visual-state-cursor 'box   ; █
        evil-normal-state-cursor 'box   ; █
        evil-insert-state-cursor 'bar   ; ⎸
        evil-emacs-state-cursor 'hbar)) ; _
        


(use-package evil-collection
  :straight t
  :init
  (setq evil-collection-setup-minibuffer t))



(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode 1))



(general-define-key
  :states '(insert)
  (general-chord "jk") 'evil-normal-state
  (general-chord "kj") 'evil-normal-state)



(general-define-key
 :keymaps '(evil-ex-completion-map evil-ex-search-keymap)
 :states 'insert
 "C-o" 'evil-normal-state
 "<escape>" 'keyboard-escape-quit)

(add-hook 'minibuffer-setup-hook 'evil-insert-state)


(general-define-key
 :keymaps 'minibuffer-local-map
 :states 'normal
 "<escape>" 'keyboard-escape-quit)

(general-define-key
  :keymaps 'evil-motion-state-map
  [down-mouse-1] nil)


(use-package ivy
  :straight t
  :init
  (setq ivy-do-completion-in-region nil)
  (evil-collection-ivy-setup)
  :config
  (ivy-mode 1))

;; (use-package swiper
;;   ;; swiper is distributed with ivy
;;   :after ivy)

(use-package counsel
  :straight t
  :after (ivy)
  :config
  (counsel-mode 1))



(use-config ivy
  (setq ivy-use-selectable-prompt t))

(general-define-key
 :keymaps 'ivy-minibuffer-map
 :states 'insert
 "<RET>" 'ivy-done
 "<tab>" 'ivy-partial-or-done)



(general-define-key
 ;; TODO test this :keymaps 'override
 :states '(normal visual)
 "/" 'swiper)
 


(general-define-key
  :keymaps 'ivy-minibuffer-map
  :states '(insert normal)
  "C-o" 'better-ivy/body
  "<escape>" 'better-ivy/keyboard-escape-quit-and-exit)

(defun better-ivy/get-ivy-matcher-desc ()
  "Return description of `ivy--regex-function'."
  (let ((cell (assq ivy--regex-function ivy-preferred-re-builders)))
    (if cell
        (cdr cell)
      "other")))

(use-config (ivy hydra)
  (defhydra better-ivy (:hint nil :color pink)
    "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(better-ivy/get-ivy-matcher-desc) _t_runcate: %-11`truncate-lines
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



(defun my/ivy-sort-by-length (_name candidates)
  (-sort (lambda (f1 f2)
           (< (length f1) (length f2)))
         (copy-sequence candidates)))

(add-to-list 'ivy-sort-matches-functions-alist
             '(counsel-find-file . my/ivy-sort-by-length)
             t)


(use-package helm
  :straight t
  :defer t)

(use-config helm
  ;; allow display settings to be overriden
  (setq helm-display-function 'pop-to-buffer))
  


(general-define-key
 :keymaps '(helm-map helm-M-x-map)
 :states '(insert normal emacs visual)
 "C-o" 'hydra-helm/body
 "<escape>" 'helm-keyboard-quit)

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



(use-package hippie-expand
  :defer t)

(general-define-key
 :states '(normal insert)
 "C-y" 'hippie-expand)



(use-package hydra
  :straight t)


(use-package smex
 :straight t)


(use-package which-key
  :straight t
  :init
  :config
  (setq which-key-allow-evil-operators t)
  (which-key-mode 1))



(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save/files")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/var/auto-save/files" t)))

(setq make-backup-files t
      version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t)

(setq backup-directory-alist `(("." . "~/.emacs.d/var/backup")))


(setq desktop-dirname             "~/.emacs.d/var/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$"  ;reload tramp paths
      desktop-load-locked-desktop t
      desktop-auto-save-timeout   0
      desktop-restore-frames      nil
      desktop-restore-eager       20
      
      desktop-restore-in-current-display nil)



(add-hook 'after-init-hook 'my/maybe-enable-desktop)

(defun my/maybe-enable-desktop ()
  (if (f-exists?
       (f-join desktop-dirname desktop-base-lock-name))
      (message "Desktop save is off!")
    (progn
      (require 'desktop)
      (desktop-save-mode 1)
      (if (not (ignore-errors (desktop-read)))
          (message "Desktop save failed loading. There are probably some unset protected variables.")
        (message "Desktop save is on!")))))



(use-package recentf
  :straight nil
  :config
  (recentf-mode 1))

(use-config recentf
  (setq recentf-max-menu-items 20)
  (setq recentf-max-saved-items 50)
  (add-to-list 'recentf-exclude "/\\.emacs\\.d/elpa")
  (add-to-list 'recentf-exclude "/nix/store")
  (add-to-list 'recentf-exclude "\\.orhc-bibtex-cache"))

(general-define-key
 "C-x l" 'counsel-recentf)


(savehist-mode 1)


(use-package undo-tree
  :straight t
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo")))
  (global-undo-tree-mode 1))



(use-package operate-on-number
  :straight t)

(general-define-key
 :prefix "C-c o"
 "+"  'apply-operation-to-number-at-point
 "-"  'apply-operation-to-number-at-point
 "*"  'apply-operation-to-number-at-point
 "/"  'apply-operation-to-number-at-point
 "\\" 'apply-operation-to-number-at-point
 "^"  'apply-operation-to-number-at-point
 "<"  'apply-operation-to-number-at-point
 ">"  'apply-operation-to-number-at-point
 "#"  'apply-operation-to-number-at-point
 "%"  'apply-operation-to-number-at-point
 "'"  'operate-on-number-at-point)



(defun my/evil-visual-line-I ()
  (interactive)
  (evil-first-non-blank-of-visual-line)
  (evil-insert 1))

;; [MAYBE TODO: make sure this properly finds the last char]
(defun my/evil-visual-line-A ()
  (interactive)
  (evil-end-of-visual-line)
  (evil-insert 1))



(general-define-key
 :states 'normal
 :keymaps 'visual-line-mode-map
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line
 "^" 'evil-first-non-blank-of-visual-line
 "$" 'evil-end-of-visual-line
 "I" 'my/evil-visual-line-I
 "A" 'my/evil-visual-line-A
 "D" "d$"
 "C" "c$"
 "<down>" "j"
 "<up>" "k")

(general-define-key
 :states '(normal insert)
 :keymaps 'visual-line-mode-map
 "<down>" 'evil-next-visual-line
 "<up>" 'evil-previous-visual-line)


(use-package visual-fill-column
  :straight t
  :hook (visual-line-mode . visual-fill-column-mode))


(defun my/narrow-dwim (p)
  (interactive "P")
  (cond
   ((region-active-p)
    (narrow-to-region (region-beginning)
                      (region-end)))
   ((derived-mode-p 'org-mode)
    ;; `org-edit-src-code' is not a real narrowing
    ;; command. Remove this first conditional if
    ;; you don't want it.
    (cond
     ((ignore-errors (org-edit-src-code) t))
     ((ignore-errors (org-narrow-to-block) t))
     (t (org-narrow-to-subtree))))
   ((derived-mode-p 'latex-mode)
    (LaTeX-narrow-to-environment))
   (t (narrow-to-defun))))

(defun my/widen-dwim ()
  (interactive)
  (cond
   ((bound-and-true-p org-src-mode)
    (org-edit-src-exit))
   (t (widen))))

(general-define-key
  :states '(normal visual)
  "zn" 'my/narrow-dwim
  "zw" 'my/widen-dwim)



(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))



(general-define-key
 :states 'normal
 ">" 'sp-forward-slurp-sexp
 "<" 'sp-forward-barf-sexp)



(defun my/indent-between-braces (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-config smartparens
  (sp-local-pair 'prog-mode "{" nil
                 :post-handlers '((my/indent-between-braces "RET"))))



(use-package paren
  :config
  (show-paren-mode))
  


(use-package yasnippet
  :straight t
  :defer t)

(use-package ivy-yasnippet
  :straight t
  :after yasnippet)


;; [TODO: manage snippets]
; (org-babel-load-file (f-join user-emacs-directory "snippets.org"))


(use-package avy
  :straight t
  :config
  (setq avy-case-fold-search nil))

(general-define-key
  :states '(normal visual)
  "s" 'evil-avy-goto-char
  "S" nil
  "SPC" 'evil-avy-goto-word-or-subword-1)



(use-package expand-region
  :straight t)

(general-define-key
 "C-0" 'er/expand-region)

(general-define-key
 :states '(normal visual)
 "+" 'er/expand-region
 "-" 'er/contract-region)



(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default truncate-lines t)
(add-hook 'custom-mode-hook (lambda () (setq-local truncate-lines nil)))


(use-package lsp-mode
  :straight t
  :after projectile
  :commands lsp)

(use-package lsp-ui
  :straight t
  :after lsp-mode)

(use-package company-lsp
  :straight t
  :after (lsp-mode company yasnippet))



(use-package dap-mode
  :straight t
  :after lsp-mode)



(use-package projectile
  :straight t
  :config
  (projectile-mode 1))

(use-package counsel-projectile
  :straight t
  :after (projectile counsel)
  :config
  (counsel-projectile-mode))

(use-config projectile
  (setq projectile-completion-system 'ivy))



(use-config projectile
  (setq projectile-project-root-files-functions '(projectile-root-top-down))
  (setq projectile-project-root-files
        '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile"))) 


(use-config projectile
  (general-define-key
    "C-x p" projectile-command-map))


(use-package company
  :straight t
  :defer t
  :hook ((prog-mode . company-mode)
         (company-mode . yas-minor-mode)))



(use-config company
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  (setq company-dabbrev-downcase nil)
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-tng-frontend
                            company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)))

(general-define-key
 :keymaps 'company-active-map
 :states nil
 "<tab>" 'company-select-next
 "<backtab>" 'company-select-previous
 "<C-y>" 'company-complete)


(use-package outshine
  :straight t
  :after org
  :hook (emacs-lisp-mode . outshine-mode))


(defun my/outshine-smart-tab ()
  (interactive)
  (if (outline-on-heading-p)
      (outshine-cycle)
    (indent-for-tab-command)))

;; terrible hack to keep font-lock after reload
(use-config outshine
  (ignore-errors
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (if (bound-and-true-p outshine-mode)
            (font-lock-add-keywords nil (outshine-fontify-headlines (outshine-calc-outline-regexp)))
          (outshine-font-lock-flush))))))

;; use same colors as org-mode
(use-config outshine
  (set-face-attribute 'outshine-level-1 nil :inherit 'org-level-1)
  (set-face-attribute 'outshine-level-2 nil :inherit 'org-level-2)
  (set-face-attribute 'outshine-level-3 nil :inherit 'org-level-3)
  (set-face-attribute 'outshine-level-4 nil :inherit 'org-level-4)
  (set-face-attribute 'outshine-level-5 nil :inherit 'org-level-5)
  (set-face-attribute 'outshine-level-6 nil :inherit 'org-level-6)
  (set-face-attribute 'outshine-level-7 nil :inherit 'org-level-7)
  (set-face-attribute 'outshine-level-8 nil :inherit 'org-level-8))


(use-package flycheck
  :straight t
  :defer t)

(use-package ispell
  :defer t)

(use-package flyspell
  :straight t
  :defer t)

(setq ispell-program-name "hunspell")

(use-config flyspell
  (setq flyspell-duplicate-distance 0)
  (setq flyspell-issue-message-flag nil))

(setq ispell-dictionary "en_US")
(setq flyspell-default-dictionary "en_US")


;; [TODO: fix dictionaries]
(setq my/ispell-wanted-dictionary-list
      '("en_US" "de_AT"
        "en_US,de_AT"))

(setq ispell-dictionary-alist
      (-map (lambda (dict) `(,dict
	                          "[[:alpha:]]" "[^[:alpha:]]" "[']"
	                          nil ("-d" ,dict)
	                          nil utf-8))
            my/ispell-wanted-dictionary-list))


(use-package lispy
  :straight t
  :defer t
  :hook (emacs-lisp-mode . lispy-mode))

(use-package aggressive-indent
  :straight t
  :defer t
  :hook (lispy-mode . aggressive-indent-mode))

(use-package nix-mode
  :straight t
  :defer t
  :mode "\\.nix\\'")


(use-package tex
  :straight (auctex :type git :host github 
                    :repo "emacs-straight/auctex")
  :defer t
  :hook
  (TeX-mode . visual-line-mode)
  (LaTeX-mode . visual-line-mode)
  :config
  (TeX-source-correlate-mode)
  (TeX-PDF-mode))


(use-config tex
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura"))
  
  (add-to-list 'TeX-expand-list
               '("%sn" (lambda () server-name)))
  
  (add-to-list 'TeX-view-program-list
               '("Zathura"
                 ("zathura %o"
                  (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
                 "zathura")))

(use-package markdown-mode
  :straight t
  :defer t
  :hook (markdown-mode . visual-line-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))


(use-package rust-mode
  :straight t
  :mode "\\.rs\\'") ;; this is already done by rust-mode

(use-package flycheck-rust
  :straight t
  :after rust-mode
  :hook (rust-mode . flycheck-rust-setup))

(use-package cargo
  :straight t
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))



(defun my/setup-rust-env ()
  "RLS requires some environment variables to be setup. We use rustup to get the values."
  (when (executable-find "rustup")
    (setq rust-default-toolchain
          ;; [TODO: s.el]
          (car (s-split " " (-first
                             (lambda (line) (s-match "default" line)) 
                             (s-lines (shell-command-to-string "rustup toolchain list"))))))
    ;; tell racer to use the rustup-managed rust-src
    ;; rustup component add rust-src
    (setq rust-src-path (f-join (getenv "HOME") ".multirust/toolchains" rust-default-toolchain "lib/rustlib/src/rust/src"))
    (setq rust-bin-path (f-join (getenv "HOME") ".multirust/toolchains" rust-default-toolchain "bin"))
    (setq racer-rust-src-path rust-src-path)
    (setenv "RUST_SRC_PATH" rust-src-path)
    (setenv "RUSTC" rust-bin-path))
  ;; only call once
  (remove-hook 'rust-mode-hook 'my/setup-rust-env))

(add-hook 'rust-mode-hook 'my/setup-rust-env)



(use-package dired
  :defer t
  :init
  ;; most of these bindings are overriden later
  (evil-collection-dired-setup))

(use-config dired
  (setq dired-listing-switches "-alhv")
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t))



;; use evil even without evil-collection
(add-hook 'dired-mode-hook 'evil-normal-state)

(general-define-key
 :keymaps 'dired-mode-map
 :states '(normal visual)
 "h" 'backward-char
 "j" 'next-line
 "k" 'previous-line
 "l" 'forward-char
 "f" 'dired-find-file
 "v" 'evil-visual-line

 "n" 'dired-next-marked-file
 "N" 'dired-prev-marked-file

 "H" 'dired-hide-details-mode
 "W" (lambda ()
       (interactive)
       (wdired-change-to-wdired-mode)
       (evil-normal-state) (forward-char))

 "<backspace>" 'dired-up-directory
 "<RET>" 'dired-find-file

 "q" 'quit-window)



(use-package dired+
  :straight (dired+
             :type git :host github
             :repo "emacsmirror/emacswiki.org"
             :files ("dired+.el"))
  :after dired
  :hook (dired-mode . image-dired-minor-mode)
  :init
  ;; setup evil and add missing lazy binds
  (evil-collection-image-dired-setup)
  (general-define-key
    :keymaps 'image-dired-thumbnail-mode-map
    :states 'normal
    "<left>" 'image-dired-backward-image
    "<right>" 'image-dired-forward-image
    "<up>" 'image-dired-previous-line
    "<down>" 'image-dired-next-line))



(general-define-key
  :states '(normal visual insert emacs)
  :keymaps 'dired-mode-map
  "C-t" (cdr (assoc 20 image-dired-minor-mode-map))
  "C-t C-r" 'image-dired)

(evil-set-initial-state 'image-dired-minor-mode 'emacs)



(use-package diredfl
  :straight t
  :after dired
  :config
  (diredfl-global-mode))



(use-package dired-filter
  :straight t
  :after dired
  :hook (dired-mode . dired-filter-mode))



(general-define-key
  :keymaps 'dired-mode-map
  :states '(normal visual)
  "F" dired-filter-map
  "M" dired-filter-mark-map)



(use-package dired-ranger
  :straight t
  :after dired)



(general-define-key
 :keymaps 'dired-mode-map
 :states '(normal visual)
 "y" 'dired-ranger-copy
 "a" (key-fn dired-ranger-copy t)
 "pc" 'dired-ranger-paste
 "pm" 'dired-ranger-move

 "U" (lambda ()
       (interactive)
       (dired-unmark-all-marks)
       (ring-remove dired-ranger-copy-ring 0)))

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



(use-package dired-quick-sort
  :straight t
  :after dired)



(general-define-key
  :keymaps 'dired-mode-map
  :states '(normal visual)
  "o" 'hydra-dired-quick-sort/body)



(use-package magit
  :straight t
  :defer t)

(general-define-key
 :prefix "C-x"
 "g" 'magit-status)


(use-package forge
  :straight (forge :host github :repo "magit/forge")
  :after magit)

(use-config evil-collection
  (evil-collection-help-setup))


(use-package mpdel
  :straight t
  :defer t)

(use-package ivy-mpdel
  :straight t
  :after mpdel)


(setq message-send-mail-function 'message-send-mail-with-sendmail
      send-mail-function 'sendmail-send-it)


;; substitute sendmail with msmtp
(setq sendmail-program "msmtp")

;; allow setting account through email header
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil t)



(add-hook 'message-send-hook
          (lambda ()
            (unless (y-or-n-p "Sure you want to send this?")
              (signal 'quit nil))))


(use-package org
  :straight org-plus-contrib
  :requires (org-capture org-protocol))

(setq org-default-notes-file "~/Projects/notes.org")
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE BUT" "DONE")
        (sequence "MAYBE" "CANCELED" "|")))

(use-config org
  (org-add-link-type
   "project" 'projectile-switch-project-by-name))

(setq org-capture-templates
      '(("g" "Generic" entry
         ;; [TODO: make this work]
         (file+function "" (lambda ()
                             (goto-char (opj/get-pos t))))
         "* %?%i")
        ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n")
        ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
         "* %?")
        ("L" "Protocol Link" entry (file+headline org-default-notes-file "Weblinks")
         "* %:description%?\n[[%:link]]\n%t"
         :immediate-finish t)
        ("p" "Protocol Selection" entry (file+headline org-default-notes-file "Weblinks")
         "* %:description\n[[%:link]]\n%t\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?")))

(add-hook 'org-mode-hook 'visual-line-mode)

(load-library "org-tempo")


(use-package org-pretty-jump
  :straight (org-pretty-jump :type git :host github
                             :repo "LeOtaku/org-pretty-jump")
  :after org)

(general-define-key
  :states '(normal visual)
  "gh" 'opj-contrib/jump
  "gH" (key-fn opj-contrib/jump 1))


(use-package worf
  :straight t
  :after org)

(setq org-adapt-indentation nil)


(use-config org-mode
  (setq my/org-list-font-locks
        '(("^ *\\([0-9]+\\.\\)" (1 'bold))
          ("^ *\\([0-9]+)\\)" (1 'bold))
          ("^ *\\([+-]\\)" (1 'bold))
          ("^ +\\([*]\\)" (1 'bold))))
  
  (setq my/org-misc-font-locks
        '(("\\(->\\)" (1 'bold))
          ("\\(@\\)" (1 'org-meta-line))))

  (font-lock-add-keywords 'org-mode (concat
                                     my/org-misc-font-locks
                                     my/org-list-font-locks)))



;; [TODO: ensure this works]
; (defface org-canceled '(:color red) "")
; (defface org-maybe    '(:color magenta) "")

(setq org-todo-keyword-faces
      '(("TODO"      . 'org-todo)
        ("DOING"     . 'org-todo)
        ("DONE BUT"  . 'org-done)
        ("DONE"      . 'org-done)
        ("MAYBE"     . 'org-maybe)
        ("PLANNED"   . 'org-maybe)
        ("CANCELED"  . 'org-canceled)
        ("DROPPED"   . 'org-canceled)))


(setq org-image-actual-width 400)
(setq org-startup-with-inline-images t)

(setq org-link-file-path-type 'relative)

(add-to-list 'ivy-sort-matches-functions-alist
             '(org-insert-link . my/ivy-sort-by-length)
             t)

(use-config (org-capture evil)
  (add-hook 'org-capture-mode-hook (lambda () (evil-append 1))))

(use-config org
  (setq org-latex-pdf-process
        (list "latexmk -interaction=nonstopmode -output-directory=%o -shell-escape -bibtex -f -pdf %f")))


(use-package ox-hugo
  :straight t)



(use-package org-ref
  :straight t
  :after (org ivy-bibtex)
  :requires (doi-utils org-ref-isbn org-ref-ivy org-ref-helm)
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite))



(setq org-ref-biblatex-types
  '(;; "Cite"
    "parencite" ;; "Parencite"
    "footcite" "footcitetext"
    "textcite"  ;; "Textcite"
    "smartcite" ;; "Smartcite"
    "cite*" "parencite*" "supercite"
    "autocite"  ;; "Autocite"
    "autocite*" ;; "Autocite*"
    ;; "Citeauthor*"
    "citetitle" "citetitle*"
    "citedate" "citedate*"
    "citeurl"
    "fullcite" "footfullcite"
    ;; "volcite" "Volcite" cannot support the syntax
    "notecite"  ;; "Notecite"
    "pnotecite" ;; "Pnotecite"
    "fnotecite"
    ;; multicites. Very limited support for these.
    "cites"      ;; "Cites"
    "parencites" ;; "Parencites"
    "footcites" "footcitetexts"
    "smartcites"  ;; "Smartcites"
    "textcites" ;; "Textcites"
    "supercites" "autocites"
    ;; "Autocites"
    ))



(defun my/org-ref-ivy-insert-cite-link ()
  (interactive)
  (let ((bibtex-completion-bibliography org-ref-bibliography-files))
    (setq org-ref-ivy-cite-marked-candidates '())

    (ivy-read "Open: " (bibtex-completion-candidates)
	          :require-match t
	          :keymap org-ref-ivy-cite-keymap
	          :re-builder org-ref-ivy-cite-re-builder
	          :action 'or-ivy-bibtex-insert-cite
	          :caller 'my/org-ref-ivy-insert-cite-link)))

(setq org-ref-insert-link-function 'my/org-ref-ivy-insert-cite-link)
(setq org-ref-insert-cite-function 'my/org-ref-ivy-insert-cite-link)
(setq org-ref-insert-label-function 'org-ref-ivy-insert-label-link)
(setq org-ref-insert-ref-function 'org-ref-ivy-insert-ref-link)
(setq org-ref-cite-onclick-function 'org-ref-ivy-onclick-actions)


(setq org-src-window-setup 'current-window)

(setq org-src-preserve-indentation nil 
      org-edit-src-content-indentation 0
      ;; [TODO make this work with custom bindings]
      org-src-tab-acts-natively t)

(advice-add 'evil-write :before-while 'my/advice-evil-write)
(defun my/advice-evil-write (&rest ignore)
  (if (org-src-edit-buffer-p)
      (progn 
        (org-edit-src-save)
        nil)
    t))

(advice-add 'org-babel-tangle :before-while 'my/advice-org-babel-tangle)
(defun my/advice-org-babel-tangle (&optional arg target-file lang)
  (let ((bin (f-join (getenv "HOME") ".nimble/bin/ntangle")))
    (if (f-exists-p bin)
        (progn 
          (async-start-process
           ;; name + cmd
           "org-async-tangle" bin
           ;; callback
           (lambda (i) (message (format "%s: exited sucessfully" i)))
           ;; arguments
           buffer-file-name)
          nil)
      t)))

(provide 'init)
