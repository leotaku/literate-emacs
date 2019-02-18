;; [[file:~/.emacs.d/init.org::*straight.el][straight.el:1]]
(setq straight-check-for-modifications
      '(find-when-checking check-on-save))
      
(setq straight-recipe-repositories
      '(org-elpa melpa emacsmirror gnu-elpa))
;; straight.el:1 ends here

;; [[file:~/.emacs.d/init.org::*straight.el][straight.el:2]]
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
;; straight.el:2 ends here

;; [[file:~/.emacs.d/init.org::*org-mode%20hack][org-mode hack:1]]
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
;; org-mode hack:1 ends here

;; [[file:~/.emacs.d/init.org::*hydra][hydra:1]]
(with-eval-after-load straight
  (with-eval-after-load hydra
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
      ("q" nil))))
;; hydra:1 ends here

;; [[file:~/.emacs.d/init.org::*use-package][use-package:1]]
(straight-use-package 'use-package)
;; use-package:1 ends here

;; [[file:~/.emacs.d/init.org::*use-config%20macro][use-config macro:1]]
(defmacro use-config (after &rest body)
  "use-package like wrapper for configurations"
  (macroexp-progn
   (use-package-require-after-load after body)))

(put 'use-config 'lisp-indent-function 'defun)
;; use-config macro:1 ends here

;; [[file:~/.emacs.d/init.org::*no-littering][no-littering:1]]
(use-package no-littering
  :straight t)

(use-config (no-littering recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))
;; no-littering:1 ends here

;; [[file:~/.emacs.d/init.org::*bug-hunter][bug-hunter:1]]
(use-package bug-hunter
  :straight t
  :defer t)
;; bug-hunter:1 ends here

;; [[file:~/.emacs.d/init.org::*general][general:1]]
(use-package general
  :straight t
  :defer t)

;; TODO: test this
(put 'general-define-key 'lisp-indent-function 'defun)
;; general:1 ends here

;; [[file:~/.emacs.d/init.org::*general%20key-fn][general key-fn:1]]
(defmacro key-fn (action &rest args)
  (let ((fun-name (intern (format "hack/general-which-key-functions-%s-%s" (symbol-name action) args))))
    `(progn
       (defun ,fun-name ()
         (interactive)
         (apply ',action ',args))
       (quote (,fun-name
               :wk ,(concat (symbol-name action)
                            "-"
                            (mapconcat 'identity args "-")))))))
;; general key-fn:1 ends here

;; [[file:~/.emacs.d/init.org::*key-chord][key-chord:1]]
(use-package key-chord
  :config
  (key-chord-mode 1))
;; key-chord:1 ends here

;; [[file:~/.emacs.d/init.org::*subr][subr:1]]
(use-package subr)
;; subr:1 ends here

;; [[file:~/.emacs.d/init.org::*dash][dash:1]]
(use-package dash
  :straight t
  :config
  (dash-enable-font-lock))
;; dash:1 ends here

;; [[file:~/.emacs.d/init.org::*f][f:1]]
(use-package f
  :straight t)
;; f:1 ends here

;; [[file:~/.emacs.d/init.org::*s][s:1]]
(use-package s
  :straight t)
;; s:1 ends here

;; [[file:~/.emacs.d/init.org::*Default][Default:1]]
(setq gc-cons-threshold 50000000)
(setq max-specpdl-size 1200)
(setq max-lisp-eval-depth 800)
;; Default:1 ends here

;; [[file:~/.emacs.d/init.org::*Default][Default:2]]
(fset 'yes-or-no-p 'y-or-n-p)
;; Default:2 ends here

;; [[file:~/.emacs.d/init.org::*Default][Default:3]]
(setq inhibit-message nil)
;; Default:3 ends here

;; [[file:~/.emacs.d/init.org::*Default][Default:4]]
(setq find-file-visit-truename nil)
;; Default:4 ends here

;; [[file:~/.emacs.d/init.org::*Commands][Commands:1]]
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
;; Commands:1 ends here

;; [[file:~/.emacs.d/init.org::*Look][Look:1]]
(scroll-bar-mode 0)
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)
;; Look:1 ends here

;; [[file:~/.emacs.d/init.org::*Themes][Themes:1]]
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
;; Themes:1 ends here

;; [[file:~/.emacs.d/init.org::*Themes][Themes:2]]
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; Themes:2 ends here

;; [[file:~/.emacs.d/init.org::*Themes][Themes:3]]
(setq custom-safe-themes t)
;; Themes:3 ends here

;; [[file:~/.emacs.d/init.org::*Themes][Themes:4]]
(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
;; Themes:4 ends here

;; [[file:~/.emacs.d/init.org::*Themes][Themes:5]]
(add-to-list 'desktop-globals-to-save 'custom-enabled-themes)
(add-hook 'desktop-after-read-hook (lambda ()
                                     (mapc 'load-theme
                                           custom-enabled-themes)))
;; Themes:5 ends here

;; [[file:~/.emacs.d/init.org::*Modeline][Modeline:1]]
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
;; Modeline:1 ends here

;; [[file:~/.emacs.d/init.org::*Bell][Bell:1]]
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line (face-foreground 'org-level-1))
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))
;; Bell:1 ends here

;; [[file:~/.emacs.d/init.org::*Dashboard][Dashboard:1]]
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
;; Dashboard:1 ends here

;; [[file:~/.emacs.d/init.org::*Dashboard][Dashboard:2]]
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
;; Dashboard:2 ends here

;; [[file:~/.emacs.d/init.org::*Scrollbar][Scrollbar:1]]
;; (use-package yascroll
;;   :straight t
;;   :defer t
;;   :config
;;   (require 'cl)
;;   (setq yascroll:delay-to-hide 0.5))

;; (global-yascroll-bar-mode 1)
;; Scrollbar:1 ends here

;; [[file:~/.emacs.d/init.org::*Page%20breaks][Page breaks:1]]
(use-package page-break-lines
  :straight t
  :config
  (global-page-break-lines-mode))
;; Page breaks:1 ends here

;; [[file:~/.emacs.d/init.org::*Popups/Shackle][Popups/Shackle:1]]
(use-package shackle
  :straight t)
;; Popups/Shackle:1 ends here

;; [[file:~/.emacs.d/init.org::*Popups/Shackle][Popups/Shackle:2]]
(setq shackle-default-rule '()
      shackle-rules '())

;; TODO: add shackle rules
;; Popups/Shackle:2 ends here

;; [[file:~/.emacs.d/init.org::*Buffer%20list][Buffer list:1]]
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
;; Buffer list:1 ends here

;; [[file:~/.emacs.d/init.org::*Buffer%20list][Buffer list:2]]
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
    (string-match-p "\\*Backtrace\\*" (buffer-name))))

(general-define-key
 :states (insert normal visual motion emacs)
 "<XF86Back>" 'my/prev-buffer
 "<XF86Forward>" 'my/next-buffer
 "C-q" 'my/quit-window-kill-buffer)
;; Buffer list:2 ends here

;; [[file:~/.emacs.d/init.org::*Tabs/Eyebrowse][Tabs/Eyebrowse:1]]
(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode))

(use-config eyebrowse
  (setq eyebrowse-wrap-around t))
;; Tabs/Eyebrowse:1 ends here

;; [[file:~/.emacs.d/init.org::*Misc/Keys][Misc/Keys:1]]
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
 "x" 'delete-other-windows
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
  nil)

(defun my/close-window-or-eyebrowse ()
  (interactive)
  nil)
;; Misc/Keys:1 ends here

;; [[file:~/.emacs.d/init.org::*Misc/Keys][Misc/Keys:2]]
(general-define-key
 :prefix "C-x"
 :keymaps 'override
 "b" 'ivy-switch-buffer
 "k" 'kill-buffer

 "f" 'counsel-find-file
 "C-f" 'counsel-locate
 "F" 'counsel-fzf)
;; Misc/Keys:2 ends here

;; [[file:~/.emacs.d/init.org::*OS%20Integration][OS Integration:1]]
(setq browse-url-generic-program "firefox")
;; OS Integration:1 ends here

;; [[file:~/.emacs.d/init.org::*OS%20Integration][OS Integration:2]]
(use-package openwith
  :straight t
  :config
  (openwith-mode t)
  (setq openwith-associations nil))

(use-config openwith
   (setq openwith-associations '(("\\.pdf\\'" "zathura" (file)))))
;; OS Integration:2 ends here

;; [[file:~/.emacs.d/init.org::*evil][evil:1]]
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
;; evil:1 ends here

;; [[file:~/.emacs.d/init.org::*evil][evil:2]]
(use-package evil-collection
  :straight t
  :init
  (setq evil-collection-setup-minibuffer t))
;; evil:2 ends here

;; [[file:~/.emacs.d/init.org::*evil][evil:4]]
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
;; evil:4 ends here

;; [[file:~/.emacs.d/init.org::*evil][evil:5]]
(general-define-key
  :states '(insert)
  (general-chord "jk") 'evil-normal-state
  (general-chord "kj") 'evil-normal-state)
;; evil:5 ends here

;; [[file:~/.emacs.d/init.org::*ivy][ivy:1]]
(use-package ivy
  :straight t
  :init
  (setq ivy-do-completion-in-region nil)
  :config
  (ivy-mode 1))

;; (use-package swiper
;;   ;; swiper is distributed with ivy
;;   :after ivy)

(use-package counsel
  :straight t
  :after ivy)
;; ivy:1 ends here

;; [[file:~/.emacs.d/init.org::*ivy][ivy:2]]
(use-config ivy
  (setq ivy-use-selectable-prompt t))

;; (general-define-key
;; :keymaps 'ivy-minibuffer-map
;; :states 'insert
;; "<RET>" 'ivy-done
;; "<tab>" 'ivy-partial-or-done)
;; ivy:2 ends here

;; [[file:~/.emacs.d/init.org::*ivy][ivy:3]]
;; swiper
(general-define-key
 ;; TODO test this :keymaps 'override
 :states '(normal visual)
 "/" 'swiper)
;; ivy:3 ends here

;; [[file:~/.emacs.d/init.org::*ivy][ivy:4]]
(general-define-key
 :keymaps 'ivy-minibuffer-map
 :states '(insert normal)
 "C-o" 'better-ivy/body
 "<escape>" 'better-ivy/keyboard-escape-quit-and-exit)

(use-config (ivy hydra)
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
;; ivy:4 ends here

;; [[file:~/.emacs.d/init.org::*ivy][ivy:5]]
(defun my/ivy-sort-by-length (_name candidates)
  (-sort (lambda (f1 f2)
           (< (length f1) (length f2)))
         (copy-sequence candidates)))
;; ivy:5 ends here

;; [[file:~/.emacs.d/init.org::*helm][helm:1]]
(use-package helm
  :straight t
  :defer t)

(use-config helm
  ;; allow display settings to be overriden
  (setq helm-display-function 'pop-to-buffer))
;; helm:1 ends here

;; [[file:~/.emacs.d/init.org::*helm][helm:2]]
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
;; helm:2 ends here

;; [[file:~/.emacs.d/init.org::*hippie-expand][hippie-expand:1]]
(use-package hippie-expand
  :defer t)

(general-define-key
 :states '(normal insert)
 "C-y" 'hippie-expand)
;; hippie-expand:1 ends here

;; [[file:~/.emacs.d/init.org::*hydra][hydra:1]]
(use-package hydra
  :straight t)
;; hydra:1 ends here

;; [[file:~/.emacs.d/init.org::*which-key][which-key:1]]
(use-package which-key
  :straight t
  :init
  :config
  (setq which-key-allow-evil-operators t)
  (which-key-mode 1))
;; which-key:1 ends here

;; [[file:~/.emacs.d/init.org::*Auto-save][Auto-save:1]]
(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save/files")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/var/auto-save/files" t)))
;; Auto-save:1 ends here

;; [[file:~/.emacs.d/init.org::*Backup][Backup:1]]
(setq make-backup-files t)
(setq version-control t)
(setq delete-old-versions nil)
(setq backup-by-copying t)

(setq backup-directory-alist `(("." . "~/.emacs.d/var/backup")))
;; Backup:1 ends here

;; [[file:~/.emacs.d/init.org::*Desktop][Desktop:1]]
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
;; Desktop:1 ends here

;; [[file:~/.emacs.d/init.org::*Desktop][Desktop:2]]
(if (file-exists-p
     (f-join desktop-dirname desktop-base-lock-name))
    (message "Desktop save has been turned off!")
  (progn
    (desktop-save-mode 1)
    (message "Desktop save is on!")))
;; Desktop:2 ends here

;; [[file:~/.emacs.d/init.org::*Recentf][Recentf:1]]
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
;; Recentf:1 ends here

;; [[file:~/.emacs.d/init.org::*Undo-tree][Undo-tree:1]]
(use-package undo-tree
  :straight t
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo")))
  (global-undo-tree-mode 1))
;; Undo-tree:1 ends here

;; [[file:~/.emacs.d/init.org::*Prelude-esque][Prelude-esque:1]]
(use-package operate-on-number
  :straight t)

(general-define-key
 :prefix "C-c ."
 "+" apply-operation-to-number-at-point
 "-" apply-operation-to-number-at-point
 "*" apply-operation-to-number-at-point
 "/" apply-operation-to-number-at-point
 "\\" apply-operation-to-number-at-point
 "^" apply-operation-to-number-at-point
 "<" apply-operation-to-number-at-point
 ">" apply-operation-to-number-at-point
 "#" apply-operation-to-number-at-point
 "%" apply-operation-to-number-at-point
 "'" operate-on-number-at-point)
;; Prelude-esque:1 ends here

;; [[file:~/.emacs.d/init.org::*Visual%20line][Visual line:1]]
(defun my/evil-visual-line-I ()
  (interactive)
  (evil-first-non-blank-of-visual-line)
  (evil-insert 1))

(defun my/evil-visual-line-A ()
  (interactive)
  (evil-end-of-visual-line)
  (evil-insert 1))
;; Visual line:1 ends here

;; [[file:~/.emacs.d/init.org::*Visual%20line][Visual line:2]]
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
;; Visual line:2 ends here

;; [[file:~/.emacs.d/init.org::*Visual%20line][Visual line:3]]
(use-package visual-fill-column
  :straight t
  :hook (visual-line-mode . visual-fill-column-mode))
;; Visual line:3 ends here

;; [[file:~/.emacs.d/init.org::*Pairs][Pairs:1]]
(use-package smartparens
  :straight t
  :requires smartparens-config
  :config
  (smartparens-global-mode))
;; Pairs:1 ends here

;; [[file:~/.emacs.d/init.org::*Pairs][Pairs:2]]
(general-define-key
 :states 'normal
 ">" 'sp-forward-slurp-sexp
 "<" 'sp-forward-barf-sexp)
;; Pairs:2 ends here

;; [[file:~/.emacs.d/init.org::*Pairs][Pairs:3]]
(defun my/indent-between-braces (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair 'prog-mode "{" nil
               :post-handlers '((my/indent-between-braces "RET")))
;; Pairs:3 ends here

;; [[file:~/.emacs.d/init.org::*Pairs][Pairs:4]]
(use-package paren
  :config
  (show-paren-mode))
;; Pairs:4 ends here

;; [[file:~/.emacs.d/init.org::*Snippets][Snippets:1]]
(use-package yasnippet
  :straight t
  :defer t)

(use-package ivy-yasnippet
  :straight t
  :after yasnippet)
;; Snippets:1 ends here

;; [[file:~/.emacs.d/init.org::*Snippets][Snippets:2]]
(org-babel-load-file (f-join user-emacs-directory "snippets.org"))
;; Snippets:2 ends here

;; [[file:~/.emacs.d/init.org::*Jumping][Jumping:1]]
(use-package avy
  :straight t
  :config
  (setq avy-case-fold-search nil))

(general-define-key
  :states '(normal visual)
  "s" 'evil-avy-goto-char
  "S" nil
  "SPC" 'evil-avy-goto-word-or-subword-1)
;; Jumping:1 ends here

;; [[file:~/.emacs.d/init.org::*Expand-region][Expand-region:1]]
(use-package expand-region
  :straight t)

(general-define-key
 :keymaps 'override
 "C-0" 'er/expand-region)
;; Expand-region:1 ends here

;; [[file:~/.emacs.d/init.org::*Defaults][Defaults:1]]
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Defaults:1 ends here

;; [[file:~/.emacs.d/init.org::*Defaults][Defaults:2]]
(setq-default truncate-lines t)
(add-hook 'custom-mode-hook (lambda () (setq-local truncate-lines nil)))
;; Defaults:2 ends here

;; [[file:~/.emacs.d/init.org::*LSP/DAP%20family][LSP/DAP family:1]]
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
;; LSP/DAP family:1 ends here

;; [[file:~/.emacs.d/init.org::*LSP/DAP%20family][LSP/DAP family:2]]
(use-package dap-mode
  :straight t
  :after lsp-mode)
;; LSP/DAP family:2 ends here

;; [[file:~/.emacs.d/init.org::*Projectile][Projectile:1]]
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
;; Projectile:1 ends here

;; [[file:~/.emacs.d/init.org::*Projectile][Projectile:2]]
(use-config projectile
  (setq projectile-project-root-files-functions '(projectile-root-top-down))
  (setq projectile-project-root-files
        '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile")))
;; Projectile:2 ends here

;; [[file:~/.emacs.d/init.org::*Company][Company:1]]
(use-package company
  :straight t
  :defer t
  :hook ((prog-mode . company-mode)
         (company-mode . yas-minor-mode)))
;; Company:1 ends here

;; [[file:~/.emacs.d/init.org::*Company][Company:2]]
(use-config company
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  (setq company-dabbrev-downcase nil)
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-tng-frontend
                            company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)))
;; Company:2 ends here

;; [[file:~/.emacs.d/init.org::*Outline/shine][Outline/shine:1]]
(use-package outshine
  :straight t
  :after org
  :add-hook (emacs-lisp-mode . outshine-mode))
;; Outline/shine:1 ends here

;; [[file:~/.emacs.d/init.org::*Outline/shine][Outline/shine:2]]
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
  (set-face-attribute 'outshine-level-1 nil :inherit 'org-level-1 :height 100)
  (set-face-attribute 'outshine-level-2 nil :inherit 'org-level-2 :height 100)
  (set-face-attribute 'outshine-level-3 nil :inherit 'org-level-3 :height 100)
  (set-face-attribute 'outshine-level-4 nil :inherit 'org-level-4)
  (set-face-attribute 'outshine-level-5 nil :inherit 'org-level-5)
  (set-face-attribute 'outshine-level-6 nil :inherit 'org-level-6)
  (set-face-attribute 'outshine-level-7 nil :inherit 'org-level-7)
  (set-face-attribute 'outshine-level-8 nil :inherit 'org-level-8))
;; Outline/shine:2 ends here

;; [[file:~/.emacs.d/init.org::*Flycheck][Flycheck:1]]
(use-package flycheck
  :straight t
  :defer t)
;; Flycheck:1 ends here

;; [[file:~/.emacs.d/init.org::*Fly/ispell][Fly/ispell:1]]
(use-package ispell
  :defer t)
;; Fly/ispell:1 ends here

;; [[file:~/.emacs.d/init.org::*Fly/ispell][Fly/ispell:2]]
(use-package flyspell
  :straight t
  :defer t)
;; Fly/ispell:2 ends here

;; [[file:~/.emacs.d/init.org::*Fly/ispell][Fly/ispell:3]]
(setq ispell-program-name "hunspell")
;; Fly/ispell:3 ends here

;; [[file:~/.emacs.d/init.org::*Fly/ispell][Fly/ispell:4]]
(use-config flyspell
  (setq flyspell-duplicate-distance 0)
  (setq flyspell-issue-message-flag nil))
;; Fly/ispell:4 ends here

;; [[file:~/.emacs.d/init.org::*Dictionaries][Dictionaries:1]]
(setq ispell-dictionary "en_US")
(setq flyspell-default-dictionary "en_US")
;; Dictionaries:1 ends here

;; [[file:~/.emacs.d/init.org::*Dictionaries][Dictionaries:2]]
(setq ispell-dictionary-alist
      `(("en_US")
        ("de_AT")
        ("en_US,de_AT")))

(dolist (dict ispell-dictionary-alist)
  (ispell-hunspell-fill-dictionary-entry (car dict)))
;; Dictionaries:2 ends here

;; [[file:~/.emacs.d/init.org::*Emacs%20Lisp][Emacs Lisp:1]]
(use-package lispy
  :straight t
  :defer t
  :hook (emacs-lisp-mode . lispy-mode))
;; Emacs Lisp:1 ends here

;; [[file:~/.emacs.d/init.org::*Emacs%20Lisp][Emacs Lisp:2]]
(use-package aggressive-indent
  :straight t
  :defer t
  :hook (lispy-mode . aggressive-indent-mode))
;; Emacs Lisp:2 ends here

;; [[file:~/.emacs.d/init.org::*Nix][Nix:1]]
(use-package nix-mode
  :straight t
  :defer t
  :mode "\\.nix\\'")
;; Nix:1 ends here

;; [[file:~/.emacs.d/init.org::*TeX/LaTeX][TeX/LaTeX:1]]
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
;; TeX/LaTeX:1 ends here

;; [[file:~/.emacs.d/init.org::*TeX/LaTeX][TeX/LaTeX:2]]
(add-to-list 'TeX-view-program-selection
             '(output-pdf "Zathura"))

(add-to-list 'TeX-expand-list
             '("%sn" (lambda () server-name)))

(add-to-list 'TeX-view-program-list
             '("Zathura"
               ("zathura %o"
                (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
               "zathura")))
;; TeX/LaTeX:2 ends here

;; [[file:~/.emacs.d/init.org::*Markdown][Markdown:1]]
(use-package markdown-mode
  :straight t
  :defer t
  :hook (markdown-mode . visual-line-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))
;; Markdown:1 ends here

;; [[file:~/.emacs.d/init.org::*Rust][Rust:1]]
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
;; Rust:1 ends here

;; [[file:~/.emacs.d/init.org::*Rust][Rust:2]]
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
;; Rust:2 ends here

;; [[file:~/.emacs.d/init.org::*Files/Dired][Files/Dired:1]]
(use-package dired
  :defer t)

(use-config dired
  (setq dired-listing-switches "-alhv")
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t))
;; Files/Dired:1 ends here

;; [[file:~/.emacs.d/init.org::*Files/Dired][Files/Dired:2]]
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
;; Files/Dired:2 ends here

;; [[file:~/.emacs.d/init.org::*Extensions][Extensions:1]]
(use-package dired+
  :straight (dired+
             :type git :host github
             :repo "emacsmirror/emacswiki.org"
             :files ("dired+.el"))
  :after dired)
;; Extensions:1 ends here

;; [[file:~/.emacs.d/init.org::*Extensions][Extensions:2]]
(general-define-key
 :states '(normal visual insert emacs)
 :keymaps 'dired-mode-map
 "C-t C-r" 'image-dired)

(evil-set-initial-state 'image-dired-minor-mode 'emacs)
;; Extensions:2 ends here

;; [[file:~/.emacs.d/init.org::*Extensions][Extensions:3]]
(use-package diredfl
  :straight t
  :after dired
  :config
  (diredfl-global-mode))
;; Extensions:3 ends here

;; [[file:~/.emacs.d/init.org::*Extensions][Extensions:4]]
(use-package dired-filter
  :straight t
  :after dired
  :hook (dired-mode . dired-filter-mode))
;; Extensions:4 ends here

;; [[file:~/.emacs.d/init.org::*Extensions][Extensions:5]]
(general-define-key
  :keymaps 'dired-mode-map
  :states '(normal visual)
  "F" dired-filter-map
  "M" dired-filter-mark-map)

#+end_src)

Dired-ranger is an extension offers some improvements to make dired feel more like the ranger terminal file manager.
I mainly use its copy and paste functionality.

#+begin_src elisp

(use-package dired-ranger
  :straight t
  :after dired)
;; Extensions:5 ends here

;; [[file:~/.emacs.d/init.org::*Extensions][Extensions:6]]
(general-define-key
  :keymaps 'dired-mode-map
  :states '(normal visual)
  "F" dired-filter-map
  "M" dired-filter-mark-map)

#+end_src)

Dired-ranger is an extension offers some improvements to make dired feel more like the ranger terminal file manager.
I mainly use its copy and paste functionality.

#+begin_src elisp

(use-package dired-ranger
  :straight t
  :after dired)
;; Extensions:6 ends here

;; [[file:~/.emacs.d/init.org::*Extensions][Extensions:7]]
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
;; Extensions:7 ends here

;; [[file:~/.emacs.d/init.org::*Extensions][Extensions:8]]
(use-package dired-quick-sort
  :straight t
  :after dired)
;; Extensions:8 ends here

;; [[file:~/.emacs.d/init.org::*Extensions][Extensions:9]]
(general-define-key
  :keymaps 'dired-mode-map
  :states '(normal visual)
  "o" 'hydra-dired-quick-sort/body)
;; Extensions:9 ends here

;; [[file:~/.emacs.d/init.org::*Git/Magit][Git/Magit:1]]
(use-package magit
  :straight t
  :defer t)

(general-define-key
 :prefix "C-x"
 "g" 'magit-status)
;; Git/Magit:1 ends here

;; [[file:~/.emacs.d/init.org::*Git/Magit][Git/Magit:2]]
(use-package forge
  :straight (forge :host github :repo "magit/forge")
  :after magit)
;; Git/Magit:2 ends here

;; [[file:~/.emacs.d/init.org::*Music/MPD][Music/MPD:1]]
(use-package mpdel
  :straight t
  :defer t)

(use-package ivy-mpdel
  :straight t
  :after mpdel)
;; Music/MPD:1 ends here

;; [[file:~/.emacs.d/init.org::*Message][Message:1]]
(setq message-send-mail-function 'message-send-mail-with-sendmail
      send-mail-function 'sendmail-send-it)
;; Message:1 ends here

;; [[file:~/.emacs.d/init.org::*Message][Message:2]]
;; substitute sendmail with msmtp
(setq sendmail-program "msmtp")

;; allow setting account through email header
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil t)
;; Message:2 ends here

;; [[file:~/.emacs.d/init.org::*Message][Message:3]]
(add-hook 'message-send-hook
          (lambda ()
            (unless (y-or-n-p "Sure you want to send this?")
              (signal 'quit nil))))
;; Message:3 ends here

;; [[file:~/.emacs.d/init.org::*Org][Org:1]]
(use-package org
  :straight org-plus-contrib
  :requires (org-capture org-protocol))
;; Org:1 ends here

;; [[file:~/.emacs.d/init.org::*Personal][Personal:1]]
(setq org-default-notes-file "~/Projects/notes.org")
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE BUT" "DONE")
        (sequence "MAYBE" "CANCELED" "|")
;; Personal:1 ends here

;; [[file:~/.emacs.d/init.org::*Link%20types][Link types:1]]
(use-config org
  (org-add-link-type
   "project" 'projectile-switch-project-by-name))
;; Link types:1 ends here

;; [[file:~/.emacs.d/init.org::*Capture%20templates][Capture templates:1]]
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
;; Capture templates:1 ends here

;; [[file:~/.emacs.d/init.org::*Buffer][Buffer:1]]
(add-hook 'org-mode-hook 'visual-line-mode)
;; Buffer:1 ends here

;; [[file:~/.emacs.d/init.org::*Buffer][Buffer:2]]
(load-library "org-tempo")
;; Buffer:2 ends here

;; [[file:~/.emacs.d/init.org::*Buffer][Buffer:3]]
(use-package org-pretty-jump
  :straight (org-pretty-jump :type git :host github
                             :repo "LeOtaku/org-pretty-jump")
  :after org)
;; Buffer:3 ends here

;; [[file:~/.emacs.d/init.org::*Buffer][Buffer:4]]
(use-package worf
  :straight t
  :after org)
;; Buffer:4 ends here

;; [[file:~/.emacs.d/init.org::*Buffer][Buffer:5]]
(setq org-adapt-indentation nil)
;; Buffer:5 ends here

;; [[file:~/.emacs.d/init.org::*Buffer][Buffer:6]]
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
;; Buffer:6 ends here

;; [[file:~/.emacs.d/init.org::*Buffer][Buffer:7]]
;; [TODO: ensure this works]
(defface org-canceled '(:color red) "")
(defface org-maybe    '(:color magenta) "")

(setq org-todo-keyword-faces
      '(("TODO"      . 'org-todo)
        ("DOING"     . 'org-todo)
        ("DONE BUT"  . 'org-done)
        ("DONE"      . 'org-done)
        ("MAYBE"     . 'org-maybe)
        ("PLANNED"   . 'org-maybe)
        ("CANCELED"  . 'org-canceled)
        ("DROPPED"   . 'org-canceled)))
;; Buffer:7 ends here

;; [[file:~/.emacs.d/init.org::*Buffer][Buffer:8]]
(setq org-image-actual-width 400)
(setq org-startup-with-inline-images t)
;; Buffer:8 ends here

;; [[file:~/.emacs.d/init.org::*Links][Links:1]]
(setq org-link-file-path-type 'relative)
;; Links:1 ends here

;; [[file:~/.emacs.d/init.org::*Links][Links:2]]
(add-to-list 'ivy-sort-matches-functions-alist
             '(org-insert-link . my/ivy-sort-by-length)
             t)
;; Links:2 ends here

;; [[file:~/.emacs.d/init.org::*Capture][Capture:1]]
(use-config (org-capture evil)
  (add-hook 'org-capture-mode-hook (lambda () (evil-append 1))))
;; Capture:1 ends here

;; [[file:~/.emacs.d/init.org::*Export][Export:1]]
(use-config org
  (setq org-latex-pdf-process
        (list "latexmk -interaction=nonstopmode -output-directory=%o -shell-escape -bibtex -f -pdf %f")))
;; Export:1 ends here

;; [[file:~/.emacs.d/init.org::*Export][Export:2]]
(use-package ox-hugo
  :straight t)
;; Export:2 ends here

;; [[file:~/.emacs.d/init.org::*Org-ref][Org-ref:1]]
(use-package org-ref
  :straight t
  :after (org ivy-bibtex)
  :requires (doi-utils org-ref-isbn org-ref-ivy org-ref-helm)
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite))
;; Org-ref:1 ends here

;; [[file:~/.emacs.d/init.org::*Org-ref][Org-ref:2]]
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
;; Org-ref:2 ends here

;; [[file:~/.emacs.d/init.org::*Org-ref][Org-ref:3]]
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
;; Org-ref:3 ends here

;; [[file:~/.emacs.d/init.org::*Babel][Babel:1]]
(setq org-src-window-setup 'current-window)
;; Babel:1 ends here

;; [[file:~/.emacs.d/init.org::*Babel][Babel:2]]
(setq org-src-preserve-indentation nil 
      org-edit-src-content-indentation 0
      ;; [TODO make this work with custom bindings]
      org-src-tab-acts-natively t)
;; Babel:2 ends here

;; [[file:~/.emacs.d/init.org::*Babel][Babel:3]]
(advice-add 'evil-write :before-while 'my/advice-evil-write)
(defun my/advice-evil-write (&rest ignore)
  (if (org-src-edit-buffer-p)
      (progn 
        (org-edit-src-save)
        nil)
    t))
;; Babel:3 ends here

;; [[file:~/.emacs.d/init.org::*End][End:1]]
(provide 'init)
;; End:1 ends here
