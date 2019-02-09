- [Preface](#org6ab6f86)
- [Package management](#org331a958)
  - [straight.el](#org0d74e0a)
    - [org-mode hack](#orgf8c77bf)
    - [hydra](#org2793f8e)
  - [use-package](#orgd0748c2)
    - [use-config macro](#org022bbb6)
  - [no-littering](#orgb9fc89f)
- [Configuration](#org929a1b1)
  - [Helpers](#org21b21c2)
    - [bug-hunter](#org9b8ae84)
    - [general](#org4df9422)
    - [lib](#org4b3411f)
  - [Emacs](#org08b5909)
    - [Default](#orgb3b4906)
    - [Look](#org939d311)
    - [Navigation](#org5da5411)
    - [OS Integration](#org82ac931)
    - [Extensible Interfaces](#orga4008d4)
    - [Saving](#orgc148458)
  - [Editing](#org89b9ece)
    - [Prelude-esque](#org53925c0)
    - [Visual line](#org880f4af)
    - [Pairs](#org804d99a)
    - [Snippets](#org8e34657)
    - [Jumping](#org04edb98)
  - [Programming](#org9b6cb4a)
    - [Defaults](#org88ab24e)
    - [LSP/DAP family](#org31bcba5)
    - [Projectile](#org8231b55)
    - [Company](#org389b614)
    - [Outline/shine](#orgd850aee)
    - [Flycheck](#org4def239)
    - [Fly/ispell](#orgac444a9)
  - [Languages](#orge9644cb)
    - [Emacs Lisp](#orgdcccf2d)
    - [Haskell](#orgc9dde88)
    - [Nix](#org236e104)
    - [TeX/LaTeX](#org8979e40)
    - [Markdown](#org1fd3e7d)
    - [Python](#orgbcaeb1a)
    - [Rust](#orgfc01ea3)
    - [Shell](#org162689b)
  - [Misc](#orgc0e1656)
    - [Files/Dired](#org338db5d)
    - [Git/Magit](#org2d52b0b)
    - [Music/MPD](#orgca59335)
    - [Mail](#org519a6fb)
    - [Org](#org11359b7)
    - [RSS/Elfeed](#org4ce92af)
- [End](#org2caab55)



<a id="org6ab6f86"></a>

# Preface

This is my attempt at modularizing my `.emacs.d` directory and preventing eventual *Emacs Bankruptcy* by exporting my configurations through the [org-mode](http://orgmode.org/) [tangle](https://orgmode.org/manual/tangle.html) system.

I am still in the progress of figuring out the best structure for this kind document. Scattered around the file you might find a plethora of `TODO`-markings for all different kinds of stuff.

Most of this file has unfortunately been written in a really inconsistent style. This applies to its structure as well as the language used.

These are some of the things that (probably) will need to be fixed at some point:

-   Structure
    -   Linebreaks in/after sentences
    -   Code blocks
        -   Blank lines at beginning/end?
        -   Blank lines before/after?
    -   Footnotes
-   Language
    -   Inconsistent third/first person
    -   Inconsistent formality
    -   Active/Passive
        -   This does that
        -   Do that here
    -   Repetition of phrases
    -   Strange sentence structure
-   Spelling
    -   General spelling mistakes
    -   Uppercase/Downcase
        -   Lists
        -   Package names
        -   Application names

Please don't use this on your own machine. Just don't. <sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup>

This setup was heavily inspired by [wasamasa](https://github.com/wasamasa)s [dotemacs](https://github.com/wasamasa/dotemacs) repository.


<a id="org331a958"></a>

# Package management

As using Emacs without any extensions<sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup> is pretty undesirable, the ability to easily and quickly load collections of elisp from external sources. However the builtin way of achieving this (package.el + `require`) is wonky at best.

For this reason I choose to leave package.el behind and instead use the new and shiny straight.el in conjunction with use-package, which acts as an improved `require` wrapper.


<a id="org0d74e0a"></a>

## straight.el

This package is responsible for correctly setting the load path to various collections of elisp code that live in `.emacs.d/straight`. straight.el ensures absolute configuration reproducibility through its use of a lockfile located in `straight/versions/default.el`. It also allows my own packages to comfortably live inside `straight/repos` as it's various end-user functions allow for interactively managing state changes.

```elisp

(setq straight-check-for-modifications
      '(find-when-checking check-on-save))

(setq straight-recipe-repositories
      '(org-elpa melpa emacsmirror gnu-elpa))

```

This package has to be loaded first and using a manual \`bootstrap\` procedure, because it is the package responsible for loading all other packages. (Chicken and Egg)

```elisp

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

```


<a id="orgf8c77bf"></a>

### org-mode hack

This is the first of hopefully only a few \`hacks\`, configuration snippets that are particularly unidiomatic and maybe even impossible to understand without knowledge of the associated extensions, mostly because they have to account for some edge-case of said extension, I will have to use in this configuration.

Without this snippet Emacs would load the outdated version of org-mode distributed with its source code, instead of the upstream package from org-elpa. <sup><a id="fnr.3" class="footref" href="#fn.3">3</a></sup>

```elisp

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

```


<a id="org2793f8e"></a>

### hydra

The official hydra wiki provides a hydra for working with straight.el

The code uses `with-eval-after-load` instead of the superior `use-config` because it has not yet been defined at this point.

```elisp

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

```


<a id="orgd0748c2"></a>

## use-package

This package provides a wrapper macro around Emacs' builtin way of =require=ing packages. It allows for easily specifying the correct order elisp code should be run at, without heavily jeopardizing the actual structure of said code.

Straight.el also provides integration with use-package, allowing for the use of `:straight t`<sup><a id="fnr.4" class="footref" href="#fn.4">4</a></sup> instead of the builtin `:ensure t`. Use-package itself of course gets loaded using only the `straight-use-package` function provided by straight.el

```elisp
(straight-use-package 'use-package)
```


<a id="org022bbb6"></a>

### use-config macro

I like split the configuration block of a package from its use-package block for visual clarity. Especially with org-tangle, where I can specify the `use-config` macro to wrap a code block, this approach makes the configuration much more readable.

[TODO: add use-package error handling]

```elisp

(defmacro use-config (after &rest body)
  "use-package like wrapper for configurations"
  (macroexp-progn
   (use-package-require-after-load after body)))

(put 'use-config 'lisp-indent-function 'defun)

```


<a id="orgb9fc89f"></a>

## no-littering

No-littering is a useful Emacs extension that tries to revert any inconsistent paths used by popular or builtin packages and instead sets them to semantic entries in either `.emacs.d/var` or `.emacs.d/etc`.

```elisp

(use-package no-littering
  :straight t)

(use-config (no-littering recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

```


<a id="org929a1b1"></a>

# Configuration

The heart and soul of this file and by extension of my Emacs configuration. Headers are grouped by their most significant functionality. If no logical order is present, alphabetical order should be upheld. [TODO] <sup><a id="fnr.5" class="footref" href="#fn.5">5</a></sup>


<a id="org21b21c2"></a>

## Helpers

This header contains configurations or packages with the main purpose of easing the configuration of other packages. For this reason they have to be loaded very early during the initialization. <sup><a id="fnr.6" class="footref" href="#fn.6">6</a></sup>


<a id="org9b8ae84"></a>

### bug-hunter

Putting this package here might seem like a bit of a stretch, but I think being able to easily debug generic problems is important for any Emacs configuration.

```elisp
(use-package bug-hunter
  :straight t
  :defer t)
```


<a id="org4df9422"></a>

### general

General allows for easy mapping of keys to commands by adding an abstraction over the builtin `define-key` many well known key-centric extensions such as evil.

```elisp

(use-package general
  :straight t
  :defer t)

```

1.  general key-fn

    `key-fn` is an elisp macro written by myself in order to ease the mapping of non-`interactive` functions to keys with general. It also extends generals which-key integration.
    
    ```elisp
    
    (defmacro key-fn (action &rest args)
      (let ((fun-name (intern (format "my/general-which-key-functions-%s-%s" (symbol-name action) args))))
        `(progn
           (defun ,fun-name ()
             (interactive)
             (apply ',action ',args))
           (quote (,fun-name
                   :wk ,(concat (symbol-name action)
                                "-"
                                (mapconcat 'identity args "-")))))))
    
    ```
    
    Here are some usage examples:
    
    ```elisp
    
    (general-define-key
     "C-y" (key-fn insert "yes"))
    
    ```


<a id="org4b3411f"></a>

### lib

1.  subr

    Subr is an emacs-lisp library that provides common reusable subroutines. It is distributed with Emacs. I use it in some of my functions and configurations.
    
    ```elisp
    (use-package subr)
    ```

2.  dash

    Dash is a modern list library for emacs-lisp which can replace the outdated cl-lib libraries. I use it in some of my functions and configurations.
    
    ```elisp
    (use-package dash
      :straight t
      :config
      (dash-enable-font-lock))
    ```

3.  f

    F is a modern emacs-lisp library for working with files and filesystem paths. I use it in some of my functions and configurations.
    
    ```elisp
    (use-package f
      :straight t)
    ```

4.  s

    S is a modern emacs-lisp library for string manipulation. I use it in some of my functions and configurations.
    
    ```elisp
    (use-package s
      :straight t)
    ```


<a id="org08b5909"></a>

## Emacs

This is the subtree for configurations that affect all of Emacs.


<a id="orgb3b4906"></a>

### Default

This subtree contains redefinitions for built-in Emacs settings I dislike/want changed.

Increase garbage collector threshold. Also manually set maximum recursion and definiton values.

```elisp
(setq gc-cons-threshold 50000000)
(setq max-specpdl-size 1200)
(setq max-lisp-eval-depth 800)
```

Only show y-or-n prompts, even for "important" stuff.

```elisp
(fset 'yes-or-no-p 'y-or-n-p)
```

Show all messages

```elisp
(setq inhibit-message nil)
```

Disallow recursive minibuffers, as they can sometimes cause weird glitches that require Emacs to be restarted.

```
(setq enable-recursive-minibuffers nil)
```

Disable Emacs from following symlinks as it can lead to unexpected behavior.

```elisp
(setq find-file-visit-truename nil)
```


<a id="org939d311"></a>

### Look

By default Emacs is not much to look at, but by setting some builtin options and loading a nice theme this can easily be changed.

```elisp

(scroll-bar-mode 0)
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)

```

1.  Themes

    There are many nice themes for Emacs. I personally happen to enjoy the doom, sanityinc and poet families of themes.
    
    ```elisp
    
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
    
    ```
    
    I have also created my own theme
    
    ```elisp
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
    ```
    
    By default Emacs seems to assume that all themes are in fact malware. Disable this behavior.
    
    ```elisp
    (setq custom-safe-themes t)
    ```
    
    Sometimes I want to just remove all active themes for debugging purposes. Add a function to easily achieve this.
    
    ```elisp
    (defun disable-all-themes ()
      "disable all active themes."
      (dolist (i custom-enabled-themes)
        (disable-theme i)))
    ```
    
    Because I switch between different themes relatively often, I would like Emacs to always remember and load my currently active theme. This is achieved here using desktop-mode. [TODO: test this]
    
    ```elisp
    (add-to-list 'desktop-globals-to-save 'custom-enabled-themes)
    (add-hook 'desktop-after-read-hook (lambda ()
                                         (mapc 'load-theme
                                               custom-enabled-themes)))
    ```

2.  Modeline

    The Emacs modeline is a place for buffers to display arbitrary information grouped in blocks. The content it shows is defined in the `mode-line-format` variable.
    
    There are different packages that allow one to easily create nice modeline setups. I currently am swithing between doom-modeline and moody.
    
    ```elisp
    
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
    
    ```

3.  Dashboard

    Dashboard is an Emacs package ripped from the spacemacs distribution. It, like the name suggests, provides a dashboard that can display all different types of content.
    
    Some keys for easier dashboard navigation are bound.
    
    ```elisp
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
    
    ```
    
    I like to see my dashboard when I open a new frame.
    
    ```elisp
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
    ```

4.  Scrollbar

    The yascroll package provides a nice vertical scrollbar that also works in the terminal. Unfortunately it is no longer maintained and somewhat buggy. Disable it for now.
    
    ```elisp
    
    ;; (use-package yascroll
    ;;   :straight t
    ;;   :defer t
    ;;   :config
    ;;   (require 'cl)
    ;;   (setq yascroll:delay-to-hide 0.5))
    
    ;; (global-yascroll-bar-mode 1)
    
    ```

5.  Page breaks

    Many Emacs buffers use the `^L` character to indicate a page break. This package automatically replaces them with a pretty horizontal line.
    
    ```elisp
    (use-package page-break-lines
      :straight t
      :config
      (global-page-break-lines-mode))
    ```


<a id="org5da5411"></a>

### Navigation

Switching from mostly using terminal utilities to Emacs, the challenge I have faced is having to navigate the large amount of buffers that accumulate during a typical Emacs session. I still consider what I have achieved so far suboptimal and will have to thoroughly and incrementally improve this part of my configuration until I get something that is usable.

Important concepts include window management, buffer management, finding of files <sup><a id="fnr.7" class="footref" href="#fn.7">7</a></sup> as well as automatic grouping and skipping of buffers.


<a id="org82ac931"></a>

### OS Integration

While living in Emacs is *of course* the ultimate goal, I would also like to enjoy some of the benefits the last +40 years have brought us. Being able to actually browse modern webpages is OFC also a plus.

```elisp
(setq browse-url-generic-program "firefox")
```

```elisp

(use-package openwith
  :straight t
  :config
  (openwith-mode t)
  (setq openwith-associations nil))

(use-config openwith
   (setq openwith-associations '(("\\.pdf\\'" "zathura" (file)))))

```


<a id="orga4008d4"></a>

### Extensible Interfaces

Because Emacs is so very extensible, it is also possible to add entirely new interfaces to its core functionality. Many of these "interface extensions" are some of the most widely used Emacs packages. Packages that fall into this category might include the builtin IDO, helm and ivy. They are often referred to as "narrowing completion frameworks". Other than that there is also hydra, a package for defining colorful keyboard-centric pop-up menus and evil, the extensible vi layer. <sup><a id="fnr.8" class="footref" href="#fn.8">8</a></sup>

1.  ivy

    Ivy is the best available narrowing completion framework. It, unlike helm, makes use of the minibuffer as its main interface. The ivy repo also includes swiper, an isearch replacement with an overview and the counsel utilities, which are wrappers arround existing commands with added ivy completion. Moreover by enabling `ivy-mode` you may allow ivy to automatically hijack well known completion functions.
    
    Keys for binding different types of dispatch MAYBE bound in the future.
    
    ```elisp
    
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
    
    ```
    
    ```elisp
    
    (use-config ivy
      (setq ivy-use-selectable-prompt t))
    
    ;; (general-define-key
    ;; :keymaps 'ivy-minibuffer-map
    ;; :states 'insert
    ;; "<RET>" 'ivy-done
    ;; "<tab>" 'ivy-partial-or-done)
    
    ```
    
    ```elisp
    
    ;; swiper
    (general-define-key
     :keymaps 'override
     :states '(normal visual)
     "/" 'swiper)
    
    ```
    
    A hydra for easier navigation is also provided. Keys for accessing the hydra in ivy-minibuffers are bound.
    
    ```elisp
    
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
    
    
    ```
    
    Add a sort-by-length ivy sort functions that can later be used by different completion functions.
    
    ```elisp
    (defun my/ivy-sort-by-length (_name candidates)
      (-sort (lambda (f1 f2)
               (< (length f1) (length f2)))
             (copy-sequence candidates)))
    ```

2.  helm

    Helm is the most well known narrowing completion framework for Emacs. It also has the largest amount of completion sources. Unfortunately it is no longer fully maintained, slower than ivy and somewhat intrusive.
    
    I personally load this package for some of it's additional sources, but do NOT enable `helm-mode`.
    
    ```elisp
    
    (use-package helm
      :straight t
      :defer t)
    
    (use-config helm
      ;; allow display settings to be overriden
      (setq helm-display-function 'pop-to-buffer))
    
    ```
    
    Just like with ivy an, albeit inferior, hydra is provided. Keys for accessing it in the helm buffer are bound.
    
    ```elisp
    
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
    
    ```

3.  hydra

    Hydra is a package that allows the end user or library authors to define keyboard-centric menus of arbitrary complexity. They may also define a doc-string which is then displayed as a formatted, visual presentation of the menu.
    
    ```elisp
    
    (use-package hydra
      :straight t)
    
    ```

4.  evil

    Evil is the extensible vi(m) layer for Emacs. It provides WORD vim emulation for Emacs, including motions, text objects visual, visual block and line selection, the vim command line, improved isearch [TODO fix ugly artefacts], interactive search and replace as well as ports of many popular vim extensions. Evil is activated through the global-only `evil-mode` and changes a huge amount of mappings. It also introduces the concept of states and intercept keymaps [TODO: understand this and then fix issues] which are fortunately handled relatively well by general.
    
    ```elisp
    
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
    
    ```
    
    Evil-collection aims to bring the advantages of the vim modal editing model to many popular Emacs modes. It can sometimes be problematic, as it may override user-defined bindings.
    
    ```elisp
    
    (use-package evil-collection
      :straight t
      :init
      (setq evil-collection-setup-minibuffer t))
    
    ```
    
    Integration may be activated for a specific mode in the following fashion:
    
    ```elisp
    
    (require 'evil-collection-MODE)
    (evil-collection-MODE-setup)
    
    ```
    
    Where MODE is the mode to which you want to add the evil integration.
    
    As previously mentioned there exists a huge number of additional packages improving on evil-mode. I personally load quite a few of them myself.
    
    Evil surround and commentary emulate the popular vim plugins of the same name. They automatically bind their keys through `evil-surround-mode` and `evil-commentary-mode`.
    
    ```elisp
    
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
    
    ```
    
    [TODO: Evil multiedit or mc] [TODO: Evil terminal cursor]

5.  hippie-expand

    Hippie-expand provides expansion based on multiple different simultaneous sources. I don't use it often, but it can sometimes be nice to have.
    
    The default `hippie-expand` gets bound to `<C-y>` in insert and normal mode.
    
    ```elisp
    
    (use-package hippie-expand
      :defer t)
    
    (general-define-key
     :states '(normal insert)
     "C-y" 'hippie-expand)
    
    ```


<a id="orgc148458"></a>

### Saving


<a id="org89b9ece"></a>

## Editing


<a id="org53925c0"></a>

### Prelude-esque

The [Emacs Prelude](https://github.com/bbatsov/prelude#helm) project, among other things, provides a set of helpful and generic keybindings for editing text. I have extracted some of them for my own configuration.

There are bindings for operating on numbers, bound to the `C-c .` prefix.

```elisp

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

```


<a id="org880f4af"></a>

### Visual line

When I have enabled `visual-line-mode`, I want visual lines to be treated nearly the same as hard lines would be without it. This means that some evil keys have to be rebound. Note that motions that operate on lines such as for example "dd" still operate on hard lines.

```elisp

(defun my/evil-visual-line-I ()
  (interactive)
  (evil-first-non-blank-of-visual-line)
  (evil-insert 1))

(defun my/evil-visual-line-A ()
  (interactive)
  (evil-end-of-visual-line)
  (evil-insert 1))

```

```elisp

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

```

Visual-fill-column provides a `fill-column`-like experience for buffers using `visual-line-mode`.

```elisp
(use-package visual-fill-column
  :straight t
  :hook (visual-line-mode . visual-fill-column-mode))
```


<a id="org804d99a"></a>

### Pairs

While there are programming language specific packages that handle automatic pairing (eg. lispy) I would like to have this functionality for all programming languages as well as prose. Smartparens has automatic detection for most programming and markup languages, as well as sensible defaults for unknown modes.

```elisp

(use-package smartparens
  :straight t
  :requires smartparens-config
  :config
  (smartparens-global-mode))

```

Using lispy has also made me dependent the ability to slurp and barf sexps. Bind keys to access that functionality in non-lisp buffers.

```elisp

(general-define-key
 :states 'normal
 ">" 'sp-forward-slurp-sexp
 "<" 'sp-forward-barf-sexp)

```

For c-style languages, I would like to be able to automatically have braces indented for me. [TODO: Language handling]

```elisp

(defun my/indent-between-braces (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair 'prog-mode "{" nil
               :post-handlers '((my/indent-between-braces "RET")))

```

Automatically highlight the pair opposite to the one at point.

```elisp

(use-package paren
  :config
  (show-paren-mode))

```


<a id="org8e34657"></a>

### Snippets


<a id="org04edb98"></a>

### Jumping


<a id="org9b6cb4a"></a>

## Programming


<a id="org88ab24e"></a>

### Defaults

This subtree contains sensible defaults for coding/writing. Different major modes may override these.

Tabs are 4 spaces.

```elisp
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
```

Never truncate lines. (May be overriden by `visual-line-mode`) Custom-mode relies on truncating lines.

```elisp
(setq-default truncate-lines t)
(add-hook 'custom-mode-hook (lambda () (setq-local truncate-lines nil)))
```


<a id="org31bcba5"></a>

### LSP/DAP family

Lsp-mode is a emacs package that handles talking to language servers supporting Microsofts open-source language server protocol. It then provides several interfaces to the intelligent code information provided by these servers. Some of the more generic interfaces are provided by the core lsp-mode package, while others reside in external packages such as lsp-ui and company-lsp. Some languages may need additional packages for LSP support. These are managed in their respective subtrees.

```elisp

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

```

DAP is another protocol designed by Microsoft for its Visual Code editor. It functions similarly to the LSP, but is instead designed for debuggers. Dap-mode for Emacs is also maintained by the emacs-lsp organization. The project is currently in an early stage of development.

```elisp

(use-package dap-mode
  :straight t
  :after lsp-mode)

```

LLDB support currently requires manual intervention. [TODO: LLDB for DAP]


<a id="org8231b55"></a>

### Projectile

Projectile is the de-facto standard project management library for emacs. It has support for detecting most VC and build systems by default but can be customized further to fit specific workflows.

While projectile has a great user-facing API, some packages in the Emacs ecosystem also call into the library for context on the currently edited projects. (eg. LSP)

Projectile heavily relies on completion for its user-facing API and has builtin support for all NCFs through completion-in-region. Moreover there also exist some packages with additional user commands, optimized for the use with specific NCFs. I prefer ivy and counsel-projectile.

```elisp

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

```

As previously mentioned, projectile has great support for automatically finding projects based on VC and/or build-root files. It also handles nested repositories. However I mostly find these features to be annoying for actual day-to-day use. Therefore I configure projectile to

1.  only detect some common VC systems
2.  only index the first project and not care about nesting

```elisp

(use-config projectile
  (setq projectile-project-root-files-functions '(projectile-root-top-down))
  (setq projectile-project-root-files
        '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile"))) 

```


<a id="org389b614"></a>

### Company

Company is the most popular replacement for Emacs' built in auto-complete library and the de-facto default solution for autocompletion in Emacs. Its main selling points include the ability to perform asynchronous completion and its separation into completion front- and backends. Some company sources also provide parameter expansion using the yasnippet minor mode. Other packages may define arbitrary completion backends for company. These are managed in their respective subtrees.

Company is automatically enabled in all programming buffers. Yasnippet is needed for parameter expansion.

```elisp

(use-package company
  :straight t
  :defer t
  :hook ((prog-mode . company-mode)
         (company-mode . yas-minor-mode)))

```

Misc company setting, mostly related to frontend display. [TODO: set best frontends]

```elisp

(use-config company
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  (setq company-dabbrev-downcase nil)
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-tng-frontend
                            company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)))

```


<a id="orgd850aee"></a>

### Outline/shine

Outline is a library for structuring files using nested headings that can be folded. It is most famously used by the org-mode extension.

Outshine is a package that promises to bring the advantages of structuring your files with outline to all programming languages.

I personally only use it for Elisp and would like to remove it as soon as possible, because it is very unstable and requires intrusive hacks in order to work properly.

```elisp
(use-package outshine
  :straight t
  :after org
  :add-hook (emacs-lisp-mode . outshine-mode))
```

```elisp

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

```


<a id="org4def239"></a>

### Flycheck


<a id="orgac444a9"></a>

### Fly/ispell

Ispell is the most common spell checking utility on unix-like systems. Emacs ispell offers integration with the program by calling it on some, usually a buffers, text and then walking through all reported errors.

```elisp
(use-package ispell
  :defer t)
```

Flyspell offers more traditional "red underline" spell-checking using Emacs ispell as its backend.

```elisp
(use-package flyspell
  :straight t
  :defer t)
```

The command used for spell checking can be changed. I personally use hunspell, which has better support for some languages and is also used by the LibreOffice project.

```elisp
(setq ispell-program-name "hunspell")
```

1.  Dictionaries

    The language to spell-check for will usually be set as a file-local variable. If not, default to "en<sub>US</sub>".
    
    ```elisp
    (setq ispell-dictionary "en_US")
    (setq flyspell-default-dictionary "en_US")
    ```
    
    Using ispell with hunspell requires some additional setup to correctly handle dictionaries.
    
    ```elisp
    
    (setq ispell-dictionary-alist
          `(("en_US")
            ("de_AT")
            ("en_US,de_AT")))
    
    (dolist (dict ispell-dictionary-alist)
      (ispell-hunspell-fill-dictionary-entry (car dict)))
    
    ```


<a id="orge9644cb"></a>

## Languages


<a id="orgdcccf2d"></a>

### Emacs Lisp

Emacs Lisp is a Lisp dialect used mainly as a scripting and configuration language for GNU Emacs and most other Emacs variants.

As Emacs Lisp is the only Lisp dialect I regularly use, this subtree also contains settings that affect general Lisp editing.

Lispy is a package that provides an improved Lisp editing experience. It is a spiritual sucessor to the popular paredit.

```elisp
(use-package lispy
  :straight t
  :defer t
  :hook (emacs-lisp-mode . lispy-mode))
```

Aggressive-indent automatically updates indentation after every keystroke, for instant visual feedback. This is especially useful for Lisp, where most code structure is determined by indentation.

```elisp
(use-package aggressive-indent
  :straight t
  :defer t
  :hook (lispy-mode . aggressive-indent-mode))
```


<a id="orgc9dde88"></a>

### Haskell

Haskell is a statically typed, *"purely functional"* general-purpose programming language.


<a id="org236e104"></a>

### Nix

Nix is a functional-style programming language that can be used for package management through the Nix package manager as well as for system configuration through the NixOS project.

NixOS is my preferred GNU/Linux distribution for desktop and server use. Most of my systems currently run it.

The Nix project also provides an Emacs major mode for working with the Nix language.

```elisp
(use-package nix-mode
  :straight t
  :defer t
  :mode "\\.nix\\'")
```


<a id="org8979e40"></a>

### TeX/LaTeX

TeX is a typesetting system developed mainly by Donald Knuth. LaTeX is the most popular macro collection for TeX. It can be customized further through additional packages.

AucTeX is a popular Emacs extension that majorly improves the editing of TeX files and also provides various other TeX related utilities.

```elisp

(use-package tex
  :straight auctex
  :defer t
  :hook
  (TeX-mode . visual-line-mode)
  (LaTeX-mode . visual-line-mode)
  :config
  (TeX-source-correlate-mode)
  (TeX-PDF-mode))

```

Set Zathura as the default pdf viewer for tex. This also allows using source-correlate through SyncTeX.

```elisp
(add-to-list 'TeX-view-program-selection
             '(output-pdf "Zathura"))

(add-to-list 'TeX-expand-list
             '("%sn" (lambda () server-name)))

(add-to-list 'TeX-view-program-list
             '("Zathura"
               ("zathura %o"
                (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
               "zathura")))
```


<a id="org1fd3e7d"></a>

### Markdown

Markdown is a generally well supported loose standard for rich text formatting.

Markdown-mode provides a mode for editing generic Markdown files as well as another for the customized variant supported by Github.

```elisp
(use-package markdown-mode
  :straight t
  :defer t
  :hook (markdown-mode . visual-line-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))
```


<a id="orgbcaeb1a"></a>

### Python

Python is an easy to understand, duck-typed general purpose scripting and programming language.

I just use it for basic scripting, so the builtin python-mode is sufficient for now.


<a id="orgfc01ea3"></a>

### Rust

Rust is a modern statically typed systems programming language with support for many high-level concepts currently backed by Mozilla.

The Rust major mode adds basic highlighting and indentation support for the Rust language, flycheck-rust offers Flycheck integration for `cargo check` rls and clippy.

```elisp

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

```

The Rust Language Server as well as Flycheck need some specific environment variables to be set in order to work correctly. Set them after the first call to `rust-mode-hook`.

```elisp

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

```


<a id="org162689b"></a>

### Shell


<a id="orgc0e1656"></a>

## Misc


<a id="org338db5d"></a>

### Files/Dired


<a id="org2d52b0b"></a>

### Git/Magit

Git is the most common VC system today. Most open-source project use it, partly because of its great hosting support with Github and Gitlab providing near-unlimited public storage.

Magit is an intuitive and comprehensive graphical interface to the git CLI. It is widely considered the best currently existing git client.

```elisp
(use-package magit
  :straight t
  :defer t)
```

More recently Magit also gained support for working with git forges through the additional "forge" package. In order to correctly install and use magit-forge, you will need to have a C compiler (eg. gcc) when first running any forge command. [TODO: this is suboptimal]

```elisp
(use-package forge
  :straight (forge :host github :repo "magit/forge")
  :after magit)
```


<a id="orgca59335"></a>

### Music/MPD

MPD is an daemon for playing music. Many frontends for the MPD server exist, I use ncmpcpp for terminal and mpdel for Emacs. There's also ivy-mpdel which allows for using the ivy NCS to select songs.

```elisp

(use-package mpdel
  :straight t
  :defer t)

(use-package ivy-mpdel
  :straight t
  :after mpdel)

```


<a id="org519a6fb"></a>

### Mail

1.  Message

2.  Notmuch


<a id="org11359b7"></a>

### Org

Org is a system for keeping easily navigable plain text notes, lists, and structured documents. It also features literate programming support through org-babel, which this document uses for exporting, a customizable agenda, the ability to easily capture arbitrary text without interrupting your workflow and a large amount of export backends, ranging from LaTeX documents to complex static webpages. There exist a huge amount of extensions to the Org system which can be loaded as normal Emacs packages from repositories such as MELPA or ELPA. I have also written one of my own, [org-pretty-jump](straight/repos/org-pretty-jump/).

In no particular order, these are some of the things I use:

-   Easy buffer navigation and editing
    -   org-tempo (builtin but not loaded by default)
    -   worf
    -   org-pretty-jump goto
    -   custom keybindings
-   Authoring notes
    -   TODO keywords and states
    -   org-capture
        -   org-protocol
        -   org-pretty-jump capture
    -   org-agenda
    -   refile
        -   org-pretty-jump refile
-   Writing papers/articles
    -   org-ref
    -   export to LaTeX
        -   Bib(La)TeX support
-   Writing documentation
    -   Github support (eg. README.org)
    -   export to static page
-   Literate programming
    -   org-babel
        -   tangle
        -   in-buffer evaluation

Officially load the org-plus-contrib package and also require some optional packages.

```elisp
(use-package org
  :straight org-plus-contrib
  :requires (org-capture org-protocol))
```

1.  Personal

    These are the personal settings I need for managing my notes on my system. Even if you want to copy some of my settings, you will almost definitely want to change these.
    
    ```elisp
    (setq org-default-notes-file "~/Projects/notes.org")
    (setq org-todo-keywords
          '((sequence "TODO" "DOING" "|" "DONE BUT" "DONE")
            (sequence "MAYBE" "CANCELED" "|")
    ```
    
    1.  Link types
    
        These are my custom org-link-types.
        
        ```elisp
        (use-config org
          (org-add-link-type
           "project" 'projectile-switch-project-by-name))
        ```
    
    2.  Capture templates
    
        These are my custom org-capture templates. Probably only "g - Generic" stands out. It uses org-pretty-jump to capture to an arbitrary subtree in my `org-default-notes-file`. "L - Protocol Link" and "p - Protocol Selection" are used by org-protocol and the org-capture firefox extension.
        
        ```elisp
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
        ```

2.  Buffer

    In order to use org-mode effectively it is important to be able to comfortably manage your .org files and buffers. I personally enjoy easy to access DWIM keys and menus over having to memorize a large amount of keybindings. Some of these I have created myself, some are provided by Org.
    
    I enjoy writing prose in visual line mode. Add the required hook here.
    
    ```elisp
    (add-hook 'org-mode-hook 'visual-line-mode)
    ```
    
    As of Org 9.2 "org-tempo" is no longer loaded by default. It allows for easy insertion of code blocks through contextual binding like `<s|<TAB>`. Load it here.
    
    ```elisp
    (load-library "org-tempo")
    ```
    
    Navigating huge Org buffers can be difficult. org-pretty-jump tries to make that task easier by providing a good-looking ivy-based menu that can be used to jump to any headline. It also provides some additional functions for common Org task such as refiling as well as an easy to understand API for creating custom actions using the central menu.
    
    ```elisp
    (use-package org-pretty-jump
      :straight (org-pretty-jump :type git :host github
                                 :repo "LeOtaku/org-pretty-jump")
      :after org)
    ```
    
    Worf also makes working with Org buffers easier by adding additional stateful editing which is automatically activated when the point sits somewhere interesting.
    
    ```elisp
    (use-package worf
      :straight t
      :after org)
    ```
    
    Do not adapt indentation by inserting whitespace. This is better handled by `org-indent-mode`.
    
    ```elisp
    (setq org-adapt-indentation nil)
    ```
    
    Org mode already has great highlighting for most important syntax, some common constructs seem to be missing. This includes both `@@key: text@@` style inline markup, as well as list items. Additionally I personally enjoy using the `->` symbol in my notes to signal causation, so I would like to see that highlighted. Add the required entries here.
    
    ```elisp
    
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
    
    ```
    
    Moreover, I would like highlighting for some additional TODO states.
    
    ```elisp
    
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
    
    ```
    
    Lastly, lets display images by default, but make sure they get scaled sensibly.
    
    ```elisp
    (setq org-image-actual-width 400)
    (setq org-startup-with-inline-images t)
    ```

3.  Links

    Custom link types live in `Personal/Link Types`. These are other link-related settings not specific to my personal setup.
    
    Always link files relatively if possible.
    
    ```elisp
    (setq org-link-file-path-type 'relative)
    ```
    
    Always show the shortest candidate first when completing link-types.
    
    ```elisp
    (add-to-list 'ivy-sort-matches-functions-alist
                 '(org-insert-link . my/ivy-sort-by-length)
                 t)
    ```

4.  Capture

    Capture templates live in `Personal/Capture Templates`. These are other capture-related settings not specific to my personal setup.
    
    ```elisp
    (use-config (org-capture evil)
      (add-hook 'org-capture-mode-hook (lambda () (evil-append 1))))
    ```

5.  Export

    The org ecosystem offers many different export backends for a wide range of usecases.
    
    I mainly use the following exporters:
    
    -   **LaTeX:** writing documents
    -   **Hugo:** webpages using the gohugo static site generator
    -   **Plaintext:** email
    
    These can be accessed using the default `C-c C-e` binding provided by org mode.
    
    Set the LaTeX backend to export to pdf using the `latexmk` utility.
    
    ```elisp
    (use-config org
      (setq org-latex-pdf-process
            (list "latexmk -interaction=nonstopmode -output-directory=%o -shell-escape -bibtex -f -pdf %f")))
    ```
    
    Load and configure the external backends.
    
    ```elisp
    
    (use-package ox-hugo
      :straight t)
    
    ```

6.  Org-ref

    Org ref is a package that makes it easy to work with Bib(La)TeX bibliographies in .org buffers and files. It, among other things, provides easy access to ref/cite/label links using either helm or ivy, in-buffer error reporting and automatic link resolution for exports. It may also be used in conjunction with the ivy/helm-bibtex packages.
    
    ```elisp
    
    (use-package org-ref
      :straight t
      :after (org ivy-bibtex)
      :requires (doi-utils org-ref-isbn org-ref-ivy org-ref-helm)
      :init
      (setq org-ref-completion-library 'org-ref-ivy-cite))
    
    ```
    
    The uppercase versions of BibTeX links defined by org-ref clutter the insert link menu while not having any apparent use. Remove them for now. [TODO: test this]
    
    ```elisp
    
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
    
    ```
    
    I don't like the way ivy and helm org-ref-cite display possible citations. Moreover the system for choosing the correct NCS implementation for org-ref is a broken mess. Thus I manually set the desired values for the completion functions.
    
    `my/org-ref-ivy-insert-cite-link` works like `org-ref-ivy-insert-cite-link`, but with the prettier formatting of `ivy-bibtex`.
    
    ```elisp
    
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
    
    ```

7.  Babel

    Org-babel is a system for literate programming with support for many popular languages. I use it to literally configure my Emacs editor.
    
    Don't uselessly split org-src windows.
    
    ```elisp
    (setq org-src-window-setup 'current-window)
    ```
    
    Make source code indentation work as expected.
    
    ```elisp
    (setq org-src-preserve-indentation nil 
          org-edit-src-content-indentation 0
          ;; [TODO make this work with custom bindings]
          org-src-tab-acts-natively t)
    ```
    
    Narrowed buffers support automatic saving to their parent buffers. Org-src tries to emulate this functionality but does not support the evil `:w` command. Manually add support for it here.
    
    ```elisp
    (advice-add 'evil-write :before-while 'my/advice-evil-write)
    (defun my/advice-evil-write (&rest ignore)
      (if (org-src-edit-buffer-p)
          (progn 
            (org-edit-src-save)
            nil)
        t))
    ```


<a id="org4ce92af"></a>

### RSS/Elfeed


<a id="org2caab55"></a>

# End

I'm pretty sure putting this here actually makes any difference for my setup but it seem to be tradition to have it.

```elisp
(provide 'init)
```

## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> Why? <https://www.anishathalye.com/2014/08/03/managing-your-dotfiles/>

<sup><a id="fn.2" class="footnum" href="#fnr.2">2</a></sup> extensions, packages, collections of elisp code, &#x2026;

<sup><a id="fn.3" class="footnum" href="#fnr.3">3</a></sup> some more context can be found [here](https://github.com/raxod502/straight.el#installing-org-with-straightel)

<sup><a id="fn.4" class="footnum" href="#fnr.4">4</a></sup> 't' in this case may also be replaced with a symbolic package name or any valid straight.el recipie

<sup><a id="fn.5" class="footnum" href="#fnr.5">5</a></sup> because of VC tracking, but this should not limit refactoring

<sup><a id="fn.6" class="footnum" href="#fnr.6">6</a></sup> this could be changed by always using use-package

<sup><a id="fn.7" class="footnum" href="#fnr.7">7</a></sup> while dired is technically used to find files, it is flexible enough to warrant being but under `Misc`

<sup><a id="fn.8" class="footnum" href="#fnr.8">8</a></sup> evil might sound like a prime candidate for the `Editing` section, but because it also provides utilities like a command prompt and global keybinds, I choose to put it unter `Integration`
