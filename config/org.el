;; all of my org mode specific configurations

(setq org-default-notes-file "~/Projects/notes.org")
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE BUT" "DONE")
        (sequence "MAYBE" "CANCELED" "|")
        (sequence "WATCHING" "READING" "|" "COMPLETED")
        (sequence "PLANNED" "DROPPED" "|")))

;;; packages

(use-package org
  :straight org-plus-contrib
  ;; :demand t
  ;; :after flyspell
  :config
  (require 'org-capture)
  (require 'org-protocol)
  (load-library "org-tempo")
  :hook (org-mode . visual-line-mode))

;; (use-package worf
;;   :straight t
;;   :defer t
;;   :after (org))

;; (use-package ivy-bibtex
;;   :straight t
;;   :defer t
;;   :after (org))

(use-package org-ref
  :straight t
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  ;; (require 'doi-utils)
  ;; (require 'org-ref-isbn)
  ;; (require 'org-ref-ivy)
  ;; (require 'org-ref-helm)
  )

;;; settings
;;;; defaults

(with-eval-after-load 'org
  (setq org-src-window-setup 'current-window)

  (setq org-latex-pdf-process (list "latexmk -interaction=nonstopmode -output-directory=%o -shell-escape -bibtex -f -pdf %f"))

  (setq org-image-actual-width 400)
  (setq org-startup-with-inline-images t)

  (setq org-checkbox-hierarchical-statistics nil)

  (setq org-link-file-path-type 'relative)

  (setq org-adapt-indentation nil)
  
  (setq org-src-preserve-indentation nil 
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t))

;; (add-hook 'org-mode-hook 'org-indent-mode)

;;;; capture

(setq org-capture-templates
      '(("g" "Generic" entry
         (file+function "" (lambda () (goto-char (worf-get-this-buffer t))))
         "* %?%i")
        ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n")
        ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
         "* %?")
        ("m" "Media related captures")
        ("mn" "Insert new Media entry" entry
         (file+function "" (lambda () (goto-char (worf-get-scoped-this-buffer nil '("Media") nil 2 2))))
         "* %?")
        ("me" "Edit existing Media entry" plain
         (file+function "" (lambda ()
                             (goto-char (worf-get-scoped-this-buffer t '("Media") nil 3 3))))
         "%?")
        ("L" "Protocol Link" entry (file+headline org-default-notes-file "Weblinks")
         "* %:description%?\n[[%:link]]\n%t"
         :immediate-finish t)
        ("p" "Protocol Selection" entry (file+headline org-default-notes-file "Weblinks")
         "* %:description\n[[%:link]]\n%t\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?")))

(with-eval-after-load 'org-capture
  (add-hook 'org-capture-mode-hook (lambda () (evil-append 1))))

;;;; org-ref

(with-eval-after-load 'org-ref
  (setq orhc-multiline t)
  ;; (org-ref-helm-cite-completion)
  ;; (org-ref-ivy-cite-completion)
  ;; (defun orhc-bibtex-candidates ()
  ;;   (setq bibtex-completion-bibliography org-ref-bibliography-files)
  ;;   (bibtex-completion-candidates))
  ;; (setq bibtex-dialect 'biblatex)
  )

;;;; links

(with-eval-after-load 'org
  (org-add-link-type
   "project" 'projectile-switch-project-by-name))

;;;; highlighting

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([0-9]+\\.\\)" (1 'bold))
                          ("^ *\\([0-9]+)\\)" (1 'bold))
                          ("^ *\\([+-]\\)" (1 'bold))
                          ("^ +\\([*]\\)" (1 'bold))))

(font-lock-add-keywords 'org-mode
                        '(("\\(->\\)" (1 'bold))))

(font-lock-add-keywords 'org-mode
                        '(("\\(@\\)" (1 'org-meta-line))))

(defface org-canceled '() "")
(defface org-maybe    '() "")

(setq org-todo-keyword-faces
      '(("TODO"      . 'org-todo)
        ("DOING"     . 'org-todo)
        ("DONE BUT"  . 'org-done)
        ("DONE"      . 'org-done)
        ("MAYBE"     . 'org-maybe)
        ("PLANNED"   . 'org-maybe)
        ("CANCELED"  . 'org-canceled)
        ("DROPPED"   . 'org-canceled)))

;;; functions
;;;; lib

(defun org-show-from-top ()
  (interactive)
  (save-excursion
    (ignore-errors
      (org-back-to-heading t))
    (org-show-entry)
	(org-show-children)
    (ignore-errors
      (while t
        (outline-up-heading 1 t)
        (org-show-entry)
        (org-show-children))))
  (org-show-subtree))

(setq org-blank-before-new-entry nil)

(defun org-folded-p ()
  "Returns non-nil if point is on a folded headline or plain list
item."
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (or (invisible-p (point-at-eol)))))

;;;; commands

(defun org-toggle-export-on-save (type)
  "Enable or disable export HTML when saving current buffer."
  (interactive)
  (when (not (eq major-mode 'org-mode))
    (error "Not an org-mode file!"))
  (if (memq type after-save-hook)
      (progn (remove-hook 'after-save-hook type t)
             (message "Disabled org export on save"))
    (add-hook 'after-save-hook type nil t)
    (set-buffer-modified-p t)
    (message "Enabled org export on save")))

;;;; TODO:naming interactive

(defun org-better-cycle-back ()
  (interactive)
  (if (org-at-table-p)
      (progn
        (org-table-previous-field)
        ;; (evil-forward-WORD-end)
        ;; (evil-backward-WORD-begin)
        )
    (org-shifttab)))

(defun org-better-cycle-forward ()
  (interactive)
  (if (org-at-table-p)
      (progn
        (org-table-next-field)
        ;; (evil-forward-WORD-end)
        ;; (evil-backward-WORD-begin)
        )
    (org-force-cycle-archived)))

(defun org-dwim-cycle ()
  (interactive)
  (save-excursion
    (if (and (looking-at ".*\\]\\]") (looking-back "\\[\\[.*"))
        (org-toggle-link-display)
      (org-shiftright))))

(defun evil-org-insert-return ()
  (interactive)
  (cond
   ((org-folded-p)
    nil)
   ((org-at-heading-p)
    (evil-insert-newline-below))
   ((newline-and-indent))))

(defun evil-org-meta-return ()
  (interactive)
  (cond
   ((org-at-heading-p)
    (progn
      (org-insert-heading-respect-content)
      (evil-append 1)))
   ((org-in-item-p)
    (progn
      (evil-append-line 1)
      (org-insert-item)))
   ((progn
      (org-meta-return)))))

(defun evil-org-meta-shift-return ()
  (interactive)
  (cond
   ((org-at-heading-p)
    (progn
      (org-insert-todo-heading-respect-content)
      (evil-append 1)))
   ((org-in-item-p)
    (progn
      (evil-append-line 1)
      (org-insert-todo-heading nil)))
   ((progn
      (org-meta-return)))))

(defun evil-org-c-backspace ()
  (interactive)
  (cond
   ((org-at-heading-or-item-p)
    (progn
      (evil-beginning-of-line)
      (evil-forward-WORD-begin)
      (call-interactively 'evil-delete-line)))
   ((backward-kill-word 1))))   

(defun evil-org-backspace ()
  (interactive)
  (cond
   ((or
     (and (-any? 'looking-back '("[+-] " "* " "[0-9]+\. " ".] ")) (org-in-item-p))
     (and (-any? 'looking-back '("* " "* TODO " "* DONE " "* DOING " "* CANCELED " "* DONE BUT ")) (org-at-heading-p)))
    (progn
      (kill-whole-line)
      (org-update-checkbox-count)
      (org-update-statistics-cookies t)
      (evil-previous-line)
      (evil-append-line 1)))
   ;;(insert " ")))
   ((evil-delete-backward-char-and-join 1))))

(defun evil-org-forward-paren ()
  (interactive)
  (if (org-at-heading-p)
      (progn
        (org-next-visible-heading 1)
        (evil-forward-WORD-begin))
    (org-forward-sentence)))

(defun evil-org-backward-paren ()
  (interactive)
  (if (org-at-heading-p)
      (progn
        (org-previous-visible-heading 1)
        (evil-forward-WORD-begin))
    (org-backward-sentence)))

(defun evil-org-forward-bracket ()
  (interactive)
  (if (org-at-heading-p)
      (progn
        (org-forward-heading-same-level 1)
        (evil-forward-WORD-begin))
    (evil-forward-paragraph)))

(defun evil-org-backward-bracket ()
  (interactive)
  (if (org-at-heading-p)
      (progn
        (org-backward-heading-same-level 1)
        (evil-forward-WORD-begin))
    (evil-backward-paragraph)))

;;; keys/keybindings
;;;; TODO:naming movement

(defun my/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(general-define-key
 :states '(normal insert emacs)
 :keymaps '(org-mode-map worf-mode-map)
 "<tab>" 'org-better-cycle-forward
 "<S-iso-lefttab>" 'org-better-cycle-back
 "<C-tab>" 'org-dwim-cycle
 "<M-return>" (lambda () (interactive) (org-insert-heading-respect-content) (evil-append 1))
 "<C-return>" 'evil-org-meta-return
 "<C-S-return>" 'evil-org-meta-shift-return
 "<S-return>" 'newline-and-indent
 "<backspace>" 'evil-org-backspace
 "<C-backspace>" 'evil-org-c-backspace
 "M-j" 'org-metadown
 "M-k" 'org-metaup)

(with-eval-after-load 'org
  (general-define-key
   :keymaps 'org-mode-map
   "C-<left>" 'org-shiftleft
   "C-<right>" 'org-shiftright)
  
  (general-define-key
   :keymaps 'org-mode-map
   :states 'insert
   "<return>" 'evil-org-insert-return)
  
  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal visual)
   ")" 'evil-org-forward-paren
   "(" 'evil-org-backward-paren
   ;; "]" 'evil-org-forward-bracket
   ;; "[" 'evil-org-backward-bracket
   ;; "{" (lambda () (interactive) (org-up-element) (evil-forward-word-begin))
   ;; "}" (lambda () (interactive) (org-down-element) (evil-forward-word-begin))
   )

  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal visual)
   :prefix "z"
   "n" 'my/narrow-or-widen-dwim)

  (use-package org-pretty-jump
    :straight (org-pretty-jump :type git :host github
                               :repo "LeOtaku/org-pretty-jump")
    :after org)

  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal visual)
   :prefix "g"
   "h" 'opj/contrib-jump
   "H" (lambda () (interactive) (opj/contrib-jump t))
   "r" (lambda () (interactive) (worf-refile t))
   "R" (lambda () (interactive) (call-interactively 'org-refile))
   "x" 'org-open-at-point
   "p" (lambda () (interactive) (org-previous-link) (org-show-siblings) (org-show-children))
   "n" (lambda () (interactive) (org-next-link) (org-show-siblings) (org-show-children))))

;;;; global/capture

(general-define-key
 :keymaps 'override
 :prefix "C-x o"
 ;; TODO move this
 "i" (lambda () (interactive) (find-file user-init-file))
 "o" (lambda () (interactive) (find-file org-default-notes-file))
 "l" 'org-store-link
 "a" 'org-agenda
 "c" 'org-capture
 "b" 'org-switchb)

(general-define-key
 :keymaps 'override
 "C-x c" 'org-capture
 "C-x C" (lambda () (interactive) (org-capture nil "g"))
 "C-x o g" (lambda () (interactive) (org-capture nil "g")))

;;;; org-ref

;; (with-eval-after-load 'org
;;   (general-define-key
;;    :keymap org-mode-map
;;    :prefix "C-c ]"
;;    c (key-fn )))

;;; TODO:naming worf

;; (defun worf-get (&optional alt buffer)
;;   (save-excursion
;;     (let ((old-buffer (current-buffer)))
;;       (if buffer (switch-to-buffer buffer t) nil)
;;       (let ((cands (if alt
;;                        (org-map-entries (lambda () (cons (org-format-outline-path (org-get-outline-path t) nil) (point))))
;;                      (worf--goto-candidates))))
;;         (switch-to-buffer old-buffer)
;;         (cdr (assoc (ivy-read "Heading: " cands) cands))))))

;; (defun worf-get-this-buffer (&optional alt)
;;   (let ((cands (if alt
;;                    (org-map-entries (lambda () (cons (org-format-outline-path (org-get-outline-path t)) (point))))
;;                  (worf--goto-candidates))))
;;     (cdr (assoc (ivy-read "Heading: " cands) cands))))

;; (defun worf-get-scoped (&optional alt buffer olp go-up max-level min-level)
;;   (let ((cands (save-excursion
;;                  (setq worf-scoped-min-level min-level)
;;                  (setq worf-scoped-max-level max-level)
;;                  (let ((old-buffer (current-buffer))
;;                        (fun (lambda ()
;;                               (if (and
;;                                    (or (not worf-scoped-min-level) (<= worf-scoped-min-level (org-outline-level)))
;;                                    (or (not worf-scoped-max-level) (>= worf-scoped-max-level (org-outline-level))))
;;                                   (if alt
;;                                       (cons (org-format-outline-path (org-get-outline-path t)) (point))
;;                                     (worf--get-olp-and-point))
;;                                 nil))))
;;                    (if buffer (switch-to-buffer buffer t) nil)
;;                    (if olp (goto-char (org-find-olp olp t)) nil)
;;                    (let ((level (org-current-level)))
;;                      (if (and (/= level 1) go-up) (org-up-element) nil)
;;                      (let ((cands (if (and (= level 1) go-up)
;;                                       (org-map-entries fun)
;;                                     (org-map-tree-better fun))))
;;                        (switch-to-buffer old-buffer)
;;                        (--filter it cands)))))))
;;     (cdr (assoc (ivy-read "Heading: " cands) cands))))

;; (defun worf-get-scoped-this-buffer (&optional alt olp go-up max-level min-level)
;;   (let ((cands (progn
;;                  (setq worf-scoped-min-level min-level)
;;                  (setq worf-scoped-max-level max-level)
;;                  (let ((fun (lambda ()
;;                               (if (and
;;                                    (or (not worf-scoped-min-level) (<= worf-scoped-min-level (org-outline-level)))
;;                                    (or (not worf-scoped-max-level) (>= worf-scoped-max-level (org-outline-level))))
;;                                   (if alt
;;                                       (cons (org-format-outline-path (org-get-outline-path t)) (point))
;;                                     (worf--get-olp-and-point))
;;                                 nil)))))
;;                  (if olp (goto-char (org-find-olp olp t)) nil)
;;                  (let ((level (org-current-level)))
;;                    (if (and (/= level 1) go-up) (org-up-element) nil)
;;                    (let ((cands (if (and (= level 1) go-up)
;;                                     (org-map-entries fun)
;;                                   (org-map-tree-better fun))))
;;                      (--filter it cands)))))))
;;   (cdr (assoc (ivy-read "Heading: " cands) cands)))

;; (defun get-olp-from-pos (pos)
;;   (save-excursion
;;     (goto-char pos)
;;     (org-get-outline-path t)))

;; (defun worf-goto-alt (&optional close-others)
;;   (interactive)
;;   (let ((olp-char (worf-get t)))
;;     (if close-others (evil-close-folds) nil)
;;     (goto-char olp-char)
;;     (org-reveal))
;;   (evil-beginning-of-line)
;;   (evil-forward-WORD-begin 1))

;; (defun worf-refile (&optional alt buffer)
;;   (interactive)
;;   (let ((pos (if alt (worf-get t buffer) (worf-get nil buffer)))
;;         (buffer (or buffer (current-buffer))))
;;     (let
;;         ((marker (set-marker (make-marker) pos buffer))
;;          (headline (car (last (get-olp-from-pos pos)))))
;;       (org-refile nil nil (list headline (buffer-file-name buffer) nil marker)))))

;; (defun org-map-tree-better (fun &optional max-level)
;;   "Call FUN for every heading underneath the current one."
;;   (org-back-to-heading t)
;;   (setq org-map-tree-better-list nil)
;;   (let ((level (funcall outline-level)))
;;     (save-excursion
;;       (funcall fun)
;;       (while (and (progn
;;                     (outline-next-heading)
;;                     (> (funcall outline-level) level))
;;                   (not (eobp)))
;;         (if (or (not max-level) (<= (outline-level) max-level))
;;             (push (funcall fun) org-map-tree-better-list)))
;;       (reverse org-map-tree-better-list))))

;; (require 's)

;; (defun worf--get-olp-and-point ()
;;   (let ((olp (org-get-outline-path t)))
;;     (cons (worf--pretty-heading (s-concat (s-repeat (length olp) "")
;;                                           ""
;;                                           (car (last olp)))
;;                                 (length olp))
;;           (point))))
