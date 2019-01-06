;; all of my org mode specific configurations

;; packages used

(use-package org
  :ensure t
  :config
  (require 'org-protocol))

(use-package worf
  :ensure t
  :after (org))

(use-package org-download
  :ensure t
  :after (org)
  :config
  (org-download-enable))

(use-package ivy-bibtex
  :ensure t
  :after (org))

(use-package org-ref
  :ensure t
  :after (org ivy-bibtex)
  :init
  (setq org-ref-completion-library 'org-ref-helm-cite)
  ;; (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq orhc-multiline t)
  (org-ref-helm-cite-completion)
  (org-ref-ivy-cite-completion)
  (defun orhc-bibtex-candidates ()
    (setq bibtex-completion-bibliography org-ref-bibliography-files)
    (bibtex-completion-candidates))
  (setq bibtex-dialect 'biblatex)
  (require 'doi-utils)
  (require 'org-ref-isbn)

  ;; (setq org-ref-default-bibliography
  ;;  '("~/Projects/org.bib"
  ;;    "~/Projects/zotero.bib"))
  
  ;; (setq bibtex-completion-bibliography org-ref-default-bibliography)

  (general-define-key
   :keymap org-mode-map
   :prefix "C-c"
   "c" 'org-ref-ivy-insert-cite-link
   "C" 'org-ref-helm-cite
   "r" 'org-ref-ivy-insert-ref-link
   "R" 'org-ref-helm-insert-ref-link
   "l" 'org-ref-ivy-insert-label-link
   "L" 'org-ref-helm-insert-label-link))

;; (use-package org-evil
;;   :ensure t
;;   :after (org)
;;   :hook (org-mode . org-evil-mode))

;; (use-package evil-org
;;   :ensure t
;;   :after org
;;   :hook (org-mode . evil-org-mode)
;;   :config
;;   (general-define-key
;;    :keymaps 'org-mode-map
;;    "<C-tab>" 'org-dwim-cycle
;;    "<C-return>" 'evil-org-meta-return))

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'worf-mode)

;; settings

(setq org-ref-completion-library 'org-ref-helm-cite)

(setq org-default-notes-file "~/Projects/notes.org")
(setq org-latex-pdf-process (list "latexmk -interaction=nonstopmode -output-directory=%o -shell-escape -bibtex -f -pdf %f"))

(setq org-image-actual-width 400)
(setq org-startup-with-inline-images t)

(setq org-checkbox-hierarchical-statistics nil)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE BUT" "DONE")
        (sequence "MAYBE" "CANCELED" "|")
        (sequence "WATCHING" "READING" "|" "COMPLETED")
        (sequence "PLANNED" "DROPPED" "|")))

(setq org-link-file-path-type 'relative)

;; (add-hook 'org-capture-mode-hook (lambda () (evil-insert-state) (end-of-line)))
(add-hook 'org-capture-mode-hook (lambda () (evil-append 1)))

;; keys/keybindings

(general-define-key
 :keymaps '(org-mode-map worf-mode-map)
 "<tab>" 'org-force-cycle-archived
 "<C-tab>" 'org-dwim-cycle
 ;;"<C-S-iso-leftab>" 'org-shiftleft
 "<M-return>" (lambda () (interactive) (org-insert-heading-respect-content) (evil-append 1))
 "<C-return>" 'evil-org-meta-return
 "<C-S-return>" 'evil-org-meta-shift-return
 "<S-return>" 'newline-and-indent
 "<backspace>" 'evil-org-backspace
 "<C-backspace>" 'evil-org-c-backspace
 "M-j" 'org-metadown
 "M-k" 'org-metaup
 
 "[" 'self-insert-command
 "]" 'self-insert-command)

(general-define-key
 :keymaps 'org-mode-map
 :states 'insert
 "<return>" 'evil-org-insert-return)

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual)
 ")" (lambda () (interactive) (org-next-visible-heading 1) (evil-forward-word-begin))
 "(" (lambda () (interactive) (org-previous-visible-heading 1) (evil-forward-word-begin))
 "]" (lambda () (interactive) (org-forward-heading-same-level 1) (evil-forward-word-begin))
 "[" (lambda () (interactive) (org-backward-heading-same-level 1) (evil-forward-word-begin))
 "{" (lambda () (interactive) (org-up-element) (evil-forward-word-begin))
 "}" (lambda () (interactive) (org-down-element) (evil-forward-word-begin)))

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual)
 :prefix "g"
 "h" 'worf-goto-alt
 "H" (lambda () (interactive) (worf-goto-alt t)) 
 "r" (lambda () (interactive) (worf-refile t))
 "R" (lambda () (interactive) (call-interactively 'org-refile))
 "x" 'org-open-at-point
 "p" (lambda () (interactive) (org-previous-link) (org-show-siblings) (org-show-children))
 "n" (lambda () (interactive) (org-next-link) (org-show-siblings) (org-show-children)))

(general-define-key
 :keymaps 'override
 "C-x c" 'org-capture
 "C-x C" (lambda () (interactive) (org-capture nil "g"))
 "C-x o g" (lambda () (interactive) (org-capture nil "g")))

(general-define-key
 :keymaps 'override
 :prefix "C-x o"
 "o" (lambda () (interactive) (find-file org-default-notes-file))
 "l" 'org-store-link
 "a" 'org-agenda
 "c" 'org-capture
 "b" 'org-switchb)

(general-define-key
 :keymaps 'org-mode-map
 "C-<left>" 'org-shiftleft
 "C-<right>" 'org-shiftright)

;; better highlight

(font-lock-add-keywords 'org-mode
      '(("^ *\\([0-9]+\\.\\)" (1 'bold))
        ("^ *\\([0-9]+)\\)" (1 'bold))
        ("^ *\\([+-]\\)" (1 'bold))
        ("^ +\\([*]\\)" (1 'bold))))

(font-lock-add-keywords 'org-mode
      '(("\\(->\\)" (1 'bold))))

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

;; org links

(org-add-link-type
 "project" 'projectile-switch-project-by-name)

;; org-capture

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
        

;; evil/dwim functions for key bindings

(defun org-folded-p ()
  "Returns non-nil if point is on a folded headline or plain list
item."
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (or (invisible-p (point-at-eol)))))

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
      (kill-whole-line)
      (org-update-checkbox-count)
      (org-update-statistics-cookies t)
      (evil-previous-line)
      (evil-append-line 1)))
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

;; worf 

(defun worf-get (&optional alt buffer)
  (save-excursion
    (let ((old-buffer (current-buffer)))
      (if buffer (switch-to-buffer buffer t) nil)
      (let ((cands (if alt
                       (org-map-entries (lambda () (cons (org-format-outline-path (org-get-outline-path t) nil) (point))))
                     (worf--goto-candidates))))
        (switch-to-buffer old-buffer)
        (cdr (assoc (ivy-read "Heading: " cands) cands))))))

(defun worf-get-this-buffer (&optional alt)
  (let ((cands (if alt
                   (org-map-entries (lambda () (cons (org-format-outline-path (org-get-outline-path t)) (point))))
                 (worf--goto-candidates))))
    (cdr (assoc (ivy-read "Heading: " cands) cands))))

(defun worf-get-scoped (&optional alt buffer olp go-up max-level min-level)
  (let ((cands (save-excursion
                 (setq worf-scoped-min-level min-level)
                 (setq worf-scoped-max-level max-level)
                 (let ((old-buffer (current-buffer))
                       (fun (lambda ()
                              (if (and
                                   (or (not worf-scoped-min-level) (<= worf-scoped-min-level (org-outline-level)))
                                   (or (not worf-scoped-max-level) (>= worf-scoped-max-level (org-outline-level))))
                                  (if alt
                                      (cons (org-format-outline-path (org-get-outline-path t)) (point))
                                    (worf--get-olp-and-point))
                                nil))))
                   (if buffer (switch-to-buffer buffer t) nil)
                   (if olp (goto-char (org-find-olp olp t)) nil)
                   (let ((level (org-current-level)))
                     (if (and (/= level 1) go-up) (org-up-element) nil)
                     (let ((cands (if (and (= level 1) go-up)
                                      (org-map-entries fun)
                                    (org-map-tree-better fun))))
                       (switch-to-buffer old-buffer)
                       (--filter it cands)))))))
    (cdr (assoc (ivy-read "Heading: " cands) cands))))

(defun worf-get-scoped-this-buffer (&optional alt olp go-up max-level min-level)
  (let ((cands (progn
                 (setq worf-scoped-min-level min-level)
                 (setq worf-scoped-max-level max-level)
                 (let ((fun (lambda ()
                             (if (and
                                  (or (not worf-scoped-min-level) (<= worf-scoped-min-level (org-outline-level)))
                                  (or (not worf-scoped-max-level) (>= worf-scoped-max-level (org-outline-level))))
                                 (if alt
                                     (cons (org-format-outline-path (org-get-outline-path t)) (point))
                                   (worf--get-olp-and-point))
                               nil)))))
                 (if olp (goto-char (org-find-olp olp t)) nil)
                 (let ((level (org-current-level)))
                   (if (and (/= level 1) go-up) (org-up-element) nil)
                   (let ((cands (if (and (= level 1) go-up)
                                    (org-map-entries fun)
                                  (org-map-tree-better fun))))
                     (--filter it cands)))))))
  (cdr (assoc (ivy-read "Heading: " cands) cands)))

(defun get-olp-from-pos (pos)
  (save-excursion
    (goto-char pos)
    (org-get-outline-path t)))

(defun worf-goto-alt (&optional close-others)
  (interactive)
  (goto-char (worf-get t))
  (if close-others (evil-close-folds) nil)
  (outline-show-children 1000)
  (worf-more))

(defun worf-refile (&optional alt buffer)
  (interactive)
  (let ((pos (if alt (worf-get t buffer) (worf-get nil buffer)))
        (buffer (or buffer (current-buffer))))
    (let
        ((marker (set-marker (make-marker) pos buffer))
         (headline (car (last (get-olp-from-pos pos)))))
      (org-refile nil nil (list headline (buffer-file-name buffer) nil marker)))))

(defun org-map-tree-better (fun &optional max-level)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading t)
  (setq org-map-tree-better-list nil)
  (let ((level (funcall outline-level)))
    (save-excursion
      (funcall fun)
      (while (and (progn
                    (outline-next-heading)
                    (> (funcall outline-level) level))
                  (not (eobp)))
        (if (or (not max-level) (<= (outline-level) max-level))
            (push (funcall fun) org-map-tree-better-list)))
      (reverse org-map-tree-better-list))))

(require 's)

;; (defun worf-get-scoped (&optional go-up)
;;   (interactive)
;;   (save-excursion
;;     (ivy-read ""
;;            (let ((level (org-current-level)))
;;              (if (and (= level 1) go-up)
;;                  (org-map-entries #'worf--print-olp)
;;                (if go-up (org-up-element) nil)
;;                (org-map-tree-better #'worf--print-olp))))))

;; (defun worf-get-scoped-alt (&optional go-up)
;;   (interactive)
;;   (save-excursion
;;     (ivy-read ""
;;            (let ((level (org-current-level)))
;;              (if (and (= level 1) go-up)
;;                  (org-map-entries #'worf--get-olp-and-point)
;;                (if go-up (org-up-element) nil)
;;                (org-map-tree-better #'worf--get-olp-and-point))))))

(defun worf--get-olp-and-point ()
  (let ((olp (org-get-outline-path t)))
    (cons (worf--pretty-heading (s-concat (s-repeat (length olp) "")
                                          ""
                                          (car (last olp)))
                                (length olp))
          (point))))
