;; global key bindings

(general-define-key
 :keymaps 'override
 "M-:" 'eval-expression
 "M-x" 'helm-M-x)

(general-define-key
 :prefix "C-x"
 "f" 'find-file
 "C-f" 'counsel-locate
 "F" 'counsel-fzf)

(general-define-key
 :keymaps 'override
 :states '(normal insert visual motion emacs)
 :prefix "C-a"
 "c" 'persp-copy
 "n" 'persp-next
 "p" 'persp-prev
 "a" 'delete-other-windows
 "w" 'split-window-vertically
 "q" 'split-window-horizontally
 "X" 'persp-kill
 "x" 'evil-window-delete
 "k" 'evil-window-up
 "l" 'evil-window-right
 "h" 'evil-window-left
 "j" 'evil-window-down
 "C-k" (lambda () (interactive) (evil-window-move-very-top))
 "C-j" (lambda () (interactive) (evil-window-move-very-down))
 "C-h" (lambda () (interactive) (evil-window-move-far-left))
 "C-l" (lambda () (interactive) (evil-window-move-far-right)))

;; (general-define-key
;;  "M-k" 'evil-window-up
;;  "M-l" 'evil-window-right
;;  "M-h" 'evil-window-left
;;  "M-j" 'evil-window-down)

(defun next-code-buffer ()
  (interactive)
  (let ((current-buffer (buffer-name)))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not (string-match-p "\*dashboard\*" (buffer-name)))
         (not (string-match-p "\*scratch\*" (buffer-name)))
         (not (string-match-p "\*terminal\*" (buffer-name)))
         (not (equal current-buffer (buffer-name))))
      (next-buffer))))

(defun previous-code-buffer ()
  (interactive)
  (let ((current-buffer (buffer-name)))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not (string-match-p "\*dashboard\*" (buffer-name)))
         (not (string-match-p "\*scratch\*" (buffer-name)))
         (not (string-match-p "\*terminal\*" (buffer-name)))
         (not (equal current-buffer (buffer-name))))
      (previous-buffer))))

(general-define-key
  :keymaps 'override
  "<XF86Back>" 'previous-code-buffer
  "<XF86Forward>" 'next-code-buffer
  "C-q" 'kill-this-buffer)

(global-set-key [remap next-buffer] 'next-code-buffer)
(global-set-key [remap previous-buffer] 'previous-code-buffer)
