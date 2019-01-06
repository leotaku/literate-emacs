;; Set up the dashboard page which is shown after emacs is first started.

(use-package dashboard
  :ensure t
  :after evil
  :config
  (setq dashboard-startup-banner "~/.emacs.d/resources/icon.png")
  (setq dashboard-banner-logo-title "Emacs is sexy!")
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)
                          (agenda . 5))))

(defun my-display-startup-screen (&optional concise)
  ;; DONE: Prevent recursive calls from server-process-filter.
  (let ((buffer (get-buffer "*dashboard*")))
    (if buffer
        buffer
      (progn
        (dashboard-insert-startupify-lists)
        (get-buffer "*dashboard*")))))

(setq inhibit-splash-screen nil)
(setq initial-buffer-choice (lambda () (my-display-startup-screen)))

(general-define-key
 :keymaps 'dashboard-mode-map
 :states 'normal
 "SPC" 'dashboard-next-section
 "S-SPC" 'dashboard-previous-section
 "r" 'dashboard-refresh-buffer)
