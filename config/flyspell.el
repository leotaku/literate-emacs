;; all flyspell related configuration

(use-package flyspell
  :ensure t
  :config
  (setq flyspell-duplicate-distance 0))

(use-package flyspell-correct
  :ensure t
  :after flyspell)

(use-package flyspell-lazy
  :ensure t
  :config
  (flyspell-lazy-mode 1))

(defun flyspell-buffer-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word))
	(when (consp word)
	  (flyspell-do-correct 'buffer nil (car word) current-location (cadr word) (caddr word) current-location)))))

(defun flyspell-global-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word))
	(when (consp word)
	  (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location)))))

(general-define-key
 :keymaps 'flyspell-mode-map
 :states 'normal
 :prefix "z"
 "g" 'flyspell-buffer-word
 "G" 'flyspell-global-word
 "n" 'flyspell-goto-next-error
 "p" 'flyspell-correct-previous
 "=" 'flyspell-correct-at-point)

;; find aspell and hunspell automatically
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

(setq ispell-dictionary "en_US")
(setq flyspell-default-dictionary "en_US")
