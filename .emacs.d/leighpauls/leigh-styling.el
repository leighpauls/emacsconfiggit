;; leigh-styling.el
;; Contains all color, text, and "flare"-related settings that I like


(add-hook 'c-mode-common-hook 'leigh-enable-trailing-whitespace)
(add-hook 'python-mode-hook 'leigh-enable-trailing-whitespace)
(add-hook 'emacs-lisp-mode-hook 'leigh-emacs-lisp-highlighting)

(defun leigh-enable-trailing-whitespace ()
  "Turns on trailing whitespace"
  (interactive)
  (setq show-trailing-whitespace t))

(defun leigh-emacs-lisp-highlighting ()
  "Set up Leigh's emacs lisp highlighting prefs"
  (interactive)
  (setq whitespace-style '(face lines-tail))
  (whitespace-mode 1))

(setq visible-bell nil)

(setq-default indent-tabs-mode nil)
(setq-default c-electric-flag nil)
(setq-default c-syntactic-indentation nil)

(setq column-number-mode t)

(provide 'leigh-styling)
