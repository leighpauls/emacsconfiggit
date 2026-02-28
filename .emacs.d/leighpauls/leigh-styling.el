;; leigh-styling.el
;; Contains all color, text, and "flare"-related settings that I like

(load-theme 'modus-vivendi t)

;; font

(if (eq system-type 'windows-nt)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight regular :height 120 :width normal)))))
  (add-to-list 'default-frame-alist
               '(font . "Menlo:pixelsize=14:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")))

(menu-bar-mode 1)

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
