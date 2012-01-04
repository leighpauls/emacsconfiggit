;; remove toolbar
(tool-bar-mode -1)

;; set editor colours
(set-face-foreground 'default "yellow green")
(set-face-background 'default "DarkOliveGreen")
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; bongo-> media player
(add-to-list 'load-path "~/.emacs.d/bongo")
(autoload 'bongo "bongo"
  "Start Bongo by switching to a Bongo buffer." t)


;; transparency preferences-> for watching movies behind emacs
; (set-frame-parameter (selected-frame) 'alpha '(87 60))
; (add-to-list 'default-frame-alist '(alpha 87 60))
;; Set transparency of emacs
(defun set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; javascript mode
(load "~/.emacs.d/javascript.el")
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
