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

(setq default-tab-width 4)
(add-to-list 'load-path "~/.emacs.d/go" t)
(require 'go-mode-load)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "DarkOliveGreen" :foreground "yellow green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(setq c-default-style "linux"
	  c-basic-offset 4)

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
(load-file "~/cedet-1.0.1/common/cedet.el")

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

(setq semanticdb-project-roots
	  (list "/home/leigh/wksp/cacheflow/CF_3_1/src"
			"/home/leigh/Perforce/workspace/lp-workspace/cacheflow/CF_3_1/src"
			"/home/leigh/wksp/Carrier/CF_2_1"
			"/home/leigh/Perforce/workspace/lp-workspace/Carrier/CF_2_1"))

;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languges only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)

(defun my-semantic-hook ()
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cs" 'semantic-symref-symbol))
(add-hook 'c-mode-common-hook 'my-semantic-hook)

