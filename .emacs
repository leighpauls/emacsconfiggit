(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(setq indent-tabs-mode nil)


(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin:/opt/local/sbin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/Users/leighpauls/arm-cs-tools/bin:/Users/leighpauls/work/depot_tools:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin"))


;; remove toolbar
(tool-bar-mode -1)

;; set editor colours
;; (set-face-foreground 'default "yellow green")
;; (set-face-background 'default "DarkOliveGreen")


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

;; coffee mode
(load "~/.emacs.d/coffee-mode.el")

;; javascript mode
(load "~/.emacs.d/javascript.el")
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

(add-to-list 'auto-mode-alist '("\\wscript\\'" . python-mode))

;; ruby mode for .ru files
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))


(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; (setq default-tab-width 4)
(add-to-list 'load-path "~/.emacs.d/go" t)
(require 'go-mode-load)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (setq c-default-style "linux"
;;	  c-basic-offset 4)

(require 'tramp)

;; Open files and goto lines like we see from g++ etc. i.e. file:line#
;; (to-do "make `find-file-line-number' work for emacsclient as well")
;; (to-do "make `find-file-line-number' check if the file exists")
(defadvice find-file (around find-file-line-number
                             (filename &optional wildcards)
                             activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      ad-do-it
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))

;; Start the emacs server by default
(server-start)

;; Color theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(require 'color-theme)
(color-theme-initialize)
(color-theme-tm)

;; font
;; (set-default-font "DejaVu Sans Mono:pixelsize=14:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono:pixelsize=14:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true"))

;;; js-beautify.el -- beautify some js code
(defgroup js-beautify nil
  "Use jsbeautify to beautify some js"
  :group 'editing)

(defcustom js-beautify-args ""
  "Arguments to pass to jsbeautify script"
  :type '(string)
  :group 'js-beautify)

(defcustom js-beautify-path "/home/leigh/js-beautify/python/js-beautify"
  "Path to jsbeautifier python file"
  :type '(string)
  :group 'js-beautify)

(defun js-beautify ()
  "Beautify a region of javascript using the code from jsbeautify.org"
  (interactive)
  (let ((orig-point (point)))
    (unless (mark)
      (mark-defun))
    (shell-command-on-region (point)
                             (mark)
                             (concat js-beautify-path
                                     " --stdin "
                                     js-beautify-args)
                             nil t)
    (goto-char orig-point)))

(provide 'js-beautify)
;;; js-beautify.el ends here

;; make the blue a little brighter so that it can be seen on black
(setq ansi-color-names-vector
  ["black" "red" "green" "yellow" "#7777ff" "magenta" "cyan" "white"])

(menu-bar-mode 1)

;; google c/c++ style
(load-file "~/.emacs.d/google-c-style.el")
(add-hook 'c-mode-common-hook 'google-set-c-style)

(push "~/.emacs.d/magit-1.0.0" load-path)

(require 'magit)

(defun rotate-windows-helper(x d)
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))
 
(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq visible-bell t)

(setq compile-command "~/emacs_tintin_build.sh")
(setq compilation-skip-threshold 2)

(setq ispell-program-name "/usr/local/Cellar/aspell/0.60.6.1/bin/aspell")


(defun kill-current-buffer ()
  (interactive)
  (kill-buffer nil))

(global-set-key (kbd "C-x k") 'kill-current-buffer)

(add-to-list 'load-path "~/.emacs.d/mo-git-blame")
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;; (ido-mode nil)

; Trivial key bindings
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x p") 'other-frame)
(global-set-key (kbd "C-x C-a") 'magit-status)
(global-set-key (kbd "C-x a") 'magit-status)
(global-set-key (kbd "C-c b") 'compile)
(global-set-key (kbd "C-c i") 'ispell-comments-and-strings)
(global-set-key (kbd "C-x 9") 'delete-other-windows-vertically)
(global-set-key (kbd "C-$") 'ispell-word)


(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

; Key bindings which need to override major modes
(define-key my-keys-minor-mode-map (kbd "C-c C-r") 'rotate-windows)

(define-minor-mode my-keys-minor-mode
  "A minor mode which forces my keybindings to take precedence over major modes"
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;; Don't want them overriding in the minibuffer
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            (setq dired-omit-extensions '(".pyc" "~"))
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)
            ))

