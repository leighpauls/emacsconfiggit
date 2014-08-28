;; load the FB libs
(defvar master-dir (getenv "ADMIN_SCRIPTS"))
(load-library (concat master-dir "/master.emacs"))

;; OSX-specific key modifiers
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(setq indent-tabs-mode nil)

(setenv "EDITOR" "emacsclient")

(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin:/opt/local/sbin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/Users/leighpauls/arm-cs-tools/bin:/Users/leighpauls/work/depot_tools:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:/usr/local/share/npm/lib/node_modules/coffee-script/bin:/Users/leighpauls/scripts"))


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
;; (load "~/.emacs.d/javascript.el")
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . javascript-mode))
;; (autoload 'javascript-mode "javascript" nil t)
(autoload 'js2-mode "~/.emacs.d/js2.el" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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

(defun enable-trailing-whitespace ()
  "Turns on trailing whitespace"
  (interactive)
  (setq show-trailing-whitespace t))
(add-hook 'c-mode-common-hook 'enable-trailing-whitespace)
(add-hook 'python-mode-hook 'enable-trailing-whitespace)

(push "~/.emacs.d/magit-1.2.0" load-path)

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

(setq ispell-program-name "aspell")

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer nil))

(global-set-key (kbd "C-x k") 'kill-current-buffer)

(add-to-list 'load-path "~/.emacs.d/mo-git-blame")
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

(defun csv-to-org-table ()
  (interactive)
  (org-table-convert-region 0 (buffer-size) '(16))
  (toggle-truncate-lines 1))

(defvar my-pdb-command
  "python -m pdb /Users/leighpauls/opt/cortex_test.py"
  "Command to run with debug-cur-python-work")

(setq my-pdb-command "python -m pdb /Users/leighpauls/structed_client.py")

(defun debug-cur-python-work ()
  "Execute `my-pdb-command` in gud"
  (interactive)
  (if (get-buffer "*gud-pdb*")
      (kill-buffer "*gud-pdb*"))
  (pdb my-pdb-command))

(defun cortex-telnet ()
  "Make a telnet connect on port 11111"
  (interactive)
  (telnet "localhost" 11111))

;; (ido-mode nil)

(load-file "~/.emacs.d/camelCase-mode.el")
(add-hook 'javascript-mode-hook '(lambda () (camelCase-mode 1)))

(defun eval-buffer-with-message ()
  (interactive)
  (eval-buffer)
  (message "Evaluated buffer \"%s\" successfully!" (buffer-name)))

(defun ftp-to-robot ()
  (interactive)
  (cd "~/NetBeansProjects/Letterman/")
  (ftp "10.36.83.2"))

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

(defun magit-or-monky ()
  "Opens magit or monky, based on whether the current directory is a git or hg project"
  (interactive)
  (cond ((magit-get-top-dir default-directory) (magit-status default-directory))
        ((monky-hg-string "root") (monky-status default-directory))
        (t (error "Not inside a git or hg repo"))))

(defun split-window-right-83 ()
  "Splt window to the right, leaving this column at 83 chars"
  (interactive)
  (split-window-right 83))

; Trivial key bindings
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x p") 'other-window-reverse)
(global-set-key (kbd "C-x a") 'magit-or-monky)
(global-set-key (kbd "C-c b") 'compile)
(global-set-key (kbd "C-c i") 'ispell-comments-and-strings)
(global-set-key (kbd "C-x 9") 'delete-other-windows-vertically)
(global-set-key (kbd "C-$") 'ispell-word)
(global-set-key (kbd "C-c s") 'csv-to-org-table)
(global-set-key (kbd "C-c p") 'debug-cur-python-work)
(global-set-key (kbd "C-c r") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c C-e") 'eval-buffer-with-message)
(global-set-key (kbd "C-c n") 'rename-buffer)
(global-set-key (kbd "M-g r") 'rgrep)
(global-set-key (kbd "C-c f") 'ftp-to-robot)
(global-set-key (kbd "C-x 7") 'split-window-right-83)

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

(add-hook 'org-mode-hook
          '(lambda ()
             (visual-line-mode t)
             (org-indent-mode t)))

(load-file "~/.emacs.d/highlight-80+.el")
(add-hook 'python-mode-hook 'highlight-80+-mode)
(add-hook 'c-mode-common-hook 'highlight-80+-mode)

;; (load-file "~/opt/cortex/demo/structed.el")

(load-file "~/.emacs.d/multi-term.el")
(setq multi-term-program "/bin/bash")

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;; load monky
(add-to-list 'load-path "~/.emacs.d/monky")
(require 'monky)
;; only use one hg process
(setq monky-process-type 'cmdserver)

;; buffer menu mode name column width
(setq Buffer-menu-name-width 48)

;; clear the buffer in eshell
(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
