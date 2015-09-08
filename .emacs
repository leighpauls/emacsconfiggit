
;; remove toolbar so it doesn't flash about
(tool-bar-mode -1)

;; default values for things that should have been set in config
(when (not (boundp 'ljhp-local-config-loaded))
  (setq load-fb-devserver-config nil))

(when load-fb-devserver-config
  ;; load the FB libs
  (defvar master-dir (getenv "ADMIN_SCRIPTS"))
  (load-library (concat master-dir "/master.emacs")))

(add-to-list 'load-path "~/.emacs.d/leighpauls/")
(require 'leigh-env)
(require 'leigh-load-modes)
(require 'leigh-file-assocs)
(require 'leigh-keys)

(setq default-tab-width 4)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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

;; font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono:pixelsize=14:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true"))

;; make the blue a little brighter so that it can be seen on black
(setq ansi-color-names-vector
  ["black" "red" "green" "yellow" "#7777ff" "magenta" "cyan" "white"])

(menu-bar-mode 1)

(defun enable-trailing-whitespace ()
  "Turns on trailing whitespace"
  (interactive)
  (setq show-trailing-whitespace t))
(add-hook 'c-mode-common-hook 'enable-trailing-whitespace)
(add-hook 'python-mode-hook 'enable-trailing-whitespace)


(defun rotate-windows-helper(x d)
  (if (equal (cdr x) nil)
	  (set-window-buffer (car x) d)
    (set-window-buffer (car x)
					   (window-buffer (cadr x)))
	(rotate-windows-helper (cdr x) d)))

(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

(setq visible-bell t)

(setq ispell-program-name "/usr/local/bin/aspell")

(add-hook 'org-mode-hook
          '(lambda ()
             (visual-line-mode t)
             (org-indent-mode t)))


;; buffer menu mode name column width
(setq Buffer-menu-name-width 48)

;; facebook irc connection
(defun fb-irc ()
  "Connect to the facebook IRC server"
  (interactive)
  (rcirc-connect "irc.tfbnw.net" 6443 "leighpauls" "leighpauls"
                 rcirc-default-full-name nil "ae42-e26b-0b18-3512" 'tls))

;; clear the buffer in eshell
(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  "")

(defun magit-or-monky ()
  "Opens magit or monky, based on whether the current directory is a git or hg project"
  (interactive)
  (cond ((magit-get-top-dir default-directory) (magit-status default-directory))
        ((monky-hg-string "root") (monky-status default-directory))
        (t (error "Not inside a git or hg repo"))))

(defun two-space-indent-buck-files ()
  "sets python indenting to 2 spaces for buck files"
  (interactive)
  (when (string-match "/\\(BUCK\\|TARGETS\\)$" (buffer-file-name))
    (set-variable 'python-indent-offset 2 t)))
(add-hook 'python-mode-hook 'two-space-indent-buck-files)

(setq compilation-skip-threshold 2)
(defvar compile-from-dir-last-root nil)
(defvar compile-from-dir-last-command nil)
(defun compile-from-dir (root command)
  (interactive
   (list (file-name-directory (read-file-name
                               "Compile from: "
                               compile-from-dir-last-root
                               compile-from-dir-last-root
                               t))
         (read-string
          "Command: "
          compile-from-dir-last-command
          nil
          compile-from-dir-last-command)))
  (print (concat root ", " command))
  (let ((default-directory root))
    (compile-with-filter command))
  (setq compile-from-dir-last-root root)
  (setq compile-from-dir-last-command command))

(defun compile-with-filter (command)
  (compile (concat command " 2>&1 | egrep -v '^(BUILT|Android NDK:)'")))

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun android-logcat-cleared ()
  "Opens android-logcat after clearing it from adb, so long-running devices won't spit out logs for a long period of time"
  (interactive)
  (shell-command-to-string "adb logcat -c")
  (android-logcat))

;; Start the emacs server after everything is working
(server-start)
