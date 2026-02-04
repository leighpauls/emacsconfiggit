(print "begin loading .emacs")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; remove toolbar so it doesn't flash about
(when window-system
  (tool-bar-mode -1))

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
(require 'leigh-styling)
(require 'leigh-compilation)
(require 'leigh-lpass)

(setq default-tab-width 4)
(setq js-indent-level 2)

(add-hook 'org-mode-hook
          '(lambda () (visual-line-mode t) (org-indent-mode t)))

;; buffer-menu-mode (aka: C-x b) name column width
(setq Buffer-menu-name-width 48)

;; Start the emacs server after everything is working
(server-start)

;;
;; Helper function definitions
;;

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

;; clear the buffer in eshell
(defun eshell/clr ()
  "clear the eshell buffer."
  (interactive)
  (eshell/clear t))

(defun android-logcat-cleared ()
  "Opens android-logcat after clearing it from adb, so long-running devices won't spit out logs for a long period of time"
  (interactive)
  (shell-command-to-string "adb logcat -c")
  (android-logcat))


(put 'downcase-region 'disabled nil)

(add-hook 'dired-after-readin-hook 'hl-line-mode)
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)

(defun leigh-select-clang-format ()
  "Select clang-format based on which computer I'm on"
  (let ((roblox-exec "/Users/lpauls/git/roblox/game-engine/Tools/rbox/macos-universal/clang-format"))
    (or (and (file-executable-p roblox-exec) roblox-exec)
        (executable-find "clang-format")
        "clang-format")
  ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(clang-format-executable (leigh-select-clang-format))
 '(column-number-mode t)
 '(company-idle-delay nil)
 '(eglot-events-buffer-size 0)
 '(eglot-hierarchy-call-site t)
 '(eldoc-echo-area-use-multiline-p 5)
 '(enable-recursive-minibuffers t)
 '(git-commit-setup-hook
   '(git-commit-save-message git-commit-setup-changelog-support git-commit-propertize-diff bug-reference-mode with-editor-usage-message))
 '(ispell-program-name "aspell")
 '(magit-refs-sections-hook
   '(magit-insert-error-header magit-insert-branch-description magit-insert-local-branches))
 '(magit-status-headers-hook
   '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-repo-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header))
 '(minibuffer-depth-indicate-mode t)
 '(package-selected-packages
   '(imenu-list vterm cmake-mode eglot-supplements eglot-hierarchy clang-format glsl-mode magit rust-mode so-long protobuf-mode with-editor dashboard transient company-lsp company lsp-ui flycheck yasnippet lsp-mode free-keys yaml-mode highlight-indentation))
 '(package-vc-selected-packages
   '((eglot-hierarchy :vc-backend Git :url "https://github.com/dolmens/eglot-hierarchy")
     (eglot-supplements :vc-backend Git :url "https://codeberg.org/harald/eglot-supplements.git" :branch "v-20250114")))
 '(tool-bar-mode nil)
 '(vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git)))

(put 'upcase-region 'disabled nil)

(require 'eglot)
(require 'cc-mode)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'company-mode)

(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'company-mode)
(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

(with-eval-after-load 'company
  (define-key company-mode-map (kbd "C-<tab>") 'company-complete))

(require 'clang-format)
(define-key c++-mode-map (kbd "C-c C-f") 'clang-format)

(define-key eglot-mode-map (kbd "C-%") 'xref-find-references-and-replace)
(define-key eglot-mode-map (kbd "C-c h") 'eglot-hierarchy-call-hierarchy)


(require 'cmake-mode)

(unless (eq system-type 'windows-nt)
  (require 'vterm)
  (add-hook 'vterm-copy-mode-hook
            (lambda ()
              (if vterm-copy-mode
                  (progn
                    (setq-local vterm-copy-saved-cursor-type cursor-type)
                    (setq cursor-type t)) ;; Show cursor in copy mode
                (setq cursor-type vterm-copy-saved-cursor-type))))) ;; Hide it when returning to terminal

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(defun my-trace-buffer-send-message (format-string &rest args)
  (let ((buffer (get-buffer-create "*my-trace*")))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (insert (apply #'format format-string args))
        (insert "\n")))))

(defun my-trace/magit-process-git (orig-fun &rest args)
  "Advice to trace start and end times of a function."
  (let ((start-time (current-time)))
    ;; Run the original function with its arguments
    (let ((start-point (point))
          (result (apply orig-fun args))
          (total-time (float-time (time-subtract (current-time) start-time)))
          (end-point (point)))
      (if (> total-time 0.1)
          (my-trace-buffer-send-message "Trace: BIG magit-process-git took %.4f seconds. cmd: %s" total-time (list args))
        (my-trace-buffer-send-message "Trace: magit-process-git took %.4f seconds. cmd: %s" total-time (list args)))
      (my-trace-buffer-send-message "Trace: output: %s" (- end-point start-point))
      ;; Return the original result to not break functionality
      result)))

(advice-add 'magit-process-git :around #'my-trace/magit-process-git)

(defun my-trace/pre-refresh-hook ()
  (my-trace-buffer-send-message "=======START REFRESH========="))
(defun my-trace/post-refresh-hook ()
  (my-trace-buffer-send-message "=======END REFRESH========="))
(add-hook 'magit-pre-refresh-hook #'my-trace/pre-refresh-hook)
(add-hook 'magit-post-refresh-hook #'my-trace/post-refresh-hook)

(print "finished loading .emacs")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight regular :height 120 :width normal)))))
