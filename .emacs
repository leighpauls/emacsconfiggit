;; -*- lexical-binding:t -*-

(print "begin loading .emacs")

;;
;; Package management
;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;
;; Early UI
;;

(when window-system
  (tool-bar-mode -1))

(menu-bar-mode 1)

;;
;; Custom file
;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;;
;; Configure default faces
;;
(let ((custom-face
       (pcase system-type
         ('windows-nt '(default ((t (:family "Consolas"
                                             :foundry "outline"
                                             :slant normal
                                             :weight regular
                                             :height 120
                                             :width normal)))))
         ('darwin '(default ((t (:family "Menlo"
                                         :inherit nil
                                         :extend nil
                                         :stipple nil
                                         :background "#000000"
                                         :foreground "#ffffff"
                                         :inverse-video nil
                                         :box nil
                                         :strike-through nil
                                         :overline nil
                                         :underline nil
                                         :slant normal
                                         :weight regular
                                         :height 140
                                         :width normal
                                         :foundry "nil" )))))
         (_ nil))))
  (if custom-face
      (custom-set-faces custom-face)
    (message "No custom face defined for %s" 'system-type)))


;;
;; Load path and personal modules
;;

(add-to-list 'load-path "~/.emacs.d/leighpauls/")
(require 'leigh-env)
(require 'leigh-file-assocs)
(require 'leigh-keys)
(require 'leigh-styling)
(require 'leigh-compilation)

;;
;; Basic settings
;;

(setq default-tab-width 4)
(setq js-indent-level 2)
(setq Buffer-menu-name-width 48)

(add-hook 'org-mode-hook
          '(lambda () (visual-line-mode t) (org-indent-mode t)))

(server-start)

;;
;; use-package blocks
;;

(use-package eglot
  :hook ((c-mode c++-mode rust-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-%" . xref-find-references-and-replace)
         ("C-c h" . eglot-hierarchy-call-hierarchy))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-hierarchy-call-site t))

(use-package cc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
  :custom
  (c-basic-offset 4))

(use-package company
  :hook ((c++-mode rust-mode) . company-mode)
  :bind (:map company-mode-map
         ("C-<tab>" . company-complete))
  :custom
  (company-idle-delay nil))

(use-package clang-format
  :after cc-mode
  :bind (:map c++-mode-map
         ("C-c C-f" . clang-format))
  :init
  (defun leigh-select-clang-format ()
    "Select clang-format based on which computer I'm on"
    (let ((roblox-exec "/Users/lpauls/git/roblox/game-engine/Tools/rbox/macos-universal/clang-format"))
      (or (and (file-executable-p roblox-exec) roblox-exec)
          (executable-find "clang-format")
          "clang-format")))
  :custom
  (clang-format-executable (leigh-select-clang-format)))

(use-package rust-mode
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode rust-mode) .
                   ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))))

(use-package magit
  :config
  (transient-append-suffix 'magit-fetch "-F"
    '("-m" "Use refmap" "--refmap=+refs/heads/*:refs/remotes/origin/*"))
  (transient-append-suffix 'magit-remote "d u"
    '("M" "Update minimal refmap" leigh/magit-remote-update-limited-fetch-spec))
  (advice-add 'magit-process-git :around #'my-trace/magit-process-git)
  (add-hook 'magit-pre-refresh-hook #'my-trace/pre-refresh-hook)
  (add-hook 'magit-post-refresh-hook #'my-trace/post-refresh-hook)
  :custom
  (git-commit-setup-hook
   '(git-commit-save-message git-commit-setup-changelog-support git-commit-propertize-diff bug-reference-mode with-editor-usage-message))
  (magit-refs-sections-hook
   '(magit-insert-error-header magit-insert-branch-description magit-insert-local-branches))
  (magit-status-headers-hook
   '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-repo-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header)))

(use-package vterm
  :if (not (eq system-type 'windows-nt))
  :config
  (add-hook 'vterm-copy-mode-hook
            (lambda ()
              (if vterm-copy-mode
                  (progn
                    (setq-local vterm-copy-saved-cursor-type cursor-type)
                    (setq cursor-type t))
                (setq cursor-type vterm-copy-saved-cursor-type))))
  :bind (:map vterm-copy-mode-map
         ("C-a" . move-beginning-of-line)
         ("C-e" . claude-end-of-line)))

(use-package cmake-mode)

(use-package imenu-list)

(use-package dash)

;;
;; General settings
;;

(setq column-number-mode t)
(setq eldoc-echo-area-use-multiline-p 5)
(setq enable-recursive-minibuffers t)
(setq ispell-program-name "aspell")
(setq vc-follow-symlinks t)
(setq vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git))
(minibuffer-depth-indicate-mode t)

;;
;; Custom defuns
;;

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

(defun eshell/clr ()
  "clear the eshell buffer."
  (interactive)
  (eshell/clear t))

(defun claude-end-of-line ()
  "Move point to the end of the line, ignoring the whitespace that claude code inserts into the buffer"
  (interactive)
  (goto-char
   (save-excursion
     (progn (move-beginning-of-line ())
            (let ((start (point)))
              (move-end-of-line ())
              (if-let ((non-space-char (re-search-backward "[^ ]" start t)))
                  (+ 1 non-space-char)
                start))))))

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
    (let ((start-point (point))
          (result (apply orig-fun args))
          (total-time (float-time (time-subtract (current-time) start-time)))
          (end-point (point)))
      (if (> total-time 0.1)
          (my-trace-buffer-send-message "Trace: BIG magit-process-git took %.4f seconds. cmd: %s" total-time (list args))
        (my-trace-buffer-send-message "Trace: magit-process-git took %.4f seconds. cmd: %s" total-time (list args)))
      (my-trace-buffer-send-message "Trace: output: %s" (- end-point start-point))
      result)))

(defun my-trace/pre-refresh-hook ()
  (my-trace-buffer-send-message "=======START REFRESH========="))
(defun my-trace/post-refresh-hook ()
  (my-trace-buffer-send-message "=======END REFRESH========="))

(defun leigh/magit-remote-update-limited-fetch-spec (remote)
  "Update the fetch spec of REMOTE for limited branches.

Updates the fetch spec to include only tracking branches of existing branches
 plus stable and master."
  (interactive (list (magit-read-remote "Update fetch spec of remote")))
  (let* ((local-refs (append (magit-list-local-branch-names) '("master" "stable" "main")))
         (ref-patterns (--map (format "refs/heads/%s" it) local-refs))
         (cb (lambda (lines-str) (leigh/magit-do-update-fetch-spec remote lines-str))))
   (leigh/magit-remote-find-matching-tracking-bracnhes cb remote ref-patterns)))

(defun leigh/magit-remote-find-matching-tracking-bracnhes (cb remote &rest patterns)
  (let* ((proc (apply #'magit-run-git-async "ls-remote" remote patterns))
         (proc-buf (process-buffer proc))
         (start-pos (with-current-buffer proc-buf (copy-marker (process-mark proc)))))
    (set-process-sentinel proc
                          (lambda (p e)
                            (magit-process-sentinel p e)
                            (let* ((output-str (with-current-buffer (process-buffer p)
                                                (buffer-substring-no-properties start-pos (process-mark p))))
                                  (output-lines (split-string output-str "\n" t ".*\t")))
                              (funcall cb output-lines)))))
  ())

(defun leigh/magit-do-update-fetch-spec (remote local-specs)
  (let* ((variable (format "remote.%s.fetch" remote))
         (fetch-spec-list (--map (leigh/magit-local-spec-to-fetch-spec remote it) local-specs)))
    (message "set %s to %s" variable fetch-spec-list)
    (magit-set-all fetch-spec-list variable)))

(defun leigh/magit-local-spec-to-fetch-spec (remote local-spec)
  (if (string-match "refs/heads/\\(.*\\)" local-spec)
      (let* ((simple-name (match-string 1 local-spec)))
        (format "+refs/heads/%s:refs/remotes/%s/%s" simple-name remote simple-name))
    (error "Bad format for spec %s" local-spec)))

;;
;; Miscellaneous
;;

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'dired-after-readin-hook 'hl-line-mode)
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)

(print "finished loading .emacs")
