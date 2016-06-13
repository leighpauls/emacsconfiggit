;; leigh-compilation.el
;; Config relating to compilation mode

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
  (compile (concat command " 2>&1 | egrep -v '^(BUILT|CACHE|MATCH|FOUND|Android NDK:)'")))

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun compile-goto-error-in-idea ()
  "Visit the source for the error in Intellij Idea"
  (interactive)
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation beginning"))
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (let ((str (buffer-substring-no-properties beg (point))))
        (string-match "^\\(.+?\\):\\(.+?\\):" str)
        (shell-command (format "idea --line %s %s"
                               (match-string 2 str)
                               (match-string 1 str)))))))

(add-hook 'compilation-mode-hook
          (lambda () (local-set-key (kbd "i") 'compile-goto-error-in-idea)))

(provide 'leigh-compilation)
