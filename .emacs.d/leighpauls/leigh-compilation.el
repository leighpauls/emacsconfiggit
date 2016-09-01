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
    (compile command))
  (setq compile-from-dir-last-root root)
  (setq compile-from-dir-last-command command))

(defun leigh-compilation-buck-spam-filter ()
  (save-excursion
    (let ((filter-region-end (point)))
      (goto-char compilation-filter-start)
      (search-backward "\n")
      (while (re-search-forward "^\\(BUILT\\|CACHE\\|MATCH\\|FOUND\\|Android NDK\\:\\).*\n" nil t)
        (replace-match "" nil nil)))))
(add-hook 'compilation-filter-hook 'leigh-compilation-buck-spam-filter)

(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
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

(defun leigh-compilation-mode-hook ()
  (local-set-key (kbd "i") 'compile-goto-error-in-idea)
  (toggle-truncate-lines t))
(add-hook 'compilation-mode-hook 'leigh-compilation-mode-hook)

(provide 'leigh-compilation)
