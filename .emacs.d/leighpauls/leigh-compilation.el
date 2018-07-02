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

(defun eshell/compilehere (&rest args)
  (let ((command (mapconcat 'shell-quote-argument args " ")))
    (compile command)
    (setq compile-from-dir-last-root default-directory)
    (setq compile-from-dir-last-command command)))
  
(defun leigh-compilation-buck-spam-filter ()
  (save-excursion
    (search-backward "\n")
    (when (re-search-backward "^\\(BUILT\\|CACHE\\|MATCH\\|FOUND\\|Android NDK\\:\\).*\n" nil t)
      (while (re-search-backward "^\\(BUILT\\|CACHE\\|MATCH\\|FOUND\\|Android NDK\\:\\).*\n" nil t)
        (replace-match "" nil nil)))))

(add-hook 'compilation-filter-hook 'leigh-compilation-buck-spam-filter)

(defun leigh-compilation-slash-spam-filter ()
  (save-excursion
    (let ((filter-region-end (point)))
      (goto-char compilation-filter-start)
      (search-backward "\n")
      (while (re-search-forward "//+" nil t)
        (replace-match "/" nil nil)))))
(add-hook 'compilation-filter-hook 'leigh-compilation-slash-spam-filter)

(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun compile-goto-error-in-idea ()
  "Visit the source for the error in Intellij Idea"
  (interactive)
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation beginning"))
  (compilation--ensure-parse (point))
  (let ((compilation-message (get-text-property (point) 'compilation-message)))
    (when compilation-message
      (let* ((file-struct (compilation--loc->file-struct
                           (compilation--message->loc compilation-message)))
             (filename (car (compilation--file-struct->file-spec file-struct)))
             (line (caadr (compilation--file-struct->loc-tree file-struct))))
        (shell-command (format "idea --line %s %s" line filename))))))

(defun leigh-compilation-mode-hook ()
  (local-set-key (kbd "i") 'compile-goto-error-in-idea)
  (toggle-truncate-lines t))
(add-hook 'compilation-mode-hook 'leigh-compilation-mode-hook)

(provide 'leigh-compilation)
