(defun leigh-lpass-ensure-login ()
  (interactive)
  (let ((command-result-code (call-process-shell-command "lpass status"
                                                         nil nil nil)))
    (unless (eq 0 command-result-code)
      (eshell-command "lplogin"))))

(defun leigh-lpass-insert-pass (password-name)
  (leigh-lpass-ensure-login)
  (let ((output-buffer (get-buffer-create "*lpass-output*")))
    ;; clear the buffer
    (save-current-buffer
      (set-buffer output-buffer)
      (erase-buffer))
    (let* ((lpass-command (concat "lpass show --password " password-name))
           (command-result-code
            (call-process-shell-command lpass-command nil output-buffer nil))
           (password-or-error
            (save-current-buffer
              (set-buffer output-buffer)
              ;; Remove trailing whitespace
              (replace-regexp-in-string "\n+" "" (buffer-string)))))
      (if (eq 0 command-result-code)
          (insert password-or-error)
        (error "Lastpass command failed: (%s) %s"
               command-result-code password-or-error)))))

(defun leigh-lpass-insert-okta-pass ()
  (interactive)
  (leigh-lpass-insert-pass "okta.com"))

(defun leigh-lpass-insert-laptop-pass ()
  (interactive)
  (leigh-lpass-insert-pass "laptop"))


(provide 'leigh-lpass)

