(defun leigh-lpass-insert-okta-pass ()
  (interactive)
  (let ((output-buffer (get-buffer-create "*lpass-output*")))
    ;; clear the buffer
    (save-current-buffer
      (set-buffer output-buffer)
      (erase-buffer))
    (let* ((command-result-code
            (call-process-shell-command "lpass show --password okta.com"
                                        nil output-buffer nil))
           (password-or-error
            (save-current-buffer
              (set-buffer output-buffer)
              ;; Remove trailing whitespace
              (replace-regexp-in-string "\n+" "" (buffer-string)))))
      (if (eq 0 command-result-code)
          (insert password-or-error)
        (error "Lastpass command failed: (%s) %s" command-result-code password-or-error)))))

;; TODO: add a key binding

(provide 'leigh-lpass)
