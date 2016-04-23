;; leigh-keys.el
;; Global key-chord bindings that I like

;; Simple Custom functions
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer nil))

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

(defun eval-buffer-with-message ()
  (interactive)
  (eval-buffer)
  (message "Evaluated buffer \"%s\" successfully!" (buffer-name)))

;; for key binding
(defun split-window-right-83 ()
  "Splt window to the right, leaving this column at 83 chars"
  (interactive)
  (split-window-right 83))


;; Simple global bindings
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x a") 'magit-status)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x p") 'other-window-reverse)
(global-set-key (kbd "C-c b") 'compile-from-dir)
(global-set-key (kbd "C-c i") 'ispell-comments-and-strings)
(global-set-key (kbd "C-x 9") 'delete-other-windows-vertically)
(global-set-key (kbd "C-$") 'ispell-word)
(global-set-key (kbd "C-c p") 'debug-cur-python-work)
(global-set-key (kbd "C-c r") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c C-e") 'eval-buffer-with-message)
(global-set-key (kbd "C-c n") 'rename-buffer)
(global-set-key (kbd "M-g r") 'rgrep)
(global-set-key (kbd "C-x 7") 'split-window-right-83)
(global-set-key (kbd "C-c l") 'sort-lines)
(global-set-key (kbd "C-c A") 'android-logcat-cleared)
(global-set-key (kbd "C-x w") 'other-frame)
(global-set-key (kbd "C-c M-%") 'query-replace-regexp)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

; Key bindings which need to override major modes
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map (kbd "C-c C-r") 'rotate-windows)
(define-minor-mode my-keys-minor-mode
  "A minor mode which forces my keybindings to take precedence over major modes"
  t "my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;; Don't keys overriding in the minibuffer
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(defun use-clang-format ()
  (local-unset-key (kbd "TAB"))
  (local-set-key (kbd "TAB") 'clang-format-region))

(add-hook 'c-mode-common-hook 'use-clang-format);

(provide 'leigh-keys)
