;; leigh-load-modes.el
;; Do any loading/configuration for third-party modes I want to pull in

(add-to-list 'load-path "~/.emacs.d/js2-mode/")
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(require 'color-theme)
(color-theme-initialize)
(color-theme-tm)

(add-to-list 'load-path "~/.emacs.d/google-c-style/")
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(add-to-list 'load-path "~/.emacs.d/magit-1.2.2/")
(require 'magit)

(add-to-list 'load-path "~/.emacs.d/mo-git-blame/")
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

(add-to-list 'load-path "~/.emacs.d/camelCase-mode/")
(require 'camelCase-mode)
(add-hook 'javascript-mode-hook '(lambda () (camelCase-mode 1)))

(add-to-list 'load-path "~/.emacs.d/highlight-80+/")
(require 'highlight-80+)
(add-hook 'python-mode-hook 'highlight-80+-mode)
(add-hook 'c-mode-common-hook 'highlight-80+-mode)

(add-to-list 'load-path "~/.emacs.d/android-mode/")
(require 'android-mode)
(setq android-mode-sdk-dir "~/android-sdk-macosx/")

(add-to-list 'load-path "~/.emacs.d/dired-details/")
(require 'dired-details+)

(provide 'leigh-load-modes)
