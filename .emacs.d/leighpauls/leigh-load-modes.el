;; leigh-load-modes.el
;; Do any loading/configuration for third-party modes I want to pull in

(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(require 'color-theme)
(color-theme-initialize)
(color-theme-tm)

(add-to-list 'load-path "~/.emacs.d/camelCase-mode/")
(require 'camelCase-mode)
(add-hook 'javascript-mode-hook '(lambda () (camelCase-mode 1)))

(add-to-list 'load-path "~/.emacs.d/android-mode/")
(require 'android-mode)
(setq android-mode-sdk-dir "~/android-sdk-macosx/")

(add-to-list 'load-path "~/.emacs.d/dired-details/")
(require 'dired-details+)

(load clang-format-el-path)

(add-to-list 'load-path "~/.emacs.d/monky/")
(require 'monky)
(setq monky-process-type 'cmdserver)

(add-to-list 'load-path "~/.emacs.d/rust-mode-0.4.0/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(require 'yaml-mode)
(require 'highlight-indentation)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)

(provide 'leigh-load-modes)
