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

(require 'rust-mode)

(require 'yaml-mode)
(require 'highlight-indentation)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)

(require 'leigh-lpass)

(add-to-list 'load-path "~/.emacs.d/magit-master/lisp")
(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/magit-master/Documentation/"))

(provide 'leigh-load-modes)
