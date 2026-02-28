;; leigh-load-modes.el
;; Load/configure third-party modes

(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(require 'color-theme)
(color-theme-initialize)
(color-theme-tm)

(provide 'leigh-load-modes)
