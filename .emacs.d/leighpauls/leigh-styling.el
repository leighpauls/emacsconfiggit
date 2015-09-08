;; leigh-styling.el
;; Contains all color, text, and "flare"-related settings that I like

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; font
(add-to-list 'default-frame-alist
			 '(font . "DejaVu Sans Mono:pixelsize=14:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true"))

;; make the blue a little brighter so that it can be seen on black
(setq ansi-color-names-vector
  ["black" "red" "green" "yellow" "#7777ff" "magenta" "cyan" "white"])

(menu-bar-mode 1)

(defun enable-trailing-whitespace ()
  "Turns on trailing whitespace"
  (interactive)
  (setq show-trailing-whitespace t))
(add-hook 'c-mode-common-hook 'enable-trailing-whitespace)
(add-hook 'python-mode-hook 'enable-trailing-whitespace)

(setq visible-bell t)

(provide 'leigh-styling)
