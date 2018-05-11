;; leigh-file-assocs.el
;; Defines associations of file names to major modes

(add-to-list 'auto-mode-alist '("\\wscript\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\BUCK\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\TARGETS\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\DEFS\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))

(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.aidl\\'" . idl-mode))

(provide 'leigh-file-assocs)
