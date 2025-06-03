;; leigh-env.el
;; configures any environment settings I like

(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
      (setenv "EDITOR"
              "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
      (setenv "PAGER" "cat")
      (setenv "GIT_PAGER" "cat"))
  (setenv "EDITOR" "emacsclient"))

(setenv "TERM" "ansi")

(setq indent-tabs-mode nil)

(defvar path-additions
  '("~/.cargo/bin"
    "/usr/local/bin"
    "/usr/local/go/bin"
    "/opt/homebrew/bin"
    "/opt/local/bin"
    "/opt/local/sbin"
    "~/go/bin"
    "/Users/leigh/bin"))

(require 'cl)
(defun path-join (paths)
  "Joins paths together in the env PATH variable format"
  (cl-reduce (lambda (full-str next-element)
			   (concat full-str path-separator next-element))
			 paths))

(setenv "PATH" (path-join (append (list (getenv "PATH")) path-additions)))

(setq exec-path (append exec-path path-additions))

(provide 'leigh-env)
