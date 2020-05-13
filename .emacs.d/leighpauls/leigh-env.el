;; leigh-env.el
;; configures any environment settings I like

(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
      (setenv "EDITOR"
              "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
      (setq ispell-program-name "/usr/local/bin/aspell")
      (setq clang-format-el-path "/usr/local/share/clang/clang-format.el")
      (setenv "PAGER" "cat")
      (setenv "GIT_PAGER" "cat"))
  (setenv "EDITOR" "emacsclient")
  (setq ispell-program-name "/usr/bin/aspell")
  (setq clang-format-el-path
	(if load-fb-devserver-config
	    "/home/engshare/third-party2/clang/dev/src/clang/tools/clang-format/clang-format.el"
	  "~/.emacs.d/clang-format/clang-format.el")))

(setenv "ANDROID_NDK_REPOSITORY" "/opt/android_ndk")
(setenv "ANDROID_SDK" "/opt/android_sdk")
(setenv "ANDROID_HOME" "/opt/android_sdk")

(setenv "GOPATH" "/Users/leigh/go")
(setenv "GOBIN" "/Users/leigh/go/bin")
(setenv "GOPRIVATE" "git.corp.nextdoor.com")

(setq indent-tabs-mode nil)

(defvar path-additions
  '("~/.cargo/bin"
    "~/android-sdk-macosx/platform-tools"
    "~/android-sdk-macosx/tools"
    "/usr/local/bin"
    "/usr/local/go/bin"
    "/opt/android_sdk/platform-tools"
    "/opt/android_sdk/tools"
    "/opt/android_sdk/tools/bin"
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

(defun rebind-to-source-profile ()
  "Runs source ~/.profile and copies the env variables"
  (interactive)
  (print "hello")
  (print (shell-command-to-string "bash -c \"source ~/.profilepoop > /dev/null 2>/dev/null && env\"")))

;; (add-hook 'eshell-mode-hook 'rebind-to-source-profile)

(provide 'leigh-env)
