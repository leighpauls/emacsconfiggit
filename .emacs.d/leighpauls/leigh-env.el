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
      (setenv "PAGER" "cat"))
  (setenv "EDITOR" "emacsclient")
  (setq ispell-program-name "/usr/bin/aspell")
  (setq clang-format-el-path
	(if load-fb-devserver-config
	    "/home/engshare/third-party2/clang/dev/src/clang/tools/clang-format/clang-format.el"
	  "~/.emacs.d/clang-format/clang-format.el")))

(setenv "ANDROID_NDK_REPOSITORY" "/opt/android_ndk")
(setenv "ANDROID_SDK" "/opt/android_sdk")
(setenv "ANDROID_HOME" "/opt/android_sdk")

(setq indent-tabs-mode nil)

(defvar path-additions
  '("/Users/leighpauls/.cargo/bin"
    "/Users/leighpauls/android-sdk-macosx/platform-tools"
    "/Users/leighpauls/android-sdk-macosx/tools"
    "/usr/local/bin"
    "/usr/local/go/bin"
    "/opt/android_sdk/platform-tools"
    "/opt/android_sdk/tools"
    "/opt/facebook/bin"
    "/opt/facebook/bin"
    "/opt/facebook/hg/bin"
    "/opt/homebrew/bin"
    "/opt/local/bin"
    "/opt/local/sbin"))

(require 'cl)
(defun path-join (paths)
  "Joins paths together in the env PATH variable format"
  (cl-reduce (lambda (full-str next-element)
			   (concat full-str path-separator next-element))
			 paths))

(setenv "PATH" (path-join (append path-additions (list (getenv "PATH")))))

(setq exec-path (append exec-path path-additions))

(provide 'leigh-env)
