;; leigh-env.el
;; configures any environment settings I like

(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
      (setenv "EDITOR"
              "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
      (setenv "ANDROID_NDK_REPOSITORY" "/Users/leighpauls/android-ndk")
      (setenv "ANDROID_SDK" "/Users/leighpauls/android-sdk-macosx")
      (setq ispell-program-name "/usr/local/bin/aspell")
      (setq clang-format-el-path "/usr/local/share/clang/clang-format.el"))
  (setenv "EDITOR" "emacsclient")
  (setq ispell-program-name "/usr/bin/aspell")
  (setq clang-format-el-path "/home/engshare/third-party2/clang/dev/src/clang/tools/clang-format/clang-format.el"))

(setq indent-tabs-mode nil)

(defvar path-additions
  '("/opt/facebook/bin"
    "/Users/leighpauls/android-sdk-macosx/platform-tools"
    "/Users/leighpauls/android-sdk-macosx/tools"
    "/Users/leighpauls/pebble-dev/PebbleSDK-2.8/bin"
    "/bin"
    "/opt/local/bin"
    "/opt/local/sbin"
    "/sbin"
    "/usr/X11/bin"
    "/usr/bin"
    "/usr/local/bin"
    "/usr/local/go/bin"
    "/usr/sbin"))

(require 'cl)
(defun path-join (paths)
  "Joins paths together in the env PATH variable format"
  (cl-reduce (lambda (full-str next-element)
			   (concat full-str path-separator next-element))
			 paths))

(setenv "PATH" (path-join (cons (getenv "PATH") path-additions)))

(provide 'leigh-env)
