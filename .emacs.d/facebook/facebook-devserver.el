;; My configuration loading specific to facebook devservers

;; Preload the master setup of Facebook for Emacs.  The "master.emacs" code
;; depends on the variable 'master-dir' to locate the rest of the modules it
;; requires, so we _have_ to define that first.
(setq master-dir
      (let ((fb-master-dir-env "ADMIN_SCRIPTS"))
        (or (getenv "ADMIN_SCRIPTS")
            (error (format "%s: missing environment var"
                           fb-master-dir-env)))))

;; Preload the bits of emacs master config that I want. Loading all of
;; "$ADMIN_SCRIPTS/master.emacs" is more crap than I want
;; (let ((fb-master-config (expand-file-name "master.emacs" master-dir)))
;;   (load-library fb-master-config))

(add-to-list 'load-path (concat master-dir "/emacs-packages"))

;; PHP mode for .phpt files
(autoload 'php-mode "php-mode" nil t nil)

(autoload 'xhp-mode "xhp-mode"
  "Major mode for editing PHP code including XHP support." t)
(setq auto-mode-alist (append '(("\\.phpt?$" . xhp-mode))
                              auto-mode-alist))

;; Set PHP mode based on the #! line
(add-to-list 'interpreter-mode-alist '("php" . xhp-mode))

;; On-the-fly compilation js2-mode for editing .js files
(autoload 'js2-mode "js2-mode" nil t nil)
(setq auto-mode-alist (append '(("\\.js$" . js2-mode))
                              auto-mode-alist))


;; Thrift mode for .thrift files
(autoload 'thrift-mode "thrift" nil t nil)
(setq auto-mode-alist (append '(("\\.thrift$" . thrift-mode))
                              auto-mode-alist))

(autoload 'graphql-mode "graphql" "GraphQL DSL mode")
