;; My configuration loading specific to facebook devservers

;; Preload the master setup of Facebook for Emacs.  The "master.emacs" code
;; depends on the variable 'master-dir' to locate the rest of the modules it
;; requires, so we _have_ to define that first.
(setq master-dir
      (let ((fb-master-dir-env "ADMIN_SCRIPTS"))
        (or (getenv "ADMIN_SCRIPTS")
            (error (format "%s: missing environment var"
                           fb-master-dir-env)))))

;; Preload the bits of emacs master config that I want. Loading all of "$ADMIN_SCRIPTS/master.emacs" is more crap than I want
;; (let ((fb-master-config (expand-file-name "master.emacs" master-dir)))
;;   (load-library fb-master-config))
