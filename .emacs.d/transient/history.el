((magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-cherry-pick
  ("--ff")
  nil)
 (magit-commit nil)
 (magit-diff
  ("--no-ext-diff" "--stat")
  ("--no-ext-diff"))
 (magit-dispatch nil)
 (magit-fetch
  ("--prune")
  nil)
 (magit-log
  ("-n256" "--graph" "--decorate"))
 (magit-pull nil)
 (magit-push
  ("--force-with-lease")
  nil
  ("--force-with-lease" "--dry-run"))
 (magit-rebase
  ("--update-refs" "--autostash")
  nil
  ("--autostash")
  ("--autostash" "--interactive")
  ("--update-refs" "--autostash" "--interactive"))
 (magit-remote
  ("-f"))
 (magit-reset nil)
 (magit-revert
  ("--edit"))
 (magit-stash nil))
