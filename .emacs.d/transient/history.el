((magit-blame
  ("-w"))
 (magit-branch nil)
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
  ("--force-with-lease"))
 (magit-rebase
  ("--update-refs" "--autostash")
  nil
  ("--update-refs" "--autostash" "--interactive")
  ("--autostash")
  ("--autostash" "--interactive"))
 (magit-reset nil)
 (magit-revert
  ("--edit"))
 (magit-stash nil))
