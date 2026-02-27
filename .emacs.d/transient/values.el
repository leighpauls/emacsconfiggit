((magit-fetch "--prune")
 (magit-merge)
 (magit-push "--force-with-lease")
 (magit-rebase "--update-refs" "--autostash"))
