((magit-fetch "--prune")
 (magit-push "--force-with-lease")
 (magit-rebase "--update-refs" "--autostash"))
