((magit-fetch "--prune")
 (magit-merge "--ff-only")
 (magit-push "--force-with-lease")
 (magit-rebase "--update-refs" "--autostash"))
