magit-gerrit
============

Magit plugin for Gerrit Code Review

Author
============

Brian Fransioli  ( assem@terranpro.org )


Installation
============

```
;; add magit-gerrit to `load-path'

(require 'magit-gerrit)

(setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")
```

Workflow
============

1. Check out branch, make changes, and commit...
2. Gerrit Push Commit for Code Review => T P
3. Gerrit Add Reviewer => T A (optional)
4. Wait for code review and verification (approvals updated in magit-status)
5. Gerrit Submit Review => T S

Enjoy!

Please help improve magit-gerrit!  Pull requests welcomed!
