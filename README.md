magit-gerrit
============

Magit plugin for Gerrit Code Review


Installation
============

```
;; add magit-gerrit to `load-path'

(require 'magit-gerrit)

(setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")

;; if necessary, use an alternative remote instead of 'origin'
(setq-default magit-gerrit-remote "gerrit")  
```


Workflow
============

1. Check out branch, make changes, and commit...
2. Gerrit Push Commit for Code Review => T P
3. Gerrit Add Reviewer => T A (optional)
4. Wait for code review and verification (approvals updated in magit-status)
5. Gerrit Submit Review => T S


Magit Gerrit Configuration
============

For simple setups, it should be enough to set the default value for 
`magit-gerrit-ssh-creds` and `magit-gerrit-remote` as shown above.

For per project configurations, consider using buffer local or directory local
variables.


`/home/dev/code/prj1/.dir-locals.el`:

```
((magit-mode .
      ((magit-gerrit-ssh-creds "dev_a@prj1.server.com")
       (magit-gerrit-remote "gerrit"))))
```

Author
============

Brian Fransioli  ( assem@terranpro.org )


Acknowledgements
============

Thanks for using and improving magit-gerrit!  Enjoy!

Please help improve magit-gerrit!  Pull requests welcomed!
