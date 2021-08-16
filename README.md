magit-gerrit
============

Magit plugin for Gerrit Code Review


Installation
============

If you have a recent Emacs with `package.el`, you can install `magit-gerrit`
from [MELPA](http://melpa.milkbox.net/).

Otherwise, you'll have to download `magit-gerrit.el` and ensure it is in
a directory in your `load-path`.

Then:

```
(require 'magit-gerrit)

;; if remote url is not using the default gerrit port and
;; ssh scheme, need to manually set this variable
(setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")

;; if necessary, use an alternative remote instead of 'origin'
(setq-default magit-gerrit-remote "gerrit")  
```


Workflow
============

1. Check out branch, make changes, and commit...
2. Gerrit Push Commit for Code Review => R P
3. Gerrit Add Reviewer => R A (optional)
4. Wait for code review and verification (approvals updated in magit-status)
5. Gerrit Submit Review => R S


Magit Gerrit Configuration
============

For simple setups, it should be enough to set the default value for
`magit-gerrit-ssh-creds` and `magit-gerrit-remote` as shown above.

For per project configurations, consider using buffer local or directory local
variables.


`/home/dev/code/prj1/.dir-locals.el`:

```
((magit-mode .
      ((magit-gerrit-ssh-creds . "dev_a@prj1.server.com")
       (magit-gerrit-remote . "gerrit"))))
```

Author
============

Brian Fransioli  ( assem@terranpro.org )


Acknowledgements
============

Thanks for using and improving magit-gerrit!  Enjoy!

Please help improve magit-gerrit!  Pull requests welcomed!
