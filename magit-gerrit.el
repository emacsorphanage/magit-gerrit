;;; magit-gerrit.el ---
;;
;; Copyright (C) 2013 Brian Fransioli
;;
;; Author: Brian Fransioli <assem@terranpro.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Magit plugin to make Gerrit code review easy-to-use from emacs and
;; without the need for a browser!
;;
;; Currently uses the [deprecated] gerrit ssh interface, which has
;; meant that obtaining the list of reviewers is not possible, only
;; the list of approvals (those who have already verified and/or code
;; reviewed).
;;
;;; To Use:
;;
;; (require 'magit-gerrit)
;; (setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")
;;
;;
;; M-x `magit-status'
;; T C-h  <= magit-gerrit uses the T prefix, see help
;;
;;; Workflow:
;;
;; 1) *check out branch => changes => (ma)git commit*
;; 2) T P  <= [gerri*T* *P*ush for review]
;; 3) T A  <= [gerri*T* *A*dd reviewer] (by email address)
;; 4) *wait for verification/code reviews* [approvals shown in status]
;; 5) T S  <= [gerri*T* *S*ubmit review]
;;
;;; Other Comments:
;; `magit-gerrit-ssh-creds' is buffer local, so if you work with
;; multiple Gerrit's, you can make this a file or directory local
;; variable for one particular project.
;;
;; If your git remote for gerrit is not the default "origin", then
;; `magit-gerrit-remote' should be adjusted accordingly (e.g. "gerrit")
;;
;; Recommended to auto add reviewers via git hooks (precommit), rather
;; than manually performing 'T A' for every review.
;;
;; `magit-gerrit' will be enabled automatically on `magit-status' if
;; the git remote repo uses the same creds found in
;; `magit-gerrit-ssh-creds'.
;;
;; Ex:  magit-gerrit-ssh-creds == br.fransioli@gerrit.org
;; $ cd ~/elisp; git remote -v => https://github.com/terranpro/magit-gerrit.git
;; ^~~ `magit-gerrit-mode' would *NOT* be enabled here
;;
;; $ cd ~/gerrit/prja; git remote -v => ssh://br.fransioli@gerrit.org/.../prja
;; ^~~ `magit-gerrit-mode' *WOULD* be enabled here
;;
;;; Code:

(require 'magit)
(require 'json)

(eval-when-compile
  (require 'cl-lib))

;; Define a defvar-local macro for Emacs < 24.3
(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))

(defvar-local magit-gerrit-ssh-creds nil
  "Credentials used to execute gerrit commands via ssh of the form ID@Server")

(defvar-local magit-gerrit-remote "origin"
  "Default remote name to use for gerrit (e.g. \"origin\", \"gerrit\")")

(defun gerrit-command (cmd &rest args)
  (let ((gcmd (concat
	       "-x -p 29418 "
	       (or magit-gerrit-ssh-creds
		   (error "`magit-gerrit-ssh-creds' must be set!"))
	       " "
	       "gerrit "
	       cmd
	       " "
	       (mapconcat 'identity args " "))))
    ;(message (format "Using cmd: %s" gcmd))
    gcmd))

(defun gerrit-query (prj &optional status)
  (gerrit-command "query"
		  "--format=JSON"
		  "--all-approvals"
		  "--current-patch-set"
		  (concat "project:" prj)
		  (concat "status:" (or status "open"))))

(defun gerrit-review ())

(defun gerrit-ssh-cmd (cmd &rest args)
  (apply #'call-process
	 (executable-find "ssh") nil nil nil
	 (split-string (apply #'gerrit-command cmd args))))

(defun gerrit-review-abandon (prj rev)
  (gerrit-ssh-cmd "review" "--project" prj "--abandon" rev))

(defun gerrit-review-submit (prj rev)
  (gerrit-ssh-cmd "review" "--project" prj "--submit" rev))

(defun gerrit-code-review (prj rev score)
  (gerrit-ssh-cmd "review" "--project" prj "--code-review" score rev))

(defun gerrit-review-verify (prj rev score)
  (gerrit-ssh-cmd "review" "--project" prj "--verified" score rev))

(defun magit-gerrit-get-remote-url ()
  (magit-get "remote" magit-gerrit-remote "url"))

(defun magit-gerrit-get-project ()
 (let* ((regx (rx (zero-or-one ?:) (zero-or-more (any digit)) ?/
		  (group (not (any "/")))
		  (group (one-or-more any))))
	(str (or (magit-gerrit-get-remote-url) ""))
	(sstr (car (last (split-string str "//")))))
   (when (string-match regx sstr)
     (concat (match-string 1 sstr)
	     (match-string 2 sstr)))))

(defun magit-gerrit-string-trunc (str maxlen)
  (if (> (length str) maxlen)
      (concat (substring str 0 maxlen)
	      "...")
    str))

(defun magit-gerrit-pretty-print-reviewer (name email crdone vrdone)
  (let* ((wid (1- (window-width)))
	 (crstr (propertize (if crdone "C" " ")
			    'face '(magit-log-head-label-bisect-bad
				    bold)))
	 (vrstr (propertize (if vrdone "V" " ")
			    'face '(magit-log-head-label-bisect-good
				    bold)))
	 (namestr (propertize (or name "") 'face' magit-diff-add))
	 (emailstr (propertize (if email (concat "(" email ")") "")
			       'face 'change-log-name)))
    (format "%s %s\t%s %s" crstr vrstr namestr emailstr)))

(defun magit-gerrit-pretty-print-review (num subj owner-name)
  ;; window-width - two prevents long line arrow from being shown
  (let* ((wid (- (window-width) 2))
	 (numstr (propertize num 'face 'magit-log-sha1))
	 (nlen (length numstr))
	 (olen (length owner-name))
	 (authmaxlen (/ wid 4))
	 (subjmaxlen (/ wid 2))

	 (author (propertize (magit-gerrit-string-trunc owner-name authmaxlen)
			     'face 'magit-log-author))
	 (subjstr (propertize (magit-gerrit-string-trunc subj subjmaxlen)
			      'face 'magit-log-reflog-label-cherry-pick))
	 (authsubjpadding (make-string
			   (- wid (+ nlen 1 (length author) (length subjstr)))
			   ? )))
    (format "%s\t%s%s%s\n"
	    numstr subjstr authsubjpadding author)))

(defun magit-gerrit-wash-approval (approval)
  (let* ((approver (cdr-safe (assoc 'by approval)))
	 (approvname (cdr-safe (assoc 'name approver)))
	 (approvemail (cdr-safe (assoc 'email approver)))
	 (type (cdr-safe (assoc 'type approval)))
	 (verified (string= type "Verified"))
	 (codereview (string= type "Code-Review"))
	 (score (cdr-safe (assoc 'value approval))))

    (magit-with-section (section approval "Approval")
      (setf (magit-section-info section) approver)
	(insert (concat
	      (magit-gerrit-pretty-print-reviewer
	       approvname approvemail
	       (when codereview score)
	       (when verified score))
	      "\n")))))

(defun magit-gerrit-wash-approvals (approvals)
  (mapc #'magit-gerrit-wash-approval approvals))

(defun magit-gerrit-wash-review ()
  (let* ((beg (point))
	 (jobj (json-read))
	 (end (point))
	 (num (cdr-safe (assoc 'number jobj)))
	 (subj (cdr-safe (assoc 'subject jobj)))
	 (owner (cdr-safe (assoc 'owner jobj)))
	 (owner-name (cdr-safe (assoc 'name owner)))
	 (owner-email (cdr-safe (assoc 'email owner)))
	 (patchsets (cdr-safe (assoc 'currentPatchSet jobj)))
	 (approvs (cdr-safe (if (listp patchsets)
				(assoc 'approvals patchsets)
			      (assoc 'approvals (aref patchsets 0))))))
    (if (and beg end)
	(delete-region beg end))
    (when (and num subj owner-name)
      (magit-with-section (section review subj)
       (setf (magit-section-info section) num)
       (insert
	(propertize
	 (magit-gerrit-pretty-print-review num subj owner-name)
	 'magit-gerrit-jobj
	 jobj))
       (unless (magit-section-hidden (magit-current-section))
	 (magit-gerrit-wash-approvals approvs))
       (add-text-properties beg (point) (list 'magit-gerrit-jobj jobj)))
     t)))

(defun magit-gerrit-wash-reviews ()
  (let ((magit-old-top-section nil))
    (magit-wash-sequence #'magit-gerrit-wash-review)))

(defun magit-gerrit-section (section title washer &rest args)
  (let ((magit-git-executable (executable-find "ssh"))
	(magit-git-standard-options nil))
    (magit-cmd-insert-section (section title)
	washer magit-git-executable (split-string (car args)))))

(defun magit-gerrit-remote-update (&optional remote)
  nil)

(defun magit-gerrit-review-at-point ()
  (get-text-property (point) 'magit-gerrit-jobj))

(defun magit-gerrit-view-patchset-diff ()
  "View the Diff for a Patchset"
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (let ((ref (cdr (assoc 'ref (assoc 'currentPatchSet jobj))))
	    (dir default-directory)
	    (buf (get-buffer-create magit-diff-buffer-name)))
	(let* ((magit-custom-options (list ref))
	       (magit-proc (magit-fetch-current)))
	  (message "Waiting for git fetch to complete...")
	  (magit-process-wait))
	(message (format "Generating Gerrit Patchset for refs %s dir %s" ref dir))
	(magit-diff "FETCH_HEAD~1..FETCH_HEAD")))))

(defun magit-gerrit-download-patchset ()
  "Download a Gerrit Review Patchset"
  (interactive)
  (error "Not Yet Implemented!"))

(defun magit-insert-gerrit-reviews ()
  (magit-gerrit-section 'gerrit-reviews
			"Reviews:" 'magit-gerrit-wash-reviews
			(gerrit-query (magit-gerrit-get-project))))

(defun magit-gerrit-add-reviewer ()
  (interactive)
  "ssh -x -p 29418 user@gerrit gerrit set-reviewers --project toplvlroot/prjname --add email@addr"

  (apply #'call-process
   (executable-find "ssh") nil nil nil
   (split-string (gerrit-command "set-reviewers"
		    "--project"
		    (magit-gerrit-get-project)
		    "--add"
		    (read-string "Reviewer Name/Email: ")
		    (cdr-safe (assoc 'id (magit-gerrit-review-at-point)))))))

(defun magit-gerrit-verify-review ()
  "Verify a Gerrit Review"
  (interactive)
  (let ((score (completing-read "Score: "
				    '("-2" "-1" "0" "+1" "+2")
				    nil t
				    "+1"))
	(rev (cdr-safe (assoc
		      'revision
		      (cdr-safe (assoc 'currentPatchSet
				       (magit-gerrit-review-at-point))))))
	(prj (magit-gerrit-get-project)))
    (gerrit-review-verify prj rev score)
    (magit-refresh)))

(defun magit-gerrit-code-review ()
  "Perform a Gerrit Code Review"
  (interactive)
  (let ((score (completing-read "Score: "
				    '("-2" "-1" "0" "+1" "+2")
				    nil t
				    "+1"))
	(rev (cdr-safe (assoc
		      'revision
		      (cdr-safe (assoc 'currentPatchSet
				       (magit-gerrit-review-at-point))))))
	(prj (magit-gerrit-get-project)))
    (gerrit-code-review prj rev score)
    (magit-refresh)))

(defun magit-gerrit-submit-review ()
  (interactive)
  "ssh -x -p 29418 user@gerrit gerrit review REVISION  -- --project PRJ --submit "
  (apply #'call-process
	 (executable-find "ssh") nil nil nil
	 (split-string
	  (gerrit-command
	   "review"
	   (cdr-safe (assoc
		      'revision
		      (cdr-safe (assoc 'currentPatchSet
				       (magit-gerrit-review-at-point)))))
	   "--project"
	   (magit-gerrit-get-project)
	   "--submit")))
  (magit-fetch-current))

(defun magit-gerrit-create-review ()
  (interactive)
  (let* ((branch (or (magit-get-current-branch)
		     (error "Don't push a detached head.  That's gross")))
	 (commitid (or (when (eq (magit-section-type (magit-current-section))
				 'commit)
			 (magit-section-info (magit-current-section)))
		       (error "Couldn't find a commit at point")))
	 (rev (magit-rev-parse (or commitid
				   (error "Select a commit for review"))))

	 (branch-merge (and branch (magit-get "branch" branch "merge")))
	 (branch-pub (progn
		       (string-match (rx "refs/heads" (group (one-or-more any)))
				    branch-merge)
		       (concat "refs/publish" (match-string 1 branch-merge))))
	 (branch-remote (and branch (magit-get "branch" branch "remote"))))

    (message "Args: %s "
	     (concat rev ":" branch-pub))

    (magit-run-git-async "push" "-v" branch-remote
    			 (concat rev ":" branch-pub))))

(defun magit-gerrit-abandon-review ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
	(id (cdr-safe (assoc 'id
		     (magit-gerrit-review-at-point))))
	(rev (cdr-safe (assoc
			'revision
			(cdr-safe (assoc 'currentPatchSet
					 (magit-gerrit-review-at-point)))))))
    (message "Prj: %s Rev: %s Id: %s" prj rev id)
    (gerrit-review-abandon prj rev)
    (magit-refresh)))


(defun magit-gerrit-create-branch (branch parent))

(progn
  (magit-key-mode-add-group 'gerrit)
  (magit-key-mode-insert-action 'gerrit "P" "Push Commit For Review"
				'magit-gerrit-create-review)
  (magit-key-mode-insert-action 'gerrit "A" "Add Reviewer"
				'magit-gerrit-add-reviewer)
  (magit-key-mode-insert-action 'gerrit "V" "Verify"
				'magit-gerrit-verify-review)
  (magit-key-mode-insert-action 'gerrit "C" "Code Review"
				'magit-gerrit-code-review)
  (magit-key-mode-insert-action 'gerrit "d" "View Patchset Diff"
				'magit-gerrit-view-patchset-diff)
  (magit-key-mode-insert-action 'gerrit "D" "Download Patchset"
				'magit-gerrit-download-patchset)
  (magit-key-mode-insert-action 'gerrit "S" "Submit Review"
				'magit-gerrit-submit-review)
  (magit-key-mode-insert-action 'gerrit "B" "Abandon Review"
				'magit-gerrit-abandon-review)

  (magit-key-mode-generate 'gerrit))

(defvar magit-gerrit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "T") 'magit-key-mode-popup-gerrit)
    map))

(define-minor-mode magit-gerrit-mode "Gerrit support for Magit"
  :lighter " Gerrit" :require 'magit-topgit :keymap 'magit-gerrit-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (or magit-gerrit-ssh-creds
      (error "You *must* set `magit-gerrit-ssh-creds' to enable magit-gerrit-mode"))
  (or (magit-gerrit-get-remote-url)
      (error "You *must* set `magit-gerrit-remote' to a valid Gerrit remote"))
  (cond
   (magit-gerrit-mode
    (magit-add-section-hook 'magit-status-sections-hook
			    'magit-insert-gerrit-reviews
			    'magit-insert-stashes t t)
    (add-hook 'magit-create-branch-command-hook
	      'magit-gerrit-create-branch nil t)
    ;(add-hook 'magit-pull-command-hook 'magit-gerrit-pull nil t)
    (add-hook 'magit-remote-update-command-hook
	      'magit-gerrit-remote-update nil t)
    (add-hook 'magit-push-command-hook
	      'magit-gerrit-push nil t))

   (t
    (remove-hook 'magit-after-insert-stashes-hook
		 'magit-insert-gerrit-reviews t)
    (remove-hook 'magit-create-branch-command-hook
		 'magit-gerrit-create-branch t)
    ;(remove-hook 'magit-pull-command-hook 'magit-gerrit-pull t)
    (remove-hook 'magit-remote-update-command-hook
		 'magit-gerrit-remote-update t)
    (remove-hook 'magit-push-command-hook
		 'magit-gerrit-push t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(defun magit-gerrit-check-enable ()
  (let ((remote-url (magit-gerrit-get-remote-url)))
    (when (and remote-url
	       magit-gerrit-ssh-creds
	       (string-match magit-gerrit-ssh-creds remote-url))
     (magit-gerrit-mode t))))

(add-hook 'magit-status-mode-hook #'magit-gerrit-check-enable t)

(provide 'magit-gerrit)

;;; magit-gerrit.el ends here
