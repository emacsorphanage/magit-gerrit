;;; magit-gerrit.el --- Magit plugin for Gerrit Code Review
;;
;; Copyright (C) 2013 Brian Fransioli
;;
;; Author: Brian Fransioli <assem@terranpro.org>
;; URL: https://github.com/terranpro/magit-gerrit
;; Package-Requires: ((magit "2.1.0"))
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
;; h R  <= magit-gerrit uses the R prefix, see help
;;
;;; Workflow:
;;
;; 1) *check out branch => changes => (ma)git commit*
;; 2) R P  <= [ger*R*it *P*ush for review]
;; 3) R A  <= [ger*R*it *A*dd reviewer] (by email address)
;; 4) *wait for verification/code reviews* [approvals shown in status]
;; 5) R S  <= [ger*R*it *S*ubmit review]
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
;; than manually performing 'R A' for every review.
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

(defvar-local magit-gerrit-use-topics nil
  "Flag that indicates the default option of using a topic when pushing to remote")

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
    ;; (message (format "Using cmd: %s" gcmd))
    gcmd))

(defun gerrit-query (prj &optional status)
  (gerrit-command "query"
		  "--format=JSON"
		  "--all-approvals"
		  "--comments"
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

(defun gerrit-review-submit (prj rev &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--submit"
		  (if msg msg "") rev))

(defun gerrit-code-review (prj rev score &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--code-review" score
		  (if msg msg "") rev))

(defun gerrit-review-verify (prj rev score &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--verified" score
		  (if msg msg "") rev))

(defun magit-gerrit-get-remote-url ()
  (magit-git-string "ls-remote" "--get-url" magit-gerrit-remote))

(defun magit-gerrit-get-project ()
 (let* ((regx (rx (zero-or-one ?:) (zero-or-more (any digit)) ?/
		  (group (not (any "/")))
		  (group (one-or-more (not (any "."))))))
	(str (or (magit-gerrit-get-remote-url) ""))
	(sstr (car (last (split-string str "//")))))
   (when (string-match regx sstr)
     (concat (match-string 1 sstr)
	     (match-string 2 sstr)))))

(defun magit-gerrit-query (prompt cands)
  (let ((cmp-read (if (functionp 'ido-completing-read-haha)
		      #'ido-completing-read
		    #'completing-read)))

   (funcall cmp-read
    prompt
    cands
    nil
    t)))

(defun magit-gerrit-query-remote-branch-merge ()
  (interactive)

  (magit-gerrit-query
   "Remote Branch: "
   (let ((rbs (magit-list-remote-branch-names)))
     (mapcar
      #'(lambda (rb)
	  (and (string-match (rx bos
				 (one-or-more (not (any "/")))
				 "/"
				 (group (one-or-more any))
				 eos)
			     rb)
	       (concat ;;"refs/publish/"
		(match-string 1 rb)
		)))
      rbs))))

(defun magit-gerrit-convert-ref (ref-str from &optional to)
  "Cuts or converts a ref string prefix, e.g.  refs/heads/branch -> refs/publish/branch"
  (or (when (string-match (concat from
			     (rx (group (one-or-more any))))
			  ref-str)
	(format "%s%s"
		(or (and to (format "refs/%s/" to)) "")
		(match-string 1 ref-str)))
      ref-str))

(defun magit-gerrit-string-trunc (str maxlen)
  (if (> (length str) maxlen)
      (concat (substring str 0 maxlen)
	      "...")
    str))

(defun magit-gerrit-create-branch-force (branch parent)
  "Switch 'HEAD' to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
Succeed even if branch already exist
\('git checkout -B BRANCH REVISION')."
  (cond ((run-hook-with-args-until-success
	  'magit-create-branch-hook branch parent))
	((and branch (not (string= branch "")))
	 (magit-save-repository-buffers)
	 (magit-run-git "checkout" "-B" branch parent))))


(defun magit-gerrit-pretty-print-reviewer (name email crdone vrdone)
  (let* ((wid (1- (window-width)))
	 (crstr (propertize (if crdone (format "%+2d" (string-to-number crdone)) "  ")
			    'face '(magit-diff-lines-heading
				    bold)))
	 (vrstr (propertize (if vrdone (format "%+2d" (string-to-number vrdone)) "  ")
			    'face '(magit-diff-added-highlight
				    bold)))
	 (namestr (propertize (or name "") 'face 'magit-refname))
	 (emailstr (propertize (if email (concat "(" email ")") "")
			       'face 'change-log-name)))
    (format "%-12s%s %s" (concat crstr " " vrstr) namestr emailstr)))

(defun magit-gerrit-pretty-print-review (num subj owner-name &optional draft)
  ;; window-width - two prevents long line arrow from being shown
  (let* ((wid (- (window-width) 2))
	 (numstr (propertize (format "%-10s" num) 'face 'magit-hash))
	 (nlen (length numstr))
	 (authmaxlen (/ wid 4))

	 (author (propertize (magit-gerrit-string-trunc owner-name authmaxlen)
			     'face 'magit-log-author))

	 (subjmaxlen (- wid (length author) nlen 6))

	 (subjstr (propertize (magit-gerrit-string-trunc subj subjmaxlen)
			      'face
			      (if draft
				  'magit-signature-bad
				'magit-signature-good)))
	 (authsubjpadding (make-string
			   (max 0 (- wid (+ nlen 1 (length author) (length subjstr))))
			   ? )))
    (format "%s%s%s%s\n"
	    numstr subjstr authsubjpadding author)))

(defun magit-gerrit-wash-approval (approval)
  (let* ((approver (cdr-safe (assoc 'by approval)))
	 (approvname (cdr-safe (assoc 'name approver)))
	 (approvemail (cdr-safe (assoc 'email approver)))
	 (type (cdr-safe (assoc 'type approval)))
	 (verified (string= type "Verified"))
	 (codereview (string= type "Code-Review"))
	 (score (cdr-safe (assoc 'value approval))))

    (magit-insert-section (review approval)
      (magit-insert (concat
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
	 (owner-name (or (cdr-safe (assoc 'name owner))
			 (cdr-safe (assoc 'username owner))))
	 (owner-email (cdr-safe (assoc 'email owner)))
	 (patchsets (cdr-safe (assoc 'currentPatchSet jobj)))
	 ;; compare w/t since when false the value is => :json-false
	 (isdraft (eq (cdr-safe (assoc 'isDraft patchsets)) t))
	 (approvs (cdr-safe (if (listp patchsets)
				(assoc 'approvals patchsets)
			      (assoc 'approvals (aref patchsets 0))))))
    (if (and beg end)
	(delete-region beg end))
    (when (and num subj owner-name)
      (magit-insert-section (review subj)
	(magit-insert
	 (propertize
	  (magit-gerrit-pretty-print-review num subj owner-name isdraft)
	  'magit-gerrit-jobj
	  jobj))
	(unless (magit-section-hidden (magit-current-section))
	  (magit-gerrit-wash-approvals approvs)
	  )
	(add-text-properties beg (point) (list 'magit-gerrit-jobj jobj)))
      t)))

(defun magit-gerrit-wash-reviews (&rest args)
  (magit-wash-sequence #'magit-gerrit-wash-review))

(defun magit-gerrit-section (section title washer &rest args)
  (let ((magit-git-executable (executable-find "ssh"))
	(magit-git-standard-options nil))
    (magit-insert-section (section title)
      (magit-insert-heading title)
      (magit-git-wash washer (split-string (car args)))
      (magit-insert "\n"))))

(defun magit-gerrit-remote-update (&optional remote)
  nil)

(defun magit-gerrit-review-at-point ()
  (get-text-property (point) 'magit-gerrit-jobj))

(defun magit-gerrit-view-current-patchset-diff ()
  "View the Diff for Current Patchset"
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (let ((ref (cdr (assoc 'ref (assoc 'currentPatchSet jobj))))
            (dir default-directory))
        (let* ((magit-this-process (magit-fetch magit-gerrit-remote ref)))
          (message (format "Waiting a git fetch from %s to complete..."
                           magit-gerrit-remote))
          (magit-process-wait))
        (message (format "Generating Gerrit Patchset for refs %s dir %s" ref dir))
        (magit-diff "FETCH_HEAD~1..FETCH_HEAD")))))

(defun magit-gerrit-download-current-patchset ()
  "Download Current Gerrit Review Patchset"
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (let ((ref (cdr (assoc 'ref (assoc 'currentPatchSet jobj))))
            (dir default-directory)
            (branch (format "review/%s/%s"
                            (cdr (assoc 'username (assoc 'owner jobj)))
                            (cdr (or (assoc 'topic jobj) (assoc 'number jobj))))))
        (let* ((magit-this-process (magit-fetch magit-gerrit-remote ref)))
          (message (format "Waiting a git fetch from %s to complete..."
                           magit-gerrit-remote))
          (magit-process-wait)
          (message (format "Checking out refs %s to %s in %s" ref branch dir))
          (magit-gerrit-create-branch-force branch "FETCH_HEAD"))))))

(defun magit-gerrit-download-patchsets ()
  "Download All Gerrit Review Patchset"
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (let ((refs (mapcar (lambda (patchSet)
                            (list (cdr (assoc 'number patchSet))
                                  (aref (cdr (assoc 'parents patchSet)) 0)
                                  (cdr (assoc 'ref patchSet))))
                          (cdr (assoc 'patchSets jobj)))))
        (dolist (ref refs)
          (let ((magit-custom-options nil)
                (branch (format "review/%s/%s/%s"
                                (cdr (assoc 'username (assoc 'owner jobj)))
                                (cdr (or (assoc 'topic jobj) (assoc 'number jobj)))
                                (car ref))))
            (magit-gerrit-create-branch-force branch (nth 1 ref)))
          (let* ((magit-custom-options (nth 2 ref))
                 (magit-this-process
                  (magit-pull magit-gerrit-remote magit-custom-options)))
            (magit-process-wait)))))))

(defun magit-gerrit-browse-review ()
  "Browse the Gerrit Review with a browser."
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (if jobj
	(browse-url (cdr (assoc 'url jobj))))))

(defun magit-gerrit-copy-review (with-commit-message)
  "Copy review url and commit message."
  (let ((jobj (magit-gerrit-review-at-point)))
    (if jobj
        (kill-new
         (concat
          (cdr (assoc 'url jobj))
          (if with-commit-message
              (concat " " (car (split-string (cdr (assoc 'commitMessage jobj)) "\n" t)))))))))

(defun magit-gerrit-copy-review-url ()
  "Copy review url only"
  (interactive)
  (magit-gerrit-copy-review nil))

(defun magit-gerrit-copy-review-url-commit-message ()
  "Copy review url with commit message"
  (interactive)
  (magit-gerrit-copy-review t))

(defun magit-insert-gerrit-reviews ()
  (magit-gerrit-section 'gerrit-reviews
			"Reviews:" 'magit-gerrit-wash-reviews
			(gerrit-query (magit-gerrit-get-project))))

(defun magit-gerrit-add-reviewer ()
  (interactive)
  "ssh -x -p 29418 user@gerrit gerrit set-reviewers --project toplvlroot/prjname --add email@addr"

  (gerrit-ssh-cmd "set-reviewers"
		  "--project" (magit-gerrit-get-project)
		  "--add" (read-string "Reviewer Name/Email: ")
		  (cdr-safe (assoc 'id (magit-gerrit-review-at-point)))))

(defun magit-gerrit-popup-args (&optional something)
  (or (magit-gerrit-arguments) (list "")))

(defun magit-gerrit-verify-review (args)
  "Verify a Gerrit Review"
  (interactive (magit-gerrit-popup-args))

  (let ((score (completing-read "Score: "
				    '("-2" "-1" "0" "+1" "+2")
				    nil t
				    "+1"))
	(rev (cdr-safe (assoc
		      'revision
		      (cdr-safe (assoc 'currentPatchSet
				       (magit-gerrit-review-at-point))))))
	(prj (magit-gerrit-get-project)))
    (gerrit-review-verify prj rev score args)
    (magit-refresh)))

(defun magit-gerrit-code-review (args)
  "Perform a Gerrit Code Review"
  (interactive (magit-gerrit-popup-args))
  (let ((score (completing-read "Score: "
				    '("-2" "-1" "0" "+1" "+2")
				    nil t
				    "+1"))
	(rev (cdr-safe (assoc
		      'revision
		      (cdr-safe (assoc 'currentPatchSet
				       (magit-gerrit-review-at-point))))))
	(prj (magit-gerrit-get-project)))
    (gerrit-code-review prj rev score args)
    (magit-refresh)))

(defun magit-gerrit-submit-review (args)
  "Submit a Gerrit Code Review"
  ;; "ssh -x -p 29418 user@gerrit gerrit review REVISION  -- --project PRJ --submit "
  (interactive (magit-gerrit-popup-args))
  (gerrit-ssh-cmd "review"
		  (cdr-safe (assoc
			     'revision
			     (cdr-safe (assoc 'currentPatchSet
					      (magit-gerrit-review-at-point)))))
		  "--project"
		  (magit-gerrit-get-project)
		  "--submit"
		  args)
  (let* ((branch (or (magit-get-current-branch)
		     (error "Don't push a detached head.  That's gross")))
	 (branch-remote (and branch (magit-get "branch" branch "remote"))))
    (magit-fetch-current branch-remote)))

(defun magit-gerrit-push-review (status use-pub-branch topic)
  (let* ((branch (or (magit-get-current-branch)
		     (error "Don't push a detached head.  That's gross")))
	 (commitid (or (when (eq (magit-section-type (magit-current-section))
				 'commit)
			 (magit-section-value (magit-current-section)))
		       (error "Couldn't find a commit at point")))
	 (rev (magit-rev-parse (or commitid
				   (error "Select a commit for review"))))

	 (branch-remote (and branch (magit-get "branch" branch "remote"))))

    ;; (message "Args: %s "
    ;;	     (concat rev ":" branch-pub))

    (let* ((branch-merge (if (string= branch-remote ".")
			     (magit-gerrit-query-remote-branch-merge)
			   (and branch (magit-get "branch" branch "merge"))))
	   (branch-pub (or
			(and use-pub-branch (format "refs/%s/%s" status use-pub-branch))
			branch-merge)))


      (when (string= branch-remote ".")
	(setq branch-remote magit-gerrit-remote))

      (magit-run-git-async "push" "-v" branch-remote
			   (concat rev ":" branch-pub)))))

(defun magit-gerrit-create-review (&rest args)
  (interactive (magit-gerrit-push-review-arguments))

  (let ((rb (car-safe
	     (delete-if 'null
			(mapcar (lambda (k) (and
					     (string-match (rx "rb:" (group (zero-or-more any))) k)
					     (match-string 1 k)))
				args))))
	(tp (car-safe
	     (delete-if 'null
			(mapcar (lambda (k) (and
					     (string-match (rx "tp:" (group (zero-or-more any))) k)
					     (match-string 1 k)))
				args)))))

    (magit-gerrit-push-review 'publish rb tp)))

(defun magit-gerrit-review-is-draft ()
  (string= (cdr-safe (assoc 'status (magit-gerrit-review-at-point))) "DRAFT"))

(defun magit-gerrit-create-draft (&rest args)
  (interactive (magit-gerrit-push-review-arguments))

  (let ((rb (car-safe
	     (delete-if 'null
			(mapcar (lambda (k) (and
					     (string-match (rx "rb:" (group (zero-or-more any))) k)
					     (match-string 1 k)))
				args))))
	(tp (car-safe
	     (delete-if 'null
			(mapcar (lambda (k) (and
					     (string-match (rx "tp:" (group (zero-or-more any))) k)
					     (match-string 1 k)))
				args)))))
    (magit-gerrit-push-review 'drafts rb tp)))

(defun magit-gerrit-publish-draft ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
	(id (cdr-safe (assoc 'id
			     (magit-gerrit-review-at-point))))
	(rev (cdr-safe (assoc
			'revision
			(cdr-safe (assoc 'currentPatchSet
					 (magit-gerrit-review-at-point)))))))
    (gerrit-ssh-cmd "review" "--project" prj "--publish" rev))
  (magit-refresh))

(defun magit-gerrit-delete-draft ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
	(id (cdr-safe (assoc 'id
		     (magit-gerrit-review-at-point))))
	(rev (cdr-safe (assoc
			'revision
			(cdr-safe (assoc 'currentPatchSet
					 (magit-gerrit-review-at-point)))))))
    (gerrit-ssh-cmd "review" "--project" prj "--delete" rev))
  (magit-refresh))

(defun magit-gerrit-abandon-review ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
	(id (cdr-safe (assoc 'id
		     (magit-gerrit-review-at-point))))
	(rev (cdr-safe (assoc
			'revision
			(cdr-safe (assoc 'currentPatchSet
					 (magit-gerrit-review-at-point)))))))
    ;; (message "Prj: %s Rev: %s Id: %s" prj rev id)
    (gerrit-review-abandon prj rev)
    (magit-refresh)))

(defun magit-gerrit-read-comment (&rest args)
  (format "\'\"%s\"\'"
	  (read-from-minibuffer "Message: ")))

(defun magit-gerrit-create-branch (branch parent))

(magit-define-popup magit-gerrit-copy-review-popup
  "Popup console for copy review to clipboard."
  'magit-gerrit
  :actions '((?C "url and commit message" magit-gerrit-copy-review-url-commit-message)
	     (?c "url only" magit-gerrit-copy-review-url)))

(magit-define-popup magit-gerrit-patchset-popup
  "Popup console for download and view patchset."
  'magit-gerrit
  :actions '((?v "view current patchset diff" magit-gerrit-view-current-patchset-diff)
             (?d "download current patchset" magit-gerrit-download-current-patchset)
             (?D "download patchsets" magit-gerrit-download-patchsets)))

(magit-define-popup magit-gerrit-push-review-popup
  "Popup console for pushing reviews to Gerrit"
  'magit-gerrit
  :actions '((?P "Push"                              magit-gerrit-create-review)
	     (?D "Push Commit For Draft Review"      magit-gerrit-create-draft))
  :options '((?t "Topic"           ""       read-from-minibuffer)
	     (?b "Remote Branch"   ""       read-from-minibuffer)))

(defun magit-gerrit-build-push-popup ()
  "Fill in appropriate values for remote branch and topic and then show the push review popup"
  (interactive)
  (let* ((branch (magit-get-current-branch))
	 (branch-merge (magit-get "branch" branch "merge")))

    (magit-define-popup-action 'magit-gerrit-push-review-popup ?P "Push" 'magit-gerrit-create-review)
    (magit-define-popup-action 'magit-gerrit-push-review-popup ?D "Push Draft Review" 'magit-gerrit-create-draft)

    (magit-define-popup-option 'magit-gerrit-push-review-popup ?b "Remote Branch"
      "rb:"
      #'(lambda (&rest args)
	  (interactive)
	  (magit-gerrit-query-remote-branch-merge))
      branch-merge)

    (magit-define-popup-option 'magit-gerrit-push-review-popup ?t "Topic"
      "tp:"
      #'(lambda (&rest args)
	  (interactive)
	  (read-from-minibuffer "Topic: "))
      branch)

    (setq magit-gerrit-push-review-arguments nil)

    (when magit-gerrit-use-topics
      (setq magit-gerrit-push-review-arguments (list (concat "tp:" branch))))

    (setq magit-gerrit-push-review-arguments
	  (append magit-gerrit-push-review-arguments
		  (list (concat "rb:" (magit-gerrit-convert-ref branch-merge "refs/heads/"))))))

  (magit-gerrit-push-review-popup))

(defun magit-gerrit-build-review-popup ()
  (interactive)

  (magit-remove-popup-key 'magit-gerrit-review-popup :action ?P)
  (magit-remove-popup-key 'magit-gerrit-review-popup :action ?B)
  (magit-define-popup-action 'magit-gerrit-review-popup ?B "Abandon Review" 'magit-gerrit-abandon-review)

  (when (magit-gerrit-review-is-draft)
    (magit-define-popup-action 'magit-gerrit-review-popup ?P "Publish Draft" 'magit-gerrit-publish-draft)
    (magit-define-popup-action 'magit-gerrit-review-popup ?B "Delete Draft" 'magit-gerrit-delete-draft))

  (magit-gerrit-review-popup))


(defun magit-gerrit-build-popup ()
  "Display a custom gerrit popup based on current cursor location"
  (interactive)

  (let ((sectype (magit-section-type (magit-current-section))))
    (cond ((eq sectype 'commit)
	   (magit-gerrit-build-push-popup))

	  ((eq sectype 'review)
	   (magit-gerrit-build-review-popup))

	  (t
	   (magit-gerrit-popup)))))

(magit-define-popup magit-gerrit-review-popup
  "Popup console for manipulating gerrit reviews"
  'magit-gerrit
  :actions '((?A "Add Reviewer"                                    magit-gerrit-add-reviewer)
	     (?V "Verify"                                          magit-gerrit-verify-review)
	     (?C "Code Review"                                     magit-gerrit-code-review)
	     (?c "Copy Review"                                     magit-gerrit-copy-review-popup)
	     (?p "Patchset"                                        magit-gerrit-patchset-popup)
	     (?S "Submit Review"                                   magit-gerrit-submit-review)
	     (?B "Abandon Review"                                  magit-gerrit-abandon-review)
	     (?b "Browse Review"                                   magit-gerrit-browse-review))
  :options '((?m "Comment"                      "--message "       magit-gerrit-read-comment)))

(magit-define-popup magit-gerrit-popup
  "Popup console for magit gerrit commands."
  'magit-gerrit
  :actions '((?P "Push Commit For Review"                          magit-gerrit-build-push-popup)

	     (?p "Publish Draft Patchset"                          magit-gerrit-publish-draft)
	     (?k "Delete Draft"                                    magit-gerrit-delete-draft)
	     (?A "Add Reviewer"                                    magit-gerrit-add-reviewer)
	     (?V "Verify"                                          magit-gerrit-verify-review)
	     (?C "Code Review"                                     magit-gerrit-code-review)
	     (?c "Copy Review"                                     magit-gerrit-copy-review-popup)
	     (?p "Patchset"                                        magit-gerrit-patchset-popup)
	     (?S "Submit Review"                                   magit-gerrit-submit-review)
	     (?B "Abandon Review"                                  magit-gerrit-abandon-review)
	     (?b "Browse Review"                                   magit-gerrit-browse-review))
  :options '((?m "Comment"                      "--message "       magit-gerrit-read-comment)))

;; Attach Magit Gerrit to Magit's default help popup
(magit-define-popup-action 'magit-dispatch-popup ?R "Gerrit"
  'magit-gerrit-build-popup)

(magit-define-popup-action 'magit-gerrit-popup ?P "Push Review"
  'magit-gerrit-build-push-review-popup)

(defvar magit-gerrit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") 'magit-gerrit-build-popup)
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

(defun magit-gerrit-detect-ssh-creds (remote-url)
  "Derive magit-gerrit-ssh-creds from remote-url.
Assumes remote-url is a gerrit repo if scheme is ssh
and port is the default gerrit ssh port."
  (let ((url (url-generic-parse-url remote-url)))
    (when (and (string= "ssh" (url-type url))
	       (eq 29418 (url-port url)))
      (set (make-local-variable 'magit-gerrit-ssh-creds)
	   (format "%s@%s" (url-user url) (url-host url)))
      (message "Detected magit-gerrit-ssh-creds=%s" magit-gerrit-ssh-creds))))

(defun magit-gerrit-check-enable ()
  (let ((remote-url (magit-gerrit-get-remote-url)))
    (when (and remote-url
	       (or magit-gerrit-ssh-creds
		   (magit-gerrit-detect-ssh-creds remote-url))
	       (string-match magit-gerrit-ssh-creds remote-url))
     (magit-gerrit-mode t))))

;; Hack in dir-local variables that might be set for magit gerrit
(add-hook 'magit-status-mode-hook #'hack-dir-local-variables-non-file-buffer t)

;; Try to auto enable magit-gerrit in the magit-status buffer
(add-hook 'magit-status-mode-hook #'magit-gerrit-check-enable t)
(add-hook 'magit-log-mode-hook #'magit-gerrit-check-enable t)

(provide 'magit-gerrit)

;;; magit-gerrit.el ends here
