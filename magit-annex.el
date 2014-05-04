;;; magit-annex.el --- Use git annex within magit

;; Copyright (C) 2013 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;;         Rémi Vanicat <vanicat@debian.org>
;; URL: https://github.com/kyleam/magit-annex
;; Keywords: magit git-annex
;; Version: 0.10.0
;; Package-Requires: ((cl-lib "0.3") (magit "1.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; magit-annex adds a few git annex operations to the magit interface.
;; Most features are present under the annex popup menu, which is bound
;; to "@". This key was chosen as a leading key mostly to be consistent
;; with John Wiegley's git-annex.el [1], but also because there aren't
;; too many single letters available in the magit keymap.
;;
;; Adding files:
;;   @a   Add a file in the status buffer to annex.
;;        Behaves similarly to staging a file with 's'.
;;   @A   Add all untracked files and unstaged changes to annex.
;;        Behaves similarly to staging all files with 'S'.
;;
;; Managing file content:
;;   @fu   Unlock a file.
;;   @fl   Lock a file.
;;   @fg   Get a file.
;;   @fd   Drop a file.
;;   @fc   Copy a file.
;;   @fm   Move a file.
;;
;;   @eg   Get all files.
;;   @ed   Drop all files.
;;   @ec   Copy all files.
;;   @em   Move all files.
;;
;; Updating git annex:
;;   @m   Run `git annex merge'.
;;   @Pg  Push git annex branch.
;;   @Pb  Push current and git annex branch.
;;   @y   Run `git annex sync'.
;;
;; For working with git annex in dired, see git-annex.el [1].
;;
;; [1] https://github.com/jwiegley/git-annex-el

;;; Code:

(require 'cl-lib)
(require 'magit)


;;; Variables

(defgroup magit-annex nil
  "Use git annex within magit"
  :prefix "magit-annex"
  :group 'magit)

(defcustom magit-annex-add-all-confirm t
  "Whether to require confirmation before adding all changes to annex"
  :group 'magit-annex
  :type 'boolean)

(defcustom magit-annex-standard-options nil
  "Call git annex with these options.
These are placed after \"annex\" in the call, whereas values from
`magit-git-standard-options' are placed after \"git\"."
  :group 'magit-annex
  :type '(repeat string))

(defcustom magit-annex-limit-file-choices t
  "Limit choices for file commands based on state of repo.
For example, if locking a file, limit choices to unlocked files."
  :group 'magit-annex
  :type 'boolean)


;;; Popups

(defvar magit-annex-key-mode-group-dispatch
  '(annex-dispatch
    (man-page "git-annex")
    (actions
     ("a" "Add" magit-annex-add)
     ("@" "Add" magit-annex-add)
     ("A" "Add all" magit-annex-add-all)
     ("f" "Action on file" magit-key-mode-popup-annex-file-action)
     ("e" "Action on every file" magit-key-mode-popup-annex-all-action)
     ("G" "Get all (auto)" magit-annex-get-all-auto)
     ("y" "Sync" magit-key-mode-popup-annex-syncing)
     ("m" "Merge" magit-annex-merge)
     ("P" "Pushing" magit-key-mode-popup-annex-pushing)
     (":" "Annex subcommand (from pwd)" magit-annex-command)
     ("!" "Running" magit-key-mode-popup-annex-running))))
(add-to-list 'magit-key-mode-groups magit-annex-key-mode-group-dispatch)
(magit-key-mode-generate 'annex-dispatch)

(define-key magit-mode-map "@" 'magit-key-mode-popup-annex-dispatch)
(magit-key-mode-insert-action 'dispatch
                              "@" "Annex" 'magit-key-mode-popup-annex-dispatch)

(defvar magit-annex-key-mode-group-file-action
  '(annex-file-action
    (man-page "git-annex")
    (actions
     ("u" "Unlock file" magit-annex-unlock-file)
     ("l" "Lock file" magit-annex-lock-file)
     ("g" "Get file" magit-annex-get-file)
     ("d" "Drop file" magit-annex-drop-file)
     ("c" "Copy file" magit-annex-copy-file)
     ("m" "Move file" magit-annex-move-file))
    (switches
     ("-f" "Fast" "--fast")
     ("-F" "Force" "--force")
     ("-a" "Auto" "--auto"))
    (arguments
     ("=t" "To remote" "--to=" magit-read-remote)
     ("=f" "From remote" "--from=" magit-read-remote)
     ("=n" "Number of copies" "--numcopies=" read-from-minibuffer))))
(add-to-list 'magit-key-mode-groups magit-annex-key-mode-group-file-action)
(magit-key-mode-generate 'annex-file-action)

(defvar magit-annex-key-mode-group-all-action
  '(annex-all-action
    (man-page "git-annex")
    (actions
     ("g" "Get all" magit-annex-get-all)
     ("d" "Drop all" magit-annex-drop-all)
     ("c" "Copy all" magit-annex-copy-all)
     ("m" "Move all" magit-annex-move-all))
    (switches
     ("-f" "Fast" "--fast")
     ("-F" "Force" "--force")
     ("-a" "Auto" "--auto"))
    (arguments
     ("=t" "To remote" "--to=" magit-read-remote)
     ("=f" "From remote" "--from=" magit-read-remote)
     ("=n" "Number of copies" "--numcopies=" read-from-minibuffer))))
(add-to-list 'magit-key-mode-groups magit-annex-key-mode-group-all-action)
(magit-key-mode-generate 'annex-all-action)

(defvar magit-annex-key-mode-group-syncing
  '(annex-syncing
    (man-page "git-annex")
    (actions
     ("y" "Sync" magit-annex-sync)
     ("r" "Sync remote" magit-annex-sync-remote))
    (switches
     ("-c" "Content" "--content")
     ("-f" "Fast" "--fast")
     ("-F" "Force" "--force"))))
(add-to-list 'magit-key-mode-groups magit-annex-key-mode-group-syncing)
(magit-key-mode-generate 'annex-syncing)

(defvar magit-annex-key-mode-group-pushing
  '(annex-pushing
    (man-page "git-annex")
    (actions
     ("g" "Push git annex" magit-annex-push)
     ("b" "Push current and git annex" magit-annex-push-both))
    (switches
     ("-f" "Force" "--force")
     ("-d" "Dry run" "-n")
     ("-u" "Set upstream" "-u"))))
(add-to-list 'magit-key-mode-groups magit-annex-key-mode-group-pushing)
(magit-key-mode-generate 'annex-pushing)

(defvar magit-annex-key-mode-group-running
  '(annex-running
    (man-page "git-annex")
    (actions
     (":" "Annex subcommand (from pwd)" magit-annex-command)
     ("!" "Annex subcommand (from root)" magit-annex-command-topdir))))
(add-to-list 'magit-key-mode-groups magit-annex-key-mode-group-running)
(magit-key-mode-generate 'annex-running)


;;; Process calls

(defun magit-annex-run (&rest args)
  (apply #'magit-run-git "annex"
         (append magit-annex-standard-options args)))

(defun magit-annex-run-async (&rest args)
  (apply #'magit-run-git-async "annex"
         (append magit-annex-standard-options args)))

(defun magit-annex-command ()
  "Execute a git annex subcommand asynchronously, displaying the output.
With a prefix argument, run git annex from repository root."
  (interactive)
  (let ((args (magit-annex-command-read-args)))
    (apply #'magit-git-command args)))

(defun magit-annex-command-topdir ()
  "Execute a git annex subcommand asynchronously, displaying the output.
Run git annex in the root of the current repository."
  (interactive)
  (let ((args (magit-annex-command-read-args t)))
    (apply #'magit-git-command args)))

(defun magit-annex-command-read-args (&optional root)
  ;; Modified from `magit-git-command-read-args'.
  (let ((dir (if (or root current-prefix-arg)
                 (or (magit-get-top-dir)
                     (user-error "Not inside a Git repository"))
               default-directory)))
    (list (concat "annex " (read-string (format "Git annex subcommand (in %s): "
                                                (abbreviate-file-name dir))
                                        nil 'magit-git-command-history))
          dir)))


;;; Annexing

(defun magit-annex-add (&optional file)
  "Add the item at point to annex.
With a prefix argument, prompt for a file.
\('git annex add')"
  ;; Modified from `magit-stage-item'.
  (interactive
   (when current-prefix-arg
     (list (file-relative-name
            (read-file-name "File to add to annex: " nil nil t)
                               (magit-get-top-dir)))))
  (if file
      (magit-annex-run "add" file)
    (magit-section-action stage (info)
      ([file untracked]
       (magit-annex-run "add"
                      (if (use-region-p)
                          (magit-section-region-siblings #'magit-section-info)
                        info)))
      (untracked
       (magit-annex-run "add" (magit-git-lines "ls-files" "--other"
                                               "--exclude-standard")))
      ([diff unstaged]
       (magit-annex-run "add"
                        (if (use-region-p)
                            (magit-section-region-siblings #'magit-section-info)
                          info)))
      (unstaged
       (magit-annex-run "add" (magit-annex-unlocked-files)))
      ([* staged]
       (user-error "Already added to annex")))))

(defun magit-annex-add-all ()
  "Add all remaining changes in tracked files to staging area.
\('git annex add .')"
  ;; Modified from `magit-stage-all'.
  (interactive)
  (when (or (not magit-annex-add-all-confirm)
            (not (magit-anything-staged-p))
            (yes-or-no-p "Add all changes to annex?"))
    (magit-annex-run "add" ".")))


;;; Updating

(defun magit-annex-sync ()
  "Sync git-annex.
\('git annex sync')"
  (interactive)
  (magit-annex-run-async "sync" magit-custom-options))

(defun magit-annex-sync-remote (remote)
  "Sync git-annex with REMOTE.
\('git annex sync REMOTE')"
  (interactive (list (magit-read-remote "Remote")))
  (magit-annex-run-async "sync" magit-custom-options remote))

(defun magit-annex-merge ()
  "Merge git annex.
\('git annex merge')"
  (interactive)
  (magit-annex-run "merge"))

(defun magit-annex-push (arg)
  "Push git annex branch to a remote repository.
This behaves similarly to `magit-push-dwim' but always pushes the
branch \"git-annex\"."
  ;; Modified from `magit-push-tags' and `magit-push-dwim'.
  (interactive "P")
  (let* ((branch  "git-annex")
         (auto-remote (magit-get-remote branch))
         (used-remote (if (or arg (not auto-remote))
                          (magit-read-remote
                           (format "Push %s to remote" branch) auto-remote)
                        auto-remote)))
    (cond
     ((equal auto-remote used-remote))
     ((member "-u" magit-custom-options))
     ((>= (prefix-numeric-value arg) 16)
      (and (yes-or-no-p "Set upstream while pushing? ")
           (setq magit-custom-options
                 (cons "-u" magit-custom-options))))
     ((eq magit-set-upstream-on-push 'refuse)
      (user-error "Not pushing since no upstream has been set."))
     ((or (eq magit-set-upstream-on-push 'dontask)
          (and (eq magit-set-upstream-on-push t)
               (yes-or-no-p "Set upstream while pushing? ")))
      (setq magit-custom-options (cons "-u" magit-custom-options))))
    (magit-run-git-async "push" "-v"
                         used-remote branch magit-custom-options)))

(defun magit-annex-push-both (arg)
  "Push current branch and git annex branch to a remote repository."
  (interactive "P")
  (magit-push-dwim arg)
  (magit-process-wait)
  (magit-annex-push arg))


;;; Managing content

(defun magit-annex-get-all-auto ()
  "run git annex get --auto to get all needed files"
  (interactive)
  (magit-annex-run-async "get" "--auto"))

(defmacro magit-annex-all-action (command)
  `(defun ,(intern (concat "magit-annex-" command "-all")) ()
     (interactive)
     (magit-annex-run-async ,command magit-custom-options)))

(magit-annex-all-action "get")
(magit-annex-all-action "drop")
(magit-annex-all-action "move")
(magit-annex-all-action "copy")

(defmacro magit-annex-file-action (command file-read-func)
  `(defun ,(intern (concat "magit-annex-" command "-file")) (file)
     (interactive (list (funcall ,file-read-func
                                 ,(format "File to %s" command))))
     (setq file (expand-file-name file))
     (let ((default-directory (file-name-directory file)))
       (magit-annex-run-async ,command
                              magit-custom-options
                              (file-name-nondirectory file)))))

(magit-annex-file-action "get" 'magit-annex-read-absent-file)
(magit-annex-file-action "drop" 'magit-annex-read-present-file-unless-from)
(magit-annex-file-action "copy" 'magit-annex-read-present-file-unless-from)
(magit-annex-file-action "move" 'magit-annex-read-present-file-unless-from)
(magit-annex-file-action "unlock" 'magit-annex-read-present-file)
(magit-annex-file-action "lock" 'magit-annex-read-unlocked-file)

(defun magit-annex-read-annex-file (prompt)
  (magit-annex-completing-file-read prompt 'magit-annex-files))

(defun magit-annex-read-present-file (prompt)
  (magit-annex-completing-file-read prompt 'magit-annex-present-files))

(defun magit-annex-read-present-file-unless-from (prompt)
  (magit-annex-completing-file-read prompt 'magit-annex-present-files t))

(defun magit-annex-read-absent-file (prompt)
  (magit-annex-completing-file-read prompt 'magit-annex-absent-files))

(defun magit-annex-read-unlocked-file (prompt)
  (magit-annex-completing-file-read prompt 'magit-annex-unlocked-files))

(defun magit-annex-completing-file-read (prompt collector &optional no-from)
  "Read an annex file name.
If `magit-annex-limit-file-choices' is non-nil,
`magit-completing-read' will be called with PROMPT and the result
of the function COLLECTOR. Otherwise, `read-file-name' will be
called with PROMPT.

PROMPT should not contain a colon and trailing space because
`magit-completing-read' appends these. If PROMPT is passed to
`read-file-name', these will be added.

A non-nil value for NO-FROM indicates that all annex files should
be used, instead of the results from COLLECTOR, if the \"--from\"
argument is used. This is appropriate for commands like \"drop\",
where \"--from\" specifies to operate on a remote, making the
local state of the annex files irrelevant."
  (let ((non-magit-prompt (concat prompt ": ")))
    (if (not magit-annex-limit-file-choices)
        (read-file-name non-magit-prompt nil nil t)
      (let* ((collector
              (if (and no-from (magit-annex-from-in-options-p))
                  (function magit-annex-files)
                collector))
             (collection (funcall collector)))
        (magit-completing-read prompt collection)))))

(defun magit-annex-from-in-options-p ()
  (cl-some '(lambda (it) (string-match "--from=" it)) magit-custom-options))

(defun magit-annex-files ()
  (magit-git-lines "annex" "find" "--include" "*"))

(defun magit-annex-present-files ()
  (magit-git-lines "annex" "find"))

(defun magit-annex-absent-files ()
  (magit-git-lines "annex" "find" "--not" "--in=here"))

(defun magit-annex-unlocked-files ()
  (magit-git-lines "diff-files" "--diff-filter=T" "--name-only"))

(provide 'magit-annex)

;; magit-annex.el ends here
