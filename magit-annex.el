;;; magit-annex.el --- Use git annex within magit

;; Copyright (C) 2013 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;;         Rémi Vanicat <vanicat@debian.org>
;; URL: https://github.com/kyleam/magit-annex
;; Keywords: vc tools
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
;; with John Wiegley's git-annex.el (which provides a dired interface to
;; git annex) [1].
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
;;   @u    Browse unused files.
;;
;; Updating git annex:
;;   @m   Run `git annex merge'.
;;   @Pg  Push git annex branch.
;;   @Pb  Push current and git annex branch.
;;   @y   Run `git annex sync'.
;;
;;
;; When magit-annex is installed from MELPA, no additional setup is
;; needed. The magit-annex popup menu will be added under the main Magit
;; popup menu (and loading of magit-annex will be deferred until the
;; first time the magit-annex popup is called).
;;
;; To use magit-annex from the source repository, put
;;
;;   (require 'magit-annex)
;;
;; in your initialization file.
;;
;;
;; [1] https://github.com/jwiegley/git-annex-el

;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'magit-popup)


;;; Variables

(defgroup magit-annex nil
  "Use git annex within magit"
  :prefix "magit-annex"
  :group 'magit-extensions)

(defcustom magit-annex-add-all-confirm t
  "Whether to require confirmation before adding all changes to annex."
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

(magit-define-popup magit-annex-popup
  "Popup console for git annex commands."
  'magit-popups
  :man-page "git-annex"
  :actions  '((?a "Add" magit-annex-add)
              (?@ "Add" magit-annex-add)
              (?A "Add all" magit-annex-add-all)
              (?f "Action on file" magit-annex-file-action-popup)
              (?e "Action on every file" magit-annex-all-action-popup)
              (?G "Get all (auto)" magit-annex-get-all-auto)
              (?y "Sync" magit-annex-sync-popup)
              (?m "Merge" magit-annex-merge)
              (?P "Pushing" magit-annex-pushing-popup)
              (?u "Unused" magit-annex-unused)
              (?: "Annex subcommand (from pwd)" magit-annex-command)
              (?! "Running" magit-annex-run-popup)))

(magit-define-popup magit-annex-file-action-popup
  "Popup console for git annex file commands."
  'magit-annex-popups
  :man-page "git-annex"
  :actions  '((?g "Get file" magit-annex-get-file)
              (?d "Drop file" magit-annex-drop-file)
              (?c "Copy file" magit-annex-copy-file)
              (?m "Move file" magit-annex-move-file)
              (?l "Lock file" magit-annex-lock-file)
              (?u "Unlock file" magit-annex-unlock-file))
  :switches '((?f "Fast" "--fast")
              (?F "Force" "--force")
              (?a "Auto" "--auto"))
  :options  '((?t "To remote" "--to=" magit-read-remote)
              (?f "From remote" "--from=" magit-read-remote)
              (?n "Number of copies" "--numcopies=" read-from-minibuffer)))

(magit-define-popup magit-annex-all-action-popup
  "Popup console for git annex content commands for all files."
  'magit-annex-popups
  :man-page "git-annex"
  :actions  '((?g "Get" magit-annex-get-all)
              (?d "Drop" magit-annex-drop-all)
              (?c "Copy" magit-annex-copy-all)
              (?m "Move" magit-annex-move-all))
  :switches '((?f "Fast" "--fast")
              (?F "Force" "--force")
              (?a "Auto" "--auto"))
  :options  '((?t "To remote" "--to=" magit-read-remote)
              (?f "From remote" "--from=" magit-read-remote)
              (?n "Number of copies" "--numcopies=" read-from-minibuffer))
  :default-arguments '("--auto"))

(magit-define-popup magit-annex-sync-popup
  "Popup console for git annex sync."
  'magit-annex-popups
  :man-page "git-annex"
  :actions  '((?y "Sync" magit-annex-sync)
              (?r "Sync remote" magit-annex-sync-remote))
  :switches '((?c "Content" "--content")
              (?f "Fast" "--fast")
              (?F "Force" "--force")))

(magit-define-popup magit-annex-pushing-popup
  "Popup console for git annex pushing."
  'magit-annex-popups
  :man-page "git-annex"
  :actions  '((?g "Push git annex" magit-annex-push)
              (?b "Push current and git annex" magit-annex-push-both))
  :switches '((?f "Force" "--force")
              (?h "Disable hooks" "--no-verify")
              (?d "Dry run" "-n")
              (?u "Set upstream" "-u")))

(magit-define-popup magit-annex-run-popup
  "Popup console for running git annex commands."
  'magit-annex-popups
  :man-page "git-annex"
  :actions '((?! "Annex subcommand (from root)" magit-annex-command-topdir)
             (?: "Annex subcommand (from pwd)" magit-annex-command)))

;;;###autoload
(eval-after-load 'magit
  '(progn
     (define-key magit-mode-map "@" 'magit-annex-popup-or-init)
     (magit-define-popup-action 'magit-dispatch-popup
       ?@ "Annex" 'magit-annex-popup-or-init)))



;;; Process calls

(defun magit-annex-run (&rest args)
  "Call git annex synchronously in a separate process, and refresh.

Before ARGS are passed to git annex,
`magit-annex-standard-options' will be prepended.

See `magit-run-git' for more details on the git call."
  (apply #'magit-run-git "annex"
         (append magit-annex-standard-options args)))

(defun magit-annex-run-async (&rest args)
  "Call git annex asynchronously with ARGS.
See `magit-annex-run' and `magit-run-git-async' for more
information."
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


;;; Initialization

;;;###autoload
(defun magit-annex-popup-or-init ()
  "Call magit annex popup or offer to initialize non-annex repo."
  (interactive)
  (cond
   ((magit-annex-inside-annexdir-p)
    (magit-annex-popup))
   ((y-or-n-p (format "No git annex repository in %s. Initialize one? "
                      default-directory))
    (call-interactively 'magit-annex-init))))

;;;###autoload
(defun magit-annex-init (&optional description)
  "Initialize git annex repository.
\('git annex init [DESCRIPTION]')"
  (interactive "sDescription: ")
  (magit-annex-run "init" description))

(defun magit-annex-inside-annexdir-p ()
  (file-exists-p (concat (magit-git-dir) "annex")))


;;; Annexing

(defun magit-annex-add (&optional file)
  "Add the item at point to annex.
With a prefix argument, prompt for FILE.
\('git annex add')"
  ;; Modified from `magit-stage'.
  (interactive
   (when current-prefix-arg
     (list (magit-completing-read "Add file"
                                  (nconc (magit-annex-unlocked-files)
                                         (magit-untracked-files))))))
  (if file
      (magit-annex-run "add" file)
    (magit-section-case
      ([file untracked]
       (magit-annex-run "add"
                      (if (use-region-p)
                          (magit-section-region-siblings #'magit-section-value)
                        (magit-section-value it))))
      (untracked
       (magit-annex-run "add" (magit-untracked-files)))
      ([file unstaged]
       (magit-annex-run "add"
                        (if (use-region-p)
                            (magit-section-region-siblings #'magit-section-value)
                          (magit-section-value it))))
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
  (magit-annex-run-async "sync" magit-current-popup-args))

(defun magit-annex-sync-remote (remote)
  "Sync git-annex with REMOTE.
\('git annex sync REMOTE')"
  (interactive (list (magit-read-remote "Remote")))
  (magit-annex-run-async "sync" magit-current-popup-args remote))

(defun magit-annex-merge ()
  "Merge git annex.
\('git annex merge')"
  (interactive)
  (magit-annex-run "merge"))

(defun magit-annex-push (arg)
  "Push git annex branch to a remote repository.
This behaves similarly to `magit-push' (including the meaning of
ARG) but always pushes the branch \"git-annex\"."
  ;; Modified from `magit-push-tags' and `magit-push'.
  (interactive "P")
  (let* ((branch  "git-annex")
         (auto-remote (magit-get "branch" branch "remote"))
         (used-remote (if (or arg (not auto-remote))
                          (magit-read-remote
                           (format "Push %s to remote" branch) auto-remote)
                        auto-remote)))
    (cond
     ((equal auto-remote used-remote))
     ((member "-u" magit-current-popup-args))
     ((>= (prefix-numeric-value arg) 16)
      (and (yes-or-no-p "Set upstream while pushing? ")
           (setq magit-current-popup-args
                 (cons "-u" magit-current-popup-args))))
     ((eq magit-set-upstream-on-push 'refuse)
      (user-error "Not pushing since no upstream has been set"))
     ((or (eq magit-set-upstream-on-push 'dontask)
          (and (eq magit-set-upstream-on-push t)
               (yes-or-no-p "Set upstream while pushing? ")))
      (setq magit-current-popup-args (cons "-u" magit-current-popup-args))))
    (magit-run-git-async "push" "-v"
                         used-remote branch magit-current-popup-args)))

(defun magit-annex-push-both (arg)
  "Push current branch and git annex branch to a remote repository.
ARG is interpreted as in `magit-push'."
  (interactive "P")
  (magit-push arg)
  (magit-process-wait)
  (magit-annex-push arg))


;;; Managing content

(defun magit-annex-get-all-auto ()
  "Run `git annex get --auto'."
  (interactive)
  (magit-annex-run-async "get" "--auto"))

(defmacro magit-annex-all-action (command)
  `(defun ,(intern (concat "magit-annex-" command "-all")) ()
     ,(format "Run `git annex %s'." command)
     (interactive)
     (magit-annex-run-async ,command magit-current-popup-args)))

(magit-annex-all-action "get")
(magit-annex-all-action "drop")
(magit-annex-all-action "move")
(magit-annex-all-action "copy")

(defmacro magit-annex-file-action (command file-read-func)
  `(defun ,(intern (concat "magit-annex-" command "-file")) (file)
     ,(format "Run `git annex %s FILE'.
If called interactively, FILE is retrieved with `%s'."
              command (symbol-name (eval file-read-func)))
     (interactive (list (funcall ,file-read-func
                                 ,(format "File to %s" command))))
     (setq file (expand-file-name file))
     (let ((default-directory (file-name-directory file)))
       (magit-annex-run-async ,command
                              magit-current-popup-args
                              (file-name-nondirectory file)))))

(magit-annex-file-action "get" 'magit-annex-read-absent-file)
(magit-annex-file-action "drop" 'magit-annex-read-present-file-unless-from)
(magit-annex-file-action "copy" 'magit-annex-read-present-file-unless-from)
(magit-annex-file-action "move" 'magit-annex-read-present-file-unless-from)
(magit-annex-file-action "unlock" 'magit-annex-read-present-file)
(magit-annex-file-action "lock" 'magit-annex-read-unlocked-file)

(defun magit-annex-read-annex-file (prompt)
  "Complete read for PROMPT with all annex files.
See `magit-annex-completing-file-read' for more details."
  (magit-annex-completing-file-read prompt 'magit-annex-files))

(defun magit-annex-read-present-file (prompt)
  "Complete read for PROMPT with annex files present in current repo.
See `magit-annex-completing-file-read' for more details."
  (magit-annex-completing-file-read prompt 'magit-annex-present-files))

(defun magit-annex-read-present-file-unless-from (prompt)
  "Complete read for PROMPT with annex files present in current repo.
If \"--from\" is present in `magit-custom-options', fallback to
all annex files. See `magit-annex-completing-file-read' for more
details."
  (magit-annex-completing-file-read prompt 'magit-annex-present-files t))

(defun magit-annex-read-absent-file (prompt)
  "Complete read for PROMPT with annex files absent in current repo.
See `magit-annex-completing-file-read' for more details."
  (magit-annex-completing-file-read prompt 'magit-annex-absent-files))

(defun magit-annex-read-unlocked-file (prompt)
  "Complete read for PROMPT with unlocked files.
See `magit-annex-completing-file-read' for more details."
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
  (cl-some '(lambda (it) (string-match "--from=" it)) magit-current-popup-args))

(defun magit-annex-files ()
  "Return all annex files."
  (magit-git-lines "annex" "find" "--include" "*"))

(defun magit-annex-present-files ()
  "Return annex files that are present in current repo."
  (magit-git-lines "annex" "find"))

(defun magit-annex-absent-files ()
  "Return annex files that are absent in current repo."
  (magit-git-lines "annex" "find" "--not" "--in=here"))

(defun magit-annex-unlocked-files ()
  "Return unlocked annex files."
  (magit-git-lines "diff-files" "--diff-filter=T" "--name-only"))


;; Unused mode

(defun magit-annex-addunused ()
  "Add annex unused data back into the index."
  (interactive)
  (magit-section-case
    (unused-data
     (let ((dropped-num (if (use-region-p)
                            (mapcar #'car
                                    (magit-section-region-siblings #'magit-section-value))
                          (list (car (magit-section-value it))))))
       (magit-annex-run "addunused" dropped-num)))))

(defun magit-annex-dropunused (&optional force)
  "Drop current unused data.
With prefix argument FORCE, pass \"--force\" flag to
`git annex dropunused'."
  (interactive "P")
  (magit-section-case
    (unused-data
     (let ((dropped-num (if (use-region-p)
                            (mapcar #'car
                                    (magit-section-region-siblings #'magit-section-value))
                          (list (car (magit-section-value it))))))
       (magit-annex-run "dropunused" (if force
                                         (cons "--force" dropped-num)
                                       dropped-num))))
    (unused
     (magit-annex-run "dropunused" (if force
                                       (cons "--force" "all")
                                     "all")))))

(defun magit-annex-log-unused ()
  "Display log for unused file.
\('git log --stat -S<KEY>')"
  (interactive)
  (magit-section-case
    (unused-data
     (let ((key (cdr (magit-section-value it))))
       (magit-mode-setup magit-log-buffer-name-format nil
                         #'magit-log-mode
                         #'magit-refresh-log-buffer
                         'long "HEAD" (list "-S" key))))))

(defcustom magit-annex-unused-sections-hook
  '(magit-annex-insert-unused-headers
    magit-annex-insert-unused-data)
  "Hook run to insert sections into the unused buffer."
  :group 'magit-modes
  :type 'hook)


(defvar magit-annex-unused-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "s" #'magit-annex-addunused)
    (define-key map "k" #'magit-annex-dropunused)
    (define-key map "l" #'magit-annex-log-unused)
    map)
  "Keymap for `magit-annex-unused-mode'.")

(define-derived-mode magit-annex-unused-mode magit-mode "Magit Annex Unused"
  "Mode for looking at unused data in annex.

\\<magit-annex-unused-mode-map>\
Type \\[magit-annex-dropunused] to drop data at point.
Type \\[magit-annex-addunused] to add the unused data back into the index.
Type \\[magit-annex-log-unused] to show commit log for the unused file.
\n\\{magit-annex-unused-mode-map}"
  :group 'magit-modes)

(defvar magit-annex-unused-buffer-name "*magit-annex-unused*"
  "Name of buffer used to display unused data in the annex store.")

;;;###autoload
(defun magit-annex-unused ()
  "Show unused annexed data."
  (interactive)
  (magit-mode-setup magit-annex-unused-buffer-name nil
                    #'magit-annex-unused-mode
                    #'magit-annex-refresh-unused-buffer))

(defun magit-annex-refresh-unused-buffer ()
  "Refresh the content of the unused buffer."
  (magit-insert-section (unused)
    (run-hooks 'magit-annex-unused-sections-hook)))

(defun magit-annex-insert-unused-headers ()
  "Insert the headers in the unused buffer."
  (magit-insert-status-headers))

(defun magit-annex-insert-unused-data ()
  "Insert unused data into the current buffer."
  (magit-insert-section (unused)
    (magit-insert-heading "Unused data:")
    (magit-git-wash #'magit-annex-wash-unused
      "annex" "unused" magit-refresh-args)))

(defun magit-annex-wash-unused (&rest args)
  "Convert the output of git annex unused into magit section."
  (when (not (looking-at "unused .*
"))
    (error "Check magit-process for error"))
  (delete-region (point) (match-end 0))
  (if (not (looking-at ".*Some annexed data is no longer used by any files:
 *NUMBER *KEY
"))
      (progn
        (delete-region (point-min) (point-max))
        (magit-insert "   nothing"))
    (delete-region (point) (match-end 0))
    (magit-wash-sequence #'magit-annex-wash-unused-line)))

(defun magit-annex-wash-unused-line ()
  "Make a magit section from description of unused data."
  (when (looking-at " *\\([0-9]+\\) *\\(.*\\)$")
    (let ((num (match-string 1))
          (key (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (magit-insert-section it (unused-data (cons num key))
        (magit-insert (format "   %-3s   %s" num key))
        (forward-line)))))

(provide 'magit-annex)

;;; magit-annex.el ends here
