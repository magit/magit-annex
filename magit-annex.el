;;; magit-annex.el --- Control git-annex from Magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2018 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;;         Rémi Vanicat <vanicat@debian.org>
;; URL: https://github.com/magit/magit-annex
;; Keywords: vc tools
;; Version: 1.6.0
;; Package-Requires: ((cl-lib "0.3") (magit "2.12.0"))

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
;; Magit-annex adds a few git-annex operations to the Magit interface.
;; Annex commands are available under the annex popup menu, which is
;; bound to "@".  This key was chosen as a leading key mostly to be
;; consistent with John Wiegley's git-annex.el (which provides a Dired
;; interface to git-annex) [1].
;;
;; Adding files:
;;   @a   Add a file to the annex.
;;   @A   Add all untracked and modified files to the annex.
;;
;; Managing file content:
;;   @fu   Unlock files.
;;   @fl   Lock files.
;;   @fU   Undo files.
;;
;;   @fg   Get files.
;;   @fd   Drop files.
;;   @fc   Copy files.
;;   @fm   Move files.
;;
;;    The above commands, which operate on paths, are also useful
;;    outside of Magit buffers, especially in Dired buffers.  To make
;;    these commands easily accessible in Dired, you can add a binding
;;    for `magit-annex-file-action-popup'.  If you use git-annex.el,
;;    you can put the popup under the same binding (@f) with
;;
;;     (define-key git-annex-dired-map "f"
;;       #'magit-annex-file-action-popup)
;;
;;   @u    Browse unused files.
;;   @l    List annex files.
;;
;; Updating:
;;   @m   Run `git annex merge'.
;;   @y   Run `git annex sync'.
;;
;; In the unused buffer
;;   l    Show log for commits touching a file
;;   RET  Open a file
;;   k    Drop files
;;   s    Add files back to the index
;;
;; When Magit-annex is installed from MELPA, no additional setup is
;; needed.  The annex popup menu will be added under the main Magit
;; popup menu (and loading of Magit-annex will be deferred until the
;; first time the annex popup is called).
;;
;; To use Magit-annex from the source repository, put
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
  "Control git-annex from Magit"
  :prefix "magit-annex"
  :group 'magit-extensions)

(defcustom magit-annex-add-all-confirm t
  "Whether to confirm before adding all changes to the annex."
  :type 'boolean)

(define-obsolete-variable-alias 'magit-annex-standard-options
  'magit-annex-global-arguments "1.4.0")

(defcustom magit-annex-global-arguments nil
  "Arguments that are added to every git-annex call.
These are placed after \"annex\" in the call, whereas values from
`magit-git-global-arguments' are placed after \"git\"."
  :type '(repeat string))

(defcustom magit-annex-limit-file-choices t
  "Limit choices for file commands based on state of repo.
For example, if locking a file, limit choices to unlocked files."
  :safe 'booleanp
  :type 'boolean)

(defcustom magit-annex-confirm-all-files t
  "Require confirmation of empty input to `magit-annex-*-files' commands.
If this is nil, run the operation on all files without asking
first."
  :package-version '(magit-annex . "1.2.0")
  :type 'boolean)

(defcustom magit-annex-include-directories t
  "Whether to list directories in prompts of `magit-annex-*-files' commands.
Consider disabling this if the prompt is slow to appear in
repositories that contain many annexed files."
  :package-version '(magit-annex . "1.2.0")
  :safe 'booleanp
  :type 'boolean)

(defcustom magit-annex-unused-open-function nil
  "Function used by `magit-annex-unused-open'.

This function should take a single required argument, a file
name.  If you have configured Org mode to open files on your
system, consider using `org-open-file'.

If nil, `magit-annex-unused-open' will prompt for the name of the
program used to open the unused file."
  :type '(choice (const :tag "Read shell command" nil)
                 (function :tag "Function to open file")))

(defcustom magit-annex-unused-stat-argument t
  "Enable '--stat' flag in log popup when point is on unused item."
  :package-version '(magit-annex . "1.3.0")
  :type 'boolean)


;;; Popups

(magit-define-popup magit-annex-popup
  "Popup console for git-annex commands."
  'magit-popups
  :man-page "git-annex"
  :actions  '((?a "Add" magit-annex-add)
              (?@ "Add" magit-annex-add)
              (?A "Add all" magit-annex-add-all)
              (?f "Action on files" magit-annex-file-action-popup)
              (?G "Get all (auto)" magit-annex-get-all-auto)
              (?y "Sync" magit-annex-sync-popup)
              (?m "Merge" magit-annex-merge)
              (?u "Unused" magit-annex-unused-popup)
              (?l "List files" magit-annex-list-popup)
              (?: "Annex subcommand (from pwd)" magit-annex-command)
              (?! "Running" magit-annex-run-popup))
  :max-action-columns 3)

(magit-define-popup magit-annex-file-action-popup
  "Popup console for git-annex file commands."
  'magit-annex-popups
  :man-page "git-annex"
  :actions  '((?g "Get" magit-annex-get-files)
              (?d "Drop" magit-annex-drop-files)
              (?c "Copy" magit-annex-copy-files)
              (?m "Move" magit-annex-move-files)
              (?l "Lock" magit-annex-lock-files)
              (?u "Unlock" magit-annex-unlock-files) nil nil
              (?U "Undo" magit-annex-undo-files))
  :switches '("Switches"
              (?f "Fast" "--fast")
              (?F "Force" "--force")
              "Switches for get, drop, copy, and move"
              (?a "Auto" "--auto"))
  :options  '("Options for get, drop, copy, and move"
              (?t "To remote" "--to=" magit-read-remote)
              (?f "From remote" "--from=" magit-read-remote)
              (?n "Number of copies" "--numcopies=")
              (?j "Number of jobs" "--jobs="))
  :max-action-columns 4)

(magit-define-popup magit-annex-sync-popup
  "Popup console for git annex sync."
  'magit-annex-popups
  :man-page "git-annex-sync"
  :actions  '((?y "Sync" magit-annex-sync)
              (?r "Sync remote" magit-annex-sync-remote))
  :switches '((?c "Content" "--content")
              (?f "Fast" "--fast")
              (?F "Force" "--force")
              (?n "Don't commit local changes" "--no-commit"))
  :options  '((?j "Number of jobs" "--jobs="))
  :default-action 'magit-annex-sync)

(magit-define-popup magit-annex-unused-popup
  "Popup console for git annex unused."
  'magit-annex-popups
  :man-page "git-annex-unused"
  :actions  '((?u "Unused" magit-annex-unused)
              (?r "Unused in reflog" magit-annex-unused-reflog))
  :switches '((?f "Fast" "--fast"))
  :options '((?f "From remote" "--from=" magit-read-remote)
             (?r "Refspec" "--used-refspec="))
  :default-action 'magit-annex-unused)

(magit-define-popup magit-annex-list-popup
  "Popup console for git annex list."
  'magit-annex-popups
  :man-page "git-annex-list"
  :actions  '((?l "List files" magit-annex-list-files)
              (?d "List files in directory" magit-annex-list-dir-files))
  :switches '((?a "All repos" "--allrepos"))
  :default-action 'magit-annex-list-files)

(magit-define-popup magit-annex-run-popup
  "Popup console for running git-annex commands."
  'magit-annex-popups
  :man-page "git-annex"
  :actions '((?! "Annex subcommand (from root)" magit-annex-command-topdir)
             (?: "Annex subcommand (from pwd)" magit-annex-command)))

;;;###autoload
(eval-after-load 'magit
  '(progn
     (define-key magit-mode-map "@" 'magit-annex-popup-or-init)
     (magit-define-popup-action 'magit-dispatch-popup
       ?@ "Annex" 'magit-annex-popup-or-init ?!)))



;;; Process calls

(defun magit-annex-run (&rest args)
  "Call git-annex synchronously in a separate process, and refresh.

Before ARGS are passed to git-annex,
`magit-annex-global-arguments' will be prepended.

See `magit-run-git' for more details on the git call."
  (magit-run-git "annex" magit-annex-global-arguments args))

(defun magit-annex-run-async (&rest args)
  "Call git-annex asynchronously with ARGS.
See `magit-annex-run' and `magit-run-git-async' for more
information."
  (magit-run-git-async "annex" magit-annex-global-arguments args))

(defun magit-annex-command (command)
  "Execute COMMAND asynchonously, displaying output.
This is like `magit-git-command', but \"git annex \" rather than
\"git \" is used as the initial input."
  (interactive (list (magit-read-shell-command nil "git annex ")))
  (magit-git-command command))

(defun magit-annex-command-topdir (command)
  "Execute COMMAND asynchronously from top directory, displaying output.
This is like `magit-git-command-topdir', but \"git annex \"
rather than \"git \" is used as the initial input."
  (interactive (list (magit-read-shell-command t "git annex ")))
  (magit-git-command-topdir command))


;;; Initialization

;;;###autoload
(defun magit-annex-popup-or-init ()
  "Call Magit-annex popup or offer to initialize non-annex repo."
  (interactive)
  (cond
   ((magit-annex-inside-annexdir-p)
    (magit-annex-popup))
   ((y-or-n-p (format "No git-annex repository in %s.  Initialize one? "
                      default-directory))
    (call-interactively 'magit-annex-init))))

;;;###autoload
(defun magit-annex-init (&optional description)
  "Initialize git-annex repository.
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
    (--when-let (magit-current-section)
      (pcase (list (magit-diff-type) (magit-diff-scope))
        (`(untracked file)
         (magit-annex-run "add" (directory-file-name
                                 (oref it value))))
        (`(untracked files)
         (magit-annex-run "add" (mapcar #'directory-file-name
                                        (magit-region-values))))
        (`(untracked list)
         (magit-annex-run "add" (magit-untracked-files)))
        (`(unstaged file)
         (magit-annex-run "add" (oref it value)))
        (`(unstaged files)
         (magit-annex-run "add" (magit-region-values)))
        (`(unstaged list)
         (magit-annex-run "add" (magit-annex-unlocked-files)))))))

(defun magit-annex-add-all ()
  "Add all untracked and modified files to the annex.
\('git annex add .')"
  ;; Modified from `magit-stage-all'.
  (interactive)
  (when (or (not magit-annex-add-all-confirm)
            (not (magit-anything-staged-p))
            (yes-or-no-p "Add all changes to the annex?"))
    (magit-annex-run "add" ".")))


;;; Updating

(defun magit-annex-sync (&optional args)
  "Sync git-annex.
\('git annex sync [ARGS]')"
  (interactive (list (magit-annex-sync-arguments)))
  (magit-annex-run-async "sync" args))

(defun magit-annex-sync-remote (remote &optional args)
  "Sync git-annex with REMOTE.
\('git annex sync [ARGS] REMOTE')"
  (interactive (list (magit-read-remote "Remote")
                     (magit-annex-sync-arguments)))
  (magit-annex-run-async "sync" args remote))

(defun magit-annex-merge ()
  "Merge git-annex.
\('git annex merge')"
  (interactive)
  (magit-annex-run "merge"))


;;; Managing content

(defun magit-annex-get-all-auto ()
  "Run `git annex get --auto'."
  (interactive)
  (magit-annex-run-async "get" "--auto"))

(defun magit-annex-read-files (prompt &optional limit-to default)
  (let* ((files (pcase limit-to
                  ((guard (not magit-annex-limit-file-choices))
                   (magit-annex-files))
                  (`absent (magit-annex-absent-files))
                  (`present (magit-annex-present-files))
                  (`unlocked (magit-annex-unlocked-files))
                  (_ (magit-annex-files))))
         (dirs (and magit-annex-include-directories
                    (delete-dups
                     (sort (delq nil (mapcar #'file-name-directory files))
                           #'string-lessp))))
         (input (if files
                    (completing-read-multiple
                     (or prompt "File,s: ")
                     (cons "*all*" (if dirs (nconc dirs files) files))
                     nil nil nil nil
                     default)
                  (user-error "No files to act on"))))
    (cond
     ((and (not input) (or (not magit-annex-confirm-all-files)
                           (y-or-n-p "Act on all files?")
                           (user-error "Aborting call")))
      nil)
     ((member "*all*" input) nil)
     (t
      (cl-mapcan (lambda (f)
                   (if (string-match-p "[[.*+\\^$?]" f)
                       (file-expand-wildcards f)
                     (list f)))
                 input)))))

(defun magit-annex--dired-relist (files)
  ;; Modified from git-annex.el
  (let ((here (point)))
    (unwind-protect
        (dolist (file files)
          (dired-relist-file (expand-file-name file)))
      (goto-char here))))

(defmacro magit-annex-files-action (command &optional limit no-async)
  (declare (indent defun) (debug t))
  `(defun ,(intern (concat "magit-annex-" command "-files"))
       (files &optional args)
     ,(format "%s FILES.\n\n  git annex %s [ARGS] [FILE...]"
              (capitalize command) command)
     (interactive
      (list
       (let ((default
               (--when-let
                   (or (mapcar #'cdr (magit-region-values 'annex-list-file))
                       (-some-> (cdr (magit-section-when annex-list-file))
                                (list))
                       (and (derived-mode-p 'dired-mode)
                            (dired-get-marked-files t)))
                 (mapconcat #'identity it ","))))
         (magit-annex-read-files
          (concat ,(capitalize command)
                  " file,s"
                  (and default (format " (%s)" default))
                  ": ")
          ,limit
          default))
       (magit-annex-file-action-arguments)))
     (,(if no-async 'magit-annex-run 'magit-annex-run-async)
      ,command args files)
     (when (derived-mode-p 'dired-mode)
       (if ,no-async
           (magit-annex--dired-relist files)
         (set-process-sentinel
          magit-this-process
          (lambda (process event)
            (magit-process-sentinel process event)
            (when (eq (process-status process) 'exit)
              (magit-annex--dired-relist files)))))
       (let ((magit-display-buffer-noselect t))
         (magit-process-buffer)))))

(magit-annex-files-action "get" 'absent)
(magit-annex-files-action "drop"
  (and (not (magit-annex-from-in-options-p)) 'present))
(magit-annex-files-action "copy"
  (and (not (magit-annex-from-in-options-p)) 'present))
(magit-annex-files-action "move"
  (and (not (magit-annex-from-in-options-p)) 'present))
(magit-annex-files-action "unlock" 'present t)
(magit-annex-files-action "lock" 'unlocked t)

(magit-annex-files-action "undo" nil t)

(defun magit-annex-from-in-options-p ()
  (cl-some (lambda (it) (string-match-p "--from=" it))
           magit-current-popup-args))

(defun magit-annex-files ()
  "Return all annex files."
  (magit-git-items "annex" "find" "--print0" "--include" "*"))

(defun magit-annex-present-files ()
  "Return annex files that are present in current repo."
  (magit-git-items "annex" "find" "--print0"))

(defun magit-annex-absent-files ()
  "Return annex files that are absent in current repo."
  (magit-git-items "annex" "find" "--print0" "--not" "--in=here"))

(defun magit-annex-unlocked-files ()
  "Return unlocked annex files."
  (magit-git-items "diff-files" "-z" "--diff-filter=T" "--name-only"))


;; Unused mode

(defun magit-annex-unused-add ()
  "Add annex unused data back into the index."
  (interactive)
  (magit-section-case
    (unused-data
     (let ((data-nums (or (mapcar #'car (magit-region-values))
                          (list (car (oref it value))))))
       (magit-annex-run "addunused" data-nums)))))

(defun magit-annex-unused-drop (&optional force)
  "Drop current unused data.
With prefix argument FORCE, pass \"--force\" flag to
`git annex dropunused'."
  (interactive "P")
  (magit-section-case
    (unused-data
     (let ((data-nums (or (mapcar #'car (magit-region-values))
                          (list (car (oref it value))))))
       (magit-annex-run "dropunused" (if force
                                         (cons "--force" data-nums)
                                       data-nums))))
    (unused
     (magit-annex-run "dropunused" (if force
                                       '("--force" "all")
                                     "all")))))

(defun magit-annex-unused-log-popup ()
  "Display log for unused file.

This is like `magit-log-popup', but, if point is on an unused
file, the unused file's key is automatically supplied as the
value for the '-S' flag.  The '--stat' flag is also enabled if
`magit-annex-unused-stat-argument' is non-nil.

\('git log [--stat] -S<KEY>')"
  (interactive)
  (let ((section (magit-current-section)))
    (if (not (eq (oref section type) 'unused-data))
        (call-interactively #'magit-log-popup)
      (let ((magit-log-arguments
             `(,(concat "-S" (cdr (oref section value)))
               ,(and magit-annex-unused-stat-argument "--stat")
               ,@(cl-remove-if
                  (lambda (x) (string-prefix-p "-S" x))
                  (-if-let (buffer (magit-mode-get-buffer 'magit-log-mode))
                      (with-current-buffer buffer
                        (magit-popup-import-file-args (nth 1 magit-refresh-args)
                                                      (nth 2 magit-refresh-args)))
                    (default-value 'magit-log-arguments)))))
            (magit-popup-use-prefix-argument 'default))
        (magit-invoke-popup 'magit-log-popup nil nil)))))

(defun magit-annex--file-name-from-key (key)
  (magit-git-string "annex" "examinekey" key
                    "--format=.git/annex/objects/${hashdirmixed}${key}/${key}"))

(declare-function dired-read-shell-command "dired-aux" (prompt arg files))

(defun magit-annex-unused-open (&optional in-emacs)
  "Open an unused file.
By default, prompt for a command to open the file.  If
`magit-annex-unused-open-function' is non-nil, pass the file name
to this function instead.  With prefix argument IN-EMACS, open
the file within Emacs."
  (interactive "P")
  (magit-section-case
    (unused-data
     (let* ((key (cdr (oref it value)))
            (file (magit-annex--file-name-from-key key)))
       (cond
        (in-emacs
         (find-file file))
        (magit-annex-unused-open-function
         (funcall magit-annex-unused-open-function file))
        (t
         (require 'dired-aux)
         (let ((command (dired-read-shell-command "open %s with " ()
                                                  (list file))))
           (dired-do-async-shell-command command () (list file)))))))))

(defvar magit-annex-unused-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map (kbd "RET") #'magit-annex-unused-open)
    (define-key map "s" #'magit-annex-unused-add)
    (define-key map "k" #'magit-annex-unused-drop)
    (define-key map "l" #'magit-annex-unused-log-popup)
    map)
  "Keymap for `magit-annex-unused-mode'.")

(define-derived-mode magit-annex-unused-mode magit-mode "Magit-annex Unused"
  "Mode for looking at unused data in annex.

\\<magit-annex-unused-mode-map>\
Type \\[magit-annex-unused-drop] to drop data at point.
Type \\[magit-annex-unused-add] to add the unused data back into the index.
Type \\[magit-annex-unused-log-popup] to show commit log for the unused file.
Type \\[magit-annex-unused-open] to open the file.
\n\\{magit-annex-unused-mode-map}"
  :group 'magit-modes
  (hack-dir-local-variables-non-file-buffer))

;;;###autoload
(defun magit-annex-unused (&optional args)
  "Show unused data.
\('git annex unused [ARGS]')"
  (interactive (list (magit-annex-unused-arguments)))
  (magit-mode-setup #'magit-annex-unused-mode args))

;;;###autoload
(defun magit-annex-unused-reflog (&optional args)
  "Show unused data.
\('git annex unused --used-refspec=reflog [ARGS]')"
  (interactive (list (magit-annex-unused-arguments)))
  (if (cl-some (lambda (x) (string-prefix-p "--used-refspec=" x))
               args)
      (user-error "Flag --used-refspec was given more than once")
    (setq args (cons "--used-refspec=reflog" args)))
  (magit-mode-setup #'magit-annex-unused-mode args))

(defun magit-annex-unused-refresh-buffer (args)
  "Refresh the content of the unused buffer."
  (magit-insert-section (unused)
    (magit-insert-heading
      (concat "Unused files"
              (and args
                   (concat " (" (mapconcat #'identity args " ") ")"))
              ":"))
    (magit-git-wash #'magit-annex-unused-wash
      "annex" "unused" args)))

(defun magit-annex-unused-wash (&rest _)
  "Convert the output of git-annex unused into Magit section."
  (when (not (looking-at "unused .*
"))
    (error "Check magit-process for error"))
  (delete-region (point) (match-end 0))
  (if (not (looking-at ".*Some annexed data is no longer used by any files:
 *NUMBER *KEY
"))
      (progn
        (delete-region (point-min) (point-max))
        (insert "   nothing"))
    (delete-region (point) (match-end 0))
    (magit-wash-sequence #'magit-annex-unused-wash-line)
    (delete-region (point) (point-max))))

(defun magit-annex-unused-wash-line ()
  "Make a Magit section from description of unused data."
  (when (looking-at " *\\([0-9]+\\) *\\(.*\\)$")
    (let ((num (match-string 1))
          (key (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (magit-insert-section (unused-data (cons num key))
        (insert (format "   %-3s   %s" num key))
        (forward-line)))))


;; List mode

(defcustom magit-annex-list-sections-hook
  '(magit-annex-list-insert-headers
    magit-annex-list-insert-files)
  "Hook run to insert sections into a `magit-annex-list-mode' buffer."
  :group 'magit-modes
  :type 'hook)

(defvar magit-annex-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "f" #'magit-annex-file-action-popup)
    map)
  "Keymap for `magit-annex-list-mode'.")

(define-derived-mode magit-annex-list-mode magit-mode "Magit-annex List"
  "Mode for viewing on `git annex list' output.

\\<magit-annex-list-mode-map>\
Type \\[magit-annex-file-action-popup] to perform git-annex action
on the files selected by the region (if active) or the file at point.
\n\\{magit-annex-list-mode-map}"
  :group 'magit-modes
  (hack-dir-local-variables-non-file-buffer))

;;;###autoload
(defun magit-annex-list-files (&optional args)
  "List annex files.
\('git annex list [ARGS]')"
  (interactive (magit-annex-list-arguments))
  (magit-mode-setup #'magit-annex-list-mode nil args))

;;;###autoload
(defun magit-annex-list-dir-files (directory &optional args)
  "List annex files in DIRECTORY.
\('git annex list [ARGS] DIRECTORY')"
  (interactive
   (list (directory-file-name
          (file-relative-name (read-directory-name "List annex files in: "
                                                   nil nil t)
                              (magit-toplevel)))
         (magit-annex-list-arguments)))
  (magit-mode-setup #'magit-annex-list-mode directory args))

(defun magit-annex-list-refresh-buffer (&rest _)
  "Refresh content of a `magit-annex-list-mode' buffer."
  (magit-insert-section (annex-list-buffer)
    (run-hooks 'magit-annex-list-sections-hook)))

(defun magit-annex-list-insert-headers ()
  "Insert headers for `magit-annex-list-mode' buffer."
  (magit-insert-status-headers))

(defun magit-annex-list-insert-files ()
  "Insert output of `git annex list'."
  (let* ((subdir (car magit-refresh-args))
         (heading (if subdir
                      (format "Annex files in %s:" subdir)
                    "Annex files:")))
    (magit-insert-section (annex-list-buffer)
      (magit-insert-heading heading)
      (magit-git-wash #'magit-annex-list-wash
        "annex" "list" magit-refresh-args))))

(defconst magit-annex-list-line-re "\\([_X]+\\) \\(.*\\)$")

(defun magit-annex-list-wash (&rest _)
  "Convert the output of `git annex list' into Magit section."
  (when (looking-at "(merging .+)")
    (delete-region (point) (1+ (match-end 0))))
  (when (not (looking-at "here"))
    (error "Check magit-process for error"))
  (if (re-search-forward magit-annex-list-line-re nil t)
      (progn (beginning-of-line)
             (magit-wash-sequence #'magit-annex-list-wash-line))
    (re-search-forward "^|+$")))

(defun magit-annex-list-wash-line ()
  "Convert file line of `git annex list' into Magit section."
  (when (looking-at magit-annex-list-line-re)
    (let ((locs (match-string 1))
          (file (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (magit-insert-section (annex-list-file (cons locs file))
        (insert (format "%s %s" locs file))
        (forward-line)))))

(provide 'magit-annex)

;;; magit-annex.el ends here
