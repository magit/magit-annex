;;; magit-annex.el --- Use git annex within magit

;; Copyright (C) 2013 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/magit-annex
;; Keywords: magit git-annex
;; Version: 0.9.0
;; Package-Requires: ((magit "1.2.0"))

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
;;
;; The main feature is the ability to add files to the annex from the
;; status buffer.
;;
;;   @a   Add a file to annex.
;;        Behaves similarly to staging a file with 's'.
;;
;;   @A   Add all untracked files and unstaged changes to annex.
;;        Behaves similarly to staging all files with 'S'.
;;
;; '@' was chosen as a leading key mostly to be consistent with John
;; Wiegley's git-annex.el [1], but also because there aren't too many
;; single letters available in the magit keymap.
;;
;; There are also options to run a few other git annex commands:
;;
;;   @y   Sync git annex
;;   m@   Merge git annex (under the merging menu)
;;   P@g  Push git annex (under the pushing menu)
;;   P@b  Push current branch and git annex (under the pushing menu)
;;
;; For other git annex commands (e.g., getting, copying, and unlocking
;; annexed files), see git-annex.el [1], which integrates nicely with
;; dired.
;;
;; [1] https://github.com/jwiegley/git-annex-el

;;; Code:

(require 'magit)

;;; Variables

(defgroup magit-annex nil
  "Use git annex within magit"
  :prefix "magit-annex"
  :group 'magit)

(defcustom magit-annex-stage-all-confirm t
  "Whether to require confirmation before adding all changes to annex"
  :group 'magit-annex
  :type 'boolean)

(defcustom magit-annex-standard-options nil
  "Call git annex with these options.
These are placed after \"annex\" in the call, whereas values from
`magit-git-standard-options' are placed after \"git\"."
  :group 'magit-annex
  :type '(repeat string))

;;; Keybindings

(define-key magit-status-mode-map
  "@a" 'magit-annex-stage-item)

(define-key magit-mode-map
  "@A" 'magit-annex-stage-all)

(define-key magit-mode-map
  "@y" 'magit-annex-sync)

(magit-key-mode-insert-action 'dispatch
                              "@A" "Annex all" 'magit-annex-stage-all)

(magit-key-mode-insert-action 'dispatch
                              "@y" "Sync git annex" 'magit-annex-sync)

(magit-key-mode-insert-action 'merging
                              "@" "Merge git annex branch" 'magit-annex-merge)

(magit-key-mode-insert-action 'pushing
                              "@g" "Push git annex branch" 'magit-annex-push)

(magit-key-mode-insert-action 'pushing
                              "@b" "Push current branch and git annex branch"
                              'magit-annex-push-both)

;;; Process calls

(defun magit-annex-run (&rest args)
  (apply #'magit-run-git "annex"
         (append magit-annex-standard-options args)))

(defun magit-annex-run-async (&rest args)
  (apply #'magit-run-git-async "annex"
         (append magit-annex-standard-options args)))

;;; Annexing

(defun magit-annex-stage-item (&optional file)
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
    (magit-section-action (item info "annex-add")
      ((untracked file)
       (magit-annex-run "add"
                      (if (use-region-p)
                          (magit-region-siblings #'magit-section-info)
                        info)))
      ((untracked)
       (magit-annex-run "add" (magit-git-lines "ls-files" "--other"
                                               "--exclude-standard")))
      ((unstaged)
       (magit-annex-stage-all))
      ((staged *)
       (error "Already added to annex")))))

(defun magit-annex-stage-all ()
  "Add all remaining changes in tracked files to staging area.
\('git annex add .')"
  ;; Modified from `magit-stage-all'.
  (interactive)
  (when (or (not magit-annex-stage-all-confirm)
            (not (magit-anything-staged-p))
            (yes-or-no-p "Add all changes to annex?"))
    (magit-annex-run "add" ".")))

;;; Updating

(defun magit-annex-sync ()
  "Sync git-annex branch.
\('git annex sync')"
  (interactive)
  (magit-annex-run-async "sync"))

(defun magit-annex-merge ()
  "Merge git annex branch.
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
      (error "Not pushing since no upstream has been set."))
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

(provide 'magit-annex)

;; magit-annex.el ends here
