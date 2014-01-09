;;; magit-annex.el --- Use git annex within magit

;; Copyright (C) 2013 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/magit-annex
;; Keywords: magit git-annex
;; Version: 0.8.0
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
;; magit-annex provides the ability to add files to git annex using the
;; magit interface.
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
;; For other git annex commands (e.g., getting, copying, and unlocking
;; annexed files), see git-annex.el [1], which integrates nicely with
;; dired.
;;
;; [1] https://github.com/jwiegley/git-annex-el

;;; Code:

(require 'magit)

;; Variables

(defgroup magit-annex nil
  "Add files to git annex from magit"
  :prefix "magit-annex"
  :group 'magit)

(defcustom magit-annex-stage-all-confirm t
  "Whether to require confirmation before adding all changes to annex"
  :group 'magit-annex
  :type 'boolean)

;;; Keybindings

(define-key magit-status-mode-map
  "@a" 'magit-annex-stage-item)

(define-key magit-mode-map
  "@A" 'magit-annex-stage-all)

(magit-key-mode-insert-action 'dispatch
                              "@A" "Annex all" 'magit-annex-stage-all)

;;; Annexing

(defun magit-annex-stage-item (&optional file)
  "Add the item at point to annex.
With a prefix argument, prompt for a file.
\('git annex add')"
  ;; modified from `magit-stage-item'
  (interactive
   (when current-prefix-arg
     (list (file-relative-name
            (read-file-name "File to add to annex: " nil nil t)
                               (magit-get-top-dir)))))
  (if file
      (magit-run-git "annex" "add" file)
    (magit-section-action (item info "annex-add")
      ((untracked file)
       (magit-run-git "annex" "add"
                      (if (use-region-p)
                          (magit-region-siblings #'magit-section-info)
                        info)))
      ((untracked)
       (magit-run-git "annex" "add" (magit-git-lines "ls-files" "--other"
                                                     "--exclude-standard")))
      ((unstaged)
       (magit-annex-stage-all))
      ((staged *)
       (error "Already added to annex")))))

(defun magit-annex-stage-all ()
  "Add all remaining changes in tracked files to staging area.
\('git annex add .')"
  ;; modified from `magit-stage-all'
  (interactive)
  (when (or (not magit-annex-stage-all-confirm)
            (not (magit-anything-staged-p))
            (yes-or-no-p "Add all changes to annex?"))
    (magit-run-git "annex" "add" ".")))

(provide 'magit-annex)

;; magit-annex.el ends here
