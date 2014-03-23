;;; magit-annex-tests.el

(require 'ert)
(require 'magit-annex)

;;; Utilities (modified from magit-tests.el)

(defmacro magit-annex-tests--with-temp-dir (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (gensym)))
    `(let ((,dir (file-name-as-directory (make-temp-file "dir" t))))
       (unwind-protect
           (let ((default-directory ,dir)) ,@body)
         (delete-directory ,dir t)
         (magit-annex-tests--kill-magit-dir-buffer ,dir)
         (magit-annex-tests--kill-magit-process-buffer)))))

(defun magit-annex-tests--kill-magit-dir-buffer (dir)
  (let ((dir-buffer
         (get-buffer
          (format "*magit: %s*"
                  (file-name-nondirectory (directory-file-name dir))))))
    (when dir-buffer
      (kill-buffer dir-buffer))))

(defun magit-annex-tests--kill-magit-process-buffer ()
  (let ((process-buffer (get-buffer "*magit-process*")))
    (when process-buffer
      (kill-buffer process-buffer))))

(defmacro magit-annex-tests--with-temp-annex-repo (&rest body)
  (declare (indent 0) (debug t))
  `(magit-annex-tests--with-temp-dir
     (magit-call-git "init" ".")
     (magit-call-git "annex" "init" "test-repo")
     (unwind-protect
         (progn ,@body)
       (call-process "chmod" nil nil nil "-R" "777" "."))))

(defmacro magit-annex-tests--with-temp-annex-pair (&rest body)
  (declare (indent 0) (debug t))
  `(let ((repo1 (file-name-as-directory (make-temp-file "dir" t)))
         (repo2 (file-name-as-directory (make-temp-file "dir" t))))
     (let ((default-directory repo1))
       (magit-call-git "init" ".")
       (magit-call-git "annex" "init" "repo1")
       (magit-annex-tests--modify-file "file")
       (magit-stage-item "file")
       (magit-call-git "commit" "-m" "normal commit")
       (magit-call-git "remote" "add" "repo2" repo2))
     (let ((default-directory repo2))
       (magit-call-git "clone" "-o" "repo1" repo1 ".")
       (magit-call-git "annex" "init" "repo2"))
     (unwind-protect
         (let ((default-directory repo1)) ,@body)
       (call-process "chmod" nil nil nil "-R" "777" repo1)
       (call-process "chmod" nil nil nil "-R" "777" repo2)
       (delete-directory repo1 t)
       (delete-directory repo2 t)
       (magit-annex-tests--kill-magit-dir-buffer repo1)
       (magit-annex-tests--kill-magit-dir-buffer repo2)
       (magit-annex-tests--kill-magit-process-buffer))))

(defmacro magit-annex-tests--with-temp-bare-repo (&rest body)
  (declare (indent 0) (debug t))
  `(magit-annex-tests--with-temp-dir
     (magit-call-git "init" "--bare" ".")
     ,@body))

(defmacro magit-annex-tests--with-temp-clone (url &rest body)
  (declare (indent 1) (debug t))
  (let ((repo (gensym)))
    `(let ((,repo ,(or url 'default-directory)))
       (magit-annex-tests--with-temp-dir
         (magit-call-git "clone" ,repo ".")
         (magit-call-git "annex" "init" "test-repo")
         ;; Make a normal commit and push.
         (magit-annex-tests--modify-file "file")
         (magit-stage-item "file")
         (magit-call-git "commit" "-m" "normal commit")
         (magit-call-git "push")
         (unwind-protect
             (progn ,@body)
           (call-process "chmod" nil nil nil "-R" "777" "."))))))

(defun magit-annex-tests--modify-file (filename)
  (with-temp-file (expand-file-name filename)
    (insert (symbol-name (gensym "content")))))

(defun magit-annex-tests--should-have-item-title (title section-path)
  (magit-status default-directory)
  (should (member title
                  (mapcar 'magit-section-info
                          (magit-section-children
                           (magit-find-section section-path
                                               magit-root-section))))))

;;; Test magit-annex


;; Annexing

(ert-deftest magit-annex-add-file-to-annex ()
  (magit-annex-tests--with-temp-annex-repo
    (magit-annex-tests--modify-file "file")
    (should (not (file-symlink-p "file")))
    (magit-annex-stage-item "file")
    (should (file-symlink-p "file"))
    (magit-annex-tests--should-have-item-title
     "file" '(staged))))

(ert-deftest magit-annex-add-all-files-to-annex ()
  (magit-annex-tests--with-temp-annex-repo
    (magit-annex-tests--modify-file "file1")
    (magit-annex-tests--modify-file "file2")
    (should (not (file-symlink-p "file1")))
    (let ((magit-annex-stage-all-confirm nil))
      (magit-annex-stage-all))
    (should (file-symlink-p "file1"))
    (should (file-symlink-p "file2"))
    (magit-annex-tests--should-have-item-title
     "file1" '(staged))
    (magit-annex-tests--should-have-item-title
     "file2" '(staged))))


;; Updating

(ert-deftest magit-annex-sync ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-stage-item "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait)
      (should (magit-git-lines "diff" "repo1/master"))
      (should-not (magit-git-lines "diff" "synced/master"))
      (should (magit-annex-present-files)))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (magit-process-wait)
      (should-not (magit-annex-present-files)))))

(ert-deftest magit-annex-sync-content ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-stage-item "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (let ((magit-custom-options '("--content")))
        (magit-annex-sync)
        (magit-process-wait))
      (should (magit-git-lines "diff" "repo1/master"))
      (should-not (magit-git-lines "diff" "synced/master"))
      (should (magit-annex-present-files)))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (magit-process-wait)
      (should (magit-annex-present-files)))))

(ert-deftest magit-annex-push-git-annex ()
  (magit-annex-tests--with-temp-bare-repo
    (magit-annex-tests--with-temp-clone default-directory
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-stage-item "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (let ((magit-set-upstream-on-push 'dontask))
        (magit-annex-push nil))
      (magit-process-wait)
      ;; Only git annex should have been pushed.
      (should (magit-git-lines "diff" "origin/master")))))

(ert-deftest magit-annex-push-current-and-git-annex ()
  (magit-annex-tests--with-temp-bare-repo
    (magit-annex-tests--with-temp-clone default-directory
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-stage-item "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (let ((magit-set-upstream-on-push 'dontask))
        (magit-annex-push-both nil))
      (magit-process-wait)
      ;; Current branch should also have been pushed, so there should be
      ;; no differences.
      (should (not (magit-git-lines "diff" "origin/master"))))))


;; Managing content

(ert-deftest magit-annex-get-all ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-stage-item "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (magit-process-wait)
      (magit-annex-get-all)
      (magit-process-wait)
      ;; Shouldn't be present because of --auto flag.
      (should-not (magit-annex-present-files)))))

(ert-deftest magit-annex-get-all-increase-numcopies ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-stage-item "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (magit-process-wait)
      (let ((magit-custom-options '("--numcopies=2")))
        (magit-annex-get-all)
        (magit-process-wait))
      (should (equal (magit-annex-present-files)
                     '("annex-file"))))))

(ert-deftest magit-annex-get-file ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-stage-item "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (magit-process-wait)
      (should-not (magit-annex-present-files))
      (magit-annex-get-file "annex-file")
      (magit-process-wait)
      (should (equal (magit-annex-present-files)
                     '("annex-file"))))))

(ert-deftest magit-annex-drop-file ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-stage-item "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait)
      (let ((magit-custom-options '("--force")))
        (magit-annex-drop-file "annex-file")
        (magit-process-wait))
      (should-not (magit-annex-present-files)))))

(ert-deftest magit-annex-move-file ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-stage-item "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait)
      (let ((magit-custom-options '("--to=repo1")))
        (magit-annex-move-file "annex-file")
        (magit-process-wait))
      (should-not (magit-annex-present-files)))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (should (equal (magit-annex-present-files)
                     '("annex-file"))))))

(ert-deftest magit-annex-copy-file ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-stage-item "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait)
      (let ((magit-custom-options '("--to=repo1")))
        (magit-annex-copy-file "annex-file")
        (magit-process-wait))
      (should (equal (magit-annex-present-files)
                     '("annex-file"))))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (should (equal (magit-annex-present-files)
                     '("annex-file"))))))

(ert-deftest magit-annex-unlock-lock-file ()
  (magit-annex-tests--with-temp-annex-repo
    (magit-annex-tests--modify-file "annex-file")
    (magit-annex-stage-item "annex-file")
    (magit-call-git "commit" "-m" "annex commit")
    (should-not (magit-annex-unlocked-files))
    (magit-annex-unlock-file "annex-file")
    (magit-process-wait)
    (should (equal (magit-annex-unlocked-files)
                   '("annex-file")))
    (let ((magit-custom-options '("--force")))
      (magit-annex-lock-file "annex-file")
        (magit-process-wait))
    (should-not (magit-annex-unlocked-files))))
