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
         (magit-annex-tests--kill-magit-process-buffer ,dir)))))

(defun magit-annex-tests--kill-magit-dir-buffer (dir)
  (let ((dir-buffer
         (get-buffer
          (format "*magit: %s*" dir))))
    (when dir-buffer
      (kill-buffer dir-buffer))))

(defun magit-annex-tests--kill-magit-process-buffer (dir)
  (let ((process-buffer (magit-process-buffer dir)))
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
       (magit-stage-file "file")
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
       (magit-annex-tests--kill-magit-process-buffer repo1)
       (magit-annex-tests--kill-magit-process-buffer repo2))))

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
         (magit-stage-file "file")
         (magit-call-git "commit" "-m" "normal commit")
         (magit-call-git "push")
         (magit-call-git "push" "-u" "origin" "git-annex")
         (unwind-protect
             (progn ,@body)
           (call-process "chmod" nil nil nil "-R" "777" "."))))))

(defun magit-annex-tests--modify-file (filename)
  (with-temp-file (expand-file-name filename)
    (insert (symbol-name (gensym "content")))))

(defun magit-annex-tests--should-have-section (type info)
  (magit-status default-directory)
  (message (buffer-string))
  (should (--first (equal (magit-section-value it) info)
                   (magit-section-children
                    (magit-get-section `((,type) (status)))))))

;;; Test magit-annex


;; Annexing

(ert-deftest magit-annex-add-file-to-annex ()
  (magit-annex-tests--with-temp-annex-repo
    (magit-annex-tests--modify-file "file")
    (should (not (file-symlink-p "file")))
    (magit-annex-add "file")
    (should (file-symlink-p "file"))
    (magit-annex-tests--should-have-section 'staged "file")))

(ert-deftest magit-annex-add-all-files-to-annex ()
  (magit-annex-tests--with-temp-annex-repo
    (magit-annex-tests--modify-file "file1")
    (magit-annex-tests--modify-file "file2")
    (should (not (file-symlink-p "file1")))
    (let ((magit-annex-add-all-confirm nil))
      (magit-annex-add-all))
    (should (file-symlink-p "file1"))
    (should (file-symlink-p "file2"))
    (magit-annex-tests--should-have-section 'staged "file1")
    (magit-annex-tests--should-have-section 'staged "file2")))


;; Updating

(ert-deftest magit-annex-sync ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-add "annex-file")
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
      (magit-annex-add "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync '("--content"))
      (magit-process-wait)
      (should (magit-git-lines "diff" "repo1/master"))
      (should-not (magit-git-lines "diff" "synced/master"))
      (should (magit-annex-present-files)))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (magit-process-wait)
      (should (magit-annex-present-files)))))


;; Managing content

(ert-deftest magit-annex-get-all-auto ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-add "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (magit-process-wait)
      (magit-annex-get-all-auto)
      (magit-process-wait)
      ;; Shouldn't be present because of --auto flag.
      (should-not (magit-annex-present-files)))))

(ert-deftest magit-annex-get-file ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-add "annex-file")
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

(ert-deftest magit-annex-get-all ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file1")
      (magit-annex-add "annex-file1")
      (magit-annex-tests--modify-file "annex-file2")
      (magit-annex-add "annex-file2")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (magit-process-wait)
      (should-not (magit-annex-present-files))
      (magit-annex-get-all)
      (magit-process-wait)
      (should (equal (magit-annex-present-files)
                     '("annex-file1" "annex-file2"))))))

(ert-deftest magit-annex-drop-file ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-add "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait)
      (magit-annex-drop-file "annex-file" '("--force"))
      (magit-process-wait)
      (should-not (magit-annex-present-files)))))

(ert-deftest magit-annex-drop-drop-all ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file1")
      (magit-annex-add "annex-file1")
      (magit-annex-tests--modify-file "annex-file2")
      (magit-annex-add "annex-file2")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait)
      (magit-annex-drop-all '("--force"))
      (magit-process-wait)
      (should-not (magit-annex-present-files)))))

(ert-deftest magit-annex-move-file ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-add "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait)
      (magit-annex-move-file "annex-file" '("--to=repo1"))
      (magit-process-wait)
      (should-not (magit-annex-present-files)))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (should (equal (magit-annex-present-files)
                     '("annex-file"))))))

(ert-deftest magit-annex-move-all ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file1")
      (magit-annex-add "annex-file1")
      (magit-annex-tests--modify-file "annex-file2")
      (magit-annex-add "annex-file2")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait)
      (magit-annex-move-all '("--to=repo1"))
      (magit-process-wait)
      (should-not (magit-annex-present-files)))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (should (equal (magit-annex-present-files)
                     '("annex-file1" "annex-file2"))))))

(ert-deftest magit-annex-copy-file ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file")
      (magit-annex-add "annex-file")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait)
      (magit-annex-copy-file "annex-file" '("--to=repo1"))
      (magit-process-wait)
      (should (equal (magit-annex-present-files)
                     '("annex-file"))))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (should (equal (magit-annex-present-files)
                     '("annex-file"))))))

(ert-deftest magit-annex-copy-all ()
  (magit-annex-tests--with-temp-annex-pair
    (let ((default-directory repo2))
      (magit-annex-tests--modify-file "annex-file1")
      (magit-annex-add "annex-file1")
      (magit-annex-tests--modify-file "annex-file2")
      (magit-annex-add "annex-file2")
      (magit-call-git "commit" "-m" "annex commit")
      (magit-annex-sync)
      (magit-process-wait)
      (magit-annex-copy-all '("--to=repo1"))
      (magit-process-wait)
      (should (equal (magit-annex-present-files)
                     '("annex-file1" "annex-file2"))))
    (let ((default-directory repo1))
      (magit-annex-merge)
      (should (equal (magit-annex-present-files)
                     '("annex-file1" "annex-file2"))))))

(ert-deftest magit-annex-unlock-lock-file ()
  (magit-annex-tests--with-temp-annex-repo
    (magit-annex-tests--modify-file "annex-file")
    (magit-annex-add "annex-file")
    (magit-call-git "commit" "-m" "annex commit")
    (should-not (magit-annex-unlocked-files))
    (magit-annex-unlock-file "annex-file")
    (magit-process-wait)
    (should (equal (magit-annex-unlocked-files)
                   '("annex-file")))
    (magit-annex-lock-file "annex-file" '("--force"))
    (magit-process-wait)
    (should-not (magit-annex-unlocked-files))))
