(defun jst-guess-spec-file-location (a-file-name)
  "Given a normal js file, return spec file location by guessing."
  (let (basename extension test-or-spec-dir)
    (setq basename (jst--file-name-base a-file-name))
    (setq extension (file-name-extension a-file-name))
    (setq test-or-spec-dir (jst-figure-out-spec-dir a-file-name))
    (jst-figure-out-spec-file basename extension test-or-spec-dir)))

(defun jst-guess-target-file-location (a-file-name)
  "Given a normal spec file, return the target file location by guessing."
  (let (basename project-root)
    (setq basename (jst--file-name-base-without-test a-file-name))
    (setq project-root (jst-figure-out-project-dir a-file-name))
    (jst-figure-out-target-file basename project-root)))

(defun jst-figure-out-target-file (basename proj-root)
  "Figure out the target tile location."
  (let ((list (jst--directory-files-recursive
               proj-root
               (format "/%s\\.\\(js\\|coffee\\|\\)" (regexp-quote basename))
               10 nil)))
    (if (>= (length list) 1)
        (nth 0 list)
      (error "jst: Cannot find target file."))))

(defun jst-figure-out-spec-file (basename extension test-dir)
  "Figure out the spec file location."
  (let ((list (jst--directory-files-recursive test-dir (format ".*%s[_]?[sStT].*?" (regexp-quote basename)) 10 nil)))
    (if (= 1 (length list))
        (nth 0 list)
      (catch 'found-it
        (dolist (the-file list)
          (and (string-equal (file-name-extension the-file) extension)
               (throw 'found-it the-file)))))))

(defun jst-figure-out-spec-dir (a-file-name)
  "Return the most possible spec dir of the file.
This is different from the spec dir of the project."
  (jst--locate-dominating-file a-file-name
                               jst-possible-test-or-spec-dir-names t))

(defun jst-figure-out-project-dir (a-file-name)
  "Return the project directory full path of `a-file-name'."
  (jst--locate-dominating-file a-file-name jst-possible-project-root-landmark))

(defun jst-figure-out-project-type (root-project-dir)
  "Return project type."
  (let ((similarity-alist nil) (ret-type nil) (max-simi 0.0))
    (setq similarity-alist
          (mapcar (lambda (cell)
                    (let ((type nil) (similarity 0.0) (count 0.0) (have 0.0))
                      (setq type (car cell))
                      (dolist (maybe-file (cdr cell))
                        (setq count (1+ count))
                        (and (jst--locate-file-or-dir maybe-file
                                                      root-project-dir)
                             (setq have (1+ have))))
                      (setq similarity (/ have count))
                      (cons type similarity)))
                  jst-project-type-characteristic-alist))
    (dolist (cell similarity-alist)
      (if (> (cdr cell) max-simi)
          (progn
            (setq max-simi (cdr cell))
            (setq ret-type (car cell)))))
    ret-type))

(provide 'jst-guessing)
