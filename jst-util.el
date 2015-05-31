(defun jst-file-base-name (file-name)
  (setq file-name (replace-regexp-in-string ".*/" "" file-name))
  (replace-regexp-in-string "\\..*" "" file-name))


(defun jst--file-name-base-without-test (a-file-name)
  (let ((base (jst--file-name-base a-file-name)))
    (dolist (maybe-suffix jst-possible-test-or-spec-dir-names)
      (setq base (string-remove-suffix maybe-suffix base)))
    (setq base (string-remove-suffix "-" base))
    (setq base (string-remove-suffix "_" base))))

;; From http://turingmachine.org/bl/2013-05-29-recursively-listing-directories-in-elisp.html
(defun jst--directory-files-recursive (directory match maxdepth ignore)
  "List files in DIRECTORY and in its sub-directories.
   Return files that match the regular expression MATCH but ignore
   files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only
   to depth MAXDEPTH. If zero or negative, then do not recurse"

  (let* ((files-list '())
         (current-directory-list
          (directory-files directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond
         ((and
           ignore ;; make sure it is not nil
           (string-match ignore f))
                                        ; ignore
          nil
          )
         ((and
           (file-regular-p f)
           (file-readable-p f)
           (string-match match f))
          (setq files-list (cons f files-list))
          )
         ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))
          ;; recurse only if necessary
          (setq files-list (append files-list (jst--directory-files-recursive f match (- maxdepth -1) ignore))))
         (t)
         )
        )
      (setq current-directory-list (cdr current-directory-list))
      )
    files-list
    )
  )

(defun jst--locate-dominating-file (file-path list-of-file-names &optional file)
  "This is similar to `locate-dominating-file' but accepts a list
of arguments."
  (let (the-landmark)
    (catch 'found-it
      (dolist (possible-file list-of-file-names)
        (if (setq the-landmark
                  (locate-dominating-file file-path possible-file))
            (if file
                (throw 'found-it (expand-file-name possible-file the-landmark))
              (throw 'found-it the-landmark)))))))

(defun jst--locate-file-or-dir (a-file-name the-path)
  "Return true if the file is in the path."
  (file-exists-p (expand-file-name a-file-name the-path)))

(provide 'jst-util)
