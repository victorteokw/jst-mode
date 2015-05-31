;;; jst-mode.el --- JS test mode

;; Copyright (C) 2015  Kai Yu

;; Author: Kai Yu <yeannylam@gmail.com>
;; Version: 0.0.1
;; Keywords: js, javascript, jasmine, coffee, coffeescript

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode deals with multiple javaScript testing frameworks and supports
;; multiple programming languages derived from javaScript.

;;; Code:

(defgroup jst nil
  "jst mode"
  :prefix "jst-"
  :group 'applications)

(defcustom jst-prefix-key (kbd "C-c t")
  "Key prefix for jst-mode."
  :type 'key-sequence
  :group 'jst)

(defcustom jst-cache-info-file (convert-standard-filename "~/.emacs.d/.jst")
  "Default target for storing jst project informatin and type information."
  :group 'jst
  :type '(choice
          (const :tag "Default value is ~/.emacs.d/.jst" nil)
          file))

(defconst jst-spec-file-name-re "^.+?\\([sS]pec\\|[tT]est\\)\\.")

(defun jst-spec-file-p (a-file-name)
  "Return true if the file name is javaScript spec."
  (numberp (string-match jst-spec-file-name-re
                         (file-name-nondirectory a-file-name))))

(defun jst-buffer-is-spec-p ()
  "Return t if buffer is spec."
  (and (buffer-file-name)
       (jst-spec-file-p (buffer-file-name))))

(defvar jst-possible-project-root-landmark
  '(".karma" "package.json" "node_modules" ".git" "Gemfile")
  "The file implies the project root.")

(defun jst-figure-out-project-of-file (a-file-name)
  "Return the project directory full path of `a-file-name'."
  (let (the-landmark)
    (catch 'found-it
      (dolist (possible-file jst-possible-project-root-landmark)
        (if (setq the-landmark
                  (locate-dominating-file a-file-name possible-file))
            (throw 'found-it the-landmark))))))

(defun jst-figure-out-project-type (root-project-dir)
  )

(defun jst-enable-approriate-mode ()
  (if (jst-buffer-is-spec-p)
      (jst-mode)
    (jst-verifiable-mode)))

;; Key map

(defvar jst-prefix-map
  (let ((jst-prefix-map))
    (define-prefix-command 'jst-prefix-map)
    ;; (define-key jst-prefix-map (kbd "??") 'jst-bla-bla)
    jst-prefix-map))

(defvar jst-verifiable-map
  (let ((jst-verifiable-map))
    (define-prefix-command 'jst-prefix-map)
    ;; (define-key jst-verifiable-map (kbd "??") 'jst-verifiable-bla-bla)
    jst-verifiable-map
    ))

(defvar jst-verifiable-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map jst-prefix-key 'jst-verifiable-map)))

(defvar jst-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map jst-prefix-key 'jst-prefix-map)))

(define-minor-mode jst-mode
  "Minor mode for testing javaScript code."
  nil " JST" jst-mode-map)

(define-minor-mode jst-verifiable-mode
  "Minor mode for javaScript files that have specs."
  nil "" jst-verifiable-mode-map)

(provide 'jst-mode)
;;; jst-mode.el ends here
