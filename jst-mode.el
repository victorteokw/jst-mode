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

(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))
(require 'jst-project)
(require 'jst-guessing)
(require 'jst-util)

(defgroup jst nil
  "jst mode"
  :prefix "jst-"
  :group 'applications)

(defcustom jst-keymap-prefix (kbd "C-c t")
  "The prefix for all testing related key commands."
  :type 'string
  :group 'jst)

(defcustom jst-cache-info-file (convert-standard-filename "~/.emacs.d/.jst")
  "Default target for storing jst project informatin and type information."
  :group 'jst
  :type '(choice
          (const :tag "Default value is ~/.emacs.d/.jst" nil)
          file))

(defvar jst-possible-project-root-landmark
  '(".karma" "package.json" "node_modules" ".git" "Gemfile" "Rakefile"
    "bower.json" "Gruntfile.js")
  "The file implies the project root.")

(defvar jst-project-type-characteristic-alist
  '(("rails" . ("config.ru" "Gemfile" "app" "public" "app/controllers" "app/assets"))
    ("node" . ("package.json" "node_modules" "Gruntfile.js" "bower.json"))
    ("angular" . ("unknown yet")) ;; TODO
    ("ember" . ("unknown yet")) ;; TODO
    ))

(defvar jst-possible-test-or-spec-dir-names
  '("spec" "test" "Spec" "Test"))

(defvar jst-known-langs-and-exts (make-hash-table :test 'equal)
  "An hash table records known languages and extensions,
keys are language names such as 'LiveScript',
values are list of extensions such as ('ls').")
(setq jst-known-langs-and-exts (make-hash-table :test 'equal))

(defvar jst-known-spec-file-patterns '()
  "An list records the global spec file patterns.")

(defun jst-remember-language (&rest args)
  "Let JST remember another language."
  (let (label value lang ext)
    (while (not (= 0 (length args)))
      (setq label (pop args))
      (setq value (pop args))
      (and (equal :extension label) (setq ext value))
      (and (equal :name label) (setq lang value)))
    (if (gethash value jst-known-langs-and-exts)
        (push ext (gethash value jst-known-langs-and-exts))
      (puthash lang (list ext) jst-known-langs-and-exts))) nil)

(jst-remember-language :extension "js" :name "JavaScript")
(jst-remember-language :extension "es6" :name "ECMA6")
(jst-remember-language :extension "ts" :name "TypeScript")
(jst-remember-language :extension "coffee" :name "CoffeeScript")
(jst-remember-language :extension "ls" :name "LiveScript")

(defun jst-remember-spec-file-pattern (pattern)
  "Let JST know another spec file pattern."
  (push pattern jst-known-spec-file-patterns))

(jst-remember-spec-file-pattern "^.+?[_]?\\([sS]pec\\|[tT]est\\)\\.")

(defun jst-all-reasonable-exts ()
  "Return a list of all reasonable exts."
  (-uniq (-flatten (hash-table-values jst-known-langs-and-exts))))

(defun jst-file-is-js (file-name)
  "Return t if file is js."
  (-contains? (jst-all-reasonable-exts) (file-name-extension file-name)))

(defun jst-file-is-spec (file-name)
  "Return t if file is js and spec."
  (and (jst-file-is-js file-name)
       (jst-file-matches-spec file-name)))

(defun jst-file-matches-spec (file-name)
  (let ((base-name (file-name-nondirectory file-name)))
    (catch 'found-it
      (dolist (rex jst-known-spec-file-patterns)
        (if (numberp (string-match rex base-name))
            (throw 'found-it t))))))



(defun jst-buffer-is-js (&optional buffer)
  "Return t if buffer is js."
  (setq buffer (or buffer (current-buffer)))
  (jst-file-is-js (buffer-file-name buffer)))

(defun jst-buffer-is-spec (&optional buffer)
  "Return t if buffer is spec."
  (setq buffer (or buffer (current-buffer)))
  (jst-file-is-spec (buffer-file-name buffer)))

(defun jst-find-spec-file-other-window ()
  "Find the spec file in other window."
  (interactive)
  ;; Use guessing currently
  (find-file-other-window (jst-guess-spec-file-location (buffer-file-name))))

(defun jst-find-target-file-other-window ()
  "Find the target file in other window."
  (interactive)
  ;; Use guessing currently
  (find-file-other-window (jst-guess-target-file-location (buffer-file-name))))

(defun jst-enable-appropriate-mode ()
  "Enable appropriate mode for the opened buffer."
  (if (jst-buffer-is-spec) (jst-mode)
    (if (jst-buffer-is-js) (jst-verifiable-mode))))

;; Key map

(define-prefix-command 'jst-verifiable-command-map)
(define-prefix-command 'jst-command-map)

(define-key jst-verifiable-command-map (kbd "b")
  'jst-find-spec-file-other-window)
(define-key jst-command-map (kbd "b") 'jst-find-target-file-other-window)

(defvar jst-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map jst-keymap-prefix 'jst-command-map)
    map)
  "Keymap for `jst-mode'.")

(defvar jst-verifiable-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map jst-keymap-prefix 'jst-verifiable-command-map)
    map)
  "Keymap for `jst-verifiable-mode'.")


(define-minor-mode jst-mode
  "Minor mode for testing javaScript unit test files.

\\{jst-mode-map}"
  :lighter " JST" :keymap jst-mode-map
  ;; Do something else
  )

(define-minor-mode jst-verifiable-mode
  "Minor mode for javaScript files that have specs.

\\{jst-verifiable-mode-map}"
  :lighter "" :keymap jst-verifiable-mode-map)

(provide 'jst-mode)
;;; jst-mode.el ends here
