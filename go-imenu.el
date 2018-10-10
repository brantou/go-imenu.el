;;; go-imenu.el --- imenu for go language -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Brantou

;; Author: Brantou <brantou89@gmail.com>
;; URL: https://github.com/brantou/go-imenu.el
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; `go-imenu.el' provides imenu for Go language.
;;

;;; Requirements:
;;
;; - go-outline :: https://github.com/lukehoban/go-outline.
;;

;;; Code:
(require 'json)

(defgroup go-imenu nil
  "Options specific to the Go imenu."
  :group 'go)

(defcustom go-imenu-command "go-outline"
  "The 'go-imenu' command.
from https://github.com/lukehoban/go-outline."
  :type 'string
  :group 'go-imenu)

(defcustom go-imenu-generic-expression
  '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
    ("func" "^func *\\(.*\\) {" 1))
  "Imenu-generic-expression."
  :type 'list
  :group 'go-imenu)

(defun go-imenu-create-index ()
  "Create an imenu index of all methods in the buffer."
  (if (executable-find go-imenu-command)
      (let ((index-alist (ignore-errors (go-imenu--create-index))))
        (if index-alist
            index-alist
          (imenu--generic-function go-imenu-generic-expression)))
    (imenu--generic-function go-imenu-generic-expression)))

(defun go-imenu--create-index()
  "Create an imenu index of all methods in the buffer."
  (let ((fname (or
             (and (file-exists-p buffer-file-name) buffer-file-name)
             (make-temp-file "Go-Imenu" nil ".go")))
        (outbuf (get-buffer-create "*Go-Imenu*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (index-alist '())
        (index-var-alist '())
        (index-type-alist '())
        (index-import-alist '())
        (index-unknown-alist '())
        cmd-args)
    (unwind-protect
        (save-restriction
          (widen)
          (with-current-buffer outbuf
            (setq buffer-read-only nil)
            (erase-buffer))
          (unless (file-exists-p buffer-file-name)
            (write-region nil nil fname))
          (setq cmd-args (append cmd-args (list "-f" fname)))
          (if (zerop (apply #'call-process go-imenu-command nil outbuf nil cmd-args))
              (let* ((ol-res-str (with-current-buffer outbuf (buffer-string)))
                     (ol-res-json (let ((json-key-type 'string))
                                    (json-read-from-string ol-res-str))))
                (let ((childrens (cdr (assoc "children" (aref ol-res-json 0)))))
                  (mapc (lambda (entry)
                            (let* ((label (cdr (assoc "label" entry)))
                                   (type (cdr (assoc "type" entry)))
                                   (pos (byte-to-position (cdr (assoc "start" entry))))
                                   (receiverType (when (assoc "receiverType" entry)
                                                   (cdr (assoc "receiverType" entry))))
                                   (index-name (if receiverType
                                                   (if (string-prefix-p "*"
                                                                        receiverType)
                                                       (concat
                                                        (substring receiverType 1)
                                                        "->" label)
                                                     (concat receiverType "::" label))
                                                 label)))
                              (cond
                               ((string= "function" type)
                                (push (cons index-name pos) index-alist))
                               ((string= "variable" type)
                                (push (cons index-name pos) index-var-alist))
                               ((string= "type" type)
                                (push (cons index-name pos) index-type-alist))
                               ((string= "import" type)
                                (push (cons index-name pos) index-import-alist))
                               (t
                                (push (cons index-name pos) index-unknown-alist)))))
                          childrens)
                  ))
            (message "Could not apply go-imenu")
            (if outbuf
                (progn
                  (message (with-current-buffer outbuf (buffer-string))))
              ))))
    (kill-buffer outbuf)
    (unless (file-exists-p buffer-file-name)
      (delete-file fname))
    (setq index-alist (reverse index-alist))
    (and index-var-alist
         (push (cons "Variables" (reverse index-var-alist)) index-alist))
    (and index-type-alist
         (push (cons "Types" (reverse index-type-alist)) index-alist))
    ;;(and index-import-alist
    ;;     (push (cons "Import" index-import-alist) index-alist))
    (and index-unknown-alist
         (push (cons "Syntax-unknown" (reverse index-unknown-alist)) index-alist))
    index-alist))

;;;###autoload
(defun go-imenu-setup ()
  "Set up imenu function!"
  (interactive)
  (setq-local imenu-generic-expression nil)
  (setq-local imenu-create-index-function #'go-imenu-create-index))

(provide 'go-imenu)

;;; go-imenu.el ends here
