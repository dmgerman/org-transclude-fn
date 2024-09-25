;;; org-transclusion-fn.el --- Transclude the results of a function -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Daniel M. German <dmg@turingmachine.org>

;; Author: Daniel M. German  <dmg@turingmachine.org>
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org-transclusion "1.4.0"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file extends the `org-transclusion' package to allow for
;; transcluding the results of a function.  See readme.org

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-element)
(require 'org-transclusion)

;;;; Functions

;(setq org-transclusion-fn-token "tfn")

(defvar org-transclusion-fn-token "tfn"
  "Name of the token used to identify the type of tranclusion."
  )

(defvar org-transclusion-fn-allowed-functions (list  )
  "List of functions that can be evaluated using org-transclusion-fn.  Initially empty."
  )

(defun org-transclusion-fn-run (fn parms)
  "Empty the buffer, then run the FN with parms PARMS.  FN should insert the text into the current buffer."
  (interactive)
  (let ((pos (point)))
    (erase-buffer)
    (unless (derived-mode-p 'org-mode)
      (org-mode))
    (cond ((not parms) (funcall fn))
          ((listp parms) (apply fn parms))
          (t  (funcall fn parms)))
    ))

(defun org-transclusion-fn-keyword-value-parms (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in `org-transclusion-get-keyword-values-hook'.
Double qutations are optional \"1-10\"."
  (when (string-match ":parms +\\([^\\n]+\\)" string)
    (list :parms  (match-string 1 string))))


(defun org-transclusion-fn-function-dispatch (link plist)
  "Function used by the dispatcher of org-transclusion.
LINK is the transclusion link  and PLIST is the contains other info, such as
its parameters"
  (and (string= org-transclusion-fn-token (org-element-property :type link))
       (append '(:tc-type "org-html-file")
               (org-transclusion-fn-function-run link plist))))

(defun org-transclusion-fn-keyword-plist-to-string (plist)
  "Convert a keyword PLIST to a string.
When a tranclusion function is detached, the parameters have to put
back.  This function takes care of that by using the property :parms from
the PLIST"
  (let (
        (parms (plist-get plist :parms))
        )
    (message "Executing funciton >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>[%S]" parms)
    (when parms (format ":parms %s" parms ))))

(defun org-transclusion-fn-get-function (link)
  "Retrieve the function to be evaluated from LINK, verifying it is allowed."
  (let*(
       (fn-name (org-element-property :path link) )
       (fn      (and fn-name (intern fn-name)))
       )    
    (unless fn
      (error "org-transclude-fn: no function found in link ")
      )
    (unless (functionp fn)
      (error "org-transclude-fn: [%s] is not function" fn-name)
      )
    (unless (member fn org-transclusion-fn-allowed-functions)
      (error "org-transclude-fn: function [%s] is not permitted (see org-transclusion-fn-allowed-functions)" fn-name)
      
      )
    (unless fn
      (error "org-transclude-fn: no function found in link ")
      )
    fn 
    ) 
  )
  
(defun org-transclusion-fn-function-run (link plist)
  "Return payload list by running the function.
The function name is the path of the LINK, and
its optional parameters are in the property :parms of PLIST.
We run the function in its own buffer.  If the buffer already exits,
delete its contents.  The function, when run, should insert text in
the current buffer.  Create the result by
copying the contents of the buffer."
  (let* ((fn (org-transclusion-fn-get-function link))
         (raw-parms (plist-get plist :parms))
         (parms (and raw-parms
                     (condition-case nil
                         (read raw-parms)
                       (error (format "Invalid s-exp as parameter [%s]" raw-parms))
                       )))
         (org-buf
          (get-buffer-create
           (format " *org-transclusion-fn %s*" fn)))
         (src-content
          (with-current-buffer org-buf
            (org-transclusion-fn-run fn parms)
            (buffer-string))))
    (with-current-buffer org-buf
      (org-with-wide-buffer
       (list :src-buf (current-buffer)
             :src-beg (point-min)
             :src-end (point-max)
             :src-content src-content)))))

(defun org-transclusion-fn-follow-link ()
  (error "org-transclusion-fn-follow-link cannot be followed")
  )

;; declare org-transclusion-fn as a valid org link
(org-link-set-parameters org-transclusion-fn-token :follow #'org-transclusion-fn-follow-link)

;; declare the dispatcher
(add-hook 'org-transclusion-add-functions #'org-transclusion-fn-function-dispatch)

;; declare the name of the parameters attribute for the transclusion link
(add-hook 'org-transclusion-keyword-value-functions #'org-transclusion-fn-keyword-value-parms)

;; declare the function that will recreate the parameters of the function
;; when detaching the transclusion
;(add-hook 'org-transclusion-keyword-plist-to-string-functions
          ;#'org-transclusion-fn-keyword-plist-to-string)


(provide 'org-transclusion-fn)
;;; org-tranclusion-fn.el ends here
