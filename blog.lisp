;;; blog.lisp
;;;
;;; This is a simple blog I wrote in Common Lisp. It's friggin'
;;; awesome.

(defpackage :blog
  (:use :common-lisp :hunchentoot :cl-who :css-lite :split-sequence))

(in-package :blog)

;;;; Model

(defparameter *entries* '())

(defclass blog-entry ()
  ((slug         :reader entry-slug
                 :initarg :slug)
   (title        :reader entry-title
                 :initarg :title)
   (body         :reader entry-body
                 :initarg :body)
   (date-created :reader entry-date-created
                 :initform (get-universal-time)
                 :initarg :date-created)))

(defun entries ()
  (sort (copy-list *entries*) #'> :key #'entry-date-created))

(defun entry-from-slug (slug)
  (find slug (entries)
        :test #'string-equal
        :key #'entry-slug))

(defun slug-used? (slug)
  (entry-from-slug slug))

(defun slug-from-url (url)
  (nth 2 (split-sequence #\/ url)))

(defun entry-from-url (url)
  (entry-from-slug (slug-from-url url)))

(defun add-entry (slug title body)
  (unless (slug-used? slug)
    (push (make-instance 'blog-entry :slug slug :title title :body body)
          *entries*)))

;;;; Persistence

(defmethod print-object ((entry blog-entry) stream)
  (if *print-readably*
      (with-slots (slug title body date-created)
          entry
        (format stream
                (concatenate 'string
                             "#.(make-instance 'blog-entry "
                             ":slug ~S :title ~S :date-created ~D "
                             ":body ~S)")
                slug title date-created body))
      (call-next-method)))

(defun save-entries (entries pathname)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-readably* t)
            (*package* (find-package :common-lisp)))
        (print entries stream))))
  pathname)

(defun load-entries (pathname)
  (with-open-file (stream pathname :direction :input)
    (with-standard-io-syntax
      (let ((*package* (find-package :blog)))
        (read stream)))))

;;;; Helpers

(defconstant day-names
  '("Monday" "Tuesday" "Wednesday" "Thursday"
    "Friday" "Saturday" "Sunday"))

(defconstant month-names
  '("January" "February" "March" "April" "May" "June" "July"
    "August" "September" "October" "November" "December"))

(defun humanize-date (universal-time)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time universal-time)
    (format nil "~a, ~a ~d, ~d"
            (nth day-of-week day-names)
            (nth (1- month) month-names)
            date
            year)))

(defun humanize-time (universal-time)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time universal-time)
    (format nil "~d:~2,'0d" hour minute)))

;;;; View

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
            :xml\:lang "en"
            :lang "en"
            (:head
             (:meta :http-equiv "Content-Type"
                    :content    "text/html;charset=utf-8")
             (:link :rel  "stylesheet"
                    :href "/style")
             (:title ,title))
            (:body
             (:h1
              (:a :href "/index" "The Little Blogger"))
             ,@body))))

(defmacro make-handler ((name) &body body)
  `(push (create-prefix-dispatcher
          ,(format nil "/~(~a~)" name)
          (lambda () ,@body))
         *dispatch-table*))

(defun entry-html (entry &key (link-header 't))
  (with-html-output-to-string (*standard-output* nil)
    (:h2
     (if link-header
         (htm
          (:a :href (format nil "/entries/~a" (entry-slug entry))
              (fmt (entry-title entry))))
         (fmt (entry-title entry))))
    (:h3 :class "entry-info"
         (fmt (format nil "Posted on ~a at ~a"
                      (humanize-date (entry-date-created entry))
                      (humanize-time (entry-date-created entry)))))
    (:div (fmt (entry-body entry)))))

(make-handler (index)
  (standard-page (:title "The Little Blogger")
    (:p (:a :href "/write-entry" "Write a New Entry"))
    (:div :id "content"
          (dolist (entry (entries))
            (htm
             (:div (fmt (entry-html entry))))))))

;; Also use "/" for the index
;;;  (push (create-prefix-dispatcher "/" 'index) *dispatch-table*)

(make-handler (entries)
  (let ((entry (entry-from-url (script-name *request*))))
    (standard-page (:title (fmt (entry-title entry)))
      (:div :id "content"
            (fmt (entry-html entry :link-header nil))))))

(make-handler (write-entry)
  (standard-page (:title "Write a New Entry")
    (:div :id "content"
          (:form :action "/save-entry" :method "post"
                 (:p (:label "Title" (:br)
                             (:input :type "text"
                                     :name "title"
                                     :autofocus t)))
                 (:p (:label "Slug" (:br)
                             (:input :type "text"
                                     :name "slug")))
                 (:p (:label "Body" (:br)
                             (:textarea :name "body")))
                 (:p (:input :type "submit"
                             :value "Save")
                     " or "
                     (:a :href "/index" "Cancel"))))))

(make-handler (save-entry)
  (let ((title (parameter "title"))
        (slug (parameter "slug"))
        (body (parameter "body")))
    (add-entry slug title body)
    (redirect "/index")))

(make-handler (style)
  (setf (hunchentoot:content-type* hunchentoot:*reply*) "text/css")
  (css-lite:css
    (("h1, h2, h3, div, p, ol, ul, li")
     (:margin 0 :padding 0))
    (("body")
     (:color "#222"
      :font-family "\"DejaVu Sans\", sans-serif"
      :font-size "1em"
      :line-height "1.5em"
      :padding "2em"))
    (("a")
     (:color "#2F51A9"
      :text-decoration "none"))
    (("a:hover")
     (:text-decoration "underline"))
    (("h1, h2")
     (:font-weight "normal"))
    (("h1")
     (:margin-bottom "0.5em"))
    (("h2")
     (:margin-top "1em"
      :margin-bottom "0.2em"))
    (("h3")
     (:color "#777"
      :font-size "0.8em"))
    (("ol, ul, p+p, li>p")
     (:margin "1em 0"))
    (("input, textarea")
     (:font "inherit"))
    (("input[type='text'], textarea")
     (:border "1px solid #000"
      :width "100%"))
    (("textarea")
     (:height "20em"))
    (("#content")
     (:width "40em"))))

;;;; Web Server

(defparameter *acceptor*
  (make-instance 'hunchentoot:acceptor :port 8080))

;; (hunchentoot:start *acceptor*)
;; (hunchentoot:stop *acceptor*)