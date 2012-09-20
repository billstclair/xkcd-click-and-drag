(in-package :cl-user)

(defpackage xkcd
  (:use common-lisp)
  (:import-from :cl-user #:reload)
  (:export #:pull-all
           #:reduce-files
           #:write-html-table
           #:pull-reduce-and-index)
  )