(in-package :cl-user)

(defpackage xkcd
  (:use common-lisp)
  (:import-from :cl-user #:reload)
  (:export #:pull-all
           #:write-html-table)
  )