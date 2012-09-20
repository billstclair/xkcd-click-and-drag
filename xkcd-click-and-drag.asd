; -*- mode: lisp -*-
(in-package :cl-user)

(asdf:defsystem :xkcd-click-and-drag
  :description "Download and create a big HTML table for the XKCD 'click and drag' episode"
  :author "Bill St. Clair <billstclair@gmail.com>"
  :version "0.1.0"
  :license "Public Domain"
  :depends-on (drakma)
  :components ((:module source
                :serial t
                :components
                ((:file "package")
                 (:file "xkcd")
                 ))))

                        