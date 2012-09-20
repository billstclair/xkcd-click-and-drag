(in-package :xkcd)

(defparameter *image-url* "http://imgs.xkcd.com/clickdrag/")

(defun image-file-name (pos &optional (include-type-p t))
  (destructuring-bind (y . x) pos
    (format nil "~d~a~d~a~a"
            (if (>= y 0) (1+ y) (- y))
            (if (>= y 0) #\n #\s)
            (if (>= x 0) (1+ x) (- x))
            (if (>= x 0) #\e #\w)
            (if include-type-p ".png" ""))))

(defun parse-image-file-name (name)
  (check-type name string)
  (let (y pos ns x ew)
    (multiple-value-setq (y pos)
      (parse-integer name :junk-allowed t))
    (setf ns (elt name pos))
    (multiple-value-setq (x pos)
      (parse-integer name :junk-allowed t :start (1+ pos)))
    (setf ew (elt name pos))
    (if (eql ns #\n) (decf y) (setf y (- y)))
    (if (eql ew #\e) (decf x) (setf x (- x)))
    (cons y x)))

(defparameter *initial-pos* '(0 . 0))

(defun image-url (pos)
  (concatenate 'string
               *image-url*
               (if (stringp pos) pos (image-file-name pos))))

(defun image-path (pos)
  (make-pathname :directory '(:relative "www" "images")
                 :name (if (stringp pos) pos (image-file-name pos nil))
                 :type "png"))

(defun write-bytes-to-file (bytes file)
  (with-open-file (s file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :element-type '(unsigned-byte 8))
    (write-sequence bytes s)))

(defun read-bytes-from-file (file)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let ((res (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence res s)
      res)))

(defun get-image (pos)
  (let ((file (image-path pos)))
    (if (probe-file file)
        t
        (multiple-value-bind (image status) (drakma:http-request (image-url pos))
          (cond ((eq status 200)
                 (write-bytes-to-file image (image-path pos))
                 (length image))
                (t nil))))))

(defvar *all-pos-hash* (make-hash-table :test 'equal))

(defvar *todo-list* nil)

(defun push-todo (pos)
  (unless (gethash pos *all-pos-hash*)
    (push pos *todo-list*)
    (setf (gethash pos *all-pos-hash*) t)))

(defun pull-all (&optional restart?)
  (when restart?
    (setf *todo-list* nil)
    (clrhash *all-pos-hash*))
  (unless *todo-list*
    (push-todo *initial-pos*))
  (loop for pos = (pop *todo-list*)
     for (y . x) = pos
     while pos do
       (format t "~s " pos)
       (when (get-image pos)
         (push-todo (cons (1- y) x))
         (push-todo (cons (1+ y) x))
         (push-todo (cons y (1- x)))
         (push-todo (cons y (1+ x))))))

(defun reduce-files ()
  (let ((paths (directory "www/images/*.png")))
    (dolist (p paths)
      (let ((image (ignore-errors
                     ;; read-png-file fails sometimes
                     (opticl:resize-image (opticl:read-png-file p) 512 512)))
            (op (make-pathname
                 :directory '(:relative "www" "small-images")
                 :defaults p)))
        (if image
            (opticl:write-png-file op image)
            (write-bytes-to-file (read-bytes-from-file p) op))))))

(defun index-files ()
  (let ((paths (directory "www/images/*.png"))
        (poses nil)
        miny maxy minx maxx)
    (dolist (p paths)
      (let* ((pos (parse-image-file-name (pathname-name p)))
             (y (car pos))
             (x (cdr pos)))
        (push pos poses)
        (cond ((null miny)
               (setf miny y maxy y minx x maxx x))
              (t (when (< y miny) (setf miny y))
                 (when (> y maxy) (setf maxy y))
                 (when (< x minx) (setf minx x))
                 (when (> x maxx) (setf maxx x))))))
    (let ((a (make-array (list (1+ (- maxy miny)) (1+ (- maxx minx)))
                         :element-type 'bit)))
      (dolist (pos poses)
        (let ((y (car pos))
              (x (cdr pos)))
          (setf (aref a (- y miny) (- x minx)) 1)))
      (values a minx miny maxx maxy))))

(defun write-html-index (&optional (size 256))
  (multiple-value-bind (a minx miny) (index-files)
    (with-open-file (s "www/index.html"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (format s "<html>~%<head>~%<title>XKCD Click and Drag</title>~%</head>~%")
      (format s "<body>~%<p><a href='http://xkcd.com/1110/'>xkcd.com/1110</a> (scroll down for image)</p>~%")
      (write-html-table s a minx miny size)
      (format s "</body>~%</html>~%"))))

(defun write-html-table (s a minx miny size)
  (format s "<table cellspacing='0' cellpadding='0'>~%")
  (let* ((h (array-dimension a 0))
         (w (array-dimension a 1)))
    (loop for y from (1- h) downto 0 do
         (format s "  <tr>~%")
         (dotimes (x w)
           (let* ((pos (cons (+ y miny) (+ x minx)))
                  (bit (aref a y x))
                  (png (image-file-name pos))
                  (file (and (eql bit 1) ;change (eql bit 1) to t to use white.png
                        (format nil
                                "<a target='_blank' href='~a'><img src='~a' alt='~s' width='~d' height='~d'/></a>"
                                (format nil "images/~a" png)
                                (format nil "small-images/~a" png)
                                pos size size))))
             (format s "    <td>~a</td>~%" (or file "&nbsp;"))))
         (format s "  </tr>~%")))
  (format s "</table>~%"))

(defun pull-reduce-and-index ()
  (pull-all t)
  (reduce-files)
  (index-files))
