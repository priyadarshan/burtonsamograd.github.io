(eval-when (:compile-toplevel :load-toplevel)
  (asdf:load-system :cl-cffi-gtk))

(defpackage :gtk-tutorial
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :gtk-tutorial)


(defstruct line
  x1 y1 x2 y2 (alpha 1.0))


(defparameter *w* 1280)
(defparameter *h* 1024)
(defparameter *lines* nil)
(defparameter *time* 0)
(defparameter *phase* 0)

(defstruct task fn start end period)
(defparameter *task-queue* ())
(defparameter *task-frame* 0)
(defparameter *task-frames-per-second* 24)

(defun add-task (fn &key start end (period 1))
  (let ((start (+ *task-frame* (if start start 0))))
    (setf *task-queue* (sort (cons (make-task :fn fn
					      :start start
					      :end (if end (+ start end) most-positive-fixnum)
					      :period period)
				   *task-queue*)
			     #'< :key #'task-start))))
(defun run-tasks ()
  (let (ready-queue)
    (mapc (lambda (task)
	    (when (<= (task-start task) *task-frame*)
	      (push task ready-queue)))
	  *task-queue*)

    (mapc (lambda (task)
	    (funcall (task-fn task))
	    (setf (task-start task) (+ *task-frame*
				       (let ((period (task-period task)))
					 (if (compiled-function-p period)
					     (funcall period)
					     period))))) ready-queue)

    (setf *task-queue* (remove-if (lambda (task)
				    (> *task-frame* (task-end task))) *task-queue*))
  (incf *task-frame*)))

(add-task (lambda ()
	    (add-task (let ((x (random *w*))
			    (y (random *h*)))
			(lambda ()
			  (setf x (random *w*))
			  (push (make-line :x1 x :y1 y :x2 (+ x 16) :y2 (+ y 100) :alpha 1) *lines*)))
		      :end 20))
	  :period (lambda () (random 20)) :end 10000)

(add-task (lambda ()
	    (flet ((j (x n)
		     (+ x (+ (- n) (random (* 2 (1+ n)))))))
	      (do ((i 0 (incf i 32)))
		  ((> i *w*))
		(push (make-line :x1 (j i 4)
				 :y1 0
				 :x2 (j i 4)
				 :y2 *h*
				 :alpha (+ .1 (random .25))) *lines*)
		(push (make-line :x1 0
				 :y1 (j i 4)
				 :x2 *w*
				 :y2 (j i 4)
				 :alpha (+ .1 (random .25))) *lines*))))
	  :period 3 :end 10000)

(defun update (cr)
  (setf *lines*
	(remove-if #'null
		   (mapcar (lambda (line) 
			     (cairo-set-source-rgba cr 0 0 0 (line-alpha line))
			     (cairo-move-to cr (line-x1 line) (line-y1 line))
			     (cairo-line-to cr (line-x2 line) (line-y2 line))
			     (cairo-stroke cr)
			     (decf (line-alpha line) (random .1))
			     (if (< (line-alpha line) 0)
				 nil
				 line)) *lines*)))
  (let ((start-time (get-internal-real-time)))
    (run-tasks)
    (handler-case 
	(sleep (- (/ 1 *task-frames-per-second*)
		  (/ (- (get-internal-real-time) start-time) internal-time-units-per-second) ))
      (error (e) (declare (ignore e))
	     (print "frame took too long!")))))


(let ((surface nil))
  (defun example-drawing ()
    (within-main-loop
      (let* ((w *w*)
	     (h *h*)
	     (window (make-instance 'gtk-window
				    :type :toplevel
				    :title "Example Drawing"))
	     (frame (make-instance 'gtk-frame
				   :shadow-type :in))
	     (area (make-instance 'gtk-drawing-area
				  :width-request w
				  :height-request h)))
	(g-signal-connect window "destroy"
			  (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Signals used to handle the backing surface
	(g-signal-connect area "draw"
			  (lambda (widget cr)
			    (let ((cr (pointer cr)))
			      (update cr)
			      (cairo-destroy cr)
			      (gtk-widget-queue-draw-area widget 0 0 w h)
			      +gdk-event-propagate+)))
	(g-signal-connect area "configure-event"
			  (lambda (widget event)
			    (declare (ignore event))
			    (when surface
			      (cairo-surface-destroy surface))
			    (setf surface (gdk-window-create-similar-surface
					   (gtk-widget-window widget)
					   :color
					   (gtk-widget-get-allocated-width widget)
					   (gtk-widget-get-allocated-height widget)))
			    ;; Clear surface
			    (let ((cr (cairo-create surface)))
			      (cairo-set-source-rgb cr 1.0 1.0 1.0)
			      (cairo-paint cr)
			      (cairo-destroy cr))
			    (format t "leave event 'configure-event'~%")
			    +gdk-event-stop+))
        (gtk-container-add frame area)
        (gtk-container-add window frame)
        (gtk-widget-show-all window)))))

