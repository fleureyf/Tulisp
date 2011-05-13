;;;
;;; TP Group I1G2, ENSEIRB-MATMECA
;;; created 31 March 2009, last edition 5 April 2011
;;;  http://www.enseirb.fr/~gloess/enseignement/CL/2010_2011/
;;;
;;; Note: 5 April 2011: added MONTH-DATE-YEAR-STRING  and TWO-DIGIT-STRING
;;;                      functions, and improved UNIVERSAL-TIME-STRING,
;;;                      so that the time always has the form "hh:mm:ss".
;;;
;;; Note: 7 March 2011: adapted from CMU to SBCL and fixed EVAL-WHEN
;;;                      according to HyperSpec recommendation.
;;;
;;; Purpose: Provide time utilities, such as, UNIVERSAL-TIME-STRING,
;;;           which yields a string with the date and time in clear.
;;;
(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SBCL" "sbcl"))         ; defines SBCL-REQUIRE !!!

(defun two-digit-string (0..99)
  "Assuming 0..99: 0..99,
returns a two digit string, adding a leading 0, when 0..99 < 10."
  (format nil
	  (if (< 0..99 9)
	      "0~a"
	      "~a")
	  0..99))
;;;  (two-digit-string 5)     ; => "05"
;;; (two-digit-string 17)     ; => "17"

(defun universal-time-string ()
  "Returns a readable date and time, such as 31 March 2009 at 14:18:57, as a string."
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore daylight-p zone))
    (format nil "~A ~A ~A ~A at ~A:~A:~A"
	    (case day
	      (0 "Monday")
	      (1 "Tuesday")
	      (2 "Wednesday")
	      (3 "Thursday")
	      (4 "Friday")
	      (5 "Saturday")
	      (6 "Sunday")
	      (t "??"))
	    date
	    (case month
	      (1 "January")
	      (2 "February")
	      (3 "March")
	      (4 "April")
	      (5 "May")
	      (6 "June")
	      (7 "July")
	      (8 "August")
	      (9 "September")
	      (10 "October")
	      (11 "November")
	      (12 "December")
	      (t "???"))
	    year (two-digit-string hour) (two-digit-string minute) (two-digit-string second))))
;;; (universal-time-string)   ; => "Tuesday 5 April 2011 at 20:25:31"
;;; (universal-time-string)   ; => "Tuesday 5 April 2011 at 20:28:01"

(defun month-date-year-string ()
  "Assuming we are between year 1000 and year 9999,
Returns the date under the form \"MM-DD-YYYY\",
where MM is the two digit month number, DD the two digit date number,
and YYYY is the four digit year number.
Note: beware of year 10000 bug! People should prepare during year 9999."
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore second minute hour day daylight-p zone))
    (format nil "~a-~a-~a" (two-digit-string month) (two-digit-string date) year)))

;;; (month-date-year-string)    ; => "04-05-2011"    ; on 5 April 2011!

(provide "TIME")