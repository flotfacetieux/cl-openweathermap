(defpackage cl-openweathermap.utils
  (:use #:cl)
  (:nicknames :cl-owm.utils)
  (:import-from :cl-owm.core
		#:request
		#:parse-response)
  (:export #:location
	   #:location-uri
	   #:location-weather-report
	   #:update-location))

(in-package :cl-owm.utils)

;; Location object to store weather report

(defclass location ()
  ((uri :accessor location-uri
	:initarg :uri)
   (weather-report :accessor location-weather-report
		   :initarg :weather-report)))

(defmethod update-location ((location location))
  (setf (location-weather-report location)
	(parse-response (request (location-uri location)))))
