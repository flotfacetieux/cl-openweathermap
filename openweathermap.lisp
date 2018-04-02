(in-package :cl-openweathermap)

;;(defparameter *api-key* "76687960f8a8453d63a405f59a52dc10")

;; Global vars
(defvar *api-key* nil)

(defvar *units* nil)
;;(defparameter *units* "metric")

(defparameter *base-uri*
    "http://api.openweathermap.org/data/2.5/weather")

(defparameter *history-uri*
    "http://history.openweathermap.org/data/2.5/history")

;; Build request URI functions
(defun build-request-uri-from-coords (&key lat lon)
  (format nil
	  "~a?lat=~a&lon=~a~a&appid=~a"
	  *base-uri*
	  lat
	  lon
	  (if *units* (format nil "&units=~a" *units*) "")
	  *api-key*))

(defun build-request-uri-from-id (id)
  (format nil
	  "~a?id=~a~a&appid=~a"
	  *base-uri*
	  id
	  (if *units* (format nil "&units=~a" *units*) "")
	  *api-key*))

(defun build-request-uri-from-city (city &key country-code)
  (format nil "~a?q=~a~a~a&appid=~a"
	  *base-uri*
	  city
	  (if country-code (format nil ",~a" country-code) "")
	  (if *units* (format nil "&units=~a" *units*) "")
	  *api-key*))

;; HTTP Request and parse
(defun request (uri)
  (handler-bind ((dex:http-request-unauthorized #'dex:ignore-and-continue))
      (dex:request uri)))

(defun request-and-parse (uri)
  (multiple-value-bind (response status-code)
      (request uri)
    ;; (if (= status-code 200)
    ;; 	(jonathan:parse response :as :alist)
    ;; 	(dex:http-request-failed status-code :body response :uri uri))
    (case status-code
      (200 (jonathan:parse response :as :alist))
      (401 (error "Invalide API key. Please setup *api-key* with a valid key. See http://openweathermap.org/faq for more info."))
      (otherwise (dex:http-request-failed status-code :body response :uri uri)))))

;; Location object to store weather report
(defclass location ()
  ((latitude :accessor location-lat
	     :initarg :lat)
   (longitude :accessor location-lon
	      :initarg :lon)
   (id :accessor location-id
       :initarg :id)
   (city-name :accessor location-city-name
	      :initarg :city-name)
   (country-code :accessor location-country-code
		 :initarg :country-code)
   (weather-report :accessor location-weather-report
		   :initarg :weather-report)))

(defmacro define-location (&key lat lon id city-name country-code)
  `(make-instance 'location
		  ,@(when lat `(:lat ,lat)) 
		  ,@(when lon `(:lon ,lon))
		  ,@(when id `(:id ,id))
		  ,@(when city-name `(:city-name ,city-name))
		  ,@(when country-code `(:country-code ,country-code))))

(defun xor (exp1 exp2)
  (or (and exp1 exp2) (and (not exp1) (not exp2))))

(defmethod initialize-instance :after ((location location) &key)
  (when (not (xor (slot-boundp location 'latitude) (slot-boundp location 'longitude)))
    (error "Must supply both longitude AND latitude")))

(defmethod update-location ((location location))
  (when (not *api-key*)
    (error "Empty API key. Please setup *api-key* with a valid key.~
 See http://openweathermap.org/faq for more info."))
  (setf (location-weather-report location)
	(request-and-parse
	 (cond
	   ((slot-boundp location 'id)
	    (build-request-uri-from-id (location-id location)))
	   ((slot-boundp location 'city-name)
	    (build-request-uri-from-city
	     (location-city-name location)
	     :country-code (and (slot-boundp location 'country-code) (location-country-code location))))
	   ((and
	     (slot-boundp location 'latitude)
	     (slot-boundp location 'longitude))
	    (build-request-uri-from-coords
	     :lat (location-lat location)
	     :lon (location-lon location)))))))

;; Weather Report parsing functions
(defun assoc-value (data key)
  (alexandria:assoc-value data key :test #'string=))

(defun deep-data-search (data key-list)
  (if (= 1 (length key-list))
      (assoc-value data (first key-list))
      (assoc-value (deep-data-search data (rest key-list)) (first key-list))))

(defun weather-data (location key-list)
  "Extract data from Weather Report"
  (let ((wreport (location-weather-report location)))
    (deep-data-search wreport key-list)))

(defmethod wr-temperature ((location location))
  "Return Weather Report Temperature"
  (weather-data location '("temp" "main")))

(defmethod wr-pressure ((location location))
  "Return Weather Report Pressure"
  (weather-data location '("pressure" "main")))

(defmethod wr-humidity ((location location))
  "Return Weather Report Humidity"
  (weather-data location '("humidity" "main")))

(defmethod wr-date-time ((location location))
  "Return Weather Report DateTime in universal format"
  (let ((wdata (weather-data location '("dt"))))
    (local-time:timestamp-to-universal
     (local-time:unix-to-timestamp wdata))))

;; (decode-universal-time
;;  (local-time:timestamp-to-universal
;;   (local-time:unix-to-timestamp 1522668600)))

(defmethod wr-wind-speed ((location location))
  "Return Weather Report Wind Speed"
  (weather-data (location '("speed" "wind"))))
