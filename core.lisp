(defpackage cl-openweathermap
  (:use #:cl)
  (:nicknames :cl-owm)
  (:import-from :alexandria
		#:assoc-value)
  (:export #:*api-key*
	   #:*base-uri*
	   #:build-uri-by-city
	   #:build-uri-by-id
	   #:build-uri-by-coords
	   #:request
	   #:parse-response
	   ;;
	   #:weather-data
	   #:wr-temperature
	   #:wr-pressure
	   #:wr-humidity
	   #:wr-date-time
	   #:wr-wind-speed))

(in-package :cl-owm)

(defvar *api-key* nil)

(defparameter *base-uri*
    "http://api.openweathermap.org/data/2.5/weather")

(defparameter *history-uri*
    "http://history.openweathermap.org/data/2.5/history")

(defun build-uri (type units api-key)
  (when (not api-key)
    (error "Empty API key. Please setup *api-key* with a valid key.~
 See http://openweathermap.org/faq for more info."))
  (format nil "~a?~a~a&appid=~a"
	  *base-uri*
	  type
	  (if units (format nil "&units=~a" units) "")
	  api-key))

(defun build-uri-by-city (name &optional country-code &key units) 
  (build-uri (format nil "q=~a~a" name (if country-code
					   (format nil ",~a"
						   country-code)
					   ""))
	     units
	     *api-key*))

(defun build-uri-by-id (id &key units) 
  (build-uri (format nil "id=~a" id)
	     units
	     *api-key*))

(defun build-uri-by-coords (lat lon &key units) 
  (build-uri (format nil "lat=~a&lon=~a" lat lon)
	     units
	     *api-key*))

(defun request (uri)
  (handler-bind ((dex:http-request-unauthorized
		  #'dex:ignore-and-continue))
    (multiple-value-bind (response status-code)
	(dex:request uri)
      (case status-code
	(200 response)
	(401 (error "Invalide API key. Please setup *api-key*~
 with a valid key. See http://openweathermap.org/faq for more info."))
	(otherwise (dex:http-request-failed
		    status-code
		    :body response
		    :uri uri))))))

(defun parse-response (response)
  (jonathan:parse response :as :alist))

(defun deep-data-search (data key-list)
  (if (= 1 (length key-list))
      (assoc-value data (first key-list) :test #'string=)
      (assoc-value (deep-data-search data (rest key-list))
		   (first key-list)
		   :test #'string=)))

(defun weather-data (weather-report key-list)
  "Extract data from Weather Report"
  (deep-data-search weather-report key-list))

(defun wr-temperature (weather-report)
  "Return Weather Report Temperature"
  (weather-data weather-report '("temp" "main")))

(defun wr-pressure (weather-report)
  "Return Weather Report Pressure"
  (weather-data weather-report '("pressure" "main")))

(defun wr-humidity (weather-report)
  "Return Weather Report Humidity"
  (weather-data weather-report '("humidity" "main")))

(defun wr-date-time (weather-report)
  "Return Weather Report DateTime in universal format"
  (let ((wdata (weather-data weather-report '("dt"))))
    (local-time:timestamp-to-universal
     (local-time:unix-to-timestamp wdata))))
