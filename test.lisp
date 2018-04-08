(defpackage cl-openweathermap.tests
  (:use #:cl #:lisp-unit)
  (:nicknames :cl-owm.tests)
  (:import-from :cl-owm
		:*api-key*
		:*base-uri*
		:build-uri-by-city
		:weather-data)
  (:import-from :cl-owm.utils
		:location
		:update-location
		:location-weather-report)
  (:export :do-tests))

(in-package :cl-owm.tests)

;; Test with openweathermap sample URI

(defparameter *london-city-name* "london")
(defparameter *london-country-code* "uk")

(defparameter *london-weather-report*
  '(("cod" . 200) ("name" . "London") ("id" . 2643743)
    ("sys" ("sunset" . 1485794875) ("sunrise" . 1485762037) ("country" . "GB")
     ("message" . 0.0103) ("id" . 5091) ("type" . 1))
    ("dt" . 1485789600) ("clouds" ("all" . 90))
    ("wind" ("deg" . 80) ("speed" . 4.1)) ("visibility" . 10000)
    ("main" ("temp_max" . 281.15) ("temp_min" . 279.15) ("humidity" . 81)
     ("pressure" . 1012) ("temp" . 280.32))
    ("base" . "stations")
    ("weather"
     (("icon" . "09d") ("description" . "light intensity drizzle")
      ("main" . "Drizzle") ("id" . 300)))
    ("coord" ("lat" . 51.51) ("lon" . 0.13))))

(define-test test-request-by-name
  (let ((loc1 (make-instance
	       'location
	       :uri (build-uri-by-city *london-city-name*)))
	(loc2 (make-instance
	       'location
	       :uri (build-uri-by-city *london-city-name*
				       *london-country-code*))))
      (update-location loc1)
      (update-location loc2)
      (assert-equalp *london-city-name*
		    (weather-data (location-weather-report loc1)
				  '("name")))
      (assert-equalp *london-city-name*
		    (weather-data (location-weather-report loc1)
				  '("name")))))

(define-test test-temperature-report
  (assert-true (weather-data *london-weather-report*
			     '("temp" "main"))))

(define-test test-humidity-report
    (assert-true (weather-data *london-weather-report*
			       '("humidity" "main"))))

(defun do-tests ()
  (setq *print-failures* t)
  (setq *print-errors* t)
  (setf *api-key* "b6907d289e10d714a6e88b30761fae22"
	*base-uri* "http://samples.openweathermap.org/data/2.5/weather")
  (run-tests :all :cl-owm.tests)
  (setf *api-key* nil
	*base-uri* "http://api.openweathermap.org/data/2.5/weather"))
