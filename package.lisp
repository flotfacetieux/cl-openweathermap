(defpackage cl-openweathermap
  (:use #:cl)
  (:nicknames :cl-owm)
  (:export #:*api-key*
	   #:*base-uri*
	   #:*units*
	   #:location
	   #:define-location
	   #:update-location
	   #:location-id
	   #:location-city-name
	   #:location-country-code
	   #:location-lat
	   #:location-lon
	   #:location-weather-report
	   ;;
	   #:weather-data
	   #:wr-temperature
	   #:wr-pressure
	   #:wr-humidity
	   #:wr-date-time
	   #:wr-wind-speed))

(defpackage cl-openweathermap-tests
  (:use #:cl #:lisp-unit)
  (:nicknames :cl-owm-tests)
  (:import-from :cl-owm
		:*api-key*
		:*base-uri*
		:location
		:define-location
		:update-location
		:weather-data)
  (:export :do-tests))

