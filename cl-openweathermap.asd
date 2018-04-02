(defsystem :cl-openweathermap
  :name "Open Weather Map"
  :version "0.1"
  :maintainer "Flot Facetieux"
  :author "Flot Facetieux"
  :licence "MIT"
  :description "Get Weather Report from Open Weather Map web site"
  :serial t
  :components ((:file "package")
               (:file "openweathermap")
	       (:file "test"))
  :depends-on (:alexandria :jonathan :dexador :local-time :lisp-unit))
