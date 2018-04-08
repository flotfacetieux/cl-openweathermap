# Cl-OpenWeatherMap
cl-openweathermap allows you to retrieve the weather from [OpenWeatherMap Web site][1]

## Usage

Before starting you need specify your API key :

```
(setf cl-owm:*api-key* "YOUR_BIG_SECRET")
```

### First : define a location

You can modify the default standard units (see [documentation][2]) :
  - "standard" 
  - "metric"
  - "imperial"


#### By city name

```
(cl-owm:build-uri-by-city "London")
or
(cl-owm:build-uri-by-city "London" "uk" :units "imperial")
; "http://api.openweathermap.org/data/2.5/weather?q=London,uk&units=metric&appid=YOUR_BIG_SECRET"
```

#### By city id

```
(cl-owm:build-uri-by-id 2643743 :units "standard")
;"http://api.openweathermap.org/data/2.5/weather?id=2643743&units=imperial&appid=YOUR_BIG_SECRET"
```

#### By coords

```
(cl-owm:build-uri-by-coords 51.51 0.13)
;"http://api.openweathermap.org/data/2.5/weather?lat=51.51&lon=0.13&appid=YOUR_BIG_SECRET"
```

### Next : perform the request

```
(cl-owm:request (cl-owm:build-uri-by-coords 51.51 0.13))
```

### Next : parse the response

```
(cl-owm:parse-response (cl-owm:request (cl-owm:build-uri-by-coords 51.51 0.13)))
```

### Finaly : Display some data

```
(defparameter *weather-report* (cl-owm:parse-response (cl-owm:request (cl-owm:build-uri-by-coords 51.51 0.13))))
(cl-owm:wr-temperature *weather-report*)
; => 15
(cl-owm:wr-humidity *weather-report*)
; => 81
(cl-owm:wr-pressure *weather-report*)
; => 1012
(cl-owm:wr-wind-speed *weather-report*)
; => 4.1
```

## Installation
Clone the repository to your local-projects quicklisp directory :
```
cd ~/quicklisp/local-projects
git clone https://github.com/flotfacetieux/cl-openweathermap
```

Load with quicklisp :
```
(ql:quickload :cl-openweathermap)
```

## Requirements

- You need an API key to use it

## API

## Refs
[1]:http://openweathermap.org
[2]:http://openweathermap.org/current#data