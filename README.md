# Cl-OpenWeatherMap
cl-openweathermap allows you to retrieve the weather from [OpenWeatherMap Web site][1]

## Usage

Before starting you need specify your API key :

```
(setf cl-owm:*api-key* "YOUR_BIG_SECRET")
```

and units you want to use ("standard", "metric", "imperial") (see [documentation][2]):

```
(setf cl-owm:*units* "metric")
```

- First : define a location

```
(cl-owm:define-location :city-name "London")
```

- Next : update location weather report

```
(cl-owm:update-location location)
```

- Finaly : Display some data
```
(cl-owm:wr-temperature location)
; => 15
(cl-owm:wr-humidity location)
; => 81
(cl-owm:wr-pressure location)
; => 1012
(cl-owm:wr-wind-speed location)
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