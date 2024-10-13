Lubridate Lab
================
Tepi
10/6/2024

## Laboratorio de la libreria Lubridate

``` r
##cargar librerias
library(nycflights13)
library(lubridate)
library(dplyr)
```

## Resuelva las siguientes preguntas:

### Ejercicio 1: Convertir columnas de hora en fecha-hora

Problema: Convierte las columnas dep_time (hora de salida) y arr_time
(hora de llegada) en objetos de tipo datetime usando make_datetime() de
lubridate. Recuerda que estas columnas están en formato militar (HHMM).

Ayuda: Investiga la funcion matematica de modulo de r.

``` r
# Cargar el dataset de vuelos
flights <- nycflights13::flights

# Crear una función para convertir la hora en formato HHMM a datetime
convert_to_datetime <- function(year, month, day, time) {
  hours <- time %/% 100 # División entera para obtener las horas
  minutes <- time %% 100 # Resto para obtener los minutos
  make_datetime(year, month, day, hours, minutes)
}

# Aplicar la función a las columnas dep_time y arr_time
flights <- flights %>%
  mutate(
    dep_datetime = convert_to_datetime(year, month, day, dep_time),
    arr_datetime = convert_to_datetime(year, month, day, arr_time)
  )

# Mostrar las primeras filas para verificar la conversión
head(flights[, c("dep_time", "dep_datetime", "arr_time", "arr_datetime")])
```

    ## # A tibble: 6 × 4
    ##   dep_time dep_datetime        arr_time arr_datetime       
    ##      <int> <dttm>                 <int> <dttm>             
    ## 1      517 2013-01-01 05:17:00      830 2013-01-01 08:30:00
    ## 2      533 2013-01-01 05:33:00      850 2013-01-01 08:50:00
    ## 3      542 2013-01-01 05:42:00      923 2013-01-01 09:23:00
    ## 4      544 2013-01-01 05:44:00     1004 2013-01-01 10:04:00
    ## 5      554 2013-01-01 05:54:00      812 2013-01-01 08:12:00
    ## 6      554 2013-01-01 05:54:00      740 2013-01-01 07:40:00

## Ejercicio 2: Duracion del vuelo

Calcula el tiempo de vuelo total en minutos entre las columnas dep_time
y arr_time que calculaste en el primer Ejercicio.

``` r
# Calcular el tiempo de vuelo en minutos
flights <- flights %>%
  mutate(
    flight_duration = as.numeric(difftime(arr_datetime, dep_datetime, units = "mins"))
  )

# Mostrar las primeras filas para verificar la duración del vuelo
head(flights[, c("dep_datetime", "arr_datetime", "flight_duration")])
```

    ## # A tibble: 6 × 3
    ##   dep_datetime        arr_datetime        flight_duration
    ##   <dttm>              <dttm>                        <dbl>
    ## 1 2013-01-01 05:17:00 2013-01-01 08:30:00             193
    ## 2 2013-01-01 05:33:00 2013-01-01 08:50:00             197
    ## 3 2013-01-01 05:42:00 2013-01-01 09:23:00             221
    ## 4 2013-01-01 05:44:00 2013-01-01 10:04:00             260
    ## 5 2013-01-01 05:54:00 2013-01-01 08:12:00             138
    ## 6 2013-01-01 05:54:00 2013-01-01 07:40:00             106

## Ejercicio 3: Extraer componentes de fechas

Extrae el dia de la semana y la hora en que salieron los aviones y
guardalos en las variables `dep_day_of_week` y `dep_hour`.

``` r
# Extraer el día de la semana y la hora de salida
flights <- flights %>%
  mutate(
    dep_day_of_week = wday(dep_datetime, label = TRUE, abbr = TRUE), # Día de la semana
    dep_hour = hour(dep_datetime) # Hora de salida
  )

# Mostrar las primeras filas para verificar las nuevas variables
head(flights[, c("dep_datetime", "dep_day_of_week", "dep_hour")])
```

    ## # A tibble: 6 × 3
    ##   dep_datetime        dep_day_of_week dep_hour
    ##   <dttm>              <ord>              <int>
    ## 1 2013-01-01 05:17:00 Tue                    5
    ## 2 2013-01-01 05:33:00 Tue                    5
    ## 3 2013-01-01 05:42:00 Tue                    5
    ## 4 2013-01-01 05:44:00 Tue                    5
    ## 5 2013-01-01 05:54:00 Tue                    5
    ## 6 2013-01-01 05:54:00 Tue                    5

## Ejercicio 4: Crear nuevas columnas con el día de la semana y la semana del año

Problema: Usando la columna `time_hour`, crea una nueva columna que
indique el día de la semana y otra que indique la semana del año en la
que ocurrió el vuelo.

Ayuda: Invesitga la funcion wday de lubridate.

``` r
# Crear nuevas columnas para el día de la semana y la semana del año
flights <- flights %>%
  mutate(
    day_of_week = wday(time_hour, label = TRUE, abbr = TRUE),  # Día de la semana
    week_of_year = week(time_hour)  # Semana del año
  )

# Mostrar las primeras filas para verificar las nuevas columnas
head(flights[, c("time_hour", "day_of_week", "week_of_year")])
```

    ## # A tibble: 6 × 3
    ##   time_hour           day_of_week week_of_year
    ##   <dttm>              <ord>              <dbl>
    ## 1 2013-01-01 05:00:00 Tue                    1
    ## 2 2013-01-01 05:00:00 Tue                    1
    ## 3 2013-01-01 05:00:00 Tue                    1
    ## 4 2013-01-01 05:00:00 Tue                    1
    ## 5 2013-01-01 06:00:00 Tue                    1
    ## 6 2013-01-01 05:00:00 Tue                    1

## Ejercicio 5: Encontrar los vuelos que salieron los fines de semana

Problema: Filtra los vuelos que despegaron un sábado o domingo y
devuelve el total de vuelos en fines de semana.

``` r
# Filtrar vuelos que salieron en fin de semana
weekend_flights <- flights %>%
  filter(day_of_week %in% c("Sat", "Sun"))

# Contar el total de vuelos en fines de semana
total_weekend_flights <- nrow(weekend_flights)

# Mostrar el total de vuelos en fines de semana
total_weekend_flights
```

    ## [1] 85077
