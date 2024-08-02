Laboratorio \#1 - Isaac Cyrman (20220289)
================

# Introducción

##### En este laboratorio, abordaremos varios problemas utilizando R para realizar tareas de procesamiento y análisis de datos. A lo largo de este documento, cargaremos archivos Excel, exportaremos data frames a Excel (xlsx), calcularemos la moda de vectores y leeremos un archivo de texto delimitado, explicando cada paso detalladamente.

# Problema 1:

##### Primero, cargamos las librerías necesarias para manejar y manipular los datos, incluyendo `readxl` para leer archivos Excel, `dplyr` para manipular data frames, `lubridate` para manejar fechas y `openxlsx` para exportar datos a excel.

``` r
library(readxl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate) # Para manejar fechas mas fácilmente.
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(openxlsx)
```

    ## Warning: package 'openxlsx' was built under R version 4.3.3

##### Definimos una función llamada process_file que toma como argumentos el nombre del archivo, una fecha base y el número de meses a agregar. Esta función lee el archivo Excel, calcula la fecha correspondiente, agrega una columna FECHA y selecciona las columnas específicas que nos interesan.

``` r
# Creamos una función para agregar la columna FECHA y seleccionamos las columnas deseadas.
process_file <- function(file_name, base_date, months_to_add) {
  data <- read_excel(file_name)
  # Incrementamos la fecha base en los meses correspondientes.
  date_value <- seq.Date(base_date, by = "month", length.out = 1) + months(months_to_add - 1)
  data <- data %>% 
    mutate(FECHA = date_value) %>%
    select(COD_VIAJE, CLIENTE, UBICACION, CANTIDAD, PILOTO, Q, CREDITO, UNIDAD, FECHA)
  return(data)
}
```

##### Usamos la función process_file para leer una serie de archivos Excel correspondientes a cada mes del año 2023. La fecha base es el 1 de enero de 2023, y para cada archivo se incrementa el mes correspondiente.

``` r
# Fecha base (primer día del primer mes).
base_date <- as.Date("2023-01-01")

# Leemos archivos, agregamos la columna FECHA y seleccionamos las columnas deseadas.
X01_2023 <- process_file("01-2023.xlsx", base_date, 1)
X02_2023 <- process_file("02-2023.xlsx", base_date, 2)
X03_2023 <- process_file("03-2023.xlsx", base_date, 3)
X04_2023 <- process_file("04-2023.xlsx", base_date, 4)
X05_2023 <- process_file("05-2023.xlsx", base_date, 5)
X06_2023 <- process_file("06-2023.xlsx", base_date, 6)
X07_2023 <- process_file("07-2023.xlsx", base_date, 7)
X08_2023 <- process_file("08-2023.xlsx", base_date, 8)
```

    ## New names:
    ## • `` -> `...10`

``` r
X09_2023 <- process_file("09-2023.xlsx", base_date, 9)
X10_2023 <- process_file("10-2023.xlsx", base_date, 10)
X11_2023 <- process_file("11-2023.xlsx", base_date, 11)
```

##### Combinamos todos los data frames resultantes en uno solo utilizando bind_rows de dplyr y mostramos el data frame combinado. El resultado de unir todos los data frames fue de 2,180 filas \* 9 columnas.

``` r
# Combinamos todos los dataframes en uno solo.
combined_data <- bind_rows(X01_2023, X02_2023, X03_2023, X04_2023, X05_2023, X06_2023, X07_2023, X08_2023, X09_2023, X10_2023, X11_2023)

# Ver el dataframe combinado.
print(combined_data)
```

    ## # A tibble: 2,180 × 9
    ##    COD_VIAJE CLIENTE   UBICACION CANTIDAD PILOTO     Q CREDITO UNIDAD FECHA     
    ##        <dbl> <chr>         <dbl>    <dbl> <chr>  <dbl>   <dbl> <chr>  <date>    
    ##  1  10000001 EL PINCH…     76002     1200 Ferna… 300        30 Camio… 2023-01-01
    ##  2  10000002 TAQUERIA…     76002     1433 Hecto… 358.       90 Camio… 2023-01-01
    ##  3  10000003 TIENDA L…     76002     1857 Pedro… 464.       60 Camio… 2023-01-01
    ##  4  10000004 TAQUERIA…     76002      339 Angel…  84.8      30 Panel  2023-01-01
    ##  5  10000005 CHICHARR…     76001     1644 Juan … 411        30 Camio… 2023-01-01
    ##  6  10000006 UBIQUO L…     76001     1827 Luis … 457.       30 Camio… 2023-01-01
    ##  7  10000007 CHICHARR…     76002     1947 Ismae… 487.       90 Camio… 2023-01-01
    ##  8  10000008 TAQUERIA…     76001     1716 Juan … 429        60 Camio… 2023-01-01
    ##  9  10000009 EL GALLO…     76002     1601 Ismae… 400.       30 Camio… 2023-01-01
    ## 10  10000010 CHICHARR…     76002     1343 Ferna… 336.       90 Camio… 2023-01-01
    ## # ℹ 2,170 more rows

##### Exportamos el dataset conformado por todos los dataframes mediante la función write.xlsx().

``` r
write.xlsx(combined_data, "excel_combinado.xlsx")
```

# Problema 2:

##### Para este problema, definimos una función llamada moda que calcula la moda de un vector. La función encuentra los valores únicos del vector, cuenta la frecuencia de cada valor y devuelve el valor con la frecuencia más alta.

``` r
# Definimos la función para calcular la moda
moda <- function(x) {
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}
```

##### Creamos una lista de vectores con varios números para aplicarles la función moda.

``` r
# Creamos una lista de vectores
lista_vectores <- list(
  c(1, 2, 2, 3, 3, 3, 4, 4),
  c(5, 5, 6, 6, 7, 7, 7),
  c(8, 9, 9, 10, 10, 10, 11)
)
```

##### Utilizamos lapply para aplicar la función moda a cada vector en la lista y mostramos los resultados de manera agradable para el lector, indicando la moda de cada vector.

``` r
# Utilizamos lapply para encontrar la moda de cada vector en la lista.
modas <- lapply(lista_vectores, moda)

# Mostramos los resultados de forma agradable para el usuario.
for (i in seq_along(modas)) {
  cat(paste("Vector #", i, ": Moda =", modas[[i]], "\n", sep = ""))
}
```

    ## Vector #1: Moda =3
    ## Vector #2: Moda =7
    ## Vector #3: Moda =10

# Problema 3:

##### Para este problema, instalamos y cargamos el paquete readr para leer archivos de texto. Utilizamos read_delim para leer el archivo INE_PARQUE_VEHICULAR_080219.txt especificando el delimitador adecuado (Para este caso en especifico fue el siguiente: ” \| “).

``` r
# Instalamos y cargamos el paquete readr.
library(readr)

# Leemos el archivo con read_delim especificando el delimitador adecuado.
archivo <- "INE_PARQUE_VEHICULAR_080219.txt"
datos <- read_delim(archivo, delim = "|")
```

    ## New names:
    ## • `` -> `...11`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 2435294 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "|"
    ## chr (8): MES, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO, MODELO_VEHICULO, LINEA_...
    ## dbl (2): ANIO_ALZA, CANTIDAD
    ## lgl (1): ...11
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Mostramos las primeras filas del dataframe para verificar la lectura.
print(head(datos))
```

    ## # A tibble: 6 × 11
    ##   ANIO_ALZA MES   NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ##       <dbl> <chr> <chr>               <chr>            <chr>          
    ## 1      2007 05    HUEHUETENANGO       "HUEHUETENANGO"  2007           
    ## 2      2007 05    EL PROGRESO         "EL JICARO"      2007           
    ## 3      2007 05    SAN MARCOS          "OCOS"           2007           
    ## 4      2007 05    ESCUINTLA           "SAN JOS\xc9"    2006           
    ## 5      2007 05    JUTIAPA             "MOYUTA"         2007           
    ## 6      2007 05    GUATEMALA           "FRAIJANES"      1997           
    ## # ℹ 6 more variables: LINEA_VEHICULO <chr>, TIPO_VEHICULO <chr>,
    ## #   USO_VEHICULO <chr>, MARCA_VEHICULO <chr>, CANTIDAD <dbl>, ...11 <lgl>
