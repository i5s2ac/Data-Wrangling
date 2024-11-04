# Librerias
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

data <- read_csv("c1.csv")
View(data)

# Limpieza de Datos

# Convertir la columna 'Fecha' al formato de fecha correcto
data$Fecha <- as.Date(data$Fecha, format = "%d-%m-%y")
head(data$Fecha)

# Camion 

data$Camion_5 <- gsub("Q-", "", data$Camion_5)
data$Camion_5 <- gsub("Q", "", data$Camion_5)
data$Camion_5[data$Camion_5 == ""] <- NA  # Asigna NA a valores vacíos
data$Camion_5 <- as.numeric(data$Camion_5)
class(data$Camion_5)
colnames(data)[colnames(data) == "Camion_5"] <- "Camion"

# Pickup

data$Pickup <- gsub("Q-", "", data$Pickup)
data$Pickup <- gsub("Q", "", data$Pickup)
data$Pickup[data$Pickup == ""] <- NA  # Asigna NA a valores vacíos
data$Pickup <- as.numeric(data$Pickup)
class(data$Pickup)

# Moto

data$Moto <- gsub("Q-", "", data$Moto)
data$Moto <- gsub("Q", "", data$Moto)
data$Moto[data$Moto == ""] <- NA  # Asigna NA a valores vacíos
data$Moto <- as.numeric(data$Moto)
class(data$Moto)

# Factura

data$factura <- gsub("Q-", "", data$factura)
data$factura <- gsub("Q", "", data$factura)
data$factura[data$factura == ""] <- NA  # Asigna NA a valores vacíos
data$factura <- as.numeric(data$factura)
class(data$factura)

# directoCamion_5

data$directoCamion_5 <- gsub("Q-", "", data$directoCamion_5)
data$directoCamion_5 <- gsub("Q", "", data$directoCamion_5)
data$directoCamion_5[data$directoCamion_5 == ""] <- NA  # Asigna NA a valores vacíos
data$directoCamion_5 <- as.numeric(data$directoCamion_5)
class(data$directoCamion_5)
colnames(data)[colnames(data) == "directoCamion_5"] <- "directoCamion"

# directoPickup

data$directoPickup <- gsub("Q-", "", data$directoPickup)
data$directoPickup <- gsub("Q", "", data$directoPickup)
data$directoPickup[data$directoPickup == ""] <- NA  # Asigna NA a valores vacíos
data$directoPickup <- as.numeric(data$directoPickup)
class(data$directoPickup)

# directoMoto

data$directoMoto<- gsub("Q-", "", data$directoMoto)
data$directoMoto <- gsub("Q", "", data$directoMoto)
data$directoMoto[data$directoMoto == ""] <- NA  # Asigna NA a valores vacíos
data$directoMoto <- as.numeric(data$directoMoto)
class(data$directoMoto)

# fijoCamion_5

data$fijoCamion_5 <- gsub("Q-", "", data$fijoCamion_5)
data$fijoCamion_5 <- gsub("Q", "", data$fijoCamion_5)
data$fijoCamion_5[data$fijoCamion_5 == ""] <- NA  # Asigna NA a valores vacíos
data$fijoCamion_5 <- as.numeric(data$fijoCamion_5)
class(data$fijoCamion_5)
colnames(data)[colnames(data) == "fijoCamion_5"] <- "fijoCamion"

# fijoPickup

data$fijoPickup<- gsub("Q-", "", data$fijoPickup)
data$fijoPickup <- gsub("Q", "", data$fijoPickup)
data$fijoPickup[data$fijoPickup == ""] <- NA  # Asigna NA a valores vacíos
data$fijoPickup <- as.numeric(data$fijoPickup)
class(data$fijoPickup)

# fijoMoto

data$fijoMoto<- gsub("Q-", "", data$fijoMoto)
data$fijoMoto <- gsub("Q", "", data$fijoMoto)
data$fijoMoto[data$fijoMoto == ""] <- NA  # Asigna NA a valores vacíos
data$fijoMoto <- as.numeric(data$fijoMoto)
class(data$fijoMoto)

# Conversión de X a Booleanos

# 5-30 minutos

data$`5-30` <- gsub("x", "TRUE", data$`5-30`)
data$`5-30`[is.na(data$`5-30`) | data$`5-30` == ""] <- "FALSE"
data$`5-30` <- as.logical(data$`5-30`)
head(data$`5-30`)
class(data$`5-30`)

# 30 - 45 minutos

data$`30-45` <- gsub("x", "TRUE", data$`30-45`)
data$`30-45`[is.na(data$`30-45`) | data$`30-45` == ""] <- "FALSE"
data$`30-45` <- as.logical(data$`30-45`)
head(data$`30-45`)
class(data$`30-45`)

# 45 - 75 minutos

data$`45-75` <- gsub("x", "TRUE", data$`45-75`)
data$`45-75`[is.na(data$`45-75`) | data$`45-75` == ""] <- "FALSE"
data$`45-75` <- as.logical(data$`45-75`)
head(data$`45-75`)
class(data$`45-75`)

# 75 - 120 minutos

data$`75-120` <- gsub("x", "TRUE", data$`75-120`)
data$`75-120`[is.na(data$`75-120`) | data$`75-120` == ""] <- "FALSE"
data$`75-120` <- as.logical(data$`75-120`)
head(data$`75-120`)
class(data$`75-120`)

# Más de 120 minutos

data$`120+` <- gsub("x", "TRUE", data$`120+`)
data$`120+`[is.na(data$`120+`) | data$`120+` == ""] <- "FALSE"
data$`120+` <- as.logical(data$`120+`)
head(data$`120+`)
class(data$`120+`)

# Eliminar columnas que no sirven de nada 
data <- data[, !names(data) %in% c("...23", "...24", "...25", "...26", "...27", "...28")]
head(data)


### Exploración de datos

# *** SERVICIOS POR TIPO DE VEHICULO ***

# Iniciemos con saber con cuantos servicios cuenta cada tipo de vehiculo 
# que sirven para proporcionar servicios en Guatemala.

# Contar servicios excluyendo NA
Servicios_Camion <- data %>% filter(!is.na(Camion)) %>% nrow()
Servicios_Moto <- data %>% filter(!is.na(Moto)) %>% nrow()
Servicios_Pickup <- data %>% filter(!is.na(Pickup)) %>% nrow()

# Calcular el total de servicios combinados
TotalSevicios2017 <- Servicios_Camion + Servicios_Moto + Servicios_Pickup

# Calcular los porcentajes para cada tipo de servicio
TotalSeviciosCamiones <- (Servicios_Camion / TotalSevicios2017) * 100
TotalSeviciosMotos <- (Servicios_Moto / TotalSevicios2017) * 100
TotalSeviciosPickups <- (Servicios_Pickup / TotalSevicios2017) * 100

# Crear un dataframe con los porcentajes para cada tipo de servicio
servicios_data <- data.frame(
  Tipo = c("Camión", "Moto", "Pickup"),
  Porcentaje = c(TotalSeviciosCamiones, TotalSeviciosMotos, TotalSeviciosPickups)
)

# Verificar el dataframe creado
print(servicios_data)

# Crear la gráfica de barras
ggplot(servicios_data, aes(x = Tipo, y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentaje de Servicios por Tipo en 2017",
       x = "Tipo de Servicio",
       y = "Porcentaje (%)") +
  theme_minimal()

# *** FACTURACIÓN POR VEHICULO ***

dataAgrupada <- data %>%
  mutate(Tipo_Servicio = case_when(
    !is.na(Camion) ~ "Camión",
    !is.na(Moto) ~ "Moto",
    !is.na(Pickup) ~ "Pickup",
    TRUE ~ "Otro"  # En caso de que no aplique ninguno
  ))

# Agrupar por Tipo_Servicio y sumar la facturación para cada tipo de servicio
facturacion_por_tipo <- dataAgrupada %>%
  group_by(Tipo_Servicio) %>%
  summarise(Facturacion_Total = sum(factura), na.rm = TRUE)

# Verificar el resultado
print(facturacion_por_tipo)

Facturacion_Camiones <- 11461980
Facturacion_Motos <- 724032
Facturacion_Pickups <- 24502084

# Total de facturación en 2017
CantidadFactura2017 <- Facturacion_Camiones + Facturacion_Motos + Facturacion_Pickups

# Calcular los porcentajes de facturación para cada tipo de vehículo
PorcentajeFacturacionCamiones <- (Facturacion_Camiones / CantidadFactura2017) * 100
PorcentajeFacturacionMotos <- (Facturacion_Motos / CantidadFactura2017) * 100
PorcentajeFacturacionPickups <- (Facturacion_Pickups / CantidadFactura2017) * 100

# Crear un dataframe para los porcentajes de facturación
facturacion_data <- data.frame(
  Tipo = c("Camión", "Moto", "Pickup"),
  Porcentaje = c(PorcentajeFacturacionCamiones, PorcentajeFacturacionMotos, PorcentajeFacturacionPickups)
)

# Verificar el dataframe creado
print(facturacion_data)

# Crear la gráfica de barras
ggplot(facturacion_data, aes(x = Tipo, y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentaje de Facturación por Tipo de Vehículo en 2017",
       x = "Tipo de Vehículo",
       y = "Porcentaje de Facturación (%)") +
  theme_minimal()



# *** ANÁLISIS DE COSTOS FIJOS Y DIRECTOS ***

# Costo Directo Camiones
CostoDirecto_Camiones <- data %>% 
  select(directoCamion) %>% 
  filter(!is.na(directoCamion)) %>% 
  summarise(CostoCamionesDirecto = sum(directoCamion)) %>% 
  pull(CostoCamionesDirecto)

CostoDirecto_Camiones

# Costo Fijo Camiones
CostoFijo_Camiones <- data %>% 
  select(fijoCamion) %>% 
  filter(!is.na(fijoCamion)) %>% 
  summarise(CostoCamionesFijo = sum(fijoCamion)) %>% 
  pull(CostoCamionesFijo)

CostoFijo_Camiones

# Costo Directo Motos

CostoDirecto_Motos <- data %>% 
  select(directoMoto) %>% 
  filter(!is.na(directoMoto)) %>% 
  summarise(CostoMotosDirecto = sum(directoMoto)) %>% 
  pull(CostoMotosDirecto)

CostoDirecto_Motos

# Costo Fijo Motos
CostoFijo_Motos <- data %>% 
  select(fijoMoto) %>% 
  filter(!is.na(fijoMoto)) %>% 
  summarise(CostoMotosFijo = sum(fijoMoto)) %>% 
  pull(CostoMotosFijo)

CostoFijo_Motos

# Costo Directo Pickups

CostoDirecto_Pickups <- data %>% 
  select(directoPickup) %>% 
  filter(!is.na(directoPickup)) %>% 
  summarise(CostoPickupsDirecto = sum(directoPickup)) %>% 
  pull(CostoPickupsDirecto)

CostoDirecto_Pickups

# Costo Fijo Pickups
CostoFijo_Pickups <- data %>% 
  select(fijoPickup) %>% 
  filter(!is.na(fijoPickup)) %>% 
  summarise(CostoPickupsFijo = sum(fijoPickup)) %>% 
  pull(CostoPickupsFijo)

CostoFijo_Pickups

# *** ANALISIS SOBRE TIEMPOS ***

# Tiempos: Distancia del poste desde el punto de salida.

# Contar los servicios de camiones que tienen tiempos de 5 a 30 minutos
CountDe5a30_Camiones <- dataAgrupada %>% 
  select(`5-30`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Camión" & !is.na(`5-30`)) %>% 
  summarise(TiempoCamiones5a30 = sum(`5-30` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoCamiones5a30)

# Contar los servicios de camiones que tienen tiempos de 30 a 45 minutos
CountDe30a45_Camiones <- dataAgrupada %>% 
  select(`30-45`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Camión" & !is.na(`30-45`)) %>% 
  summarise(TiempoCamiones30a45 = sum(`30-45` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoCamiones30a45)

# Contar los servicios de camiones que tienen tiempos de 45 a 75 minutos
CountDe45a75_Camiones <- dataAgrupada %>% 
  select(`45-75`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Camión" & !is.na(`45-75`)) %>% 
  summarise(TiempoCamiones45a75 = sum(`45-75` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoCamiones45a75)

# Contar los servicios de camiones que tienen tiempos de 75 a 120 minutos
CountDe75a120_Camiones <- dataAgrupada %>% 
  select(`75-120`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Camión" & !is.na(`75-120`)) %>% 
  summarise(TiempoCamiones75a120 = sum(`75-120` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoCamiones75a120)

# Contar los servicios de camiones que tienen tiempos de 120+ minutos
CountDe120Plus_Camiones <- dataAgrupada %>% 
  select(`120+`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Camión" & !is.na(`120+`)) %>% 
  summarise(TiempoCamiones120Plus = sum(`120+` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoCamiones120Plus)

# Verificar los resultados
CountDe5a30_Camiones
CountDe30a45_Camiones
CountDe45a75_Camiones
CountDe75a120_Camiones
CountDe120Plus_Camiones

# Contar los servicios de motos que tienen tiempos de 5 a 30 minutos
CountDe5a30_Motos <- dataAgrupada %>% 
  select(`5-30`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Moto" & !is.na(`5-30`)) %>% 
  summarise(TiempoMotos5a30 = sum(`5-30` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoMotos5a30)

# Contar los servicios de motos que tienen tiempos de 30 a 45 minutos
CountDe30a45_Motos <- dataAgrupada %>% 
  select(`30-45`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Moto" & !is.na(`30-45`)) %>% 
  summarise(TiempoMotos30a45 = sum(`30-45` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoMotos30a45)

# Contar los servicios de motos que tienen tiempos de 45 a 75 minutos
CountDe45a75_Motos <- dataAgrupada %>% 
  select(`45-75`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Moto" & !is.na(`45-75`)) %>% 
  summarise(TiempoMotos45a75 = sum(`45-75` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoMotos45a75)

# Contar los servicios de motos que tienen tiempos de 75 a 120 minutos
CountDe75a120_Motos <- dataAgrupada %>% 
  select(`75-120`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Moto" & !is.na(`75-120`)) %>% 
  summarise(TiempoMotos75a120 = sum(`75-120` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoMotos75a120)

# Contar los servicios de motos que tienen tiempos de 120+ minutos
CountDe120Plus_Motos <- dataAgrupada %>% 
  select(`120+`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Moto" & !is.na(`120+`)) %>% 
  summarise(TiempoMotos120Plus = sum(`120+` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoMotos120Plus)

# Verificar los resultados
CountDe5a30_Motos
CountDe30a45_Motos
CountDe45a75_Motos
CountDe75a120_Motos
CountDe120Plus_Motos

# Contar los servicios de pickups que tienen tiempos de 5 a 30 minutos
CountDe5a30_Pickups <- dataAgrupada %>% 
  select(`5-30`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Pickup" & !is.na(`5-30`)) %>% 
  summarise(TiempoPickups5a30 = sum(`5-30` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoPickups5a30)

# Contar los servicios de pickups que tienen tiempos de 30 a 45 minutos
CountDe30a45_Pickups <- dataAgrupada %>% 
  select(`30-45`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Pickup" & !is.na(`30-45`)) %>% 
  summarise(TiempoPickups30a45 = sum(`30-45` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoPickups30a45)

# Contar los servicios de pickups que tienen tiempos de 45 a 75 minutos
CountDe45a75_Pickups <- dataAgrupada %>% 
  select(`45-75`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Pickup" & !is.na(`45-75`)) %>% 
  summarise(TiempoPickups45a75 = sum(`45-75` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoPickups45a75)

# Contar los servicios de pickups que tienen tiempos de 75 a 120 minutos
CountDe75a120_Pickups <- dataAgrupada %>% 
  select(`75-120`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Pickup" & !is.na(`75-120`)) %>% 
  summarise(TiempoPickups75a120 = sum(`75-120` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoPickups75a120)

# Contar los servicios de pickups que tienen tiempos de 120+ minutos
CountDe120Plus_Pickups <- dataAgrupada %>% 
  select(`120+`, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Pickup" & !is.na(`120+`)) %>% 
  summarise(TiempoPickups120Plus = sum(`120+` == TRUE, na.rm = TRUE)) %>% 
  pull(TiempoPickups120Plus)

# Verificar los resultados
CountDe5a30_Pickups
CountDe30a45_Pickups
CountDe45a75_Pickups
CountDe75a120_Pickups
CountDe120Plus_Pickups

# *** ANÁLSIS SOBRE ALTURA DE POSTES ATENDIDOS **

# Promedio de altura de postes para camiones
AVGPostesCamiones <- dataAgrupada %>% 
  select(height, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Camión" & !is.na(height)) %>% 
  summarise(AlturaPosteCamion = mean(height, na.rm = TRUE)) %>% 
  pull(AlturaPosteCamion)

AVGPostesCamiones

# Promedio de altura de postes para motos
AVGPostesMotos <- dataAgrupada %>% 
  select(height, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Moto" & !is.na(height)) %>% 
  summarise(AlturaPosteMoto = mean(height, na.rm = TRUE)) %>% 
  pull(AlturaPosteMoto)

AVGPostesMotos

# Promedio de altura de postes para pickups
AVGPostesPickups <- dataAgrupada %>% 
  select(height, Tipo_Servicio) %>% 
  filter(Tipo_Servicio == "Pickup" & !is.na(height)) %>% 
  summarise(AlturaPostePickup = mean(height, na.rm = TRUE)) %>% 
  pull(AlturaPostePickup)

AVGPostesPickups

# *** ANÁLISIS DE CUANTO SE FACTURA POR TIPO DE SERVICIO ***

Servicios2017 <- dataAgrupada %>% 
  select(Cod,ID) %>% 
  group_by(Cod) %>% 
  count()

Servicios2017

# Agrupar por 'Cod' y 'Tipo_Servicio' y contar la cantidad de registros
conteo_servicios <- dataAgrupada %>%
  filter(Cod %in% c("CAMBIO_CORRECTIVO", "CAMBIO_FUSIBLE", "CAMBIO_PUENTES", 
                    "OTRO", "REVISION", "REVISION_TRANSFORMADOR", 
                    "VERIFICACION_INDICADORES", "VERIFICACION_MEDIDORES", 
                    "VISITA", "VISITA_POR_CORRECCION")) %>%
  group_by(Cod, Tipo_Servicio) %>%
  summarise(Conteo = n(), .groups = 'drop')

# Verificar el resultado
print(conteo_servicios)


# Agrupar por 'Cod' y calcular la facturación total
Facturacion_servicios <- dataAgrupada %>%
  filter(Cod %in% c("CAMBIO_CORRECTIVO", "CAMBIO_FUSIBLE", "CAMBIO_PUENTES", 
                    "OTRO", "REVISION", "REVISION_TRANSFORMADOR", 
                    "VERIFICACION_INDICADORES", "VERIFICACION_MEDIDORES", 
                    "VISITA", "VISITA_POR_CORRECCION")) %>%
  group_by(Cod) %>%
  summarise(Facturación_Total = sum(factura, na.rm = TRUE), .groups = 'drop')

# Verificar el resultado
print(Facturacion_servicios)

library(dplyr)

costo_servicios <- dataAgrupada %>%
  filter(Cod %in% c("CAMBIO_CORRECTIVO", "CAMBIO_FUSIBLE", "CAMBIO_PUENTES", 
                    "OTRO", "REVISION", "REVISION_TRANSFORMADOR", 
                    "VERIFICACION_INDICADORES", "VERIFICACION_MEDIDORES", 
                    "VISITA", "VISITA_POR_CORRECCION")) %>%
  group_by(Cod) %>%
  summarise(
    Costo_Total_Camion = sum(Camion, na.rm = TRUE),
    Costo_Total_Moto = sum(Moto, na.rm = TRUE),
    Costo_Total_Pickup = sum(Pickup, na.rm = TRUE),
    Costo_total = Costo_Total_Camion + Costo_Total_Moto + Costo_Total_Pickup,
    
    # Costo Directo
    Costo_Directo_camion = sum(directoCamion, na.rm = TRUE),
    Costo_Directo_moto = sum(directoMoto, na.rm = TRUE),
    Costo_Directo_pickup = sum(directoPickup, na.rm = TRUE),
    
    # Costo Fijo
    Costo_Fijo_camion = sum(fijoCamion, na.rm = TRUE),
    Costo_Fijo_moto = sum(fijoMoto, na.rm = TRUE),
    Costo_Fijo_pickup = sum(fijoPickup, na.rm = TRUE),
    
    Costo_total_directo = (Costo_Directo_camion + Costo_Directo_moto + Costo_Directo_pickup),
    Costo_total_fijo = (Costo_Fijo_camion + Costo_Fijo_moto + Costo_Fijo_pickup),
    
    .groups = 'drop'
  )

# Aplicar formato con comas cada tres dígitos
costo_servicios_formatted <- costo_servicios %>%
  mutate(across(where(is.numeric), ~ format(., big.mark = ",", scientific = FALSE)))

# Verificar el resultado
print(costo_servicios_formatted)




# Agrupar por 'Cod' y 'Tipo_Servicio' y calcular la facturación total
Facturacion_servicios_tipo <- dataAgrupada %>%
  filter(Cod %in% c("CAMBIO_CORRECTIVO", "CAMBIO_FUSIBLE", "CAMBIO_PUENTES", 
                    "OTRO", "REVISION", "REVISION_TRANSFORMADOR", 
                    "VERIFICACION_INDICADORES", "VERIFICACION_MEDIDORES", 
                    "VISITA", "VISITA_POR_CORRECCION")) %>%
  group_by(Cod, Tipo_Servicio) %>%
  summarise(Facturación_Total = sum(factura, na.rm = TRUE), .groups = 'drop')

# Verificar el resultado
print(Facturacion_servicios_tipo)


# *** CENTROS DE DISTRIBUCIÓN ***

CentrosDistribución <- data %>% 
  group_by(origen) %>% 
  count()

CentrosDistribución

library(dplyr)

# Agrupar por centro de distribución ('origen'), tipo de servicio ('Cod') y tipo de vehículo ('Tipo_Servicio')
CentrosDistribución <- dataAgrupada %>%
  group_by(origen, Cod, Tipo_Servicio) %>%
  summarise(
    # Cantidad de servicios por tipo de vehículo y tipo de servicio
    Cantidad_Servicios = n(),
    
    # Cantidad total de servicios por tipo de vehículo (Camión, Moto, Pickup)
    Cantidad_Camion = sum(!is.na(Camion)),
    Cantidad_Moto = sum(!is.na(Moto)),
    Cantidad_Pickup = sum(!is.na(Pickup)),
    
    # Facturación total por centro de distribución, tipo de servicio y vehículo
    Facturacion_Total = sum(factura, na.rm = TRUE),
    
    # Costo total por tipo de vehículo y tipo de servicio (suma de costos directos y fijos)
    Costo_Camion = sum(directoCamion, na.rm = TRUE) + sum(fijoCamion, na.rm = TRUE),
    Costo_Moto = sum(directoMoto, na.rm = TRUE) + sum(fijoMoto, na.rm = TRUE),
    Costo_Pickup = sum(directoPickup, na.rm = TRUE) + sum(fijoPickup, na.rm = TRUE),
    
    .groups = 'drop'
  )

# *** ANÁLISIS DE LA DEMANDA ***

DemandaServiciosMes <- dataAgrupada %>%
  filter(format(as.Date(Fecha), "%Y") == "2017") %>%
  mutate(Mes = format(as.Date(Fecha), "%Y-%m")) %>%
  group_by(Mes) %>%
  summarise(Demanda = n())

# Verificar el resultado
print(DemandaServiciosMes)

# Graficar la demanda por mes con líneas que conecten los puntos
ggplot(DemandaServiciosMes, aes(x = Mes, y = Demanda)) +
  geom_point(color = "blue") +
  geom_line(color = "blue", group = 1) +  # Añadir líneas que conectan los puntos
  labs(title = "Demanda de Servicios por Mes en 2017", x = "Mes", y = "Demanda de Servicios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data <- data %>% mutate(Mes = floor_date(Fecha, "month"))

# Calcular la demanda por tipo de vehículo por mes
demanda_por_vehiculo_mes <- data %>%
  select(Mes, Camion, Moto, Pickup) %>%
  group_by(Mes) %>%
  summarise(
    Demanda_Camion = sum(!is.na(Camion)),
    Demanda_Moto = sum(!is.na(Moto)),
    Demanda_Pickup = sum(!is.na(Pickup))
  )

print("Demanda por tipo de vehículo por mes:")
print(demanda_por_vehiculo_mes)

# Calcular la demanda por tipo de vehículo por mes
demanda_Cento <- data %>%
  select(ID, origen) %>%
  group_by(origen) %>% count()


# Calcular la demanda por tipo de servicio, tipo de vehículo y mes
demanda_por_servicio_mes <- data %>%
  filter(Cod %in% c("CAMBIO_CORRECTIVO", "CAMBIO_FUSIBLE", "CAMBIO_PUENTES", 
                    "OTRO", "REVISION", "REVISION_TRANSFORMADOR", 
                    "VERIFICACION_INDICADORES", "VERIFICACION_MEDIDORES", 
                    "VISITA", "VISITA_POR_CORRECCION")) %>%
  group_by(Mes, Cod) %>%
  summarise(
    Demanda_Camion = sum(!is.na(Camion)),
    Demanda_Moto = sum(!is.na(Moto)),
    Demanda_Pickup = sum(!is.na(Pickup))
  )

print("Demanda por tipo de servicio, tipo de vehículo y mes:")
print(demanda_por_servicio_mes)

# *** PUNTO DE EQUILIBRIO ***

# Datos de costos fijos y directos
CostoFijoTotal <- CostoFijo_Camiones + CostoFijo_Motos + CostoFijo_Pickups
CostoDirectoTotal <- CostoDirecto_Camiones + CostoDirecto_Motos + CostoDirecto_Pickups

# Facturación total y total de servicios
TotalFacturacion <- Facturacion_Camiones + Facturacion_Motos + Facturacion_Pickups
TotalServicios <- Servicios_Camion + Servicios_Moto + Servicios_Pickup

# Calcular ingreso promedio por servicio
IngresoPromedioPorServicio <- TotalFacturacion / TotalServicios

# Calcular costo variable promedio por servicio
CostoVariablePromedioPorServicio <- CostoDirectoTotal / TotalServicios

# Calcular punto de equilibrio en número de servicios
PuntoEquilibrioServicios <- CostoFijoTotal / (IngresoPromedioPorServicio - CostoVariablePromedioPorServicio)

# Resultado
print(paste("El punto de equilibrio en términos de cantidad de servicios es:", round(PuntoEquilibrioServicios)))


# *** TIEMPOS CADA CENTRO DE DISTRIBUCIÓN ***


Centros_Tiempos <- data %>% 
  group_by(origen) %>% 
  summarise(
    Cinco_Treinta = sum(`5-30`, na.rm = TRUE),
    Treinta_Cuarentacinco = sum(`30-45`, na.rm = TRUE),
    Cuarentacinco_Setentacinco = sum(`45-75`, na.rm = TRUE),
    Setentacinco_Cientoveinte = sum(`75-120`, na.rm = TRUE),
    Cientoveinte_Mas = sum(`120+`, na.rm = TRUE)
  )

Centros_Tiempos

write_xlsx(Centros_Tiempos, path = "CentrosDistribuciónTiempos.xlsx")

Count_Tiempos_servicio <- dataAgrupada %>%
  filter(Cod %in% c("CAMBIO_CORRECTIVO", "CAMBIO_FUSIBLE", "CAMBIO_PUENTES", 
                    "OTRO", "REVISION", "REVISION_TRANSFORMADOR", 
                    "VERIFICACION_INDICADORES", "VERIFICACION_MEDIDORES", 
                    "VISITA", "VISITA_POR_CORRECCION")) %>%
  group_by(Cod) %>%
  summarise( Cinco_Treinta = sum(`5-30`, na.rm = TRUE),
             Treinta_Cuarentacinco = sum(`30-45`, na.rm = TRUE),
             Cuarentacinco_Setentacinco = sum(`45-75`, na.rm = TRUE),
             Setentacinco_Cientoveinte = sum(`75-120`, na.rm = TRUE),
             Cientoveinte_Mas = sum(`120+`, na.rm = TRUE))

Count_Tiempos_servicio

write_xlsx(Count_Tiempos_servicio, path = "ServiciosTiempos.xlsx")


Data_lon_latitud <- dataAgrupada %>% select(Lat,Long,ID)

# Usamos los datos que tienes en Data_lon_latitud
Data_lon_latitud <- dataAgrupada %>% select(Lat, Long, ID)

library(leaflet)
install.packages("htmlwidgets")
library(htmlwidgets)

# Cargar el paquete necesario
library(leaflet)

# Suponiendo que Data_lon_latitud tiene las columnas Lat, Long, ID
# Crear el mapa con leaflet
mapa <- leaflet(Data_lon_latitud) %>%
  addTiles() %>%  # Fondo del mapa
  setView(lng = mean(Data_lon_latitud$Long, na.rm = TRUE),  # Centro en la media de longitudes
          lat = mean(Data_lon_latitud$Lat, na.rm = TRUE),   # Centro en la media de latitudes
          zoom = 7) %>%  # Zoom adecuado para Guatemala
  addCircleMarkers(~Long, ~Lat,  # Usar las columnas de longitud y latitud
                   popup = ~paste("ID de Servicio:", ID),  # Mostrar el ID en el popup
                   radius = 3,  # Tamaño del marcador
                   color = "blue",  # Color de los puntos
                   fillOpacity = 0.6)  # Opacidad de los puntos

# Mostrar el mapa
mapa










install.packages("ggplot2")
library(ggplot2)

Data_lon_latitud <- dataAgrupada %>%
  select(Lat, Long, ID) %>%
  distinct(ID, .keep_all = TRUE)

# Mostrar el DataFrame actualizado con ID únicos
print(Data_lon_latitud)

library(dplyr)
library(revgeo)

# Crear un subconjunto único para Lat y Long
unique_coords <- Data_lon_latitud %>%
  select(Lat, Long) %>%
  distinct()

# Añadir columnas de Departamento y Zona con geocodificación inversa
unique_coords <- unique_coords %>%
  rowwise() %>%
  mutate(
    Departamento = revgeo(longitude = Long, latitude = Lat, provider = 'photon', output = 'frame')$city,
    Zona = revgeo(longitude = Long, latitude = Lat, provider = 'photon', output = 'frame')$postcode
  ) %>%
  ungroup()

# Unir de nuevo con el DataFrame original usando Lat y Long
Data_lon_latitud <- Data_lon_latitud %>%
  left_join(unique_coords, by = c("Lat", "Long"))

# Mostrar el DataFrame actualizado con las nuevas columnas
print(Data_lon_latitud)
