library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(readr)
library(ggplot2)
library(patchwork)

source("R/00-lectura.R")
source("R/01-graficos.R")
source("R/02-operadores.R")
source("R/03-dependencia.R")
source("R/04-metodos.R")
source("R/05-evaluacion.R")

if (!dir.exists("figs")) dir.create("figs")

guardar <- function(grafico, nombre, ancho = 8, alto = 5) {
  ggsave(file.path("figs", paste0(nombre, ".png")), grafico, width = ancho, height = alto, dpi = 150)
}

fuente <- "Energia del Altiplano S.A. E.S.P."

config <- list(
  serie1 = list(archivo = "datos/STGFBL-serie-1.csv",
                nombre = "Demanda mensual de energia de un distribuidor",
                unidad = "GWh", H = 12, s = 12, rezagos = c(1, 6, 12)),
  serie2 = list(archivo = "datos/STGFBL-serie-2.csv",
                nombre = "Consumo diario de energia de una estacion de bombeo",
                unidad = "MWh", H = 7, s = 7, rezagos = c(1, 2, 7)),
  serie3 = list(archivo = "datos/STGFBL-serie-3.csv",
                nombre = "Usuarios conectados a la red de un operador regional",
                unidad = "miles de usuarios", H = 21, s = 12, rezagos = c(1, 6, 12))
)

resultados <- list()


# SECCION 2: FICHAS Y PARTICION

for (k in names(config)) {
  cfg <- config[[k]]
  datos <- leer_serie(cfg$archivo, fuente = fuente, unidad = cfg$unidad)
  resultados[[k]] <- list(
    datos = datos,
    ficha = ficha_serie(datos, cfg$nombre),
    particion = particion_serie(datos, cfg$H)
  )
}

tabla_fichas <- bind_rows(lapply(resultados, `[[`, "ficha"))
tabla_particion <- bind_rows(lapply(names(resultados), function(k) {
  bind_cols(tibble(serie = k), resultados[[k]]$particion$tabla)
}))

cat("\n>>> FICHAS DE LAS SERIES\n")
print(as.data.frame(tabla_fichas))
cat("\n>>> PARTICION EN TRAMOS Y ORIGENES\n")
print(as.data.frame(tabla_particion))


# SECCION 3: DESCRIPCION Y PATRON

for (k in names(config)) {
  cfg <- config[[k]]
  datos <- resultados[[k]]$datos
  guardar(graficar_serie(datos, cfg$nombre), paste0(k, "-tiempo"))
  guardar(grafico_estacional(datos, cfg$s), paste0(k, "-estacional"))
  guardar(grafico_rezagos(datos, cfg$rezagos), paste0(k, "-rezagos"), ancho = 10, alto = 4)
  guardar(correlograma(datos, titulo = cfg$nombre), paste0(k, "-acf"), alto = 7)
  resultados[[k]]$acf_serie <- acf_muestral(datos$y, min(floor(nrow(datos) / 4), 24))
}

ev1 <- evidencia_tipo_estacional(resultados$serie1$datos, 12)
d1_adi <- descomposicion_clasica(resultados$serie1$datos, 12, "aditiva")
d1_mul <- descomposicion_clasica(resultados$serie1$datos, 12, "multiplicativa")
guardar(graficar_descomposicion(d1_adi, "Serie 1"), "serie1-descomposicion-aditiva", alto = 8)
guardar(graficar_descomposicion(d1_mul, "Serie 1"), "serie1-descomposicion-multiplicativa", alto = 8)
resultados$serie1$estacionalidad <- list(evidencia = ev1, aditiva = d1_adi, multiplicativa = d1_mul)

cat("\n>>> SERIE 1: EVIDENCIA ADITIVA O MULTIPLICATIVA\n")
print(ev1$tabla)
cat("- Correlacion media-desviacion:", ev1$correlacion_media_desviacion, "\n")
cat("- Pendiente desviacion sobre media:", ev1$pendiente_desviacion_sobre_media, "\n")
print(d1_adi$indices)

e2 <- estaciones_de(resultados$serie2$datos, 7)
media_dia <- tapply(resultados$serie2$datos$y, e2$estacion, mean)
resultados$serie2$estacionalidad <- list(
  media_por_dia = media_dia,
  rango_medias = diff(range(media_dia)),
  desviacion_serie = sd(resultados$serie2$datos$y)
)

cat("\n>>> SERIE 2: MEDIAS POR DIA DE LA SEMANA\n")
print(round(media_dia, 2))
cat("- Rango de las medias:", resultados$serie2$estacionalidad$rango_medias,
    "| Desviacion de la serie:", resultados$serie2$estacionalidad$desviacion_serie, "\n")

d3 <- descomposicion_clasica(resultados$serie3$datos, 12, "aditiva")
guardar(graficar_descomposicion(d3, "Serie 3"), "serie3-descomposicion-aditiva", alto = 8)
resultados$serie3$estacionalidad <- list(
  descomposicion = d3,
  amplitud_indices = diff(range(d3$indices$indice)),
  desviacion_residuo = sd(d3$componentes$residuo, na.rm = TRUE)
)

cat("\n>>> SERIE 3: INDICES ESTACIONALES FRENTE AL RUIDO\n")
print(d3$indices)
cat("- Amplitud de los indices:", resultados$serie3$estacionalidad$amplitud_indices,
    "| Desviacion del residuo:", resultados$serie3$estacionalidad$desviacion_residuo, "\n")