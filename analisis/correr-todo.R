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


# PUNTO 2: FICHAS Y PARTICION

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


# PUNTO 3: DESCRIPCION Y PATRON

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

# PUNTO 4: OPERADOR DE REZAGO

gamma0 <- function(x) mean((x - mean(x))^2)

pendiente_mco <- function(y) {
  X <- cbind(1, seq_along(y))
  as.numeric(solve(crossprod(X), crossprod(X, y)))[2]
}

polinomio_texto <- function(coef) {
  p <- c(1, coef)
  terminos <- character(0)
  for (i in seq_along(p)) {
    if (p[i] == 0) next
    potencia <- i - 1
    base <- if (potencia == 0) "" else if (potencia == 1) "B" else paste0("B^", potencia)
    valor <- abs(p[i])
    texto <- if (potencia == 0) as.character(valor) else if (valor == 1) base else paste0(valor, base)
    signo <- if (p[i] < 0) "- " else if (length(terminos) == 0) "" else "+ "
    terminos <- c(terminos, paste0(signo, texto))
  }
  paste(terminos, collapse = " ")
}

candidatos <- list(
  serie1 = list(
    list(nombre = "(1-B)", d = 1, D = 0, s = 1),
    list(nombre = "(1-B^12)", d = 0, D = 1, s = 12),
    list(nombre = "(1-B)(1-B^12)", d = 1, D = 1, s = 12)
  ),
  serie2 = list(
    list(nombre = "(1-B)", d = 1, D = 0, s = 1),
    list(nombre = "(1-B)^2", d = 2, D = 0, s = 1)
  ),
  serie3 = list(
    list(nombre = "(1-B)", d = 1, D = 0, s = 1),
    list(nombre = "(1-B)^2", d = 2, D = 0, s = 1),
    list(nombre = "(1-B^12)", d = 0, D = 1, s = 12)
  )
)

elegida <- c(serie1 = "(1-B^12)", serie2 = "Ninguna", serie3 = "(1-B)")

for (k in names(config)) {
  y <- resultados[[k]]$datos$y
  b1 <- pendiente_mco(y)
  filas <- list(tibble(
    transformacion = "Ninguna", polinomio = "1", n = length(y),
    varianza = gamma0(y), r1 = acf_muestral(y, 1),
    varianza_identidad = NA_real_,
    media = mean(y), media_esperada = mean(y)
  ))
  graficos_raices <- list()
  raices <- list()
  for (cand in candidatos[[k]]) {
    x <- diferenciar(y, d = cand$d, D = cand$D, s = cand$s)
    coef <- coef_diferencia(d = cand$d, D = cand$D, s = cand$s)
    raices[[cand$nombre]] <- polinomio_rezago(coef)
    graficos_raices[[cand$nombre]] <- graficar_raices(raices[[cand$nombre]], cand$nombre)
    var_identidad <- NA_real_
    if (cand$d >= 1) {
      previa <- diferenciar(y, d = cand$d - 1, D = cand$D, s = cand$s)
      var_identidad <- 2 * gamma0(previa) * (1 - acf_muestral(previa, 1))
    }
    media_esperada <- if (cand$d + cand$D >= 2) 0 else if (cand$D == 1) cand$s * b1 else b1
    filas[[length(filas) + 1]] <- tibble(
      transformacion = cand$nombre, polinomio = polinomio_texto(coef), n = length(x),
      varianza = gamma0(x), r1 = acf_muestral(x, 1),
      varianza_identidad = var_identidad,
      media = mean(x), media_esperada = media_esperada
    )
  }
  tabla <- bind_rows(filas)
  guardar(wrap_plots(graficos_raices, nrow = 1), paste0(k, "-raices"),
          ancho = 4 * length(graficos_raices), alto = 4.5)
  
  cand_elegida <- Filter(function(c) c$nombre == elegida[[k]], candidatos[[k]])
  y_transformada <- if (length(cand_elegida) == 0) y else
    diferenciar(y, d = cand_elegida[[1]]$d, D = cand_elegida[[1]]$D, s = cand_elegida[[1]]$s)
  
  resultados[[k]]$transformacion <- list(
    tabla = tabla, raices = raices, pendiente_mco = b1,
    elegida = elegida[[k]], y = y_transformada
  )
  
  cat("\n>>>", toupper(k), ": TRANSFORMACIONES CANDIDATAS\n")
  print(as.data.frame(tabla), digits = 4)
  cat("- Transformacion elegida:", elegida[[k]], "\n")
}


# PUNTO 5: DEPENDENCIA Y ESTACIONARIEDAD

for (k in names(config)) {
  x <- resultados[[k]]$transformacion$y
  n <- length(x)
  m <- min(floor(n / 4), 24)
  r <- acf_muestral(x, m)
  phi <- pacf_dl(r)
  guardar(correlograma(x, m = m, titulo = resultados[[k]]$transformacion$elegida),
          paste0(k, "-acf-transformada"), alto = 7)
  
  lb <- ljung_box(r, T = n, m = m, p = 0)
  m_corto <- ceiling(log(n))
  lb_corto <- ljung_box(r, T = n, m = m_corto, p = 0)
  
  grupo <- rep(1:4, each = ceiling(n / 4))[1:n]
  tabla_bloques <- tibble(
    bloque = 1:4,
    n = as.numeric(table(grupo)),
    media = as.numeric(tapply(x, grupo, mean)),
    varianza = as.numeric(tapply(x, grupo, var))
  )
  
  L <- floor(4 * (n / 100)^(2 / 9))
  media <- mean(x)
  ee_ingenuo <- sd(x) / sqrt(n)
  vlp <- varianza_largo_plazo(x, L)
  ee_lp <- sqrt(vlp / n)
  z <- qnorm(0.975)
  intervalos <- tibble(
    error_estandar = c("Ingenuo", "Largo plazo (Bartlett)"),
    valor = c(ee_ingenuo, ee_lp),
    limite_inferior = media - z * c(ee_ingenuo, ee_lp),
    limite_superior = media + z * c(ee_ingenuo, ee_lp)
  )
  
  resultados[[k]]$dependencia <- list(
    n = n, m = m, acf = r, pacf = phi,
    banda_ruido_blanco = banda_ruido_blanco(n), banda_bartlett = banda_bartlett(r, n),
    ljung_box = lb, ljung_box_corto = lb_corto, m_corto = m_corto, bloques = tabla_bloques,
    media = media, L = L, varianza_largo_plazo = vlp, gamma0 = gamma0(x),
    intervalos = intervalos
  )
  
  cat("\n>>>", toupper(k), ": DEPENDENCIA SOBRE", resultados[[k]]$transformacion$elegida, "\n")
  cat("- n =", n, "| m =", m, "| banda ruido blanco = +/-", round(banda_ruido_blanco(n), 4), "\n")
  cat("- Rezagos fuera de la banda de ruido blanco:",
      paste(which(abs(r) > banda_ruido_blanco(n)), collapse = ", "), "\n")
  cat("- ACF:", paste(round(r[1:min(12, m)], 3), collapse = " "), "\n")
  cat("- PACF:", paste(round(phi[1:min(12, m)], 3), collapse = " "), "\n")
  cat("- Ljung-Box: Q =", round(lb$estadistico, 3), "| gl =", lb$grados_libertad,
      "| valor critico =", round(lb$valor_critico, 3), "| valor p =", signif(lb$valor_p, 4), "\n")
  cat("- Ljung-Box con m corto =", m_corto, ": Q =", round(lb_corto$estadistico, 3),
      "| gl =", lb_corto$grados_libertad, "| valor critico =", round(lb_corto$valor_critico, 3),
      "| valor p =", signif(lb_corto$valor_p, 4), "\n")
  cat("- Media y varianza por bloque:\n")
  print(as.data.frame(tabla_bloques), digits = 4)
  cat("- Media =", round(media, 4), "| L =", L, "| gamma(0) =", round(gamma0(x), 4),
      "| varianza de largo plazo =", round(vlp, 4), "\n")
  print(as.data.frame(intervalos), digits = 4)
}


# PUNTO 6(a): METODOS, ASIGNACION Y OPTIMIZACION EN ESTIMACION

tabla_asignacion <- definir_asignacion_metodos(crear_catalogo_metodos(config$serie1$s))
tabla_optimos <- list()

for (k in names(config)) {
  cfg <- config[[k]]
  y <- resultados[[k]]$datos$y
  n0 <- resultados[[k]]$particion$n0
  y_estimacion <- y[seq_len(n0)]
  catalogo <- crear_catalogo_metodos(cfg$s)
  decisiones <- tabla_asignacion[
    tabla_asignacion$serie == k & tabla_asignacion$decision,
  ]
  ajustes <- list()

  for (id in decisiones$metodo_id) {
    especificacion <- catalogo[[id]]
    ajuste <- ajustar_especificacion(y_estimacion, especificacion)
    ajustes[[id]] <- ajuste

    if (!is.null(ajuste$superficie)) {
      parametros <- paste(
        paste(names(ajuste$parametros), unlist(ajuste$parametros), sep = "="),
        collapse = ", "
      )
      tabla_optimos[[length(tabla_optimos) + 1L]] <- tibble(
        serie = k,
        metodo_id = id,
        metodo = especificacion$nombre,
        parametros = parametros,
        mse_estimacion = ajuste$mse
      )
      guardar(
        graficar_optimizacion(
          ajuste,
          paste(especificacion$nombre, "-", k)
        ),
        paste0(k, "-mse-", id),
        ancho = if (ncol(ajuste$parametros) == 3L) 10 else 7,
        alto = if (ncol(ajuste$parametros) == 3L) 8 else 5
      )
    }
  }

  resultados[[k]]$pronostico <- list(
    y_estimacion = y_estimacion,
    asignacion = decisiones,
    ajustes_estimacion = ajustes
  )
}

tabla_optimos <- bind_rows(tabla_optimos)

cat("\n>>> ASIGNACION DE METODOS\n")
print(as.data.frame(tabla_asignacion[, c("serie", "metodo", "rol")]))
cat("\n>>> PARAMETROS OPTIMOS EN EL TRAMO DE ESTIMACION\n")
print(as.data.frame(tabla_optimos), digits = 6)


# PUNTO 6(b): REGRESIONES Y DIAGNOSTICO DE RESIDUOS

tabla_coeficientes_regresion <- list()
tabla_dependencia_regresion <- list()
tabla_normalidad_regresion <- list()

for (k in names(config)) {
  ids_regresion <- tabla_asignacion$metodo_id[
    tabla_asignacion$serie == k & tabla_asignacion$decision &
      grepl("^tendencia_", tabla_asignacion$metodo_id)
  ]
  diagnosticos <- list()

  for (id in ids_regresion) {
    ajuste <- resultados[[k]]$pronostico$ajustes_estimacion[[id]]
    modelo <- ajuste$modelo
    diagnostico <- diagnosticar_regresion(modelo)
    diagnosticos[[id]] <- diagnostico
    nombre_metodo <- tabla_asignacion$metodo[
      tabla_asignacion$serie == k & tabla_asignacion$metodo_id == id
    ][1]

    coeficientes <- modelo$parametros$coeficientes
    tabla_coeficientes_regresion[[length(tabla_coeficientes_regresion) + 1L]] <-
      bind_cols(
        tibble(serie = k, metodo = nombre_metodo, escala = diagnostico$escala),
        coeficientes
      )

    lb <- diagnostico$ljung_box
    dw <- diagnostico$durbin_watson
    tabla_dependencia_regresion[[length(tabla_dependencia_regresion) + 1L]] <- tibble(
      serie = k, metodo = nombre_metodo, escala = diagnostico$escala,
      n = diagnostico$n, m = diagnostico$m,
      r1 = dw$r1, banda = diagnostico$banda_ruido_blanco,
      Q = lb$estadistico, gl = lb$grados_libertad,
      valor_critico = lb$valor_critico, valor_p = lb$valor_p,
      decision = lb$decision, DW = dw$estadistico,
      aproximacion_DW = dw$aproximacion
    )

    jb <- diagnostico$jarque_bera
    tabla_normalidad_regresion[[length(tabla_normalidad_regresion) + 1L]] <- tibble(
      serie = k, metodo = nombre_metodo, escala = diagnostico$escala,
      JB = jb$estadistico, gl = jb$grados_libertad,
      valor_critico = jb$valor_critico, valor_p = jb$valor_p,
      decision = jb$decision, asimetria = jb$asimetria,
      curtosis = jb$curtosis
    )

    guardar(
      correlograma(
        diagnostico$residuos,
        m = diagnostico$m,
        titulo = paste(nombre_metodo, "-", k)
      ),
      paste0(k, "-residuos-", id),
      ancho = 9, alto = 7
    )
  }
  resultados[[k]]$pronostico$diagnosticos_regresion <- diagnosticos
}

tabla_coeficientes_regresion <- bind_rows(tabla_coeficientes_regresion)
tabla_dependencia_regresion <- bind_rows(tabla_dependencia_regresion)
tabla_normalidad_regresion <- bind_rows(tabla_normalidad_regresion)

cat("\n>>> COEFICIENTES DE LAS REGRESIONES\n")
print(as.data.frame(tabla_coeficientes_regresion), digits = 5)
cat("\n>>> DEPENDENCIA EN RESIDUOS DE REGRESION\n")
print(as.data.frame(tabla_dependencia_regresion), digits = 5)
cat("\n>>> NORMALIDAD EN RESIDUOS DE REGRESION\n")
print(as.data.frame(tabla_normalidad_regresion), digits = 5)

# BLOQUES DE VERIFICACION

cat("\n\n====================================================================\n")
cat("BLOQUES DE VERIFICACION\n")
cat("====================================================================\n")

y_v <- resultados$serie1$datos$y
cat("- diferenciar(d = 1) vs diff():", max(abs(diferenciar(y_v, d = 1) - diff(y_v))), "\n")
cat("- diferenciar(D = 1, s = 12) vs diff(lag = 12):",
    max(abs(diferenciar(y_v, D = 1, s = 12) - diff(y_v, lag = 12))), "\n")
cat("- diferenciar(d = 1, D = 1, s = 12) vs diff(diff(lag = 12)):",
    max(abs(diferenciar(y_v, d = 1, D = 1, s = 12) - diff(diff(y_v, lag = 12)))), "\n")

for (k in names(config)) {
  y_k <- resultados[[k]]$datos$y
  m_k <- min(floor(length(y_k) / 4), 24)
  dif_acf <- max(abs(acf_muestral(y_k, m_k) - as.numeric(acf(y_k, lag.max = m_k, plot = FALSE)$acf)[-1]))
  dif_pacf <- max(abs(pacf_dl(acf_muestral(y_k, m_k)) - as.numeric(pacf(y_k, lag.max = m_k, plot = FALSE)$acf)))
  cat("-", k, "| ACF vs acf():", dif_acf, "| PACF Durbin-Levinson vs pacf():", dif_pacf,
      "| Menor que 1e-10:", dif_pacf < 1e-10, "\n")
}
cat("====================================================================\n")

