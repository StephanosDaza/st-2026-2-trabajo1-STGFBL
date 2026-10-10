library(ggplot2)
library(patchwork)

graficar_serie <- function(datos, titulo) {
  stopifnot(
    "datos debe ser el tibble generado por leer_serie" =
      is.data.frame(datos) && all(c("fecha", "y") %in% names(datos)),
    "titulo debe ser un texto" = is.character(titulo) && length(titulo) == 1
  )
  fuente <- attr(datos, "fuente")
  unidad <- attr(datos, "unidad")
  ggplot(datos, aes(x = fecha, y = y)) +
    geom_line(color = "steelblue", linewidth = 0.7) +
    labs(
      title = titulo,
      x = "Fecha",
      y = if (is.null(unidad)) "Valor" else unidad,
      caption = paste0("Fuente: ", if (is.null(fuente)) "desconocida" else fuente,
                       " | Numero de observaciones: ", nrow(datos))
    ) +
    scale_x_date(date_labels = "%Y-%m") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, face = "italic", color = "gray30"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

grafico_estacional <- function(datos, s) {
  stopifnot(
    "datos debe ser el tibble generado por leer_serie" =
      is.data.frame(datos) && all(c("t", "fecha", "y") %in% names(datos)),
    "s debe ser un entero mayor o igual a 2" = length(s) == 1 && s == round(s) && s >= 2,
    "la serie debe tener al menos dos ciclos completos" = nrow(datos) >= 2 * s
  )
  freq <- attr(datos, "frecuencia")
  if (!is.null(freq) && freq == 12 && s == 12) {
    estacion <- as.integer(format(datos$fecha, "%m"))
    ciclo <- as.integer(format(datos$fecha, "%Y"))
    etiquetas <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                   "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
    nombre_x <- "Mes"
    nombre_ciclo <- "Anio"
  } else {
    estacion <- (datos$t - 1) %% s + 1
    ciclo <- (datos$t - 1) %/% s + 1
    etiquetas <- if (s == 7) weekdays(datos$fecha[1:7], abbreviate = TRUE) else as.character(1:s)
    nombre_x <- "Indice de estacion"
    nombre_ciclo <- "Ciclo"
  }
  df <- data.frame(estacion = estacion, ciclo = ciclo, y = datos$y)
  ggplot(df, aes(x = estacion, y = y, group = ciclo, color = ciclo)) +
    geom_line(alpha = 0.8) +
    geom_point(size = 0.8, alpha = 0.8) +
    scale_color_viridis_c(name = nombre_ciclo) +
    scale_x_continuous(breaks = 1:s, labels = etiquetas) +
    labs(
      title = paste0("Grafico estacional (periodo s = ", s, ")"),
      x = nombre_x,
      y = attr(datos, "unidad"),
      caption = paste0("Un trazo por ciclo | Numero de observaciones: ", nrow(datos))
    ) +
    theme_minimal()
}

grafico_rezagos <- function(datos, rezagos) {
  stopifnot(
    "datos debe ser el tibble generado por leer_serie" =
      is.data.frame(datos) && "y" %in% names(datos),
    "rezagos debe ser un vector de enteros positivos" =
      is.numeric(rezagos) && all(rezagos >= 1) && all(rezagos == round(rezagos)),
    "el mayor rezago debe ser menor que la longitud de la serie" = max(rezagos) < nrow(datos)
  )
  y <- datos$y
  n <- length(y)
  r <- acf_muestral(y, max(rezagos))
  df <- do.call(rbind, lapply(rezagos, function(k) {
    data.frame(
      panel = paste0("k = ", k, "   r = ", formatC(r[k], digits = 2, format = "f")),
      y_t = y[(k + 1):n],
      y_tk = y[1:(n - k)]
    )
  }))
  df$panel <- factor(df$panel, levels = unique(df$panel))
  unidad <- attr(datos, "unidad")
  ggplot(df, aes(x = y_tk, y = y_t)) +
    geom_point(color = "steelblue", alpha = 0.6, size = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
    facet_wrap(~ panel) +
    labs(
      title = "Grafico de rezagos: y(t) contra y(t - k)",
      x = paste0("y(t - k) [", unidad, "]"),
      y = paste0("y(t) [", unidad, "]"),
      caption = paste("Numero de observaciones:", n)
    ) +
    theme_minimal()
}

correlograma <- function(datos, m = NULL, titulo = NULL) {
  if (is.data.frame(datos) && "y" %in% names(datos)) {
    y <- datos$y
  } else {
    stopifnot("datos debe ser un tibble con columna y o un vector numerico" = is.numeric(datos))
    y <- datos
  }
  y <- y[!is.na(y)]
  n <- length(y)
  if (is.null(m)) m <- min(floor(n / 4), 24)
  stopifnot("m debe estar entre 1 y n - 1" = m >= 1 && m < n)
  
  r <- acf_muestral(y, m)
  phi <- pacf_dl(r)
  bw <- banda_ruido_blanco(n)
  bb <- banda_bartlett(r, n)
  
  df_acf <- data.frame(lag = 1:m, valor = r, bartlett = bb)
  df_pacf <- data.frame(lag = 1:m, valor = phi)
  
  p_acf <- ggplot(df_acf, aes(x = lag, y = valor)) +
    geom_hline(yintercept = 0) +
    geom_segment(aes(xend = lag, yend = 0)) +
    geom_hline(yintercept = c(-bw, bw), linetype = "dashed", color = "blue") +
    geom_line(aes(y = bartlett), linetype = "dotted", color = "red") +
    geom_line(aes(y = -bartlett), linetype = "dotted", color = "red") +
    scale_x_continuous(breaks = 1:m) +
    labs(
      title = if (is.null(titulo)) "ACF muestral" else paste("ACF muestral -", titulo),
      subtitle = paste0("Azul: ruido blanco +/- 1.96/sqrt(n). Rojo: Bartlett. n = ", n),
      x = NULL, y = "ACF"
    ) +
    theme_bw()
  
  p_pacf <- ggplot(df_pacf, aes(x = lag, y = valor)) +
    geom_hline(yintercept = 0) +
    geom_segment(aes(xend = lag, yend = 0)) +
    geom_hline(yintercept = c(-bw, bw), linetype = "dashed", color = "blue") +
    scale_x_continuous(breaks = 1:m) +
    labs(title = "PACF muestral (Durbin-Levinson)", x = "Rezago", y = "PACF") +
    theme_bw()
  
  p_acf / p_pacf
}

estaciones_de <- function(datos, s) {
  stopifnot(
    "datos debe ser el tibble generado por leer_serie" =
      is.data.frame(datos) && all(c("t", "fecha", "y") %in% names(datos)),
    "s debe ser un entero mayor o igual a 2" = length(s) == 1 && s == round(s) && s >= 2
  )
  freq <- attr(datos, "frecuencia")
  if (!is.null(freq) && freq == 12 && s == 12) {
    list(
      estacion = as.integer(format(datos$fecha, "%m")),
      ciclo = as.integer(format(datos$fecha, "%Y")),
      etiquetas = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                    "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
      nombre_x = "Mes",
      nombre_ciclo = "Anio"
    )
  } else {
    list(
      estacion = (datos$t - 1) %% s + 1,
      ciclo = (datos$t - 1) %/% s + 1,
      etiquetas = if (s == 7) weekdays(datos$fecha[1:7], abbreviate = TRUE) else as.character(1:s),
      nombre_x = "Indice de estacion",
      nombre_ciclo = "Ciclo"
    )
  }
}

evidencia_tipo_estacional <- function(datos, s) {
  e <- estaciones_de(datos, s)
  conteo <- table(e$ciclo)
  completos <- as.integer(names(conteo)[conteo == s])
  stopifnot("se necesitan al menos tres ciclos completos" = length(completos) >= 3)
  tabla <- do.call(rbind, lapply(completos, function(k) {
    v <- datos$y[e$ciclo == k]
    data.frame(ciclo = k, media = mean(v), desviacion = sd(v), rango = max(v) - min(v))
  }))
  pendiente <- sum((tabla$media - mean(tabla$media)) * (tabla$desviacion - mean(tabla$desviacion))) /
    sum((tabla$media - mean(tabla$media))^2)
  list(
    tabla = tabla,
    correlacion_media_desviacion = cor(tabla$media, tabla$desviacion),
    pendiente_desviacion_sobre_media = pendiente,
    cv_por_ciclo = tabla$desviacion / tabla$media
  )
}

media_movil_centrada <- function(y, s) {
  stopifnot(
    "y debe ser un vector numerico sin NA" = is.numeric(y) && !anyNA(y),
    "s debe ser un entero mayor o igual a 2" = length(s) == 1 && s == round(s) && s >= 2,
    "la serie debe ser mas larga que el periodo" = length(y) > s
  )
  n <- length(y)
  if (s %% 2 == 1) {
    q <- (s - 1) / 2
    w <- rep(1 / s, s)
  } else {
    q <- s / 2
    w <- c(0.5, rep(1, s - 1), 0.5) / s
  }
  tendencia <- rep(NA_real_, n)
  for (t in (q + 1):(n - q)) tendencia[t] <- sum(w * y[(t - q):(t + q)])
  tendencia
}

descomposicion_clasica <- function(datos, s, tipo = "aditiva") {
  stopifnot(
    "tipo debe ser 'aditiva' o 'multiplicativa'" =
      length(tipo) == 1 && tipo %in% c("aditiva", "multiplicativa"),
    "la serie debe tener al menos dos ciclos completos" = nrow(datos) >= 2 * s
  )
  if (tipo == "multiplicativa") {
    stopifnot("la version multiplicativa exige valores positivos" = all(datos$y > 0))
  }
  e <- estaciones_de(datos, s)
  y <- datos$y
  tendencia <- media_movil_centrada(y, s)
  sin_tendencia <- if (tipo == "aditiva") y - tendencia else y / tendencia
  crudos <- tapply(sin_tendencia, factor(e$estacion, levels = 1:s), mean, na.rm = TRUE)
  indices <- if (tipo == "aditiva") crudos - mean(crudos) else crudos / mean(crudos)
  estacional <- as.numeric(indices[e$estacion])
  residuo <- if (tipo == "aditiva") y - tendencia - estacional else y / (tendencia * estacional)
  list(
    tipo = tipo, s = s, unidad = attr(datos, "unidad"),
    componentes = tibble(t = datos$t, fecha = datos$fecha, y = y,
                         tendencia = tendencia, estacional = estacional, residuo = residuo),
    indices = tibble(estacion = 1:s, etiqueta = e$etiquetas, indice = as.numeric(indices))
  )
}

graficar_descomposicion <- function(desc, titulo) {
  stopifnot(
    "desc debe ser la salida de descomposicion_clasica()" =
      is.list(desc) && all(c("componentes", "indices", "tipo") %in% names(desc)),
    "titulo debe ser un texto" = is.character(titulo) && length(titulo) == 1
  )
  comp <- desc$componentes
  idx <- desc$indices
  ref <- if (desc$tipo == "aditiva") 0 else 1
  p1 <- ggplot(comp, aes(x = fecha)) +
    geom_line(aes(y = y), color = "gray60") +
    geom_line(aes(y = tendencia), color = "darkred", linewidth = 0.9, na.rm = TRUE) +
    labs(title = paste0(titulo, " - descomposicion ", desc$tipo),
         subtitle = "Gris: serie. Rojo: tendencia por media movil centrada",
         x = NULL, y = desc$unidad) +
    theme_minimal()
  p2 <- ggplot(idx, aes(x = estacion, y = indice)) +
    geom_col(fill = "steelblue") +
    geom_hline(yintercept = ref) +
    scale_x_continuous(breaks = idx$estacion, labels = idx$etiqueta) +
    labs(title = "Indices estacionales normalizados",
         subtitle = if (desc$tipo == "aditiva") "Suman cero" else "Promedian uno",
         x = NULL, y = if (desc$tipo == "aditiva") desc$unidad else "Indice") +
    theme_minimal()
  p3 <- ggplot(comp, aes(x = fecha, y = residuo)) +
    geom_line(color = "darkgreen", na.rm = TRUE) +
    geom_hline(yintercept = ref, linetype = "dashed") +
    labs(title = "Residuo", x = "Fecha",
         y = if (desc$tipo == "aditiva") desc$unidad else "Cociente") +
    theme_minimal()
  p1 / p2 / p3
}