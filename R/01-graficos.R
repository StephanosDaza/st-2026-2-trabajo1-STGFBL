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