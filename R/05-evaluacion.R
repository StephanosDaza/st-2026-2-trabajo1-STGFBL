particion_serie <- function(datos, H) {
  stopifnot(
    "datos debe ser el tibble generado por leer_serie" =
      is.data.frame(datos) && all(c("fecha", "y") %in% names(datos)),
    "H debe ser un entero mayor o igual a 4" = length(H) == 1 && H == round(H) && H >= 4
  )
  T <- nrow(datos)
  n0 <- floor(0.8 * T)
  stopifnot("H es demasiado grande para el tramo de validacion" = T - H >= n0)
  f <- datos$fecha
  list(
    T = T, n0 = n0, H = H,
    origenes_h1 = n0:(T - 1),
    origenes_H = n0:(T - H),
    tabla = tibble(
      T = T, n0 = n0, H = H,
      n_origenes_h1 = length(n0:(T - 1)),
      n_origenes_H = length(n0:(T - H)),
      estimacion = paste(f[1], "a", f[n0]),
      validacion = paste(f[n0 + 1], "a", f[T]),
      origenes_h1 = paste(f[n0], "a", f[T - 1]),
      origenes_H = paste(f[n0], "a", f[T - H])
    )
  )
}

optimizar <- function(y, ajustar, rejilla, fijos = list()) {
  y <- validar_y(y)
  if (!is.function(ajustar)) stop("ajustar debe ser una funcion.", call. = FALSE)
  if (!is.data.frame(rejilla) || nrow(rejilla) < 1L || ncol(rejilla) < 1L ||
      anyDuplicated(names(rejilla)) || any(!vapply(rejilla, is.numeric, logical(1))) ||
      any(!vapply(rejilla, function(x) all(is.finite(x)), logical(1)))) {
    stop("rejilla debe ser un data.frame no vacio de parametros numericos finitos.",
         call. = FALSE)
  }
  if (!is.list(fijos) || (length(fijos) > 0L &&
      (is.null(names(fijos)) || anyDuplicated(names(fijos)) ||
       any(names(fijos) %in% names(rejilla))))) {
    stop("fijos debe ser una lista nombrada sin parametros repetidos en rejilla.",
         call. = FALSE)
  }

  mse <- rep(Inf, nrow(rejilla))
  valido <- rep(FALSE, nrow(rejilla))
  mensaje <- rep(NA_character_, nrow(rejilla))
  for (i in seq_len(nrow(rejilla))) {
    argumentos <- c(list(y = y), as.list(rejilla[i, , drop = FALSE]), fijos)
    modelo <- try(do.call(ajustar, argumentos), silent = TRUE)
    if (inherits(modelo, "try-error")) {
      mensaje[i] <- as.character(modelo)
    } else {
      mse[i] <- mse_un_paso(modelo, y)
      valido[i] <- is.finite(mse[i])
    }
  }
  if (!any(valido)) {
    stop("Ninguna combinacion de la rejilla produjo un MSE finito.", call. = FALSE)
  }

  superficie <- tibble::as_tibble(rejilla)
  superficie$mse <- mse
  superficie$valido <- valido
  superficie$mensaje <- mensaje
  indice <- which.min(mse)
  mejores_parametros <- rejilla[indice, , drop = FALSE]
  mejor_modelo <- do.call(
    ajustar,
    c(list(y = y), as.list(mejores_parametros), fijos)
  )

  list(
    modelo = mejor_modelo,
    parametros = tibble::as_tibble(mejores_parametros),
    mse = mse[indice],
    superficie = superficie,
    fijos = fijos
  )
}

graficar_optimizacion <- function(resultado, titulo = "Optimizacion por MSE") {
  if (!is.list(resultado) || !all(c("parametros", "mse", "superficie") %in% names(resultado))) {
    stop("resultado debe provenir de optimizar().", call. = FALSE)
  }
  tabla <- resultado$superficie[resultado$superficie$valido, , drop = FALSE]
  parametros <- setdiff(names(resultado$superficie), c("mse", "valido", "mensaje"))
  if (length(parametros) < 1L || length(parametros) > 3L) {
    stop("La visualizacion admite entre uno y tres parametros.", call. = FALSE)
  }
  subtitulo <- paste0("Minimo MSE = ", format(resultado$mse, digits = 6))

  if (length(parametros) == 1L) {
    ggplot2::ggplot(tabla, ggplot2::aes(x = .data[[parametros[1]]], y = .data$mse)) +
      ggplot2::geom_line(linewidth = 0.6, colour = "#1f5a94") +
      ggplot2::geom_point(size = 1.5, colour = "#1f5a94") +
      ggplot2::geom_point(
        data = resultado$parametros,
        ggplot2::aes(x = .data[[parametros[1]]], y = resultado$mse),
        colour = "#b2182b", size = 3
      ) +
      ggplot2::labs(title = titulo, subtitle = subtitulo,
                    x = parametros[1], y = "MSE de un paso") +
      ggplot2::theme_minimal()
  } else {
    grafico <- ggplot2::ggplot(
      tabla,
      ggplot2::aes(x = .data[[parametros[1]]], y = .data[[parametros[2]]], fill = .data$mse)
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(option = "C", name = "MSE") +
      ggplot2::geom_point(
        data = resultado$parametros,
        ggplot2::aes(x = .data[[parametros[1]]], y = .data[[parametros[2]]]),
        inherit.aes = FALSE, shape = 4, stroke = 1.2, size = 3.5, colour = "white"
      ) +
      ggplot2::labs(title = titulo, subtitle = subtitulo,
                    x = parametros[1], y = parametros[2]) +
      ggplot2::theme_minimal()
    if (length(parametros) == 3L) {
      grafico <- grafico + ggplot2::facet_wrap(
        ggplot2::vars(.data[[parametros[3]]]),
        labeller = ggplot2::label_both
      )
    }
    grafico
  }
}

crear_catalogo_metodos <- function(s) {
  if (length(s) != 1L || !is.numeric(s) || !is.finite(s) || s != round(s) || s < 2) {
    stop("s debe ser un entero mayor o igual que 2.", call. = FALSE)
  }
  s <- as.integer(s)
  cuadricula <- function(...) expand.grid(..., KEEP.OUT.ATTRS = FALSE,
                                           stringsAsFactors = FALSE)
  lista <- list(
    media = list(nombre = "Media simple", ajustar = ajustar_media),
    ingenuo = list(nombre = "Ingenuo", ajustar = ajustar_ingenuo),
    ingenuo_estacional = list(
      nombre = "Ingenuo estacional", ajustar = ajustar_ingenuo_estacional,
      fijos = list(s = s)
    ),
    mm = list(
      nombre = "Media movil", ajustar = ajustar_mm,
      rejilla = data.frame(k = 2:12)
    ),
    ses = list(
      nombre = "Suavizamiento exponencial simple", ajustar = ajustar_ses,
      rejilla = data.frame(alpha = seq(0.02, 0.98, by = 0.02))
    ),
    dmm = list(
      nombre = "Doble media movil", ajustar = ajustar_dmm,
      rejilla = data.frame(k = 3:12)
    ),
    tendencia_lineal = list(
      nombre = "Tendencia lineal", ajustar = ajustar_tendencia,
      fijos = list(tipo = "lineal")
    ),
    tendencia_cuadratica = list(
      nombre = "Tendencia cuadratica", ajustar = ajustar_tendencia,
      fijos = list(tipo = "cuadratica")
    ),
    tendencia_exponencial = list(
      nombre = "Tendencia exponencial", ajustar = ajustar_tendencia,
      fijos = list(tipo = "exponencial")
    ),
    tendencia_estacional = list(
      nombre = "Tendencia con ficticias de suma cero",
      ajustar = ajustar_tendencia_estacional, fijos = list(s = s)
    ),
    holt = list(
      nombre = "Holt", ajustar = ajustar_holt,
      rejilla = cuadricula(alpha = seq(0.05, 0.95, by = 0.05),
                           beta = seq(0.05, 0.95, by = 0.05))
    ),
    hw_aditivo = list(
      nombre = "Holt-Winters aditivo", ajustar = ajustar_hw,
      rejilla = cuadricula(alpha = seq(0.1, 0.9, by = 0.1),
                           beta = seq(0.1, 0.9, by = 0.1),
                           gamma = seq(0.1, 0.9, by = 0.1)),
      fijos = list(s = s)
    ),
    hw_multiplicativo = list(
      nombre = "Holt-Winters multiplicativo", ajustar = ajustar_hw_mult,
      rejilla = cuadricula(alpha = seq(0.1, 0.9, by = 0.1),
                           beta = seq(0.1, 0.9, by = 0.1),
                           gamma = seq(0.1, 0.9, by = 0.1)),
      fijos = list(s = s)
    )
  )
  for (id in names(lista)) {
    lista[[id]]$id <- id
    if (is.null(lista[[id]]$fijos)) lista[[id]]$fijos <- list()
    if (is.null(lista[[id]]$rejilla)) lista[[id]]["rejilla"] <- list(NULL)
  }
  lista
}

ajustar_especificacion <- function(y, especificacion) {
  if (!is.list(especificacion) ||
      !all(c("id", "nombre", "ajustar", "fijos", "rejilla") %in% names(especificacion))) {
    stop("especificacion debe provenir de crear_catalogo_metodos().", call. = FALSE)
  }
  if (is.null(especificacion$rejilla)) {
    modelo <- do.call(especificacion$ajustar, c(list(y = y), especificacion$fijos))
    list(
      modelo = modelo,
      parametros = tibble::tibble(),
      mse = mse_un_paso(modelo, y),
      superficie = NULL,
      fijos = especificacion$fijos
    )
  } else {
    optimizar(y, especificacion$ajustar, especificacion$rejilla,
              fijos = especificacion$fijos)
  }
}

definir_asignacion_metodos <- function(catalogo) {
  if (!is.list(catalogo) || is.null(names(catalogo))) {
    stop("catalogo debe provenir de crear_catalogo_metodos().", call. = FALSE)
  }
  aplicados <- list(
    serie1 = c("ingenuo_estacional", "tendencia_estacional", "hw_aditivo"),
    serie2 = c("media", "ingenuo", "mm", "ses"),
    serie3 = c("ingenuo", "dmm", "tendencia_lineal", "tendencia_cuadratica",
               "tendencia_exponencial", "holt")
  )
  contrastes <- list(
    serie1 = "hw_multiplicativo",
    serie2 = "ingenuo_estacional",
    serie3 = "tendencia_estacional"
  )
  evidencia <- c(
    serie1 = paste(
      "Tendencia creciente y estacionalidad mensual s=12;",
      "se restringe la comparacion a metodos que representan estacionalidad.",
      "La amplitud es aditiva (correlacion media-desviacion 0.014)."
    ),
    serie2 = paste(
      "Nivel estable y dependencia local; el rango de medias por dia es 0.691 MWh",
      "frente a una desviacion de 4.741 MWh, sin evidencia semanal."
    ),
    serie3 = paste(
      "Tendencia creciente sin estacionalidad material; la amplitud estacional estimada es 5.421",
      "y la desviacion residual 5.772 miles de usuarios."
    )
  )

  filas <- lapply(names(aplicados), function(serie) {
    ids <- names(catalogo)
    rol <- ifelse(ids %in% contrastes[[serie]], "Contraste",
                  ifelse(ids %in% aplicados[[serie]], "Aplicado", "No aplicado"))
    tibble::tibble(
      serie = serie,
      metodo_id = ids,
      metodo = vapply(catalogo, `[[`, character(1), "nombre"),
      decision = rol != "No aplicado",
      rol = rol,
      evidencia = unname(evidencia[serie])
    )
  })
  tabla <- do.call(rbind, filas)
  if (any(!vapply(names(catalogo), function(id) {
    any(tabla$metodo_id == id & tabla$decision)
  }, logical(1)))) {
    stop("Cada metodo debe asignarse al menos a una serie.", call. = FALSE)
  }
  if (any(!vapply(names(aplicados), function(serie) {
    any(tabla$serie == serie & tabla$rol == "Contraste")
  }, logical(1)))) {
    stop("Cada serie debe incluir al menos un metodo de contraste.", call. = FALSE)
  }
  tabla
}
