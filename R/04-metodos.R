# Funciones reutilizadas de la Tarea 1 de Juan David Garro Arboleda y adaptadas
# al contrato del Trabajo 1. En esta version se endurecen las validaciones, las
# pruebas t de tendencia usan el error HAC y los pronosticos historicos de la
# tendencia exponencial evitan usar la varianza de observaciones futuras.

validar_y <- function(y, minimo = 2L) {
  if (!is.numeric(y) || !is.null(dim(y)) || length(y) < minimo || any(!is.finite(y))) {
    stop("y debe ser un vector numerico finito con al menos ", minimo, " observaciones.",
         call. = FALSE)
  }
  as.numeric(y)
}

validar_h <- function(h) {
  if (length(h) != 1L || !is.numeric(h) || !is.finite(h) || h < 1 || h != round(h)) {
    stop("h debe ser un entero positivo.", call. = FALSE)
  }
  as.integer(h)
}

validar_parametro_unitario <- function(x, nombre) {
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x <= 0 || x >= 1) {
    stop(nombre, " debe estar en (0, 1).", call. = FALSE)
  }
  as.numeric(x)
}

ajustar_media <- function(y) {
  y <- validar_y(y)
  n <- length(y)
  yhat <- rep(NA_real_, n)
  yhat[2:n] <- cumsum(y)[1:(n - 1L)] / seq_len(n - 1L)
  media_final <- mean(y)

  list(
    yhat = yhat,
    pronosticar = function(h) rep(media_final, validar_h(h)),
    parametros = list(media = media_final, n = n)
  )
}

ajustar_mm <- function(y, k) {
  y <- validar_y(y)
  n <- length(y)
  if (length(k) != 1L || !is.numeric(k) || !is.finite(k) ||
      k != round(k) || k < 2 || k >= n) {
    stop("k debe ser un entero entre 2 y n - 1.", call. = FALSE)
  }
  k <- as.integer(k)
  yhat <- rep(NA_real_, n)
  for (t in (k + 1L):n) yhat[t] <- mean(y[(t - k):(t - 1L)])
  ultima_media <- mean(tail(y, k))

  list(
    yhat = yhat,
    pronosticar = function(h) rep(ultima_media, validar_h(h)),
    parametros = list(k = k, ultima_media_movil = ultima_media)
  )
}

ajustar_ses <- function(y, alpha) {
  y <- validar_y(y)
  alpha <- validar_parametro_unitario(alpha, "alpha")
  n <- length(y)
  yhat <- rep(NA_real_, n)
  nivel <- rep(NA_real_, n)
  nivel[1] <- y[1]

  for (t in 2:n) {
    yhat[t] <- nivel[t - 1L]
    nivel[t] <- yhat[t] + alpha * (y[t] - yhat[t])
  }
  nivel_final <- nivel[n]

  list(
    yhat = yhat,
    pronosticar = function(h) rep(nivel_final, validar_h(h)),
    parametros = list(alpha = alpha, nivel_final = nivel_final, niveles = nivel)
  )
}

ajustar_dmm <- function(y, k) {
  y <- validar_y(y)
  n <- length(y)
  if (length(k) != 1L || !is.numeric(k) || !is.finite(k) ||
      k != round(k) || k < 2 || 2 * k > n) {
    stop("k debe ser entero, al menos 2, y cumplir 2k <= n.", call. = FALSE)
  }
  k <- as.integer(k)
  media_1 <- media_2 <- nivel <- pendiente <- rep(NA_real_, n)

  for (t in k:n) media_1[t] <- mean(y[(t - k + 1L):t])
  for (t in (2L * k - 1L):n) {
    media_2[t] <- mean(media_1[(t - k + 1L):t])
    nivel[t] <- 2 * media_1[t] - media_2[t]
    pendiente[t] <- 2 * (media_1[t] - media_2[t]) / (k - 1)
  }

  yhat <- rep(NA_real_, n)
  for (t in (2L * k):n) yhat[t] <- nivel[t - 1L] + pendiente[t - 1L]
  nivel_final <- nivel[n]
  pendiente_final <- pendiente[n]

  list(
    yhat = yhat,
    pronosticar = function(h) {
      h <- validar_h(h)
      nivel_final + pendiente_final * seq_len(h)
    },
    parametros = list(
      k = k,
      nivel_final = nivel_final,
      pendiente_final = pendiente_final,
      media_movil = media_1,
      doble_media_movil = media_2,
      nivel = nivel,
      pendiente = pendiente
    )
  )
}

matriz_tendencia <- function(t, tipo) {
  if (tipo == "lineal" || tipo == "exponencial") {
    cbind(intercepto = 1, t = t)
  } else {
    cbind(intercepto = 1, t = t, t2 = t^2)
  }
}

cov_hac <- function(X, e) {
  stopifnot(
    "X debe ser una matriz numerica" = is.matrix(X) && is.numeric(X),
    "e debe ser numerico y coincidir con las filas de X" =
      is.numeric(e) && length(e) == nrow(X) && !anyNA(e)
  )
  n <- nrow(X)
  L <- floor(4 * (n / 100)^(2 / 9))
  S <- matrix(0, ncol(X), ncol(X))

  for (t in seq_len(n)) S <- S + e[t]^2 * tcrossprod(X[t, ])
  if (L > 0L) {
    for (h in seq_len(L)) {
      peso <- 1 - h / (L + 1)
      G <- matrix(0, ncol(X), ncol(X))
      for (t in (h + 1L):n) {
        G <- G + e[t] * e[t - h] * tcrossprod(X[t, ], X[t - h, ])
      }
      S <- S + peso * (G + t(G))
    }
  }

  B <- solve(crossprod(X))
  list(cov = B %*% S %*% B, rezagos = L)
}

ajustar_tendencia <- function(y, tipo = c("lineal", "cuadratica", "exponencial"),
                              corregir_sesgo = FALSE) {
  tipo <- match.arg(tipo)
  y <- validar_y(y, minimo = 5L)
  if (length(corregir_sesgo) != 1L || !is.logical(corregir_sesgo) || is.na(corregir_sesgo)) {
    stop("corregir_sesgo debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (tipo == "exponencial" && any(y <= 0)) {
    stop("La tendencia exponencial exige observaciones positivas.", call. = FALSE)
  }

  n <- length(y)
  z <- if (tipo == "exponencial") log(y) else y
  X <- matriz_tendencia(seq_len(n), tipo)
  q <- ncol(X)
  coeficientes <- as.numeric(solve(crossprod(X), crossprod(X, z)))
  names(coeficientes) <- colnames(X)
  ajuste_escala <- as.numeric(X %*% coeficientes)
  residuos_escala <- z - ajuste_escala
  gl <- n - q
  sigma2 <- sum(residuos_escala^2) / gl

  cov_ordinaria <- sigma2 * solve(crossprod(X))
  hac <- cov_hac(X, residuos_escala)
  se_ordinario <- sqrt(pmax(0, diag(cov_ordinaria)))
  se_hac <- sqrt(pmax(0, diag(hac$cov)))
  t_hac <- coeficientes / se_hac
  p_hac <- 2 * stats::pt(abs(t_hac), df = gl, lower.tail = FALSE)
  tabla_coeficientes <- tibble::tibble(
    termino = names(coeficientes),
    estimacion = coeficientes,
    se_ordinario = se_ordinario,
    se_hac = se_hac,
    t_robusto = t_hac,
    p_valor_robusto = p_hac
  )

  transformar <- function(eta, varianza = sigma2) {
    if (tipo == "exponencial") {
      factor <- if (corregir_sesgo) exp(varianza / 2) else 1
      exp(eta) * factor
    } else {
      eta
    }
  }

  yhat <- rep(NA_real_, n)
  inicio <- if (tipo == "exponencial" && corregir_sesgo) q + 2L else q + 1L
  for (tt in inicio:n) {
    indices <- seq_len(tt - 1L)
    X0 <- matriz_tendencia(indices, tipo)
    z0 <- z[indices]
    b0 <- as.numeric(solve(crossprod(X0), crossprod(X0, z0)))
    var0 <- 0
    if (tipo == "exponencial" && corregir_sesgo) {
      e0 <- z0 - as.numeric(X0 %*% b0)
      var0 <- sum(e0^2) / (length(z0) - ncol(X0))
    }
    yhat[tt] <- transformar(
      as.numeric(matriz_tendencia(tt, tipo) %*% b0),
      varianza = var0
    )
  }

  ajuste <- transformar(ajuste_escala)
  ss_total <- sum((z - mean(z))^2)
  R2 <- if (ss_total == 0) NA_real_ else 1 - sum(residuos_escala^2) / ss_total
  pronosticar <- function(h) {
    h <- validar_h(h)
    eta <- as.numeric(matriz_tendencia(n + seq_len(h), tipo) %*% coeficientes)
    transformar(eta)
  }

  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(
      tipo = tipo,
      coeficientes = tabla_coeficientes,
      R2 = R2,
      sigma2 = sigma2,
      cov_hac = hac$cov,
      rezagos_hac = hac$rezagos,
      corregir_sesgo = corregir_sesgo,
      ajuste_muestra_completa = ajuste,
      residuos_escala = residuos_escala,
      matriz_diseno = X
    )
  )
}

ajustar_holt <- function(y, alpha, beta) {
  y <- validar_y(y)
  alpha <- validar_parametro_unitario(alpha, "alpha")
  beta <- validar_parametro_unitario(beta, "beta")
  n <- length(y)
  nivel <- pendiente <- yhat <- rep(NA_real_, n)
  nivel[1] <- y[1]
  pendiente[1] <- 0

  for (t in 2:n) {
    yhat[t] <- nivel[t - 1L] + pendiente[t - 1L]
    nivel[t] <- alpha * y[t] + (1 - alpha) * yhat[t]
    pendiente[t] <- beta * (nivel[t] - nivel[t - 1L]) +
      (1 - beta) * pendiente[t - 1L]
  }
  nivel_final <- nivel[n]
  pendiente_final <- pendiente[n]

  list(
    yhat = yhat,
    pronosticar = function(h) {
      h <- validar_h(h)
      nivel_final + pendiente_final * seq_len(h)
    },
    parametros = list(
      alpha = alpha,
      beta = beta,
      nivel_final = nivel_final,
      pendiente_final = pendiente_final,
      nivel = nivel,
      pendiente = pendiente
    )
  )
}

validar_periodo_estacional <- function(s, n, ciclos_minimos = 1L) {
  if (length(s) != 1L || !is.numeric(s) || !is.finite(s) ||
      s != round(s) || s < 2) {
    stop("s debe ser un entero mayor o igual que 2.", call. = FALSE)
  }
  s <- as.integer(s)
  if (n < ciclos_minimos * s) {
    stop("y debe contener al menos ", ciclos_minimos,
         " ciclos estacionales completos.", call. = FALSE)
  }
  s
}

ajustar_ingenuo <- function(y) {
  y <- validar_y(y)
  n <- length(y)
  yhat <- c(NA_real_, y[-n])
  ultimo <- y[n]

  list(
    yhat = yhat,
    pronosticar = function(h) rep(ultimo, validar_h(h)),
    parametros = list(ultimo_valor = ultimo)
  )
}

ajustar_ingenuo_estacional <- function(y, s) {
  y <- validar_y(y)
  n <- length(y)
  s <- validar_periodo_estacional(s, n)
  yhat <- rep(NA_real_, n)
  if (n > s) yhat[(s + 1L):n] <- y[seq_len(n - s)]
  ultimo_ciclo <- tail(y, s)

  list(
    yhat = yhat,
    pronosticar = function(h) {
      h <- validar_h(h)
      rep(ultimo_ciclo, length.out = h)
    },
    parametros = list(s = s, ultimo_ciclo = ultimo_ciclo)
  )
}

matriz_tendencia_estacional <- function(t, s) {
  estacion <- (as.integer(t) - 1L) %% s + 1L
  X <- cbind(intercepto = 1, t = as.numeric(t))
  for (j in 2:s) X <- cbind(X, as.numeric(estacion == j) - as.numeric(estacion == 1L))
  colnames(X)[-(1:2)] <- paste0("gamma_", 2:s)
  X
}

ajustar_tendencia_estacional <- function(y, s) {
  y <- validar_y(y)
  n <- length(y)
  s <- validar_periodo_estacional(s, n, ciclos_minimos = 2L)
  X <- matriz_tendencia_estacional(seq_len(n), s)
  q <- ncol(X)
  coeficientes <- as.numeric(solve(crossprod(X), crossprod(X, y)))
  names(coeficientes) <- colnames(X)
  ajuste <- as.numeric(X %*% coeficientes)
  residuos <- y - ajuste
  gl <- n - q
  sigma2 <- sum(residuos^2) / gl
  cov_ordinaria <- sigma2 * solve(crossprod(X))
  hac <- cov_hac(X, residuos)
  se_ordinario <- sqrt(pmax(0, diag(cov_ordinaria)))
  se_hac <- sqrt(pmax(0, diag(hac$cov)))
  t_hac <- coeficientes / se_hac
  tabla_coeficientes <- tibble::tibble(
    termino = names(coeficientes),
    estimacion = coeficientes,
    se_ordinario = se_ordinario,
    se_hac = se_hac,
    t_robusto = t_hac,
    p_valor_robusto = 2 * stats::pt(abs(t_hac), df = gl, lower.tail = FALSE)
  )

  efectos_estacionales <- c(
    -sum(coeficientes[paste0("gamma_", 2:s)]),
    coeficientes[paste0("gamma_", 2:s)]
  )
  names(efectos_estacionales) <- paste0("estacion_", seq_len(s))

  yhat <- rep(NA_real_, n)
  if (n > 2L * s) {
    for (tt in (2L * s + 1L):n) {
      indices <- seq_len(tt - 1L)
      X0 <- matriz_tendencia_estacional(indices, s)
      b0 <- as.numeric(solve(crossprod(X0), crossprod(X0, y[indices])))
      yhat[tt] <- as.numeric(matriz_tendencia_estacional(tt, s) %*% b0)
    }
  }

  list(
    yhat = yhat,
    pronosticar = function(h) {
      h <- validar_h(h)
      as.numeric(matriz_tendencia_estacional(n + seq_len(h), s) %*% coeficientes)
    },
    parametros = list(
      s = s,
      coeficientes = tabla_coeficientes,
      efectos_estacionales = efectos_estacionales,
      R2 = if (sum((y - mean(y))^2) == 0) NA_real_ else
        1 - sum(residuos^2) / sum((y - mean(y))^2),
      sigma2 = sigma2,
      cov_hac = hac$cov,
      rezagos_hac = hac$rezagos,
      ajuste_muestra_completa = ajuste,
      residuos = residuos,
      matriz_diseno = X
    )
  )
}

inicializar_hw <- function(y, s, tipo = c("aditivo", "multiplicativo")) {
  tipo <- match.arg(tipo)
  ventana <- y[seq_len(2L * s)]
  medias <- c(mean(ventana[seq_len(s)]), mean(ventana[s + seq_len(s)]))
  pendiente <- (medias[2] - medias[1]) / s
  nivel_inicial <- medias[1] - pendiente * (s - 1) / 2
  tendencia <- nivel_inicial + pendiente * (seq_len(2L * s) - 1L)

  if (tipo == "aditivo") {
    estacional <- vapply(seq_len(s), function(j) {
      mean(ventana[c(j, j + s)] - tendencia[c(j, j + s)])
    }, numeric(1))
    estacional <- estacional - mean(estacional)
  } else {
    if (any(tendencia <= 0)) {
      stop("La inicializacion multiplicativa produjo un nivel no positivo.", call. = FALSE)
    }
    estacional <- vapply(seq_len(s), function(j) {
      mean(ventana[c(j, j + s)] / tendencia[c(j, j + s)])
    }, numeric(1))
    estacional <- estacional / mean(estacional)
    if (any(!is.finite(estacional)) || any(estacional <= 0)) {
      stop("Los indices estacionales multiplicativos deben ser positivos.", call. = FALSE)
    }
  }

  list(
    nivel = nivel_inicial + pendiente * (2L * s - 1L),
    pendiente = pendiente,
    estacional = estacional,
    nivel_origen = nivel_inicial
  )
}

ajustar_hw_base <- function(y, s, alpha, beta, gamma,
                            tipo = c("aditivo", "multiplicativo")) {
  tipo <- match.arg(tipo)
  y <- validar_y(y)
  n <- length(y)
  s <- validar_periodo_estacional(s, n, ciclos_minimos = 2L)
  alpha <- validar_parametro_unitario(alpha, "alpha")
  beta <- validar_parametro_unitario(beta, "beta")
  gamma <- validar_parametro_unitario(gamma, "gamma")
  if (tipo == "multiplicativo" && any(y <= 0)) {
    stop("Holt-Winters multiplicativo exige observaciones positivas.", call. = FALSE)
  }

  inicio <- inicializar_hw(y, s, tipo)
  corte <- 2L * s
  nivel <- pendiente <- yhat <- rep(NA_real_, n)
  estacional_historia <- rep(NA_real_, n)
  nivel[corte] <- inicio$nivel
  pendiente[corte] <- inicio$pendiente
  estacional_actual <- inicio$estacional
  estacional_historia[(corte - s + 1L):corte] <- estacional_actual

  if (n > corte) {
    for (tt in (corte + 1L):n) {
      j <- (tt - 1L) %% s + 1L
      base <- nivel[tt - 1L] + pendiente[tt - 1L]
      if (tipo == "aditivo") {
        yhat[tt] <- base + estacional_actual[j]
        nivel[tt] <- alpha * (y[tt] - estacional_actual[j]) + (1 - alpha) * base
        pendiente[tt] <- beta * (nivel[tt] - nivel[tt - 1L]) +
          (1 - beta) * pendiente[tt - 1L]
        estacional_actual[j] <- gamma * (y[tt] - nivel[tt]) +
          (1 - gamma) * estacional_actual[j]
      } else {
        if (base <= 0) stop("El nivel multiplicativo debe permanecer positivo.", call. = FALSE)
        yhat[tt] <- base * estacional_actual[j]
        nivel[tt] <- alpha * (y[tt] / estacional_actual[j]) + (1 - alpha) * base
        pendiente[tt] <- beta * (nivel[tt] - nivel[tt - 1L]) +
          (1 - beta) * pendiente[tt - 1L]
        if (nivel[tt] <= 0) stop("El nivel multiplicativo debe permanecer positivo.", call. = FALSE)
        estacional_actual[j] <- gamma * (y[tt] / nivel[tt]) +
          (1 - gamma) * estacional_actual[j]
      }
      estacional_historia[tt] <- estacional_actual[j]
    }
  }

  nivel_final <- nivel[n]
  pendiente_final <- pendiente[n]
  estacional_final <- estacional_actual
  pronosticar <- function(h) {
    h <- validar_h(h)
    posiciones <- (n + seq_len(h) - 1L) %% s + 1L
    base <- nivel_final + pendiente_final * seq_len(h)
    if (tipo == "aditivo") base + estacional_final[posiciones] else
      base * estacional_final[posiciones]
  }

  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(
      tipo = tipo,
      s = s,
      alpha = alpha,
      beta = beta,
      gamma = gamma,
      nivel_inicial = inicio$nivel_origen,
      pendiente_inicial = inicio$pendiente,
      estacional_inicial = inicio$estacional,
      nivel_final = nivel_final,
      pendiente_final = pendiente_final,
      estacional_final = estacional_final,
      nivel = nivel,
      pendiente = pendiente,
      estacional = estacional_historia
    )
  )
}

ajustar_hw <- function(y, s, alpha, beta, gamma) {
  ajustar_hw_base(y, s, alpha, beta, gamma, tipo = "aditivo")
}

ajustar_hw_mult <- function(y, s, alpha, beta, gamma) {
  ajustar_hw_base(y, s, alpha, beta, gamma, tipo = "multiplicativo")
}

mse_un_paso <- function(modelo, y) {
  y <- validar_y(y)
  if (!is.list(modelo) || !all(c("yhat", "pronosticar", "parametros") %in% names(modelo)) ||
      length(modelo$yhat) != length(y)) {
    stop("modelo no cumple el contrato de los metodos de pronostico.", call. = FALSE)
  }
  validos <- is.finite(modelo$yhat)
  if (!any(validos)) Inf else mean((y[validos] - modelo$yhat[validos])^2)
}
