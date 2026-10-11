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

mse_un_paso <- function(modelo, y) {
  y <- validar_y(y)
  if (!is.list(modelo) || !all(c("yhat", "pronosticar", "parametros") %in% names(modelo)) ||
      length(modelo$yhat) != length(y)) {
    stop("modelo no cumple el contrato de los metodos de pronostico.", call. = FALSE)
  }
  validos <- is.finite(modelo$yhat)
  if (!any(validos)) Inf else mean((y[validos] - modelo$yhat[validos])^2)
}
