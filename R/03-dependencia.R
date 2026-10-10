acf_muestral <- function(y, m) {
  stopifnot(
    "y debe ser un vector numerico" = is.numeric(y),
    "y no puede tener NA" = !anyNA(y),
    "m debe ser un entero entre 1 y n - 1" =
      length(m) == 1 && m == round(m) && m >= 1 && m < length(y)
  )
  n <- length(y)
  y_bar <- mean(y)
  c0 <- sum((y - y_bar)^2) / n
  r <- numeric(m)
  for (h in 1:m) {
    r[h] <- sum((y[1:(n - h)] - y_bar) * (y[(1 + h):n] - y_bar)) / n / c0
  }
  r
}

banda_ruido_blanco <- function(n, ci = 0.95) {
  stopifnot("n debe ser positivo" = n > 0, "ci debe estar en (0, 1)" = ci > 0 && ci < 1)
  qnorm((1 + ci) / 2) / sqrt(n)
}

banda_bartlett <- function(r, n, ci = 0.95) {
  stopifnot(
    "r debe ser un vector numerico sin NA" = is.numeric(r) && !anyNA(r),
    "n debe ser positivo" = n > 0,
    "ci debe estar en (0, 1)" = ci > 0 && ci < 1
  )
  m <- length(r)
  suma_previa <- c(0, cumsum(r^2))[1:m]
  qnorm((1 + ci) / 2) * sqrt((1 + 2 * suma_previa) / n)
}

pacf_dl <- function(r) {
  stopifnot(
    "r debe ser un vector numerico sin NA" = is.numeric(r) && !anyNA(r),
    "r debe tener al menos un rezago" = length(r) >= 1,
    "las autocorrelaciones deben estar en [-1, 1]" = all(abs(r) <= 1)
  )
  m <- length(r)
  pacf <- numeric(m)
  phi <- r[1]
  pacf[1] <- r[1]
  if (m >= 2) {
    for (k in 2:m) {
      num <- r[k] - sum(phi * r[(k - 1):1])
      den <- 1 - sum(phi * r[1:(k - 1)])
      phi_kk <- num / den
      phi <- c(phi - phi_kk * rev(phi), phi_kk)
      pacf[k] <- phi_kk
    }
  }
  pacf
}

ljung_box <- function(r, T, m, p = 0) {
  stopifnot(
    "r debe ser numerico" = is.numeric(r),
    "r debe tener al menos m autocorrelaciones" = length(r) >= m,
    "T debe ser mayor que m" = T > m,
    "m debe ser mayor que p" = m > p,
    "p no puede ser negativo" = p >= 0
  )
  Qm <- T * (T + 2) * sum((r[1:m]^2) / (T - (1:m)))
  gl <- m - p
  list(estadistico = Qm, m = m, grados_libertad = gl,
       valor_critico = qchisq(0.95, gl), valor_p = 1 - pchisq(Qm, gl))
}


varianza_largo_plazo <- function(y, L) {
  stopifnot(
    "y debe ser un vector numerico sin NA" = is.numeric(y) && !anyNA(y),
    "L debe ser un entero no negativo menor que n" =
      length(L) == 1 && L == round(L) && L >= 0 && L < length(y)
  )
  n <- length(y)
  y_c <- y - mean(y)
  gamma <- function(h) sum(y_c[1:(n - h)] * y_c[(1 + h):n]) / n
  total <- gamma(0)
  if (L > 0) {
    for (h in 1:L) total <- total + 2 * (1 - h / (L + 1)) * gamma(h)
  }
  total
}