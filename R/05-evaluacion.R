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