library(ggplot2)

diferenciar <- function(y, d = 0, D = 0, s = 1) {
  stopifnot(
    "y debe ser un vector numerico" = is.numeric(y),
    "y no puede tener NA" = !anyNA(y),
    "d debe ser un entero no negativo" = length(d) == 1 && d == round(d) && d >= 0,
    "D debe ser un entero no negativo" = length(D) == 1 && D == round(D) && D >= 0,
    "s debe ser un entero mayor o igual a 1" = length(s) == 1 && s == round(s) && s >= 1,
    "la serie es demasiado corta para las diferencias pedidas" = length(y) > d + D * s
  )
  x <- y
  if (D > 0) {
    for (j in 1:D) {
      n <- length(x)
      x <- x[(s + 1):n] - x[1:(n - s)]
    }
  }
  if (d > 0) {
    for (j in 1:d) {
      n <- length(x)
      x <- x[2:n] - x[1:(n - 1)]
    }
  }
  x
}

multiplicar_polinomios <- function(a, b) {
  stopifnot("a y b deben ser vectores numericos" = is.numeric(a) && is.numeric(b))
  resultado <- numeric(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      resultado[i + j - 1] <- resultado[i + j - 1] + a[i] * b[j]
    }
  }
  resultado
}

coef_diferencia <- function(d = 0, D = 0, s = 1) {
  stopifnot(
    "d debe ser un entero no negativo" = length(d) == 1 && d == round(d) && d >= 0,
    "D debe ser un entero no negativo" = length(D) == 1 && D == round(D) && D >= 0,
    "s debe ser un entero mayor o igual a 1" = length(s) == 1 && s == round(s) && s >= 1,
    "debe haber al menos una diferencia" = d + D >= 1
  )
  p <- 1
  if (d > 0) for (j in 1:d) p <- multiplicar_polinomios(p, c(1, -1))
  if (D > 0) for (j in 1:D) p <- multiplicar_polinomios(p, c(1, rep(0, s - 1), -1))
  p[-1]
}

polinomio_rezago <- function(coef) {
  stopifnot(
    "coef debe ser un vector numerico sin NA" = is.numeric(coef) && !anyNA(coef),
    "coef debe tener al menos un coeficiente" = length(coef) >= 1,
    "el ultimo coeficiente no puede ser cero" = coef[length(coef)] != 0
  )
  raices <- polyroot(c(1, coef))
  argumento <- Arg(raices)
  tibble(
    real = round(Re(raices), 10),
    imaginaria = round(Im(raices), 10),
    modulo = Mod(raices),
    argumento = argumento,
    periodo = ifelse(abs(argumento) < 1e-8, Inf, 2 * pi / abs(argumento))
  )
}

graficar_raices <- function(raices, titulo) {
  stopifnot(
    "raices debe ser la salida de polinomio_rezago()" =
      is.data.frame(raices) && all(c("real", "imaginaria", "modulo") %in% names(raices)),
    "titulo debe ser un texto" = is.character(titulo) && length(titulo) == 1
  )
  circulo <- data.frame(x = cos(seq(0, 2 * pi, length.out = 200)),
                        y = sin(seq(0, 2 * pi, length.out = 200)))
  conteo <- aggregate(list(multiplicidad = rep(1, nrow(raices))),
                      by = list(real = round(raices$real, 6), imaginaria = round(raices$imaginaria, 6)),
                      FUN = sum)
  ggplot() +
    geom_path(data = circulo, aes(x = x, y = y), color = "gray50", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "gray80") +
    geom_vline(xintercept = 0, color = "gray80") +
    geom_point(data = conteo, aes(x = real, y = imaginaria, size = factor(multiplicidad)),
               color = "darkred") +
    scale_size_manual(values = c("1" = 3, "2" = 5, "3" = 7), name = "Multiplicidad") +
    coord_equal(xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3)) +
    labs(title = titulo, subtitle = "Linea punteada: circulo unitario",
         x = "Parte real", y = "Parte imaginaria") +
    theme_minimal()
}