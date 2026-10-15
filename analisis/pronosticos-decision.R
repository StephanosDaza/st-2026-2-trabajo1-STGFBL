# Trabajo 1 STGFBL: pronosticos para las decisiones del punto 8.
# Ejecutar en una sesion de R DESPUES de source("analisis/correr-todo.R").
# No modifica los resultados ni los graficos existentes del punto 6.

if (!exists("tabla_origen_movil") || !exists("resultados")) {
  stop('Primero ejecuta source("analisis/correr-todo.R") en esta sesion de R.')
}
if (!dir.exists("figs")) dir.create("figs")
if (!dir.exists("informe")) dir.create("informe")

pronosticos_decision <- list()
for (k in names(config)) {
  H <- config[[k]]$H
  filas <- tabla_origen_movil[
    tabla_origen_movil$serie == k & tabla_origen_movil$horizonte == H, ]
  mejor <- filas[which.min(filas$RMSE), ]
  id <- mejor$metodo_id[1]
  especificacion <- crear_catalogo_metodos(config[[k]]$s)[[id]]
  inicial <- resultados[[k]]$pronostico$ajustes_estimacion[[id]]
  parametros <- c(as.list(inicial$parametros), especificacion$fijos)
  errores <- resultados[[k]]$pronostico$origen_movil[[id]]
  final <- pronostico_final(
    datos = resultados[[k]]$datos, H = H,
    ajustar = especificacion$ajustar,
    parametros = parametros, errores_origen = errores)
  pronosticos_decision[[k]] <- final$tabla
  nombre_csv <- paste0("informe/", k, "-pronostico-decision.csv")
  write.csv(final$tabla, nombre_csv, row.names = FALSE)
  grafico <- graficar_pronostico_final(
    resultados[[k]]$datos, final$tabla,
    titulo = paste(config[[k]]$nombre, "-", mejor$metodo[1]))
  ggsave(paste0("figs/", k, "-decision.png"), grafico,
         width = 8, height = 4.3, dpi = 150)
  cat("\n", k, "| Horizonte:", H, "| Metodo:", mejor$metodo[1],
      "| RMSE:", round(mejor$RMSE[1], 3), "\n")
  print(final$tabla[, c("fecha", "pronostico")], row.names = FALSE)
}

# Identificar el primer mes en que se preve superar la capacidad de 450 mil usuarios.
usuarios <- pronosticos_decision$serie3
cruces <- which(usuarios$pronostico >= 450)
if (length(cruces)) {
  i <- cruces[1]
  fecha_cruce <- usuarios$fecha[i]
  fecha_orden <- seq(fecha_cruce, by = "-9 months", length.out = 2)[2]
  cat("\nSERIE 3 - Primer cruce previsto de 450 mil:",
      as.character(fecha_cruce), "\n")
  cat("Fecha de orden orientativa (9 meses antes):",
      as.character(fecha_orden), "\n")
  cat("Advertencia: fechas basadas en el pronostico puntual; no son garantias.\n")
} else {
  cat("\nSERIE 3: el pronostico puntual no alcanza 450 mil usuarios",
      "en los", config$serie3$H, "meses evaluados.\n")
}
cat("\nSe guardaron 3 CSV en informe/ y 3 graficos en figs/.\n")
