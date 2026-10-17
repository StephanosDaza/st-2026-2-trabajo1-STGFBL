https://github.com/StephanosDaza/st-2026-2-trabajo1-STGFBL — Código del equipo: STGFBL

# Trabajo 1: tres series de Energía del Altiplano

Series de Tiempo, Pregrado en Estadística, UNAL Medellín, 2026-II. Equipo STGFBL.

## Integrantes y desarrollo del trabajo

- **Esteban Daza Palomino.** Puntos 1 a 5: lectura y fichas de las series, partición en tramos y orígenes, gráficos, decisión sobre la estacionalidad y descomposición clásica, operador de rezago (diferencias, polinomios y raíces) y dependencia y estacionariedad (ACF, PACF, Ljung-Box, bloques y varianza de largo plazo). Secciones 1 a 5 del informe, bloques de verificación y diapositivas de la exposición.
- **Juan David Garro Arboleda.** Punto 6: los trece métodos de pronóstico, la optimización en el tramo de estimación, las regresiones con error robusto, la evaluación por origen móvil, la validación de los ganadores y el pronóstico final. Pruebas de los métodos en `tests/` y sección 6 del informe.
- **Sara Jurado Giraldo.** Puntos 7 a 9: conclusión técnica, argumentos para los interesados, nota de una página, pronósticos para las decisiones del punto 8, README y reproducibilidad. Secciones 7 y 8 del informe.

## Contenido del repositorio

| Archivo | Contenido | Autor |
|:---|:---|:---|
| `datos/` | Los tres csv, tal como se entregaron | |
| `R/00-lectura.R` | `leer_serie()` y `ficha_serie()` | Esteban Daza |
| `R/01-graficos.R` | `graficar_serie()`, `grafico_estacional()`, `grafico_rezagos()`, `correlograma()`, `descomposicion_clasica()` y sus gráficos | Esteban Daza |
| `R/02-operadores.R` | `diferenciar()`, `polinomio_rezago()`, coeficientes de los polinomios de diferencia y gráfico de raíces | Esteban Daza |
| `R/03-dependencia.R` | `acf_muestral()`, `pacf_dl()`, `ljung_box()`, bandas de ruido blanco y de Bartlett, `varianza_largo_plazo()` | Esteban Daza |
| `R/04-metodos.R` | Un método por función: media, ingenuo, ingenuo estacional, media móvil, SES, doble media móvil, tendencias, tendencia con ficticias, Holt y Holt-Winters aditivo y multiplicativo | Juan David Garro |
| `R/05-evaluacion.R` | `particion_serie()` (Esteban Daza); `optimizar()`, `origen_movil()`, `medidas()`, `validar_errores()`, diagnóstico de regresiones y pronóstico final (Juan David Garro) | Esteban Daza, Juan David Garro |
| `analisis/correr-todo.R` | Carga `R/`, corre las tres series en orden, guarda las figuras, ejecuta los bloques de verificación y escribe `sesion-info.txt` | Esteban Daza (puntos 2 a 5), Juan David Garro (punto 6), Sara Jurado (punto 8) |
| `analisis/pronosticos-decision.R` | Pronósticos con el ganador de cada horizonte de decisión, para el punto 8 | Sara Jurado |
| `tests/` | Pruebas de los métodos sobre ejemplos pequeños calculables a mano | Juan David Garro |
| `informe/informe.qmd` | Informe; toma todas las cifras de la ejecución de `correr-todo.R` | Esteban Daza (1 a 5), Juan David Garro (6), Sara Jurado (7 y 8) |
| `informe/informe.html` | El informe renderizado | |
| `informe/nota-interesados.qmd`, `.css`, `.pdf` | Nota de una página para los interesados | Sara Jurado |
| `informe/serieK-pronostico-decision.csv` | Pronósticos del punto 8, generados por `pronosticos-decision.R` | |
| `exposicion/STGFBL.pdf` | Diapositivas de la exposición | Esteban Daza |
| `figs/` | Figuras generadas por `correr-todo.R` | |
| `figs/demostracion-holt.jpg`, `figs/demostracion-holt-winters.jpg` | Fotos de las demostraciones a mano del punto 4(d) | Esteban Daza |
| `sesion-info.txt` | Salida de `sessionInfo()` de la máquina en la que se cerró la entrega | Esteban Daza |

### Archivos reutilizados de la Tarea 1

- `R/00-lectura.R`: `leer_serie()` es de la Tarea 1 de Esteban Daza. Se agregó `ficha_serie()`.
- `R/01-graficos.R`: `graficar_serie()` y `correlograma()` son de la Tarea 1 de Esteban Daza. El correlograma se amplió con la PACF por Durbin-Levinson y la banda de Bartlett; se agregaron el gráfico estacional, el de rezagos y la descomposición clásica.
- `R/03-dependencia.R`: `acf_muestral()`, `banda_ruido_blanco()` y `ljung_box()` son de la Tarea 1 de Esteban Daza, donde estaban en `01-graficos.R` y `03-evaluacion.R`. Se agregaron `pacf_dl()`, `banda_bartlett()` y `varianza_largo_plazo()`.
- `R/04-metodos.R`: la media, la media móvil, el SES, la doble media móvil, las tendencias y Holt son de la Tarea 1 de Juan David Garro. Se endurecieron las validaciones, las pruebas t de las tendencias pasaron a usar el error estándar robusto con núcleo de Bartlett y los pronósticos históricos de la tendencia exponencial dejaron de usar la varianza de observaciones futuras. Se agregaron el ingenuo, el ingenuo estacional, la tendencia con ficticias y Holt-Winters.
- `R/05-evaluacion.R`: `optimizar()`, `medidas()`, `validar_errores()`, `jarque_bera()` y `durbin_watson()` son de la Tarea 1 de Juan David Garro. Se adaptaron a la evaluación con varios horizontes y se agregaron `origen_movil()`, el diagnóstico de las regresiones y el pronóstico final con bandas. `particion_serie()` se escribió para este trabajo.
- `R/02-operadores.R` se escribió para este trabajo.

## Ejecución

Desde la raíz del repositorio, con R 4.3 o posterior y los paquetes `dplyr`, `tidyr`, `purrr`, `tibble`, `readr`, `ggplot2` y `patchwork`:

```r
source("analisis/correr-todo.R")
```

Para renderizar el informe y la nota, con Quarto instalado:

```
quarto render informe/informe.qmd
quarto render informe/nota-interesados.qmd --to pdf
```

La nota en PDF requiere una instalación de LaTeX, como TinyTeX.

Tiempo de ejecución observado de `correr-todo.R`: 25 segundos (R 4.3.3, Ubuntu 24.04 en GitHub Codespaces).

## Resumen de resultados

| Serie | Transformación elegida | Ganador en h = 1 y parámetros | RMSE h = 1 (referente) | Ganador en H y parámetros | RMSE en H (referente) |
|:---|:---|:---|:---|:---|:---|
| 1. Demanda mensual (GWh), H = 12 | $(1-B^{12})$ | Tendencia con ficticias de suma cero, pendiente 1.97 GWh por mes | 12.32 (30.91, ingenuo estacional) | Holt-Winters aditivo, α = β = γ = 0.1 | 10.09 (27.54) |
| 2. Consumo diario (MWh), H = 7 | Ninguna | SES, α = 0.80 | 4.25 (4.41, ingenuo) | Media simple, sin parámetros | 5.77 (7.32) |
| 3. Usuarios conectados (miles), H = 21 | $(1-B)$ | Holt, α = 0.55, β = 0.05 | 6.94 (7.48, ingenuo) | Tendencia cuadrática | 7.53 (29.81) |

Los RMSE salen de la evaluación por origen móvil, con los mismos orígenes para todos los métodos de cada serie y horizonte. El detalle, las pruebas y las decisiones están en `informe/informe.html`.

## Declaración de uso de IA

**Esteban Daza Palomino** usó Claude (Anthropic) en los puntos 1 a 5.

- *Qué se pidió:* revisión de las funciones de la Tarea 1 para reutilizarlas en el trabajo y propuesta de código en R para adaptarlas a lo que pide el enunciado del trabajo. Además se solicitó apoyo en la redacción de las secciones 4 y 5 del informe y en una revisión final del repositorio.
- *Qué se recibió:* propuestas de código en R a partir de las funciones de la Tarea 1 y sugerencias de redacción para las secciones 4 y 5 del informe. En la revisión final se recibieron además dos ajustes al punto 6: la corrección de la prueba de fechas en `tests/pruebas-optimizacion-asignacion.R`, que fallaba en R 4.5.1, y el bloque de verificación que compara `ajustar_tendencia()` con `lm()`.
- *Qué se verificó por cuenta propia:* cada función se corrió sobre las tres series y su salida se revisó en consola; `diferenciar()`, `acf_muestral()` y `pacf_dl()` se compararon con `diff()`, `acf()` y `pacf()`, con diferencias menores que 1e-10; las cifras del informe salen del código con texto en línea; las decisiones de estacionalidad, de tipo de descomposición y de transformación se contrastaron con los gráficos y las tablas de cada serie.

**Juan David Garro Arboleda y Sara Jurado Giraldo** usaron ChatGPT (OpenAI).

- *Qué se pidió:* asistencia para organizar el código, interpretar resultados estadísticos, revisar la coherencia de las conclusiones, estructurar los argumentos dirigidos a los interesados y preparar documentación reproducible. En los puntos 7 a 9, apoyo para redactar las conclusiones técnicas, elaborar la nota para los interesados, integrar los scripts y organizar el repositorio.
- *Qué se recibió:* propuestas de código, de estructura y de redacción.
- *Qué se verificó por cuenta propia:* los resultados numéricos provienen de la ejecución de los scripts de R sobre las series entregadas, y las interpretaciones se construyeron a partir de esos resultados y de las condiciones del contexto del trabajo. Los métodos se probaron con los ejemplos de `tests/`, que se ejecutan desde `correr-todo.R`.
