Repositorio: https://github.com/StephanosDaza/st-2026-2-trabajo1-STGFBL — Código: STGFBL

# Trabajo 1 — Series de Tiempo

**Empresa:** Energía del Altiplano S.A. E.S.P.  
**Código del equipo:** STGFBL  
**Entorno de desarrollo:** R, Quarto y GitHub Codespaces.

## 1. Integrantes y responsabilidades

El trabajo fue desarrollado conjuntamente por tres integrantes, con la siguiente distribución de responsabilidades:

- **Esteban Daza:** desarrollo de los puntos 1 al 3, correspondientes a la caracterización de las series, partición de los datos y análisis exploratorio de sus patrones temporales.
- **Juan David Garro Arboleda:** desarrollo de los puntos 4 al 6, correspondientes al análisis de operadores, dependencia, estacionariedad, construcción de métodos de pronóstico, evaluación por origen móvil y validación de errores.
- **Sara Jurado Giraldo:** desarrollo de los puntos 7 al 9, correspondientes a conclusiones técnicas, argumentos para los interesados, nota ejecutiva, integración de resultados y organización del repositorio.

Las contribuciones se realizaron sobre un repositorio compartido, conservando el historial de versiones mediante Git.

## 2. Descripción del proyecto

El trabajo analiza tres series de tiempo asociadas a decisiones de planificación energética e infraestructura de Energía del Altiplano S.A. E.S.P.

Los problemas estudiados corresponden a la demanda mensual de energía, el consumo diario de una estación de bombeo y el crecimiento de usuarios conectados a una red de distribución.

El objetivo consiste en identificar los patrones temporales de cada serie, estudiar su dependencia y estacionariedad, construir métodos de pronóstico y comparar su desempeño mediante evaluación por origen móvil.

La selección de los métodos considera tanto el pronóstico a un paso como el horizonte de decisión de cada problema. Posteriormente, los resultados estadísticos se relacionan con las consecuencias económicas y operativas de los errores de pronóstico.

## 3. Estructura del repositorio

El repositorio separa los datos originales, las funciones, los scripts de ejecución, los gráficos y los documentos finales. Esta organización permite reproducir los análisis y modificar el informe sin alterar directamente las observaciones originales.

### Archivos principales y responsables

| Archivo o carpeta | Contenido | Responsable |
|---|---|---|
| `datos/` | Series originales suministradas para el trabajo | Equipo |
| `R/00-lectura.R` | Lectura y preparación de los datos | Esteban Daza |
| `R/01-graficos.R` | Gráficos y descripción de las series | Esteban Daza |
| `R/02-operadores.R` | Operadores de rezago y diferencias | Juan Garro |
| `R/03-dependencia.R` | Análisis de dependencia y estacionariedad | Juan Garro |
| `R/04-metodos.R` | Implementación de métodos de pronóstico | Juan Garro |
| `R/05-evaluacion.R` | Evaluación y diagnóstico de errores | Juan Garro |
| `analisis/correr-todo.R` | Ejecución integrada de los análisis | Equipo |
| `analisis/pronosticos-decision.R` | Pronósticos para las decisiones del punto 8 | Sara Jurado |
| `informe/informe.qmd` | Informe académico completo | Equipo |
| `informe/informe.html` | Versión HTML del informe | Equipo |
| `informe/nota-interesados.qmd` | Documento fuente de la nota ejecutiva | Sara Jurado |
| `informe/nota-interesados.css` | Diseño de la nota en HTML | Sara Jurado |
| `informe/nota-interesados.pdf` | Nota ejecutiva de una página | Sara Jurado |
| `figs/` | Figuras del análisis y los pronósticos | Equipo |
| `tests/` | Pruebas y verificaciones | Equipo |
| `exposicion/` | Material para la sustentación | Equipo |
| `sesion-info.txt` | Información del entorno de ejecución | Sara Jurado |

Los archivos CSV originales se conservan sin modificaciones. Los resultados derivados y las figuras pueden regenerarse mediante los scripts del proyecto.

Los archivos `informe/informe.qmd` y `informe/nota-interesados.qmd` constituyen las fuentes editables de los documentos finales.

## 4. Requisitos y reproducción

### Entorno utilizado

El proyecto fue ejecutado en GitHub Codespaces con las siguientes características:

- Sistema operativo: Ubuntu 24.04 LTS.
- Lenguaje: R 4.3.3.
- Generación de documentos: Quarto.
- Generación de PDF: TinyTeX.
- Control de versiones: Git y GitHub.

Entre las bibliotecas utilizadas se encuentran `dplyr`, `tidyr`, `purrr`, `tibble`, `readr`, `ggplot2`, `patchwork`, `knitr` y `rmarkdown`.

Las versiones instaladas de los paquetes y la configuración del entorno se documentan en `sesion-info.txt`.

### Instrucciones de ejecución

Los siguientes comandos deben ejecutarse desde la raíz del repositorio.

**1. Ejecutar los análisis estadísticos y generar los resultados:**

```bash
Rscript analisis/correr-todo.R
```

Este script ejecuta el análisis de las tres series y llama al archivo `analisis/pronosticos-decision.R`, encargado de generar los pronósticos correspondientes a las decisiones empresariales del punto 8.

**2. Generar el informe académico en HTML:**

```bash
quarto render informe/informe.qmd --to html
```

El resultado se guarda en `informe/informe.html`.

**3. Generar la nota ejecutiva en PDF:**

```bash
quarto render informe/nota-interesados.qmd --to pdf
```

El resultado se guarda en `informe/nota-interesados.pdf`.

Para generar el PDF se requiere una instalación funcional de LaTeX, como TinyTeX.

### Tiempo de ejecución

La ejecución del script principal se verificó mediante el comando:

```bash
time Rscript analisis/correr-todo.R
```

En GitHub Codespaces, el tiempo observado fue:

- **Tiempo real:** 24,825 segundos.
- **Tiempo de usuario:** 24,030 segundos.
- **Tiempo de sistema:** 0,344 segundos.

Por tanto, la reproducción del análisis estadístico y de los pronósticos requirió aproximadamente **25 segundos** en el entorno utilizado, sin incluir la generación posterior de los documentos mediante Quarto.

El tiempo puede variar según las características del equipo y las bibliotecas instaladas.

## 5. Resumen de resultados

Los métodos fueron evaluados mediante origen móvil, utilizando los mismos orígenes para comparar alternativas dentro de cada horizonte.

La principal medida de selección fue el RMSE, complementada con MAD, MAPE y diagnósticos de los errores.

### Comparación de métodos ganadores

| Serie | Horizonte | Método ganador | RMSE | RMSE referente | Mejora |
|---|---|---|---:|---:|---:|
| 1. Demanda mensual | 1 mes | Tendencia estacional | 12,30 | 30,90 | 60,1 % |
| 1. Demanda mensual | 12 meses | Holt-Winters aditivo | 10,09 | 27,50 | 63,4 % |
| 2. Consumo diario | 1 día | SES | 4,25 | 4,41 | 3,77 % |
| 2. Consumo diario | 7 días | Media simple | 5,77 | 7,32 | 21,2 % |
| 3. Usuarios conectados | 1 mes | Holt | 6,94 | 7,48 | 7,24 % |
| 3. Usuarios conectados | 21 meses | Tendencia cuadrática | 7,53 | 29,80 | 74,7 % |

**Unidades:** serie 1 en GWh, serie 2 en MWh y serie 3 en miles de usuarios.

La mejora porcentual corresponde a la reducción del RMSE respecto al método referente del mismo horizonte. Los resultados de horizontes diferentes se obtienen con conjuntos de evaluación que pueden diferir en cantidad de orígenes, por lo que no deben interpretarse como comparaciones directas entre horizontes.

### Serie 1. Demanda mensual de energía

La demanda mensual presenta una tendencia creciente y un patrón estacional anual de período 12. La transformación seleccionada fue la diferencia estacional, que permite estudiar las variaciones respecto al mismo mes del año anterior.

A un paso, la tendencia con ficticias estacionales obtuvo el menor RMSE, aproximadamente **12,30 GWh**, con una reducción del **60,1 %** frente al ingenuo estacional.

La regresión estima una pendiente de aproximadamente **1,97 GWh mensuales**, condicionada a los efectos estacionales.

A doce meses, Holt-Winters aditivo obtuvo el menor RMSE, aproximadamente **10,09 GWh**, con una mejora del **63,4 %** frente al referente. Sus parámetros optimizados fueron:

- α = 0,10.
- β = 0,10.
- γ = 0,10.

Los pronósticos para 2026 se encuentran aproximadamente entre **771 y 845 GWh mensuales**.

La recomendación consiste en contratar cantidades diferentes según el mes y considerar que un déficit de energía ocasiona un sobrecosto del 35 %, mientras que un excedente representa una pérdida del 20 % sobre el valor correspondiente.

### Serie 2. Consumo diario de la estación de bombeo

La serie presenta fluctuaciones alrededor de un nivel relativamente estable, sin evidencia suficiente de tendencia o estacionalidad semanal persistente.

No fue necesario aplicar diferenciación para representar su comportamiento principal.

A un día, el suavizamiento exponencial simple (SES) obtuvo el menor RMSE, aproximadamente **4,25 MWh**, con un parámetro optimizado α = 0,80.

La mejora frente al referente fue del **3,77 %**, relativamente pequeña en comparación con las otras series.

A siete días, la media simple obtuvo el menor RMSE, aproximadamente **5,77 MWh**, con una reducción del **21,2 %** frente al referente.

El pronóstico para los siete días posteriores al último dato disponible, del 19 al 25 de septiembre de 2025, fue aproximadamente **47,7 MWh diarios**, equivalentes a **333,9 MWh semanales**.

La recomendación de contratación considera que cada MWh faltante genera un costo adicional de 360.000 COP, mientras que cada MWh excedente produce una pérdida de 170.000 COP.

Estas cifras muestran que la cantidad contratada debe considerar tanto el pronóstico como las consecuencias económicas de los errores.

### Serie 3. Usuarios conectados a la red

La serie presenta una tendencia creciente en el número de usuarios conectados. La transformación seleccionada fue la primera diferencia, utilizada para estudiar los incrementos mensuales.

A un mes, Holt obtuvo el menor RMSE, aproximadamente **6,94 mil usuarios**, con una mejora del **7,24 %** frente al referente. Los parámetros seleccionados fueron:

- α = 0,55.
- β = 0,05.

Para el horizonte de veintiún meses, la tendencia cuadrática obtuvo el menor RMSE, aproximadamente **7,53 mil usuarios**, con una reducción del **74,7 %** respecto al referente.

La regresión cuadrática presentó un coeficiente lineal aproximado de 1,58 y un coeficiente cuadrático de −0,000422. Sin embargo, la curvatura no presentó evidencia estadística suficiente para considerarse distinta de cero al 5 % con el error estándar robusto. Por tanto, la selección del método se fundamenta en su desempeño predictivo y no en una afirmación concluyente de crecimiento cuadrático.

La proyección alcanza el umbral de **450 mil usuarios en septiembre de 2027**.

Como la instalación de un nuevo transformador requiere nueve meses, **diciembre de 2026** constituye una fecha orientativa para emitir la orden de compra.

La inversión asciende a **6.800 millones de COP**, por lo que la fecha propuesta debe revisarse a medida que se dispone de nuevas observaciones.

## 6. Diagnóstico y limitaciones

La selección de los métodos se basó en su desempeño sobre los orígenes de evaluación disponibles. Esto no garantiza que mantengan la misma precisión en períodos futuros.

Los diagnósticos de los errores de los ganadores a un paso no identificaron evidencia suficiente para rechazar la ausencia conjunta de autocorrelación al 5 %. Sin embargo, en la serie 1 se observó una señal aislada en el rezago 6, compatible con una dependencia semestral débil.

Las bandas de pronóstico construidas a partir de la dispersión de los errores históricos son heurísticas. No constituyen intervalos probabilísticos formales ni garantizan una cobertura del 95 %.

También debe considerarse que minimizar el RMSE no equivale necesariamente a minimizar el costo económico esperado de una decisión cuando los errores por exceso y por defecto tienen consecuencias diferentes.

En particular, la fecha estimada para alcanzar la capacidad máxima de la red depende de que la evolución de usuarios mantenga un comportamiento comparable al observado.

## 7. Documentos de consulta

El desarrollo técnico, las ecuaciones, las pruebas, los gráficos y los resultados detallados se encuentran en el informe principal:

- [Informe completo en HTML](informe/informe.html).
- [Informe editable en Quarto](informe/informe.qmd).
- [Nota ejecutiva para los interesados](informe/nota-interesados.pdf).

La nota ejecutiva resume en una página las recomendaciones para las tres decisiones empresariales e incluye un gráfico por serie.

## 8. Declaración de uso de inteligencia artificial

Se utilizó ChatGPT como herramienta de apoyo durante el desarrollo documental y técnico del trabajo.

Su uso incluyó asistencia para organizar código, interpretar resultados estadísticos, revisar la coherencia de las conclusiones, estructurar los argumentos dirigidos a los interesados y preparar documentación reproducible.

En particular, durante los puntos 7 al 9 se utilizó para apoyar la redacción de conclusiones técnicas, la elaboración de la nota ejecutiva, la integración de scripts y la organización final del repositorio.

Los resultados numéricos reportados proceden de la ejecución de los scripts de R sobre las series suministradas. Las interpretaciones fueron construidas a partir de los resultados calculados y las condiciones planteadas en el trabajo.

La utilización de inteligencia artificial no sustituye la verificación de los cálculos, la evaluación de los métodos ni la responsabilidad académica de los integrantes.

## 9. Control de versiones y reproducibilidad

El proyecto se desarrolló sobre un repositorio compartido de GitHub, utilizando commits para registrar los avances de los integrantes.

En la última verificación del historial se identificaron **29 commits de tres autores distintos**, superando el requisito mínimo de 15 commits de dos autores.

Los datos originales, las funciones de R, los scripts de ejecución y los documentos editables se conservan para facilitar la reproducción y revisión del trabajo.

La versión definitiva será identificada mediante la etiqueta `v1.0` una vez completada la verificación final de los entregables.
