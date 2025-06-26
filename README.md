# Análisis de Sentimientos de Culpa y Arrepentimiento en la Biblia (Reina-Valera 1909)

**Autor:** Alejo Ortega  
**Curso:** Text Mining  
**Fecha:** Junio 2025  

---

## 🧾 Resumen del Proyecto

Este proyecto realiza un análisis de sentimientos computacional sobre el texto completo de la Biblia, versión Reina-Valera 1909. El objetivo principal es explorar y cuantificar la distribución y el contexto de dos conceptos teológicos fundamentales: la **culpa** y el **arrepentimiento**.

Utilizando técnicas de procesamiento de lenguaje natural (PLN) en R, el análisis compara la prevalencia y las asociaciones semánticas de estos sentimientos entre el Antiguo y el Nuevo Testamento.

La metodología se basa en la construcción de léxicos semánticos personalizados para *"Culpa"* y *"Arrepentimiento"*. El texto bíblico es procesado, tokenizado y lematizado para normalizar las palabras a su raíz léxica. Posteriormente, se realizan análisis de frecuencia, se exploran los contextos de aparición (KWIC), se visualizan redes de coocurrencia de palabras y se investigan las asociaciones con figuras y grupos demográficos clave de la narrativa bíblica.

---

## 🔍 Hallazgos Principales

### 📖 Prevalencia por Testamento
Se observa una **mayor frecuencia de términos asociados a la culpa en el Antiguo Testamento**, a menudo en contextos de ley, sacrificio y transgresión (por ejemplo, en *Levítico*, *Números* y *Salmos*). Por el contrario, el **arrepentimiento**, entendido como *metanoia* (un cambio de mente y dirección), gana una prominencia significativa en el Nuevo Testamento, especialmente en los Evangelios y las epístolas paulinas.

### 🔗 Asociaciones Semánticas
Las redes de coocurrencia muestran que en el **Antiguo Testamento**, la culpa está fuertemente ligada a conceptos como *"pecado"*, *"iniquidad"*, *"sangre"* y *"sacrificio"*. En el **Nuevo Testamento**, el arrepentimiento se asocia con *"fe"*, *"reino"*, *"perdón"* y la figura de *"Jesús"*.

### 👥 Contexto Demográfico
Figuras como **David** y el **pueblo de Israel** son centrales en las narrativas de culpa y arrepentimiento del Antiguo Testamento. En el Nuevo Testamento, los conceptos se vinculan frecuentemente con las enseñanzas de **Jesús**, la predicación de los apóstoles como **Pedro** y **Pablo**, y la respuesta de grupos como los **fariseos** y los **gentiles**.

---

## 🛠️ Cómo Reproducir este Análisis

### 🔧 Prerrequisitos

- Tener instalado [R](https://www.r-project.org/) (versión 4.0 o superior).
- Tener instalado [RStudio](https://posit.co/download/rstudio-desktop/).
- Una conexión a internet para descargar los paquetes de R y el modelo de lenguaje.

### 📥 Clonar el Repositorio

```bash
git clone https://github.com/405755-Ortega-Alejo/Trabajo-Final---Text-Mining---Ortega.git
cd Trabajo-Final---Text-Mining---Ortega
```

### 📦 Instalar Paquetes Requeridos

Abre el proyecto en RStudio (haciendo doble clic en `Trabajo-Final---Text-Mining---Ortega.Rproj`) y ejecuta el siguiente comando en la consola de R para instalar todas las dependencias necesarias:

```r
install.packages(c(
  "readr", "dplyr", "stringr", "tidytext", "udpipe", "ggplot2",
  "ggwordcloud", "igraph", "ggraph", "textdata", "tm", "stopwords",
  "widyr", "future", "future.apply", "patchwork", "purrr", "tidyverse"
))
```

### ▶️ Ejecutar el Script de Análisis
```r
source("scripts/01_analisis_sentimientos_biblia.R")
```
>> 📌 Nota: El proceso de lematización es computacionalmente intensivo. El script utiliza procesamiento paralelo para acelerar el análisis, pero aun así puede tardar varios minutos en completarse. Se mostrarán mensajes en consola con el progreso.

---

## 📁 Estructura del Repositorio
```

├── .github/
│   └── workflows/
│       └── deploy-gh-pages.yml
├── data/
│   └── raw/
│       └── rv_1909.txt
├── output/
│   ├── data_processed/
│   └── figures/
├── scripts/
│   └── 01_analisis_sentimientos_biblia.R
├── .gitignore
├── index.qmd
├── README.md
└── Trabajo-Final---Text-Mining---Ortega.Rproj
```

---

## 📄 Descripción de los Datos

### 📥 Datos Brutos

- `data/raw/rv_1909.txt`:  
  El texto completo de la Santa Biblia, versión **Reina-Valera 1909**.  
  Este es el corpus de entrada para todo el análisis.
---
### 📊 Datos Procesados y Resultados

Los siguientes archivos son generados por el script `scripts/01_analisis_sentimientos_biblia.R` y se almacenan en la carpeta `output/data_processed/`.

| Archivo                              | Descripción                                                                                      |
|--------------------------------------|--------------------------------------------------------------------------------------------------|
| `biblia_lemas_final.csv`             | El corpus completo, procesado, tokenizado y lematizado, con una palabra (lema) por fila.         |
| `frecuencia_comparativa_testamento.csv` | Tabla con la frecuencia absoluta y normalizada (por cada 10.000 palabras) de los sentimientos de culpa y arrepentimiento por Testamento. |
| `frecuencia_sentimiento_por_libro.csv` | Frecuencia absoluta de los términos de sentimiento para cada libro de la Biblia.                |
| `asociaciones_demograficas_pivot.csv` | Tabla pivote que muestra las 10 principales figuras o grupos demográficos asociados con la culpa y el arrepentimiento en cada Testamento. |
| `lexico_sentimientos_custom.csv`     | Léxico personalizado creado para identificar términos relacionados con culpa y arrepentimiento. |
