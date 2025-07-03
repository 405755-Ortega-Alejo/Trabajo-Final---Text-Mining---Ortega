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
## 📥 Datos Brutos

- `data/raw/rv_1909.txt`:  
  El texto completo de la Santa Biblia, versión **Reina-Valera 1909**.  
  Este es el corpus de entrada para todo el análisis.
---
