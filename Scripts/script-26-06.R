---
  title: "Trabajo Final: 
        Análisis de Sentimientos en Textos Bíblicos"
subtitle: "Culpa y Arrepentimiento en la Biblia (Reina-Valera 1909)"
author: 
  - name: "Ortega, Alejo"
affiliation: "Licenciatura en Sociología — UNVM | IAPCS"
institute: "Universidad de Flores"
date: "Junio 2025"

format: 
  revealjs:
  slide-level: 1  
css: estilos.css             
toc: false
number-sections: true        
code-fold: false              
code-tools: true             
theme: league               
transition: slide            
highlight-style: haddok       
smooth-scroll: true          
self-contained: true   
height: 900
width: 1280
margin: 0.2    
lang: es                        
editor: visual                   
output-file: "informe_text_mining_ortega"
---
  
  ```{r}
#| label: setup
#| include: false

knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 4.5,
  fig.align = "center"
)
```

## Instalación de Paquetes

Se instalan los paquetes requeridos para el análisis de texto, manipulación de datos y visualización.

```{r}
#| label: instalar-paquetes
#| eval: false

# Este chunk instala los paquetes necesarios. 
# Se recomienda ejecutarlo solo una vez.
install.packages(c(
  "readr", "dplyr", "stringr", "tidytext", "udpipe", "ggplot2", 
  "ggwordcloud", "igraph", "ggraph", "textdata", "tm", "stopwords", 
  "widyr", "future", "future.apply", "patchwork", "tidyverse"
))
```
---
  
  ## Carga de Librerías
  
  Se cargan las librerías que se utilizarán a lo largo del documento.

```{r}
#| label: cargar-librerias

# Cargar librerías para la sesión actual
library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(udpipe)
library(ggplot2)
library(ggwordcloud)
library(igraph)
library(ggraph)
library(textdata)
library(stopwords)
library(widyr)
library(future)
library(future.apply)
library(tm)
library(purrr)
library(tidyverse)
library(patchwork)
```
---
  
  # Carga y Estructuración del Corpus
  
  El corpus textual, la Biblia Reina-Valera 1909, se carga en memoria y se estructura en un formato tabular para facilitar su procesamiento.

## Lectura y Limpieza Inicial

Se lee el archivo `.txt` y se eliminan las líneas de metadatos iniciales.

```{r}
#| label: cargar-corpus

# Definir la ruta al archivo del corpus
ruta_biblia <- "data/raw/rv_1909.txt"

# Leer el archivo línea por línea, omitiendo líneas vacías
lineas_biblia_raw <- read_lines(ruta_biblia, skip_empty_rows = TRUE)

# El archivo rv_1909.txt tiene 3 líneas de metadatos al inicio que deben ser omitidas
lineas_biblia <- lineas_biblia_raw[-(1:3)]
```
---
  
  ## Parseo de Versículos
  
  Se utiliza una expresión regular para identificar y extraer las partes de cada versículo: Libro, Capítulo, Versículo y Texto.

```{r}
#| label: parsear-versiculos

# Expresión regular para parsear Libro, Capítulo, Versículo y Texto
regex_biblia <- "^([1-3]?\\s*[A-Za-záéíóúñÑ]+(?:\\s[A-Za-záéíóúñÑ]+)*)\\s+(\\d+):(\\d+)\\s+(.*)$"

# Filtrar solo las líneas que coinciden con el formato de un versículo
lineas_validas_indices <- str_which(lineas_biblia, regex_biblia)
lineas_validas <- lineas_biblia[lineas_validas_indices]

# Extraer las partes de cada línea usando la regex
partes_biblia <- str_match(lineas_validas, regex_biblia)

# Convertir la matriz de resultados en un tibble (data frame mejorado)
biblia_df_temp <- tibble(
  Libro_Parsed = str_trim(partes_biblia[,2]),
  Capitulo_Parsed = as.integer(partes_biblia[,3]),
  Versiculo_Parsed = as.integer(partes_biblia[,4]),
  Texto_Parsed = partes_biblia[,5]
)
```
---
  
  ## Creación del DataFrame Final
  
  Se clasifica cada libro en Antiguo o Nuevo Testamento y se crea el `data.frame` final, eliminando cualquier libro no clasificado.

```{r}
#| label: crear-dataframe

# Definir listas de libros para el Antiguo y Nuevo Testamento
AT_libros <- c("Génesis", "Éxodo", "Levítico", "Números", "Deuteronomio",
               "Josué", "Jueces", "Rut", "1 Samuel", "2 Samuel", "1 Reyes", "2 Reyes",
               "1 Crónicas", "2 Crónicas", "Esdras", "Nehemías", "Ester", "Job",
               "Salmos", "Proverbios", "Eclesiastés", "Cantares",
               "Isaías", "Jeremías", "Lamentaciones", "Ezequiel", "Daniel",
               "Oseas", "Joel", "Amós", "Abdías", "Jonás", "Miqueas",
               "Nahúm", "Habacuc", "Sofonías", "Hageo", "Zacarías", "Malaquías")

NT_libros <- c("Mateo", "Marcos", "Lucas", "Juan", "Hechos", "Romanos",
               "1 Corintios", "2 Corintios", "Gálatas", "Efesios", "Filipenses",
               "Colosenses", "1 Tesalonicenses", "2 Tesalonicenses", "1 Timoteo",
               "2 Timoteo", "Tito", "Filemón", "Hebreos", "Santiago", "1 Pedro",
               "2 Pedro", "1 Juan", "2 Juan", "3 Juan", "Judas", "Apocalipsis")

# Crear el data frame final con la estructura deseada
biblia_df <- biblia_df_temp %>%
  # Ajuste para "Cantar de los Cantares"
  mutate(Libro_Parsed = if_else(Libro_Parsed == "Cantar de los Cantares", "Cantares", Libro_Parsed)) %>%
  mutate(
    Testamento = case_when(
      Libro_Parsed %in% AT_libros ~ "Antiguo Testamento",
      Libro_Parsed %in% NT_libros ~ "Nuevo Testamento",
      TRUE ~ "Desconocido"
    ),
    Libro = Libro_Parsed,
    Capitulo = Capitulo_Parsed,
    Versiculo = Versiculo_Parsed,
    Texto = Texto_Parsed,
    ID_Versiculo = paste(Libro_Parsed, Capitulo_Parsed, Versiculo_Parsed, sep = "_")
  ) %>%
  select(ID_Versiculo, Testamento, Libro, Capitulo, Versiculo, Texto) %>%
  # Filtrar para eliminar cualquier testamento "Desconocido"
  filter(Testamento != "Desconocido")

message("DataFrame de la Biblia creado y filtrado.")
```
---
  
  # Preprocesamiento Avanzado del Texto
  
  Se aplican técnicas estándar de preprocesamiento de texto para limpiar y normalizar el corpus.

## Limpieza de Texto

Se convierte el texto a minúsculas y se eliminan signos de puntuación, números y espacios extra.

```{r}
#| label: limpieza-texto

biblia_df_procesado <- biblia_df %>%
  mutate(
    Texto_Procesado = tolower(Texto),
    Texto_Procesado = str_remove_all(Texto_Procesado, "[[:punct:]]"),
    Texto_Procesado = str_remove_all(Texto_Procesado, "\\d+"),
    Texto_Procesado = str_squish(Texto_Procesado)
  )
```

## Tokenización y Stop Words

Se convierte el texto en palabras (tokens) y se eliminan las palabras comunes sin significado (stop words).

```{r}
#| label: tokenizacion-stopwords

# Tokenización
biblia_tokens <- biblia_df_procesado %>%
  select(ID_Versiculo, Testamento, Libro, Capitulo, Versiculo, Texto_Procesado) %>%
  unnest_tokens(output = "Palabra", input = "Texto_Procesado")

# Eliminación de Stop Words
lista_stopwords_es <- stopwords::stopwords("es", source = "snowball")
stopwords_df <- tibble(Palabra = lista_stopwords_es)

biblia_tokens_sin_stopwords <- biblia_tokens %>%
  anti_join(stopwords_df, by = "Palabra")
```

# Lematización Paralela

Para agrupar palabras con la misma raíz (ej. "pecado", "pecar"), se realiza una lematización. Este proceso se acelera mediante ejecución en paralelo.

## Carga del Modelo UDPipe

Se descarga y carga un modelo pre-entrenado para el español.

```{r}
#| label: cargar-modelo-udpipe

modelo_dir <- "models"
if (!dir.exists(modelo_dir)) {
  dir.create(modelo_dir)
}
nombre_modelo_udpipe <- file.path(modelo_dir, "spanish-ancora-ud-2.5-191206.udpipe")

if (!file.exists(nombre_modelo_udpipe)) {
  message("Descargando modelo de udpipe para español...")
  udpipe_download_model(language = "spanish-ancora", model_dir = modelo_dir)
}

modelo_udpipe <- udpipe_load_model(file = nombre_modelo_udpipe)
message("Modelo udpipe cargado.")
```

## Ejecución de Lematización en Paralelo

El corpus se divide en lotes y se procesa en paralelo para mayor eficiencia.

```{r}
#| label: lematizacion-paralela

# Preparar lotes
tamanio_lote <- 1000
biblia_df_lotes <- biblia_df_procesado %>%
  mutate(lote_id = ((row_number() - 1) %/% tamanio_lote) + 1) %>%
  group_by(lote_id) %>%
  nest() %>%
  pull(data)

# Configurar paralelización
num_cores <- availableCores() - 1
if (num_cores < 1) num_cores <- 1
plan(multisession, workers = num_cores)

# Función de trabajo
procesar_lote_udpipe <- function(lote_df, ruta_modelo) {
  library(udpipe)
  library(dplyr)
  modelo_local <- udpipe_load_model(file = ruta_modelo)
  anotaciones <- udpipe_annotate(
    object = modelo_local, x = lote_df$Texto_Procesado,
    doc_id = lote_df$ID_Versiculo, parser = "none"
  )
  as_tibble(anotaciones)
}

# Ejecutar y consolidar
message("Iniciando lematización paralela...")
resultados_paralelos <- future_lapply(
  X = biblia_df_lotes, FUN = procesar_lote_udpipe,
  ruta_modelo = nombre_modelo_udpipe, future.seed = TRUE
)
plan(sequential)
message("Lematización completada.")

# Consolidar resultados
biblia_lemas_df_raw <- bind_rows(resultados_paralelos)
biblia_lemas_df <- biblia_lemas_df_raw %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ", "ADV")) %>%
  select(doc_id, token, lemma, upos) %>%
  rename(ID_Versiculo = doc_id, Palabra_Original_Token = token, Lema = lemma, POS_Tag = upos) %>%
  mutate(Lema = tolower(Lema)) %>%
  anti_join(tibble(Lema = lista_stopwords_es), by = "Lema")

biblia_lemas_final_df <- biblia_lemas_df %>%
  left_join(biblia_df %>% select(ID_Versiculo, Testamento, Libro, Capitulo, Versiculo), by = "ID_Versiculo") %>%
  select(ID_Versiculo, Testamento, Libro, Capitulo, Versiculo, Palabra_Original_Token, Lema, POS_Tag)
```

# Construcción del Léxico de Sentimientos

Se crea un léxico personalizado para los sentimientos de "Culpa" y "Arrepentimiento".

```{r}
#| label: construir-lexico

lexico_culpa_terminos <- c("culpa", "culpabilidad", "culpable", "delito", "falta", "pecado", "pecar", "ofensa", "yerro", "error", "infracción", "incumplimiento", "transgresión", "iniquidad", "maldad", "agravio", "responsabilidad", "imprudencia", "omisión", "desliz", "fallo", "tropiezo")

lexico_arrepentimiento_terminos <- c("arrepentimiento", "arrepentir", "arrepentirse", "contrición", "contrito", "pesar", "pesadumbre", "remordimiento", "penitencia", "confesar", "confesión", "convertir", "convertirse", "volver", "tornar", "dolor", "aflicción", "compunción", "abatimiento", "agobio")

lematizar_lista_lexico <- function(terminos_lista, modelo) {
  if (length(terminos_lista) == 0) return(character(0))
  anotaciones <- udpipe_annotate(modelo, x = terminos_lista, doc_id = seq_along(terminos_lista), parser = "none")
  as_tibble(anotaciones) %>% filter(upos %in% c("NOUN", "VERB", "ADJ")) %>% pull(lemma) %>% tolower() %>% unique()
}

lexico_culpa_lemas <- lematizar_lista_lexico(lexico_culpa_terminos, modelo_udpipe)
lexico_arrepentimiento_lemas <- lematizar_lista_lexico(lexico_arrepentimiento_terminos, modelo_udpipe)

lexico_culpa_lemas <- setdiff(lexico_culpa_lemas, lista_stopwords_es)
lexico_arrepentimiento_lemas <- setdiff(lexico_arrepentimiento_lemas, lista_stopwords_es)

lexico_sentimientos_df <- bind_rows(
  tibble(Lema = lexico_culpa_lemas, Sentimiento = "Culpa"),
  tibble(Lema = lexico_arrepentimiento_lemas, Sentimiento = "Arrepentimiento")
) %>% distinct()

message("Léxico de sentimientos creado.")
```

# Análisis de Frecuencia y Distribución

Se analiza la frecuencia de los sentimientos en el Antiguo y Nuevo Testamento.

## Frecuencia de Sentimientos por Testamento

```{r}
#| label: frecuencia-testamento
#| echo: false
#| message: false
#| warning: false

biblia_sentimientos_detectados <- biblia_lemas_final_df %>%
  inner_join(lexico_sentimientos_df, by = "Lema")

frecuencia_por_testamento <- biblia_sentimientos_detectados %>%
  count(Testamento, Sentimiento, name = "Frecuencia_Absoluta")

total_palabras_por_testamento <- biblia_lemas_final_df %>%
  count(Testamento, name = "Total_Palabras")

frecuencia_comparativa_testamento <- frecuencia_por_testamento %>%
  left_join(total_palabras_por_testamento, by = "Testamento") %>%
  mutate(Frecuencia_Normalizada = (Frecuencia_Absoluta / Total_Palabras) * 10000) %>%
  select(Testamento, Sentimiento, Frecuencia_Absoluta, Frecuencia_Normalizada)

# Mostrar solo 6 filas como ejemplo
knitr::kable(head(frecuencia_comparativa_testamento), 
             caption = "Frecuencia Comparativa de Sentimientos por Testamento (muestra)")
```

## Frecuencia de Sentimientos por Libro

```{r}
#| label: frecuencia-libro
#| echo: false
#| message: false
#| warning: false

frecuencia_sentimiento_por_libro <- biblia_sentimientos_detectados %>%
  group_by(Testamento, Libro, Sentimiento) %>%
  summarise(Frecuencia = n(), .groups = 'drop') %>%
  arrange(Testamento, desc(Frecuencia))

# Mostrar solo las 10 combinaciones más frecuentes
top_10_libros <- frecuencia_sentimiento_por_libro %>%
  slice_max(Frecuencia, n = 10)

knitr::kable(top_10_libros, 
             caption = "Top 10 combinaciones Libro–Sentimiento por frecuencia")
```

# Visualización de la Distribución

Se visualiza la distribución de los términos de culpa y arrepentimiento en los libros de la Biblia.

```{r}
#| label: visualizacion-distribucion
#| fig-cap: "Top 10 libros con mayor frecuencia de términos de 'Culpa' y 'Arrepentimiento'."

# Preparar datos para el gráfico: Top 10 libros por sentimiento
top_libros_sentimiento_facetado <- frecuencia_sentimiento_por_libro %>%
  group_by(Testamento, Sentimiento) %>%
  slice_max(order_by = Frecuencia, n = 10) %>% # CAMBIO: de 15 a 10
  ungroup() %>%
  mutate(Libro = reorder_within(Libro, Frecuencia, Testamento))

# Crear el gráfico
grafico_sentimientos_libro_facetado <- ggplot(top_libros_sentimiento_facetado, 
                                              aes(x = Libro, y = Frecuencia, fill = Sentimiento)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Testamento, scales = "free") +
  scale_x_reordered() +
  labs(
    title = "Top 10 Libros con Mayor Frecuencia de Culpa y Arrepentimiento",
    subtitle = "Comparación entre Antiguo y Nuevo Testamento",
    x = "Libro Bíblico", y = "Frecuencia Absoluta de Términos"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 16)
  ) +
  scale_fill_manual(values = c("Culpa" = "#d95f02", "Arrepentimiento" = "#1b9e77")) +
  facet_grid(Testamento ~ Sentimiento, scales = "free", space = "free_y")

print(grafico_sentimientos_libro_facetado)
```

# Análisis de Contexto (KWIC)

Se extraen versículos de ejemplo para comprender el uso de los términos.

```{r}
#| label: analisis-contexto

obtener_contextos <- function(sentimiento_buscado, testamento_buscado, n_ejemplos = 3) {
  ids_versiculos <- biblia_sentimientos_detectados %>%
    filter(Sentimiento == sentimiento_buscado, Testamento == testamento_buscado) %>%
    pull(ID_Versiculo) %>% unique()
  
  biblia_df %>%
    filter(ID_Versiculo %in% ids_versiculos) %>%
    sample_n(min(n_ejemplos, length(ids_versiculos))) %>%
    select(Libro, Capitulo, Versiculo, Texto)
}

knitr::kable(obtener_contextos("Culpa", "Antiguo Testamento"), caption = "Contextos de 'Culpa' (AT)")
knitr::kable(obtener_contextos("Culpa", "Nuevo Testamento"), caption = "Contextos de 'Culpa' (NT)")
knitr::kable(obtener_contextos("Arrepentimiento", "Antiguo Testamento"), caption = "Contextos de 'Arrepentimiento' (AT)")
knitr::kable(obtener_contextos("Arrepentimiento", "Nuevo Testamento"), caption = "Contextos de 'Arrepentimiento' (NT)")
```

# Redes de Coocurrencia

Se visualizan las palabras que co-ocurren con los términos de sentimiento.

```{r}
#| label: analisis-redes
#| fig-cap: "Redes de coocurrencia para 'Culpa' y 'Arrepentimiento'."

# Calcular coocurrencias
coocurrencias_por_testamento <- biblia_lemas_final_df %>%
  group_by(Testamento) %>%
  pairwise_count(item = Lema, feature = ID_Versiculo, sort = TRUE) %>%
  ungroup()

# Función para preparar los datos para el grafo
preparar_datos_grafo <- function(coocurrencias_df, lexico, sentimiento, testamento, top_n = 30) {
  lemas_sentimiento <- lexico %>% filter(Sentimiento == sentimiento) %>% pull(Lema)
  
  coocurrencias_filtradas <- coocurrencias_df %>%
    filter(Testamento == testamento) %>%
    filter(item1 %in% lemas_sentimiento, !item2 %in% lemas_sentimiento) %>%
    slice_max(order_by = n, n = top_n)
  
  igraph::graph_from_data_frame(coocurrencias_filtradas, directed = FALSE)
}

# Definir la función de visualización
visualizar_grafo <- function(grafo, titulo) { 
  ggraph(grafo, layout = "fr") + 
    geom_edge_fan(aes(alpha = n), color = "gray70") + 
    geom_node_point(size = 4, color = "skyblue") + 
    geom_node_text(aes(label = name), repel = TRUE, size = 3, color = "gray10") + 
    theme_void(base_size = 12) + 
    labs(title = titulo, edge_alpha = "Frecuencia") + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
}

# Crear y visualizar grafos
vis_culpa_at <- visualizar_grafo(preparar_datos_grafo(coocurrencias_por_testamento, lexico_sentimientos_df, "Culpa", "Antiguo Testamento"), "Culpa (AT)")
vis_culpa_nt <- visualizar_grafo(preparar_datos_grafo(coocurrencias_por_testamento, lexico_sentimientos_df, "Culpa", "Nuevo Testamento"), "Culpa (NT)")
vis_arrepentimiento_at <- visualizar_grafo(preparar_datos_grafo(coocurrencias_por_testamento, lexico_sentimientos_df, "Arrepentimiento", "Antiguo Testamento"), "Arrepentimiento (AT)")
vis_arrepentimiento_nt <- visualizar_grafo(preparar_datos_grafo(coocurrencias_por_testamento, lexico_sentimientos_df, "Arrepentimiento", "Nuevo Testamento"), "Arrepentimiento (NT)")

# Combinar y mostrar
final_network_plot <- (vis_culpa_at | vis_culpa_nt) / (vis_arrepentimiento_at | vis_arrepentimiento_nt) +
  plot_annotation(tag_levels = 'A')

print(final_network_plot)
```

# Asociaciones Demográficas

Se investiga la asociación entre sentimientos y figuras o grupos demográficos.

```{r}
#| label: analisis-demografico

terminos_demograficos <- c("adán", "eva", "caín", "abraham", "moisés", "aaron", "david", "salomón", "job", "isaías", "jeremías", "israel", "hijo de israel", "judío", "levita", "sacerdote", "jesús", "cristo", "juan bautista", "pedro", "pablo", "judas", "discípulo", "apóstol", "fariseo", "saduceo", "gentil", "iglesia")
lexico_demografico_lemas <- lematizar_lista_lexico(terminos_demograficos, modelo_udpipe)
lexico_demografico_df <- tibble(Lema = lexico_demografico_lemas, Tipo = "Demográfico")

asociaciones_demograficas <- biblia_lemas_final_df %>%
  inner_join(lexico_demografico_df, by = "Lema") %>%
  rename(Lema_Demografico = Lema) %>%
  inner_join(
    biblia_sentimientos_detectados %>% select(ID_Versiculo, Sentimiento, Lema_Sentimiento = Lema),
    by = "ID_Versiculo", relationship = "many-to-many"
  ) %>%
  distinct(ID_Versiculo, Testamento, Lema_Demografico, Sentimiento)

tabla_asociaciones_pivote <- asociaciones_demograficas %>%
  count(Testamento, Lema_Demografico, Sentimiento, sort = TRUE) %>%
  group_by(Testamento, Sentimiento) %>%
  slice_max(order_by = n, n = 10) %>% # CAMBIO: de 15 a 10
  ungroup() %>%
  tidyr::pivot_wider(names_from = Sentimiento, values_from = n, values_fill = 0) %>%
  arrange(Testamento, desc(Culpa + Arrepentimiento))

knitr::kable(tabla_asociaciones_pivote, caption = "Top 10 Asociaciones Demográficas por Sentimiento y Testamento")
```

# Guardado de Resultados

Finalmente, se guardan los `data.frames` y gráficos generados en el análisis.

```{r}
#| label: guardar-resultados

if (!dir.exists("output/data_processed")) dir.create("output/data_processed", recursive = TRUE)
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

message("Guardando resultados del análisis...")

readr::write_csv(biblia_lemas_final_df, "output/data_processed/biblia_lemas_final.csv")
readr::write_csv(frecuencia_comparativa_testamento, "output/data_processed/frecuencia_comparativa_testamento.csv")
readr::write_csv(frecuencia_sentimiento_por_libro, "output/data_processed/frecuencia_sentimiento_por_libro.csv")
readr::write_csv(tabla_asociaciones_pivote, "output/data_processed/asociaciones_demograficas_pivot.csv")
readr::write_csv(lexico_sentimientos_df, "output/data_processed/lexico_sentimientos_custom.csv")

ggsave("output/figures/distribucion_sentimientos_por_libro.png", plot = grafico_sentimientos_libro_facetado, width = 12, height = 10, dpi = 300)
ggsave("output/figures/redes_coocurrencia_comparativas.png", plot = final_network_plot, width = 14, height = 12, dpi = 300)

message("Resultados guardados en la carpeta 'output/'.")
```
