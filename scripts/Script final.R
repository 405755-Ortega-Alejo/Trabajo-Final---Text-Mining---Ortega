# ========================================================================
#
# Título: Análisis de Sentimientos en Textos Bíblicos
# Subtítulo: Culpa y Arrepentimiento en la Biblia (Reina-Valera 1909)
# Autor: Ortega, Alejo
#
# ========================================================================


# ------------------------------------------------------------------------
# Presentación y configuración inicial
# ------------------------------------------------------------------------
# Este trabajo presenta un análisis computacional de sentimientos aplicado a un
# corpus religioso de alto valor simbólico y cultural: la Biblia, en su
# versión Reina-Valera, 1909. El objetivo central es explorar dos emociones
# teológicas fundamentales —la culpa y el arrepentimiento— a partir de sus
# apariciones en el Antiguo y Nuevo Testamento.

# ------------------------------------------------------------------------
# Instalación y Carga de Paquetes
# ------------------------------------------------------------------------

# ## Instalación de paquetes
# ⚠️ Este bloque debe ejecutarse solo una vez en el entorno local.
# install.packages(c(
#  "readr", "dplyr", "stringr", "tidytext", "udpipe", "ggplot2",
#  "ggwordcloud", "igraph", "ggraph", "textdata", "tm", "stopwords",
#  "widyr", "future", "future.apply", "patchwork", "tidyverse",
#  "spacyr", "syuzhet", "here", "DT"
#  ))
# message("Paquetes instalados.")


# ## Carga de librerías
# Se cargan las librerías necesarias para la sesión actual.
library(tidyverse)
library(tidytext)
library(treemapify)
library(udpipe)
library(ggwordcloud)
library(igraph)
library(ggraph)
library(textdata)
library(stopwords)
library(widyr)
library(future)
library(future.apply)
library(tm)
library(spacyr)
library(syuzhet)
library(here)
library(patchwork)

message("Librerías cargadas.")


# ------------------------------------------------------------------------
# Carga y Estructuración del Corpus
# ------------------------------------------------------------------------

# ## Lectura y limpieza inicial
# Se lee el archivo .txt y se eliminan las líneas de metadatos iniciales.

# 💡 NOTA: El script asume que existe el archivo "rv_1909.txt"
# dentro de una carpeta "data/raw/" en el directorio del proyecto.
ruta_biblia <- here::here("data", "raw", "rv_1909.txt")

if (!file.exists(ruta_biblia)) {
  stop(paste("No se encontró el archivo en la ruta especificada:", ruta_biblia))
}
lineas_biblia_raw <- readr::read_lines(ruta_biblia, skip_empty_rows = TRUE)

# El archivo rv_1909.txt tiene 2 líneas de metadatos al inicio que se omiten.
lineas_biblia <- lineas_biblia_raw[-(1:2)]
message("Limpieza inicial terminada.")


# ## Parseo de Versículos
# Se utiliza una expresión regular para identificar y extraer las partes de cada versículo.
regex_biblia <- "^([1-3]?\\s*[A-Za-záéíóúñÑ]+(?:\\s[A-Za-záéíóúñÑ]+)*)\\s+(\\d+):(\\d+)\\s+(.*)$"

# Filtrar solo las líneas que coinciden con el formato de un versículo.
lineas_validas_indices <- str_which(lineas_biblia, regex_biblia)
lineas_validas <- lineas_biblia[lineas_validas_indices]

# Extraer las partes de cada línea usando la regex.
partes_biblia <- str_match(lineas_validas, regex_biblia)

biblia_df_temp <- tibble(
  Libro_Parsed = str_trim(partes_biblia[,2]),
  Capitulo_Parsed = as.integer(partes_biblia[,3]),
  Versiculo_Parsed = as.integer(partes_biblia[,4]),
  Texto_Parsed = partes_biblia[,5]
)
message("Parseo de la Biblia creado.")


# ## Creación del Data Frame
# Se clasifica cada versículo según su testamento.
AT_libros <- c("Génesis", "Éxodo", "Levítico", "Números", "Deuteronomio", "Josué", "Jueces", "Rut", "1 Samuel", "2 Samuel", "1 Reyes", "2 Reyes", "1 Crónicas", "2 Crónicas", "Esdras", "Nehemías", "Tobías", "Judit", "Esther", "1 Macabeos", "2 Macabeos", "Job", "Salmos", "Proverbios", "Canción de canciones", "Sabiduría", "Ecclesiastés", "Isaías", "Jeremías", "Lamentaciones", "Baruc", "Ezequiel", "Daniel", "Oseas", "Joel", "Amós", "Abdías", "Jonás", "Miqueas", "Nahum", "Habacuc", "Sofonías", "Haggeo", "Zacarías", "Malaquías")
NT_libros <- c("Mateo", "Marcos", "Lucas", "Juan", "Hechos", "Romanos", "1 Corintios", "2 Corintios", "Gálatas", "Efesios","Filipenses", "Colosenses", "1 Tesalonicenses", "2 Tesalonicenses", "1 Timoteo", "2 Timoteo", "Tito", "Filemón", "Hebreos", "Santiago", "1 Pedro", "2 Pedro", "1 Juan", "2 Juan", "3 Juan", "Judas", "Apocalipsis", "Revelación")

biblia_df <- biblia_df_temp %>%
  mutate(
    Testamento = case_when(
      Libro_Parsed %in% AT_libros ~ "Antiguo Testamento",
      Libro_Parsed %in% NT_libros ~ "Nuevo Testamento"
    ),
    Libro = Libro_Parsed,
    Capitulo = Capitulo_Parsed,
    Versiculo = Versiculo_Parsed,
    Texto = Texto_Parsed,
    ID_Versiculo = paste(Libro_Parsed, Capitulo_Parsed, Versiculo_Parsed, sep = "_")
  ) %>%
  select(ID_Versiculo, Testamento, Libro, Capitulo, Versiculo, Texto) %>%
  filter(!is.na(Testamento))
message("DF de la Biblia creado y filtrado.")

# Visualización de las primeras filas del DataFrame.
print("--- Visualización del DataFrame de la Biblia (primeras 10 filas) ---")
print(head(biblia_df, 10))


# ------------------------------------------------------------------------
# Preprocesamiento Avanzado del Texto
# ------------------------------------------------------------------------

# ## Limpieza de texto
# Se aplican transformaciones estándar de PLN para normalizar el corpus.
biblia_df_procesado <- biblia_df %>%
  mutate(
    Texto_Procesado = tolower(Texto),
    Texto_Procesado = stringi::stri_trans_general(Texto_Procesado, "Latin-ASCII"),
    Texto_Procesado = str_remove_all(Texto_Procesado, "[[:punct:]]"),
    Texto_Procesado = str_remove_all(Texto_Procesado, "\\d+"),
    Texto_Procesado = str_squish(Texto_Procesado)
  )
message("Corpus preprocesado y listo.")


# ## Tokenización y Stop Words
# El texto se descompone en tokens y se eliminan las stop words.
biblia_tokens <- biblia_df_procesado %>%
  select(ID_Versiculo, Testamento, Libro, Capitulo, Versiculo, Texto_Procesado) %>%
  unnest_tokens(output = "Palabra", input = "Texto_Procesado")

lista_stopwords_es <- stopwords::stopwords("es", source = "snowball")
stopwords_df <- tibble(Palabra = lista_stopwords_es)

biblia_tokens_sin_stopwords <- biblia_tokens %>%
  anti_join(stopwords_df, by = "Palabra")
message("Corpus tokenizado y sin stop words.")

# Visualización de los tokens.
print("--- Visualización de Tokens sin Stop Words (primeras 10 filas) ---")
print(head(biblia_tokens_sin_stopwords, 10))


# ------------------------------------------------------------------------
# Análisis de sentimientos con léxico NRC
# ------------------------------------------------------------------------
# Se cuantifican y comparan las emociones entre los testamentos.
nrc_sentimientos <- get_nrc_sentiment(
  biblia_df_procesado$Texto_Procesado,
  language = "spanish"
)
biblia_nrc <- bind_cols(biblia_df_procesado, nrc_sentimientos)

biblia_nrc_summary <- biblia_nrc %>%
  group_by(Testamento) %>%
  summarise(across(anger:positive, sum, .names = "total_{.col}")) %>%
  pivot_longer(
    cols = -Testamento,
    names_to = "Emocion",
    values_to = "Conteo",
    names_prefix = "total_"
  ) %>%
  left_join(
    biblia_df %>% count(Testamento, name = "Total_Versiculos"),
    by = "Testamento"
  ) %>%
  mutate(
    Frecuencia_Normalizada = (Conteo / Total_Versiculos) * 1000,
    Emocion = str_to_title(Emocion)
  )
message("Análisis NRC finalizado.")

# ## Visualización de resultados NRC
biblia_nrc_summary_es <- biblia_nrc_summary %>%
  mutate(
    Emocion = recode(Emocion,
                     "Anger" = "Ira", "Anticipation" = "Anticipación", "Disgust" = "Repulsión",
                     "Fear" = "Miedo", "Joy" = "Alegría", "Sadness" = "Tristeza",
                     "Surprise" = "Sorpresa", "Trust" = "Confianza", "Positive" = "Positividad",
                     "Negative" = "Negatividad"
    )
  )

plot_nrc <- ggplot(
  biblia_nrc_summary_es %>% filter(!Emocion %in% c("Positividad", "Negatividad")),
  aes(x = reorder(Emocion, Frecuencia_Normalizada), y = Frecuencia_Normalizada, fill = Testamento)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Perfil Emocional Comparativo de los Testamentos",
    subtitle = "Frecuencia de emociones por 1,000 versículos",
    x = "Emoción", y = "Frecuencia Normalizada", fill = "Testamento"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2")

print("--- Mostrando Gráfico de Análisis NRC ---")
print(plot_nrc)


# ------------------------------------------------------------------------
# Lematización Paralela
# ------------------------------------------------------------------------

# ## Carga del Modelo UDPipe
# 💡 NOTA: Se creará una carpeta 'models' para descargar el modelo de lenguaje.
modelo_dir <- "models"
if (!dir.exists(modelo_dir)) {
  dir.create(modelo_dir)
}
nombre_modelo_udpipe <- file.path(modelo_dir, "spanish-ancora-ud-2.5-191206.udpipe")

if (!file.exists(nombre_modelo_udpipe)) {
  message("Intentando descargar el modelo UDPipe para español...")
  udpipe::udpipe_download_model(language = "spanish-ancora-ud-2.5-191206", model_dir = modelo_dir)
}
modelo_udpipe <- udpipe::udpipe_load_model(file = nombre_modelo_udpipe)
message("Modelo UDPipe cargado correctamente.")


# ## Ejecución de Lematización en Paralelo
tamanio_lote <- 500
biblia_df_lotes <- biblia_df_procesado %>%
  mutate(lote_id = ((row_number() - 1) %/% tamanio_lote) + 1) %>%
  group_by(lote_id) %>%
  nest() %>%
  pull(data)

num_cores <- availableCores() - 1
if (num_cores < 1) num_cores <- 1
plan(multisession, workers = num_cores)

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

message("Iniciando lematización paralela...")
resultados_paralelos <- future_lapply(
  X = biblia_df_lotes, FUN = procesar_lote_udpipe,
  ruta_modelo = nombre_modelo_udpipe, future.seed = TRUE
)
plan(sequential)
message("Lematización completada.")

# Consolidar resultados.
biblia_lemas_df_raw <- bind_rows(resultados_paralelos)
biblia_lemas_df <- biblia_lemas_df_raw %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ")) %>%
  select(doc_id, token, lemma, upos) %>%
  rename(ID_Versiculo = doc_id, Palabra_Original_Token = token, Lema = lemma, POS_Tag = upos) %>%
  mutate(Lema = tolower(Lema)) %>%
  anti_join(tibble(Lema = lista_stopwords_es), by = "Lema")

biblia_lemas_final_df <- biblia_lemas_df %>%
  left_join(biblia_df %>% select(ID_Versiculo, Testamento, Libro, Capitulo, Versiculo), by = "ID_Versiculo") %>%
  select(ID_Versiculo, Testamento, Libro, Capitulo, Versiculo, Palabra_Original_Token, Lema, POS_Tag)
message("Resultados de lematización consolidados.")

# Visualización de la lematización.
print("--- Visualización de Lematización (primeras 20 filas) ---")
print(head(biblia_lemas_final_df, 20))


# ------------------------------------------------------------------------
# Construcción del Léxico de Sentimientos
# ------------------------------------------------------------------------
lexico_sentimientos_biblia <- c(
  "culpa", "culpabilidad", "culpable", "culposo", "responsabilidad", "iniquidad", "irresponsabilidad", "negligencia", "responsabilizar", "condena", "condenación",
  "arrepentimiento", "arrepentir", "arrepentirse", "contrición", "contrito", "penitencia", "penitente", "confesar", "confesión", "enmienda", "enmendar", "expiación", "resarcir", "desagravio", "convertir", "convertirse", "redención", "restitución", "conversión",
  "remordimiento", "pesar", "pesadumbre", "dolor", "aflicción", "compunción", "humillación", "abatimiento", "quebranto", "autorreproche", "angustia", "tribulación", "conciencia", "conmiseración"
)
message("Léxico de sentimientos creado.")

# ## Lematización del léxico
lematizar_lista_lexico <- function(terminos_lista, modelo) {
  if (length(terminos_lista) == 0) return(character(0))
  anotaciones <- udpipe_annotate(modelo, x = terminos_lista, doc_id = seq_along(terminos_lista), parser = "none")
  as_tibble(anotaciones) %>%
    filter(upos %in% c("NOUN", "VERB", "ADJ")) %>%
    pull(lemma) %>%
    tolower() %>%
    unique()
}

lexico_sentimientos_lemas <- lematizar_lista_lexico(lexico_sentimientos_biblia, modelo_udpipe)
lexico_sentimientos_lemas <- setdiff(lexico_sentimientos_lemas, lista_stopwords_es)

lexico_sentimientos_df <- tibble(
  Lema = lexico_sentimientos_lemas,
  Sentimiento = "Culpa/Arrepentimiento"
)
message("Léxico de sentimientos lematizado y listo.")

# Visualización del léxico.
print("--- Léxico Final de Sentimientos Lematizados ---")
print(lexico_sentimientos_df)


# ------------------------------------------------------------------------
# Detección de Sentimientos en el Corpus
# ------------------------------------------------------------------------
biblia_sentimientos_detectados <- biblia_lemas_final_df %>%
  inner_join(lexico_sentimientos_df, by = "Lema") %>%
  distinct(ID_Versiculo, Lema, Sentimiento, .keep_all = TRUE)
message("Sentimientos detectados y Data Frame creado.")

# Visualización de detecciones.
print("--- Visualización de Sentimientos Detectados (primeras 10 filas) ---")
print(head(biblia_sentimientos_detectados, 10))


# ## Visualización con nube de palabras
frecuencia_at <- biblia_sentimientos_detectados %>%
  filter(Testamento == "Antiguo Testamento") %>%
  count(Lema, sort = TRUE) %>%
  slice_max(order_by = n, n = 60)

frecuencia_nt <- biblia_sentimientos_detectados %>%
  filter(Testamento == "Nuevo Testamento") %>%
  count(Lema, sort = TRUE) %>%
  slice_max(order_by = n, n = 60)

nube_at <- ggplot(frecuencia_at, aes(label = Lema, size = n)) +
  geom_text_wordcloud(color = "#1f78b4") +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  labs(title = "Antiguo Testamento") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

nube_nt <- ggplot(frecuencia_nt, aes(label = Lema, size = n)) +
  geom_text_wordcloud(color = "#33a02c") +
  scale_size_area(max_size = 12) +
  theme_minimal() +
  labs(title = "Nuevo Testamento") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

print("--- Mostrando Nubes de Palabras ---")
print(nube_at + nube_nt)


# ------------------------------------------------------------------------
# Análisis de Frecuencia y Distribución
# ------------------------------------------------------------------------

# ## Frecuencia por Testamento
frecuencia_por_testamento <- biblia_sentimientos_detectados %>%
  count(Testamento, Sentimiento, name = "Frecuencia_Absoluta")

total_palabras_por_testamento <- biblia_lemas_final_df %>%
  count(Testamento, name = "Total_Palabras")

frecuencia_comparativa_testamento <- frecuencia_por_testamento %>%
  left_join(total_palabras_por_testamento, by = "Testamento") %>%
  mutate(Frecuencia_Normalizada = (Frecuencia_Absoluta / Total_Palabras) * 10000) %>%
  select(Testamento, Sentimiento, Frecuencia_Absoluta, Frecuencia_Normalizada)
message("Frecuencia por testamento calculada.")
print("--- Frecuencia de Sentimientos por Testamento ---")
print(frecuencia_comparativa_testamento)


# ## Frecuencia por Libro
frecuencia_sentimiento_por_libro <- biblia_sentimientos_detectados %>%
  group_by(Testamento, Libro, Sentimiento) %>%
  summarise(Frecuencia = n(), .groups = 'drop') %>%
  arrange(Testamento, desc(Frecuencia))
message("Frecuencia por libro calculada.")
print("--- Top 10 Libros con mayor frecuencia de términos ---")
print(head(frecuencia_sentimiento_por_libro, 10))


# ## Visualización de la Distribución por Libro
frecuencia_sentimientos_por_libro_viz <- biblia_sentimientos_detectados %>%
  group_by(Testamento, Libro) %>%
  summarise(Frecuencia = n(), .groups = "drop")

top_libros_testamento <- frecuencia_sentimientos_por_libro_viz %>%
  group_by(Testamento) %>%
  slice_max(order_by = Frecuencia, n = 10) %>%
  ungroup() %>%
  mutate(Libro = reorder_within(Libro, Frecuencia, Testamento))

grafico_top_libros <- ggplot(top_libros_testamento, aes(x = Libro, y = Frecuencia, fill = Testamento)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Testamento, scales = "free_y") +
  scale_x_reordered() +
  labs(
    title = "Top 10 Libros con Mayor Frecuencia de Términos Emocionales",
    subtitle = "Distribución por Testamento (Culpa y Arrepentimiento combinados)",
    x = "Libro Bíblico", y = "Frecuencia Absoluta"
  ) +
  theme_minimal(base_size = 11)

print("--- Mostrando Gráfico de Top 10 Libros ---")
print(grafico_top_libros)


# ------------------------------------------------------------------------
# Análisis de Contexto (KWIC)
# ------------------------------------------------------------------------
obtener_contextos <- function(sentimiento_buscado, testamento_buscado, n_ejemplos = 5) {
  ids_versiculos <- biblia_sentimientos_detectados %>%
    filter(Sentimiento == sentimiento_buscado, Testamento == testamento_buscado) %>%
    pull(ID_Versiculo) %>% unique()
  
  if (length(ids_versiculos) == 0) return(tibble())
  
  biblia_df %>%
    filter(ID_Versiculo %in% ids_versiculos) %>%
    sample_n(min(n_ejemplos, length(ids_versiculos))) %>%
    select(Libro, Capitulo, Versiculo, Texto)
}
message("Función KWIC creada.")

print("--- Contextos de Culpa/Arrepentimiento (Antiguo Testamento) ---")
print(obtener_contextos("Culpa/Arrepentimiento", "Antiguo Testamento"))

print("--- Contextos de Culpa/Arrepentimiento (Nuevo Testamento) ---")
print(obtener_contextos("Culpa/Arrepentimiento", "Nuevo Testamento"))


# ------------------------------------------------------------------------
# Asociaciones Demográficas
# ------------------------------------------------------------------------
terminos_demograficos <- c(
  "adán", "eva", "caín", "abraham", "isaac", "jacob", "moisés", "aaron",
  "david", "salomón", "job", "isaías", "jeremías", "daniel", "josué", "elías", "eliseo",
  "jesús", "cristo", "maría", "jose", "juan", "pedro", "pablo", "judas", "tomás", "discípulo", "apóstol", "israel", "israelita", "judío", "samaritano", "gentil", "cananeo", "egipcio", "romano", "fariseo", "saduceo", "sacerdote", "levita", "profeta", "rey", "escriba", "doctor", "tribu", "pueblo", "nación", "iglesia", "multitud", "mujer", "varón", "niño", "niña", "jóven", "anciano"
)
terminos_simples <- terminos_demograficos[!stringr::str_detect(terminos_demograficos, "\\s")]
lemas_simples_raw <- lematizar_lista_lexico(terminos_simples, modelo_udpipe)

correcciones_lemas <- c(
  "ad" = "adán", "evar" = "eva", "caír" = "caín", "davir" = "david",
  "isaía" = "isaías", "jeremía" = "jeremías", "josar" = "josué",
  "elía" = "elías", "elisear" = "eliseo", "jesú" = "jesús",
  "crer" = "cristo", "marir" = "maría", "juar" = "juan", "juda" = "judas",
  "discípular" = "discípulo", "israelitar" = "israelita", "judiar" = "judío",
  "romanar" = "romano", "saducear" = "saduceo", "sacerdotar" = "sacerdote",
  "levitar" = "levita", "profetar" = "profeta", "escribir" = "escriba",
  "iglesiar" = "iglesia", "jóvar" = "jóven", "aar" = "aaron"
)

lemas_simples_corregidos <- ifelse(lemas_simples_raw %in% names(correcciones_lemas), correcciones_lemas[lemas_simples_raw], lemas_simples_raw)
lexico_demografico_lemas <- unique(c(lemas_simples_corregidos, terminos_demograficos[stringr::str_detect(terminos_demograficos, "\\s")]))
lexico_demografico_lemas <- setdiff(lexico_demografico_lemas, lista_stopwords_es)
lexico_demografico_df <- tibble(Lema = lexico_demografico_lemas, Tipo = "Demográfico")
message("Léxico demográfico creado con éxito.")
print("--- Léxico Demográfico ---")
print(lexico_demografico_df)


# ## Coocurrencia entre Sentimientos y Términos Demográficos
versiculos_con_demograficos <- biblia_lemas_final_df %>% semi_join(lexico_demografico_df, by = "Lema") %>% distinct(ID_Versiculo)
versiculos_con_sentimientos <- biblia_sentimientos_detectados %>% distinct(ID_Versiculo)
versiculos_comunes <- intersect(versiculos_con_demograficos$ID_Versiculo, versiculos_con_sentimientos$ID_Versiculo)

asociaciones_demograficas <- biblia_lemas_final_df %>%
  filter(ID_Versiculo %in% versiculos_comunes) %>%
  inner_join(lexico_demografico_df, by = "Lema") %>%
  rename(Lema_Demografico = Lema) %>%
  inner_join(
    biblia_sentimientos_detectados %>% select(ID_Versiculo, Sentimiento, Lema_Sentimiento = Lema),
    by = "ID_Versiculo"
  ) %>%
  distinct(Testamento, ID_Versiculo, Lema_Demografico, Sentimiento)


# ## Visualización con gráfico de barras
datos_grafico_demo <- asociaciones_demograficas %>%
  count(Testamento, Lema_Demografico, sort = TRUE) %>%
  group_by(Testamento) %>%
  slice_max(n, n = 15) %>%
  ungroup()

plot_barras_demo <- datos_grafico_demo %>%
  mutate(Lema_Demografico = reorder_within(Lema_Demografico, n, Testamento)) %>%
  ggplot(aes(x = Lema_Demografico, y = n, fill = Testamento)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~Testamento, scales = "free_y") +
  labs(
    title = "Términos demográficos más frecuentes en contextos emocionales",
    subtitle = "Top 15 por Testamento",
    x = "Término Demográfico",
    y = "Frecuencia de Coocurrencia"
  ) +
  theme_minimal()

print("--- Mostrando Gráfico de Barras de Términos Demográficos ---")
print(plot_barras_demo)


# ## Coocurrencia con grafos
red_at_df <- asociaciones_demograficas %>%
  filter(Testamento == "Antiguo Testamento") %>%
  count(Sentimiento, Lema_Demografico, sort = TRUE) %>%
  filter(n >= 5)

red_nt_df <- asociaciones_demograficas %>%
  filter(Testamento == "Nuevo Testamento") %>%
  count(Sentimiento, Lema_Demografico, sort = TRUE) %>%
  filter(n >= 5)

visualizar_red <- function(df, titulo) {
  grafo <- graph_from_data_frame(df, directed = FALSE)
  V(grafo)$tipo <- ifelse(V(grafo)$name %in% unique(df$Sentimiento), "Sentimiento", "Demográfico")
  
  ggraph(grafo, layout = "fr") +
    geom_edge_link(aes(width = n, alpha = n), color = "gray") +
    scale_edge_width(range = c(0.5, 4)) +
    geom_node_point(aes(color = tipo), size = 5) +
    scale_color_manual(values = c("Sentimiento" = "#e41a1c", "Demográfico" = "#377eb8")) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
    theme_void() +
    labs(title = titulo, edge_width = "Frecuencia", color = "Tipo de Nodo") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
}

plot_at <- visualizar_red(red_at_df, "Antiguo Testamento")
plot_nt <- visualizar_red(red_nt_df, "Nuevo Testamento")
redes_plot <- plot_at | plot_nt

print("--- Mostrando Grafos de Coocurrencia ---")
print(redes_plot)


# ------------------------------------------------------------------------
# Guardar Resultados
# ------------------------------------------------------------------------
# 💡 NOTA: Se creará una carpeta 'output' para guardar los resultados.
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
ruta_guardado <- here::here(output_dir, "resultados_analisis.RData")

save(
  # Data Frames del Corpus
  biblia_df,
  biblia_df_procesado,
  biblia_lemas_final_df,
  # Léxicos Construidos
  lexico_sentimientos_df,
  lexico_demografico_df,
  # Resultados de Análisis
  biblia_sentimientos_detectados,
  asociaciones_demograficas,
  biblia_nrc_summary_es,
  # Gráficos
  nube_at,
  nube_nt,
  grafico_top_libros,
  redes_plot,
  # Archivo a guardar
  file = ruta_guardado
)

message(paste("Resultados del análisis guardados exitosamente en:", ruta_guardado))