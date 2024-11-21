# Limpiar el entorno
rm(list = ls())

# Cargar librerías necesarias
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(rstatix)

# Obtener el directorio actual
setwd("~/dmeyf2024/src/exp-colaborativo-canaritos")
root_folder <- file.path(getwd(), "modelos")

# Listar todas las subcarpetas (solo un nivel de profundidad)
subfolders <- list.dirs(root_folder, full.names = TRUE, recursive = FALSE)

# Verificar las subcarpetas listadas
print(subfolders)

# Inicializar una lista vacía para almacenar dataframes
data_list <- list()

# Recorrer cada subcarpeta
for (subfolder in subfolders) {
  # Definir la ruta al archivo ganancias_log.txt
  file_path <- file.path(subfolder, "ganancias_log.txt")
  
  # Verificar si el archivo existe
  if (file.exists(file_path)) {
    # Leer el archivo en un dataframe
    temp_df <- read_delim(file_path, show_col_types = FALSE)
    temp_df <- head(temp_df, -1)
    
    # Agregar una nueva columna 'modelo' con el nombre de la subcarpeta
    temp_df$modelo <- basename(subfolder)
    
    # Agregar el dataframe a la lista
    data_list[[length(data_list) + 1]] <- temp_df
  }
}

# Combinar todos los dataframes en un único dataframe
df_resultados <- bind_rows(data_list)

# Crear la tabla base con los nombres de los modelos y la cantidad de columnas
tabla_base_modelos <- data.frame(
  modelo = c(
    "W-CN-1-0", "W-CN-0.6-0", "W-SIN-CN", "W-CN-0.8-MAS-2", "W-CN-0.4-MAS-2", 
    "W-CN-0.2-0", "W-CN-0.8-0", "W-CN-1-MAS-1", "W-CN-0.4-0", "W-CN-0.2-MAS-2", 
    "W-CN-0.6-MAS-2", "W-CN-0.4-MENOS-1", "W-CN-0.2-MENOS-1", "W-CN-1-MENOS-1", 
    "W-CN-0.6-MAS-1", "W-CN-0.6-MENOS-1", "W-CN-0.4-MAS-1", "W-CN-0.8-MENOS-1", 
    "W-CN-0.2-MENOS-2", "W-CN-0.8-MAS-1", "W-CN-1-MAS-2", "W-CN-0.4-MENOS-2", 
    "W-CN-0.2-MAS-1", "W-CN-0.6-MENOS-2", "W-CN-0.8-MENOS-2", "W-CN-1-MENOS-2"
  ),
  Cantidad_columnas = c(
    246.0, 216.0, 1116.0, 440.0, 392.0, 
    188.0, 242.0, 352.0, 220.0, 319.0, 
    402.0, 172.0, 145.0, 171.0, 
    281.0, 170.0, 279.0, 189.0, 
    62.0, 293.0, 427.0, 56.0, 
    239.0, 41.0, 46.0, 20.0
  )
)

# Agregar la cantidad de columnas al dataframe original de modelos
df_resultados <- df_resultados %>%
  left_join(tabla_base_modelos, by = "modelo")

# Mostrar la tabla enriquecida
print(df_resultados)

# Ordenar los modelos por la mediana de las ganancias
df_resultados$modelo <- factor(
  df_resultados$modelo,
  levels = df_resultados %>%
    group_by(modelo) %>%
    summarise(mediana_ganancia = median(ganancia, na.rm = TRUE)) %>%
    arrange(desc(mediana_ganancia)) %>%
    pull(modelo)
)

# Realizar el test de Wilcoxon entre todos los modelos
test_wilcoxon <- pairwise.wilcox.test(df_resultados$ganancia, df_resultados$modelo, p.adjust.method = "bonferroni")

# Crear una tabla con los resultados del test de Wilcoxon, incluyendo significancia
crear_tabla_comparaciones_completa <- function(test_wilcoxon) {
  # Convertir la matriz de p-valores a un dataframe ordenado
  tabla_comparaciones <- as.data.frame(as.table(test_wilcoxon$p.value)) %>%
    filter(!is.na(Freq)) %>%  # Remover valores NA
    rename(modelo1 = Var1, modelo2 = Var2, p_valor = Freq) %>%  # Renombrar columnas
    arrange(p_valor) %>%  # Ordenar por p-valor
    mutate(significativo = ifelse(p_valor < 0.05, "Sí", "No"))  # Agregar columna de significancia
  return(tabla_comparaciones)
}

# Generar la tabla completa
tabla_comparaciones_completa <- crear_tabla_comparaciones_completa(test_wilcoxon)

# Mostrar la tabla en consola
print("Comparaciones - Todos los Modelos:")
print(tabla_comparaciones_completa)

# Guardar la tabla como archivo CSV
write.csv(tabla_comparaciones_completa, "tabla_comparaciones_completa.csv", row.names = FALSE)

# Crear boxplots de las ganancias ordenados
grafico_boxplots <- ggplot(data = df_resultados, aes(x = modelo, y = ganancia)) +
  stat_boxplot(geom = "errorbar") +  # Mostrar barras de error
  geom_boxplot(outlier.shape = NA) +  # Boxplot sin outliers visibles
  theme_bw() +
  geom_jitter(color = "red", width = 0.1, size = 2, alpha = 0.6) +  # Mostrar puntos originales en rojo
  labs(
    title = "Comparación de Ganancias por Modelo (Ordenados)",
    x = "Modelo (Ordenado por Mediana)",
    y = "Ganancia"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje X

# Mostrar el gráfico
print(grafico_boxplots)

# ===========================
# Comparar un modelo específico contra los demás
# ===========================

# Función para comparar un modelo específico contra todos los demás
comparar_modelo_especifico <- function(test_wilcoxon, modelo_seleccionado) {
  # Convertir la matriz de p-valores a un dataframe
  tabla_comparaciones <- as.data.frame(as.table(test_wilcoxon$p.value)) %>%
    filter(!is.na(Freq)) %>%  # Remover valores NA
    rename(modelo1 = Var1, modelo2 = Var2, p_valor = Freq) %>%  # Renombrar columnas
    filter(modelo1 == modelo_seleccionado | modelo2 == modelo_seleccionado) %>%  # Filtrar comparaciones con el modelo seleccionado
    mutate(significativo = ifelse(p_valor < 0.05, "Sí", "No"))  # Agregar columna de significancia
  return(tabla_comparaciones)
}

# Modelo a comparar (puedes cambiar este valor para seleccionar otro modelo)
modelo_a_comparar <- "W-CN-0.8-MAS-2"

# Generar tabla de comparaciones para el modelo seleccionado
tabla_modelo_seleccionado <- comparar_modelo_especifico(test_wilcoxon, modelo_a_comparar)

# Mostrar la tabla en consola
print(paste("Comparaciones para el modelo:", modelo_a_comparar))
print(tabla_modelo_seleccionado)


# Guardar la tabla como archivo CSV
write.csv(tabla_modelo_seleccionado, paste0("comparaciones_", modelo_a_comparar, ".csv"), row.names = FALSE)

# Ordenar los niveles de los modelos por la mediana de las ganancias
orden_modelos <- df_resultados %>%
  group_by(modelo) %>%
  summarise(mediana_ganancia = median(ganancia, na.rm = TRUE)) %>%
  arrange(desc(mediana_ganancia)) %>%
  pull(modelo)

# Reordenar el factor 'modelo' en el dataframe
df_resultados$modelo <- factor(df_resultados$modelo, levels = orden_modelos)

# Crear el gráfico de barras original
grafico_barras_columnas <- ggplot(df_resultados, aes(x = modelo, y = Cantidad_columnas)) +
  geom_bar(stat = "identity", fill = "steelblue", color = NA, width = 0.7) +  # Barras sin bordes
  geom_text(aes(label = Cantidad_columnas), vjust = -0.5, size = 3.5, fontface = "bold") +  # Etiquetas claras
  labs(
    title = "Cantidad de columnas luego de los canaritos por Modelo",
    x = "Modelo",
    y = "Cantidad de Columnas"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotar y ajustar tamaño de etiquetas en eje X
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Título centrado y resaltado
    axis.title = element_text(size = 12)  # Tamaño mayor para los títulos de los ejes
  )

# Mostrar el gráfico
print(grafico_barras_columnas)




