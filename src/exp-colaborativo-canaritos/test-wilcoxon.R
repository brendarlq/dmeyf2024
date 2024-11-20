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

# Crear boxplots de las ganancias
grafico_boxplots <- ggplot(data = df_resultados, aes(x = modelo, y = ganancia)) +
  stat_boxplot(geom = "errorbar") +  # Mostrar barras de error
  geom_boxplot(outlier.shape = NA) +  # Boxplot sin outliers visibles
  theme_bw() +
  geom_jitter(color = "red", width = 0.1, size = 2, alpha = 0.6) +  # Mostrar puntos originales en rojo
  labs(
    title = "Comparación de Ganancias por Modelo",
    x = "Modelo",
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
    filter(modelo1 == modelo_seleccionado | modelo2 == modelo_seleccionado) %>%  # Filtrar por el modelo seleccionado
    mutate(significativo = ifelse(p_valor < 0.05, "Sí", "No"))  # Agregar columna de significancia
  return(tabla_comparaciones)
}

# Modelo a comparar (puedes cambiar este valor para seleccionar otro modelo)
modelo_a_comparar <- "W-CN-1-0"

# Generar tabla de comparaciones para el modelo seleccionado
tabla_modelo_seleccionado <- comparar_modelo_especifico(test_wilcoxon, modelo_a_comparar)

# Mostrar la tabla en consola
print(paste("Comparaciones para el modelo:", modelo_a_comparar))
print(tabla_modelo_seleccionado)

# Guardar la tabla como archivo CSV
write.csv(tabla_modelo_seleccionado, paste0("comparaciones_", modelo_a_comparar, ".csv"), row.names = FALSE)
