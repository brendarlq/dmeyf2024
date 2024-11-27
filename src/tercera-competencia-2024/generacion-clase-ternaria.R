require("data.table")

# Leo el dataset comprimido en .gz
dataset <- fread("~/buckets/b1/datasets/competencia_03_crudo.csv.gz")

# Calculo el periodo0 consecutivo
dsimple <- dataset[, list(
  "pos" = .I,
  numero_de_cliente,
  periodo0 = as.integer(foto_mes / 100) * 12 + foto_mes %% 100)]

# Ordeno
setorder(dsimple, numero_de_cliente, periodo0)

# Detecto huecos de un mes
dsimple[, next_periodo0 := shift(periodo0, type = "lead"), by = numero_de_cliente]
dsimple[, hay_hueco := (next_periodo0 - periodo0 > 1)]

# Calculo topes
periodo_ultimo <- dsimple[, max(periodo0)]
periodo_anteultimo <- periodo_ultimo - 1

# Calculo los leads de orden 1 y 2
dsimple[, c("periodo1", "periodo2") := shift(periodo0, n = 1:2, fill = NA, type = "lead"), by = numero_de_cliente]

# Asigno "CONTINUA" por defecto
dsimple[periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA"]

# Calculo BAJA+1
dsimple[periodo0 < periodo_ultimo &
          (is.na(periodo1) | periodo0 + 1 < periodo1 | hay_hueco), 
        clase_ternaria := "BAJA+1"]

# Calculo BAJA+2
dsimple[periodo0 < periodo_anteultimo & (periodo0 + 1 == periodo1) &
          (is.na(periodo2) | periodo0 + 2 < periodo2 | hay_hueco),
        clase_ternaria := "BAJA+2"]

# Agrego la columna de clase_ternaria al dataset original
setorder(dsimple, pos)
dataset[, clase_ternaria := dsimple$clase_ternaria]

# Guardo el resultado en un archivo comprimido .gz
fwrite(dataset, file = "~/buckets/b1/datasets/competencia_03_R.csv.gz", sep = ",")
