# esqueleto de grid search con Montecarlo Cross Validation
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
require("primes")

PARAM <- list()
# reemplazar por su primer semilla
PARAM$semilla_primigenia <- 270143
PARAM$qsemillas <- 20

PARAM$training_pct <- 70L  # entre 1L y 99L
PARAM$dataset_nom <- "./datasets/competencia_01_R.csv"

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}

#------------------------------------------------------------------------------
ArbolEstimarGanancia <- function(semilla, training_pct, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset,
              division = c(training_pct, 100L - training_pct), 
              agrupa = "clase_ternaria",
              seed = semilla
  )
  
  # Extraer los datos de entrenamiento (fold == 1)
  data_train <- dataset[fold == 1]
  
  # Generar el modelo
  modelo <- rpart("clase_ternaria ~ .",
                  data = data_train, # fold==1 es training, el 70% de los datos
                  xval = 0,
                  control = param_basicos
  )
  
  # Aplicar el modelo a los datos de testing
  prediccion <- predict(modelo, dataset[fold == 2], type = "prob")
  
  # Calcular la ganancia en testing que es fold==2
  ganancia_test <- dataset[fold == 2,
                           sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
                                      ifelse(clase_ternaria == "BAJA+2", 273000, -3000),
                                      0
                           ))
  ]
  
  # Escalar la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / ((100 - PARAM$training_pct) / 100)
  
  # Retornamos un data.table con las clases consistentes
  return( data.table(
    semilla = as.integer(semilla),  # Forzamos a integer
    cp = as.numeric(param_basicos$cp),  # Forzamos a numeric
    maxdepth = as.integer(param_basicos$maxdepth),  # Forzamos a integer
    minsplit = as.integer(param_basicos$minsplit),  # Forzamos a integer
    minbucket = as.integer(param_basicos$minbucket),  # Forzamos a integer
    ganancia_test = as.numeric(ganancia_test_normalizada)  # Forzamos a numeric
  )
  )
}

#------------------------------------------------------------------------------
ArbolesMontecarlo <- function(semillas, param_basicos) {
  salida <- mcmapply(ArbolEstimarGanancia,
                     semillas,
                     MoreArgs = list(PARAM$training_pct, param_basicos),
                     SIMPLIFY = FALSE,
                     mc.cores = 1  # Usamos un solo núcleo para depurar
  )
  return(salida)
}

#------------------------------------------------------------------------------
# Establecer el directorio de trabajo
setwd("~/Desktop/DMEYF")

# Generar numeros primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia)
PARAM$semillas <- sample(primos, PARAM$qsemillas)

# Cargar los datos
dataset <- fread(PARAM$dataset_nom)
dataset <- dataset[foto_mes == 202104]

# Crear la carpeta del experimento
dir.create("~/Desktop/DMEYF/b1/exp/HT2810/", showWarnings = FALSE)
setwd("~/Desktop/DMEYF")

# Crear la tabla de resultados para el Grid Search
tb_grid_search_detalle <- data.table(
  semilla = integer(),
  cp = numeric(),
  maxdepth = integer(),
  minsplit = integer(),
  minbucket = integer(),
  ganancia_test = numeric()
)

# Bucle para recorrer los hiperparámetros, comenzando con cp = -0.5
for (vcp in c(-0.5, 0.001, 0.01, 0.05, 0.1)) {  # Incluir cp = -0.5
  for (vmax_depth in c(4, 6, 8, 10, 12, 14)) {
    for (vmin_split in c(1000, 800, 600, 400, 200, 100, 50, 20, 10)) {
      for (vmin_bucket in c(5, 10, 15, 20)) {
        if (vmin_bucket <= vmin_split) {
          param_basicos <- list(
            "cp" = vcp,  # Aquí incluimos el valor de cp en el bucle
            "maxdepth" = vmax_depth,
            "minsplit" = vmin_split,
            "minbucket" = vmin_bucket
          )
          
          # Ejecutar Montecarlo
          ganancias <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
          
          # Combinar los resultados, forzando las clases consistentes
          ganancias <- lapply(ganancias, function(x) {
            set(x, j = "semilla", value = as.integer(x$semilla))
            set(x, j = "cp", value = as.numeric(x$cp))
            set(x, j = "maxdepth", value = as.integer(x$maxdepth))
            set(x, j = "minsplit", value = as.integer(x$minsplit))
            set(x, j = "minbucket", value = as.integer(x$minbucket))
            set(x, j = "ganancia_test", value = as.numeric(x$ganancia_test))
            return(x)
          })
          
          tb_grid_search_detalle <- rbindlist( 
            list(tb_grid_search_detalle, rbindlist(ganancias)),
            fill = TRUE, use.names = TRUE
          )
        }
      }
    }
  }
  
  # Guardar resultados intermedios
  fwrite(tb_grid_search_detalle,
         file = "gridsearch_detalle.txt",
         sep = "\t")
}

#----------------------------

# Crear y guardar el resumen
tb_grid_search <- tb_grid_search_detalle[,
                                         list(
                                           "ganancia_mean" = mean(ganancia_test),
                                           "qty" = .N
                                         ),
                                         list(cp, maxdepth, minsplit, minbucket)
]

# Ordenar por ganancia
setorder(tb_grid_search, -ganancia_mean)

# Asignar un ID a cada combinación
tb_grid_search[, id := .I]

# Guardar el resumen en un archivo
fwrite(tb_grid_search,
       file = "gridsearch.txt",
       sep = "\t")
