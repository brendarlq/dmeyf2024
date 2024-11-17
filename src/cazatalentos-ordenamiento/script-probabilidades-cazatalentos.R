# Definir datos iniciales de las candidatas
candidatas <- list(
  C1 = list(poblacion = 100, ciclos = 1, aciertos = 80, tiros = 100),
  C2 = list(poblacion = 200, ciclos = 1, aciertos = 80, tiros = 100),
  C3 = list(poblacion = 1, ciclos = 1, aciertos = 80, tiros = 100, sesgo = TRUE),  # Indicador de sesgo
  C4 = list(poblacion = 2, ciclos = 1, aciertos = 80, tiros = 100),
  C5 = list(poblacion = 1, ciclos = 10, aciertos = mean(c(68, 74, 78, 70, 68, 63, 80, 68, 67, 65)), tiros = 100)
)

# Función para calcular probabilidad ponderada
calcular_probabilidad_ponderada <- function(candidata) {
  p_observada <- candidata$aciertos / candidata$tiros
  n <- candidata$poblacion
  c <- candidata$ciclos
  
  # Calcular evidencia
  evidencia <- n * c
  
  # Ajustar probabilidad por evidencia
  p_ponderada <- p_observada * log10(1 + evidencia) / log10(1 + max(sapply(candidatas, function(x) x$poblacion * x$ciclos)))
  
  # Penalización adicional para C3 (sesgo subjetivo)
  if (!is.null(candidata$sesgo) && candidata$sesgo) {
    penalizacion <- 0.8 / sqrt(n)
    p_ponderada <- p_ponderada * (1 - penalizacion)
  }
  
  return(min(p_ponderada, 1))  # Limitar a 1 como probabilidad máxima
}

# Calcular probabilidades ajustadas para todas las candidatas
probabilidades <- sapply(names(candidatas), function(k) {
  calcular_probabilidad_ponderada(candidatas[[k]])
})

# Ordenar candidatas por probabilidad ajustada
orden <- order(probabilidades, decreasing = TRUE)

# Mostrar resultados
cat("Orden de las candidatas: ", paste(names(candidatas)[orden], collapse = " < "), "\n")
print(probabilidades)
