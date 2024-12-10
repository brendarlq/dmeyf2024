## Instructivo tercera competencia

1. Generación de dataset con clase ternaria con generacion-clase-ternaria.r
2. Ejecutar 909_run_orden227.r

### Modificaciones realizadas al script 990_workflow_orden227_SEMI.r
1. Semilla: 111119
2. Etapa FEhist: Lags 1,2 y 3 y tendencias de primer orden y de segundo orden
3. Etapa FErf_attributes: valores experimento colaborativo (num_iteration=25)
4. TS_strategy: Saque los meses 202104, 202006, 201910, 201905 y undersampling: 0.08
5. HT_tuning_semillerio: 20 iteraciones
6. FM_final_models_lightgbm_semillerio: 100 semillerio, 2 repeticiones
7. Punto de corte: 10500
