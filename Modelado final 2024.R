# Cargar los paquetes necesarios
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "dplyr", "ggplot2", "raster", "sp", "car", "maxnet", "pROC"))
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)
library(terra)
library(car)
library(maxnet)
library(pROC)

# Establecer el directorio de trabajo
path <- "C:/Users/HP/Desktop/Maestria PUCE 2023/Proyecto de Investigación/R/"

# Descargar datos climáticos de temperatura mínima, máxima y precipitación de WorldClim
wc_tmin <- worldclim_global(var = "tmin", res = 2.5, path = path)
wc_tmax <- worldclim_global(var = "tmax", res = 2.5, path = path)
wc_prec <- worldclim_global(var = "prec", res = 2.5, path = path)

# Filtrar las provincias de interés
provincias_interes <- c("Los Rios", "Manabi", "Esmeraldas", "Guayas", "Santo Domingo de los Tsáchilas")
mapa_provincias <- ecuador_map %>%
  filter(name %in% provincias_interes)

# Visualizar el mapa de las provincias seleccionadas
ggplot(data = mapa_provincias) +
  geom_sf(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Provincias Seleccionadas para el Estudio del Cacao")

# Recortar los datos climáticos a las provincias seleccionadas
wc_tmin_recortado <- crop(wc_tmin, st_bbox(mapa_provincias))
wc_tmax_recortado <- crop(wc_tmax, st_bbox(mapa_provincias))
wc_prec_recortado <- crop(wc_prec, st_bbox(mapa_provincias))

# Extraer estadísticas climáticas (media)
tmin_stats <- terra::extract(wc_tmin_recortado, vect(mapa_provincias), fun = mean, na.rm = TRUE)
tmax_stats <- terra::extract(wc_tmax_recortado, vect(mapa_provincias), fun = mean, na.rm = TRUE)
prec_stats <- terra::extract(wc_prec_recortado, vect(mapa_provincias), fun = mean, na.rm = TRUE)

# Unir los resultados en un data frame
climate_data <- data.frame(
  Provincia = provincias_interes,
  Temp_Minima = tmin_stats,
  Temp_Maxima = tmax_stats,
  Precipitacion = prec_stats
)

# Cargar Datos de Rendimiento del Cacao
rendimiento_cacao <- read.csv("C:/Users/HP/Desktop/Maestria PUCE 2023/Proyecto de Investigación/R/tabla_provincias.csv")

# Unir los datos
datos_completos <- merge(rendimiento_cacao, climate_data, by = "Provincia")

summary(datos_completos)

names(datos_completos)

# Crear la columna de presencia y seleccionar las columnas relevantes
presencias <- datos_completos %>%
  mutate(presencia = ifelse(`Rendimiento.Promedio.tm` > 0, 1, 0)) %>%
  dplyr::select(
    `Temperatura.Minima.Promedio.C`, 
    `Temperatura.Maxima.Promedio.C`, 
    `Precipitacion.Anual.Promedio.mm`, 
    presencia
  )



# Verificar las primeras filas del dataframe resultante
head(presencias)

nrow(datos_completos)

# Modelo con un solo predictor
modelo_simple <- lm(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C, data = datos_completos)
summary(modelo_simple)

# Modelo con dos predictores
modelo_dos <- lm(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C + Temperatura.Maxima.Promedio.C, data = datos_completos)
summary(modelo_dos)

summary(datos_completos$Rendimiento.Promedio.tm)

#Visualización: Usa gráficos para visualizar la distribución de Rendimiento.Promedio.tm y su relación con otras variables.

# Histograma
hist(datos_completos$Rendimiento.Promedio.tm, main = "Distribución del Rendimiento Promedio", xlab = "Rendimiento Promedio (tm)")

# Boxplot
boxplot(datos_completos$Rendimiento.Promedio.tm, main = "Boxplot del Rendimiento Promedio", ylab = "Rendimiento Promedio (tm)")

# Gráfico de dispersión con predictores
plot(datos_completos$Temperatura.Minima.Promedio.C, datos_completos$Rendimiento.Promedio.tm, 
     xlab = "Temperatura Mínima Promedio (°C)", ylab = "Rendimiento Promedio (tm)", 
     main = "Rendimiento vs. Temperatura Mínima")


#Transformación Logarítmica:
datos_completos$log_Rendimiento <- log(datos_completos$Rendimiento.Promedio.tm + 1)
modelo_log <- lm(log_Rendimiento ~ Temperatura.Minima.Promedio.C + Temperatura.Maxima.Promedio.C, data = datos_completos)
summary(modelo_log)

#Transformaciones Cuadráticas o Polinómicas:
modelo_pol <- lm(Rendimiento.Promedio.tm ~ poly(Temperatura.Minima.Promedio.C, 2) + poly(Temperatura.Maxima.Promedio.C, 2), data = datos_completos)
summary(modelo_pol)

# Cargar las librerías necesarias
library(dplyr)

# Preparar los datos
datos_completos <- datos_completos %>%
  # Asegúrate de que las columnas numéricas estén en formato numérico
  mutate(
    Temperatura.Minima.Promedio.C = as.numeric(Temperatura.Minima.Promedio.C),
    Temperatura.Maxima.Promedio.C = as.numeric(Temperatura.Maxima.Promedio.C),
    Precipitacion.Anual.Promedio.mm = as.numeric(Precipitacion.Anual.Promedio.mm),
    Rendimiento.Promedio.tm = as.numeric(Rendimiento.Promedio.tm)
  )

# Crear el modelo de regresión lineal
modelo_lineal <- lm(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C + Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm, data = datos_completos)

# Resumen del modelo
summary(modelo_lineal)

# Opcional: Guardar los resultados del modelo
write.csv(summary(modelo_lineal)$coefficients, file = "resultados_modelo_lineal.csv")


# Modelos de Machine Learning
# Instalar el paquete si no está instalado
install.packages("rpart")

library(rpart)

# Ajustar un modelo de árbol de decisión
modelo_arbol <- rpart(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C + Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm, data = datos_completos)
summary(modelo_arbol)

#Visualización del Árbol:
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(modelo_arbol)


# Ejemplo de uso de validación cruzada con el paquete `cvms`
install.packages("cvms")
library(cvms)

# Dividir el conjunto de datos en entrenamiento y prueba
set.seed(123)
train_index <- sample(seq_len(nrow(datos_completos)), size = 0.7 * nrow(datos_completos))
train_data <- datos_completos[train_index, ]
test_data <- datos_completos[-train_index, ]

# Ajustar el modelo en el conjunto de entrenamiento
modelo_arbol <- rpart(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C + Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm, data = train_data)

# Realizar la predicción en el conjunto de prueba
predicciones <- predict(modelo_arbol, newdata = test_data)

# Evaluar el rendimiento del modelo
mse <- mean((test_data$Rendimiento.Promedio.tm - predicciones)^2)
print(paste("MSE en el conjunto de prueba:", mse))

# Crear un data frame con las predicciones y los valores reales
resultados <- data.frame(
  Real = test_data$Rendimiento.Promedio.tm,
  Prediccion = predicciones
)

# Graficar las predicciones frente a los valores reales
library(ggplot2)
ggplot(resultados, aes(x = Real, y = Prediccion)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs. Valores Reales",
       x = "Valores Reales",
       y = "Predicciones")


#Modelos de Regresión No Lineal (nls)

# Ajustar un modelo de regresión no lineal con parámetros iniciales ajustados
modelo_nls_simple <- nls(Rendimiento.Promedio.tm ~ a * Temperatura.Minima.Promedio.C, 
                         data = datos_completos, 
                         start = list(a = 10000), 
                         control = nls.control(maxiter = 200, tol = 1e-8))
summary(modelo_nls_simple)

#Evaluar la Calidad del Modelo
#Gráfica de Ajuste y Residuos
library(ggplot2)

ggplot(datos_completos, aes(x = Temperatura.Minima.Promedio.C, y = Rendimiento.Promedio.tm)) +
  geom_point() +
  geom_abline(intercept = coef(modelo_nls_simple)["a"], slope = 0, color = "blue") +
  labs(title = "Ajuste del Modelo No Lineal Simple",
       x = "Temperatura Mínima Promedio (°C)",
       y = "Rendimiento Promedio (tm)") +
  theme_minimal()

#Gráfica de Residuos
residuos <- residuals(modelo_nls_simple)

ggplot(data = data.frame(Residuos = residuos), aes(x = residuos)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  labs(title = "Histograma de Residuos",
       x = "Residuos",
       y = "Frecuencia") +
  theme_minimal()


#Cálculo del Error Cuadrático Medio (MSE)
predicciones <- predict(modelo_nls_simple, newdata = datos_completos)
mse <- mean((datos_completos$Rendimiento.Promedio.tm - predicciones)^2)
mse

#Comparar con Otros Modelos

#a. Regresión Lineal
modelo_lineal <- lm(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C, data = datos_completos)
predicciones_lineales <- predict(modelo_lineal, newdata = datos_completos)
mse_lineal <- mean((datos_completos$Rendimiento.Promedio.tm - predicciones_lineales)^2)
mse_lineal

#b. Modelo de Árbol de Decisión
library(rpart)

modelo_arbol <- rpart(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C + Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm, data = datos_completos)
predicciones_arbol <- predict(modelo_arbol, newdata = datos_completos)
mse_arbol <- mean((datos_completos$Rendimiento.Promedio.tm - predicciones_arbol)^2)
mse_arbol

#c Regresión Generalizada (GLM)

# Asegúrate de que no hay NA en los datos
summary(datos_completos)

# Verifica que los datos son correctos
head(datos_completos)

# Ajustar nuevamente el modelo GLM y revisar el resumen
modelo_glm <- glm(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C + 
                    Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm,
                  data = datos_completos, family = gaussian())
summary(modelo_glm)

# Ajustar un modelo de regresión generalizada
modelo_glm <- glm(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C + 
                    Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm,
                  data = datos_completos, family = gaussian())

# Resumen del modelo
summary(modelo_glm)

# Verificar datos
summary(datos_completos)
str(datos_completos)

# Comprobar multicolinealidad
cor(datos_completos[, c("Temperatura.Minima.Promedio.C", "Temperatura.Maxima.Promedio.C", "Precipitacion.Anual.Promedio.mm")])

# Verificar valores faltantes
sum(is.na(datos_completos))

# Ajustar un modelo más simple si es posible
modelo_simple <- glm(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C, data = datos_completos)
summary(modelo_simple)

# Obtener las predicciones
predicciones_glm <- predict(modelo_glm, newdata = datos_completos)

# Imprimir algunas predicciones y valores reales
head(predicciones_glm)
head(datos_completos$Rendimiento.Promedio.tm)

# Calcular el MSE nuevamente
mse_glm <- mean((datos_completos$Rendimiento.Promedio.tm - predicciones_glm)^2)
print(paste("MSE del modelo GLM:", mse_glm))


#d Redes Neuronales
# Cargar la librería
library(nnet)

# Ajustar un modelo de red neuronal
modelo_nn <- nnet(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C + 
                    Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm,
                  data = datos_completos, size = 5, linout = TRUE)

# Resumen del modelo
summary(modelo_nn)

# Predicciones y MSE
predicciones_nn <- predict(modelo_nn, newdata = datos_completos)
mse_nn <- mean((datos_completos$Rendimiento.Promedio.tm - predicciones_nn)^2)
print(paste("MSE del modelo de red neuronal:", mse_nn))


#e Support Vector Machines (SVM)
# Cargar la librería
library(e1071)
library(caTools)  # Para la división de los datos

# Ajustar un modelo SVM
modelo_svm <- svm(Rendimiento.Promedio.tm ~ Temperatura.Minima.Promedio.C + 
                    Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm,
                  data = datos_completos)

# Resumen del modelo
summary(modelo_svm)

# Predicciones y MSE
predicciones_svm <- predict(modelo_svm, newdata = datos_completos)
mse_svm <- mean((datos_completos$Rendimiento.Promedio.tm - predicciones_svm)^2)
print(paste("MSE del modelo SVM:", mse_svm))


# Dividir los datos en entrenamiento y prueba (70% entrenamiento, 30% prueba)
set.seed(123)  # Para reproducibilidad
sample <- sample.split(datos_completos$Rendimiento.Promedio.tm, SplitRatio = 0.7)
entrenamiento <- subset(datos_completos, sample == TRUE)
prueba <- subset(datos_completos, sample == FALSE)

# Construcción del Modelo Predictivo SVM
modelo_final <- svm(Rendimiento.Promedio.tm ~ Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm, 
                    data = entrenamiento)

# Validación del Modelo
predicciones <- predict(modelo_final, prueba)
mse_final <- mean((predicciones - prueba$Rendimiento.Promedio.tm)^2)
print(mse_final)

# Escalar las variables predictoras
entrenamiento[, c("Temperatura.Maxima.Promedio.C", "Precipitacion.Anual.Promedio.mm")] <- scale(entrenamiento[, c("Temperatura.Maxima.Promedio.C", "Precipitacion.Anual.Promedio.mm")])
prueba[, c("Temperatura.Maxima.Promedio.C", "Precipitacion.Anual.Promedio.mm")] <- scale(prueba[, c("Temperatura.Maxima.Promedio.C", "Precipitacion.Anual.Promedio.mm")])

# Volver a construir el modelo
modelo_final <- svm(Rendimiento.Promedio.tm ~ Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm, 
                    data = entrenamiento)

# Validar el modelo
predicciones <- predict(modelo_final, prueba)
mse_final <- mean((predicciones - prueba$Rendimiento.Promedio.tm)^2)
print(mse_final)

#Modelos de Series Temporales (si aplica)
# Cargar la librería
install.packages("forecast")
library(forecast)

# Ajustar un modelo ARIMA
modelo_arima <- auto.arima(datos_completos$Rendimiento.Promedio.tm)

# Resumen del modelo
summary(modelo_arima)

# Predicciones y MSE
predicciones_arima <- forecast(modelo_arima, h = length(datos_completos$Rendimiento.Promedio.tm))$mean
mse_arima <- mean((datos_completos$Rendimiento.Promedio.tm - predicciones_arima)^2)
print(paste("MSE del modelo ARIMA:", mse_arima))


#Comparación de Modelos

#Resumen de Resultados

# Crear un dataframe con los resultados
resultados_mse <- data.frame(
  Modelo = c("Regresión Generalizada", "Red Neuronal", "Árbol de Decisión", "SVM", "ARIMA"),
  MSE = c(mse_glm, mse_nn, mse_arbol, mse_svm, mse_arima)
)

print(resultados_mse)

#Graficar el MSE

library(ggplot2)

# Crear el dataframe con los resultados
resultados_mse <- data.frame(
  Modelo = c("Regresión Generalizada", "Red Neuronal", "Árbol de Decisión", "SVM", "ARIMA"),
  MSE = c(4.896909e-22, 3.118809e+07, 3.118809e+07, 7.237342e+05, 3.118809e+07)
)

# Crear el gráfico de barras
ggplot(resultados_mse, aes(x = Modelo, y = MSE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Comparación del Error Cuadrático Medio (MSE) de los Modelos",
       x = "Modelo",
       y = "Error Cuadrático Medio (MSE)") +
  scale_y_log10() + # Usar escala logarítmica para mejor visualización
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Modelo final predictivo

#División de Datos

str(datos_completos$Rendimiento.Promedio.tm)
summary(datos_completos$Rendimiento.Promedio.tm)
names(datos_completos)


install.packages("caTools")
library(caTools)

#Divide los datos en un conjunto de entrenamiento y uno de prueba. Por ejemplo, un 70% de los datos para entrenamiento y un 30% para prueba
set.seed(123)  # Para reproducibilidad
sample <- sample.split(datos_completos$Rendimiento.Promedio.tm, SplitRatio = 0.7)
entrenamiento <- subset(datos_completos, sample == TRUE)
prueba <- subset(datos_completos, sample == FALSE)

#Construcción del Modelo Predictivo
modelo_final <- svm(Rendimiento.Promedio.tm ~ Temperatura.Maxima.Promedio.C + Precipitacion.Anual.Promedio.mm, data = entrenamiento)

#Validación del Modelo

predicciones <- predict(modelo_final, prueba)
mse_final <- mean((predicciones - prueba$Rendimiento.Promedio.tm )^2)
print(mse_final)

#seguir ajustando o validando tu modelo, puedes hacerlo de la siguiente manera:
#a. Visualización de Predicciones vs Valores Reales:

library(ggplot2)

ggplot() +
  geom_point(aes(x = prueba$Rendimiento.Promedio.tm, y = predicciones), color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Predicciones vs Valores Reales", x = "Valores Reales", y = "Predicciones") +
  theme_minimal()

#b. Evaluación Adicional del Modelo:

# Calcular MAE
mae_final <- mean(abs(predicciones - prueba$Rendimiento.Promedio.tm))
print(mae_final)

# Calcular R²
r_squared <- 1 - sum((predicciones - prueba$Rendimiento.Promedio.tm)^2) / sum((prueba$Rendimiento.Promedio.tm - mean(prueba$Rendimiento.Promedio.tm))^2)
print(r_squared)

#Vamos a crear escenarios de cambio climático y aplicar tu modelo predictivo
#Definir los escenarios climáticos

#Vamos a crear varios escenarios basados en cambios en la temperatura mínima, máxima, y precipitación. Un ejemplo podría ser:
  
#Escenario 1: Aumento de temperatura mínima y máxima en +1°C, sin cambios en precipitación.
#Escenario 2: Aumento de temperatura mínima y máxima en +2°C, reducción de precipitación en -10%.
#Escenario 3: Aumento de temperatura mínima y máxima en +3°C, aumento de precipitación en +15%.

# Copia del dataset original
escenario_1 <- datos_completos
escenario_2 <- datos_completos
escenario_3 <- datos_completos

# Escenario 1: Aumento de temperatura +1°C
escenario_1$Temperatura.Minima.Promedio.C <- escenario_1$Temperatura.Minima.Promedio.C + 1
escenario_1$Temperatura.Maxima.Promedio.C <- escenario_1$Temperatura.Maxima.Promedio.C + 1

# Escenario 2: Aumento de temperatura +2°C y reducción de precipitación -10%
escenario_2$Temperatura.Minima.Promedio.C <- escenario_2$Temperatura.Minima.Promedio.C + 2
escenario_2$Temperatura.Maxima.Promedio.C <- escenario_2$Temperatura.Maxima.Promedio.C + 2
escenario_2$Precipitacion.Anual.Promedio.mm <- escenario_2$Precipitacion.Anual.Promedio.mm * 0.90

# Escenario 3: Aumento de temperatura +3°C y aumento de precipitación +15%
escenario_3$Temperatura.Minima.Promedio.C <- escenario_3$Temperatura.Minima.Promedio.C + 3
escenario_3$Temperatura.Maxima.Promedio.C <- escenario_3$Temperatura.Maxima.Promedio.C + 3
escenario_3$Precipitacion.Anual.Promedio.mm <- escenario_3$Precipitacion.Anual.Promedio.mm * 1.15

#Realizar predicciones bajo los escenarios

#Ahora aplicaremos tu modelo a cada uno de los escenarios.

# Realiza predicciones usando el modelo entrenado en cada escenario
predicciones_escenario_1 <- predict(modelo_final, newdata = escenario_1)
predicciones_escenario_2 <- predict(modelo_final, newdata = escenario_2)
predicciones_escenario_3 <- predict(modelo_final, newdata = escenario_3)

# Agregar predicciones al dataset
escenario_1$Prediccion <- predicciones_escenario_1
escenario_2$Prediccion <- predicciones_escenario_2
escenario_3$Prediccion <- predicciones_escenario_3

#Comparar los resultados
# Comparar los resultados en una tabla o un gráfico
resultados <- data.frame(
  Provincia = datos_completos$Provincia,
  Escenario_1 = escenario_1$Prediccion,
  Escenario_2 = escenario_2$Prediccion,
  Escenario_3 = escenario_3$Prediccion
)

print(resultados)

#Crear un gráfico de barras comparativo
# Cargar la librería ggplot2
library(ggplot2)
library(reshape2)

# Reestructurar los datos para el gráfico
resultados_melt <- melt(resultados, id.vars = "Provincia", 
                        variable.name = "Escenario", 
                        value.name = "Prediccion")

# Crear un gráfico de barras
ggplot(resultados_melt, aes(x = Provincia, y = Prediccion, fill = Escenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparación de predicciones por escenario",
       x = "Provincia",
       y = "Predicción de rendimiento",
       fill = "Escenario")

#Alternativamente, crear un gráfico de líneas
# Crear un gráfico de líneas
ggplot(resultados_melt, aes(x = Provincia, y = Prediccion, group = Escenario, color = Escenario)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Comparación de predicciones por escenario",
       x = "Provincia",
       y = "Predicción de rendimiento",
       color = "Escenario")


#Definición de Escenarios (final):

#Aquí definimos tres escenarios climáticos con variaciones en temperatura y precipitación:

# Escenario 1: Aumento de temperatura mínima y máxima en +1°C, sin cambios en precipitación.
escenario_1 <- datos_completos
escenario_1$Temperatura.Minima.Promedio.C <- escenario_1$Temperatura.Minima.Promedio.C + 1
escenario_1$Temperatura.Maxima.Promedio.C <- escenario_1$Temperatura.Maxima.Promedio.C + 1

# Escenario 2: Aumento de temperatura mínima y máxima en +2°C, reducción de precipitación en -10%.
escenario_2 <- datos_completos
escenario_2$Temperatura.Minima.Promedio.C <- escenario_2$Temperatura.Minima.Promedio.C + 2
escenario_2$Temperatura.Maxima.Promedio.C <- escenario_2$Temperatura.Maxima.Promedio.C + 2
escenario_2$Precipitacion.Anual.Promedio.mm <- escenario_2$Precipitacion.Anual.Promedio.mm * 0.9

# Escenario 3: Aumento de temperatura mínima y máxima en +3°C, aumento de precipitación en +15%.
escenario_3 <- datos_completos
escenario_3$Temperatura.Minima.Promedio.C <- escenario_3$Temperatura.Minima.Promedio.C + 3
escenario_3$Temperatura.Maxima.Promedio.C <- escenario_3$Temperatura.Maxima.Promedio.C + 3
escenario_3$Precipitacion.Anual.Promedio.mm <- escenario_3$Precipitacion.Anual.Promedio.mm * 1.15

#Aplicación del Modelo SVM a los Escenarios

#Ahora aplicamos el modelo SVM que ajustaste previamente a estos escenarios y obtenemos las predicciones de rendimiento de cacao bajo estas condiciones:

# Aplicar el modelo SVM al Escenario 1
predicciones_escenario_1 <- predict(modelo_final, newdata = escenario_1)

# Aplicar el modelo SVM al Escenario 2
predicciones_escenario_2 <- predict(modelo_final, newdata = escenario_2)

# Aplicar el modelo SVM al Escenario 3
predicciones_escenario_3 <- predict(modelo_final, newdata = escenario_3)

# Agregar las predicciones a los escenarios para comparación
escenario_1$Prediccion_Rendimiento <- predicciones_escenario_1
escenario_2$Prediccion_Rendimiento <- predicciones_escenario_2
escenario_3$Prediccion_Rendimiento <- predicciones_escenario_3

#Comparación Visual: 
# Podemos realizar una comparación visual de los rendimientos predichos bajo cada escenario para visualizar el impacto del cambio climático.
# Crear un data frame para facilitar la comparación
resultados_escenarios <- rbind(
  data.frame(Provincia = escenario_1$Provincia, Escenario = "Escenario 1", Prediccion = predicciones_escenario_1),
  data.frame(Provincia = escenario_2$Provincia, Escenario = "Escenario 2", Prediccion = predicciones_escenario_2),
  data.frame(Provincia = escenario_3$Provincia, Escenario = "Escenario 3", Prediccion = predicciones_escenario_3)
)

# Gráfico de predicciones por escenario
ggplot(resultados_escenarios, aes(x = Provincia, y = Prediccion, fill = Escenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparación de Rendimiento de Cacao Bajo Diferentes Escenarios Climáticos",
       x = "Provincia", y = "Rendimiento Predicho (tm)")

#Evaluación del Impacto
#También puedes comparar el impacto promedio de cada escenario en todo el litoral:

# Calcular el promedio de las predicciones para cada escenario
promedio_escenario_1 <- mean(predicciones_escenario_1)
promedio_escenario_2 <- mean(predicciones_escenario_2)
promedio_escenario_3 <- mean(predicciones_escenario_3)

# Imprimir los resultados
print(paste("Promedio del Rendimiento en Escenario 1:", promedio_escenario_1))
print(paste("Promedio del Rendimiento en Escenario 2:", promedio_escenario_2))
print(paste("Promedio del Rendimiento en Escenario 3:", promedio_escenario_3))

# Cargar la librería ggplot2
library(ggplot2)

# Crear un data frame con los promedios de cada escenario
datos_escenarios <- data.frame(
  Escenario = c("Escenario 1", "Escenario 2", "Escenario 3"),
  Promedio_Rendimiento = c(promedio_escenario_1, promedio_escenario_2, promedio_escenario_3)
)

# Crear el gráfico de barras
ggplot(datos_escenarios, aes(x = Escenario, y = Promedio_Rendimiento, fill = Escenario)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  labs(title = "Comparación del Rendimiento Promedio en Diferentes Escenarios",
       x = "Escenario",
       y = "Rendimiento Promedio (tm/ha)") +
  theme_minimal()












