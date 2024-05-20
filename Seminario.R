install.packages("stargazer")
library(stargazer)
install.packages("readxl")
library(readxl)

datos <- read_excel("C:/Users/migue/Downloads/Base_Datos_Seminario.xlsx", sheet = "Modelo")


"Checar que si sean los datos"

head(datos)
str(datos)
summary(datos)



#Modelo por OLS__________________________________________________________________________________________________________________________________________________________
modelo <- lm(`temp_media` ~ `parque_vehicular` + `natalidad`, data=datos)
summary(modelo)
tabla_ols <- stargazer(modelo, type = "text", title = "Mínimos Cuadrados Ordinarios", align = TRUE)
print(tabla_ols)
# Obtener el número de observaciones y parámetros estimados
n_obs <- length(residuals(modelo))
n_params <- length(coefficients(modelo))
# Calcular el AIC y BIC manualmente Efectos Fijos
AIC_value <- n_obs * log(sum(residuals(modelo)^2) / n_obs) + 2 * n_params
BIC_value <- n_obs * log(sum(residuals(modelo)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value
BIC_value
#Estimación por FE, RE, FD y POLS, y sus respectivos Akaike y Schwartz________________________________________________________________________________________________________________
install.packages("plm")
library(plm)

# Estimar el modelo con Efectos Fijos -> model = "within" indica que será estimador de Efectos Fijos
modelo_fen <- plm(`temp_media` ~ `parque_vehicular` + `natalidad` - 1, 
                  data = datos,
                  model = "within")  # "within" indica Efectos Fijos
#Summary Efectos Fijos
summary(modelo_fen)
# Obtener el número de observaciones y parámetros estimados
n_obs <- length(residuals(modelo_fen))
n_params <- length(coefficients(modelo_fen))
# Calcular el AIC y BIC manualmente Efectos Fijos
AIC_value_fen <- n_obs * log(sum(residuals(modelo_fen)^2) / n_obs) + 2 * n_params
BIC_value_fen <- n_obs * log(sum(residuals(modelo_fen)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value_fen
BIC_value_fen


# Estimar el modelo con Efectos Aleatorios
modelo_re <- plm(`temp_media` ~ `parque_vehicular` + `natalidad`, data = datos,
                 model = "random")  # "random" indica Efectos Aleatorios
# Mostrar un resumen del modelo de Efectos Aleatorios
summary(modelo_re)
# Calcular el AIC y BIC manualmente Random Effect
AIC_value_re <- n_obs * log(sum(residuals(modelo_re)^2) / n_obs) + 2 * n_params
BIC_value_re <- n_obs * log(sum(residuals(modelo_re)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value_re
BIC_value_re


# Estimar el modelo con Diferencias Primeras (First Difference)
modelo_fd <- plm(`temp_media` ~ `parque_vehicular` + `natalidad`, 
                 data = datos,
                 model = "fd")  # "fd" indica First Difference
# Mostrar un resumen del modelo de Diferencias Primeras
summary(modelo_fd)
durbinWatsonTest(modelo_fd, max.lag = 4)
residuos_fd <- residuals(modelo_fd)
plot(residuos_fd, type = "l") #muestra un patrón en el tiempo, hay autocorrelación no whitenoise
# Calcular el AIC y BIC manualmente First Difference
AIC_value_fd <- n_obs * log(sum(residuals(modelo_fd)^2) / n_obs) + 2 * n_params
BIC_value_fd <- n_obs * log(sum(residuals(modelo_fd)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value_fd
BIC_value_fd

# Estimar el modelo con Pooled Ordinary Least Squares (POLS)
modelo_pols <- lm(`temp_media` ~ `parque_vehicular` + `natalidad`, 
                  data = datos)
# Mostrar un resumen del modelo de Pooled OLS
summary(modelo_pols)
# Calcular el AIC y BIC manualmente First Difference
AIC_value_pols <- n_obs * log(sum(residuals(modelo_fd)^2) / n_obs) + 2 * n_params
BIC_value_pols <- n_obs * log(sum(residuals(modelo_fd)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value_pols
BIC_value_pols

# Variance Inflator Factor______________________________________________________________________________________________________________________________________
install.packages("car")
library(car)

# Calcula el VIF para cada modelo
vif_pols <- vif(modelo_pols)
vif_fd <- vif(modelo_fd)
vif_fe <- vif(modelo_fen)
vif_re <- vif(modelo_re)

print(vif_fd)
print(vif_fe)
print(vif_pols)
print(vif_re)

#Prueba de Hausman
hausman_test <- phtest(modelo_fen, modelo_re)

print(hausman_test)

#MODELO EN LOGS_________________________________________________________________________________________________________________________________________
datos$log_temp <- log(datos$temp_media)
datos$log_coches <- log(datos$parque_vehicular)
datos$log_nat <- log(datos$natalidad)

modelo_logs <- lm(`log_temp` ~ `log_coches` + `log_nat`, data=datos)
summary(modelo_logs)

#Tabla Resultados
tabla_ols_logs <- stargazer(modelo_logs, type = "text", title = "Mínimos Cuadrados Ordinarios en Logaritmos", align = TRUE)
print(tabla_ols_logs)

# Calcular el AIC y BIC manualmente First Difference
AIC_value_logs <- n_obs * log(sum(residuals(modelo_logs)^2) / n_obs) + 2 * n_params
BIC_value_logs <- n_obs * log(sum(residuals(modelo_logs)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value_logs
BIC_value_logs

vif_logs <- vif(modelo_logs)
vif_logs

modelo_logs_1 <- lm(log10(temp_media) ~ log10(parque_vehicular) + log10(natalidad), data=datos)
summary(modelo_logs_1)
# Calcular el AIC y BIC manualmente First Difference
AIC_value_logs_1 <- n_obs * log(sum(residuals(modelo_logs_1)^2) / n_obs) + 2 * n_params
BIC_value_logs_1 <- n_obs * log(sum(residuals(modelo_logs_1)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value_logs_1
BIC_value_logs_1

##MODELO CON AUTOCORRELACIÓN_______________________________________________________________________________________________________________________
#Se necesita el paquete car para hacer Durbin-Watson
#El modelo posee autocorrelación positiva de primer orden, y el p_value es 0 así que es significativa.
install.packages("car")
library(car)
durbinWatsonTest(modelo_logs, max.lag = 4)

residuos_logs <- residuals(modelo_logs)
plot(residuos_logs, type = "l")

#Corriendo el test para 4 lags, vemos que sólo hay correlación significativa en lags 1 y 2.

#Incluir variables rezagadas
datos$Xt_lag1 <- lag(datos$log_coches, 1)
datos$Zt_lag1 <- lag(datos$log_nat, 1)
modelo_con_lags <- lm(log_temp ~ log_coches + log_nat + Xt_lag1 + Zt_lag1, data = datos)
summary(modelo_con_lags)

durbinWatsonTest(modelo_con_lags, max.lag = 4) #da los mismos resultados que el modelo_logs

residuos_lags <- residuals(modelo_con_lags)
plot(residuos_lags, type = "l") #muestra un patrón en el tiempo, hay autocorrelación no whitenoise

#Modelos autorregresivos (AR)
install.packages("nlme")
library(nlme)
modelo_ar <- gls(log_temp ~ log_coches + log_nat, correlation = corARMA(p = 1), data = datos)
summary(modelo_ar)

# Cargar el paquete stats
install.packages("stats")
library(stats)

# Calcular los residuos del modelo
residuos_ar <- residuals(modelo_ar)

# Realizar la prueba de Ljung-Box
Box.test(residuos_ar, lag = 4, type = "Ljung-Box") #sigue mostrando bastante autocorrelacion significativa

plot(residuos_ar, type = "l") #esta gráfica muestra que si hay un patrón autocorrelación no hay whitenoise.


#Modelos ARIMA
install.packages("forecast")
library(forecast)
modelo_arima <- auto.arima(datos$log_temp, xreg = cbind(datos$log_coches, datos$log_nat))
summary(modelo_arima)
#CALCULAR P-VALUE
# Definir los grados de libertad
grados_libertad <- 177

# Coeficientes y errores estándar para xreg1 y xreg2
coef_xreg1 <- 0.1331
se_xreg1 <- 0.0051
coef_xreg2 <- 0.0621
se_xreg2 <- 0.0073

# Calcular valor t y valor p para xreg1
t_value_xreg1 <- coef_xreg1 / se_xreg1
p_value_xreg1 <- 2 * pt(abs(t_value_xreg1), df = grados_libertad, lower.tail = FALSE)

# Calcular valor t y valor p para xreg2
t_value_xreg2 <- coef_xreg2 / se_xreg2
p_value_xreg2 <- 2 * pt(abs(t_value_xreg2), df = grados_libertad, lower.tail = FALSE)

# Mostrar los valores p
p_value_xreg1
p_value_xreg2

# Calcular los residuos del modelo ARIMA
residuos_arima <- residuals(modelo_arima)

# Gráfico de autocorrelación de los residuos
acf(residuos_arima, lag.max = 20)

# Test de Durbin-Watson
durbinWatsonTest(residuos_arima)

# Prueba de Ljung-Box
Box.test(residuos_arima, lag = 20, type = "Ljung-Box")

# Gráfico de residuos en función del tiempo
plot(residuos_arima, type = "l") #según esta gráfica si no hay patron entonces no hay evidencia de autocorrelación.(whitenoise)

#Interpretación de modelo arima, 

#Parque vehicular (xreg1): El coeficiente estimado para xreg1 es 0.1331, 
#con un error estándar de 0.0051. Esto indica que, manteniendo constante 
#la natalidad y otros factores, un aumento de una unidad en el parque vehicular 
#se asocia, en promedio, con un aumento de aproximadamente 0.1331 en la temperatura 
#media en México.
#Natalidad (xreg2): El coeficiente estimado para xreg2 es 0.0621, con un error estándar 
#de 0.0073. Esto indica que, manteniendo constante el parque vehicular y otros factores, 
#un aumento de una unidad en la natalidad se asocia, en promedio, con un aumento de 
#aproximadamente 0.0621 en la temperatura media en México. Ambos coeficientes son 
#significativos estadísticamente (debido a que sus valores p son menores a 0.05), 
#lo que sugiere que tanto el parque vehicular como la natalidad tienen un efecto 
#estadísticamente significativo en la temperatura media en México, según este modelo. 
#Sin embargo, es importante tener en cuenta que la interpretación causal de estos resultados 
#debe hacerse con precaución y considerando posibles factores omitidos y la complejidad del sistema climático.





















#GRÁFICAS_________________________________________________________________________________________________________________________________________
install.packages("ggplot2")
library(ggplot2)

#Gráfico de dispersión
scatter_plot <- function(data, title) {
  ggplot(data, aes(x = fertility_rate, y = female_employment)) +
    geom_point() +
    labs(x = "Fertilidad", y = "Participación Laboral Femenina", title = title) +
    theme_minimal()
}

#Aquí filtramos los datos para incluir solo las observaciones de cada país y hacer los gráficos de dispersión.
data_mexico <- subset(Base_de_Datos_1_, country_code == "MEX")
data_colombia <- subset(Base_de_Datos_1_, country_code == "COL")
data_chile <- subset(Base_de_Datos_1_, country_code == "CHL")
data_argentina <- subset(Base_de_Datos_1_, country_code == "ARG")
data_brasil <- subset(Base_de_Datos_1_, country_code == "BRA")
data_peru <- subset(Base_de_Datos_1_, country_code == "PER")
data_uruguay <- subset(Base_de_Datos_1_, country_code == "URY")
data_usa <- subset(Base_de_Datos_1_, country_code == "USA")


scatter_plot(data_mexico, "Fertilidad y Participación Laboral Femenina en México")
scatter_plot(data_colombia, "Fertilidad y Participación Laboral Femenina en Colombia")
scatter_plot(data_chile, "Fertilidad y Participación Laboral Femenina en Chile")
scatter_plot(data_argentina, "Fertilidad y Participación Laboral Femenina en Argentina")
scatter_plot(data_brasil, "Fertilidad y Participación Laboral Femenina en Brasil")
scatter_plot(data_peru, "Fertilidad y Participación Laboral Femenina en Perú")
scatter_plot(data_uruguay, "Fertilidad y Participación Laboral Femenina en Uruguay")
scatter_plot(data_usa, "Fertilidad y Participación Laboral Femenina en USA")

# Serie de tiempo
ggplot(data_mexico, aes(x = year, y = female_employment)) +
  geom_line() +
  labs(x = "Años", y = "Participación Laboral Femenina",
       title = "Serie de Tiempo de Participación Laboral de Mujeres en México (2005-2015)") +
  theme_minimal()


#Promedio__________________________
# Filtra los datos para incluir solo las observaciones de USA y para el período de 2005 a 2015
datos_usa <- subset(Base_de_Datos_1_, country_code == "USA" & year >= 2005 & year <= 2015)

# Calcula el promedio de la participación femenina
promedio_participacion <- mean(datos_usa$female_employment, na.rm = TRUE)

# Imprime el resultado
print(paste("El promedio de participación femenina en USA (2005-2015) es:", round(promedio_participacion, 2)))
