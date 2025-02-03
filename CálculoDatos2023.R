# ---------------------instalar y cargar paquetes----------------------------------------
install.packages("readxl")
library(readxl)

install.packages("ggplot2")
library(ggplot2)


# ---------------------datos I trimestre hombres----------------------------------------
# poner "Set working directory, en la parte de arriba"
datosTri1 <- read_xlsx("I Trimestre 2023.xlsx")

# salario hombres no asegurados 
salarioTri1Hom <- datosTri1$Salario_bruto[datosTri1$Sexo == 1 & datosTri1$Seguro == 2]

# quitar valores de que no respondieron
salarioTri1Hom <- na.omit(salarioTri1Hom)

# calcular el rango intercuartílico
cuantil1 <- quantile(salarioTri1Hom, 0.25)
cuantil3 <- quantile(salarioTri1Hom, 0.75)
RIQ <- cuantil3 - cuantil1

# son los límites de cada uno de los outliers ligeros
limiteInferior <- cuantil1 - 1.5 * RIQ
limiteSuperior <- cuantil3 + 1.5 * RIQ

# son los límites de cada uno de los outliers extremos
limiteInferiorEx <- cuantil1 - 3 * RIQ
limiteSuperiorEx <- cuantil3 + 3 * RIQ

# eliminar outliers ligeros
salariosTri1Hom <- salarioTri1Hom[salarioTri1Hom >= limiteInferior & salarioTri1Hom <= limiteSuperior]

# eliminar outliers extremos
salariosTrim1Hom <- salariosTri1Hom[salariosTri1Hom >= limiteInferiorEx & salariosTri1Hom <= limiteSuperiorEx]

salariosTrim1Hom <- na.omit(salariosTrim1Hom)

# datos como la media, mediana, primer y tercer cuartil
datosSalarioTri1Hom <- summary(salariosTrim1Hom)
print(datosSalarioTri1Hom)

# varianza y desviación estándar
varSalarioTri1Hom <- var(salariosTrim1Hom)
desvSalarioTri1Hom <- sqrt(varSalarioTri1Hom)

# ---------------------datos II trimestre hombres----------------------------------------
datosTri2 <- readxl::read_xlsx("II Trimestre 2023.xlsx")

# salario hombres no asegurados 
salarioTri2Hom <- datosTri2$Salario_bruto[datosTri2$Sexo == 1 & datosTri2$Seguro == 2]

# quitar valores de que no respondieron
salarioTri2Hom <- na.omit(salarioTri2Hom)

# calcular el rango intercuartílico
cuantil1 <- quantile(salarioTri2Hom, 0.25)
cuantil3 <- quantile(salarioTri2Hom, 0.75)
RIQ <- cuantil3 - cuantil1

# son los límites de cada uno de los outliers ligeros
limiteInferior <- cuantil1 - 1.5 * RIQ
limiteSuperior <- cuantil3 + 1.5 * RIQ

# son los límites de cada uno de los outliers extremos
limiteInferiorEx <- cuantil1 - 3 * RIQ
limiteSuperiorEx <- cuantil3 + 3 * RIQ

# eliminar outliers ligeros
salariosTri2Hom <- salarioTri2Hom[salarioTri2Hom >= limiteInferior & salarioTri2Hom <= limiteSuperior]

# eliminar outliers extremos
salariosTrim2Hom <- salariosTri2Hom[salariosTri2Hom >= limiteInferiorEx & salariosTri2Hom <= limiteSuperiorEx]

salariosTrim2Hom <- na.omit(salariosTrim2Hom)

# datos como la media, mediana, primer y tercer cuartil
datosSalarioTri2Hom <- summary(salariosTrim2Hom)
print(datosSalarioTri2Hom)

# varianza y desviación estándar
varSalarioTri2Hom <- var(salariosTrim2Hom)
desvSalarioTri2Hom <- sqrt(varSalarioTri2Hom)

# ---------------------datos III trimestre hombres----------------------------------------
datosTri3 <- readxl::read_xlsx("III Trimestre 2023.xlsx")

# salario hombres no asegurados 
salarioTri3Hom <- datosTri3$Salario_bruto[datosTri3$Sexo == 1 & datosTri3$Seguro == 2]

# quitar valores de que no respondieron
salarioTri3Hom <- na.omit(salarioTri3Hom)

# calcular el rango intercuartílico
cuantil1 <- quantile(salarioTri3Hom, 0.25)
cuantil3 <- quantile(salarioTri3Hom, 0.75)
RIQ <- cuantil3 - cuantil1

# son los límites de cada uno de los outliers ligeros
limiteInferior <- cuantil1 - 1.5 * RIQ
limiteSuperior <- cuantil3 + 1.5 * RIQ

# son los límites de cada uno de los outliers extremos
limiteInferiorEx <- cuantil1 - 3 * RIQ
limiteSuperiorEx <- cuantil3 + 3 * RIQ

# eliminar outliers ligeros
salariosTri3Hom <- salarioTri3Hom[salarioTri3Hom >= limiteInferior & salarioTri3Hom <= limiteSuperior]

# eliminar outliers extremos
salariosTrim3Hom <- salariosTri3Hom[salariosTri3Hom >= limiteInferiorEx & salariosTri3Hom <= limiteSuperiorEx]

salariosTrim3Hom <- na.omit(salariosTrim3Hom)

# datos como la media, mediana, primer y tercer cuartil
datosSalarioTri3Hom <- summary(salariosTrim3Hom)
print(datosSalarioTri3Hom)

# varianza y desviación estándar
varSalarioTri3Hom <- var(salariosTrim3Hom)
desvSalarioTri3Hom <- sqrt(varSalarioTri3Hom)

# ---------------------datos IV trimestre hombres----------------------------------------
datosTri4 <- readxl::read_xlsx("IV Trimestre 2023.xlsx")

# salario hombres no asegurados 
salarioTri4Hom <- datosTri4$Salario_bruto[datosTri4$Sexo == 1 & datosTri4$Seguro == 2]

# quitar valores de que no respondieron
salarioTri4Hom <- na.omit(salarioTri4Hom)

# calcular el rango intercuartílico
cuantil1 <- quantile(salarioTri4Hom, 0.25)
cuantil3 <- quantile(salarioTri4Hom, 0.75)
RIQ <- cuantil3 - cuantil1

# son los límites de cada uno de los outliers ligeros
limiteInferior <- cuantil1 - 1.5 * RIQ
limiteSuperior <- cuantil3 + 1.5 * RIQ

# son los límites de cada uno de los outliers extremos
limiteInferiorEx <- cuantil1 - 3 * RIQ
limiteSuperiorEx <- cuantil3 + 3 * RIQ

# eliminar outliers ligeros
salariosTri4Hom <- salarioTri4Hom[salarioTri4Hom >= limiteInferior & salarioTri4Hom <= limiteSuperior]

# eliminar outliers extremos
salariosTrim4Hom <- salariosTri4Hom[salariosTri4Hom >= limiteInferiorEx & salariosTri4Hom <= limiteSuperiorEx]

salariosTrim4Hom <- na.omit(salariosTrim4Hom)

# datos como la media, mediana, primer y tercer cuartil
datosSalarioTri4Hom <- summary(salariosTrim4Hom)
print(datosSalarioTri4Hom)

# varianza y desviación estándar
varSalarioTri4Hom <- var(salariosTrim4Hom)
desvSalarioTri4Hom <- sqrt(varSalarioTri4Hom)

# ---------------------datos I trimestre mujeres----------------------------------------

# salario mujeres no aseguradas 
salarioTri1Muj <- datosTri1$Salario_bruto[datosTri1$Sexo == 2 & datosTri1$Seguro == 2]

# quitar valores de que no respondieron
salarioTri1Muj <- na.omit(salarioTri1Muj)

# calcular el rango intercuartílico
cuantil1 <- quantile(salarioTri1Muj, 0.25)
cuantil3 <- quantile(salarioTri1Muj, 0.75)
RIQ <- cuantil3 - cuantil1

# son los límites de cada uno de los outliers ligeros
limiteInferior <- cuantil1 - 1.5 * RIQ
limiteSuperior <- cuantil3 + 1.5 * RIQ

# son los límites de cada uno de los outliers extremos
limiteInferiorEx <- cuantil1 - 3 * RIQ
limiteSuperiorEx <- cuantil3 + 3 * RIQ

# eliminar outliers ligeros
salariosTri1Muj <- salarioTri1Muj[salarioTri1Muj >= limiteInferior & salarioTri1Muj <= limiteSuperior]

# eliminar outliers extremos
salariosTrim1Muj <- salariosTri1Muj[salariosTri1Muj >= limiteInferiorEx & salariosTri1Muj <= limiteSuperiorEx]

salariosTrim1Muj <- na.omit(salariosTrim1Muj)

# datos como la media, mediana, primer y tercer cuartil
datosSalarioTri1Muj <- summary(salariosTrim1Muj)
print(datosSalarioTri1Muj)

# varianza y desviación estándar
varSalarioTri1Muj <- var(salariosTrim1Muj)
desvSalarioTri1Muj <- sqrt(varSalarioTri1Muj)

# ---------------------datos II trimestre mujeres----------------------------------------

# salario mujeres no aseguradas 
salarioTri2Muj <- datosTri2$Salario_bruto[datosTri2$Sexo == 2 & datosTri2$Seguro == 2]

# quitar valores de que no respondieron
salarioTri2Muj <- na.omit(salarioTri2Muj)

# calcular el rango intercuartílico
cuantil1 <- quantile(salarioTri2Muj, 0.25)
cuantil3 <- quantile(salarioTri2Muj, 0.75)
RIQ <- cuantil3 - cuantil1

# son los límites de cada uno de los outliers ligeros
limiteInferior <- cuantil1 - 1.5 * RIQ
limiteSuperior <- cuantil3 + 1.5 * RIQ

# son los límites de cada uno de los outliers extremos
limiteInferiorEx <- cuantil1 - 3 * RIQ
limiteSuperiorEx <- cuantil3 + 3 * RIQ

# eliminar outliers ligeros
salariosTri2Muj <- salarioTri2Muj[salarioTri2Muj >= limiteInferior & salarioTri2Muj <= limiteSuperior]

# eliminar outliers extremos
salariosTrim2Muj <- salariosTri2Muj[salariosTri2Muj >= limiteInferiorEx & salariosTri2Muj <= limiteSuperiorEx]

salariosTrim2Muj <- na.omit(salariosTrim2Muj)

# datos como la media, mediana, primer y tercer cuartil
datosSalarioTri2Muj <- summary(salariosTrim2Muj)
print(datosSalarioTri2Muj)

# varianza y desviación estándar
varSalarioTri2Muj <- var(salariosTrim2Muj)
desvSalarioTri2Muj <- sqrt(varSalarioTri2Muj)

# ---------------------datos III trimestre mujeres----------------------------------------

# salario mujeres no aseguradas 
salarioTri3Muj <- datosTri3$Salario_bruto[datosTri3$Sexo == 2 & datosTri3$Seguro == 2]

# quitar valores de que no respondieron
salarioTri3Muj <- na.omit(salarioTri3Muj)

# calcular el rango intercuartílico
cuantil1 <- quantile(salarioTri3Muj, 0.25)
cuantil3 <- quantile(salarioTri3Muj, 0.75)
RIQ <- cuantil3 - cuantil1

# son los límites de cada uno de los outliers ligeros
limiteInferior <- cuantil1 - 1.5 * RIQ
limiteSuperior <- cuantil3 + 1.5 * RIQ

# son los límites de cada uno de los outliers extremos
limiteInferiorEx <- cuantil1 - 3 * RIQ
limiteSuperiorEx <- cuantil3 + 3 * RIQ

# eliminar outliers ligeros
salariosTri3Muj <- salarioTri3Muj[salarioTri3Muj >= limiteInferior & salarioTri3Muj <= limiteSuperior]

# eliminar outliers extremos
salariosTrim3Muj <- salariosTri3Muj[salariosTri3Muj >= limiteInferiorEx & salariosTri3Muj <= limiteSuperiorEx]

salariosTrim3Muj <- na.omit(salariosTrim3Muj)

# datos como la media, mediana, primer y tercer cuartil
datosSalarioTri3Muj <- summary(salariosTrim3Muj)
print(datosSalarioTri3Muj)

# varianza y desviación estándar
varSalarioTri3Muj <- var(salariosTrim3Muj)
desvSalarioTri3Muj <- sqrt(varSalarioTri3Muj)

# ---------------------datos IV trimestre mujeres----------------------------------------

# salario mujeres no aseguradas 
salarioTri4Muj <- datosTri4$Salario_bruto[datosTri4$Sexo == 2 & datosTri4$Seguro == 2]

# quitar valores de que no respondieron
salarioTri4Muj <- na.omit(salarioTri4Muj)

# calcular el rango intercuartílico
cuantil1 <- quantile(salarioTri4Muj, 0.25)
cuantil3 <- quantile(salarioTri4Muj, 0.75)
RIQ <- cuantil3 - cuantil1

# son los límites de cada uno de los outliers ligeros
limiteInferior <- cuantil1 - 1.5 * RIQ
limiteSuperior <- cuantil3 + 1.5 * RIQ

# son los límites de cada uno de los outliers extremos
limiteInferiorEx <- cuantil1 - 3 * RIQ
limiteSuperiorEx <- cuantil3 + 3 * RIQ

# eliminar outliers ligeros
salariosTri4Muj <- salarioTri4Muj[salarioTri4Muj >= limiteInferior & salarioTri4Muj <= limiteSuperior]

# eliminar outliers extremos
salariosTrim4Muj <- salariosTri4Muj[salariosTri4Muj >= limiteInferiorEx & salariosTri4Muj <= limiteSuperiorEx]

salariosTrim4Muj <- na.omit(salariosTrim4Muj)

# datos como la media, mediana, primer y tercer cuartil
datosSalarioTri4Muj <- summary(salariosTrim4Muj)
print(datosSalarioTri4Muj)

# varianza y desviación estándar
varSalarioTri4Muj <- var(salariosTrim4Muj)
desvSalarioTri4Muj <- sqrt(varSalarioTri4Muj)

# ---------------------Sumar las medias de todos los trimestres----------------------------------------

# media de los salarios de los hombres no asegurados
sumatoriaMediaHom <- datosSalarioTri1Hom["Mean"] + datosSalarioTri2Hom["Mean"] + datosSalarioTri3Hom["Mean"] + datosSalarioTri4Hom["Mean"] 
mediaHom <- sumatoriaMediaHom / 4

# media de los salarios de las mujeres no aseguradas
sumatoriaMediaMuj <- datosSalarioTri1Muj["Mean"] + datosSalarioTri2Muj["Mean"] + datosSalarioTri3Muj["Mean"] + datosSalarioTri4Muj["Mean"] 
mediaMuj <- sumatoriaMediaMuj / 4

# graficando ambas medias
poblaciones <- c("Hombres no asegurados", "Mujeres no aseguradas")
medias <- c(mediaHom, mediaMuj) 
datos <- data.frame(Población = poblaciones, Media = medias)

ggplot(datos, aes(x = Población, y = Media, fill = Población)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Medias de salarios de hombres y mujeres no aseguradas",
       x = "Población",
       y = "Media") +
  scale_y_continuous(breaks = seq(0, max(datos$Media) + 100000, by = 100000), 
                     labels = scales::comma) +  
  scale_fill_manual(values = c("Hombres no asegurados" = "green", "Mujeres no aseguradas" = "orange")) +  
  theme_minimal()

# ---------------------Sumar las medianas de todos los trimestres----------------------------------------

# mediana de los salarios de los hombres no asegurados
sumatoriaMedianaHom <- datosSalarioTri1Hom["Median"] + datosSalarioTri2Hom["Median"] + datosSalarioTri3Hom["Median"] + datosSalarioTri4Hom["Median"] 
medianaHom <- sumatoriaMedianaHom / 4

# mediana de los salarios de las mujeres no aseguradas
sumatoriaMedianaMuj <- datosSalarioTri1Muj["Median"] + datosSalarioTri2Muj["Median"] + datosSalarioTri3Muj["Median"] + datosSalarioTri4Muj["Median"] 
medianaMuj <- sumatoriaMedianaMuj / 4

# graficando ambas medianas
poblaciones <- c("Hombres no asegurados", "Mujeres no aseguradas")
medianas <- c(medianaHom, medianaMuj) 
datos <- data.frame(Población = poblaciones, Mediana = medianas)

ggplot(datos, aes(x = Población, y = Mediana, fill = Población)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Medianas de salarios de hombres y mujeres no aseguradas",
       x = "Población",
       y = "Mediana") +
  scale_y_continuous(breaks = seq(0, max(datos$Mediana) + 100000, by = 100000), 
                     labels = scales::comma) +  
  scale_fill_manual(values = c("Hombres no asegurados" = "yellow", "Mujeres no aseguradas" = "cyan")) +  
  theme_minimal()

# ---------------------Sumar las varianzas de todos los trimestres----------------------------------------

# varianza de los salarios de los hombres no asegurados
sumatoriaVarHom <- varSalarioTri1Hom + varSalarioTri2Hom + varSalarioTri3Hom + varSalarioTri4Hom 
varHom <- sumatoriaVarHom / 4

# varianza de los salarios de las mujeres no aseguradas
sumatoriaVarMuj <- varSalarioTri1Muj + varSalarioTri2Muj + varSalarioTri3Muj + varSalarioTri4Muj
varMuj <- sumatoriaVarMuj / 4

# graficando ambas varianzas
poblaciones <- c("Hombres no asegurados", "Mujeres no aseguradas")
varianzas <- c(varHom, varMuj) 
datos <- data.frame(Población = poblaciones, Varianza = varianzas)

# Normalizar las varianzas dividiéndolas por 1e9
datos$Varianza_normalizada <- datos$Varianza / 1e9

ggplot(datos, aes(x = Población, y = Varianza_normalizada, fill = Población)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Varianzas de salarios de hombres y mujeres no aseguradas",
       x = "Población",
       y = "Varianza (en miles de millones)") +
  scale_y_continuous(labels = scales::comma) +  
  scale_fill_manual(values = c("Hombres no asegurados" = "pink", "Mujeres no aseguradas" = "purple")) +  
  theme_minimal()

# ---------------------Cuartiles con boxplots----------------------------------------

cuartiles1Hom <- quantile(salariosTrim1Hom, probs = c(0.25, 0.5, 0.75))
cuartiles1Muj <- quantile(salariosTrim1Muj, probs = c(0.25, 0.5, 0.75))
cuartiles2Hom <- quantile(salariosTrim2Hom, probs = c(0.25, 0.5, 0.75))
cuartiles2Muj <- quantile(salariosTrim2Muj, probs = c(0.25, 0.5, 0.75))
cuartiles3Hom <- quantile(salariosTrim3Hom, probs = c(0.25, 0.5, 0.75))
cuartiles3Muj <- quantile(salariosTrim3Muj, probs = c(0.25, 0.5, 0.75))
cuartiles4Hom <- quantile(salariosTrim4Hom, probs = c(0.25, 0.5, 0.75))
cuartiles4Muj <- quantile(salariosTrim4Muj, probs = c(0.25, 0.5, 0.75))

datos1Hom <- data.frame(Valores = salariosTrim1Hom, Sexo = "Hombres no asegurados")
datos1Hom$Cuartil <- cut(datos1Hom$Valores, 
                          breaks = c(-Inf, cuartiles1Hom, Inf), 
                          labels = c("Q1", "Q2", "Q3", "Q4"),
                          include.lowest = TRUE)

datos1Muj <- data.frame(Valores = salariosTrim1Muj, Sexo = "Mujeres no aseguradas")
datos1Muj$Cuartil <- cut(datos1Muj$Valores, 
                          breaks = c(-Inf, cuartiles1Muj, Inf), 
                          labels = c("Q1", "Q2", "Q3", "Q4"),
                          include.lowest = TRUE)

datos2Hom <- data.frame(Valores = salariosTrim2Hom, Sexo = "Hombres no asegurados")
datos2Hom$Cuartil <- cut(datos2Hom$Valores, 
                          breaks = c(-Inf, cuartiles2Hom, Inf), 
                          labels = c("Q1", "Q2", "Q3", "Q4"),
                          include.lowest = TRUE)

datos2Muj <- data.frame(Valores = salariosTrim2Muj, Sexo = "Mujeres no aseguradas")
datos2Muj$Cuartil <- cut(datos2Muj$Valores, 
                          breaks = c(-Inf, cuartiles2Muj, Inf), 
                          labels = c("Q1", "Q2", "Q3", "Q4"),
                          include.lowest = TRUE)

  datos3Hom <- data.frame(Valores = salariosTrim3Hom, Sexo = "Hombres no asegurados")
datos3Hom$Cuartil <- cut(datos3Hom$Valores, 
                          breaks = c(-Inf, cuartiles3Hom, Inf), 
                          labels = c("Q1", "Q2", "Q3", "Q4"),
                          include.lowest = TRUE)

datos3Muj <- data.frame(Valores = salariosTrim3Muj, Sexo = "Mujeres no aseguradas")
datos3Muj$Cuartil <- cut(datos3Muj$Valores, 
                          breaks = c(-Inf, cuartiles3Muj, Inf), 
                          labels = c("Q1", "Q2", "Q3", "Q4"),
                          include.lowest = TRUE)

datos4Hom <- data.frame(Valores = salariosTrim4Hom, Sexo = "Hombres no asegurados")
datos4Hom$Cuartil <- cut(datos4Hom$Valores, 
                          breaks = c(-Inf, cuartiles4Hom, Inf), 
                          labels = c("Q1", "Q2", "Q3", "Q4"),
                          include.lowest = TRUE)

datos4Muj <- data.frame(Valores = salariosTrim4Muj, Sexo = "Mujeres no aseguradas")
datos4Muj$Cuartil <- cut(datos4Muj$Valores, 
                          breaks = c(-Inf, cuartiles4Muj, Inf), 
                          labels = c("Q1", "Q2", "Q3", "Q4"),
                          include.lowest = TRUE)


datos_df <- rbind(datos1Hom, datos1Muj, datos2Hom, datos2Muj, datos3Hom, datos3Muj, datos4Hom, datos4Muj)


ggplot(datos_df, aes(x = Cuartil, y = Valores, fill = Sexo)) +
  geom_boxplot() +
  labs(title = "Comparación de Salarios por Cuartil",
       x = "Cuartil",
       y = "Salarios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------------Estadística Inferencial------------------------------
install.packages("nortest")
library(nortest)

install.packages("car")
library(car)

install.packages("effsize")
library(effsize)


#combinar datos
salariosHom <- c(salariosTrim1Hom, salariosTrim2Hom, salariosTrim3Hom, salariosTrim4Hom)
salariosMuj <- c(salariosTrim1Muj, salariosTrim2Muj, salariosTrim3Muj, salariosTrim4Muj)

# determinar normalidad en los datos 
# hombres
lillie.test(salariosHom)

qqnorm(salariosHom, main="Gráfico Q-Q de Salarios hombres 2023")
qqline(salariosHom, col="red")


qqPlot(salariosHom, 
       main = "Gráfico Q-Q de Salarios hombres 2023",
       ylab = "Salarios",
       xlab = "Cuantiles")


# mujeres
lillie.test(salariosMuj)

qqnorm(salariosMuj, main="Gráfico Q-Q de Salarios mujeres 2023")
qqline(salariosMuj, col="red")
qqPlot(salariosMuj, 
       main = "Gráfico Q-Q de Salarios mujeres 2023",
       ylab = "Salarios",
       xlab = "Cuantiles")

