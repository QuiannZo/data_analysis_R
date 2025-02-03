# Cargar librerías necesarias
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("nortest")) install.packages("nortest")
if (!require("car")) {
  install.packages("nloptr")
  install.packages("RcppEigen")
  install.packages("lme4")
  install.packages("pbkrtest")
  install.packages("quantreg")
  install.packages("car")
}
if (!require("effsize")) install.packages("effsize")

library(nortest)
library(car)
library(effsize)
library(readxl)
library(dplyr)
library(ggplot2)

# Archivos
files <- c("I Trimestre 2023.xlsx", "II Trimestre 2023.xlsx", "III Trimestre 2023.xlsx", "IV Trimestre 2023.xlsx")

# Leer y combinar los datos de los cuatro archivos
all_data <- lapply(files, read_excel)
combined_data <- bind_rows(all_data)

# Filtrar solo hombres (Sexo == 1) y valores de ingreso bruto en el rango seleccionado
filtered_data_hom <- combined_data %>%
  filter(Sexo == 1, 
         !is.na(Ingreso_bruto_total_trabajo), Ingreso_bruto_total_trabajo >= 50000, 
         Ingreso_bruto_total_trabajo <= 10000000)

# Separar hombres asegurados (Seguro == 1) y no asegurados (Seguro == 2)
asegurados_hom <- filtered_data_hom %>%
  filter(Seguro == 1)

no_asegurados_hom <- filtered_data_hom %>%
  filter(Seguro == 2)

# Estadísticas para asegurados
media_asegurados_hom <- mean(asegurados_hom$Ingreso_bruto_total_trabajo, na.rm = TRUE)
print(media_asegurados_hom)
mediana_asegurados_hom <- median(asegurados_hom$Ingreso_bruto_total_trabajo, na.rm = TRUE)
print(mediana_asegurados_hom)
varianza_asegurados_hom <- var(asegurados_hom$Ingreso_bruto_total_trabajo, na.rm = TRUE)
print(varianza_asegurados_hom)

# Estadísticas para no asegurados
media_no_asegurados_hom <- mean(no_asegurados_hom$Ingreso_bruto_total_trabajo, na.rm = TRUE)
print(media_no_asegurados_hom)
mediana_no_asegurados_hom <- median(no_asegurados_hom$Ingreso_bruto_total_trabajo, na.rm = TRUE)
print(mediana_no_asegurados_hom)
varianza_no_asegurados_hom <- var(no_asegurados_hom$Ingreso_bruto_total_trabajo, na.rm = TRUE)
print(varianza_no_asegurados_hom)

# Dataframe para las estadísticas
estadisticas_hom <- data.frame(
  Grupo = c("Asegurados", "No asegurados"),
  Media = c(media_asegurados_hom, media_no_asegurados_hom),
  Mediana = c(mediana_asegurados_hom, mediana_no_asegurados_hom),
  Varianza = c(varianza_asegurados_hom, varianza_no_asegurados_hom)
)

# Gráfico de cajas y bigotes para los ingresos brutos
ggplot(filtered_data_hom, aes(x = factor(Seguro), y = Ingreso_bruto_total_trabajo, fill = factor(Seguro))) +
  geom_boxplot() +
  labs(title = "Distribución de Ingresos Brutos para Hombres",
       x = "Grupo de Seguro", y = "Ingreso Bruto Total") +
  scale_fill_manual(values = c("1" = "beige", "2" = "grey")) +
  theme_minimal()

#---------------Estadística Inferencial--------------------

# Determinar normalidad en los datos, con prueba de normalidad de Lilliefors

# Asegurados
salariosAsegurados_hom <- asegurados_hom$Ingreso_bruto_total_trabajo
lillie.test(salariosAsegurados_hom)

qqnorm(salariosAsegurados_hom, main="Gráfico Q-Q de Salarios asegurados 2023")
qqline(salariosAsegurados_hom, col="red")

qqPlot(salariosAsegurados_hom, 
       main = "Gráfico Q-Q de Salarios asegurados 2023",
       ylab = "Salarios",
       xlab = "Cuantiles")

# No asegurados
salariosNoAsegurados_hom <- no_asegurados_hom$Ingreso_bruto_total_trabajo
lillie.test(salariosNoAsegurados_hom)

qqnorm(salariosNoAsegurados_hom, main="Gráfico Q-Q de Salarios no asegurados 2023")
qqline(salariosNoAsegurados_hom, col="red")
qqPlot(salariosNoAsegurados_hom, 
       main = "Gráfico Q-Q de Salarios no asegurados 2023",
       ylab = "Salarios",
       xlab = "Cuantiles")

# Los datos no son normales
# Calcular el estadístico de Mann-Whitney para cada muestra

# Se combinan las muestras para sacar la suma de los rangos
datosCombinados_hom <- c(salariosAsegurados_hom, salariosNoAsegurados_hom)
tamanoAsegurados_hom <- length(salariosAsegurados_hom)
tamanoNoAsegurados_hom <- length(salariosNoAsegurados_hom)

rangos_hom <- rank(datosCombinados_hom)

rangosVec1_hom <- rangos_hom[1:tamanoAsegurados_hom]
rangosVec2_hom <- rangos_hom[(tamanoAsegurados_hom+1):length(rangos_hom)]

sumaRangosVec1_hom <- sum(rangosVec1_hom)
sumaRangosVec2_hom <- sum(rangosVec2_hom)

# Se calculan los estadísticos
U1_hom <- tamanoAsegurados_hom * tamanoNoAsegurados_hom + ((tamanoAsegurados_hom * (tamanoAsegurados_hom + 1)))/2 - sumaRangosVec1_hom
U2_hom <- tamanoAsegurados_hom * tamanoNoAsegurados_hom + ((tamanoNoAsegurados_hom * (tamanoNoAsegurados_hom + 1)))/2 - sumaRangosVec2_hom

# Calcular media y desviación estándar
media_hom <- (tamanoAsegurados_hom * tamanoNoAsegurados_hom)/2

desv_hom <- sqrt((tamanoAsegurados_hom * tamanoNoAsegurados_hom * (tamanoAsegurados_hom + tamanoNoAsegurados_hom + 1))/12)

# Valor de Z para normalizar la prueba de Mann-Whitney
Z_hom <- (U1_hom - media_hom)/desv_hom
print(Z_hom)

# Nivel de significancia
alfa <- 0.05

# Calcular los valores críticos para hacer la prueba de dos colas
valorCriticoInf_hom <- qnorm(alfa/2)
valorCriticoSup_hom <- qnorm(1-alfa/2)

# Imprimiendo los resultados
print(paste("Valor crítico inferior:", round(valorCriticoInf_hom, 4)))
print(paste("Valor crítico superior:", round(valorCriticoSup_hom, 4)))

print(paste("Valor de Z:", round(Z_hom, 4), "se sale de valores críticos -1.96 y 1.96, se rechaza hipótesis nula (las medias son iguales)"))
