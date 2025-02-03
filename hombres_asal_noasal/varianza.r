library(dplyr)
library(ggplot2)

# Cargar archivos
files <- c("2023/1.csv", "2023/2.csv", "2023/3.csv", "2023/4.csv")

#Procesar archivos
process_file <- function(file) {
  if (!file.exists(file)) {
    return(NULL)
  }
  
  #Cargar csv
  tryCatch({
    data <- read.csv(file, sep = ";", header = TRUE, stringsAsFactors = FALSE, fill = TRUE, na.strings = "")
    
    # Verificar que las columnas necesarias existan
    required_columns <- c("Sexo", "Seguro", "Salario_bruto")
    missing_columns <- setdiff(required_columns, names(data))
    
    if (length(missing_columns) > 0) {
      return(NULL)
    }
    
    # Filtrar hombres
    data_filtered <- data %>% filter(Sexo == 1)
    
    # Filtrar valores atípicos en Salario_bruto usando desviacion estandar
    mean_salario <- mean(data_filtered$Salario_bruto, na.rm = TRUE)
    sd_salario <- sd(data_filtered$Salario_bruto, na.rm = TRUE)
    threshold <- 3 

    data_filtered <- data_filtered %>%
      filter(abs(Salario_bruto - mean_salario) < threshold * sd_salario)
    
    return(data_filtered)
  }, error = function(e) {
    return(NULL)
  })
}

# Leer y procesar todos los archivos
data_list <- lapply(files, process_file)

# Eliminar elementos NULL de la lista
data_list <- Filter(Negate(is.null), data_list)

# Verificar que hay datos en la lista
if (length(data_list) == 0) {
  stop("No se pudieron cargar los datos.")
}

# Unir todos los data frames en uno solo
combined_data <- bind_rows(data_list)

# Verificar que las columnas necesarias existan
required_columns <- c("Sexo", "Seguro", "Salario_bruto")
missing_columns <- setdiff(required_columns, names(combined_data))

if (length(missing_columns) > 0) {
  stop("Faltan columnas en los datos leidos")
}

# Filtrar asegurados y no asegurados
hombres_asegurados <- combined_data %>% filter(Seguro == 1)
hombres_no_asegurados <- combined_data %>% filter(Seguro == 2)

# Calcular las varianzas de los salarios brutos (Salario_bruto)
varianza_salario_asegurados <- var(hombres_asegurados$Salario_bruto, na.rm = TRUE)
varianza_salario_no_asegurados <- var(hombres_no_asegurados$Salario_bruto, na.rm = TRUE)

# Crear un data frame con las varianzas
varianzas <- data.frame(
  Grupo = c("Asegurados", "No Asegurados"),
  Varianza_Salario = c(varianza_salario_asegurados, varianza_salario_no_asegurados)
)

# Crear el grafico
ggplot(varianzas, aes(x = Grupo, y = Varianza_Salario, fill = Grupo)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Varianzas de salarios de hombres asegurados y hombres no asegurados",
       x = "Grupo",
       y = "Varianza") +
  theme_minimal() +
  scale_fill_manual(values = c("Asegurados" = "blue", "No Asegurados" = "red")) +
  theme(legend.position = "none")

# Guardar el grafico en un png
ggsave("comparacion_varianzas_salarios.png", width = 8, height = 6)
