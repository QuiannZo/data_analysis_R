# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# Lista de archivos
files <- c("I Trimestre 2023.csv", "II Trimestre 2023.csv", "III Trimestre 2023.csv", "IV Trimestre 2023.csv")

# Función para procesar y limpiar cada archivo
process_file <- function(file) {
  if (!file.exists(file)) {
    cat("El archivo no existe:", file, "\n")
    return(NULL)
  }
  
  # Intentar leer el archivo CSV
  tryCatch({
    data <- read.csv(file, sep = ";", header = TRUE, stringsAsFactors = FALSE, fill = TRUE, na.strings = "")
    
    # Verificar que las columnas necesarias existan
    required_columns <- c("Sexo", "Seguro", "Salario_bruto")
    missing_columns <- setdiff(required_columns, names(data))
    
    if (length(missing_columns) > 0) {
      cat("Faltan las siguientes columnas en el archivo", file, ": ", paste(missing_columns, collapse = ", "), "\n")
      return(NULL)
    }
    
    # Filtrar asegurados (Seguro == 1)
    data_filtered <- data %>% filter(Seguro == 1)
    
    # Filtrar valores atípicos en Salario_bruto usando desviación estándar
    mean_salario <- mean(data_filtered$Salario_bruto, na.rm = TRUE)
    sd_salario <- sd(data_filtered$Salario_bruto, na.rm = TRUE)
    threshold <- 3  # Puedes ajustar este umbral según tus criterios
    
    data_filtered <- data_filtered %>%
      filter(abs(Salario_bruto - mean_salario) < threshold * sd_salario)
    
    return(data_filtered)
  }, error = function(e) {
    cat("Error al leer el archivo:", file, "\n", e$message, "\n")
    return(NULL)
  })
}

# Leer y procesar todos los archivos
data_list <- lapply(files, process_file)

# Eliminar elementos NULL de la lista
data_list <- Filter(Negate(is.null), data_list)

# Verificar que hay datos en la lista
if (length(data_list) == 0) {
  stop("No se pudieron cargar los datos de ningún archivo.")
}

# Unir todos los data frames en uno solo
combined_data <- bind_rows(data_list)

# Verificar que las columnas necesarias existan
required_columns <- c("Sexo", "Seguro", "Salario_bruto")
missing_columns <- setdiff(required_columns, names(combined_data))

if (length(missing_columns) > 0) {
  stop("Faltan las siguientes columnas necesarias en los datos combinados: ", paste(missing_columns, collapse = ", "))
}

# Filtrar hombres y mujeres asegurados
hombres_asegurados <- combined_data %>% filter(Sexo == 2, Seguro == 1)
mujeres_aseguradas <- combined_data %>% filter(Sexo == 1, Seguro == 1)

# Calcular las varianzas de los salarios brutos (Salario_bruto)
varianza_salario_hombres_asegurados <- var(hombres_asegurados$Salario_bruto, na.rm = TRUE)
varianza_salario_mujeres_aseguradas <- var(mujeres_aseguradas$Salario_bruto, na.rm = TRUE)

# Crear un data frame con las varianzas para facilitar el gráfico
varianzas <- data.frame(
  Grupo = c("Hombres Asegurados", "Mujeres Aseguradas"),
  Varianza_Salario = c(varianza_salario_hombres_asegurados, varianza_salario_mujeres_aseguradas)
)

# Crear el gráfico de barras
ggplot(varianzas, aes(x = Grupo, y = Varianza_Salario, fill = Grupo)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Varianzas de salarios de hombres asegurados y mujeres aseguradas",
       x = "Grupo",
       y = "Varianza") +
  theme_minimal() +
  scale_fill_manual(values = c("Hombres Asegurados" = "blue", "Mujeres Aseguradas" = "red")) +
  theme(legend.position = "none")

# Guardar el gráfico en un archivo PNG
ggsave("comparacion_varianzas_salarios.png", width = 8, height = 6)
