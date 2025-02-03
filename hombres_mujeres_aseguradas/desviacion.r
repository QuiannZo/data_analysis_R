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
    data_filtered <- data %>% 
      filter(Seguro == 1) %>%
      filter(!is.na(Salario_bruto))  # Eliminar filas con Salario_bruto NA
    
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

# Filtrar valores atípicos en Salario_bruto usando el rango intercuartílico (IQR)
filter_outliers <- function(data) {
  Q1 <- quantile(data$Salario_bruto, 0.25, na.rm = TRUE)
  Q3 <- quantile(data$Salario_bruto, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data %>% filter(Salario_bruto >= lower_bound & Salario_bruto <= upper_bound)
}

hombres_asegurados <- filter_outliers(hombres_asegurados)
mujeres_aseguradas <- filter_outliers(mujeres_aseguradas)

# Crear un data frame con los datos combinados
df <- rbind(mutate(hombres_asegurados, Grupo = "Hombres Asegurados"),
            mutate(mujeres_aseguradas, Grupo = "Mujeres Aseguradas"))

# Convertir a factor para orden correcto en el gráfico
df$Grupo <- factor(df$Grupo, levels = c("Hombres Asegurados", "Mujeres Aseguradas"))

# Crear el gráfico tipo boxplot
ggplot(df, aes(x = Grupo, y = Salario_bruto, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Distribución de Salarios Brutos por Grupo",
       x = "Grupo",
       y = "Salario Bruto") +
  theme_minimal() +
  scale_fill_manual(values = c("Hombres Asegurados" = "blue", "Mujeres Aseguradas" = "red"))

# Guardar el gráfico en un archivo PNG
ggsave("boxplot_salarios_hombres_mujeres.png", width = 10, height = 8)
