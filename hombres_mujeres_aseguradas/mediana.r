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
    
    # Filtrar asegurados (Seguro == 1) y datos válidos de salario
    data_filtered <- data %>% 
      filter(Seguro == 1) %>%
      filter(!is.na(Salario_bruto)) %>%  # Eliminar filas con Salario_bruto NA
      filter(Salario_bruto > quantile(Salario_bruto, 0.01) & Salario_bruto < quantile(Salario_bruto, 0.99))  # Filtrar outliers
    
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

# Calcular las medianas de los salarios brutos (Salario_bruto)
mediana_salario_hombres_asegurados <- median(hombres_asegurados$Salario_bruto, na.rm = TRUE)
mediana_salario_mujeres_aseguradas <- median(mujeres_aseguradas$Salario_bruto, na.rm = TRUE)

# Crear un data frame con las medianas para facilitar el gráfico
medianas <- data.frame(
  Grupo = c("Hombres Asegurados", "Mujeres Aseguradas"),
  Mediana_Salario = c(mediana_salario_hombres_asegurados, mediana_salario_mujeres_aseguradas)
)

# Crear el gráfico de barras con etiquetas de eje y en notación estándar
ggplot(medianas, aes(x = Grupo, y = Mediana_Salario, fill = Grupo)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Medianas de salarios de hombres asegurados y mujeres aseguradas",
       x = "Grupo",
       y = "Mediana") +
  theme_minimal() +
  scale_fill_manual(values = c("Hombres Asegurados" = "blue", "Mujeres Aseguradas" = "red")) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))  # Formato de etiquetas en notación estándar

# Guardar el gráfico en un archivo PNG
ggsave("comparacion_medianas_salarios_hombres_mujeres_asegurados.png", width = 8, height = 6)
