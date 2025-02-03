library(dplyr)
library(ggplot2)

# Cargar archivos
files <- c("2023/1.csv", "2023/2.csv", "2023/3.csv", "2023/4.csv")

# Función para procesar y limpiar cada archivo
process_file <- function(file) {
  if (!file.exists(file)) {
    return(NULL)
  }
  # Leer csv
  tryCatch({
    data <- read.csv(file, sep = ";", header = TRUE, stringsAsFactors = FALSE, fill = TRUE, na.strings = "")
    
    # Verificar columnas 
    required_columns <- c("Sexo", "Seguro", "Salario_bruto")
    missing_columns <- setdiff(required_columns, names(data))
    
    # Faltan columnas, finalizar
    if (length(missing_columns) > 0) {
      return(NULL)
    }
    
    # Filtrar hombres (Sexo = 1)
    data_filtered <- data %>% filter(Sexo == 1)
    
    # Filtrar valores atipicos en Salario_bruto
    q1 <- quantile(data_filtered$Salario_bruto, 0.25, na.rm = TRUE)
    q3 <- quantile(data_filtered$Salario_bruto, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    data_filtered <- data_filtered %>%
      filter(Salario_bruto >= lower_bound & Salario_bruto <= upper_bound)
    
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
  stop("No se pudieron cargar los datos de ningún archivo.")
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

# Calcular las medias de los salarios brutos
media_salario_asegurados <- mean(hombres_asegurados$Salario_bruto, na.rm = TRUE)
media_salario_no_asegurados <- mean(hombres_no_asegurados$Salario_bruto, na.rm = TRUE)

# Crear un data frame con las medias
medias <- data.frame(
  Grupo = c("Asegurados", "No Asegurados"),
  Media_Salario = c(media_salario_asegurados, media_salario_no_asegurados)
)

# Crear el grafico
ggplot(medias, aes(x = Grupo, y = Media_Salario, fill = Grupo)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Medias de salarios de hombres asegurados y hombres no asegurados",
       x = "Grupo",
       y = "Media") +
  theme_minimal() +
  scale_fill_manual(values = c("Asegurados" = "blue", "No Asegurados" = "red")) +
  theme(legend.position = "none")

# Guardar el grafico
ggsave("medias.png", width = 8, height = 6)
