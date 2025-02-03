library(dplyr)
library(ggplot2)

# Cargar archivos (se cambiaron los nombres para facilidad, uno por trimestre)
files <- c("2023/1.csv", "2023/2.csv", "2023/3.csv", "2023/4.csv")

process_file <- function(file) {
  if (!file.exists(file)) {
    cat("El archivo no existe:", file, "\n")
    return(NULL)
  }
  
  # Leer CSV
  tryCatch({
    data <- read.csv(file, sep = ";", header = TRUE, stringsAsFactors = FALSE, fill = TRUE, na.strings = "")
    
    # Verificar columnas
    required_columns <- c("Sexo", "Seguro", "Salario_bruto")
    missing_columns <- setdiff(required_columns, names(data))
    
    if (length(missing_columns) > 0) {
      cat("Faltan las siguientes columnas en el archivo", file, ": ", paste(missing_columns, collapse = ", "), "\n")
      return(NULL)
    }
    
    # Filtrar hombres (Sexo == 1)
    data_filtered <- data %>% 
      filter(Sexo == 1) %>%
      filter(!is.na(Salario_bruto)) %>%  # Eliminar filas con Salario_bruto NA
      filter(Salario_bruto > quantile(Salario_bruto, 0.01) & Salario_bruto < quantile(Salario_bruto, 0.99))  # Filtrar atipicos
    
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

# Filtrar asegurados y no asegurados (Seguro)
hombres_asegurados <- combined_data %>% filter(Seguro == 1)
hombres_no_asegurados <- combined_data %>% filter(Seguro == 2)

# Crear un data frame con los datos combinados
df <- rbind(mutate(hombres_asegurados, Grupo = "Asegurados"),
            mutate(hombres_no_asegurados, Grupo = "No Asegurados"))

# Ordenar datos en el grafico
df$Grupo <- factor(df$Grupo, levels = c("Asegurados", "No Asegurados"))

# Filtrar datos atipicos
q1 <- quantile(df$Salario_bruto, 0.25)
q3 <- quantile(df$Salario_bruto, 0.75)
iqr <- q3 - q1
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Filtrar datos
df_filtered <- df %>% filter(Salario_bruto >= lower_bound & Salario_bruto <= upper_bound)

# Crear el boxplot
ggplot(df_filtered, aes(x = Grupo, y = Salario_bruto, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Distribución de Salarios Brutos por Grupo",
       x = "Grupo",
       y = "Salario Bruto") +
  theme_minimal() +
  scale_fill_manual(values = c("Asegurados" = "blue", "No Asegurados" = "red"))
  
# Guardar el grafico
ggsave("boxplot_salarios_filtrado.png", width = 8, height = 6)
