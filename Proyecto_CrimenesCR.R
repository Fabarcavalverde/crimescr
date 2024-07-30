# Limpiar el entorno de objetos existentes
rm(list = ls())

# Instalar y cargar los paquetes necesarios
packages <- c("readr", "tidyr", "stringr", "lubridate", "plotly", "dplyr")
install.packages(packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# Cargar los datos
Crimenes_CR <- read.csv("C:\\CrimenesCr.csv")

# Examinar la estructura del DataFrame
str(Crimenes_CR)

# Resumen de datos para verificar la presencia de valores NA en todas las columnas
summarise_all(Crimenes_CR, ~sum(!is.na(.)))

# Inspeccionar columna "X.."
Crimenes_CR %>% select(X..)

# Eliminar las columnas "X.." y "SubVictima"
Crimenes_CR <- Crimenes_CR %>% select(-X.., -SubVictima)

# Cambiar nombres de las columnas
Crimenes_CR <- Crimenes_CR %>%
  rename(
    Hora = Victima,
    Victima = Edad,
    Edad = Genero,
    Genero = Nacionalidad,
    Nacionalidad = Provincia,
    Provincia = Canton,
    Canton = Distrito
  )

# Verificar la estructura después de renombrar
str(Crimenes_CR)

# Filtrar para asegurarse de que todos los casos son completos
Crimenes_CR <- Crimenes_CR %>% filter(complete.cases(.))

# Ordenar datos por fecha
Crimenes_CR <- Crimenes_CR %>% arrange(Fecha) %>% view()

# Eliminar registros de "Delito" con valores específicos
Crimenes_CR <- Crimenes_CR %>% filter(Delito != ";;")

# Modificar formatos de fecha y simplificar formato de hora
Crimenes_CR$Fecha <- as.Date(Crimenes_CR$Fecha, format = "%Y-%m-%d")
Crimenes_CR$Hora <- sub("^([0-9]{2}:[0-9]{2}):[0-9]{2}.*", "\\1", Crimenes_CR$Hora)


# Filtrar solo los casos de homicidio
homicidios_por_canton <- Crimenes_CR %>%
  filter(grepl("homicidio", Delito, ignore.case = TRUE)) %>%  # Ajusta el término de búsqueda según tus datos
  group_by(Canton) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

# Seleccionar los top 10 cantones con más homicidios
top_cantones_homicidios <- head(homicidios_por_canton, 10)

# Crear un gráfico de barras usando Plotly
plot_homicidios_cantones <- plot_ly(data = top_cantones_homicidios, x = ~Canton, y = ~Count, type = 'bar',
                                    marker = list(color = 'red'),
                                    hoverinfo = 'text',
                                    text = ~paste('Cantón:', Canton, '<br>Homicidios:', Count)) %>%
  layout(title = 'Top 10 Cantones con más Homicidios',
         xaxis = list(title = 'Cantón'),
         yaxis = list(title = 'Número de Homicidios'))

# Mostrar el gráfico
plot_homicidios_cantones


# Filtrar solo los casos de homicidio
homicidios_por_canton <- Crimenes_CR %>%
  filter(grepl("homicidio", Delito, ignore.case = TRUE)) %>%  # Ajusta el término de búsqueda según tus datos
  group_by(Canton) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(Count)  # Ordena ascendente por número de homicidios

# Seleccionar los 10 cantones con menos homicidios
bottom_cantones_homicidios <- head(homicidios_por_canton, 10)

# Crear un gráfico de barras usando Plotly
plot_homicidios_cantones_menos <- plot_ly(data = bottom_cantones_homicidios, x = ~Canton, y = ~Count, type = 'bar',
                                          marker = list(color = 'blue'),
                                          hoverinfo = 'text',
                                          text = ~paste('Cantón:', Canton, '<br>Homicidios:', Count)) %>%
  layout(title = 'Top 10 Cantones con menos Homicidios',
         xaxis = list(title = 'Cantón'),
         yaxis = list(title = 'Número de Homicidios'))

# Mostrar el gráfico
plot_homicidios_cantones_menos

# Filtrar solo los casos de homicidio
homicidios_genero <- Crimenes_CR %>%
  filter(grepl("homicidio", Delito, ignore.case = TRUE)) %>%  # Ajusta esta parte según cómo se registren los homicidios en tus datos
  group_by(Genero) %>%
  summarise(Count = n(), .groups = 'drop')

# Crear un gráfico de barras usando Plotly para visualizar la relación entre homicidios y género
plot_genero_homicidios <- plot_ly(data = homicidios_genero, x = ~Genero, y = ~Count, type = 'bar',
                                  marker = list(color = 'rgba(255, 99, 132, 0.7)', 
                                                line = list(color = 'rgba(255, 99, 132, 1.0)', width = 1.5)),
                                  hoverinfo = 'text',
                                  text = ~paste('Género:', Genero, '<br>Homicidios:', Count)) %>%
  layout(title = 'Relación entre Homicidios y Género',
         xaxis = list(title = 'Género'),
         yaxis = list(title = 'Número de Homicidios'))

# Mostrar el gráfico
plot_genero_homicidios


# Filtrar solo los casos de homicidio
homicidios_nacionalidad <- Crimenes_CR %>%
  filter(grepl("homicidio", Delito, ignore.case = TRUE)) %>%  # Asegúrate de que esto coincide con cómo se registran los homicidios en tus datos
  group_by(Nacionalidad) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count)) %>%
  top_n(10, Count)  # Seleccionar las 10 nacionalidades con más homicidios

# Crear un gráfico de barras usando Plotly para visualizar la relación entre homicidios y nacionalidad
plot_top_nacionalidad_homicidios <- plot_ly(data = homicidios_nacionalidad, x = ~Nacionalidad, y = ~Count, type = 'bar',
                                            marker = list(color = 'rgba(255, 159, 64, 0.7)',
                                                          line = list(color = 'rgba(255, 159, 64, 1.0)', width = 1.5)),
                                            hoverinfo = 'text',
                                            text = ~paste('Nacionalidad:', Nacionalidad, '<br>Homicidios:', Count)) %>%
  layout(title = 'Top 10 Nacionalidades con más Homicidios',
         xaxis = list(title = 'Nacionalidad', tickangle = -45),
         yaxis = list(title = 'Número de Homicidios'))

# Mostrar el gráfico
plot_top_nacionalidad_homicidios

# Filtrar solo los casos de homicidio
homicidios_edad <- Crimenes_CR %>%
  filter(grepl("homicidio", Delito, ignore.case = TRUE)) %>%
  group_by(Edad) %>%
  summarise(Count = n(), .groups = 'drop')

# Crear un gráfico de barras usando Plotly para visualizar la relación entre homicidios y categoría de edad
plot_edad_homicidios <- plot_ly(data = homicidios_edad, x = ~Edad, y = ~Count, type = 'bar',
                                marker = list(color = c('rgba(255, 99, 132, 0.7)', 'rgba(54, 162, 235, 0.7)'),
                                              line = list(color = c('rgba(255, 99, 132, 1.0)', 'rgba(54, 162, 235, 1.0)'), width = 1.5)),
                                hoverinfo = 'text',
                                text = ~paste('Categoría de Edad:', Edad, '<br>Homicidios:', Count)) %>%
  layout(title = 'Homicidios por Categoría de Edad',
         xaxis = list(title = 'Categoría de Edad'),
         yaxis = list(title = 'Número de Homicidios'))

# Mostrar el gráfico
plot_edad_homicidios



# Función para calcular la moda en variables categóricas
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calcular la moda para Delito, Género y Nacionalidad
moda_delito <- moda(Crimenes_CR$Delito)
moda_genero <- moda(Crimenes_CR$Genero)
moda_nacionalidad <- moda(Crimenes_CR$Nacionalidad)

# Mostrar la moda de las variables categóricas
print(paste("Moda de Delito:", moda_delito))
print(paste("Moda de Género:", moda_genero))
print(paste("Moda de Nacionalidad:", moda_nacionalidad))

# Extracción de la hora del campo 'Hora'
Crimenes_CR$Hora_Solo <- sapply(strsplit(Crimenes_CR$Hora, ":"), function(x) x[1])  # Extraer sólo la hora

# Convertir la hora extraída a formato numérico
Crimenes_CR$Hora_Solo <- as.numeric(Crimenes_CR$Hora_Solo)


# Calcular y mostrar media y mediana de Hora_Solo
media_hora <- mean(Crimenes_CR$Hora_Solo, na.rm = TRUE)
mediana_hora <- median(Crimenes_CR$Hora_Solo, na.rm = TRUE)

print(paste("Media de Hora:", media_hora))
print(paste("Mediana de Hora:", mediana_hora))

