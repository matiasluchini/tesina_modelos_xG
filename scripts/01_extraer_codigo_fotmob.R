#---- Carga de librerias ----
library(tidyverse)
library(readxl)
library(openxlsx)
library(here)

#---- Carga de datos ----
ubicacion <- here("data", "manual", "tabla_para_extraer_codigos_fotmob.xlsx")

torneos <- c("copa2021", 
             "liga2021", 
             "copa2022", 
             "liga2022", 
             "liga2023", 
             "copa2023", 
             "copa2024", 
             "liga2024")

for (i in torneos) {
  datos <- read_excel(ubicacion, sheet = i, col_names = FALSE)
  assign(paste0("fixture_", i), datos)
}

lista_fixtures <- list(fixture_copa2021, 
                       fixture_copa2022, 
                       fixture_copa2023, 
                       fixture_copa2024, 
                       fixture_liga2021, 
                       fixture_liga2022, 
                       fixture_liga2023, 
                       fixture_liga2024)

#---- Limpieza de la base ----

# Creo una funcion qu extrae la cadena de texto que se encuentra entre # y ).
extraer_textos <- function(df) {
  df_texto <- df %>% mutate(across(everything(), as.character))
  resultados <- apply(df_texto, c(1, 2), function(x) {
    str_extract_all(x, "#(.*?)\\)")[[1]]
  })
  vector_final <- unlist(resultados)
  vector_limpio <- na.omit(vector_final)
  vector_limpio <- str_remove_all(vector_limpio, "^#|\\)$")
  return(vector_limpio)
}

# Aplico a toda la lista.
vectores_por_fixture <- lapply(lista_fixtures, extraer_textos)

# Asigno nombres a los vectores por comodidad.
for (i in seq_along(vectores_por_fixture)) {
  assign(paste0("vector_fixture", i), vectores_por_fixture[[i]])
}

# Construyo dataframes con esos vectores.
df_copa2021 <- data.frame(
  codigos = vector_fixture1
)
df_liga2021 <- data.frame(
  codigos = vector_fixture2
)
df_copa2022 <- data.frame(
  codigos = vector_fixture3
)
df_liga2022 <- data.frame(
  codigos = vector_fixture4
)
df_liga2023 <- data.frame(
  codigos = vector_fixture5
)
df_copa2023 <- data.frame(
  codigos = vector_fixture6
)
df_copa2024 <- data.frame(
  codigos = vector_fixture7
)
df_liga2024 <- data.frame(
  codigos = vector_fixture8
)

#---- Conversion a archivo .xlsx ----
write.xlsx(list("copa2021" = df_copa2021, 
                "liga2021" = df_liga2021,
                "copa2022" = df_copa2022, 
                "liga2022" = df_liga2022, 
                "liga2023" = df_liga2023,
                "copa2023" = df_copa2023, 
                "copa2024" = df_copa2024, 
                "liga2024" = df_liga2024), 
           file = here("data", "manual", "codigos_fotmob.xlsx"))
