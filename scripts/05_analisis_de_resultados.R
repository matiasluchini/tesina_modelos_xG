#---- Librerias ----
library(readr)
library(tidyverse)
library(ggsoccer)
library(patchwork)
library(rstan)
library(bayesplot)
library(tidybayes)
library(ggdist)
library(forcats)
library(ggdist)
library(ggthemes)
library(ncdf4)
library(posterior)
library(scoringRules)
library(readxl)
library(purrr)

#---- Tema de graficos ----
tema_mio <- function() {
  theme_bw() +
    theme(plot.title = element_text(face = "bold", size = 14),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_blank())
}

W <- 6
H <- 4

cols <- c("#9BBB59", "#8064A2", "#F3A447","#4BACC6")

#---- Carga de datos ----
disparos_2023_2024 <- read_csv("Datos/disparos_2023_2024.csv")

#---- Agregar localia a la base ----

# Esto no se termino usando, pero lo dejo para futuros trabajos.

ubicacion <- "C:/Users/Usuario/Downloads/resultados_y_xG.xlsx"

torneos <- c("liga2023", 
             "copa2023", 
             "copa2024", 
             "liga2024")

for (i in torneos) {
  datos <- read_excel(ubicacion, sheet = i)
  assign(paste0("resultados_y_xG_", i), datos)
}

resultados_y_xG_liga2024 <- resultados_y_xG_liga2024 %>% 
  mutate(equipo_local = case_when(equipo_local == "Estudiantes–LP" ~ "Estudiantes", 
                                  sede == "Estadio Juan Bautista Gargantini" ~ "Independiente Rivadavia",
                                  equipo_local == "Independiente" & sede == "Estadio Malvinas Argentinas" ~ "Independiente Rivadavia",
                                  (equipo_local == "Independiente" & equipo_visita == "Arg Juniors") & jornada == 25 ~ "Independiente Rivadavia",
                                  TRUE ~ equipo_local),
         equipo_visita= case_when(equipo_visita == "Estudiantes–LP" ~ "Estudiantes", 
                                  (equipo_visita == "Independiente" & equipo_local == "Belgrano") & marcador_local == 0 ~ "Independiente Rivadavia",
                                  (equipo_visita == "Independiente" & equipo_local == "Estudiantes") & marcador_local == 1 ~ "Independiente Rivadavia",
                                  (equipo_visita == "Independiente" & equipo_local == "Huracán") & jornada == 5 ~ "Independiente Rivadavia",
                                  (equipo_visita == "Independiente" & equipo_local == "Lanús") & jornada == 1 ~ "Independiente Rivadavia",
                                  (equipo_visita == "Independiente" & equipo_local == "Newell's OB") & jornada == 7 ~ "Independiente Rivadavia",
                                  (equipo_visita == "Independiente" & equipo_local == "Racing Club") & jornada == 22 ~ "Independiente Rivadavia",
                                  (equipo_visita == "Independiente" & equipo_local == "Sarmiento") & jornada == 3 ~ "Independiente Rivadavia",
                                  (equipo_visita == "Independiente" & equipo_local == "Tigre") & jornada == 26 ~ "Independiente Rivadavia",
                                  equipo_local == "Atlé Tucumán" & equipo_visita == "Independiente" ~ "Independiente Rivadavia",
                                  equipo_local == "Talleres" & equipo_visita == "Independiente" ~ "Independiente Rivadavia",
                                  equipo_local == "Vélez Sarsfield" & equipo_visita == "Independiente" ~ "Independiente Rivadavia",
                                  equipo_local == "Banfield" & equipo_visita == "Independiente" ~ "Independiente Rivadavia",
                                  equipo_local == "Barracas Central" & equipo_visita == "Independiente" ~ "Independiente Rivadavia",
                                  equipo_local == "Cen. Córdoba–SdE" & equipo_visita == "Independiente" ~ "Independiente Rivadavia",
                                  TRUE ~ equipo_visita), 
         sede = case_when(equipo_local == "Independiente Rivadavia" & sede == "Estadio Libertadores de América" ~ "Estadio Juan Bautista Gargantini", 
                          TRUE ~ sede))

lista_resultados <- list(resultados_y_xG_copa2023, 
                         resultados_y_xG_copa2024, 
                         resultados_y_xG_liga2023,
                         resultados_y_xG_liga2024)

conteo_equipos_sedes <- lista_resultados %>%
  map_dfr(~ select(.x, equipo_local, sede)) %>%
  count(equipo_local, sede, name = "conteo")

localias <- data.frame(
  equipo = c("Arg Juniors",
             "Arsenal",
             "Atlé Tucumán",
             "Banfield",
             "Barracas Central",
             "Belgrano",
             "Belgrano",
             "Boca Juniors",
             "Cen. Córdoba–SdE",
             "Cen. Córdoba–SdE",
             "Cen. Córdoba–SdE",
             "Colón",
             "Defensa y Just",
             "Deportivo Riestra",
             "Estudiantes",
             "Estudiantes–LP",
             "Gimnasia–LP",
             "Godoy Cruz",
             "Godoy Cruz",
             "Godoy Cruz",
             "Huracán",
             "Independiente",
             "Independiente",
             "Independiente Rivadavia",
             "Instituto",
             "Instituto",
             "Instituto",
             "Lanús",
             "Newell's OB",
             "Platense",
             "Racing Club",
             "River Plate",
             "Rosario Central",
             "Rosario Central",
             "Rosario Central",
             "San Lorenzo",
             "Sarmiento",
             "Talleres",
             "Tigre",
             "Unión",
             "Vélez Sarsfield"),
  estadio = c("Estadio Diego Armando Maradona", 
              "Estadio Julio Humberto Grondona", 
              "Estadio Monumental Presidente José Fierr...", 
              "Estadio Florencio Solá", 
              "Estadio Claudio Fabián Tapia", 
              "Estadio Julio César Villagra", 
              "Estadio Mario Alberto Kempes", 
              "Estadio Alberto José Armando", 
              "Estadio Alfredo Terrera",
              "Estadio Victor Antonio Aguirre",
              "Estadio Único Madre de Ciudades", 
              "Estadio Brigadier General Estanislao Lóp...", 
              "Estadio Norberto Tito Tomaghello", 
              "Estadio Guillermo Laza", 
              "Estadio Jorge Luis Hirschi",
              "Estadio Jorge Luis Hirschi",
              "Estadio Juan Carmelo Zerillo", 
              "Estadio Feliciano Gambarte", 
              "Estadio Malvinas Argentinas",
              "Estadio Víctor Antonio Legrotaglie",
              "Estadio Tomás Adolfo Ducó", 
              "Estadio Libertadores de América", 
              "Estadio Malvinas Argentinas", 
              "Estadio Juan Bautista Gargantini", 
              "Estadio Juan Domingo Perón", 
              "Estadio Presidente Juan Domingo Perón", 
              "Estadio Mario Alberto Kempes", 
              "Estadio Ciudad de Lanús", 
              "Estadio Marcelo Alberto Bielsa", 
              "Estadio Ciudad de Vicente López", 
              "Estadio Presidente Juan Domingo Perón",
              "Estadio Mâs Monumental", 
              "Estadio Gigante de Arroyito",
              "El Gigante de Arroyito",
              "Estadio Dr. Lisandro de la Torre",
              "Estadio Pedro Bidegaín", 
              "Estadio Eva Peron de Junín", 
              "Estadio Mario Alberto Kempes", 
              "Estadio José Dellagiovanna", 
              "Estadio 15 de Abril", 
              "Estadio José Amalfitani"))

for (i in torneos) {
  nombre <- paste0("resultados_y_xG_", i)
  datos <- get(nombre)
  datos <- datos %>%
    rowwise() %>% 
    mutate(fue_local = ifelse(
      equipo_local %in% localias$equipo & sede %in% localias$estadio[localias$equipo == equipo_local],
      "Sí",
      "No")) %>%
    ungroup()
  assign(paste0("resultados_y_xG_", i), datos)
}

lista_resultados <- list(resultados_y_xG_copa2023, 
                         resultados_y_xG_copa2024, 
                         resultados_y_xG_liga2023,
                         resultados_y_xG_liga2024)

rm(resultados_y_xG_copa2023,
   resultados_y_xG_copa2024, 
   resultados_y_xG_liga2023, 
   resultados_y_xG_liga2024, 
   i, 
   nombre, 
   torneos, 
   ubicacion, 
   localias, 
   conteo_equipos_sedes, 
   datos)

disparos_2023_2024 <- disparos_2023_2024 %>% 
  mutate(equipo = case_when(equipo == "Argentinos Juniors" ~ "Arg Juniors",
                            equipo == "Arsenal Sarandi" ~ "Arsenal",
                            equipo == "Atletico Tucuman" ~ "Atlé Tucumán",
                            equipo == "Central Cordoba de Santiago" ~ "Cen. Córdoba–SdE",
                            equipo == "Club Atletico Platense" ~ "Platense",
                            equipo == "Colon" ~ "Colón",
                            equipo == "Defensa y Justicia" ~ "Defensa y Just",
                            equipo == "Gimnasia LP" ~ "Gimnasia–LP",
                            equipo == "Huracan" ~ "Huracán",
                            equipo == "Lanus" ~ "Lanús",
                            equipo == "Newell's Old Boys" ~ "Newell's OB",
                            equipo == "Union" ~ "Unión",
                            equipo == "Velez Sarsfield" ~ "Vélez Sarsfield",
                            TRUE ~ equipo))

df <- disparos_2023_2024 %>% 
  group_by(torneo, partido) %>% 
  summarise(val1 = unique(equipo)[1],
            val2 = unique(equipo)[2]) %>%
  group_by(torneo) %>%
  group_split()

df[[1]] <- df[[1]] %>% filter(partido != 65)
lista_resultados[[3]] <- lista_resultados[[3]] %>% filter(ronda != "Desempate") 

df <- map2(df, lista_resultados, ~ {
  
  lista_unica <- .y %>%
    mutate(match_id = paste0(pmin(equipo_local, equipo_visita), "_", 
                             pmax(equipo_local, equipo_visita))) %>%
    group_by(match_id) %>%
    summarise(fue_local = ifelse(n() > 1, "REVISAR", first(fue_local)),
              local = ifelse(n() > 1, "REVISAR", first(equipo_local)),
              .groups = "drop")
  
  .x %>%
    mutate(match_id = paste0(pmin(val1, val2), "_", 
                             pmax(val1, val2))) %>%
    left_join(lista_unica, by = "match_id")
})

df[[1]] <- df[[1]] %>% 
  mutate(fue_local = case_when(partido == 112 ~ "Sí",
                               partido == 120 ~ "No",
                               partido == 64 ~ "No",
                               partido == 110 ~ "Sí", 
                               TRUE ~ fue_local), 
         local = case_when(partido == 112 ~ "Rosario Central",
                           partido == 120 ~ "Neutral",
                           partido == 64 ~ "Neutral", 
                           partido == 110 ~ "Godoy Cruz", 
                           TRUE ~ local))

df[[2]] <- df[[2]] %>% 
  mutate(fue_local = case_when(partido == 27 ~ "Sí",
                               partido == 48 ~ "Sí",
                               partido == 55 ~ "Sí",
                               partido == 65 ~ "No",
                               partido == 66 ~ "No",
                               partido == 67 ~ "No",
                               TRUE ~ fue_local), 
         local = case_when(partido == 27 ~ "River Plate",
                           partido == 48 ~ "Estudiantes",
                           partido == 55 ~ "Vélez Sarsfield", 
                           partido == 65 ~ "Neutral", 
                           partido == 67 ~ "Neutral", 
                           partido == 66 ~ "Neutral", 
                           TRUE ~ local))

df <- bind_rows(df[[1]], df[[2]], df[[3]], df[[4]]) %>% 
  select(torneo, partido, fue_local, local)

disparos_2023_2024 <- disparos_2023_2024 %>%
  left_join(df %>% select(torneo, partido, local, fue_local),
            by = c("torneo", "partido")) %>%
  mutate(fue_local_disparo = case_when(
    equipo == local & fue_local == "Sí" ~ 1,
    TRUE ~ 0)) %>%
  select(-local, -fue_local)

rm(df, lista_resultados)

#---- Eliminar las tandas de penales ----
id_borrar <- disparos_2023_2024 %>% 
  filter(torneo %in% c("copa2023", "copa2024"), 
         situacion == "Penal", 
         minuto > 90) %>% 
  pull(...1)

disparos_2023_2024 <- disparos_2023_2024 %>% 
  filter(!(...1 %in% id_borrar))

rm(id_borrar)

#---- Limpieza de la base ----

# El partido numero 48 de la copa2024 tiene disparos al arco en la direccion opuesta.
# Debo corregirlos.
shot_angle <- function(x, y) {
  a <- 7.32
  b <- sqrt(((105 - x)^2) + ((30.34 - y)^2))
  c <- sqrt(((105 - x)^2) + ((37.66 - y)^2))
  
  numerador <- (a^2) - (b^2) - (c^2)
  denominador <- (-2)*b*c
  
  angulo <- (acos(numerador/denominador) * 180)/pi
}

datos <- disparos_2023_2024 %>% 
  mutate(x = case_when((torneo == "copa2024" & partido == 48 & distancia > 50) ~ (105 - x),
                       TRUE ~ x), 
         angulo = case_when((torneo == "copa2024" & partido == 48 & distancia > 50) ~ shot_angle(x, y), 
                            TRUE ~ angulo),
         distancia = case_when((torneo == "copa2024" & partido == 48 & distancia > 50) ~ sqrt((105 - x)^2 + (34 - y)^2), 
                               TRUE ~ distancia))

# Elimino la funcion creada:
rm(shot_angle)

# Chequeo que no haya errores en los ID:

# Hay tres casos de nombres repetidos con diferente ID, pero refieren a personas
# diferentes (Fernando Martinez, Matias Moreno y Tomas Perez).
disparos_2023_2024 %>% 
  count(jugador, 
        id_jugador) %>% 
  group_by(jugador) %>% 
  summarise(cantidad = n()) %>% 
  filter(cantidad > 1)

# Hay tres casos de ID repetidos.
disparos_2023_2024 %>% 
  count(jugador, 
        id_jugador) %>% 
  group_by(id_jugador) %>% 
  summarise(cantidad = n()) %>% 
  filter(cantidad > 1)

# Dos jugadores tienen el ID 1362516 pero refieren a la misma persona.
# Dos jugadores tienen el ID 1486264 pero refieren a la misma persona.
# Cuatro jugaodres tienen el ID -1. Debo corregirlos.

# Estos jugadores son: 
disparos_2023_2024 %>% filter(id_jugador == -1) %>% count(jugador)

# Voy a asignarles los ID mas altos. Matias Moreno tiene un ID ya puesto en otras jugadas.
ID_maximo <- max(disparos_2023_2024$id_jugador)

# Ahora si modifico la base con todos los datos:
# Acomodo la base y selecciono las variables que necesito por ahora. Matias Moreno ya tiene un
# ID asignado, ese -1 es un error. 
# Saco las situaciones de pelota parada ya que son situaciones en las que las probabilidades
# de gol se comportan distinto.
# Tambien saco los disparos de arqueros que son muy pocos.
# Por ultimo estandarizo las variables para facilitar el muestreo.
datos <- datos %>% 
  filter(fue_autogol != 1,
         situacion != "TiroLibre",
         situacion != "SetPiece",
         situacion != "ThrowInSetPiece",
         situacion != "Corner",
         situacion != "Penal") %>% 
  mutate(id_jugador = case_when(jugador == "Claudio Valverde"       ~ ID_maximo + 1, 
                                jugador == "Fabricio Gabriel López" ~ ID_maximo + 2,
                                jugador == "Matias Moreno"          ~ 1605748, 
                                jugador == "Nicolás Caro Torres"    ~ ID_maximo + 3, 
                                TRUE                                ~ id_jugador), 
         posicion_nueva = case_when((posicion == "MF" & posicion_alternativa == "DF") ~ "MF_no_ofen",
                                    (posicion == "MF" & is.na(posicion_alternativa)) ~ "MF_no_ofen",
                                    (posicion == "MF" & posicion_alternativa == "FW") ~ "MF_ofen",
                                    TRUE ~ posicion),
         interaccion = distancia*angulo,
         distancia_est = as.numeric(scale(distancia)),
         angulo_est = as.numeric(scale(angulo)),
         interaccion_est = as.numeric(scale(interaccion)), 
         interaccion2 = distancia_est*angulo_est, 
         cabeza = case_when(parte_del_cuerpo == "Cabeza" ~ 1, 
                            TRUE                         ~ 0), 
         otro = case_when(parte_del_cuerpo == "Otro" ~ 1, 
                          TRUE                       ~ 0)) %>% 
  select(equipo, 
         id_jugador, 
         posicion, 
         posicion_nueva,
         x, 
         y,
         distancia, 
         angulo, 
         interaccion,
         distancia_est, 
         angulo_est, 
         interaccion_est,
         interaccion2,
         cabeza,
         otro,
         fue_local_disparo,
         situacion,
         xG, 
         gol) %>% 
  filter(otro == 0)

# Elimino el elemento creado.
rm(ID_maximo)

# Construyo una base para tener un registro de la cantidad de goles de cada 
# jugador. 
goles_por_jugador <- datos %>% 
  group_by(id_jugador) %>%
  summarise(disparos = n(), 
            goles = sum(gol), 
            goles_cabeza = sum(gol * cabeza),
            proporcion = goles / disparos, 
            dist_media = mean(distancia), 
            posicion = first(posicion_nueva),
            .groups = "drop") %>%
  left_join(disparos_2023_2024 %>% 
              group_by(id_jugador) %>% 
              summarise(jugador = first(jugador), .groups = "drop"),
            by = "id_jugador") %>%
  mutate(jugador = if_else(id_jugador == "1685813", 
                           "Claudio Valverde", 
                           jugador)) %>%
  arrange(desc(proporcion))

#---- Analisis descriptivo ----

# Gol:
table(datos$gol)              
prop.table(table(datos$gol))

# Distancia:
datos %>% 
  ggplot(aes(x = distancia)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  theme_minimal()

datos %>% 
  ggplot(aes(x = as.factor(gol), y = distancia)) + 
  geom_boxplot(fill = "skyblue", color = "black")

datos %>% 
  ggplot(aes(x = distancia, fill = as.factor(gol))) +
  geom_density(alpha = 0.5)

datos %>% 
  ggplot(aes(x = distancia, y = gol)) + 
  geom_jitter(size = 0.2)

datos %>% # El pico es por el gol de Ramon Sosa a Independiente
  mutate(distancia_agrupada = cut(distancia, breaks = seq(0, 150, by = 5))) %>% 
  group_by(distancia_agrupada) %>% 
  summarize(gol_rate = mean(gol == 1)) %>% 
  ggplot(aes(x = distancia_agrupada, y = gol_rate)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# Angulo:
datos %>% 
  ggplot(aes(x = angulo)) +
  geom_histogram(binwidth = 3, fill = "skyblue", color = "black") +
  theme_minimal()

datos %>% 
  ggplot(aes(x = as.factor(gol), y = angulo)) + 
  geom_boxplot(fill = "skyblue", color = "black")

datos %>%
  ggplot(aes(x = angulo, fill = as.factor(gol))) +
  geom_density(alpha = 0.5)

datos %>% 
  ggplot(aes(x = angulo, y = gol)) + 
  geom_jitter(size = 0.2)

datos %>% 
  mutate(angulo_agrupada = cut(angulo, breaks = seq(0, 160, by = 10))) %>% 
  group_by(angulo_agrupada) %>% 
  summarize(gol_rate = mean(gol == 1)) %>% 
  ggplot(aes(x = angulo_agrupada, y = gol_rate)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# Angulo y distancia:
datos %>% 
  filter(gol == 1) %>% 
  ggplot(aes(x = distancia, y = angulo)) +
  geom_point(size = 0.2) +
  xlim(0, 120) + 
  ylim(0, 180) 

datos %>% 
  filter(gol == 0) %>% 
  ggplot(aes(x = distancia, y = angulo)) +
  geom_point(size = 0.2) +
  xlim(0, 120) + 
  ylim(0, 180)

# Angulo, distancia y gol:
get_midpoint <- function(x) {
  as.numeric(sub("\\((.+),(.+)\\]", "\\1", x)) + 
    (as.numeric(sub("\\((.+),(.+)\\]", "\\2", x)) - 
       as.numeric(sub("\\((.+),(.+)\\]", "\\1", x))) / 2
}
datos %>%
  mutate(distancia_bin = cut(distancia, breaks = 35),
         angulo_bin = cut(angulo, breaks = 35)) %>%
  group_by(distancia_bin, angulo_bin) %>%
  summarise(total = n(),
            goles = sum(gol),
            tasa_gol = goles / total,
            .groups = "drop") %>%
  mutate(distancia_mid = get_midpoint(as.character(distancia_bin)),
         angulo_mid = get_midpoint(as.character(angulo_bin))) %>% 
  ggplot(aes(x = distancia_mid, y = angulo_mid, fill = tasa_gol)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "red2",
    mid = "yellow",
    high = "green2",
    midpoint = 0.5,
    name = "Tasa de gol"
  ) +
  labs(x = "Distancia", y = "Ángulo") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    legend.title = element_blank(),
    legend.text  = element_blank(), 
    legend.position = "none"
  )
rm(get_midpoint)

# Ubicacion:
datos %>% 
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_bin2d(binwidth = c(2.5, 2.5), aes(fill = after_stat(count)), alpha = 0.8) +
  scale_fill_gradient2(low = "lightgreen", mid = "yellow", high = "red", midpoint = 125) +
  ggtitle("Heatmap de disparos hacia un arco") +
  coord_fixed()

# Ubicación pero con tasa de gol: 
datos %>% 
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_summary_2d(aes(z = gol, fill = after_stat(value)), 
                  binwidth = c(2.5, 2.5),
                  fun = mean,
                  alpha = 0.8) +
  scale_fill_gradient2(low = "lightgreen", 
                       mid = "yellow", 
                       high = "red",
                       midpoint = 0.5,
                       limits = c(0, 1),
                       name = "Tasa de gol") +
  ggtitle("Tasa de gol por ubicación del disparo") +
  coord_fixed()

# Posicion:
table(datos$posicion_nueva, datos$gol)
prop.table(table(datos$posicion_nueva, datos$gol))
prop.table(table(datos$posicion_nueva, datos$gol), margin = 1)

datos %>%
  count(posicion_nueva) %>%
  ggplot(aes(x = posicion_nueva, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  theme_minimal()

datos %>%
  filter(gol == 1) %>%
  count(posicion_nueva) %>%
  ggplot(aes(x = posicion_nueva, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  theme_minimal()

# Posicion y xG (Opta):
datos %>%
  group_by(posicion_nueva) %>%
  summarise(goles = sum(gol, na.rm = TRUE),
            xG_total = sum(xG, na.rm = TRUE))

# Posicion y distancia:
datos %>% 
  ggplot(aes(x = posicion_nueva, y = distancia)) + 
  geom_boxplot(fill = "skyblue", color = "black")

# Posicion y angulo:
datos %>% 
  ggplot(aes(x = posicion_nueva, y = angulo)) + 
  geom_boxplot(fill = "skyblue", color = "black")

# Ubicacion por posicion:
max_count <- datos %>%
  mutate(x = x - 1,
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>%
  group_by(pos_bin_x = cut(x_std, breaks = seq(0, 100, 2.5)),
           pos_bin_y = cut(y_std, breaks = seq(0, 100, 2.5))) %>%
  summarise(count = n(), .groups = "drop") %>%
  summarise(max_count = max(count)) %>%
  pull(max_count)
p1 <- datos %>%
  filter(posicion_nueva == "DF") %>% 
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_bin2d(binwidth = c(2.5, 2.5), aes(fill = after_stat(count)), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#ccffcc", "yellow", "orange", "red"),
                       limits = c(0, max_count),
                       oob = scales::squish) +
  ggtitle("Defensores") +
  coord_fixed()
p2 <- datos %>%
  filter(posicion_nueva == "MF_no_ofen") %>% 
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_bin2d(binwidth = c(2.5, 2.5), aes(fill = after_stat(count)), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#ccffcc", "yellow", "orange", "red"),
                       limits = c(0, max_count),
                       oob = scales::squish) +
  ggtitle("Mediocampistas no ofensivos") +
  coord_fixed()
p3 <- datos %>%
  filter(posicion_nueva == "MF_ofen") %>% 
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_bin2d(binwidth = c(2.5, 2.5), aes(fill = after_stat(count)), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#ccffcc", "yellow", "orange", "red"),
                       limits = c(0, max_count),
                       oob = scales::squish) +
  ggtitle("Mediocampistas ofensivos") +
  coord_fixed()
p4 <- datos %>%
  filter(posicion_nueva == "FW") %>% 
  mutate(x = x - 1,
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_bin2d(binwidth = c(2.5, 2.5), aes(fill = after_stat(count)), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#ccffcc", "yellow", "orange", "red"),
                       limits = c(0, max_count),
                       oob = scales::squish) +
  ggtitle("Delanteros") +
  coord_fixed()
(p1 | p2) / (p3 | p4)
rm(list = c("max_count", "p1", "p2", "p3", "p4"))

# Ubicacion por posicion (solo goles): 
max_count <- datos %>%
  filter(gol == 1) %>% 
  mutate(x = x - 1,
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>%
  group_by(pos_bin_x = cut(x_std, breaks = seq(0, 100, 2.5)),
           pos_bin_y = cut(y_std, breaks = seq(0, 100, 2.5))) %>%
  summarise(count = n(), .groups = "drop") %>%
  summarise(max_count = max(count)) %>%
  pull(max_count)
p1 <- datos %>%
  filter(posicion_nueva == "DF", 
         gol == 1) %>% 
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_bin2d(binwidth = c(2.5, 2.5), aes(fill = after_stat(count)), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#ccffcc", "yellow", "orange", "red"),
                       limits = c(0, max_count),
                       oob = scales::squish) +
  ggtitle("Defensores") +
  coord_fixed()
p2 <- datos %>%
  filter(posicion_nueva == "MF_no_ofen", 
         gol == 1) %>%  
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_bin2d(binwidth = c(2.5, 2.5), aes(fill = after_stat(count)), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#ccffcc", "yellow", "orange", "red"),
                       limits = c(0, max_count),
                       oob = scales::squish) +
  ggtitle("Mediocampistas no ofensivos") +
  coord_fixed()
p3 <- datos %>%
  filter(posicion_nueva == "MF_ofen", 
         gol == 1) %>%  
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_bin2d(binwidth = c(2.5, 2.5), aes(fill = after_stat(count)), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#ccffcc", "yellow", "orange", "red"),
                       limits = c(0, max_count),
                       oob = scales::squish) +
  ggtitle("Mediocampistas ofensivos") +
  coord_fixed()
p4 <- datos %>%
  filter(posicion_nueva == "FW", 
         gol == 1) %>% 
  mutate(x = x - 1,
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_bin2d(binwidth = c(2.5, 2.5), aes(fill = after_stat(count)), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#ccffcc", "yellow", "orange", "red"),
                       limits = c(0, max_count),
                       oob = scales::squish) +
  ggtitle("Delanteros") +
  coord_fixed()
(p1 | p2) / (p3 | p4)
rm(list = c("max_count", "p1", "p2", "p3", "p4"))

#---- Traigo las muestras del modelo ajustado en Python -----

# Primero para el modelo 1:

nc_1 <- nc_open("Datos/ModeloColab/muestras_modelo_1_chequear.nc")

alpha_1 <- ncvar_get(nc_1, "alpha")
beta_angulo_1 <- ncvar_get(nc_1, "beta_angulo")
beta_distancia_1 <- ncvar_get(nc_1, "beta_distancia")
beta_interaccion_1 <- ncvar_get(nc_1, "beta_interaccion")

nc_close(nc_1)

draws_m1 <- list(
  alpha = as_draws_matrix(alpha_1),
  beta_angulo = as_draws_matrix(beta_angulo_1),
  beta_distancia = as_draws_matrix(beta_distancia_1),
  beta_interaccion = as_draws_matrix(beta_interaccion_1)
)

rhat_m1 <- sapply(draws_m1, rhat)
ess_bulk_m1 <- sapply(draws_m1, ess_bulk)

rhat_m1
ess_bulk_m1

muestras_modelo1 <- map_dfc(draws_m1, ~ as.vector(as_draws_matrix(.x)))

names(muestras_modelo1) <- names(draws_m1)

rm(list = c("nc_1",
            "alpha_1",
            "beta_angulo_1",
            "beta_distancia_1",
            "beta_interaccion_1",
            "draws_m1",
            "rhat_m1",
            "ess_bulk_m1"))

# Para el modelo 2: 

nc_2 <- nc_open("Datos/ModeloColab/muestras_modelo_2_chequear.nc")

alpha_2 <- ncvar_get(nc_2, "alpha")
beta_distancia_2 <- ncvar_get(nc_2, "beta_distancia")
beta_angulo_2 <- ncvar_get(nc_2, "beta_angulo")
beta_interaccion_2 <- ncvar_get(nc_2, "beta_interaccion")

posiciones_2 <- nc_2$dim[["posicion"]]$vals

nc_close(nc_2)

draws_m2 <- list(
  beta_angulo = as_draws_matrix(beta_angulo_2),
  beta_distancia = as_draws_matrix(beta_distancia_2),
  beta_interaccion = as_draws_matrix(beta_interaccion_2)
)

alphas_list <- lapply(1:dim(alpha_2)[1], function(k) {
  as_draws_matrix(alpha_2[k,,])
})

names(alphas_list) <- paste0("alpha_", posiciones_2)

draws_m2 <- c(draws_m2, alphas_list)

rhat_m2 <- sapply(draws_m2, rhat)
ess_bulk_m2 <- sapply(draws_m2, ess_bulk)

rhat_m2
ess_bulk_m2

muestras_modelo2 <- map_dfc(draws_m2, ~ as.vector(as_draws_matrix(.x)))

names(muestras_modelo2) <- names(draws_m2)

rm(list = c("nc_2",
            "alpha_2",
            "beta_angulo_2",
            "beta_distancia_2",
            "beta_interaccion_2", 
            "posiciones_2",
            "alphas_list",
            "draws_m2",
            "rhat_m2",
            "ess_bulk_m2"))

# Para el modelo 3: 

nc_3 <- nc_open("Datos/ModeloColab/muestras_modelo_3_chequear.nc")

alpha_3 <- ncvar_get(nc_3, "alpha")
beta_distancia_3 <- ncvar_get(nc_3, "beta_distancia")
beta_angulo_3 <- ncvar_get(nc_3, "beta_angulo")
beta_interaccion_3 <- ncvar_get(nc_3, "beta_interaccion")
sigma_sq_gamma_3 <- ncvar_get(nc_3, "sigma_sq_gamma")

posiciones_3 <- nc_3$dim[["posicion"]]$vals

nc_close(nc_3)

draws_m3 <- list(
  beta_angulo = as_draws_matrix(beta_angulo_3),
  beta_distancia = as_draws_matrix(beta_distancia_3),
  beta_interaccion = as_draws_matrix(beta_interaccion_3),
  sigma_sq_gamma = as_draws_matrix(sigma_sq_gamma_3)
)

alphas_list <- lapply(1:dim(alpha_3)[1], function(k) {
  as_draws_matrix(alpha_3[k,,])
})

names(alphas_list) <- paste0("alpha_", posiciones_3)

draws_m3 <- c(draws_m3, alphas_list)

rhat_m3 <- sapply(draws_m3, rhat)
ess_bulk_m3 <- sapply(draws_m3, ess_bulk)

rhat_m3
ess_bulk_m3

muestras_modelo3 <- map_dfc(draws_m3, ~ as.vector(as_draws_matrix(.x)))

names(muestras_modelo3) <- names(draws_m3)

rm(list = c("nc_3",
            "alpha_3",
            "beta_angulo_3",
            "beta_distancia_3",
            "beta_interaccion_3", 
            "sigma_sq_gamma_3",
            "posiciones_3",
            "alphas_list",
            "draws_m3",
            "rhat_m3",
            "ess_bulk_m3"))

#---- Construyo resumenes de los posteriors -----
resumen_modelo1 <- muestras_modelo1 %>%
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  group_by(parametro) %>%
  summarise(media = round(mean(valor), 3),
            desvio = round(sd(valor), 3),
            q05 = round(quantile(valor, 0.05), 3),
            q95 = round(quantile(valor, 0.95), 3),
            .groups = "drop")

resumen_modelo2 <- muestras_modelo2 %>%
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  group_by(parametro) %>%
  summarise(media = round(mean(valor), 3),
            desvio = round(sd(valor), 3),
            q05 = round(quantile(valor, 0.05), 3),
            q95 = round(quantile(valor, 0.95), 3),
            .groups = "drop")

resumen_modelo3 <- muestras_modelo3 %>%
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  group_by(parametro) %>%
  summarise(media = round(mean(valor), 3),
            desvio = round(sd(valor), 3),
            q05 = round(quantile(valor, 0.05), 3),
            q95 = round(quantile(valor, 0.95), 3),
            .groups = "drop")

#---- Densidades a posteriori ----
interceptos_modelo1 <- muestras_modelo1 %>%
  pivot_longer(cols = alpha,
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = "alpha") %>%
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_y_discrete(labels = function(x) rep("  ", length(x)), 
                   expand = expansion(add = c(0.1, 0.2))) +
  tema_mio()+  
  theme(
    axis.text.y = element_text(size = 14),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 14),
    axis.text.x = element_text(size = 8), 
    plot.margin = margin(t = 5, r = 5, b = 5, l = 10)
  ) + 
  labs(x = expression(alpha),
       y = NULL)

explicativas_modelo1 <- muestras_modelo1 %>%
  select(beta_distancia,
         beta_angulo,
         beta_interaccion) %>% 
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = recode(parametro,
                            "beta_distancia" = "beta[1]",
                            "beta_angulo" = "beta[2]",
                            "beta_interaccion" = "beta[3]"),
         parametro = factor(parametro,
                            levels = rev(c("beta[1]",
                                           "beta[2]",
                                           "beta[3]")))) %>% 
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  tema_mio() +  
  theme(
    axis.text.y = element_text(size = 14),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12), 
    axis.text.x = element_text(size = 8)
  ) +
  labs(x = "Valor del parámetro",
       y = NULL)

interceptos_modelo2 <- muestras_modelo2 %>%
  select(alpha_DF,
         alpha_FW,
         alpha_MF_no_ofen,
         alpha_MF_ofen) %>% 
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = recode(parametro,
                            "alpha_DF"       = "Defensor",
                            "alpha_FW"       = "Delantero",
                            "alpha_MF_no_ofen" = "MC[no~ofensivo]",
                            "alpha_MF_ofen"    = "MC[ofensivo]"),
         parametro = factor(parametro,
                            levels = c("Defensor",
                                       "MC[no~ofensivo]",
                                       "MC[ofensivo]",
                                       "Delantero"))) %>% 
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  scale_x_continuous(breaks = seq(-3.20, -2.4, by = 0.2)) +
  tema_mio() +  
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 14), 
    axis.text.x = element_text(size = 8)
  ) +
  labs(x = expression(alpha),
       y = NULL)

explicativas_modelo2 <- muestras_modelo2 %>%
  select(beta_distancia,
         beta_angulo,
         beta_interaccion) %>% 
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = recode(parametro,
                            "beta_distancia" = "beta[1]",
                            "beta_angulo" = "beta[2]",
                            "beta_interaccion" = "beta[3]"),
         parametro = factor(parametro,
                            levels = rev(c("beta[1]",
                                           "beta[2]",
                                           "beta[3]")))) %>% 
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  tema_mio() +  
  theme(
    axis.text.y = element_text(size = 12, 
                               margin = margin(l = 48)),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12), 
    axis.text.x = element_text(size = 8)
  ) +
  labs(x = "Valor del parámetro",
       y = NULL)

interceptos_modelo3 <- muestras_modelo3 %>%
  select(alpha_DF,
         alpha_FW,
         alpha_MF_no_ofen,
         alpha_MF_ofen) %>%
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = recode(parametro,
                            "alpha_DF"       = "Defensor",
                            "alpha_FW"       = "Delantero",
                            "alpha_MF_no_ofen" = "MC[no~ofensivo]",
                            "alpha_MF_ofen"    = "MC[ofensivo]"),
         parametro = factor(parametro,
                            levels = c("Defensor",
                                       "MC[no~ofensivo]",
                                       "MC[ofensivo]",
                                       "Delantero"))) %>%
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  tema_mio() +
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 14), 
    axis.text.x = element_text(size = 8)
  ) +
  labs(x = expression(alpha),
       y = NULL)

explicativas_modelo3 <- muestras_modelo3 %>%
  select(beta_distancia,
         beta_angulo,
         beta_interaccion) %>%
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = recode(parametro,
                            "beta_distancia" = "beta[1]",
                            "beta_angulo" = "beta[2]",
                            "beta_interaccion" = "beta[3]"),
         parametro = factor(parametro,
                            levels = rev(c("beta[1]",
                                           "beta[2]",
                                           "beta[3]")))) %>%
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  tema_mio() +
  theme(
    axis.text.y = element_text(size = 12, 
                               margin = margin(l = 48)),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12), 
    axis.text.x = element_text(size = 8)
  ) +
  labs(x = "Valor del parámetro",
       y = NULL)

variabilidad_modelo3 <- muestras_modelo3 %>%
  pivot_longer(cols = sigma_sq_gamma,
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = "sigma[u]") %>%
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_y_discrete(labels = NULL, 
                   expand = expansion(add = c(0.1, 0.2))) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1,
                                                   decimal.mark = ","),
                     limits = c(0.05, 0.40)) +
  tema_mio() +
  theme(
    axis.text.y = element_text(size = 12, 
                               margin = margin(l = 48)),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 14), 
    axis.text.x = element_text(size = 8), 
    plot.margin = margin(t = 5, b = 5, l = 40, r = 10)
  ) +
  labs(x = expression(sigma[u]),
       y = NULL)

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/interceptos_modelo1.pdf", 
       plot = interceptos_modelo1, 
       width = W, 
       height = H, 
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/explicativas_modelo1.pdf", 
       plot = explicativas_modelo1, 
       width = W, 
       height = H, 
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/interceptos_modelo2.pdf", 
       plot = interceptos_modelo2, 
       width = W, 
       height = H, 
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/explicativas_modelo2.pdf", 
       plot = explicativas_modelo2, 
       width = W, 
       height = H, 
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/interceptos_modelo3.pdf", 
       plot = interceptos_modelo3, 
       width = W, 
       height = H, 
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/explicativas_modelo3.pdf", 
       plot = explicativas_modelo3, 
       width = W, 
       height = H, 
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/variabilidad_modelo3.pdf", 
       plot = variabilidad_modelo3, 
       width = W, 
       height = H, 
       units = "in")

rm(list = c("interceptos_modelo1", 
            "interceptos_modelo2",
            "interceptos_modelo3",
            "explicativas_modelo1",
            "explicativas_modelo2",
            "explicativas_modelo3",
            "variabilidad_modelo3"))

#---- Densidades a posteriori (en un mismo panel) ----

# MODELO 1:

# Primero creo estos elementos para centrar los graficos:
step <- 0.5

beta_breaks <- seq(-0.5, 1.0, by = step)
rango_beta  <- range(c(muestras_modelo1$beta_distancia,
                       muestras_modelo1$beta_angulo,
                       muestras_modelo1$beta_interaccion))
pad_beta  <- diff(rango_beta) * 0.05
lim_beta  <- rango_beta + c(-1, 1) * pad_beta

rango_alpha <- range(muestras_modelo1$alpha)
delta <- round((mean(rango_alpha) - mean(rango_beta)) / step) * step

alpha_breaks <- beta_breaks + delta
lim_alpha    <- lim_beta + delta

# Creo los graficos:
interceptos_modelo1 <- muestras_modelo1 %>%
  pivot_longer(cols = alpha,
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = factor(parametro,
                            levels = rev(c("alpha")))) %>%
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_x_continuous(limits = lim_alpha,
                     breaks = alpha_breaks,
                     expand = c(0, 0), 
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_y_discrete(expand = expansion(add = c(0.1, 0.2)), 
                   labels = function(x) parse(text = x)) +
  tema_mio() +  
  theme(axis.text.y = element_text(size = 14),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8)) + 
  labs(x = expression(alpha),
       y = NULL)

explicativas_modelo1 <- muestras_modelo1 %>%
  select(beta_distancia,
         beta_angulo,
         beta_interaccion) %>% 
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = recode(parametro,
                            "beta_distancia" = "beta[1]",
                            "beta_angulo" = "beta[2]",
                            "beta_interaccion" = "beta[3]"),
         parametro = factor(parametro,
                            levels = rev(c("beta[1]",
                                           "beta[2]",
                                           "beta[3]")))) %>% 
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_x_continuous(limits = lim_beta,
                     breaks = beta_breaks,
                     expand = c(0, 0), 
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  tema_mio() +  
  theme(
    axis.text.y = element_text(size = 14),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12), 
    axis.text.x = element_text(size = 8)
  ) +
  labs(x = "Valor del parámetro",
       y = NULL)

# Los combino en una sola figura:
combinado <- interceptos_modelo1 / explicativas_modelo1 + 
  plot_layout(heights = c(1, 2))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/Combinados/parametros_mod1.pdf",
       plot = combinado,
       width = W,
       height = H*1.5,
       units = "in")

rm(step, 
   beta_breaks, 
   rango_beta, 
   pad_beta, 
   lim_beta, 
   rango_alpha, 
   delta, 
   alpha_breaks, 
   lim_alpha, 
   interceptos_modelo1, 
   explicativas_modelo1, 
   combinado)

# MODELO 2:

step <- 0.5

beta_breaks <- seq(-0.5, 1.0, by = step)
rango_beta  <- range(c(muestras_modelo2$beta_distancia,
                       muestras_modelo2$beta_angulo,
                       muestras_modelo2$beta_interaccion))
pad_beta  <- diff(rango_beta) * 0.05
lim_beta  <- rango_beta + c(-1, 1) * pad_beta

rango_alpha <- c(range(muestras_modelo2$alpha_DF)[1], range(muestras_modelo2$alpha_FW)[2])
delta <- round((mean(rango_alpha) - mean(rango_beta)) / step) * step

alpha_breaks <- beta_breaks + delta
lim_alpha    <- lim_beta + delta

interceptos_modelo2 <- muestras_modelo2 %>%
  select(alpha_DF,
         alpha_FW,
         alpha_MF_no_ofen,
         alpha_MF_ofen) %>% 
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = recode(parametro,
                            "alpha_DF"         = "alpha[DF]",
                            "alpha_FW"         = "alpha[DL]",
                            "alpha_MF_no_ofen" = "alpha[MCD]",
                            "alpha_MF_ofen"    = "alpha[MCO]"),
         parametro = factor(parametro,
                            levels = c("alpha[DF]",
                                       "alpha[MCD]",
                                       "alpha[MCO]",
                                       "alpha[DL]"))) %>% 
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_x_continuous(limits = lim_alpha,
                     breaks = alpha_breaks,
                     expand = c(0, 0), 
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  tema_mio() +  
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 8)
  ) +
  labs(x = NULL,
       y = NULL)

explicativas_modelo2 <- muestras_modelo2 %>%
  select(beta_distancia,
         beta_angulo,
         beta_interaccion) %>% 
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = recode(parametro,
                            "beta_distancia" = "beta[1]",
                            "beta_angulo" = "beta[2]",
                            "beta_interaccion" = "beta[3]"),
         parametro = factor(parametro,
                            levels = rev(c("beta[1]",
                                           "beta[2]",
                                           "beta[3]")))) %>% 
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_x_continuous(limits = lim_beta,
                     breaks = beta_breaks,
                     expand = c(0, 0), 
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  tema_mio() +  
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12), 
    axis.text.x = element_text(size = 8)
  ) +
  labs(x = "Valor del parámetro",
       y = NULL)

combinado <- interceptos_modelo2 / explicativas_modelo2 + 
  plot_layout(heights = c(4, 3))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/Combinados/parametros_mod2.pdf",
       plot = combinado,
       width = W,
       height = H*1.5,
       units = "in")

rm(step, 
   beta_breaks, 
   rango_beta, 
   pad_beta, 
   lim_beta, 
   rango_alpha,
   delta, 
   alpha_breaks, 
   lim_alpha, 
   interceptos_modelo2, 
   explicativas_modelo2, 
   combinado)

# MoDELO 3:

step <- 0.5

beta_breaks <- seq(-0.5, 1.0, by = step)
rango_beta  <- range(c(muestras_modelo3$beta_distancia,
                       muestras_modelo3$beta_angulo,
                       muestras_modelo3$beta_interaccion))
pad_beta  <- diff(rango_beta) * 0.05
lim_beta  <- rango_beta + c(-1, 1) * pad_beta

rango_alpha <- c(range(muestras_modelo3$alpha_DF)[1], range(muestras_modelo3$alpha_FW)[2])
delta_alpha <- round((mean(rango_alpha) - mean(rango_beta)) / step) * step
alpha_breaks <- beta_breaks + delta_alpha
lim_alpha    <- lim_beta + delta_alpha

interceptos_modelo3 <- muestras_modelo3 %>%
  select(alpha_DF,
         alpha_FW,
         alpha_MF_no_ofen,
         alpha_MF_ofen) %>%
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = recode(parametro,
                            "alpha_DF"         = "alpha[DF]",
                            "alpha_FW"         = "alpha[DL]",
                            "alpha_MF_no_ofen" = "alpha[MCD]",
                            "alpha_MF_ofen"    = "alpha[MCO]"),
         parametro = factor(parametro,
                            levels = c("alpha[DF]",
                                       "alpha[MCD]",
                                       "alpha[MCO]",
                                       "alpha[DL]"))) %>%
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_x_continuous(limits = lim_alpha,
                     breaks = alpha_breaks,
                     expand = c(0, 0), 
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  tema_mio() +
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 8)
  ) +
  labs(x = NULL,
       y = NULL)

explicativas_modelo3 <- muestras_modelo3 %>%
  select(beta_distancia,
         beta_angulo,
         beta_interaccion) %>%
  pivot_longer(cols = everything(),
               names_to = "parametro",
               values_to = "valor") %>%
  mutate(parametro = recode(parametro,
                            "beta_distancia" = "beta[1]",
                            "beta_angulo" = "beta[2]",
                            "beta_interaccion" = "beta[3]"),
         parametro = factor(parametro,
                            levels = rev(c("beta[1]",
                                           "beta[2]",
                                           "beta[3]")))) %>%
  ggplot(aes(x = valor, y = parametro)) +
  stat_halfeye(fill = "#C0504D",
               color = "#C0504D",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_x_continuous(limits = lim_beta,
                     breaks = beta_breaks,
                     expand = c(0, 0), 
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  tema_mio() +
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12),, 
    axis.text.x = element_text(size = 8)
  ) +
  labs(x = "Valor del parámetro",
       y = NULL)

combinado <- interceptos_modelo3 / explicativas_modelo3 + 
  plot_layout(heights = c(4, 3))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/Combinados/parametros_mod3.pdf",
       plot = combinado,
       width = W,
       height = H*1.5,
       units = "in")

rm(step, 
   beta_breaks, 
   rango_beta, 
   pad_beta, 
   lim_beta, 
   rango_alpha,
   delta_alpha, 
   alpha_breaks, 
   lim_alpha, 
   interceptos_modelo3, 
   explicativas_modelo3, 
   combinado)

#---- Intervalos de credibilidad para el parametro jugador ----

# Primero cargo las muestras de Colab de 40 jugadores.
intervalitos_por_jugador <- read_csv("Datos/ModeloColab/muestras_40_jugadores_equiespaciados.csv") %>%
  group_by(jugador) %>%
  summarise(media = mean(gamma),
            li = quantile(gamma, 0.05),
            ls = quantile(gamma, 0.95),
            .groups = "drop") %>%
  arrange(media) %>%
  mutate(jugador_num = row_number()) %>% 
  ggplot(aes(x = media, y = jugador_num)) +
  geom_linerange(aes(xmin = li, xmax = ls),
                 linewidth = 1,
                 color = "#C0504D") +
  geom_point(size = 2,
             color = "#C0504D") +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() +
  theme(panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10), 
                                    size = 12),
        axis.title = element_text(size = 12, 
                                  margin = margin(l=5, r = 5)),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        plot.margin = margin(t = 5, r = 10, b = 5, l = 15)) + 
  labs(x = expression(italic(u)),
       y = "Jugador seleccionado")

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/intervalitos_por_jugador.pdf",
       plot = intervalitos_por_jugador,
       width = W ,
       height = H,
       units = "in")

rm(intervalitos_por_jugador)

#---- Obtener los valores x e y de un disparo tipico ----

# Voy a calcular la distancia y el angulo de un disparo tipico. Voy al grafico
# de la cancha y me quedo con el cuadrado que tenga mayor frecuencia, luego 
# calculo el punto medio de ese cuadrado y saco el angulo y la distancia de 
# esos valores de x e y. 

# Hago el grafico
bins <- datos %>% 
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  stat_bin2d(binwidth = c(2.5, 2.5)) 

# Extraigo datos del gráfico
df_bins <- ggplot_build(bins)$data[[1]]

# Veo el bin con mayor frecuencia
df_bins %>% 
  arrange(desc(count)) %>% 
  slice(1)

top_bin <- df_bins %>% 
  arrange(desc(count)) %>% 
  slice(1) %>% 
  mutate(x_centro = (xmin + xmax) / 2,
         y_centro = (ymin + ymax) / 2)

x_medio <- top_bin %>% 
  select(x_centro) %>% 
  pull()

y_medio <- top_bin %>% 
  select(y_centro) %>% 
  pull()

shot_angle <- function(x, y) {
  
  a <- 7.32
  b <- sqrt(((105 - x)^2) + ((30.34 - y)^2))
  c <- sqrt(((105 - x)^2) + ((37.66 - y)^2))
  
  numerador <- (a^2) - (b^2) - (c^2)
  denominador <- (-2)*b*c
  
  angulo <- (acos(numerador/denominador) * 180)/pi
  
  return(angulo)
}

shot_distance <- function(x, y) {
  
  distancia <- sqrt((105 - x)^2 + (34 - y)^2)
  
  return(distancia)
}

# Devuelvo los valores de x_medio e y_medio (que van a estar en escala 0 a 100)
# a valores entre 0 y 105, y entre 0 y 68.

x_medio = x_medio / 100 * 105
y_medio = y_medio / 100 * 68

shot_distance(x_medio, y_medio)# 10.12288
shot_angle(x_medio, y_medio)   # 37.05253

dist_est <- (10.12288 - mean(datos$distancia))/sd(datos$distancia) # -1.109235
ang_est <- (37.05253 - mean(datos$angulo))/sd(datos$angulo) # 1.057081

rm(list = c("df_bins", 
            "top_bin", 
            "bins", 
            "x_medio", 
            "y_medio", 
            "shot_angle", 
            "shot_distance", 
            "dist_est",
            "ang_est"))

#---- Traigo las muestras de Python de los pi de este diparo tipico ----
muestras_tipico_mod1 <- read_csv("Datos/ModeloColab/muestras_tipico_mod1.csv")
muestras_tipico_mod2 <- read_csv("Datos/ModeloColab/muestras_tipico_mod2.csv")
muestras_tipico_mod3 <- read_csv("Datos/ModeloColab/muestras_tipico_mod3.gz")

#---- Posteriors de disparo tipico por modelo ----
tiro_promedio_mod1 <- muestras_tipico_mod1 %>%
  mutate(parametro = "xG") %>% 
  ggplot(aes(x = pi, y = parametro)) +
  stat_halfeye(fill = "#F3A447",
               color = "#F3A447",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_y_discrete(labels = function(x) rep("  ", length(x)), 
                   expand = expansion(add = c(0.1, 0.2))) +
  scale_x_continuous(limits = c(0.13, 0.19),
                     breaks = seq(0.13, 0.19, by = 0.02),
                     labels = scales::label_number(accuracy = 0.01,
                                                   decimal.mark = ",")) +
  tema_mio() +  
  theme(
    axis.text.y = element_text(size = 14),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12),
    axis.text.x = element_text(size = 8), 
    plot.margin = margin(t = 5, r = 5, b = 5, l = 26)
  ) + 
  labs(x = "Probabilidad de gol",
       y = NULL)

tiro_promedio_mod2 <- muestras_tipico_mod2 %>%
  mutate(posicion = recode(posicion,
                           "0" = "DF",
                           "1" = "DL",
                           "2" = "MCD",
                           "3" = "MCO"),
         posicion = factor(posicion,
                           levels = c("DF",
                                      "MCD",
                                      "MCO",
                                      "DL"))) %>% 
  ggplot(aes(x = pi, y = posicion, fill = posicion)) +
  stat_halfeye(alpha = 0.6,
               .width = 0.9,
               fill = "#F3A447",
               color = "#F3A447",
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") + 
  scale_y_discrete(labels = function(x) parse(text = x)) +
  scale_x_continuous(limits = c(0.09, 0.21),
                     breaks = seq(0.09, 0.21, by = 0.03),
                     labels = scales::label_number(accuracy = 0.01,
                                                   decimal.mark = ",")) +
  tema_mio() +  
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12), 
    axis.text.x = element_text(size = 8),
    legend.position = "none"
  ) +
  labs(x = "Probabilidad de gol",
       y = NULL)

tiro_promedio_mod3 <- muestras_tipico_mod3 %>%
  mutate(posicion = recode(posicion,
                           "DF" = "DF",
                           "MF_no_ofen" = "MCD",
                           "MF_ofen" = "MCO",
                           "FW" = "DL"),
         posicion = factor(posicion, 
                           levels = c("DF",
                                      "MCD",
                                      "MCO",
                                      "DL"))) %>%
  group_by(id_jugador, posicion) %>%
  summarise(media = mean(pi),
            li = quantile(pi, 0.05),  
            ls = quantile(pi, 0.95),   
            .groups = "drop") %>%
  arrange(posicion, media) %>%
  mutate(jugador_num = row_number()) %>% 
  ggplot(aes(x = media, y = jugador_num, color = posicion)) +
  geom_linerange(aes(xmin = li, xmax = ls),
                 linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, 0.55), 
                     breaks = seq(0, 0.55, by = 0.1), 
                     labels = scales::label_number(accuracy = 0.01, 
                                                   decimal.mark = ","))+
  scale_color_manual(name = "Posición",
                     values = cols,
                     labels = c("DF",
                                "MCD",
                                "MCO",
                                "DL")) +
  tema_mio() +
  theme(panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10), 
                                    size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        plot.margin = margin(t = 5, r = 5, b = 5, l = 11), 
        legend.position = c(0.82, 0.76),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.5, "cm")) + 
  labs(x = "Probabilidad de gol",
       y = "Jugador seleccionado")

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/tiro_promedio_mod1.pdf", 
       plot = tiro_promedio_mod1, 
       width = W, 
       height = H-1, 
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/tiro_promedio_mod2.pdf",
       plot = tiro_promedio_mod2,
       width = W,
       height = H,
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/tiro_promedio_mod3.pdf",
       plot = tiro_promedio_mod3,
       width = W,
       height = H,
       units = "in")

rm(tiro_promedio_mod1, 
   tiro_promedio_mod2, 
   tiro_promedio_mod3, 
   muestras_tipico_mod1, 
   muestras_tipico_mod2, 
   muestras_tipico_mod3)

#---- Obtener los valores x e y de un disparo no tan tipico ----

# Voy a calcular la distancia y el angulo de un disparo no tan tipico. 
# Voy al grafico de la cancha y me quedo con el cuadrado que tenga frecuencia
# amarilla, luego calculo el punto medio de ese cuadrado y saco el angulo 
# y la distancia de esos valores de x e y. 

# Hago el grafico
bins <- datos %>% 
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  stat_bin2d(binwidth = c(2.5, 2.5)) 

# Extraigo datos del gráfico
df_bins <- ggplot_build(bins)$data[[1]]

# Veo el bin con frecuencia media
df_bins %>% 
  arrange(desc(count)) %>% 
  slice(100)

top_bin <- df_bins %>% 
  arrange(desc(count)) %>% 
  slice(100) %>% 
  mutate(x_centro = (xmin + xmax) / 2,
         y_centro = (ymin + ymax) / 2)

x_medio <- top_bin %>% 
  select(x_centro) %>% 
  pull()

y_medio <- top_bin %>% 
  select(y_centro) %>% 
  pull()

shot_angle <- function(x, y) {
  
  a <- 7.32
  b <- sqrt(((105 - x)^2) + ((30.34 - y)^2))
  c <- sqrt(((105 - x)^2) + ((37.66 - y)^2))
  
  numerador <- (a^2) - (b^2) - (c^2)
  denominador <- (-2)*b*c
  
  angulo <- (acos(numerador/denominador) * 180)/pi
  
  return(angulo)
}

shot_distance <- function(x, y) {
  
  distancia <- sqrt((105 - x)^2 + (34 - y)^2)
  
  return(distancia)
}

x_medio = x_medio / 100 * 105
y_medio = y_medio / 100 * 68

shot_distance(x_medio, y_medio)# 7.818498
shot_angle(x_medio, y_medio)   # 45.18199

# Estandarizo: 
dist_est <- (7.818498 - mean(datos$distancia))/sd(datos$distancia) # -1.380092
ang_est <- (45.18199 - mean(datos$angulo))/sd(datos$angulo) # 1.648544

rm(list = c("df_bins", 
            "top_bin", 
            "bins", 
            "x_medio", 
            "y_medio", 
            "shot_angle", 
            "shot_distance", 
            "dist_est",
            "ang_est"))

#---- Traigo las muestras de Python de los pi de este diparo no tan tipico ----
muestras_no_tan_tipico_mod1 <- read_csv("Datos/ModeloColab/muestras_no_tan_tipico_mod1.csv")
muestras_no_tan_tipico_mod2 <- read_csv("Datos/ModeloColab/muestras_no_tan_tipico_mod2.csv")
muestras_no_tan_tipico_mod3 <- read_csv("Datos/ModeloColab/muestras_no_tan_tipico_mod3.gz")

#---- Posteriors de disparo no tan tipico por modelo ----
tiro_no_tan_tipico_mod1 <- muestras_no_tan_tipico_mod1 %>%
  mutate(parametro = "xG") %>% 
  ggplot(aes(x = pi, y = parametro)) +
  stat_halfeye(fill = "#F3A447",
               color = "#F3A447",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  scale_y_discrete(labels = function(x) rep("  ", length(x)), 
                   expand = expansion(add = c(0.1, 0.2))) +
  scale_x_continuous(
    limits = c(0.18, 0.27),
    breaks = seq(0.18, 0.27, by = 0.03),
    labels = scales::label_number(accuracy = 0.01, 
                                  decimal.mark = ",")) +
  tema_mio() +  
  theme(
    axis.text.y = element_text(size = 14),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12),
    axis.text.x = element_text(size = 8), 
    plot.margin = margin(t = 5, r = 5, b = 5, l = 26)
  ) + 
  labs(x = "Probabilidad de gol",
       y = NULL)

tiro_no_tan_tipico_mod2 <- muestras_no_tan_tipico_mod2 %>%
  mutate(posicion = recode(posicion,
                           "0" = "DF",
                           "1" = "DL",
                           "2" = "MCD",
                           "3" = "MCO"),
         posicion = factor(posicion,
                           levels = c("DF",
                                      "MCD",
                                      "MCO",
                                      "DL"))) %>% 
  ggplot(aes(x = pi, y = posicion, fill = posicion)) +
  stat_halfeye(alpha = 0.6,
               .width = 0.9,
               fill = "#F3A447",
               color = "#F3A447",
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") + 
  scale_y_discrete(labels = function(x) parse(text = x)) +
  scale_x_continuous(
    limits = c(0.12, 0.28),
    breaks = seq(0.12, 0.28, by = 0.04),
    labels = scales::label_number(accuracy = 0.01, 
                                  decimal.mark = ",")) +
  tema_mio() +  
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12), 
    axis.text.x = element_text(size = 8),
    legend.position = "none"
  ) +
  labs(x = "Probabilidad de gol",
       y = NULL)

tiro_no_tan_tipico_mod3 <- muestras_no_tan_tipico_mod3 %>%
  mutate(posicion = recode(posicion,
                           "DF" = "DF",
                           "MF_no_ofen" = "MCD",
                           "MF_ofen" = "MCO",
                           "FW" = "DL"),
         posicion = factor(posicion, 
                           levels = c("DF",
                                      "MCD",
                                      "MCO",
                                      "DL"))) %>%
  group_by(id_jugador, posicion) %>%
  summarise(media = mean(pi),
            li = quantile(pi, 0.05),  
            ls = quantile(pi, 0.95),   
            .groups = "drop") %>%
  arrange(posicion, media) %>%
  mutate(jugador_num = row_number()) %>% 
  ggplot(aes(x = media, y = jugador_num, color = posicion)) +
  geom_linerange(aes(xmin = li, xmax = ls),
                 linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    limits = c(0.05, 0.6),
    breaks = seq(0.05, 0.6, by = 0.1),
    labels = scales::label_number(accuracy = 0.01, 
                                  decimal.mark = ",")) +
  scale_color_manual(name = "Posición",
                     values = cols,
                     labels = c("DF",
                                "MCD",
                                "MCO",
                                "DL")) +
  tema_mio() +
  theme(panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10), 
                                    size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        plot.margin = margin(t = 5, r = 5, b = 5, l = 11), 
        legend.position = c(0.84, 0.76),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.5, "cm")) + 
  labs(x = "Probabilidad de gol",
       y = "Jugador seleccionado")

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/tiro_no_tan_tipico_mod1.pdf", 
       plot = tiro_no_tan_tipico_mod1, 
       width = W, 
       height = H-1, 
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/tiro_no_tan_tipico_mod2.pdf",
       plot = tiro_no_tan_tipico_mod2,
       width = W,
       height = H,
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/tiro_no_tan_tipico_mod3.pdf",
       plot = tiro_no_tan_tipico_mod3,
       width = W,
       height = H,
       units = "in")

rm(tiro_no_tan_tipico_mod1,
   tiro_no_tan_tipico_mod2,
   tiro_no_tan_tipico_mod3, 
   muestras_no_tan_tipico_mod1,
   muestras_no_tan_tipico_mod2,
   muestras_no_tan_tipico_mod3)

#---- Traigo las muestras de 6 jugadores especificos ----
muestras_gamma_6_jugadores_especificos <- read_csv("Datos/ModeloColab/muestras_6_jugadores_especificos.csv")

#---- Obtener probabilidad de gol para valores de las explicativas ----

# Tomo los valores de las explicativas de un NO gol de Brahian Aleman.
distancia_val <- -1.18901
angulo_val <- 1.274299
interaccion_val <- -1.515155
xG_val <- 0.1835718

df1 <- muestras_modelo1 %>%
  mutate(eta = alpha +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 1") %>%
  select(prob_gol, modelo)

df2 <- muestras_modelo2 %>%
  mutate(eta = alpha_FW +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 2") %>%
  select(prob_gol, modelo)

gamma_jugador1 <- muestras_gamma_6_jugadores_especificos %>% 
  filter(jugador == "882933") %>% 
  pull(gamma)

df3 <- muestras_modelo3 %>% 
  mutate(gamma = gamma_jugador1,
         eta = alpha_FW +
           gamma +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 3 (Martínez)") %>%
  select(prob_gol, modelo)

gamma_jugador2 <- muestras_gamma_6_jugadores_especificos %>% 
  filter(jugador == "245345") %>% 
  pull(gamma)

df4 <- muestras_modelo3 %>% 
  mutate(gamma = gamma_jugador2,
         eta = alpha_MF_no_ofen +
           gamma +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 3") %>%
  select(prob_gol, modelo)

disparo_aleman <- bind_rows(df1, df2, df4) %>%
  mutate(modelo = factor(modelo,
                         levels = rev(c("Modelo 1",
                                        "Modelo 2",
                                        "Modelo 3")))) %>% 
  ggplot(aes(x = prob_gol, y = modelo)) +
  stat_halfeye(normalize = "groups",
               fill = "#F3A447",
               color = "#F3A447",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  geom_vline(xintercept = xG_val,
             color = "#0B5A70",
             linetype = "33",
             linewidth = 1,
             show.legend = FALSE) +
  geom_segment(data = tibble(tipo = "Stats Perform"),
               aes(x = 0, xend = 1,
                   y = 0, yend = 0,
                   color = tipo,
                   linetype = tipo),
               inherit.aes = FALSE,
               linewidth = 1) +
  scale_color_manual(values = c("Stats Perform" = "#0B5A70"),
                     name = NULL) +
  scale_linetype_manual(values = c("Stats Perform" = "31"),
                        name = NULL) +
  guides(linetype = "none",
         color = guide_legend(override.aes = list(
           linetype = "31",
           linewidth = 1))) +
  coord_cartesian(xlim = c(0, 0.5),
                  ylim = c(1, 3.4)) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  labs(x = "Probabilidad de gol",
       y = NULL) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10),
                                    size = 12),
        axis.text.x = element_text(size = 8),
        legend.position = c(0.8, 0.82),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black")) 

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/disparo_aleman.pdf",
       plot = disparo_aleman,
       width = W ,
       height = H,
       units = "in")

# Tomo los valores de las explicativas de un gol de Maravilla.
distancia_val <- -1.4064777
angulo_val <- 2.09304542
interaccion_val <- -2.94382176
xG_val <- 0.16143404

df1 <- muestras_modelo1 %>%
  mutate(eta = alpha +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 1") %>%
  select(prob_gol, modelo)

df2 <- muestras_modelo2 %>%
  mutate(eta = alpha_FW +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 2") %>%
  select(prob_gol, modelo)

gamma_jugador1 <- muestras_gamma_6_jugadores_especificos %>% 
  filter(jugador == "882933") %>% 
  pull(gamma)

df3 <- muestras_modelo3 %>% 
  mutate(gamma = gamma_jugador1,
         eta = alpha_FW +
           gamma +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 3") %>%
  select(prob_gol, modelo)

gamma_jugador2 <- muestras_gamma_6_jugadores_especificos %>% 
  filter(jugador == "245345") %>% 
  pull(gamma)

df4 <- muestras_modelo3 %>% 
  mutate(gamma = gamma_jugador2,
         eta = alpha_MF_no_ofen +
           gamma +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 3 (Alemán)") %>%
  select(prob_gol, modelo)

disparo_maravilla <- bind_rows(df1, df2, df3) %>%
  mutate(modelo = factor(modelo,
                         levels = rev(c("Modelo 1",
                                        "Modelo 2",
                                        "Modelo 3")))) %>% 
  ggplot(aes(x = prob_gol, y = modelo)) +
  stat_halfeye(normalize = "groups",
               fill = "#F3A447",
               color = "#F3A447",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  geom_vline(xintercept = xG_val,
             color = "#0B5A70",
             linetype = "33",
             linewidth = 1,
             show.legend = FALSE) +
  geom_segment(data = tibble(tipo = "Stats Perform"),
               aes(x = 0, xend = 1,
                   y = 0, yend = 0,
                   color = tipo,
                   linetype = tipo),
               inherit.aes = FALSE,
               linewidth = 1) +
  scale_color_manual(values = c("Stats Perform" = "#0B5A70"),
                     name = NULL) +
  scale_linetype_manual(values = c("Stats Perform" = "31"),
                        name = NULL) +
  guides(linetype = "none",
         color = guide_legend(override.aes = list(
           linetype = "31",
           linewidth = 1))) +
  coord_cartesian(xlim = c(0, 0.5),
                  ylim = c(1, 3.4)) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10), 
                                    size = 12), 
        axis.text.x = element_text(size = 8), 
        legend.position = c(0.8, 0.82),   
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(color = "black")) +
  labs(x = "Probabilidad de gol",
       y = NULL)

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/disparo_maravilla.pdf",
       plot = disparo_maravilla,
       width = W ,
       height = H,
       units = "in")

# Tomo los valores de las explicativas de un gol de Merentiel.
distancia_val <- -0.33656427
angulo_val <- 0.0846789
interaccion_val <- -0.02849989
xG_val <- 0.31888145

df1 <- muestras_modelo1 %>%
  mutate(eta = alpha +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 1") %>%
  select(prob_gol, modelo)

df2 <- muestras_modelo2 %>%
  mutate(eta = alpha_FW +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 2") %>%
  select(prob_gol, modelo)

gamma_jugador1 <- muestras_gamma_6_jugadores_especificos %>% 
  filter(jugador == "882933") %>% 
  pull(gamma)

df3 <- muestras_modelo3 %>% 
  mutate(gamma = gamma_jugador1,
         eta = alpha_FW +
           gamma +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 3 (Martínez)") %>%
  select(prob_gol, modelo)

gamma_jugador2 <- muestras_gamma_6_jugadores_especificos %>% 
  filter(jugador == "826418") %>% 
  pull(gamma)

df4 <- muestras_modelo3 %>% 
  mutate(gamma = gamma_jugador2,
         eta = alpha_MF_no_ofen +
           gamma +
           beta_distancia * distancia_val +
           beta_angulo * angulo_val +
           beta_interaccion * interaccion_val,
         prob_gol = plogis(eta),
         modelo = "Modelo 3") %>%
  select(prob_gol, modelo)

disparo_merentiel <- bind_rows(df1, df2, df4) %>%
  mutate(modelo = factor(modelo,
                         levels = rev(c("Modelo 1",
                                        "Modelo 2",
                                        "Modelo 3")))) %>% 
  ggplot(aes(x = prob_gol, y = modelo)) +
  stat_halfeye(normalize = "groups",
               fill = "#F3A447",
               color = "#F3A447",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  geom_vline(xintercept = xG_val,
             color = "#0B5A70",
             linetype = "33",
             linewidth = 1,
             show.legend = FALSE) +
  geom_segment(data = tibble(tipo = "Stats Perform"),
               aes(x = 0, xend = 1,
                   y = 0, yend = 0,
                   color = tipo,
                   linetype = tipo),
               inherit.aes = FALSE,
               linewidth = 1) +
  scale_color_manual(values = c("Stats Perform" = "#0B5A70"),
                     name = NULL) +
  scale_linetype_manual(values = c("Stats Perform" = "31"),
                        name = NULL) +
  guides(linetype = "none",
         color = guide_legend(override.aes = list(
           linetype = "31",
           linewidth = 1))) +
  coord_cartesian(xlim = c(0, 0.5),
                  ylim = c(1, 3.4)) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10), 
                                    size = 12), 
        axis.text.x = element_text(size = 8), 
        legend.position = c(0.8, 0.82),   
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(color = "grey")) +
  labs(x = "Probabilidad de gol",
       y = NULL)

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/disparo_merentiel.pdf",
       plot = disparo_merentiel,
       width = W ,
       height = H,
       units = "in")

rm(list = c("df1", 
            "df2",
            "df3",
            "df4",
            "gamma_jugador1",
            "gamma_jugador2",
            "angulo_val",
            "distancia_val",
            "interaccion_val",
            "xG_val", 
            "disparo_aleman", 
            "disparo_maravilla", 
            "disparo_merentiel"))

#---- Cargo datos de partido nuevo (Racing vs Union) ----
disparos_racing_union <- read_csv("Datos/Extras/disparos_nuevos_racing_union.csv") %>% 
  mutate(id_disparo = row_number())

#---- xG para un disparo nuevo de Maravilla ----

x_nuevo <- disparos_racing_union %>% 
  filter(playerName == "Adrian Martinez", 
         gol == "Si") %>% 
  pull(x)

y_nuevo <-  disparos_racing_union %>% 
  filter(playerName == "Adrian Martinez", 
         gol == "Si") %>% 
  pull(y)

shot_angle <- function(x, y) {
  
  a <- 7.32
  b <- sqrt(((105 - x)^2) + ((30.34 - y)^2))
  c <- sqrt(((105 - x)^2) + ((37.66 - y)^2))
  
  numerador <- (a^2) - (b^2) - (c^2)
  denominador <- (-2)*b*c
  
  angulo <- (acos(numerador/denominador) * 180)/pi
  
  return(angulo)
}

shot_distance <- function(x, y) {
  
  distancia <- sqrt((105 - x)^2 + (34 - y)^2)
  
  return(distancia)
}

dist_nuevo <- shot_distance(x_nuevo, y_nuevo)# 7.07721
ang_nuevo <- shot_angle(x_nuevo, y_nuevo)   # 54.56401

# Estandarizo los valores:
dist_nuevo_est <- (dist_nuevo - mean(datos$distancia)) / sd(datos$distancia)
ang_nuevo_est <- (ang_nuevo - mean(datos$angulo)) / sd(datos$angulo)

xG_nuevo <-  disparos_racing_union %>% 
  filter(playerName == "Adrian Martinez", 
         gol == "Si") %>% 
  pull(expectedGoals)

# Distancia = -1.467223
# Angulo = 2.331137
# xG = 0.3412333

rm(list = c("x_nuevo", 
            "y_nuevo", 
            "shot_angle", 
            "shot_distance", 
            "dist_nuevo", 
            "ang_nuevo"))

pis_nuevo_mod1 <- muestras_modelo1 %>%
  mutate(eta = alpha +
           beta_angulo * ang_nuevo_est +
           beta_distancia * dist_nuevo_est +
           beta_interaccion * ang_nuevo_est * dist_nuevo_est,
         pi = plogis(eta)) %>% 
  select(pi)

pis_nuevo_mod2 <- muestras_modelo2 %>%
  mutate(eta = alpha_FW +
           beta_angulo * ang_nuevo_est +
           beta_distancia * dist_nuevo_est +
           beta_interaccion * ang_nuevo_est * dist_nuevo_est,
         pi = plogis(eta)) %>% 
  select(pi)

gamma_maravilla <- muestras_gamma_6_jugadores_especificos %>% 
  filter(jugador == "882933") %>% 
  select(gamma)

muestras_modelo3_maravilla <- cbind(muestras_modelo3, gamma_maravilla)

pis_nuevo_mod3 <- muestras_modelo3_maravilla %>%
  mutate(eta = alpha_FW +
           gamma +
           beta_angulo * ang_nuevo_est +
           beta_distancia * dist_nuevo_est +
           beta_interaccion * ang_nuevo_est * dist_nuevo_est,
         pi = plogis(eta)) %>% 
  select(pi)

disparo_nuevo_maravilla <- bind_rows(pis_nuevo_mod1 %>% mutate(modelo = "Modelo 1"),
                                     pis_nuevo_mod2 %>% mutate(modelo = "Modelo 2"),
                                     pis_nuevo_mod3 %>% mutate(modelo = "Modelo 3")) %>%
  mutate(modelo = factor(modelo,
                         levels = rev(c("Modelo 1",
                                        "Modelo 2",
                                        "Modelo 3")))) %>% 
  ggplot(aes(x = pi, y = modelo)) +
  stat_halfeye(normalize = "groups",
               fill = "#F3A447",
               color = "#F3A447",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  geom_vline(xintercept = xG_nuevo,
             color = "#0B5A70",
             linetype = "dashed",
             linewidth = 1,
             show.legend = FALSE) +
  geom_segment(
    data = tibble(tipo = "Stats Perform"),
    aes(x = 0, xend = 1,
        y = 0, yend = 0,
        color = tipo,
        linetype = tipo),
    inherit.aes = FALSE,
    linewidth = 1) +
  scale_color_manual(values = c("Stats Perform" = "#0B5A70"),
                     name = NULL) +
  scale_linetype_manual(values = c("Stats Perform" = "dashed"),
                        name = NULL) +
  guides(linetype = "none",
         color = guide_legend(override.aes = list(
           linetype = "31",
           linewidth = 1))) +
  coord_cartesian(xlim = c(0, 0.5), 
                  ylim = c(1, 3.4)) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10),
                                    size = 12),
        axis.text.x = element_text(size = 8),
        legend.position = c(0.2, 0.82),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black")) +
  labs(x = "Probabilidad de gol",
       y = NULL)

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/disparo_nuevo_maravilla.pdf",
       plot = disparo_nuevo_maravilla,
       width = W,
       height = H,
       units = "in")

rm(list = c("dist_nuevo_est", 
            "ang_nuevo_est", 
            "xG_nuevo", 
            "pis_nuevo_mod1", 
            "pis_nuevo_mod2", 
            "pis_nuevo_mod3", 
            "gamma_maravilla", 
            "muestras_modelo3_maravilla", 
            "disparo_nuevo_maravilla"))

#---- Base para un disparo nuevo ----

# Cargo los datos:
nc_3 <- nc_open("Datos/ModeloColab/muestras_modelo_3_chequear.nc")

# ¿Tengo todo lo que necesito?
names(nc_3$var)
# Si. La posicion 2 es la posicion de Delantero. 

# Calculo la distancia y angulo para un disparo de Julian Palacios: 
id <- 8

x_nuevo_jugador <- disparos_racing_union %>% 
  filter(id_disparo == id) %>% 
  pull(x)

y_nuevo_jugador <-  disparos_racing_union %>% 
  filter(id_disparo == id) %>% 
  pull(y)

shot_angle <- function(x, y) {
  
  a <- 7.32
  b <- sqrt(((105 - x)^2) + ((30.34 - y)^2))
  c <- sqrt(((105 - x)^2) + ((37.66 - y)^2))
  
  numerador <- (a^2) - (b^2) - (c^2)
  denominador <- (-2)*b*c
  
  angulo <- (acos(numerador/denominador) * 180)/pi
  
  return(angulo)
}

shot_distance <- function(x, y) {
  
  distancia <- sqrt((105 - x)^2 + (34 - y)^2)
  
  return(distancia)
}

dist_nuevo <- shot_distance(x_nuevo_jugador, y_nuevo_jugador)# 11.3875
ang_nuevo <- shot_angle(x_nuevo_jugador, y_nuevo_jugador)   # 21.846

# Estandarizo los valores:
dist_nuevo_est <- (dist_nuevo - mean(datos$distancia)) / sd(datos$distancia) # -0.9605909
ang_nuevo_est <- (ang_nuevo - mean(datos$angulo)) / sd(datos$angulo) # -0.04927783

xG_nuevo_jugador <-  disparos_racing_union %>% 
  filter(id_disparo == id) %>% 
  pull(expectedGoals)

rm(shot_angle, 
   shot_distance, 
   y_nuevo_jugador, 
   x_nuevo_jugador, 
   id, 
   dist_nuevo, 
   ang_nuevo)

# Obtengo el df: 

# Extraigo los parámetros:
alpha <- ncvar_get(nc_3, "alpha")
gamma <- ncvar_get(nc_3, "gamma")

alpha_fw <- c(alpha[2,,])

beta_dist <- c(ncvar_get(nc_3, "beta_distancia"))
beta_ang  <- c(ncvar_get(nc_3, "beta_angulo"))
beta_int  <- c(ncvar_get(nc_3, "beta_interaccion"))

sigma_u <- sqrt(
  c(ncvar_get(nc_3, "sigma_sq_gamma"))
)

# CASO 1: jugador promedio.

eta_promedio <- (alpha_fw +
                   beta_dist * dist_nuevo_est +
                   beta_ang * ang_nuevo_est +
                   beta_int * dist_nuevo_est * ang_nuevo_est)

p_promedio <- plogis(eta_promedio)

# CASO 2: nuevo jugador paramétrico.

u_param <- rnorm(n = 16000,
                 mean = 0,
                 sd = sigma_u)

eta_param <- (alpha_fw +
                u_param +
                beta_dist * dist_nuevo_est +
                beta_ang * ang_nuevo_est +
                beta_int * dist_nuevo_est * ang_nuevo_est)

p_param <- plogis(eta_param)

# CASO 3: nuevo jugador empírico.

gamma_long <- matrix(aperm(gamma, c(2,3,1)),
                     nrow = 16000,
                     ncol = 1052)

set.seed(123)
jugador_sorteado <- sample(1:1052,
                           size = 16000,
                           replace = TRUE)

u_emp <- gamma_long[cbind(1:16000,
                          jugador_sorteado)]

eta_emp <- (alpha_fw +
              u_emp +
              beta_dist * dist_nuevo_est +
              beta_ang  * ang_nuevo_est +
              beta_int * dist_nuevo_est * ang_nuevo_est)

p_emp <- plogis(eta_emp)

# Uno todo en un df:
posteriores <- tibble(promedio = p_promedio,
                      parametrico = p_param,
                      empirico = p_emp)

# Elimino los elementos:
rm(u_param, 
   u_emp, 
   sigma_u, 
   p_promedio, 
   p_param, 
   p_emp, 
   jugador_sorteado, 
   gamma, 
   eta_promedio, 
   eta_param, 
   eta_emp, 
   dist_nuevo_est, 
   beta_int, 
   beta_dist, 
   beta_ang, 
   ang_nuevo_est, 
   alpha_fw, 
   alpha, 
   gamma_long, 
   nc_3)

#---- xG para un disparo de un jugador nuevo ----

x_nuevo <- disparos_racing_union %>% 
  filter(id_disparo == "8") %>% 
  pull(x)

y_nuevo <-  disparos_racing_union %>% 
  filter(id_disparo == "8") %>% 
  pull(y)

shot_angle <- function(x, y) {
  
  a <- 7.32
  b <- sqrt(((105 - x)^2) + ((30.34 - y)^2))
  c <- sqrt(((105 - x)^2) + ((37.66 - y)^2))
  
  numerador <- (a^2) - (b^2) - (c^2)
  denominador <- (-2)*b*c
  
  angulo <- (acos(numerador/denominador) * 180)/pi
  
  return(angulo)
}

shot_distance <- function(x, y) {
  
  distancia <- sqrt((105 - x)^2 + (34 - y)^2)
  
  return(distancia)
}

dist_nuevo <- shot_distance(x_nuevo, y_nuevo)# 11.3875
ang_nuevo <- shot_angle(x_nuevo, y_nuevo)   # 21.846

# Estandarizo los valores:
dist_nuevo_est <- (dist_nuevo - mean(datos$distancia)) / sd(datos$distancia)
ang_nuevo_est <- (ang_nuevo - mean(datos$angulo)) / sd(datos$angulo)

# Distancia = -0.9605909
# Angulo = -0.04927783
# xG = 0.1662833

rm(list = c("x_nuevo", 
            "y_nuevo", 
            "shot_angle", 
            "shot_distance", 
            "dist_nuevo", 
            "ang_nuevo"))

pis_nuevo_mod1 <- muestras_modelo1 %>%
  mutate(eta = alpha +
           beta_angulo * ang_nuevo_est +
           beta_distancia * dist_nuevo_est +
           beta_interaccion * ang_nuevo_est * dist_nuevo_est,
         pi = plogis(eta)) %>% 
  select(pi)

pis_nuevo_mod2 <- muestras_modelo2 %>%
  mutate(eta = alpha_FW +
           beta_angulo * ang_nuevo_est +
           beta_distancia * dist_nuevo_est +
           beta_interaccion * ang_nuevo_est * dist_nuevo_est,
         pi = plogis(eta)) %>% 
  select(pi)

pis_nuevo_mod3 <- posteriores %>%
  select(pi = empirico)

disparo_nuevo_jugador <- bind_rows(pis_nuevo_mod1 %>% mutate(modelo = "Modelo 1"),
                                   pis_nuevo_mod2 %>% mutate(modelo = "Modelo 2"),
                                   pis_nuevo_mod3 %>% mutate(modelo = "Modelo 3")) %>%
  mutate(modelo = factor(modelo,
                         levels = rev(c("Modelo 1",
                                        "Modelo 2",
                                        "Modelo 3")))) %>% 
  ggplot(aes(x = pi, y = modelo)) +
  stat_halfeye(normalize = "groups",
               fill = "#F3A447",
               color = "#F3A447",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  geom_vline(xintercept = xG_nuevo_jugador,
             color = "#0B5A70",
             linetype = "dashed",
             linewidth = 1,
             show.legend = FALSE) +
  geom_segment(
    data = tibble(tipo = "Stats Perform"),
    aes(x = 0, xend = 1,
        y = 0, yend = 0,
        color = tipo,
        linetype = tipo),
    inherit.aes = FALSE,
    linewidth = 1) +
  scale_color_manual(values = c("Stats Perform" = "#0B5A70"),
                     name = NULL) +
  scale_linetype_manual(values = c("Stats Perform" = "dashed"),
                        name = NULL) +
  guides(linetype = "none",
         color = guide_legend(override.aes = list(
           linetype = "31",
           linewidth = 1))) +
  coord_cartesian(xlim = c(0, 0.5), 
                  ylim = c(1, 3.4)) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10),
                                    size = 12),
        axis.text.x = element_text(size = 8),
        legend.position = c(0.8, 0.82),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black")) +
  labs(x = "Probabilidad de gol",
       y = NULL)

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/disparo_jugador_nuevo.pdf",
       plot = disparo_nuevo_jugador,
       width = W,
       height = H,
       units = "in")

rm(list = c("dist_nuevo_est", 
            "ang_nuevo_est", 
            "xG_nuevo_jugador", 
            "pis_nuevo_mod1", 
            "pis_nuevo_mod2", 
            "pis_nuevo_mod3", 
            "disparo_nuevo_jugador", 
            "posteriores", 
            "disparos_racing_union"))

#---- Traigo de Python las muestras para graficos de acumulado jugador ----
pp1_goles_maravilla <- read_csv("Datos/ModeloColab/pp1_goles_maravilla.csv") %>% 
  mutate(pp_goles = pp1_goles_maravilla) %>% 
  pull(pp_goles)
pp1_goles_merentiel <- read_csv("Datos/ModeloColab/pp1_goles_merentiel.csv") %>% 
  mutate(pp_goles = pp1_goles_merentiel) %>% 
  pull(pp_goles)
pp1_goles_dominguez <- read_csv("Datos/ModeloColab/pp1_goles_dominguez.csv") %>% 
  mutate(pp_goles = pp1_goles_dominguez) %>% 
  pull(pp_goles)

pp2_goles_maravilla <- read_csv("Datos/ModeloColab/pp2_goles_maravilla.csv") %>% 
  mutate(pp_goles = pp2_goles_maravilla) %>% 
  pull(pp_goles)
pp2_goles_merentiel <- read_csv("Datos/ModeloColab/pp2_goles_merentiel.csv") %>% 
  mutate(pp_goles = pp2_goles_merentiel) %>% 
  pull(pp_goles)
pp2_goles_dominguez <- read_csv("Datos/ModeloColab/pp2_goles_dominguez.csv") %>% 
  mutate(pp_goles = pp2_goles_dominguez) %>% 
  pull(pp_goles)

pp3_goles_maravilla <- read_csv("Datos/ModeloColab/pp3_goles_maravilla.csv") %>% 
  mutate(pp_goles = pp3_goles_maravilla) %>% 
  pull(pp_goles)
pp3_goles_merentiel <- read_csv("Datos/ModeloColab/pp3_goles_merentiel.csv") %>% 
  mutate(pp_goles = pp3_goles_merentiel) %>% 
  pull(pp_goles)
pp3_goles_dominguez <- read_csv("Datos/ModeloColab/pp3_goles_dominguez.csv") %>% 
  mutate(pp_goles = pp3_goles_dominguez) %>% 
  pull(pp_goles)

#---- Predictiva a posteriori por jugador y total ----

ancho_barra <- 0.5

breaks_bastones <- sort(c(0:50 - ancho_barra/2, 0:50 + ancho_barra/2))

# Primero para Maravilla:

goles_maravilla <- datos %>% 
  filter(id_jugador == 882933) %>% 
  group_by(id_jugador) %>% 
  summarise(goles = sum(gol)) %>% 
  pull(goles)

xG_maravilla <- datos %>% 
  filter(id_jugador == 882933) %>% 
  group_by(id_jugador) %>% 
  summarise(xG = sum(xG)) %>% 
  pull(xG)

xGs <- datos %>% 
  filter(id_jugador == 882933) %>% 
  pull(xG)

S <- 16000

cantidad_goles_opta <- numeric(S)

for (s in seq_len(S)) {
  cantidad_goles_opta[s] <- sum(rbinom(length(xGs), 1, xGs))
}

acumulado_maravilla <- tibble(pp_goles = c(pp1_goles_maravilla, 
                                           pp2_goles_maravilla, 
                                           pp3_goles_maravilla, 
                                           cantidad_goles_opta),
                              grupo = rep(c("Modelo 1", 
                                            "Modelo 2",
                                            "Modelo 3", 
                                            "Stats Perform"), each = 16000)) %>%
  mutate(grupo = factor(grupo, levels = c("Stats Perform",
                                          "Modelo 3",
                                          "Modelo 2",
                                          "Modelo 1"))) %>%
  ggplot(aes(x = pp_goles, y = grupo)) +
  stat_histinterval(fill = "#F3A447",
                    color = "#F3A447",
                    slab_color = "#F3A447",
                    slab_linewidth = 0.4,
                    outline_bars = TRUE,
                    alpha = 0.7,
                    .width = 0.9,
                    point_interval = mean_qi,
                    interval_color = "black",
                    point_color = "black",
                    point_size = 2,
                    breaks = breaks_bastones) +
  geom_vline(xintercept = goles_maravilla, 
             linetype = "dashed", 
             color = "#9C0824",
             linewidth = 1,
             show.legend = FALSE) +
  geom_segment(data = tibble(tipo = "Cantidad observada"),
               aes(x = 0, xend = 1, y = 0, yend = 0, color = tipo),
               inherit.aes = FALSE,
               linetype = "31",
               linewidth = 1) +
  scale_color_manual(values = c("Cantidad observada" = "#9C0824"),
                     labels = c("Cantidad observada" = "Cantidad real"),
                     name = NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.2))) +
  scale_x_continuous(limits = c(0, 46), breaks = seq(0, 46, by = 10)) +
  coord_cartesian(ylim = c(1, 4.4)) +
  labs(x = "Cantidad de goles", y = NULL) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
        axis.title.x = element_text(margin = margin(t = 10), 
                                    size = 12), 
        axis.text.x = element_text(size = 8),
        legend.position = c(0.80, 0.88),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.5, "cm"))

# Despues para Merentiel:

goles_merentiel <- datos %>% 
  filter(id_jugador == 826418) %>% 
  group_by(id_jugador) %>% 
  summarise(goles = sum(gol)) %>% 
  pull(goles)

xG_merentiel <- datos %>% 
  filter(id_jugador == 826418) %>% 
  group_by(id_jugador) %>% 
  summarise(xG = sum(xG)) %>% 
  pull(xG)

xGs <- datos %>% 
  filter(id_jugador == 826418) %>% 
  pull(xG)

S <- 16000

cantidad_goles_opta <- numeric(S)

for (s in seq_len(S)) {
  cantidad_goles_opta[s] <- sum(rbinom(length(xGs), 1, xGs))
}

acumulado_merentiel <- tibble(pp_goles = c(pp1_goles_merentiel, 
                                           pp2_goles_merentiel, 
                                           pp3_goles_merentiel, 
                                           cantidad_goles_opta),
                              grupo = rep(c("Modelo 1", 
                                            "Modelo 2",
                                            "Modelo 3", 
                                            "Stats Perform"), each = 16000)) %>%
  mutate(grupo = factor(grupo, levels = c("Stats Perform",
                                          "Modelo 3",
                                          "Modelo 2",
                                          "Modelo 1"))) %>%
  ggplot(aes(x = pp_goles, y = grupo)) +
  stat_histinterval(fill = "#F3A447",
                    color = "#F3A447",
                    slab_color = "#F3A447",
                    slab_linewidth = 0.4,
                    outline_bars = TRUE,
                    alpha = 0.7,
                    .width = 0.9,
                    point_interval = mean_qi,
                    interval_color = "black",
                    point_color = "black",
                    point_size = 2,
                    breaks = breaks_bastones) +
  geom_vline(xintercept = goles_merentiel, 
             linetype = "dashed", 
             color = "#9C0824",
             linewidth = 1,
             show.legend = FALSE) +
  geom_segment(data = tibble(tipo = "Cantidad observada"),
               aes(x = 0, xend = 1, y = 0, yend = 0, color = tipo),
               inherit.aes = FALSE,
               linetype = "31",
               linewidth = 1) +
  scale_color_manual(values = c("Cantidad observada" = "#9C0824"),
                     labels = c("Cantidad observada" = "Cantidad real"),
                     name = NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.2))) +
  scale_x_continuous(limits = c(0, 46), breaks = seq(0, 46, by = 10)) +
  coord_cartesian(ylim = c(1, 4.4)) +
  labs(x = "Cantidad de goles", y = NULL) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
        axis.title.x = element_text(margin = margin(t = 10), 
                                    size = 12), 
        axis.text.x = element_text(size = 8),
        legend.position = c(0.80, 0.88),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.5, "cm"))

# Por ultimo para Dominguez:

goles_dominguez <- datos %>% 
  filter(id_jugador == 1316088) %>% 
  group_by(id_jugador) %>% 
  summarise(goles = sum(gol)) %>% 
  pull(goles)

xG_dominguez <- datos %>% 
  filter(id_jugador == 1316088) %>% 
  group_by(id_jugador) %>% 
  summarise(xG = sum(xG)) %>% 
  pull(xG)

xGs <- datos %>% 
  filter(id_jugador == 1316088) %>% 
  pull(xG)

S <- 16000

cantidad_goles_opta <- numeric(S)

for (s in seq_len(S)) {
  cantidad_goles_opta[s] <- sum(rbinom(length(xGs), 1, xGs))
}

acumulado_dominguez <- tibble(pp_goles = c(pp1_goles_dominguez, 
                                           pp2_goles_dominguez, 
                                           pp3_goles_dominguez, 
                                           cantidad_goles_opta),
                              grupo = rep(c("Modelo 1", 
                                            "Modelo 2",
                                            "Modelo 3", 
                                            "Stats Perform"), each = 16000)) %>%
  mutate(grupo = factor(grupo, levels = c("Stats Perform",
                                          "Modelo 3",
                                          "Modelo 2",
                                          "Modelo 1"))) %>%
  ggplot(aes(x = pp_goles, y = grupo)) +
  stat_histinterval(fill = "#F3A447",
                    color = "#F3A447",
                    slab_color = "#F3A447",
                    slab_linewidth = 0.4,
                    outline_bars = TRUE,
                    alpha = 0.7,
                    .width = 0.9,
                    point_interval = mean_qi,
                    interval_color = "black",
                    point_color = "black",
                    point_size = 2,
                    breaks = breaks_bastones) +
  geom_vline(xintercept = goles_dominguez, 
             linetype = "dashed", 
             color = "#9C0824",
             linewidth = 1,
             show.legend = FALSE) +
  geom_segment(data = tibble(tipo = "Cantidad observada"),
               aes(x = 0, xend = 1, y = 0, yend = 0, color = tipo),
               inherit.aes = FALSE,
               linetype = "31",
               linewidth = 1) +
  scale_color_manual(values = c("Cantidad observada" = "#9C0824"),
                     labels = c("Cantidad observada" = "Cantidad real"),
                     name = NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.2))) +
  scale_x_continuous(limits = c(0, 46), breaks = seq(0, 46, by = 10)) +
  coord_cartesian(ylim = c(1, 4.4)) +
  labs(x = "Cantidad de goles", y = NULL) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
        axis.title.x = element_text(margin = margin(t = 10), 
                                    size = 12), 
        axis.text.x = element_text(size = 8),
        legend.position = c(0.80, 0.88),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.5, "cm"))

# Para todos juntos: 

goles_totales_modelo1 <- read.csv("Datos/ModeloColab/pp1_goles_totales.csv")
goles_totales_modelo2 <- read.csv("Datos/ModeloColab/pp2_goles_totales.csv")
goles_totales_modelo3 <- read.csv("Datos/ModeloColab/pp3_goles_totales.csv")

real <- sum(datos$gol)

xGs <- datos %>% 
  pull(xG)

S <- 16000

cantidad_goles_opta <- numeric(S)

set.seed(123)

for (s in seq_len(S)) {
  cantidad_goles_opta[s] <- sum(rbinom(length(xGs), 1, xGs))
}

paso <- 15
ancho_barra <- 0

rango_datos <- range(c(goles_totales_modelo1$pp_goles,
                       goles_totales_modelo2$pp_goles,
                       goles_totales_modelo3$pp_goles, 
                       cantidad_goles_opta))

minimo <- floor(rango_datos[1] / paso) * paso
maximo <- ceiling(rango_datos[2] / paso) * paso

centros <- seq(minimo, maximo, by = paso)
breaks_bastones <- sort(c(centros - paso * ancho_barra/2, 
                          centros + paso * ancho_barra/2))

acumulado <- tibble(pp_goles = c(goles_totales_modelo1$pp_goles,
                                 goles_totales_modelo2$pp_goles,
                                 goles_totales_modelo3$pp_goles, 
                                 cantidad_goles_opta),
                    grupo = rep(c("Modelo 1", 
                                  "Modelo 2",
                                  "Modelo 3", 
                                  "Stats Perform"), each = 16000)) %>%
  mutate(grupo = factor(grupo, levels = c("Stats Perform",
                                          "Modelo 3",
                                          "Modelo 2",
                                          "Modelo 1")),
         pp_goles = round(pp_goles / paso) * paso) %>%  # <- clave: redondea al centro del bin
  ggplot(aes(x = pp_goles, y = grupo)) +
  stat_histinterval(fill = "#F3A447",
                    color = "#F3A447",
                    slab_color = "#F3A447",
                    slab_linewidth = 0.4,
                    outline_bars = TRUE,
                    alpha = 0.7,
                    .width = 0.9,
                    point_interval = mean_qi,
                    interval_color = "black",
                    point_color = "black",
                    point_size = 2,
                    breaks = breaks_bastones) +
  geom_vline(xintercept = real, 
             linetype = "dashed", 
             color = "#9C0824",
             linewidth = 1,
             show.legend = FALSE) +
  geom_segment(data = tibble(tipo = "Cantidad observada"),
               aes(x = 0, xend = 1, y = 0, yend = 0, color = tipo),
               inherit.aes = FALSE,
               linetype = "31",
               linewidth = 1) +
  scale_color_manual(values = c("Cantidad observada" = "#9C0824"),
                     labels = c("Cantidad observada" = "Cantidad real"),
                     name = NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.2))) +
  scale_x_continuous(limits = c(minimo, maximo), breaks = scales::breaks_width(150)) +
  coord_cartesian(ylim = c(1, 4.4)) +
  labs(x = "Cantidad de goles", y = NULL) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), 
        axis.title.x = element_text(margin = margin(t = 10), 
                                    size = 12), 
        axis.text.x = element_text(size = 8),
        legend.position = c(0.80, 0.88),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.5, "cm"))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/acumulado_maravilla.pdf",
       plot = acumulado_maravilla,
       width = W,
       height = H,
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/acumulado_merentiel.pdf",
       plot = acumulado_merentiel,
       width = W ,
       height = H,
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/acumulado_dominguez.pdf",
       plot = acumulado_dominguez,
       width = W,
       height = H,
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/pp_goles_totales.pdf",
       plot = acumulado,
       width = W,
       height = H,
       units = "in")

rm(list = c("acumulado_dominguez", 
            "acumulado_maravilla", 
            "acumulado_merentiel", 
            "goles_dominguez", 
            "goles_maravilla", 
            "goles_merentiel", 
            "pp1_goles_dominguez", 
            "pp1_goles_maravilla", 
            "pp1_goles_merentiel", 
            "pp2_goles_dominguez", 
            "pp2_goles_maravilla", 
            "pp2_goles_merentiel", 
            "pp3_goles_dominguez", 
            "pp3_goles_maravilla", 
            "pp3_goles_merentiel", 
            "xG_dominguez", 
            "xG_maravilla", 
            "xG_merentiel", 
            "xGs", 
            "S", 
            "cantidad_goles_opta"))

rm(goles_totales_modelo1,
   goles_totales_modelo2,
   goles_totales_modelo3,
   real, 
   acumulado, 
   ancho_barra, 
   breaks_bastones, 
   centros,
   maximo, 
   minimo, 
   paso, 
   rango_datos)

#---- Construyo funciones para hacer traceplots -----
to_df_mat <- function(mat, param_name) {
  n_iter_real   <- nrow(mat)
  n_chains_real <- ncol(mat)
  data.frame(
    iter  = rep(seq_len(n_iter_real), times = n_chains_real),
    value = as.vector(mat),
    chain = factor(rep(seq_len(n_chains_real), each = n_iter_real)),
    param = param_name
  )
}

make_trace <- function(df, label, show_x = FALSE) {
  p <- ggplot(df, aes(iter, value, color = chain)) +
    geom_line(linewidth = 0.4) +
    scale_color_manual(values = cols) +
    labs(y = label) +
    tema_mio() +
    theme(legend.position = "none",
          axis.title.y = element_text(size = 12, 
                                      angle = 0,  
                                      vjust = 0.5, 
                                      margin = margin(l = 23, r = 10)),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  if (!show_x) {
    p <- p + theme(axis.title.x = element_blank(),
                   axis.text.x  = element_blank(),
                   axis.ticks.x = element_blank())
  } else {
    p <- p + labs(x = "Iteración") +
      theme(axis.title.x = element_text(size = 12),
            axis.text.x  = element_text(size = 8))
  }
  p
}

#---- Traceplots del Modelo 1 ----

nc_1 <- nc_open("Datos/ModeloColab/muestras_modelo_1_chequear.nc")

alpha_1 <- ncvar_get(nc_1, "alpha")
beta_angulo_1 <- ncvar_get(nc_1, "beta_angulo")
beta_distancia_1 <- ncvar_get(nc_1, "beta_distancia")
beta_interaccion_1 <- ncvar_get(nc_1, "beta_interaccion")

nc_close(nc_1)

draws_m1 <- list(
  alpha = as_draws_matrix(alpha_1),
  beta_angulo = as_draws_matrix(beta_angulo_1),
  beta_distancia = as_draws_matrix(beta_distancia_1),
  beta_interaccion = as_draws_matrix(beta_interaccion_1)
)

rm(nc_1, 
   alpha_1, 
   beta_angulo_1, 
   beta_distancia_1, 
   beta_interaccion_1)

# Data frames modelo 1:
df_m1_alpha <- to_df_mat(draws_m1$alpha, "alpha")
df_m1_beta1 <- to_df_mat(draws_m1$beta_distancia, "beta_distancia")
df_m1_beta2 <- to_df_mat(draws_m1$beta_angulo, "beta_angulo")
df_m1_beta3 <- to_df_mat(draws_m1$beta_interaccion, "beta_interaccion")

rm(draws_m1)

# Traceplots modelo 1:
p_m1_alpha <- make_trace(df_m1_alpha, expression(alpha))
p_m1_beta1 <- make_trace(df_m1_beta1, expression(beta[1]))
p_m1_beta2 <- make_trace(df_m1_beta2, expression(beta[2]))
p_m1_beta3 <- make_trace(df_m1_beta3, expression(beta[3]), show_x = TRUE)

traceplots_m1 <- p_m1_alpha / p_m1_beta1 / p_m1_beta2 / p_m1_beta3

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/Combinados/traceplots_m1.pdf",
       plot = traceplots_m1,
       width = W,
       height = H,
       units = "in")

rm(df_m1_alpha, 
   df_m1_beta1, 
   df_m1_beta2, 
   df_m1_beta3,
   p_m1_alpha, 
   p_m1_beta1, 
   p_m1_beta2,
   p_m1_beta3,
   traceplots_m1)

#---- Traceplots del Modelo 2 ----

nc_2 <- nc_open("Datos/ModeloColab/muestras_modelo_2_chequear.nc")

alpha_2 <- ncvar_get(nc_2, "alpha")
beta_distancia_2 <- ncvar_get(nc_2, "beta_distancia")
beta_angulo_2 <- ncvar_get(nc_2, "beta_angulo")
beta_interaccion_2 <- ncvar_get(nc_2, "beta_interaccion")
posiciones_2 <- nc_2$dim[["posicion"]]$vals

orden_posiciones <- c("DF", "MF_no_ofen", "MF_ofen", "FW")
posiciones_2 <- posiciones_2[match(orden_posiciones, posiciones_2)]

nc_close(nc_2)

draws_m2 <- list(
  beta_angulo = as_draws_matrix(beta_angulo_2),
  beta_distancia = as_draws_matrix(beta_distancia_2),
  beta_interaccion = as_draws_matrix(beta_interaccion_2)
)

alphas_list_m2 <- lapply(1:dim(alpha_2)[1], function(k) as_draws_matrix(alpha_2[k,,]))
names(alphas_list_m2) <- paste0("alpha_", posiciones_2)
draws_m2 <- c(draws_m2, alphas_list_m2)

rm(nc_2, 
   alpha_2, 
   beta_distancia_2, 
   beta_angulo_2, 
   beta_interaccion_2, 
   alphas_list_m2)

# Data frames modelo 2:
df_m2_beta1 <- to_df_mat(draws_m2$beta_distancia, "beta_distancia")
df_m2_beta2 <- to_df_mat(draws_m2$beta_angulo, "beta_angulo")
df_m2_beta3 <- to_df_mat(draws_m2$beta_interaccion, "beta_interaccion")

df_m2_alphas <- lapply(seq_along(posiciones_2), function(k) {
  to_df_mat(draws_m2[[paste0("alpha_", posiciones_2[k])]], paste0("alpha_", k))
})

rm(draws_m2)

# Traceplots modelo 2 (betas)
p_m2_beta1 <- make_trace(df_m2_beta1, expression(beta[1])) + 
  theme(axis.title.y = element_text(size = 12, 
                                    angle = 0, 
                                    vjust = 0.5, 
                                    margin = margin(l = 8, r = 10)))
p_m2_beta2 <- make_trace(df_m2_beta2, expression(beta[2])) + 
  theme(axis.title.y = element_text(size = 12, 
                                    angle = 0, 
                                    vjust = 0.5, 
                                    margin = margin(l = 8, r = 10)))
p_m2_beta3 <- make_trace(df_m2_beta3, expression(beta[3]), show_x = TRUE) +
  theme(axis.title.y = element_text(size = 12, 
                                    angle = 0, 
                                    vjust = 0.5, 
                                    margin = margin(l = 8, r = 10)))

traceplots_m2_betas <- p_m2_beta1 / p_m2_beta2 / p_m2_beta3

# Traceplots modelo 2 (alphas)
nombres_posiciones <- c(
  "DF" = "DF",
  "FW" = "DL",
  "MF_no_ofen" = "MCD",
  "MF_ofen" = "MCO"
)

p_m2_alphas <- lapply(seq_along(posiciones_2), function(k) {
  es_ultimo  <- k == length(posiciones_2)
  etiqueta   <- nombres_posiciones[posiciones_2[k]]
  make_trace(df_m2_alphas[[k]], bquote(alpha[.(etiqueta)]), show_x = es_ultimo) +
    theme(axis.title.y = element_text(size = 12, angle = 0, vjust = 0.5,
                                      margin = margin(l = 8, r = 10)))
})

traceplots_m2_alphas <- Reduce("/", p_m2_alphas)

traceplots_m2_alphas <- traceplots_m2_alphas + 
  labs(x = NULL) +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank())

combinado <- traceplots_m2_alphas / traceplots_m2_betas +
  plot_layout(heights = c(1, 1, 1, 1, 3.5))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/Combinados/traceplots_m2.pdf",
       plot = combinado,
       width = W,
       height = H+3,
       units = "in")

rm(df_m2_beta1, 
   df_m2_beta2, 
   df_m2_beta3, 
   df_m2_alphas,
   p_m2_beta1, 
   p_m2_beta2, 
   p_m2_beta3, 
   p_m2_alphas,
   traceplots_m2_betas, 
   traceplots_m2_alphas,
   posiciones_2, 
   combinado)

#---- Traceplots del Modelo 3 ----

nc_3 <- nc_open("Datos/ModeloColab/muestras_modelo_3_chequear.nc")

alpha_3 <- ncvar_get(nc_3, "alpha")
beta_distancia_3 <- ncvar_get(nc_3, "beta_distancia")
beta_angulo_3 <- ncvar_get(nc_3, "beta_angulo")
beta_interaccion_3 <- ncvar_get(nc_3, "beta_interaccion")
sigma_sq_gamma_3 <- ncvar_get(nc_3, "sigma_sq_gamma")
posiciones_3 <- nc_3$dim[["posicion"]]$vals

nc_close(nc_3)

draws_m3 <- list(
  beta_angulo = as_draws_matrix(beta_angulo_3),
  beta_distancia = as_draws_matrix(beta_distancia_3),
  beta_interaccion = as_draws_matrix(beta_interaccion_3),
  sigma_sq_gamma = as_draws_matrix(sigma_sq_gamma_3)
)

alphas_list_m3 <- lapply(1:dim(alpha_3)[1], function(k) as_draws_matrix(alpha_3[k,,]))
names(alphas_list_m3) <- paste0("alpha_", posiciones_3)
draws_m3 <- c(draws_m3, alphas_list_m3)

rm(nc_3, 
   alpha_3, 
   beta_distancia_3, 
   beta_angulo_3, 
   beta_interaccion_3, 
   sigma_sq_gamma_3, 
   alphas_list_m3)

# Data frames modelo 3
df_m3_beta1 <- to_df_mat(draws_m3$beta_distancia, "beta_distancia")
df_m3_beta2 <- to_df_mat(draws_m3$beta_angulo, "beta_angulo")
df_m3_beta3 <- to_df_mat(draws_m3$beta_interaccion, "beta_interaccion")
df_m3_sigma <- to_df_mat(draws_m3$sigma_sq_gamma, "sigma_sq_gamma")

orden_posiciones_3 <- c("DF", "MF_no_ofen", "MF_ofen", "FW")
posiciones_3       <- posiciones_3[match(orden_posiciones_3, posiciones_3)]

df_m3_alphas <- lapply(seq_along(posiciones_3), function(k) {
  to_df_mat(draws_m3[[paste0("alpha_", posiciones_3[k])]], paste0("alpha_", k))
})

rm(draws_m3, orden_posiciones_3)

# Traceplots modelo 3 - betas + sigma
p_m3_beta1 <- make_trace(df_m3_beta1, expression(beta[1])) + 
  theme(axis.title.y = element_text(size = 12, 
                                    angle = 0, 
                                    vjust = 0.5, 
                                    margin = margin(l = 8, r = 10)))
p_m3_beta2 <- make_trace(df_m3_beta2, expression(beta[2])) + 
  theme(axis.title.y = element_text(size = 12, 
                                    angle = 0, 
                                    vjust = 0.5, 
                                    margin = margin(l = 8, r = 10)))
p_m3_beta3 <- make_trace(df_m3_beta3, expression(beta[3])) + 
  theme(axis.title.y = element_text(size = 12, 
                                    angle = 0, 
                                    vjust = 0.5, 
                                    margin = margin(l = 8, r = 10)))
p_m3_sigma <- make_trace(df_m3_sigma, expression(sigma^2), show_x = TRUE) + 
  theme(axis.title.y = element_text(size = 12, 
                                    angle = 0, 
                                    vjust = 0.5, 
                                    margin = margin(l = 8, r = 10)))

traceplots_m3_betas <- p_m3_beta1 / p_m3_beta2 / p_m3_beta3 / p_m3_sigma

# Traceplots modelo 3 - alphas
nombres_posiciones <- c(
  "DF"  = "DF",
  "FW" = "DL",
  "MF_no_ofen" = "MCD",
  "MF_ofen" = "MCO"
)

p_m3_alphas <- lapply(seq_along(posiciones_3), function(k) {
  es_ultimo <- k == length(posiciones_3)
  etiqueta  <- nombres_posiciones[posiciones_3[k]]
  make_trace(df_m3_alphas[[k]], bquote(alpha[.(etiqueta)]), show_x = es_ultimo) + 
    theme(axis.title.y = element_text(size = 12, 
                                      angle = 0, 
                                      vjust = 0.5, 
                                      margin = margin(l = 8, r = 10)))
})

traceplots_m3_alphas <- Reduce("/", p_m3_alphas)

traceplots_m3_alphas <- traceplots_m3_alphas + 
  labs(x = NULL) +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank())

combinado <- traceplots_m3_alphas / traceplots_m3_betas +
  plot_layout(heights = c(1, 1, 1, 1, 4.5))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/Combinados/traceplots_m3.pdf",
       plot = combinado,
       width = W,
       height = H + 4,
       units = "in")

rm(df_m3_beta1,
   df_m3_beta2, 
   df_m3_beta3, 
   df_m3_sigma, 
   df_m3_alphas,
   p_m3_beta1, 
   p_m3_beta2, 
   p_m3_beta3, 
   p_m3_sigma, 
   p_m3_alphas,
   traceplots_m3_betas, 
   traceplots_m3_alphas,
   posiciones_3, 
   nombres_posiciones)

#---- Construyo la base para Calibration Plot y Metricas ----

# Necesito una base que tenga lo siguiente: 
# - 19460 filas (una por disparo).
# - Una columna con el xG de Opta.
# - Una columna con el resultado del disparo. 
# - Tres columnas (una por modelo) con el xG_medio para ese disparo. 

df_con_cobertura_xG_mod1 <- read.csv("Datos/ModeloColab/df_con_cobertura_xG_mod1.csv")  %>% 
  rename(xG_modelo1 = xG_media_posterior) %>% 
  select(disparo = Unnamed..0, 
         xG_modelo1)

df_con_cobertura_xG_mod2 <- read.csv("Datos/ModeloColab/df_con_cobertura_xG_mod2.csv") %>% 
  rename(xG_modelo2 = xG_media_posterior) %>% 
  select(disparo = Unnamed..0, 
         xG_modelo2)

df_con_cobertura_xG_mod3 <- read.csv("Datos/ModeloColab/df_con_cobertura_xG_mod3.csv") %>% 
  rename(xG_modelo3 = xG_media_posterior) %>% 
  select(disparo = Unnamed..0, 
         xG_modelo3)

reales <- datos %>% 
  mutate(disparo = row_number()) %>%   
  select(disparo, 
         opta = xG, 
         resultado = gol)

df <- df_con_cobertura_xG_mod1 %>%
  inner_join(df_con_cobertura_xG_mod2, by = "disparo") %>%
  inner_join(df_con_cobertura_xG_mod3, by = "disparo") %>%
  inner_join(reales, by = "disparo")

colores <- c("Modelo 1" = "#F3A447",
             "Modelo 2" = "#F3A447",
             "Modelo 3" = "#F3A447",
             "Stats Perform" = "#4BACC6")

rm(df_con_cobertura_xG_mod1,
   df_con_cobertura_xG_mod2,
   df_con_cobertura_xG_mod3,
   reales)

#---- Calibration Plots ----

calibracion <- bind_rows(
  df %>%
    arrange(xG_modelo1) %>%
    mutate(bin = ntile(xG_modelo1, 10)) %>%
    group_by(bin) %>%
    summarise(pred = mean(xG_modelo1),
              lower = quantile(xG_modelo1, 0.05),
              upper = quantile(xG_modelo1, 0.95),
              obs = mean(resultado),
              .groups = "drop") %>%
    mutate(Modelo = "Modelo 1",
           color = "#F3A447"),
  
  df %>%
    arrange(xG_modelo2) %>%
    mutate(bin = ntile(xG_modelo2, 10)) %>%
    group_by(bin) %>%
    summarise(pred = mean(xG_modelo2),
              lower = quantile(xG_modelo2, 0.05),
              upper = quantile(xG_modelo2, 0.95),
              obs = mean(resultado),
              .groups = "drop") %>%
    mutate(Modelo = "Modelo 2",
           color = "#F3A447"),
  
  df %>%
    arrange(xG_modelo3) %>%
    mutate(bin = ntile(xG_modelo3, 10)) %>%
    group_by(bin) %>%
    summarise(pred = mean(xG_modelo3),
              lower = quantile(xG_modelo3, 0.05),
              upper = quantile(xG_modelo3, 0.95),
              obs = mean(resultado),
              .groups = "drop") %>%
    mutate(Modelo = "Modelo 3",
           color = "#F3A447"),
  
  df %>%
    arrange(opta) %>%
    mutate(bin = ntile(opta, 10)) %>%
    group_by(bin) %>%
    summarise(pred = mean(opta),
              lower = quantile(opta, 0.05),
              upper = quantile(opta, 0.95),
              obs = mean(resultado),
              .groups = "drop") %>%
    mutate(Modelo = "Stats Perform",
           color = "#F3A447")) %>%
  
  mutate(Modelo = factor(Modelo,
                         levels = c("Modelo 1",
                                    "Modelo 2",
                                    "Modelo 3",
                                    "Stats Perform"))) %>% 
  ggplot(aes(x = obs, y = pred)) +
  geom_abline(slope = 1,
              intercept = 0) +
  geom_linerange(aes(ymin = lower,
                     ymax = upper,
                     color = color),
                 linewidth = 1,
                 show.legend = FALSE) +
  geom_point(aes(color = color),
             size = 2,
             show.legend = FALSE) +
  scale_color_identity() +
  facet_wrap(~Modelo,
             ncol = 1) +
  coord_cartesian(xlim = c(0, 0.40),
                  ylim = c(0, 0.90)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.3), 
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  labs(x = "Proporción de gol observada",
       y = "Probabilidad de gol") +
  tema_mio() +
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/Combinados/calibration_plots_realidad.pdf",
       plot = calibracion,
       width = W,
       height = H+3,
       units = "in")

rm(calibracion)

#---- Brier Score y Log-Loss ----

# Para Opta:
metricas_opta <- df %>%
  summarise(proporcion_real_goles = mean(resultado),
            brier_score = mean((opta - resultado)^2),
            log_loss = {
              pred <- pmin(pmax(opta, 1e-15), 1 - 1e-15)
              -mean(resultado * log(pred) +
                      (1 - resultado) * log(1 - pred))})

# Para xG_modelo1
metricas_modelo1 <- df %>%
  summarise(proporcion_real_goles = mean(resultado),
            brier_score = mean((xG_modelo1 - resultado)^2),
            log_loss = {
              pred <- pmin(pmax(xG_modelo1, 1e-15), 1 - 1e-15)
              -mean(resultado * log(pred) +
                      (1 - resultado) * log(1 - pred))})

# Para xG_modelo2
metricas_modelo2 <- df %>%
  summarise(proporcion_real_goles = mean(resultado),
            brier_score = mean((xG_modelo2 - resultado)^2),
            log_loss = {
              pred <- pmin(pmax(xG_modelo2, 1e-15), 1 - 1e-15)
              -mean(resultado * log(pred) +
                      (1 - resultado) * log(1 - pred))})

# Para xG_modelo3
metricas_modelo3 <- df %>%
  summarise(proporcion_real_goles = mean(resultado),
            brier_score = mean((xG_modelo3 - resultado)^2),
            log_loss = {
              pred <- pmin(pmax(xG_modelo3, 1e-15), 1 - 1e-15)
              -mean(resultado * log(pred) +
                      (1 - resultado) * log(1 - pred))})

metricas_opta
metricas_modelo1
metricas_modelo2
metricas_modelo3

rm(metricas_opta,
   metricas_modelo1,
   metricas_modelo2,
   metricas_modelo3)

#---- CRPS ----

# Debo hacer esto para cada modelo: 
# - Voy a comparar toda la distribucion a posteriori de cada disparo con
#   la estimacion puntual de Opta para este. Esto es, hacer el CRPS para 
#   cada disparo en particular.
# - Voy a obtener 19460 valores de Opta y 19460 valores de CRPS. 
# - Aca puedo graficarlo de dos maneras.
#     1) Graficar todos estos puntos y agregar una curva de suavizado.
#     2) Ordenar de menor a mayor segun los valores de Opta y crear 10 bins para 
#        resumir los 19460/10 valores de Opta con su media (esto es lo que va 
#        en el eje X) y los 19460/10 valores de CRPS con su media (esto es lo
#        que va en el eje Y).
# - Tambien puedo calcular el MAE entre la media de la distribucion a posteriori
#   y la estimacion de Opta.

# Necesito que mi df tenga 19460 filas y 4 columnas: crps_mod1, crps_mod2, 
# crps_mod3 y xG_opta.

# Agrego los valores del modelo 1:

# Cargo el modelo:
nc_1 <- nc_open("Datos/ModeloColab/muestras_modelo_1_chequear.nc")

# Construyo los posteriors de los xG:

alpha <- as.vector(ncvar_get(nc_1, "alpha"))
beta_distancia <- as.vector(ncvar_get(nc_1, "beta_distancia"))
beta_angulo <- as.vector(ncvar_get(nc_1, "beta_angulo"))
beta_interaccion <- as.vector(ncvar_get(nc_1, "beta_interaccion"))

S <- 16000
N <- nrow(datos)

eta <- matrix(0, nrow = S, ncol = N)

for (s in 1:S) {
  eta[s, ] <-
    alpha[s] +
    beta_distancia[s] * datos$distancia_est +
    beta_angulo[s] * datos$angulo_est +
    beta_interaccion[s] * datos$distancia_est * datos$angulo_est
}

p <- 1 / (1 + exp(-eta))

crps_vs_opta <- numeric(N)

for (i in 1:N) {
  crps_vs_opta[i] <- crps_sample(y = df$opta[i], dat = p[, i])
}

df$crps_mod1 <- crps_vs_opta

rm(eta, 
   nc_1, 
   p, 
   beta_angulo, 
   beta_distancia, 
   beta_interaccion, 
   crps_vs_opta,
   S, 
   s, 
   N,
   alpha)

# Agrego los valores del modelo 2: 

# Cargo el modelo:
nc_2 <- nc_open("Datos/ModeloColab/muestras_modelo_2_chequear.nc")

# Reconstruyo los indices de jugador y posicion:
posiciones <- unique(datos$posicion_nueva)

pos_idx <- match(datos$posicion_nueva, posiciones)

rm(posiciones)

# Construyo los posteriors de los xG:

beta_distancia <- as.vector(ncvar_get(nc_2, "beta_distancia"))
beta_angulo <- as.vector(ncvar_get(nc_2, "beta_angulo"))
beta_interaccion <- as.vector(ncvar_get(nc_2, "beta_interaccion"))

alpha <- ncvar_get(nc_2, "alpha")
alpha_vec <- array(alpha, dim = c(4, 16000))
alpha_vec <- t(alpha_vec) 

S <- 16000
N <- nrow(datos)

eta <- matrix(0, nrow = S, ncol = N)

for (s in 1:S) {
  eta[s, ] <-
    alpha_vec[s, pos_idx] +
    beta_distancia[s] * datos$distancia_est +
    beta_angulo[s] * datos$angulo_est +
    beta_interaccion[s] * datos$distancia_est * datos$angulo_est
}

p <- 1 / (1 + exp(-eta))

crps_vs_opta <- numeric(N)

for (i in 1:N) {
  crps_vs_opta[i] <- crps_sample(y = df$opta[i], dat = p[, i])
}

df$crps_mod2 <- crps_vs_opta

rm(eta, 
   nc_2,  
   p, 
   beta_angulo, 
   beta_distancia, 
   beta_interaccion, 
   crps_vs_opta, 
   S, 
   s, 
   N,
   pos_idx, 
   alpha_vec, 
   alpha)

# Agrego los valores del modelo 3: 

# Cargo el modelo:
nc_3 <- nc_open("Datos/ModeloColab/muestras_modelo_3_chequear.nc")

# Reconstruyo los indices de jugador y posicion:
posiciones <- unique(datos$posicion_nueva)
jugadores  <- unique(datos$id_jugador)

pos_idx <- match(datos$posicion_nueva, posiciones)
jug_idx <- match(datos$id_jugador, jugadores)

rm(posiciones,
   jugadores)

# Construyo los posteriors de los xG:

beta_distancia <- as.vector(ncvar_get(nc_3, "beta_distancia"))
beta_angulo <- as.vector(ncvar_get(nc_3, "beta_angulo"))
beta_interaccion <- as.vector(ncvar_get(nc_3, "beta_interaccion"))

alpha <- ncvar_get(nc_3, "alpha")
alpha_vec <- array(alpha, dim = c(4, 16000))
alpha_vec <- t(alpha_vec) 

gamma <- ncvar_get(nc_3, "gamma")
gamma_vec <- array(gamma, dim = c(length(unique(datos$id_jugador)), 16000))
gamma_vec <- t(gamma_vec)

S <- 16000
N <- nrow(datos)

eta <- matrix(0, nrow = S, ncol = N)

for (s in 1:S) {
  eta[s, ] <-
    alpha_vec[s, pos_idx] +
    gamma_vec[s, jug_idx] +
    beta_distancia[s] * datos$distancia_est +
    beta_angulo[s] * datos$angulo_est +
    beta_interaccion[s] * datos$distancia_est * datos$angulo_est
}

p <- 1 / (1 + exp(-eta))

crps_vs_opta <- numeric(N)

for (i in 1:N) {
  crps_vs_opta[i] <- crps_sample(y = df$opta[i], dat = p[, i])
}

df$crps_mod3 <- crps_vs_opta

rm(eta, 
   nc_3, 
   gamma_vec, 
   p, 
   beta_angulo, 
   beta_distancia, 
   beta_interaccion, 
   jug_idx, 
   crps_vs_opta, 
   S, 
   s, 
   N,
   pos_idx, 
   gamma, 
   alpha_vec, 
   alpha)

# Calculo los MAE:

df <- df %>%
  mutate(mae_mod1 = abs(xG_modelo1 - opta), 
         mae_mod2 = abs(xG_modelo2 - opta), 
         mae_mod3 = abs(xG_modelo3 - opta))

# Construyo los graficos con un mapa de calor:

# Modelo 1:

df_heat <- df %>%
  mutate(x_bin = cut(opta, 
                     breaks = seq(0, 1, 0.1),
                     include.lowest = TRUE, 
                     labels = FALSE),
         y_bin = cut(crps_mod1, 
                     breaks = seq(0, 1, 0.1), 
                     include.lowest = TRUE, 
                     labels = FALSE))

conteos <- df_heat %>%
  count(x_bin, y_bin, name = "n")

total_puntos <- nrow(df_heat)

grilla <- expand.grid(x_bin = 1:10, y_bin = 1:10) %>%
  left_join(conteos, by = c("x_bin", "y_bin")) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         pct = n / total_puntos * 100,
         pct_fill = ifelse(n == 0, NA, pct),
         xmin = (x_bin - 1) * 0.1,
         xmax = x_bin * 0.1,
         ymin = (y_bin - 1) * 0.1,
         ymax = y_bin * 0.1,
         x_mid = (xmin + xmax) / 2,
         y_mid = (ymin + ymax) / 2,
         label_pct = ifelse(n == 0, "", 
                            paste0(formatC(pct, format = "f", digits = 1, decimal.mark = ","), "%")))

crps_todos1 <- ggplot(grilla) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = pct_fill),
            color = "white", linewidth = 0.4) +
  geom_text(aes(x = x_mid, y = y_mid, label = label_pct),
            size = 2.6, color = "grey15") +
  scale_fill_gradient(low = "#FFF3E0", high = "#F3A447",
                      na.value = "grey85",
                      name = "Porcentaje de tiros",
                      breaks = c(0, 25, 50, 75, 100),
                      labels = c("0%", "25%", "50%", "75%", "100%"),
                      limits = c(0, 100)) +
  scale_x_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2),
                     expand = c(0, 0), 
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2),
                     expand = c(0, 0), 
                     labels = scales::label_number(decimal.mark = ",")) +
  labs(x = "Probabilidad de gol (Stats Perform)",
       y = "CRPS",
       title = NULL) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 12),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, l = 13)),
        axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 12, hjust = 0.5, vjust = 1,
                                    margin = margin(b = 8)),
        legend.text = element_text(size = 10))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/crps1_todos.pdf",
       plot = crps_todos1,
       width = W ,
       height = H,
       units = "in")

rm(df_heat, 
   conteos, 
   total_puntos, 
   grilla, 
   crps_todos1)

# Modelo 2:

df_heat <- df %>%
  mutate(x_bin = cut(opta, 
                     breaks = seq(0, 1, 0.1),
                     include.lowest = TRUE, 
                     labels = FALSE),
         y_bin = cut(crps_mod2, 
                     breaks = seq(0, 1, 0.1), 
                     include.lowest = TRUE, 
                     labels = FALSE))

conteos <- df_heat %>%
  count(x_bin, y_bin, name = "n")

total_puntos <- nrow(df_heat)

grilla <- expand.grid(x_bin = 1:10, y_bin = 1:10) %>%
  left_join(conteos, by = c("x_bin", "y_bin")) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         pct = n / total_puntos * 100,
         pct_fill = ifelse(n == 0, NA, pct),
         xmin = (x_bin - 1) * 0.1,
         xmax = x_bin * 0.1,
         ymin = (y_bin - 1) * 0.1,
         ymax = y_bin * 0.1,
         x_mid = (xmin + xmax) / 2,
         y_mid = (ymin + ymax) / 2,
         label_pct = ifelse(n == 0, "", 
                            paste0(formatC(pct, format = "f", digits = 1, decimal.mark = ","), "%")))

crps_todos2 <- ggplot(grilla) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = pct_fill),
            color = "white", linewidth = 0.4) +
  geom_text(aes(x = x_mid, y = y_mid, label = label_pct),
            size = 2.6, color = "grey15") +
  scale_fill_gradient(low = "#FFF3E0", high = "#F3A447",
                      na.value = "grey85",
                      name = "Porcentaje de tiros",
                      breaks = c(0, 25, 50, 75, 100),
                      labels = c("0%", "25%", "50%", "75%", "100%"),
                      limits = c(0, 100)) +
  scale_x_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2), 
                     expand = c(0, 0), 
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2), 
                     expand = c(0, 0), 
                     labels = scales::label_number(decimal.mark = ",")) +
  labs(x = "Probabilidad de gol (Stats Perform)",
       y = "CRPS",
       title = NULL) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 12),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, l = 13)),
        axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 12, hjust = 0.5, vjust = 1,
                                    margin = margin(b = 8)),
        legend.text = element_text(size = 10))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/crps2_todos.pdf",
       plot = crps_todos2,
       width = W ,
       height = H,
       units = "in")

rm(df_heat, 
   conteos, 
   total_puntos, 
   grilla, 
   crps_todos2)

# Modelo 3:

df_heat <- df %>%
  mutate(x_bin = cut(opta, 
                     breaks = seq(0, 1, 0.1),
                     include.lowest = TRUE, 
                     labels = FALSE),
         y_bin = cut(crps_mod3, 
                     breaks = seq(0, 1, 0.1), 
                     include.lowest = TRUE, 
                     labels = FALSE))

conteos <- df_heat %>%
  count(x_bin, y_bin, name = "n")

total_puntos <- nrow(df_heat)

grilla <- expand.grid(x_bin = 1:10, y_bin = 1:10) %>%
  left_join(conteos, by = c("x_bin", "y_bin")) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         pct = n / total_puntos * 100,
         pct_fill = ifelse(n == 0, NA, pct),
         xmin = (x_bin - 1) * 0.1,
         xmax = x_bin * 0.1,
         ymin = (y_bin - 1) * 0.1,
         ymax = y_bin * 0.1,
         x_mid = (xmin + xmax) / 2,
         y_mid = (ymin + ymax) / 2,
         label_pct = ifelse(n == 0, "", 
                            paste0(formatC(pct, format = "f", digits = 1, decimal.mark = ","), "%")))

crps_todos3 <- ggplot(grilla) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = pct_fill),
            color = "white", linewidth = 0.4) +
  geom_text(aes(x = x_mid, y = y_mid, label = label_pct),
            size = 2.6, color = "grey15") +
  scale_fill_gradient(low = "#FFF3E0", high = "#F3A447",
                      na.value = "grey85",
                      name = "Porcentaje de tiros",
                      breaks = c(0, 25, 50, 75, 100),
                      labels = c("0%", "25%", "50%", "75%", "100%"),
                      limits = c(0, 100)) +
  scale_x_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2), 
                     expand = c(0, 0),
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2), 
                     expand = c(0, 0),
                     labels = scales::label_number(decimal.mark = ",")) +
  labs(x = "Probabilidad de gol (Stats Perform)",
       y = "CRPS",
       title = NULL) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 12),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, l = 13)),
        axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 12, hjust = 0.5, vjust = 1,
                                    margin = margin(b = 8)),
        legend.text = element_text(size = 10))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/crps3_todos.pdf",
       plot = crps_todos3,
       width = W ,
       height = H,
       units = "in")

rm(df_heat, 
   conteos, 
   total_puntos, 
   grilla, 
   crps_todos3)

# Construyo los graficos resumido en bins: 

crps_resumido1 <- df %>%
  mutate(bin = ntile(opta, 10)) %>%
  group_by(bin) %>%
  summarise(xG_mean = mean(opta),
            crps_mean1 = mean(crps_mod1),
            mae_mean = mean(mae_mod1),
            .groups = "drop") %>%
  pivot_longer(cols = c(crps_mean1, 
                        mae_mean),
               names_to = "Metrica",
               values_to = "Valor") %>%
  mutate(Metrica = case_when(Metrica == "crps_mean1" ~ "CRPS",
                             Metrica == "mae_mean" ~ "MAE")) %>% 
  ggplot(aes(x = xG_mean, y = Valor, color = Metrica)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("CRPS" = "#F3A447",
                                "MAE" = "#8064A2"),
                     name = NULL) + 
  geom_point(size = 3) +
  labs(x = "Probabilidad de gol (Stats Perform)",
       y = "Error",
       color = "") +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() + 
  theme(axis.text.y = element_text(size = 8),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10),
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 10, l = 13),
                                    size = 12),
        axis.text.x = element_text(size = 8),
        legend.position = c(0.2, 0.85),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.5, "cm"))

crps_resumido2 <- df %>%
  mutate(bin = ntile(opta, 10)) %>%
  group_by(bin) %>%
  summarise(xG_mean = mean(opta),
            crps_mean2 = mean(crps_mod2),
            mae_mean = mean(mae_mod2),
            .groups = "drop") %>%
  pivot_longer(cols = c(crps_mean2, 
                        mae_mean),
               names_to = "Metrica",
               values_to = "Valor") %>%
  mutate(Metrica = case_when(Metrica == "crps_mean2" ~ "CRPS",
                             Metrica == "mae_mean" ~ "MAE")) %>% 
  ggplot(aes(x = xG_mean, y = Valor, color = Metrica)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("CRPS" = "#F3A447",
                                "MAE" = "#8064A2"),
                     name = NULL) + 
  geom_point(size = 3) +
  labs(x = "Probabilidad de gol (Stats Perform)",
       y = "Error",
       color = "") +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() + 
  theme(axis.text.y = element_text(size = 8),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10),
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 10, l = 13),
                                    size = 12),
        axis.text.x = element_text(size = 8),
        legend.position = c(0.2, 0.85),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.5, "cm"))

crps_resumido3 <- df %>%
  mutate(bin = ntile(opta, 10)) %>%
  group_by(bin) %>%
  summarise(xG_mean = mean(opta),
            crps_mean3 = mean(crps_mod3),
            mae_mean = mean(mae_mod3),
            .groups = "drop") %>%
  pivot_longer(cols = c(crps_mean3, 
                        mae_mean),
               names_to = "Metrica",
               values_to = "Valor") %>%
  mutate(Metrica = case_when(Metrica == "crps_mean3" ~ "CRPS",
                             Metrica == "mae_mean" ~ "MAE")) %>% 
  ggplot(aes(x = xG_mean, y = Valor, color = Metrica)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("CRPS" = "#F3A447",
                                "MAE" = "#8064A2"),
                     name = NULL) + 
  geom_point(size = 3) +
  labs(x = "Probabilidad de gol (Stats Perform)",
       y = "Error",
       color = "") +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() + 
  theme(axis.text.y = element_text(size = 8),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10),
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 10, l = 13),
                                    size = 12),
        axis.text.x = element_text(size = 8),
        legend.position = c(0.2, 0.85),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = "white",
                                         color = "black"),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.5, "cm"))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/crps1_resumido.pdf",
       plot = crps_resumido1,
       width = W ,
       height = H,
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/crps2_resumido.pdf",
       plot = crps_resumido2,
       width = W ,
       height = H,
       units = "in")
ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/crps3_resumido.pdf",
       plot = crps_resumido3,
       width = W ,
       height = H,
       units = "in")

#---- Tamaño de las canchas ----

tamaños_canchas <- data.frame(
  nombre = c("Arg Juniors", "Arsenal", "Atlético Tucumán",
             "Banfield", "Barracas Central", "Belgrano",
             "Boca Juniors", "Central Córdoba–SdE", "Colón",
             "Defensa y Justicia", "Deportivo Riestra", "Estudiantes",
             "Gimnasia–LP", "Godoy Cruz", "Huracán",
             "Independiente", "Independiente Rivadavia", "Instituto",
             "Lanús", "Newell's Old Boys", "Platense",
             "Racing Club", "River Plate", "Rosario Central",
             "San Lorenzo", "Sarmiento", "Talleres",
             "Tigre", "Unión", "Vélez Sarsfield"),
  largo = c(101, 105, 105, 104, 101, 105, 105, 105, 
            104, 100, 100, 105, 104, 105, 105, 106, 
            104, 105, 105, 105, 105, 103, 105, 105,
            110, 100, 106, 105, 105, 105),
  ancho = c(66, 70, 70, 68, 68, 68, 68, 68, 70, 70,
            65, 68, 68, 72, 70, 68, 70, 68, 70, 70, 
            68, 70, 70, 70, 70, 67, 70, 68, 70, 68))

# Agrego una variable para los disparos de afuera del area:
datos <- datos %>% 
  mutate(afuera_area = if_else(x < 88.5 | y < 13.84 | y > 54.16, 1, 0))

# Estos son los equipos cuya cancha no mide 105 metros:
equipos_cancha_distinta <- tamaños_canchas %>% 
  filter(!(largo == 105 & ancho == 68)) %>% 
  pull(nombre)

# Me quedo con los datos que podrian generar un problema:
datos_a_chequear <- datos %>% 
  filter(equipo %in% equipos_cancha_distinta, 
         afuera_area == 1)

# Creo dos funciones para calcular distancia y angulo pero considerando 
# el tamaño de la cancha donde se jugo:
shot_distance <- function(x, y, largo, ancho) {
  
  x_real <- x * largo / 105
  y_real <- y * ancho / 68
  
  distancia <- sqrt((largo - x_real)^2 + (ancho / 2 - y_real)^2)
  
  return(distancia)
}

shot_angle <- function(x, y, largo, ancho) {
  
  x_real <- x * largo / 105
  y_real <- y * ancho / 68
  
  a <- 7.32
  
  b <- sqrt((largo - x_real)^2 + ((ancho / 2 - a / 2) - y_real)^2)
  
  c <- sqrt((largo - x_real)^2 + ((ancho / 2 + a / 2) - y_real)^2)
  
  numerador <- a^2 - b^2 - c^2
  denominador <- -2 * b * c
  
  angulo <- acos(numerador / denominador) * 180 / pi
  
  return(angulo)
}

# Prueba con un disparo:
x <- 85
y <- 30

shot_distance(x, y, 110, 70)
shot_distance(x, y, 105, 68)
shot_distance(x, y, 100, 65)
shot_angle(x, y, 110, 70)
shot_angle(x, y, 105, 68)
shot_angle(x, y, 100, 65)

#---- xG vs Goles por jugador ----

xG_vs_goles <- df %>% 
  left_join(datos %>% 
              mutate(disparo = row_number()) %>% 
              select(disparo, id_jugador, posicion_nueva),
            by = "disparo") %>% 
  group_by(id_jugador) %>% 
  summarise(goles = sum(resultado),
            `Modelo 1` = sum(xG_modelo1),
            `Modelo 2` = sum(xG_modelo2),
            `Modelo 3` = sum(xG_modelo3),
            `Stats Perform` = sum(opta),
            .groups = "drop") %>% 
  pivot_longer(cols = c(`Modelo 1`,
                        `Modelo 2`,
                        `Modelo 3`,
                        `Stats Perform`),
               names_to = "Modelo",
               values_to = "xG_acumulado") %>% 
  ggplot(aes(x = xG_acumulado, y = goles)) +
  geom_point(color = "#F3A447",
             alpha = 1,
             size = 2) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "black",
              linewidth = 0.6) +
  facet_wrap(~Modelo,
             nrow = 2) +
  labs(x = "Suma de goles esperados",
       y = "Goles convertidos") +
  tema_mio() +
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 8),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 8))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/Nuevos/xG_vs_goles.pdf",
       plot = xG_vs_goles,
       width = W,
       height = H+1,
       units = "in")
