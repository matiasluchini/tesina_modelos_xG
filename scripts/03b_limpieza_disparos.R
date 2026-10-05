#---- Carga de librerias ----
library(tidyverse)
library(readxl)
library(here)

#---- Carga de datos ----
# Objetos que genera el script 03a:
todos_los_datos <- readRDS(here("data", "intermediate", "todos_los_datos.rds"))

fbref <- readRDS(here("data", "intermediate", "jugadores_fbref.rds"))
jugadores_fbref_copa2023 <- fbref$copa2023
jugadores_fbref_liga2023 <- fbref$liga2023
jugadores_fbref_copa2024 <- fbref$copa2024
jugadores_fbref_liga2024 <- fbref$liga2024

#---- Acomodo la base ----

# Con esos archivos csv construyo el diccionario de nombres. 
nombres_copa2023 <- read_excel(here("data", "manual", "nombre_jugadores_fotmob_fbref.xlsx"), 
                               sheet = "copa2023")
nombres_liga2023 <- read_excel(here("data", "manual", "nombre_jugadores_fotmob_fbref.xlsx"), 
                               sheet = "liga2023")
nombres_copa2024 <- read_excel(here("data", "manual", "nombre_jugadores_fotmob_fbref.xlsx"), 
                               sheet = "copa2024")
nombres_liga2024 <- read_excel(here("data", "manual", "nombre_jugadores_fotmob_fbref.xlsx"), 
                               sheet = "liga2024")

# De info_fbref_torneoXXXX me quedo solo con nombre y posicion en la que juegan.
# Hago la asociacion de esos nombres a los que tienen en Fotmob usando nombres_torneoXXXX.
jugadores_fbref_copa2023 <- jugadores_fbref_copa2023 %>% 
  select(-stats_Squad) %>% 
  filter(Player %in% nombres_copa2023$fbref) %>%
  group_by(Player) %>%
  slice_max(nchar(stats_Pos), with_ties = FALSE) %>%
  ungroup()
nombres_con_posicion_copa2023 <- nombres_copa2023 %>%
  left_join(jugadores_fbref_copa2023, 
            by = c("fbref" = "Player")) %>% 
  select(-fbref)

jugadores_fbref_liga2023 <- jugadores_fbref_liga2023 %>% 
  select(-stats_Squad) %>% 
  filter(Player %in% nombres_liga2023$fbref) %>%
  group_by(Player) %>%
  slice_max(nchar(stats_Pos), with_ties = FALSE) %>%
  ungroup()
nombres_con_posicion_liga2023 <- nombres_liga2023 %>%
  left_join(jugadores_fbref_liga2023, 
            by = c("fbref" = "Player")) %>% 
  select(-fbref)

jugadores_fbref_copa2024 <- jugadores_fbref_copa2024 %>% 
  select(-stats_Squad) %>% 
  filter(Player %in% nombres_copa2024$fbref) %>%
  group_by(Player) %>%
  slice_max(nchar(stats_Pos), with_ties = FALSE) %>%
  ungroup()
nombres_con_posicion_copa2024 <- nombres_copa2024 %>%
  left_join(jugadores_fbref_copa2024, 
            by = c("fbref" = "Player")) %>% 
  select(-fbref)

jugadores_fbref_liga2024 <- jugadores_fbref_liga2024 %>% 
  select(-stats_Squad) %>% 
  filter(Player %in% nombres_liga2024$fbref) %>%
  group_by(Player) %>%
  slice_max(nchar(stats_Pos), with_ties = FALSE) %>%
  ungroup()
nombres_con_posicion_liga2024 <- nombres_liga2024 %>%
  left_join(jugadores_fbref_liga2024, 
            by = c("fbref" = "Player")) %>% 
  select(-fbref)

# Agrego la columna de posicion a cada fila de todos_los_datos.
tiros_copa2023 <- todos_los_datos %>% 
  filter(torneo == "copa2023") %>% 
  left_join(nombres_con_posicion_copa2023, 
            by = c("playerName" = "Fotmob")) %>% 
  mutate(stats_Pos = case_when(playerName == "Sebastian Boselli" ~ "DF", 
                               playerName == "Claudio Jeremias Echeverri" ~ "MF,DF", 
                               TRUE ~ stats_Pos))

tiros_liga2023 <- todos_los_datos %>% 
  filter(torneo == "liga2023") %>% 
  left_join(nombres_con_posicion_liga2023, 
            by = c("playerName" = "Fotmob")) %>% 
  mutate(stats_Pos = case_when(playerName == "Jonathan Menendez" ~ "FW", 
                               TRUE ~ stats_Pos))

tiros_copa2024 <- todos_los_datos %>% 
  filter(torneo == "copa2024") %>% 
  left_join(nombres_con_posicion_copa2024, 
            by = c("playerName" = "Fotmob"))

tiros_liga2024 <- todos_los_datos %>% 
  filter(torneo == "liga2024") %>% 
  left_join(nombres_con_posicion_liga2024, 
            by = c("playerName" = "Fotmob")) %>% 
  mutate(stats_Pos = case_when(playerName == "Matias Alejandro Galarza" ~ "MF", 
                               TRUE ~ stats_Pos))

# Debo cambiar los ID de los equipos que juegan cada partido por su correpondiente 
# nombre.
id_equipos_copa2023 <- read_csv(here("data", "raw", "fotmob", "equipos", "id_equipos_copa2023.csv"))
id_equipos_copa2024 <- read_csv(here("data", "raw", "fotmob", "equipos", "id_equipos_copa2024.csv"))
id_equipos_liga2023 <- read_csv(here("data", "raw", "fotmob", "equipos", "id_equipos_liga2023.csv"))
id_equipos_liga2024 <- read_csv(here("data", "raw", "fotmob", "equipos", "id_equipos_liga2024.csv"))

# Primero construyo un data set que tenga el nombre de cada equipo asociado
# a su ID.
IDS <- bind_rows(id_equipos_copa2023, 
                 id_equipos_copa2024, 
                 id_equipos_liga2023, 
                 id_equipos_liga2024) %>% 
  count(name, 
        id) %>% 
  select(-n)

# Realizo un left join para hacer la conversion.
tiros_copa2023 <- tiros_copa2023 %>%
  left_join(IDS, by = c("teamId" = "id")) %>%
  rename(equipo = name)

tiros_liga2023 <- tiros_liga2023 %>%
  left_join(IDS, by = c("teamId" = "id")) %>%
  rename(equipo = name)

tiros_copa2024 <- tiros_copa2024 %>%
  left_join(IDS, by = c("teamId" = "id")) %>%
  rename(equipo = name)

tiros_liga2024 <- tiros_liga2024 %>%
  left_join(IDS, by = c("teamId" = "id")) %>%
  rename(equipo = name)

# Construyo una funcion que obtenga el angulo del tiro.
calcular_angulo_tiro <- function(X, Y) {
  
  poste_superior <- c(105, 37.66)
  poste_inferior <- c(105, 30.34)
  
  v1 <- c(poste_superior[1] - X, poste_superior[2] - Y)
  v2 <- c(poste_inferior[1] - X, poste_inferior[2] - Y)
  
  prod_escalar <- sum(v1 * v2)
  
  norma_v1 <- sqrt(sum(v1^2))
  norma_v2 <- sqrt(sum(v2^2))
  
  angulo_rad <- acos(prod_escalar / (norma_v1 * norma_v2))
  
  angulo_grados <- angulo_rad * (180 / pi)
  
  return(angulo_grados)
}

# Acomodo la base y agrego distancia y angulo de tiro.
datos <- bind_rows(tiros_copa2023, 
                   tiros_liga2023, 
                   tiros_copa2024, 
                   tiros_liga2024) %>% 
  mutate(distancia = case_when(isOwnGoal == "False" ~ sqrt((105 - x)^2 + (34 - y)^2), 
                               TRUE ~ NA), 
         angulo = case_when(isOwnGoal == "False" ~ pmap_dbl(list(x, y), calcular_angulo_tiro),
                            TRUE ~ NA), 
         fue_al_arco = case_when(isOnTarget == "False" ~ 0,
                                 TRUE ~ 1), 
         fue_bloqueado = case_when(isBlocked == "False" ~ 0, 
                                   TRUE ~ 1), 
         parte_del_cuerpo = case_when(shotType == "Header" ~ "Cabeza",
                                      shotType == "OtherBodyParts" ~ "Otro",
                                      shotType == "LeftFoot" ~ "Zurda",
                                      shotType == "RightFoot" ~ "Derecha"), 
         situacion = case_when(situation == "FastBreak" ~ "Contragolpe",
                               situation == "FreeKick" ~ "TiroLibre",
                               situation == "FromCorner" ~ "Corner",
                               situation == "IndividualPlay" ~ "JugadaIndividual",
                               situation == "Penalty" ~ "Penal",
                               situation == "RegularPlay" ~ "Jugada",
                               situation == "SetPiece" ~ "SetPiece",
                               situation == "ThrowInSetPiece" ~ "ThrowInSetPiece"), 
         resultado = case_when(eventType == "AttemptSaved" ~ "Atajado",
                               eventType == "Goal" ~ "Gol",
                               eventType == "Miss" ~ "Errado",
                               eventType == "Post" ~ "Palo"), 
         gol = case_when(resultado == "Gol" ~ 1, 
                         TRUE ~ 0), 
         fue_autogol = case_when(isOwnGoal == "False" ~ 0, 
                                 TRUE ~ 1)) %>% 
  separate(stats_Pos, 
           into = c("posicion", "posicion_alternativa"), 
           sep = ",", 
           fill = "right", 
           extra = "merge") %>% 
  select(torneo, 
         partido, 
         equipo,
         jugador = playerName, 
         id_jugador = playerId,
         posicion, 
         posicion_alternativa,
         minuto = min, 
         minuto_agregado = minAdded,
         x, 
         y, 
         distancia, 
         angulo, 
         situacion,
         parte_del_cuerpo,
         fue_bloqueado, 
         x_bloqueado = blockedX, 
         y_bloqueado = blockedY, 
         fue_al_arco, 
         y_fondo = goalCrossedY, 
         z_fondo = goalCrossedZ, 
         resultado,
         gol,
         fue_autogol, 
         xG = expectedGoals, 
         xGoT = expectedGoalsOnTarget)

# Base completa, antes de agregar localía y filtrar
disparos_2023_2024 <- datos %>% 
  mutate(id_disparo = row_number())

#---- Agregar localia a la base ----

# Esto no se termino usando, pero lo dejo para futuros trabajos.

ubicacion <- here("data", "manual", "resultados_y_xG.xlsx")

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
  pull(id_disparo)

disparos_2023_2024 <- disparos_2023_2024 %>% 
  filter(!(id_disparo %in% id_borrar))

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

#---- Conversion a archivos .csv ----

# Base para el análisis de resultados (con todas las variables)
write.csv(datos, 
          here("data", "processed", "datos_analisis.csv"), 
          row.names = FALSE)

# Base reducida para entrenar los modelos
disparos_reducido <- datos %>% 
  select(id_jugador, 
         posicion_nueva, 
         distancia_est, 
         angulo_est, 
         interaccion2, 
         gol, 
         xG)

write.csv(disparos_reducido, 
          here("data", "processed", "disparos_2023_2024_reducido.csv"), 
          row.names = FALSE)

# Base completa de disparos (antes de los filtros del modelo)
write.csv(disparos_2023_2024, 
          here("data", "processed", "disparos_2023_2024.csv"), 
          row.names = FALSE)

# Goles por jugador
write.csv(goles_por_jugador, 
          here("data", "processed", "goles_por_jugador.csv"), 
          row.names = FALSE)