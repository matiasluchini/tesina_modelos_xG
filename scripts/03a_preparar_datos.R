#---- Carga de librerias ----
library(tidyverse)
library(readr)
library(readxl)
library(here)

#---- Carga de datos ----
df <- list()

# Los CSV de cada partido están en data/raw/fotmob/partidos/<torneo>/
# (las carpetas deben llamarse igual que los elementos de "torneos")
torneos <- c("liga2023", 
             "copa2023", 
             "copa2024", 
             "liga2024")

# Cantidad de partidos de cada torneo:
duracion <- c(378, 204, 203, 378)

for (i in seq_along(torneos)) {
  df[[i]] <- list()
}

# Los partidos se deben llamar fotmob_torneoXXXX_partidoN.csv
for (i in torneos) {
  for (j in 1:duracion[which(torneos == i)]) {
    nombre_archivo <- paste0("fotmob_", i, "_partido", j, ".csv")
    df[[which(torneos == i)]][[j]] <- read.csv(
      here("data", "raw", "fotmob", "partidos", i, nombre_archivo)
    )
  }
}

names(df) <- torneos

for (i in torneos) {
  for (j in 1:duracion[which(torneos == i)]) {
    df[[i]][[j]] <- df[[i]][[j]] %>%
      select(x, 
             y, 
             min,
             minAdded,
             eventType, 
             teamId, 
             playerId,
             playerName, 
             #firstName, 
             #lastName, 
             #fullName,
             isBlocked, 
             isOnTarget, 
             blockedX, 
             blockedY, 
             goalCrossedY, 
             goalCrossedZ, 
             expectedGoals,
             expectedGoalsOnTarget, 
             shotType, 
             situation, 
             isOwnGoal, 
             isSavedOffLine,
             #isFromInsideBox, 
             goalMouthY, 
             goalMouthZ,
             zoomRatio)
  }
}

# Para llamar a cada dataframe usamos df[["torneoXXXX"]][[N]]

# Guarda la lista en un archivo .RDS
saveRDS(df, here("data", "intermediate", "fotmob_informacion_por_partido.rds"))

# Si quisiera cargarlo uso:
# df <- readRDS(here("data", "intermediate", "fotmob_informacion_por_partido.rds"))

# Construyo una funcion que una las sublistas en un solo df.
todos_los_datos <- imap_dfr(df, function(sublista, nombre_torneo) {
  imap_dfr(sublista, function(dataset, numero_partido) {
    dataset %>%
      mutate(
        torneo = nombre_torneo,
        partido = numero_partido
      )
  })
}) 

#---- Correcciones a nombres de jugadores ----

# Realizo algunas correciones a los nombres de algunos jugadores.
# - Agustin Garcia Basso es el mismo jugador que Agustin Basso.
# - Franco Jara es el mismo jugador que Franco Daniel Jara.
# - Hay dos jugadores que se llaman Alan Rodriguez. Le agrego su segundo nombre.
# - Hay dos jugadores que se llaman Ignacio Fernández. Uno es el Equi.
# - Hay dos jugadores que se llaman Matias Gomez. Le agrego su segundo nombre.
# - Hay dos jugadores que se llaman Nicolas Fernandez. Uno es el Uvita.
# - Benjamin Schamine aparece a veces con tilde y a veces sin.
# - Hay dos jugadores que se llaman Julian Fernandez. Le agrego su segundo nombre.
# - Hay dos jugadores que se llaman Tiago Palacios. Le agrego su segundo nombre.
# - Hay dos jugadores que se llaman Eric Ramirez. Le agrego su segundo nombre.
# - Hay dos jugadores que se llaman Matias Galarza. Le agrego su segundo nombre.
# - Hay dos jugadores que se llaman Santiago Sosa. Le agrego su segundo nombre.4
# - Le agrego segundo nombre a Francisco Gonzalez para no confundilo con Gonzalez Metilli.
# - Carlos Arce es el mismo jugador que Carlos Martin Arce.
# - Lucas Menossi es el mismo jugador que Lucas Ariel Menossi.
# - Eric Remedi es el mismo jugador que Eric Daian Remedi.
# - Yonatan Goitia es el mismo jugador que Jonatan Goitia.

todos_los_datos <- todos_los_datos %>% 
  mutate(playerId = case_when(playerName == "Agustín García Basso" ~ 447095, 
                              playerName == "Benjamín Schamine" ~ 1607551, 
                              playerName == "Maximiliano González" ~ 1293954, 
                              TRUE ~ playerId),  
         playerName = case_when(playerName == "Agustín García Basso" ~ "Agustin Basso",
                                playerName == "Franco Jara" ~ "Franco Daniel Jara",
                                playerId == 887142 ~ "Alan Francisco Rodriguez",
                                playerId == 1035611 ~ "Alan Jesus Rodriguez",
                                playerId == 1199959 ~ "Equi Fernandez",
                                playerId == 1504612 ~ "Matias Ezequiel Gomez",
                                playerId == 933706 ~ "Matias Nicolas Gomez",
                                playerId == 889729 ~ "Roberto Nicolas Fernandez",
                                playerId == 566676 ~ "Uvita Fernandez",
                                playerName == "Benjamín Schamine" ~ "Benjamin Schamine",
                                playerId == 1338015 ~ "Julian Fernandez",
                                playerId == 453412 ~ "Julian Rodrigo Fernandez",
                                playerId == 1344726 ~ "Tiago Tomas Palacios",
                                playerId == 1231195 ~ "Tiago Asael Palacios",
                                playerId == 837890 ~ "Eric Kleybel Ramirez",
                                playerId == 624411 ~ "Eric Ivan Ramirez",
                                playerId == 1277340 ~ "Matias Alejandro Galarza",
                                playerId == 1241957 ~ "Matias Galarza Fonda",
                                playerId == 1635811 ~ "Santiago Nahuel Sosa",
                                playerId == 958860 ~ "Santiago Leonel Sosa",
                                playerId == 1042571 ~ "Francisco Agustin Gonzalez",
                                playerId == 850731 ~ "Carlos Arce",
                                playerId == 246408 ~ "Lucas Menossi",
                                playerId == 642763 ~ "Eric Daian Remedi",
                                playerId == 902983 ~ "Yonatan Goitia",
                                TRUE ~ playerName)) 

# Construyo df con los jugadores que hayan realizado un tiro al arco en cada 
# torneo.
jugadores_fotmob_copa2023 <- todos_los_datos %>% 
  filter(torneo == "copa2023") %>% 
  count(playerName, playerId) %>% 
  select(playerName) %>% 
  count(playerName) %>% 
  arrange(playerName) 
jugadores_fotmob_liga2023 <- todos_los_datos %>% 
  filter(torneo == "liga2023") %>% 
  count(playerName, playerId) %>% 
  select(playerName) %>% 
  count(playerName) %>% 
  arrange(playerName) 
jugadores_fotmob_copa2024 <- todos_los_datos %>% 
  filter(torneo == "copa2024") %>% 
  count(playerName, playerId) %>% 
  select(playerName) %>%
  count(playerName) %>%  
  arrange(playerName)
jugadores_fotmob_liga2024 <- todos_los_datos %>% 
  filter(torneo == "liga2024") %>% 
  count(playerName, playerId) %>% 
  select(playerName) %>% 
  count(playerName) %>%  
  arrange(playerName) 

# Cargo la informacion descargada de fbref.
info_fbref_copa2023 <- read_csv(here("data", "raw", "fbref", "info_jugadores_copa2023.csv"))
info_fbref_liga2023 <- read_csv(here("data", "raw", "fbref", "info_jugadores_liga2023.csv"))
info_fbref_copa2024 <- read_csv(here("data", "raw", "fbref", "info_jugadores_copa2024.csv"))
info_fbref_liga2024 <- read_csv(here("data", "raw", "fbref", "info_jugadores_liga2024.csv"))

# Filtro para quedarme solo con los nombres de los jugadores.
jugadores_fbref_copa2023 <- info_fbref_copa2023 %>%  
  mutate(Player = case_when(Player == "Francisco González" & stats_Squad == "Arg Juniors" ~ "Francisco González Metilli", 
                            Player == "Francisco González" & stats_Squad == "Newell's OB" ~ "Francisco Agustín González", 
                            Player == "Alan Rodríguez" & stats_Squad == "Rosario Central" ~ "Alan Francisco Rodriguez", 
                            Player == "Alan Rodríguez" & stats_Squad == "Arg Juniors" ~ "Alan Jesus Rodriguez",
                            Player == "Ezequiél Fernández" & stats_Squad == "Boca Juniors" ~ "Equi Fernández",
                            Player == "Matías Gómez" & stats_Squad == "Huracán" ~ "Matias Nicolas Gomez",
                            Player == "Nicolás Fernández" & stats_Squad == "Defensa y Just" ~ "Uvita Fernandez",
                            Player == "Nicolás Fernández" & stats_Squad == "Estudiantes–LP" ~ "Nicolas Andres Fernandez",
                            Player == "Julián Fernández" & stats_Squad == "Lanús" ~ "Julian Rodrigo Fernandez",
                            Player == "Tomás Palacios" & stats_Squad == "Talleres" ~ "Tiago Tomas Palacios",
                            Player == "Eric Ramírez" & stats_Squad == "Gimnasia–LP" ~ "Eric Ivan Ramirez",
                            Player == "Matías Galarza" & stats_Squad == "Talleres" ~ "Matias Galarza Fonda",
                            Player == "David González" & stats_Squad == "Lanús" ~ "Maximiliano González",
                            TRUE ~ Player)) %>% 
  select(Player, 
         stats_Squad, 
         stats_Pos) %>% 
  arrange(Player)

jugadores_fbref_liga2023 <- info_fbref_liga2023 %>% 
  mutate(Player = case_when(Player == "Francisco González" & stats_Squad == "Arg Juniors" ~ "Francisco González Metilli", 
                            Player == "Francisco González" & stats_Squad == "Newell's OB" ~ "Francisco Agustín González", 
                            Player == "Alan Rodríguez" & stats_Squad == "Rosario Central" ~ "Alan Francisco Rodriguez", 
                            Player == "Alan Rodríguez" & stats_Squad == "Arg Juniors" ~ "Alan Jesus Rodriguez",
                            Player == "Ezequiél Fernández" & stats_Squad == "Boca Juniors" ~ "Equi Fernández",
                            Player == "Matías Gómez" & stats_Squad == "Huracán" ~ "Matias Nicolas Gomez",
                            Player == "Nicolás Fernández" & stats_Squad == "Defensa y Just" ~ "Uvita Fernandez",
                            Player == "Nicolás Fernández" & stats_Squad == "Estudiantes–LP" ~ "Nicolas Andres Fernandez",
                            Player == "Julián Fernández" & stats_Squad == "Lanús" ~ "Julian Rodrigo Fernandez",
                            Player == "Tomás Palacios" & stats_Squad == "Talleres" ~ "Tiago Tomas Palacios",
                            Player == "Eric Ramírez" & stats_Squad == "Gimnasia–LP" ~ "Eric Ivan Ramirez",
                            Player == "David González" & stats_Squad == "Lanús" ~ "Maximiliano González",
                            TRUE ~ Player)) %>% 
  select(Player, 
         stats_Squad, 
         stats_Pos) %>% 
  arrange(Player)

jugadores_fbref_copa2024 <- info_fbref_copa2024 %>% 
  mutate(Player = case_when(Player == "Francisco González" & stats_Squad == "Belgrano" ~ "Francisco González Metilli", 
                            Player == "Francisco González" & stats_Squad == "Newell's OB" ~ "Francisco Agustín González", 
                            Player == "Alan Rodríguez" & stats_Squad == "Rosario Central" ~ "Alan Francisco Rodriguez", 
                            Player == "Alan Rodríguez" & stats_Squad == "Arg Juniors" ~ "Alan Jesus Rodriguez",
                            Player == "Ezequiél Fernández" & stats_Squad == "Boca Juniors" ~ "Equi Fernández",
                            Player == "Matías Gómez" & stats_Squad == "Huracán" ~ "Matias Nicolas Gomez",
                            Player == "Nicolás Fernández" & stats_Squad == "Defensa y Just" ~ "Uvita Fernandez",
                            Player == "Nicolás Fernández" & stats_Squad == "Estudiantes–LP" ~ "Nicolas Andres Fernandez",
                            Player == "Julián Fernández" & stats_Squad == "Newell's OB" ~ "Julian Rodrigo Fernandez",
                            Player == "Tomás Palacios" & stats_Squad == "Ind. Rivadavia" ~ "Tiago Tomas Palacios",
                            Player == "Tiago Palacios" & stats_Squad == "Estudiantes–LP" ~ "TTiago Asael Palacios",
                            Player == "Eric Ramírez" & stats_Squad == "Gimnasia–LP" ~ "Eric Ivan Ramirez",
                            Player == "David González" & stats_Squad == "Lanús" ~ "Maximiliano González",
                            Player == "Matías Galarza" & stats_Squad == "Talleres" ~ "Matias Galarza Fonda",
                            Player == "Santiago Sosa" & stats_Squad == "Racing Club" ~ "Santiago Leonel Sosa",
                            TRUE ~ Player)) %>% 
  select(Player, 
         stats_Squad, 
         stats_Pos) %>% 
  arrange(Player)

jugadores_fbref_liga2024 <- info_fbref_liga2024 %>% 
  mutate(Player = case_when(Player == "Francisco González" & stats_Squad == "Belgrano" ~ "Francisco González Metilli", 
                            Player == "Francisco González" & stats_Squad == "Newell's OB" ~ "Francisco Agustín González",
                            Player == "David González" & stats_Squad == "Lanús" ~ "Maximiliano González",
                            Player == "David González" & stats_Squad == "Defensa y Just" ~ "Maximiliano González",
                            Player == "Alan Rodríguez" & stats_Squad == "Rosario Central" ~ "Alan Francisco Rodriguez", 
                            Player == "Alan Rodríguez" & stats_Squad == "Arg Juniors" ~ "Alan Jesus Rodriguez",
                            Player == "Ezequiél Fernández" & stats_Squad == "Boca Juniors" ~ "Equi Fernández",
                            Player == "Matías Gómez" & stats_Squad == "Huracán" ~ "Matias Nicolas Gomez",
                            Player == "Nicolás Fernández" & stats_Squad == "Defensa y Just" ~ "Uvita Fernandez",
                            Player == "Nicolás Fernández" & stats_Squad == "Belgrano" ~ "Uvita Fernandez",
                            Player == "Nicolás Fernández" & stats_Squad == "Estudiantes–LP" ~ "Nicolas Andres Fernandez",
                            Player == "Julián Fernández" & stats_Squad == "Newell's OB" ~ "Julian Rodrigo Fernandez",
                            Player == "Tomás Palacios" & stats_Squad == "Ind. Rivadavia" ~ "Tiago Tomas Palacios",
                            Player == "Tiago Palacios" & stats_Squad == "Estudiantes–LP" ~ "TTiago Asael Palacios",
                            Player == "Eric Ramírez" & stats_Squad == "Gimnasia–LP" ~ "Eric Ivan Ramirez",
                            Player == "Eric Ramírez" & stats_Squad == "Huracán" ~ "Eric Ivan Ramirez",
                            Player == "Eric Ramírez" & stats_Squad == "Tigre" ~ "Eric Kleybel Ramirez",
                            Player == "Matías Galarza" & stats_Squad == "Talleres" ~ "Matias Galarza Fonda",
                            Player == "Santiago Sosa" & stats_Squad == "Racing Club" ~ "Santiago Leonel Sosa",
                            Player == "Santiago Sosa" & stats_Squad == "San Lorenzo" ~ "Santiago Nahuel Sosa",
                            TRUE ~ Player)) %>% 
  select(Player, 
         stats_Squad, 
         stats_Pos) %>% 
  arrange(Player)

# Divido los data sets en dos partes para poder pasarlos por una IA a que
# busque las coincidencias.
jugadores_fotmob_copa2023_1 <- jugadores_fotmob_copa2023 %>% 
  filter(str_detect(playerName, "^[A-Ja-jÁáÉéÍí]"))
jugadores_fotmob_copa2023_2 <- jugadores_fotmob_copa2023 %>% 
  filter(str_detect(playerName, "^[K-Zk-zÓÚóú]"))
jugadores_fbref_copa2023_1 <- jugadores_fbref_copa2023 %>% 
  filter(str_detect(Player, "^[A-Ja-jÁáÉéÍí]"))
jugadores_fbref_copa2023_2 <- jugadores_fbref_copa2023 %>% 
  filter(str_detect(Player, "^[K-Zk-zÓÚóú]"))

jugadores_fotmob_liga2023_1 <- jugadores_fotmob_liga2023 %>% 
  filter(str_detect(playerName, "^[A-Ja-jÁáÉéÍí]"))
jugadores_fotmob_liga2023_2 <- jugadores_fotmob_liga2023 %>% 
  filter(str_detect(playerName, "^[K-Zk-zÓÚóú]"))
jugadores_fbref_liga2023_1 <- jugadores_fbref_liga2023 %>% 
  filter(str_detect(Player, "^[A-Ja-jÁáÉéÍí]"))
jugadores_fbref_liga2023_2 <- jugadores_fbref_liga2023 %>% 
  filter(str_detect(Player, "^[K-Zk-zÓÚóú]"))

jugadores_fotmob_copa2024_1 <- jugadores_fotmob_copa2024 %>% 
  filter(str_detect(playerName, "^[A-Ja-jÁáÉéÍí]"))
jugadores_fotmob_copa2024_2 <- jugadores_fotmob_copa2024 %>% 
  filter(str_detect(playerName, "^[K-Zk-zÓÚóú]"))
jugadores_fbref_copa2024_1 <- jugadores_fbref_copa2024 %>% 
  filter(str_detect(Player, "^[A-Ja-jÁáÉéÍí]"))
jugadores_fbref_copa2024_2 <- jugadores_fbref_copa2024 %>% 
  filter(str_detect(Player, "^[K-Zk-zÓÚóú]"))

jugadores_fotmob_liga2024_1 <- jugadores_fotmob_liga2024 %>% 
  filter(str_detect(playerName, "^[A-Ja-jÁáÉéÍí]"))
jugadores_fotmob_liga2024_2 <- jugadores_fotmob_liga2024 %>% 
  filter(str_detect(playerName, "^[K-Zk-zÓÚóú]"))
jugadores_fbref_liga2024_1 <- jugadores_fbref_liga2024 %>% 
  filter(str_detect(Player, "^[A-Ja-jÁáÉéÍí]"))
jugadores_fbref_liga2024_2 <- jugadores_fbref_liga2024 %>% 
  filter(str_detect(Player, "^[K-Zk-zÓÚóú]"))

# Guardo como csv los archivos.
write.csv(jugadores_fotmob_copa2023_1, file = here("data", "intermediate", "jugadores_fotmob_copa2023_1.csv"))
write.csv(jugadores_fotmob_copa2023_2, file = here("data", "intermediate", "jugadores_fotmob_copa2023_2.csv"))
write.csv(jugadores_fbref_copa2023_1, file = here("data", "intermediate", "jugadores_fbref_copa2023_1.csv"))
write.csv(jugadores_fbref_copa2023_2, file = here("data", "intermediate", "jugadores_fbref_copa2023_2.csv"))

write.csv(jugadores_fotmob_liga2023_1, file = here("data", "intermediate", "jugadores_fotmob_liga2023_1.csv"))
write.csv(jugadores_fotmob_liga2023_2, file = here("data", "intermediate", "jugadores_fotmob_liga2023_2.csv"))
write.csv(jugadores_fbref_liga2023_1, file = here("data", "intermediate", "jugadores_fbref_liga2023_1.csv"))
write.csv(jugadores_fbref_liga2023_2, file = here("data", "intermediate", "jugadores_fbref_liga2023_2.csv"))

write.csv(jugadores_fotmob_copa2024_1, file = here("data", "intermediate", "jugadores_fotmob_copa2024_1.csv"))
write.csv(jugadores_fotmob_copa2024_2, file = here("data", "intermediate", "jugadores_fotmob_copa2024_2.csv"))
write.csv(jugadores_fbref_copa2024_1, file = here("data", "intermediate", "jugadores_fbref_copa2024_1.csv"))
write.csv(jugadores_fbref_copa2024_2, file = here("data", "intermediate", "jugadores_fbref_copa2024_2.csv"))

write.csv(jugadores_fotmob_liga2024_1, file = here("data", "intermediate", "jugadores_fotmob_liga2024_1.csv"))
write.csv(jugadores_fotmob_liga2024_2, file = here("data", "intermediate", "jugadores_fotmob_liga2024_2.csv"))
write.csv(jugadores_fbref_liga2024_1, file = here("data", "intermediate", "jugadores_fbref_liga2024_1.csv"))
write.csv(jugadores_fbref_liga2024_2, file = here("data", "intermediate", "jugadores_fbref_liga2024_2.csv"))

# Objetos que necesita el script 03b:
saveRDS(todos_los_datos, here("data", "intermediate", "todos_los_datos.rds"))
saveRDS(list(copa2023 = jugadores_fbref_copa2023,
             liga2023 = jugadores_fbref_liga2023,
             copa2024 = jugadores_fbref_copa2024,
             liga2024 = jugadores_fbref_liga2024),
        here("data", "intermediate", "jugadores_fbref.rds"))
