# Modelos de probabilidad de gol: evaluación del efecto jugador en la Liga Profesional de Fútbol

Repositorio de la tesina de grado de **Matías Luchini** (Facultad de Ciencias Económicas y Estadística, Universidad Nacional de Rosario). Contiene el código para reproducir el análisis: obtención de los datos de disparos de la Liga Profesional y la Copa de la Liga (temporadas 2023 y 2024), limpieza de la base, ajuste de tres modelos bayesianos de xG y generación de resultados y figuras.

## Datos

Los archivos pesados no están en el repositorio. Se publicaron en Zenodo:

| Contenido | DOI | Acceso |
|---|---|---|
| Muestras de la distribución posterior de los modelos (`.nc` y CSV de 40 jugadores) | [10.5281/zenodo.23221639](https://doi.org/10.5281/zenodo.23221639) | Abierto |
| Datos originales de disparos descargados de FotMob (un CSV por partido) | [10.5281/zenodo.23238800](https://doi.org/10.5281/zenodo.23238800) | Restringido (se concede para replicación) |

**Dónde copiar lo descargado:**

- Muestras de los modelos → `data/processed/muestras_modelos/`
- CSV de partidos de FotMob → `data/raw/fotmob/partidos/<torneo><año>/` (carpetas `copa2023`, `liga2023`, `copa2024` y `liga2024`). Esta carpeta **no está en el repositorio**: hay que crearla y llenarla pidiendo acceso al depósito de Zenodo o regenerándola con el notebook `02_web_scraping.ipynb`.

Los datos originales provienen de FotMob y FBref y están sujetos a sus términos de uso. Se comparten únicamente para permitir la replicación de la tesina.

## Estructura del repositorio

```
tesina_modelos_xG/
├── README.md
├── LICENSE
├── tesina_modelos_xG.Rproj
├── renv.lock
├── data/
│   ├── manual/          Archivos armados a mano o con herramientas del navegador
│   ├── raw/
│   │   ├── fbref/       info_jugadores_<torneo>.csv
│   │   └── fotmob/
│   │       ├── equipos/     id_equipos_<torneo>.csv
│   │       └── partidos/    (solo local, ver sección Datos)
│   ├── intermediate/    Archivos intermedios del paso 03a
│   ├── processed/       Bases finales y muestras de los modelos
│   └── extras/          Datos de un partido usado como ejemplo
├── scripts/
│   ├── 01_extraer_codigos_fotmob.R
│   ├── 02_web_scraping.ipynb
│   ├── 03a_preparar_datos.R
│   ├── 03b_limpieza_disparos.R
│   ├── 04_entrenamiento_modelos.ipynb
│   ├── 05_analisis_de_resultados.R
│   └── 06_graficos_para_informe.R
└── output/
    └── figuras/
```

Los archivos `.rds`, la carpeta `data/raw/fotmob/partidos/` y la carpeta `data/processed/muestras_modelos/` están excluidos de Git (ver `.gitignore`).

## Requisitos

- **R 4.5.0** y los paquetes registrados en `renv.lock`. Para instalarlos, abrir `tesina_modelos_xG.Rproj` y correr en la consola:
  ```r
  renv::restore()
  ```
  En Windows, instalar `rstan` puede requerir Rtools.
- **Google Colab** para los notebooks `02` y `04`. Las dependencias están en las primeras celdas de cada notebook. El scraping usa `LanusStats==1.8.2`.

Todas las rutas son relativas a la raíz del proyecto (paquete `here`), así que hay que abrir siempre el `.Rproj`.

## Orden de ejecución

| Paso | Dónde | Qué hace | Entra | Sale |
|---|---|---|---|---|
| 1 | **Manual** (navegador) | Se copia con la extensión Table Capture la tabla de partidos de cada torneo desde FotMob | FotMob | `data/manual/tabla_para_extraer_codigos_fotmob.xlsx` |
| 2 | R | `01_extraer_codigos_fotmob.R` extrae los códigos de partido de FotMob | tabla del paso 1 | `data/manual/codigos_fotmob.xlsx` |
| 3 | Colab | `02_web_scraping.ipynb`, secciones de FotMob: descarga los disparos de cada partido, en tandas de 10 archivos (límite de descargas de Colab) | `codigos_fotmob.xlsx` | `data/raw/fotmob/partidos/<torneo>/fotmob_<torneo>_partido<N>.csv` |
| 4 | Colab | `02_web_scraping.ipynb`, sección de FBref: descarga la posición de los jugadores | — | `data/raw/fbref/info_jugadores_<torneo>.csv` |
| 5 | Colab | `02_web_scraping.ipynb`, sección de equipos: descarga los equipos con su ID de FotMob | — | `data/raw/fotmob/equipos/id_equipos_<torneo>.csv` |
| 6 | R | `03a_preparar_datos.R` junta los partidos, corrige nombres y prepara las listas de jugadores (tarda varios minutos) | pasos 3 y 4 | `data/intermediate/` |
| 7 | **Manual** | Se emparejan los nombres de jugadores de FotMob con los de FBref. Una parte se hizo con ayuda de una IA y el resto a mano | `data/intermediate/jugadores_*_1/_2.csv` | `data/manual/nombre_jugadores_fotmob_fbref.xlsx` |
| 8 | R | `03b_limpieza_disparos.R` hace la limpieza final y arma las bases | pasos 5, 6 y 7 | `data/processed/*.csv` |
| 9 | Colab | `04_entrenamiento_modelos.ipynb` ajusta los tres modelos bayesianos | `disparos_2023_2024_reducido.csv` | `.nc` y `muestras_40_jugadores_equiespaciados.csv` |
| 10 | R | `05_analisis_de_resultados.R` analiza los resultados de los modelos | pasos 8 y 9 | figuras en `output/figuras/` |
| 11 | R | `06_graficos_para_informe.R` genera los gráficos del informe | pasos 8 y 9 | figuras en `output/figuras/` |

Notas:

- **Para replicar sin repetir el scraping**, se puede descargar de Zenodo los CSV de partidos (depósito restringido), copiarlos a `data/raw/fotmob/partidos/` y correr desde el paso 6; el resto de los insumos ya está en el repositorio.
- **Para replicar solo los resultados** (pasos 10 y 11), alcanza con el repositorio (`data/processed/`) y las muestras de los modelos descargadas de Zenodo.
- El scraping depende de que FotMob y FBref sigan respondiendo igual, por lo que puede no volver a funcionar. Por eso los datos originales están archivados en Zenodo.
- El paso 7 no se puede regenerar automáticamente. `nombre_jugadores_fotmob_fbref.xlsx` tiene una hoja por torneo (`copa2023`, `liga2023`, `copa2024`, `liga2024`) con los nombres de FotMob y FBref de cada jugador, y está incluido en el repositorio.
- La sección de FBref del notebook `02` también descarga `info_arqueros_<torneo>.csv`, que no se usa en el análisis.

## Bases en `data/processed/`

Las genera `03b_limpieza_disparos.R`:

| Archivo | Contenido | Lo usa |
|---|---|---|
| `disparos_2023_2024.csv` | Todos los disparos de las cuatro competencias, con las variables construidas y la localía, sin los filtros del modelo | Análisis descriptivos |
| `datos_analisis.csv` | Disparos filtrados para el modelo (sin autogoles, tiros libres, córners, penales ni otras jugadas a balón parado), con las variables estandarizadas | `05_analisis_de_resultados.R` |
| `disparos_2023_2024_reducido.csv` | Solo las variables del modelo: `id_jugador`, `posicion_nueva`, `distancia_est`, `angulo_est`, `interaccion2`, `gol`, `xG` | `04_entrenamiento_modelos.ipynb` |
| `goles_por_jugador.csv` | Disparos, goles y proporción de gol por jugador | Análisis y gráficos |

## Insumos manuales

- **`data/manual/tabla_para_extraer_codigos_fotmob.xlsx`**: tabla de partidos de cada torneo, copiada de FotMob con la extensión Table Capture.
- **`data/manual/nombre_jugadores_fotmob_fbref.xlsx`**: emparejamiento de nombres de jugadores entre FotMob y FBref (ver paso 7).
- **`data/manual/resultados_y_xG.xlsx`**: resultados y xG por partido, obtenidos de FBref (no se registró la fecha de descarga). Se usa solo para construir la variable de localía (`fue_local_disparo`), que **no se incluye en ningún modelo** de la tesina.
- **`data/extras/disparos_nuevos_racing_union.csv`**: disparos del partido Racing Club vs. Unión, copiados manualmente desde la [página del partido en FotMob](https://www.fotmob.com/es/matches/racing-club-vs-union/3d6wk8#4698510:tab=stats). Se usa como ejemplo de aplicación del modelo a un partido que no está en la muestra.

## Licencia

El código se distribuye bajo licencia MIT (ver `LICENSE`). Los datos provienen de FotMob y FBref y están sujetos a sus términos de uso; la licencia de cada depósito de Zenodo figura en su página.

## Cómo citar

Luchini, M. *Modelos de probabilidad de gol: evaluación del efecto jugador en la Liga Profesional de Fútbol*. Tesina de grado, Universidad Nacional de Rosario. Código: https://github.com/matiasluchini/tesina_modelos_xG. Datos: https://doi.org/10.5281/zenodo.23221639 y https://doi.org/10.5281/zenodo.23238800.
