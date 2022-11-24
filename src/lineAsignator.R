#'
#' @title Line Asignator Sistem
#'
#' @description Algoritmo desarrollado para la asignación de contadores a linea
#' Como punto de inicio, se tienen:
#' - las curvas de carga horarias (CCH) de los contadores (CNs)
#' - las de tensiones e intensidades de los contadores de supervisión (SPV) de
#'   los Centros de Transformación (CT).
#' Esta versión está modificada a partir de la anterior para dar como opción
#' las diferentes fases de los diferentes supervisores que pudiera tener un ct
#' en cada una de sus posiciones y así poder encontrar errores de inventario
#' en caso de que un contador fuera asignado a una fase-spv que no es el que
#' marca el inventario.
#'
#' @details se utilizan los scripts de
#' - f_ConsultaDatos_v3.R: función encargada de descargar y guardar los datos
#' - f_lmMethod.R: función que asigna los contadores
#' - f_pas_v3.R: función que itera sobre la asignación de contadores a cada uno
#' de los posibles supervisores del CT.
#'
#'
#' @return la fase-supervisor de la que cuelga cada contador
#'
#' @author UCT
#'

rm(list = ls())
cat("\014")

# cargamos librerias
libs <- c("tidyverse", "here")
sapply(libs[!libs %in% installed.packages()], install.packages)
suppressPackageStartupMessages( sapply(libs, library, character.only = TRUE))

source(here("src", "f_ConsultaDatos.R"))
source(here("src","f_las.R"))


# opciones lectura datos
cod_ct = 10200580
pos = 0
From = '01/01/2021'
To = '01/11/2022'
loss = 0.5
# allowNegDif = FALSE
rmTriPhaseNoise = FALSE
rmOnlyTriphaseCn = FALSE
# opciones estimación
onlyPos = TRUE        # fijo

LPdata <- ConsultaDatos(COD_CT = cod_ct 
                        , posicion = pos
                        , from = From, to = To
                        , maxLoss = loss
                        , minLoss = -0.2
                        , allowNegDif = FALSE
                        , rmTriPhaseNoise = FALSE, rmOnlyTriphaseCn = FALSE
                        , overWrite = TRUE)


res <- LAS(LPdata
            # opciones estimación
            , significanceLvl = 0.95
            # , step = 0.05
            , step = c(0.25, 0.125, 0.075, 0.05, 0.025)
            , mda = 0.5)


# mostramos resultados
res$cnData %>%
  # filter(status == "ok") %>%
  group_by(NUM_LINEA_BT, line) %>%
  tally() %>%
  pivot_wider(names_from = NUM_LINEA_BT, values_from = n, values_fill = 0) %>%
  mutate(orden = nchar(line)) %>%
  arrange(line, orden) %>%
  select(-orden)
