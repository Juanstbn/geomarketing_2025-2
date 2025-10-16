##1. Librerias
library(rakeR)
library(RPostgres)
library(DBI)

##2. Entradas

#Rutas de las entradas
ruta_casen= "data/casen_rm.rds"
ruta_censo= "data/cons_censo_df.rds"

#DF's de CASEN y CENSO
casen_raw= readRDS(ruta_casen)
cons_censo_df= readRDS(ruta_censo)



##3. PRE- PROCESAMIENTO

###3.1 CENSO

#Extraemos los nombres de las columnas que nos son utiles
col_cons= sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))


#Generamos los niveles para edad escolaridad y sexo

age_levels= grep("^edad", col_cons, value = TRUE)
esc_levels= grep("^esco", col_cons, value = TRUE)
sexo_levels= grep("^sexo", col_cons, value = TRUE)


###3.2 CASEN

# se seleccionan las variables de interes

vars_base = c("estrato", #para extraer comuna
              "esc", #para años de escolaridad
              "edad",
              "sexo",
              "e6a", #para imputar datos
              "ypc") # IMPORTANTE: VARIABLE A MICROSIMULAR

