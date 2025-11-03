## =========================================================
## 1. Librerías ####
## =========================================================
library(rakeR)
library(RPostgres)
library(DBI)
library(dplyr)
library(sf)

## =========================================================
## 2. Entradas ####
## =========================================================

ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds"

casen_raw     = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)

## =========================================================
## 3) PRE-PROCESAMIENTO
## =========================================================

###CENSO: identificar columnas de constraints

# quitamos identificadores
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))

# detectar niveles que ya vienen en el censo como dummies
age_levels  = grep("^edad", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)

### CASEN: seleccionar variables útiles
# s28 = pregunta de salud. opción 3 = diabetes.
vars_base = c(
  "estrato",  # para extraer comuna
  "edad",
  "sexo",
  "s28"       # variable objetivo a microsimular
)

# Se filtra la CASEN con las variables de interés
casen = casen_raw[, vars_base, drop = FALSE]
# Limpiar memoria
rm(casen_raw)

# Extraer comuna desde estrato 
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
# Se elimina la columna estrato
casen$estrato = NULL

# Quitar etiquetas haven y cambiar tipo de datos
casen$edad = as.integer(unclass(casen$edad))
casen$sexo = as.integer(unclass(casen$sexo))
casen$s28  =as.integer(unclass(casen$s28))  

## VARIABLE OBJETIVO: DIABETES
# s28 == 3 = Diabetes
# Creamos variable binaria
casen$diabetes = ifelse(casen$s28 == 3, 1L, 0L)

# Añadimos a un ID único
casen$ID = as.character(seq_len(nrow(casen)))

#Filtrar casos válidos
casen = casen %>% filter(!is.na(diabetes))

##  RE-CODIFICACIÓN: dejar CASEN con las MISMAS categorías
##     que existen en las constraints del CENSO

# edad en categorías 
casen$edad_cat = cut(
  casen$edad,
  breaks = c(0, 30, 40, 50, 60, 70, 80, Inf),
  labels = age_levels,
  right  = FALSE,
  include.lowest = TRUE
)

# sexo en los mismos labels que el censo
casen$sexo_cat = factor(
  ifelse(casen$sexo == 2, sexo_levels[1],   # en CASEN 2 = mujer
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),  # 1 = hombre
  levels = sexo_levels
)

## =========================================================
## 4) MICROSIMULACIÓN ESPACIAL
## =========================================================

# crear lista de constrainst por COMUNA
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)

# lista INDS
inds_list = split(casen, casen$Comuna)

# Aplicar microsimulación 
sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i = cons_censo_comunas[[zona]]
  #elimina columna esc
  cons_i <- cons_i[, !grepl("^esco", names(cons_i)), drop = FALSE]
  # ordenar columnas: primero GEOCODIGO y luego las dummies
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  # individuos de esa zona
  tmp = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","sexo_cat"), drop = FALSE]
  names(inds_i) <- c("ID","Edad","Sexo")

  # Raking
  w_frac  = weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad","Sexo"))
  sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i,
        tmp[, c("ID","diabetes")],
        by = "ID", all.x = TRUE)
})
  
# Data Frame de toda la población
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

# Calcular prevalencia = (personas con diabetes)/(total personas simuladas)
zonas_diabetes = aggregate(
  diabetes ~ zone,
  data = sim_df,
  FUN  = function(x) median(x, na.rm = TRUE) #promedio /proporcion
)
names(zonas_diabetes) <- c("geocodigo", "prev_diabetes")

# Si quieres en porcentaje:
zonas_diabetes$prev_diabetes_pct <- round(zonas_diabetes$prev_diabetes * 100, 2)

## =========================================================
## 5) Conexión a BD ####
## =========================================================

db_host = "localhost"
db_port = 5432
db_name = "censo_rm_clases"
db_user = "postgres"
db_password = "postgres"

# Conexión
con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)


# Escribimos la tabla dentro de la BD

dbWriteTable(
  con,
  name = DBI::SQL("output.zonas_diabetes"),
  value = zonas_diabetes,
  row.names = FALSE
)

# Leer zonas censales a partir de la bd

query_gs = "
SELECT *
FROM dpa.zonas_censales_rm
WHERE urbano = 1 AND (
    nom_provin = 'SANTIAGO' OR
    nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO')
)"

zonas_gs = st_read(con, query = query_gs)


# Se cambia el tipo de dato de la columna de geocodigo
zonas_gs$geocodigo = as.character(zonas_gs$geocodigo)

# Unir por geocódigo
zonas_gs_diabetes = left_join(zonas_gs, zonas_diabetes, by = "geocodigo")


# Escribir tabla espacial

st_write(
  zonas_gs_diabetes,
  dsn = con,
  layer = DBI::SQL("output.zc_diabetes_microsim",
                   driver = "PostreSQL")
)

# =============================================================================
# 6) MAPAS 
# =============================================================================
