## =========================================================
## 1. Librerías ####
## =========================================================

library(rakeR)
library(RPostgres)
library(DBI)
library(dplyr)
library(sf)
library(ggplot2)


## =========================================================
## 2. Entradas ####
## =========================================================

# casen en formato .rds

ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds"

casen_raw = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)


## =========================================================
## 3) PRE-PROCESAMIENTO
## =========================================================

## datos CENSO
# Extraemos los nombres de las columnas que nos son útiles
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))


# Generamos los niveles para edad, escolaridad y sexo

age_levels = grep("^edad", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)

## datos CASEN 
# ingenieria de dagtos para re codificar y obtener la variable de interes: 

# Se seleccionan las variables de interés

vars_base = c("estrato", # Para extraer comuna
              "edad",
              "sexo",
              "s28") # s28= variable a microsimular

# Se filtra la CASEN con las variables de interés
casen = casen_raw[, vars_base, drop = FALSE]

# Limpiar memoria
# Importante para objetos muy grandes
rm(casen_raw)

# Extraemos la comuna para cada registro
casen$Comuna = substr(as.character(casen$estrato), 1, 5)

# Se elimina la columna estrato
casen$estrato = NULL

# Quitar etiquetas haven y cambiar tipo de datos
casen$edad = as.integer(unclass(casen$edad))
casen$sexo = as.integer(unclass(casen$sexo))
casen$s28 = as.integer(unclass(casen$s28))


#  Crear variable binaria de hipertensión
casen <- casen %>%
  mutate(
    diabetes = case_when(
      s28 == 3 ~ 1,        # Tratamiento por DIABETES
      s28 > 1 ~ 0,         # Otros tratamientos médicos
      s28 == -88 ~ NA_real_  # No responde / no aplica
    )
  )


# Añadimos a un ID único
casen$ID = as.character(seq_len(nrow(casen)))

#Filtrar casos válidos
casen <- casen %>% filter(!is.na(diabetes))

## re-codificación

# Categorización de edad
casen$edad_cat = cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

# Recodificación de sexo
casen$sexo_cat = factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)

## =========================================================
## 4) MICROSIMULACIÓN ESPACIAL
## =========================================================

# crear la lista de constraints POR COMUNA
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)

# Lista de INDS 
inds_list = split(casen, casen$Comuna)

sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i    = cons_censo_comunas[[zona]]
  
  #elimina columna esc
  cons_i <- cons_i[, !grepl("^esco", names(cons_i)), drop = FALSE]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Sexo")
  
  
  
  w_frac  = weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad","Sexo"))
  sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i,
        tmp[, c("ID","diabetes")],
        by = "ID", all.x = TRUE)
})

# Data Frame de toda la población
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

# Se agregan los datos
zonas_diabetes = aggregate(
  diabetes ~ zone,
  data = sim_df,
  FUN = function(x) mean(x, na.rm = TRUE) 
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
   delete_layer= TRUE)
)

# =============================================================================
# 6) MAPAS 
# =============================================================================
#  Mapa temático univariado: prevalencia de diabetes
map_diabetes <- ggplot(zonas_gs_diabetes) +
  geom_sf(aes(fill = prev_diabetes_pct), color = "grey85", size = 0.2) +
  scale_fill_distiller(
    palette= "RdYlBu",
    direction = 1,
    na.value = "grey90",
    name     = "% Diabetes (microsim.)"
  ) +
  labs(
    title    = "Prevalencia microsimulada de diabetes",
    subtitle = "Gran Santiago — zona censal (CASEN s28 == 3)",
    caption  = "Fuente: CASEN RM + Censo 2017 (edad, sexo) — microsimulación con rakeR"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    plot.subtitle= element_text(hjust = 0.5)
  )

print(map_diabetes)

# ---------------------------------------------------------
# TOP 10 COMUNAS CON MAYOR PREVALENCIA

# Agrupar por comuna y calcular promedio de prevalencia zonal
top10_comunas <- zonas_gs_diabetes |>
  st_drop_geometry() |>
  group_by(nom_comuna) |>
  summarise(prev_diabetes_comuna = mean(prev_diabetes_pct, na.rm = TRUE)) |>
  arrange(desc(prev_diabetes_comuna)) |>
  slice_head(n = 10)

# Gráfico del Top 10 de comunas

grafico_top10 <- ggplot(top10_comunas,
                        aes(x = reorder(nom_comuna, prev_diabetes_comuna),
                            y = prev_diabetes_comuna)) +
  geom_col(fill = "#cb181d") +
  coord_flip() +
  labs(
    title = "Top 10 comunas con mayor prevalencia microsimulada de diabetes",
    subtitle = "Gran Santiago — estimación a nivel de zona censal (CASEN s28 == 3)",
    x = "Comuna",
    y = "% de prevalencia de diabetes"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    plot.subtitle= element_text(hjust = 0.5)
  )

print(grafico_top10)
