## =========================================================
## 1) LIBRERÍAS
## =========================================================
library(rakeR)
library(RPostgres)
library(DBI)
library(dplyr)
library(sf)
library(ggplot2)
library(data.table)
library(factoextra)

## =========================================================
## 2) ENTRADAS
## =========================================================
ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds"

casen_raw = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)

## =========================================================
## 3) PRE-PROCESAMIENTO
## =========================================================

### 3.1 CENSO ###
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))

# Niveles
age_levels  = grep("^edad", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)

### 3.2 CASEN ###
vars_base = c("estrato", "edad", "sexo", "s28", "esc", "ypc") 
casen = casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# Identificar comuna
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

# Transformar tipos
casen$edad = as.integer(unclass(casen$edad))
casen$sexo = as.integer(unclass(casen$sexo))
casen$s28  = as.integer(unclass(casen$s28))
casen$esc  = as.integer(unclass(casen$esc))
casen$ypc  = as.numeric(unclass(casen$ypc))

## --- Variable DIABETES (microsimulación objetivo) ---
casen <- casen %>%
  mutate(
    diabetes = case_when(
      s28 == 3 ~ 1,        # Diabetes
      s28 > 1  ~ 0,        # Otros tratamientos
      s28 == -88 ~ NA_real_
    )
  )

casen <- casen %>% filter(!is.na(diabetes))
casen$ID = as.character(seq_len(nrow(casen)))

## --- Categorización de edad ---
casen$edad_cat = cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

## --- Recodificación de sexo ---
casen$sexo_cat = factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)

## =========================================================
## 4) MICROSIMULACIÓN ESPACIAL
## =========================================================
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)
inds_list = split(casen, casen$Comuna)

sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i = cons_censo_comunas[[zona]]
  cons_i <- cons_i[, !grepl("^esco", names(cons_i)), drop = FALSE]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Sexo")
  
  w_frac = weight(cons = cons_i, inds = inds_i, vars = c("Edad","Sexo"))
  sim_i = integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  merge(sim_i, tmp[, c("ID","diabetes","esc","ypc","edad")], by = "ID", all.x = TRUE)
})

sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

## =========================================================
## 5) AGREGAR VARIABLES DE SALUD Y SOCIOECONÓMICAS POR ZONA
## =========================================================

zonas_vars = sim_df %>%
  group_by(zone) %>%
  summarise(
    prev_diabetes      = mean(diabetes, na.rm = TRUE),
    prev_diabetes_pct  = round(prev_diabetes * 100, 2),
    promedio_escolaridad_18mas = round(mean(esc[edad >= 18], na.rm = TRUE), 2),
    ingreso_promedio   = round(mean(ypc, na.rm = TRUE), 0)
  ) %>%
  rename(geocodigo = zone)

## =========================================================
## 6) CONEXIÓN A LA BASE DE DATOS Y ESCRITURA
## =========================================================
db_host = "localhost"
db_port = 5432
db_name = "censo_rm_clases"
db_user = "postgres"
db_password = "postgres"

con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# Guardar tabla resumen (no espacial)
dbWriteTable(
  con,
  name = DBI::SQL("output.zonas_diabetes"),
  value = zonas_vars,
  row.names = FALSE,
  overwrite = TRUE
)

## =========================================================
## 7) UNIÓN CON ZONAS CENSALES Y EXPORTACIÓN ESPACIAL
## =========================================================
query_gs = "
SELECT *
FROM dpa.zonas_censales_rm
WHERE urbano = 1 AND (
    nom_provin = 'SANTIAGO' OR
    nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO')
)"
zonas_gs = st_read(con, query = query_gs)
zonas_gs$geocodigo = as.character(zonas_gs$geocodigo)

# Unir geometría con los resultados
zonas_gs_diabetes = left_join(zonas_gs, zonas_vars, by = "geocodigo")

# Exportar a la BD
st_write(
  zonas_gs_diabetes,
  dsn = con,
  layer = DBI::SQL("output.zc_diabetes_microsim"),
  delete_layer = TRUE
)

## =========================================================
## 8) SELECCIÓN DE VARIABLES PARA K-MEANS
## =========================================================
# Crear ID único
zonas_gs_diabetes$id <- seq_len(nrow(zonas_gs_diabetes))

# Crear dataframe sin geometría y solo con las columnas necesarias
vars_clusters <- zonas_gs_diabetes %>%
  st_drop_geometry() %>%
  dplyr::select(id, prev_diabetes_pct, promedio_escolaridad_18mas, ingreso_promedio)

# Convertir a numéricas
vars_clusters <- vars_clusters %>%
  mutate(across(c(prev_diabetes_pct, promedio_escolaridad_18mas, ingreso_promedio), as.numeric))

# Eliminar filas con NA, guardando ID
vars_clusters_valid <- vars_clusters %>% na.omit()

# Escalar variables
vars_scaled <- scale(vars_clusters_valid[, c("prev_diabetes_pct", "promedio_escolaridad_18mas", "ingreso_promedio")])

## =========================================================
## 9) ANÁLISIS DE AGRUPAMIENTO (K-MEANS)
## =========================================================
# Método del codo
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(title = "Método del Codo", x = "Número de Clusters (k)", y = "WSS")

set.seed(123)
km <- kmeans(vars_scaled, centers = 3, nstart = 25)

# Asociar clusters al subset válido
vars_clusters_valid$cluster <- as.factor(km$cluster)

# Unir nuevamente al dataset espacial
zonas_gs_diabetes <- zonas_gs_diabetes %>%
  left_join(vars_clusters_valid[, c("id", "cluster")], by = "id")

# Verifica que la columna exista
table(is.na(zonas_gs_diabetes$cluster))

## =========================================================
## 10) VISUALIZACIÓN — ANÁLISIS DE CLUSTERS
## =========================================================
## Prevalencia de Diabetes vs Escolaridad Promedio
ggplot(zonas_gs_diabetes %>% st_drop_geometry() %>% filter(!is.na(cluster)),
       aes(x = promedio_escolaridad_18mas, y = prev_diabetes_pct, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    title = "Prevalencia de Diabetes vs Escolaridad Promedio",
    subtitle = "Agrupación por K-Means (k = 3)",
    x = "Promedio de años de escolaridad (≥18 años)",
    y = "Prevalencia de diabetes (%)"
  ) +
  scale_color_brewer(palette = "Set2", name = "Cluster") +
  theme_minimal()

## Prevalencia de diabetes VS Ingreso promedio
ggplot(
  zonas_gs_diabetes %>% 
    st_drop_geometry() %>% 
    filter(!is.na(cluster)),
  aes(
    x = ingreso_promedio,
    y = prev_diabetes_pct,
    color = cluster
  )
) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    title = "Prevalencia de Diabetes vs Ingreso Promedio",
    subtitle = "Agrupación por K-Means (k = 3)",
    x = "Ingreso promedio del hogar (CASEN)",
    y = "Prevalencia de diabetes (%)"
  ) +
  scale_color_brewer(palette = "Set1", name = "Cluster") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

## MAPA DE CLUSTERS - GRAN SANTIAGO
# Obtener bounding box del mapa
bbox = st_bbox(zonas_gs_diabetes)

# Crear mapa de clusters
mapa_clusters = ggplot() +
  geom_sf(
    data = zonas_gs_diabetes,
    aes(fill = cluster),
    color = NA
  ) +  # Zonas censales coloreadas por cluster
  
  # Límites comunales (usando zonas_gs, que sí existe)
  geom_sf(
    data = zonas_gs,
    fill = NA,
    color = "black",
    size = 0.3
  ) +
  
  # Etiquetas comunales usando centroides de zonas_gs
  geom_sf_text(
    data = zonas_gs %>% group_by(nom_comuna) %>% summarise() %>% st_centroid(),
    aes(label = nom_comuna),
    size = 2.5,
    fontface = "bold",
    color = "black"
  ) +
  
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  
  labs(
    title = "Distribución Espacial de Clusters de Zonas Censales",
    subtitle = "Gran Santiago - K-Means (k = 3)",
    caption = "Fuente: Microsimulación CASEN + CENSO 2017"
  ) +
  
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

print(mapa_clusters)

#Mapa variabilidad intra-comunal
# Calcular indice de Shannon por comuna
shannon_df <- zonas_gs_diabetes %>%
  st_drop_geometry() %>%
  filter(!is.na(cluster)) %>%   # asegurarse que no haya NA
  group_by(nom_comuna, cluster) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n),
         H = -p * log(p)) %>%
  summarise(shannon = sum(H), .groups = "drop")  # índice final

# Crear sf comunal uniendo zonas
sf_comunas_santiago <- zonas_gs %>%
  group_by(nom_comuna) %>%
  summarise(geometry = st_union(geom))

sf_comunas_shannon <- sf_comunas_santiago %>%
  left_join(shannon_df, by = "nom_comuna")

ggplot(sf_comunas_shannon) +
  geom_sf(aes(fill = shannon), color = "white", size = 0.3) +
  scale_fill_viridis_c(option = "plasma", direction = -1,
                       name = "Índice de Shannon") +
  labs(
    title = "Variabilidad Intracomunal de Clusters",
    subtitle = "Índice de Shannon aplicado a zonas censales",
    caption = "Fuente: Microsimulación CASEN + CENSO 2017"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

