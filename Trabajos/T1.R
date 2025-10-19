# =============================================================================
# 1) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(biscale)

# =============================================================================
# 2) CONEXIÓN A BASE DE DATOS
# =============================================================================
con <- dbConnect(
  Postgres(),
  dbname   = "censo_v_2017",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

# =============================================================================
# 3) CONSULTA SQL: IVMP Y NIVEL EDUCACIONAL (MAYORES DE 18 AÑOS)
# =============================================================================
#  filtran solo personas con P09 > 18 años en el cálculo educativo

sql_indicadores <- "
WITH agg AS (
  SELECT
    z.geocodigo::double precision AS geocodigo,
    c.nom_comuna,

    -- IVMP: % viviendas con materialidad precaria (muros OR techo OR piso)
    COALESCE(ROUND(
      COUNT(*) FILTER (
        WHERE v.p02 = 1 AND (
          v.p03a = 6 OR          -- muros: materiales precarios
          v.p03b IN (6,7) OR     -- techo: precarios o sin cubierta sólida
          v.p03c IN (4,5)        -- piso: capa de cemento sobre tierra / tierra
        )
      ) * 100.0 /
      NULLIF(COUNT(*) FILTER (WHERE v.p02 = 1), 0), 2), 0) AS ivmp,

    -- % personas mayores de 18 años con P15 >= 7 (media completa o superior)
    COALESCE(ROUND(
      COUNT(*) FILTER (WHERE p.p09 > 18 AND p.p15 >= 7) * 100.0 /
      NULLIF(COUNT(*) FILTER (WHERE p.p09 > 18), 0), 2), 0) AS edu_alta

  FROM public.personas   AS p
  JOIN public.hogares    AS h  ON p.hogar_ref_id    = h.hogar_ref_id
  JOIN public.viviendas  AS v  ON h.vivienda_ref_id = v.vivienda_ref_id
  JOIN public.zonas      AS z  ON v.zonaloc_ref_id  = z.zonaloc_ref_id
  JOIN public.comunas    AS c  ON z.codigo_comuna   = c.codigo_comuna
  JOIN public.provincias AS pr ON pr.provincia_ref_id = c.provincia_ref_id
  WHERE pr.nom_provincia = 'SAN ANTONIO'
  GROUP BY z.geocodigo, c.nom_comuna
)
SELECT
  geocodigo,
  nom_comuna,
  ivmp,
  edu_alta
FROM agg
ORDER BY ivmp DESC;
"

df_indicadores <- dbGetQuery(con, sql_indicadores)

# =============================================================================
# 4) CARGAR GEOMETRÍA DE ZONAS CENSALES (PROVINCIA SAN ANTONIO)
# =============================================================================
sql_geometria <- "
SELECT
  geocodigo::double precision AS geocodigo,
  geom
FROM dpa.zonas_censales_v
WHERE nom_provin = 'SAN ANTONIO';
"
sf_zonas <- st_read(con, query = sql_geometria, quiet = TRUE)

# =============================================================================
# 5) COMBINAR DATOS TABULARES Y ESPACIALES
# =============================================================================
sf_mapa <- merge(x = sf_zonas, y = df_indicadores, by = 'geocodigo', all.x = FALSE)

# =============================================================================
# 6) MAPAS TEMÁTICOS UNIVARIADOS
# =============================================================================
map_ivmp <- ggplot(sf_mapa) +
  geom_sf(aes(fill = ivmp), color = 'gray85', size = 0.2) +
  scale_fill_distiller(palette = 'Reds', direction = 1) +
  labs(title = 'IVMP (%) — Materialidad precaria de la vivienda',
       fill  = 'IVMP (%)') +
  theme_minimal()

map_edu <- ggplot(sf_mapa) +
  geom_sf(aes(fill = edu_alta), color = 'gray85', size = 0.2) +
  scale_fill_distiller(palette = 'Blues', direction = 1) +
  labs(title = 'Educación ≥ media completa (%) — Población >18 años',
       fill  = '% Educación') +
  theme_minimal()

print(map_ivmp)
print(map_edu)

# =============================================================================
# 7) GRÁFICO DE DISPERSIÓN POR CUADRANTES (IVMP vs EDUCACIÓN)
# =============================================================================
mediana_ivmp <- median(sf_mapa$ivmp, na.rm = TRUE)
mediana_edu  <- median(sf_mapa$edu_alta, na.rm = TRUE)

sf_mapa$cuadrante <- with(sf_mapa, ifelse(
  ivmp >= mediana_ivmp & edu_alta >= mediana_edu, 'Q1: IVMP alto / Edu alta',
  ifelse(ivmp >= mediana_ivmp & edu_alta <  mediana_edu, 'Q2: IVMP alto / Edu baja',
         ifelse(ivmp <  mediana_ivmp & edu_alta <  mediana_edu, 'Q3: IVMP bajo / Edu baja',
                'Q4: IVMP bajo / Edu alta'))))

colores_cuadrantes <- c(
  'Q1: IVMP alto / Edu alta' = '#08519c',
  'Q2: IVMP alto / Edu baja' = '#6baed6',
  'Q3: IVMP bajo / Edu baja' = '#eff3ff',
  'Q4: IVMP bajo / Edu alta' = '#bdd7e7'
)

grafico_cuadrantes <- ggplot(sf_mapa, aes(x = ivmp, y = edu_alta, color = cuadrante)) +
  geom_point(size = 2) +
  geom_vline(xintercept = mediana_ivmp, linetype = 'dashed') +
  geom_hline(yintercept = mediana_edu,  linetype = 'dashed') +
  scale_color_manual(values = colores_cuadrantes) +
  labs(x = 'IVMP (%)',
       y = 'Educación ≥ media (%) (Población >18)',
       title = 'Dispersión: IVMP vs Educación (zonas censales, Prov. San Antonio)') +
  theme_minimal()

print(grafico_cuadrantes)

# =============================================================================
# 8) MAPA BIVARIADO CON BISCALE
# =============================================================================
sf_mapa_bi <- bi_class(
  sf_mapa,
  x = ivmp,
  y = edu_alta,
  dim = 3,
  style = 'jenks'
)

sql_comunas <- "
SELECT nom_comuna, geom
FROM dpa.comunas_v
WHERE nom_provin = 'SAN ANTONIO';
"
sf_comunas_sa <- st_read(con, query = sql_comunas, quiet = TRUE)

# CRS proyectado para evitar advertencias al generar centroides
crs_mapa <- st_crs(sf_mapa_bi)
sf_comunas_proj <- st_transform(sf_comunas_sa, 32719)
sf_centroides_proj <- st_point_on_surface(sf_comunas_proj)
sf_centroides <- st_transform(sf_centroides_proj, crs_mapa)

caja <- st_bbox(sf_mapa_bi)

mapa_bivariado_etiquetas <- ggplot() +
  geom_sf(data = sf_mapa_bi, aes(fill = bi_class), color = NA) +
  geom_sf(data = sf_comunas_sa, fill = NA, color = 'black', size = 0.3) +
  geom_sf_text(data = sf_centroides, aes(label = nom_comuna), size = 2.2, fontface = 'bold') +
  bi_scale_fill(pal = 'DkBlue', dim = 3) +
  labs(title = 'Mapa bivariado: IVMP vs Educación ≥ media (Población >18 años)',
       subtitle = 'Provincia de San Antonio') +
  coord_sf(xlim = c(caja['xmin'], caja['xmax']),
           ylim = c(caja['ymin'], caja['ymax']),
           expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

leyenda_bivariada <- bi_legend(
  pal = 'DkBlue', dim = 3,
  xlab = 'IVMP (bajo → alto)',
  ylab = 'Educación ≥ media (bajo → alto)',
  size = 8
)

mapa_final <- ggdraw() +
  draw_plot(mapa_bivariado_etiquetas, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(leyenda_bivariada, x = 0.7, y = 0.05, width = 0.25, height = 0.25)

print(mapa_final)
