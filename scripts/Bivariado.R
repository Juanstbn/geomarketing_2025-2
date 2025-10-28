##################################
## librerias para llamar librerias
##################################

library(DBI)
library(RPostgres)
library(sf) #instalar
library(ggplot2) #instalar
##############################################
## conectar a base de datos. configuración bd
##############################################

db_host = "localhost"
db_port = 5432
db_name = "censo_v_2017"
db_user = "postgres"
db_password= "postgres"

##establecer conexión usando rpostgres

con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

## consulta SQL##

consulta_sql= "

SELECT z.geocodigo, c.nom_comuna,
      ROUND (COUNT (*) FILTER (WHERE p.p15 >= 12 AND p.p15 <=14)*100.0 /  
      COUNT (*) FILTER (WHERE p.p09 > 18),2)AS tasa_prof -- al divirlo solo por la poblacion mayor de edad se hace mas representativo 
FROM personas AS p
JOIN hogares h ON h.hogar_ref_id= p.hogar_ref_id
JOIN viviendas v ON h.vivienda_ref_id= v.vivienda_ref_id
JOIN zonas z ON z.zonaloc_ref_id= v.zonaloc_ref_id
JOIN comunas c ON z.codigo_comuna=c.codigo_comuna
GROUP BY c.nom_comuna,z.geocodigo
ORDER BY tasa_prof DESC;
"

## ejecutar la cnsulta

df_profesionales = dbGetQuery(con, consulta_sql)


##############
##30-09-2025##
#############

## cargar zonas censales y comunas

zonas_profesionales = st_read(con, query = "SELECT * FROM output.tasas_profesionales_geom;") 
comunas = st_read(con, query = "SELECT * FROM dpa.comunas_v")


##

ggplot(zonas_profesionales, aes(x = tasa_prof)) + 
  geom_histogram(
    bins = 20, 
    fill = 'steelblue',
    alpha = 0.8
  ) +
  labs(
    title = "distribución de tasa de profesionales",
    x = "% profesionales", 
    y = "frecuencia"
  )