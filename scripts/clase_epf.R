# --- CARGA DE LIBRERÍAS ---
library(haven)
library(pROC)
library(mgcv)
library(ggplot2)


# --- CARGA DE DATOS EPF ---
personas   <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos     <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif       <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# --- FILTRADO: Gran Santiago y datos válidos ---
valores_invalidos <- c(-99, -88, -77)

personas_gs <- subset(
  personas,
  macrozona == 2 &
    !(edad %in% valores_invalidos) &
    !(edue %in% valores_invalidos) &
    ing_disp_hog_hd_ai >= 0
)

# --- VARIABLES DERIVADAS ---
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas
personas_gs$id_persona <- paste(personas_gs$folio, personas_gs$n_linea, sep = "_")
cantidades$id_persona  <- paste(cantidades$folio, cantidades$n_linea, sep = "_")

# --- FILTRO GASTO EN VINO (Gran Santiago) ---
cantidades_vino <- subset(cantidades, (ccif == "02.1.2.01.01") & macrozona == 2)

# --- SUMA GASTO TOTAL EN VINO POR PERSONA ---
gasto_vino_por_persona <- aggregate(gasto ~ id_persona, data = cantidades_vino, sum)
names(gasto_vino_por_persona)[2] <- "gasto_vino"

# --- MERGE: Gasto con personas ---
personas_gs <- merge(personas_gs, gasto_vino_por_persona, by = "id_persona", all.x = TRUE)
personas_gs$gasto_vino[is.na(personas_gs$gasto_vino)] <- 0

# --- VARIABLE BINARIA DE GASTO ---
personas_gs$incurre_gasto <- ifelse(personas_gs$gasto_vino > 0, 1, 0)

# --- AGRUPACIÓN ESCOLARIDAD ---
personas_gs$grupo_escolaridad <- cut(
  personas_gs$edue,
  breaks = c(-Inf, 12, 14, 16, Inf),
  labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
  right = TRUE
)

# --- BASE PARA MODELO CONTINUO (solo quienes gastan) ---
tabla_gasto <- subset(personas_gs, gasto_vino > 0)
tabla_gasto <- tabla_gasto[, c("sexo", "edad", "edue", "ing_pc", "gasto_vino", "grupo_escolaridad")]

# --- TRANSFORMACIONES DE VARIABLES ---
tabla_gasto$sexo <- factor(tabla_gasto$sexo, labels = c("Hombre", "Mujer"))
tabla_gasto$log_ing_pc <- log(tabla_gasto$ing_pc)
tabla_gasto$log_gasto_vino <- log(tabla_gasto$gasto_vino + 1)
tabla_gasto$rango_edad <- cut(tabla_gasto$edad,
                              breaks = c(0, 29, 44, 64, Inf),
                              labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
)



# --- FILTRO DE OUTLIERS (percentil 1 y 99) ---
q_ing <- quantile(tabla_gasto$ing_pc, probs = c(0.01, 0.99))
q_gasto <- quantile(tabla_gasto$gasto_vino, probs = c(0.01, 0.99))

tabla_gasto <- subset(tabla_gasto,
                      ing_pc >= q_ing[1] & ing_pc <= q_ing[2] &
                        gasto_vino >= q_gasto[1] & gasto_vino <= q_gasto[2]
)

# --- GRAFICOS EXPLORATORIOS ---
# DISTRIBUCIÓN DEL INGRESO
hist(tabla_gasto$ing_pc, breaks = 30, col = "lightblue",
     main = "Distribución del Ingreso", xlab = "Ingreso per cápita")

# DISTRIBUCIÓN DEL GASTO EN VINO
hist(tabla_gasto$gasto_vino, breaks = 30, col = "lightblue",
     main = "Distribución del Gasto en Vino", xlab = "Gasto en vino")

# GASTO EN VINO SEGÚN SEXO
boxplot(gasto_vino ~ factor(sexo), data = tabla_gasto,
        main = "Gasto en Vino según Sexo", xlab = "Sexo",
        col = c("tomato", "lightgreen"))

# GASTO EN FUNCIÓN DE LA EDAD
plot(tabla_gasto$edad, tabla_gasto$gasto_vino,
     main = "Edad vs Gasto en Vino", xlab = "Edad", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$edad, tabla_gasto$gasto_vino), col = "red", lwd = 2)

# GASTO EN FUNCIÓN DEL INGRESO
plot(tabla_gasto$ing_pc, tabla_gasto$gasto_vino,
     main = "Ingreso vs Gasto en Vino", xlab = "Ingreso per cápita", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$ing_pc, tabla_gasto$gasto_vino), col = "blue", lwd = 2)

# BOXPLOT GASTO SEGÚN ESCOLARIDAD
boxplot(gasto_vino ~ grupo_escolaridad, data = tabla_gasto,
        main = "Gasto en Vino según Escolaridad", xlab = "Escolaridad",
        col = "skyblue")

