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


# --- FILTRO GASTO EN CAFÉ (Gran Santiago) ---
cantidades_cafe <- subset(cantidades, (ccif == "01.2.2.01.01") & macrozona == 2)


# --- SUMA GASTO TOTAL EN CAFÉ POR PERSONA ---
gasto_cafe_por_persona <- aggregate(gasto ~ id_persona, data = cantidades_cafe, sum)
names(gasto_cafe_por_persona)[2] <- "gasto_cafe"


# --- MERGE: Gasto café con personas ---
personas_gs <- merge(personas_gs, gasto_cafe_por_persona, by = "id_persona", all.x = TRUE)
personas_gs$gasto_cafe[is.na(personas_gs$gasto_cafe)] <- 0


# --- VARIABLE BINARIA DE GASTO ---
personas_gs$incurre_gasto_cafe <- ifelse(personas_gs$gasto_cafe > 0, 1, 0)


# --- AGRUPACIÓN ESCOLARIDAD ---
personas_gs$grupo_escolaridad <- cut(
  personas_gs$edue,
  breaks = c(-Inf, 12, 14, 16, Inf),
  labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
  right = TRUE
)


# --- BASE PARA MODELO CONTINUO (solo quienes gastan) ---
tabla_gasto <- subset(personas_gs, gasto_cafe > 0)
tabla_gasto <- tabla_gasto[, c("sexo", "edad", "edue", "ing_pc", "gasto_cafe", "grupo_escolaridad")]


# --- TRANSFORMACIONES DE VARIABLES ---
tabla_gasto$sexo <- factor(tabla_gasto$sexo, labels = c("Hombre", "Mujer"))
tabla_gasto$log_ing_pc <- log(tabla_gasto$ing_pc)
tabla_gasto$log_gasto_cafe <- log(tabla_gasto$gasto_cafe + 1)
tabla_gasto$rango_edad <- cut(tabla_gasto$edad,
                              breaks = c(0, 29, 44, 64, Inf),
                              labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
)


# --- FILTRO DE OUTLIERS (percentil 1 y 99) ---
q_ing <- quantile(tabla_gasto$ing_pc, probs = c(0.01, 0.99))
q_gasto <- quantile(tabla_gasto$gasto_cafe, probs = c(0.01, 0.99))

tabla_gasto <- subset(tabla_gasto,
                      ing_pc >= q_ing[1] & ing_pc <= q_ing[2] &
                        gasto_cafe >= q_gasto[1] & gasto_cafe <= q_gasto[2]
)


# --- GRAFICOS EXPLORATORIOS ---
# DISTRIBUCIÓN DEL INGRESO
hist(tabla_gasto$ing_pc, breaks = 30, col = "lightblue",
     main = "Distribución del Ingreso", xlab = "Ingreso per cápita")

# DISTRIBUCIÓN DEL GASTO EN CAFÉ
hist(tabla_gasto$gasto_cafe, breaks = 30, col = "lightblue",
     main = "Distribución del Gasto en Café", xlab = "Gasto en café")

# GASTO EN CAFÉ SEGÚN SEXO
boxplot(gasto_cafe ~ factor(sexo), data = tabla_gasto,
        main = "Gasto en Café según Sexo", xlab = "Sexo",
        col = c("tomato", "lightgreen"))

# GASTO EN FUNCIÓN DE LA EDAD
plot(tabla_gasto$edad, tabla_gasto$gasto_cafe,
     main = "Edad vs Gasto en Café", xlab = "Edad", ylab = "Gasto en café",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$edad, tabla_gasto$gasto_cafe), col = "red", lwd = 2)

# GASTO EN FUNCIÓN DEL INGRESO
plot(tabla_gasto$ing_pc, tabla_gasto$gasto_cafe,
     main = "Ingreso vs Gasto en Café", xlab = "Ingreso per cápita", ylab = "Gasto en café",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$ing_pc, tabla_gasto$gasto_cafe), col = "blue", lwd = 2)

# BOXPLOT GASTO SEGÚN ESCOLARIDAD
boxplot(gasto_cafe ~ grupo_escolaridad, data = tabla_gasto,
        main = "Gasto en Café según Escolaridad", xlab = "Escolaridad",
        col = "skyblue")

