#cargar librerias
library(haven)
library(pROC)
library(mgcv)
library(ggplot2)

#carga de datos EPF
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")
