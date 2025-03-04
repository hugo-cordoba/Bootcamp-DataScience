library(ggplot2)
library(dplyr)
library(readxl)

options(scipen = 999)

# 1. CARGAR DATOS

bbdd_problema <- read_excel("BBDD_Problema-Lab-01.xlsx")

# 2. GENERAR MODELO PROBLEMA

names(bbdd_problema)

model_problema <- lm(Ventas ~ 1 +
                       TV +
                       Radio +
                       Social
                       , bbdd_problema)

# 3. RESULTADOS

summary(model_problema)

# Calculamos el ajuste del modelo

ajustes <- bbdd_problema %>%
  select(c(Semanas, Ventas)) %>%
  bind_cols(data.frame(ajuste=model_problema$fitted.values))

# Calculamos los aportes del modelo

aportes <- bbdd_problema %>% select(Semanas) %>% as.data.frame()
aux <- data.frame(model.matrix(model_problema) %*% diag(model_problema$coefficients))
names(aux) <- names(model_problema$coefficients)
aportes <- aportes %>% bind_cols(aux)

# Calculamos los aportes por campana

aportes_campana1 <- sum(aportes$TV)
aportes_campana2 <- sum(aportes$Radio)
aportes_campana3 <- sum(aportes$Social)

# Calculamos la inversion por campana

inv_campana1 <- sum(bbdd_problema$TV)
inv_campana2 <- sum(bbdd_problema$Radio)
inv_campana3 <- sum(bbdd_problema$Social)

# ROI

ROI_campana1 <- aportes_campana1/inv_campana1*1000
ROI_campana2 <- aportes_campana2/inv_campana2*1000
ROI_campana3 <- aportes_campana3/inv_campana3*1000

