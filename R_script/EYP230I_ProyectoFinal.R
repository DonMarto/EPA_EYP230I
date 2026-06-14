# Definimos el working directory a la carpeta donde se encuentra el archivo
# IMOPRTANTE: el archivo de datos debe estar en la misma carpeta que este script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# leemos los datos
df <- read.csv("vehicles_clean.csv")

# Filtramos las filas que tienen CVT, para evitar problemas con valores nulos en
# gears
df$displ_per_cyl <- df$displ / df$cylinders
df_filtered <- df[df$transType_CVT == 0, ]
df_filtered$transType_CVT <- NULL

# Centramos los años para evitar problemas de colinealidad estructural
df_filtered$year_c <- df_filtered$year - mean(df_filtered$year)

# Modelo base,
# comb08 = b0 + b1*displ + b2*year + b3*(displ:year) +
# b4*gears + b5*tCharger + b6*sCharger + b7*startStop + b8*FWD + b9*RWD +
# b10*Manual + e
M0 <- lm(comb08 ~ displ*year_c +
           gears + tCharger + sCharger + startStop +
           drive_FWD + drive_RWD + transType_Manual,
         data = df_filtered
         )

# Modelo con cilindrada como categorías (displ_grupo: 0-2L,2-3L,3-4L,4-5L,5-6L,6-7L,7L+),
# comb08 = b0 + b1*displ + b2*year + b3*(displ:year) +
# b4*displ_2-3L + b5*displ_3-4L + b6*displ_4-5L + b7*displ_5-6L + b8*displ_6-7L + b9*displ_7L+ +
# b10*gears + b11*tCharger + b12*sCharger + b13*startStop + b14*FWD + b15*RWD + b16*Manual + e
M1 <- lm(comb08 ~ displ*year_c +
           displ_grupo + gears + tCharger + sCharger + startStop +
           drive_FWD + drive_RWD + transType_Manual,
         data = df_filtered
         )

# Modelo con volumen por cilindro, sin cilindrada por categoría,
# comb08 = b0 + b1*displ_per_cyl + b2*year + b3*(displ_per_cyl:year) +
# b4*startStop + b5*gears + b6*tCharger + b7*sCharger + b8*RWD + b9*FWD + b10*Manual + e
M2 <- lm(comb08 ~ displ_per_cyl*year_c +
           startStop + gears + tCharger + sCharger + drive_RWD + drive_FWD +
           transType_Manual,
         data = df_filtered
         )


#
summary(M0)
summary(M1)
summary(M2)

AIC(M0, M1, M2)
BIC(M0, M1, M2)
c(R2adj_M0 = summary(M0)$adj.r.squared,
  R2adj_M1 = summary(M1)$adj.r.squared,
  R2adj_M2 = summary(M2)$adj.r.squared)

anova(M0, M1)   # F parcial, M0 vs M1 (anidados)
















