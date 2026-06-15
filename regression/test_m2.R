# Libraries
library(dplyr)
library(car)
library(ggplot2)
library(lmtest)
library(modelsummary)
library(effectsize)


# Data loading
df <- read.csv(file.choose())

## Generate displ_per_cyl. Note that cyl>0
df$displ_per_cyl <- df$displ / df$cylinders

# Filter out CVT
df_filtered <- df %>% filter(transType_CVT == 0)
df_filtered$transType_CVT <- NULL

# Center year
df_filtered$year_c <- df_filtered$year - mean(df_filtered$year)
df_filtered$year <- NULL


## Run first model
model_M2 <- lm(comb08 ~ displ_per_cyl * year_c + startStop + atvType + 
                 gears + tCharger + sCharger + drive_RWD + drive_FWD +
                 transType_Manual, data = df_filtered)
## Alternatives
model_M2_0 <- lm(comb08 ~ displ_per_cyl * year_c, 
                 data = df_filtered)

model_M2_A <- lm(comb08 ~ I(1/displ_per_cyl) * year_c + startStop + atvType + 
                   gears + tCharger + sCharger + drive_RWD + drive_FWD +
                   transType_Manual, data = df_filtered)

model_M2_A2 <- lm(comb08 ~ displ * year_c + cylinders + startStop + atvType + 
                    gears + tCharger + sCharger + drive_RWD + drive_FWD +
                    transType_Manual, data = df_filtered)

model_M2_A3 <- lm(comb08 ~ I(1/displ) * year_c + cylinders + startStop + atvType + 
                    gears + tCharger + sCharger + drive_RWD + drive_FWD +
                    transType_Manual, data = df_filtered)

model_M2_A4 <- lm(comb08 ~ poly(displ, 3) * year_c + cylinders + startStop + atvType + 
                    gears + tCharger + sCharger + drive_RWD + drive_FWD +
                    transType_Manual, data = df_filtered)

model_M2_A5 <- lm(comb08 ~ displ_per_cyl * year_c + startStop + atvType + 
                    tCharger + sCharger + drive_RWD + drive_FWD +
                    transType_Manual, data = df_filtered)

model_M2_A6 <- lm(comb08 ~ displ * year_c + I(1/cylinders) + startStop + atvType + 
                    tCharger + sCharger + drive_RWD + drive_FWD +
                    transType_Manual, data = df_filtered)

model_M2_A7 <- lm(comb08 ~ displ * year_c + I(1/cylinders) + startStop + atvType + 
                    gears + tCharger + sCharger + drive_RWD + drive_FWD +
                    transType_Manual, data = df_filtered)


## Summary of model
summary(model_M2)
summary(model_M2_A)$Estimate
summary(model_M2_A2)
summary(model_M2_A3)
summary(model_M2_A4)
summary(model_M2_A5)
summary(model_M2_A6)

anova(model_M2, model_M2_A5)

modelsummary(
  list("M0" = model_M2, "M1" = model_M2_A3, "M2" = model_M2_A2),
  output = "latex",
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "AIC", "BIC", "nobs")
)


standardize_parameters(model_M2_A2)
method="basic"

## VIF
vif(model_M2)

## AIC BIC
AIC(model_M2)
BIC(model_M2)

## plot

ggplot(df_filtered, aes(x = displ_per_cyl, y = comb08)) +
  geom_point(alpha = 0.2) +
  geom_smooth(aes(color = year_c), method = "lm", se = FALSE)

## plot 2
mean_year <- 0  # centered

slope_avg <- coef(model_M2)["displ_per_cyl"]

slope_future <- slope_avg +
  coef(model_M2)["displ_per_cyl:year_c"] * 10


## qqtest

par(mfrow = c(2,2))
plot(model_M2)


## heteroskr
bptest(model_M2)

plot(df_filtered$comb08, I(1/df_filtered$displ))
plot(df_filtered$comb08, I(df_filtered$displ))
##
plot(df$displ, df$comb08)
x <- seq(0,1,length.out = 1001)
plot(x,1/x)


## 
models <- list(model_M2, model_M2_0, model_M2_A)
sapply(models, AIC)
sapply(models, BIC)

summary(model_M2_0)

model_m0_0 <- lm(comb08 ~ displ * year_c, data = df_filtered)

summary(model_m0_0)


model_m1 <- lm(comb08 ~ poly(displ, 2) + year_c + displ:year_c, data = df_filtered)

summary(model_m1)

model_mlog <- lm(comb08 ~ log(displ) * year_c, data = df_filtered)

summary(model_mlog)

model_minv <- lm(comb08 ~ I(1/displ) * year_c, data = df_filtered)

summary(model_minv)

## ocupar el power del test (power(modelo))
# UV de krammer
# power.t.test()
# reportar los intervalos de confianza


## exclusión de peso
# induce endogeneidad, explicar como limitaciones 

power.t.test(length(df_filtered), 0.5)



# 1. Partir con introducir modelos según f(displ) para diferentes f
# 2. Crear modelos secuenciales de agregar variables
# 3. Analizar el Ngears (maybe trany * gears)
# 4. Maybe ingresar CVT si es que eliminar gears
# 5. Histogramas por categoría para las variables
# 6. Displacement * year o displacement / cyl * year

model(x~displ*year + I(1/cyl))


plot(df_filtered$displ, df_filtered$comb08)
