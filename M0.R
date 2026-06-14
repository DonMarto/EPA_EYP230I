library(car)
library(lmtest)

vehicles_clean <- read.csv("vehicles_clean.csv")

vehicles_clean$year_c <- vehicles_clean$year - mean(vehicles_clean$year)

modelo <- lm(
  comb08 ~ displ*year_c +
    gears +
    tCharger +
    sCharger +
    startStop +
    drive_FWD +
    drive_RWD +
    transType_Manual,
  data = vehicles_clean
)

summary(modelo)


vif(modelo)
