# Libraries
library(dplyr)
library(car)
library(ggplot2)
library(lmtest)


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


# Test type of relation for displacement
mx    <- lm(comb08 ~ poly(displ, 1), data = df_filtered)
mx2   <- lm(comb08 ~ poly(displ, 2), data = df_filtered)
mx3   <- lm(comb08 ~ poly(displ, 3), data = df_filtered)
mlogx <- lm(comb08 ~ log(displ), data = df_filtered)
m1x   <- lm(comb08 ~ I(1/displ), data = df_filtered)

# R^2 for all cases. No need for adjust since n is the same
c(
  summary(mx)$r.squared, 
  summary(mx2)$r.squared, 
  summary(mx3)$r.squared, 
  summary(mlogx)$r.squared, 
  summary(m1x)$r.squared
)

# AIC for all cases
c(
  AIC(mx), 
  AIC(mx2),
  AIC(mx3),
  AIC(mlogx),
  AIC(m1x)
)

# BIC for all cases
c(
  BIC(mx),
  BIC(mx2),
  BIC(mx3),
  BIC(mlogx),
  BIC(m1x)
)

rmse <- function(model) {
  sqrt(mean(residuals(model)^2))
}

c(
  rmse(mx),
  rmse(mx2),
  rmse(mx3),
  rmse(mlogx),
  rmse(m1x)
)


## All metrics favour m1x, so that's the relation chosen for further analysis

# Test type of relation for displacement
pmx    <- lm(comb08 ~ displ_per_cyl, data = df_filtered)
pmx2   <- lm(comb08 ~ poly(displ_per_cyl, 2), data = df_filtered)
pmx3   <- lm(comb08 ~ poly(displ_per_cyl, 3), data = df_filtered)
pmlogx <- lm(comb08 ~ log(displ_per_cyl), data = df_filtered)
pm1x   <- lm(comb08 ~ I(1/displ_per_cyl), data = df_filtered)

# R^2 for all cases. No need for adjust since n is the same
c(
  summary(pmx)$r.squared, 
  summary(pmx2)$r.squared, 
  summary(pmx3)$r.squared, 
  summary(pmlogx)$r.squared, 
  summary(pm1x)$r.squared
)

# AIC for all cases
c(
  AIC(pmx), 
  AIC(pmx2),
  AIC(pmx3),
  AIC(pmlogx),
  AIC(pm1x)
)

# BIC for all cases
c(
  BIC(pmx),
  BIC(pmx2),
  BIC(pmx3),
  BIC(pmlogx),
  BIC(pm1x)
)

c(
  rmse(pmx),
  rmse(pmx2),
  rmse(pmx3),
  rmse(pmlogx),
  rmse(pm1x)
)


## Additionally, all models perform uniformly worse when using displ_per_cyl
## In this very specific case, X^3 outperforms 1/X, but not for displ.

df_filtered$displ_inv <- I(1/df_filtered$displ)


stepwise_f <- function(data, response, fixed_vars = character(),
                       candidate_vars,
                       alpha_enter = 0.05,
                       alpha_remove = 0.10) {
  
  current_vars <- fixed_vars
  
  repeat {
    
    changed <- FALSE
    
    ### FORWARD STEP ###
    excluded_vars <- setdiff(candidate_vars, current_vars)
    
    if (length(excluded_vars) > 0) {
      
      pvals <- sapply(excluded_vars, function(var) {
        
        reduced_formula <- as.formula(
          paste(response, "~",
                paste(current_vars, collapse = " + "))
        )
        
        full_formula <- as.formula(
          paste(response, "~",
                paste(c(current_vars, var), collapse = " + "))
        )
        
        reduced_model <- lm(reduced_formula, data = data)
        full_model <- lm(full_formula, data = data)
        
        anova(reduced_model, full_model)$F[2]
      })
      
      best_var <- names(which.max(pvals))
      
      if (max(pvals) < alpha_enter) {
        current_vars <- c(current_vars, best_var)
        changed <- TRUE
      }
    }
    
    ### BACKWARD STEP ###
    removable_vars <- setdiff(current_vars, fixed_vars)
    
    if (length(removable_vars) > 0) {
      
      pvals <- sapply(removable_vars, function(var) {
        
        reduced_vars <- setdiff(current_vars, var)
        
        reduced_formula <- as.formula(
          paste(response, "~",
                paste(reduced_vars, collapse = " + "))
        )
        
        full_formula <- as.formula(
          paste(response, "~",
                paste(current_vars, collapse = " + "))
        )
        
        reduced_model <- lm(reduced_formula, data = data)
        full_model <- lm(full_formula, data = data)
        
        anova(reduced_model, full_model)$F[2]
      })
      
      worst_var <- names(which.min(pvals))
      
      if (min(pvals) > alpha_remove) {
        current_vars <- setdiff(current_vars, worst_var)
        changed <- TRUE
      }
    }
    
    if (!changed)
      break
  }
  
  final_formula <- as.formula(
    paste(response, "~", paste(current_vars, collapse = " + "))
  )
  
  lm(final_formula, data = data)
}


stepwise_p <- function(data, response, fixed_vars = character(),
                       candidate_vars,
                       alpha_enter = 0.05,
                       alpha_remove = 0.10) {
  
  current_vars <- fixed_vars
  
  repeat {
    
    changed <- FALSE
    
    ### FORWARD STEP ###
    excluded_vars <- setdiff(candidate_vars, current_vars)
    
    if (length(excluded_vars) > 0) {
      
      pvals <- sapply(excluded_vars, function(var) {
        
        reduced_formula <- as.formula(
          paste(response, "~",
                paste(current_vars, collapse = " + "))
        )
        
        full_formula <- as.formula(
          paste(response, "~",
                paste(c(current_vars, var), collapse = " + "))
        )
        
        reduced_model <- lm(reduced_formula, data = data)
        full_model <- lm(full_formula, data = data)
        
        anova(reduced_model, full_model)$"Pr(>F)"[2]
      })
      
      best_var <- names(which.min(pvals))
      
      if (min(pvals) < alpha_enter) {
        current_vars <- c(current_vars, best_var)
        changed <- TRUE
      }
    }
    
    ### BACKWARD STEP ###
    removable_vars <- setdiff(current_vars, fixed_vars)
    
    if (length(removable_vars) > 0) {
      
      pvals <- sapply(removable_vars, function(var) {
        
        reduced_vars <- setdiff(current_vars, var)
        
        reduced_formula <- as.formula(
          paste(response, "~",
                paste(reduced_vars, collapse = " + "))
        )
        
        full_formula <- as.formula(
          paste(response, "~",
                paste(current_vars, collapse = " + "))
        )
        
        reduced_model <- lm(reduced_formula, data = data)
        full_model <- lm(full_formula, data = data)
        
        anova(reduced_model, full_model)$"Pr(>F)"[2]
      })
      
      worst_var <- names(which.max(pvals))
      
      if (max(pvals) > alpha_remove) {
        current_vars <- setdiff(current_vars, worst_var)
        changed <- TRUE
      }
    }
    
    if (!changed)
      break
  }
  
  final_formula <- as.formula(
    paste(response, "~", paste(current_vars, collapse = " + "))
  )
  
  lm(final_formula, data = data)
}


stepwise_R <- function(data, response, fixed_vars = character(),
                       candidate_vars,
                       alpha_enter = 0.05,
                       alpha_remove = 0.10) {
  
  current_vars <- fixed_vars
  
  repeat {
    
    changed <- FALSE
    
    ### FORWARD STEP ###
    excluded_vars <- setdiff(candidate_vars, current_vars)
    
    if (length(excluded_vars) > 0) {
      
      pvals <- sapply(excluded_vars, function(var) {
        
        reduced_formula <- as.formula(
          paste(response, "~",
                paste(current_vars, collapse = " + "))
        )
        
        full_formula <- as.formula(
          paste(response, "~",
                paste(c(current_vars, var), collapse = " + "))
        )
        
        reduced_model <- lm(reduced_formula, data = data)
        full_model <- lm(full_formula, data = data)
        summary(full_model)$adj.r.squared - summary(reduced_model)$adj.r.squared
      })
      
      best_var <- names(which.max(pvals))
      if (max(pvals) > alpha_enter) {
        current_vars <- c(current_vars, best_var)
        changed <- TRUE
      }
    }
    
    ### BACKWARD STEP ###
    removable_vars <- setdiff(current_vars, fixed_vars)
    
    if (length(removable_vars) > 0) {
      
      pvals <- sapply(removable_vars, function(var) {
        
        reduced_vars <- setdiff(current_vars, var)
        
        reduced_formula <- as.formula(
          paste(response, "~",
                paste(reduced_vars, collapse = " + "))
        )
        
        full_formula <- as.formula(
          paste(response, "~",
                paste(current_vars, collapse = " + "))
        )
        
        reduced_model <- lm(reduced_formula, data = data)
        full_model <- lm(full_formula, data = data)
        
        summary(full_model)$adj.r.squared - summary(reduced_model)$adj.r.squared
      })
      
      worst_var <- names(which.min(pvals))
      if (length(pvals) > 1) {
        if (min(pvals) < alpha_remove) {
          current_vars <- setdiff(current_vars, worst_var)
          changed <- TRUE
        }
      }
    }
    if (!changed)
      break
  }
  
  final_formula <- as.formula(
    paste(response, "~", paste(current_vars, collapse = " + "))
  )
  
  lm(final_formula, data = data)
}

model <- stepwise_R(
  data = df_filtered,
  response = "comb08",
  fixed_vars = c("displ*year_c"),
  candidate_vars = c("displ*year_c", "tCharger", "sCharger",
                     "startStop", "gears", "drive_FWD", "drive_RWD",
                     "transType_Manual"),
  alpha_enter = 0.004,
  alpha_remove = 0.001
)

summary(modd)

modd <- lm(comb08 ~ displ + displ:year_c + 
            gears + drive_FWD, data = df_filtered)

############## Model start
m0 <- lm(comb08 ~ displ_inv * year_c, data = df_filtered)
m1 <- lm(comb08 ~ displ_inv * year_c + gears, data = df_filtered)
m2 <- lm(comb08 ~ displ_inv * year_c + cylinders, data = df_filtered)
m3 <- lm(comb08 ~ displ_inv * year_c + tCharger, data = df_filtered)
m4 <- lm(comb08 ~ displ_inv * year_c + sCharger, data = df_filtered)
m5 <- lm(comb08 ~ displ_inv * year_c + atvType, data = df_filtered)
m6 <- lm(comb08 ~ displ_inv * year_c + startStop, data = df_filtered)
m7 <- lm(comb08 ~ displ_inv * year_c + drive_FWD, data = df_filtered)
m8 <- lm(comb08 ~ displ_inv * year_c + drive_RWD, data = df_filtered)
m9 <- lm(comb08 ~ displ_inv * year_c + transType_Manual, data = df_filtered)
summary(m0)$r.squared
summary(m1)$r.squared
anova(m0, m2)$F
anova(m0, m3)$F
anova(m0, m4)$F
anova(m0, m5)$F
anova(m0, m6)$F
anova(m0, m7)$F
anova(m0, m8)$F
anova(m0, m9)$F

m0 <- lm(comb08 ~ displ_inv * year_c + drive_FWD, data = df_filtered)
m1 <- lm(comb08 ~ displ_inv * year_c + drive_FWD + gears, data = df_filtered)
m2 <- lm(comb08 ~ displ_inv * year_c + drive_FWD + cylinders, data = df_filtered)
m3 <- lm(comb08 ~ displ_inv * year_c + drive_FWD + tCharger, data = df_filtered)
m4 <- lm(comb08 ~ displ_inv * year_c + drive_FWD + sCharger, data = df_filtered)
m5 <- lm(comb08 ~ displ_inv * year_c + drive_FWD + atvType, data = df_filtered)
m6 <- lm(comb08 ~ displ_inv * year_c + drive_FWD + startStop, data = df_filtered)
# m7 <- lm(comb08 ~ displ_inv * year_c + drive_FWD, data = df_filtered)
m8 <- lm(comb08 ~ displ_inv * year_c + drive_FWD + drive_RWD, data = df_filtered)
m9 <- lm(comb08 ~ displ_inv * year_c + drive_FWD + transType_Manual, data = df_filtered)
anova(m0, m1)$F
anova(m0, m2)$F
anova(m0, m3)$F
anova(m0, m4)$F
anova(m0, m5)$F
anova(m0, m6)$F
# anova(m0, m7)$F
anova(m0, m8)$F
anova(m0, m9)$F




summary(mx)
summary(mx2)
summary(mx3)
summary(mlogx)
summary(m1x)
abline(mx)

ggplot(df_filtered, aes(x = comb08, y = displ)) +
  geom_point(size = 3) +                                      # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") + # Adjusted line
  labs(title = "ggplot2 Regression Line", x = "X Axis", y = "Y Axis") +
  theme_minimal()

ggplot(df_filtered, aes(x = comb08, y = I(1/displ))) +
  geom_point(size = 3) +                                      # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") + # Adjusted line
  labs(title = "ggplot2 Regression Line", x = "X Axis", y = "Y Axis") +
  theme_minimal()


model <- lm(comb08 ~ displ, data = df_filtered)

# Sort by displ so the line is smooth
df_plot <- df_filtered[order(df_filtered$displ), ]

# Predicted values
df_plot$pred <- predict(model, newdata = df_plot)

ggplot(df_plot, aes(x = displ, y = comb08)) +
  geom_point(size = 3) +
  geom_line(aes(y = pred), color = "darkgreen", linewidth = 1.2) +
  labs(
    title = "Regression with 1/displ transformation",
    x = "Displacement",
    y = "Combined MPG"
  ) +
  theme_minimal()
