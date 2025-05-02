library(tidyverse)
library(piecewiseSEM)
library(nlme)
library(patchwork)
library(MASS)

df <- read_csv("dat.csv")

df_fresh <- df %>%
  filter(CommunityStationFront == "Freshwater")

# Identify Species Columns
species_names <- names(df_fresh %>% dplyr::select(Acer_negundo:Zizip_Mill.))

# Remove rows with no species cover
df_fresh <- df_fresh %>%
  filter(rowSums(dplyr::select(., all_of(species_names)), na.rm = TRUE) > 0)

# Print top 4 species from the dataset 
species_totals <- colSums(df_fresh[, species_names], na.rm = TRUE)
top4_found <- names(sort(species_totals, decreasing = TRUE))[1:4]
cat("Top 4 dominant species in freshwater marsh (based on data):\n", paste(top4_found, collapse = ", "), "\n")

df_fresh <- df_fresh %>%
  mutate(
    Accretion = as.numeric(Accretion),
    Richness = as.numeric(Richness),
    SummedCover = as.numeric(SummedCover),
    Sagit_lancifolia = as.numeric(Sagit_lancifolia),
    Panic_hemitomon = as.numeric(Panic_hemitomon),
    Alter_philoxeroid = as.numeric(Alter_philoxeroid),
    Vigna_luteola = as.numeric(Vigna_luteola),
    BGB_Live = as.numeric(BelowgroundLive),
    BGB_Dead = as.numeric(BelowgroundDead),
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    Region = factor(if_else(lon > -92, "East", "West"))
  ) %>%
  filter(
    if_all(c("Accretion", "Richness", "SummedCover", "Sagit_lancifolia",
             "BGB_Live", "BGB_Dead", "lat", "lon"), is.finite)
  )

# Plot Relationships

Richness <- ggplot(data = df_fresh, aes(x = Richness, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) 
  

Summedcover <- ggplot(data = df_fresh, aes(x = SummedCover, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) 
  

(Richness | Summedcover)


dominant1 <- ggplot(df_fresh, aes(x = Sagit_lancifolia, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Accretion vs. dominant_species")

dominant2 <-ggplot(df_fresh, aes(x = Panic_hemitomon, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

dominant3 <-ggplot(df_fresh, aes(x = Alter_philoxeroid, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

dominant4 <-ggplot(df_fresh, aes(x = Vigna_luteola, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

(dominant1 | dominant2) / (dominant3 | dominant4)

#  Plots of accretion vs. belowground biomass
below_live <- ggplot(df_fresh, aes(x = BGB_Live, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)


below_dead <-ggplot(df_fresh, aes(x = BGB_Dead, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) 

(below_live | below_dead)

# gls model
  options(contrasts = c("contr.helmert", "contr.poly"))

m1 <- gls(
  Accretion ~ Richness + SummedCover + Sagit_lancifolia + 
    Panic_hemitomon + Alter_philoxeroid + Vigna_luteola,
  correlation = corSpher(form = ~ lat+lon),
  data = df_fresh,
  method = "REML"
)

# Type III-style ANOVA (F-tests)
cat("\n--- Type III-style ANOVA (F-tests from nlme) ---\n")
anova(m1, type = "marginal")

# Approximate R² ---
model_var <- var(m1$residuals)
null_model <- gls(Accretion ~ 1, data = df_fresh)
null_var <- var(null_model$residuals)
r_squared <- 1 - (model_var / null_var)
cat("\nApproximate R²:", round(r_squared, 3), "\n")

# Model Validation Plots
plot(fitted(m1), resid(m1, type = "normalized"),
     xlab = "Fitted values", ylab = "Normalized residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

hist(resid(m1, type = "normalized"),
     main = "Histogram of Normalized Residuals",
     xlab = "Normalized Residuals",
     col = "lightblue", border = "white")


# Adding biomass live, dead and region to the model

# Full GLS model
options(constrasts = c("contr.helmert", "contr.poly"))
m2 <- gls(
  Accretion ~ Richness + SummedCover + Sagit_lancifolia +
    BGB_Live + BGB_Dead + Region,
  correlation = corSpher(form = ~ lat+lon),
  data = df_fresh,
  method = "ML"
)

# Backward selection using stepAIC 
m3 <- stepAIC(m2, direction = "backward", trace = TRUE)

# Refit final model using REML
m4 <- update(m3, method = "REML")

# Type III-style ANOVA
anova_table <- anova(m4, type = "marginal")
print(anova_table)

# Approximate R² 
model_var <- var(resid(m4))
null_model <- gls(Accretion ~ 1, data = df_fresh)
null_var <- var(resid(null_model))
r_squared <- 1 - (model_var / null_var)
cat("Approximate R²:", round(r_squared, 3), "\n")

# Model Validation Plots
plot(fitted(m4), resid(m4, type = "normalized"),
     xlab = "Fitted values", ylab = "Normalized residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

hist(resid(m4, type = "normalized"),
     main = "Histogram of Normalized Residuals",
     xlab = "Normalized Residuals",
     col = "lightblue", border = "white")



# Full 4-species model
options(constrasts = c("contr.helmert", "contr.poly"))
m5 <- gls(
  Accretion ~ Richness + SummedCover + Sagit_lancifolia + 
    Panic_hemitomon + Alter_philoxeroid + Vigna_luteola +
    BGB_Live + BGB_Dead + Region,
  correlation = corSpher(form = ~ lat+lon),
  data = df_fresh,
  method = "ML"
)

# Model selection
m6 <- stepAIC(m5, direction = "backward")

# Refit with REML
m7 <- gls(Accretion ~ SummedCover + Sagit_lancifolia + Alter_philoxeroid + 
      BGB_Dead + Region, 
    correlation = corSpher(form = ~ lat+lon), 
    method = "REML", data = df_fresh )

# ANOVA
anova_4sp <- anova(m7, type = "marginal")
print(anova_4sp)

# R²
model_var_4sp <- var(resid(m7))
null_var <- var(resid(null_model))  # Use your existing null_model
r2_4sp <- 1 - (model_var_4sp / null_var)
cat("Approximate R² (4-species):", round(r2_4sp, 3), "\n")

# Model Validation Plots
plot(fitted(m7), resid(m7, type = "normalized"),
     xlab = "Fitted values", ylab = "Normalized residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

hist(resid(m7, type = "normalized"),
     main = "Histogram of Normalized Residuals",
     xlab = "Normalized Residuals",
     col = "lightblue", border = "white")

