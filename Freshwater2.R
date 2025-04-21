
library(tidyverse)
library(nlme)

#  Load Dataset
df <- read_csv("dat.csv")

# Filter for Freshwater Marshes
df_fresh <- df %>%
  filter(CommunityStationFront == "Freshwater")

# Identify Species Columns
species_names <- names(df_fresh %>% select(Acer_negundo:Zizip_Mill.))

# Remove rows with no species cover
df_fresh <- df_fresh %>%
  filter(rowSums(select(., all_of(species_names)), na.rm = TRUE) > 0)

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
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  ) %>%
  filter(
    is.finite(Accretion),
    is.finite(Richness),
    is.finite(SummedCover),
    is.finite(Sagit_lancifolia),
    is.finite(Panic_hemitomon),
    is.finite(Alter_philoxeroid),
    is.finite(Vigna_luteola),
    is.finite(lat),
    is.finite(lon)
  ) %>%
  distinct(lat, lon, .keep_all = TRUE)

# Plot Relationships
ggplot(df_fresh, aes(x = Sagit_lancifolia, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Accretion vs. Sagit_lancifolia")

ggplot(df_fresh, aes(x = Panic_hemitomon, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Accretion vs. Panic_hemitomon")

ggplot(df_fresh, aes(x = Alter_philoxeroid, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Accretion vs. Alter_philoxeroid")

ggplot(df_fresh, aes(x = Vigna_luteola, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Accretion vs. Vigna_luteola")

# Fit GLS Model with Spatial Autocorrelation
options(contrasts = c("contr.helmert", "contr.poly"))

gls_model <- gls(
  Accretion ~ Richness + SummedCover + Sagit_lancifolia + 
    Panic_hemitomon + Alter_philoxeroid + Vigna_luteola,
  correlation = corExp(form = ~ lon + lat, nugget = TRUE),
  data = df_fresh,
  method = "REML"
)

# Type III-style ANOVA (F-tests)
cat("\n--- Type III-style ANOVA (F-tests from nlme) ---\n")
anova(gls_model, type = "marginal")

# Approximate R² ---
model_var <- var(gls_model$residuals)
null_model <- gls(Accretion ~ 1, data = df_fresh)
null_var <- var(null_model$residuals)
r_squared <- 1 - (model_var / null_var)
cat("\nApproximate R²:", round(r_squared, 3), "\n")

# Model Validation Plots
plot(fitted(gls_model), resid(gls_model, type = "normalized"),
     xlab = "Fitted values", ylab = "Normalized residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

hist(resid(gls_model, type = "normalized"),
     main = "Histogram of Normalized Residuals",
     xlab = "Normalized Residuals",
     col = "lightblue", border = "white")
