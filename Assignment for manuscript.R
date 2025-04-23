# --- Load Required Packages ---
library(tidyverse)
library(nlme)
library(car)
library(patchwork)

# --- Step 1: Load Dataset ---
df <- read_csv("dat.csv")

# --- Step 2: Filter for Freshwater Marshes ---
df_fresh <- df %>%
  filter(CommunityStationFront == "Freshwater")

# --- Step 3: Identify Species Columns (Acer_negundo to Zizip_Mill.) ---
species_names <- names(df_fresh %>% select(Acer_negundo:Zizip_Mill.))

# --- Step 4: Remove rows with no species cover ---
df_fresh <- df_fresh %>%
  filter(rowSums(select(., all_of(species_names)), na.rm = TRUE) > 0)

# --- Step 5: Calculate Dominant Species (overall) ---
species_totals <- colSums(df_fresh[, species_names], na.rm = TRUE)
dominant_species <- names(species_totals)[which.max(species_totals)]
cat("Dominant species in freshwater marsh:", dominant_species, "\n")

# --- Step 6: Add Dominant Species Cover Column ---
df_fresh <- df_fresh %>%
  mutate(DominantCover = .[[dominant_species]])

# --- Step 7: Clean key variables and remove invalid entries ---
df_fresh <- df_fresh %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    Accretion = as.numeric(Accretion),
    Richness = as.numeric(Richness),
    SummedCover = as.numeric(SummedCover),
    DominantCover = as.numeric(DominantCover)
  ) %>%
  filter(
    is.finite(lat), is.finite(lon),
    is.finite(Accretion),
    is.finite(Richness),
    is.finite(SummedCover),
    is.finite(DominantCover)
  ) %>%
  distinct(lat, lon, .keep_all = TRUE)  # Remove duplicate coordinates if needed

# --- Step 8: Exploratory Plots ---
p1 <- ggplot(df_fresh, aes(x = Richness, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Accretion vs. Richness")

p2 <- ggplot(df_fresh, aes(x = SummedCover, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Accretion vs. Summed Cover")

p3 <- ggplot(df_fresh, aes(x = DominantCover, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = paste("Accretion vs.", dominant_species))

(p1 | p2) / p3

# --- Step 9: Fit GLS Model with Spatial Correlation ---
options(contrasts = c("contr.helmert", "contr.poly"))

gls_model <- gls(Accretion ~ Richness + SummedCover + DominantCover,
                 correlation = corExp(form = ~ lon + lat, nugget = TRUE),
                 data = df_fresh,
                 method = "REML")

# --- Step 10: Type III ANOVA ---
Anova(gls_model, type = "III")

# --- Step 11 (Optional): R-squared Approximation ---
model_var <- var(gls_model$residuals)
null_model <- gls(Accretion ~ 1, data = df_fresh)
null_var <- var(null_model$residuals)

r_squared <- 1 - (model_var / null_var)
cat("Approximate R²:", round(r_squared, 3), "\n")
