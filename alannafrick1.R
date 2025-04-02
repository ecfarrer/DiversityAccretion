install.packages("scales")
install.packages("ggthemes")

library(scales)
library(ggthemes)
pal <- c("#cdb4db", "#ffc8dd", "#ffafcc", "#bde0fe", "#a2d2ff")
show_col(pal)
palette_fn <- colorRampPalette(pal)
palette_fn(100)
image(
  x = matrix(1:100, ncol = 1), 
  col = palette_fn(100),
  useRaster = TRUE,
  axes = FALSE
) 
canva_palettes[[101]]
show_col(canva_palettes[[101]])