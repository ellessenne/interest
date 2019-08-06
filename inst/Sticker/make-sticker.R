### R Script to make hex sticker
library(hexSticker)
# Using dev version of hexSticker from:
# devtools::install_github("GuangchuangYu/hexSticker")
library(sysfonts)

sysfonts::font_add(family = "Iosevka Slab", regular = "iosevka-slab-regular.ttc")
p_family <- "Iosevka Slab"

# Convert .eps to .png
# Requires imagemagick
dpi <- 600
system(paste0("convert -colorspace rgb -density ", dpi, " -units PixelsPerInch inst/Sticker/analytics.eps inst/Sticker/analytics.png"))

# Plot here
hexSticker::sticker(
  subplot = "inst/Sticker/analytics.png",
  s_x = 1,
  s_y = 1,
  s_width = 2 / 3,
  s_height = 2 / 3,
  package = "",
  p_color = "#000000",
  p_family = p_family,
  p_x = 1,
  p_y = 2,
  p_size = 6,
  h_size = 5,
  h_fill = "#FFFFFF",
  h_color = "#99D0A7",
  url = "INTEREST",
  u_x = 0.935,
  u_y = 0.16,
  u_color = "#F1705E",
  u_family = p_family,
  u_size = 6,
  filename = "inst/Sticker/interest.png",
  dpi = dpi,
  white_around_sticker = TRUE,
  asp = 0.863
)

# Plot in man/figures folder
hexSticker::sticker(
  subplot = "inst/Sticker/analytics.png",
  s_x = 1,
  s_y = 1,
  s_width = 2 / 3,
  s_height = 2 / 3,
  package = "",
  p_color = "#000000",
  p_family = p_family,
  p_x = 1,
  p_y = 2,
  p_size = 6,
  h_size = 5,
  h_fill = "#FFFFFF",
  h_color = "#99D0A7",
  url = "INTEREST",
  u_x = 0.935,
  u_y = 0.16,
  u_color = "#F1705E",
  u_family = p_family,
  u_size = 6,
  filename = "man/figures/hex.png",
  dpi = dpi,
  white_around_sticker = TRUE,
  asp = 0.863
)
