

require(palmerpenguins)

image_file = "ugly_histogram.png"

png(
  here("images", image_file),
  width = 1200, height = 1000
)

hist(penguins$flipper_length_mm)

dev.off()


save_png_1 = function(image_file)
{
  require(here)
  image_file = "ugly_histogram.png"

png(
  here("images", image_file),
  width = 1200, height = 1000
)

hist(penguins$flipper_length_mm)
}



save_png_1("ugly_histo_2.png")

hist(penguins$body_massg)

dev.off()



dat_vec = penguins$body_mass_g
my_title = "Mike's Histogram!"
x_label = "Mike's Data!"


hist(dat_vec,
     col = "steelblue",
     main = my_title,
     xlab = x_label)


steelblue_hist_fun = function(dat_vec, my_title, x_label)
{
  hist(dat_vec,
       col = "steelblue",
       main = my_title,
       xlab = x_label)
}

steelblue_hist_fun(
  dat_vec = sample(100x = 1:100, size = 1000, replace = TRUE),
  my_title = "Mike's random numbers", 
  x_label = "x-values"
)

sample(100x = 1:100, size = 1000, replace = TRUE)


