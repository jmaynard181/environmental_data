require(palmerpenguins)


#found flipper length for 3 species
#aggregate method 1
aggregate(penguins$flipper_length_mm, 
          list(penguins$species),
          FUN = mean, na.rm = TRUE)

#aggregate method 2 with formula notation
aggregate(flipper_length_mm ~ species,
          data = penguins,
          FUN = mean, na.rm = TRUE)

#boxplot with formula notation
boxplot(flipper_length_mm,
        data = penguins,
        FUN = mean, nam.rm = TRUE)

#what are the mean flipper length and sex
aggregate(flipper_length_mm ~ species + sex,
          data = penguins,
          FUN = mean, na.rm = TRUE)


boxplot(flipper_length_mm ~ species + sex,
          data = penguins,
          FUN = mean, na.rm = TRUE)

