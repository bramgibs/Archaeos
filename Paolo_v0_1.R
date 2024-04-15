### Paolo script for Archaeos
### April, 11, 2024
### Prepared by BGibson
rm(list=ls())

### Setting directory for '~/Downloads' but can adjust to project folder
getwd()
setwd('Downloads')

### Getting packages set
require(readxl)
require(ggplot2)

### Read in data as tibble
d <- read_xlsx('Archaeo.xlsx',col_names = T)
str(d)

### Basic stats using same method from ggplot2
summary(d)
d.lm <- lm(Large_incurrent_canal_diameter ~ Ostia_diameter, data = d)
summary(d.lm)

### Plotting in Base R
#note mu coding using unicode character 
plot(d,
     xlab = paste0('Ostia diameter (',"\U003BC",'m)'), 
     ylab = paste0('Incurrent Canal Diameter (',"\U003BC",'m)'),
     pch = 19)
abline(d.lm)
### Using ggplot2; assigned plot to variable
p <- ggplot(data = d, aes(Ostia_diameter,Large_incurrent_canal_diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Ostia diameter (',"\U003BC",'m)')) +
  ylab(paste0('Incurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')

### Display plot
p
