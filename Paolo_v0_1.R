### Paolo script for Archaeos
### April, 11, 2024
### Prepared by BGibson
rm(list=ls())

### Setting directory for '~/Downloads' but can adjust to project folder
getwd()
setwd('~/Documents/GitHub/Archaeos')

### Getting packages set
require(readxl)
require(ggplot2)

### Read in data as tibble
d.Demo <- read_xlsx('Demosponge Measurements.xlsx',col_names = T)
str(d.Demo)

d.Archaeo <- read_xlsx('Archaeos Measurements.xlsx',col_names = T)
str(d.Archaeo)

### Basic stats using same method from ggplot2
summary(d)
d.lm <- lm(Large_Incurrent_Canal_Diameter ~ Ostia_Diameter, data = d)
summary(d.lm)

### Plotting in Base R
#note mu coding using unicode character 
plot(d,
     xlab = paste0('Ostia Diameter (',"\U003BC",'m)'), 
     ylab = paste0('Large Incurrent Canal Diameter (',"\U003BC",'m)'),
     pch = 19)
abline(d.lm)
### Using ggplot2; assigned plot to variable
p <- ggplot(data = d, aes(Ostia_Diameter,Large_Incurrent_Canal_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Ostia Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Large Incurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')

### Display plot
p




### Ostia vs Med Incurrent

### Basic stats using same method from ggplot2
summary(d)
d.lm <- lm(Medium_Incurrent_Canal_Diameter ~ Ostia_Diameter, data = d)
summary(d.lm)

### Plotting in Base R
#note mu coding using unicode character 
plot(d,
     xlab = paste0('Ostia Diameter (',"\U003BC",'m)'), 
     ylab = paste0('Medium Incurrent Canal Diameter (',"\U003BC",'m)'),
     pch = 19)
abline(d.lm)
### Using ggplot2; assigned plot to variable
p <- ggplot(data = d, aes(Ostia_Diameter,Medium_Incurrent_Canal_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Ostia Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Medium Incurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')

### Display plot
p




### Ostia vs Small Incurrent

### Basic stats using same method from ggplot2
summary(d)
d.lm <- lm(Small_Incurrent_Canal_Diameter ~ Ostia_Diameter, data = d)
summary(d.lm)

### Plotting in Base R
#note mu coding using unicode character 
plot(d,
     xlab = paste0('Ostia Diameter (',"\U003BC",'m)'), 
     ylab = paste0('Small Incurrent Canal Diameter (',"\U003BC",'m)'),
     pch = 19)
abline(d.lm)
### Using ggplot2; assigned plot to variable
p <- ggplot(data = d, aes(Ostia_Diameter,Small_Incurrent_Canal_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Ostia Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Small Incurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')

### Display plot
p





### Ostia vs Prosopyle

### Basic stats using same method from ggplot2
summary(d)
d.lm <- lm(Prosopyle_Diameter ~ Ostia_Diameter, data = d)
summary(d.lm)

### Plotting in Base R
#note mu coding using unicode character 
plot(d,
     xlab = paste0('Ostia Diameter (',"\U003BC",'m)'), 
     ylab = paste0('Prosopyle Diameter (',"\U003BC",'m)'),
     pch = 19)
abline(d.lm)
### Using ggplot2; assigned plot to variable
p <- ggplot(data = d, aes(Ostia_Diameter,Prosopyle_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Ostia Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Prosopyle Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')

### Display plot
p





### Large Ex vs Med Ex

### Basic stats using same method from ggplot2
summary(d)
d.lm <- lm(Medium_Excurrent_Canal_Diameter ~ Large_Excurrent_Canal_Diameter, data = d)
summary(d.lm)

### Plotting in Base R
#note mu coding using unicode character 
plot(d,
     xlab = paste0('Large Excurrent Canal Diameter (',"\U003BC",'m)'), 
     ylab = paste0('Medium Excurrent Canal Diameter (',"\U003BC",'m)'),
     pch = 19)
abline(d.lm)
### Using ggplot2; assigned plot to variable
p <- ggplot(data = d, aes(Large_Excurrent_Canal_Diameter,Medium_Excurrent_Canal_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Large Excurrent Canal Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Medium Excurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')

### Display plot
p




### Large Ex vs Small Ex

### Basic stats using same method from ggplot2
summary(d)
d.lm <- lm(Small_Excurrent_Canal_Diameter ~ Large_Excurrent_Canal_Diameter, data = d)
summary(d.lm)

### Plotting in Base R
#note mu coding using unicode character 
plot(d,
     xlab = paste0('Large Excurrent Canal Diameter (',"\U003BC",'m)'), 
     ylab = paste0('Small Excurrent Canal Diameter (',"\U003BC",'m)'),
     pch = 19)
abline(d.lm)
### Using ggplot2; assigned plot to variable
p <- ggplot(data = d, aes(Large_Excurrent_Canal_Diameter,Small_Excurrent_Canal_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Large Excurrent Canal Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Small Excurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')

### Display plot
p




### Large Ex vs Apopyle

### Basic stats using same method from ggplot2
summary(d)
d.lm <- lm(Apopyle_Diameter ~ Large_Excurrent_Canal_Diameter, data = d)
summary(d.lm)

### Plotting in Base R
#note mu coding using unicode character 
plot(d,
     xlab = paste0('Large Excurrent Canal Diameter (',"\U003BC",'m)'), 
     ylab = paste0('Apopyle Diameter (',"\U003BC",'m)'),
     pch = 19)
abline(d.lm)
### Using ggplot2; assigned plot to variable
p <- ggplot(data = d, aes(Large_Excurrent_Canal_Diameter,Apopyle_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Large Excurrent Canal Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Apopyle Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')

### Display plot
p




### Large Ex vs Apopyle

### Basic stats using same method from ggplot2
summary(d)
d.lm <- lm(Excurrent_Velocity ~ Osculum_Diameter, data = d)
summary(d.lm)

### Plotting in Base R
#note mu coding using unicode character 
plot(d,
     xlab = paste0('Osculum Diameter (',"\U003BC",'m)'), 
     ylab = paste0('Excurrent Velocity (', 'mm/s)'),
     pch = 19)
abline(d.lm)
### Using ggplot2; assigned plot to variable
p <- ggplot(data = d, aes(Osculum_Diameter,Excurrent_Velocity)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Osculum Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Excurrent Velocity (', 'mm/s)')) +
  geom_smooth(formula = y~x, method = 'lm')

### Display plot
p

