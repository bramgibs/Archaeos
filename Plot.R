### Script for Plotting
### July, 3, 2024
### Prepared by P. Attanasio
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
d.Archaeo <- read_xlsx('Archaeos_Calculations.xlsx',col_names = T)
str(d.Archaeo)

###########################################
### Large Incurrent Diameter vs Ostia Diameter
# Basic stats using same method from ggplot2
summary(d.Demo)
d.lm <- lm(Large_Incurrent_Canal_Diameter ~ Ostia_Diameter, data = d.Demo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Demo, aes(Ostia_Diameter,Large_Incurrent_Canal_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Ostia Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Large Incurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
### Med Incurrent Diameter vs Ostia Diameter
# Basic stats using same method from ggplot2
summary(d.Demo)
d.lm <- lm(Medium_Incurrent_Canal_Diameter ~ Ostia_Diameter, data = d.Demo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Demo, aes(Ostia_Diameter,Medium_Incurrent_Canal_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Ostia Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Medium Incurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
### Small Incurrent Diameter vs Ostia Diameter
# Basic stats using same method from ggplot2
summary(d.Demo)
d.lm <- lm(Small_Incurrent_Canal_Diameter ~ Ostia_Diameter, data = d.Demo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Demo, aes(Ostia_Diameter,Small_Incurrent_Canal_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Ostia Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Small Incurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
### Prosopyle Diameter vs Ostia Diameter
# Basic stats using same method from ggplot2
summary(d.Demo)
d.lm <- lm(Prosopyle_Diameter ~ Ostia_Diameter, data = d.Demo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Demo, aes(Ostia_Diameter,Prosopyle_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Ostia Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Prosopyle Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
### Med Ex Diameter vs Large Ex Diameter
# Basic stats using same method from ggplot2
summary(d.Demo)
d.lm <- lm(Medium_Excurrent_Canal_Diameter ~ Large_Excurrent_Canal_Diameter, data = d.Demo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Demo, aes(Large_Excurrent_Canal_Diameter,Medium_Excurrent_Canal_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Large Excurrent Canal Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Medium Excurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
### Small Ex Diameter vs Large Ex Diameter
# Basic stats using same method from ggplot2
summary(d.Demo)
d.lm <- lm(Small_Excurrent_Canal_Diameter ~ Large_Excurrent_Canal_Diameter, data = d.Demo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Demo, aes(Large_Excurrent_Canal_Diameter,Small_Excurrent_Canal_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Large Excurrent Canal Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Small Excurrent Canal Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
### Apopyle Diameter vs Large Ex Diameter
# Basic stats using same method from ggplot2
summary(d.Demo)
d.lm <- lm(Apopyle_Diameter ~ Large_Excurrent_Canal_Diameter, data = d.Demo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Demo, aes(Large_Excurrent_Canal_Diameter,Apopyle_Diameter)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Large Excurrent Canal Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Apopyle Diameter (',"\U003BC",'m)')) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
### Ex Velocity vs Osculum Diameter
# Basic stats using same method from ggplot2
summary(d.Demo)
d.lm <- lm(Excurrent_Velocity ~ Osculum_Diameter, data = d.Demo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Demo, aes(Osculum_Diameter,Excurrent_Velocity)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(paste0('Osculum Diameter (',"\U003BC",'m)')) +
  ylab(paste0('Excurrent Velocity (', 'mm/s)')) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
#### Sponge Pump Plots
### Osculum Area vs Surface Area
# Basic stats using same method from ggplot2
summary(d.Archaeo)
d.lm <- lm(Osculum_Area ~ Surface_Area, data = d.Archaeo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Archaeo, aes(Surface_Area,Osculum_Area)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(expression(paste('Surface Area (',"\U003BC",m^{2},')'))) +
  ylab(expression(paste('Osculum Area (',"\U003BC",m^{2},')'))) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
### Volumetric Flow vs Surface Area
# Basic stats using same method from ggplot2
summary(d.Archaeo)
d.lm <- lm(Volumetric_Oscular_Flow_Rate ~ Surface_Area, data = d.Archaeo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Archaeo, aes(Surface_Area,Volumetric_Oscular_Flow_Rate)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(expression(paste('Surface Area (',"\U003BC",m^{2},')'))) +
  ylab(expression(paste('Volumetric Oscular Flow Rate (',"\U003BC",m^{3},s^{-1},')'))) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
### Excurrent Speed vs Osculum Area
# Basic stats using same method from ggplot2
summary(d.Archaeo)
d.lm <- lm(Excurrent_Velocity ~ Osculum_Area, data = d.Archaeo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Archaeo, aes(Osculum_Area,Excurrent_Velocity)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(expression(paste('Osculum Area (',"\U003BC",m^{2},')'))) +
  ylab(expression(paste('Excurrent Speed (',"\U003BC",m^{3},s^{-1},')'))) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

###########################################
### Volumetric Flow vs OSA/SA
# Basic stats using same method from ggplot2
summary(d.Archaeo)
d.lm <- lm(Volumetric_Oscular_Flow_Rate ~ OSA_SA_Ratio, data = d.Archaeo)
summary(d.lm)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Archaeo, aes(OSA_SA_Ratio,Volumetric_Oscular_Flow_Rate)) + 
  geom_point() +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(expression(paste('OSA/SA (',"\U003BC",m^{2},')'))) +
  ylab(expression(paste('Volumetric Oscular Flow Rate (',"\U003BC",m^{3},s^{-1},')'))) +
  geom_smooth(formula = y~x, method = 'lm')
# Display plot
p

