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
require(ggtext)
require(ggpubr)


### Read in data as tibble
d.All <- read_xlsx('All Species Calculations.xlsx',col_names = T)
str(d.All)
d.Archaeo <- read_xlsx('Archaeos_Calculations_Edit.xlsx',col_names = T)
str(d.Archaeo)


######################################################################################
#### Sponge Pump Plots
### Log Osculum Area vs Log Surface Area
OsculumArea.Archaeo <- (d.Archaeo[["Osculum_Area"]] / 100000000)
SurfaceArea.Archaeo <- (d.Archaeo[["Surface_Area"]] / 100000000)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Archaeo, aes(SurfaceArea.Archaeo,OsculumArea.Archaeo)) + 
  geom_point(data = d.Archaeo, show.legend = FALSE, aes(color = Species, shape = Species, fill = Species)) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4)) +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(expression(paste('log Surface Area (c', m^{2},')'))) +
  ylab(expression(paste('log Osculum Area (c', m^{2},')'))) +
  geom_smooth(formula = y~x, method = 'lm', alpha = .1, color='#0000004D') +
  theme(legend.text=element_text(face = "italic"))
# Log10 scale both X and Y axes
p <- p + scale_x_continuous(trans='log10', labels = scales::comma) +
  scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p

############
### Demosponges vs Archaeos
### Log Osculum Area vs Log Surface Area
OsculumArea.Archaeo <- (d.All[["Osculum_Area"]] / 100000000)
SurfaceArea.Archaeo <- (d.All[["Surface_Area"]] / 100000000)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.All, aes(SurfaceArea.Archaeo,OsculumArea.Archaeo)) + 
  geom_point(data = d.All, show.legend = T, aes(color = Class, shape = Class, fill = Class)) +
  #geom_point(mapping = aes(colour=species)) + #can color coordinate when you add species column
  xlab(expression(paste('log Surface Area (c', m^{2},')'))) +
  ylab(expression(paste('log Osculum Area (c', m^{2},')'))) +
  scale_colour_manual(values = c("#f8766d", "#01bec4", "#01ba39")) +
  scale_fill_manual(values = c("#f8766d", "#01bec4", "#01ba39")) +
  geom_smooth(method = 'lm', show.legend = F, se = T, aes(color = Class, fill = Class), alpha = .1)
# Log10 scale both X and Y axes
p <- p + scale_x_continuous(trans='log10', labels = scales::comma) +
  scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p



###########################################
### log Volumetric Flow vs log Surface Area
VolumetricOscularFlowRate.Archaeo <- (d.Archaeo[["Volumetric_Oscular_Flow_Rate"]] / 1000000000000)
SurfaceArea.Archaeo <- (d.Archaeo[["Surface_Area"]] / 100000000)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Archaeo, aes(SurfaceArea.Archaeo,VolumetricOscularFlowRate.Archaeo)) + 
  geom_point(data = d.Archaeo, show.legend = FALSE, aes(color = Species, shape = Species, fill = Species)) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4)) +
  xlab(expression(paste('log Surface Area (c',m^{2},')'))) +
  ylab(expression(paste('log Volumetric Oscular Flow Rate (c', m^{3}, ' ',s^{-1},')'))) +
  geom_smooth(formula = y~x, method = 'lm', alpha = .1, color='#0000004D') +
  theme(legend.text=element_text(face = "italic"))  
# Log10 scale both X and Y axes
p <- p + scale_x_continuous(trans='log10', labels = scales::comma) +
  scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p

############
### Demosponges vs Archaeos
### log Volumetric Flow vs log Surface Area
VolumetricOscularFlowRate.Archaeo <- (d.All[["Volumetric_Oscular_Flow_Rate"]] / 1000000000000)
SurfaceArea.Archaeo <- (d.All[["Surface_Area"]] / 100000000)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.All, aes(SurfaceArea.Archaeo,VolumetricOscularFlowRate.Archaeo)) + 
  geom_point(data = d.All, show.legend = F, aes(color = Class, shape = Class, fill = Class)) +
  xlab(expression(paste('log Surface Area (c',m^{2},')'))) +
  ylab(expression(paste('log Volumetric Oscular Flow Rate (c', m^{3}, ' ',s^{-1},')'))) +
  scale_colour_manual(values = c("#f8766d", "#01bec4", "#01ba39")) +
  scale_fill_manual(values = c("#f8766d", "#01bec4", "#01ba39")) +
  geom_smooth(method = 'lm', show.legend = F, se = T, aes(color = Class, fill = Class), alpha = .1) +
  theme(legend.text=element_text(face = "italic"))  
# Log10 scale both X and Y axes
p <- p + scale_x_continuous(trans='log10', labels = scales::comma) +
  scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p



###########################################
### log Excurrent Speed vs log Osculum Area
# Unit conversion from um to cm
OsculumArea.Archaeo <- (d.Archaeo[["Osculum_Area"]] / 100000000)
ExcurrentVelocity.Archaeo <- (d.Archaeo[["Excurrent_Velocity"]] / 10000)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Archaeo, aes(OsculumArea.Archaeo,ExcurrentVelocity.Archaeo)) + 
  geom_point(data = d.Archaeo, show.legend = F, aes(color = Species, shape = Species, fill = Species)) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4)) +
  xlab(expression(paste('log Osculum Area (c',m^{2},')'))) +
  ylab(expression(paste('log Excurrent Speed (c','m ',s^{-1},')'))) +
  geom_smooth(formula = y~x, method = 'lm', show.legend = F, se = T, alpha = .1, color='#0000004D') +
  theme(legend.text=element_text(face = "italic")) 
# Log10 scale both X and Y axes
p <- p + scale_x_continuous(trans='log10', labels = scales::comma) +
  scale_y_continuous(trans='log10', labels = scales::comma)
  #scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p

############
### Demosponges vs Archaeos
### log Excurrent Speed vs log Osculum Area
# Unit conversion from um to cm
OsculumArea.Archaeo <- (d.All[["Osculum_Area"]] / 100000000)
ExcurrentVelocity.Archaeo <- (d.All[["Excurrent_Velocity"]] / 10000)
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.All, aes(OsculumArea.Archaeo,ExcurrentVelocity.Archaeo)) + 
  geom_point(data = d.All, show.legend = F, aes(color = Class, shape = Class, fill = Class)) +
  xlab(expression(paste('log Osculum Area (c',m^{2},')'))) +
  ylab(expression(paste('log Excurrent Speed (c','m ',s^{-1},')'))) +
  scale_colour_manual(values = c("#f8766d", "#01bec4", "#01ba39")) +
  scale_fill_manual(values = c("#f8766d", "#01bec4", "#01ba39")) +
  geom_smooth(method = 'lm', show.legend = F, se = T, aes(color = Class, fill = Class), alpha = .1) +
  theme(legend.text=element_text(face = "italic")) 
# Log10 scale both X and Y axes
p <- p + scale_x_continuous(trans='log10', labels = scales::comma) +
  scale_y_continuous(trans='log10', labels = scales::comma)
#scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p



###########################################
### log Volumetric Flow vs log OSA/SA
# Unit conversion from um to cm
VolumetricOscularFlowRate.Archaeo <- (d.Archaeo[["Volumetric_Oscular_Flow_Rate"]] / 1000000000000)
OsaSaRatio.Archaeo <- (d.Archaeo[["OSA_SA_Ratio"]])
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Archaeo, aes(OsaSaRatio.Archaeo,VolumetricOscularFlowRate.Archaeo)) + 
  geom_point(data = d.Archaeo, show.legend = F, aes(color = Species, shape = Species, fill = Species)) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4)) +
  xlab(expression(paste('log OSA/SA'))) +
  ylab(expression(paste('log Volumetric Oscular Flow Rate (c',m^{3}, ' ', s^{-1},')'))) +
  geom_smooth(formula = y~x, method = 'lm', show.legend = F, se = T, alpha = .1, color='#0000004D') +
  theme(legend.text=element_text(face = "italic"))
#  geom_smooth(method = 'lm',   se = FALSE, aes(color = Species), alpha = .15)
# Log10 scale both X and Y axes
p <- p + scale_x_continuous(trans='log10', labels = scales::comma) +
  scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p

############
### Demosponges vs Archaeos
### log Volumetric Flow vs log OSA/SA
# Unit conversion from um to cm
VolumetricOscularFlowRate.Archaeo <- (d.All[["Volumetric_Oscular_Flow_Rate"]] / 1000000000000)
OsaSaRatio.Archaeo <- (d.All[["OSA_SA_Ratio"]])
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.All, aes(OsaSaRatio.Archaeo,VolumetricOscularFlowRate.Archaeo)) + 
  geom_point(data = d.All, show.legend = F, aes(color = Class, shape = Class, fill = Class)) +
  xlab(expression(paste('log OSA/SA'))) +
  ylab(expression(paste('log Volumetric Oscular Flow Rate (c',m^{3}, ' ', s^{-1},')'))) +
  scale_colour_manual(values = c("#f8766d", "#01bec4", "#01ba39")) +
  scale_fill_manual(values = c("#f8766d", "#01bec4", "#01ba39")) +
  geom_smooth(method = 'lm', show.legend = F, se = T, aes(color = Class, fill = Class), alpha = .1) +
  theme(legend.text=element_text(face = "italic"))
#  geom_smooth(method = 'lm',   se = FALSE, aes(color = Species), alpha = .15)
# Log10 scale both X and Y axes
p <- p + scale_x_continuous(trans='log10', labels = scales::comma) +
  scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p






######################################################################################
### log Velocity vs Region
# Unit conversion from um to mm
Velocity.Archaeo <- (d.Archaeo[["Velocity"]] / 1000)
Region.Archaeo <- (d.Archaeo[["Region"]])
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Archaeo, aes(Region.Archaeo, Velocity.Archaeo, group = Class_Name)) + 
  geom_smooth( method = 'loess', alpha = 0.1, color='#0000004D') +
#  geom_line(data = d.Archaeo, aes(color = Species_Name))+
#  geom_boxplot(outlier.shape = NA) +
#  geom_line(data = d.Archaeo, aes(color = Species_Name), alpha = 0.50)+
  geom_point(data = d.Archaeo, alpha = 0.5, show.legend = FALSE, aes(color = Species_Name, shape = Species_Name)) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4)) +
  xlab(expression(paste('Region of Aquiferous Canal System'))) +
  ylab(expression(paste('log Velocity (m','m ',s^{-1},')'))) +
  scale_x_discrete(labels = c("Ostia", "Large Incurrent Canal", "Medium Incurrent Canal", "Small Incurrent Canal", "Prosopyle", "Apopyle", "Small Excurrent Canal", "Medium Excurrent Canal", "Large Excurrent Canal")) +
  theme(legend.text=element_text(face = "italic"), 
        axis.title.x = element_text(face = "bold", vjust = -5),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_markdown(angle = 60, vjust = 0.5),
        plot.margin = margin(0.25,0.25,1.5,0.25,"cm"))
#  geom_smooth(method = 'lm',   se = FALSE, aes(color = Species), alpha = .15)
# Log10 scale Y axis
p <- p  + scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p

############
### Demosponges vs Archaeos
### log Velocity vs Region
# Unit conversion from um to mm
Velocity.Archaeo <- (d.All[["Velocity"]] / 1000)
Region.Archaeo <- (d.All[["Region"]])
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.All, aes(Region.Archaeo, Velocity.Archaeo, group = Class_Name)) + 
  geom_smooth( method = 'loess', show.legend = FALSE, alpha = 0.1, aes(color = Class_Name, fill = Class_Name)) +
#  geom_line(data = d.All, aes(color = Class_Name))+
  geom_point(data = d.All, show.legend = FALSE, alpha = 0.5, aes(color = Class_Name, shape = Class_Name, fill = Class_Name)) +
  xlab(expression(paste('Region of Aquiferous Canal System'))) +
  ylab(expression(paste('log Velocity (m','m ',s^{-1},')'))) +
  scale_x_discrete(labels = c("Ostia", "Large Incurrent Canal", "Medium Incurrent Canal", "Small Incurrent Canal", "Prosopyle", "Apopyle", "Small Excurrent Canal", "Medium Excurrent Canal", "Large Excurrent Canal")) +
  theme(legend.text=element_text(face = "italic"), 
        axis.title.x = element_text(vjust = -5),
        axis.text.x = element_markdown(angle = 60, vjust = 0.5),
        plot.margin = margin(0.25,0.25,1.5,0.25,"cm"))
#  geom_smooth(method = 'lm',   se = FALSE, aes(color = Species), alpha = .15)
# Log10 scale Y axis
p <- p  + scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p



###########################################
### log Cross-Sectional Area vs Region
# Unit conversion from um to mm
AreaPer.Archaeo <- (d.Archaeo[["Area_Per_1mm3"]] / 1000000)
Region.Archaeo <- (d.Archaeo[["Region"]])
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.Archaeo, aes(Region.Archaeo, AreaPer.Archaeo, group = Class_Name)) + 
  geom_smooth( method = 'loess', alpha = 0.1, color='#0000004D') +
#  geom_line(data = d.Archaeo, aes(color = Species_Name))+
  geom_point(data = d.Archaeo, alpha = 0.5, show.legend = FALSE, aes(color = Species_Name, shape = Species_Name, fill = Species_Name)) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4)) +
  xlab(expression(paste('Region of Aquiferous Canal System'))) +
  ylab(expression(paste('log Cross-Sectional Area per 1m',m^{3}, ' (m',m^{2},')'))) +
  scale_x_discrete(labels = c("Ostia", "Large Incurrent Canal", "Medium Incurrent Canal", "Small Incurrent Canal", "Prosopyle", "Apopyle", "Small Excurrent Canal", "Medium Excurrent Canal", "Large Excurrent Canal")) +
  theme(legend.text=element_text(face = "italic"), 
        axis.title.x = element_text(vjust = -5),
        axis.text.x = element_markdown(angle = 60, vjust = 0.5),
        plot.margin = margin(0.25,0.25,1.5,0.25,"cm"))
#  geom_smooth(method = 'lm',   se = FALSE, aes(color = Species), alpha = .15)
# Log10 scale Y axis
p <- p  + scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p

############
### Demosponges vs Archaeos
### log Cross-Sectional Area vs Region
# Unit conversion from um to mm
AreaPer.Archaeo <- (d.All[["Area_Per_1mm3"]] / 1000000)
Region.Archaeo <- (d.All[["Region"]])
# Using ggplot2; assigned plot to variable
p <- ggplot(data = d.All, aes(Region.Archaeo, AreaPer.Archaeo, group = Class_Name)) + 
  geom_smooth(method = 'loess', show.legend = FALSE, alpha = 0.1, aes(color = Class_Name, fill = Class_Name)) +
  #  geom_line(data = d.Archaeo, aes(color = Species_Name))+
  geom_point(data = d.All, show.legend = FALSE, alpha = 0.5, aes(color = Class_Name, shape = Class_Name, fill = Class_Name)) +
  xlab(expression(paste('Region of Aquiferous Canal System'))) +
  ylab(expression(paste('log Cross-Sectional Area per 1m',m^{3}, ' (m',m^{2},')'))) +
  scale_x_discrete(labels = c("Ostia", "Large Incurrent Canal", "Medium Incurrent Canal", "Small Incurrent Canal", "Prosopyle", "Apopyle", "Small Excurrent Canal", "Medium Excurrent Canal", "Large Excurrent Canal")) +
  theme(legend.text=element_text(face = "italic"), 
        axis.title.x = element_text(vjust = -5),
        axis.text.x = element_markdown(angle = 60, vjust = 0.5),
        plot.margin = margin(0.25,0.25,1.5,0.25,"cm"))
#  geom_smooth(method = 'lm',   se = FALSE, aes(color = Species), alpha = .15)
# Log10 scale Y axis
p <- p  + scale_y_continuous(trans='log10', labels = scales::comma)
# Display plot
p

