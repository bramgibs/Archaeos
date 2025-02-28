### Script for Correlation Calculation
### Prepared by P. Attanasio
rm(list=ls())

################################################################
### Setting directory
getwd()
setwd('~/Documents/GitHub/Archaeos')

### Getting packages set
pkgs <- c('readxl','writexl','ggplot2')
invisible(lapply(pkgs,library,character.only=T))
  
### Read in data as tibble
d.Demo <- read_xlsx('Demosponge Measurements.xlsx',col_names = T)
d.Archaeo <- read_xlsx('Archaeos Measurements.xlsx',col_names = T)

################################################################


### Large Incurrent Canal Area Per 1mm^3 linear regression


LInAreaPer.lm <- lm(Large_Incurrent_Canal_Area_Per_1mm3 ~ Ostia_Area_Per_1mm3, data = d.Demo)

# Archaeo Large Incurrent Canal Area Per 1mm^3 calculation
OstiaAreaPer.Archaeo <- d.Archaeo[["Ostia_Area_Per_1mm3"]]

LargeIncurrentAreaPer.Archaeo <- OstiaAreaPer.Archaeo*LInAreaPer.lm$coefficients[2]+LInAreaPer.lm$coefficients[1]
LargeIncurrentAreaPer.Archaeo



### Medium Incurrent Canal Area Per 1mm^3 linear regression
MInAreaPer.lm <- lm(Medium_Incurrent_Canal_Area_Per_1mm3 ~ Large_Incurrent_Canal_Area_Per_1mm3, data = d.Demo)

# Archaeo Medium Incurrent Canal Area calculation
MedIncurrentAreaPer.Archaeo <- LargeIncurrentAreaPer.Archaeo*MInAreaPer.lm$coefficients[2]+MInAreaPer.lm$coefficients[1]
MedIncurrentAreaPer.Archaeo



### Small Incurrent Canal Area linear regression
SInAreaPer.lm <- lm(Small_Incurrent_Canal_Area_Per_1mm3 ~ Medium_Incurrent_Canal_Area_Per_1mm3, data = d.Demo)

# Archaeo Small Incurrent Canal Area calculation
SmallIncurrentAreaPer.Archaeo <- MedIncurrentAreaPer.Archaeo*SInAreaPer.lm$coefficients[2]+SInAreaPer.lm$coefficients[1]
SmallIncurrentAreaPer.Archaeo



### Prosopyle Area linear regression
ProsopyleAreaPer.lm <- lm(Prosopyle_Area_Per_1mm3 ~ Small_Incurrent_Canal_Area_Per_1mm3, data = d.Demo)

# Archaeo Prosopyle Area calculation
ProsopyleAreaPer.Archaeo <- SmallIncurrentAreaPer.Archaeo*ProsopyleAreaPer.lm$coefficients[2]+ProsopyleAreaPer.lm$coefficients[1]
ProsopyleAreaPer.Archaeo






### Medium Excurrent Canal Area linear regression
MExAreaPer.lm <- lm(Medium_Excurrent_Canal_Area_Per_1mm3 ~ Large_Excurrent_Canal_Area_Per_1mm3, data = d.Demo)

# Archaeo Medium Excurrent Canal Area calculation
LargeExcurrentAreaPer.Archaeo <- d.Archaeo[["Large_Excurrent_Canal_Area_Per_1mm3"]]

MedExcurrentAreaPer.Archaeo <- LargeExcurrentAreaPer.Archaeo*MExAreaPer.lm$coefficients[2]+MExAreaPer.lm$coefficients[1]
MedExcurrentAreaPer.Archaeo



### Small Excurrent Canal Area linear regression
SExAreaPer.lm <- lm(Small_Excurrent_Canal_Area_Per_1mm3 ~ Medium_Excurrent_Canal_Area_Per_1mm3, data = d.Demo)

# Archaeo Small Excurrent Canal Area calculation
SmallExcurrentAreaPer.Archaeo <- MedExcurrentAreaPer.Archaeo*SExAreaPer.lm$coefficients[2]+SExAreaPer.lm$coefficients[1]
SmallExcurrentAreaPer.Archaeo



### Apopyle Area linear regression
ApopyleAreaPer.lm <- lm(Apopyle_Area_Per_1mm3 ~ Small_Excurrent_Canal_Area_Per_1mm3, data = d.Demo)

# Archaeo Apopyle Area calculation
ApopyleAreaPer.Archaeo <- SmallExcurrentAreaPer.Archaeo*ApopyleAreaPer.lm$coefficients[2]+ApopyleAreaPer.lm$coefficients[1]
ApopyleAreaPer.Archaeo






### Excurrent Velocity linear regression
ExVelocity.lm <- lm(Excurrent_Velocity ~ Osculum_Diameter, data = d.Demo)

### Archaeo velocity calculation
OsculumDiameter.Archaeo <- d.Archaeo[["Osculum_Diameter"]]
mean(OsculumDiameter.Archaeo)

ExcurrentVelocity.Archaeo <- OsculumDiameter.Archaeo*ExVelocity.lm$coefficients[2]+ExVelocity.lm$coefficients[1]
ExcurrentVelocity.Archaeo



### Archaeo Volumetric Flow calculation
OsculumArea.Archaeo <- d.Archaeo[["Osculum_Area"]]

VolumetricOscularFlowRate.Archaeo <- OsculumArea.Archaeo * ExcurrentVelocity.Archaeo


### Archaeo total wall thickness calculation
OuterWallThickness.Archaeo <- d.Archaeo[["Outer_Wall_Thickness"]]

InnerWallThickness.Archaeo <- d.Archaeo[["Inner_Wall_Thickness"]]

IntervallumWidth.Archaeo <- d.Archaeo[["Intervallum_Width"]]

TotalThickness.Archaeo <- OuterWallThickness.Archaeo + InnerWallThickness.Archaeo + IntervallumWidth.Archaeo



################################################################
### Archaeo Velocity per opening Calculation for Ostia
SurfaceArea.Archaeo <- d.Archaeo[["Surface_Area"]]

OstiaVelocity.Archaeo <- ((ExcurrentVelocity.Archaeo / 10^6)   /   ((((OstiaAreaPer.Archaeo/10^6) * (100 / (TotalThickness.Archaeo / 10^3)))  /  (100 / (TotalThickness.Archaeo / 10^3)) * 100)   /   ((((pi * ((OsculumDiameter.Archaeo/2)/1000)^2) / ((SurfaceArea.Archaeo / 10^8) *1000) * 100)  /  (100 / (TotalThickness.Archaeo / 10^3))) * 100))*1000000)

### Archaeo Velocity per opening Calculation for Large Incurrent Canal
LargeIncurrentVelocity.Archaeo <- ((ExcurrentVelocity.Archaeo / 10^6)   /   ((((LargeIncurrentAreaPer.Archaeo/10^6) * (100))  /  (100 / (TotalThickness.Archaeo / 10^3)) * 100)   /   ((((pi * ((OsculumDiameter.Archaeo/2)/1000)^2) / ((SurfaceArea.Archaeo / 10^8) *1000) * 100)  /  (100 / (TotalThickness.Archaeo / 10^3))) * 100))*1000000)

### Archaeo Velocity per opening Calculation for Medium Incurrent Canal
MedIncurrentVelocity.Archaeo <- ((ExcurrentVelocity.Archaeo / 10^6)   /   ((((MedIncurrentAreaPer.Archaeo/10^6) * (100))  /  (100 / (TotalThickness.Archaeo / 10^3)) * 100)   /   ((((pi * ((OsculumDiameter.Archaeo/2)/1000)^2) / ((SurfaceArea.Archaeo / 10^8) *1000) * 100)  /  (100 / (TotalThickness.Archaeo / 10^3))) * 100))*1000000)

### Archaeo Velocity per opening Calculation for Small Incurrent Canal
SmallIncurrentVelocity.Archaeo <- ((ExcurrentVelocity.Archaeo / 10^6)   /   ((((SmallIncurrentAreaPer.Archaeo/10^6) * (100))  /  (100 / (TotalThickness.Archaeo / 10^3)) * 100)   /   ((((pi * ((OsculumDiameter.Archaeo/2)/1000)^2) / ((SurfaceArea.Archaeo / 10^8) *1000) * 100)  /  (100 / (TotalThickness.Archaeo / 10^3))) * 100))*1000000)

### Archaeo Velocity per opening Calculation for Prosopyle
ProsopyleVelocity.Archaeo <- ((ExcurrentVelocity.Archaeo / 10^6)   /   ((((ProsopyleAreaPer.Archaeo/10^6) * (100))  /  (100 / (TotalThickness.Archaeo / 10^3)) * 100)   /   ((((pi * ((OsculumDiameter.Archaeo/2)/1000)^2) / ((SurfaceArea.Archaeo / 10^8) *1000) * 100)  /  (100 / (TotalThickness.Archaeo / 10^3))) * 100))*1000000)

### Archaeo Velocity per opening Calculation for Apopyle
ApopyleVelocity.Archaeo <- ((ExcurrentVelocity.Archaeo / 10^6)   /   ((((ApopyleAreaPer.Archaeo/10^6) * (100))  /  (100 / (TotalThickness.Archaeo / 10^3)) * 100)   /   ((((pi * ((OsculumDiameter.Archaeo/2)/1000)^2) / ((SurfaceArea.Archaeo / 10^8) *1000) * 100)  /  (100 / (TotalThickness.Archaeo / 10^3))) * 100))*1000000)

### Archaeo Velocity per opening Calculation for Small Excurrent Canal
SmallExcurrentVelocity.Archaeo <- ((ExcurrentVelocity.Archaeo / 10^6)   /   ((((SmallExcurrentAreaPer.Archaeo/10^6) * (100))  /  (100 / (TotalThickness.Archaeo / 10^3)) * 100)   /   ((((pi * ((OsculumDiameter.Archaeo/2)/1000)^2) / ((SurfaceArea.Archaeo / 10^8) *1000) * 100)  /  (100 / (TotalThickness.Archaeo / 10^3))) * 100))*1000000)

### Archaeo Velocity per opening Calculation for Medium Excurrent Canal
MedExcurrentVelocity.Archaeo <- ((ExcurrentVelocity.Archaeo / 10^6)   /   ((((MedExcurrentAreaPer.Archaeo/10^6) * (100))  /  (100 / (TotalThickness.Archaeo / 10^3)) * 100)   /   ((((pi * ((OsculumDiameter.Archaeo/2)/1000)^2) / ((SurfaceArea.Archaeo / 10^8) *1000) * 100)  /  (100 / (TotalThickness.Archaeo / 10^3))) * 100))*1000000)

### Archaeo Velocity per opening Calculation for Large Excurrent Canal
LargeExcurrentVelocity.Archaeo <- ((ExcurrentVelocity.Archaeo / 10^6)   /   ((((LargeExcurrentAreaPer.Archaeo/10^6) * (100))  /  (100 / (TotalThickness.Archaeo / 10^3)) * 100)   /   ((((pi * ((OsculumDiameter.Archaeo/2)/1000)^2) / ((SurfaceArea.Archaeo / 10^8) *1000) * 100)  /  (100 / (TotalThickness.Archaeo / 10^3))) * 100))*1000000)



################################################################
#### Assign measured Archaeos data to variables
ID.Archaeo <- d.Archaeo[["ID"]]

Species.Archaeo <- d.Archaeo[["Species"]]

OstiaDiameter.Archaeo <- d.Archaeo[["Ostia_Diameter"]]

OstiaLintelWidth.Archaeo <- d.Archaeo[["Ostia_Lintel_Width"]]

OstiaArea.Archaeo <- d.Archaeo[["Ostia_Area"]]

LargeExcurrentDiameter.Archaeo <- d.Archaeo[["Large_Excurrent_Canal_Diameter"]]

LargeExcurrentLintelWidth.Archaeo <- d.Archaeo[["Large_Excurrent_Canal_Lintel_Width"]]

LargeExcurrentArea.Archaeo <- d.Archaeo[["Large_Excurrent_Canal_Area"]]

CupDiameter.Archaeo <- d.Archaeo[["Cup_Diameter"]]

OsaSaRatio.Archaeo <- d.Archaeo[["OSA_SA_Ratio"]]

Height.Archaeo <- d.Archaeo[["Height"]]



################################################################
####Data table of calculated values
Archaeos_Calculations <- data.frame("ID" = c(ID.Archaeo), 
                                    "Species" = c(Species.Archaeo), 
                                    "Ostia_Diameter" = c(OstiaDiameter.Archaeo), 
                                    "Ostia_Lintel_Width" = c(OstiaLintelWidth.Archaeo), 
                                    "Ostia_Area" = c(OstiaArea.Archaeo), 
                                    "Ostia_Area_Per_1mm3" = c(OstiaAreaPer.Archaeo), 
                                    "Ostia_Velocity" = c(OstiaVelocity.Archaeo), 
                                    "Large_Incurrent_Canal_Area_Per_1mm3" = c(LargeIncurrentAreaPer.Archaeo), 
                                    "Large_Incurrent_Canal_Velocity" = c(LargeIncurrentVelocity.Archaeo), 
                                    "Medium_Incurrent_Canal_Area_Per_1mm3" = c(MedIncurrentAreaPer.Archaeo), 
                                    "Medium_Incurrent_Canal_Velocity" = c(MedIncurrentVelocity.Archaeo), 
                                    "Small_Incurrent_Canal_Area_Per_1mm3" = c(SmallIncurrentAreaPer.Archaeo), 
                                    "Small_Incurrent_Canal_Velocity" = c(SmallIncurrentVelocity.Archaeo),
                                    "Prosopyle_Area_Per_1mm3" = c(ProsopyleAreaPer.Archaeo), 
                                    "Prosopyle_Velocity" = c(ProsopyleVelocity.Archaeo), 
                                    "Apopyle_Area_Per_1mm3" = c(ApopyleAreaPer.Archaeo), 
                                    "Apopyle_Velocity" = c(ApopyleVelocity.Archaeo),
                                    "Small_Excurrent_Canal_Area_Per_1mm3" = c(SmallExcurrentAreaPer.Archaeo), 
                                    "Small_Excurrent_Canal_Velocity" = c(SmallExcurrentVelocity.Archaeo), 
                                    "Medium_Excurrent_Canal_Area_Per_1mm3" = c(MedExcurrentAreaPer.Archaeo), 
                                    "Medium_Excurrent_Canal_Velocity" = c(MedExcurrentVelocity.Archaeo),
                                    "Large_Excurrent_Canal_Diameter" = c(LargeExcurrentDiameter.Archaeo), 
                                    "Large_Excurrent_Canal_Lintel_Width" = c(LargeExcurrentLintelWidth.Archaeo), 
                                    "Large_Excurrent_Canal_Area" = c(LargeExcurrentArea.Archaeo), 
                                    "Large_Excurrent_Canal_Area_Per_1mm3" = c(LargeExcurrentAreaPer.Archaeo), 
                                    "Large_Excurrent_Canal_Velocity" = c(LargeExcurrentVelocity.Archaeo), 
                                    "Osculum_Diameter" = c(OsculumDiameter.Archaeo), 
                                    "Excurrent_Velocity" = c(ExcurrentVelocity.Archaeo), 
                                    "Cup_Diameter" = c(CupDiameter.Archaeo),
                                    "Outer_Wall_Thickness" = c(OuterWallThickness.Archaeo),
                                    "Inner_Wall_Thickness" = c(InnerWallThickness.Archaeo),
                                    "Intervallum_Width" = c(IntervallumWidth.Archaeo),
                                    "Surface_Area" = c(SurfaceArea.Archaeo),
                                    "Osculum_Area" = c(OsculumArea.Archaeo),
                                    "OSA_SA_Ratio" = c(OsaSaRatio.Archaeo),
                                    "Height" = c(Height.Archaeo),
                                    "Volumetric_Oscular_Flow_Rate" = c(VolumetricOscularFlowRate.Archaeo)
                                    )



#### Write data frame to Excel
write_xlsx(
  Archaeos_Calculations, "Archaeos_Calculations.xlsx",
  col_names = TRUE)

