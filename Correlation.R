### Script for Correlation Calculation
### May 2, 2024
### Prepared by P. Attanasio
rm(list=ls())

### Setting directory for '~/Downloads' but can adjust to project folder
getwd()
setwd('~/Documents/GitHub/Archaeos')

### Getting packages set
require(readxl)
require(writexl)
require(ggplot2)

### Read in data as tibble
d.Demo <- read_xlsx('Demosponge Measurements.xlsx',col_names = T)
str(d.Demo)

d.Archaeo <- read_xlsx('Archaeos Measurements.xlsx',col_names = T)
str(d.Archaeo)



### Large Incurrent Canal Diameter linear regression
LInDiameter.lm <- lm(Large_Incurrent_Canal_Diameter ~ Ostia_Diameter, data = d.Demo)
summary(LInDiameter.lm)

# Archaeo Large Incurrent Canal Diameter calculation
OstiaDiameter.Archaeo <- d.Archaeo[["Ostia_Diameter"]]

LargeIncurrentDiameter.Archaeo <- OstiaDiameter.Archaeo*LInDiameter.lm$coefficients[2]+LInDiameter.lm$coefficients[1]
LargeIncurrentDiameter.Archaeo



### Medium Incurrent Canal Diameter linear regression
MInDiameter.lm <- lm(Medium_Incurrent_Canal_Diameter ~ Ostia_Diameter, data = d.Demo)
summary(MInDiameter.lm)

# Archaeo Medium Incurrent Canal Diameter calculation
MedIncurrentDiameter.Archaeo <- OstiaDiameter.Archaeo*MInDiameter.lm$coefficients[2]+MInDiameter.lm$coefficients[1]
MedIncurrentDiameter.Archaeo



### Small Incurrent Canal Diameter linear regression
SInDiameter.lm <- lm(Small_Incurrent_Canal_Diameter ~ Ostia_Diameter, data = d.Demo)
summary(SInDiameter.lm)

# Archaeo Small Incurrent Canal Diameter calculation
SmallIncurrentDiameter.Archaeo <- OstiaDiameter.Archaeo*SInDiameter.lm$coefficients[2]+SInDiameter.lm$coefficients[1]
SmallIncurrentDiameter.Archaeo



### Prosopyle Diameter linear regression
ProsopyleDiameter.lm <- lm(Prosopyle_Diameter ~ Ostia_Diameter, data = d.Demo)
summary(ProsopyleDiameter.lm)

# Archaeo Prosopyle Diameter calculation
ProsopyleDiameter.Archaeo <- OstiaDiameter.Archaeo*ProsopyleDiameter.lm$coefficients[2]+ProsopyleDiameter.lm$coefficients[1]
ProsopyleDiameter.Archaeo



### Medium Excurrent Canal Diameter linear regression
MExDiameter.lm <- lm(Medium_Excurrent_Canal_Diameter ~ Large_Excurrent_Canal_Diameter, data = d.Demo)
summary(MExDiameter.lm)

# Archaeo Medium Excurrent Canal Diameter calculation
LargeExcurrentDiameter.Archaeo <- d.Archaeo[["Large_Excurrent_Canal_Diameter"]]

MedExcurrentDiameter.Archaeo <- LargeExcurrentDiameter.Archaeo*MExDiameter.lm$coefficients[2]+MExDiameter.lm$coefficients[1]
MedExcurrentDiameter.Archaeo



### Small Excurrent Canal Diameter linear regression
SExDiameter.lm <- lm(Small_Excurrent_Canal_Diameter ~ Large_Excurrent_Canal_Diameter, data = d.Demo)
summary(SExDiameter.lm)

# Archaeo Small Excurrent Canal Diameter calculation
SmallExcurrentDiameter.Archaeo <- LargeExcurrentDiameter.Archaeo*SExDiameter.lm$coefficients[2]+SExDiameter.lm$coefficients[1]
SmallExcurrentDiameter.Archaeo



### Apopyle Diameter linear regression
ApopyleDiameter.lm <- lm(Apopyle_Diameter ~ Large_Excurrent_Canal_Diameter, data = d.Demo)
summary(ApopyleDiameter.lm)

# Archaeo Apopyle Diameter calculation
ApopyleDiameter.Archaeo <- LargeExcurrentDiameter.Archaeo*ApopyleDiameter.lm$coefficients[2]+ApopyleDiameter.lm$coefficients[1]
ApopyleDiameter.Archaeo



### Excurrent Velocity linear regression
ExVelocity.lm <- lm(Excurrent_Velocity ~ Osculum_Diameter, data = d.Demo)
summary(ExVelocity.lm)

# Archaeo velocity calculation
OsculumDiameter.Archaeo <- d.Archaeo[["Osculum_Diameter"]]
mean(OsculumDiameter.Archaeo)

ExcurrentVelocity.Archaeo <- OsculumDiameter.Archaeo*ExVelocity.lm$coefficients[2]+ExVelocity.lm$coefficients[1]
ExcurrentVelocity.Archaeo



### Archaeo Volumetric Flow calculation

OsculumArea.Archaeo <- d.Archaeo[["Osculum_Area"]]

VolumetricOscularFlowRate.Archaeo <- OsculumArea.Archaeo * ExcurrentVelocity.Archaeo





#### Data table of calculated values
ID.Archaeo <- d.Archaeo[["ID"]]

Species.Archaeo <- d.Archaeo[["Species"]]

CupDiameter.Archaeo <- d.Archaeo[["Cup_Diameter"]]

OuterWallThickness.Archaeo <- d.Archaeo[["Outer_Wall_Thickness"]]

InnerWallThickness.Archaeo <- d.Archaeo[["Inner_Wall_Thickness"]]

IntervallumWidth.Archaeo <- d.Archaeo[["Intervallum_Width"]]

SurfaceArea.Archaeo <- d.Archaeo[["Surface_Area"]]

OsaSaRatio.Archaeo <- d.Archaeo[["OSA_SA_Ratio"]]

Height.Archaeo <- d.Archaeo[["Height"]]



Archaeos_Calculations <- data.frame("ID" = c(ID.Archaeo), 
                                    "Species" = c(Species.Archaeo), 
                                    "Ostia_Diameter" = c(OstiaDiameter.Archaeo), 
                                    "Large_Incurrent_Diameter" = c(LargeIncurrentDiameter.Archaeo), 
                                    "Medium_Incurrent_Diameter" = c(MedIncurrentDiameter.Archaeo), 
                                    "Small_Incurrent_Diameter" = c(SmallIncurrentDiameter.Archaeo), 
                                    "Prosopyle_Diameter" = c(ProsopyleDiameter.Archaeo), 
                                    "Apopyle_Diameter" = c(ApopyleDiameter.Archaeo), 
                                    "Small_Excurrent_Canal_Diameter" = c(SmallExcurrentDiameter.Archaeo), 
                                    "Medium_Excurrent_Canal_Diameter" = c(MedExcurrentDiameter.Archaeo), 
                                    "Large_Excurrent_Canal_Diameter" = c(LargeExcurrentDiameter.Archaeo), 
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
Archaeos_Calculations


#### Write data frame to Excel
write_xlsx(
  Archaeos_Calculations, "Archaeos_Calculations.xlsx",
  col_names = TRUE)

