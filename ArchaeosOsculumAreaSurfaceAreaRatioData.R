#rm(list=ls())

library('readxl')
library('tidyr')
library('tidyverse')
library('ggplot2')

#Just comparing osculum surface area to the surface area of the entire shape

#Reformat References

#Get into R properly
dir = file.choose()

d=read_xlsx(dir,sheet=1,col_names=T)

#setting variables to be in mm as opposed to µm
d$dia.mm=d$Osculum_Diameter/1000
d$cupdia.mm=d$Cup_Diameter/1000

range(d$Height)

par(mfrow = c(1,1))
#hist for height for comparison
hist(d$Height,main="Height Plotted",
xlab="Heights (μm)",ylab="# of Specimens",xaxt="n", col="darkblue", 
border="black", breaks=20)
axis(side=1,at=seq(5000,50000,by=2500),las=1)

#setting variable to create osculum area
osculumarea=pi*((d$dia.mm/2)^2)

range (osculumarea)
#setting manual range by omitting nulls and setting them as N/A
minx <- min(na.omit(osculumarea))
maxx <- max(na.omit(osculumarea))
bys = (maxx-minx)/8

hist(osculumarea,main="Osculum Area",
     xlab=expression("Osculum Area (mm"^"2" ~")"),ylab="# of Specimens", col="maroon", 
     border="black", xaxt ='n',breaks=30)
axis(side=1,at=seq(minx,maxx,by=bys),las=1)


#setting variables to make this formula easier to write
r.mm=d$cupdia.mm/2
h.mm=d$Height/1000

#creating surface area formula
surfacearea=(pi*r.mm)*(r.mm + sqrt((h.mm^2)+(r.mm^2)))

#omitting nulls in range, setting them as N/A
miny <- min(na.omit(surfacearea))
maxy <- max(na.omit(surfacearea))
bysy = (maxy-miny)/8

#hist to see the values
hist(surfacearea,main="Surface Area",
     xlab=expression("Surface Area (mm"^"2" ~")"),ylab="# of Specimens", col="wheat", 
     border="black", xaxt ='n',breaks=30)
axis(side=1,at=seq(miny,maxy,by=bysy),las=1)

#lets compare it with the original values he got

par(mfrow = c(1,2))
    hist(d$Surface_Area,main="Surface Area",
         xlab=expression("Surface Area ("*mu*"m"^"2" ~")"),ylab="# of Specimens", col="wheat", 
         border="black",breaks=30)
  
    hist(surfacearea,main="Surface Area",
         xlab=expression("Surface Area (mm"^"2" ~")"),ylab="# of Specimens", col="wheat", 
         border="black",breaks=30, xaxt ='n')
axis(side=1,at=seq(miny,maxy,by=bysy),las=1)
    
#thank god that looks good

    
#time to ggplot this against osculum area

#set this dataframe up
#got lines 82-96 from Katrina Torres
df=data.frame(
  value = c(osculumarea, surfacearea),
  measurement_type = c(rep("Osculum Area", length(osculumarea)),
                       rep("Surface Area", length(surfacearea)))
)

#actually plot it and it work hopefully
ggplot(df, aes(x = value, fill = measurement_type)) +
  geom_histogram(position = "dodge", bins = 30, color = "black") +
  labs(x = "Measurement Value (mm²)",
       y = "Number of Specimens",
       title = "Osculum Area vs Surface Area") +
  scale_fill_manual(values = c("skyblue", "orange"),
                    labels = c("Osculum Area", "Surface Area")) +
  theme_minimal()

#nice!

#time to actually scatterplot the data
par(mfrow = c(1,1))
plot(surfacearea,osculumarea,
main="Surface Area vs Osculum area", xlab=expression("Surface Area (mm"^"2" ~")"), ylab=expression("Osculum Area (mm"^"2" ~")"), col="seagreen3",pch=19)

#data needs to be logarithmic because its all on one side

plot(surfacearea,osculumarea,
     main="Surface Area vs Osculum area", xlab=expression("Surface Area (mm"^"2" ~")"), ylab=expression("Osculum Area (mm"^"2" ~")"), col="seagreen3",pch=19,log="xy")

#nice

#remove 0s from data for the regression line

#set the model up

model <- lm(osculumarea ~ surfacearea)
abline(model, col = "red", lwd = 2)

#new issue, i cant get the abline to work in logspace


plot(surfacearea,osculumarea,
     main="Surface Area vs Osculum area", xlab=expression("Surface Area (mm"^"2" ~")"), ylab=expression("Osculum Area (mm"^"2" ~")"), col="seagreen3",pch=19,log="xy")

model <- lm(log(osculumarea) ~ log(surfacearea))


#got lines 129-130 from chatgpt
curve(exp(coef(model)[1]) * x^coef(model)[2],
      add = TRUE, col = "red", lwd = 2)

