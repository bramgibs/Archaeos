rm(list=ls())

library('readxl')
library('tidyr')
library('tidyverse')

#Just comparing osculum surface area to the surface area of the entire shape

#Reformat References

#Get into R properly
dir = file.choose()

d=read_xlsx(dir,sheet=1,col_names=T)

d$dia.mm=d$Osculum_Diameter/1000
#Add a shape column
###STILL WORKING ON THIS AND ADDING SHAPES TO EACH INDIVIDUAL SPECIES
range(d$Height)

hist(d$Height,main="Height Plotted",
xlab="Heights (μm)",ylab="# of Specimens",xaxt="n", col="darkblue", 
border="black", breaks=20)
axis(side=1,at=seq(5000,50000,by=2500),las=1)

#setting variable to create osculum area
osculumarea=pi*((d$dia.mm/2)^2)

range (osculumarea)
#this guy, really?
#plugged numbers into chat gpt to get manual range
minx <- min(na.omit(osculumarea))
maxx <- max(na.omit(osculumarea))
bys = (maxx-minx)/8

hist(osculumarea,main="Osculum Area",
     xlab=expression("Osculum Area (mm"^"2" ~")"),ylab="# of Specimens", col="maroon", 
     border="black", xaxt ='n',breaks=30)
axis(side=1,at=seq(minx,maxx,by=bys),las=1)

     