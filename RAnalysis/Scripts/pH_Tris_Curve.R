# modified for personal Lenovo PC at PT Whitney summer 2019
# last modification on 20250621 by S.Gurr for the OA lab at Downeast Institute
setwd("C:/Users/samjg/Documents/Github_repositories/Pmagellanicus_omics/RAnalysis/Data/Seawater_chemistry/pH_Calibration_Files/") #set working directory
path = "C:/Users/samjg/Documents/Github_repositories/Pmagellanicus_omics/RAnalysis/Data/Seawater_chemistry/pH_Calibration_Files/"

# change line 7
Calib.Data <-read.table("20250623.csv", header=TRUE, sep=",", na.string="NA", as.is=TRUE) #reads in the data files
model <-lm(mVTris ~ TTris, data=Calib.Data) #runs a linear regression of mV as a function of temperature
coe <- coef(model) #extracts the coeffecients
R2<-summary(model)$r.squared

plot(mVTris ~ TTris, data=Calib.Data)
abline(lm(mVTris ~ TTris, data=Calib.Data))
legend('topleft', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')

# change line 17. done.
pdf(paste0(path,"20250623.pdf"))
plot(mVTris ~ TTris, data=Calib.Data)
abline(lm(mVTris ~ TTris, data=Calib.Data))
legend('topleft', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
dev.off()