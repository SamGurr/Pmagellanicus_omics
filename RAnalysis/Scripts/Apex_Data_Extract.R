#http://www.informit.com/articles/article.aspx?p=2215520

#modified as of 20250625by SJG
#NOTE: April - July conditioning (pediveliger to juvenile) only involves T5, T6, and T7; 
# T5 = elevated pCO2 (head tank) 
# T6 = elevated pCO2 (top heath tray in stack)
# T7 = ambient conditions (second heath tray in stack)
#added reminders on lines 39 and 43 to prevent overwritting files ->  SJG

library("XML")
library("plyr")
library(RCurl)
library(tidyr)

# Set your Apex controller's IP address and XML endpoint - load into the computer 
# NOTE: you must be on the same wifi IP address that the Apex is connected to
xmlfile.tanks_1_6 <- xmlParse("http://10.80.55.38:80/cgi-bin/datalog.xml?sdate=250611&days=15") # tanks 1 - 7
xmlfile.tanks_7_12 <- xmlParse("http://10.80.55.33:80/cgi-bin/datalog.xml?sdate=250611&days=15") #
xmlfile.tanks_13_20 <- xmlParse("http://10.80.55.75:80/cgi-bin/datalog.xml?sdate=250611&days=15") #

# tanks 1 - 6 as data frame
ApexData.tanks_1_6 <- ldply(xmlToList(xmlfile.tanks_1_6), data.frame) #convert xml to dataframe
head(ApexData.tanks_1_6) # check the first few lines to see the first few hrs of the extracted data
tail(ApexData.tanks_1_6) # check to end to dertmine if the xmlParse extracted up to present day
ApexData.tanks_1_6 <- ApexData.tanks_1_6[4:nrow(ApexData.tanks_1_6),] #remove extra metadata from top
ApexData.tanks_1_6 <- head(ApexData.tanks_1_6,-2) #remove extra metadata from bottom

# tanks 7 - 12 as data frame
ApexData.tanks_7_12 <- ldply(xmlToList(xmlfile.tanks_7_12), data.frame) #convert xml to dataframe
head(ApexData.tanks_7_12) # check the first few lines to see the first few hrs of the extracted data
tail(ApexData.tanks_7_12) # check to end to dertmine if the xmlParse extracted up to present day
ApexData.tanks_7_12 <- ApexData.tanks_7_12[4:nrow(ApexData.tanks_7_12),] #remove extra metadata from top
ApexData.tanks_7_12<- head(ApexData.tanks_7_12,-2) #remove extra metadata from bottom

# tanks 13 - 20 as data frame
ApexData.tanks_13_20 <- ldply(xmlToList(xmlfile.tanks_13_20), data.frame) #convert xml to dataframe
head(ApexData.tanks_13_20) # check the first few lines to see the first few hrs of the extracted data
tail(ApexData.tanks_13_20) # check to end to dertmine if the xmlParse extracted up to present day
ApexData.tanks_13_20 <- ApexData.tanks_13_20[4:nrow(ApexData.tanks_13_20),] #remove extra metadata from top
ApexData.tanks_13_20 <- head(ApexData.tanks_13_20,-2) #remove extra metadata from bottom

# ----------------------------------------------- #
# CLEAN UP THE DATA CALLING TARGET COLUMNS 
# ----------------------------------------------- #

# ProbeData tanks 1 - 6 
head(ApexData.tanks_1_6[1,])
ProbeData.tanks_1_6 <- as.data.frame(ApexData.tanks_1_6) %>% 
                                dplyr::select('date',
                                              'probe.value.44', # temperature tank 1
                                              'probe.value.45', # pH tank 1
                                              
                                              'probe.value.40', # temperature tank 2
                                              'probe.value.41', # pH tank 2
                                              
                                              'probe.value.42',# temperature tank 3
                                              'probe.value.43', # pH tank 3
                                              
                                              'probe.value.38',# temperature tank 4
                                              'probe.value.39',# pH tank 4
                                              
                                              'probe.value',# temperature tank 5
                                              'probe.value.1',# pH tank 5
                                              
                                              'probe.value.36',# temperature tank 6
                                              'probe.value.37'# pH tank 6
                                              )  %>% 
                                dplyr::rename(
                                  Tank1_temp  = probe.value.44,
                                  Tank1_pHNBS = probe.value.45,
                                  
                                  Tank2_temp  = probe.value.40,
                                  Tank2_pHNBS = probe.value.41,
                                  
                                  Tank3_temp  = probe.value.42,
                                  Tank3_pHNBS = probe.value.43,
                                  
                                  Tank4_temp  = probe.value.38,
                                  Tank4_pHNBS = probe.value.39,
                                  
                                  Tank5_temp  = probe.value,
                                  Tank5_pHNBS = probe.value.1,
                                  
                                  Tank6_temp  = probe.value.36,
                                  Tank6_pHNBS = probe.value.37
                                ) %>% 
                          dplyr::mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S", tz="HST"))





# ProbeData tanks 7 - 12 
head(ApexData.tanks_7_12[1,])
ProbeData.tanks_7_12 <- as.data.frame(ApexData.tanks_7_12) %>% 
  dplyr::select('date',
                'probe.value.2', # temperature tank 7
                'probe.value.3', # pH tank 7
                
                'probe.value.25', # temperature tank 8
                'probe.value.26', # pH tank 8
                
                'probe.value.27',# temperature tank 9
                'probe.value.28', # pH tank 9
                
                'probe.value.4',# temperature tank 10
                'probe.value.5',# pH tank 10
                
                'probe.value.6',# temperature tank 11
                'probe.value.7',# pH tank 11
                
                'probe.value',# temperature tank 12
                'probe.value.1'# pH tank 12
  )  %>% 
  dplyr::rename(
    Tank7_temp  = probe.value.2,
    Tank7_pHNBS = probe.value.3,
    
    Tank8_temp  = probe.value.25,
    Tank8_pHNBS = probe.value.26,
    
    Tank9_temp  = probe.value.27,
    Tank9_pHNBS = probe.value.28,
    
    Tank10_temp  = probe.value.4,
    Tank10_pHNBS = probe.value.5,
    
    Tank11_temp  = probe.value.6,
    Tank11_pHNBS = probe.value.7,
    
    Tank12_temp  = probe.value,
    Tank12_pHNBS = probe.value.1
  ) %>% 
  dplyr::mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S", tz="HST"))












# ProbeData tanks 13 - 20 
head(ApexData.tanks_13_20[1,])
ProbeData.tanks_13_20 <- as.data.frame(ApexData.tanks_13_20) %>% 
  dplyr::select('date',
                'probe.value.36', # temperature tank 13
                'probe.value.37', # pH tank 13
                
                'probe.value.38', # temperature tank 14
                'probe.value.39', # pH tank 14
                
                'probe.value',# temperature tank 15
                'probe.value.1', # pH tank 15
                
                'probe.value.40',# temperature tank 16
                'probe.value.41'# pH tank 16
  )  %>% 
  dplyr::rename(
    Tank13_temp  = probe.value.36,
    Tank13_pHNBS = probe.value.37,
    
    Tank14_temp  = probe.value.38,
    Tank14_pHNBS = probe.value.39,
    
    Tank15_temp  = probe.value,
    Tank15_pHNBS = probe.value.1,
    
    Tank16_temp  = probe.value.40,
    Tank16_pHNBS = probe.value.41
  ) %>% 
  dplyr::mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S", tz="HST"))



















# CHANGE DATE FOR NEW CSV (risk overwritting previous)
write.csv(ApexData.tanks_1_6, "C:/Users/samjg/Documents/Github_repositories/Pmagellanicus_omics/RAnalysis/Data/Seawater_chemistry/Apex_raw/Apex_raw_tanks_1_6.csv") #write file to save data
write.csv(ProbeData.tanks_1_6, "C:/Users/samjg/Documents/Github_repositories/Pmagellanicus_omics/RAnalysis/Data/Seawater_chemistry/Apex_raw/Apex_temp_pH_tanks_1_6.csv") #write file to save data

write.csv(ApexData.tanks_7_12, "C:/Users/samjg/Documents/Github_repositories/Pmagellanicus_omics/RAnalysis/Data/Seawater_chemistry/Apex_raw/Apex_raw_tanks_7_12.csv") #write file to save data
write.csv(ProbeData.tanks_7_12, "C:/Users/samjg/Documents/Github_repositories/Pmagellanicus_omics/RAnalysis/Data/Seawater_chemistry/Apex_raw/Apex_temp_pH_tanks_7_12.csv") #write file to save data

write.csv(ApexData.tanks_13_20, "C:/Users/samjg/Documents/Github_repositories/Pmagellanicus_omics/RAnalysis/Data/Seawater_chemistry/Apex_raw/Apex_raw_tanks_13_16.csv") #write file to save data
write.csv(ProbeData.tanks_13_20, "C:/Users/samjg/Documents/Github_repositories/Pmagellanicus_omics/RAnalysis/Data/Seawater_chemistry/Apex_raw/Apex_temp_pH_tanks_13_16.csv") #write file to save data



#plot Temp and pH and save to output
# CHANGE DATE FOR NEW PDF (risk overwritting previous)
pdf("C:/Users/samjg/Documents/My_Projects/Inragenerational_thresholds_OA/RAnalysis/Data/Apex_data/Graphs/20190815_Apex_Data_Output.data.pdf")
par(mfrow=c(2,1))
plot(as.numeric(as.character(Tank1_temp)) ~ date, ProbeData.tanks_1_6, col = "black", type="l", ylim=c(8, 18),  xlab="Time", ylab="Temperature °C")
lines(as.numeric(as.character(Tank2_temp)) ~ date, ProbeData.tanks_1_6, col = "red")
lines(as.numeric(as.character(Tank3_temp)) ~ date, ProbeData.tanks_1_6, col = "yellow")
lines(as.numeric(as.character(Tank4_temp)) ~ date, ProbeData.tanks_1_6, col = "blue")
lines(as.numeric(as.character(Tank5_temp)) ~ date, ProbeData.tanks_1_6, col = "red")
lines(as.numeric(as.character(Tank6_temp)) ~ date, ProbeData.tanks_1_6, col = "blue")

axis.POSIXct(side=1, ProbeData.tanks_1_6$date)

plot(as.numeric(as.character(Tank1_pHNBS)) ~ date, ProbeData.tanks_1_6, col = "black", type="l", ylim=c(7.5, 8),  xlab="Time", ylab="pH (NBS)")
lines(as.numeric(as.character(Tank2_pHNBS)) ~ date, ProbeData.tanks_1_6, col = "red")
lines(as.numeric(as.character(Tank3_pHNBS)) ~ date, ProbeData.tanks_1_6, col = "yellow")
lines(as.numeric(as.character(Tank4_pHNBS)) ~ date, ProbeData.tanks_1_6, col = "blue")
lines(as.numeric(as.character(Tank5_pHNBS)) ~ date, ProbeData.tanks_1_6, col = "red")
lines(as.numeric(as.character(Tank6_pHNBS)) ~ date, ProbeData.tanks_1_6, col = "blue")

axis.POSIXct(side=1, ProbeData.tanks_1_6$date)

# plot(as.numeric(as.character(Salt_XL)) ~ Date.Time, Probe.Data, col = "grey", type="l", ylim=c(20, 35),  xlab="Time", ylab="Salinity psu")
# lines(as.numeric(as.character(Salt_L)) ~ Date.Time, Probe.Data, col = "red")
# lines(as.numeric(as.character(Salt_A)) ~ Date.Time, Probe.Data, col = "blue")
# axis.POSIXct(side=1, Probe.Data$Date.Time)
dev.off()

