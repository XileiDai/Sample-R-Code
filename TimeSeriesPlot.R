load('oridata.Rdata')
oridata$season[(oridata$Time>=BegT1 & oridata$Time<EndT1) | 
                 (oridata$Time>=BegT2 & oridata$Time<EndT2)]='autumn'
oridata$SC = as.numeric(oridata$SC)
tapply(oridata$CO2[oridata$CO2>500], oridata$season[oridata$CO2>500], quantile, 0.1, na.rm = T)
tapply(oridata$CO2[oridata$CO2>500], oridata$season[oridata$CO2>500], quantile, 0.25, na.rm = T)
tapply(oridata$CO2[oridata$CO2>500], oridata$season[oridata$CO2>500], quantile, 0.5, na.rm = T)
tapply(oridata$CO2[oridata$CO2>500], oridata$season[oridata$CO2>500], quantile, 0.75, na.rm = T)
tapply(oridata$CO2[oridata$CO2>500], oridata$season[oridata$CO2>500], quantile, 0.9, na.rm = T)
