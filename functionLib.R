Sys.setlocale("LC_ALL","Chinese")
Sys.setenv(TZ="Asia/Shanghai")
HourlyAvg = function(index, Data){
  lapply(index, function(i){
    Data_i = Data[Data$Name == device_list_total$Name[i] &
                    Data$Room == device_list_total$Room[i],]
    Data_i$Time = as.POSIXct(Data_i$Time, format = '%Y-%m-%d %H:%M:%S')
    Data_i = Data_i[!is.na(Data_i$Time),]
    Data_i$Time = cut(Data_i$Time, breaks ='hour')
    Time = unique(Data_i$Time)
    Tem = tapply(Data_i$Tem, Data_i$Time, mean, na.rm = T)
    RH = tapply(Data_i$RH, Data_i$Time, mean, na.rm = T)
    FA = tapply(Data_i$FA, Data_i$Time, mean, na.rm = T)
    CO2 = tapply(Data_i$CO2, Data_i$Time, mean, na.rm = T)
    TVOC = tapply(Data_i$TVOC, Data_i$Time, mean, na.rm = T)
    PM = tapply(Data_i$PM, Data_i$Time, mean, na.rm = T)
    Mins = tapply(Data_i$PM, Data_i$Time, length)
    nrow = length(CO2)
    result = data.frame(Name = rep(device_list_total$Name[i], nrow),
                        Room = rep(device_list_total$Room[i], nrow),
                        Time = names(Tem), Index = Mins,
                        Tem = Tem,
                        RH = RH, FA = FA, CO2 = CO2, TVOC = TVOC, PM = PM)
    return(result)
  })
}
MatchOutPara = function(index, Data){
  lapply(index, function(i){
    Data_i = Data[Data$Name == device_list_total$Name[i] &
                    Data$Room == device_list_total$Room[i] &
                    Data$PM_pos == device_list_total$PM_pos[i],]
    # Data_i$Time = as.POSIXct(Data_i$Time, format = '%Y-%m-%d %H:%M:%S')
    
    OutPM_i = OutPM[OutPM$City == unique(Data_i$City) & 
                      OutPM$Position == unique(Data_i$PM_pos),]
    OutWeather_i = OutWeather[OutWeather$Position == unique(Data_i$Tem_pos), ]
    OutPM_i$Time = as.POSIXct(OutPM_i$Time, format = '%Y-%m-%dT%H:%M:%SZ')
    OutWeather_i$Time = as.POSIXct(OutWeather_i$Time, format = '%Y-%m-%d %H:%M:%S')
    OutWeather_i$Time = cut(OutWeather_i$Time, breaks = 'hour')
    OutWeather_i$Time = as.POSIXct(OutWeather_i$Time, format = '%Y-%m-%d %H:%M:%S')
    Data_i$PM_out = OutPM_i[match(Data_i$Time, OutPM_i$Time), 4]
    Data_i$AQI_out = OutPM_i[match(Data_i$Time, OutPM_i$Time), 5]
    Data_i$Tem_out = OutWeather_i[match(Data_i$Time, OutWeather_i$Time), 4]
    Data_i$Hum_out = OutWeather_i[match(Data_i$Time, OutWeather_i$Time), 5]
    Data_i$Wind = OutWeather_i[match(Data_i$Time, OutWeather_i$Time), 6]
    Data_i$SC = OutWeather_i[match(Data_i$Time, OutWeather_i$Time), 7]
    
    
    Data_i$Time2 = as.POSIXlt(Data_i$Time, format = '%Y-%m-%d %H:%M:%S')
    Data_i$Time2$hour = Data_i$Time2$hour - 1
    Data_i$Time2 = as.POSIXct(Data_i$Time2, format = '%Y-%m-%d %H:%M:%S')
    Data_i$PM_out2 = OutPM_i[match(Data_i$Time2, OutPM_i$Time), 4]
    Data_i$AQI_out2 = OutPM_i[match(Data_i$Time2, OutPM_i$Time), 5]
    Data_i$Tem2_out = OutWeather_i[match(Data_i$Time2, OutWeather_i$Time), 4]
    Data_i$Hum2_out = OutWeather_i[match(Data_i$Time2, OutWeather_i$Time), 5]
    Data_i$Wind2 = OutWeather_i[match(Data_i$Time2, OutWeather_i$Time), 6]
    Data_i$SC2 = OutWeather_i[match(Data_i$Time2, OutWeather_i$Time), 7]
    
    Data_i$PM_out[is.na(Data_i$PM_out)]=Data_i$PM_out2[is.na(Data_i$PM_out)]
    Data_i$AQI_out[is.na(Data_i$AQI_out)]=Data_i$AQI_out2[is.na(Data_i$AQI_out)]
    Data_i$Tem_out[is.na(Data_i$Tem_out)]=Data_i$Tem2_out[is.na(Data_i$Tem_out)]
    Data_i$Hum_out[is.na(Data_i$Hum_out)]=Data_i$Hum2_out[is.na(Data_i$Hum_out)]
    Data_i$Wind[is.na(Data_i$Wind)]=Data_i$Wind2[is.na(Data_i$Wind)]
    Data_i$SC[is.na(Data_i$SC)]=Data_i$SC2[is.na(Data_i$SC)]
    Data_i = Data_i[,c(1:4,5:15,16,17,18,19)]
    
    Data_i$Time2 = as.POSIXlt(Data_i$Time, format = '%Y-%m-%d %H:%M:%S')
    Data_i$Time2$hour = Data_i$Time2$hour + 1
    Data_i$Time2 = as.POSIXct(Data_i$Time2, format = '%Y-%m-%d %H:%M:%S')
    Data_i$PM_out2 = OutPM_i[match(Data_i$Time2, OutPM_i$Time), 4]
    Data_i$AQI_out2 = OutPM_i[match(Data_i$Time2, OutPM_i$Time), 5]
    Data_i$Tem2_out = OutWeather_i[match(Data_i$Time2, OutWeather_i$Time), 4]
    Data_i$Hum2_out = OutWeather_i[match(Data_i$Time2, OutWeather_i$Time), 5]
    Data_i$Wind2 = OutWeather_i[match(Data_i$Time2, OutWeather_i$Time), 6]
    Data_i$SC2 = OutWeather_i[match(Data_i$Time2, OutWeather_i$Time), 7]
    
    Data_i$PM_out[is.na(Data_i$PM_out)]=Data_i$PM_out2[is.na(Data_i$PM_out)]
    Data_i$AQI_out[is.na(Data_i$AQI_out)]=Data_i$AQI_out2[is.na(Data_i$AQI_out)]
    Data_i$Tem_out[is.na(Data_i$Tem_out)]=Data_i$Tem2_out[is.na(Data_i$Tem_out)]
    Data_i$Hum_out[is.na(Data_i$Hum_out)]=Data_i$Hum2_out[is.na(Data_i$Hum_out)]
    Data_i$Wind[is.na(Data_i$Wind)]=Data_i$Wind2[is.na(Data_i$Wind)]
    Data_i$SC[is.na(Data_i$SC)]=Data_i$SC2[is.na(Data_i$SC)]
    Data_i = Data_i[,c(1:4,5:15,16,17,18,19)]
    return(Data_i)
  })
}
DayAvg = function(Name){
  lapply(Name, function(Name_i){
    CQ_Name = CQ[CQ$Name == Name_i,]
    Room = unique(na.omit(CQ_Name$Room))
    DayAvg2 = function(Room){
      lapply(Room, function(Room_i){
        CQ_Name_Room = CQ_Name[CQ_Name$Room == Room_i,]
        print(Name_i)
        print(Room_i)
        Tem = tapply(CQ_Name_Room$Tem, CQ_Name_Room$Time, mean, na.rm = T)
        RH = tapply(CQ_Name_Room$RH, CQ_Name_Room$Time, mean, na.rm = T)
        FA = tapply(CQ_Name_Room$FA, CQ_Name_Room$Time, mean, na.rm = T)
        CO2 = tapply(CQ_Name_Room$CO2, CQ_Name_Room$Time, mean, na.rm = T)
        TVOC = tapply(CQ_Name_Room$TVOC, CQ_Name_Room$Time, mean, na.rm = T)
        PM = tapply(CQ_Name_Room$PM, CQ_Name_Room$Time, mean, na.rm = T)
        PM_out = tapply(CQ_Name_Room$PM_out, CQ_Name_Room$Time, mean, na.rm = T)
        AQI_out = tapply(CQ_Name_Room$AQI_out, CQ_Name_Room$Time, mean, na.rm = T)
        Tem_out = tapply(CQ_Name_Room$Tem_out, CQ_Name_Room$Time, mean, na.rm = T)
        Hum_out = tapply(CQ_Name_Room$Hum_out, CQ_Name_Room$Time, mean, na.rm = T)
        Time <<- row.names(Hum_out)
        windData = data.frame(Time = NULL, SC = NULL, 北风 = NULL, 东北风 = NULL, 东风 = NULL, 东南风 = NULL,
                              南风 = NULL, 无持续风向 = NULL, 西北风 = NULL, 西风 = NULL, 西南风 = NULL)
        timelist = na.omit(unique(CQ_Name_Room$Time))
        
        
        GetWind = function(timelist){
          lapply(timelist,function(timelist_i){
            CQ_Name_Room_Time = CQ_Name_Room[CQ_Name_Room$Time == timelist_i,]
            SC_time=SelectMainFactor(CQ_Name_Room_Time$SC)
            Wind_time = table(na.omit(CQ_Name_Room_Time$Wind))
            Wind_time = sort(Wind_time, decreasing = T)
            #print(SC_time)
            if(is.null(SC_time)) return(NULL)
            windData_i = data.frame(Time = timelist_i, SC = SC_time, 北风 = 0, 东北风 = 0, 东风 = 0, 东南风 = 0,
                                    南风 = 0, 无持续风向 = 0, 西北风 = 0, 西风 = 0, 西南风 = 0)
            
            windData_i[1,(names(windData_i)) == names(Wind_time[1])]=Wind_time[1]
            windData_i[1,(names(windData_i)) == names(Wind_time[2])]=Wind_time[2]
            windData_i[1,(names(windData_i)) == names(Wind_time[3])]=Wind_time[3]
            err = try(data.frame(Time = timelist[k], SC = SC_time, Wind = Wind_time), silent = T)
            if('try-error' %in% class(err)) return(NULL)
            #windData = rbind(windData, windData_i)
            return(windData_i)
          })
        }
        windData = do.call('rbind',GetWind(timelist))
        # for(k in 1:length(timelist)){
        #   CQ_Name_Room_Time = CQ_Name_Room[CQ_Name_Room$Time == timelist[k],]
        #   SC_time=SelectMainFactor(CQ_Name_Room_Time$SC)
        #   Wind_time = table(na.omit(CQ_Name_Room_Time$Wind))
        #   Wind_time = sort(Wind_time, decreasing = T)
        #   #print(SC_time)
        #   if(is.null(SC_time)) next
        #   windData_i = data.frame(Time = timelist[k], SC = SC_time, 北风 = 0, 东北风 = 0, 东风 = 0, 东南风 = 0,
        #                           南风 = 0, 无持续风向 = 0, 西北风 = 0, 西风 = 0, 西南风 = 0)
        #   
        #   windData_i[1,(names(windData_i)) == names(Wind_time[1])]=Wind_time[1]
        #   windData_i[1,(names(windData_i)) == names(Wind_time[2])]=Wind_time[2]
        #   windData_i[1,(names(windData_i)) == names(Wind_time[3])]=Wind_time[3]
        #   err = try(data.frame(Time = timelist[k], SC = SC_time, Wind = Wind_time), silent = T)
        #   if('try-error' %in% class(err)) next
        #   windData = rbind(windData, windData_i)
        #   
        # }
        
        
        
        #Wind = rep(SelectMainFactor(CQ_Name_Room$Wind), length(RH))
        result = data.frame(Name = rep(Name_i, length(RH)), Room = rep(Room_i, length(RH)),Time = Time,
                            Tem = Tem, RH = RH, FA = FA, CO2 = CO2, TVOC = TVOC, PM = PM, PM_out = PM_out,
                            AQI_out = AQI_out, Tem_out = Tem_out, Hum_out = Hum_out)
        print(head(result$Time))
        result$Time = as.POSIXct(result$Time, format = '%Y-%m-%d')
        windData$SC = as.character(windData$SC)
        windData$Time = as.POSIXct(windData$Time, format = '%Y-%m-%d')
        #windData$Wind = as.character(windData$Wind)
        result$SC = windData[match(result$Time, windData$Time), 2]
        result$北风 = windData[match(result$Time, windData$Time), 3]
        result$东北风 = windData[match(result$Time, windData$Time), 4]
        result$东风 = windData[match(result$Time, windData$Time), 5]
        result$东南风 = windData[match(result$Time, windData$Time), 6]
        result$南风 = windData[match(result$Time, windData$Time), 7]
        result$无持续风向 = windData[match(result$Time, windData$Time), 8]
        result$西北风 = windData[match(result$Time, windData$Time), 9]
        result$西风 = windData[match(result$Time, windData$Time), 10]
        result$西南风 = windData[match(result$Time, windData$Time), 11]
        return(result)
      })
    }
    result = do.call('rbind', DayAvg2(Room))
    return(result)
  })
  #return(result)
}
