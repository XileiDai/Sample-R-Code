rm(list = ls())
library(rJava)
library(xlsxjars)
library(xlsx)
library(readxl)
library(data.table)
library(MASS)
Sys.setlocale("LC_ALL","Chinese") 
path_all=list.dirs(root_path)
data_log = data.frame(name = '',month = '', room = '', days = '')
#globalVariables(data_log)
main=function(ID){
  lapply(ID,function(ID_i){
    main2=function(path){
      lapply(path,function(path_i){
        setwd(path_i)
        name.list=list.files(pattern=c("*.xls"))
        if(length(name.list)>0){
          position=sapply(ID_i,grepl,name.list)
          if(sum(as.numeric(position))>0){
            print(ID_i)
            file.target_all=name.list[which(position==T)]
            if(TRUE){
              main4=function(file.target_all){
                lapply(file.target_all,function(file.target){
                  #print(ID_i)
                  name.sheet=excel_sheets(file.target)
                  sheet_position1=sapply('11',grepl,name.sheet)
                  sheet_position2=sapply('22',grepl,name.sheet)
                  sheet_position = sheet_position1 | sheet_position2
                  if(sum(as.numeric(sheet_position1))+sum(as.numeric(sheet_position2))>0){
                    sheet_position=which(sheet_position==T)
                    main3=function(sheet_position_fun){
                      lapply(sheet_position_fun,function(sheet_position_i){
                        oridata=read_excel(file.target,sheet=sheet_position_i,skip=0,col_names =T)
                        sheet_name = excel_sheets(file.target)
                        sheet_name = sheet_name[sheet_position_i]
                        #print(file.target)
                        #print(sheet_name)
                        if(sapply(c('ikair'),grepl,sheet_name)){
                          oridata=oridata[,1:16]
                          oridata = oridata[,c(1, 8, 10:16)]
                          #oridata = oridata[!is.na(oridata$time),]
                          if(length(na.omit(oridata$time))>2){
                            #print('dxl')
                            #print(file.target)
                            oridata$户名=rep(unique(na.omit(oridata$户名)),nrow(oridata))
                            

                            if(length(na.omit(oridata$设备名称))>0){
                              oridata$设备名称 = rep(unique(na.omit(oridata$设备名称)),nrow(oridata))
                            }
                            else{
                              oridata$设备名称=rep('卧室（新）',nrow(oridata))
                            }
                            days_posi = as.POSIXct(oridata$time, format = '%Y-%m-%d %H:%M:%S')
                            days = cut(days_posi, breaks = 'day')
                            month = unique(cut(days_posi, breaks = 'month'))
                            print(unique(na.omit(oridata$户名)),na.omit(month))
                            data_log_i = data.frame(name = unique(na.omit(oridata$户名)),
                                                    month = na.omit(month),
                                                    room = unique(na.omit(oridata$设备名称)), 
                                                    days = length(unique(days)))
                           
                            data_log <<-  rbind(data_log_i, data_log)
                            #print(data_log_i)
                            #print(info)
                            #write.csv2(t(info), file = 'Transition file.csv', append = TRUE)
                            return(oridata)
                          }
                          else{#print('no data in file')
                            return(NULL)}
                        } else if(sapply(c('111'),grepl,sheet_name)){
                          #print(file.target)
                          #print(head(oridata))
                          if(ncol(oridata)==16){
                            oridata$CO2=NA
                            print('CO2未录入!!!!!!')}
                          oridata=oridata[,1:17]
                          names(oridata)=c('户名','气候区','省','市','区','地址','类型','设备名称','Mac',
                                           'time','Tem','RH','PM2.5','PM10','PM100','TVOC','CO2')
                          oridata = data.frame(户名= oridata$户名, 设备名称=oridata$设备名称, time = oridata$time,
                                                 Tem = oridata$Tem, RH = rep(NA,nrow(oridata)), FA = rep(NA,nrow(oridata)),
                                                 CO2 = oridata$CO2, TVOC = oridata$TVOC, PM = oridata$PM2.5)
                          #oridata = oridata[!is.na(oridata$time),]
                          if(length(na.omit(oridata$time))>2){
                           # print('dxl')
                           # print(file.target)
                            oridata$户名=rep(unique(na.omit(oridata$户名)),nrow(oridata))
                            if(length(na.omit(oridata$设备名称))>0){
                              oridata$设备名称 = rep(unique(na.omit(oridata$设备名称)),nrow(oridata))
                            }
                            else{
                              oridata$设备名称=rep('bed',nrow(oridata))
                            }
                            days_posi = as.POSIXct(oridata$time, format = '%Y-%m-%d %H:%M:%S')
                            days = cut(days_posi, breaks = 'day')
                            month = unique(cut(days_posi, breaks = 'month'))
                            data_log_i = data.frame(name = unique(na.omit(oridata$户名)),
                                                    month = na.omit(month),
                                                    room = unique(na.omit(oridata$设备名称)), 
                                                    days = length(unique(days)))
                            data_log <<-  rbind(data_log_i, data_log)
                            #print(info)
                            #write.csv2(t(info), file = 'Transition file.csv', append = TRUE)
                            return(oridata)
                          }
                          else{#print('no data in file')
                            return(NULL)}
                        }
                        
                      }
                      )
                    }
                    result=do.call('rbind',main3(sheet_position))
                    return(result)
                  }
                  else{
                   # print('no ikair sheet')
                    return(NULL)
                  }
                  
                })
              }
              
              result=do.call('rbind',main4(file.target_all))
              return(result)
            }
            else{
              #print('file cannot open')
              return(NULL)
            }
            
          }
          else{
            #print('No corresponding file')
            return(NULL)
          }
        }
        else{
          print('Folder with no file')
          return(NULL)
        }
      })
    }
    result=do.call('rbind',main2(path_all))
    result = data.table(result, key = c('Name','Room','Time'))
    result = result[!duplicated(result),]
    result = as.data.frame(result)
    return(result)
  })
  # return(result)
}

