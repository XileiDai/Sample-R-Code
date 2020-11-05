rm(list = ls())
library(rJava)
library(xlsxjars)
library(xlsx)
library(readxl)
library(data.table)
library(MASS)
Sys.setlocale("LC_ALL","Chinese") 
main=function(ID){
  lapply(ID,function(ID_i){
    main2=function(path){
      lapply(path,function(path_i){
        setwd(path_i)
        #print(path_i)
        name.list=list.files(pattern=c("*.xlsx"))
        if(length(name.list)>0){
          position=sapply(ID_i,grepl,name.list)
          if(sum(as.numeric(position))>0){
            file.target_all=name.list[which(position==T)]
            if(TRUE){
              main4=function(file.target_all){
                lapply(file.target_all,function(file.target){
                  #print(file.target)
                  fit = try(excel_sheets(file.target),silent = T)
                  if('try-error' %in% class(fit)){
                    #print(class(fit))
                    #filecannotopen <<-  append(filecannotopen, file.target)
                    return(NULL)
                  } 
                  name.sheet=excel_sheets(file.target)
                  sheet_position=sapply('窗',grepl,name.sheet)
                  sheet_position2=sapply('窗',grepl,name.sheet)
                  if(sum(as.numeric(sheet_position | sheet_position2))>0){
                    sheet_position=which(sheet_position==T)
                    main3=function(sheet_position_fun){
                      lapply(sheet_position_fun,function(sheet_position_i){
                        fit = try(read_excel(file.target,sheet=sheet_position_i,skip=0,col_names =T),silent = T)
                        #print(class(fit))
                        if('try-error' %in% class(fit)){
                          #print(class(fit))
                          return(NULL)
                        } else{
                          oridata=read_excel(file.target,sheet=sheet_position_i,skip=0,col_names =T)
                          #print(file.target)
                          AllFile_i = data.frame(fileName = file.target)
                          AllFile <<- rbind(AllFile, AllFile_i)
                          sheet_name = excel_sheets(file.target)

                          if(ncol(oridata) >= 13){
                            fit = try(oridata[,1:13],silent = T)
                            if('try-error' %in% class(fit)){
                              #print(class(fit))
                              filecannotopen <<-  append(filecannotopen, c(file.target,sheet_name[sheet_position_i]))
                              return(NULL)
                            }
                            oridata=oridata[,1:13]

                            if(is.na(oridata$房间)){
                              if(sapply('卧', grepl, sheet_name[sheet_position_i])){
                                oridata$房间 = rep('卧室',nrow(oridata))
                              } else if(sapply('客', grepl, sheet_name[sheet_position_i])){
                                oridata$房间 = rep('客厅',nrow(oridata))
                              } else {
                                oridata$房间 = rep('Unknown',nrow(oridata))
                              }
                            }
                            oridata = oridata[,c(1, 8, 9, 12:13)]
                            #oridata = oridata[!is.na(oridata$time),] 
                          } else {
                            fit = try(oridata[,1:4],silent = T)
                            if('try-error' %in% class(fit)){
                              #print(class(fit))
                              filecannotopen <<-  append(filecannotopen, c(file.target,sheet_name[sheet_position_i]))
                              return(NULL)
                            }
                            namelist = names(oridata)
                            #print(namelist)
                            if(namelist[3] == '时间'){
                              oridata=oridata[,1:4]
                              names(oridata)=c('户名','设备名','Time','Status')
                              oridata$房间 = NA
                              oridata = oridata[, c(1,2,5,3:4)]
                            } else if(namelist[3] == '位置'){
                              oridata=oridata[,1:5]
                              names(oridata)=c('户名','设备名','房间','Time','Status')
                            } else {print('未知格式')}

                            if(is.na(oridata$房间)){
                              if(sapply('卧', grepl, sheet_name[sheet_position_i])){
                                oridata$房间 = rep('卧室',nrow(oridata))
                              } else if(sapply('客', grepl, sheet_name[sheet_position_i])){
                                oridata$房间 = rep('客厅',nrow(oridata))
                              } else {
                                oridata$房间 = rep('Unknown',nrow(oridata))
                              }
                            }
                          }
                          if(length(na.omit(oridata$Time))>0){
                            #print('dxl')
                            #print(file.target)
                            oridata$户名=rep(unique(na.omit(oridata$户名)),nrow(oridata))
                            if(length(na.omit(oridata$设备名))>0){
                              oridata$设备名 = rep(unique(na.omit(oridata$设备名)),nrow(oridata))
                            }else{
                              oridata$设备名=rep('Unknown',nrow(oridata))
                            }
                            if(length(na.omit(oridata$房间))>0){
                              oridata$房间 = rep(unique(na.omit(oridata$房间)),nrow(oridata))
                            }else{
                              oridata$房间=rep('Unknown',nrow(oridata))
                            }
                            days_posi = as.POSIXct(oridata$Time, format = '%Y-%m-%d %H:%M:%S')
                            days = cut(days_posi, breaks = 'day')
                            month = unique(cut(days_posi, breaks = 'month'))
                            #print(unique(na.omit(oridata$户名)))
                            #print(na.omit(month))
                            data_log_i = data.frame(name = unique(na.omit(oridata$户名)),
                                                    month = na.omit(month),
                                                    room = unique(na.omit(oridata$房间)), 
                                                    type = unique(na.omit(oridata$设备名)),
                                                    sheetName = name.sheet[sheet_position_i],
                                                    days = length(unique(days)))
                            if(data_log_i$room == 'Unknown'){
                              if(sapply('卧', grepl, as.character(data_log_i$type))){
                                data_log_i$room = '卧室'
                              } else if(sapply('客', grepl, as.character(data_log_i$type))){
                                data_log_i$room = '客厅'
                              } else {
                                data_log_i$room = 'dxl'
                              }
                            }
                            data_log <<-  rbind(data_log_i, data_log)
                            #print(info)
                            #write.csv2(t(info), file = 'Transition file.csv', append = TRUE)
                            return(oridata)
                          }
                          else{
                            #print('no data in file')
                            oridata$户名=rep(unique(na.omit(oridata$户名)),1)
                            if(is.na(oridata$房间)){
                              if(sapply('卧', grepl, sheet_name[sheet_position_i])){
                                oridata$房间 = rep('卧室',nrow(oridata))
                              } else if(sapply('客', grepl, sheet_name[sheet_position_i])){
                                oridata$房间 = rep('客厅',nrow(oridata))
                              } else {
                                oridata$房间 = rep('Unknown',nrow(oridata))
                              }
                            }
                            if(length(na.omit(oridata$设备名))>0){
                              oridata$设备名 = rep(unique(na.omit(oridata$设备名)),1)
                            }else{
                              oridata$设备名=rep('Unknown',1)
                            }
                            if(length(na.omit(oridata$房间))>0){
                              oridata$房间 = rep(unique(na.omit(oridata$房间)),1)
                            }else{
                              oridata$房间=rep('Unknown',1)
                            }

                            data_log_i = data.frame(name = unique(na.omit(oridata$户名)),
                                                    month = 'dxl',
                                                    room = unique(na.omit(oridata$房间)), 
                                                    type = unique(na.omit(oridata$设备名)),
                                                    sheetName = name.sheet[sheet_position_i],
                                                    days = 0)
                            if(data_log_i$room == 'Unknown'){
                              if(sapply('卧', grepl, as.character(data_log_i$type))){
                                data_log_i$room = '卧室'
                              } else if(sapply('客', grepl, as.character(data_log_i$type))){
                                data_log_i$room = '客厅'
                              } else {
                                data_log_i$room = 'dxl'
                              }
                            }
                            data_log <<-  rbind(data_log_i, data_log)
                            return(oridata)}
                        }
                      })
                    }
                    result=do.call('rbind',main3(sheet_position))
                    return(result)
                  }
                  else{
                    #print('no ikair sheet')
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
          #print('Folder with no file')
          return(NULL)
        }
      })
    }
    result=do.call('rbind',main2(path_all))
    return(result)
  })
  # return(result)
}
result=do.call('rbind',main(ID_all))


months = unique(data_log$month)
months = as.character(months)
ncol = length(months)
names = unique(data_log$name)
types = unique(data_log$type)
log_summary = as.data.frame(matrix(NA,ncol=ncol +4))
names(log_summary) = c('name','room','type', 'sheetName', months)
log_summary_total = data.frame()
k = 1
for(i in 1:length(names)){
  room_i = unique(data_log$room[data_log$name==names[i]])
  for (j in 1:length(room_i)){
    type_i = unique(data_log$type[data_log$name==names[i]&data_log$room==room_i[j]])
    for (q in 1:length(type_i)){
      sheetName_i = unique(data_log$sheetName[data_log$name==names[i]&data_log$room==room_i[j] & data_log$type == type_i[q]])
      for(p in 1:length(sheetName_i)){
        log_summary[k,1]=as.character(names[i])
        log_summary[k,2]=as.character(room_i[j])
        log_summary[k,3]=as.character(type_i[q])
        log_summary[k,4]=as.character(sheetName_i[p])
        k= k+ 1
      }
    }
  }
}

log_summary = log_summary[log_summary$name!='',]
for(i in 1:nrow(data_log)){
  name = data_log$name[i]
  room = data_log$room[i]
  type = data_log$type[i]
  sheetName = data_log$sheetName[i]
  #if(room!='卧室'|room!='客厅'){room='卧室（新）'}
  month = data_log$month[i]
  name = as.character(name)
  room = as.character(room)
  type = as.character(type)
  sheetName = as.character(sheetName)
  index_month = which(as.character(month)==names(log_summary))
  days = data_log$days[i]
  log_summary[log_summary$name==name & log_summary$room==room &
                log_summary$type == type & log_summary$sheetName==sheetName, index_month]=days
}

setwd(root_path)
log_summary = log_summary[order(log_summary$name),]
log_summary = log_summary[, which(names(log_summary)!='dxl')]
write.csv(log_summary[,1:ncol(log_summary)-1], file = '数据总结.csv', row.names = FALSE)
ErrorFile = data.frame(file = NA, sheet = NA)
for(i in 1:(length(filecannotopen)/2)){
  ErrorFile_i = data.frame(file = as.character(filecannotopen[2*i-1][1]),
                           sheet = as.character(filecannotopen[(i)*2][1]))
  ErrorFile = rbind(ErrorFile, ErrorFile_i)
}

write.csv(ErrorFile, file = 'unopenable file.csv', row.names = FALSE)
View(log_summary)

AllFile$exist = NA
FileName = AllFile$fileName
for(i in 1:nrow(AllId)){
  index = sapply(AllId$Name[i],grepl,FileName)
  index = as.numeric(sum(index))
  AllId$exist[i] = index
}
AllId = AllId[AllId$exist<1,]
write.csv(AllId,file = '缺文件.csv')