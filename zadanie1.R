library(tidyverse)
library(rnoaa)

#install.packages(c("tidyverse","rnoaa"))

#station_data = ghcnd_stations() #Может занять несколько минут лучше выполнить один раз в месте с хорошим интернетом и сохранить результат

#write.csv(station_data,"station_data.csv")

station_data = read.csv("station_data.csv")

names(station_data)

#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
city = data.frame(id = "Bryansk", latitude = 53.2520900,  longitude = 34.3716700)



city_around = meteo_nearby_stations(lat_lon_df = city, station_data = station_data,
                                    limit = 11, var = c("PRCP", "TAVG"),
                                    year_min = 2010, year_max = 2015)

city_around[[1]][,1:3]

city_id = city_around[[1]][["id"]][1]

#tula_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Тулы, очевидно что первым элементом таблицы будет идентификатор метеостанции Тулы, его то мы и попытаемся получить
#tula_id = tula_around[["TULA"]][["id"]][1]
#Для получения всех данных с метеостанции, зная ее идентификатор, используйте след. команду
all_city_data = meteo_tidy_ghcnd(stationid = city_id,date_min = "2008-01-01",date_max = "2013-12-31")
head(all_city_data$date)
tail(all_city_data$date)

t_more_5<-(all_city_data$tavg>5)

#mm<-gsub('.{0,3}$', '', all_city_data$date)

#mm<-str_sub(string = all_city_data$date, start = 6, end = 7)

all_city_data2<-all_city_data[t_more_5,]

mm<-str_sub(string = all_city_data2$date, start = 6, end = 7)

all_city_data2$mm<-mm

#df1<-data.frame(mid=mm,t_more_5=t_more_5)

ss <- split.data.frame(all_city_data2,all_city_data2$mm)

tavg<-c()
for (i in 1:12) tavg<-c(tavg,sum(ss[i][[1]]$tavg))

#i<-2

tab1<-read.csv("tab1.csv")

s<-tavg

f<-tab1$afi+tab1$bfi*s

#я тут, посмотрим... скобка не закрыта!!! скобки подсвечиваются так цифр то нет все равно. как в случае с f

y<-10^6*(f*tab1$di*300/(1600*2.2*(100-25)))/10^7
y
            
            

#mm2<-gsub('.{0,3}', '', mm)

