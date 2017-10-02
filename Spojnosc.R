packages<- c("reshape2","ggplot2","Hmisc","plyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(reshape2)
library(ggplot2)
library(Hmisc)
library(plyr)


#Trzeba ustawić katalog roboczy, w którym masz te pliki SPSSowe
setwd("~/Desktop/Spojnosc")
library(Hmisc)
library(reshape2)
library(plyr)
library(ggplot2)

#załadowanie danych z czterech eksperymentów
badanie_1<-spss.get('obrazki - gra1.sav',use.value.labels = T, to.data.frame = T)
badanie_2<-spss.get('obrazki gra2.sav',use.value.labels = T, to.data.frame = T)
badanie_3<-spss.get('obrazki badanie 3.sav',use.value.labels = T, to.data.frame = T)
badanie_4<-spss.get('Badanie 4.sav',use.value.labels = T, to.data.frame = T)
#badanie_3<-spss.get('Badanie 3 - wyniki (6paź16).sav',use.value.labels=T,to.data.frame=T)
#badanie_2<-spss.get('Badanie 2 - wyniki (28wrz16).sav',use.value.labels = T, to.data.frame = T)
#badanie_1<-spss.get('Badanie 1 - wyniki (222wrz16).sav',use.value.labels=T,to.data.frame=T)

sd(as.numeric(as.character(badanie_3$year)))

table(badanie_3$edu4)

ggplot(badanie_1)+geom_histogram(aes(czas))
ggplot(badanie_2)+geom_histogram(aes(czas))
ggplot(badanie_3)+geom_histogram(aes(czas))

range(badanie_1$czas)
mean(badanie_1$czas)
median(badanie_1$czas)
range(badanie_2$czas)
mean(badanie_2$czas)
median(badanie_2$czas)
range(badanie_3$czas)
mean(badanie_3$czas)
median(badanie_3$czas)
table(badanie_1$czas)
109/60
table(badanie_1$czas>60)
table(badanie_2$czas>45)
table(badanie_3$czas>360)

120/1069
102/1111
102/1088
#5% dolne usunięcie
0.05*1088
table(badanie_1$czas>52)

badanie_1<-badanie_1[badanie_1$czas>60,]
badanie_2<-badanie_2[badanie_2$czas>45,]
badanie_3<-badanie_3[badanie_3$czas>360,]
badanie_2<-badanie_2[(badanie_2[,14]=="Tak"),]
badanie_3<-badanie_3[(badanie_3[,14]=="Tak"),]
badanie_4<-badanie_4[badanie_4[,14]=="Tak",]

#przygotowanie danych
colnames(badanie_1)[14]<-"blok"
badanie_2<-badanie_2[,c(-2,-14)]
badanie_3<-badanie_3[,c(-2,-14)]
badanie_4<-badanie_4[,c(-2,-14)]

#usunięcie braków danych
split_and_remove<-function(table){
  columns_with_na<-apply(table,2,function(x){
    return(sum(is.na(x)))
  })>(dim(table)[1]-1)
  wynik<-table[,!columns_with_na]
  column_names<-grep("q6",colnames(wynik))
  colnames(wynik)[column_names]<-c(1:length(column_names))
  return(wynik)
}

badanie_1_dane<-ddply(badanie_1,.(blok),split_and_remove)
badanie_2_dane<-ddply(badanie_2,.(blok),split_and_remove)
badanie_3_dane<-ddply(badanie_3,.(blok),split_and_remove)
badanie_4_dane<-ddply(badanie_4,.(blok),split_and_remove)

#zamiana na long. To znaczy na taki format gdzie pomiar jest rekordem.
help_table_3<-matrix(c(15:50),nrow = 4,ncol = 9,byrow=F)
help_table_3<-as.list(as.data.frame(t(help_table_3)))

help_table<-matrix(c(15:176),nrow=18,ncol=9,byrow=F)
help_table<-as.list(as.data.frame(t(help_table)))

badanie_1_long<-reshape(badanie_1_dane,as.character(c(1:9)),"gra_zauf",direction="long")
badanie_2_long<-reshape(badanie_2_dane,as.character(c(1:9)),"zaufanie",direction="long")
badanie_3_long<-reshape(badanie_3_dane,idvar="ID",varying = help_table_3,v.names=c("pyt_1","pyt_2","pyt_3","pyt_4"),direction="long")
badanie_4_long<-reshape(badanie_4_dane,idvar="ID",varying = help_table,v.names=as.character(c(1:18)),direction="long")


#Plik 1 - B1, PLik 2 - A2, Plik 3 - A1, Plik 4 - B2
#Tutaj są warunki w poszczególnych blokach zapisane oraz nazwy bloków
A1_list<-list(c("A","B","C","D","E","F","G","H","I"),c("S","N","N","S","S","N","S","N","N"))
A2_list<-list(c("J","K","L","M","N","O","P","R","E"),c("S","N","S","S","N","N","N","S","N"))
B1_list<-list(c("H","G","C","M","F","E","B","A","J"),c("S","N","S","N","S","N","S","N","N"))
B2_list<-list(c("K","R","O","L","D","N","P","E","I"),c("S","N","S","N","N","S","S","N","S"))
warunek<-c("zestaw #A1","zestaw #A2","zestaw #B1","zestaw #B2")

#to jest funkcja, która sprawdza, który jest blok, które zdjęcie i tworzy kolumny z osobą oraz spójnością.
warunek_fun<-function(table) {
  table<-ddply(table,.(blok),function(table){
    table<-ddply(table,.(time),function(table){
      warunek_nr<-grep(table$blok[1],warunek)
      picture_nr<-table$time
      vector_of_lists<-c("A1_list","A2_list","B1_list","B2_list")
      table$osoba<-get(vector_of_lists[warunek_nr])[[1]][picture_nr]
      table$spojnosc<-get(vector_of_lists[warunek_nr])[[2]][picture_nr]
      return(table)
    })
  })
}

badanie_1_anova<-warunek_fun(badanie_1_long)
badanie_2_anova<-warunek_fun(badanie_2_long)
badanie_3_anova<-warunek_fun(badanie_3_long)
badanie_4_anova<-warunek_fun(badanie_4_long)

#to zamienia zmienną zależną, która nie jest numeric na numeric.
zamiana_numeric<-function(vector){
  if (grepl("1",vector)){
    vector<-1
  }
  if (grepl("7",vector)){
    vector<-7
  }
  vector<-as.numeric(vector)
  return(vector)
}

badanie_3_anova[,(29:32)]<-apply(badanie_3_anova[,(29:32)],c(1,2),zamiana_numeric)
badanie_2_anova[,(28:29)]<-apply(badanie_2_anova[,(28:29)],c(1,2),zamiana_numeric)
badanie_4_anova[,(28:45)]<-apply(badanie_4_anova[,(28:45)],c(1,2),zamiana_numeric)

#zapisanie do plików
write.csv(badanie_1_anova,"Badanie_1_MM.csv")
write.csv(badanie_3_anova,"Badanie_3_MM.csv")
write.csv(badanie_2_anova,"Badanie_2_MM.csv")
write.csv(badanie_4_anova,"Badanie_4_MM.csv")

Badanie_1_MM <- read.csv("~/Desktop/Spojnosc/Badanie_1_MM.csv")
Badanie_2_MM <- read.csv("~/Desktop/Spojnosc/Badanie_2_MM.csv")
Badanie_3_MM <- read.csv("~/Desktop/Spojnosc/Badanie_3_MM.csv")

library(plyr)

#policzenie wyników oceny koherencji dla każdej osoby
Badanie_3_MM$skala<-apply(Badanie_3_MM[,29:32],1,mean)
#policzenie średnich oceny koherencji dla zdjęć
dane_mean<-aggregate(Badanie_3_MM$skala, by=list(Badanie_3_MM$osoba,Badanie_3_MM$spojnosc),mean)
#dodanie kolumny, w której zapisana jest ocena spójności każdego zdjęcia dla Badania_1
Badanie_1_MM<-ddply(Badanie_1_MM,.(osoba),function(table){
  ddply(table,.(spojnosc),function(table){
    a<-as.character(table$osoba[1])
    b<-as.character(table$spojnosc[1])
    table$mean_spojnosc<-dane_mean$x[(dane_mean$Group.1==a)+(dane_mean$Group.2==b)>1]
    return(table)
  })
})
#dodanie kolumny, w której zapisana jest ocena spójności każdego zdjęcia dla Badania_2
Badanie_2_MM<-ddply(Badanie_2_MM,.(osoba),function(table){
  ddply(table,.(spojnosc),function(table){
    a<-as.character(table$osoba[1])
    b<-as.character(table$spojnosc[1])
    table$mean_spojnosc<-dane_mean$x[(dane_mean$Group.1==a)+(dane_mean$Group.2==b)>1]
    return(table)
  })
})

write.csv(Badanie_1_MM,"Badanie_1_MM.csv")
write.csv(Badanie_2_MM,"Badanie_2_MM.csv")
write.csv(Badanie_3_MM,"Badanie_3_MM.csv")
