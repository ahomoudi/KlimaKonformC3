
#plot climate diagramms for "Plauen", "Gera-Leumnitz" und "Zeitz"
library(rdwd)
library(KlimaKonformC3)

stations<-c("Plauen", "Gera-Leumnitz","Zeitz" )
setwd("D:/AHomoudi/NextCloud/Shared/KlimaKonform-Results/Klima_gitter/")

for (istation in stations){

  rdwd::findID(istation, exactmatch=FALSE)

  # select a dataset (e.g. last year's daily climate data from Potsdam city):
  link <- selectDWD(istation, res="daily", var="", per="historical")

  # Actually download that dataset, returning the local storage file name:
  file <- dataDWD(link, read=FALSE)

  # Read the file from the zip folder:
  clim <- readDWD(file, varnames=TRUE)

  clim<- clim[[1]]

  clim<-clim[c("STATIONS_ID", "MESS_DATUM", "RSK.Niederschlagshoehe", "TMK.Lufttemperatur",
         "TXK.Lufttemperatur_Max", "TNK.Lufttemperatur_Min")]

  clim$MESS_DATUM<-as.Date(clim$MESS_DATUM)



}
