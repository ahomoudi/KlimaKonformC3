
#plot climate diagramms for "Plauen", "Gera-Leumnitz" und "Zeitz"
library(rdwd)
stations<-c("Plauen", "Gera-Leumnitz","Zeitz" )

for (istation in stations){

  rdwd::findID(istation, exactmatch=FALSE)

  # select a dataset (e.g. last year's daily climate data from Potsdam city):
  link <- selectDWD(istation, res="daily", var="", per="historical")

  # Actually download that dataset, returning the local storage file name:
  file <- dataDWD(link, read=FALSE)

  # Read the file from the zip folder:
  clim <- readDWD(file, varnames=TRUE)

  clim<- clim[c(1,4)]

  names(clim$daily_kl_historical_tageswerte_KL_05750_19520901_20211231_hist)
  names(clim$daily_water_equiv_historical_tageswerte_Wa_05750_19930129_20211227_hist)
  names(clim$daily_more_precip_historical_tageswerte_RR_05750_19520901_20211231_hist)
  clim_df<-dplyr::bind_rows(clim)

  names(clim_df)

  c("TMK.Lufttemperatur", "TXK.Lufttemperatur_Max", "TNK.Lufttemperatur_Min")
}
