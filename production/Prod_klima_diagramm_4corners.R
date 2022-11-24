
library(KlimaKonformC3)

dirs<-list.dirs(path = "/media/ahmed/Daten/WHK2/Data/climate", full.names = T, recursive = F)
dirs<-list.dirs(path = "D:/AHomoudi/KlimaKonform/5ter_Lauf_2022-10-28", full.names = T, recursive = F)
idir =1
for(idir in 1:length(dirs)){

  csv.files<-list.files(path = dirs[idir],
                        full.names = T,
                        recursive = T,
                        pattern = ".csv$")

  klima_df<- csv.files%>%
    lapply(readr::read_csv, skip=2)

  #167 rows & 133 columns
  names(klima_df)<-unlist(lapply(csv.files, function(x){

    x<-unlist(stringr::str_split(string = x, "/"))

    x.row<-x[c(length(x)-1)]

    x.col<-unlist(stringr::str_split(string = x[length(x)],
                                 "[.]"))[1]%>%
      readr::parse_number()

    return(paste0(x.row,"_",x.col))

  }))

  klima_df<-dplyr::bind_rows(klima_df, .id = "id")

  colnames(klima_df)<-unlist(lapply(colnames(klima_df), function(x){

    x<-unlist(stringr::str_split(string = x, ":"))


    return( x[length(x)])

  }))


  corners<- data.frame(corners=c("LO", #0,0
             "LM", #83,0
             "LU", #166,0
             "MO", #0,67
             "MM", #83,67
             "MU", #166,67
             "RO", #0,133
             "RM"), #83,133
             id = c("0_0",
                    "83_0",
                    "166_0",
                    "0_-67",
                    "83_-67",
                    "166_-67",
                    "0_-133",
                    "83_-133"
                    ))


  klima_df<-klima_df%>%dplyr::left_join(y = corners, by = "id")

  rm(corners)

  klima_df$Date<-as.Date(klima_df$Date)

  run_id<-stringr::str_sub(dirs[idir], -6)

  klima_df<-klima_df%>%
    dplyr::select(c("Date", "Tmin", "Tavg", "Tmax", "Precip", "corners"))

  lines<-c("LO", #0,0
           "LM", #83,0
           "LU", #166,0
           "MO", #0,67
           "MM", #83,67
           "MU", #166,67
           "RO", #0,133
           "RM")

  corners<-unique(klima_df$corners)

  for( icorner in 1:length(corners)){

    #icorner<-1
    sub_df<-klima_df%>%dplyr::filter(corners==corners[icorner])

    # calculate monthly mean values of Precip, Tmin, Tavg, and Tmean for each month


    data_mon<-sub_df%>%
      dplyr::mutate(yearmonth = zoo::as.yearmon(Date)) %>%
      dplyr::group_by(yearmonth)%>%
      dplyr::select(-Date)%>%
      dplyr::summarise(Precip = sum(Precip, na.rm = T),
                       Tmin = mean(Tmin, na.rm = T),
                       Tavg= mean(Tavg, na.rm = T),
                       Tmax = mean(Tmax, na.rm = T))

    # get periods
    r.time<-zoo::as.Date(data_mon$yearmonth)

    time_indices<-  periods <- list(
      period1 = which(r.time > as.Date("1970-12-31") &
                        r.time < as.Date("2000-02-01")),
      period2 = which(r.time > as.Date("1990-12-31") &
                        r.time < as.Date("2020-02-01")),
      period3 = which(r.time > as.Date("2020-12-31") &
                        r.time < as.Date("2050-02-01")),
      period4 = which(r.time > as.Date("2069-12-31") &
                        r.time < as.Date("2099-02-01"))
    )

    #assign periods
    data_mon$Period<-NA
    data_mon$Period[time_indices$period1]<-"1971-2000"
    data_mon$Period[time_indices$period2]<-"1991-2020"
    data_mon$Period[time_indices$period3]<-"2021-2050"
    data_mon$Period[time_indices$period4]<-"2070-2099"


    # calculate periods mean
    data_seasonal<-data_mon%>%
      dplyr::mutate(Date = zoo::as.Date(yearmonth))%>%
      dplyr::mutate(MONTH = lubridate::month(Date))%>%
      dplyr::group_by(Period, MONTH)%>%
      dplyr::select(-yearmonth, -Date)%>%
      dplyr::summarise(Precip = mean(Precip, na.rm = T),
                       Tmin = mean(Tmin, na.rm = T),
                       Tavg= mean(Tavg, na.rm = T),
                       Tmax = mean(Tmax, na.rm = T))

    # clean the data
    data_seasonal<-na.omit(data_seasonal)


    # get long term means
    data_longterm<-sub_df%>%
      dplyr::mutate(year = lubridate::year(Date)) %>%
      dplyr::group_by(year)%>%
      dplyr::select(-Date)%>%
      dplyr::summarise(Precip = sum(Precip, na.rm = T),
                       Tmin = mean(Tmin, na.rm = T),
                       Tavg= mean(Tavg, na.rm = T),
                       Tmax = mean(Tmax, na.rm = T))


    # get periods
    r.time<-data_longterm$year

    time_indices<-  periods <- list(
      period1 = which(r.time > 1970 &
                        r.time < 2001),#Referenz
      period2 = which(r.time > 1990 &
                        r.time < 2021),
      period3 = which(r.time > 2020 &
                        r.time < 2051),
      period4 = which(r.time > 2069&
                        r.time < 2100)
    )

    #assign periods
    data_longterm$Period<-NA
    data_longterm$Period[time_indices$period1]<-"1971-2000"
    data_longterm$Period[time_indices$period2]<-"1991-2020"
    data_longterm$Period[time_indices$period3]<-"2021-2050"
    data_longterm$Period[time_indices$period4]<-"2070-2099"

    data_longterm<-data_longterm%>%
      dplyr::group_by(Period)%>%
      dplyr::select(-year)%>%
      dplyr::summarise(Precip = mean(Precip, na.rm = T),
                       Tmin = mean(Tmin, na.rm = T),
                       Tavg= mean(Tavg, na.rm = T),
                       Tmax = mean(Tmax, na.rm = T))



    # clean the data
    data_longterm<-na.omit(data_longterm)



    # get P1
    clima_diagramm_abs(data = data_seasonal%>% dplyr::filter(Period =="1971-2000"),
                       temp_precip_mean = data_longterm[1,c(4,2)],
                       run_id = run_id)


    # get P2

    # get P3

    # get P4





    ggplot2::ggplot(data = data_seasonal,
                    mapping = ggplot2::aes(x = MONTH))+

      ggplot2::geom_col(mapping = ggplot2::aes(
        y = Precip))+

      ggplot2::geom_line(mapping = ggplot2::aes(
        y = Tmin))+

      ggplot2::facet_grid(Period~corners)+

      ggplot2::scale_y_continuous(breaks = seq(-10,50,5),
                                  sec.axis = ggplot2::sec_axis(~.*100))

    # calculate statistics

    clima_plot_cornors(data=,
                       run_id = run_id,
                       output_path = paste0(dirs[idir],"/"))
  }


}
