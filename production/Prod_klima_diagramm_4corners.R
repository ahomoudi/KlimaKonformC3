
library(KlimaKonformC3)

dirs<-list.dirs(path = "/media/ahmed/Daten/WHK2/Data/climate", full.names = T, recursive = F)

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

  clima_plot_cornors(data=klima_df%>%dplyr::filter(corners=="MO"|corners=="MM"|corners=="MU"),
                     run_id = run_id,
                     output_path = paste0(dirs[idir],"/"))

}
