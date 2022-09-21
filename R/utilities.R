

# mean of SpatRaster
f1_mean<-function(x){

  y<- terra::as.array(x) %>%
    apply(MARGIN = c(3), FUN = mean, na.rm =TRUE)

  return(y)
}



# max of spatRaster in vector forum
f1_max<-function(x){

  y<- terra::as.array(x)%>%
    apply(MARGIN = c(3), FUN = max, na.rm =TRUE)

  return(y)
}

# min
f1_min<-function(x){

  y<- terra::as.array(x)%>%
    apply(MARGIN = c(3), FUN = min, na.rm =TRUE)

  return(y)
}
