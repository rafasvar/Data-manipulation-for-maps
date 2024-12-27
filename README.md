# Data-manipulation-for-maps-in-r
Compute mean of a categorical to join with map data later

#with-catname-we-describe-the-categorical-variable
#with-varname-we-describe-the-discrete-variable-of-our-data-frame

MeanCat <- function(df,varname,catname){
  var <- which(colnames(df)== varname)
  cat <- which(colnames(df)== catname)
  numrowcol <- dim(df)
  #use tapply to compute the mean of each catvar 
  Mesos <- with (df, tapply(df[,var],df[,cat],mean))
  rowcat <- dimnames(Mesos)
  row.names(Mesos) <- 1:dim(Mesos)[1]
  Mesos <- as.data.frame(Mesos)
  Mesos <- cbind(rowcat,Mesos)
  if(class(catname)=="character" & class(varname) == "character" ){
    colnames(Mesos) <- c(catname,varname) 
    
  }else{
    print("varname and catname should be characters")
  }
  return(Mesos)
}
