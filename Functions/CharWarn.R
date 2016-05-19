CharWarn <- function (df) {
  warn<-sapply(df,class)
  for (i in 1:length(warn)) 
  {
    `%ni%` <-Negate(`%in%`)
    if (warn[i] %ni% c('integer','numeric')) {print (paste(names(warn)[warn[i]]," is not numeric"))}
  }

}
