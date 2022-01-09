plot.TempConvert <-
function(x,...){
    data<-data.frame(
    name=c(x$Temperatura,x$Konwersja),
    value=c(x$Temp,x$Konw)
  )
  barplot(height=data$value, names=data$name, col=rgb(0.2,0.4) )
}
