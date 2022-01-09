TempConvert.default <-
function(x,metoda="ctok") {
  # Sprawdzanie poprawnoœci argumentów
  stopifnot(is.numeric(x))
  metoda <- match.arg(metoda, c("ctok", "ftoc","ctof","ktoc","ftok","ktof"))
  # Konwersja Celsius - Kelvin
  temp_K <- x + 273.15
  result <- list(Temperatura = paste(x,'°C'), Konwersja = paste(temp_K,'°K'),Temp=x,Konw=temp_K)
  class(result)<-append(class(result),'TempConvert')
  return(result)
}
