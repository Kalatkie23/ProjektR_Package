#' @title Projekt konwertujacy temperatury
#' @author Bartlomiej Kalata
#' @description Funkcja zwraca przekonwertowane temperatury wykorzystujac klasyczne przeliczniki. Skala Celsjusza, Farenheita, Kelwina.
#' @param x numeryczna wartosc
#' @param metoda wartosc  wartosc typu: "ctok", "ftoc","ctof","ktoc","ftok","ktof"
#' @param ... inne argumenty
#' @return Funkcja zwaraca w wyniku przekonwertowana temperature jako wartosc numeryczna
#' @rdname TempConvert
#' @export
TempConvert <- function(x, metoda, ...) UseMethod("TempConvert")



#' @title TempConvert
#' @author Bartlomiej Kalata
#' @description Funkcja zwraca przekonwertowane temperatury wykorzystujac klasyczne przeliczniki. Skala Celsjusza, Farenheita, Kelwina.
#' @param x numeryczna wartosc
#' @param metoda wartosc  wartosc typu: "ctok", "ftoc","ctof","ktoc","ftok","ktof"
#' @param ... inne argumenty
#' @return Funkcja zwaraca w wyniku przekonwertowana temperature jako wartosc numeryczna
#' @rdname TempConvert.default
#' @export
TempConvert.default<- function(x,metoda="ctok",...){
  # Sprawdzanie poprawnosci argumentow
  stopifnot(is.numeric(x))
  metoda <- match.arg(metoda, c("ctok", "ftoc","ctof","ktoc","ftok","ktof"))
  switch (metoda,
    ctok={
      # Konwersja Celsius - Kelvin
      temp_K <- x + 273.15
      result <- list(Temperatura = paste(x,'\u00B0 C'), Konwersja = paste(temp_K,'\u00B0 K'),Temp=x,Konw=temp_K)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },
    ftoc={
      # Konwersja Fahrenheit-Celsius
      temp_C <- (x - 32) * 5 / 9
      result <- list(Temperatura = paste(x,'\u00B0 F'), Konwersja = paste(temp_C,'\u00B0 C'),Temp=x,Konw=temp_C)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },
    ctof={
      # Konwersja Celsius-Fahrenheit
      temp_F <- 32+(9/5*x)
      result <- list(Temperatura = paste(x,'\u00B0 C'), Konwersja = paste(temp_F,'\u00B0 F'),Temp=x,Konw=temp_F)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },
    ktoc={
      # Konwersja Kelvin - Celsius
      temp_C <- x - 273.15
      result <- list(Temperatura = paste(x,'\u00B0 K'), Konwersja = paste(temp_C,'\u00B0 C'),Temp=x,Konw=temp_C)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },
    ftok={
      # Konwersja Fahrenheit - Kelvin
      temp_K <- (x+459.67)*5/9
      result <- list(Temperatura = paste(x,'\u00B0 F'), Konwersja = paste(temp_K,'\u00B0 K'),Temp=x,Konw=temp_K)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },
    ktof={
      # Konwersja  Kelvin-Fahrenheit
      temp_F <- (x*1.8)-459.67
      result <- list(Temperatura = paste(x,'\u00B0 K'), Konwersja = paste(temp_F,'\u00B0 F'),Temp=x,Konw=temp_F)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },

  )

}



#' @title Przyrosty Print
#' @author Bartlomiej Kalata
#' @description Funkcja wypisuje wprowadzone i przekonwertowane temperatury
#' @param x temperatura
#' @param metoda wartosc  wartosc typu: "ctok", "ftoc","ctof","ktoc","ftok","ktof"
#' @param ... inne argumenty
#' @rdname print.TempConvert
#' @export
print.TempConvert <- function(x,metoda,...)
{
  cat("Temperatura wejsciowa\n")
  print(x$Temperatura)
  cat("\nTemperatura po konwersji:\n")
  print(x$Konwersja)
}




#' @title plot.TempConvert
#' @author Bartlomiej Kalata
#' @description Funkcja zwraca wykres w postaci Bar Plotu dla temperatur
#' @param x temperatura
#' @param metoda wartosc  wartosc typu: "ctok", "ftoc","ctof","ktoc","ftok","ktof"
#' @param ... inne argumenty
#' @rdname plot.TempConvert
#' @export
plot.TempConvert<-function(x,metoda,...){
    data<-data.frame(
    Temperatura=c(x$Temperatura,x$Konwersja),
    Wartosc=c(x$Temp,x$Konw)
  )
  #barplot(height=data$value, names=data$name, col=rgb(0.2,0.4,0.6))
  ggplot(data = data, mapping = aes(x=Temperatura, y=Wartosc)) +
      geom_bar(stat="identity", position = "dodge") +
      geom_text(aes(label = Temperatura), vjust = -0.2, size = 5,
                position = position_dodge(0.9)) +
      ylim(0, max(data$Wartosc)*1.1) +
    theme_classic() +
    scale_fill_brewer(palette = "Pastel1")
}


#' @title Konwersja Temperatury Summary
#' @author Bartlomiej Kalata
#' @description Funkcja zwraca podstawowe informacje o obiekcie zwracanym z funkcji
#' @param x  temperatura
#' @param metoda wartosc  wartosc typu: "ctok", "ftoc","ctof","ktoc","ftok","ktof"
#' @param ... inne argumenty
#' @rdname summary.TempConvert
#' @export
summary.TempConvert <- function(x,metoda, ...){
  cat("Struktura:\n")
  str(x)
  cat("Opis obiektu:\n")
  summary(x$Temp)
  summary(x$Konw)
}


