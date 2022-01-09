TempConvert <-
function(x,metoda,...){
  # Sprawdzanie poprawno?ci argument?w
  stopifnot(is.numeric(x))
  metoda <- match.arg(metoda, c("ctok", "ftoc","ctof","ktoc","ftok","ktof"))
  switch (metoda,
    ctok={
      # Konwersja Celsius - Kelvin
      temp_K <- x + 273.15
      result <- list(Temperatura = paste(x,'?C'), Konwersja = paste(temp_K,'?K'),Temp=x,Konw=temp_K)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },
    ftoc={
      # Konwersja Fahrenheit-Celsius
      temp_C <- (x - 32) * 5 / 9
      result <- list(Temperatura = paste(x,'?F'), Konwersja = paste(temp_C,'?C'),Temp=x,Konw=temp_C)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },
    ctof={
      # Konwersja Celsius-Fahrenheit
      temp_F <- 32+(9/5*x)
      result <- list(Temperatura = paste(x,'?C'), Konwersja = paste(temp_F,'?F'),Temp=x,Konw=temp_F)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },
    ktoc={
      # Konwersja Kelvin - Celsius
      temp_C <- x - 273.15
      result <- list(Temperatura = paste(x,'?K'), Konwersja = paste(temp_C,'?C'),Temp=x,Konw=temp_C)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },
    ftok={
      # Konwersja Fahrenheit - Kelvin
      temp_K <- (x+459.67)*5/9
      result <- list(Temperatura = paste(x,'?F'), Konwersja = paste(temp_K,'?K'),Temp=x,Konw=temp_K)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    },
    ktof={
      # Konwersja  Kelvin-Fahrenheit
      temp_F <- (x*1.8)-459.67
      result <- list(Temperatura = paste(x,'?K'), Konwersja = paste(temp_F,'?F'),Temp=x,Konw=temp_F)
      class(result)<-append(class(result),'TempConvert')
      return(result)
    }
  )

}
