print.TempConvert <-
function(x,...)
{
  cat("Temperatura wejœciowa\n")
  print(x$Temperatura)
  cat("\nTemperatura po konwersji:\n")
  print(x$Konwersja)
}
