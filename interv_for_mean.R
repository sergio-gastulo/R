#Intervalo de Confianza para una muestra pequenha:

interv_for_mean <- function(vect,alpha){
  n = length(vect)
  Y = mean(vect)
  var_muestr = 1/(n-1) * sum((vect-Y)**2)
  if (n<30){
  quantile = qt(alpha/2, 
                df = n-1, 
                lower.tail = FALSE)
  value = quantile*sqrt(var_muestr)/sqrt(n)
  }
  else{
  quantile = qnorm(alpha/2, 
                mean = mean,
                sd = sd(vect),
                lower.tail = FALSE)
  value = quantile*sqrt(var_muestr)/sqrt(n)
  }
  cat("Intervalo de Confianza: [",
      Y-value, ",", Y+value, "] con probabilidad ",alpha)
}