library(ggplot2)
library(patchwork)

beta <- 12
n <- 25
n_sim <- 1000

vector_fill_1 <- function(n){
  x = runif(n,0,beta)
  return(max(x)*n/(n+1))
}

vector_fill_2 <- function(n){
  x = runif(n,0,beta)
  return(2*mean(x))
}

df <- data.frame(
  nmax = sapply(1:n_sim, vector_fill_1),
  mean_2 = sapply(1:n_sim, vector_fill_2)
)

p <- ggplot(data = df, 
            aes(x=1:n_sim)) + 
  geom_line(aes(y=mean_2), colour = 'blue') +
  geom_line(aes(y=nmax), colour = 'red') + 
  geom_hline(yintercept = beta, colour = 'black') + 
  labs(
    x = 'Trials',
    y = 'Results',
    title = 'Simulation')
  

q <- ggplot(data = df) + 
  geom_histogram(aes(x = nmax), fill = 'red', binwidth=0.15)+
  geom_histogram(aes(x = mean_2), fill = 'blue', binwidth=0.15)+
  labs(x = 'Values',y = 'Amount',title = 'Histograms')



