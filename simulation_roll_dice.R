#flipping a coin:

library(ggplot2)

nth_term <- function(n){
  a<-rbinom(n,1,0.5)
  return(sum(a)/n)
}

n=4000
c<- sapply(1:n,nth_term)

df <- data.frame(
  intento = 1:n,
  probabilidad = c
)

p = ggplot(df, aes(x = intento, y = probabilidad)) +
  geom_line(color = "blue") +
  geom_hline(
    yintercept = 0.5, 
    color = "red")
  labs(
    x = "Attempt", 
    y = "Proportion of Successes", 
    title = "Simulation of Rolling a Dice") +
  theme_minimal()

print(p)
