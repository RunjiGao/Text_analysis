library(tidyverse)

# R has following core data structures:
#Vectors -> atomic and lists
#atomic -> character, numeric(double), integer, logical
#Factors
#Lists
#Matrices/arrays
#Data frames

x = factor(rep(letters[1:3],e =10))
attributes(x)
levels(x)
class(x)
as.numeric(x)
#sum(x) Error in Summary.factor(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,  : 
#‘sum’ not meaningful for factors

x = sample(state.name, 10000, replace=T)
format(object.size(factor(x)), units='Kb')

df = 
  crossing(factor_1 = c('A', 'B'),
           factor_2 = c('Q', 'X', 'J')) %>% 
  mutate(x=rnorm(6),
         y=rnorm(6))

df
x = c('A', '1', 'Q')
paste(x,collapse = "_")

paste(c('A', '1', 'Q'), c('B', '2', 'z'),sep = " - ")

grep(starwars$name, pattern = ?)
