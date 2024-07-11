# Set up environment
library(tidyverse)

# Function to generate test data

tstdata <- function(TrueMeas, TrueMSx, RunSize, RepSize, Pot = TRUE, Dates = TRUE) {
  TrueMeas <- if (Pot) {log10(TrueMeas)}
  
  StdDev <- if (Pot) {log10(TrueMSx) / (2 * sqrt(2))}
    else {TrueMSx/(2 * sqrt(2))}
  
  Run <- if(Dates){
    seq(from = mdy('1/1/2023'), by = '1 week', length.out = RunSize)
  } else {c(1:SmplSize)} 
  Run <-  rep(Run, each = RepSize)
  
  Replicate <- rep(c(1:RepSize), times = RunSize)
  
  Measure <- rnorm(n = RunSize * RepSize, mean = TrueMeas, sd = StdDev)
  
  Output <- tibble(Run,Replicate, Measure, Pot) %>% 
    mutate(Measure = if_else(Pot, 10 ^ Measure, Measure))
}

TestData <- tstdata(TrueMeas = 10, TrueMSx = 3, RunSize = 5, RepSize = 3, Pot = TRUE, Dates = TRUE)

 Run <-  rep(Run, each = RepSize)

write_csv(TestData,'TestData/TestData')