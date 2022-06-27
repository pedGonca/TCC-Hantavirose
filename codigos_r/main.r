# Testando o Visual Studio XD
rm(list = ls())
library(tidyverse)


df <- data.frame("a" = c(1.5,1.6,1.8,1.34,1.64,1.98),
                 "b" = c(1.3,1.4,1.8,1.3,1.7,2.01)) ; df

lmObj <- lm(a ~ b, data = df)
summary(lmObj)

par(mfrow = c(2,2))
plot(lmObj)

ggplot(df, aes(a, b)) + 
  geom_point() + 
  geom_line(position = "identity")

## Funciona!
