
library(tidyverse)
library(brms)
library(ggpubr)

# Read database
db <- read.csv2("bd.csv")
names(db)

db_PC <- db[,c(1:2,79:342)]
names(db_PC)

## Lactate
db_lactate_pre <- create_db(varname = "LACTATO", pre_post = "PRE", full_db = db_PC)
db_lactate_post <- create_db(varname = "LACTATO", pre_post = "POST", full_db = db_PC)

## Wrong data
ggplot(db_lactate_pre, aes(x = Time, y = Values, color = Condition)) + geom_point()
View(db_lactate_pre)
db_lactate_pre[78,4] <- NA
db_lactate_pre[100,4] <- NA

ggplot(db_lactate_post, aes(x = Time, y = Values, color = Condition)) + geom_point()

# weakly informative priors on regression coefficients
sd(db_lactate_pre$Values, na.rm = T)*2.5
priors <- prior(normal(0, 4.84), class = "b")

sd(db_lactate_post$Values, na.rm = T)*2.5
priors <- prior(normal(0, 6.27), class = "b")











