library(tidyverse)

#Converting into Numerics For Characters:
c <- c("12","13","14","12","12")
c %>% glimpse()

d <- c %>% as.numeric()
d %>% glimpse()

#Converting into Numerics For Factors:
f <- c("12","13","14","12","12") %>% as.factor()
f %>% glimpse()

i <- f %>% as.numeric()
i %>% glimpse()

#Correct Way:
n <- f %>% as.character() %>% as.numeric() # factor --> as.character --> as.numeric
n %>% glimpse()


# Date data type ----
x <- "2019-03-21"
y1 <- x %>% as.Date()
y1

x <- "2019-mar-21"
y2 <- x %>% as.Date("%Y-%b-%d")
y2

x <- "21-mar-19"
y3 <- x %>% as.Date("%d-%b-%y")
y3

x <- "21/MAR/19"
y4 <- x %>% as.Date("%d/%B/%y")
y4

x <- "3.21.2019"
y5 <- x %>% as.Date("%m.%d.%Y")
y5

identical(y1,y2,y3,y4,y5); y1; y2; y3; y4; y5


# lubridate()
library(lubridate) 

ymd(20191123)
dmy(23112019)
mdy(11232019)

lubridate::year(y1)
lubridate::month(y2)
lubridate::day(y3)

interval("2017-11-01","2018-11-08") %>% as.period(unit = "year") %>% #month,day
  as.character() %>% substr(1,nchar(.)-9)
