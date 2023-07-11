
# G Vagni 2023/06
# EUI

#
require(tidyverse)
require(lubridate)
#

# you have a dataset like this #
x = rbind(c(1, 2010, 3, 2010, 6, 100), # id = 1
      c(1, 2010, 7, 2015, 1, 102), # id = 1
      
      c(2, 2003, 2, 2015, 8, 201), # id = 2
      c(2, 2015, 9, 2015, 12, 305)) # id = 2
#
x = data.frame(x)
colnames(x) = c('id', 'year_start', 'month_start', 'year_end', 'month_end', "states")
#
# with states being the states
x
#

unique_id = unique(x$id)

# 1. we add a starting row for everyone #
# re-order by id
# with all ideas
x = x %>% 
  add_row(id = unique_id, year_start = 2000, month_start = 1, states = 000) %>% 
  arrange(id, year_start, month_start) #
#
x
#

# let's add a starting month too #
x = x %>% 
  mutate(from = paste(year_start, month_start, "01", sep = '-'), 
         end = paste(year_end, month_end, sep = '-'))
#

x

#
# function days_in_month()
days_in_month(1)
days_in_month(2)
# we use days in month function to get the last day of each month #
x$end = paste(x$end, days_in_month(x$month_end), sep = "-")
#
x
#
x
#

# function ymd()
x$from = ymd(x$from) # ym if you dont want to set a day #
#

#
# x %>% group_by(id) %>% mutate(floor_date(lead(from)) - days(1))
#
y1 <- ymd("2000-01-01")
y2 <- ymd("2010-03-01")
y = c(y1,y2)
# get the last date
ymd(lead(floor_date(y, "year")) - days(1))
#

## from lead # which is the next "from" #
# floor_date()
x = x %>% group_by(id) %>% 
  mutate(end = floor_date(lead(from)) - days(1)) %>% 
  mutate(end = ymd(end))
#
x
#

# we correct the end date, # find last row for each id #
w = which(is.na(x$end))
w
x$end[w] <- ymd(paste(x$year_end[w], x$month_end[w], days_in_month(x$month_end[w])))
x
#

# fill the missing start end #
x$year_end = year(x$end)
x$month_end = month(x$end)
#
x
#

################################################################################
################################################################################

# we calculate the duration with 
# interval() and as.period()
x
x = x %>% group_by(id) %>% 
  mutate(dur = interval(from, end) ) %>%
  mutate(DUR = as.period(dur))
#

# number of days
x$DUR %/% days(1)
x$DUR %/% months(1)
x$DUR %/% years(1)
#

x$days_duration = x$DUR %/% days(1)
x$months_duration = x$DUR %/% months(1)
#

as.data.frame(x)

################################################################################
################################################################################

#
x$init_start = interval( ymd("2000-01-01"), x$from) %/% months(1)
x$init_end = interval( ymd("2000-01-01"), x$end) %/% months(1)
#

# we now set a starting date in MONTH based on a reference date, here: 2000-01-01
as.data.frame(x)
#

################################################################################
################################################################################

# put a 1 into the starting period #
x$init_start[x$init_start == 0] <- 1
#
x
#

################################################################################
################################################################################

#
library(TraMineR)
#

## Converting to STS format with alignement on ages
bf.sts.a <- seqformat(x, from = "SPELL", to = "STS",
                      id = "id", begin = "init_start", 
                      end = "init_end", 
                      status = "states",
                      process = TRUE, limit = 200)
bf.sts.a
#

#
sq = seqdef(bf.sts.a)
#

seqiplot(sq)
#
