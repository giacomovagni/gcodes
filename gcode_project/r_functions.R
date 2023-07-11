
print("hello")

#
library(tidyverse)
#

#var = atus$rectype.x
#w = "labels"

# extract labels #
lab_r = function(var, w = "question"){
  message("question or labels")
  
  if(w == "question"){
    attr(var, which = "label")
  }
  if(w == "labels"){
     attr(var, which = "labels")
  }
}

#
diff_simple <- function(x, g, index = c("all", "1", "2")) {
  index <- match.arg(index)
  
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]
  
  if (!is.factor(g)) {
    g <- factor(g)
  }
  k <- length(levels(g))
  
  if (!identical(k, 2L)) stop("Must have two groups")
  
  m <- as.vector(by(x, g, mean))
  v <- as.vector(by(x, g, var))
  n <- as.vector(by(x, g, length))
  
  useV <- switch(index,
                 `all` = sum((n - 1) * v) / (sum(n) - k),
                 `1` = v[1],
                 `2` = v[2])
  
  out <- m
  
  names(out) <- c('0', '1')
  return(out)
}
#

#
diff_simple(mtcars$hp, mtcars$vs)
#

#
md <- function(x, g, index = c("all", "1", "2")) {
  index <- match.arg(index)
  
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]
  
  if (!is.factor(g)) {
    g <- factor(g)
  }
  k <- length(levels(g))
  
  if (!identical(k, 2L)) stop("Must have two groups")
  
  m <- as.vector(by(x, g, mean))
  v <- as.vector(by(x, g, var))
  n <- as.vector(by(x, g, length))
  
  useV <- switch(index,
                 `all` = sum((n - 1) * v) / (sum(n) - k),
                 `1` = v[1],
                 `2` = v[2])
  
  out <- diff(m) # abs(diff(m) ) #/ sqrt(useV))
  
  names(out) <- "MD"
  return(out)
}

smd <- function(x, g, index = c("all", "1", "2")) {
  index <- match.arg(index)
  
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]
  
  if (!is.factor(g)) {
    g <- factor(g)
  }
  k <- length(levels(g))
  
  if (!identical(k, 2L)) stop("Must have two groups")
  
  m <- as.vector(by(x, g, mean))
  v <- as.vector(by(x, g, var))
  n <- as.vector(by(x, g, length))
  
  useV <- switch(index,
                 `all` = sum((n - 1) * v) / (sum(n) - k),
                 `1` = v[1],
                 `2` = v[2])
  
  # out <- abs( diff(m) / sqrt(useV) )
  
  out_m <- m
  names(out_m) <- c('0', '1')
  
  out_md <-  diff(m)
  out_smd <-  diff(m) / sqrt(useV) 
  
  outall = data.frame( t(data.frame(out_m)), md = out_md, smd = out_smd)
  
  # names(out) <- "SMD"
  return(outall)
}

#
#aggregate(hp ~ vs, mtcars, mean)
#

#
#diff_simple(mtcars$hp, mtcars$vs)
#md(mtcars$hp, mtcars$vs)
#smd(x = mtcars$hp, g = mtcars$vs, index = 'all')
#

#
# smd(m1$distance_OM, m1$treated)
#


#
greplreshape = function(data, expr = "HSEX") colnames(data )[ grep(expr, colnames(data)) ]
#

#  
#sqfam = dtacoupleIII[,c('Q2', 'ChildrenHouseholdH', 'Q13_recBothHigh', 'Q13_rec2H', 'Q13_rec2F', greplreshape(dtacoupleIII, 'famtime'))]  %>% 
#  melt(id.vars = c('Q2', 'ChildrenHouseholdH', 'Q13_recBothHigh', 'Q13_rec2H', 'Q13_rec2F')) %>% dplyr::arrange(Q2, variable) 
#

#
frac_ex = function(range_frac = 1:13, range_whole = 1:5){
  
  # fractions #
  range_nb = range_frac
  
  #
  n1 = sample(x = range_nb, size = 2)
  n2 = n1[order(n1)]
  #
  answer1 = n2[1]/n1[2]
  #
  
  #
  x1 = sample(x = range_whole, size = 1)
  #
  
  # n-1 times the whole + the fractional part #
  whole = x1*n2[2]
  grand_fract = (x1+1)*n2[2]
  #
  
  #
  total = whole + n2[1]
  res = total/n2[2]
  #
  
  #
  a1 = data.frame(x = rep(1, n2[1]), v = rep('numerator', n2[1]) )
  a2 = data.frame(x = rep(1, n2[2]), v = rep('denominator', n2[2]) )
  #
  
  #
  rb = suppressWarnings( bind_rows(a1,a2) )
  rb = as.data.frame(rb)
  #
  
  rb = rb %>% group_by(v) %>% mutate(cs = cumsum(x), s = sum(x))
  #
  
  rb2 = rb[rb$v == 'denominator', ]
  
  #
  bp = suppressWarnings( bind_rows( replicate( x1 , rb2, simplify = FALSE), .id = 'p')  )
  bp = bp %>% select(p, v, x)
  bp$p = as.numeric(bp$p)
  
  #
  bp2 = rb2 %>% mutate(d = n2[1]) %>% 
    mutate(x = ifelse(d >= cs, 0, 1)) %>%
    group_by() %>% 
    mutate(v = ifelse(d >= cs, 'denominator', 'numerator')) %>% 
    mutate(p = x1+1) %>% select(p, v, x)
  #
  
  bp = bind_rows(bp, bp2)
  #tail(bp)
  #
  
  bp = bp %>% group_by(p) %>% mutate(n = 1:n())
  bp$y = 1
  
  #
  bp$v = ifelse(bp$v == 'denominator', 'slice', 'pie')
  #
  
  #x1+(n2[1]/n2[2]) == res
  #
  
  #
  g = ggplot(bp, aes(x="", y = y, fill = v ))+
    geom_bar(width = 1, color = 'black', stat = "identity") + 
    facet_wrap(~p) +
    coord_polar("y", start=0) + 
    theme(axis.text.x=element_blank()) + 
    scale_fill_manual(values = c('gray80', 'coral')) + 
    ggtitle(label = paste('results:', x1, '+', n2[1], '/', n2[2], '=', res ) ) 
  #
  
  #
  answer2 = x1 + (n2[1]/n2[2])
  res_turn = list(bp, answer1, answer2, g)
  #
  
  #
  cat('Compute and Draw:', x1, '+', n2[1], '/', n2[2])
  #
  return(res_turn)
}

#
secondes_f = function(hours = 1){
  minutes = 60
  secondes = 60
  sm = hours*minutes*secondes
  paste(hours, 'hour(s)', 'equal to', sm, 'secondes')
  return(sm)
}

#
secondes_f(2)
#

#
df_labs_dta = function(x = x){
  labels_df = lapply(x, function(x) attributes(x)$label)
  labels_df2 = rbindlist(list(labels_df)) %>% mutate(n = 1) %>% melt(id.vars = 'n')
  return(labels_df2)
}
df_search = function(df_labs_dta, pattern = pattern) df_labs_dta[grepl(pattern, df_labs_dta$value), ] 
#

#
count_n = function(data, ...) {
  data %>% group_by(.dots = lazyeval::lazy_dots(...)) %>%
    summarise(n = n())
}

fn = function(data, ...) {
  data %>% group_by(.dots = lazyeval::lazy_dots(...)) %>%
    summarise(n = n()) %>%
    mutate(n = round( n/sum(n),4)*100)
}
#

#
fn2 <- function(df, ..., var) {

  #var = as_factor(var)
  
  quo_group_var <- enquos(...)
  quo_var <- enquo(var)
  #
  
  #
  df %>%
    count(!!! quo_group_var, !! quo_var) %>% 
    group_by(!!! quo_group_var) %>% mutate(n = round( n/sum(n),4)*100) %>% 
    spread(!! quo_var, n, 0) %>% group_by() %>% mutate(tot = rowSums(select(., -1)))
}
#

#
dff = data.frame(id = c(1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4), 
                 x = c('A','A','A','A', 'A','A','B','B', 'A','A','B','A', 'C', 'C', 'C', 'C'))
#



#
dspreadmean10 = function(data, ...) {
  data %>% group_by_(.dots = lazyeval::lazy_dots(...)) %>%
    summarise(n = n()*10) %>% spread(value, n, fill = 0, drop = F)
}

dspreadmean5 = function(data, ...) {
  data %>% group_by_(.dots = lazyeval::lazy_dots(...)) %>%
    summarise(n = n()*5) %>% spread(value, n, fill = 0, drop = F)
}

dspreadmean30 = function(data, ...) {
  data %>% group_by_(.dots = lazyeval::lazy_dots(...)) %>%
    summarise(n = n()*30) %>% spread(value, n, fill = 0, drop = F)
}


#
TimeClock = function(start = 240, x = 162.4, print = F){
  
  message('\n 4am = 240 \n 6am = 360 \n 8am = 480 \n')
  
  x = x + start
  
  # 4am to 4am == 240 -> 1680 #
  # if past midnight #
  x = x %% 1440
  
  hours = x / 60
  hours = gsub('\\..*', '', hours) %>% as.numeric()
  
  min = round(x %% 60) # rounding of secondes #
  
  hours = ifelse(hours < 1, '00:', ifelse(hours < 10, paste('0', hours, ':', sep = ''), paste('', hours, ':', sep = '') ) )
  min = ifelse(min < 10, paste('0', min, sep = ''), paste('', min, sep = '') )
  
  time_clock = paste(hours, min, sep = '') 
  time_clock
  
  if(print == T){
    print( cbind(x, x - start, time_clock) )  
  } 
  
  return(time_clock)
}

########################################################################################################################
########################################################################################################################

#
frec = function(x){
  #
  x2 = factor(x, labels = c('14pm-19pm', '4am-7am', '19pm-22pm', '22pm-12am', 
                            '12pm-14pm', '7am-12pm', '12am-4am'))
  #
  x2 = factor(x2, levels = c('4am-7am', '7am-12pm', '12pm-14pm', 
                             '14pm-19pm', '19pm-22pm', '22pm-12am', '12am-4am'))
  #
  return(x2)
} 

#
frecDF =function(x){
  #
  # x2 = factor(x, labels = c('14pm-19pm', '4am-7am', '19pm-22am', '22am-12am', 
  #                           '12pm-14pm', '7am-12pm', '12am-4am'))
  #
  x2 = factor(x, labels = c('4am-7am', '7am-12pm', '12pm-14pm', 
                            '14pm-19pm', '19pm-22pm', '22pm-12am', '12am-4am'))
  #
  return(x2)
}

######################################################
######################################################

#
df0 = data.frame(hours = TimeClock(240, seq(from = 0, to = 143, 6)*10) )
#
hours1 = unlist( lapply(1:24, function(i) rep(df0$hours[i], 6)) )
#
dfhours1 = data.frame(epnum = 1:144, hours = hours1, min = TimeClock(240, seq(from = 0, to = 143, 1)*10) )
#

#
dfhours1$time_interval = paste(dfhours1$min, dplyr::lead(dfhours1$min), sep = '-')
#
dfhours1$time_interval[length(dfhours1$time_interval)]  <- "03:50-4:00"
#

# 4 am is 240
240/60

# france 2010 #
# start at 00 == 0 minutes #

TimeClock(start = 0, seq(from = 0, to = 144, 1)*10) 
hours_france2010 = data.frame(epnum = 1:144, hours = TimeClock(start = 0, seq(from = 0, to = 143, 1)*10)   ) 
hours_france2010

hours_france2010$time_interval = paste(hours_france2010$hours, dplyr::lead(hours_france2010$hours), sep = '-')
hours_france2010$time_interval[length(hours_france2010$time_interval)]  <- "23:50-00:00"
#
hours_france2010$epnum = hours_france2010$epnum - 24
#

# cool #
hours_france2010
#

# start at 5am == 300 minutes #
TimeClock(start = 300, seq(from = 0, to = 252, 6)*5) 
hours_1974 = data.frame(epnum = 1:42, hours = TimeClock(start = 300, seq(from = 0, to = 251, 6)*5)  ) 

hours_1974$time_interval = paste(hours_1974$hours, dplyr::lead(hours_1974$hours), sep = '-')
hours_1974$time_interval[length(hours_1974$time_interval)]  <- "01:30-02:00"
#

# start at 6.30am == 300 minutes to 11.30-midnight #
data.frame( TimeClock(start = 390, seq(from = 0, to = 228, 6)*5)  )
hours_1961 = data.frame(epnum = 1:39, hours = TimeClock(start = 390, seq(from = 0, to = 228, 6)*5)  ) 
hours_1961_suite = hours_1961[1:36, ]
hours_1961 = hours_1961[1:35, ]

hours_1961$time_interval = paste(hours_1961$hours, dplyr::lead(hours_1961$hours), sep = '-')
hours_1961$time_interval[length(hours_1961$time_interval)]  <- "23:30-00:00"
#

# hours_1961_suite$epnum+13

#
setDT(dfhours1)
#
dfhours1[, hours_of_day := ifelse(epnum %in% c(1:18), 'Early Morning (4am-7am)',  # , 121:144
                                  ifelse(epnum %in% c(19:48), 'Morning (7am-12pm)', 
                                         ifelse(epnum %in% c(49:60), 'Lunch (12pm-14pm)', 
                                                ifelse(epnum %in% c(61:90), 'Afternoon (14pm-19pm)', 
                                                       ifelse(epnum %in% c(91:108), 'Evening (19pm-22pm)',
                                                              ifelse(epnum %in% c(109:120), 'Evening (22pm-12am)', 'Night (12am-4am)')))) ) ) ,] 
#

dfhours1$hours_of_day = factor(dfhours1$hours_of_day, levels = c('Early Morning (4am-7am)','Morning (7am-12pm)', 'Lunch (12pm-14pm)', 'Afternoon (14pm-19pm)', 'Evening (19pm-22pm)', 'Evening (22pm-12am)', 'Night (12am-4am)'))
#

#
dfhours1[, hours_of_day2 := ifelse(epnum %in% c(1:18), 'Early Morning (4am-7am)',  # , 121:144
                                   ifelse(epnum %in% c(19:30), 'Morning (7am-9pm)',
                                          ifelse(epnum %in% c(31:48), 'Morning (9am-12pm)',
                                                 ifelse(epnum %in% c(49:60), 'Lunch (12pm-14pm)', 
                                                        ifelse(epnum %in% c(61:72), 'Afternoon (14pm-17pm)', 
                                                               ifelse(epnum %in% c(73:90), 'Afternoon (17pm-19pm)', 
                                                                      ifelse(epnum %in% c(91:108), 'Evening (19pm-22pm)',
                                                                             ifelse(epnum %in% c(109:120), 'Night (22pm-12am)', 
                                                                                    ifelse(epnum %in% c(121:132), 'Night (12am-2am)', 
                                                                                           ifelse(epnum %in% c(133:144), 'Night (2am-4am)', 'else')))) ) ) ) ) ) ) ,] 
#

table(dfhours1$hours_of_day2)

dfhours1$hours_of_day2 = factor(dfhours1$hours_of_day2, levels = c('Early Morning (4am-7am)','Morning (7am-9pm)', 'Morning (9am-12pm)', 'Lunch (12pm-14pm)', 'Afternoon (14pm-17pm)' , 'Afternoon (17pm-19pm)', 'Evening (19pm-22pm)', 'Night (22pm-12am)', 'Night (12am-2am)', 'Night (2am-4am)'))
#

# as.data.frame(dfhours1)

#
dfhours1[, hours_daily_7am19pm := ifelse(epnum %in% c(19:90),  1, 0), ]  #'Day (7am-19pm)',
dfhours1[, hours_daily_8am18pm := ifelse(epnum %in% c(25:85),  1, 0), ]  #'Day (7am-19pm)',
dfhours1[, hours_daily_7am22pm := ifelse(epnum %in% c(19:108),  1, 0), ]  #'Day (7am-19pm)',
#

#
setDT(dfhours1)
#

dfhours1a = dfhours1 %>% mutate(ep = 1)
dfhours1b = dfhours1 %>% mutate(ep = 2)

#
dfhours = rbind(dfhours1a, dfhours1b) %>% arrange(epnum)
dfhours$ep_num = dfhours$epnum
dfhours$epnum = 1:288
#

head(dfhours);tail(dfhours)

#
setDT(dfhours)
#
dfhoursEP1 = dfhours[dfhours$ep==1, ]
#

#
minutes30 = TimeClock(start = 240, seq(0, 1440, by = 30))
minutes30 = minutes30[1:48]
#
pp30 = data.frame(min = minutes30, min30 = minutes30)
pp30
pp30
#

#
dfhoursEP1 = merge(dfhoursEP1, pp30, by = c("min"), all = T) %>% arrange(ep_num)
dfhoursEP1$min30 = as.character(dfhoursEP1$min30)

dfhoursEP1$min30

#
dfhoursEP_30min = dfhoursEP1 %>% fill(min30, .direction = "down")
#

#
dfhours[!ep_num %in% c(1:6, 134:144), c('epnum', 'hours', 'min', 'hours_of_day', 'ep_num')]#

## select on 5am-2pm #
dfhours_74 = dfhours[!ep_num %in% c(1:6, 133:144), c('epnum', 'hours', 'min', 'hours_of_day', 'ep_num')]#
##

#
# dfhours = dfhours %>% mutate(epnum = 1:n()) %>% group_by(ep) %>% mutate(ep_num = 1:n())
#
setDT(dfhours)
# dfhours



