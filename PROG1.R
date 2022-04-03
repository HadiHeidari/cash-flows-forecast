#############################################################
                
              ###SATNA RTGS98-99-1400 12 mahe kamel###

##In bakhsh Baraye satna ast az tarikh 1398 01 05 ta 1399 12 28

#1 in az dadehaye gerfte shode az mali aghaye ahangari ast. 
# majmoee 3 ta kanal ast: satna+ paya+ check ke ramzdar va check adi hast!!


#2 dade ha ebteda tarikhha chek shodan khili tatbigh baraye har se goroh yani
# satna va paya va check moshkel bod. paya roz tatik bod!! hazf shdan

#2) dar poshe E:\KING OF SOFT WARE yek add-in excel hast ejra shod 
#3) dar excel az function s2m baraye tabdil tarikhhaye shamsi be miladi
#estefade shod
# 4)dar excel rozhaye hafte moshakhas shod tabee darad!

#############################################################
library(readxl)

RTGS<- read_excel("satna-paya-Rnew.xlsx",sheet='Total')
summary(RTGS)
typeof(RTGS)





library(fpp2)
library(dplyr)
library(tidyr)
library(zoo)
library(xts)
library(lubridate)
library(forecast)
library(TSstudio)
library(plotly)
library(dplyr)
library(lubridate)
library(stats)
library(datasets)
library(base)

##https://blog.exploratory.io/filter-with-date-function-ce8e84be680
#library(readr)
#RTGS1<- read_csv("satna-paya-R.csv")

# hatman baraye nomodare ggplot2 bayad zamn character nabashad

#noee az list ast pas az `` estefade shode!

RTGS$datee <- ymd(RTGS$`datee`)
RTGS$datee <-as.Date(RTGS$`datee`) 

# taghvim miladi mah va roze haftee
RTGS$wday<-weekdays(RTGS$datee)
RTGS$month<-months(RTGS$datee)
RTGS$years<-year(RTGS$datee)
# end 

# shamsi!!!
RTGS$ tarikh1 <- ymd(RTGS$`tarikh`)
RTGS$ tarikh1<-as.Date(RTGS$`tarikh`)
RTGS$month.shamsi<-months(RTGS$tarikh1)
RTGS$years.shamsi<-year(RTGS$tarikh1)
RTGS$day.shamsi<-mday(RTGS$tarikh1)

# end 

## tedade rozhaye tatili 
RTGS$holydays<-as.numeric(RTGS$datee)

RTGS$diff <- RTGS$`holydays` - lag(RTGS$holydays, default = first(RTGS$holydays))


## end 

library("ggplot2")
library(ggridges)
theme_set(theme_ridges())

ggplot(data=RTGS, aes(x=`datee`, y=`totalout`))+
  geom_line()+
 ylab("Rials-OUT") +
  xlab("Time")


ggplot(data=RTGS, aes(x=`datee`, y=`totalin`))+ 
  geom_line()+
  geom_smooth(formula = y ~ x, method = "lm")+
  ylab(" Rials-IN") +
  xlab("Time")

ggplot(data=RTGS, aes(x=`datee`, y=`totalin`))+ 
  geom_line()+
  geom_smooth(method = "loess")+
  ylab(" Rials-IN") +
  xlab("Time")



 ggplot(RTGS, aes(`datee`)) + 
  geom_line(aes(y = `totalout`, colour = "Outflows"))+
  geom_line(aes(y = `totalin`, colour = "Inflows"))+
    ylab("Rials") +  xlab("Time")

ggplot(RTGS, aes(`datee`)) + 
  geom_line(aes(y = `totalout`, colour = "Outflows")) + 
  geom_line(aes(y = `totalin`, colour = "In flows"))+
  geom_point(aes(y = `net`, colour = "net"))+
  ylab("Rials") +  xlab("Time")






### density poshte sare ham mohane
### sale 99 ast va april mishavad 14 farvardin 99 
ggplot(
  RTGS[295:577,],
  aes(x =`totalin`, y = `month`)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Rials"
  )+  
  scale_y_discrete(limit = c("April","May","June","July","August","September","October","November","December","January","February","March"))+
  labs(title = 'Cash in Flow 1399')



ggplot(
  RTGS[295:577,],
  aes(x =`totalout`, y = `month`)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Rials"
  )+  
  scale_y_discrete(limit = c("April","May","June","July","August","September","October","November","December","January","February","March"))+
  labs(title = 'Cash out Flow 1399')



ggplot(
  RTGS[295:577,],
  aes(x =`net`, y = `month`)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Rials"
  )+  
  scale_y_discrete(limit = c("April","May","June","July","August","September","October","November","December","January","February","March"))+
  labs(title = 'Net Cash Flow 1399')


#end

### tabe tozee poshte sar ham ba estefade az month.shamsi 1398

ss<-na.omit(RTGS[1:288,])

ggplot(
  ss[1:284,],
  aes(x =`net`, y = `month.shamsi`)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Rials"
  )+  
  scale_y_discrete(limit = c("January","February","March","April","May","June","July","August","September","October","November","December"))+
  labs(title = 'Net Cash Flow 1398')

### tabe tozee poshte sar ham ba estefade az month.shamsi 1399

ss1<-na.omit(RTGS[289:577,])

ggplot(
  ss1[1:282,],
  aes(x =`net`, y = `month.shamsi`)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Rials"
  )+  
  scale_y_discrete(limit = c("January","February","March","April","May","June","July","August","September","October","November","December"))+
  labs(title = 'Net Cash Flow 1399')

##ent

### density poshte sare ham Haftegi

ggplot(
  RTGS[295:577,],
  aes(x =`totalin`, y = `wday`)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Rials"
  )+ scale_y_discrete(limit = c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday"))+

  labs(title = 'Cash in Flow')


ggplot(
  RTGS[295:577,],
  aes(x =`totalout`, y = `wday`)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Rials"
  )+ scale_y_discrete(limit = c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday"))+
  
  labs(title = 'Cash Out Flow')


ggplot(
  RTGS[289:577,],
  aes(x =`net`, y = `wday`)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Rials"
  )+ scale_y_discrete(limit = c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday"))+
  
  labs(title = 'Net Cash Flow 1399')


#end

library(ggplot2)
library(patchwork)

ggplot(ss1) + geom_boxplot(aes(net, day.shamsi, group = day.shamsi))



### tahlil kanalha 
x <- data.frame("chanel" = c("check","paya","rtgs","total"),
                "in1" = c (1.01e+12,1.3e+11,2.3e+12,3.52e+12),
                "out1" = c(7.7e+11,1.03e+12,2.7e+12,3.48e+12))

barplot(t(as.matrix(x[,2:3])),
        main = "Red in and Green out",
        xlab = "chanels",
        ylab = "Rials",
        names.arg =x$chanel ,
        col =c("red","green"),inside = TRUE)
abline(h=48)

### heat map ha

library(tidyquant)
library(ggplot2)
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")

r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384") 
calendarHeat(RTGS$datee, RTGS$`net`, ncolors = 99, color = "r2g", varname="NETFLOW")
calendarHeat(ss1$tarikh1, ss$`net`, ncolors = 99, color = "r2g", varname="NETFLOW 1399")

### Histogram
hist(RTGS$net,col="gray", breaks=20, labels = TRUE, xlim=c(-14e+12,10e+12))
hist(ss1$net,col="gray", breaks="FD", labels = TRUE, xlim=c(-13e+12,8e+12))
hist(RTGS[288:613,]$net,col="gray", breaks="FD", labels = TRUE, xlim=c(-13e+12,8e+12))

## cumulative distibution function mahaneee##

plot(ecdf(ss1$net))
ggplot(RTGS, aes(net)) + stat_ecdf(geom = "step")
ggplot(ss1, aes(net)) + stat_ecdf(geom = "step")
ggplot(ss1, aes(net, colour = diff)) + stat_ecdf()

## end cumulative

##wrangling tidyverse

summary_98 <- ss %>% 
  group_by(wday) %>% 
  summarize(mean = mean(net, na.rm = TRUE), 
            std_dev = sd(net, na.rm = TRUE),
            count = n(),
            q1 = quantile(net, 0.25, na.rm = TRUE),
            median = quantile(net, 0.5, na.rm = TRUE),
            q3 = quantile(net, 0.75, na.rm = TRUE),
            )
summary_98

summary_99 <- ss1 %>% 
  group_by(wday) %>% 
  summarize(mean = mean(net, na.rm = TRUE), 
            std_dev = sd(net, na.rm = TRUE),
            count = n(),
            q1 = quantile(net, 0.25, na.rm = TRUE),
            median = quantile(net, 0.5, na.rm = TRUE),
            q3 = quantile(net, 0.75, na.rm = TRUE),
  )
summary_99

summary_1400 <- RTGS[578:613,] %>% 
  group_by(wday) %>% 
  summarize(mean = mean(net, na.rm = TRUE), 
            std_dev = sd(net, na.rm = TRUE),
            count = n(),
            q1 = quantile(net, 0.25, na.rm = TRUE),
            median = quantile(net, 0.5, na.rm = TRUE),
            q3 = quantile(net, 0.75, na.rm = TRUE),
  )
summary_1400


summary_99_out <- ss1 %>% 
  group_by(wday) %>% 
  summarize(mean = mean(totalout, na.rm = TRUE), 
            std_dev = sd(totalout, na.rm = TRUE),
            count = n(),
            q1 = quantile(totalout, 0.25, na.rm = TRUE),
            median = quantile(totalout, 0.5, na.rm = TRUE),
            q3 = quantile(totalout, 0.75, na.rm = TRUE),
  )
summary_99_out


summary_99_in <- ss1 %>% 
  group_by(wday) %>% 
  summarize(mean = mean(totalin, na.rm = TRUE), 
            std_dev = sd(totalin, na.rm = TRUE),
            count = n(),
            q1 = quantile(totalin, 0.25, na.rm = TRUE),
            median = quantile(totalin, 0.5, na.rm = TRUE),
            q3 = quantile(totalin, 0.75, na.rm = TRUE),
  )
summary_99_in


length(which(RTGS$net< 0))
length(which(subset(ss1,month.shamsi== "January")$net< 0))
length(which(subset(ss1,month.shamsi== "February")$net< 0))
length(which(subset(ss1,month.shamsi== "March")$net< 0))
length(which(subset(ss1,month.shamsi== "April")$net< 0))


RTGS %>%  group_by(years.shamsi) %>% 
  filter(month.shamsi == "January") %>% 
   summarize(mean = mean(net, na.rm = TRUE), 
            std_dev = sd(net, na.rm = TRUE),
            count = n(),
            q1 = quantile(net, 0.25, na.rm = TRUE),
            median = quantile(net, 0.5, na.rm = TRUE),
            q3 = quantile(net, 0.75, na.rm = TRUE),
   )


RTGS %>%  group_by(years.shamsi) %>% 
  filter(month.shamsi == "February") %>% 
  summarize(mean = mean(net, na.rm = TRUE), 
            std_dev = sd(net, na.rm = TRUE),
            count = n(),
            q1 = quantile(net, 0.25, na.rm = TRUE),
            median = quantile(net, 0.5, na.rm = TRUE),
            q3 = quantile(net, 0.75, na.rm = TRUE),
  )

aaa<- RTGS %>%  group_by(years.shamsi, month.shamsi) %>%
  filter(month.shamsi == "January") %>%
  summarize(mean = mean(net, na.rm = TRUE), 
            std_dev = sd(net, na.rm = TRUE),
            count = n(),
            q1 = quantile(net, 0.05, na.rm = TRUE),
            median = quantile(net, 0.4, na.rm = TRUE),
            q3 = quantile(net, 0.7, na.rm = TRUE),
  )
write.csv(aaa, file = "aaa.csv")

RTGS %>%  group_by(years.shamsi , diff) %>% 
    summarize(mean = mean(net, na.rm = TRUE), 
            std_dev = sd(net, na.rm = TRUE),
            count = n(),
            q1 = quantile(net, 0.25, na.rm = TRUE),
            median = quantile(net, 0.5, na.rm = TRUE),
            q3 = quantile(net, 0.75, na.rm = TRUE),
  )


###hamkhatihaye, ya hambastegihaye kole sale 1398 , 99 , 1400
library("GGally")


RTGS[1:288,9:10] %>% GGally::ggpairs()+labs(title = '1398')
RTGS[289:577,9:10] %>% GGally::ggpairs()+labs(title = '1399')
RTGS[578:613,9:10] %>% GGally::ggpairs()+labs(title = '1400')

### hambastegi mahaneeee

RTGSJanuary <- subset(RTGS[,9:20], month.shamsi =="January")
RTGSJanuary[1:20,1:2] %>% GGally::ggpairs()+labs(title = 'January 1398')
RTGSJanuary[21:40,1:2] %>% GGally::ggpairs()+labs(title = 'January 1399')
RTGSJanuary[41:61,1:2] %>% GGally::ggpairs()+labs(title = 'January 1400')


RTGSFebruary <- subset(RTGS[,9:20], month.shamsi =="February")
RTGSFebruary[1:23,1:2] %>% GGally::ggpairs()+labs(title = 'February 1398')
RTGSFebruary[24:47,1:2] %>% GGally::ggpairs()+labs(title = 'February 1399')
RTGSFebruary[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'Februaryy 1400')


RTGSMarch <- subset(RTGS[,9:20], month.shamsi =="March")
RTGSMarch[1:22,1:2] %>% GGally::ggpairs()+labs(title = 'March 1398')
RTGSMarch[23:47,1:2] %>% GGally::ggpairs()+labs(title = 'March 1399')
RTGSMarch[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'March 1400')


RTGSApril <- subset(RTGS[,9:20], month.shamsi =="April")
RTGSApril[1:25,1:2] %>% GGally::ggpairs()+labs(title = 'April 1398')
RTGSApril[26:51,1:2] %>% GGally::ggpairs()+labs(title = 'April 1399')
RTGSApril[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'April 1400')


RTGSMay <- subset(RTGS[,9:20], month.shamsi =="May")
RTGSMay[1:25,1:2] %>% GGally::ggpairs()+labs(title = 'May 1398')
RTGSMay[26:50,1:2] %>% GGally::ggpairs()+labs(title = 'May 1399')
RTGSMay[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'May 1400')


RTGSJune <- subset(RTGS[,9:20], month.shamsi =="June")
RTGSJune[1:23,1:2] %>% GGally::ggpairs()+labs(title = 'June 1398')
RTGSJune[24:47,1:2] %>% GGally::ggpairs()+labs(title = 'June 1399')
RTGSJune[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'June 1400')


RTGSJuly <- subset(RTGS[,9:20], month.shamsi =="July")
RTGSJuly[1:25,1:2] %>% GGally::ggpairs()+labs(title = 'July 1398')
RTGSJuly[26:49,1:2] %>% GGally::ggpairs()+labs(title = 'July 1399')
RTGSJuly[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'July 1400')


RTGSAugust <- subset(RTGS[,9:20], month.shamsi =="August")
RTGSAugust[1:23,1:2] %>% GGally::ggpairs()+labs(title = 'August 1398')
RTGSAugust[24:46,1:2] %>% GGally::ggpairs()+labs(title = 'August 1399')
RTGSAugust[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'August 1400')


RTGSSeptember <- subset(RTGS[,9:20], month.shamsi =="September")
RTGSSeptember[1:25,1:2] %>% GGally::ggpairs()+labs(title = 'September 1398')
RTGSSeptember[26:51,1:2] %>% GGally::ggpairs()+labs(title = 'September 1399')
RTGSSeptember[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'September 1400')


RTGSOctober <- subset(RTGS[,9:20], month.shamsi =="October")
RTGSOctober[1:26,1:2] %>% GGally::ggpairs()+labs(title = 'October 1398')
RTGSOctober[27:51,1:2] %>% GGally::ggpairs()+labs(title = 'October 1399')
RTGSOctober[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'October 1400')


RTGSNovember <- subset(RTGS[,9:20], month.shamsi =="November")
RTGSNovember[1:24,1:2] %>% GGally::ggpairs()+labs(title = 'November 1398')
RTGSNovember[25:49,1:2] %>% GGally::ggpairs()+labs(title = 'November 1399')
RTGSNovember[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'November 1400')


RTGSDecember <- subset(RTGS[,9:20], month.shamsi =="December")
RTGSDecember[1:23,1:2] %>% GGally::ggpairs()+labs(title = 'December 1398')
RTGSDecember[24:45,1:2] %>% GGally::ggpairs()+labs(title = 'December 1399')
RTGSDecember[48:62,1:2] %>% GGally::ggpairs()+labs(title = 'December 1400')

## hambastegi end mahane 

cor(RTGSDecember[1:23,1], RTGSDecember[1:23,2], method = c("kendall"))
cor.test(RTGSDecember$totalin[1:23],RTGSDecember$totalout[1:23], method=c("pearson", "kendall", "spearman"))





## amarheye max min , ...

summary_99_in_monthly <- ss1 %>% 
  group_by(month.shamsi) %>% 
  summarize(mean = mean(totalin, na.rm = TRUE), 
            std_dev = sd(totalin, na.rm = TRUE),
            count = n(),
            q1 = quantile(totalin, 0.4, na.rm = TRUE),
            median = quantile(totalin, 0.6, na.rm = TRUE),
            q3 = quantile(totalin, 0.9, na.rm = TRUE),
  )
summary_99_in_monthly


summary_99_out_monthly <- ss1 %>% 
  group_by(month.shamsi) %>% 
  summarize(mean = mean(totalout, na.rm = TRUE), 
            std_dev = sd(totalout, na.rm = TRUE),
            count = n(),
            q1 = quantile(totalout, 0.4, na.rm = TRUE),
            median = quantile(totalout, 0.6, na.rm = TRUE),
            q3 = quantile(totalout, 0.9, na.rm = TRUE),
  )
summary_99_out_monthly

### nomodare mahane 98, 99,1400 , mehvar ofoghi 
## holydays:haman tarikh adadi shode doroste!! raftare yek mah dar 3 sal!!!

ggplot(RTGSJanuary, aes(x=holydays , y=totalin)) +
  geom_line()

ggplot(RTGSJanuary, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))

ggplot(RTGSFebruary, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))


ggplot(RTGSMarch, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))


ggplot(RTGSApril, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))

ggplot(RTGSJune, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))

ggplot(RTGSJuly, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))

ggplot(RTGSAugust, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))


ggplot(RTGSMay, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))


ggplot(RTGSSeptember, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))

ggplot(RTGSOctober, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))

ggplot(RTGSNovember, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))


ggplot(RTGSDecember, aes(holydays)) + 
  geom_line(aes(y=totalin, colour = "IN")) + 
  geom_line(aes(y = totalout, colour = "OUT"))

## end mahene khati
###hamkhatihaye haftegi , sal


RTGSsaturday <- subset(RTGS[,9:20], wday =="Saturday")
RTGSsaturday[1:49,1:2] %>% GGally::ggpairs()+labs(title = 'Saturday 1398')
RTGSsaturday[50:95,1:2] %>% GGally::ggpairs()+labs(title = 'Saturday 1399')
RTGSsaturday[98:104,1:2] %>% GGally::ggpairs()+labs(title = 'Saturday 1400')

RTGSsunday <- subset(RTGS[,9:20], wday =="Sunday")
RTGSsunday[1:48,1:2] %>% GGally::ggpairs()+labs(title = 'Sunday 1398')
RTGSsunday[49:95,1:2] %>% GGally::ggpairs()+labs(title = 'Sunday 1399')
RTGSsunday[96:104,1:2] %>% GGally::ggpairs()+labs(title = 'Sunday 1400')

RTGSMonday <- subset(RTGS[,9:20], wday =="Monday")
RTGSMonday[1:47,1:2] %>% GGally::ggpairs()+labs(title = 'Monday 1398')
RTGSMonday[48:97,1:2] %>% GGally::ggpairs()+labs(title = 'Monday 1399')
RTGSMonday[98:102,1:2] %>% GGally::ggpairs()+labs(title = 'Monday 1400')


RTGSTuesday <- subset(RTGS[,9:20], wday =="Tuesday")
RTGSTuesday[1:47,1:2] %>% GGally::ggpairs()+labs(title = 'Tuesday 1398')
RTGSTuesday[48:97,1:2] %>% GGally::ggpairs()+labs(title = 'Tuesday 1399')
RTGSTuesday[98:102,1:2] %>% GGally::ggpairs()+labs(title = 'Tuesday 1400')

RTGSWednesday <- subset(RTGS[,9:20], wday =="Wednesday")
RTGSWednesday[1:48,1:2] %>% GGally::ggpairs()+labs(title = 'Wednesday 1398')
RTGSWednesday[49:96,1:2] %>% GGally::ggpairs()+labs(title = 'Wednesday 1399')
RTGSWednesday[97:102,1:2] %>% GGally::ggpairs()+labs(title = 'Wednesday 1400')

RTGSThursday <- subset(RTGS[,9:20], wday =="Thursday")
RTGSThursday[1:50,1:2] %>% GGally::ggpairs()+labs(title = 'Thursday 1398')
RTGSThursday[51:97,1:2] %>% GGally::ggpairs()+labs(title = 'Thursday 1399')
RTGSThursday[98:103,1:2] %>% GGally::ggpairs()+labs(title = 'Thursday 1400')

## ende hambastegi haftegi

length(which(RTGSsaturday$net< 0))

## jadavel hambastegi


cor(x, y, method = c("pearson", "kendall", "spearman"))
cor.test(x, y, method=c("pearson", "kendall", "spearman"))


##tedad roze manfi mosbate haftegi
library(dplyr)

RTGS %>% filter(net < 0) %>%
  group_by(wday) %>%
  summarize(n()) -> calweekef1 

RTGS %>% filter(net > 0) %>%
  group_by(wday) %>%
  summarize(n()) -> calweekef2 

calweekef<-cbind(calweekef1,calweekef2)
calweekef <- calweekef[,-3]

names(calweekef)<- c("wday","nminday","nposday")

## az soton estefade konim tartib eshtebahe!!
barplot(t(as.matrix(calweekef[,2:3])),
        main = "Red net<0 and Green net>0",
        xlab = "days of week",
        ylab = "number of pos and neg days",
        names.arg =calweekef$wday ,
        col =c("red","green"),inside = TRUE)
abline(h=48)
###end
barplot(t(as.matrix(calweekef[,2:3])),
        main = "Red net<0 and Green net>0",
        ylim=c(0,110),
        xlab = "days of week",
        ylab = "number of pos and neg days",
        names.arg =c("Saturday", "Sunday", "Monday" ," Tuesday" , "Wednesday", "Tursday"),
        col =c("red","green"),beside= TRUE)
abline(h=48)
## myangin roze hafte va sale
#filter(RTGS, datee1 > as.Date("2021-03-12"))
RTGS %>% filter(datee > as.Date("2019-03-25")) %>% select(totalin)  

RTGS %>% filter(between(datee, as.Date("2019-03-25"), as.Date("2020-03-24"))) %>%
  select(datee, wday, month,totalin,totalout,month.shamsi) -> inout98 

RTGS %>% filter(between(datee, as.Date("2020-03-25"), as.Date("2021-03-18"))) %>%
  select(datee, wday, month,totalin,totalout,month.shamsi) -> inout99 


## Quantiles... asare rozhaye tatili va taghvimi

diff_99_in_monthly <- ss1 %>% 
  group_by(diff) %>% 
  summarize(mean = mean(totalin, na.rm = TRUE), 
            std_dev = sd(totalin, na.rm = TRUE),
            count = n(),
            q1 = quantile(totalin, 0.4, na.rm = TRUE),
            median = quantile(totalin, 0.6, na.rm = TRUE),
            q3 = quantile(totalin, 0.9, na.rm = TRUE),
  )
diff_99_in_monthly


diff_99_net_monthly <- ss1 %>% 
  group_by(diff) %>% 
  summarize(mean = mean(net, na.rm = TRUE), 
            std_dev = sd(net, na.rm = TRUE),
            count = n(),
            q1 = quantile(net, 0.1, na.rm = TRUE),
            median = quantile(net, 0.5, na.rm = TRUE),
            q3 = quantile(net, 0.75, na.rm = TRUE),
  )
diff_99_net_monthly

write.csv(diff_99_net_monthly, file = "diff_99_net_monthly.csv")

RTGS %>% group_by(diff) %>%
  summarize(mean = mean(totalout), sd = sd(totalout),n = n())

RTGS %>% group_by(diff) %>%
  filter(net<0) %>%
  select(net)

RTGS %>% group_by(diff,years) %>%
  summarize(mean = mean(net), sd = sd(net),n = n(),max=max(net),min=min(net))

RTGS %>% group_by(wday,years.shamsi) %>%
  summarize(mean = mean(net), sd = sd(net),n = n(),max=max(net),min=min(net),sum=sum(net < 0))

RTGS %>% filter(diff==4) %>%
  select(net) 
## end tatili

inout98 %>% group_by(wday) %>%
  summarize(mean = mean(totalin), sd = sd(totalin),n = n()) ->in98meansd
names(in98meansd)<- c("dayweek", "mean98in","sd98in","n98in")


inout99 %>% group_by(wday) %>%
  summarize(mean = mean(totalin),sd = sd(totalin), n = n()) ->in99meansd
names(in99meansd)<- c("dayweek","mean99in","sd99in","n99in")

inout98 %>% group_by(wday) %>%
  summarize(mean = mean(totalout),sd = sd(totalout), n = n()) ->out98meansd
names(out98meansd)<- c("dayweek","mean98out","sd98out","n98out")

inout99 %>% group_by(wday) %>%
  summarize(mean = mean(totalout),sd = sd(totalout), n = n()) ->out99meansd
names(out99meansd)<- c("dayweek","mean99out","sd99out","n99out")


inoutmeansd_9899<-cbind(in98meansd,out98meansd,in99meansd,out99meansd)
inoutmeansd_9899 <- inoutmeansd_9899[,-c(4,5,8,9,12,13,16)]

y1 = c(0, 0.5e+12, 1e+12, 1.5e+12,2.5e+12)

barplot(t(as.matrix(inoutmeansd_9899[,2:5])),
        main = "Mean SD 98",
        ylim=range(y1),
        xlab = "days of week",
        ylab = "Rials",
        names.arg =c("Monday" ,"Saturday", "Sunday","Thursday", " Tuesday" , "Wednesday"),
        col =c("red","green","blue","black"),beside= TRUE)

legend("topright", legend = c("Mean98IN", "SD98IN","Mean98OUT", "SD98OUT"), fill= c("red","green","blue","black"))


y = c(0, 1e+12, 2e+12,3e+12,4e+12,5.3e+12)
barplot(t(as.matrix(inoutmeansd_9899[,6:9])),
        main = "Mean SD 99",
        ylim= range(y),
        xlab = "days of week",
        ylab = "Rials",
        names.arg =c("Monday" ,"Saturday", "Sunday","Thursday", " Tuesday" , "Wednesday"),
        col =c("red","green","blue","black"),beside= TRUE)

legend("topright", legend = c("Mean99IN", "SD99IN","Mean99OUT", "SD99OUT"), fill= c("red","green","blue","black"))
#end


### nomodar khati seri zamani va hambastegi va tabee tozee har roze hafte in va out

plot(totalin~datee,data=subset(inout99, wday =="Wednesday"),type="l",col="red", ylim=c(0 ,12.5e+12))
lines(totalout~datee,data=subset(inout99, wday =="Wednesday"),type="b",col="green",lwd=2, pch=19)
#legend(2,3.5e+13,legend=c("IN","OUT"), col=c("red","green"),lwd=c(5,2),pch=c(15,19), y.intersp=1.5)

plot(totalin~datee,data=subset(RTGS, wday =="Saturday"),type="l",col="red", ylim=c(0 ,12.5e+12))
lines(totalout~datee,data=subset(RTGS, wday =="Saturday"),type="b",col="green",lwd=2, pch=19)
#legend(2,3.5e+13,legend=c("IN","OUT"), col=c("red","green"),lwd=c(5,2),pch=c(15,19), y.intersp=1.5)
#end

## tavabe tozee chegali roye ham ghar migirad ya varonee!!
library(ggplot2)
library(hrbrthemes)

p1 <- ggplot(RTGSsaturday, aes(x=x) ) +
  # Top
  geom_density( aes(RTGSsaturday$totalin, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1e13, y=4e-13, label="saturdayIN"), color="#69b3a2") +
  # Bottom
  geom_density( aes(RTGSsaturday$totalout, y = -..density..), fill= "#404080") +
  geom_label( aes(x=1e13, y=-4e-13, label="saturdayOUT"), color="#404080") +
  theme_ipsum() +  xlab("value of x")  


p2 <- ggplot(RTGSsaturday, aes(x=x) ) +
  # green
  geom_density( aes(RTGSsaturday$totalin, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1e12, y=4e-13, label="IN"), color="#69b3a2") +
  #violet
  geom_density( aes(RTGSsaturday$totalout, y = ..density..), fill= "#404080") +
  geom_label( aes(x=1.5e13, y=-4e-13, label="OUT"), color="#404080") +
  xlab("value of x")  

## end tavabe tozee
####### end of tahlil dadeha

## model kamel bedone tafkik
library(forecast)
ggAcf(RTGS$net,lag=200)
ggPacf(RTGS$net,lag=100)

ggAcf(RTGS$totalout,lag=300)
ggPacf(RTGS$totalout,lag=300)

ggAcf(RTGS$totalin,lag=30)
ggPacf(RTGS$totalin,lag=30)


# tabdil be ts object ketab Rami Krispi p 77

library(TSstudio)
library(dplyr)

RTGS_ts <- ts(RTGS, frequency = 365)
RTGSxts <- xts(RTGS, order.by=RTGS$datee)

ts_lags(RTGS_ts[,9], lags = c(7, 14, 21, 28))

### modelsazi barasase rozhaye hafte


ggAcf(RTGSsaturday$totalin,lag=100)
ggPacf(RTGSsaturday$totalin,lag=100)

ggAcf(diff(RTGSsaturday$totalin),lag=100)
ggPacf(diff(RTGSsaturday$totalin),lag=100)

ggAcf(RTGSsaturday$totalout,lag=100)
ggPacf(RTGSsaturday$totalout,lag=100)

## tebghe ketabe hydeman p 27 dadeha besorate ts tabdil shodan
#az dastore xts zir!! estefade nashode mikhastim ba ketab va modehash hamahang bashad

RTGSsaturday_ts <- ts(RTGSsaturday, start=2019, frequency=52)
RTGSsunday_ts <- ts(RTGSsunday, start=2019, frequency=52)
RTGSmonday_ts <- ts(RTGSmonday, start=2019, frequency=52)
RTGStusday_ts <- ts(RTGStusday, start=2019, frequency=52)
RTGSwednesday_ts <- ts(RTGSwednesday, start=2019, frequency=52)
RTGSthursday_ts <- ts(RTGSthursday, start=2019, frequency=52)

autoplot(RTGSsaturday_ts[,10]) 
# unit root
library(urca)
RTGSsaturday_ts[,10] %>% ur.kpss() %>% summary()

### Arima ba arima 2 ta package motafavete!!!! fpp2 va stats!!!!
## arima seasonal ba tabdil BOXCOX lambda RMSE kamtar fit3 Hydenman book

### day of week estimation
#  model for saturday- cash out

RTGSsaturday_ts[,10]%>%
  Arima(order=c(7,1,4), lambda="0", xreg=fourier(RTGSsaturday_ts[,10],K=7)) -> fitsat 
 summary(fitsat)
 checkresiduals(fitsat)
 
 forcsat <- forecast(fitsat, xreg=fourier(RTGSsaturday_ts[,10], K=7, h=20), level = c(60,80), h = 20)
 autoplot(forcsat)+
   autolayer(forcsat$fitted, series="Fitted")+
   autolayer(RTGSsaturday_ts[,10]) 
 
 autoplot(fitsat$fitted)+
   autolayer(RTGSsaturday_ts[,10]) 
 
#order=c(3,1,1), seasonal=c(1,0,2)
 
#Harmonic hatman hatman diff log bashad!!!
 
 #model 1 feuree
 y <- ts(na.omit(diff(log(RTGSsaturday_ts[,10]))), f=97)
 fit <- Arima(y, order=c(1,0,1), xreg=fourier(y, K=30))
 forecast4 <- forecast(fit, h=20, xreg=fourier(y, K=30, h=20))

 autoplot(forecast4)+
   autolayer(forecast4$fitted, series="Fitted")
   #+autolayer(na.omit(diff(log(RTGSsaturday_ts[,10])))) 
 
 ## vahede zamani yeki nist serie zamani ra bebein b va c
 
b<-na.omit(diff(log(RTGSsaturday_ts[,10])))
c<-forecast4$fitted
c_ts <- ts(forecast4$fitted, start=c(2019,2), frequency=52)
cforecast4_ts <- ts(forecast4, start=c(2019,2), frequency=52)

autoplot(b) +
  autolayer(c_ts)+
  autolayer(cforecast4_ts)

#Rami Krispi and Hydenman

y <- ts(na.omit(diff(log(RTGSsaturday_ts[,10]))), f=96)

 RTGSsaturday_ts_split_out <- ts_split(na.omit(diff(log(RTGSsaturday_ts[,10]))), sample.out = 20)

 train_out <- RTGSsaturday_ts_split_out$train
 test_out <- RTGSsaturday_ts_split_out$test
 
 fit_out <- Arima(train_out, order=c(1,0,1), xreg=fourier(train_out, K=15))
 
 fit_out_fc <- forecast(fit_out,xreg=fourier(test_out, K=15, h=20))
 
 autoplot(forecast(fit_out,xreg=fourier(test_out, K=15, h=20)))
 
 accuracy(fit_out_fc, test_out)

 test_forecast(na.omit(diff(log(RTGSsaturday_ts[,10]))),forecast.obj = fit_out_fc,test = test_out)
 
 # end saturdy cash out
 
### pishbini saturday cash in // vorodi roze shanbe
 
## ketabe hydenman
## khode serie
 
 
 RTGSsaturday_ts[,9]%>%
   Arima(order=c(5,1,3), lambda="0", xreg=fourier(RTGSsaturday_ts[,9],K=10)) -> fitsat1 
 summary(fitsat1)
 checkresiduals(fitsat1)
 
 forcsat1 <- forecast(fitsat1, xreg=fourier(RTGSsaturday_ts[,9], K=10, h=20), level = c(60,80), h = 20)
 autoplot(forcsat1)+
   autolayer(forcsat1$fitted, series="Fitted")+
   autolayer(RTGSsaturday_ts[,9]) 
 
 autoplot(fitsat$fitted)+
   autolayer(RTGSsaturday_ts[,10]) 
 
 
   ## logdiff
 RTGSsaturday_ts[,9]%>%
   Arima(order=c(5,1,1), lambda="0") -> fitsat 
 summary(fitsat)
 checkresiduals(fitsat)
 
 forcsat <- forecast(fitsat,xreg=fourier(test_in, K=7, h=20), level = c(80,90), h = 10)
 autoplot(forcsat)+
   autolayer(forcsat$fitted, series="Fitted")+
   autolayer(RTGSsaturday_ts[,9]) 
 
 autoplot(fitsat$fitted)+
   autolayer(RTGSsaturday_ts[,9]) 
 
 
 
 ##ketabe krispi
 
 y1 <- ts(na.omit(diff(log(RTGSsaturday_ts[,9]))), f=96)
 
 RTGSsaturday_ts_split_in <- ts_split(na.omit(diff(log(RTGSsaturday_ts[,9]))), sample.out = 20)
 
 train_in <- RTGSsaturday_ts_split_in$train
 test_in <- RTGSsaturday_ts_split_in$test
 
 fit_in <- Arima(train_in, order=c(3,0,1), xreg=fourier(train_in, K=7))
 
 fit_in_fc <- forecast(fit_in,xreg=fourier(test_in, K=7, h=20))
 
 autoplot(forecast(fit_in,xreg=fourier(test_in, K=7, h=20)))
 
 accuracy(fit_in_fc, test_in)
 
 test_forecast(na.omit(diff(log(RTGSsaturday_ts[,9]))),forecast.obj = fit_in_fc,test = test_in)
 
 # end satrdy cash in
 
 
 #  model for SUNDAY- cash out
 
 RTGSsunday_ts[,10]%>%
 Arima(order=c(6,1,4), lambda="0", xreg=fourier(RTGSsunday_ts[,10],K=15)) -> fitsun 
 
 summary(fitsun)
 checkresiduals(fitsun)
 
 forcsun <- forecast(fitsun, xreg=fourier(RTGSsunday_ts[,10], K=15, h=20), level = c(60,80), h = 20)
 
 autoplot(forcsun)+
   autolayer(forcsun$fitted, series="Fitted")+
   autolayer(RTGSsunday_ts[,10]) 

 #Harmonic hatman hatman diff log bashad!!!
 
 # end sunday cash out
 
 ### pishbini sunday cash in // vorodi roze yekshanbe
 
 ## ketabe hydenman
 ## khode serie
 
 
 RTGSsunday_ts[,9]%>%
 Arima(order=c(3,1,2), lambda="0", xreg=fourier(RTGSsunday_ts[,9],K=10)) -> fitsun1 
 summary(fitsun1)
 checkresiduals(fitsun1)
 
 forcsun1 <- forecast(fitsun1, xreg=fourier(RTGSsunday_ts[,9], K=10, h=20), level = c(60,80), h = 20)
 
 autoplot(forcsun1)+
   autolayer(forcsun1$fitted, series="Fitted")+
   autolayer(RTGSsunday_ts[,9]) 

 # end sunday cash in

 ## cumulative distibution function mahaneee##
  RTGS %>% filter(month.shamsi == "January") %>% select(net) -> net.farvardin
 plot(ecdf(net.farvardin$net))
 
 if(forcsat1$mean>quantile(in.farvardin$totalin,0.95))
   if(forcsat$mean>quantile(out.farvardin$totalout,0.95))
     
forcsat2 <- mutate(qqqq, mean2 = ifelse(qqqq>quantile(in.farvardin$totalin,0.05), quantile(in.farvardin$totalin,0.05), qqqq))

 ##end cumulative dis func

### takhmin ba motaghyer Boronza!!!
 ## zaher seri zamani neshan az tasire shadid va oft aval sal darad!!!
 ### cash out
 ### motaghyer boronza va eeror term arima p 377 ketab Rami Krispi
 
 md2<- Arima(RTGS_ts[2:577,10], order=c(8,1,6), xreg=cbind(RTGS_ts[2:577,9], RTGS_ts[1:576,9]))
 
 summary(md2)
 checkresiduals(md2)

forc4 <- forecast(md2, level = c(60,80), h = 30,
                   xreg=cbind(RTGS_ts[2:31,9],RTGS_ts[1:30,9]))

autoplot(forc4)+
   autolayer(forc4$fitted, series="Fitted")+
   autolayer(RTGS_ts[,10])+ ylab("Out Flow Rials") +
   ggtitle("Forecast outflows")
 
### in ba estefade feature engineering!!! ln gereftim haman log
 
 md3<- Arima(log(RTGS_ts[1:613,9]), order=c(1,1,2),
             xreg=cbind(log(RTGS_ts[2:613,10]), log(RTGS_ts[1:613,10])))

 summary(md3)
 checkresiduals(md3)
 
 
 forc5 <- forecast(md3, level = c(60,80), h = 30,
                   xreg=cbind(log(RTGS_ts[289:319,10]),log(RTGS_ts[288:318,10])))
 
 
 autoplot(forc5)+
   autolayer(forc5$fitted, series="Fitted")+
   autolayer(log(RTGS_ts[,9]))+ ylab("Log inflow") +
   ggtitle("Forecast inflow")
 
 
### end in ba motaghyer bronza 
 
 
 #Dynamic harmonic regression page430 hygenman
##https://github.com/robjhyndman/forecast/issues/350
## mitavanad seasonal pattern haye motafavet bashad 
## masalan baraye dade haftegi mitavanad ham mahane va ham salanee bashad 
## model diirence va mana shode!!

y2 <- msts(diff(log(RTGS_ts[,9])),seasonal.periods=c(7,365.25),ts.frequency=7)

#model 1 feuree
fit5 <- auto.arima(y2, seasonal=FALSE, xreg=fourier(y2, K=c(3,30)))
fit_f5<- forecast(fit5, xreg=fourier(y2, K=c(3,30), 180), 180)
plot(fit_f5)

# end

## model2 fourue
y3 <- msts(RTGS_ts[,9],seasonal.periods=c(7,288),ts.frequency=7)
##payam system is computationally singular midahad!!!
## bayad dadeha meghyaseshan kochak shavad!!! dar 0.00001 zarb!!!

fit6 <- Arima((0.000001*y3), order=c(0,1,1), xreg=fourier(y3, K=c(2,15)))

fit_f6<- forecast(fit6, xreg=fourier(y3, K=c(2,15), 30), 30)

plot((fit_f6))

#end

##Rami Krespi dar pishbini va hyden man dar harmonic time series
RTGS_ts_split_in1 <- ts_split(y2, sample.out = 60)

train_in1 <- RTGS_ts_split_in1$train
test_in1 <- RTGS_ts_split_in1$test

fit_in1 <- Arima(train_in1, order=c(0,0,1), xreg=fourier(train_in1, K=c(3,80)))

fit_in_fc1 <- forecast(fit_in1, xreg=fourier(y2, K=c(3,80), 60), 60)
accuracy(fit_in_fc1, test_in1)

test_forecast(y2,forecast.obj = fit_in_fc1,test = test_in1)
#(3,0,3)K=c(3,150)
#(1,0,1)K=c(3,150)
#c(0,0,1)K=c(3,80)

####end

## model bedone diff va mana na shode shode!!

y3 <- msts(RTGS_ts[,9],seasonal.periods=c(7,365.25),ts.frequency=7)

#model 1 feuree
fit7 <- auto.arima(y3, seasonal=FALSE, xreg=fourier(y3, K=c(3,30)))
fit_f7<- forecast(fit7, xreg=fourier(y7, K=c(3,30), 180), 180)
plot(fit_f7)

# end

### ertebate bin outliers 
RTGSINoutlier<-tsoutliers(RTGSxts[,1])
RTGSxts[RTGSINoutlier[[1]],1]
plot(RTGSINoutlier[[1]])
RTGSOUT_outlier<-tsoutliers(RTGSxts[,2])
RTGSxts[RTGSOUT_outlier[[1]],2]
      
###end


#### Rami krispin 366
#https:/ / github. com/ PacktPublishing/ Hands- On- Time- Series- Analysis- with- R/ tree/
  #master/ Chapter11
library(fpp2)
library(dplyr)
library(tidyr)
library(zoo)
library(xts)
library(lubridate)
library(forecast)
library(TSstudio)
library(plotly)
library(dplyr)
library(lubridate)
library(stats)
library(datasets)
library(base)
## auto arima

##############################################################
#### estefade az Matt Dancho model modeltime,....

library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(rsample)
library(parsnip)
library(workflows)
library(recipes)
library(earth)
library(randomForest)
library(modeltime.gluonts)
library(reticulate)
library(png)
install_gluonts()

#######
## az in site in modelha amadeh ast
### https://cran.r-project.org/web/packages/modeltime/vignettes/getting-started-with-modeltime.html

fig<- RTGS %>%
  plot_time_series(datee, totalin, .interactive = TRUE)

fig<- RTGS %>%
  plot_time_series(datee, totalout, .interactive = TRUE)
fig

splits <- initial_time_split(RTGS, prop = 0.9)

fig1 <- plot_ly(RTGS, x = ~datee, y = ~totalin, name = 'Cash in flow', type = 'scatter', mode = 'lines') 
fig2 <- fig1 %>% add_trace(y = ~totalout, name = 'Cash out flow', mode = 'lines+markers') 
fig2

# Model 1: auto_arima ----

model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(totalin ~ datee, data = training(splits))


# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(totalin ~ datee + as.numeric(datee) + factor(month(datee, label = TRUE), ordered = F),
      data = training(splits))

# Model 3: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(totalin ~ datee, data = training(splits))
#frequency = 12 observations per 1 year

# Model 4: prophet ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(totalin ~ datee, data = training(splits))
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.

# Model 5: lm ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(totalin ~ as.numeric(datee) + factor(month(datee, label = TRUE),
      ordered = FALSE),data = training(splits))
###end model 5

# Model 6: earth ----
model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 

recipe_spec <- recipe(totalin ~ datee, data = training(splits)) %>%
  step_date(datee, features = "month", ordinal = FALSE) %>%
  step_mutate(datee_num = as.numeric(datee)) %>%
  step_normalize(datee_num) %>%
  step_rm(datee)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))
###End of Model6

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  wflw_fit_mars
)

models_tbl
##################
#################

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl


calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = RTGS[,9]
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 56, # For mobile screens
    .interactive      = interactive
  )


calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )



refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = RTGS)

refit_tbl %>%
  modeltime_forecast(h = "57 days", actual_data = RTGS[,9]) %>%
  plot_modeltime_forecast(
    .legend_max_width = 57, 
    .interactive      = interactive
  )

###https://www.r-bloggers.com/2020/06/introducing-modeltime-tidy-time-series-forecasting-using-tidymodels/

## sadekardan serie zamani be 2 serie zaman va meghdar baraye inflow

totalin_tbl <- RTGS %>%
  select(datee, totalin) %>%
  set_names(c("date", "value")) 

totalin_tbl

totalin_tbl %>%
  plot_time_series(date, value, .interactive = FALSE)

splits1 <- totalin_tbl %>%
  time_series_split(assess = "3 months", cumulative = TRUE)


splits1 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)

model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits1))

model_fit_arima

model_fit_arima1 <- arima_reg(
  seasonal_period          = 1,
  non_seasonal_ar          = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma          = 1,
  seasonal_ar              = 1,
  seasonal_differences     = 0,
  seasonal_ma              = 1
  ) %>%
  set_engine("arima") %>%
  fit(log(value) ~ date, training(splits1))

model_fit_arima1


model_fit_prophet <- prophet_reg() %>%
  set_engine("prophet", yearly.seasonality = TRUE) %>%
  fit(value ~ date, training(splits1))


recipe_spec <- recipe(value ~ date, training(splits1)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()

model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits1))


model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
  set_engine("randomForest")

workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits1))


model_spec_prophet_boost <- prophet_boost() %>%
  set_engine("prophet_xgboost", yearly.seasonality = TRUE) 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits1))

model_table <- modeltime_table(
  model_fit_arima, 
  model_fit_arima1, 
  model_fit_prophet,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost
) 

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits1))

calibration_table

calibration_table %>%
  modeltime_forecast(actual_data = totalin_tbl) %>%
  plot_modeltime_forecast(.interactive = TRUE)


calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)


calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id != 2) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(totalin_tbl) %>%
  modeltime_forecast(h = "1 months", actual_data = totalin_tbl) %>%
  plot_modeltime_forecast(.interactive = TRUE)

## end inflow

## sadekardan serie zamani be 2 serie zaman va meghdar baraye NET flow

net_tbl <- RTGS %>%
  select(datee, net) %>%
  set_names(c("date", "value")) 

net_tbl

net_tbl %>%
  plot_time_series(date, value, .interactive = FALSE)

splits2 <- net_tbl %>%
  time_series_split(assess = "3 months", cumulative = TRUE)


splits2 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive =  TRUE)

model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits2))

model_fit_arima

model_fit_arima1 <- arima_reg(
  non_seasonal_ar          = 3,
  non_seasonal_differences = 1,
  non_seasonal_ma          = 2,
) %>%
  set_engine("arima") %>%
  fit(value ~ date, training(splits2))
#fit(na.omit(log(value)) ~ date, training(splits2))

model_fit_arima1


model_fit_prophet <- prophet_reg() %>%
  set_engine("prophet", yearly.seasonality = TRUE) %>%
  fit(value ~ date, training(splits2))


recipe_spec <- recipe(value ~ date, training(splits2)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 365, K = 10) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()

model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits2))


model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
  set_engine("randomForest")

workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits2))


model_spec_prophet_boost <- prophet_boost() %>%
  set_engine("prophet_xgboost", yearly.seasonality = TRUE) 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits2))

model_table <- modeltime_table(
  model_fit_arima, 
  model_fit_arima1, 
  model_fit_prophet,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost
) 

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits2))

calibration_table

calibration_table %>%
  filter(.model_id != 2) %>%
  modeltime_forecast(actual_data = net_tbl) %>%
  plot_modeltime_forecast(.interactive = TRUE)


calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id != 2) %>%
  # Refit and Forecast Forward
  modeltime_refit(net_tbl) %>%
  modeltime_forecast(h = "1 months", actual_data = net_tbl) %>%
  plot_modeltime_forecast(.interactive = TRUE)



calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)


calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id != 2) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(net_tbl) %>%
  modeltime_forecast(h = "1 months", actual_data = net_tbl) %>%
  plot_modeltime_forecast(.interactive = TRUE)

## end net

###ketabe Hydenman edition 3
###https://otexts.com/fpp3/tsibbles.html
###https://fable.tidyverts.org/
library(fpp3)
library(tsibble)
library(tsibbledata)
library(lubridate)
library(dplyr)
library(fable)
library(tidyverse)

RTGS_tsibble <- RTGS %>% as_tsibble(index=datee)

RTGS_tsibble  %>%
   model(
    ets = ETS(box_cox(totalin, 0.3)),
    arima = ARIMA(log(totalin)),
    snaive = SNAIVE(totalin)
  ) %>%
  forecast(h = "20 days") %>% 
  autoplot(filter(RTGS_tsibble, datee > 2021), level = NULL)


### GARCH MODELS for net onely

library(aTSA)
mod <- arima(RTGS_ts[,11],order = c(1,1,0))
arch.test(mod)
## p-value azmone arch.test BISH az 5% farze h0 pazirofte misshod!!
## inja rad ast seri net daraye hetroscedasticity ast!!!
garchSpec <- ugarchspec(
  variance.model=list(model="sGARCH",
                      garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(1,1)), 
  distribution.model="std")

garchFit <- ugarchfit(spec=garchSpec, data=RTGS_ts[,11])

coef(garchFit)
rhat <- garchFit@fit$fitted.values
plot.ts(rhat)
hhat <- ts(garchFit@fit$sigma^2)
plot.ts(hhat)


####https://business-science.github.io/timetk/articles/TK03_Forecasting_Using_Time_Series_Signature.html
##https://github.com/business-science/modeltime.gluonts/issues/4

Sys.which("python")

reticulate::py_install(
  envname  = "gpu-gluonts",
  python_version = "3.6",
  packages = c(
    "mxnet",
    "gluonts",
    "pandas",
    "numpy",
    "pathlib"
  ),
  method = "conda",
  pip = TRUE
)

#https://cran.r-project.org/web/packages/reticulate/vignettes/calling_python.html

library(reticulate)

np <- import('numpy')
py_install("pandas", pip = TRUE)
np <- import('pandas')
reticulate::py_available("gluonts")
reticulate::py_available("pandas")

##f<- reticulate::pandas.DataFrame({"a":[2,5]})

###kar roye transform dadeha 1400-5-5

RTGS$trans1 <- log(RTGS[,9]/RTGS[,10])
RTGS$trans2 <- RTGS[,9]/RTGS[,10]

RTGS$trans1 <- unlist(RTGS$trans1)
RTGS$trans2 <- unlist(RTGS$trans2)


ggplot(data = RTGS, aes(x= datee, y= trans2))+
  geom_line()+
  ylab("LnTransformIntoOut") +
  xlab("Time")

