library(tidyverse)
library(lubridate)
library(ggpubr)
library(qqplotr)
library(hms)
library(Hmisc)
library(PerformanceAnalytics)
library(dplyr)

setwd("~/GitHub/Project/course-paper-or-presentation-MichaelMarcaccio/Data")

orig_callData <- read.csv("CSV Interaction Call Center.csv")
orig_agentData <- read.csv("CSV Agent Table.csv")


callData_withTimes <- orig_callData

# Use lubridate
callData_withTimes$START15 <- mdy_hm(orig_callData$START15)
callData_withTimes$STARTTIME <- mdy_hm(orig_callData$STARTTIME)
callData_withTimes$ENDTIME <- mdy_hm(orig_callData$ENDTIME)

range(callData_withTimes$STARTTIME) #Remove the 3rd, so ends at a full week (Saturday, November 1st, 2022)



callData_withTimes <- callData_withTimes %>% mutate(StartTIME_DATEONLY = floor_date(callData_withTimes$STARTTIME, "day"))
callData_withTimes <- callData_withTimes %>% mutate(StartTIME_DATEONLY = ymd(callData_withTimes$StartTIME_DATEONLY))

str(callData_withTimes$StartTIME_DATEONLY)
callData_withOut_10_3_22 <- callData_withTimes %>% filter(StartTIME_DATEONLY != "2022-10-03") 

str(callData_withOut_10_3_22)



callData_withOut_10_3_22_nChat <- callData_withOut_10_3_22 %>% filter(ï..INTERACTIONTYPE!= "Chat") 

## REMOVING VARIABLES:
removeVarList1 <- c("INTERACTIONTYPE", "CALLDATE", "INTRCREATETYPNAME", "INTR_ID", "QUEUEDYN", "INTERACTIONID", "EVOLVEID", "QUEUED", "ANSWEREDYN", 
                    "ABANDONED", "ABANDONEDYN", "EXCLUDESERVICELEVELYN", "INTERACTIONOUTCOME...14", "LASTINTERACTIONSTATE")


callData_reduced1<-callData_withOut_10_3_22_nChat[, !names(callData_withOut_10_3_22_nChat) %in% c("INTERACTIONTYPE", "CALLDATE", "INTRCREATETYPNAME", "INTR_ID", "QUEUEDYN", "INTERACTIONID", "EVOLVEID", "QUEUED", "ANSWEREDYN", 
                                                                                                  "ABANDONED", "ABANDONEDYN", "EXCLUDESERVICELEVELYN", "INTERACTIONOUTCOME...14", "LASTINTERACTIONSTATE")] # delete unneccesary Variables


#Removes Nulls
callData_rduce1_nNlls1 <- callData_reduced1[!is.na(callData_reduced1$NBRAGENTS),] # Nulls NBRAGENTS

callData_rduce1_nNlls2 <- callData_rduce1_nNlls1[!is.na(callData_rduce1_nNlls1$WAITTIME),] # Nulls WAITTIME

callData_rduce1_nNlls3 <- callData_rduce1_nNlls2[!is.na(callData_rduce1_nNlls2$AGENTTALKDURATION),] # Nulls NBRAGENTS 

callData_rduce1_nNlls4 <- callData_rduce1_nNlls3[!is.na(callData_rduce1_nNlls3$AGENTHANDLETIME),] # Nulls NBRAGENTS 

noNulls = na.omit(callData_rduce1_nNlls4)




inOrder <- c(16, 1, 4, 8, 9, 10, 11, 12, 3, 2, 13, 17, 5, 6, 7, 14, 15, 18) #Reorganizing Variables

callData_rduce2 <- callData_rduce1_nNlls4[,inOrder]



INTERACTIONOUTCOME_times <- callData_rduce2 %>%
  group_by(INTERACTIONOUTCOME.1) %>%
  dplyr::summarize(count = n(), 
                   wait_avg = mean(WAITTIME), 
                   talk_avg = mean(AGENTTALKDURATION),
                   hold_avg = mean(HOLDTIME), 
                   wrap_avg = mean(WRAPUPTIME), 
                   totalTalk_avg = mean(AGENTHANDLETIME))





callData_rduce2$CallFreq <- as.numeric(table(callData_rduce2$CALLERID)[callData_rduce2$CALLERID])

inorder2 <- seq(1,19)

inOrder2 <- c(1, 2, 19, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
callData_rduce3 <- callData_rduce2[,inOrder2]



callData_rduce3$callRepeat0_1 <- 0
callData_rduce3$callRepeat0_1[callData_rduce3$CallFreq > 1] <- 1




inorder2 <- 1:20

inOrder2 <- c(1, 2, 3, 20, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)

callData_rduce4 <- callData_rduce3[,inOrder2]

str(callData_rduce4)
#
# agentByCountAndHoldWait <- callData_rduce4 %>% group_by(AGENT) %>% summarize(count = n(), hold_avg = mean(HOLDTIME), wait_avg = mean(WAITTIME), talk_avg = mean(AGENTTALKDURATION))
#
# groupingAlt_Specific_ByCountAndHoldWait <- callData_rduce4 %>% group_by(INTERACTIONDISPOSITION) %>% summarize(count = n(), hold_avg = mean(HOLDTIME), wait_avg = mean(WAITTIME), talk_avg = mean(AGENTTALKDURATION))
#
# QUEUENAME_ByCountAndHoldWait <- callData_rduce4 %>% group_by(QUEUENAME) %>% summarize(count = n(), hold_avg = mean(HOLDTIME), wait_avg = mean(WAITTIME), talk_avg = mean(AGENTTALKDURATION))
#
# QUEUENAME_ByCountAndHoldWait <- callData_rduce4 %>% group_by(QUEUENAME) %>% summarize(count = n(), hold_avg = mean(HOLDTIME), wait_avg = mean(WAITTIME), talk_avg = mean(AGENTTALKDURATION))
#
#
# 
# 
# 
# 
# justWaitAndHold <- callData_rduce4 %>% select(WAITTIME, HOLDTIME)
# 
# 
# summary(callData_rduce4$WAITTIME)
# hist(callData_rduce4$WAITTIME)
# table(callData_rduce4$WAITTIME)
# length(callData_rduce4$WAITTIME[callData_rduce4$WAITTIME < 20])/length(callData_rduce4$WAITTIME)
# length(callData_rduce4$WAITTIME[callData_rduce4$WAITTIME < 180])/length(callData_rduce4$WAITTIME)
# length(callData_rduce4$WAITTIME[callData_rduce4$WAITTIME >= 180 & callData_rduce4$WAITTIME < 360])/length(callData_rduce4$WAITTIME)
# 
# summary(callData_rduce4$HOLDTIME)
# hist(callData_rduce4$HOLDTIME)
# length(callData_rduce4$HOLDTIME[callData_rduce4$HOLDTIME < 20])/length(callData_rduce4$HOLDTIME)
# length(callData_rduce4$HOLDTIME[callData_rduce4$HOLDTIME < 180])/length(callData_rduce4$HOLDTIME)
# length(callData_rduce4$HOLDTIME[callData_rduce4$HOLDTIME >= 180 & callData_rduce4$HOLDTIME < 360])/length(callData_rduce4$HOLDTIME)
# 
# summary(callData_rduce4$HOLDTIME)
# hist(callData_rduce4$HOLDTIME)
# length(callData_rduce4$HOLDTIME[callData_rduce4$HOLDTIME < 20])/length(callData_rduce4$HOLDTIME)
# length(callData_rduce4$HOLDTIME[callData_rduce4$HOLDTIME < 180])/length(callData_rduce4$HOLDTIME)
# length(callData_rduce4$HOLDTIME[callData_rduce4$HOLDTIME >= 180 & callData_rduce4$HOLDTIME < 360])/length(callData_rduce4$HOLDTIME)

################
str(callData_rduce4)
###############

# 
# talkHoldTime <- callData_rduce4 %>% mutate(talkHoldTime1 = HOLDTIME + AGENTTALKDURATION, talkHoldTime2 = AGENTHANDLETIME - WAITTIME) %>% select(talkHoldTime1, talkHoldTime2)
# 
# talkHoldTime2 <- callData_withTimes %>% mutate(talkHoldTime1 = AGENTHANDLETIME, talkHoldTime2 = HOLDTIME + WRAPUPTIME + AGENTTALKDURATION) %>% select(talkHoldTime1, talkHoldTime2)

unique(callData_rduce4$QUEUENAME)

unique(callData_rduce4$INTERACTIONDISPOSITION)







goalTime_queue <- callData_rduce4 %>% select(ANSWEREDGOALTIME, WAITTIME, QUEUENAME)
goalTime_queue2 <- callData_rduce4 %>% select(ANSWEREDGOALTIME, AGENTTALKDURATION, QUEUENAME)
goalTime_interact <- callData_rduce4 %>% select(ANSWEREDGOALTIME, WAITTIME, INTERACTIONDISPOSITION)
goalTime_interact2 <- callData_rduce4 %>% select(ANSWEREDGOALTIME, AGENTTALKDURATION, INTERACTIONDISPOSITION)

INTERACTIONDISPOSITION_goalTime2 <- callData_rduce4 %>% 
  select(ANSWEREDGOALTIME, WAITTIME, INTERACTIONDISPOSITION) %>% 
  filter(ANSWEREDGOALTIME == 0) %>%
  group_by(INTERACTIONDISPOSITION) %>% 
  summarise(max_wait = min(WAITTIME))


goalTime <- callData_rduce4 %>% select(ANSWEREDGOALTIME, WAITTIME)

# ENTERING TIME ANALYSIS BELOW
#### CREATE DATA

callData_wMYtime <- callData_rduce4 %>%
  mutate(Start15_Mike = round_date(STARTTIME, "15 minutes"),
         month = month(callData_rduce4$STARTTIME),
         year = year(callData_rduce4$STARTTIME),
         YMchar = substr(callData_rduce4$STARTTIME,0,7),
         STARTTIME_time = as_hms(callData_rduce4$STARTTIME),
         ENDTIME_time = as_hms(callData_rduce4$ENDTIME),
         START15_time = as_hms(Start15_Mike),
         Start15_time_format = format(as.POSIXct(START15_time), format = "%H:%M:%S"),
         dayOfWeek =  wday(callData_rduce4$STARTTIME))

callData_wMYtime$InteractionValue <- 0
callData_wMYtime$InteractionValue[callData_wMYtime$INTERACTIONOUTCOME.1 == "Handled"] <- 1
callData_wMYtime$InteractionValue[callData_wMYtime$INTERACTIONOUTCOME.1 == "Abandoned"] <- -1
callData_wMYtime$InteractionValue[callData_wMYtime$INTERACTIONOUTCOME.15 == "Leave Number"] <- -1

rearrange3 <- seq(1,34)
rearrange3 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29)
callData_wMYtime = callData_wMYtime[,rearrange3]

callData_rduce4$Start15_Mike <- 0
callData_rduce4$Start15_Mike[]

callData_wMYtime$yearSEQ <- 0
callData_wMYtime$yearSEQ[callData_wMYtime$year == 2020] <- 1
callData_wMYtime$yearSEQ[callData_wMYtime$year == 2021] <- 2
callData_wMYtime$yearSEQ[callData_wMYtime$year == 2022] <- 3

#Convert Everything to start at 0
callData_wMYtime$monthSEQ <- 0
callData_wMYtime$monthSEQ <- (callData_wMYtime$month + ((callData_wMYtime$yearSEQ-1) * 12)) - 1


callData_wMYtime$daySEQ <- 0
callData_wMYtime$daySEQ <- (callData_wMYtime$DAYOFYEAR + (((callData_wMYtime$yearSEQ-1) * 365) + 1)) - 34

callData_wMYtime$weekSEQ <- 0
callData_wMYtime$weekSEQ <- (callData_wMYtime$WEEKOFYEAR + (((callData_wMYtime$yearSEQ-1) * 52) )) - 5


#### BY ENTIRE YEAR

ggplot(callData_wMYtime, aes(fill = factor(year))) + geom_histogram(aes(factor(year)), stat = "count", breaks = 33) +
  xlab("") + ylab("Number Of Calls") +
  labs(title = "BY YEAR -- Call Volume", fill = "  Year = ") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))


#### BY MONTH
ggplot(callData_wMYtime, aes(fill = factor(year))) + geom_histogram(aes(YMchar), stat = "count", breaks = 33) +
  xlab("") + ylab("Number Of Calls") +
  labs(title = "By Month of Call Volume", fill = "  Year = ") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))

#### By Week
ggplot(callData_wMYtime, aes(fill = factor(year))) + geom_histogram(aes(factor(weekSEQ)), stat = "count", bins = 30) +
  xlab("Each Week in Analysis") + ylab("Total Calls") +
  labs(title = "Week Total Calls Grouped By Year", fill = "  Year = ") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))

#### TIME

callData_wMYtime$Start15_unix <- as.numeric(callData_wMYtime$START15)
callData_wMYtime$StartTime_unix <- as.numeric(callData_wMYtime$STARTTIME)
callData_wMYtime$ENDTIME_unix <- as.numeric(callData_wMYtime$ENDTIME)

ggplot(callData_wMYtime, aes(fill = factor(year))) + geom_histogram(aes(as.character(START15_time)), stat = "count", breaks = 10) +
  xlab("") + ylab("Number Of Calls") +
  labs(title = "Call Volume By Time -- Grouped By YEAR", fill = "  Year = ") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))


ggplot(callData_wMYtime, aes(fill = factor(year))) + geom_histogram(aes(STARTTIME_time), stat = "count", breaks = 10) +
  xlab("") + ylab("Number Of Calls") +
  labs(title = "Call Volume By Time -- Grouped By YEAR", fill = "  Year = ") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))





CallsFor15MinPeriodsbyWeekday <- callData_wMYtime %>% dplyr::group_by(weekSEQ,Start15_Mike, .group = FALSE) %>%
  dplyr::summarize(callsper15minperweek = n(), 
                   Start15_Mike = first(as.character((as_hms(Start15_Mike)))), 
                   dayOfWeek = first(dayOfWeek)) %>% as_tibble()


CallsFor15MinPeriodsbyMonThruFri <- CallsFor15MinPeriodsbyWeekday %>% filter(dayOfWeek < 7)

# CallsFor15MinPeriodsbyWeekday$charDayOfWeek <- ""
# CallsFor15MinPeriodsbyWeekday$charDayOfWeek[CallsFor15MinPeriodsbyWeekday$dayOfWeek == 2] <- "Mon"
# CallsFor15MinPeriodsbyWeekday$charDayOfWeek[CallsFor15MinPeriodsbyWeekday$dayOfWeek == 3] <- "Tues"
# CallsFor15MinPeriodsbyWeekday$charDayOfWeek[CallsFor15MinPeriodsbyWeekday$dayOfWeek == 4] <- "Wed"
# CallsFor15MinPeriodsbyWeekday$charDayOfWeek[CallsFor15MinPeriodsbyWeekday$dayOfWeek == 5] <- "Thurs"
# CallsFor15MinPeriodsbyWeekday$charDayOfWeek[CallsFor15MinPeriodsbyWeekday$dayOfWeek == 6] <- "Fri"
# CallsFor15MinPeriodsbyWeekday$charDayOfWeek[CallsFor15MinPeriodsbyWeekday$dayOfWeek == 7] <- "Sat"


ggplot(CallsFor15MinPeriodsbyWeekday, aes(fill = factor(dayOfWeek))) + geom_bar(aes(Start15_Mike), stat = "count") +
  xlab("") + ylab("Number Of Calls") + 
  labs(title = "Call Volume By 15 Minute Periods -- Stacked By Weekday", fill = "Day Of Week") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))


ggplot(CallsFor15MinPeriodsbyMonThruFri, aes(fill = factor(dayOfWeek))) + geom_bar(aes(Start15_Mike), stat = "count") +
  xlab("") + ylab("Number Of Calls") +
  labs(title = "Call Volume By 15 Minute Periods -- Grouped By Weekday", fill = "  Day Of Week = ") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))

ggplot(callData_wMYtime, aes(fill = factor(year))) + geom_histogram(aes(weekSEQ), stat = "bin", bins = 20) +
  xlab("") + ylab("Number Of Calls") +
  labs(title = "By Week of Call Volume", fill = "  Year = ") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))



callData_wMYtime %>% group_by(weekSEQ) %>% dplyr::summarize(count = n())

callData_wMYtime %>% group_by(START15_time) %>% dplyr::summarize(count = n())



str(callData_wMYtime)


#  **DOESN'T AVERAGE OUT VALUES BETWEEN DAYS OF THE WEEK**
6 # : days of the week (each occur different numbers times)
47 # : periods of 15 minutes across any day (each occur differenet number of times)
213 # ---> 15 minute intervals across each week (each occur different numbers times)
#  **DOESN'T AVERAGE OUT VALUES BETWEEN DAYS OF THE WEEK**


# FAILS ACCOUNT FOR DISCREPENCIES BETWEEN DAYS OF THE WEEK**
139 # ---> weeks in all the data
27377 # ---> 15 minute intervals in all the data
# FAILS ACCOUNT FOR DISCREPENCIES BETWEEN DAYS OF THE WEEK**



### ALL 15 MINUTES PERIODS FOR EACH DAY OF WEEK
forEach15MinutePeriodsAllWeek <- callData_wMYtime %>% group_by(START15_time, dayOfWeek, .groups = FALSE) %>%
  dplyr::summarise(numberOfWeeks = n_distinct(weekSEQ), 
                   avgNumAgents = mean(NBRAGENTS), 
                   avgNumOfCalls = n()/numberOfWeeks,
                   avgValueInteractionResponse = mean(InteractionValue)) %>% 
  mutate(ratioAgentsPerCall = avgNumAgents / avgNumOfCalls, callsPerAgent = avgNumOfCalls / avgNumAgents) %>% as_tibble() 


forEach15MinutePeriodAllWeek2 <- callData_wMYtime %>% dplyr::group_by(START15_time, dayOfWeek, .groups = FALSE) %>%
  dplyr::summarise(numberOfWeeks = n_distinct(weekSEQ), 
                   totalNumOfCalls= n(), 
                   avgNumOfCalls = n()/numberOfWeeks,
                   avgNumAgents = mean(NBRAGENTS),
                   avgCallTime = mean(AGENTHANDLETIME), 
                   avgGoalMet = mean(ANSWEREDGOALTIME), 
                   avgWaitTime = mean(WAITTIME),
                   avgInteractValue = mean(InteractionValue),
                   avgGoalMet = mean(ANSWEREDGOALTIME)) %>%  
  mutate(ratioAgentsPerCall = avgNumAgents / avgNumOfCalls, callsPerAgent = avgNumOfCalls / avgNumAgents) %>% as_tibble() 


allUnique15MinPeriodsAnyDay <- callData_wMYtime %>% group_by(Start15_time_format) %>% dplyr::summarize(numberOfCallsPer15MinPeriod = n())


# TREATS A 15 MINUTE PERIOD THAT HAPPENS LESS OFTEN (1 to 20), THE SAME WEIGHT AS A TIME PERIOD HAPPENING 189
forEachWeekDayTheAvgOfEach15MinutePeriod_ALT <- forEach15MinutePeriodAllWeek2 %>% group_by(dayOfWeek) %>%
  dplyr::summarise(numberOfTimePeriods = n(),
                   totalNumOfCalls = sum(totalNumOfCalls),
                   avgNumCalls = (sum(totalNumOfCalls)/n()),
                   avgNumAgents = mean(avgNumAgents),
                   callsPerAgent = totalNumOfCalls / avgNumAgents,
                   avgCallTime = mean(avgCallTime),
                   avgWaitTime = mean(avgWaitTime),
                   avgGoalMet = mean(avgGoalMet),
                   avgInteractValue = mean(avgInteractValue),
  )


groupDayOfWeek <- callData_wMYtime %>% group_by(dayOfWeek) %>% summarise(countDaysPerWeekDay = n_distinct(daySEQ))
groupDayOfWeek


forEachWeekDayTheAvgOfEach15MinutePeriod <- callData_wMYtime %>%
  group_by(dayOfWeek) %>% dplyr::summarise(totalNumOfCalls = n(),
                                           avgNumAgents = mean(NBRAGENTS),
                                           callsPerAgent = totalNumOfCalls / avgNumAgents,
                                           avgCallTime = mean(AGENTHANDLETIME),
                                           avgWaitTime = mean(WAITTIME),
                                           avgGoalMet = mean(ANSWEREDGOALTIME),
                                           avgInteractValue = mean(InteractionValue))

forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrect <- 0
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrect[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 2] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 36 / 119)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrect[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 3] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 36 / 136)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrect[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 4] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 36 / 137)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrect[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 5] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 36 / 136)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrect[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 6] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 44 / 138)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrect[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 7] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 18 / 133)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrectAllDay <- 0
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrectAllDay[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 2] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 36)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrectAllDay[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 3] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 36)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrectAllDay[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 4] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 36)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrectAllDay[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 5] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 36)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrectAllDay[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 6] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 44)
forEachWeekDayTheAvgOfEach15MinutePeriod$avgCallsCorrectAllDay[forEachWeekDayTheAvgOfEach15MinutePeriod$dayOfWeek == 7] <- (forEachWeekDayTheAvgOfEach15MinutePeriod$totalNumOfCalls / 18)
forEachDayOfWeekTheAvgEach15MinPeriod_final <- forEachWeekDayTheAvgOfEach15MinutePeriod %>% mutate(callsPerAgent = (avgCallsCorrect/avgNumAgents))

forEachWeekDayTheAvgOfEach15MinutePeriod_ALT
forEachWeekDayTheAvgOfEach15MinutePeriod
mean(forEachWeekDay$avgNumAgents[1:5])
length(callData_wMYtime)

forEachDayOfWeekTheAvgEach15MinPeriod_final

str(forEach15MinutePeriodAllWeek2)
summary(forEach15MinutePeriodAllWeek2)
str(forEachDayOfWeekTheAvgEach15MinPeriod_final)
summary(forEachDayOfWeekTheAvgEach15MinPeriod_final)

############
## TIME COMPLEXITIES
############


#  **DOESN'T AVERAGE OUT VALUES BETWEEN DAYS OF THE WEEK**

6 # : days of the week *in any week*
summary(groupDayOfWeek$countDaysPerWeekDay) # <----- (each occur different number of times)

47 # : 15 minutes intervals *across any day*
summary(forEach15MinutePeriodAllWeek2$numberOfWeeks) # <----- (each occur different number of times)

213 # ---> 15 minute intervals *across any week* 
summary(allUnique15MinPeriodsAnyDay$numberOfCallsPer15MinPeriod) # <----(each occur different number of times)

#  **DOESN'T AVERAGE OUT VALUES BETWEEN DAYS OF THE WEEK**




# FAILS ACCOUNT FOR DISCREPENCIES BETWEEN DAYS OF THE WEEK** 

139 # ---> weeks *in all the data*
971 # days *in all the data*
27377 # ---> 15 minute intervals *in all the data*

# FAILS ACCOUNT FOR DISCREPENCIES BETWEEN DAYS OF THE WEEK**


### CALL TIME, VALUES ABOUT THE CALL, etc ARE DATA EXPLICITLY ABOUT THAT CALL (the row itself)
### NUMBER OF AGENTS REFLECT THE GENREAL CONDITION WHILE THAT CALL TOOK PLACE
##    ## IN RESULT, WE HAVE TO NORMLAIZE THE NUMBER OF AGENTS BY NUMBER OF CALL DURING THAT PERIOD RECORDED
##    ## BEFORE WE RUN A MODEL COMPARING BOTH VALUES







# dependent variables predicted
avgAgentsPerEachDayOfWeek <- callData_wMYtime %>% group_by(Start15_Mike) %>% summarise(avgNumberOfAgents = as.numeric(mean(NBRAGENTS)), 
                                                                                       dayOfWeek = first(dayOfWeek), weekSEQ = first(weekSEQ))
avgWaitTimePerEachDayOfWeek <- callData_wMYtime %>% group_by(dayOfWeek) %>% summarise(avgWaitTime = as.numeric(mean(WAITTIME)))
avgHandleTimePerEachDayOfWeek <- callData_wMYtime %>% group_by(dayOfWeek) %>% summarise(avgHandleTime = as.numeric(mean(AGENTHANDLETIME)))
avgGoalMetPerEachDayOfWeek <- callData_wMYtime %>% group_by(dayOfWeek) %>% summarise(avgGoalMet = as.numeric(mean(ANSWEREDGOALTIME)))



avgCallsPerEachDayOfWeek <- callData_wMYtime %>% group_by(dayOfWeek) %>% summarise(numberOfCalls = as.numeric(n()))


callsPerEachDayOfWeek <- callData_wMYtime %>% group_by(dayOfWeek) %>% summarise(numberOfCalls = as.numeric(n()))

callsPerEachDayOfWeek$numberOfCalls <- as.numeric(callsPerEachDayOfWeek$numberOfCalls)

callsPerEachDayOfWeek2 <- callsPerEachDayOfWeek %>% mutate(averageCallsPerWeekDay = numberOfCalls / groupDayOfWeek$countDaysPerWeekDay)

summary(callData_wMYtime$weekSEQ)









averageCallsWeekday_SUM = sum(callsPerEachDayOfWeek2$averageCallsPerWeekDay)

callsPerEachDayOfWeek2 <- callsPerEachDayOfWeek2 %>% mutate(ratioDayPerWeek = averageCallsPerWeekDay / 3478.18)

callsPerWeek <- callData_wMYtime %>% group_by(weekSEQ) %>% summarise(numberOfCalls = n())

meanTotalCallsPerWeek <- mean(callsPerWeek$numberOfCalls)

callsPerEachDayOfWeek3 <- callsPerEachDayOfWeek2 %>% mutate(projecetedCallsPerWeekDay = meanTotalCallsPerWeek * ratioDayPerWeek)







CallsFor15MinPeriodsbyWeekday <- callData_wMYtime %>% group_by(year, weekSEQ) %>%
  dplyr::summarize(numberOfAgents = mean(NBRAGENTS)) %>% as_tibble() 


#AgentsPerWeekByYear <- callData_wMYtime %>% group_by(year, weekSEQ) %>%
  #summarise(n = n(), numberOfAgents = mean(NBRAGENTS)) %>% dplyr::summarise(numberOfCalls = mean(n), numberOfAgents = first) %>%
  #mutate(differenceFromTotalMean = averageCallsWeekday_SUM - numberOfCalls, ratioOfTotalMean = numberOfCalls/averageCallsWeekday_SUM)


##############################
###### Set up Regression
#############################

callsPerEach15MinInterval <- callData_wMYtime %>% group_by(Start15_Mike) %>%
  summarise(numberOfCalls = as.numeric(n()),
            avgNumberOfAgents = mean(NBRAGENTS),
            avgWaitTime = mean(WAITTIME),
            avgAgentHandleTime = mean(AGENTHANDLETIME),
            avgAnswerGoalTimeMet = mean(ANSWEREDGOALTIME),
            avgScoreAnswerGoalTimeMet = mean(InteractionValue)) %>%  
  mutate(ratioCallsPerAgent = numberOfCalls/avgNumberOfAgents) %>% select(-Start15_Mike)


callsPerWeek <- callData_wMYtime %>% group_by(weekSEQ) %>%
  summarise(numberOfCalls = as.numeric(n()),
            avgNumberOfAgents = mean(NBRAGENTS),
            avgWaitTime = mean(WAITTIME),
            avgAgentHandleTime = mean(AGENTHANDLETIME),
            avgAnswerGoalTimeMet = mean(ANSWEREDGOALTIME),
            avgScoreAnswerGoalTimeMet = mean(InteractionValue)) %>% 
  mutate(ratioCallsPerAgent = numberOfCalls/avgNumberOfAgents) %>% select(-weekSEQ)



cor(callsPerEach15MinInterval, method = "pearson")

chart.Correlation(callsPerWeek, histogram=TRUE, pch=19)


timePeriods_n213 <- forEach15MinutePeriodAllWeek2[,]

weekDayPeriods_n6 <- forEachDayOfWeekTheAvgEach15MinPeriod_final

#Correlations
cor.test(timePeriods_n213$avgWaitTime, timePeriods_n213$avgCallTime) # good
cor.test(weekDayPeriods_n6$avgWaitTime, weekDayPeriods_n6$avgCallTime) # good

#Add Agents to Reduce Wait Time
cor.test(timePeriods_n213$callsPerAgent, timePeriods_n213$avgWaitTime) # good
cor.test(weekDayPeriods_n6$callsPerAgent, weekDayPeriods_n6$avgWaitTime) # not good

#Add Agents to Reduce Call Time
cor.test(timePeriods_n213$callsPerAgent, timePeriods_n213$avgCallTime) # good
cor.test(weekDayPeriods_n6$callsPerAgent, weekDayPeriods_n6$avgCallTime) # not good


#Add Agents to Reduce Wait Goal Not Being Met
cor.test(timePeriods_n213$callsPerAgent, timePeriods_n213$avgGoalMet) # good
cor.test(weekDayPeriods_n6$callsPerAgent, weekDayPeriods_n6$avgGoalMet) # not good


#Add Agents to Reduce Calls not being "Handled"
cor.test(timePeriods_n213$callsPerAgent, timePeriods_n213$avgInteractValue) # not Good-Correlation doesnt make sense
cor.test(weekDayPeriods_n6$callsPerAgent, weekDayPeriods_n6$avgInteractValue) # not Good



# square values for polynomial relationships

callsPerAgent_sq = timePeriods_n213$callsPerAgent * timePeriods_n213$callsPerAgent
avgWaitTime_sq = timePeriods_n213$avgWaitTime * timePeriods_n213$avgWaitTime
avgCallTime_sq= timePeriods_n213$avgCallTime * timePeriods_n213$avgCallTime
avgGoalMet_sq= timePeriods_n213$avgGoalMet * timePeriods_n213$avgGoalMet
avgInteractValue_sq= timePeriods_n213$avgInteractValue * timePeriods_n213$avgInteractValue

summary(lm(avgWaitTime ~ callsPerAgent, data = timePeriods_n213))
summary(lm(avgWaitTime ~ callsPerAgent + callsPerAgent_sq, data = timePeriods_n213))



summary(lm(avgCallTime ~ callsPerAgent, data = timePeriods_n213))
summary(lm(avgCallTime ~ callsPerAgent + callsPerAgent_sq, data = timePeriods_n213))



summary(lm(avgGoalMet ~ callsPerAgent, data = timePeriods_n213))
summary(lm(avgGoalMet ~ callsPerAgent + callsPerAgent_sq, data = timePeriods_n213))


summary(lm(avgInteractValue ~ callsPerAgent, data = timePeriods_n213))
summary(lm(avgInteractValue ~ callsPerAgent + callsPerAgent_sq, data = timePeriods_n213))





#Other Models

lm(avgNumberOfAgents ~ numberOfCalls, data = callsPerEach15MinInterval)
summary(lm(avgNumberOfAgents ~ numberOfCalls, data = callsPerEach15MinInterval))

cor.test(callsPerEach15MinInterval$avgWaitTime, callsPerEach15MinInterval$avgAgentHandleTime)
cor.test(callsPerWeek$avgWaitTime, callsPerWeek$avgAgentHandleTime)

cor.test(callsPerEach15MinInterval$ratioCallsPerAgent, callsPerEach15MinInterval$avgAgentHandleTime)
cor.test(callsPerWeek$ratioCallsPerAgent, callsPerWeek$avgAgentHandleTime)

cor.test(callsPerEach15MinInterval$avgWaitTime, callsPerEach15MinInterval$ratioCallsPerAgent)
cor.test(callsPerWeek$avgWaitTime, callsPerWeek$ratioCallsPerAgent)


cor.test(callsPerEach15MinInterval$avgScoreAnswerGoalTimeMet, callsPerEach15MinInterval$ratioCallsPerAgent)
cor.test(callsPerWeek$avgScoreAnswerGoalTimeMet, callsPerWeek$ratioCallsPerAgent)


lm(avgWaitTime ~ ratioCallsPerAgent, data = callsPerEach15MinInterval)
summary(lm(avgWaitTime ~ ratioCallsPerAgent, data = callsPerEach15MinInterval))



lm(avgWaitTime ~ numberOfCalls / avgNumberOfAgents, data = callsPerEach15MinInterval)
summary(lm(avgWaitTime ~ numberOfCalls / avgNumberOfAgents + numberOfCalls^2/avgNumberOfAgents^2, data = callsPerEach15MinInterval))



lm(avgWaitTime ~ numberOfCalls / avgNumberOfAgents, data = callsPerEach15MinInterval)
summary(lm(avgWaitTime ~ avgNumberOfAgents + avgNumberOfAgents:numberOfCalls, data = callsPerEach15MinInterval)) 

lm(avgWaitTime ~ numberOfCalls / avgNumberOfAgents, data = callsPerEach15MinInterval)
summary(lm(avgWaitTime ~ avgNumberOfAgents + avgNumberOfAgents:numberOfCalls + avgNumberOfAgents^2 + avgNumberOfAgents^2:numberOfCalls^2, data = callsPerEach15MinInterval))


lm(avgWaitTime ~ ratioCallsPerAgent, data = callsPerEach15MinInterval)
summary(lm(avgWaitTime ~ ratioCallsPerAgent, data = callsPerEach15MinInterval))


lm(avgAnswerGoalTimeMet ~ ratioCallsPerAgent, data = callsPerEach15MinInterval)
summary(lm(avgAnswerGoalTimeMet ~ ratioCallsPerAgent, data = callsPerEach15MinInterval))


lm(ratioCallsPerAgent ~ avgWaitTime + avgAgentHandleTime + avgScoreAnswerGoalTimeMet, data = callsPerEach15MinInterval)
summary(lm(ratioCallsPerAgent ~ avgWaitTime + avgAgentHandleTime + avgScoreAnswerGoalTimeMet, data = callsPerEach15MinInterval))


avgWaitTime_sq <- callsPerEach15MinInterval$avgWaitTime * callsPerEach15MinInterval$avgWaitTime
avgAgentHandleTime_sq <- callsPerEach15MinInterval$avgAgentHandleTime * callsPerEach15MinInterval$avgAgentHandleTime

lm(ratioCallsPerAgent ~ avgWaitTime + avgWaitTime_sq + avgAgentHandleTime + avgAgentHandleTime_sq + avgScoreAnswerGoalTimeMet, data = callsPerEach15MinInterval)
summary(lm(ratioCallsPerAgent ~ avgWaitTime + avgWaitTime_sq + avgAgentHandleTime + avgAgentHandleTime_sq + avgScoreAnswerGoalTimeMet, data = callsPerEach15MinInterval))


avgWaitTime_sq <- callsPerWeek$avgWaitTime * callsPerWeek$avgWaitTime
avgAgentHandleTime_sq <- callsPerWeek$avgAgentHandleTime * callsPerWeek$avgAgentHandleTime


lm(ratioCallsPerAgent ~ avgWaitTime + avgAgentHandleTime + avgScoreAnswerGoalTimeMet, data = callsPerEach15MinInterval)
summary(lm(ratioCallsPerAgent ~ avgWaitTime + avgAgentHandleTime + avgScoreAnswerGoalTimeMet, data = callsPerEach15MinInterval))

lm(ratioCallsPerAgent ~ avgWaitTime + avgWaitTime_sq + avgAgentHandleTime, data = callsPerWeek)
summary(lm(ratioCallsPerAgent ~ avgWaitTime + avgWaitTime_sq + avgAgentHandleTime, data = callsPerWeek))




