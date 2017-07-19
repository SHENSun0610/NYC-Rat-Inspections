# Kelly Xie
# Prof. Tambe, ADE Spring 2017
# Homework 1: NYC Rat Inspection Analysis


########################## PART 1A ##########################


library(ggplot2)
library(lubridate)
library(dplyr)

rodent = read.csv("~/Desktop/Kelly Y Xie")

# parses inspection dates
parseDate = parse_date_time(rodent$INSPECTION_DATE, c("%m/%d/%Y %H:%M:%S"))
#charDates = as.character(as.Date(parseDate))
as.Date(as.character(x$V1), format = "%m/%d/%Y")

# adds years and months to data frame
year = substr(parseDate,1,4)
rodent = cbind(rodent,year)

month = substr(parseDate,6,7)
rodent = cbind(rodent,month)

rodent$yearmonth = paste(rodent$year,rodent$month, sep="-") #year-month pairing

# adds row of ones to keep track of count for every unique year-month pairing
rodent$ones = 1

# grabs data for rat sightings in years 2011-2015
rodentFound = rodent[which(rodent$RESULT == "Active Rat Signs"),]
rodentFound = subset(rodentFound, 
                     as.numeric(as.character(year))>2010 & as.numeric(as.character(year))<2016)

# separates data by borough
manhattan = subset(rodentFound, BOROUGH=="Manhattan")
bronx = subset(rodentFound, BOROUGH=="Bronx")
brooklyn = subset(rodentFound, BOROUGH=="Brooklyn")
queens = subset(rodentFound, BOROUGH=="Queens")
statenIsland = subset(rodentFound, BOROUGH=="Staten Island")

# counts up total sightings by borough for each year
manhattanCount = manhattan %>% group_by(yearmonth) %>% 
  summarise(count = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
bronxCount = bronx %>% group_by(yearmonth) %>% 
  summarise(count = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
brooklynCount = brooklyn %>% group_by(yearmonth) %>% 
  summarise(count = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
queensCount = queens %>% group_by(yearmonth) %>% 
  summarise(count = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
statenCount = statenIsland %>% group_by(yearmonth) %>% 
  summarise(count = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)

# formats all character dates to Date
manhattanCount$yearmonth = as.Date(paste(manhattanCount$yearmonth,"01",sep="-"))
bronxCount$yearmonth = as.Date(paste(bronxCount$yearmonth,"01",sep="-"))
brooklynCount$yearmonth = as.Date(paste(brooklynCount$yearmonth,"01",sep="-"))
queensCount$yearmonth = as.Date(paste(queensCount$yearmonth,"01",sep="-"))
statenCount$yearmonth = as.Date(paste(statenCount$yearmonth,"01",sep="-"))

# plots all boroughs on one chart
gg = ggplot() + 
  geom_line(data=manhattanCount, aes(x=yearmonth, y=count, color='Manhattan')) +
  geom_line(data=bronxCount, aes(x=yearmonth, y=count, color='Bronx')) +
  geom_line(data=brooklynCount, aes(x=yearmonth, y=count, color='Brooklyn')) +
  geom_line(data=queensCount, aes(x=yearmonth, y=count, color='Queens')) +
  geom_line(data=statenCount, aes(x=yearmonth, y=count, color='StatenIsland')) +
  xlab("Date (Month-Year)") +
  ylab("Number of Rat Sightings") +
  ggtitle("Rat Sightings in NYC Boroughs from 2011-2015") +
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(date_breaks="1 month", date_labels = "%b %Y") +
  geom_vline(xintercept=as.numeric(manhattanCount$yearmonth[23])) +
  scale_colour_manual(name="Borough", 
                      values=c(Manhattan="red", 
                               Bronx="blue", 
                               Brooklyn="purple", 
                               Queens="green", 
                               StatenIsland="yellow"))
gg


########################## PART 1B ##########################


# grabs data for all inspections in years 2011-2015
rodentInspections = subset(rodent, 
                           as.numeric(as.character(year))>2010 & as.numeric(as.character(year))<2016)

# separates by borough for rat sightings
man = subset(rodentInspections, BOROUGH=="Manhattan" & RESULT=="Active Rat Signs")
brx = subset(rodentInspections, BOROUGH=="Bronx" & RESULT=="Active Rat Signs")
brk = subset(rodentInspections, BOROUGH=="Brooklyn" & RESULT=="Active Rat Signs")
qns = subset(rodentInspections, BOROUGH=="Queens" & RESULT=="Active Rat Signs")
stn = subset(rodentInspections, BOROUGH=="Staten Island" & RESULT=="Active Rat Signs")

# separates by borough for total inspections
man2 = subset(rodentInspections, BOROUGH=="Manhattan")
brx2 = subset(rodentInspections, BOROUGH=="Bronx")
brk2 = subset(rodentInspections, BOROUGH=="Brooklyn")
qns2 = subset(rodentInspections, BOROUGH=="Queens")
stn2 = subset(rodentInspections, BOROUGH=="Staten Island")

# counts up rat sightings and total inspections by borough for each year
manCount = man %>% group_by(yearmonth) %>% 
  summarise(active = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
brxCount = brx %>% group_by(yearmonth) %>% 
  summarise(active = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
brkCount = brk %>% group_by(yearmonth) %>% 
  summarise(active = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
qnsCount = qns %>% group_by(yearmonth) %>% 
  summarise(active = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
stnCount = stn %>% group_by(yearmonth) %>% 
  summarise(active = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)

manCount2 = man2 %>% group_by(yearmonth) %>% 
  summarise(total = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
brxCount2 = brx2 %>% group_by(yearmonth) %>% 
  summarise(total = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
brkCount2 = brk2 %>% group_by(yearmonth) %>% 
  summarise(total = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
qnsCount2 = qns2 %>% group_by(yearmonth) %>% 
  summarise(total = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)
stnCount2 = stn2 %>% group_by(yearmonth) %>% 
  summarise(total = sum(ones, na.rm=TRUE)) %>% arrange(yearmonth)

# joins tables for each borough by year
manCount = inner_join(manCount,manCount2,by="yearmonth")
brxCount = inner_join(brxCount,brxCount2,by="yearmonth")
brkCount = inner_join(brkCount,brkCount2,by="yearmonth")
qnsCount = inner_join(qnsCount,qnsCount2,by="yearmonth")
stnCount = inner_join(stnCount,stnCount2,by="yearmonth")

# calculates efficiency for each year
# Note: efficiency for a given month is the number of inspections 
# yielding "Active Rat Signs" in that month divided by 
# the total number of inspections in that month
manCount$eff = manCount$active / manCount$total
brxCount$eff = brxCount$active / brxCount$total
brkCount$eff = brkCount$active / brkCount$total
qnsCount$eff = qnsCount$active / qnsCount$total
stnCount$eff = stnCount$active / stnCount$total

# formats all character dates to Date
manCount$yearmonth = as.Date(paste(manCount$yearmonth,"01",sep="-"))
brxCount$yearmonth = as.Date(paste(brxCount$yearmonth,"01",sep="-"))
brkCount$yearmonth = as.Date(paste(brkCount$yearmonth,"01",sep="-"))
qnsCount$yearmonth = as.Date(paste(qnsCount$yearmonth,"01",sep="-"))
stnCount$yearmonth = as.Date(paste(stnCount$yearmonth,"01",sep="-"))

# plots all boroughs on one chart
gg = ggplot() + 
  geom_point(color="blue") + 
  geom_line(data=manCount, aes(x=yearmonth, y=eff, color='Manhattan')) +
  geom_line(data=brxCount, aes(x=yearmonth, y=eff, color='Bronx')) +
  geom_line(data=brkCount, aes(x=yearmonth, y=eff, color='Brooklyn')) +
  geom_line(data=qnsCount, aes(x=yearmonth, y=eff, color='Queens')) +
  geom_line(data=stnCount, aes(x=yearmonth, y=eff, color='StatenIsland')) +
  xlab("Date (Month-Year)") +
  ylab("Efficiency of Rat Inspections") +
  ggtitle("Rat Inspection Efficiency in NYC Boroughs from 2011-2015") +
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(date_breaks="1 month", date_labels = "%b %Y") +
  geom_vline(xintercept=as.numeric(manCount$yearmonth[23])) +
  scale_colour_manual(name="Borough", 
                      values=c(Manhattan="red", 
                               Bronx="blue", 
                               Brooklyn="purple", 
                               Queens="green", 
                               StatenIsland="yellow"))
gg


########################## PART 1C ##########################


# grabs subset of data for inspections that yield active rat signs
activeRat = subset(rodent, RESULT=="Active Rat Signs" &
                   as.numeric(as.character(year))>2010 & as.numeric(as.character(year))<2016)

# get counts of active rat inspections for each unique zipcode
zipRat = activeRat %>% group_by(ZIP_CODE) %>% 
  summarise(count = sum(ones, na.rm=TRUE)) %>% arrange(desc(count))

# appends corresponding boroughs
temp = subset(activeRat, select=c("ZIP_CODE","BOROUGH"))
zipRat = unique(inner_join(zipRat,temp,by="ZIP_CODE") %>% group_by(ZIP_CODE)) # appends boroughs

# prints top ten "hot spot" zipcodes among all 5 boroughs
print(zipRat[1:10,])


########################## PART 2A ##########################


# using the rodent inspection database

# top 20 zip codes for rat activity prior to 2012
rodentsBefore = subset(rodent, RESULT=="Active Rat Signs" &
                         as.numeric(as.character(year))<2012)
topBefore = rodentsBefore %>% group_by(ZIP_CODE) %>% 
  summarise(count = sum(ones, na.rm=TRUE)) %>% arrange(desc(count)) # get counts for unique zipcodes

tempBefore = subset(rodentsBefore, select=c("ZIP_CODE","BOROUGH"))
topBefore = unique(inner_join(topBefore,tempBefore,by="ZIP_CODE") %>% group_by(ZIP_CODE)) # appends boroughs
print(topBefore[1:20,])

# top 20 zip codes for rat activity after 2012
rodentsAfter = subset(rodent, RESULT=="Active Rat Signs" &
                        as.numeric(as.character(year))>2012)
topAfter = rodentsAfter %>% group_by(ZIP_CODE) %>% 
  summarise(count = sum(ones, na.rm=TRUE)) %>% arrange(desc(count)) # get counts for unique zipcodes

tempAfter = subset(rodentsAfter, select=c("ZIP_CODE","BOROUGH"))
topAfter = unique(inner_join(topAfter,tempAfter,by="ZIP_CODE") %>% group_by(ZIP_CODE)) # appends boroughs
print(topAfter[1:20,])

# top 20 zip codes for rat activity during Hurricane Sandy
sandy = read.csv("~/Desktop/sandyrelated.csv") # Hurricane Sandy 311 database
sandy = subset(sandy, Complaint.Type=="Rodent" & Descriptor!="Condition Attracting Rodents")
sandy$ones = 1
topSandy = sandy %>% group_by(Incident.Zip) %>% 
  summarise(count = sum(ones, na.rm=TRUE)) %>% arrange(desc(count)) # get counts for unique zipcodes
tempSandy = subset(sandy, select=c("Incident.Zip","Borough"))
topSandy = unique(inner_join(topSandy,tempSandy,by="Incident.Zip") %>% group_by(Incident.Zip)) # appends boroughs
print(topSandy[1:20,])


########################## PART 3 ##########################


# restaurant inspection database
restaurant = read.csv("~/Desktop/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

# STEP 1
# variable for year-month-zipcode combination
rodent$yearmonthzip = paste(rodent$year,rodent$month,rodent$ZIP_CODE,sep="-")

# grabs rat sightings and total inspections
ratSightings = filter(rodent,RESULT=="Active Rat Signs" & 
                        as.numeric(as.character(year))>2009 & 
                        as.numeric(as.character(year))<2016) %>% 
  group_by(yearmonthzip) %>% summarise(rat = sum(ones, na.rm=TRUE)) %>% arrange(yearmonthzip)

totalInspections = filter(rodent,
                         as.numeric(as.character(year))>2009 & 
                         as.numeric(as.character(year))<2016) %>% 
  group_by(yearmonthzip) %>% summarise(total = sum(ones, na.rm=TRUE)) %>% arrange(yearmonthzip)

# outer joins two tables and compute likelihood
ratSightings = full_join(ratSightings,totalInspections,by="yearmonthzip")
activeRatSightings = (ratSightings$rat / ratSightings$total)
likelihood = as.data.frame(cbind(yearmonthzip = ratSightings$yearmonthzip,activeRatSightings))

# Note: activeRatSightings is predictor variable: the likelihood of active rat signs
# in the same zip code as the restaurant, and in the same month and year 
# in which the restaurant was inspected


# STEP 2
# merges likelihood with restaurant violation data
mth = substr(as.character(restaurant$INSPECTION.DATE),1,2)
yr = substr(as.character(restaurant$INSPECTION.DATE),7,10)
restaurant$yearmonthzip = paste(yr, mth, restaurant$ZIPCODE, sep="-")
restaurant = full_join(restaurant,likelihood,by="yearmonthzip") # outer join

# converts all NA to 0 values for activeRatSightings
restaurant$activeRatSightings = as.numeric(as.character(restaurant$activeRatSightings))
restaurant$activeRatSightings[is.na(restaurant$activeRatSightings)] <- 0


# STEP 3
# sets seed
set.seed(4698)


# STEP 4
# binary variable ratViolation that takes the value 1 or 0 
# depending on whether a restaurant violation is due to rat problems
restaurant = mutate(restaurant, ratViolation = as.numeric(VIOLATION.CODE == "04L" | 
                                        VIOLATION.CODE == "04K" | 
                                        VIOLATION.CODE == "08A"))

# converts month and year to factor variables
restaurant$year = factor(substr(restaurant$yearmonthzip,1,4))
restaurant$month = factor(substr(restaurant$yearmonthzip,6,7))

# gets subset of data for years 2012-2015
restaurant = restaurant %>% subset(as.numeric(as.character(year)>2011) &
                                     as.numeric(as.character(year)<2016))


# runs a logistic regression on merged data set to test 
# activeRatSightings variable has a statistically significant relationship 
# to ratViolation predictor
fit = glm(ratViolation ~ activeRatSightings + month + year, data=restaurant, family=binomial)
summary(fit)



