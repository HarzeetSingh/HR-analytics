hr=read.csv('C:/Users/Administrator/Documents/HR Analytics.csv')
View(hr)
odi=read.csv('c:/Users/Administrator/Documents/odi-batting.csv')
View(odi)
####HW#####
#IDENTIFY TOTAL NO OF ROWS AND COLUMN
dim(hr)

#TOTAL NO OF ROWS
length(hr)

#IDENTIFY UNIQUE JOB ROLES
unique_job=c()
for (job in hr$JobRole) {
  if(job%in%unique_job){
    
  }
  else{unique_job=c(unique_job,job)}
}
print (unique_job)
##########################C L A S S #########################
library(dplyr)
hr=read.csv('C:/Users/Administrator/Documents/HR Analytics.csv')
View(hr)
odi=read.csv('c:/Users/Administrator/Documents/odi-batting.csv')
View(odi)


#filter data
#filter for Sachin R Tendulkar
sachin_rows=odi %>% filter(Player=='Sachin R Tendulkar')#AFTER TYPING FILTER CHECK IF IT IS FROM DDPLYR OR NOR, THATS OUR LIBRARY PACKAGE
nrow(sachin_rows)


#filterring one column with multiple values

indian_players=c('Sachin R Tendulkar', 'Ashish Nehra', 'Virender Sehwag')
indian_player_rows=odi%>%filter(Player%in%indian_players)
nrow(indian_player_rows)

#sachin vs australia

sachin_vs_australia=odi%>%filter(Player=='Sachin R Tendulkar',
                                 Versus=="Australia")
nrow(sachin_vs_australia)

#sachin vs ausi with 0
sachin_vs_australia=odi%>%filter(Player=='Sachin R Tendulkar',
                                 Versus=="Australia",Runs=='0')
nrow(sachin_vs_australia)

#sachin vs ausi with century

sachin_vs_australia=odi%>%filter(Player=='Sachin R Tendulkar',
                                 Versus=="Australia",Runs>99)
nrow(sachin_vs_australia)

#SACHIN VS AUSI WITH 50-100

sachin_vs_australia=odi%>%filter(Player=='Sachin R Tendulkar',
                                 Versus=='Australia',Runs>49, Runs<100)
nrow(sachin_vs_australia)


####################           G R O U P I N G 
###CONVERTING HIGH DETAIL DETAIL TO LOW ONE IS CALLED GROUPING, SUMMERISING,AGGEREGATING####
###LIKE IF YOU HAVE 10 ELEMENTS AFTER GROUPING WILL CONVERTED TO OONLY 3###
###


#grouping operations
#compute player wise total runs
ttp=odi%>%group_by(Player) %>%
  summarise(total_runs=sum(Runs))
class(ttp)
nrow(ttp)
View(ttp)

#compute player wise total runs and identify top 10 players
ttp_2=odi%>%group_by(Player) %>%
  summarise(total_runs=sum(Runs))%>%arrange(-total_runs)%>%head(10)
#"use tail(10) for bottom 10"
View(ttp_2)
ttp_2
#'-' sign we do a desending order sorting
#or can use 'desc' command
#we use total_runs because all other column are notthere now coz its filtered now.
#'head(10)' will pick first 10 

#METHOD 2
ttp_2=odi%>%group_by(Player) %>%
  summarise(total_runs=sum(Runs))%>%arrange(-total_runs)%>%head(ttp_2,10)
###head(ttp,10) is secound method of same thing
View(ttp_2)
ttp_2


##top ten indian players based on maximum runs

top_10_players=odi%>% filter(Country=='India')%>%group_by(Player)%>%
  summarise(max_run=max(Runs))%>%
  arrange(-max_run)%>%
  head(10)
View(top_10_players)


#top 10 players who have played better against Australia
#based on their total runs


top_10_players_vs_ausi=odi%>% filter(Versus=='Australia')%>%group_by(Country,Player)%>%
  summarise(total_run=sum(Runs))%>%
  arrange(-total_run)%>%
  head(10)
View(top_10_players_vs_ausi)

#top 10 players based on total matches

players_total_matches =odi%>%group_by(Player)%>%summarise(total_matches=n())%>% arrange(-total_matches)%>%head(10)
View(players_total_matches)

#summarize many matrics
#-total runs
#total matches

library(dplyr)


palyers_summary=odi%>%group_by(Country, Player)%>%
  summarise(total_runs=sum(Runs,na.rm=T),total_matches=n())
View(palyers_summary)

##################show tue false in odi in
#centuries,ducks,fifies, missed centuries, double century

odix=odi%>%filter(Country=='India')
odix$Duck=ifelse(Runs==0,TRUE,FALSE)
odix$fifty=ifelse(odi$Runs>49 & odi$Runs<100,TRUE,FALSE)
Odix$missed_century=ifelse(odi$Runs>90 & odi$Rund<100,TRUE,FALSE)
odix$century=ifelse(odi$Runs>99 & odi$Runs<199,TRUE,FALSE)
odix$double_country=ifelse(odi$Runs>199,TRUE,FALSE)
View(odix)





odi$Century=ifelse(odi$Runs>99,TRUE,FALSE)
odi$Duck=ifelse(odi$Runs==0,TRUE,FALSE)
odi$fifty=ifelse(odi$Runs>49 & odi$Runs<100,TRUE,FALSE)
odi$double_Century=ifelse(odi$Runs>199 & odi$Runs<300,TRUE,FALSE)
odi$missed_Century=ifelse(odi$Runs>90 & odi$Runs<100,TRUE,FALSE)
View(odi)


#COUNT NO OF RUNS,MATCHES,centuries,ducks,fifies, missed centuries, double century)
#na.rm will make true as 1 and false as 0 and then we can easily add these details


players_summary=odi%>%
  group_by(Country, Player)%>%
  summarise(total_runs=sum(Runs,na.rm=T),
            total_matches=n(),
centuries=sum(Century,na.rm=T),
ducks=sum(Duck,na.rm=T),
fifties=sum(fifty,na.rm=T),
double=sum(double_Century,na.rm=T),
missed=sum(missed_Century,na.rm=T))
View(players_summary)


odixyz=odi%>%
  group_by(Country,Player)%>%
summarise(Ducks=sum(Duck,na.rm = T),
          fifties=sum(fifty,na.rm = T),
         centuries=sum(century,na.rm=T)
         
          )
View(odixyz)






odi%>%group_by(Country,Player)%>%
  summarise(centuries=sum(Runs>99,na.rm=T),
            ducks=sum(Runs==0,na.rm=T),
            total_runs=sum(Runs,na.rm=T))%>%
  arrange(-total_runs)%>%
View()




#############  H R-home work

#job roles-
#wise total no of employees
#average monyhly income
#no of females
#no of males
#max monthly income
#min monthly income


hr=read.csv('C:/Users/Administrator/Documents/HR Analytics.csv')
View(hr)



te=hr%>%group_by(JobRole) %>%
  summarise(total_emp=sum(EmployeeCount),
average_income=mean(MonthlyIncome),
male=sum(Gender=='Male'),
female=sum(Gender=='Female'),
max_monthly_income=max(MonthlyIncome),
min_monthly_income=min(MonthlyIncome))
View(te)



View(odi)
#working eith match date
odi$date=as.Date(odi$MatchDate, "%d-%m-%Y")
odi$Year=format(odi$date,'%Y')



#CALCULATE YEAR WISE TOTAL centuries,ducks,fifies, missed centuries, double century
#SCORED BY SACHIN

sachin_yearwise_perf=odi%>%
filter(Player=='Sachin R Tendulkar')
group_by(Year)%>%
  summarise(total_runs=sum(Runs,na.rm=T),
            total_matches=n(),
            centuries=sum(Century,na.rm=T),
            ducks=sum(Duck,na.rm=T),
            fifties=sum(fifty,na.rm=T),
            double=sum(double_Century,na.rm=T),
            missed=sum(missed_Century,na.rm=T))
View(sachin_yearwise_perf)
