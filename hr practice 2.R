hr=read.csv('C:/Users/Administrator/Documents/HR Analytics.csv')
View(hr)
#no of employes acording to job role.

View(hr%>%group_by(JobRole)%>%summarise(no.of.employees=sum(EmployeeCount)))
View(hr%>%group_by(JobRole)%>%summarise(no.of.employees=n()))

#average monthly income of all the employees.

View(hr%>%group_by(JobRole)%>%summarise(avg.monthly.income=mean(MonthlyIncome)))

#no of female
View(hr%>%group_by(JobRole)%>%summarise(no.of.female=sum(Gender=='Female')))

View(hr%>%filter(Gender=='Female')%>%group_by(JobRole)%>%summarise(no.of.female=n()))

#no of male

View(hr%>%group_by(JobRole)%>%summarise(no.of.male=sum(Gender=='Male')))
View(hr%>%filter(Gender=='Female')%>%group_by(JobRole)%>%summarise(no.of.male=n()))

#maximum and monthly income
View(hr%>%group_by(JobRole)%>%summarise(min.salary=min(MonthlyIncome),max.salary=max(MonthlyIncome)))

#no of employees according to job role,average monthly income,
#no of females, no of males,maximum,minimum

View(hr%>%group_by(JobRole)%>%summarise(no.of.employees=sum(EmployeeCount),avg.month.inc=)