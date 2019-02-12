hr=read.csv('C:/Users/Administrator/Documents/HR Analytics.csv')
View(hr)

#job roles wise-
#total no of employees
#average monyhly income
#no of females
#no of males
#max monthly income
#min monthly income

#1.total no of employees accord to their jobroles
hr%>%group_by(JobRole)%>%summarise(no_of_employees=n())

#2. average monthly income of all the employees
hr%>%group_by(JobRole)%>%summarise(average_salary=mean(MonthlyIncome))

#3. no of female and male staff in each jobrole
hr%>%group_by(JobRole)%>%
  summarise(female_staff=sum(Gender=='Female',na.rm = T),
            male_staff=sum(Gender=='Male',na.rm = T))

#4. maximum and minimum monthly income of jobroles
hr%>%group_by(JobRole)%>%summarise(maximum_salary=max(MonthlyIncome),minimum_salary=min(MonthlyIncome))

#5.do all in one only
a=hr%>%group_by(JobRole)%>%summarise(total_employees=n(),
average_monthly_income=mean(MonthlyRate),female=sum(Gender=='Female'),
male=sum(Gender=='Male'),maximum=max(MonthlyIncome),minimum=min(MonthlyIncome))

View(a)

