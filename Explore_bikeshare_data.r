
**Firstly we read the csv files into our jupyter R workspace on which the analysis will be performed.**

#reading the csv files
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)#checking the structure of New York's dataset

head(wash)#checking the structure of washington's dataset

head(chi)#checking the structure of Chicago's dataset

#After checking the structure of file we move forward to answering the questions. But firstly we attach the packages that we are going to need in our project.
# Packages used
library(ggplot2) #popular package used for exploratry data analysis for creating graphs in R
library(lubridate) #package used to deal easily with date objects in R

#function to plot uses in each month
plot_count_per_month<-function(m){
months<- c("Jan","Feb","March","Apr","May","June") #for putting as label
qplot(factor(m) ,xlab="Months" , ylab="Count" , main="Total uses in each months for New York",fill=m)+
scale_x_discrete(labels= months) #graphing the month to see which month has most activity
}

#New York
#We only deal with the Start.Time column of our datasets as it will be enough for the question we want answered
class(ny$Start.Time) #to check the class or datatype of this column

table(is.na(ny$Start.Time)) #to check whether there are any NA's present in the table

ny$Start.Time<-as.Date(ny$Start.Time) # to convert the values to an object of  date type
class(ny$Start.Time)
head(ny$Start.Time) #to check the changes
summary(ny$Start.Time) #to explore the column more

month_count<-month(as.POSIXlt(ny$Start.Time, format="%d/%m/%Y")) #to extract months from the date variable that we created earlier
table(month_count) #to check which months have the most number of use

#From above it is clear that most uses were in June, graphically representing it using qplot function of ggplot2
plot_count_per_month(month_count)

#Washington
#We only deal with the Start.Time column of our datasets as it will be enough for the question we want answered
class(wash$Start.Time) #to check the class or datatype of this column

table(is.na(wash$Start.Time)) #to check whether there are any NA's present in the table

which(!complete.cases(wash)) # to check which element of the data frame contains NA

wash[89051,] #to check the element containing NA's

wash$Start.Time<-as.Date(wash$Start.Time) # to convert the values to an object of  date type
class(wash$Start.Time)
head(wash$Start.Time) #to check the changes
summary(wash$Start.Time[-89051]) #to explore the column more

m<-month(as.POSIXlt(wash$Start.Time[-89051], format="%d/%m/%Y")) #to extract months from the date variable that we created earlier except the one with NA's
table(m) #to check which months have the most number of use

#Again in the month of june the service has mostly been used,graphically representing it too
plot_count_per_month(m)

#Chicago
#We only deal with the Start.Time column of our datasets as it will be enough for the question we want answered
class(chi$Start.Time) #to check the class or datatype of this column

table(is.na(chi$Start.Time)) #to check whether there are any NA's present in the table

chi$Start.Time<-as.Date(chi$Start.Time) # to convert the values to an object of  date type
class(chi$Start.Time)
head(chi$Start.Time) #to check the changes
summary(chi$Start.Time) #to explore the column more

m<-month(as.POSIXlt(chi$Start.Time, format="%d/%m/%Y")) #to extract months from the date variable that we created earlier
table(m) #to check which months have the most number of use

#Again in the month of june the service has mostly been used,graphically representing it too
plot_count_per_month(m)

# from above 3 graphs it is clear that the most activity or bookings of the bikes took place in the month of JUne in all three
#We plot a graph to show the most activity combined all 3 places
tot.df<-rbind(ny[,1:7],wash,chi[,1:7])
table(is.na(tot.df$Start.Time))
which(is.na(tot.df$Start.Time))

m<-month(as.POSIXlt(tot.df$Start.Time[-143821], format="%d/%m/%Y")) #to extract months from the date variable that we created earlier
table(m) #to check which months have the most number of use

#Finally combing all three files we get that most in the month of june the service has mostly been used,graphically representing it too
months<- c("Jan","Feb","March","Apr","May","June")#for putting as label
qplot(factor(m) ,xlab="Months" , ylab="Count" , main="Total uses in each months for All the 3 cities",fill=m)+
scale_x_discrete(labels= months) #graphing the month to see which month has most activity

# New York
table(is.na(ny$Birth.Year))# to check for NA's in New York dataset
# Chicago
table(is.na(chi$Birth.Year))# to check for NA's in Chicago dataset

summary(ny$Birth.Year)# findind summary of Birth Year of New York dataset to check which age group uses this service more in new York
summary(chi$Birth.Year)#finding summary of Birth Year of Chicago dataset to check which age group uses this service more in Chicago

nrow(ny)#total number of entries in New York dataset
nrow(chi)#total number of entries in Chicago dataset
table(ny$Gender)#Gender wise distribution of the entries in New York dataset to find which gender uses the service more
table(chi$Gender)#Gender wise distribution of the entries in Chicago dataset to find which gender uses the service more

by(ny$Birth.Year,factor(ny$Gender),summary)#comparing gender vise users birth years in New york
by(chi$Birth.Year,factor(chi$Gender),summary)#comparing gender vise users birth years in Chicago

#plotting a boxplot to compare gender vise users birth years in New york
ggplot(ny,aes(x=Gender,y=Birth.Year,colour=Gender),ylim=c(1880,2010))+geom_boxplot()+labs(x="Gender",y="Birth Year")+ggtitle("Gender wise distribution of birth years across New York")

#plotting a boxplot to compare gender vise users birth years in Chicago
ggplot(chi,aes(x=Gender,y=Birth.Year,colour=Gender))+geom_boxplot()+labs(x="Gender",y="Birth Year")+ggtitle("Gender wise distribution of birth years across Chicago")

#creating function to find total and average trip duration of the 3 cities
tot_avg_trip_dur<-function(city){
table(is.na(city$Trip.Duration))# to chek for NA's
x<-which(is.na(city$Trip.Duration))# to find for the position of NA's to ignore them to ignore them during calculations
if(length(x)!=0){
   tot_time=sum(city$Trip.Duration[-x])# total trip duration is the sum of all the trips present in a database in seconds
    avg_time=mean(city$Trip.Duration[-x])# to find average time the bike sharing service has been used
    }else{
    tot_time=sum(city$Trip.Duration)# total trip duration is the sum of all the trips present in a database in seconds
    avg_time=mean(city$Trip.Duration)# to find average time the bike sharing service has been used
}
print(tot_time)
print(avg_time)}

#New York
tot_avg_trip_dur(ny)

#Washington
tot_avg_trip_dur(wash)

#Chicago
tot_avg_trip_dur(chi)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
