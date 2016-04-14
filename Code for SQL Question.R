# Question 2.1
library(RSQLite)
db = dbConnect(SQLite(), dbname = "C:\\Users\\Administrator\\Desktop\\141 HW6\\lahman2013.sqlite")
alltable=dbListTables(db)

getallnames = function(db, index, database) {
  # index is the index of the table we want to get all names in database
  # database is the database in which we want to find the table
  
  # the fuction is to get all names of the table we choose in database
  table = database[index]
  query = 'SELECT * FROM '
  query = paste0(query, table)
  names(dbGetQuery(db, query))
}

# get all col names of all tables
nameall=sapply(1:length(alltable), function(x) getallnames(db, x, alltable))

# check if the col names include yearID data
namematchyearID=sapply(1:length(nameall), function(x) match(nameall[[x]],"yearID"))

# select tables with yearID data
namewithyearID=sapply(1:length(nameall), function(x) any(namematchyearID[[x]]==1,na.rm=TRUE))
sometable=alltable[namewithyearID]

getyearIDs = function(db, index, database) {
  # index is the index of the table we want to get the data years in database
  # database is the database in which we want to find the table to get years
  
  # the fuction is to get the data years in the table we choose
  table = database[index]
  query = 'SELECT yearID FROM '
  query = paste0(query, table)
  dbGetQuery(db, query)
}

allyear=sapply(1:length(sometable), function(x) getyearIDs(db, x, sometable))

# the years covered
range(unlist(allyear))

# check if there are data for each of these years
number=range(unlist(allyear))[2]-range(unlist(allyear))[1]+1
yearnumber=sapply(1:length(allyear), function(x) length(table(allyear[[x]])))
data_for_each_year=sometable[which(yearnumber==number)]


# Question 2.2
# number of managers
allmanager = dbGetQuery(db, 'SELECT DISTINCT playerID FROM Managers')
numallmanager = length(allmanager$playerID)

# number of players
allplayer = dbGetQuery(db, 'SELECT playerID FROM MASTER')
numallplayer = length(allplayer$playerID)

# Unique people for some people who were not only players but were also manaagers
length(union(allmanager$playerID,allplayer$playerID))


## Question 3
# team win the World Series in 2000
WinSeries2000 = dbGetQuery(db, 'SELECT teamID, name 
                           FROM Teams 
                           WHERE  WSWin = "Y" AND    yearID = 2000')
                           
## Question 4
# team lost each year
teamlostSeries = dbGetQuery(db, 'SELECT yearID, name 
                            FROM Teams 
                            WHERE WSWin = "N" AND LgWin = "Y" ')
                                  

## Question 5
team_WSwin = dbGetQuery(db, 'SELECT yearID, W, G  
                        FROM Teams 
                        WHERE WSWin = "Y" ')


plot(team_WSwin$yearID,team_WSwin$W/team_WSwin$G, ylab="wining ratio in a season",
     xlab="years", main=" The wining ratio of final Champion team in a season")
boxplot(team_WSwin$W/team_WSwin$G, main=" The wining ratio of final Champion team in a season", 
        ylab="wining ratio in a season")


## Question 6
playersalary = dbGetQuery(db, 'SELECT DISTINCT salary  
                          FROM   Salaries 
                          WHERE  yearID = 2003 ')
sort(playersalary$salary,decreasing= TRUE)[1:3]


## Question 7
teamsalary = dbGetQuery(db, 'SELECT SUM(salary) AS Sumsalary, teamID  
                        FROM Salaries 
                        WHERE yearID = 1999
                        GROUP BY teamID ')


teamsalary2 = dbGetQuery(db, 'SELECT yearID, teamID, SUM(salary)
                         FROM Salaries
                         GROUP BY yearID, teamID;')

install.packages("ggplot2")
library(ggplot2)
names(teamsalary2) = c('Year', 'TeamID', 'TotalPayrolls')
ggplot(teamsalary2, aes(Year, TotalPayrolls)) + geom_point(aes(color = TeamID)) + labs(title="TotalPayrolls over years")


## Question 8
# input inflation rate and change it to the rate based on 1985
mydata=read.table("C:\\Users\\Administrator\\Desktop\\141 HW6\\Inflation rate.txt", header=FALSE)
inflation=mydata$V2/100

a=rep(1,29)

for(i in 2:29){
  # calculate the inflation rate based on 1985
  a[i]=a[i-1]*(1+inflation[i-1])
}

baseinflation=a   # the inflation rate vector

# update the dataframe teamsalary2 and add the adjusted salary into it.
year=matrix(c(1985:2013))
inflationrate=data.frame(year,baseinflation)
teamsalary3=merge(teamsalary2,inflationrate,by.x="Year",by.y="year")
teamsalary3$adjustedsalary=teamsalary3$TotalPayrolls/teamsalary3$baseinflation

# plot the adjusted salary for each teams over years
ggplot(teamsalary3, aes(Year, adjustedsalary)) + geom_point(aes(color = TeamID)) + labs(title="Adjusted Salaries over years")


## Question 9
# compare payrolls for the teams that are in the same leagues
teamsalary4NL = dbGetQuery(db, 'SELECT yearID, teamID, SUM(salary), lgID
                           FROM Salaries
                           WHERE lgID = "NL"
                           GROUP BY yearID, teamID;')

names(teamsalary4NL) = c('Year', 'TeamID', 'TotalPayrolls')
ggplot(teamsalary4NL, aes(Year, TotalPayrolls)) + geom_point(aes(color = TeamID)) + labs(title="TotalPayrolls over years in league NL")

teamsalary4AL = dbGetQuery(db, 'SELECT yearID, teamID, SUM(salary), lgID 
                           FROM Salaries
                           WHERE lgID = "AL"
                           GROUP BY yearID, teamID;')

names(teamsalary4AL) = c('Year', 'TeamID', 'TotalPayrolls')
ggplot(teamsalary4AL, aes(Year, TotalPayrolls)) + geom_point(aes(color = TeamID)) + labs(title="TotalPayrolls over years in league AL")


# compare payrolls for the teams that are in the same leagues and in the same division

teamsalary4AL = dbGetQuery(db, 'SELECT yearID, teamID, SUM(salary), lgID
                           FROM Salaries
                           WHERE lgID = "AL"
                           GROUP BY yearID, teamID;')

frame=dbGetQuery(db, 'SELECT teamID, divID, yearID from Teams')
teamsalary4ALdiv=merge(teamsalary4AL, frame, by=c("teamID","yearID"))

# the team in AL league and in w, E, c divisions.
teamsalary4ALdivw=teamsalary4ALdiv[which(teamsalary4ALdiv$divID=="W"),]
names(teamsalary4ALdivw) = c('TeamID','Year','TotalPayrolls')
ggplot(teamsalary4ALdivw, aes(Year, TotalPayrolls)) + geom_point(aes(color = TeamID)) + labs(title="TotalPayrolls over years in league AL and in division W")

teamsalary4ALdivE=teamsalary4ALdiv[which(teamsalary4ALdiv$divID=="E"),]
names(teamsalary4ALdivE) = c('TeamID','Year','TotalPayrolls')
ggplot(teamsalary4ALdivE, aes(Year, TotalPayrolls)) + geom_point(aes(color = TeamID)) + labs(title="TotalPayrolls over years in league AL and in division E")

teamsalary4ALdivC=teamsalary4ALdiv[which(teamsalary4ALdiv$divID=="C"),]
names(teamsalary4ALdivC) = c('TeamID','Year','TotalPayrolls')
ggplot(teamsalary4ALdivC, aes(Year, TotalPayrolls)) + geom_point(aes(pch = TeamID)) + labs(title="TotalPayrolls over years in league AL and in division C")


# Join two tables
teamsalary4NLdiv=merge(teamsalary4NL, frame, by=c("teamID","yearID"))

# the team in AL league and in w, E, c divisions.
teamsalary4NLdivw=teamsalary4NLdiv[which(teamsalary4NLdiv$divID=="W"),]
names(teamsalary4NLdivw) = c('TeamID','Year','TotalPayrolls')
ggplot(teamsalary4NLdivw, aes(Year, TotalPayrolls)) + geom_point(aes(color = TeamID)) + labs(title="TotalPayrolls over years in league NL and in division W")

teamsalary4NLdivE=teamsalary4NLdiv[which(teamsalary4NLdiv$divID=="E"),]
names(teamsalary4NLdivE) = c('TeamID','Year','TotalPayrolls')
ggplot(teamsalary4NLdivE, aes(Year, TotalPayrolls)) + geom_point(aes(color = TeamID)) + labs(title="TotalPayrolls over years in league NL and in division E")

teamsalary4NLdivC=teamsalary4NLdiv[which(teamsalary4NLdiv$divID=="C"),]
names(teamsalary4NLdivC) = c('TeamID','Year','TotalPayrolls')
ggplot(teamsalary4NLdivC, aes(Year, TotalPayrolls)) + geom_point(aes(pch = TeamID)) + labs(title="TotalPayrolls over years in league NL and in division C")


# draw a plot to show a connection between payroll and performance
names(teamsalary2)
names(dataframe)

dataframe=dbGetQuery(db, 'SELECT yearID, teamID, AVG(W) AS avgW, AVG(G) AS avgG 
                     FROM Teams
                     Where yearID > 1984
                     GROUP BY teamID, yearID')

performance_pay= merge(teamsalary2, dataframe, by=c("yearID", "teamID"))
performance_pay$winratio=performance_pay$avgW/performance_pay$avgG

# show relationship between payoff and performance
plot(performance_pay[,3],performance_pay$winratio, xlab="payroll", ylab="wining ratio", main="Relationship between performance and payroll")
cor(performance_pay[,3],performance_pay$winratio)

## Question 10
# first get a dataframe in which one observation includes year teamID whole 
# year's Homerun for the team in the year and whole game number of the team
Homerun = dbGetQuery(db, 'SELECT yearID, teamID, SUM(HR) AS sumHR, SUM(G) AS sumG 
                     From Teams
                     GROUP BY yearID, teamID ')

Homerun$ratio = Homerun$sumHR/ Homerun$sumG

# draw the average Homerun per game for all teams over years
names(Homerun)=c("Year","Team", "Homerun_number", "game_number","Homerun_ratio")
ggplot(Homerun, aes(Year, Homerun_ratio)) + geom_point(aes(color = Team)) + labs(title="average Homerun per game for all teams over years")


