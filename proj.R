#Loads the whole data into a data table
data = fread("E:\\Educational\\Statistical inference\\project\\FIFA18\\FIFA18\\fifa18.csv")

#------------------------------------------------------------------------------------------------------------

#Question number 0

#Shows the portion of number of null observation in all observations per feature
sapply(data, function(x) sum(is.na(x))/length(x)) 

#Shows number of non empty observation in whole dataset
sum( sapply(data, function(x) sum(is.na(x))/length(x)) != 0  )

#------------------------------------------------------------------------------------------------------------

#Question number 1:

#feature eur_wage is selected for this question.
wage = data[,"eur_wage",]

#Collects a few colors for for each pilar
colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan")

#The command to plot the diagram
hist( x, col=colors, main="Wage distribution of soccer players", xlab ="nummbe of playesrs in each bucket" ,ylab="Wage for each bucket", breaks=10 )

plot(density(x))
