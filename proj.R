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
hist( unlist(wage), col=colors, main="Wage distribution of soccer players", xlab ="nummbe of playesrs in each bucket" ,ylab="Wage for each bucket", breaks=10 )

#The density plot
ggplot(data, aes(x=wage))+  geom_density(color="darkblue", fill="lightblue", linetype="dashed")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Boxplot
ggplot(data, aes( x=ID ,y=wage)) + geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)

#Detect outliers
upper_outliers = wage[ wage > 27000 ]
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q2 P1
ggplot(data, aes(x=age, y=wage))+geom_point()

#P2
cor(wage, age)

#p3
ggplot(data, aes(x=age, y=wage)) + 
  +     geom_point()+
  +     geom_smooth()

#P4
ggplot(data, aes(x=data[,.(height_cm),], y=wage))+geom_point()
cor(wage, unlist( data[,.(height_cm),] ))
ggplot(data, aes(x=height_cm, y=wage)) + geom_point() + geom_smooth()


ggplot(data, aes(x=height_cm, y=age))+geom_point() 
cor(age, unlist( data[,.(height_cm),] ))
ggplot(data, aes(x=height_cm, y=age)) + geom_point() + geom_smooth()

#P5
bin<-hexbin(age, wage)
plot(bin)
