#loading the data set
climate <-read.csv("C:/Users/nikit/Downloads/climate_data.csv")

#structure and summary
str(climate)
summary(climate)

#checking for missing values
colSums(is.na(climate))

#removing a column
climate <- subset(climate, select = -c(Maximum.rain.per.minute) )
colnames(climate)

#renaming month
climate$Month[climate$Month == 1] = "January"
climate$Month[climate$Month == 2] = "February"
climate$Month[climate$Month == 3] = "March"
climate$Month[climate$Month == 4] = "April"
climate$Month[climate$Month == 5] = "May"
climate$Month[climate$Month == 6] = "June"
climate$Month[climate$Month == 7] = "July"
climate$Month[climate$Month == 8] = "August"
climate$Month[climate$Month == 9] = "September"
climate$Month[climate$Month == 10] = "October"
climate$Month[climate$Month == 11] = "November"
climate$Month[climate$Month == 12] = "December"
unique(climate$Month)

#describe()
climate %>%
  describe()

#histogram of max temp
ggplot(climate, aes(x=Minimum.temperature...F., fill=Month)) +
  geom_histogram(colour = "black", alpha = 0.5, position = "identity") + ggtitle("Distribution of maximum temp")+
  xlab("Temp") + ylab("Density")

#density
ggplot(climate, aes(x= Maximum.heat.index...F., fill=Region)) + 
  geom_density(alpha=0.5) +
  xlab("Heat Index")+
  ylab("Count") +
  ggtitle("Analysis of Heat Index") +
  scale_fill_discrete(name = "Region")

#scatter plot of temp vs humidity
ggplot(data=climate,aes(x=Average.temperature...F.,y=Average.humidity....,
                        color = Month, size=factor(Month)))+
  geom_point(alpha=0.3)+
  xlab("Temp")+
  ylab("Humidity") + 
  labs(color="Month") + 
  guides(size=FALSE)+
  ggtitle("Temp vs Humidity")


#boxplot of humidity by month
ggplot(climate, aes(x = Month, y = Maximum.humidity....)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_boxplot() + xlab("Month") + 
  ylab("Humidity") + 
  ggtitle("Boxplots of Humidity by month")

#boxplot of maximum pressure
ggplot(climate, aes(x = Region, y = Maximum.pressure)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_boxplot() + xlab("Region") + 
  ylab("Pressure") + 
  ggtitle("Boxplots of Pressure by Region")

#jitter plot of temp vs humidity
ggplot(climate, aes(Maximum.temperature..?.F., Maximum.humidity....,
                    color=Month )) + geom_jitter() + 
  ggtitle("Scatterplot of temperature vs humidity")

#crosstable
library(gmodels)
crosstable(climate, c(Maximum.pressure, Minimum.pressure), by=Region) %>%
  as_flextable(keep_id=TRUE)
mean(climate$Average.temperature...F.)

east <- subset(climate, subset=(Region=="East"))
west <- subset(climate, subset=(Region=="West"))

April <- subset(climate, subset=(Month==4))
August <- subset(climate, subset=(Month==8))

#t test
t.test(climate$Average.temperature...F., mu=36, alternative = "two.sided")

#Two sample t-test
t.test(east$Rainfall.for.year..in., west$Rainfall.for.year..in.,var.equal = F)

#Two sample t-test
t.test(April$Maximum.humidity...., August$Maximum.humidity....,var.equal = F)
#filtering the necessary attributes
new_climate <- climate[c("Average.temperature...F.", "Average.humidity....", "Average.dewpoint...F.", "Average.windspeed..mph.", "Average.gustspeed..mph.")]
head(new_climate)


#correlation table
correlation_table_climate <- cor(new_climate)
correlation_table_climate

#correlation plot
corrplot(correlation_table_climate)

#regression plot of temp vs humidity

lr1 = lm(climate$Average.humidity.... ~ climate$Average.temperature...F., data=climate)
lr1
summary(lr1)

ggplot(data=climate,aes(x=Average.temperature...F.,y=Average.humidity....,
                        color = Month, size=factor(Month)))+
  geom_point(alpha=0.3)+
  geom_abline(slope=lr1$coefficients[2],
              intercept=lr1$coefficients[1],
              color="black",
              size=1)+
  xlab("Temp")+
  ylab("Humidity") + 
  labs(color="Month") + 
  guides(size=FALSE)+
  ggtitle("Temp vs Humidity")


#regression plot of temp vs rainfall

lr2 = lm(climate$Rainfall.for.year..in. ~ climate$Maximum.temperature...F., data=climate)
lr2
summary(lr2)

ggplot(data=climate,aes(x=climate$Maximum.temperature...F.,y=climate$Rainfall.for.year..in.,
                        color = Region, size=factor(Region)))+
  geom_point(alpha=0.3)+
  geom_abline(slope=lr2$coefficients[2],
              intercept=lr2$coefficients[1],
              color="black",
              size=1)+
  xlab("Temp")+
  ylab("Rainfall") + 
  labs(color="Region") + 
  guides(size=FALSE)+
  ggtitle("Temp vs Rainfall")

#regression plot of pressure vs heat index

lr3 = lm(climate$Maximum.heat.index...F. ~ climate$Maximum.pressure, data=climate)
lr3
summary(lr3)

ggplot(data=climate,aes(x=climate$Maximum.pressure,y=climate$Maximum.heat.index...F.,
                        color = Region, size=factor(Region)))+
  geom_point(alpha=0.3)+
  geom_abline(slope=lr3$coefficients[2],
              intercept=lr3$coefficients[1],
              color="black",
              size=1)+
  xlab("Pressure")+
  ylab("Heat Index") + 
  labs(color="Region") + 
  guides(size=FALSE)+
  ggtitle("Pressure vs Heat Index")

#ifelse to convert categorical to dummy variable: Male 1 Female 0
climate$Reg <- ifelse(climate$Region=='South', 1,0)
climate$Reg
climate$Reg <- as.numeric(climate$Reg)

#regression plot of windspeed vs temperature in southern region

lr4 = lm(climate$Average.temperature...F. ~ climate$Average.windspeed..mph.+Reg, data=climate)
lr4
summary(lr4)

plot(climate$Average.windspeed..mph.[climate$Reg==1], climate$Average.temperature...F.[climate$Reg==1], xlab="Windpseed", 
     ylab="Temperature", main = "Multiple regression line", pch=19, col="red")
legend("topleft", legend=c("South", "Other"),
       col=c("red", "purple"), pch=19,cex=0.8)
legend("topright", legend=c("South", "Other"),
       col=c("Black", "orange"), lty = 1,cex=0.8)
points(climate$Average.windspeed..mph.[Salaries$Male==0], climate$Average.temperature...F.[Salaries$Male==0], col="Purple", pch=19)

#coefficients of model
lr4$coefficients

#other regions
abline(a=lr4$coefficients[1], b=lr4$coefficients[2], col="Orange", lwd = 4)

#south
abline(a=lr4$coefficients[1]+lr4$coefficients[3], b=lr4$coefficients[2], col="Black", lwd = 4)

#creating subset
south = subset(climate,subset = (climate$Region=="South"))

#head of subsets
head(south, 10)

#summary
summary(south)

#regression analysis plot
plot(x=south$Average.windspeed..mph., y=south$Average.temperature...F., data=south, col="red", ylab="Temperature", xlab="Windspeed", 
     main="Regression on temp vs windspeed", pch=19)

#regression analysis
southline = lm(south$Average.temperature...F. ~ south$Average.windspeed..mph., data=south)
summary(southline)
abline(southline, lwd=4)