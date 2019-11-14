
listings <- read.csv("~/Downloads/listings.csv")
## removing listings where availability=0
newlistings <- listings[!listings$availability_365==0,]

#replacing NAs with zero in review_per_month
newlistings$reviews_per_month[newlistings$number_of_reviews==0] <- 0



####summary statistics of numeric variables only
options(scipen = 999)
k <- NULL
for(i in 1:ncol(newlistings)){
  k[i] <- is.numeric(newlistings[,i])}
View(t(sapply(newlistings[,k], summary)))


#DISTRIBUTIONS
#distribution is very skewed & many outliers
par(mfrow=c(2,3))
for(i in c(10,11,12,14,15,16)){
  hist(newlistings[,i], main=paste("Histogram of", names(newlistings)[i]))
}

#remove outliers from price
newlistings <- newlistings[newlistings$price <  (quantile(newlistings$price, .75)+(IQR(newlistings$price)*1.5 )) ,]
newlistings <- newlistings[newlistings$price !=0 ,]

#removing records with more than 365 minimum nights
newlistings <- newlistings[newlistings$minimum_nights <= 365,]

library(ggplot2)
#500 400
ggplot(newlistings, aes(x=price)) + geom_histogram( color="black",fill="#4A6FA5")+theme(panel.background = element_rect( color="black", fill="white"), panel.grid = element_blank())+
  xlab("Listing price in dollars")+scale_x_continuous(labels=function(x){paste("$", x, sep="")})
par(mfrow=c(1,1))
#350 450
boxplot(newlistings$price, col="#4A6FA5", ylab="Listing Price in dollars")

#recommend using log of price...log(price) looks more normal
ggplot(newlistings, aes(x=log(price))) + geom_histogram( color="black",fill="#4A6FA5")+theme(panel.background = element_rect( color="black", fill="white"), panel.grid = element_blank())
boxplot(log(newlistings$price), col="#4A6FA5", ylab="Listing Price in dollars")

#create new variable log(price)
newlistings$logprice <- log(newlistings$price)

newlistings <- newlistings[newlistings$logprice >= 2.846,]

#log price by neighborhood group 
#500 425
ggplot(newlistings, aes(x=neighbourhood_group, y=log(price), fill=neighbourhood_group))+
  geom_boxplot()+theme(panel.background = element_rect(fill="white", color="black"), legend.position = "none", text = element_text(family="Avenir"), axis.text = element_text(size=13))+
  xlab("")+scale_fill_manual(values=c("#FFE548", "#FFB20F", "#FF4B3E", "#386FA4", "#1D3461"))

#log price by room type
ggplot(newlistings, aes(x=room_type, y=log(price), fill=room_type))+geom_boxplot()+
  theme(panel.background = element_rect(fill="white", color="black"), legend.position = "none", text = element_text(family="Avenir"), axis.text = element_text(size=13))+
  xlab("")+scale_fill_manual(values=c("#FFE548", "#FFB20F", "#FF4B3E", "#386FA4", "#1D3461"))

#DISTRIBUTION OF CATEGORICAL 
table(newlistings$room_type)
table(newlistings$neighbourhood_group)
ggplot(newlistings, aes(neighbourhood_group))+geom_bar(color="black",fill="#4A6FA5")+
  theme(panel.background = element_rect( color="black", fill="white"), panel.grid = element_blank(), text=element_text(family="Avenir"))
ggplot(newlistings, aes(room_type))+geom_bar(color="black",fill="#4A6FA5")+
  theme(panel.background = element_rect( color="black", fill="white"), panel.grid = element_blank(), text=element_text(family="Avenir"))


#creating dummy variables
library(fastDummies)
newlistings1 = dummy_columns(newlistings, select_columns = 'neighbourhood_group', remove_most_frequent_dummy = T)
newlistings2 = dummy_columns(newlistings1, select_columns = 'neighbourhood', remove_most_frequent_dummy = T)
newlistings3 = dummy_columns(newlistings2, select_columns = 'room_type', remove_most_frequent_dummy = T)

#making original as Null 
newlistings3$neighbourhood <- NULL
newlistings3$neighbourhood_group <- NULL
newlistings3$room_type <- NULL

write.csv(newlistings3, "~/Downloads/no outliers - listings.csv")

