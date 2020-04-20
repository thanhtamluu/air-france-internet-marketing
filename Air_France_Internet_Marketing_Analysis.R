library(readxl)
library(ggplot2)

data <- read_excel('Air France Case Spreadsheet Supplement.xls', sheet = 2)


#creating profit column (Amount - Total Cost)
data$profit <- data$Amount - data$`Total Cost`

#subsetting profit > 0
profit_data <- data[which( data$profit > 0), ]


#check the 80% of revenue made
quantile(data$profit, 0.8)


#orderby
data <- data[order(-data$profit),]


corr_data <- profit_data[,c(24,19,16,17)]
                          
corr_data
cor(corr_data)

#result = no correlation in the data that isnt subset per publisher

profit_data$profit_quant <- NA
  
a <- quantile(profit_data$profit, probs = 0.8)
  
for(i in 1:nrow(profit_data)){
  if(profit_data$profit[i] > a){
    profit_data$profit_quant[i] <- 1
    }
    else{profit_data$profit_quant[i] <- 0}
  }



bigprofit_data <- profit_data[which(profit_data$profit_quant == 1),]


#keyword graphs
ggplot(bigprofit_data, 
       aes(x= bigprofit_data$`Publisher Name`, y=bigprofit_data$profit, fill= bigprofit_data$`Keyword`)) +
  geom_bar(stat="identity")+
  labs(x="Publisher Name", y="Profit", fill = 'Keyword')+
  ggtitle("Keywords That Generates 80% of the Profit")+
  coord_flip()

ggplot(bigprofit_data, 
       aes(x= bigprofit_data$`Publisher Name` , y=bigprofit_data$Impressions, fill= bigprofit_data$Keyword )) +
  geom_bar(stat="identity")+
  labs(x="Publisher Name", y="Impression",fill = 'Keyword')+ 
  ggtitle("Keywords That Impressed Customers and  Generates 80% of the Profit")+
  coord_flip()


#check keyword 

airine_ticket_data <- profit_data[which(profit_data$Keyword == 'airline ticket'),]

europe_travel_data <- data[which(data$Keyword == 'europe travel'),]

#result = overture dominates and should not make uniform strategy

#campaign graphs

ggplot(bigprofit_data, 
       aes(x= bigprofit_data$`Publisher Name`, y=bigprofit_data$`profit`, fill= bigprofit_data$Campaign)) +
  geom_bar(stat="identity")+
  labs(x="Publisher Name", y="Profit", fill = 'Campaign')+ 
  ggtitle("Campaigns That Generates 80% of the Profit")+
  coord_flip()

ggplot(bigprofit_data, 
       aes(x= bigprofit_data$`Publisher Name`, y=bigprofit_data$Impressions, fill= bigprofit_data$Campaign)) +
  geom_bar(stat="identity")+
  labs(x="Publisher Name", y="Impression", fill = 'Campaign')+ 
  ggtitle("Campaigns That Impressed Customers and  Generates 80% of the Profit")+
  coord_flip()


#overall data graphs

ggplot(data, 
       aes(x= data$`Publisher Name` ,  y= data$profit, fill = data$Status )) +
  geom_bar(stat = "identity")+
  labs(x= 'Publisher Name' , y = "Profit", fill = "Status") +
  ggtitle("Profit - Raw Data")


ggplot(data, 
       aes(x= data$`Publisher Name` ,  y= data$`Total Cost`, fill = data$Status )) +
  geom_bar(stat = "identity")+
  labs(x= 'Publisher Name' , y = "Total Cost", fill = "Status") +
  ggtitle("Total Cost - Raw Data")


ggplot(data, 
       aes(x= data$`Campaign`, y=data$`Total Cost`, fill= data$Status)) +
  geom_bar(stat="identity")+
  labs(x="Campaign", y="Total Cost", fill = 'Status') +
  ggtitle("Cost of Campaigns That Generates 80% of Profits") + 
  coord_flip()


ggplot(bigprofit_data, 
       aes(x= bigprofit_data$`Campaign`, y=bigprofit_data$`profit`, fill= bigprofit_data$Status)) +
  geom_bar(stat="identity")+
  labs(x="Campaign", y="Profit", fill = 'Status') + 
  ggtitle("Status of Campaigns That Generates 80% of Profits") +
  coord_flip()



#---------------------------------kayak data---------------------------------
kayak_data <- read_excel('Air France Case Spreadsheet Supplement.xls', sheet = 3)

#labelling
names(kayak_data)[1] <- "Search Engine"
names(kayak_data)[2] <- "Clicks "
names(kayak_data)[3] <- "Media Cost"
names(kayak_data)[4] <- "Total Bookings"
names(kayak_data)[5] <- "Avg Ticket"
names(kayak_data)[6] <- "Total Revenue"
names(kayak_data)[7] <- "Net Revenue"


kayak_data <- kayak_data[3,]


#classify per publisher
yahoo_us_data <- data[which(data$`Publisher Name` ==  'Yahoo - US'),]
google_us_data <- data[which(data$`Publisher Name` == 'Google - US'),]
msn_us_data <- data[which(data$`Publisher Name` == 'MSN - US'),]
overture_us_data <- data[which(data$`Publisher Name` == 'Overture - US'),]
google_global_data <- data[which(data$`Publisher Name` == 'Google - Global'),]
msn_global_data <- data[which(data$`Publisher Name` == 'MSN - Global'),]
overture_global_data <- data[which(data$`Publisher Name` == 'Overture - Global'),]


#subset kayak_df to only wanted columns
kayak_data <- kayak_data[c(1:4, 7)]

#creating values to generate the dataframe
aa <- "Google US"
bb <- "Yahoo US"
cc <- "MSN US"
dd <- "Overture US"
ee <- "Google Global"
ff <- "MSN Global"
gg <- "Overture Global"


aaa <- sum(google_us_data$Clicks) 
bbb <- sum(yahoo_us_data$Clicks) 
ccc <- sum(msn_us_data$Clicks) 
ddd <- sum(overture_us_data$Clicks) 
eee <- sum(google_global_data$Clicks) 
fff <- sum(msn_global_data$Clicks) 
ggg <- sum(overture_global_data$Clicks) 

aaaa <- sum(google_us_data$`Total Cost`) 
bbbb <- sum(yahoo_us_data$`Total Cost`) 
cccc <- sum(msn_us_data$`Total Cost`) 
dddd <- sum(overture_us_data$`Total Cost`) 
eeee <- sum(google_global_data$`Total Cost`) 
ffff <- sum(msn_global_data$`Total Cost`) 
gggg <- sum(overture_global_data$`Total Cost`) 

aaaaa <- sum(google_us_data$`Total Volume of Bookings`) 
bbbbb <- sum(yahoo_us_data$`Total Volume of Bookings`) 
ccccc <- sum(msn_us_data$`Total Volume of Bookings`) 
ddddd <- sum(overture_us_data$`Total Volume of Bookings`) 
eeeee <- sum(google_global_data$`Total Volume of Bookings`) 
fffff <- sum(msn_global_data$`Total Volume of Bookings`) 
ggggg <- sum(overture_global_data$`Total Volume of Bookings`) 

aaaaaa <- sum(google_us_data$Amount) 
bbbbbb <- sum(yahoo_us_data$Amount) 
cccccc <- sum(msn_us_data$Amount) 
dddddd <- sum(overture_us_data$Amount) 
eeeeee <- sum(google_global_data$Amount) 
ffffff <- sum(msn_global_data$Amount) 
gggggg <- sum(overture_global_data$Amount) 

#appending kayak_data 

kayak_data <- rbind(kayak_data, c(aa, aaa, aaaa, aaaaa, aaaaaa))
kayak_data <- rbind(kayak_data, c(bb, bbb, bbbb, bbbbb, bbbbbb))
kayak_data <- rbind(kayak_data, c(cc, ccc, cccc, ccccc, cccccc))
kayak_data <- rbind(kayak_data, c(dd, ddd, dddd, ddddd, dddddd))
kayak_data <- rbind(kayak_data, c(ee, eee, eeee, eeeee, eeeeee))
kayak_data <- rbind(kayak_data, c(ff, fff, ffff, fffff, ffffff))
kayak_data <- rbind(kayak_data, c(gg, ggg, gggg, ggggg, gggggg))

#kayak plots
ggplot(kayak_data, 
       aes(x= kayak_data$`Clicks `, y=kayak_data$`Total Bookings`, fill= kayak_data$`Search Engine`)) +
  geom_bar(stat="identity")+
  labs(x="Clicks", y="Total Bookings", fill = 'Search Engines') + 
  ggtitle("Search Engine Comparison - Clicks &  Bookings") 


ggplot(kayak_data, 
       aes(x= kayak_data$`Media Cost`, y=kayak_data$`Net Revenue`, fill= kayak_data$`Search Engine`)) +
  geom_bar(stat="identity")+
  labs(x="Media Cost", y="Net Revenue", fill = 'Search Engines') + 
  ggtitle("Search Engine Comparison - Media Cost &  ") +
  coord_flip()

#getting empirical values on dataset for slide purposes

x <- bigprofit_data[which(bigprofit_data$Keyword == "airline ticket"), ]
y <- bigprofit_data[which(bigprofit_data$Keyword == "[air france]"), ]
z <- bigprofit_data[which(bigprofit_data$Keyword == "[air france]"), ]
xx <- bigprofit_data[which(bigprofit_data$Keyword == "air france"), ]
yy <- bigprofit_data[which(bigprofit_data$Keyword == "airfrance"), ]

zz <- bigprofit_data[which(bigprofit_data$Keyword == "europe travel"), ]

xxx <- bigprofit_data[which(bigprofit_data$Campaign == "Air France Branded"), ] 

yyy <- bigprofit_data[which(bigprofit_data$Campaign == "Unassigned"), ] 

sum(x$profit)


sum(y$profit)

sum(xx$profit)
sum(yy$profit)


sum(zz$Impressions)
sum(xxx$profit)

sum(yyy$Impressions)


z <- bigprofit_data[which(bigprofit_data$Keyword == "[air france]"), ]
xx <- bigprofit_data[which(bigprofit_data$Keyword == "air france"), ]
yy <- bigprofit_data[which(bigprofit_data$Keyword == "airfrance"), ]

sum(z$Impressions)
sum(xx$Impressions)
sum(yy$Impressions)


#Subset analysis

#Create subsets based on publisher name
yahoo_us_data <-data[which(profit_data$`Publisher Name`=='Yahoo - US'),]

google_us_data <-data[which(profit_data$`Publisher Name`=='Google - US'),]
google_global_data <-data[which(profit_data$`Publisher Name`=='Google - Global'),]

msn_us_data <-data[which(profit_data$`Publisher Name`=='MSN - US'),]
msn_global_data <-data[which(profit_data$`Publisher Name`=='MSN - Global'),]

overture_us_data <-data[which(profit_data$`Publisher Name`=='Overture - US'),]
overture_global_data <-data[which(profit_data$`Publisher Name`=='Overture - Global'),]

#Create quantiles for each publisher at 0.8
a=quantile(yahoo_us_data$profit, probs=0.8)
b=quantile(google_us_data$profit, probs=0.8)
c=quantile(google_global_data$profit, probs=0.8)
d=quantile(msn_us_data$profit, probs=0.8)
e=quantile(msn_global_data$profit, probs=0.8)
f=quantile(overture_us_data$profit, probs=0.8)
g=quantile(overture_global_data$profit, probs=0.8)

#creating new empty column
yahoo_us_data$profit_quant <- NA
google_us_data$profit_quant <- NA
yahoo_us_data$profit_quant <- NA
overture_us_data$profit_quant <- NA


for (i in (1:nrow(yahoo_us_data)))
{
  
  if (yahoo_us_data$profit[i] > a)
  {
    yahoo_us_data$profit_quant[i] <- 1
  }
  else 
  {
    yahoo_us_data$profit_quant[i] <- 0
  }
}

yahoo_us_data$profit_quant

for (i in (1:nrow(google_us_data)))
{
  
  if (google_us_data$profit[i] > a)
  {
    google_us_data$profit_quant[i] <- 1
  }
  else 
  {
    google_us_data$profit_quant[i] <- 0
  }
}

google_us_data$profit_quant

for (i in (1:nrow(google_global_data)))
{
  
  if (google_global_data$profit[i] > a)
  {
    google_global_data$profit_quant[i] <- 1
  }
  else 
  {
    google_global_data$profit_quant[i] <- 0
  }
}

google_global_data$profit_quant

for (i in (1:nrow(google_us_data)))
{
  
  if (google_us_data$profit[i] > a)
  {
    google_us_data$profit_quant[i] <- 1
  }
  else 
  {
    google_us_data$profit_quant[i] <- 0
  }
}

google_us_data$profit_quant

for (i in (1:nrow(msn_us_data)))
{
  
  if (msn_us_data$profit[i] > a)
  {
    msn_us_data$profit_quant[i] <- 1
  }
  else 
  {
    msn_us_data$profit_quant[i] <- 0
  }
}

msn_us_data$profit_quant

for (i in (1:nrow(msn_global_data)))
{
  
  if (msn_global_data$profit[i] > a)
  {
    msn_global_data$profit_quant[i] <- 1
  }
  else 
  {
    msn_global_data$profit_quant[i] <- 0
  }
}

msn_global_data$profit_quant

for (i in (1:nrow(overture_global_data)))
{
  
  if (overture_global_data$profit[i] > a)
  {
    overture_global_data$profit_quant[i] <- 1
  }
  else 
  {
    overture_global_data$profit_quant[i] <- 0
  }
}

overture_us_data$profit_quant

for (i in (1:nrow(overture_us_data)))
{
  
  if (overture_us_data$profit[i] > a)
  {
    overture_us_data$profit_quant[i] <- 1
  }
  else 
  {
    overture_us_data$profit_quant[i] <- 0
  }
}

#checking the new data
overture_us_data$profit_quant

#create new variable corr to save the correlation between few metrics
#Correlation yahoo
corr_data_yus<-yahoo_us_data[,c(24,19,16,17)]
cor(corr_data_yus)
#Correlation google US
corr_data_gus<-google_us_data[,c(24,19,16,17)]
cor(corr_data_gus)
#Correlation google Global
corr_data_ggl<-google_global_data[,c(24,19,16,17)]
cor(corr_data_ggl)
#Correlation msn US
corr_data_mus<-msn_us_data[,c(24,19,16,17)]
cor(corr_data_mus)
#Correlation msn Global
corr_data_mgl<-msn_global_data[,c(24,19,16,17)]
cor(corr_data_mgl)
#Correlation overture US
corr_data_ous<-overture_us_data[,c(24,19,16,17)]
cor(corr_data_ous)
#Correlation Overture Global
corr_data_ogl<-overture_global_data[,c(24,19,16,17)]
cor(corr_data_ogl)


#logistic regression
data_log<-lm(profit_quant==1 ~yahoo_us_data$Impressions + yahoo_us_data$`Engine Click Thru %` , data=yahoo_us_data,family='binomial')
summary(data_log)

#the regression is to justify our model
#we found that engine click through and trans conv% are noise variables since they can't really represent the performance of the dependent variable which is profits and impressions