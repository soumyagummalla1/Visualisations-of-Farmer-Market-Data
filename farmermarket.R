#Name- Soumya Gummalla, #Section 04, Course ID- IE 6600

#Problem 1
library(dplyr)

farmers_market_info$State<-tolower(farmers_market_info$State)
farmers_market_info$State<-na.omit(farmers_market_info$State)

farmers_market_info<- drop_na()
State<-table(farmers_market_info$State) 
State<-as.data.frame(State)

State$Var1<-factor(State$Var1)

levels(State$Var1)<- list(
  'New England'=c('connecticut','maine','massachusetts','new hampshire','rhode island','vermont'),
  'Mideast'=c('delaware', 'district of columbia','maryland','new jersey','new york','pennsylvania'),
  'Great Lakes'=c('illinois','indiana', 'michigan', 'ohio','wisconsin'),
  'Plains'=c('iowa', 'kansas', 'minnesota', 'missouri', 'nebraska', 'north dakota','south dakota'),
  'Southeast'=c( 'florida', 'georgia','alabama', 'arkansas','kentucky', 'north carolina', 'south carolina','louisiana','mississippi', 'virginia','west virginia','tennessee'),
  'Southwest'=c(  'arizona','new mexico','oklahoma','texas'),
  'Rocky Mountain'= c( 'colorado', 'idaho', 'montana',   'utah','wyoming'),
  'Far West' =c('alaska', 'california', 'hawaii', 'oregon','washington','nevada'),
  'Island'=c('puerto rico','virgin islands')
)

library(ggplot2)
region_market <- ggplot(State, aes(x = reorder(Var1,Freq), y = Freq))+ geom_bar(stat="identity",fill='limegreen')+ggtitle("Number of Farmer Markets with Respect to Region") +
  xlab("Region") + ylab("Number of Markets")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))+coord_flip()
region_market 

#Problem 2

library(stringr)
library(lubridate)

#seperating start date for season1
Season1_date<-str_split_fixed(farmers_market_info$Season1Date, "to", 2)
Season1_date<-as.data.frame(Season1_date)
Season1_date$V1<-mdy(Season1_date$V1)
Season1_date$V2<-mdy(Season1_date$V2)
Season1_date<-na.omit(Season1_date)

#extracting start month
Season1_date<-Season1_date%>%
  mutate(Start_Month=month(V1))

#converting month into words
Season1_date$Start_Month<-month.abb[Season1_date$Start_Month]

#getting month frequency
Month_Season1<-table(Season1_date$Start_Month)
Month_Season1<-as.data.frame(Month_Season1)

#plot for months
Farm_time<-ggplot(Month_Season1, mapping = aes(x = Var1, y = Freq, group=1) ) + geom_line(color='grey',size=1)+geom_point(color='steel blue')+ggtitle("Change of Farmer Makerkets Over Months") +
  xlab("Month") + ylab("Markets")+ labs(color = "Delay Type")+theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))
Farm_time

#extrcating start year
year1<-Season1_date%>%
  mutate(Start_Year=year(V1))

#getting year frequency
startyear<-table(year1$Start_Year)
startyear<-as.data.frame(startyear)

#plot for years
Farm_Year<-ggplot(startyear, mapping = aes(x = Var1, y = Freq, group=1) ) + geom_line(color='grey',size=1)+geom_point(color='steel blue')+ggtitle("Change of Farmer Makerkets Over Years") +
  xlab("Year") + ylab("Markets")+ labs(color = "Delay Type")+theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))
Farm_Year


#Problem 3
library(plyr)
library(tidyr)
#selecting all the columns that have information about different products
produce <- farmers_market_info%>%
  select(29:58)
#delete columns with null values
produce<-na.omit(produce)

#collapse the columns into key-value pairs
produce<-produce %>% gather(key = "Type_of_produce", value= "No_of_Markets" )

#replacing the catagorical data with numbers
produce$No_of_Markets<-as.character(produce$No_of_Markets)
produce$No_of_Markets[produce$No_of_Markets == "Y"] <- "1"
produce$No_of_Markets<-as.numeric(produce$No_of_Markets)

#filtering the data to get the markets that have the products
produce<-produce%>%
  filter(No_of_Markets=="1")%>%
  select(Type_of_produce, No_of_Markets)
produce<-summarise_at(group_by(produce,Type_of_produce),vars(No_of_Markets),funs(sum(.,na.rm=TRUE)))

produce$Type_of_produce<-factor(produce$Type_of_produce)

#dividing the products into groups
levels(produce$Type_of_produce)<- list(
  'Processed'=c('Bakedgoods','Cheese','Prepared','Tofu','Coffee'),
  'Plant_Products'=c('Beans','Flowers','Fruits','Grains','Herbs','Mushrooms','Nursery','Nuts','Organic','Plants','Trees','Vegetables','WildHarvested'),
  'Liquid_Products'=c('Wine','Honey','Jams','Juices','Maple'),
  'Non_Veg'=c('Eggs','Meat','PetFood','Poultry','Seafood'),
  'Non_Food'=c('Crafts','Soap')
)
#plot to represent products in different markets
produceplot <- ggplot(produce, aes(x = reorder(Type_of_produce, No_of_Markets), y = No_of_Markets))+ geom_bar(stat="identity", fill='steelblue')+ggtitle("Number of Markets Having Different Tpyes of Products") +
  xlab("Type of Product") + ylab("Count")+theme( plot.title = element_text(color = "Red", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "Red",size=14,face="bold"))

produceplot

#Problem 4
#selecting all the columns with payments 
payment <- farmers_market_info%>%
  select(24:28)
#remove null values
payment<-na.omit(payment)

#collapsong into into key-value pairs 
payment<-payment %>% gather(key = "Type_of_payment", value= "Yes" )

#assigning numeric values 
payment$Yes<-as.character(payment$Yes)
payment$Yes[payment$Yes == "Y"] <- "1"
payment$Yes<-as.numeric(payment$Yes)

#extracting all the values with yes
payment<-payment%>%
  filter(Yes=="1")%>%
  select(Type_of_payment,Yes)

#obtaining the count of different payment methods 
payment<-summarise_at(group_by(payment,Type_of_payment),vars(Yes),funs(sum(.,na.rm=TRUE)))

#plotting the values 
paymentplot <- ggplot(payment, aes(x = reorder(Type_of_payment,Yes), y = Yes))+ geom_bar(stat="identity",fill='orange')+ggtitle("Different Types of Payment Accepted in the Farmer Markets") +
  xlab("Type of Payment") + ylab("Count")+theme( plot.title = element_text(color = "darkgreen", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "darkgreen",size=14,face="bold"))
paymentplot

#Problem 5
#getting the required columns
farm<-farmers_market_info
Payment_State<- farm%>%
  select(State, Credit, WICcash, WIC, SNAP, SFMNP)

Payment_State$State<-factor(Payment_State$State)

#dividing the states into regions
levels(Payment_State$State)<- list(
  'New England'=c('connecticut','maine','massachusetts','new hampshire','rhode island','vermont'),
  'Mideast'=c('delaware', 'district of columbia','maryland','new jersey','new york','pennsylvania'),
  'Great Lakes'=c('illinois','indiana', 'michigan', 'ohio','wisconsin'),
  'Plains'=c('iowa', 'kansas', 'minnesota', 'missouri', 'nebraska', 'north dakota','south dakota'),
  'Southeast'=c( 'florida', 'georgia','alabama', 'arkansas','kentucky', 'north carolina', 'south carolina','louisiana','mississippi', 'virginia','west virginia','tennessee'),
  'Southwest'=c(  'arizona','new mexico','oklahoma','texas'),
  'Rocky Mountain'= c( 'colorado', 'idaho', 'montana',   'utah','wyoming'),
  'Far West' =c('alaska', 'california', 'hawaii', 'oregon','washington','nevada'),
  'Island'=c('puerto rico','virgin islands')
)

#coverting the catagorical values of the paymnet columns into numbers 
Payment_State$Credit[Payment_State$Credit == "Y"] <- "1"
Payment_State$Credit[Payment_State$Credit == "N"] <- "0"
Payment_State$Credit<-as.numeric(Payment_State$Credit)

Payment_State$WICcash[Payment_State$WICcash == "Y"] <- "1"
Payment_State$WICcash[Payment_State$WICcash == "N"] <- "0"
Payment_State$WICcash<-as.numeric(Payment_State$WICcash)

Payment_State$WIC[Payment_State$WIC == "Y"] <- "1"
Payment_State$WIC[Payment_State$WIC == "N"] <- "0"
Payment_State$WIC<-as.numeric(Payment_State$WIC)

Payment_State$SNAP[Payment_State$SNAP == "Y"] <- "1"
Payment_State$SNAP[Payment_State$SNAP == "N"] <- "0"
Payment_State$SNAP<-as.numeric(Payment_State$SNAP)

Payment_State$SFMNP[Payment_State$SFMNP == "Y"] <- "1"
Payment_State$SFMNP[Payment_State$SFMNP == "N"] <- "0"
Payment_State$SFMNP<-as.numeric(Payment_State$SFMNP)

#summerising the data to get the total count according to the region
Payment_State<-Payment_State%>%
  group_by(State)%>%
  summarise_each(funs(sum))%>%
  gather(key = "Type_of_payment", value= "No_of_Markets", -State)

#plotting the data on a grouped bar chart
plot_paystate<-ggplot(Payment_State, aes(x=reorder(State,No_of_Markets), y=No_of_Markets, fill=Type_of_payment)) +
  geom_bar(stat="identity",position=position_dodge())+ggtitle("Type of Payments Accepted in Different Regions") +
  xlab("Region") + ylab("Count of Payment Type")+  theme_minimal()+labs(fill = "Tpye of Payment")+theme( plot.title = element_text(color = "dark blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "dark blue",size=14,face="bold"))+facet_wrap(~State,scales = "free_x")
plot_paystate






