
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", dependencies = TRUE)
if(!require("ggmap")) install.packages("ggmap", dependencies = TRUE)
if(!require(maptools)) install.packages("maptools", dependencies = TRUE)
if(!require(formattable)) install.packages("formattable", dependencies = TRUE)
if(!require(treemap)) install.packages("treemap", dependencies = TRUE)
if(!require(qcc)) install.packages("qcc", dependencies = TRUE)
if(!require(maps)) install.packages("maps", dependencies = TRUE)
if(!require(factoextra)) install.packages("factoextra", dependencies = TRUE)
if(!require(randomForest)) install.packages("randomForest", dependencies = TRUE)
if(!require(party)) install.packages("party", dependencies = TRUE)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(DiagrammeR)) install.packages("DiagrammeR", dependencies = TRUE)

library(rsvg)
library(DiagrammeR)
library(kableExtra)
library(party)
library(rpart)
library(rpart.plot)
library("factoextra")
library(randomForest)
library(tidyr)
library(dplyr)
library(readr)
library(formattable)
library(stringr)
library(ggplot2)
library(maps)
library("ggmap")
library(treemap)
library(maptools)
library(qcc)


SiteResult <- read_csv("Zone-h.csv")
#Quick view of how information is formatted in dataframe

View(SiteResult)

set.seed(1)
dim(SiteResult)
kable(head(SiteResult), caption="Sample Data", booktabs=TRUE, linesep="")
nrow(SiteResult)
summary(SiteResult)

SiteResult %>% summarise_all(n_distinct)

#Reviewing columns that carry 1 variable

table <- SiteResult %>% select(H,M,R,L, View) %>% count(H,M,R,L,View)
# Column L and View do not provide any significant input to data set, therefore they will be removed
#Reviewing remaining columns
#H column indicates Homepage defacement
#M column indicates Mass defacement
#R Redefacement
#there will be 332 entries that will not be considered if we use the attributes H,M,R in our analysis.
Result <- SiteResult %>% select(-H,-M,-R,-L, -View) 


Result %>% summarise_all(n_distinct)

Result %>% select(OS) %>% unique()

#Reviewing the Domain column we see that the webaddresses contain subfolder location which is not required as of now
Result %>% select(Domain) %>% unique() %>% filter(str_detect(Domain,"/"))
#Removing trailing information after "/" from Domain Column
Result <- suppressWarnings(separate(data = Result, col= Domain, into = c("Domains", "Ext"), sep="/", convert = TRUE))
Result <- Result %>% select(-Ext, -"S No.")
Result$Domains


#result looks a better. exporting the Domain list to see if we can get further information
write.csv(Result$Domains, file = "SiteCheck.csv", row.names = FALSE)
#Refer to Python script where I have extracted Site information from ISP 

####Building Flow chart##########################################
grViz("digraph flowchart {
      # node definitions with substituted label text
      graph [label ='Webscraping Flow Chart', labelloc = t]
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']

      # edge definitions with the node IDs
      tab1 -> tab2 -> tab3;
      tab3 -> tab5;
      tab3 -> tab4 -> tab6 -> tab7
      }

      [1]: 'Create list from File'
      [2]: 'Cycle each element in list'
      [3]: 'Test DNS resolution of each element '
      [4]: 'If True - sent element for webscraping'
      [5]: 'If False - No action needed'
      [6]: 'Connect with IPAPI and webscrape data'
      [7]: 'Save recorded data in CSV file'
      ")


#################################################################

#reading Site data
ISPResult <- read.csv(file = "webscrape.csv")


#Checking duplicate information
ISPResult[duplicated(ISPResult$Site),]

duplicated(ISPResult$Site) %>% sum()
#only one duplicate. examinging the duplicat rows

#removing duplicate information
head(ISPResult, n=2)
ISPResult <- ISPResult[-c(2),]
ISPResult %>% summarise_all(n_distinct)

#Reviewing ISP dataset to drop columns that do not contribute to dataset
ISPResult %>% select(Services, Assignment, Blacklist) %>% unique()
#Removing columns not needed
ISPResult <- ISPResult %>% select(-a, -Services, -Assignment, -Blacklist)


ISPResult %>% select(IP.Address, IP.Decimel, ASN, Postal.Code)
#we do not require IP.Decimal, Postal.code. Dropping respective columns
ISPResult <- ISPResult %>% select(-IP.Decimel, -Postal.Code)

#examining latitude and longitude column
ISPResult %>% select(Latitude, Longitude) %>% head(n=10)
# we can exclude data after Â symbol for both columns.

ISPResult <- suppressWarnings(separate(data = ISPResult, col= Latitude, into = c("Lat"), sep="Â", convert = TRUE))
ISPResult <- suppressWarnings(separate(data = ISPResult, col= Longitude, into = c("Lon"), sep="Â", convert = TRUE))

#Merging both datasets to a Final dataset
Final <- merge(Result, ISPResult, by.x = "Domains", by.y = "Site")
#Formatting the Date column
Final$Dates <- as.Date(Final$Date, "%m/%d/%Y")
#Remving the old Date column 
Final <- Final %>% select(-Date)
#Removing the original datasets
remove(Result,SiteResult,ISPResult)


#Examining the Final dataset for further cleaning
Summarized <- Final %>% summarise_all(n_distinct)
Summarized
#count of Sites no longer active after hack. 
Final %>% filter(str_length(IP.Address) < 2) %>% nrow()

#Removing Sites that have been decomissioned.
Final <- Final %>% filter(str_length(IP.Address) > 2)
nrow(Final)



Final %>% group_by(Notifier) %>% arrange(Notifier) %>% select(Notifier) %>% unique() 
#Remove ./ from some names. these are improper data capture
Final %>% group_by(Notifier) %>% filter(str_detect(Notifier, "./"))  
Final$Notifier <-  str_replace(Final$Notifier,"./","")  


Final %>% summarise_all(n_distinct)
#convert character to lowercase to match multiple variance
Final$Notifier <- tolower(Final$Notifier)

Final %>% summarise_all(n_distinct)
#the number of Hackers under the Notifier column has decreased. This is because they were registered in duplicate as the 1st character for capital or small




#Selecting the 1st notifier we will try to detect pattern of behavior
top <- count(Final, Notifier) %>% arrange(desc(n)) %>% top_n(1) %>% select(Notifier)


#Grouping Hacker based on the IP addresses that they have attached we get about 1,973 combinations
Final %>% group_by(Notifier) %>% select(IP.Address, Lat, Lon)

#Total number of Hackers are 452
unique(Final$Notifier) %>% length()

#Filtering informaiton on the top most hacker. Total contributiosn will be about 160 attacks
Final %>% group_by(Notifier) %>% filter(Notifier == top) %>% nrow()

#List of IP addresses that were affected by Hacker
Final %>% group_by(Notifier) %>% filter(Notifier == top) %>% select(IP.Address) %>% unique()

#Lets visualize the information to view the hackers attack pattern
#Viewing the overall attacks captured in the database
mapper <- Final %>% select(Lon,Lat) %>% count(Lon,Lat) 
mapWorld <- borders("world", colour="gray50", fill="gray50")
mp <- ggplot() +   mapWorld
mp <- mp+ geom_point(aes(x=mapper$Lon, y=mapper$Lat) ,color="blue", size=2, alpha=0.4 )
mp

remove(mp)
mapper <- Final %>% group_by(Notifier) %>% filter(Notifier %in% top$Notifier) %>% select(Notifier,Lon,Lat) 
mapWorld <- borders("world", colour="gray50", fill="gray50")
mp <- ggplot() +   mapWorld
mp <- mp+ geom_point(aes(x=mapper$Lon, y=mapper$Lat) ,color="blue", size=2, alpha=0.4 ) 
mp
#we observer the the number of attacks are concentrated to a few specific region

Final %>% group_by(Notifier) %>% filter(Notifier==top) %>% select(Notifier,Lon,Lat) %>% count(Lon,Lat)
Final %>% group_by(Notifier) %>% filter(Notifier==top) %>% distinct(Organization, ISP)
Final %>% group_by(Notifier) %>% filter(Notifier==top) %>% count(ISP) %>% arrange(desc(n))

#Isolating the highest attack count top hacker and quering total attacks received by ISP 
Final %>% group_by(ISP) %>% filter(ISP == " OVH SAS") %>% distinct(Notifier)
#We observer that there is a general interest by Hackers relating to certain ISP. This could be due to underlining weekness from ISP infrastructure

#Quering the highest attack count an ISP has received. 
top <- Final %>% group_by(ISP) %>% count(ISP) %>% arrange(desc(n)) %>% head(n=1) 
#We identify that the "Cyber Internet Services" has highest count

Final %>% group_by(ISP) %>% filter(ISP ==top$ISP) %>% count(Notifier)  
#We observer that 62 Hackers has attacked this service provider.



#Global Hacking distribution

topResult <- Final  %>% group_by(Notifier) %>% count(Notifier) %>% arrange(desc(n)) 
topResult$cumsum <-  cumsum(topResult$n)
topResult$obs <- 1:nrow(topResult)  

ggplot(topResult, aes(x=obs)) +
  geom_bar(aes(y=n), fill="blue", stat="identity") +
  geom_point(aes(y=cumsum)) +
  geom_path(aes(y=cumsum, group=1))

#Initial obversation states that majority of result is focused on below 100 records

#refining to top 100 results
topResult %>% head(n=100) %>%
  ggplot(aes(x=obs)) +
  geom_bar(aes(y=n), fill="blue", stat="identity") +
  geom_point(aes(y=cumsum)) +
  geom_path(aes(y=cumsum, group=1))

#we can decrease a bit more
#using pareto library for better visual

Pareto <- head(topResult$n,n=30)
names(Pareto) = head(topResult$obs,n=30)
pareto.chart(Pareto,cumperc=seq(0,100, by=10), main="Pareto Analysis", col = rainbow(50),ylab="Count", ylab2="Freq"  )

#Eventhough chart depicts that 80% of attacks came from less then 20 hackers we will still keep our data refined top 30 hackers
top <- count(Final, Notifier) %>% arrange(desc(n)) %>% top_n(30) %>% select(Notifier)
remove(mp)
mapper <- Final %>% group_by(Notifier) %>% filter(Notifier %in% top$Notifier) %>% select(Notifier,Lon,Lat) 
mapWorld <- borders("world", colour="gray50", fill="gray50")
mp <- ggplot() +   mapWorld
mp <- mp+ geom_point(aes(x=mapper$Lon, y=mapper$Lat) ,color="blue", size=2, alpha=0.4 ) 
mp
#the three cluster matches the initial assesment

#focusing on Operating systems attacked
Final$OS %>% unique() 
Final$OS %>% unique() %>% length()
#out of the 12 Operating system attacked. there are only 3 that are prefered by top 30 Hackers.
#this could also mean that the respective region only supports certain OS. Further clarification is required.


#top <- Final %>% group_by(Notifier) %>% count(Notifier,OS,Lat,Lon) %>% arrange(desc(n)) %>% head(n=30)
OSCount <- Final %>%  filter(Final$Notifier %in% top$Notifier) %>% count(OS) 

#Two out of the 3 Operation system match our assesment





#Comparing to OS distribution worldwide
Final %>% group_by(OS) %>% count(OS) %>% arrange(desc(n))

mapWorld <- borders("world", colour="gray50", fill="gray50")
Final %>% group_by(OS) %>% count(OS, Lon,Lat)  %>% 
  ggplot() + mapWorld +
  geom_point(aes(x=Lon, y=Lat ,color=OS)) +
  theme(legend.position = "none" , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~OS)
#We observer that the Linux, win2000, win2003, win2012 match our 3 cluster but this does not coencide with our assesment. Operating system may not be the deciding factor that attracts hackers.

Final %>% filter(Notifier %in% top$Notifier) %>% group_by(OS) %>% count(OS, Lon,Lat) %>%
  ggplot() + mapWorld +
  geom_point(aes(x=Lon, y=Lat ,color=OS))+ 
  theme(legend.position = "none" , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~OS)
#we observer that Linux and win2012 still follow our assesment patern for top 30 hacker interest

#Reviewing countries affected by attacks
input <- Final %>% count(Country) %>% arrange(desc(n))

Pareto <-  input$n
names(Pareto) = input$Country
pareto.chart(Pareto,cumperc=seq(0,100, by=20), main="Pareto Analysis", col = rainbow(50),ylab="Count", ylab2="Freq"  )

#Examining the result we can deduce that 80% of web content is hosted in Pakistan and US. For better saturation we will include top 3 countries.
#Focusing on the top 3 countries 
input <- head(input, n=3)

Final %>% filter(Country %in% input$Country) %>% count(Country,OS, ISP) %>% arrange(desc(n))
Final %>% count(Country,OS, ISP) %>% arrange(desc(n))
#we can deduce that three clusters can be created basked on country affected.
#these clusters will be the same irrespective of what attribute we select


remove(mp)
mapper <- Final %>% group_by(OS) %>% select(OS,Lon,Lat) %>% count(OS, Lon,Lat) 
mapWorld <- borders("world", colour="gray50", fill="gray50")
mp <- ggplot() +   mapWorld
mp <- mp + #geom_point(aes(x=mapper$Lon, y=mapper$Lat ,color=mapper$OS), size=2, alpha=0.7 ) +
  stat_density2d(aes(x = mapper$Lon, y = mapper$Lat), size = 0.3) +
  scale_fill_gradient(low = "light blue", high= "dark blue") 
mp

#Clustering based on OS attacked provides us 3 groups

remove(mp)
mapper <- Final %>% group_by(ISP) %>% select(ISP, Lon,Lat) %>% count(ISP,Lon,Lat) 
mapWorld <- borders("world", colour="gray50", fill="gray50")
mp <- ggplot() +   mapWorld
mp <- mp + #geom_point(aes(x=mapper$Lon, y=mapper$Lat ,color=mapper$OS), size=2, alpha=0.7 ) +
  stat_density2d(aes(x = mapper$Lon, y = mapper$Lat), size = 0.3) +
  scale_fill_gradient(low = "light blue", high= "dark blue") 
mp
#Clustering based on ISP produces the same result


#str(Final)
#Final$ISP <- as.factor(Final$ISP)
#Final$Country <- as.factor(Final$Country)


a <- Final %>% group_by(Notifier) %>% count(Notifier) %>% arrange(desc(n)) %>% select(Notifier)
b <- seq.int(nrow(a))
Final1 <- data.frame(a,Hacker = b)
Final <- left_join(Final,Final1, by="Notifier")

a <- Final %>% group_by(OS) %>% count(OS) %>% arrange(desc(n)) %>% select(OS)
b <- seq.int(nrow(a))
Final1 <- data.frame(a,OpSys = b)
Final <- left_join(Final,Final1, by="OS")

a <- Final %>% group_by(ISP) %>% count(ISP) %>% arrange(desc(ISP)) %>% select(ISP)
b <- seq.int(nrow(a))
Final1 <- data.frame(a,ISPs = b)
Final <- left_join(Final,Final1, by="ISP")

a <- Final %>% group_by(Country) %>% count(Country) %>% arrange(desc(n)) %>% select(Country)
b <- seq.int(nrow(a))
Final1 <- data.frame(a,Ctry = b)
Final <- left_join(Final,Final1, by="Country")
remove(Final1,a,b)
set.seed(123)


#ML Kmeans - Selecting the Hacker,ISPs, Ctry, OpSys column to identify contributers to Kmean clustering algoritm
Final1 <- Final %>% select(Hacker, OpSys, ISPs, Ctry)
results <- kmeans(Final1, 3)
plot(Final1[c("Hacker", "ISPs", "Ctry", "OpSys")], col=results$cluster)
#We can conclude from graph that the deciding factor for Hackers to pick a relevant targit is the ISP first, then the country hosted and lastly the OS. 


#Our first prediction was based on Hackers perspective. This report is to propose a web developement plan that minimizes risk of being attacked.
#We need to consider when developing a web portal the initial decision that takes place.
#The language it will be programed in
#The OS it will be hosted on
#The country it will be hosted in
#The service provider it will be hosted in


#lets remove the Hacker column and see what other clusters emerge. 
Final1$Hacker <- NULL
fviz_nbclust(Final1, kmeans, method = "gap_stat")
#a total of 6 clusters are applicable to our filtered data set 
#but for some reason when i use the same code in knitr it shows me 2 clusters.
#therefore i have removed it from my final documentation. 


#####Extra code that i worked on. Might use it in later projects
#Converting the Operating system to a factor to be further used in ML algorithm
#
#rf <- randomForest(OpSys~., data=Final1)
#print(rf)
#plot(rf)
#OOB estimate of  error rate: 30.01% 
###########################################################


#Splitting the dataset to a training and test data set at 80:20 ration
remove(Final1)
Final1 <- Final %>% select(OpSys, ISPs, Ctry)
Final1$OpSys <- as.factor(Final1$OpSys)
pd <- sample(2, nrow(Final1), replace = TRUE, prob = c(0.8,0.2))
train <- Final1[pd==1,]
test <- Final1[pd==2,]

#building a Decision tree  
tree <- ctree(OpSys~., data=train, controls = ctree_control(mincriterion = 0.9, minsplit = 300) )
tree
plot(tree)
#there are a total of 19 nodes in this tree. the tree is set to 90 % confidence level and split is kept at 300
#this diagram is basically upside down with root at the top and leaves at the bottom. 
#checking the probability distribution of our dataset we observer that all rebictions belong to 1st 4 classes
predict(tree, test, type="prob")
predict(tree, test)
#We will try to use rpart for better visual plot

#Building a decision tree 
tree1 <- rpart(OpSys~., train)
rpart.plot(tree1, extra = 3)
rpart.rules(tree1)

#prediction 
predict(tree1, test)


#Misclassification error for "train" data
tab <- table(predict(tree), train$OpSys)
print(tab)
1-sum(diag(tab))/sum(tab)
#misclassification error is about 33% based on training data and is focused on top 4 used operating systems


testPrediction <- predict(tree, newdata=test)
tab <- table(testPrediction, test$OpSys)
print(tab)
1-sum(diag(tab))/sum(tab)
#misclassification error is about 33% based on test data and is also focused on top 4 used operating systems

