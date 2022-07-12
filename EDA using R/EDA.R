#OBJECTIVE:
#            I) To find out most successful teams and players contributing to win or loss
#            II)Suggest teams, players and companies who should endorse for its products


#DATA DESCRIPTION: The following report uses the dataset 'Matches' and 'Delivery' to make the analysis

library(readxl)

Mat= read.csv("C://Users//Srikar//Desktop//Study stuff//Internship//Grips Foundation//Task 5//Indian Premier League//Matches.csv")

head(Mat) #Viewing the first 6 rows of the dataset

#The dataset mathces contains the following variables
colnames(Mat)

length(Mat$id)
#The dataset contains 756 rows and 18 columns

#The dataset contains the following types of data
str(Mat)
#The dataset contains integers and Factor values

#Summary Statistics of the dataset, 'Matches'
summary(Mat)


#ANALYSIS:

#1) Finding the correlation between the variables in the dataset

#Keeping the variables that 

Mat1=Mat[,sapply(Mat,class)!= "factor"]
head(Mat1)

corr=cor(Mat1)
corr
corrplot::corrplot(corr,method=c('shade'))

#We observe negative and positive correlations in the data.

#Large Correlations: Large coreelations can be observed between the following variables:

#i) ID and Season, wins by runs and wins by wickets
#Ii) Rest are small and medium medium correlations 


#A) Finding the top 5 teams 

table(Mat$winner)

library(ggplot2)
ggplot(Mat, aes(winner,fill='sample'))+ geom_bar()

# The top 5 teams who have won most games since 2007 are:

#i) Mumbai Indians, Chennai Super Kings,Kolkata Knight Riders, Royal Challengers Bangalore and Kings XI Punjab

#B) Finding the most successful players based on 'Man of the Match'

MOM=table(Mat$player_of_match)
MOM

library(dplyr)
MOM1=Mat %>%
  count(Mat$player_of_match)

MOM2=data.frame(MOM1)
MOM2

colnames(MOM2)=c("Player","NS")
J=head(MOM2,10)

ggplot(J,aes(Player,NS))+geom_bar(stat='identity')

#C) Finding the number of mathches played in different citites

ggplot(Mat[which(!is.na(Mat$city)),],aes(city,fill= city,rm.na=T)) +geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  ylab("Number of Matches Played") +
  guides(fill=FALSE)

#D)Finding if toss influences games

Mat$toss_match=ifelse(as.character(Mat$toss_winner)==as.character(Mat$winner),"Won","Lost")
ggplot(Mat[which(!is.na(Mat$toss_match)),],aes(toss_match, fill = toss_match))+ 
  geom_bar()+ xlab("Toss") +ylab("Number of matches won")+ ggtitle("Influence of toss on game")



deliveries=read.csv("C://Users//Srikar//Desktop//Study stuff//Internship//Grips Foundation//Task 5//Indian Premier League//deliveries.csv")

#ii) Finding the top bowlers by the wickets taken out

df=deliveries %>% group_by(bowler) %>% filter(player_dismissed!="") %>% summarise(wickets= length(player_dismissed)) %>% top_n(n=10,wt=wickets) 
df %>% ggplot(aes(reorder(bowler,-wickets),wickets,fill=bowler))+geom_bar(stat = "identity") + ylab("Wickets")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ ggtitle("Top Bowlers")+ guides(fill=F)  

#iii) finding the batsmen with top strike rate

deliveries %>% group_by(batsman) %>% filter(length(total_runs)>500) %>% summarise(strike_rate= mean(batsman_runs)*100) %>% top_n(n=10,wt=strike_rate) %>%
  ggplot(aes(reorder(batsman,-strike_rate),strike_rate,fill=batsman))+ geom_bar(stat="identity",fill='red')+ xlab("Batsman") + ylab("Strike Rate") +
  ggtitle("Batsmen with top strike rate",subtitle = "Minimum 500 balls faced")+
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) 

#iv)Finding the best batsmen

df1=deliveries %>% group_by(batsman)%>% summarise(runs=sum(batsman_runs)) %>% arrange(desc(runs)) %>%
  filter(runs > 3000) 
df1 %>% ggplot(aes(reorder(batsman,-runs),runs,fill=batsman)) +geom_bar(stat = "identity") +xlab("Batsman")+ ylab("Runs")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ ggtitle("Best 15 batsmen")+ guides(fill=F)


#INTERPRETATION

# 1) The most successful teams are Chennai Super Kings, Mumbai Indians and Kolkata Knight Riders. 

# 2) The most successful players are A.Mishra, A.Nehra and chandala are the best bowlers. The best batsmen include
#      V.Kohli, SK Raina and RG Sharma.

# 3) Factors contributing to win or loss are Tossing coin, city played and stadium played seem to have an effect on the games won.

# 4) The players and teams endorsed would be the best teams and players mentioned above,
     

