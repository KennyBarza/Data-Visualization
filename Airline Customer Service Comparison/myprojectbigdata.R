library(twitteR)
library(RCurl)
library(stringr)
library(ggplot2)
library(tm)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)


######################### PLEASE NOTE BEFORE USING THIS PROGRAME!!! ######################################################################
#### THIS SCRIPT IS BEST USED IF YOU USE THE USERTIMLINE FUNCTION. RESULTS MAY CHANGE A LITTLE BIT, BUT THE MAIN IDEA IS THE SAME. I WILL
#BE PROVIDING MY DATA ANYWAY. 
#Importig the data will result in some error in the code. 



### Getting R connected to twitter API
key<-"wlkaw8Jyl7UWQZjgAuAVJCNOe"
secret_key<-"K0u2wOuXwxNkWtc1TJVg81VvGMobVug4Wj9moNvV3teWSS5pDZ"

access_token<-"2933481336-M01ifDI9ExCamnSR7sHaiGAShGJxpkSjyTWYmVq"
access_secret<-"f0iItuKeSz4oB4WApTWsg4uMY8Ez5VDm7yQ6nplkgZqdW"

setup_twitter_oauth(consumer_key = key,consumer_secret = secret_key,access_token = access_token,access_secret = access_secret)
#Getting the first 2000 tweets tweeted by American Airlines (@AmericaAir)
americanarilines<-userTimeline("AmericanAir",n=2000)
#getting the text only
americanarilines_text<-sapply(americanarilines, function(x) x$getText())
#getting the date in whic it was created
american_truedate<-sapply(americanarilines, function(x) x$getCreated())
#transforming it into real date because at first, the date was goven numerically
american_truedate<-as_datetime(american_truedate)


#Transforming our date time to only hours
new_time<-hour(american_truedate)
#transforming our list to data frame
new_time<-as.data.frame(new_time)
#adding in the name of the column
colnames(new_time)<-"time"
#arranging bu descending order
new_time<-arrange(new_time, desc(time))
#Creating a histogram
hist(new_time$time,col = "white",border="blue", 
     main="Variation of the numeber of tweets over time for american airlines",
     xlab="time in hour")

#Replacing thank you by thanks in each tweet
americanarilines_text<-sub('thank you','thanks',americanarilines_text[1:2000],ignore.case = T)
#Replacing we're by we are in each tweet
americanarilines_text<-sub("we're","we are",americanarilines_text[1:2000],ignore.case = T)
#Replacing we'd by we would in each tweet###########
americanarilines_text<-sub("we'd","we woudl",americanarilines_text[1:2000],ignore.case = T)
#Replacing we'll by we will in each tweet
americanarilines_text<-sub("we'll","we will",americanarilines_text[1:2000],ignore.case = T)
#Replacing you're by you are in each tweet
americanarilines_text<-sub("you're","you are",americanarilines_text[1:2000],ignore.case = T)
#Replacing pls by please in each tweet
americanarilines_text<-sub("pls","please",americanarilines_text[1:2000],ignore.case = T)
#Replacing sry by sorry in each tweet
americanarilines_text<-sub("sry","sorry",americanarilines_text[1:2000],ignore.case = T)

americanarilines_text<-gsub("&amp","",americanarilines_text[1:2000])
#Removing punctuation
americanarilines_text<-gsub("[[:punct:]]","",americanarilines_text[1:2000])
#Removing the
americanarilines_text<-gsub("the","",americanarilines_text[1:2000])
#Removing you
americanarilines_text<-gsub("you","",americanarilines_text[1:2000])
#Removing your
americanarilines_text<-gsub("your","",americanarilines_text[1:2000])
#Removing your
americanarilines_text<-gsub("and","",americanarilines_text[1:2000])
#Removing and
americanarilines_text<-gsub("with","",americanarilines_text[1:2000])
#Removing with
americanarilines_text<-gsub("for","",americanarilines_text[1:2000])
#Removing for
americanarilines_text<-gsub("this","",americanarilines_text[1:2000])
#Removing this
americanarilines_text<-gsub("that","",americanarilines_text[1:2000])
#removing are
americanarilines_text<-gsub("are","",americanarilines_text[1:2000])

#Counting how many http were use, then how many numbers, then DM
http<-sum(grepl('http',americanarilines_text,ignore.case = T))/2000*100
number<-sum(grepl('[0-9]{3}|[0-9]{4}',americanarilines_text))/2000*100
DM<-sum(grepl('DM',americanarilines_text))/2000*100

http<-sum(grepl('http&[0-9]{3}|[0-9]{4}',americanarilines_text,ignore.case = T))/2000*100

#putting the result in a bar graph
bar1<-data.frame("possibilities"=c("http","number","DM"),"Result"=c(http,number,DM))
bar1

#plotting the result
bar<-ggplot(data=bar1,aes(x=possibilities,y=Result))+ geom_bar(stat="identity",color="blue",fill="white")+
  geom_text(aes(label=Result),vjust=-0.3,size=3.5)+theme_minimal()
bar

########################
#checkint if these are put together. We are counting how many tweets that contain both DM and http, DM number ornumber http
DM_number <- grepl("[0-9]{3}|[0-9]{4}", americanarilines_text) & grepl("DM", americanarilines_text) 
DM_number<-sum(DM_number)/2000*100
http_number <- grepl("[0-9]{3}|[0-9]{4}", americanarilines_text) & grepl("http", americanarilines_text) 
http_number<-sum(http_number)/2000*100
DM_http<- grepl("http", americanarilines_text) & grepl("DM", americanarilines_text) 
DM_http<-sum(DM_http)/2000*100


#putting the result in a data frame
bar2<-data.frame("possibilities"=c("http&DM","number&DM","http&number"),"Result"=c(DM_http,DM_number,http_number))
bar2

#plotting the result i a bar graph
bar22<-ggplot(data=bar2,aes(x=possibilities,y=Result))+ geom_bar(stat="identity",color="blue",fill="white")+
  geom_text(aes(label=Result),vjust=-0.3,size=3.5)+theme_minimal()
bar22


library(tm)
library(stringi)

#putting the trweets as a data frame
tweets<-data.frame(doc_id=seq(1:2000),text=americanarilines_text)

#creating our own stopwrods list. We added to the old, these words
custom.stopwords<-c(stopwords('english'),'lol','smh','american airlines')

clean.corpus<-function(corpus)
{
  #trasforming every letter to lower case
  corpus<-tm_map(corpus,content_transformer(tolower))
  #removing stopwords
  corpus<-tm_map(corpus,removeWords,custom.stopwords)
  #remove punctuation
  corpus<-tm_map(corpus,removePunctuation)
  #remove whitespaces
  corpus<-tm_map(corpus,stripWhitespace)
  #remove numbers
  corpus<-tm_map(corpus,removeNumbers)
  return(corpus)
}
#transforming tweets iinto a Corpus so that we can use tm package
corpus<-VCorpus(DataframeSource(tweets))
clean.corpus(corpus)

#transforming the corpus into TDM format
tdm<-TermDocumentMatrix(corpus)
tdm.tweets<-as.matrix(tdm)

#creating a table that displays how many times each word have been used
term.freq<-rowSums(tdm.tweets)
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)

###################

#Plotting the top 20 most used words
ggplot(freq.df%>%
         arrange(desc(frequency))%>%
         filter(frequency>138)
       ,aes(x=reorder(word,-frequency),y=frequency))+
  geom_bar(stat='identity',fill='darkred')+coord_flip()+
  geom_text(aes(label=frequency),colour="white",hjust=1.25,size=5.0)


#findAssocs function allows us to find assoiciation with a specific word. in this example we are finding what are the most used word with the word 'our'
associationsour<-findAssocs(tdm,'our',0.11)
associationsour<-as.data.frame(associationsour)
associationsour<-mutate(associationsour,terms=row.names(associationsour))
associationsour<-top_n(associationsour,8)

#we plotted the the 8 words most used with our
ggplot(associationsour,aes(y=terms))+
  geom_point(aes(x=our),data=associationsour,size=5)+
  geom_text(aes(x=our,label=our),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())

#we did the same thing with please
associationsplease<-findAssocs(tdm,'please',0.11)
associationsplease<-as.data.frame(associationsplease)
associationsplease<-mutate(associationsplease,terms=row.names(associationsplease))
associationsplease<-top_n(associationsplease,8)

ggplot(associationsplease,aes(y=terms))+
  geom_point(aes(x=please),data=associationsplease,size=5)+
  geom_text(aes(x=please,label=please),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())

#we did the same thing with will
associationswill<-findAssocs(tdm,'will',0.11)
associationswill<-as.data.frame(associationswill)
associationswill<-mutate(associationswill,terms=row.names(associationswill))
associationswill<-top_n(associationswill,8)

ggplot(associationswill,aes(y=terms))+
  geom_point(aes(x=will),data=associationswill,size=5)+
  geom_text(aes(x=will,label=will),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())

#we did the same thing with sorry
associationssorry<-findAssocs(tdm,'sorry',0.11)
associationssorry<-as.data.frame(associationssorry)
associationssorry<-mutate(associationssorry,terms=row.names(associationssorry))
associationssorry<-top_n(associationssorry,8)

ggplot(associationssorry,aes(y=terms))+
  geom_point(aes(x=sorry),data=associationssorry,size=5)+
  geom_text(aes(x=sorry,label=sorry),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())

#we did the same thing with team
associationsteam<-findAssocs(tdm,'team',0.11)
associationsteam<-as.data.frame(associationsteam)
associationsteam<-mutate(associationsteam,terms=row.names(associationsteam))
associationsteam<-top_n(associationsteam,8)


ggplot(associationsteam,aes(y=terms))+
  geom_point(aes(x=team),data=associationsteam,size=5)+
  geom_text(aes(x=team,label=team),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())


#we did the same thing with thanks
associationsthank<-findAssocs(tdm,'thanks',0.11)
associationsthank<-as.data.frame(associationsthank)
associationsthank<-mutate(associationsthank,terms=row.names(associationsthank))
associationsthank<-top_n(associationsthank,8)

ggplot(associationsthank,aes(y=terms))+
  geom_point(aes(x=thanks),data=associationsthank,size=5)+
  geom_text(aes(x=thanks,label=thanks),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())


#we did the same thing with flight
associationsflight<-findAssocs(tdm,'flight',0.11)
associationsflight<-as.data.frame(associationsflight)
associationsflight<-mutate(associationsflight,terms=row.names(associationsflight))
associationsflight<-top_n(associationsflight,8)

ggplot(associationsflight,aes(y=terms))+
  geom_point(aes(x=flight),data=associationsflight,size=5)+
  geom_text(aes(x=flight,label=flight),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())


#Here we are creating a tree with words. The closer the words, the closer are they related. we used sparse=0.95. The greter the sparse the more accurate it is
tdm2<-removeSparseTerms(tdm,sparse=0.95)
hc<-hclust(dist(tdm2,method="euclidean"),method="complete")
plot(hc,main="decision tree")

library(dendextend)
library(circlize)
hcd<-color_labels(hc,30,col=rainbow(30))
hcd<-color_branches(hcd,30,col=rainbow(30))
plot(hcd)

library(wordcloud)

wordcloud(corpus,max.words = 100,colors=c("Red","Darkred"))










#Getting tweets,tweeted by @Delta, 2000 last tweets

Delta<-userTimeline("Delta",n=2000)
Delta_text<-sapply(Delta, function(x) x$getText())




delta_time<-sapply(Delta, function(x) x$getCreated())


new_time1<-hour(delta_time)
new_time1<-as.data.frame(new_time1)
colnames(new_time1)<-"time"
new_time1<-arrange(new_time1, desc(time))
hist(new_time1$time,col = "white",border="blue", main="Graph showing how the numer of tweet change over time for Delta airline",xlab="time in hour")



#Subbing thank you with than you with thanks
Delta_text<-sub('thank you','thanks',Delta_text[1:2000],ignore.case = T)
#Subbing thank you with than you with thanks
Delta_text<-sub("we're","we are",Delta_text[1:2000],ignore.case = T)
#Subbing thank you with than you with thanks
Delta_text<-sub("we'd","we woudl",Delta_text[1:2000],ignore.case = T)
#Subbing thank you with than you with thanks
Delta_text<-sub("we'll","we will",Delta_text[1:2000],ignore.case = T)
#Subbing thank you with than you with thanks
Delta_text<-sub("you're","you are",Delta_text[1:2000],ignore.case = T)
#Subbing thank you with than you with thanks
Delta_text<-sub("pls","please",Delta_text[1:2000],ignore.case = T)
#Subbing thank you with than you with thanks
Delta_text<-sub("sry","sorry",Delta_text[1:2000],ignore.case = T)

#removing none useful words and punctuations
Delta_text<-gsub("&amp","",Delta_text[1:2000])
#removing punctuations
Delta_text<-gsub("[[:punct:]]","",Delta_text[1:2000])
#removing the
Delta_text<-gsub("the","",Delta_text[1:2000])
#removing you
Delta_text<-gsub("you","",Delta_text[1:2000])
Delta_text<-gsub("your","",Delta_text[1:2000])
Delta_text<-gsub("and","",Delta_text[1:2000])
Delta_text<-gsub("with","",Delta_text[1:2000])
Delta_text<-gsub("are","",Delta_text[1:2000])
Delta_text<-gsub("for","",Delta_text[1:2000])


#how many time do we have http. DMs or number
http1<-sum(grepl('http',Delta_text,ignore.case = T))/2000*100
number1<-sum(grepl('[0-9]{3}|[0-9]{4}',Delta_text))/2000*100
DM1<-sum(grepl('DM',Delta_text))/2000*100

# putting them into a data fame
bar1<-data.frame("possibilities"=c("http","number","DM"),"Result"=c(http1,number1,DM1))
bar1

#barplotting
bar<-ggplot(data=bar1,aes(x=possibilities,y=Result))+ geom_bar(stat="identity",color="black",fill="orange")+
  geom_text(aes(label=Result),vjust=-0.3,size=3.5)+theme_minimal()
bar


#checkingt if thes ar eput together. We are countng how many tweets that contain both DM and http, DM number or number http
DM_number <- grepl("[0-9]{3}|[0-9]{4}", Delta_text) & grepl("DM", Delta_text) 
DM_number<-sum(DM_number)/2000*100
http_number <- grepl("[0-9]{3}|[0-9]{4}", Delta_text) & grepl("http", Delta_text) 
http_number<-sum(http_number)/2000*100
DM_http<- grepl("http", Delta_text) & grepl("DM", Delta_text) 
DM_http<-sum(DM_http)/2000*100

#putting the result in a data frame
bar3<-data.frame("possibilities"=c("http&DM","number&DM","http&number"),"Result"=c(DM_http,DM_number,http_number))
bar3

#barploting the result
bar22<-ggplot(data=bar3,aes(x=possibilities,y=Result))+ geom_bar(stat="identity",color="black",fill="orange")+
  geom_text(aes(label=Result),vjust=-0.3,size=3.5)+theme_minimal()
bar22

#putting the tweets into data frame
tweets<-data.frame(doc_id=seq(1:2000),text=Delta_text)

#creatiing custom stopwords
custom.stopwords<-c(stopwords('english'),'lol','smh','delta')

#function tha cleans a corpus
clean.corpus<-function(corpus)
{
  #transform to lower case
  corpus<-tm_map(corpus,content_transformer(tolower))
  #remove stopwords from the corpus
  corpus<-tm_map(corpus,removeWords,custom.stopwords)
  #remove punctuations
  corpus<-tm_map(corpus,removePunctuation)
  #remove white spaces
  corpus<-tm_map(corpus,stripWhitespace)
  #remove numbers
  corpus<-tm_map(corpus,removeNumbers)
  return(corpus)
}
#transforming the data frame containing the tweets into a corpus
corpus<-VCorpus(DataframeSource(tweets))
clean.corpus(corpus)

#transforming the corpus into a TDM
tdm<-TermDocumentMatrix(corpus)
tdm.tweets<-as.matrix(tdm)
dim(tdm.tweets)
#Creatng a table that contains the top most used words
term.freq<-rowSums(tdm.tweets)
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)


#plotting the most  used words (top 20) into a bar graph
ggplot(freq.df%>%
         arrange(desc(frequency))%>%
         filter(frequency>138),aes(x=reorder(word,-frequency),y=frequency))+
  geom_bar(stat='identity',fill='darkblue')+
  coord_flip()+geom_text(aes(label=frequency),
                         colour="white",hjust=1.25,size=5.0)

#findAssocs function allows us to find assoiciation with a specific word. in this example we are finding what are the most used word with the word 'our'
associationsour<-findAssocs(tdm,'our',0.11)
associationsour<-as.data.frame(associationsour)
associationsour<-mutate(associationsour,terms=row.names(associationsour))
associationsour<-top_n(associationsour,8)

#we plotted the the 8 words most used with our
ggplot(associationsour,aes(y=terms))+
  geom_point(aes(x=our),data=associationsour,size=5)+
  geom_text(aes(x=our,label=our),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())

#we did the same thing with please
associationsplease<-findAssocs(tdm,'please',0.11)
associationsplease<-as.data.frame(associationsplease)
associationsplease<-mutate(associationsplease,terms=row.names(associationsplease))
associationsplease<-top_n(associationsplease,8)


ggplot(associationsplease,aes(y=terms))+
  geom_point(aes(x=please),data=associationsplease,size=5)+
  geom_text(aes(x=please,label=please),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())

#we did the same thing with confirmsation
associationshappy<-findAssocs(tdm,'happy',0.11)
associationshappy<-as.data.frame(associationshappy)
associationshappy<-mutate(associationshappy,terms=row.names(associationshappy))
associationshappy<-top_n(associationshappy,8)

ggplot(associationshappy,aes(y=terms))+
  geom_point(aes(x=happy),data=associationshappy,size=5)+
  geom_text(aes(x=happy,label=happy),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())

#we did the same thing with sorry
associationssorry<-findAssocs(tdm,'sorry',0.11)
associationssorry<-as.data.frame(associationssorry)
associationssorry<-mutate(associationssorry,terms=row.names(associationssorry))
associationssorry<-top_n(associationssorry,8)


ggplot(associationssorry,aes(y=terms))+
  geom_point(aes(x=sorry),data=associationssorry,size=5)+
  geom_text(aes(x=sorry,label=sorry),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())

#we did the same thing with happy
associationshappy<-findAssocs(tdm,'happy',0.11)
associationshappy<-as.data.frame(associationshappy)
associationshappy<-mutate(associationshappy,terms=row.names(associationshappy))
associationshappy<-top_n(associationshappy,8)

ggplot(associationshappy,aes(y=terms))+
  geom_point(aes(x=happy),data=associationshappy,size=5)+
  geom_text(aes(x=happy,label=happy),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())


#we did the same thing with thanks
associationsthank<-findAssocs(tdm,'thanks',0.11)
associationsthank<-as.data.frame(associationsthank)
associationsthank<-mutate(associationsthank,terms=row.names(associationsthank))
associationsthank<-top_n(associationsthank,8)

ggplot(associationsthank,aes(y=terms))+
  geom_point(aes(x=thanks),associationsthank,size=5)+
  geom_text(aes(x=thanks,label=thanks),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())


#we did the same thing with apologize
associationsapologize<-findAssocs(tdm,'apologize',0.11)
associationsapologize<-as.data.frame(associationsapologize)
associationsapologize<-mutate(associationsapologize,terms=row.names(associationsapologize))
associationsapologize<-top_n(associationsapologize,8)


ggplot(associationsapologize,aes(y=terms))+
  geom_point(aes(x=apologize),data=associationsapologize,size=5)+
  geom_text(aes(x=apologize,label=apologize),
            colour="darkred",hjust=-0.25,size=4)+
  theme(text=element_text(size=20),axis.title.y = element_blank())


#Herer wera creating a tree with words. The closer the words, the closer are they related. we used sparse=0.95. The greter the sparse the more accurate it is
tdm2<-removeSparseTerms(tdm,sparse=0.95)
hc<-hclust(dist(tdm2,method="euclidean"),method="complete")
plot(hc,main="decision tree")

library(dendextend)
library(circlize)
#Changing the the form pf the tree
hcd<-color_labels(hc,30,col=rainbow(30))
hcd<-color_branches(hcd,30,col=rainbow(30))
plot(hcd)

library(wordcloud)

#wordcloud
wordcloud(corpus,max.words = 100,colors=c("Darkgreen","green"))







# in this section we are looking to compare the two custom service


#creating a custom words
custom.stopwords1<-c(stopwords('english'),'sorry','amp','delta','amazon')

#function that cleans a vect
clean.vec<-function(text.vec){
  text.vec<-tolower(text.vec)
  text.vec<-removeWords(text.vec,custom.stopwords1)
  text.vec<-removePunctuation(text.vec)
  text.vec<-stripWhitespace(text.vec)
  text.vec<-removeNumbers(text.vec)
  return(text.vec)
}
#applying these two functions
americanairline.vect<-clean.vec(americanarilines_text)
delta.vect<-clean.vec(Delta_text)

#each vecteur will become a complete block of text
americanairline.vect<-paste(americanairline.vect,collapse = " ")
delta.vect<-paste(delta.vect,collapse=" ")

#putting these two texts into one variable
all<-c(americanairline.vect,delta.vect)
#creating a new corpus
corpusall<-VCorpus(VectorSource(all))

#creating a tdm document matirx (TDM)
tdmall<-TermDocumentMatrix(corpusall)
tdmall.m<-as.matrix(tdmall)
tdmall.m
#adding column names to each words in what text was found
colnames(tdmall.m)=c("americanairline","Delta")

display.brewer.all()

#these are the different colours that we can use. We decjded to use the purples pallete
pal<-brewer.pal(8,"Purples")
pal<-pal[-(1:4)]
### word in common between the two texts
commonality.cloud(tdmall.m,max.words = 200,random.order=F,colors=pal)

#Comparison cloud
comparison.cloud(tdmall.m,max.words = 200,
                 random.order =T,
                 title.size = 1.0,
                 colors=brewer.pal(ncol(tdmall.m),"Dark2"))


library(plotrix)

#creating a subset with all the common words
commonwords<-subset(tdmall.m,tdmall.m[,1]>0&tdmall.m[,2]>0)

#ablying the difference in absolute term. Lets say "sorry" wase used 100 time in Delta and 80 in american, we will have 20.
difference<-abs(commonwords[,1]-commonwords[,2])###absolute value

###bounding the difference column with the commonword
commonwords<-cbind(commonwords,difference)
#putting them in decreasing order
commonwords<-commonwords[order(commonwords[,3],decreasing=T),]

#putting the top 25 commonword in a data frame
top25.df<-data.frame(x=commonwords[1:25,1],y=commonwords[1:25,2],labels=rownames(commonwords[1:25,]))

#plotting a pyramid whic will represent the the differences
pyramid.plot(top25.df$x,top25.df$y,labels=top25.df$labels,
             gap=125,top.labels = c("Americanairlines","Words","delta"),
             main="words in common",laxlab = NULL,
             raxlab = NULL,unit=NULL)



install.packages("qdap")
library(qdap)
library(ggthemes)

#performing a sentiment analysis using the polarity function.
p<-polarity(americanarilines_text)
p2<-polarity(Delta_text)
#plotting the result
ggplot(p$all,aes(x=polarity,y=..density..))+theme_gdocs()+
  geom_histogram(binwidth = .25,
                 fill="darkred",colour="grey60",size=.2)+
  geom_density(size=.75)


ggplot(p2$all,aes(x=polarity,y=..density..))+theme_gdocs()+
  geom_histogram(binwidth = .25,
                 fill="darkred",colour="grey60",size=.2)+
  geom_density(size=.75)

#### ALWAYS REINSTALL QDAP IN ORDER TO USE IT, IT SEEMS THAT IS IT IS BUGGING.


#Calculating the frequency of the tweets.
k1<-abs((american_truedate[2000]-american_truedate[1]))*24*60*60
k1/2000


k2<-abs((delta_time[2000]-delta_time[1]))*24*60*60
k2/2000
##### here the number is gicen as date unit but it is not. It is in seconds that is due to the lubridate function

####I just copied the result. if i put the variable k ill get an error.
table5<-data.frame(company=c("American Airlines","Delta_Airlines"),
                   Secondpertweet<-c(212.2,236.2))

p2<-ggplot(data=table5, aes(x=company, y=Secondpertweet)) +
  geom_bar(stat="identity",color="blue",fill='white')
p2

#Calculating the mean of these tweets

mean_aa<-mean(nchar(americanarilines_text))
mean_delta<- mean(nchar(Delta_text))



#plotting the mean
table2<-c("american airlines","delta airlines")
table<-c(mean_aa,mean_delta)
tale4<-data.frame(company=c("American Airlines","Delta Airlines"),mean=c(mean_aa,mean_delta))
p<-ggplot(data=tale4, aes(x=company, y=mean)) +
  geom_bar(stat="identity",color="blue",fill='white')
p









### saving variable.
write.table(americanarilines_text,file="data.csv")
write.table(Delta_text,file="data2.txt")
write.table(american_truedate,file = "data3.txt")
write.table(delta_time,file="data4.txt")


Delta_text
