install.packages("tm")
library("tm")
build<-read.csv(file.choose())
attach(build)
data<-(build[,1:3])
View(data)
attach(data)
data_a<-data[data$Type=="A",]###3987
data_b<-data[data$Type=="B",]###1246
data_c<-data[data$Type=="C",]####685
data_d<-data[data$Type=="D",]####152
DAT_A<-rbind(data_a[1:3987,],data_b[1:1246,],data_c[1:685,],data_d[1:152,])
DAT_B<-rbind(data_a[3988:5697,],data_b[1247:1781,],data_c[686:979,],data_d[153:218,])
main_dat<-rbind(DAT_A,DAT_B)
sms_corpus<-Corpus(VectorSource(main_dat$Posts))
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
stopwdrds = readLines(file.choose())
stopwdrds=c(stopwdrds,"http","https","com","dont", "people", "feel", "time", "ive", "things", "love", "good", "life" ,"lot", "find" ,"youre", "make", "thing", "type" ,"person", "infp", "infj" ,"pretty", "intp", "friends", "read", "years", "hard" ,"doesnt" ,"back", "didnt", "work", "thread", "mind" ,"friend", "thought", "makes", "kind", "world" ,"talk", "intj", "post" ,"understand", "give", "day", "idea", "ill" ,"thinking", "bit" ,"made", "bad" ,"agree", "long", "point", "feeling", "guess", "partschool" ,"lol", "types", "infps" ,"happy" ,"personality", "social" ,"sense", "relationship", "times", "end", "great", "tend" ,"hate", "music", "reason", "interesting" ,"reading" ,"wrong", "high" ,"stuff" ,"job" ,"found", "isnt" ,"live", "feelings", "words", "answer", "care", "shes", "problem" ,"fun" ,"put" ,"talking","real" ,"infjs" ,"year", "theyre", "man" ,"question","enfp", "havent" ,"nice" ,"ago" ,"start" ,"guy" ,"show" ,"big", "enjoy", "hes", "change", "play" ,"experience", "wouldnt", "guys", "sounds" ,"yeah", "remem","dont", "people" ,"time", "ive", "feel" ,"love", "good", "things", "lot", "enfp", "type", "make", "youre", "pretty", "thing", "entp" ,"thought", "find", "friends", "life" ,"bit", "work", "ill", "person", "friend", "back", "didnt", "relationship", "doesnt", "give", "read", "infj", "infp", "entj" ,"made" ,"post", "years", "makes", "guys", "point" ,"intj" ,"guess", "talkkind", "school", "bad", "hard" ,"day", "long", "thread", "question", "understand" ,"world" ,"enfj", "agreeentps" ,"thinking" ,"year", "personality", "mbti", "great", "care", "fun", "functions", "hes" ,"isnt", "mind" ,"happy", "enfps", "test", "theyre" ,"wanted", "experience", "havent", "types" ,"interesting" ,"times", "making", "part", "put", "wrong", "talking", "social", "sense", "guy", "ago", "true", "feeling", "start", "intp", "fact", "yeah", "music", "end" ,"idea" ,"stuff", "reading" ,"found", "high" ,"lol" ,"wont", "forum" ,"feelings", "close", "hate", "relate", "told", "big","man", "problem", "interested", "tend", "totally" ,"place" )
corpus_clean<-tm_map(corpus_clean,removeWords,stopwdrds)
corpus_clean<-tm_map(corpus_clean,removePunctuation)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "",x)
corpus_clean<- tm_map(corpus_clean,content_transformer(removeNumPunct))
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)
corpus_clean$content[1:2]
sms_dtm<-DocumentTermMatrix(corpus_clean)
class(sms_dtm)
library(caTools)
library(caTools)
View(sms_dtm)
sms_raw_train<-main_dat[1:6073,]
sms_raw_test<-main_dat[6074:8675,]
sms_dtm_train<-sms_dtm[1:6073,]
sms_dtm_test<-sms_dtm[6074:8675,]
sms_corpus_train<-corpus_clean[1:6073]
sms_corpus_test<-corpus_clean[6074:8675]
prop.table(table(main_dat$Type))
prop.table(table(sms_raw_test$Type))
sms_dict<-findFreqTerms(sms_dtm_train,3)
list(sms_dict[1:100])
sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
convert_counts<- function(x){
  
  x <-ifelse(x>0,1,0)
  x<-factor(x,levels=c(0,1),labels=c("No","Yes"))
}
sms_train<-apply(sms_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_test,MARGIN = 2,convert_counts)
library("e1071")
sms_classifier <- naiveBayes(sms_train,sms_raw_train$Type)
sms_train
sms_classifier$levels
sms_raw_train$Type
sms_test_pred <-predict(sms_classifier,sms_test)
sms_test_pred[1:25]
table1<-table(sms_test_pred,sms_raw_test$Type)
install.packages("gmodels")
library("gmodels")
CrossTable(sms_test_pred,sms_raw_test$Type,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,dnn = c('predicted','actual'))
sms_classifier2<-naiveBayes(sms_train,sms_raw_train$Type,laplace = 0.5)
sms_test_pred2 <-predict(sms_classifier2,sms_test)
table2<-table(sms_test_pred2,sms_raw_test$Type)
table2
accuracy1=(sum(diag(table1))/sum(table1))
accuracy1
accuracy2=(sum(diag(table2))/sum(table2))
accuracy2
