rm(list=ls())

library(readxl)
library(stringr)
library(memoise)
library(KoNLP)
library(rvest)
library(rJava)
library(dplyr)


getwd()
setwd("C:/Users/eunse/Desktop/cr")
dir()
news = read.csv('news.csv')
View(news)
str(news)
news$date=as.character(news$date)
unique(news$ox)
id = which(news$ox==unique(news$ox)[1])
news = news[id,]
dim(news)
news = news[,-5]
news$text = as.character(news$text)
str(news)
text = news$text
length(text)

q1=news[which(news$date %in% c('2014.01','2014.02','2014.03')),4]
q2=news[which(news$date %in% c('2014.04','2014.05','2014.06')),4]
q3=news[which(news$date %in% c('2014.07','2014.08','2014.09')),4]
q4=news[which(news$date %in% c('2014.10','2014.11','2014.12')),4]
q5=news[which(news$date %in% c('2015.01','2015.02','2015.03')),4]
q6=news[which(news$date %in% c('2015.04','2015.05','2015.06')),4]
q7=news[which(news$date %in% c('2015.07','2015.08','2015.09')),4]
q8=news[which(news$date %in% c('2015.10','2015.11','2015.12')),4]
q9=news[which(news$date %in% c('2016.01','2016.02','2016.03')),4]
q10=news[which(news$date %in% c('2016.04','2016.05','2016.06')),4]
q11=news[which(news$date %in% c('2016.07','2016.08','2016.09')),4]
q12=news[which(news$date %in% c('2016.10','2016.11','2016.12')),4]
q13=news[which(news$date %in% c('2017.01','2017.02','2017.03')),4]
q14=news[which(news$date %in% c('2017.04','2017.05','2017.06')),4]
q15=news[which(news$date %in% c('2017.07','2017.08','2017.09')),4]
q16=news[which(news$date %in% c('2017.10','2017.11','2017.12')),4]
q17=news[which(news$date %in% c('2018.01','2018.02','2018.03')),4]
q18=news[which(news$date %in% c('2018.04','2018.05','2018.06')),4]
q19=news[which(news$date %in% c('2018.07','2018.08')),4]

list = c(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,q16,q17,q18,q19)

q<-str_replace_all(q17,"\\W"," ")
noun<-extractNoun(q)
wordcount<-table(unlist(noun))
wordcount<-as.data.frame(wordcount)
wordcount[,1]<-gsub("\\d+",NA,wordcount[,1])
wordcount<-na.omit(wordcount)
df.word<-as.data.frame(wordcount,stringAsFactors=F)
df.word<-rename(df.word,word=Var1,freq=Freq)
df.word17 <- filter(df.word, nchar(word)>=2)

q<-str_replace_all(q18,"\\W"," ")
noun<-extractNoun(q)
wordcount<-table(unlist(noun))
wordcount<-as.data.frame(wordcount)
wordcount[,1]<-gsub("\\d+",NA,wordcount[,1])
wordcount<-na.omit(wordcount)
df.word<-as.data.frame(wordcount,stringAsFactors=F)
df.word<-rename(df.word,word=Var1,freq=Freq)
df.word18 <- filter(df.word, nchar(word)>=2)

q<-str_replace_all(q19,"\\W"," ")
noun<-extractNoun(q)
wordcount<-table(unlist(noun))
wordcount<-as.data.frame(wordcount)
wordcount[,1]<-gsub("\\d+",NA,wordcount[,1])
wordcount<-na.omit(wordcount)
df.word<-as.data.frame(wordcount,stringAsFactors=F)
df.word<-rename(df.word,word=Var1,freq=Freq)
df.word19 <- filter(df.word, nchar(word)>=2)

q<-str_replace_all(q16,"\\W"," ")
noun<-extractNoun(q)
wordcount<-table(unlist(noun))
wordcount<-as.data.frame(wordcount)
wordcount[,1]<-gsub("\\d+",NA,wordcount[,1])
wordcount<-na.omit(wordcount)
df.word<-as.data.frame(wordcount,stringAsFactors=F)
df.word<-rename(df.word,word=Var1,freq=Freq)
df.word16 <- filter(df.word, nchar(word)>=2)

##전체에서 뽑은거;
dim(news)
t=news$text
t<-str_replace_all(t,"\\W"," ")
noun<-extractNoun(t)
wordcount<-table(unlist(noun))
wordcount<-as.data.frame(wordcount)
wordcount[,1]<-gsub("\\d+",NA,wordcount[,1])
wordcount<-na.omit(wordcount)
df.word<-as.data.frame(wordcount,stringAsFactors=F)
df.word<-rename(df.word,word=Var1,freq=Freq)
df.word <- filter(df.word, nchar(word)>=2)

##merge

k = rep(0,dim(df.word)[1])
df.word$q1 = k;df.word$q2 = k;df.word$q3 = k; df.word$q4 = k;
df.word$q5 = k;df.word$q6 = k;df.word$q7 = k; df.word$q8 = k;
df.word$q9 = k;df.word$q10 = k;df.word$q11 = k; df.word$q12 = k;
df.word$q13 = k;df.word$q14 = k;df.word$q15 = k; df.word$q16 = k;
df.word$q17 = k;df.word$q18 = k;df.word$q19 = k; 

df.word$total = df.word$freq
df.word = df.word[,-2]

for (i in 1:dim(df.word1)[1]){
  df.word1[i,1]==df.word[,1]
}

for (i in 1:dim(df.word13)[1]){ 
  df.word[grep(paste0('^',df.word13[i,1],'$'),df.word[,1]),14]=df.word13[i,2]
}

for (i in 1:dim(df.word14)[1]){ 
  df.word[grep(paste0('^',df.word14[i,1],'$'),df.word[,1]),15]=df.word14[i,2]
}
for (i in 1:dim(df.word15)[1]){ 
  df.word[grep(paste0('^',df.word15[i,1],'$'),df.word[,1]),16]=df.word15[i,2]
}
for (i in 1:dim(df.word16)[1]){ 
  df.word[grep(paste0('^',df.word16[i,1],'$'),df.word[,1]),17]=df.word16[i,2]
}
for (i in 1:dim(df.word17)[1]){ 
  df.word[grep(paste0('^',df.word17[i,1],'$'),df.word[,1]),18]=df.word17[i,2]
}
for (i in 1:dim(df.word18)[1]){ 
  df.word[grep(paste0('^',df.word18[i,1],'$'),df.word[,1]),19]=df.word18[i,2]
}
for (i in 1:dim(df.word19)[1]){ 
  df.word[grep(paste0('^',df.word19[i,1],'$'),df.word[,1]),20]=df.word19[i,2]
}

dim(df.word)
idx =which(df.word$total==1)
df.word2 = df.word[-idx,]
dim(df.word2)
View(df.word2)

write.csv(df.word2,'news_word.csv',row.names=FALSE)
