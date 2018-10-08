rm(list=ls())
getwd()
setwd("C:/Users/eunse/Desktop/medical cr/Dbpia/의료사고")

if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(rvest)) install.packages("rvest"); library(rvest)
if(!require(stringr)) install.packages("stringr"); library(stringr)

url="http://www.dbpia.co.kr/SearchResult/Search?q=%28%28%5B%EC%9D%98%EB%A3%8C%EC%82%AC%EA%B3%A0%C2%A7coldb%C2%A72%C2%A751%C2%A73%5D%29%29&searchWord=%EC%A0%84%EC%B2%B4%3D%5E%24%EC%9D%98%EB%A3%8C%EC%82%AC%EA%B3%A0%5E*&searchWordCondition=%EC%9E%90%EB%A3%8C%EC%9C%A0%ED%98%95%3D%5E%24%EC%A0%84%EC%B2%B4%5E*&Collection=0&nSort=1&nSorttype=desc&Page=1&nPagesize=200&Multimedia=0&isFullText=0&Collection=0&SearchKeyword=%EC%9D%98%EB%A3%8C%EC%82%AC%EA%B3%A0&SearchOper=1&SearchOption=0&SearchMethod=2&Category=002008&Language=154001&SrvYN=Y&PublishDate=4&PublishSttDate=2014&PublishEndDate=2018"
x=read_html(url)
##################
#index=c(3,4)
title=x%>% html_nodes('.titleWarp')%>%html_text
#title = title[-index]
l= c()
for (m in 1:length(title)){
  l[m] = strsplit(title[m],'\r\n')[[1]][1]
}
l=gsub('\u00a0','',l)
title = l

sub=x%>%html_nodes('.content01Warp')%>%html_text
#idx=c(index*3, index*3-1, index*3-2)
#sub=sub[-idx]

#################
id=seq(1,156,3)
author=sub[id]
author = str_extract_all(author, "[가-힣]+")

q=c()
for (i in 1:length(author)){
  q[i]=paste(author[[i]], collapse = ',')
}
author=q

source_date=sub[id+1]
source = str_extract_all(source_date, "[가-힣]+")
q=c()
for (i in 1:length(source)){
  q[i]=paste(source[[i]], collapse = ',')
}
source=q

date=c()
for (i in 1:length(source_date)){
  date[i] = strsplit(source_date[[i]],",")[[1]][3]
}
date

dtext=x%>%html_nodes('.btnText') %>%html_nodes('a')%>%html_attr('href')
dte=str_extract_all(dtext,'[0-9]')
subnum=c()
for(i in 1:length(dte)){
  subnum[i] = paste0(dte[[i]],collapse = '')
}

sublink=c()
suburl="http://www.dbpia.co.kr/Journal/TextViewNew?id=NODE"

suburl2="&prevPathCode="
for (i in 1:length(subnum)){
  sublink[i]=paste0(suburl,subnum[i],suburl2,collapse = '')
}
sublink

text = c()
for(i in 1:length(sublink)){
  f = read_html(sublink[i])
  text[i] = f%>%html_node('xmp')%>%html_text
}
te=c()
for (i in 1:length(text)){
  te[i] = paste(str_extract_all(text[i],"[가-힣]+")[[1]],collapse = ' ')
}
length(te)

df = data.frame(title,author,source,date,te)
View(df)
write.csv(df,'의료사고.csv',row.names=FALSE)
