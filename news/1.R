rm(list=ls())

#<병원 + 환자안전>
#"병원" "환자안전"이 정확하게 일치하고 '파업' '보험' '치매' '케이크' '폭행' '라이프' '반려견' '헌혈' '미용' '컨설팅'을 제외한 상세검색 결과

if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(rvest)) install.packages("rvest"); library(rvest)
if(!require(stringr)) install.packages("stringr"); library(stringr)
if(!require(httr)) install.packages("httr"); library(httr)

url="https://search.naver.com/search.naver?sm=tab_dts&where=news&query=+%22%EB%B3%91%EC%9B%90%22+%22%ED%99%98%EC%9E%90%EC%95%88%EC%A0%84%22+-%ED%8C%8C%EC%97%85+-%EB%B3%B4%ED%97%98+-%EC%B9%98%EB%A7%A4+-%EC%BC%80%EC%9D%B4%ED%81%AC+-%ED%8F%AD%ED%96%89+-%EB%9D%BC%EC%9D%B4%ED%94%84+-%EB%B0%98%EB%A0%A4%EA%B2%AC+-%ED%97%8C%ED%98%88+-%EB%AF%B8%EC%9A%A9+-%EC%BB%A8%EC%84%A4%ED%8C%85+&oquery=%22%EB%B3%91%EC%9B%90%22+%22%ED%99%98%EC%9E%90%EC%95%88%EC%A0%84%22+-%22%EC%8B%A0%EA%B8%B0%EC%88%A0+%EA%B0%9C%EB%B0%9C%22+-%22%EC%8B%A0%EC%A0%9C%ED%92%88+%EC%B6%9C%EC%8B%9C%22+-%ED%8C%8C%EC%97%85+-%EB%B3%B4%ED%97%98+-%EC%B9%98%EB%A7%A4+-%EC%BC%80%EC%9D%B4%ED%81%AC+-%ED%8F%AD%ED%96%89+-%EB%9D%BC%EC%9D%B4%ED%94%84+-%EB%B0%98%EB%A0%A4%EA%B2%AC+-%ED%97%8C%ED%98%88+-%EB%AF%B8%EC%9A%A9+-%EC%BB%A8%EC%84%A4%ED%8C%85&tqi=TIWrzdpySDlssbyFkqwssssstVs-096592&qdt=1&qdt=1"
x=read_html(url)
sublink = x %>% html_nodes('.type01')%>% html_nodes('dt')%>%html_nodes('a')%>%html_attr('href')

title = x %>% html_nodes('.type01')%>% html_nodes('dt')%>%html_nodes('a')%>%html_text

com = x%>% html_nodes('._sp_each_source')%>%html_text

r=data.frame(title, com)
View(r)
setwd("C:/Users/eunse/Desktop/medical cr/news")
write.csv(r,'x.csv')









x=list()
for (i in length(sublink)){
  x[i]=readLines(sublink[i])
  
}

k=c()

x=GET(sublink[1])
x=html(x)
y=html_nodes(x,'.read_body')
te=html_text(y)
k[1]=te


















x=readLines(sublink[2],encoding = 'utf-8')
x=html(x)
index = which(str_detect(x, "<tbody")==TRUE)

a=html_nodes(x,'.contents')
y=content(x)

html_nodes(x,'.read_body')

a=html_nodes(y,'.read_body')

for (i in 1:5){
  suburl[i]= read_html(link[i],encoding = 'EUC-KR')
}

h=c()
x=1:12
for (i in x){
  h[i]=read_html(link[i])
  
}
