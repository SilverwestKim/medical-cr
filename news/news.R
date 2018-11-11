rm(list=ls())
getwd()
setwd("C:/Users/eunse/Desktop/crawling/news")
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(rvest)) install.packages("rvest"); library(rvest)
if(!require(stringr)) install.packages("stringr"); library(stringr)

#<병원 + 환자안전>################

ur="https://search.naver.com/search.naver?&where=news&query=%2B%22%EB%B3%91%EC%9B%90%22%20%2B%22%ED%99%98%EC%9E%90%EC%95%88%EC%A0%84%22%20-%ED%8C%8C%EC%97%85%20-%EB%B3%B4%ED%97%99%20-%EC%B9%98%EB%A7%A4%20-%EC%BC%80%EC%9D%B4%ED%81%AC%20-%ED%8F%AD%ED%96%89%20-%EB%9D%BC%EC%9D%B4%ED%94%84%20-%EB%B0%98%EB%A0%A4%EA%B2%AC%20-%ED%97%8C%ED%98%88%20-%EB%AF%B8%EC%9A%A9%20-%EC%BB%A8%EC%84%A4%ED%8C%85%20-%EC%8B%A0%EC%A0%9C%ED%92%88%20-%EC%8B%A0%EA%B8%B0%EC%88%A0&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2014.01.01&de=2018.08.31&docid=&nso=so:r,p:from20140101to20180831,a:all&mynews=1&cluster_rank=10&refresh_start=0&start="

subi = seq(1,16990,10)
url = paste0(ur,subi)

rst = data.frame(NA,NA,NA)
colnames(rst)= c('title','com','sublink')

for (q in 1:455){
  x=read_html(url[q])
  sublink = x %>% html_nodes('.type01')%>% html_nodes('dl')%>%html_nodes('dt')%>%html_nodes('a')%>%html_attr('href')
  title = x %>% html_nodes('.type01')%>% html_nodes('dt')%>%html_nodes('a')%>%html_text
  com = x%>% html_nodes('._sp_each_source')%>%html_text
  result = data.frame(title,com,sublink)
  rst = rbind(rst,result)
}
for (q in 456:length(url)){
  x=read_html(url[q])
  sublink = x %>% html_nodes('.type01')%>% html_nodes('dl')%>%html_nodes('dt')%>%html_nodes('a')%>%html_attr('href')
  title = x %>% html_nodes('.type01')%>% html_nodes('dt')%>%html_nodes('a')%>%html_text
  com = x%>% html_nodes('._sp_each_source')%>%html_text
  result = data.frame(title,com,sublink)
  rst = rbind(rst,result)
}
i1 = which(rst$com=='경향신문'); i2=which(rst$com=='국민일보'); i3=which(rst$com=='동아일보');
i4=which(rst$com=='조선일보'); i5=which(rst$com=='한국일보');i6=which(rst$com=='중앙일보');
i7=which(rst$com=='매일일보'); i8=which(rst$com=='문화일보'); i9=which(rst$com=='아시아투데이');i10=which(rst$com =='한겨레')

dim(rst)
##경향
qw = rst[i1,]
text=c()
for (w in 1:length(qw[,3])){
  p = read_html(rst[i1,3][w],'euc-kr')
  t = p%>%html_nodes('.art_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
qw[,4] = text
qw = qw[,-3]
colnames(qw)[3] = 'text'
qwe = cbind(unique(qw[,1]),unique(qw[,2]),unique(qw[,3]))
colnames(qwe)= c('title','com','text')
##국민
we = rst[i2,]
text=c()
for (w in 1:length(we[,3])){
  p = read_html(rst[i2,3][w],'euc-kr')
  t = p%>%html_node('.tx')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
we[,4] = text
we = we[,-3]
colnames(we)[3] = 'text'
wer = cbind(unique(we[,1]),unique(we[,2]),unique(we[,3]))
colnames(wer)= c('title','com','text')

##동아
er = rst[i3,]
text=c()
for (w in 1:length(er[,3])){
  p = read_html(rst[i3,3][w])
  t = p%>%html_node('.article_txt')%>%html_text
  t = strsplit(t,'관련기사')[[1]][1]
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
er[,4] = text
er = er[,-3]
colnames(er)[3] = 'text'
ert = cbind(unique(er[,1]),unique(er[,2]),unique(er[,3]))
colnames(ert)= c('title','com','text')

##조선
rt=rst[i4,]
text=c()
for (w in 1:length(rt[,3])){
  p = read_html(rst[i4,3][w])
  t = p%>%html_node('.news_article')%>%html_node('.article')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
rt[,4] = text
rt = rt[,-3]
colnames(rt)[3] = 'text'
rt = na.omit(rt)
rty = cbind(unique(rt[,1]),unique(rt[,2]),unique(rt[,3]))
colnames(rty)= c('title','com','text')
##한국일보
ty=rst[i5,]
text=c()
for (w in 1:length(ty[,3])){
  p = read_html(rst[i5,3][w])
  t = p%>%html_node('.article-story')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ty[,4] = text
ty = ty[,-3]
colnames(ty)[3] = 'text'
ty = na.omit(ty)
tyu = cbind(unique(ty[,1]),unique(ty[,2]),unique(ty[,3]))
colnames(tyu)= c('title','com','text')

##중앙
yu=rst[i6,]
text=c()
for (w in 1:length(yu[,3])){
  p = read_html(rst[i6,3][w])
  t = p%>%html_node('.article_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
yu[,4] = text
yu = yu[,-3]
colnames(yu)[3] = 'text'
yu = na.omit(yu)
yui = cbind(unique(yu[,1]),unique(yu[,2]),unique(yu[,3]))
colnames(yui)= c('title','com','text')

##매일
ui=rst[i7,]
text=c()
for (w in 1:length(ui[,3])){
  p = read_html(rst[i7,3][w])
  t = p%>%html_node('.cont-body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ui[,4] = text
ui = ui[,-3]
colnames(ui)[3] = 'text'
ui = na.omit(ui)
uio = cbind(unique(ui[,1]),unique(ui[,2]),unique(ui[,3]))
colnames(uio)= c('title','com','text')

##문화
io=rst[i8,]
text=c()
for (w in 1:length(io[,3])){
  p = read_html(rst[i8,3][w])
  t = p%>%html_node('.body2')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
io[,4] = text
io = io[,-3]
colnames(io)[3] = 'text'
io = na.omit(io)
iop = cbind(unique(io[,1]),unique(io[,2]),unique(io[,3]))
colnames(iop)= c('title','com','text')

##아시아투데이
as=rst[i9,]
text=c()
for (w in 1:length(as[,3])){
  p = read_html(rst[i9,3][w])
  t = p%>%html_node('.news_bm')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
as[,4] = text
as = as[,-3]
colnames(as)[3] = 'text'
as = na.omit(as)
asd = cbind(unique(as[,1]),unique(as[,2]),unique(as[,3]))
colnames(asd)= c('title','com','text')

##한겨레
sd=rst[i10,]
text=c()
for (w in 1:length(sd[,3])){
  p = read_html(rst[i10,3][w])
  t = p%>%html_node('.article-text')%>%html_node('.text')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
sd[,4] = text
sd = sd[,-3]
colnames(sd)[3] = 'text'
sd = na.omit(sd)
sdf = cbind(unique(sd[,1]),unique(sd[,2]),unique(sd[,3]))
colnames(sdf)= c('title','com','text')

rs = rbind(qwe,wer,ert,rty,tyu,yui,uio,iop,asd,sdf)
setwd("C:/Users/eunse/Desktop/medical cr/news")
write.csv(rs,'병원_환자안전.csv')

#환자안전법###############################################
ur="https://search.naver.com/search.naver?&where=news&query=%2B%ED%99%98%EC%9E%90%EC%95%88%EC%A0%84%EB%B2%95%20-%ED%8C%8C%EC%97%85%20-%EB%B3%B4%ED%97%99%20-%EC%B9%98%EB%A7%A4%20-%EC%BC%80%EC%9D%B4%ED%81%AC%20-%ED%8F%AD%ED%96%89%20-%EB%9D%BC%EC%9D%B4%ED%94%84%20-%EB%B0%98%EB%A0%A4%EA%B2%AC%20-%ED%97%8C%ED%98%88%20-%EB%AF%B8%EC%9A%A9%20-%EC%BB%A8%EC%84%A4%ED%8C%85%20-%EC%8B%A0%EC%A0%9C%ED%92%88%20-%EC%8B%A0%EA%B8%B0%EC%88%A0&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2014.01.01&de=2018.08.31&docid=&nso=so:r,p:from20140101to20180831,a:all&mynews=0&cluster_rank=18&refresh_start=0&start="

subi = seq(1,1654,10)
url = paste0(ur,subi)

rst = data.frame(NA,NA,NA)
colnames(rst)= c('title','com','sublink')

for (q in 1:length(url)){
  x=read_html(url[q])
  sublink = x %>% html_nodes('.type01')%>% html_nodes('dl')%>%html_nodes('dt')%>%html_nodes('a')%>%html_attr('href')
  title = x %>% html_nodes('.type01')%>% html_nodes('dt')%>%html_nodes('a')%>%html_text
  com = x%>% html_nodes('._sp_each_source')%>%html_text
  result = data.frame(title,com,sublink)
  rst = rbind(rst,result)
}
dim(rst)
i1 = which(rst$com=='경향신문'); i2=which(rst$com=='국민일보'); i3=which(rst$com=='동아일보');
i4=which(rst$com=='조선일보'); i5=which(rst$com=='한국일보');i6=which(rst$com=='중앙일보');
i7=which(rst$com=='매일일보'); i8=which(rst$com=='문화일보'); i9=which(rst$com=='아시아투데이');i10=which(rst$com =='한겨레')
##경향
qw = rst[i1,]
text=c()
for (w in 1:length(qw[,3])){
  p = read_html(rst[i1,3][w],'euc-kr')
  t = p%>%html_nodes('.art_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
qw[,4] = text
qw = qw[,-3]
colnames(qw)[3] = 'text'
qwe = cbind(unique(qw[,1]),unique(qw[,2]),unique(qw[,3]))
colnames(qwe)= c('title','com','text')
##국민
we = rst[i2,]
we = we[1:24,]
text=c()
for (w in 1:length(we[,3])){
  p = read_html(rst[i2,3][w],'euc-kr')
  t = p%>%html_node('.tx')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
we[,4] = text
we = we[,-3]
colnames(we)[3] = 'text'
wer = cbind(unique(we[,1]),unique(we[,2]),unique(we[,3]))
colnames(wer)= c('title','com','text')

##동아
er = rst[i3,]
text=c()
for (w in 1:length(er[,3])){
  p = read_html(rst[i3,3][w])
  t = p%>%html_node('.article_txt')%>%html_text
  t = strsplit(t,'관련기사')[[1]][1]
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
er[,4] = text
er = er[,-3]
colnames(er)[3] = 'text'
ert = cbind(unique(er[,1]),unique(er[,2]),unique(er[,3]))
colnames(ert)= c('title','com','text')

##조선
rt=rst[i4,]
text=c()
for (w in 1:length(rt[,3])){
  p = read_html(rst[i4,3][w])
  t = p%>%html_node('.news_article')%>%html_node('.article')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
rt[,4] = text
rt = rt[,-3]
colnames(rt)[3] = 'text'
rt = na.omit(rt)
rty = cbind(unique(rt[,1]),unique(rt[,2]),unique(rt[,3]))
colnames(rty)= c('title','com','text')
##한국일보
ty=rst[i5,]
text=c()
for (w in 1:length(ty[,3])){
  p = read_html(rst[i5,3][w])
  t = p%>%html_node('.article-story')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ty[,4] = text
ty = ty[,-3]
colnames(ty)[3] = 'text'
ty = na.omit(ty)
tyu = cbind(unique(ty[,1]),unique(ty[,2]),unique(ty[,3]))
colnames(tyu)= c('title','com','text')

##중앙
yu=rst[i6,]
text=c()
for (w in 1:length(yu[,3])){
  p = read_html(rst[i6,3][w])
  t = p%>%html_node('.article_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
yu[,4] = text
yu = yu[,-3]
colnames(yu)[3] = 'text'
yu = na.omit(yu)
yui = cbind(unique(yu[,1]),unique(yu[,2]),unique(yu[,3]))
colnames(yui)= c('title','com','text')

##매일
ui=rst[i7,]
text=c()
for (w in 1:length(ui[,3])){
  p = read_html(rst[i7,3][w])
  t = p%>%html_node('.cont-body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ui[,4] = text
ui = ui[,-3]
colnames(ui)[3] = 'text'
ui = na.omit(ui)
uio = cbind(unique(ui[,1]),unique(ui[,2]),unique(ui[,3]))
colnames(uio)= c('title','com','text')

##문화
io=rst[i8,]
text=c()
for (w in 1:length(io[,3])){
  p = read_html(rst[i8,3][w])
  t = p%>%html_node('.body2')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
io[,4] = text
io = io[,-3]
colnames(io)[3] = 'text'
io = na.omit(io)
iop = cbind(unique(io[,1]),unique(io[,2]),unique(io[,3]))
colnames(iop)= c('title','com','text')

##아시아투데이
as=rst[i9,]
text=c()
for (w in 1:length(as[,3])){
  p = read_html(rst[i9,3][w])
  t = p%>%html_node('.news_bm')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
as[,4] = text
as = as[,-3]
colnames(as)[3] = 'text'
as = na.omit(as)
asd = cbind(unique(as[,1]),unique(as[,2]),unique(as[,3]))
colnames(asd)= c('title','com','text')

##한겨레
sd=rst[i10,]
text=c()
for (w in 1:length(sd[,3])){
  p = read_html(rst[i10,3][w])
  t = p%>%html_node('.article-text')%>%html_node('.text')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
sd[,4] = text
sd = sd[,-3]
colnames(sd)[3] = 'text'
sd = na.omit(sd)
sdf = cbind(unique(sd[,1]),unique(sd[,2]),unique(sd[,3]))
colnames(sdf)= c('title','com','text')

rs = rbind(qwe,wer,ert,tyu,yui,iop,asd,sdf)
setwd("C:/Users/eunse/Desktop/medical cr/news")
write.csv(rs,'환자안전법.csv')

##병원+투약오류/ 병원+투약사고######################################################
ur="https://search.naver.com/search.naver?&where=news&query=%2B%22%EB%B3%91%EC%9B%90%22%20%2B%22%ED%88%AC%EC%95%BD%EC%98%A4%EB%A5%98%22%20-%ED%8C%8C%EC%97%85%20-%EB%B3%B4%ED%97%99%20-%EC%B9%98%EB%A7%A4%20-%EC%BC%80%EC%9D%B4%ED%81%AC%20-%ED%8F%AD%ED%96%89%20-%EB%9D%BC%EC%9D%B4%ED%94%84%20-%EB%B0%98%EB%A0%A4%EA%B2%AC%20-%ED%97%8C%ED%98%88%20-%EB%AF%B8%EC%9A%A9%20-%EC%BB%A8%EC%84%A4%ED%8C%85%20-%EC%8B%A0%EC%A0%9C%ED%92%88%20-%EC%8B%A0%EA%B8%B0%EC%88%A0&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2014.01.01&de=2018.08.31&docid=&nso=so:r,p:from20140101to20180831,a:all&mynews=0&cluster_rank=14&refresh_start=0&start="

subi = seq(1,554,10)
url = paste0(ur,subi)

rst = data.frame(NA,NA,NA)
colnames(rst)= c('title','com','sublink')

for (q in 1:length(url)){
  x=read_html(url[q])
  sublink = x %>% html_nodes('.type01')%>% html_nodes('dl')%>%html_nodes('dt')%>%html_nodes('a')%>%html_attr('href')
  title = x %>% html_nodes('.type01')%>% html_nodes('dt')%>%html_nodes('a')%>%html_text
  com = x%>% html_nodes('._sp_each_source')%>%html_text
  result = data.frame(title,com,sublink)
  rst = rbind(rst,result)
}

i1 = which(rst$com=='경향신문'); i2=which(rst$com=='국민일보'); i3=which(rst$com=='동아일보');
i4=which(rst$com=='조선일보'); i5=which(rst$com=='한국일보');i6=which(rst$com=='중앙일보');
i7=which(rst$com=='매일일보'); i8=which(rst$com=='문화일보'); i9=which(rst$com=='아시아투데이');
i10=which(rst$com =='한겨레')

dim(rst)
##경향
qw = rst[i1,]
text=c()
for (w in 1:length(qw[,3])){
  p = read_html(rst[i1,3][w],'euc-kr')
  t = p%>%html_nodes('.art_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
qw[,4] = text
qw = qw[,-3]
colnames(qw)[3] = 'text'
qwe = cbind(unique(qw[,1]),unique(qw[,2]),unique(qw[,3]))
colnames(qwe)= c('title','com','text')
##국민
we = rst[i2,]
we = we[-14,]
text=c()
for (w in 1:length(we[,3])){
  p = read_html(rst[i2,3][w],'euc-kr')
  t = p%>%html_node('.tx')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
we[,4] = text
we = we[,-3]
colnames(we)[3] = 'text'
wer = cbind(unique(we[,1]),unique(we[,2]),unique(we[,3]))
colnames(wer)= c('title','com','text')

##동아
er = rst[i3,]
text=c()
for (w in 1:length(er[,3])){
  p = read_html(rst[i3,3][w])
  t = p%>%html_node('.article_txt')%>%html_text
  t = strsplit(t,'관련기사')[[1]][1]
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
er[,4] = text
er = er[,-3]
colnames(er)[3] = 'text'
ert = cbind(unique(er[,1]),unique(er[,2]),unique(er[,3]))
colnames(ert)= c('title','com','text')

##조선
rt=rst[i4,]
text=c()
for (w in 1:length(rt[,3])){
  p = read_html(rst[i4,3][w])
  t = p%>%html_node('.news_article')%>%html_node('.article')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
rt[,4] = text
rt = rt[,-3]
colnames(rt)[3] = 'text'
rt = na.omit(rt)
rty = cbind(unique(rt[,1]),unique(rt[,2]),unique(rt[,3]))
colnames(rty)= c('title','com','text')
##한국일보
ty=rst[i5,]
text=c()
for (w in 1:length(ty[,3])){
  p = read_html(rst[i5,3][w])
  t = p%>%html_node('.article-story')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ty[,4] = text
ty = ty[,-3]
colnames(ty)[3] = 'text'
ty = na.omit(ty)
tyu = cbind(unique(ty[,1]),unique(ty[,2]),unique(ty[,3]))
colnames(tyu)= c('title','com','text')

##중앙
yu=rst[i6,]
text=c()
for (w in 1:length(yu[,3])){
  p = read_html(rst[i6,3][w])
  t = p%>%html_node('.article_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
yu[,4] = text
yu = yu[,-3]
colnames(yu)[3] = 'text'
yu = na.omit(yu)
yui = cbind(unique(yu[,1]),unique(yu[,2]),unique(yu[,3]))
colnames(yui)= c('title','com','text')

##매일
ui=rst[i7,]
text=c()
for (w in 1:length(ui[,3])){
  p = read_html(rst[i7,3][w])
  t = p%>%html_node('.cont-body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ui[,4] = text
ui = ui[,-3]
colnames(ui)[3] = 'text'
ui = na.omit(ui)
uio = cbind(unique(ui[,1]),unique(ui[,2]),unique(ui[,3]))
colnames(uio)= c('title','com','text')

##문화
io=rst[i8,]
text=c()
for (w in 1:length(io[,3])){
  p = read_html(rst[i8,3][w])
  t = p%>%html_node('.body2')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
io[,4] = text
io = io[,-3]
colnames(io)[3] = 'text'
io = na.omit(io)
iop = cbind(unique(io[,1]),unique(io[,2]),unique(io[,3]))
colnames(iop)= c('title','com','text')

##아시아투데이
as=rst[i9,]
text=c()
for (w in 1:length(as[,3])){
  p = read_html(rst[i9,3][w])
  t = p%>%html_node('.news_bm')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
as[,4] = text
as = as[,-3]
colnames(as)[3] = 'text'
as = na.omit(as)
asd = cbind(unique(as[,1]),unique(as[,2]),unique(as[,3]))
colnames(asd)= c('title','com','text')

##한겨레
sd=rst[i10,]
text=c()
for (w in 1:length(sd[,3])){
  p = read_html(rst[i10,3][w])
  t = p%>%html_node('.article-text')%>%html_node('.text')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
sd[,4] = text
sd = sd[,-3]
colnames(sd)[3] = 'text'
sd = na.omit(sd)
sdf = cbind(unique(sd[,1]),unique(sd[,2]),unique(sd[,3]))
colnames(sdf)= c('title','com','text')

rs = rbind(qwe,wer,ert,tyu,yui,asd,sdf)

setwd("C:/Users/eunse/Desktop/medical cr/news")
write.csv(rst,'병원_투약오류_사고.csv')

#병원감염##########################################################
ur="https://search.naver.com/search.naver?&where=news&query=%22%EB%B3%91%EC%9B%90%EA%B0%90%EC%97%BC%22%20-%ED%8C%8C%EC%97%85%20-%EB%B3%B4%ED%97%99%20-%EC%B9%98%EB%A7%A4%20-%EC%BC%80%EC%9D%B4%ED%81%AC%20-%ED%8F%AD%ED%96%89%20-%EB%9D%BC%EC%9D%B4%ED%94%84%20-%EB%B0%98%EB%A0%A4%EA%B2%AC%20-%ED%97%8C%ED%98%88%20-%EB%AF%B8%EC%9A%A9%20-%EC%BB%A8%EC%84%A4%ED%8C%85%20-%EC%8B%A0%EC%A0%9C%ED%92%88%20-%EC%8B%A0%EA%B8%B0%EC%88%A0&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2014.01.01&de=2018.08.31&docid=&nso=so:r,p:from20140101to20180831,a:all&mynews=0&cluster_rank=16&refresh_start=0&start="

subi = seq(1,12754,10)
url = paste0(ur,subi)

rst = data.frame(NA,NA,NA)
colnames(rst)= c('title','com','sublink')

for (q in 1:length(url)){
  x=read_html(url[q])
  sublink = x %>% html_nodes('.type01')%>% html_nodes('dl')%>%html_nodes('dt')%>%html_nodes('a')%>%html_attr('href')
  title = x %>% html_nodes('.type01')%>% html_nodes('dt')%>%html_nodes('a')%>%html_text
  com = x%>% html_nodes('._sp_each_source')%>%html_text
  result = data.frame(title,com,sublink)
  rst = rbind(rst,result)
}

i1 = which(rst$com=='경향신문'); i2=which(rst$com=='국민일보'); i3=which(rst$com=='동아일보');
i4=which(rst$com=='조선일보'); i5=which(rst$com=='한국일보');i6=which(rst$com=='중앙일보');
i7=which(rst$com=='매일일보'); i8=which(rst$com=='문화일보'); i9=which(rst$com=='아시아투데이');
i10=which(rst$com =='한겨레')

dim(rst)
##경향
qw = rst[i1,]
text=c()
for (w in 1:length(qw[,3])){
  p = read_html(rst[i1,3][w],'euc-kr')
  t = p%>%html_nodes('.art_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
qw[,4] = text
qw = qw[,-3]
colnames(qw)[3] = 'text'
qwe = cbind(unique(qw[,1]),unique(qw[,2]),unique(qw[,3]))
colnames(qwe)= c('title','com','text')
##국민
we = rst[i2,]
we = we[1:59,]
text=c()
for (w in 1:length(we[,3])){
  p = read_html(rst[i2,3][w],'euc-kr')
  t = p%>%html_node('.tx')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
we[,4] = text
we = we[,-3]
colnames(we)[3] = 'text'
wer = cbind(unique(we[,1]),unique(we[,2]),unique(we[,3]))
colnames(wer)= c('title','com','text')

##동아
er = rst[i3,]
text=c()
for (w in 1:length(er[,3])){
  p = read_html(rst[i3,3][w])
  t = p%>%html_node('.article_txt')%>%html_text
  t = strsplit(t,'관련기사')[[1]][1]
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
er[,4] = text
er = er[,-3]
colnames(er)[3] = 'text'
ert = cbind(unique(er[,1]),unique(er[,2]),unique(er[,3]))
colnames(ert)= c('title','com','text')

##조선
rt=rst[i4,]
text=c()
for (w in 1:length(rt[,3])){
  p = read_html(rst[i4,3][w])
  t = p%>%html_node('.news_article')%>%html_node('.article')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
rt[,4] = text
rt = rt[,-3]
colnames(rt)[3] = 'text'
rt = na.omit(rt)
rty = cbind(unique(rt[,1]),unique(rt[,2]),unique(rt[,3]))
colnames(rty)= c('title','com','text')
##한국일보
ty=rst[i5,]
text=c()
for (w in 1:length(ty[,3])){
  p = read_html(rst[i5,3][w])
  t = p%>%html_node('.article-story')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ty[,4] = text
ty = ty[,-3]
colnames(ty)[3] = 'text'
ty = na.omit(ty)
tyu = cbind(unique(ty[,1]),unique(ty[,2]),unique(ty[,3]))
colnames(tyu)= c('title','com','text')

##중앙
yu=rst[i6,]
text=c()
for (w in 1:length(yu[,3])){
  p = read_html(rst[i6,3][w])
  t = p%>%html_node('.article_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
yu[,4] = text
yu = yu[,-3]
colnames(yu)[3] = 'text'
yu = na.omit(yu)
yui = cbind(unique(yu[,1]),unique(yu[,2]),unique(yu[,3]))
colnames(yui)= c('title','com','text')

##매일
ui=rst[i7,]
text=c()
for (w in 1:length(ui[,3])){
  p = read_html(rst[i7,3][w])
  t = p%>%html_node('.cont-body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ui[,4] = text
ui = ui[,-3]
colnames(ui)[3] = 'text'
ui = na.omit(ui)
uio = cbind(unique(ui[,1]),unique(ui[,2]),unique(ui[,3]))
colnames(uio)= c('title','com','text')

##문화
io=rst[i8,]
text=c()
for (w in 1:length(io[,3])){
  p = read_html(rst[i8,3][w])
  t = p%>%html_node('.body2')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
io[,4] = text
io = io[,-3]
colnames(io)[3] = 'text'
io = na.omit(io)
iop = cbind(unique(io[,1]),unique(io[,2]),unique(io[,3]))
colnames(iop)= c('title','com','text')

##아시아투데이
as=rst[i9,]
text=c()
for (w in 1:length(as[,3])){
  p = read_html(rst[i9,3][w])
  t = p%>%html_node('.news_bm')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
as[,4] = text
as = as[,-3]
colnames(as)[3] = 'text'
as = na.omit(as)
asd = cbind(unique(as[,1]),unique(as[,2]),unique(as[,3]))
colnames(asd)= c('title','com','text')

##한겨레
sd=rst[i10,]
text=c()
for (w in 1:length(sd[,3])){
  p = read_html(rst[i10,3][w])
  t = p%>%html_node('.article-text')%>%html_node('.text')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
sd[,4] = text
sd = sd[,-3]
colnames(sd)[3] = 'text'
sd = na.omit(sd)
sdf = cbind(unique(sd[,1]),unique(sd[,2]),unique(sd[,3]))
colnames(sdf)= c('title','com','text')

rs = rbind(qwe,wer,ert,rty,tyu,yui,uio,iop,asd,sdf)

setwd("C:/Users/eunse/Desktop/medical cr/news")
write.csv(rst,'병원감염.csv')

#수술 의료사고############
ur="https://search.naver.com/search.naver?&where=news&query=%2B%22%EC%88%98%EC%88%A0%20%EC%9D%98%EB%A3%8C%EC%82%AC%EA%B3%A0%22%20-%ED%8C%8C%EC%97%85%20-%EB%B3%B4%ED%97%99%20-%EC%B9%98%EB%A7%A4%20-%EC%BC%80%EC%9D%B4%ED%81%AC%20-%ED%8F%AD%ED%96%89%20-%EB%9D%BC%EC%9D%B4%ED%94%84%20-%EB%B0%98%EB%A0%A4%EA%B2%AC%20-%ED%97%8C%ED%98%88%20-%EB%AF%B8%EC%9A%A9%20-%EC%BB%A8%EC%84%A4%ED%8C%85%20-%EC%8B%A0%EC%A0%9C%ED%92%88%20-%EC%8B%A0%EA%B8%B0%EC%88%A0&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2014.01.01&de=2018.08.31&docid=&nso=so:r,p:from20140101to20180831,a:all&mynews=0&cluster_rank=19&refresh_start=0&start="

subi = seq(1,136,10)
url = paste0(ur,subi)

rst = data.frame(NA,NA,NA)
colnames(rst)= c('title','com','sublink')

for (q in 1:length(url)){
  x=read_html(url[q])
  sublink = x %>% html_nodes('.type01')%>% html_nodes('dl')%>%html_nodes('dt')%>%html_nodes('a')%>%html_attr('href')
  title = x %>% html_nodes('.type01')%>% html_nodes('dt')%>%html_nodes('a')%>%html_text
  com = x%>% html_nodes('._sp_each_source')%>%html_text
  result = data.frame(title,com,sublink)
  rst = rbind(rst,result)
}

i1 = which(rst$com=='경향신문'); i2=which(rst$com=='국민일보'); i3=which(rst$com=='동아일보');
i4=which(rst$com=='조선일보'); i5=which(rst$com=='한국일보');i6=which(rst$com=='중앙일보');
i7=which(rst$com=='매일일보'); i8=which(rst$com=='문화일보'); i9=which(rst$com=='아시아투데이');
i10=which(rst$com =='한겨레')

dim(rst)


##경향
qw = rst[i1,]
text=c()
for (w in 1:length(qw[,3])){
  p = read_html(rst[i1,3][w],'euc-kr')
  t = p%>%html_nodes('.art_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
qw[,4] = text
qw = qw[,-3]
colnames(qw)[3] = 'text'
qwe = cbind(unique(qw[,1]),unique(qw[,2]),unique(qw[,3]))
colnames(qwe)= c('title','com','text')
##국민
we = rst[i2,]
text=c()
for (w in 1:length(we[,3])){
  p = read_html(rst[i2,3][w],'utf-8')
  t = p%>%html_node('.tx')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
we[,4] = text
we = we[,-3]
colnames(we)[3] = 'text'
wer = cbind(unique(we[,1]),unique(we[,2]),unique(we[,3]))
colnames(wer)= c('title','com','text')

##동아
er = rst[i3,]
text=c()
for (w in 1:length(er[,3])){
  p = read_html(rst[i3,3][w])
  t = p%>%html_node('.article_txt')%>%html_text
  t = strsplit(t,'관련기사')[[1]][1]
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
er[,4] = text
er = er[,-3]
colnames(er)[3] = 'text'
ert = cbind(unique(er[,1]),unique(er[,2]),unique(er[,3]))
colnames(ert)= c('title','com','text')

##조선
rt=rst[i4,]
text=c()
for (w in 1:length(rt[,3])){
  p = read_html(rst[i4,3][w])
  t = p%>%html_node('.news_article')%>%html_node('.article')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
rt[,4] = text
rt = rt[,-3]
colnames(rt)[3] = 'text'
rt = na.omit(rt)
rty = cbind(unique(rt[,1]),unique(rt[,2]),unique(rt[,3]))
colnames(rty)= c('title','com','text')
##한국일보
ty=rst[i5,]
text=c()
for (w in 1:length(ty[,3])){
  p = read_html(rst[i5,3][w])
  t = p%>%html_node('.article-story')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ty[,4] = text
ty = ty[,-3]
colnames(ty)[3] = 'text'
ty = na.omit(ty)
tyu = cbind(unique(ty[,1]),unique(ty[,2]),unique(ty[,3]))
colnames(tyu)= c('title','com','text')

##중앙
yu=rst[i6,]
text=c()
for (w in 1:length(yu[,3])){
  p = read_html(rst[i6,3][w])
  t = p%>%html_node('.article_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
yu[,4] = text
yu = yu[,-3]
colnames(yu)[3] = 'text'
yu = na.omit(yu)
yui = cbind(unique(yu[,1]),unique(yu[,2]),unique(yu[,3]))
colnames(yui)= c('title','com','text')

##매일
ui=rst[i7,]
text=c()
for (w in 1:length(ui[,3])){
  p = read_html(rst[i7,3][w])
  t = p%>%html_node('.cont-body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ui[,4] = text
ui = ui[,-3]
colnames(ui)[3] = 'text'
ui = na.omit(ui)
uio = cbind(unique(ui[,1]),unique(ui[,2]),unique(ui[,3]))
colnames(uio)= c('title','com','text')

##문화
io=rst[i8,]
text=c()
for (w in 1:length(io[,3])){
  p = read_html(rst[i8,3][w])
  t = p%>%html_node('.body2')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
io[,4] = text
io = io[,-3]
colnames(io)[3] = 'text'
io = na.omit(io)
iop = cbind(unique(io[,1]),unique(io[,2]),unique(io[,3]))
colnames(iop)= c('title','com','text')

##아시아투데이
as=rst[i9,]
text=c()
for (w in 1:length(as[,3])){
  p = read_html(rst[i9,3][w])
  t = p%>%html_node('.news_bm')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
as[,4] = text
as = as[,-3]
colnames(as)[3] = 'text'
as = na.omit(as)
asd = cbind(unique(as[,1]),unique(as[,2]),unique(as[,3]))
colnames(asd)= c('title','com','text')

##한겨레
sd=rst[i10,]
text=c()
for (w in 1:length(sd[,3])){
  p = read_html(rst[i10,3][w])
  t = p%>%html_node('.article-text')%>%html_node('.text')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
sd[,4] = text
sd = sd[,-3]
colnames(sd)[3] = 'text'
sd = na.omit(sd)
sdf = cbind(unique(sd[,1]),unique(sd[,2]),unique(sd[,3]))
colnames(sdf)= c('title','com','text')

rs = rbind(qwe,wer)
setwd("C:/Users/eunse/Desktop/medical cr/news")
write.csv(rs,'수술_의료사고.csv')
#인공호흡기 감염###############

ur="https://search.naver.com/search.naver?&where=news&query=%2B%EC%9D%B8%EA%B3%B5%ED%98%B8%ED%9D%A1%EA%B8%B0%20%2B%EA%B0%90%EC%97%BC%20%20-%ED%8C%8C%EC%97%85%20-%EB%B3%B4%ED%97%99%20-%EC%B9%98%EB%A7%A4%20-%EC%BC%80%EC%9D%B4%ED%81%AC%20-%ED%8F%AD%ED%96%89%20-%EB%9D%BC%EC%9D%B4%ED%94%84%20-%EB%B0%98%EB%A0%A4%EA%B2%AC%20-%ED%97%8C%ED%98%88%20-%EB%AF%B8%EC%9A%A9%20-%EC%BB%A8%EC%84%A4%ED%8C%85%20-%EC%8B%A0%EC%A0%9C%ED%92%88%20-%EC%8B%A0%EA%B8%B0%EC%88%A0&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2014.01.01&de=2018.08.31&docid=&nso=so:r,p:from20140101to20180831,a:all&mynews=0&cluster_rank=16&refresh_start=0&start="

subi = seq(1,3116,10)
url = paste0(ur,subi)

rst = data.frame(NA,NA,NA)
colnames(rst)= c('title','com','sublink')

for (q in 1:length(url)){
  x=read_html(url[q])
  sublink = x %>% html_nodes('.type01')%>% html_nodes('dl')%>%html_nodes('dt')%>%html_nodes('a')%>%html_attr('href')
  title = x %>% html_nodes('.type01')%>% html_nodes('dt')%>%html_nodes('a')%>%html_text
  com = x%>% html_nodes('._sp_each_source')%>%html_text
  result = data.frame(title,com,sublink)
  rst = rbind(rst,result)
}

i1 = which(rst$com=='경향신문'); i2=which(rst$com=='국민일보'); i3=which(rst$com=='동아일보');
i4=which(rst$com=='조선일보'); i5=which(rst$com=='한국일보');i6=which(rst$com=='중앙일보');
i7=which(rst$com=='매일일보'); i8=which(rst$com=='문화일보'); i9=which(rst$com=='아시아투데이');i10=which(rst$com =='한겨레')

dim(rst)
##경향
qw = rst[i1,]
qw = cbind(unique(qw[,1]),unique(qw[,2]),unique(qw[,3]))
text=c()
for (w in 1:length(qw[,3])){
  p = read_html(rst[i1,3][w],'euc-kr')
  t = p%>%html_nodes('.art_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
qw =  cbind(qw,text)
qw = qw[,-3]
colnames(qw)[3] = 'text'
qwe = cbind(unique(qw[,1]),unique(qw[,2]),unique(qw[,3]))
colnames(qwe)= c('title','com','text')
##국민
we = rst[i2,]
we = cbind(unique(we[,1]),unique(we[,2]),unique(we[,3]))
text=c()
for (w in 1:length(we[,3])){
  p = read_html(rst[i2,3][w],'euc-kr')
  t = p%>%html_node('.tx')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
we = cbind(we, text)
we = we[,-3]
colnames(we)[3] = 'text'
wer = cbind(unique(we[,1]),unique(we[,2]),unique(we[,3]))
colnames(wer)= c('title','com','text')

##동아
er = rst[i3,]
text=c()
for (w in 1:length(er[,3])){
  p = read_html(rst[i3,3][w])
  t = p%>%html_node('.article_txt')%>%html_text
  t = strsplit(t,'관련기사')[[1]][1]
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
er[,4] = text
er = er[,-3]
colnames(er)[3] = 'text'
ert = cbind(unique(er[,1]),unique(er[,2]),unique(er[,3]))
colnames(ert)= c('title','com','text')

##조선
rt=rst[i4,]
text=c()
for (w in 1:length(rt[,3])){
  p = read_html(rst[i4,3][w])
  t = p%>%html_node('.news_article')%>%html_node('.article')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
rt[,4] = text
rt = rt[,-3]
colnames(rt)[3] = 'text'
rt = na.omit(rt)
rty = cbind(unique(rt[,1]),unique(rt[,2]),unique(rt[,3]))
colnames(rty)= c('title','com','text')
##한국일보
ty=rst[i5,]
text=c()
for (w in 1:length(ty[,3])){
  p = read_html(rst[i5,3][w])
  t = p%>%html_node('.article-story')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ty[,4] = text
ty = ty[,-3]
colnames(ty)[3] = 'text'
ty = na.omit(ty)
tyu = cbind(unique(ty[,1]),unique(ty[,2]),unique(ty[,3]))
colnames(tyu)= c('title','com','text')

##중앙
yu=rst[i6,]
text=c()
for (w in 1:length(yu[,3])){
  p = read_html(rst[i6,3][w])
  t = p%>%html_node('.article_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
yu = cbind(yu,text)
yu = yu[,-3]
colnames(yu)[3] = 'text'
yu = na.omit(yu)
yui = cbind(unique(yu[,1]),unique(yu[,2]),unique(yu[,3]))
colnames(yui)= c('title','com','text')

##매일
ui=rst[i7,]
text=c()
for (w in 1:length(ui[,3])){
  p = read_html(rst[i7,3][w])
  t = p%>%html_node('.cont-body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ui[,4] = text
ui = ui[,-3]
colnames(ui)[3] = 'text'
ui = na.omit(ui)
uio = cbind(unique(ui[,1]),unique(ui[,2]),unique(ui[,3]))
colnames(uio)= c('title','com','text')

##문화
io=rst[i8,]
text=c()
for (w in 1:length(io[,3])){
  p = read_html(rst[i8,3][w])
  t = p%>%html_node('.body2')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
io[,4] = text
io = io[,-3]
colnames(io)[3] = 'text'
io = na.omit(io)
iop = cbind(unique(io[,1]),unique(io[,2]),unique(io[,3]))
colnames(iop)= c('title','com','text')

##아시아투데이
as=rst[i9,]
text=c()
for (w in 1:length(as[,3])){
  p = read_html(rst[i9,3][w])
  t = p%>%html_node('.news_bm')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
as[,4] = text
as = as[,-3]
colnames(as)[3] = 'text'
as = na.omit(as)
asd = cbind(unique(as[,1]),unique(as[,2]),unique(as[,3]))
colnames(asd)= c('title','com','text')

##한겨레
sd=rst[i10,]
text=c()
for (w in 1:length(sd[,3])){
  p = read_html(rst[i10,3][w])
  t = p%>%html_node('.article-text')%>%html_node('.text')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
sd[,4] = text
sd = sd[,-3]
colnames(sd)[3] = 'text'
sd = na.omit(sd)
sdf = cbind(unique(sd[,1]),unique(sd[,2]),unique(sd[,3]))
colnames(sdf)= c('title','com','text')

rs = rbind(qwe,wer,ert,tyu,iop,asd,sdf)
setwd("C:/Users/eunse/Desktop/medical cr/news")
write.csv(rs,'인공호흡기감염.csv')


#병원_의료기기_의료장비_결함_오류################

ur="https://search.naver.com/search.naver?sm=tab_dts&where=news&query=+%2B%EB%B3%91%EC%9B%90+%2B%EC%9D%98%EB%A3%8C%EA%B8%B0%EA%B8%B0+%2B%EA%B2%B0%ED%95%A8+-%ED%8C%8C%EC%97%85+-%EB%B3%B4%ED%97%99+-%EC%B9%98%EB%A7%A4+-%EC%BC%80%EC%9D%B4%ED%81%AC+-%ED%8F%AD%ED%96%89+-%EB%9D%BC%EC%9D%B4%ED%94%84+-%EB%B0%98%EB%A0%A4%EA%B2%AC+-%ED%97%8C%ED%98%88+-%EB%AF%B8%EC%9A%A9+-%EC%BB%A8%EC%84%A4%ED%8C%85+-%EC%8B%A0%EC%A0%9C%ED%92%88+-%EC%8B%A0%EA%B8%B0%EC%88%A0+&oquery=%2B%22%EB%B3%91%EC%9B%90%22+%2B%22%EC%88%98%ED%98%88%EC%82%AC%EA%B3%A0%22+-%ED%8C%8C%EC%97%85+-%EB%B3%B4%ED%97%99+-%EC%B9%98%EB%A7%A4+-%EC%BC%80%EC%9D%B4%ED%81%AC+-%ED%8F%AD%ED%96%89+-%EB%9D%BC%EC%9D%B4%ED%94%84+-%EB%B0%98%EB%A0%A4%EA%B2%AC+-%ED%97%8C%ED%98%88+-%EB%AF%B8%EC%9A%A9+-%EC%BB%A8%EC%84%A4%ED%8C%85+-%EC%8B%A0%EC%A0%9C%ED%92%88+-%EC%8B%A0%EA%B8%B0%EC%88%A0&tqi=T%2BQR1lpySoossbaxiyGssssstRN-384709&qdt=1&nso=so%3Ar%2Cp%3Afrom20140101to20180831%2Ca%3Aall&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2014.01.01&de=2018.08.31&mynews=0&refresh_start=0&related=0&mson=1"

subi = seq(1,271,10)
url = paste0(ur,subi)

rst = data.frame(NA,NA,NA)
colnames(rst)= c('title','com','sublink')

for (q in 1:length(url)){
  x=read_html(url[q])
  sublink = x %>% html_nodes('.type01')%>% html_nodes('dl')%>%html_nodes('dt')%>%html_nodes('a')%>%html_attr('href')
  title = x %>% html_nodes('.type01')%>% html_nodes('dt')%>%html_nodes('a')%>%html_text
  com = x%>% html_nodes('._sp_each_source')%>%html_text
  result = data.frame(title,com,sublink)
  rst = rbind(rst,result)
}

i1 = which(rst$com=='경향신문'); i2=which(rst$com=='국민일보'); i3=which(rst$com=='동아일보');
i4=which(rst$com=='조선일보'); i5=which(rst$com=='한국일보');i6=which(rst$com=='중앙일보');
i7=which(rst$com=='매일일보'); i8=which(rst$com=='문화일보'); i9=which(rst$com=='아시아투데이');i10=which(rst$com =='한겨레')

dim(rst)
##경향
qw = rst[i1,]
text=c()
for (w in 1:length(qw[,3])){
  p = read_html(rst[i1,3][w],'euc-kr')
  t = p%>%html_nodes('.art_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
qw[,4] = text
qw = qw[,-3]
colnames(qw)[3] = 'text'
qwe = cbind(unique(qw[,1]),unique(qw[,2]),unique(qw[,3]))
colnames(qwe)= c('title','com','text')
##국민
we = rst[i2,]
text=c()
for (w in 1:length(we[,3])){
  p = read_html(rst[i2,3][w],'euc-kr')
  t = p%>%html_node('.tx')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
we[,4] = text
we = we[,-3]
colnames(we)[3] = 'text'
wer = cbind(unique(we[,1]),unique(we[,2]),unique(we[,3]))
colnames(wer)= c('title','com','text')

##동아
er = rst[i3,]
text=c()
for (w in 1:length(er[,3])){
  p = read_html(rst[i3,3][w])
  t = p%>%html_node('.article_txt')%>%html_text
  t = strsplit(t,'관련기사')[[1]][1]
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
er[,4] = text
er = er[,-3]
colnames(er)[3] = 'text'
ert = cbind(unique(er[,1]),unique(er[,2]),unique(er[,3]))
colnames(ert)= c('title','com','text')

##조선
rt=rst[i4,]
text=c()
for (w in 1:length(rt[,3])){
  p = read_html(rst[i4,3][w])
  t = p%>%html_node('.news_article')%>%html_node('.article')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
rt[,4] = text
rt = rt[,-3]
colnames(rt)[3] = 'text'
rt = na.omit(rt)
rty = cbind(unique(rt[,1]),unique(rt[,2]),unique(rt[,3]))
colnames(rty)= c('title','com','text')
##한국일보
ty=rst[i5,]
text=c()
for (w in 1:length(ty[,3])){
  p = read_html(rst[i5,3][w])
  t = p%>%html_node('.article-story')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ty[,4] = text
ty = ty[,-3]
colnames(ty)[3] = 'text'
ty = na.omit(ty)
tyu = cbind(unique(ty[,1]),unique(ty[,2]),unique(ty[,3]))
colnames(tyu)= c('title','com','text')

##중앙
yu=rst[i6,]
text=c()
for (w in 1:length(yu[,3])){
  p = read_html(rst[i6,3][w])
  t = p%>%html_node('.article_body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('  ','',t)
  text[w]=t
}
yu[,4] = text
yu = yu[,-3]
colnames(yu)[3] = 'text'
yu = na.omit(yu)
yui = cbind(unique(yu[,1]),unique(yu[,2]),unique(yu[,3]))
colnames(yui)= c('title','com','text')

##매일
ui=rst[i7,]
text=c()
for (w in 1:length(ui[,3])){
  p = read_html(rst[i7,3][w])
  t = p%>%html_node('.cont-body')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
ui[,4] = text
ui = ui[,-3]
colnames(ui)[3] = 'text'
ui = na.omit(ui)
uio = cbind(unique(ui[,1]),unique(ui[,2]),unique(ui[,3]))
colnames(uio)= c('title','com','text')

##문화
io=rst[i8,]
text=c()
for (w in 1:length(io[,3])){
  p = read_html(rst[i8,3][w])
  t = p%>%html_node('.body2')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
io[,4] = text
io = io[,-3]
colnames(io)[3] = 'text'
io = na.omit(io)
iop = cbind(unique(io[,1]),unique(io[,2]),unique(io[,3]))
colnames(iop)= c('title','com','text')

##아시아투데이
as=rst[i9,]
text=c()
for (w in 1:length(as[,3])){
  p = read_html(rst[i9,3][w])
  t = p%>%html_node('.news_bm')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
as[,4] = text
as = as[,-3]
colnames(as)[3] = 'text'
as = na.omit(as)
asd = cbind(unique(as[,1]),unique(as[,2]),unique(as[,3]))
colnames(asd)= c('title','com','text')

##한겨레
sd=rst[i10,]
text=c()
for (w in 1:length(sd[,3])){
  p = read_html(rst[i10,3][w])
  t = p%>%html_node('.article-text')%>%html_node('.text')%>%html_text
  t = gsub('\r','',t)
  t = gsub('\n','',t)
  t = gsub('\t','',t)
  t = gsub('\u00a0','',t)
  t = gsub('  ','',t)
  text[w]=t
}
sd[,4] = text
sd = sd[,-3]
colnames(sd)[3] = 'text'
sd = na.omit(sd)
sdf = cbind(unique(sd[,1]),unique(sd[,2]),unique(sd[,3]))
colnames(sdf)= c('title','com','text')

rs = rbind(qwe)
setwd("C:/Users/eunse/Desktop/medical cr/news")
write.csv(rs,'병원_의료장비_의료기기.csv')
