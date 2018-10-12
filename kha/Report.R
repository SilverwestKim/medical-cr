rm(list=ls())
getwd()
setwd("C:/Users/eunse/Desktop/medical cr/kha")

if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(rvest)) install.packages("rvest"); library(rvest)
if(!require(stringr)) install.packages("stringr"); library(stringr)

sub = "https://www.kha.or.kr/board/dept/list?&brdMstIdx=27&keyWord=환자안전&menuIdx=446&rMnuGb=IMP&siteCd=HOME&siteGb=HOME&page="
url = c()
for (k in 1:6){
  url[k] = paste0(sub,k)
}

final = NULL

for (p in 1:length(url)){
  x = read_html(url[p])
  s = x%>%html_nodes('.con_wrap')%>%html_nodes('tr')
  s = s[-c(1,2)]
  s = html_nodes(s,'td')%>%html_text
  i = seq(1,130,13)
  num = s[i]
  source = s[i+2]
  title = x%>%html_nodes('.subject.ellipsis')%>%html_nodes('a')%>%html_text
  date = s[i+8]
  text = c()
  for (g in 1:10){
    subly = "https://www.kha.or.kr/board/dept/view?&brdMstIdx=27&keyWord=환자안전&menuIdx=446&rMnuGb=IMP&siteCd=HOME&siteGb=HOME&page="
    hp = "&brdIdx="
    texturl = c()
    texturl= paste0(subly, p, hp,num[g])
    test = read_html(texturl)%>%html_nodes('td')%>%html_text
    text[g] = test[9]
  }
  f = data.frame(title, source, date, text)
  final = rbind(final,f)
}

final$text=gsub('\u00a0','',final$text)
final = final[1:53,]
write.csv(final, "Report.csv", row.names=FALSE)
