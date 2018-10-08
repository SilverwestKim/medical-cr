rm(list=ls())

if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(rvest)) install.packages("rvest"); library(rvest)
if(!require(stringr)) install.packages("stringr"); library(stringr)

url="https://search.naver.com/search.naver?sm=tab_dts&where=news&query=+%22%EB%B3%91%EC%9B%90%22+%22%ED%99%98%EC%9E%90%EC%95%88%EC%A0%84%22+-%22%EC%8B%A0%EC%A0%9C%ED%92%88+%EC%B6%9C%EC%8B%9C%22+-%EC%8B%A0%EA%B8%B0%EC%88%A0+-%ED%8C%8C%EC%97%85+-%EB%B3%B4%ED%97%98+-%EC%B9%98%EB%A7%A4+-%EC%BC%80%EC%9D%B4%ED%81%AC+-%ED%8F%AD%ED%96%89+-%EB%9D%BC%EC%9D%B4%ED%94%84+-%EB%B0%98%EB%A0%A4%EA%B2%AC+-%ED%97%8C%ED%98%88+-%EB%AF%B8%EC%9A%A9+-%EC%BB%A8%EC%84%A4%ED%8C%85+&oquery=%2B%ED%99%98%EC%9E%90%EC%95%88%EC%A0%84%EB%B2%95+-%ED%8C%8C%EC%97%85+-%EB%B3%B4%ED%97%98+-%EC%B9%98%EB%A7%A4+-%EC%BC%80%EC%9D%B4%ED%81%AC+-%ED%8F%AD%ED%96%89+-%EB%9D%BC%EC%9D%B4%ED%94%84+-%EB%B0%98%EB%A0%A4%EA%B2%AC+-%ED%97%8C%ED%98%88+-%EB%AF%B8%EC%9A%A9+-%EC%BB%A8%EC%84%A4%ED%8C%85+-%22%EC%8B%A0%EC%A0%9C%ED%92%88+%EC%B6%9C%EC%8B%9C%22+-%22%EC%8B%A0%EA%B8%B0%EC%88%A0+%EA%B0%9C%EB%B0%9C%22&tqi=TIkROspySEKssZwJ%2BIsssssstg0-501000&qdt=1"

cont = read_html(url)
subcont = html_nodes(cont, '.type01')
subc= html_nodes(subcont, 'dt')
subcont1 = html_nodes(subc, 'a')
subcont2 = html_attr(subcont1, 'href')
title = html_text(subcont1)


