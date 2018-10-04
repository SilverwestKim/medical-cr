rm(list=ls())

if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(rvest)) install.packages("rvest"); library(rvest)
if(!require(stringr)) install.packages("stringr"); library(stringr)
#if(!require(xlsx)) install.packages("xlsx"); library(xlsx)
#if(!require(readxl)) install.packages("readxl"); library(readxl)
url="http://www.dbpia.co.kr/SearchResult/Search?q=%28%28%5B%EB%B3%91%EC%9B%90%EA%B0%90%EC%97%BC%C2%A7coldb%C2%A72%C2%A751%C2%A73%5D%29%29&searchWord=%EC%A0%84%EC%B2%B4%3D%5E%24%EB%B3%91%EC%9B%90%EA%B0%90%EC%97%BC%5E*&searchWordCondition=%EC%9E%90%EB%A3%8C%EC%9C%A0%ED%98%95%3D%5E%24%EC%A0%84%EC%B2%B4%5E*&Collection=0&nSort=1&nSorttype=desc&Page=1&nPagesize=200&Multimedia=0&isFullText=0&Collection=0&SearchKeyword=%EB%B3%91%EC%9B%90%EA%B0%90%EC%97%BC&SearchOper=1&SearchOption=0&SearchMethod=2&Category=002008&Language=154001&SrvYN=&PublishDate=11&PublishSttDate=&PublishEndDate="

x=read_html(url)
title=x%>% html_nodes('.titleWarp')%>%html_text


sub=x%>%html_nodes('.content01Warp')%>%html_text

idx=seq(1,180,3)
author=sub[idx]
author = str_extract_all(author, "[가-힣]+")

source_date=sub[idx+1]
source = str_extract_all(source_date, "[가-힣]+")











