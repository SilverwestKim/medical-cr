rm(list=ls())

if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(rvest)) install.packages("rvest"); library(rvest)
if(!require(stringr)) install.packages("stringr"); library(stringr)
if(!require(httr)) install.packages("httr"); library(httr)

#보도자료 사이트
url="https://www.kha.or.kr/impart/notice/list"

cont = read_html(url)
subcont = html_nodes(cont, '.board_list')



