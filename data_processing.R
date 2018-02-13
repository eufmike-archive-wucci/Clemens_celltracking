library(ggplot2)
library(dplyr)
library(xlsx)

setwd('~/Documents/wucci_data/Clemens');

source('functions.R')
center = read.xlsx('./documents/center.xlsx', sheetIndex = 1, header = T)

# loading files
data = StatReader('./rawcsv', congrp = 'WT', expgrp = 'KO', sheetname = 'Tracked_Position.csv')
summary(data)

data = tbl_df(data)

pattern = '0926 WT'
data1 = data %>% filter(., grepl(pattern, sample))

## ggplot(data1, aes(Position.X, Position.Y, colour = TrackID)) + 
        geom_path()
