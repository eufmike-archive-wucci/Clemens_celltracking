# Written by Chien-cheng Shih
# Date: 02-15-2018

library(dplyr) # data processing
library(xlsx) # data loading
library(ggplot2) # for plot
library(pals) # color scale palette
library(magrittr) # for using pipe-like operator

# import customized functions
source('/Users/michaelshih/Documents/code/wucci/Clemens_celltracking/functions.R');

# setting for dplyr
options(dplyr.width = Inf);

# set working directory
setwd('~/Documents/wucci_data/Clemens Lab');

# data loading ------------------------------------------------------------
# goal: load files; covert empty cells to NA; remove NA
# check dimesion beffore and after data merging

# load files
data_position = PositionReader('./rawcsv');
data_velocity = VelocityReader('./rawcsv');

# remove empty data
data_position[data_position == ""] = NA;
data_position = na.omit(data_position); 
data_velocity[data_velocity == ""] = NA;
data_position = na.omit(data_position);

# merge position data and velocity data
data = merge(data_position, data_velocity, by = c('filename', 'sample', 'group', 'ID', 'Category'))

# survey dimension
summary(data_position)
summary(data_velocity)
dim(data_position) 
dim(data_velocity) 
dim(data)

# load files with center XYZ-coordination 
center = read.xlsx('./documents/center.xlsx', sheetIndex = 1, header = T);

# data processing ------------------------------------------------------------
# goal: add relative distance (xyz), chemoidx, total velocity,
#       Time(min)

# convert to datatable
data = tbl_df(data);

# add relative distance and Chemotactic index
data = data %>% AddRelativeDistance() %>%
                AddChemoidxVelocity()

# generate plots ------------------------------------------------------------

outputdir1 = '~/Documents/wucci_data/Clemens Lab/documents/plot/PlotDTChemoV';
outputdir2 = '~/Documents/wucci_data/Clemens Lab/documents/plot/Plotxy';
outputdir3 = '~/Documents/wucci_data/Clemens Lab/documents/plot/PlotDTChemoVLim';

data1 = data %>% filter(., grepl(levels(data$sample)[1], sample));
p = PlotDTChemoV(data1)
p

for (m in levels(data$sample)){
        datatmp = data %>% filter(., grepl(m, sample));  
        # plot size
        dpi = 150;
        HeightPixel = 700;
        WidthPixel = 1000;
        HeightCalc = HeightPixel / dpi;
        WidthCalc = WidthPixel / dpi;
        
        p1 = PlotDTChemoV(datatmp);
        outputfilename = file.path(outputdir1, paste(m, '.png', sep = ''));
        ggsave(outputfilename, p1, dpi = dpi, width = WidthCalc, height = HeightCalc, units = 'in');
   
        # plot size
        dpi = 150;
        HeightPixel = 700;
        WidthPixel = 700;
        HeightCalc = HeightPixel / dpi;
        WidthCalc = WidthPixel / dpi;

        p2 = Plotxy(datatmp)
        outputfilename = file.path(outputdir2, paste(m, '.png', sep = ''));
        ggsave(outputfilename, p2, dpi = dpi, width = WidthCalc, height = HeightCalc, units = 'in');
        
        # plot size
        dpi = 150;
        HeightPixel = 700;
        WidthPixel = 1000;
        HeightCalc = HeightPixel / dpi;
        WidthCalc = WidthPixel / dpi;
        
        p3 = PlotDTChemoV(datatmp, xlimits = c(0, 50));
        outputfilename = file.path(outputdir3, paste(m, '.png', sep = ''));
        ggsave(outputfilename, p3, dpi = dpi, width = WidthCalc, height = HeightCalc, units = 'in');
}

