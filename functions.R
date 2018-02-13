StatReader = function(path, congrp = 'WT', expgrp = 'KO', sheetname = 'Tracked_Position.csv'){
        dirls = list.dirs(path);
        dirls = dirls[grepl('Tracked_Statistics', dirls)];
        data = data_frame();
        for (m in 1:length(dirls)){
                filels = list.files(dirls[m]);
                filename = file.path(dirls[m], filels[grepl(sheetname, filels)])
                datatmp = read.csv(filename, header = T, skip = 3, 
                                   colClasses = c('numeric', 
                                                  'numeric', 
                                                  'numeric', 
                                                  'factor', 
                                                  'factor',
                                                  'factor',
                                                  'numeric', 
                                                  'factor',
                                                  'factor'));
                
                datatmp = datatmp[, 1:9];
                
                ## add sample names
                datatmp$sample = basename(dirls[m]);
                
                ## add group
                if (grepl(expgrp, dirls[m])){
                        datatmp$group = expgrp;
                } else {
                        datatmp$group = congrp;
                }
                
                data = rbind(data, datatmp);
        }
        data$sample = as.factor(data$sample);
        data$group = as.factor(data$group);
        data <- data[!((data$TrackID) == ''), ];
        
        return(data)       
}
