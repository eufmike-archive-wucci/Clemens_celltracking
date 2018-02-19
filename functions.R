
# Read Tracked_Position.csv -----------------------------------------------

PositionReader = function(path, congrp = 'WT', expgrp = 'KO', sheetname = 'Tracked_Position.csv'){
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
                datatmp$filename = basename(dirls[m]);
                sample_name = unlist(strsplit(basename(dirls[m]), " "));
                datatmp$sample = paste(sample_name[1:2], collapse = " ");
                
                ## add group
                if (grepl(expgrp, dirls[m])){
                        datatmp$group = expgrp;
                } else {
                        datatmp$group = congrp;
                }
                
                data = rbind(data, datatmp);
        }
        data$filename = as.factor(data$filename);
        data$sample = as.factor(data$sample);
        data$group = as.factor(data$group);
        data$TrackID = factor(droplevels(data$TrackID));
        
        return(data)       
}

# Read Tracked_Velocity.csv -----------------------------------------------

VelocityReader = function(path, congrp = 'WT', expgrp = 'KO', sheetname = 'Tracked_Velocity.csv'){
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
                datatmp$filename = basename(dirls[m]);
                sample_name = unlist(strsplit(basename(dirls[m]), " "));
                datatmp$sample = paste(sample_name[1:2], collapse = " ");
                
                ## add group
                if (grepl(expgrp, dirls[m])){
                        datatmp$group = expgrp;
                } else {
                        datatmp$group = congrp;
                }
                
                data = rbind(data, datatmp);
        }
        
        data$TrackID = factor(droplevels(data$TrackID));
        
        return(data)       
}


# Calculate relative distance to the center  ------------------------------

GetDistance = function(data, center){
        data$relative.X = data$Position.X - center$Position.X.µm
        data$relative.Y = data$Position.Y - center$Position.Y.µm
        data$relative.Z = data$Position.Z - center$Position.Z.µm
        data$distance = sqrt(data$relative.X^2 + data$relative.Y^2 + data$relative.Z^2)
        return(data) 
}

# add the XYZ relative distance to the data -------------------------------

AddRelativeDistance = function(data){
        data_relative = data.frame();
        for (m in levels(data$sample)){
                pattern = m;
                datatmp = data %>% filter(., grepl(pattern, sample));
                center1 = center[(center$filename == pattern), ];
                datatmp = GetDistance(datatmp, center1);
                data_relative = rbind(data_relative, datatmp)
        }
        data_relative = tbl_df(data_relative);
        return(data_relative)
}

# add chemoidx and total velocity -----------------------------------------

# add chemoidx and total velocity
AddChemoidxVelocity = function(data) {
        datatmp = data %>% 
                mutate(., chemoidx = chemoidx(relative.X, relative.Y, relative.Z, 
                                              Velocity.X, Velocity.Y, Velocity.Z)) %>%
                mutate(., Velocity.total = sqrt(Velocity.X^2 + Velocity.Y^2 + Velocity.Z^2)) %>%
                mutate(., Velocity.totalµm_min = Velocity.total * 60) %>% 
                group_by(sample) %>%
                arrange(., Time.x)  
        datatmp$Time.min = datatmp$Time.x / 2
        return(datatmp)        
}

# calculate chemoidx
chemoidx = function(relative.X, relative.Y, relative.Z, Velocity.X, Velocity.Y, Velocity.Z){
        vector1 = -1 * cbind(relative.X, relative.Y, relative.Z);
        vector2 = cbind(Velocity.X, Velocity.Y, Velocity.Z);
        theta = includedangle(vector1, vector2)
        result = cos(theta)
        return(result)
}

# calculate included angle
includedangle = function(x,y){
        x = data.matrix(x)
        y = data.matrix(y)
        dot.prod = rowSums(x * y) 
        norm.x = apply(x, 1, norm, type = "2")
        norm.y = apply(y, 1, norm, type = "2")
        theta = acos(dot.prod / (norm.x * norm.y))
        return(as.numeric(theta))
}

# make plots --------------------------------------------------------------

# distance-time plot plus chemoidx and velocity
PlotDTChemoV = function(data, linewidth = 1, xlimits = NULL, ylimits = NULL){
        plot = ggplot(data, aes(Time.min, distance)) + 
                geom_line(aes(group = TrackID.y, colour = chemoidx, alpha = Velocity.totalµm_min), size = linewidth) + 
                scale_color_gradientn(name = "Chemotactic index (color)", colors = kovesi.rainbow_bgyr_35_85_c73(255), limits = c(-1, 1)) +
                scale_alpha_continuous(name = expression(paste("Velocity (", mu, "m/min)")), limits = c(0, 10)) +
                scale_x_continuous(name = "Time (min)") +
                scale_y_continuous(name = expression(paste("Distance (", mu, "m)"))) + 
                theme_classic()
                
        if(!is.null(xlimits))
                plot = plot + xlim(xlimits); 
        if(!is.null(ylimits))
                plot = plot + ylim(ylimits);
        return(plot);
}

# x-y plot
Plotxy = function(data, linewidth = 1){
        plot = ggplot(data, aes(relative.X, relative.Y)) + 
                geom_path(aes(group = TrackID.y, color = TrackID.y), size = linewidth, show.legend=F) + 
                scale_alpha_continuous(limits = c(0, 10)) +
                scale_x_continuous(name = expression(paste("Relative distance x (", mu, 'm)'))) +
                scale_y_continuous(name = expression(paste("Relative distance x (", mu, "m)"))) + 
                theme_classic()
        
        return(plot);
}