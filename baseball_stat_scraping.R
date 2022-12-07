library(httr)
library(jsonlite)

league_request <- GET("https://bdfed.stitch.mlbinfra.com/bdfed/milb-stats/default/default/en-US?=&contextTeamId=&contextLeagueId=117")

league_data <- fromJSON(content(league_request, as = "text"))

league_ids <- league_data$leagues$id

league_ids <- gsub("l", "", league_ids)

league_link <- glue::glue("https://bdfed.stitch.mlbinfra.com/bdfed/stats/player?stitch_env=prod&season=2022&sportId=11&stats=season&group=pitching&gameType=RS&limit=1000&offset=0&sortStat=earnedRunAverage&order=asc&leagueIds={league_ids}&playerPool=ALL")

all_pitching_data <- purrr::map_df(league_link, ~{
  Sys.sleep(runif(1, 0, 1.2))
  initial_request <- GET(.x)
  
  json_data <- fromJSON(content(initial_request, as = "text"))
  
  player_data <- json_data$stats
  
  return(player_data)

})


mlb_link <- "https://bdfed.stitch.mlbinfra.com/bdfed/stats/player?stitch_env=prod&season=2022&sportId=1&stats=season&group=pitching&gameType=R&limit=1000&offset=0&sortStat=earnedRunAverage&order=asc&playerPool=ALL"

mlb_pitch_request <- GET(mlb_link)

mlb_pitch_json <- fromJSON(content(mlb_pitch_request, as = "text"))





mlb_pitch_data <- mlb_pitch_json$stats

#Hitting Data

league_request <- GET("https://bdfed.stitch.mlbinfra.com/bdfed/milb-stats/default/default/en-US?=&contextTeamId=&contextLeagueId=117")

league_data <- fromJSON(content(league_request, as = "text"))

league_ids <- league_data$leagues$id

league_ids <- gsub("l", "", league_ids)

league_link <- glue::glue("https://bdfed.stitch.mlbinfra.com/bdfed/stats/player?stitch_env=prod&season=2022&sportId=11&stats=season&group=hitting&gameType=RS&limit=1000&offset=0&sortStat=earnedRunAverage&order=asc&leagueIds={league_ids}&playerPool=ALL")

all_hitting_data <- purrr::map_df(league_link, ~{
  Sys.sleep(runif(1, 0, 1.2))
  initial_request <- GET(.x)
  
  json_data <- fromJSON(content(initial_request, as = "text"))
  
  player_data <- json_data$stats
  
  return(player_data)
  
})

mlb_link <- "https://bdfed.stitch.mlbinfra.com/bdfed/stats/player?stitch_env=prod&season=2022&sportId=1&stats=season&group=hitting&gameType=R&limit=1000&offset=0&sortStat=earnedRunAverage&order=asc&playerPool=ALL"

mlb_hit_request <- GET(mlb_link)

mlb_hit_json <- fromJSON(content(mlb_pitch_request, as = "text"))



head(mlb_pitch_data)

summary(mlb_pitch_data)

mlb_dat_2 <- mlb_pitch_data

mlb_hit_dat <- mlb_hit_dat

save(mlb_dat_2, all_pitching_data, file = "proc_data.rda")

save(mlb_hit_dat, all_hitting_data, file = "hit_data.rda")

load("proc_data.rda")
load("hit_data.rda")

#All the data will be loaded in from mlb.com and milb.com with the above code


library(tidyverse)
library(cluster) 
library(factoextra)

#We need to quickly clean the data for clustering. This involved changing most variables to numeric, scaling the data, removing any NA's and remvoing columns not necesaary for clustering.

cluster_data <- mlb_dat_2[, c(20:107)]
cluster_data_2 <- all_pitching_data[, c(20:107)]

cluster_combined <- rbind.data.frame(cluster_data, cluster_data_2)

cluster_combined[,] <- lapply(cluster_combined, as.numeric)

cluster_combined[is.na(cluster_combined)] <- 0

player_combined <- c(mlb_dat_2$playerName, all_pitching_data$playerFullName)

s_dat <- scale(cluster_combined)

# Create silhouette plot summary

fviz_nbclust(s_dat, # Set dataset
             kmeans,# Set clustering method
             method = "silhouette") # Set evaluation method

# I beleived 5 clusters was the most effective for my data rather than using the suggested 2

cluster_number <- 5

set.seed(12345)

# Fit model and assign centers and clusters for ggplot modeling

fit_1 <- kmeans(x = s_dat, 
                centers = cluster_number, 
                nstart = 25, 
                iter.max = 1000 ) 


clusters_1 <- fit_1$cluster
# Extract centers
centers_1 <- fit_1$centers

player_combined[clusters_1 == 1]
player_combined[clusters_1 == 2]
player_combined[clusters_1 == 3]

summary(as.factor(clusters_1))

# I wanted check how many players were in each cluster

summary(as.factor(clusters_1[1:nrow(mlb_pitch_data)]))
summary(as.factor(clusters_1[(nrow(mlb_pitch_data) + 1):length(clusters_1)]))

# Create vector of clusters
cluster <- c(1:cluster_number)
# Extract centers
center_df <- data.frame(cluster, centers_1)

# Reshape the data
center_reshape <- gather(center_df, features, values, winningPercentage:sacFlies)
# View first few rows
head(center_reshape)

# Create plot
g_heat_1 <- ggplot(data = center_reshape, # Set dataset
                   aes(x = features, y = cluster, fill = values)) + # Set aesthetics
  scale_y_continuous(breaks = seq(1, cluster_number, by = 1)) + # Set y axis breaks
  geom_tile() + # Geom tile for heatmap
  coord_equal() +  # Make scale the same for both axis
  theme_set(theme_bw(base_size = 22) ) + # Set theme
  scale_fill_gradient2(low = "blue", # Choose low color
                       mid = "white", # Choose mid color
                       high = "red", # Choose high color
                       midpoint =0, # Choose mid point
                       space = "Lab", 
                       na.value ="grey", # Choose NA value
                       guide = "colourbar", # Set color bar
                       aesthetics = "fill") + # Select aesthetics to apply
  coord_flip() # Rotate plot to view names clearly
# Generate plot
g_heat_1

# I found the best players were in clsuter 5 so I wanted to check who was in this cluster.

mlb_pitch_data$playerName[clusters_1 == 5]


# The SparseDC package allows us to relate two sets of data together, allowing me to view similar minor league players to our MLB cluster 5 we found earlier
library(SparseDC)

# Pre-process the data
set.seed(12345)
pre_data <- pre_proc_data(t(cluster_combined[1:nrow(mlb_dat_2),]), t(cluster_combined[(nrow(mlb_dat_2) + 1):nrow(cluster_combined),]), norm = FALSE, log = TRUE,
                          center = TRUE)
# Extract Data
pdata_A <- pre_data[[1]]
pdata_B <- pre_data[[2]]


# Calculate the lambda 1 value
lambda1 <- lambda1_calculator(pdat1 = pre_data[[1]], pdat2 = pre_data[[2]], ncluster=5,
                   alpha1 = 0.5, nboot1 = 1000)


# Calculate the lambda 2 value
lambda2 <- lambda2_calculator(pdat1 = pre_data[[1]], pdat2 = pre_data[[2]], ncluster = 5,
                   alpha2 = 0.5, nboot2 = 1000)

sdc_res <- sparsedc_cluster(pdat1 = pre_data[[1]], pdat2 = pre_data[[2]], ncluster=5, lambda1, lambda2, nitter = 20,
                 nstarts = 50, init_iter = 5)


sdc_res

#Assign clusters and centers based off minor or major league

major_cluster <- sdc_res$clusters1
minor_cluster <- sdc_res$clusters2

major_center <- sdc_res$centers1
minor_center <- sdc_res$centers2

# View the players in each cluster along with assign them to a new dataset for later in the project

mlb_dat_2$playerName[major_cluster == 4]

mlb_dat_3 <- mlb_dat_2[major_cluster == 4,]

minor_3 <- all_pitching_data[minor_cluster == 4,]

all_pitching_data$playerFullName[minor_cluster == 4]



# Create vector of clusters
cluster <- c(1:5)

#By changing minor_center to major_center below we can change the heat map to view both

rownames(minor_center) <- names(cluster_combined)
# Extract centers
center_df <- data.frame(cluster, t(minor_center[rowSums(abs(minor_center)) != 0,]))

# Reshape the data
center_reshape <- gather(center_df, features, values, winningPercentage:sacFlies)
# View first few rows
head(center_reshape)

# Create plot
g_heat_1 <- ggplot(data = center_reshape, # Set dataset
                   aes(x = features, y = cluster, fill = values)) + # Set aesthetics
  scale_y_continuous(breaks = seq(1, cluster_number, by = 1)) + # Set y axis breaks
  geom_tile() +
  labs(title = "Minor League Map")+# Geom tile for heatmap
  coord_equal() +  # Make scale the same for both axis
  theme_set(theme_bw(base_size = 22) ) + # Set theme
  scale_fill_gradient2(low = "blue", # Choose low color
                       mid = "white", # Choose mid color
                       high = "red", # Choose high color
                       midpoint =0, # Choose mid point
                       space = "Lab", 
                       na.value ="grey", # Choose NA value
                       guide = "colourbar", # Set color bar
                       aesthetics = "fill") + # Select aesthetics to apply
  coord_flip() # Rotate plot to view names clearly
# Generate plot
g_heat_1

#The blocks below allow us to view which variables Major league players score higher in compared to Minor League players
#This may not always be a good thing
#Of course, the second code chunk allows us to view which variables Minor League players score higher in


names(cluster_combined)[which(major_center[,1] > minor_center[,1])]
names(cluster_combined)[which(major_center[,2] > minor_center[,2])]
names(cluster_combined)[which(major_center[,3] > minor_center[,3])]
names(cluster_combined)[which(major_center[,4] > minor_center[,4])]
names(cluster_combined)[which(major_center[,5] > minor_center[,5])]


names(cluster_combined)[which(major_center[,1] < minor_center[,1])]
names(cluster_combined)[which(major_center[,2] < minor_center[,2])]
names(cluster_combined)[which(major_center[,3] < minor_center[,3])]
names(cluster_combined)[which(major_center[,4] < minor_center[,4])]
names(cluster_combined)[which(major_center[,5] < minor_center[,5])]

major_center[order(major_center[,4], decreasing = TRUE),4]
cbind.data.frame(names(cluster_combined), major_center[,4])

mlb_dat_2$playerName[major_cluster == 4]

all_pitching_data$playerFullName[minor_cluster == 4]

summary(as.factor(minor_cluster))


#### Elite Cluster #### 

# Here I wanted to re-cluster based solely off the best cluster from the previous code using the SparseDC package again

# Pre-process the data
set.seed(12345)
elite_data <- pre_proc_data(t(cluster_combined[1:nrow(mlb_dat_2),][major_cluster == 4,]), 
                            t(cluster_combined[(nrow(mlb_dat_2) + 1):nrow(cluster_combined),][minor_cluster == 4,]), norm = FALSE, log = TRUE,
                          center = TRUE)
# Extract Data
edata_A <- elite_data[[1]]
edata_B <- elite_data[[2]]

# The below code reassigns the clusters to a new data set to allow us to properly view the players later on after the next clustering

cluster_comb_2_mlb <- cluster_combined[1:nrow(mlb_dat_2),][major_cluster == 4,]
cluster_comb_2_minor <- cluster_combined[(nrow(mlb_dat_2) + 1):nrow(cluster_combined),][minor_cluster == 4,]


# Calculate the lambda 1 value
lambda1 <- lambda1_calculator(pdat1 = elite_data[[1]], pdat2 = elite_data[[2]], ncluster=5,
                              alpha1 = 0.5, nboot1 = 1000)


# Calculate the lambda 2 value
lambda2 <- lambda2_calculator(pdat1 = elite_data[[1]], pdat2 = elite_data[[2]], ncluster = 5,
                              alpha2 = 0.5, nboot2 = 1000)

sdc_eli <- sparsedc_cluster(pdat1 = elite_data[[1]], pdat2 = elite_data[[2]], ncluster=5, lambda1, lambda2, nitter = 20,
                            nstarts = 50, init_iter = 5)


sdc_eli


major_cluster <- sdc_eli$clusters1
minor_cluster <- sdc_eli$clusters2

summary(as.factor(major_cluster))
summary(as.factor(minor_cluster))


rownames(minor_center) <- names(cluster_combined)
# Extract centers
center_df <- data.frame(cluster, t(minor_center[rowSums(abs(minor_center)) != 0,]))

# Reshape the data
center_reshape <- gather(center_df, features, values, battersFaced:sacFlies)
# View first few rows
head(center_reshape)

cluster <- c(1:5)

# Create plot
g_heat_1 <- ggplot(data = center_reshape, # Set dataset
                   aes(x = features, y = cluster, fill = values)) + # Set aesthetics
  scale_y_continuous(breaks = seq(1, cluster_number, by = 1)) + # Set y axis breaks
  geom_tile() +
  labs(title = 'Minor League Graph')+# Geom tile for heatmap
  coord_equal() +  # Make scale the same for both axis
  theme_set(theme_bw(base_size = 22) ) + # Set theme
  scale_fill_gradient2(low = "blue", # Choose low color
                       mid = "white", # Choose mid color
                       high = "red", # Choose high color
                       midpoint =0, # Choose mid point
                       space = "Lab", 
                       na.value ="grey", # Choose NA value
                       guide = "colourbar", # Set color bar
                       aesthetics = "fill") + # Select aesthetics to apply
  coord_flip() # Rotate plot to view names clearly
# Generate plot
g_heat_1




mlb_dat_3$playerName[major_cluster == 5]

minor_3$playerFullName[minor_cluster == 5]


# Create vector of clusters

names(cluster_combined)[which(major_center[,1] > minor_center[,1])]
names(cluster_combined)[which(major_center[,2] > minor_center[,2])]
names(cluster_combined)[which(major_center[,3] > minor_center[,3])]
names(cluster_combined)[which(major_center[,4] > minor_center[,4])]
names(cluster_combined)[which(major_center[,5] > minor_center[,5])]

names(cluster_combined)[which(major_center[,1] < minor_center[,1])]
names(cluster_combined)[which(major_center[,2] < minor_center[,2])]
names(cluster_combined)[which(major_center[,3] < minor_center[,3])]
names(cluster_combined)[which(major_center[,4] < minor_center[,4])]
names(cluster_combined)[which(major_center[,5] < minor_center[,5])]

major_center[order(major_center[,5], decreasing = TRUE),5]
cbind.data.frame(names(cluster_combined), major_center[,5])

mlb_dat_2$playerName[major_cluster == 5]

all_pitching_data$playerFullName[minor_cluster == 5]

# We now have a solid list of 149 MLB players who are considered the best of the best along with 293 MiLB players that have been identified as similar players
# We started off with thousands of MiLB players and now have a list of around 300 that have a good chance to be great pros
#This code allows us to quickly decrease the amount of players that we would want to scout to add to our team.


