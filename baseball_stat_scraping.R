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



#install.packages(c("cluster", "factoextra"))



# May need to create stats that control for player stats


head(mlb_pitch_data)

summary(mlb_pitch_data)

mlb_dat_2 <- mlb_pitch_data

mlb_hit_dat <- mlb_hit_dat

save(mlb_dat_2, all_pitching_data, file = "proc_data.rda")

save(mlb_hit_dat, all_hitting_data, file = "hit_data.rda")

load("proc_data.rda")
load("hit_data.rda")


library(tidyverse)
library(cluster) # Load cluster
library(factoextra) # clustering algorithms & visualization

cluster_data <- mlb_dat_2[, c(20:107)]
cluster_data_2 <- all_pitching_data[, c(20:107)]

#cluster_data_2[, c(20:107)] <- as.numeric(cluster_data_2[, c(20:107)])

cluster_combined <- rbind.data.frame(cluster_data, cluster_data_2)

cluster_combined[,] <- lapply(cluster_combined, as.numeric)

cluster_combined[is.na(cluster_combined)] <- 0



player_combined <- c(mlb_dat_2$playerName, all_pitching_data$playerFullName)

s_dat <- scale(cluster_combined)

# Create silhouette plot summary
fviz_nbclust(s_dat, # Set dataset
             kmeans,# Set clustering method
             method = "silhouette") # Set evaluation method

cluster_number <- 5

set.seed(12345) # Set seed for reproducibility
fit_1 <- kmeans(x = s_dat, # Set data as explantory variables 
                centers = cluster_number,  # Set number of clusters
                nstart = 25, # Set number of starts
                iter.max = 1000 ) # Set maximum number of iterations to use


clusters_1 <- fit_1$cluster
# Extract centers
centers_1 <- fit_1$centers

player_combined[clusters_1 == 1]
player_combined[clusters_1 == 2]
player_combined[clusters_1 == 3]

summary(as.factor(clusters_1))

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


mlb_pitch_data$playerName[clusters_1 == 5]


# Attempt at finding the important variables


#km.perm <- KMeansSparseCluster.permute(x = s_dat, # Set data
#                                       K=3, # Set cluster number
 #                                      nperms=5)

#print(km.perm)

#km.sparse <- KMeansSparseCluster(x = s_dat, # Set data
                               #  K=3, # Set cluster number
                                # wbounds =  2.2994, # Set tuning parameter
                                # nstart = 25, # Set number of starts
                                # maxiter=100)
#clusters_4 <- km.sparse[[1]]$Cs # Extract clusters

#centers_4 <- km.sparse[[1]]$ws


#centers_4

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

major_cluster <- sdc_res$clusters1
minor_cluster <- sdc_res$clusters2

major_center <- sdc_res$centers1
minor_center <- sdc_res$centers2


mlb_dat_2$playerName[major_cluster == 4]

mlb_dat_3 <- mlb_dat_2[major_cluster == 4,]
minor_3 <- all_pitching_data[minor_cluster == 4,]

all_pitching_data$playerFullName[minor_cluster == 4]

summary(as.factor(major_cluster))

# Create vector of clusters
cluster <- c(1:5)

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




# Pre-process the data
set.seed(12345)
elite_data <- pre_proc_data(t(cluster_combined[1:nrow(mlb_dat_2),][major_cluster == 4,]), 
                            t(cluster_combined[(nrow(mlb_dat_2) + 1):nrow(cluster_combined),][minor_cluster == 4,]), norm = FALSE, log = TRUE,
                          center = TRUE)
# Extract Data
edata_A <- elite_data[[1]]
edata_B <- elite_data[[2]]

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




