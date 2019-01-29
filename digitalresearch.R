library(tidyverse)
library(magrittr)
library(igraph)
library(dils)
library(descr)
library(gdata)

sample <- read.csv("data.csv", stringsAsFactors = F)
View(sample)

# Extracting all apps in sequence
library(qdap)
library(dplyr)

name_1 <- rm_between(sample$user_apps_1[1], "n=", ",", extract = TRUE)[[1]]
count_1 <-rm_between(sample$user_apps_1[1], "s=", ",", extract = TRUE)[[1]]
duration_1 <- rm_between(sample$user_apps_1[1], "d=", ",", extract = TRUE)[[1]]
day_1 <- rm_between(sample$user_apps_1[1], "ud=", "}", extract = TRUE)[[1]]

# Exacting the 1st app's traces
app1 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app1) <- c("name1", "count1", "duration1", "day1")

user_apps_1 <- sample$user_apps_1
for (i in 1:4002) {
  app1$name1[i] <- rm_between(sample$user_apps_1[i], "n=", ",", extract = TRUE)[[1]]
  app1$count1[i] <-rm_between(sample$user_apps_1[i], "s=", ",", extract = TRUE)[[1]]
  app1$duration1[i] <- rm_between(sample$user_apps_1[i], "d=", ",", extract = TRUE)[[1]]
  app1$day1[i] <- rm_between(sample$user_apps_1[i], "ud=", "}", extract = TRUE)[[1]]
}

app1$count1 <- as.numeric(app1$count1)
app1$duration1 <- as.numeric((app1$duration1))
app1$day1 <- as.numeric(app1$day1)


# Exacting the 2nd app's traces
app2 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app2) <- c("name2", "count2", "duration2", "day2")

user_apps_2 <- sample$user_apps_2
for (i in 1:4002) {
  app2$name2[i] <- rm_between(sample$user_apps_2[i], "n=", ",", extract = TRUE)
  app2$count2[i] <-rm_between(sample$user_apps_2[i], "s=", ",", extract = TRUE)
  app2$duration2[i] <- rm_between(sample$user_apps_2[i], "d=", ",", extract = TRUE)
  app2$day2[i] <- rm_between(sample$user_apps_2[i], "ud=", "}", extract = TRUE)
}

app2$count2 <- as.numeric(app2$count2)
app2$duration2 <- as.numeric((app2$duration2))
app2$day2 <- as.numeric(app2$day2)

# Exacting the 3rd app's traces
app3 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app3) <- c("name3", "count3", "duration3", "day3")

user_apps_3 <- sample$user_apps_3
for (i in 1:4002) {
  app3$name3[i] <- rm_between(sample$user_apps_3[i], "n=", ",", extract = TRUE)
  app3$count3[i] <-rm_between(sample$user_apps_3[i], "s=", ",", extract = TRUE)
  app3$duration3[i] <- rm_between(sample$user_apps_3[i], "d=", ",", extract = TRUE)
  app3$day3[i] <- rm_between(sample$user_apps_3[i], "ud=", "}", extract = TRUE)
}

app3$count3 <- as.numeric(app3$count3)
app3$duration3 <- as.numeric((app3$duration3))
app3$day3 <- as.numeric(app3$day3)

# Exacting the 4th app's traces
app4 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app4) <- c("name4", "count4", "duration4", "day4")

user_apps_4 <- sample$user_apps_4
for (i in 1:4002) {
  app4$name4[i] <- rm_between(sample$user_apps_4[i], "n=", ",", extract = TRUE)
  app4$count4[i] <-rm_between(sample$user_apps_4[i], "s=", ",", extract = TRUE)
  app4$duration4[i] <- rm_between(sample$user_apps_4[i], "d=", ",", extract = TRUE)
  app4$day4[i] <- rm_between(sample$user_apps_4[i], "ud=", "}", extract = TRUE)
}

app4$count4 <- as.numeric(app4$count4)
app4$duration4 <- as.numeric((app4$duration4))
app4$day4 <- as.numeric(app4$day4)

# Exacting the 5th app's traces
app5 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app5) <- c("name5", "count5", "duration5", "day5")

user_apps_5 <- sample$user_apps_5
for (i in 1:4002) {
  app5$name5[i] <- rm_between(sample$user_apps_5[i], "n=", ",", extract = TRUE)
  app5$count5[i] <-rm_between(sample$user_apps_5[i], "s=", ",", extract = TRUE)
  app5$duration5[i] <- rm_between(sample$user_apps_5[i], "d=", ",", extract = TRUE)
  app5$day5[i] <- rm_between(sample$user_apps_5[i], "ud=", "}", extract = TRUE)
}

app5$count5 <- as.numeric(app5$count5)
app5$duration5 <- as.numeric((app5$duration5))
app5$day5 <- as.numeric(app5$day5)

# Exacting the 6th app's traces
app6 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app6) <- c("name6", "count6", "duration6", "day6")

user_apps_6 <- sample$user_apps_6
for (i in 1:4002) {
  app6$name6[i] <- rm_between(sample$user_apps_6[i], "n=", ",", extract = TRUE)
  app6$count6[i] <-rm_between(sample$user_apps_6[i], "s=", ",", extract = TRUE)
  app6$duration6[i] <- rm_between(sample$user_apps_6[i], "d=", ",", extract = TRUE)
  app6$day6[i] <- rm_between(sample$user_apps_6[i], "ud=", "}", extract = TRUE)
}

app6$count6 <- as.numeric(app6$count6)
app6$duration6 <- as.numeric((app6$duration6))
app6$day6 <- as.numeric(app6$day6)

# Exacting the 7th app's traces
app7 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app7) <- c("name7", "count7", "duration7", "day7")

user_apps_7 <- sample$user_apps_7
for (i in 1:4002) {
  app7$name7[i] <- rm_between(sample$user_apps_7[i], "n=", ",", extract = TRUE)
  app7$count7[i] <-rm_between(sample$user_apps_7[i], "s=", ",", extract = TRUE)
  app7$duration7[i] <- rm_between(sample$user_apps_7[i], "d=", ",", extract = TRUE)
  app7$day7[i] <- rm_between(sample$user_apps_7[i], "ud=", "}", extract = TRUE)
}

app7$count7 <- as.numeric(app7$count7)
app7$duration7 <- as.numeric((app7$duration7))
app7$day7 <- as.numeric(app7$day7)

# Exacting the 8th app's traces
app8 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app8) <- c("name8", "count8", "duration8", "day8")

user_apps_8 <- sample$user_apps_8
for (i in 1:4002) {
  app8$name8[i] <- rm_between(sample$user_apps_8[i], "n=", ",", extract = TRUE)
  app8$count8[i] <-rm_between(sample$user_apps_8[i], "s=", ",", extract = TRUE)
  app8$duration8[i] <- rm_between(sample$user_apps_8[i], "d=", ",", extract = TRUE)
  app8$day8[i] <- rm_between(sample$user_apps_8[i], "ud=", "}", extract = TRUE)
}

app8$count8 <- as.numeric(app8$count8)
app8$duration8 <- as.numeric((app8$duration8))
app8$day8 <- as.numeric(app8$day8)

# Exacting the 9th app's traces
app9 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app9) <- c("name9", "count9", "duration9", "day9")

user_apps_9 <- sample$user_apps_9
for (i in 1:4002) {
  app9$name9[i] <- rm_between(sample$user_apps_9[i], "n=", ",", extract = TRUE)
  app9$count9[i] <-rm_between(sample$user_apps_9[i], "s=", ",", extract = TRUE)
  app9$duration9[i] <- rm_between(sample$user_apps_9[i], "d=", ",", extract = TRUE)
  app9$day9[i] <- rm_between(sample$user_apps_9[i], "ud=", "}", extract = TRUE)
}

app9$count9 <- as.numeric(app9$count9)
app9$duration9 <- as.numeric((app9$duration9))
app9$day9 <- as.numeric(app9$day9)

# Exacting the 10th app's traces
app10 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app10) <- c("name10", "count10", "duration10", "day10")

user_apps_10 <- sample$user_apps_10
for (i in 1:4002) {
  app10$name10[i] <- rm_between(sample$user_apps_10[i], "n=", ",", extract = TRUE)
  app10$count10[i] <-rm_between(sample$user_apps_10[i], "s=", ",", extract = TRUE)
  app10$duration10[i] <- rm_between(sample$user_apps_10[i], "d=", ",", extract = TRUE)
  app10$day10[i] <- rm_between(sample$user_apps_10[i], "ud=", "}", extract = TRUE)
}

app10$count10 <- as.numeric(app10$count10)
app10$duration10 <- as.numeric((app10$duration10))
app10$day10 <- as.numeric(app10$day10)

# Exacting the 11th app's traces
app11 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app11) <- c("name11", "count11", "duration11", "day11")

user_apps_11 <- sample$user_apps_11
for (i in 1:4002) {
  app11$name11[i] <- rm_between(sample$user_apps_11[i], "n=", ",", extract = TRUE)
  app11$count11[i] <-rm_between(sample$user_apps_11[i], "s=", ",", extract = TRUE)
  app11$duration11[i] <- rm_between(sample$user_apps_11[i], "d=", ",", extract = TRUE)
  app11$day11[i] <- rm_between(sample$user_apps_11[i], "ud=", "}", extract = TRUE)
}

app11$count11 <- as.numeric(app11$count11)
app11$duration11 <- as.numeric((app11$duration11))
app11$day11 <- as.numeric(app11$day11)

# Exacting the 12th app's traces
app12 <- data.frame(matrix(NA, nrow = 4002, ncol = 4))
colnames(app12) <- c("name12", "count12", "duration12", "day12")

user_apps_12 <- sample$user_apps_12
for (i in 1:4002) {
  app12$name12[i] <- rm_between(sample$user_apps_12[i], "n=", ",", extract = TRUE)
  app12$count12[i] <-rm_between(sample$user_apps_12[i], "s=", ",", extract = TRUE)
  app12$duration12[i] <- rm_between(sample$user_apps_12[i], "d=", ",", extract = TRUE)
  app12$day12[i] <- rm_between(sample$user_apps_12[i], "ud=", "}", extract = TRUE)
}

app12$count12 <- as.numeric(app12$count12)
app12$duration12 <- as.numeric((app12$duration12))
app12$day12 <- as.numeric(app12$day12)

# Store all app traces
app <- data.frame(sample$id, app1, app2, app3, app4,app5, app6, app7, app8, app9, app10, app11, app12)
appnames <- select(app, sample.id, name1, name2, name3, name4, name5, name6, name7, name8, name9, name10, name11, name12)

# Input Categories and Categoris/Genres
genre <- read.csv("genre.csv", stringsAsFactors = F)
View(genre)

library(descr) #freq
genre_freq <- freq(genre$Genre)

# Match the names of apps with the genres of apps
appnames$genre1 <- genre$Genre[match(appnames$name1, genre$Tag)]
appnames$cat1 <- genre$Cat[match(appnames$name1,genre$Tag)]

appnames$genre2 <- genre$Genre[match(appnames$name2, genre$Tag)]
appnames$cat2 <- genre$Cat[match(appnames$name2,genre$Tag)]

appnames$genre3 <- genre$Genre[match(appnames$name3, genre$Tag)]
appnames$cat3 <- genre$Cat[match(appnames$name3,genre$Tag)]

appnames$genre4 <- genre$Genre[match(appnames$name4, genre$Tag)]
appnames$cat4 <- genre$Cat[match(appnames$name4,genre$Tag)]

appnames$genre5 <- genre$Genre[match(appnames$name5, genre$Tag)]
appnames$cat5 <- genre$Cat[match(appnames$name5,genre$Tag)]

appnames$genre6 <- genre$Genre[match(appnames$name6, genre$Tag)]
appnames$cat6 <- genre$Cat[match(appnames$name6,genre$Tag)]

appnames$genre7 <- genre$Genre[match(appnames$name7, genre$Tag)]
appnames$cat7 <- genre$Cat[match(appnames$name7,genre$Tag)]

appnames$genre8 <- genre$Genre[match(appnames$name8, genre$Tag)]
appnames$cat8 <- genre$Cat[match(appnames$name8,genre$Tag)]

appnames$genre9 <- genre$Genre[match(appnames$name9, genre$Tag)]
appnames$cat9 <- genre$Cat[match(appnames$name9,genre$Tag)]

appnames$genre10 <- genre$Genre[match(appnames$name10, genre$Tag)]
appnames$cat10 <- genre$Cat[match(appnames$name10,genre$Tag)]

appnames$genre11 <- genre$Genre[match(appnames$name11, genre$Tag)]
appnames$cat11 <- genre$Cat[match(appnames$name11,genre$Tag)]

appnames$genre12 <- genre$Genre[match(appnames$name12, genre$Tag)]
appnames$cat12 <- genre$Cat[match(appnames$name12,genre$Tag)]

# A combined new database for app categories and use intensity.
app <- merge(app, appnames)

library(psych)
library(ltm) # cronbach.alpha(na.omit(app1_rel))




######### Making networks among app categories
### select apps'categories
cat_node <- app[,c(1, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73)]

# Create issue edges
cat_edge = data.frame()
for (i in 1: length(dimnames(cat_node)[[1]])) {
  v = unlist(cat_node[i, 2:13]) 
  v = t(combn(v,2))
  cat_edge = rbind(cat_edge, v)
} 
cat_edge <- na.omit(cat_edge)
cat_edge$weight <- 1


# Transforming the name to be coded value.
cat_edge$V1R <- cat_code$Value[match(cat_edge$V1,cat_code$Cat)]
cat_edge$V2R <- cat_code$Value[match(cat_edge$V2,cat_code$Cat)]

# Turn to undirectional
cat_edge_R <- cat_edge[,c(4, 5, 3)]
cat_edge_R <- t(apply(cat_edge_R,1,sort)) %>% as.data.frame() 
colnames(cat_edge_R)[colnames(cat_edge_R) == 'V3'] <- 'weight'
cat_edge_R <- aggregate(weight~V1+V2, data = cat_edge_R, FUN = sum)


# Creating full matrix: 22*22
cat_network <- AdjacencyFromEdgelist(cat_edge_R, check.full = TRUE) %>% as.data.frame()
cat_network <- cat_network[1:22, 1:22]

# Renaming matrix names
colnames(cat_network) <- cat_code$Cat
rownames(cat_network) <- cat_code$Cat
write.csv(cat_code, file = "genre_node.csv")

# Adjusting final matrix
cat_network <- cat_network %>% as.matrix
diag(cat_network) <- 0
lowerTriangle(cat_network) <- upperTriangle(cat_network, byrow = TRUE)

write.csv(cat_network, file = "app_genre_matrix.csv")

# Plotting
install.packages("GGally")
library(GGally)

ggnet2(cat_network, label = TRUE)

plot1 <- graph_from_adjacency_matrix(cat_network, mode = c("undirected"), weighted = TRUE, diag = FALSE,add.rownames = TRUE)
# If you want to set the vertex labels to be agenda1 ... agenda 10, then change "add.colnames" to be "add.rownames".
summary(plot1)
# Call for the list of edge weight
E(plot1)$weight
# Plot network (edge weight as the thickness and distance), Names of vertex represent the order of agendas accordingly.
plot.igraph(plot1,vertex.label=V(plot1)$name,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(plot1)$weight*.005)

# Exporting the edge list
library(reshape2)
app_genre_edge <- melt(cat_network)
write.csv(app_genre_edge, file = "app_genre_edge.csv")


# Relabeling
cat_label <- read.csv("genre_node_E&C.csv", stringsAsFactors = F)
# For network
colnames(cat_network) <- cat_label$Label[match(colnames(cat_network),cat_label$Cat)]
rownames(cat_network) <- cat_label$Label[match(rownames(cat_network),cat_label$Cat)]
# For edge list
app_genre_edge <- melt(cat_network)

plot1 <- graph_from_adjacency_matrix(cat_network, mode = c("undirected"), weighted = TRUE, diag = FALSE,add.rownames = TRUE)



### Genre Networks for Male and Female



### Media consumption
app <- apply(app,2,as.character)
write.csv(app, file = "appuse.csv")


## Calculating the occurence of each app category.
appcat <- app %>% select(62:73)
appcat$id <- 1:4002

# long format to wide format for each column
library(reshape)
appcat$n = 1
appcat1 = cast(appcat, id~cat1, value='n')
appcat2 = cast(appcat, id~cat2, value='n')
appcat3 = cast(appcat, id~cat3, value='n')
appcat4 = cast(appcat, id~cat4, value='n')
appcat5 = cast(appcat, id~cat5, value='n')
appcat6 = cast(appcat, id~cat6, value='n')
appcat7 = cast(appcat, id~cat7, value='n')
appcat8 = cast(appcat, id~cat8, value='n')
appcat9 = cast(appcat, id~cat9, value='n')
appcat10 = cast(appcat, id~cat10, value='n')
appcat11 = cast(appcat, id~cat11, value='n')
appcat12 = cast(appcat, id~cat12, value='n')

# merge/sum up all together
library(plyr)
pp <- cbind(names=c(rownames(appcat1), rownames(appcat2), rownames(appcat3), rownames(appcat4), rownames(appcat5), rownames(appcat6), rownames(appcat7), rownames(appcat8), rownames(appcat9), rownames(appcat10), rownames(appcat11), rownames(appcat12)), 
            rbind.fill(list(appcat1, appcat2, appcat3, appcat4, appcat5, appcat6, appcat7, appcat8, appcat9, appcat10, appcat11, appcat12)))
appcat_matrix <- ddply(pp, .(names), function(x) colSums(x[,-1], na.rm = TRUE))

write.csv(appcat_matrix, file = "appcounts.csv")

# add the media use for each app (1-12)
appcat1$count1 <- app$count1
appcat1$duration1 <- app$duration1
appcat1$day1 <- app$day1
appcat1$adhesion1 <- (appcat1$duration1/7)/(appcat1$count1/appcat1$day1) #how many seconds per use


appcat2$count2 <- app$count2
appcat2$duration2 <- app$duration2
appcat2$day2 <- app$day2

appcat3$count3 <- app$count3
appcat3$duration3 <- app$duration3
appcat3$day3 <- app$day3

appcat4$count4 <- app$count4
appcat4$duration4 <- app$duration4
appcat4$day4 <- app$day4

appcat5$count5 <- app$count5
appcat5$duration5 <- app$duration5
appcat5$day5 <- app$day5

appcat6$count6 <- app$count6
appcat6$duration6 <- app$duration6
appcat6$day6 <- app$day6

appcat7$count7 <- app$count7
appcat7$duration7 <- app$duration7
appcat7$day7 <- app$day7

appcat8$count8 <- app$count8
appcat8$duration8 <- app$duration8
appcat8$day8 <- app$day8

appcat9$count9 <- app$count9
appcat9$duration9 <- app$duration9
appcat9$day9 <- app$day9

appcat10$count10 <- app$count10
appcat10$duration10 <- app$duration10
appcat10$day10 <- app$day10

appcat11$count11 <- app$count11
appcat11$duration11 <- app$duration11
appcat11$day11 <- app$day11

appcat12$count12 <- app$count12
appcat12$duration12 <- app$duration12
appcat12$day12 <- app$day12


### Genre's network (weight as use adherece)
app_adhere <- zap_formats(app_adhere)
app_adhere <- zap_widths(app_adhere)
app_adhere <- remove_labels(app_adhere)

app_adhere_network <- t(as.matrix(app_adhere[,2:23]))  %*% as.matrix(app_adhere[,2:23]) 
app_adhere_network <- app_adhere_network/60/60

app_adhere_network <- app_adhere_network %>% as.matrix
diag(app_adhere_network) <- 0
lowerTriangle(app_adhere_network) <- upperTriangle(app_adhere_network, byrow = TRUE)

write.csv(app_adhere_network, file = "app_adhere_network.csv")

app_adhere_edge <- melt(app_adhere_network)
write.csv(app_adhere_edge, file = "app_adhere_edge.csv")