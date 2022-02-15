
music <- read.csv("/Users/lexnavarra/Desktop/DoubleProjectGroup/131 Project/Spotify Dataset/musicdata.csv")

View(music)

artist_pop1 <- aggregate(music$popularity, list(music$artists), FUN = mean)
View(artist_pop1)

x1 <- music[1:100,]
y1 <- artist_pop1[1:100,]
View(x1)
View(y1)
merge <- merge(x = x1, y = y1, by.x = c("artists"), by.y = c("Group.1"), all.x = TRUE)
View(merge)

full_merge <- merge(x = music, y = artist_pop1, by.x = c("artists"), by.y = c("Group.1"), all.x = TRUE)
View(full_merge)
#artists <- music$artists
#for (artist in artists){
 # artist_music <- music
#}
