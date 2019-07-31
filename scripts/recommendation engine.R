
# Cosine matrix -----------------------------------------------------------

z <- apply(cleaned[,2:290], 1, sum)
cleaned$emo <- z
hist(log(z))

user_sim = cosine(as.matrix(t(x)))

# Build recommendation engine ---------------------------------------------
#source:https://www.r-bloggers.com/basic-recommendation-engine-using-r/

rec_itm_for_user = function(userNo) 
{ #calculate column wise sum 
  col_sums= list()
  rat_user = critics[userNo,2:7]
  x=1 
  tot = list()
  z=1
  for(i in 1:ncol(rat_user)){ 
    if(is.na(rat_user[1,i])) 
    { 
      col_sums[x] = sum(weight_mat[,i],na.rm=TRUE)
      x=x+1
      temp = as.data.frame(weight_mat[,i])
      sum_temp=0
      for(j in 1:nrow(temp))
      { if(!is.na(temp[j,1]))
      {
        sum_temp = sum_temp+user_sim[j,7]
      }
      } 
      tot[z] = sum_temp 
      z=z+1 
    }
  }
  z=NULL
  z=1
  for(i in 1:ncol(rat_user)){ 
    if(is.na(rat_user[1,i]))
    {
      rat_user[1,i] = col_sums[[z]]/tot[[z]] z=z+1 
    }
  } 
  return(rat_user)
}

rec_itm_for_user(7)

# Get input album moods from allmusic -------------------------------------

setwd('C:/Users/Niccolo/Documents/GitHub/personal/album_recommendations')

source <- readLines('Moods - My Beautiful Dark Twisted Fantasy.txt')

# Extract moods for album
z <- grepl('/mood/', source)

hasmood <- source[z]
moods <- trimws(gsub('<a.*>', "",
                gsub("</a>.*", "", hasmood)
                      ))

album <- data.frame(moods = moods, count = 1)

row <- as.data.frame(t(album), stringsAsFactors = F)
names(row) <- row[1,]

library(dplyr)

row <- mutate(row, album = "My Beautiful Dark Twisted Fantasy")
row <- row[2,]
row[,1:17] <- as.numeric(row[,1:17])

# Append album with moods to cleaned
z <- rbind.fill(cleaned, row)
z <- replace(z, is.na(z), 0) # replace all NAs with 0

# Find similar albums using cosine similarity
library(lsa)
alb_sim = cosine(as.matrix(t(z %>% select(-album))))

sim <- alb_sim[4753, 1:4753]
sum(sim > 0) # 721 albums are similar to Kanye West's album

sim_score <- cbind(z$album, sim) # filter to get your album

write.csv(sim_score, "C:/Users/Niccolo/Documents/Work life/Deliberate practice/album_recommendations/simscore.csv")

