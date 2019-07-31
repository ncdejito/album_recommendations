setwd('C:/Users/Niccolo/Documents/GitHub/personal/album_recommendations')

source <- readLines('allmusic page source.txt')

# Get links of all moods --------------------------------------------------

z <- grepl('http://www.allmusic.com/mood/', source)

haslinks <- source[z]

haslinks <- trimws(haslinks)

haslinks <- trimws(
  gsub("</a></h2>", "",
  gsub("\">", " ", 
  gsub("<h2><a href=\"", "", haslinks)))
)

moods <- unlist(strsplit(haslinks, " "))
rm(z, haslinks, source)


# Get album titles for each mood ------------------------------------------

library(RCurl)
library(beepr)

albvsmds <- data.frame(album = NA, mood = NA)

for (i in 1:(length(moods)/2)) {
  my.url <- paste0(moods[2*i-1], '/albums')
  html.file <- getURI(my.url, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE, followlocation = TRUE)
  
  z <- unlist(strsplit(html.file, 'href="/album/'))
  z <- z[2:length(z)] # remove super long first part
  
  albums <- trimws(gsub('\"\n', "", 
                        gsub(".*title=\"", "",
                             gsub("data-tooltip=.*", "", z)
                        )))
  
  z <- data.frame(album = albums, mood = moods[2*i])
  albvsmds <- rbind(albvsmds, z)
}
albvsmds <- albvsmds[2:nrow(albvsmds),]
rm(z, my.url, html.file, i, albums, moods)
beep("fanfare")

# create album vs mood matrix
library(dplyr)
library(reshape2)

counted <- albvsmds %>%
  group_by(album, mood) %>%
  summarise(counts = n())
cleaned <- dcast(counted, album ~ mood, fill = 0)
rm(counted)
