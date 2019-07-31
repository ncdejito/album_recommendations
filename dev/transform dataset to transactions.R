library("arules")

data("AdultUCI")

#convert all columns of data frame to factors
AdultUCI[] <- lapply(AdultUCI, factor)

Adult = as(AdultUCI, "transactions")

my_data = paste("1,2","1","2,3", sep="\n")

write(my_data, file = "my_basket")

trans = read.transactions("my_basket", format = "basket", sep=",")

inspect(trans)
