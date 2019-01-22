##Scrapping images from TMDB website

#Load library
library(rvest)
library(purrr)
library(data.table)

#use rvest to scrape poster titles and url from themoviedatabase
url <- lapply(paste0("https://www.themoviedb.org/movie?page=",1:993),
              function(url){
                pr <- url %>% read_html()%>%html_nodes("[class='poster lazyload fade']")
                name <- pr %>% html_attr('alt')
                imgs <- pr %>% html_attr('data-src')
                c(name, imgs)
              })

#find the page numbers that lack poster pictures(it should be 20 images per page)
list_lost_pic <- NA
for (i in 1:993){
  list_lost_pic[i] <- length(url[[i]])<40
}

num <- which(list_lost_pic)
for (i in num){
  print(i)
  print(length(url[[i]]))
}

#we can see that most pages lack 1 or 2 images
#first deal with pages that lack 1 image
lost_pic1 <- NA
for (i in num){
  lost_pic1[i] <- length(url[[i]]) == 38
}
num4 <- which(lost_pic1)

#then deal with pages that lack 2 images
lost_pic2 <- NA
for (i in num){
  lost_pic2[i] <- length(url[[i]]) == 36
}
num5 <- which(lost_pic2)

num2 <- seq(1:993)
num3 <- setdiff(num2,num)
num3 <- num3[! num3 %in% 1:2]

#create dataframe that includes movie name and movie url
d <- data.frame(matrix(unlist(url[[1]]), nrow=20, ncol=2))
q <- data.frame(matrix(unlist(url[[2]]),nrow=20,ncol=2))
dat <- rbind(d,q)
for (i in num3){
  q <- data.frame(matrix(unlist(url[[i]]), nrow=20, ncol=2))
  dat <- rbind(dat,q);
}

#append pages that lack 1 image to dat
for (i in num4){
  q1 <- data.frame(matrix(unlist(url[[i]]),nrow=19,ncol=2))
  dat <- rbind(dat,q1)
}

#append pages that lack 2 images to dat
for (i in num5){
  q2 <- data.frame(matrix(unlist(url[[i]]),nrow=18,ncol=2))
  dat <- rbind(dat,q2)
}

#in the dat dataframe, only include the movies from the tmdb dataset
mw <- fread("./data/movie_words.csv",encoding="UTF-8",header=T)
dat$X1 <- str_replace_all(dat$X1,pattern="[[:punct:]]|[^[:ascii:]]", "")
d1 <- dat[dat$X1%in% mw$title,]

#write the title of movie and the url to download the poster into a csv file
write_csv(d1,path="./data/movie_image2.csv")

#download all the images into img folder
m <- fread("./data/movie_image2.csv",encoding="UTF-8",header=T)
n <- nrow(movie_img)
a <- rep(NA,n)
b <- rep(NA,n)
for (i in 1:n){
  a[i] <- m[i,2]
  b[i] <- m[i,1]
  download.file(paste0(a[i]),paste("./img/",b[i],".JPEG",sep=""),mode="wb")
}