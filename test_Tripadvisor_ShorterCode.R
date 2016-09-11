
library(rvest)
library(dplyr)
library(stringr)

#allReviews <- data.frame()
hotelUrls <- hotels %>% 
  filter(!city %in% c("New York") & !is.na(reviews) & cityRank>250 & cityRank<301) %>% 
#  filter(!city %in% c("New York") & !is.na(reviews) & cityRank>300 & cityRank<501) %>% 
  distinct(hotelId,.keep_all = TRUE)

for(h in 83:nrow(hotelUrls)){
#for(h in 440:445){
    print(paste0("Hotel ",h,"/",nrow(hotelUrls), " @ ", Sys.time()))
  
  #Each subsequent webpage differs, in the middle of the address, by the terms in the following 'looping' vector. Change number of terms depending on case
  #looping<-c("","or10-","or20-","or30-","or40-","or50-")
  
  url <- hotelUrls$hotelURL[h]
  # url <-
  #   "http://www.tripadvisor.com/Hotel_Review-g37209-d1762915-Reviews-JW_Marriott_Indianapolis-Indianapolis_Indiana.html"
  pages <- url %>%
    read_html() %>%
    html_nodes('#REVIEWS > div.deckTools.btm.test > div > div > a:nth-child(8)') %>%
    html_text() %>%
    strsplit(" ") %>%
    unlist()
  
  pages <- ifelse(is.null(pages),1,pages)  
  pages <- 1:pages - 1
  looping <- paste0("or", pages * 10, "-")
  
  n<-length(looping)
  tableout <- data.frame()
  n2 = 1
  for(i in looping){
#    if (i %% 20 == 0)
      print(paste0(n2,"/",n, " @ ", Sys.time()))
    #Change URL address here depending on attraction for review
    #      urllink <- paste ("http://www.tripadvisor.com/Hotel_Review-g37209-d1762915-Reviews-",ifelse(i == "or0-","",i),"JW_Marriott_Indianapolis-Indianapolis_Indiana.html",sep="")
    urllink <- paste0(str_c(str_split_fixed(url,"-",n = 5)[1:4],collapse = "-"),"-",ifelse(i == "or0-","",i),str_c(str_split_fixed(url,"-",n = 5)[5],collapse = "-"))
    
    
    reviews <- urllink %>%
      read_html() %>%
      html_nodes("#REVIEWS .innerBubble")
    
    reviewId <- reviews %>%
      html_node(".quote a") %>%
      html_attr("id")
    
    quote <- reviews %>%
      html_node(".quote span") %>%
      html_text()
    
    rating <- reviews %>%
      html_node(".rating .rating_s_fill") %>%
      html_attr("alt") %>%
      gsub(" of 5 stars", "", .) %>%
      as.integer()
    
    date <- reviews %>%
      html_node(".rating .ratingDate") %>%
      html_text() %>% 
      gsub("Reviewed ","", .) %>% 
      strptime("%b %d, %Y") %>% 
      as.Date()
    
    # date2 <- reviews %>%
    #   html_node(".rating .ratingDate") %>% 
    #   html_attr("title") %>% 
    #   mdy()
    date2 <- reviews %>%
      html_node(".rating .ratingDate") %>% 
      html_attr("title") %>% 
      strptime("%b %d, %Y") %>% 
      as.Date()
    
    review <- reviews %>%
      html_node(".entry .partial_entry") %>%
      html_text() 
    review <- gsub("\n", "", review)
    
    urlpage <- urllink
    
    temp.tableout <- data.frame(reviewId, quote, rating, date, date2, review) 
    if(nrow(temp.tableout)>0){
      temp.tableout$urlLink <- urllink
      temp.tableout$hotelId <- hotelUrls$hotelId[h]
      temp.tableout$hotelName <- hotelUrls$hotelName[h]
      
      tableout <- bind_rows(tableout,temp.tableout)
    }
    
    n2 = n2 + 1
  }
  allReviews <- bind_rows(allReviews,tableout)
}
save.image()

## Create full review URL and Collect Full Reviews
allReviews$reviewId2 = gsub("rn", "", allReviews$reviewId)
allReviews$reviewURL <-
  paste0(
    gsub(
      "Hotel_Review",
      "ShowUserReviews",
      str_split_fixed(allReviews$urlLink, "Reviews-", 2)[, 1]
    ),
    "r",
    allReviews$reviewId2,
    "-",
    str_split_fixed(allReviews$urlLink, "Reviews-", 2)[, 2]
  )
allReviews$fullReview <- NA

for(r in 1:nrow(allReviews)){
#for (r in 1:25) {
  if (r %% 50 == 0)
    print(paste0(r, "/", nrow(allReviews), " @ ", Sys.time()))
  
  urllink <- allReviews$reviewURL[r]
  revid <-
    paste0("#UR",
           allReviews$reviewId2[r],
           " #review_",
           allReviews$reviewId2[r])
  review_node <- urllink %>% read_html() %>% html_node(revid)
  ifelse(
    length(review_node) != 0,
    fullReview <- review_node %>%
      html_text() %>%
      gsub("\n", "", .),
    NA
  )
  allReviews$fullReview[r] <- fullReview
}


#Change output file name depending on attraction for review
write.csv(tableout, "C:\\Users\\DNLCA_000\\Documents\\ParadiseIsland.csv")


save.image()



