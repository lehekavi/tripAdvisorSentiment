# Inspired by
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html

library(rvest)

url <- "http://www.tripadvisor.com/Hotel_Review-g37209-d1762915-Reviews-JW_Marriott_Indianapolis-Indianapolis_Indiana.html"
url <- "https://www.tripadvisor.co.uk/Hotel_Review-g60763-d93555-Reviews-Sheraton_New_York_Times_Square_Hotel-New_York_City_New_York.html"

reviews <- url %>%
  read_html() %>%
  html_nodes("#REVIEWS .innerBubble")

id <- reviews %>%
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
  html_attr("title") %>%
  strptime("%b %d, %Y") %>%
  as.POSIXct()

review <- reviews %>%
  html_node(".entry .partial_entry") %>%
  html_text()

data.frame(id, quote, rating, date, review, stringsAsFactors = FALSE) %>% View()

#__________________________________________________

pages <- url %>% read_html() %>%
  html_nodes('#REVIEWS > div.deckTools.btm.test > div > div > a:nth-child(8)') %>%
  html_text() %>% strsplit(" ") %>% unlist()

for(i in 1:pages){
  print(i)
}

map_df(1:100, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  reviews <- read_html(sprintf(url, i))
  
  data.frame(id = reviews %>%
               html_nodes(".quote a") %>%
               html_attr("id"),
             quote = reviews %>%
               html_nodes(".quote span") %>%
               html_text(),
    rating = reviews %>%
               html_nodes(".rating .rating_s_fill") %>%
               html_attr("alt") %>%
               gsub(" of 5 stars", "", .) %>%
               as.integer(),
             date = reviews %>%
               html_nodes(".rating .ratingDate") %>%
               html_attr("title") %>%
               strptime("%d %B %Y") %>%
               as.POSIXct(),
             review = reviews %>%
               html_nodes(".entry .partial_entry") %>%
               html_text(),
             stringsAsFactors=FALSE)
  
}) -> hotels


