library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

ratingsSum <- allReviews %>% 
  tbl_df() %>% 
  distinct() %>% 
  mutate(newdate = ifelse(!is.na(date), date, date2)) %>% 
  mutate(newdate = as.Date(newdate, origin = "1970-01-01")) %>% 
  select(hotelId, reviewId, rating, newdate, rating) %>% 
  left_join(hotels %>% 
              select(hotelId, hotelName, city), by = "hotelId") %>% 
  mutate(mth = format(newdate, "%Y-%m"),yr = format(newdate, "%Y")) 
  #filter(hotelId == "d112064") %>% 
  #group_by(city,yr) %>% 
  #summarise(aveRating = mean(rating))


ggplot(
  data=ratingsSum %>% group_by(city, yr) %>% summarise(aveRating = mean(rating)), 
  aes(x=yr, y=aveRating, fill=city)) +
geom_bar(stat="identity", position="dodge") + theme_bw()  + scale_fill_brewer()

#BUBBLE PLOT
ggplot(
  ratingsSum %>% 
    group_by(city,yr) %>%
    summarise(averank = mean(rating)),
  aes(x=city, y=averank, label = yr),guide=FALSE) +
  geom_jitter(aes(size = averank, colour=city)) +
  geom_text(size = 3.5) +
  theme_bw()

