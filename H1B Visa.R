
rm(list=ls())

library(dplyr)
library(tools)
library(leaflet)
setwd("C:/Users/Deepam Jain/Downloads")

df <- read.csv('h1b_kaggle.csv')

head(df,10)

df$state <- trimws(gsub("^.*,", "", df$WORKSITE))

df$rlat = round(df$lat,1)
df$rlon = round(df$lon,1)

#Get number of applications by case status

df %>% filter(!is.na(CASE_STATUS)) %>% group_by(CASE_STATUS) %>% 
  summarise(nr = length(lat)) %>% ungroup() -> dc

ggplot(data = dc, aes(x = reorder(CASE_STATUS,nr), y = nr/1000)) +  
  geom_bar(stat="identity", fill="blue", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="", x ="Case status", y = "Number of applications (thousands)")

#Get number of applications by case status per year

df %>% filter(!is.na(CASE_STATUS)) %>% filter(!is.na(YEAR)) %>% 
    group_by(CASE_STATUS,YEAR) %>% summarise(nr = length(CASE_STATUS)) -> dcy1

ggplot(data=dcy,aes(x=YEAR,y=nr/1000,colour = CASE_STATUS)) + geom_line() + geom_point() +
  theme_gray() + theme(legend.position = "right") + labs(x="Year", y="Applications (thousands)", colour="Case status", 
                                                         title="Case status (per year)")
#Get number of applicants for most popular top 50 jobs

df %>% filter(!is.na(SOC_NAME)) %>% group_by(SOC_NAME) %>% summarise(nr = length(SOC_NAME)) %>% 
  top_n(n=100) %>% arrange(-nr) %>% ungroup() -> ds

ds$SOC_NAME = toTitleCase(tolower(ds$SOC_NAME))
ds %>% group_by(SOC_NAME) %>% summarise(nr = sum(nr)) %>% 
  top_n(n=50) %>% arrange(-nr) %>% ungroup() -> ds

ggplot(data = ds, aes(x = reorder(SOC_NAME,nr), y = nr/1000)) +  
  geom_bar(stat="identity", fill="purple", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="", x ="Speciality", y = "Number of applications (thousands)")

#Trend of top 10 most popular job by year

df %>% filter(!is.na(SOC_NAME)) %>%  filter(!is.na(YEAR)) %>%
  filter(SOC_NAME %in% ds[1:10,]$SOC_NAME) %>%
  group_by(SOC_NAME,YEAR) %>% summarise(nr = length(SOC_NAME)) %>% ungroup() -> dsy

ggplot(data = dsy, aes(x = YEAR, y = nr/1000, colour = SOC_NAME)) +  
  geom_line() + geom_point() + theme_bw() + theme(legend.position="right") +
  labs(x="Year", y="Applications (thousands)", colour="Occupational code", 
       title="Occupational code (per year)")

#Get number of applications for top 50 job titles

df %>% group_by(JOB_TITLE) %>% summarise(nr = length(lat)) %>% 
  top_n(n=50) %>% arrange(-nr) %>% ungroup() -> dj

ggplot(data = dj, aes(x = reorder(JOB_TITLE,nr), y = nr)) +  
  geom_bar(stat="identity", fill="green", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="", x ="Job title (top 50)", y = "Number of applications")

#Application

df %>% filter(!is.na(rlat)) %>% filter(!is.na(rlon)) %>% group_by(rlat,rlon) %>%
  summarise(nr = length(rlat)) %>% ungroup() -> dl

colnames(dl) <- c("lat","lon","value")
bins <- c(max(dl$value),150000,100000,50000,min(dl$value))
pal <- colorBin("RdYlGn", domain = dl$value, bins = bins)


leaflet(data = dl) %>%
  addTiles() %>% setView(-99, 35, zoom = 4) %>%
  addCircleMarkers(
    lat=dl$lat, lng=dl$lon, radius=sqrt(dl$value)/10, color = ~pal(dl$value), weight=1.5, opacity=0.8,
    popup= paste("<br><strong>Applications: </strong>", dl$value
    ))


df %>% filter(!is.na(rlat)) %>% filter(!is.na(rlon)) %>% 
  filter(CASE_STATUS == "CERTIFIED") %>% group_by(rlat,rlon) %>%
  summarise(avg = mean(PREVAILING_WAGE)) %>% ungroup() -> dw1

colnames(dw1) <- c("lat","lon","value")
bins <- c(min(dw1$value),50000, 100000, 150000, 200000 ,max(dl$value))
pal <- colorBin("RdYlGn", domain = dw1$value, bins = bins)


leaflet(data = dw1) %>%
  addTiles() %>% setView(-99, 35, zoom = 4) %>%
  addCircleMarkers(
    lat=dw1$lat, lng=dw1$lon, radius=sqrt(dw1$value)/20, color = ~pal(dw1$value), weight=1.5, opacity=0.8,
    popup= paste("<br><strong>Average wage: </strong>", round(dw1$value/1000,0), "k$"
    ))
