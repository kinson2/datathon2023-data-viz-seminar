library(tidyverse)
###############
# art theater #
###############
film_titles <- read_csv("C:/Users/kinson2/Documents/data_the_art_theater/ticket_sales_by_showing.csv")
film_titles$EventName

#using old omdb from RStudio shiny gallery "movie explorer"
library(RSQLite)
db <- src_sqlite("C:/Users/kinson2/Documents/data_the_art_theater/movies.db")
omdb <- tbl(db, "omdb")
tomatoes <- tbl(db, "tomatoes")
colnames(omdb)
colnames(tomatoes)
all_movies <- inner_join(omdb, tomatoes, by = "ID")

all_movies$Title

# Join tables, filtering out those with <10 reviews, and select specified columns
all_movies <- inner_join(omdb, tomatoes, by = "ID") %>%
  select(ID, imdbID, Title, Year, Rating_m = Rating.x, Runtime, Genre, Released,
    Director, Writer, imdbRating, imdbVotes, Language, Country, Oscars,
    Rating = Rating.y, Meter, Reviews, Fresh, Rotten, userMeter, userRating, userReviews,
    BoxOffice, Production, Cast)

xx=as.data.frame(all_movies %>% filter(Title>1))

film0 <- inner_join(xx, film_titles, by = c("Title" = "EventName"))
dim(film0)
length(unique(film_titles$EventName))-length(unique(film0$Title)) #need 367 more genres of these films

## looping for updated omdb data
library(httr)
fen <- film_titles %>% 
  distinct(EventName) %>%
  mutate(Title2 = str_replace_all(tolower(EventName), "\\s", "+"))
fen2 <- setdiff(fen$EventName,film0$Title)
fen3 <- str_replace_all(tolower(fen2), "\\s", "+")

#this works great
#df <- vector("list", length = length(fen3))
#for(i in 1:length(fen3)){
#path <- paste0("https://www.omdbapi.com/?apikey=e1f3b7e7&t=",fen3[i])
#df[[i]] <- jsonlite::fromJSON(content(GET(url = path), as = "text", encoding = "UTF-8"),flatten = TRUE)
#}

df2 <- vector("list", length = length(fen3))
for(i in 1:length(fen3)){
  df2[[i]] <- df[[i]][-15]
}
new_films <- as.data.frame(do.call("rbind", df2))
x=jsonlite::flatten(new_films)
write_csv(, "C:/Users/kinson2/Documents/data_the_art_theater/omdb-api-2014-present.csv")
jsonlite::toJSON(x)






names(xx)
names(new_films)

film1 <- film0 %>%
  mutate(Released2 = lubridate::ymd(Released),
         ScreenDate = as.Date(EventDate, "%b %d, %Y %H:%M:%S %p"))

film2 <- film1 %>%
  mutate(Age = (ScreenDate - Released2)/365)
film2$Age

film3 <- film2 %>%
  group_by(Title) %>%
  mutate(TotalSalesPrice = sum(Price)) %>%
  distinct(Title, .keep_all = TRUE)

film33 <- film3 %>%
  select(Title,Age, Price, Quantity) %>%
  drop_na()
cor(as.numeric(film33$Age), as.numeric(film33$Price))
cor(as.numeric(film33$Age), as.numeric(film33$Quantity))

plot(film33$Age,film33$Price)
plot(film33$Age,film33$Quantity)

pairs()


###################
# urbana policing #
###################
up1 <- read_csv("C:/Users/kinson2/Documents/data_urbana_policing/urbana-police-incidents-data-part01.csv")
up2 <- read_csv("C:/Users/kinson2/Documents/data_urbana_policing/urbana-police-incidents-data-part02.csv")
up3 <- read_csv("C:/Users/kinson2/Documents/data_urbana_policing/urbana-police-incidents-data-part03.csv")
up4 <- read_csv("C:/Users/kinson2/Documents/data_urbana_policing/urbana-police-incidents-data-part04.csv")
up5 <- read_csv("C:/Users/kinson2/Documents/data_urbana_policing/urbana-police-incidents-data-part05.csv")
up6 <- read_csv("C:/Users/kinson2/Documents/data_urbana_policing/urbana-police-incidents-data-part06.csv")

#unique ids
length(unique(up1$INCIDENT))

#crime category, month, hour, year
up11 <- up1 %>%
  select(INCIDENT,`CRIME CODE`,`CRIME DESCRIPTION`,
         `CRIME CATEGORY`,`CRIME CATEGORY DESCRIPTION`,
         `YEAR OCCURRED`,`MONTH OCCURRED`)
up22 <- up2 %>%
  select(INCIDENT,`CRIME CODE`,`CRIME DESCRIPTION`,
         `CRIME CATEGORY`,`CRIME CATEGORY DESCRIPTION`,
         `YEAR OCCURRED`,`MONTH OCCURRED`)
up33 <- up3 %>%
  select(INCIDENT,`CRIME CODE`,`CRIME DESCRIPTION`,
         `CRIME CATEGORY`,`CRIME CATEGORY DESCRIPTION`,
         `YEAR OCCURRED`,`MONTH OCCURRED`)
up44 <- up4 %>%
  select(INCIDENT,`CRIME CODE`,`CRIME DESCRIPTION`,
         `CRIME CATEGORY`,`CRIME CATEGORY DESCRIPTION`,
         `YEAR OCCURRED`,`MONTH OCCURRED`)
up55 <- up5 %>%
  select(INCIDENT,`CRIME CODE`,`CRIME DESCRIPTION`,
         `CRIME CATEGORY`,`CRIME CATEGORY DESCRIPTION`,
         `YEAR OCCURRED`,`MONTH OCCURRED`)
up66 <- up6 %>%
  select(INCIDENT,`CRIME CODE`,`CRIME DESCRIPTION`,
         `CRIME CATEGORY`,`CRIME CATEGORY DESCRIPTION`,
         `YEAR OCCURRED`,`MONTH OCCURRED`)

policing <- bind_rows(up11,up22,up33,up44,up55,up66) %>%
  distinct(INCIDENT,.keep_all = TRUE)

policing %>%
  count(`YEAR OCCURRED`)
table(policing$`YEAR OCCURRED`)
tab_crime_yr <- table(policing$`YEAR OCCURRED`)

plot(names(tab_crime_yr)[-c(1:12)],unname(tab_crime_yr)[-c(1:12)], type="l", xlab="", ylab="", axes=FALSE,
     lwd=2, col="gray")
title(main="Number of Incidents Involving the Urbana Police", adj=0)
title(xlab="YEAR", adj=0)
#title(ylab="TOTAL", adj=1)
axis(1, seq(as.numeric(names(tab_crime_yr)[12]),as.numeric(names(tab_crime_yr)[length(tab_crime_yr)]),5), col="lightgrey")
#axis(2, seq(0,max(tab_crime_yr),5000), tick = FALSE)
#abline(h=seq(0,max(tab_crime_yr),5000), v=seq(as.numeric(names(tab_crime_yr)[1]),as.numeric(names(tab_crime_yr)[length(tab_crime_yr)]),10), lty=3, col="lightgrey")
#tab_crime_yr[13]
points(1988,tab_crime_yr[13], pch=19, lwd=2,col=rgb(84/255,12/255,0))
text(1988, tab_crime_yr[13]+300, format(tab_crime_yr[13], big.mark=",")  , font=2, col="#004854")
text(1988.5, tab_crime_yr[13]-500,"1988: official start\nof data collection", col=rgb(84/255,12/255,0), adj=0, cex=1.25)
points(2004, tab_crime_yr[29],pch=19, lwd=2, col=rgb(84/255,12/255,0)) #2004all-time high
text(2005.25, tab_crime_yr[29], format(tab_crime_yr[29], big.mark=",")  , font=2, col="#004854")
text(2003, tab_crime_yr[29], "2004: All-time high", col=rgb(84/255,12/255,0), adj=1, cex=1.25)
points(2007,tab_crime_yr[32], pch=19,lwd=2,col=rgb(84/255,12/255,0))
text(2007, tab_crime_yr[32]-300, format(tab_crime_yr[32], big.mark=",")  , font=2, col="#004854")
lines(2007:2009,tab_crime_yr[32:34], lwd=2,col=rgb(84/255,12/255,0))
points(2009,tab_crime_yr[34], pch=19,lwd=2,col=rgb(84/255,12/255,0))
text(2010.25, tab_crime_yr[34], format(tab_crime_yr[34], big.mark=",")  , font=2, col="#004854")
text(2005, tab_crime_yr[34]-1500, "2007-2009:\nGreat\nRecession", col=rgb(84/255,12/255,0), adj=0, cex=1.25)
lines(2020:2022,tab_crime_yr[45:47],pch=19, lwd=2, col=rgb(84/255,12/255,0))
text(2014.25, tab_crime_yr[45]-500,"2020-present:\nGreat Lockdown", col=rgb(84/255,12/255,0), adj=0, cex=1.25) #2021 recent low
points(2020,tab_crime_yr[45],pch=19, lwd=2, col=rgb(84/255,12/255,0))
#text(2020.5, 8500,"2021: All-time low", col=rgb(84/255,12/255,0), adj=1, cex=1.25) #2021 recent low
text(2021.125,tab_crime_yr[45], format(tab_crime_yr[45], big.mark=",")  , font=2, col="#004854")
points(2022,tab_crime_yr[47],pch=19, lwd=2, col=rgb(84/255,12/255,0))
text(2021.6,tab_crime_yr[47]+300, format(tab_crime_yr[47], big.mark=",")  , font=2, col="#004854")

#barplot
tab_crime_cat <- sort(table(policing$`CRIME CATEGORY DESCRIPTION`), decreasing = TRUE)
tab_crime_cat
barplot(sort(tab_crime_cat[1:5]), axes=FALSE,
        main="Top 5 Most Common Crime Categories for Urbana Police during 1988-2022",
        horiz = TRUE, col=rgb(84/255,12/255,0),
        names.arg = "")
axis(2, c(0.7,1.85,3.15,4.25,5.5), tick = FALSE, hadj = 0.75,
     labels=c("Criminal\nDamage",names(sort(tab_crime_cat[1:5]))[2:4],"Traffic\nOffenses"), las=2)
text(unname(sort(tab_crime_cat[1:5]))+c(6000,6000,6000,6000,-10000),c(0.7,1.85,3.15,4.25,5.5), format(unname(sort(tab_crime_cat[1:5])), big.mark = ","), cex=2, col=c(rep("black",4),"white") )

####################################
# champaign county traffic crashes #
####################################

#mapping
cc1 <- jsonlite::fromJSON("C:/Users/kinson2/Documents/data_champaign_county_traffic_crashes/champaign-county-A-injury-crashes-data.json")[[2]]
cc2 <- jsonlite::fromJSON("C:/Users/kinson2/Documents/data_champaign_county_traffic_crashes/champaign-county-bicycle-crashes-data.json")[[2]]
cc3 <- jsonlite::fromJSON("C:/Users/kinson2/Documents/data_champaign_county_traffic_crashes/champaign-county-fatal-crashes-data.json")[[2]]
cc4 <- jsonlite::fromJSON("C:/Users/kinson2/Documents/data_champaign_county_traffic_crashes/champaign-county-pedestrian-crashes-data.json")[[2]]

names(cc1)
table(cc4[[1]]) #not helpful
dim(cc1[[2]]) #1704 by 10
cc1[[3]] #id column
length(cc1[[4]][[2]]) #only [[2]] helpful
#do.call("rbind",cc1[[4]][[2]])
#do.call("rbind",cc1[[4]][[2]])[,1]
cc11 <- tibble(cc1[[2]], cc1[[3]], do.call("rbind",cc1[[4]][[2]])[,1], do.call("rbind",cc1[[4]][[2]])[,2])
colnames(cc11) <- c(colnames(cc11)[1:10], "id","longitude", "latitude")
cc22 <- tibble(cc2[[2]], cc2[[3]], do.call("rbind",cc2[[4]][[2]])[,1], do.call("rbind",cc2[[4]][[2]])[,2])
colnames(cc22) <- c(colnames(cc22)[1:10],"id", "longitude", "latitude")
cc33 <- tibble(cc3[[2]], cc3[[3]], do.call("rbind",cc3[[4]][[2]])[,1], do.call("rbind",cc3[[4]][[2]])[,2])
colnames(cc33) <- c(colnames(cc33)[1:10],"id", "longitude", "latitude")
cc44 <- tibble(cc4[[2]], cc4[[3]], do.call("rbind",cc4[[4]][[2]])[,1], do.call("rbind",cc4[[4]][[2]])[,2])
colnames(cc44) <- c(colnames(cc44)[1:10],"id", "longitude", "latitude")

crashes <- bind_rows(cc11,cc22,cc33,cc44)
names(crashes)

plot(crashes$longitude, crashes$latitude, 
     pch=21, lwd=20, axes=FALSE,
     xlab="",ylab="",
     col=rgb(46/255,49/255,145/255, alpha=0.25))##2E3191

table(crashes$id)

###################
# illini football #
###################
#illini_scores <- read_csv("C:/Users/kinson2/Documents/data_illini_football_scores/illini-football-scores-data.csv")
#names(illini_scores)

#illini_scoring <- read_csv("C:/Users/kinson2/Documents/data_illini_football_scores/sr-illini-scoring-data.csv")
#names(illini_scoring)

illini_seasons <- read_csv("C:/Users/kinson2/Documents/data_illini_football_scores/sr-illini-seasons-data.csv")
names(illini_seasons)
illini_seasons$Conf_Pct
plot(sort(illini_seasons$Year)[-c(1:4)],100*(illini_seasons$Conf_Pct[order(illini_seasons$Year)])[-c(1:4)], type="l",
     xlab="SEASON", ylab="WIN PERCENTAGE", main="Illini Football In-Conference Games", lwd=2,
     axes=FALSE, col="grey", col.lab="black")
axis(2, at=seq(0,100,20), las = 1, tick=FALSE, col.axis="black")
axis(1, at=seq(sort(illini_seasons$Year)[5],sort(illini_seasons$Year)[length(illini_seasons$Year)], 5), tick = FALSE,col.axis="black")
lines(sort(illini_seasons$Year)[-c(1:4)],rep(mean(100*(illini_seasons$Conf_Pct[order(illini_seasons$Year)])[-c(1:4)]), length(sort(illini_seasons$Year)[-c(1:4)])), col=1, lwd=2, lty=2)
#lm(illini_seasons$Conf_Pct~illini_seasons$Year)$coef
lines(sort(illini_seasons$Year)[-c(1:4)], sort(illini_seasons$Year)[-c(1:4)]*(-0.1707)+379.5677, col=1, lwd=2)
lines(sort(illini_seasons$Year)[-c(1:4)], rep(50,length(sort(illini_seasons$Year)[-c(1:4)])), lwd=2, lty=3)
text(1900, 42, "Mean", col=1, cex=1)
text(1900, 59, "Regression", col=1, cex=1)
text(2016, 54, "50%")
rect(1988,0,1990,100, col = rgb(255/255,95/255,5/255), density=0)
rect(1913,0,1941,100,col=rgb(19/255,41/255,75/255), density=0)
text(c(1991,1942),c(98,98),c("Coach Mackovic has \nhighest mean win %","Coach Zuppke \nhas most wins"), col=c("#FF5F05","#13294B"), adj=0)

##adding coaches
illini_seasons$Coaches2 <- str_trim(str_remove_all(illini_seasons$Coaches,"\\(|\\)|\\-|\\d"))
illini_seasons %>%
  count(Coaches2) %>%
  mutate(avg=mean(n))
illini_seasons_coaches <- illini_seasons %>%
  group_by(Coaches2) %>%
  mutate(Conf_Pct_By_Coach=mean(Conf_Pct,na.rm=TRUE),
         Conf_W_By_Coach=sum(Conf_W,na.rm = TRUE))
illini_seasons_coaches
illini_seasons_coaches %>%
  arrange(desc(Conf_W_By_Coach)) %>%
  select(Coaches2,Conf_W_By_Coach)


####################################################
# redoing some student plots with visual hierarchy #
####################################################
knitr::purl(input=paste0("C:/Users/kinson2/Documents/data_urbana_policing/analysis/prezicode.Rmd"),
            output=paste0("C:/Users/kinson2/Box/STAT 448 KINSON/spring2023/sample-analysis/analysis-lit2.R"), 
            documentation = 0)

