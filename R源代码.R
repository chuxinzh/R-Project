#https://www.kaggle.com/gregorut/videogamesales
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(ggforce)
library(ggrepel)
library(tidyr)

game <- read.csv(file.choose(),header = TRUE
                 ,stringsAsFactors = FALSE)

lapply(game,class)
game <- game[which(game$Year != "N/A"),]
game <- game[which(game$Publisher != "N/A"),]
game <- game[which(game$Year != "Adventure"),]
game <- game[which(game$Year != "2017"),]
game <- game[which(game$Year != "2020"),]


###########

sale.year <- aggregate(x = game$Global_Sales
                       , by = list(game$Year)
                       , FUN = sum)
colnames(sale.year) <- c("year","total")

sale.year.eu <- aggregate(x = game$EU_Sales
                          , by = list(game$Year)
                          , FUN = sum)
sale.year.jp <- aggregate(x = game$JP_Sales
                          , by = list(game$Year)
                          , FUN = sum)
sale.year.na <- aggregate(x = game$NA_Sales
                          , by = list(game$Year)
                          , FUN = sum)
sale.year.oth <- aggregate(x=game$Other_Sales
                           ,by=list(game$Year)
                           ,FUN=sum)
sale.year$eu <- sale.year.eu$x
sale.year$jp <- sale.year.jp$x
sale.year$na <- sale.year.na$x
sale.year$oth <- sale.year.oth$x
sale.year$year <- as.numeric(sale.year$year)

sale.year.p <- ggplot() +geom_bar(data=filter(sale.year), aes(x = year, y = total) , stat ="identity")
sale.year.p <- sale.year.p + geom_point(data=filter(sale.year),aes(x = year, y = eu),color=c('#FFCC00')) +
  geom_line(data=filter(sale.year),aes(x = year, y = eu),color=c('#FFCC00')) 
sale.year.p <- sale.year.p + geom_point(data=filter(sale.year),aes(x = year, y = jp), color=c('#009999')) +
  geom_line(data=filter(sale.year),aes(x = year, y = jp), color=c('#009999')) 
sale.year.p <- sale.year.p + geom_point(data=filter(sale.year),aes(x = year, y = na),color=c('#CC3333')) +
  geom_line(data=filter(sale.year),aes(x = year, y = na),color=c('#CC3333'))
sale.year.p <- sale.year.p + geom_point(data=filter(sale.year),aes(x = year, y = oth),color=c('#990066')) +
  geom_line(data=filter(sale.year),aes(x = year, y = oth),color=c('#990066'))
sale.year.p <- sale.year.p + ggtitle("Game Total Sales") + theme_classic()
sale.year.p + scale_x_continuous(breaks=seq(1980,2016,2))

#######

game.genre.prop <- table(game$Genre)
game.genre.prop <- data.frame(game.genre.prop)
colnames(game.genre.prop) <- c('Genre','num')

label_value <- paste('(', round(game.genre.prop$num/sum(game.genre.prop$num) * 100, 1), '%)', sep = '')
label_value
label <- paste(game.genre.prop$Genre, label_value, sep = '')
label

game.genre.prop <- game.genre.prop %>% 
  mutate(end = 2 * pi * cumsum(num)/sum(num),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
p <- ggplot(game.genre.prop) + geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = Genre))
p <- p + geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = label,
                hjust = hjust, vjust = vjust)) + coord_fixed() + theme_void() 
p <- p + scale_x_continuous(limits = c(-1.5, 1.4),
                     name = "", breaks = NULL, labels = NULL) +
         scale_y_continuous(limits = c(-1.3, 1.2),
                     name = "", breaks = NULL, labels = NULL)
p

#####

game.plat.prop <- table(game$Platform)
game.plat.prop <- data.frame(game.plat.prop)
colnames(game.plat.prop) <- c('Platform','num')
game.plat.prop <- game.plat.prop[which(game.plat.prop$num > 96),]
game.plat.prop$prop <- game.plat.prop$num / sum(game.plat.prop$num)
game.plat.prop <- game.plat.prop[order(game.plat.prop$prop,decreasing=T), ]
game.plat.prop$ymax <- cumsum(game.plat.prop$prop)
game.plat.prop$ymin = c(0, head(game.plat.prop$ymax, n=-1))

hole <- ggplot(game.plat.prop, aes(fill=Platform, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme_bw() +
        theme(panel.grid=element_blank()) +
        theme(axis.text=element_blank()) +
        theme(axis.ticks=element_blank())

hole.label_value <- paste('(', round(game.plat.prop$prop * 100, 2), '%)', sep = '')
hole.label_value
hole.label <- paste(game.plat.prop$Platform, hole.label_value, sep = '')
hole.label

hole + geom_label_repel(aes(label=hole.label,x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE) + theme_void()

######

game.puber <- data.frame(table(game$Publisher))
colnames(game.puber) <- c("Publisher","num")
game.puber <- game.puber[order(game.puber$num), ]
game.puber <- tail(game.puber,10)


game.puber.sale <- aggregate(x=game$Global_Sales
                             ,by=list(game$Publisher)
                             ,FUN=sum)
colnames(game.puber.sale) <- c("Publisher","total")
game.puber.sale <- game.puber.sale[order(game.puber.sale$total), ]
game.puber.sale <- tail(game.puber.sale,10)
game.puber.sale

par(mar = c(5,16,4,2))
tmp <- barplot(game.puber$num, names.arg = game.puber$Publisher,horiz = TRUE,las=1)
mtext(text="Game Publish Amount by Publisher", side = 3, line = 1, cex = 2.7)

tmp <- barplot(-game.puber.sale$total, names.arg = game.puber.sale$Publisher,horiz = TRUE,las=1)
mtext(text="Game Publish Sale by Publisher", side = 3, line = 1, cex = 2.7)

#####

sales.data.region <- game %>% gather(key = "sales_region", value = "sales", "NA_Sales":"Other_Sales") 

genres.regions <- sales.data.region %>% 
  group_by(Genre, sales_region) %>% 
  summarise(total_units_sold = sum(sales))

norm = function(dat, term) {
  filt = dat %>% filter(sales_region == term)
  summed = sum(filt$total_units_sold)
  filt$norm = filt$total_units_sold / summed
  return(filt)
}

eu = norm(genres.regions, "EU_Sales")
jp = norm(genres.regions, "JP_Sales")
na = norm(genres.regions, "NA_Sales")
oth = norm(genres.regions, "Other_Sales")
genre.region.norm <- rbind(eu, jp, na, oth)

ggplot(genre.region.norm, aes(x = Genre, y = norm, fill = norm)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low = "lightyellow", high = "orange") + 
  facet_wrap(~sales_region) + xlab("Genre") + ylab("Normalized sales") + ggtitle("Game Genre Sales by region") +
  theme(axis.text.x = element_text(angle = 90, size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12), 
        plot.title = element_text(size = 16),
        strip.text	= element_text(size = 12)) 



game.2008 <- game[which(game$Year == "2008"),]
game.2008 <- game.2008[order(game.2008$Global_Sales,decreasing = T),]
head(game.2008)

game[which(game$NA_Sales == max(game$NA_Sales)),]
game[which(game$JP_Sales == max(game$JP_Sales)),]
game[which(game$EU_Sales == max(game$EU_Sales)),]
game[which(game$Other_Sales == max(game$Other_Sales)),]
game[which(game$Global_Sales == max(game$Global_Sales)),]


