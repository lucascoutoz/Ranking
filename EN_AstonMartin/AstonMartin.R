library(ggbump)
library(ggplot2)
library(ggtext)
library(magick)
library(png)
library(ragg)
library(showtext)
library(sysfonts)
library(tidyverse)

F1 = 
  tribble(
    ~race, ~standing, ~driver, ~iso3, ~country,
    1, 15, "Vettel", "BHR", "Bahrain",
    1, 10, "Stroll", "BHR", "Bahrain",
    2, 15, "Vettel", "ITA", "Italy",
    2, 8, "Stroll", "ITA", "Italy",
    3, 13, "Vettel", "PRT", "Portugal",
    3, 14, "Stroll", "PRT", "Portugal",
    4, 13, "Vettel", "ESP", "Spain",
    4, 11, "Stroll", "ESP", "Spain",
    5, 5, "Vettel", "MCO", "Monaco",
    5, 8, "Stroll", "MCO", "Monaco",
    6, 2, "Vettel", "AZE", "Azerbaijan",
    6, 19, "Stroll", "AZE", "Azerbaijan", 
    7, 9, "Vettel", "FRA", "France",
    7, 10, "Stroll", "FRA", "France",
    8, 12, "Vettel", "AUT", "Austria",
    8, 8, "Stroll", "AUT", "Austria",
    9, 17, "Vettel", "AUT", "Austria",
    9, 13, "Stroll", "AUT", "Austria", 
    10, 19, "Vettel", "GBR", "United Kingdom",
    10, 8, "Stroll", "GBR", "United Kingdom",
    11, 2, "Vettel", "HUN", "Hungary",
    11, 19, "Stroll", "HUN", "Hungary",
    12, 5, "Vettel", "BEL", "Belgium",
    12, 20, "Stroll", "BEL", "Belgium",
    13, 13, "Vettel", "NLD", "Netherlands",
    13, 12, "Stroll", "NLD", "Netherlands",
    14, 12, "Vettel", "ITA", "Italy",
    14, 7, "Stroll", "ITA", "Italy", 
    15, 12, "Vettel", "RUS", "Russia",
    15, 11, "Stroll", "RUS", "Russia",
    16, 18, "Vettel", "TUR", "Turkey",
    16, 9, "Stroll", "TUR", "Turkey",
    17, 10, "Vettel", "USA", "United States",
    17, 12, "Stroll", "USA", "United States", 
    18, 7, "Vettel", "MEX", "Mexico",
    18, 14, "Stroll", "MEX", "Mexico",
    19, 11, "Vettel", "BRA", "Brazil",
    19, 20, "Stroll", "BRA", "Brazil",
    20, 10, "Vettel", "QAT", "Qatar",
    20, 6, "Stroll", "QAT", "Qatar",
    21, 16, "Vettel", "SAU", "Saudi Arabia",
    21, 11, "Stroll", "SAU", "Saudi Arabia", 
    22, 11, "Vettel", "ARE", "United Arab Emirates",
    22, 13, "Stroll", "ARE", "United Arab Emirates") %>%
  mutate(race = as.integer(race))
    

          

flags =
  tribble(
    ~whatever , ~flag,
    "a", "https://cdn.countryflags.com/thumbs/bahrain/flag-round-250.png",
           "b" , "https://cdn.countryflags.com/thumbs/italy/flag-round-250.png",
           "c" , "https://cdn.countryflags.com/thumbs/portugal/flag-round-250.png",
           "d" , "https://cdn.countryflags.com/thumbs/spain/flag-round-250.png",
           "e" , "https://cdn.countryflags.com/thumbs/monaco/flag-round-250.png",
           "f" , "https://cdn.countryflags.com/thumbs/azerbaijan/flag-round-250.png",
           "g" , "https://cdn.countryflags.com/thumbs/france/flag-round-250.png",
           "h" , "https://cdn.countryflags.com/thumbs/austria/flag-round-250.png",
           "i" , "https://cdn.countryflags.com/thumbs/austria/flag-round-250.png",
           "j" , "https://cdn.countryflags.com/thumbs/united-kingdom/flag-round-250.png",
           "k" , "https://cdn.countryflags.com/thumbs/hungary/flag-round-250.png",
           "l" , "https://cdn.countryflags.com/thumbs/belgium/flag-round-250.png",
           "m" , "https://cdn.countryflags.com/thumbs/netherlands/flag-round-250.png",
           "n" , "https://cdn.countryflags.com/thumbs/italy/flag-round-250.png",
           "o" , "https://cdn.countryflags.com/thumbs/russia/flag-round-250.png",
           "p" , "https://cdn.countryflags.com/thumbs/turkey/flag-round-250.png",
           "q" , "https://cdn.countryflags.com/thumbs/united-states-of-america/flag-round-250.png",
           "r" , "https://cdn.countryflags.com/thumbs/mexico/flag-round-250.png",
           "s" , "https://cdn.countryflags.com/thumbs/brazil/flag-round-250.png",
           "t" , "https://cdn.countryflags.com/thumbs/qatar/flag-round-250.png",
           "u" , "https://cdn.countryflags.com/thumbs/saudi-arabia/flag-round-250.png",
          "v" , "https://cdn.countryflags.com/thumbs/united-arab-emirates/flag-round-250.png")
           
           

link_to_img <- function(x, width = 30) {
  glue::glue("<img src='{x}' width='{width}'/>")
}          
             
             
flags1 <- flags %>% mutate(flag = link_to_img(flag))


##Adding Fonts
font_add(family = "regular", "Optima_Roman.ttf")
font_add(family= "bold", "OptimaBold.ttf")
showtext_auto()    

#plot theme
astonmartin_theme <- theme(
  #Title, Subtitle, Caption
  plot.title=element_text(family="bold", size=60, color="white", hjust=0.5, vjust=1),
  plot.title.position = "plot",
  plot.caption=element_text(family="regular", size=40, color="white", hjust=1, vjust=1),
  plot.caption.position = "plot",
  #Panel and Background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#045562"),
  plot.background = element_rect(fill = "#045562"),
  #Axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_text(size=45, family="regular", color="white"),
  axis.text.x = element_markdown(size=40, family="regular", color="white"),
  axis.text.y = element_text(size=30, family="regular", color="white"),
  #Legend
  legend.position = "top",
  legend.background = element_rect(fill="#045562"),
  legend.key = element_rect(fill = "#045562"),
  legend.title = element_blank(),
  legend.text = element_text(size=60, family="regular", color="white"))



f2 <- F1 %>%
  ggplot(aes(race, standing, color = driver)) +
  geom_point(size = 3) +
  geom_bump(size = 1) +
  scale_color_manual(values=c("#9282b1","#d0c8b4")) +
  scale_y_reverse(breaks = 1:20,
                  limits = c(20, 1))+
  scale_x_continuous(breaks = 1:22,
                   limits = c(1,22),
                   labels = c("1" = "<img src='https://cdn.countryflags.com/thumbs/bahrain/flag-round-250.png' width='18'/>",
                              "2" = "<img src='https://cdn.countryflags.com/thumbs/italy/flag-round-250.png' width='18'/>",
                              "3" = "<img src='https://cdn.countryflags.com/thumbs/portugal/flag-round-250.png' width='18'/>",
                              "4" = "<img src='https://cdn.countryflags.com/thumbs/spain/flag-round-250.png' width='18'/>",
                              "5" = "<img src='https://cdn.countryflags.com/thumbs/monaco/flag-round-250.png' width='18'/>",
                              "6" = "<img src='https://cdn.countryflags.com/thumbs/azerbaijan/flag-round-250.png' width='18'/>",
                              "7" = "<img src='https://cdn.countryflags.com/thumbs/france/flag-round-250.png' width='18'/>",
                              "8" = "<img src='https://cdn.countryflags.com/thumbs/austria/flag-round-250.png' width='18'/>",
                              "9" = "<img src='https://cdn.countryflags.com/thumbs/austria/flag-round-250.png' width='18'/>",
                            "10" = "<img src='https://cdn.countryflags.com/thumbs/united-kingdom/flag-round-250.png' width='18'/>",
                            "11" = "<img src='https://cdn.countryflags.com/thumbs/hungary/flag-round-250.png' width='18'/>",
                            "12" = "<img src='https://cdn.countryflags.com/thumbs/belgium/flag-round-250.png' width='18'/>",
                            "13" = "<img src='https://cdn.countryflags.com/thumbs/netherlands/flag-round-250.png' width='18'/>",
                            "14" = "<img src='https://cdn.countryflags.com/thumbs/italy/flag-round-250.png' width='18'/>",
                            "15" = "<img src='https://cdn.countryflags.com/thumbs/russia/flag-round-250.png' width='18'/>",
                            "16" = "<img src='https://cdn.countryflags.com/thumbs/turkey/flag-round-250.png' width='18'/>",
                            "17" = "<img src='https://cdn.countryflags.com/thumbs/united-states-of-america/flag-round-250.png' width='18'/>",
                            "18" = "<img src='https://cdn.countryflags.com/thumbs/mexico/flag-round-250.png' width='18'/>",
                            "19" = "<img src='https://cdn.countryflags.com/thumbs/brazil/flag-round-250.png' width='18'/>",
                            "20" = "<img src='https://cdn.countryflags.com/thumbs/qatar/flag-round-250.png' width='18'/>",
                            "21" = "<img src='https://cdn.countryflags.com/thumbs/saudi-arabia/flag-round-250.png' width='18'/>",
                            "22" = "<img src='https://cdn.countryflags.com/thumbs/united-arab-emirates/flag-round-250.png' width='18'/>")) +
  ylab("") + xlab("") +
  labs(title = "Aston Martin drivers results in 2021 F1",
       caption="Lucas Couto | Twitter: @lucas_coutoz")  +
  astonmartin_theme

f3 <- f2 + 
  geom_curve(aes(x = 14, y= 3, xend = 12, yend= 2), arrow = arrow(length=unit(0.5, "cm")), curvature = 0.2, colour = "#d0c8b4") +
  annotate(geom = "text", x = 14.2, y= 3, hjust=0, label="Vettel was disqualified from \n the Hungarian Grand Prix \n =(", family="regular", size=12, lineheight = 0.3, colour = "#d0c8b4")
  

ggsave("aston_martin.png",
       plot=f3,
       device = agg_png(width = 10, height = 8, units = "in", res = 300))

#plot theme
astonmartin_theme2 <- theme(
  #Title, Subtitle, Caption
  plot.title=element_text(family="bold", size=60, color="white", hjust=0.5, vjust=1),
  plot.caption=element_text(family="regular", size=40, color="white", hjust=1, vjust=1),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  #Panel and Background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#00584f"),
  plot.background = element_rect(fill = "#00584f"),
  #Axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_text(size=45, family="regular", color="white"),
  axis.text.x = element_markdown(size=40, family="regular", color="white"),
  axis.text.y = element_text(size=30, family="regular", color="white"),
  #Legend
  legend.position = "top",
  legend.background = element_rect(fill="#00584f"),
  legend.key = element_rect(fill = "#00584f"),
  legend.title = element_blank(),
  legend.text = element_text(size=60, family="regular", color="white"))



f2.2 <- F1 %>%
  ggplot(aes(race, standing, color = driver)) +
  geom_point(size = 3) +
  geom_bump(size = 1) +
  scale_color_manual(values=c("#9282b1","#d0c8b4")) +
  scale_y_reverse(breaks = 1:20,
                  limits = c(20, 1))+
  scale_x_continuous(breaks = 1:22,
                     limits = c(1,22),
                     labels = c("1" = "<img src='https://cdn.countryflags.com/thumbs/bahrain/flag-round-250.png' width='18'/>",
                                "2" = "<img src='https://cdn.countryflags.com/thumbs/italy/flag-round-250.png' width='18'/>",
                                "3" = "<img src='https://cdn.countryflags.com/thumbs/portugal/flag-round-250.png' width='18'/>",
                                "4" = "<img src='https://cdn.countryflags.com/thumbs/spain/flag-round-250.png' width='18'/>",
                                "5" = "<img src='https://cdn.countryflags.com/thumbs/monaco/flag-round-250.png' width='18'/>",
                                "6" = "<img src='https://cdn.countryflags.com/thumbs/azerbaijan/flag-round-250.png' width='18'/>",
                                "7" = "<img src='https://cdn.countryflags.com/thumbs/france/flag-round-250.png' width='18'/>",
                                "8" = "<img src='https://cdn.countryflags.com/thumbs/austria/flag-round-250.png' width='18'/>",
                                "9" = "<img src='https://cdn.countryflags.com/thumbs/austria/flag-round-250.png' width='18'/>",
                                "10" = "<img src='https://cdn.countryflags.com/thumbs/united-kingdom/flag-round-250.png' width='18'/>",
                                "11" = "<img src='https://cdn.countryflags.com/thumbs/hungary/flag-round-250.png' width='18'/>",
                                "12" = "<img src='https://cdn.countryflags.com/thumbs/belgium/flag-round-250.png' width='18'/>",
                                "13" = "<img src='https://cdn.countryflags.com/thumbs/netherlands/flag-round-250.png' width='18'/>",
                                "14" = "<img src='https://cdn.countryflags.com/thumbs/italy/flag-round-250.png' width='18'/>",
                                "15" = "<img src='https://cdn.countryflags.com/thumbs/russia/flag-round-250.png' width='18'/>",
                                "16" = "<img src='https://cdn.countryflags.com/thumbs/turkey/flag-round-250.png' width='18'/>",
                                "17" = "<img src='https://cdn.countryflags.com/thumbs/united-states-of-america/flag-round-250.png' width='18'/>",
                                "18" = "<img src='https://cdn.countryflags.com/thumbs/mexico/flag-round-250.png' width='18'/>",
                                "19" = "<img src='https://cdn.countryflags.com/thumbs/brazil/flag-round-250.png' width='18'/>",
                                "20" = "<img src='https://cdn.countryflags.com/thumbs/qatar/flag-round-250.png' width='18'/>",
                                "21" = "<img src='https://cdn.countryflags.com/thumbs/saudi-arabia/flag-round-250.png' width='18'/>",
                                "22" = "<img src='https://cdn.countryflags.com/thumbs/united-arab-emirates/flag-round-250.png' width='18'/>")) +
  ylab("") + xlab("") +
  labs(title = "Aston Martin drivers results in 2021 F1",
       caption="Lucas Couto | Twitter: @lucas_coutoz")  +
  astonmartin_theme2

f3.2 <- f2.2 + 
  geom_curve(aes(x = 14, y= 3, xend = 12, yend= 2), arrow = arrow(length=unit(0.5, "cm")), curvature = 0.2, colour = "#d0c8b4") +
  annotate(geom = "text", x = 14.2, y= 3, hjust=0, label="Vettel was disqualified from \n the Hungarian Grand Prix \n =(", family="regular", size=12, lineheight = 0.3, colour = "#d0c8b4", fontface = 2)


ggsave("aston_martin2.png",
       plot=f3.2,
       device = agg_png(width = 10, height = 8, units = "in", res = 300))


##Magick
teste <- image_read("aston_martin2.png")

logo <- image_read("https://amsc-prod-cd.azureedge.net/-/media/aston-martin/images/brand/amf1-logo-2021-march.jpg?mw=1920&rev=c8428793565d4c61aaa31410f82715f0&format=webp&hash=A28388C33760317FF457D358894B8226") %>% 
  image_resize(750)

plot_height <- magick::image_info(teste)$height
plot_width <- magick::image_info(teste)$width

logo_width <- magick::image_info(logo)$width
logo_height <- magick::image_info(logo)$height

plot_width * 0.01

plot_height - logo_height - plot_height * 0.99

f4 <- teste %>% 
  image_composite(logo, offset = "-2250-20")

