library(ggbump)
library(ragg)
library(showtext)
library(tidyverse)

DF = 
  tribble(
    ~year, ~ranking, ~team, ~divisao,
    2011, 47, "Brasiliense", "Série C",
    2011, 89, "Gama", "Série D", 
    2011, 90, "Formosa", "Série D",
    2012, 50, "Brasiliense", "Série C",
    2012, 75, "Ceilândia", "Série D",
    2012, 98, "Sobradinho", "Série D",
    2013, 58, "Brasiliense", "Série C",
    2013, 96, "Brasília", "Série D",
    2014, 65, "Brasiliense", "Série D",
    2014, 79, "Luziânia", "Série D",
    2015, 79, "Gama", "Série D",
    2016, 70, "Ceilândia", "Série D",
    2016, 109, "Luziânia", "Série D",
    2017, 75, "Ceilândia", "Série D",
    2017, 93, "Luziânia", "Série D",
    2018, 72, "Brasiliense", "Série D",
    2018, 111, "Ceilândia", "Série D",
    2019, 85, "Brasiliense", "Série D",
    2019, 126, "Sobradinho", "Série D",
    2020, 69, "Brasiliense", "Série D",
    2020, 77, "Gama", "Série D",
    2021, 90, "Brasiliense", "Série D",
    2021, 115, "Gama", "Série D")
    
#Colour and fonts
palette <- c("Brasília" ="#fe121c",
             "Brasiliense" =  "#f3dc04",
             "Ceilândia" = "#3c3c3c",
             "Formosa" = "#74a48f",
             "Gama" = "#026b4a",
             "Luziânia" = "#0579c1",
             "Sobradinho" = "#ca9732")

font_add(family = "regular", "Barlow Semi Condensed-Regular.ttf")
font_add(family= "bold", "BarlowSemiCondensed-SemiBold.ttf")
showtext_auto()   



#Plots
red1 <- data.frame(xmin1=2010.5, xmax1=2021.5, ymin1=60, ymax1=57)
green1 <- data.frame(xmin1=2010.5, xmax1=2021.5, ymin1=44, ymax1=41)
green2 <- data.frame(xmin1=2010.5, xmax1=2021.5, ymin1=64, ymax1=60.1)
black1 <- data.frame(xmin1=2010.5, xmax1=2013, ymin1=128, ymax1=101)
black2 <- data.frame(xmin1=2013, xmax1=2014, ymin1=128, ymax1=102)
black3 <- data.frame(xmin1=2014, xmax1=2015, ymin1=128, ymax1=101)



df <- DF %>% 
  ggplot(aes(year, ranking, color = team)) +
  geom_rect(data = red1, aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
            fill = "#940608",
            alpha = 0.4,
            inherit.aes = FALSE) +
  geom_rect(data = green1, aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
            fill = "#089406",
            alpha = 0.4,
            inherit.aes = FALSE) +
  geom_rect(data = green2, aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
            fill = "#089406",
            alpha = 0.4,
            inherit.aes = FALSE) +
  geom_rect(data = black1, aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
            fill = "black",
            alpha = 1,
            inherit.aes = FALSE) +
  geom_rect(data = black2, aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
            fill = "black",
            alpha = 1,
            inherit.aes = FALSE) +
  geom_rect(data = black3, aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
            fill = "black",
            alpha = 1,
            inherit.aes = FALSE) +
  geom_point(size = 5) +
  geom_bump() +
  scale_y_reverse(breaks = c(41, 45, 50, 55, 60, 65, 70, 75, 80,
                             85, 90, 95, 100, 105, 110, 115,
                             120, 125, 128),
                  limits = c(128, 41)) +
  scale_x_continuous(breaks = 2011:2021,
                     limits = c(2010.5,2021.5)) +
  scale_color_manual(values = palette) +
  coord_cartesian(expand = FALSE) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "#060606", size = 1) +
  annotate("text", label = "S?rie C", x = 2010.8, y = 56, size = 15, color = "#000000", family = "regular") +
  annotate("text", label = "S?rie D", x = 2010.8, y = 99.9, size = 15, color = "#000000", family = "regular") +
  ylab("Classificação conjunta de todas as divis?es do Brasileiro") + xlab ("") +
  labs(title = "10 anos perdidos?",
       subtitle = "Desempenho dos times do DF nos campeonatos brasileiros nos últimos 10 anos",
       caption = "Lucas Couto | Twitter: @lucas_coutoz") +
  theme(
    #Title, Subtitle, Caption
    plot.title=element_text(family="bold", size=60, color="black", hjust=0.5, vjust=1),
    plot.subtitle=element_text(family="bold", size=50, color="black", hjust=0.5, vjust=1),
    plot.caption=element_text(family="regular", size=40, color="black", hjust=1, vjust=1),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #Panel and Background
    panel.border=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    #Axes
    axis.ticks.length=unit(0.15, "cm"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title = element_text(size=45, family="regular"),
    axis.text.x = element_text(size=40, family="regular"),
    axis.text.y = element_text(size=40, family="regular"),
    #Legend
    legend.position = "top",
    legend.background = element_rect(fill="white"),
    legend.key = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.text = element_text(size=60, family="regular"))
  
    
ggsave("df.png",
       plot=df,
       device = agg_png(width = 10, height = 8, units = "in", res = 300))

    
   