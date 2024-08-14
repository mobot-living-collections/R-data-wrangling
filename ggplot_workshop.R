library(tidyverse)
library(readxl)
library(RColorBrewer)

sorghum_data <- read.csv("sorghum_phenotyper_formatted.csv")
summary(sorghum_data)
#how would you determine the number of nitrogen treatments used in this experiment?
length(unique(sorghum_data$Treatment))

#what is the average area for the genotype 'Della-S' on DAP 24?
#we need the mean of the area measurement, then need to subset the data with [] to specify we want 24 days after planting...
#...(DAP), then we need the specific genotype Della-S, so we add in the & to make the path for the Line_name Della-S
#the == means we want this character exactly. Noting the $ after the data name says we want to look within that dataset to find that variable
mean(sorghum_data$area[sorghum_data$DAP==24 & sorghum_data$Line_name=="Della-S"])

#gather data from the last day of this experiment and store it in an object named last_day
last_day <- sorghum_data %>%
  filter(DAP==max(DAP))
#investigate the datapoints of our data subset
plot(data=last_day, area~perimeter)

#make a blank figure stored into an object named "p"
p <- ggplot()
p
#define the data we're going to use
p1 <- last_day %>%
  ggplot()
p1

#establish the info on the x and y axes
#aes function enables you to create a set of mappings from your dataset to the geoms, connecting them
p2 <- last_day %>%
  ggplot(aes(perimeter, area))
p2

# + adds another layer to an existing map
p3 <- last_day %>%
  ggplot(aes(perimeter, area)) +
  geom_point()
p3

#another method for adding layers
p3a <- p2 + geom_point()
p3a

#for our data we will color code our nitrogen treatments
p4 <- last_day %>%
  ggplot(aes(perimeter, area)) +
  geom_point(aes(color=factor(Treatment)))
p4

#we can alter the color of our data by adding a scale layer to change the aesthetic
p5 <- p4 + 
  scale_color_manual(values = c('red', 'blue', 'green'))
p5
#reference guide for scales and guides https://ggplot2-book.org/scales-guides.html


#Relabel x and y axes
p6 <- p5 + 
  xlab("CLR") +
  ylab("CLR")
p6
#change the plot's background
p7 <- p6 +theme_light()
p7
#change a lot of different things in the plot
p8 <- p7 +
  theme(strip.background = element_rect(fill = "gray50"),
        strip.text.x = element_text(size=14, color="white"),
        strip.text.y = element_text(size=14, color="white"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(0.2, "cm")) +               	
  guides(color = guide_legend(title = "Nitrogen"))
p8

#change the axes to have labels for area and perimeter
p9 <- p8 + labs(x="Perimeter", y=expression("Area (cm)"^2))
p9

#save the plot
ggsave("sorghum_nitrogen_perimeter_vs_area.png", 
       width = 8.91, height = 4.55, plot = p9, dpi = 300)

#Faceting https://ggplot2-book.org/facet.html
#generates small multiples for displaying different subsets of data

#use facet_grid() to separate our nitrogen
#treatments across 3 different small plots
#separate by treatment, the ~ says "separate it by treatment"
f <- p9 + facet_grid(~Treatment)
f

#p <- ggplot(data=last_day, aes(perimeter,area)) +
 # facet_grid(~Treatment) +
  #geom_point(aes(color=factor(Treatment))) +
  #theme_light() +
  #theme(strip.backround=element_rect(fill='gray50'),
   #     strip.text.x=element_text(size=14,color='white'),
    #    strip.text.y=element_text(size=14,color='white'),
     #   axis.title=element_text(size=18),
      #  axis.text=element_text(size=14),
       # axis.ticks.length=unit(0.2,"cm")) +
#guides(color=guide_legend(title="Nitrogen"))
#p


#use only the data subsset for treatment=10, subset the data so it
#only includes low concentration Nitrogen
#use the following attributes: geom_hline(), geom_boxplot(),
#theme(axis.text.x = element_text(angle = 45, hjust=)), ylab, and xlab

lowN <- last_day %>%
  filter(Treatment==10)
c1 <- lowN %>%
  ggplot(aes(Line_name,area)) +
  geom_boxplot(aes(fill=Line_name)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  geom_hline(yintercept = mean(last_day$area),
             linetype="dashed")
c1
ggsave("ggplot2_challenege1.png", width = 8.91,height = 4.55,plot = c1, dpi = 300)

#illustrate how Nitrogen treatments affect mean plant area
my_df <- aggregate(data=sorghum_data, area~Line_name:Treatment:DAP,FUN="mean")
#plotting mean sorghum plant area by Line_name and Day after Planting (DAP)

q <- my_df %>% 
  ggplot(aes(factor(DAP), Line_name))+
  facet_grid(~Treatment)+ 
  geom_tile(aes(fill=area)) 
q
#changing color scheme
q1 <- q + scale_fill_gradientn(colors = 
        (colorRampPalette(brewer.pal(9, "Blues"))(10)), 
            "value") + theme_light()
q1

install.packages("viridis")
library(viridis)
install.packages("wesanderson")
library(wesanderson)
