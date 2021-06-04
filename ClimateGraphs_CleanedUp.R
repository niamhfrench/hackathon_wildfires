# Hackathon Animated Graphs for MSR 

library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)
library(tidyr)
library(plotly)
library(gridExtra)

# blue, green, yellow, orange, red 
graph_colors <- c("#2B3A67","#2D936C","#F5CB5C","#F98948","#B10F2E")

# Loading in the MSR data
msr <- read.csv("MSRstats.csv",header = TRUE)
msr <- msr[,-1]

# Adding the months
months <- c("December","January","February","March","April","May","June","July","August","September","October","November")
msr$month <- rep(months,100)
# Converting to factors
msr$month <- factor(msr$month, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))

# Adding the year - change years if you need to 
years <- 1981:2080
yearvec <- c(1980)
for (y in years) {
  yearvec <- c(yearvec,rep(y,12))
}
msr$year <- yearvec[1:1200]

# Adding risk levels based on msr scale
msr$risk <- rep("Very Low",1200)
msr[msr$max > 6.16,7] <- "Extreme"
msr[(msr$max <= 6.16) & (msr$max > 1.82),7] <- "High"
msr[(msr$max <= 1.82) & (msr$max > 0.46),7] <- "Medium"
msr[(msr$max <= 0.46) & (msr$max > 0.1),7] <- "Low"
msr$risk <- factor(msr$risk, levels = c("Very Low","Low","Medium","High","Extreme"))

# Adding black theme:
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

# Static plot
p <- ggplot(msr, aes(x = year, y = max)) + 
  geom_point(aes(color = risk)) +
  scale_colour_manual(values = graph_colors[2:5]) +
  geom_vline(xintercept=2021, linetype="dashed", color = "grey") +
  xlab("Year") + ylab("Maximum Monthly Severity Rating") +
  theme_black()

anim <- p + 
  transition_states(month, transition_length = 100, state_length = 200) + 
  ggtitle('Maximum UK Monthly Severity Rating by Year: {closest_state}') + # , subtitle = 'Frame {frame} of {nframes}'
  enter_fade() + 
  exit_shrink()
animate(anim, renderer = gifski_renderer(), res = 160, width = 1200, height = 800)
anim_save("monthly_max2.gif",animation = last_animation())

# - - - GRAPHING PROPORTION OF POINTS IN EACH CATEGORY - - - 

props <- read.csv("MSRprops2.csv",header = TRUE)
props <- props[,-1]
# Adding high and extreme values
props$HE <- props$high + props$extreme
props$month <- rep(months,100)
props$month <- factor(props$month, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
props$year <- yearvec[1:1200]

# - - - HEATMAP OF HIGH & EXTREME PROPORTIONS - - - 

props$total <- props$verylow + props$low + props$medium + props$HE
props$pHE <- props$HE / props$total

# There aren't any high or extreme events in December - March o
props %>% group_by(month) %>% summarise(maxP = max(pHE))
# Reducing the data to just the relevant months
prop_red <- props[props$month %in% c("April","May","June","July","August","September","October"),]
prop_red <- prop_red[prop_red$year >= 2020,]
#prop_red <- props[props$year >= 2020,]

ggplot(data = prop_red, 
       mapping = aes(x = year,y = month,fill = pHE)) +
  geom_tile() +
  scale_fill_gradient(name = "Proportion",
                      low = graph_colors[1],
                      high = graph_colors[5]) +
  xlab(label = "Year") + ylab(label = "Month") + scale_x_continuous(breaks=decades) +
  ggtitle('Proportion of High and Extreme Monthly Severity Ratings across UK') + theme_black()

# - - - DECADES - - - 

# 1980: (1 to 109), 1990: (110 to 229), 
decades <- seq(from = 1980, by = 10, to = 2080)
props$decades <- rep(1980,1200)
for (d in decades) {
  props[(props$year >= d) & (props$year < (d + 10)),11] <- d
}

# Summing over the decade
decade_prop <- as.data.frame(props %>% group_by(decades,month) %>% summarise(vlT = sum(verylow),lowT = sum(low),medT = sum(medium),highT = sum(high),xT = sum(extreme)))
decade_prop$total <- decade_prop$vlT + decade_prop$lowT + decade_prop$medT + decade_prop$highT + decade_prop$xT
decade_prop[,9:13] <- round(decade_prop[,3:7]/decade_prop$total,4)
colnames(decade_prop)[9:13] <- c("VL","L","M","H","E")

# Converting so that all proportions are in one column
Prop <- c(decade_prop$VL,decade_prop$L,decade_prop$M,decade_prop$H,decade_prop$E)
D <- data.frame(Prop)
D$Risk <- c(rep("Very Low",131),rep("Low",131),rep("Medium",131),rep("High",131),rep("Extreme",131))
D$Risk <- factor(D$Risk, levels = c("Very Low","Low","Medium","High","Extreme"))
D$month <- rep(decade_prop$month,5)
D$month <- factor(D$month, levels = c(months[2:12],months[1]))
D$decade <- rep(decade_prop$decades,5)

D <- D[(D$decade != 2080) & (D$decade >= 2020),]

p4 <- ggplot(D, aes(x = decade, y = Prop)) + 
  geom_col(aes(fill = Risk)) +
  scale_fill_manual(values = graph_colors) +
  xlab("Decade") + ylab("Proportion of Ratings Across UK") + 
  scale_x_continuous(breaks=decades) + # name ="Dose (mg)", 
  theme_black()

anim <- p4 + 
  transition_states(month, transition_length = 100, state_length = 400) + 
  ggtitle('Monthly Severity Ratings by Decade: {closest_state}') # + , subtitle = 'Frame {frame} of {nframes}'
  #enter_fade() + exit_shrink()
animate(anim, renderer = gifski_renderer(), res = 100, width = 900, height = 600)
anim_save("decade_rating2.gif",animation = last_animation())

