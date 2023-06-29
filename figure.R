


library(ggplot2)
library(tidyr)
library(readxl)

data <- read_excel("data.xlsx")
data <- read_excel("data.xlsx", sheet = 'data')
head(data)

# Convert the date column to a date object
data$date <- as.Date(data$date)


# Again: First, reorganize
data$var = factor(data$var, 
                  levels=c('canopy','leaf',
                           'collectors','scapers'))

# Seocnd, rename taxa
var_new <- c('canopy' = "Canopy",
             'leaf' = "Leaf Litter",
             'collectors' = "Collectors",
             'scapers'='Scapers')

# Create the plot
p <- ggplot(data, aes(date, value, colour=event)) +
  geom_smooth(method = 'loess', span = 0.2, se = FALSE) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  scale_color_manual(values = c("#ce1256", "#0570b0")) +  # Select color lines
  
  labs(x = "", y = "value") +
  #  facet_wrap(~ var, ncol = 1, scales = "free_y") +
  # Panel   
  theme(panel.grid.major = element_line(color = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.margin = margin(0.5, 0.4, 1, 0.4, "cm"))  # Increase the margin

p


p <- p + facet_wrap_custom(~var, scales = "free", ncol = 1, 
                           scale_overrides = list(
                             scale_override(1, scale_y_continuous(breaks = c(0,20,40,60,80,100), limits = c(0, 100))),
                             scale_override(2, scale_y_continuous(breaks = c(0,2,4,6,8)))),
                           labeller = labeller(var = as_labeller(var_new)))  +
  
  # Remove axis ticks
  theme(axis.text.x=element_blank()) + #subaxis x
  theme(legend.position = "none")  # Remove legend

p


p <- p  + geom_vline(xintercept = as.Date("2017-12-01"), 
                     linetype = "dashed", color = "#df65b0",linewidth = 1.5) +
  geom_vline(xintercept = as.Date("2018-12-01"), 
             linetype = "dashed", color = "#df65b0",linewidth = 1.5)
p

library(cowplot)
p <- ggdraw(p) + draw_label("Normal year", x = 0.15, y = 0.04)
p
p <- ggdraw(p) + draw_label("Post \n disturbance", x = 0.37, y = 0.03)
p
p <- ggdraw(p) + draw_label("Years after disturbance", x = 0.7, y = 0.04)
p


#Ecology format
ggsave(file="Figure.jpeg", p, height = 10, width = 6, dpi = 600)



