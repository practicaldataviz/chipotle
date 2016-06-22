# Histogram of Chipotle Meal Calories

setwd("~/Dropbox/pdv/data/chipotle") # set working directory if necessary

library(ggplot2) # load ggplot2 library
meals <- read.csv("meals.csv") # load meals data

# assign hex colors to variables
dark_pink <- "#9E4B6C"
white <- "#FFFFFF"

ggplot(data=meals, # select the data
       aes(x=total_calories)) +  # map total_calories to x-axis
       geom_histogram(fill = dark_pink, # set fill color
                      color = white, # set border color
                      binwidth = 50, # set binwidth to 50
                      center = 25)  # set first bar center to 25

# Remove background
ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_histogram(fill = dark_pink,
                      col = light_gray,
                      binwidth = 50,
                      center = 25) +
       theme(panel.background = element_rect(fill=white)) # set bg to white
             

# Remove background, tickmarks, and titles
ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_histogram(fill = dark_pink,
                      col = white,
                      binwidth = 50,
                      center = 25) +
       theme(panel.background = element_rect(fill=white), # set bg to white
             axis.ticks = element_blank(), # remove axis tickmarks
             axis.title = element_blank()) # remove axis titles

# Remove background, tickmarks, titles, and empty space between axes and data
ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_histogram(fill = dark_pink,
                      col = white,
                      binwidth = 50,
                      center = 25) +
       scale_x_continuous(expand = c(0, 0)) +
       scale_y_continuous(expand = c(0, 0)) +
       theme(panel.background = element_rect(fill=white), # set bg to white
             axis.ticks = element_blank(), # remove axis tickmarks
             axis.title = element_blank()) # remove axis titles

## Adding contextual elements

# Add more labels to x-axis

# set breaks
x_breaks = seq(200, 2800, 200)

# set labels
x_labels <- c("200", "400", "600", "800", "1,000",
              "1,200", "1,400", "1,600", "1,800", "2,000",
              "2,200", "2,400", "2,600", "2,800")

ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_histogram(fill = dark_pink,
                      col = white,
                      binwidth = 50,
                      center = 25) +
       scale_x_continuous(expand = c(0, 0),
                          breaks = x_breaks, 
                          labels = x_labels) + 
       scale_y_continuous(expand = c(0, 0)) +
       theme(panel.background = element_rect(fill=white),
             axis.ticks = element_blank(), 
             axis.title = element_blank(),
             axis.text.x = element_text(hjust = 0.1))  # change horizontal justification

# Change y-axis labels to percentages 

# set breaks
y_breaks = c(30, 60, 90, 120, 150, 181, 211)

# set labels
y_labels = c("1%", "2%", "3%", "4%", "5%", "6%", "7% of meals")

ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_histogram(fill = dark_pink,
                      col = white,
                      binwidth = 50,
                      center = 25) +
       scale_x_continuous(expand = c(0, 0),
                          breaks = x_breaks, 
                          labels = x_labels) + 
       scale_y_continuous(expand = c(0, 0),
                          breaks = y_breaks, # add new breakpoints
                          labels = y_labels) + # add new labels
       theme(panel.background = element_rect(fill=white),
             axis.ticks = element_blank(), 
             axis.title = element_blank(),
             axis.text.x = element_text(hjust = 0.1))

# adjust horizontal justification of y-axis labels:

ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_histogram(fill = dark_pink,
                      col = white,
                      binwidth = 50,
                      center = 25) +
       scale_x_continuous(expand = c(0, 0),
                          breaks = x_breaks, 
                          labels = x_labels) + 
       scale_y_continuous(expand = c(0, 0),
                          breaks = y_breaks, # add new breakpoints
                          labels = y_labels) + # add new labels
       theme(panel.background = element_rect(fill=white),
             axis.ticks = element_blank(), 
             axis.title = element_blank(),
             axis.text.x = element_text(hjust = 0.1),
             axis.text.y = element_text(hjust = 0))

# add horizontal gridlines

light_gray <- "#F0F0F0"

ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_hline(yintercept = y_breaks, # add horizontal lines at y_breaks
                  color = light_gray) + # set to light gray
       geom_histogram(fill = dark_pink,
                      color = white,
                      binwidth = 50,
                      center = 25) +
       scale_x_continuous(expand = c(0, 0),
                          breaks = x_breaks, 
                          labels = x_labels) + 
       scale_y_continuous(expand = c(0, 0),
                          breaks = y_breaks, 
                          labels = y_labels) + 
       theme(panel.background = element_rect(fill=white),
             axis.ticks = element_blank(), 
             axis.title = element_blank(),
             axis.text.x = element_text(hjust = 0.1),
             axis.text.y = element_text(hjust = 0))

# shift labels on top of gridlines 

ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_hline(yintercept = y_breaks, 
                  color = light_gray) + 
       geom_histogram(fill = dark_pink,
                      color = white,
                      binwidth = 50,
                      center = 25) +
       scale_x_continuous(expand = c(0, 0),
                          breaks = x_breaks, 
                          labels = x_labels) + 
       scale_y_continuous(expand = c(0, 0),
                          breaks = y_breaks, 
                          labels = y_labels) + 
       theme(panel.background = element_rect(fill=white),
             axis.ticks = element_blank(), 
             axis.title = element_blank(),
             axis.text.x = element_text(hjust = 0.1),
             axis.text.y = element_text(hjust = 0,
                                        vjust = -0.3, # shift labels up
                                        margin = margin(r = -53))) # pull all axis text into plot

# Adding text annotations

# colors
white <- "#FFFFFF"
dark_pink <- "#9E4B6C"
dark_gray <- "#333333"
light_gray <- "#F0F0F0"

# annotations
twoperc_text <- "About 2 percent of meals\nhad more than 2,000\ncalories."

# lineheight for annotations
lh = 1.1

ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_hline(yintercept = y_breaks, 
                  color = light_gray) + 
       geom_histogram(fill = dark_pink,
                      color = white,
                      binwidth = 50,
                      center = 25) +
       annotate(geom = "text", # add a text geom
                label = twoperc_text, # set label for geom
                x = 2015, # set horizontal position
                y = 45,  # set vertical position
                hjust = 0, # left-justify text
                lineheight = lh, # set lineheight to 1.1
                color = dark_gray) + # set color to dark gray
       scale_x_continuous(expand = c(0, 0),
                          breaks = x_breaks, 
                          labels = x_labels) + 
       scale_y_continuous(expand = c(0, 0),
                          breaks = y_breaks, 
                          labels = y_labels) + 
       theme(panel.background = element_rect(fill=white),
             axis.ticks = element_blank(), 
             axis.title = element_blank(),
             axis.text.x = element_text(hjust = 0.1),
             axis.text.y = element_text(hjust = 0,
                                        vjust = -0.3,
                                        margin = margin(r = -53))) 

# add remaining annotations

average_text <- "Many burritos and burrito\nbowls end up with 900 to\n1,000 calories."
guacbump_text <- "This small bump is\nmostly from sides of\nchips and guacamole."
rda_text <- "Recommended dietary allowance\n2,000 calories"
twoperc_text <- "About 2 percent of meals\nhad more than 2,000\ncalories."

ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_hline(yintercept = y_breaks, 
                  color = light_gray) + 
       geom_histogram(fill = dark_pink,
                      color = white,
                      binwidth = 50,
                      center = 25) +
       annotate(geom = "text", 
                label = average_text,
                x = 1050, 
                y = 225, 
                hjust = 0, 
                lineheight = lh, 
                color = dark_gray) + 
       annotate(geom = "text", 
                label = guacbump_text, 
                x = 1450,
                y = 120, 
                hjust = 0, 
                lineheight = lh, 
                color = dark_gray) + 
       annotate(geom = "text", 
                label = rda_text,
                size = 3, # set a smaller text size
                x = 2015,
                y = 218, 
                hjust = 0,
                lineheight = lh,
                color = dark_gray) +
       annotate(geom = "text",
                label = twoperc_text, 
                x = 2015, 
                y = 45, 
                hjust = 0, 
                lineheight = lh,
                color = dark_gray) +
       scale_x_continuous(expand = c(0, 0),
                          breaks = x_breaks, 
                          labels = x_labels) + 
       scale_y_continuous(expand = c(0, 0),
                          breaks = y_breaks, 
                          labels = y_labels) + 
       theme(panel.background = element_rect(fill=white),
             axis.ticks = element_blank(), 
             axis.title = element_blank(),
             axis.text.x = element_text(hjust = 0.1),
             axis.text.y = element_text(hjust = 0,
                                        vjust = -0.3,
                                        margin = margin(r = -53))) 

# Add vertical guideline at x = 2000

ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_hline(yintercept = y_breaks, 
                  color = light_gray) + 
       geom_histogram(fill = dark_pink,
                      color = white,
                      binwidth = 50,
                      center = 25) +
       geom_vline(xintercept = 2000, # add vertical line at x = 2000
                  color = dark_gray) + # set to dark gray
       annotate(geom = "text", 
                label = average_text,
                x = 1050, 
                y = 225, 
                hjust = 0, 
                lineheight = lh, 
                color = dark_gray) + 
       annotate(geom = "text", 
                label = guacbump_text, 
                x = 1450,
                y = 120, 
                hjust = 0, 
                lineheight = lh, 
                color = dark_gray) + 
       annotate(geom = "text", 
                label = rda_text,
                size = 3,
                x = 2015,
                y = 218, 
                hjust = 0,
                lineheight = lh,
                color = dark_gray) +
       annotate(geom = "text",
                label = twoperc_text, 
                x = 2015, 
                y = 45, 
                hjust = 0, 
                lineheight = lh,
                color = dark_gray) +
       scale_x_continuous(expand = c(0, 0),
                          breaks = x_breaks, 
                          labels = x_labels) + 
       scale_y_continuous(expand = c(0, 0),
                          breaks = y_breaks, 
                          labels = y_labels) + 
       theme(panel.background = element_rect(fill=white),
             axis.ticks = element_blank(), 
             axis.title = element_blank(),
             axis.text.x = element_text(hjust = 0.1),
             axis.text.y = element_text(hjust = 0,
                                        vjust = -0.3,
                                        margin = margin(r = -53)))

# add plot title
plot_title <- "At Chipotle, How Many Calories Do People Really Eat?"

ggplot(data=meals, 
       aes(x=total_calories)) +
       geom_hline(yintercept = y_breaks, 
                  color = light_gray) + 
       geom_histogram(fill = dark_pink,
                      color = white,
                      binwidth = 50,
                      center = 25) +
       geom_vline(xintercept = 2000,
                  color = dark_gray) +
       ggtitle(plot_title) + # add plot title
       annotate(geom = "text", 
                label = average_text,
                x = 1050, 
                y = 225, 
                hjust = 0, 
                lineheight = lh, 
                color = dark_gray) + 
       annotate(geom = "text", 
                label = guacbump_text, 
                x = 1450,
                y = 120, 
                hjust = 0, 
                lineheight = lh, 
                color = dark_gray) + 
       annotate(geom = "text", 
                label = rda_text,
                size = 3,
                x = 2015,
                y = 218, 
                hjust = 0,
                lineheight = lh,
                color = dark_gray) +
       annotate(geom = "text",
                label = twoperc_text, 
                x = 2015, 
                y = 45, 
                hjust = 0, 
                lineheight = lh,
                color = dark_gray) +
       scale_x_continuous(expand = c(0, 0),
                          breaks = x_breaks, 
                          labels = x_labels) + 
       scale_y_continuous(expand = c(0, 0),
                          breaks = y_breaks, 
                          labels = y_labels) + 
       theme(panel.background = element_rect(fill=white),
             plot.title = element_text(size = 26, # increase title font size
                                       margin = margin(b = 30)), # add bottom margin
             axis.ticks = element_blank(), 
             axis.title = element_blank(),
             axis.text.x = element_text(hjust = 0.1),
             axis.text.y = element_text(hjust = 0,
                                        vjust = -0.3,
                                        margin = margin(r = -53)))


####################################################################
# Final plotting code:

library(ggplot2) # load ggplot2 library
meals <- read.csv("meals.csv") # load meals data

# colors
white <- "#FFFFFF"
dark_pink <- "#9E4B6C"
dark_gray <- "#333333"
light_gray <- "#F0F0F0"

# axis breaks and labels
x_breaks = seq(200, 2800, 200)
x_labels <- c("200", "400", "600", "800", "1,000",
              "1,200", "1,400", "1,600", "1,800", "2,000",
              "2,200", "2,400", "2,600", "2,800")

y_breaks = c(30, 60, 90, 120, 150, 181, 211)
y_labels = c("1%", "2%", "3%", "4%", "5%", "6%", "7% of meals")

# lineheight for annotations
lh = 1.1

# annotation text
average_text <- "Many burritos and burrito\nbowls end up with 900 to\n1,000 calories."
guacbump_text <- "This small bump is\nmostly from sides of\nchips and guacamole."
rda_text <- "Recommended dietary allowance\n2,000 calories"
twoperc_text <- "About 2 percent of meals\nhad more than 2,000\ncalories."

# plot title
plot_title <- "At Chipotle, How Many Calories Do People Really Eat?"

ggplot(data=meals, # use meals dataframe 
       aes(x=total_calories)) + # map x to total_calories
  geom_hline(yintercept = y_breaks, # draw horizontal lines at y_breaks
             color = light_gray) + 
  geom_histogram(fill = dark_pink,
                 color = white,
                 binwidth = 50, # set binwidth to 50 calories wide
                 center = 25) + # center bins on multiples of 25
  geom_vline(xintercept = 2000, # draw vertical line at x = 2000
             color = dark_gray) +
  ggtitle(plot_title) + # add plot title
  annotate(geom = "text", # add average_text text annotation
           label = average_text,
           x = 1050, # set x-position
           y = 225,  # set y-position
           hjust = 0, # left-justify text
           lineheight = lh, # set smaller lineheight
           color = dark_gray) + 
  annotate(geom = "text", 
           label = guacbump_text, 
           x = 1450,
           y = 120, 
           hjust = 0, 
           lineheight = lh, 
           color = dark_gray) + 
  annotate(geom = "text", 
           label = rda_text,
           size = 3,
           x = 2015,
           y = 218, 
           hjust = 0,
           lineheight = lh,
           color = dark_gray) +
  annotate(geom = "text",
           label = twoperc_text, 
           x = 2015, 
           y = 45, 
           hjust = 0, 
           lineheight = lh,
           color = dark_gray) +
  scale_x_continuous(expand = c(0, 0), # reduce padding around axis
                     breaks = x_breaks, # set x-axis breaks
                     labels = x_labels) +  # set x-axis labels
  scale_y_continuous(expand = c(0, 0),
                     breaks = y_breaks, 
                     labels = y_labels) + 
  theme(panel.background = element_rect(fill=white), # set background to white
        plot.title = element_text(size = 26, # increase title font size
                                  margin = margin(b = 30)), # add bottom margin
        axis.ticks = element_blank(), # remove axis ticks
        axis.title = element_blank(), # remove axis titles
        axis.text.x = element_text(hjust = 0.1), # shift x-axis labels right
        axis.text.y = element_text(hjust = 0, # left-justify y-axis labels
                                   vjust = -0.3, # shift y-axis labels up
                                   margin = margin(r = -53))) # pull y-axis
                                                              # labels into
                                                              # plot with
                                                              # negative margin