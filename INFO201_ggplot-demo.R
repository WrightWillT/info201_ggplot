########################################################################################################################
# PURPOSE AND NOTES
########################################################################################################################

# Purpose: Provide an overview of ggplot2's capabilities and give tips on how to handle common plotting scenarios.
# Notes: this was prepared for the 2020-05-11 lecture for UW's INFO 201 by Will Wright

########################################################################################################################
# SETUP
########################################################################################################################

library(tidyverse)
library(scales)
library(gridExtra)
library(ggthemes)

########################################################################################################################
# GGPLOT BASICS
########################################################################################################################

# First, let's review the mtcars datset
str(mtcars)
View(mtcars)

# Simple scatter plot
g <- ggplot(data = mtcars, mapping = aes(x = mpg, y = hp))
g + geom_point() # <- Notice that this function inherits the data and aesthetic of g so no arguments are required

?geom_point # check out the documentation

# We can, however, change the data or add other aesthetics that will only be applied to the scatterplot
g + geom_point(aes(alpha = 0.7), # useful to turn this down if you have a lot of points in the same area
               color = "red",
               shape = 3, # I've almost never used shape and advise against it, in general
               size = 10, # relative size
               stroke = 3) # boldness

# With ggplot, something to remember is that you can continue to add more elements, including the same elements
g + geom_point(aes(alpha = 0.7), # useful to turn this down if you have a lot of points in the same area
               color = "red",
               shape = 3, # I've almost never used shape and advise against it, in general
               size = 10, # relative size
               stroke = 1) + # boldness
  geom_point(aes(alpha = 0.7), # useful to turn this down if you have a lot of points in the same area
             color = "black",
             shape = 1, # I've almost never used shape and advise against it, in general
             size = 10, # relative size
             stroke = 2) 


# Seems like we'll want some labels:
?labs # check out documentation
g + geom_point() +
  labs(title = "Title of the plot",
       subtitle = "Subtitle",
       caption = "An insightful caption",
       x = "x-axis title",
       y = "y-axis title")

# Notice that the x-axis starts at 10.  What if we wanted a different set of x-limits? Depends on continuous/discrete
?scale_x_continuous
?scale_x_discrete
g + geom_point() +
  scale_x_continuous(limits = c(0, max(mtcars$mpg)))

# One of the most common modifications for y-axis labels is to convert to percents.
# Another common activity is converting data to a percentile via the empirical cumulative distribution function
wt_ecdf <- ecdf(mtcars$wt)
mtcars$wt_pct <- wt_ecdf(mtcars$wt)

# Before
p <- ggplot(mtcars, aes(x = mpg, y = wt_pct))
p + geom_point()

# After
p + geom_point() +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) 

# Often times, you'll want to put labels on the data, but we'll need to transform the data into strings with '%'
p + geom_point() +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = paste0(round(wt_pct,2)*100,"%"), x = mpg, vjust = -1), size = 4) 


# Yeah, but this chart looks so basic... how do we make it prettier? We can use a theme!

p <- p + geom_point() +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "MPG", y = "Weight Percentile")

# Black and white theme
p + theme_bw() # this is the best theme, imo

# Can we see all the theme options at once? Yes, with grid.arrange()
a <- p + theme_base() + labs(title = "Base")
b <- p + theme_bw() + labs(title = "Black and White")
c <- p + theme_calc() + labs(title = "Calc")
d <- p + theme_classic() + labs(title = "Classic")
e <- p + theme_clean() + labs(title = "Clean")
f <- p + theme_dark() + labs(title = "Dark")
g <- p + theme_economist() + labs(title = "Economist")
h <- p + theme_economist_white() + labs(title = "Economist White")
i <- p + theme_excel() + labs(title = "Excel")
j <- p + theme_fivethirtyeight() + labs(title = "Five Thirty Eight")
k <- p + theme_gdocs() + labs(title = "Google Docs")
l <- p + theme_minimal() + labs(title = "Minimal")
m <- p + theme_solarized() + labs(title = "Solarized")
n <- p + theme_tufte() + labs(title = "Tufte")
o <- p + theme_wsj() + labs(title = "Wall Street Journal")
q <- p + theme_void() + labs(title = "Void")

grid.arrange(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, q,
             ncol = 4,
             nrow = 4)

# Howbout if we want to show multiple plots--one for each facet? Then we'd use facet_grid or facet_wrap
# facet_grid produces a 2d grid of panels defined by the variables which form the rows and columns
# facet_wrap  produces a 1d ribbon which reads left to right, top to bottom

g <- ggplot(mtcars, aes(x = mpg, y = hp))
g + geom_point() +
  facet_grid(~cyl) + 
  theme_bw()

# What if we want to show some sort of trend?
g + geom_point() +
  facet_grid(~cyl) + 
  theme_bw() +
  geom_smooth() # loess method is default

g + geom_point() +
  facet_grid(~cyl) + 
  theme_bw() +
  geom_smooth(method = "lm", # linear model
              se = FALSE) # remove standard error

# What about splitting a factor without using separate charts?

g <- ggplot(mtcars, aes(x = mpg, y = hp, color = cyl))
g + geom_point()+
  theme_bw()
str(mtcars) # remember that ggplot will interpret numeric data as continuous and default to a color scale

# if we want to see discrete colors, we'll need to convert to a factor
g <- ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl)))
g + geom_point() +
  theme_bw()




########################################################################################################################
# GGPLOT ADVANCED EXAMPLES
########################################################################################################################

# You can (and should) put your neato plots in functions for reusability
# Personally, I source all my helper functions at the top of my cleaning/get set up scripts
norm_distributer <- function(value_input, title_input, label_input, x_input){
  # create normal curve
  norm_PDF <- data.frame(x_input = seq(0,100, by = 0.01), density = dnorm(seq(0,100, by = 0.01), 50, 15))
  
  # create shaded region
  shade <- rbind(c(0,0), 
                 subset(norm_PDF, x_input>=0 & x_input<value_input),
                 c(value_input,0))
  
  # plot
  g <- ggplot(norm_PDF, aes(x = x_input, y = density))
  g + geom_line() +
    geom_polygon(data = shade, aes(x = x_input, y = density, fill = "red", alpha = 0.2)) +
    labs(title = title_input,
         x = x_input,
         y = "") +
    theme_bw() +
    scale_y_continuous(breaks = NULL) +
    geom_vline(col = "red", xintercept = value_input, linetype = "dashed") +
    annotate("label", x = value_input, y = 0.023, label = label_input) +
    guides(fill = FALSE, alpha = FALSE) +
    scale_x_continuous(breaks = c(seq(0,100,10)))
}

norm_distributer(23,
                 "Facility Risk Level Distribution", 
                 "Your facility is in the \n 23rd percentile",
                 "Risk Level")

# If you have a client or employer with specific colors, use them!!

# wec colors
colors <- c("#6B3248", #Grape
            "#A05974", #Light Plum
            "#FFB636", #Mango
            "#37A8B3", #Greeny Blue
            "#6CC8D1", #Seafoam Blue
            "#CC9128", #Brownish Orange
            "#4A5459", #River Bed Gray
            "#8E99A0", #Regent Gray
            "#EBF0F2", #Mystic Gray
            "#F14956", # Watermelon - Error
            "#4DAF50") # Dark Bastel Green - Success


distribution_visualizer_wecTheme_advanced <- function(
  data, 
  title = "Histogram and Density", 
  x = "Values", 
  binwidthInput = (max(data)-min(data))/15){
  binwidthInput <- binwidthInput
  binCounts <- .bincode(data, seq(0,max(data), binwidthInput))
  xbar <- round(mean(data),1)
  sd <- round(sd(data),1)
  g <- ggplot(data.frame(data), aes(data)) +
    geom_histogram(fill = colors[1],
                   color = colors[2],
                   binwidth = binwidthInput) +
    geom_vline(aes(xintercept = mean(data)),
               color = colors[3],
               linetype = "dashed",
               size = 0.7) +
    geom_density(aes(y = binwidthInput * ..count..), 
                 alpha = 0.2, 
                 fill = colors[2],
                 color = colors[1]) +
    labs(title = title,
         x = x,
         y = "Frequency") +
    annotate("text", 
             x = mean(data)*1.25, 
             y = max(binCounts, na.rm = TRUE)*0.75, 
             label = paste0("Mean = ", xbar), # "x\u0305 = " for Windows
             size = 3) +
    theme_bw()  
  
  
  p <- ggplot(data.frame(data), aes(sample = data)) +
    stat_qq_band(color = colors[1], fill = colors[2]) +
    stat_qq_line(color = colors[3], linetype = "dashed", size = 0.7) +
    stat_qq_point(size = 0.8, alpha = 0.3) +
    labs(title = "Q-Q Plot",
         x = "Theoretical Values",
         y = "Sample Values") +
    theme_bw()
  
  grid.arrange(g, p, ncol = 2)
}

distribution_visualizer_wecTheme_advanced(mtcars$mpg, 
                                          "MPG Histogram and Density",
                                          "MPG")

# I use this diagnostic tool all the time

distribution_visualizer_wecTheme_basic <- function(
  data, 
  title = "Histogram and Density", 
  x = "Values", 
  binwidthInput = (max(data)-min(data))/15){
  binwidthInput <- binwidthInput
  binCounts <- .bincode(data, seq(0,max(data), binwidthInput))
  xbar <- round(mean(data),1)
  xmedian <- round(median(data),1)
  sd <- round(sd(data),1)
  g <- ggplot(data.frame(data), aes(data)) +
    geom_histogram(fill = colors[2],
                   color = colors[1],
                   binwidth = binwidthInput) +
    geom_vline(aes(xintercept = mean(data)),
               color = colors[3],
               linetype = "dashed",
               size = 0.7) +
    geom_vline(aes(xintercept = median(data)),
               color = colors[5],
               linetype = "dashed",
               size = 0.7) +
    geom_density(aes(y = binwidthInput * ..count..), 
                 alpha = 0.2, 
                 fill = colors[1],
                 color = colors[2]) +
    labs(title = title,
         x = x,
         y = "Frequency") +
    annotate("text", 
             x = mean(data)*1.2, 
             y = max(binCounts, na.rm = TRUE)*0.75, 
             label = paste0("Mean = ", xbar), # "x\u0305 = " for Windows
             size = 5,
             family = "Avenir",
             col = colors[3]) +
    annotate("text", 
             x = median(data)*1.2, 
             y = max(binCounts, na.rm = TRUE)*0.55, 
             label = paste0("Median = ", xmedian), # "x\u0305 = " for Windows
             size = 5,
             family = "Avenir",
             col = colors[5]) +
    theme_bw() +
    theme(plot.title = element_text(color = colors[7], size = 18, family = "Avenir"),
          axis.text.x = element_text(size = 14, color = colors[7], family = "Avenir"),
          axis.text.y = element_text(size = 14, color = colors[7], family = "Avenir"),
          axis.title.x = element_text(size = 14, color = colors[7], family = "Avenir"),
          axis.title.y = element_text(size = 14, color = colors[7], family = "Avenir"),
          axis.line = element_line(colour = colors[1]))
  print(g)
}

distribution_visualizer_wecTheme_basic(mtcars$mpg, 
                                          "MPG Histogram and Density",
                                          "MPG")


# Crosstabs
# This is some really early work in my career and it is terrible code, but I wanted to give an example of how
  # involved this can get

crosstabber <- function(var1_input, var2_input){
  # generate cross tabs in both margins and prop format
  xtabs <- table(surveyData[,var1_input], surveyData[,var2_input])
  xtabs_prop <- prop.table(xtabs, 1)
  pval <- summary(xtabs)$p.value
  sigSymbol <- symnum(pval, corr = FALSE,
                      cutpoints = c(0,  .001,.01,.05, .1, 1),
                      symbols = c("***","**","*","."," "))
  
  xtabs_df <- as.data.frame(xtabs)
  
  # generate margins and proportions for the xvar
  margins <- as.data.frame(margin.table(xtabs,1))
  props <- as.data.frame(prop.table(xtabs,1))
  
  # convert labels to view-friendly format
  suppressMessages(library(Hmisc)) #silently load package that does the capitalization
  xlab <- capitalize(questionLookup$questionCode[var1_input])
  legendLab <- capitalize(questionLookup$questionCode[var2_input])
  titleVolume <- paste0(capitalize(questionLookup$questionCode[var1_input]),
                        " cut by ",
                        capitalize(questionLookup$questionCode[var2_input]), " - Response Volumes",
                        sigSymbol)
  titleProp <- paste0(capitalize(questionLookup$questionCode[var1_input]),
                      " cut by ",
                      capitalize(questionLookup$questionCode[var2_input]), " - Factor Proportions",
                      sigSymbol)
  detach("package:Hmisc", unload = TRUE) # always unload this package since it interferes with other packages
  # convert answers to view-friendly format via lookup based on questionCode and answerCode
  
  #----PLOTS----
  # standard bar chart with the fill being another factor
  p1 <- ggplot(xtabs_df) +
    geom_bar(stat = "identity", aes(x = xtabs_df[,1], fill = xtabs_df[,2], y = Freq)) +
    labs(title = titleVolume, x = xlab, y = "Responses") +
    scale_fill_discrete(name = legendLab) + 
    theme_bw() + 
    geom_text(data = margins, 
              aes(label = paste0(Freq," (",round(Freq/nrow(surveyData)*100,1),"%)"),
                  x = Var1,
                  y = Freq), 
              vjust = -0.5) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    if(nrow(subset(xtabs_df,Freq>15))>0) { # setting these as conditional because it'll fail if the subsetting below have 0 rows
      geom_text(data = subset(xtabs_df,Freq>15),
                aes(label = paste0(Freq," (",round(Freq/sum(Freq)*100,1),"%)"), 
                    x = Var1, 
                    y = Freq,
                    group = Var2),
                position = position_stack(vjust = 0.5),
                color = "white")
    }
  
  
  # 100% stacked bar chart
  p2 <- ggplot(xtabs_df) +
    geom_bar(stat = "identity", position = "fill", aes(x = xtabs_df[,1], fill = xtabs_df[,2], y = Freq)) +
    labs(title = titleProp, x = xlab, y = "Within-Factor Composition") +
    theme_bw() + 
    scale_fill_discrete(name = legendLab) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    scale_y_continuous(labels=scales::percent) +
    geom_text(data = subset(props,Freq>0.02),
              aes(label = paste0(round(Freq*100,1),"%"),
                  x = Var1,
                  y = Freq,
                  group = Var2),
              color = "white",
              position = position_stack(vjust = 0.5))
  
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g<-gtable:::rbind_gtable(g1, g2, "first")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels] <- unit(c(1,1), "null")
  grid.newpage()
  grid.draw(g)
}


# function to compare one factor by all the others and create a single .pdf
# this version for v04 allows the user to specify a different number for the file name
singleFactor_pdfGenerator<-function(numericFactor_input, fileName_input = numericFactor_input){
  name<-paste0(sprintf("%03d",fileName_input),"_",colnames(surveyData)[numericFactor_input])
  num.plots<-dim(surveyData)[2]
  my.plots<-vector(num.plots, mode='list')
  for(i in 1:dim(surveyData)[2]) {
    crosstabber(numericFactor_input,i)
    my.plots[[i]]<-recordPlot()
  }
  
  graphics.off()
  
  pdf(paste0(name,".pdf"), onefile=TRUE, width = 7, height = 11)
  for (my.plot in my.plots) {
    replayPlot(my.plot)
  }
  graphics.off()
}



