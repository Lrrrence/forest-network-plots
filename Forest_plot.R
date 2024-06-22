library(ggplot2)
library(ggnewscale)
library(tidyverse)
library(grid)
library(rstudioapi) # get working dir

# set working directory
current_file <- rstudioapi::getActiveDocumentContext()$path
current_dir <- dirname(current_file)
setwd(current_dir)
cat("Current working directory set to:", getwd(), "\n")

# import data manually using "import dataset", name df, include header.
# with all files in the same folder, import all
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

# rename current dataset to df
df = `adverse events ADs`

#rename columns
df <- df %>% 
  rename(
    mean = logOR,
    lower = lwrCrI..2.5..,
    upper =  uppCrI..97.5..,
    #lower = X2.50.,
    #upper = X97.50.,
    #treatment = trt
  )

# reverses the order
df <- arrange(df, desc(row_number()))
  
# change to continuous scale  
y_position = 1:nrow(df)
df = cbind(df, y_position = y_position)

#generate data for background colour strips
strip_data <- df %>%
  select(treatment, y_position) %>%
  mutate(xmin = y_position - 0.5,
         xmax = y_position + 0.5,
         ymin = min(df$lower)-0.1,
         ymax = max(df$upper)+0.1,
         fill = c(rep(c("a", "b"), length.out=nrow(.)))
  ) %>%
  pivot_longer(cols=c(xmin, xmax), names_to="min_max", values_to="x")

# plot
p <- ggplot(data=df, aes(x= y_position, y=mean, ymin=lower, ymax=upper)) +
  geom_ribbon(data=strip_data, aes(x=x, ymin=ymin, ymax=ymax, fill=fill, group=treatment),inherit.aes=FALSE, alpha=0.4) +
  geom_hline(yintercept=1, linetype="dashed")+ # change value for log odds or relative effect
  scale_fill_manual(values = c("white","grey"))+ #set colours for background strips
  new_scale_color() +
  geom_pointrange(aes(colour = class, size=n), shape=15, linewidth = 1) + 
  #scale_color_brewer(palette = 'Dark2') +
  xlab("Treatment") + ylab("Relative effect") +
  theme_light()+  # use a white background 
  theme(text=element_text(size=14),
        plot.title = element_text(face="bold", hjust = 0.5, size = 14),
        axis.text.y = element_text(face=c("plain","bold")),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(t = 2,  # Top margin
                             r = 2,  # Right margin
                             b = 2,  # Bottom margin
                             l = 2))+ # Left margin)
  scale_x_continuous(breaks = df$y_position,
                    labels = df$treatment,
                    expand = expansion(mult = c(0.13,0)))+ # expand bottom of plot for text
  scale_y_continuous(expand=c(0,0))+
  scale_size_continuous(range = c(0.5, 1.5)) + # scale size of points
  guides(fill = "none")+
  labs(size="Total sample size", colour="Treatment class")+
  ggtitle("Sleep - Post intervention")+
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="first",
                                                 length=unit(3,"mm")), 
                                     gp=gpar(col="black", lwd=2)), xmin = 0, xmax = 0, ymin = min(df$lower), ymax = -0.8) +
  annotation_custom(grob = grid::textGrob(label = "Lower is better", just="centre", gp=gpar(col="black", cex=1.1)),
                    xmin = 0, xmax = 0, ymin = (min(df$lower)+max(df$upper))/2, ymax = (min(df$lower)+max(df$upper))/2) +
  coord_cartesian(clip="off") +
  coord_flip() # flip coordinates (puts labels on y axis)
p

#########################

# adjust scale_x_continuous expand value (e.g 0.01) to control spacing at bottom of plot for arrow

# height 15ish for small plots, 38 for large plots
ggsave(plot = p, width = 38, height = 15, units = "cm", filename = "sleep_post-int.pdf")

#########################

# adverse: lower better
# moderate pain: higher better
# mood: lower better
# pain intensity: lower better
# PGIC cont: lower better
# PGIC much/very much improved: higher better
# physical function: lower better
# quality of life: lower better
# serious adverse: lower better
# sleep: lower better
# substantial pain: higher better
# withdrawal: lower better

###########################

# lower better:
# ymin = min(df$lower), ymax = -1)

# higher better:
# ymin = 1, ymax = max(df$upper)

  
