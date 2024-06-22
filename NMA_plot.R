library(multinma)
library(ggraph)
library(data.table)
library(ggrepel)
library(tidyverse)
library(tidygraph)
options(mc.cores = parallel::detectCores())

setwd("C://Users//yulel//Documents//R//NMA//Network plots")

# use "import dataset", rename to "df". 

# import data manually using "import dataset", name df, include header.
# with all files in the same folder, import all
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

# rename current dataset to df
df = withdrawal_updated

# process df with multinma
db_net <- set_agd_arm(df, 
                      study = study,
                      trt = trt,
                      r = r, # change to r where necessary
                      n = n, 
                      trt_class = class,
                      trt_ref = "placebo") # have to set to placebo for rearrangement of dataframe to work.

# plot with multinma for comparison
plot(db_net, weight_nodes = TRUE, weight_edges = TRUE, show_trt_class = TRUE, nudge = 0.02) +
  ggplot2::theme(legend.position = "bottom", 
                 legend.box = "vertical")

#############################

# convert from multinma format to ggraph
new_db_net = tidygraph::as_tbl_graph(db_net) 

# colour by class for each node
class_abc <- df %>%
  distinct(trt, .keep_all = TRUE) %>% # find all distinct treatments
  arrange(trt) # order treatments alphabetically
placebo1 = class_abc[class_abc$trt %in% c('placebo'),] # find placebo
class_abc = class_abc[!grepl("placebo", class_abc$trt),] # remove placebo from original data frame
class_abc = rbind(placebo1, class_abc) # add placebo to top

# node size by number of samples (n)
data_group <- df %>%                   
  group_by(trt) %>%
  dplyr::summarize(gr_sum = sum(n)) %>% 
  as.data.frame()
placebo2 = data_group[data_group$trt %in% c('placebo'),] # find placebo
data_group = data_group[!grepl("placebo", data_group$trt),] # remove placebo from original data frame
data_group = rbind(placebo2, data_group) # add placebo to top

#get connection list
tofrom = activate(new_db_net,edges) %>%
        as_tibble()

########################

# pre-plot to initialise p
p <-  ggraph(new_db_net, layout="star")+
      geom_edge_link(aes(edge_width = tofrom$.nstudy),color = "grey66") + # fixed colour
      geom_node_point(aes(size = data_group$gr_sum,
                      fill = class_abc$class),
                  colour = "black",
                  shape = 21)

pm = 1 #plot margin size

# full plot
# change layout to "star" to centre placebo
p <-  ggraph(new_db_net, layout="star")+
      geom_edge_link(aes(edge_width = tofrom$.nstudy),color = "grey66") + # fixed colour
      geom_node_point(aes(size = data_group$gr_sum,
                      fill = class_abc$class),
                      colour = "black",
                      shape = 21) + # size by total number of samples, colour by class
      geom_label_repel(aes(p$data$x,p$data$y,label = db_net$treatments,fill = factor(class_abc$class)),
                   label.padding = unit(0.2, "lines"), 
                   #nudge_x = p$df$x * .15, # shift from point x
                   #nudge_y = p$df$y * .15, # shift from point y
                   size = 4, #text size of labels 
                   force = 10,
                   box.padding = 0.5, # padding around the text label
                   max.iter = 1e7,
                   max.overlaps = Inf)+
      scale_size_continuous(range = c(2, 25)) + #scale size of nodes
      scale_fill_brewer(palette = "Set3") +
      theme_void()+
      theme(legend.position = "bottom", 
                 legend.box = "vertical",
                 plot.margin = margin(pm, pm, pm, pm, "cm"),
                 text=element_text(size=12),
                plot.title = element_text(face="bold", hjust = 0.5, size = 14),)+
      guides(fill = guide_legend(override.aes = aes(label = "")))+
      ggtitle("Withdrawal")+
      labs(size="Total sample size", edge_width="Number of studies", fill="Treatment class")
p

setwd("C://Users//yulel//Documents//R//NMA//Network plots//Plots")

ggsave(plot = p, width = 38, height = 38, units = "cm", filename = "withdrawal.pdf")

# tweak force, box.padding, and width/height (25 small, 38 big) as the study size changes.

