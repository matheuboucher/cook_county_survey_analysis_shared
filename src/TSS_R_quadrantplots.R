## THIS FILE GENERATES QUADRANT PLOTS OF TENANT SATISFACTION FOR USING THE ##
## VARIOUS FACILITIES TSS_R_FUNCTIONS FILE ##

#Source the functions file
source("./TSS_R_functions.R")

# QUADRANT ANALYSIS --------------------------------------------------------
#set up plot
ceiling_any = function(x, accuracy){ceiling(x/ accuracy) * accuracy}

f_quad_plot = function(pchange_table) {
  quad_table = pchange_table %>% 
    mutate(., comp = if_else(change_from_2021<0, "worse", "better", missing=NULL)) %>% 
    #group_by(., comp) %>% 
    left_join(., resp_by_facility(current_survey), by = "facility") %>% na.omit(.)
  
  y_max = max(ceiling_any(abs(quad_table$'change_from_2021'), 0.5), na.rm = TRUE)
  
  Quad22Better = quad_table %>% filter(., comp=="better") %>% 
    select(facility, current, change_from_2021, n)
  Quad22Worse = quad_table %>% filter(., comp=="worse") %>% 
    select(facility, current, change_from_2021, n)
  
  Quad22BetterLarge = Quad22Better %>% 
    filter(., n>=20)
  Quad22BetterSmall = Quad22Better %>% 
    filter(., n<20)
  Quad22WorseLarge = Quad22Worse %>% 
    filter(., n>=20)
  Quad22WorseSmall = Quad22Worse %>% 
    filter(., n<20)
  
  options(ggrepel.max.overlaps = Inf)
  ggplot()+
    geom_point(data=Quad22Better, aes(current, change_from_2021), size= 0.75)+ #Input the data--improve over last year.
    geom_point(data=Quad22Worse,
               aes(current, change_from_2021), color='#FF0000', size = 0.75)+ #Decline from last year.
    geom_label_repel(data=Quad22BetterLarge, aes(current, change_from_2021),
                     label=Quad22BetterLarge$facility, size = 2, box.padding = 0.5,
                     nudge_y = 0.05)+
    geom_label_repel(data=Quad22WorseLarge, aes(current, change_from_2021),
                     label=Quad22WorseLarge$facility, color='#FF0000',
                     nudge_y = -0.15, size = 2, box.padding=0.5)+ #Hexadecimal color codes. 000000 is black, the default.FFFFFF is white.
    geom_label_repel(data=Quad22BetterSmall, aes(current, change_from_2021), label=Quad22BetterSmall$facility, alpha=0.6,
                     size = 2, nudge_x = 0.1, min.segment.length = 0, box.padding=0.4, point.padding = 0.2)+ #Alpha is used to lower the opacity.
    geom_label_repel(data=Quad22WorseSmall, aes(current, change_from_2021), label=Quad22WorseSmall$facility, color='#FF0000', alpha=0.6,
                     size = 2, nudge_y = -0.05, nudge_x = -0.05)+
    ggplot2::annotate('text',x=4.75, y=y_max-0.05, label="Good in 2022", size=3)+
    ggplot2::annotate('text',x=1.25, y=-y_max+0.05, label="Bad in 2022", size=3)+
    ggplot2::annotate('text',x=1.25, y=y_max-0.05, label="Bad in 2022", size=3)+
    ggplot2::annotate('text',x=4.75, y=-y_max+0.05, label="Good in 2022", size=3)+
    ggplot2::annotate('text',x=4.75, y=y_max-0.09, label="Better than 2021", size=3)+
    ggplot2::annotate('text',x=1.25, y=y_max-0.09, label="Better than 2021", size=3)+
    ggplot2::annotate('text',x=1.25, y=-y_max+0.09, label="Worse than 2021", size=3)+
    ggplot2::annotate('text',x=4.75, y=-y_max+0.09, label="Worse than 2021", size=3)+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=3)+
    xlim(1,5)+
    ylim(-y_max, y_max)+
    xlab("Satisfaction in 2022")+
    ylab("Difference")+
    theme_bw()
  ggsave("../analysis/quadrant_plots/f_quad_plot.jpeg", device = "jpeg")
}

#Question quadrant analysis
q_quad_plot = function(pchange_table) {
  q_quad_table = pchange_table %>% 
    mutate(., comp = if_else(change_from_2021<0, "worse", "better", missing=NULL))
  
  x = q_quad_table$'current'
  y = q_quad_table$'change_from_2021'
  y_max = max(ceiling_any(abs(y), 0.5), na.rm = TRUE)
  
  qQuad22Better = q_quad_table %>% 
    filter(., comp=="better" & current>=3) %>% 
    select(question, current, change_from_2021)
  qQuad22Worse = q_quad_table %>% 
    filter(., comp=="worse" | current<3) %>% 
    select(question, current, change_from_2021)
  
  ggplot()+
    geom_point(data=qQuad22Better, aes(current, change_from_2021), size = 0.8)+ #Input the data--improve over last year.
    geom_point(data=qQuad22Worse, aes(current, change_from_2021), color='#FF0000', size = 0.8)+ #Decline from last year.
    geom_label_repel(data=qQuad22Worse, aes(current, change_from_2021),
                     label=qQuad22Worse$question, color='#FF0000',
                     nudge_y = -0.15, size = 2, box.padding=0.5)+
    ggplot2::annotate('text',x=4.75, y=y_max-0.05, label="Good in 2022", size=3)+
    ggplot2::annotate('text',x=1.25, y=-y_max+0.05, label="Bad in 2022", size=3)+
    ggplot2::annotate('text',x=1.25, y=y_max-0.05, label="Bad in 2022", size=3)+
    ggplot2::annotate('text',x=4.75, y=-y_max+0.05, label="Good in 2022", size=3)+
    ggplot2::annotate('text',x=4.75, y=y_max-0.09, label="Better than 2021", size=3)+
    ggplot2::annotate('text',x=1.25, y=y_max-0.09, label="Better than 2021", size=3)+
    ggplot2::annotate('text',x=1.25, y=-y_max+0.09, label="Worse than 2021", size=3)+
    ggplot2::annotate('text',x=4.75, y=-y_max+0.09, label="Worse than 2021", size=3)+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=3)+
    xlim(1,5)+
    ylim(-y_max, y_max)+
    xlab("Satisfaction in 2022")+
    ylab("Difference")+
    theme_bw()
  ggsave("../analysis/quadrant_plots/q_quad_plot.jpeg", device = "jpeg")
}

# Generate Quadrant Plots -------------------------------------------------

#facility quadrant plot
f_quad_plot(mean_by_f)
#question quadrant plot
q_quad_plot(mean_by_q)
