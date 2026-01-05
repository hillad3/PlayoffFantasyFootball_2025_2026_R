library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)

rm(list = ls())
gc()

playoff_standings <- tribble(
  ~team, ~conf, ~conf_rank, ~x, ~y,
  "DEN", "AFC", 1,          0,  10,
  "NE",  "AFC", 2,          0,  2,
  "JAX", "AFC", 3,          0,  5,
  "PIT", "AFC", 4,          0,  8,
  "HOU", "AFC", 5,          0,  7,
  "BUF", "AFC", 6,          0,  4,
  "LAC", "AFC", 7,          0,  1,
  "SEA", "NFC", 1,          12, 10,
  "CHI", "NFC", 2,          12, 2,
  "PHI", "NFC", 3,          12, 5,
  "CAR", "NFC", 4,          12, 8,
  "LA",  "NFC", 5,          12, 7,
  "SF",  "NFC", 6,          12, 4,
  "GB",  "NFC", 7,          12, 1
) |>
  as.data.table()

if(length(playoff_standings[conf=="AFC"]$team)!=7L){
  stop("There is an issue with the number of AFC teams")
}
if(!all(playoff_standings[conf=="AFC"]$conf_rank %in% 1:7)){
  stop("The ranking of AFC teams is incomplete")
}
if(length(playoff_standings[conf=="NFC"]$team)!=7L){
  stop("There is an issue with the number of NFC teams")
}
if(!all(playoff_standings[conf=="NFC"]$conf_rank %in% 1:7)){
  stop("The ranking of NFC teams is incomplete")
}


premake_bracket <- function(ps = playoff_standings){
  x1_len = 1
  y1_len = 1

  ps |> ggplot(aes(x, y, label = paste0(conf_rank,": ",team))) +
    scale_x_continuous(limits = c(-1, 13)) +
    scale_y_continuous(limits = c(-1, 14)) +

    # separate AFC and NFC sections
    # annotate("rect", xmin = -1, xmax = 6, ymin = -1, ymax = 14, alpha = .2, fill = "red") +
    # annotate("rect", xmin = 6, xmax = 13, ymin = -1, ymax = 14, alpha = .2, fill = "blue") +

    # AFC and NFC headers
    annotate("text",
             x = ps[conf=="AFC" & conf_rank==1L]$x+3,
             y = ps[conf=="AFC" & conf_rank==1L]$y+y1_len*2,
             label = "AFC") +
    annotate("text",
             x = ps[conf=="NFC" & conf_rank==1L]$x-3,
             y = ps[conf=="NFC" & conf_rank==1L]$y+y1_len*2,
             label = "NFC") +

    # Week subheaders
    annotate("text",
             x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*0,
             y = ps[conf=="AFC" & conf_rank==1L]$y+y1_len,
             label = "Week 1") +
    annotate("text",
             x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*2,
             y = ps[conf=="AFC" & conf_rank==1L]$y+y1_len,
             label = "Week 2") +
    annotate("text",
             x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4,
             y = ps[conf=="AFC" & conf_rank==1L]$y+y1_len,
             label = "Week 3") +
    annotate("text",
             x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*6,
             y = ps[conf=="AFC" & conf_rank==1L]$y+y1_len,
             label = "Week 4") +
    annotate("text",
             x = ps[conf=="NFC" & conf_rank==1L]$x+x1_len*0,
             y = ps[conf=="NFC" & conf_rank==1L]$y+y1_len,
             label = "Week 1") +
    annotate("text",
             x = ps[conf=="NFC" & conf_rank==1L]$x+x1_len*-2,
             y = ps[conf=="NFC" & conf_rank==1L]$y+y1_len,
             label = "Week 2") +
    annotate("text",
             x = ps[conf=="NFC" & conf_rank==1L]$x+x1_len*-4,
             y = ps[conf=="NFC" & conf_rank==1L]$y+y1_len,
             label = "Week 3") +

    # week 1, byes
    annotate("text",
             x = ps[conf=="AFC" & conf_rank==1L]$x-x1_len,
             y = ps[conf=="AFC" & conf_rank==1L]$y,
             label = "Bye") +
    annotate("text",
             x = ps[conf=="NFC" & conf_rank==1L]$x+x1_len,
             y = ps[conf=="NFC" & conf_rank==1L]$y,
             label = "Bye") +

    # week 1, rank 1 segment; no vertical connector because of the bye
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==1L]$x,
             xend = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*2,
             y = ps[conf=="AFC" & conf_rank==1L]$y,
             yend = ps[conf=="AFC" & conf_rank==1L]$y) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==1L]$x,
             xend = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*2,
             y = ps[conf=="NFC" & conf_rank==1L]$y,
             yend = ps[conf=="NFC" & conf_rank==1L]$y) +

    # AFC week 1, rank 2 and 7 bracket
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==2L]$x,
             xend = ps[conf=="AFC" & conf_rank==2L]$x+x1_len,
             y = ps[conf=="AFC" & conf_rank==2L]$y,
             yend = ps[conf=="AFC" & conf_rank==2L]$y) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==7L]$x,
             xend = ps[conf=="AFC" & conf_rank==7L]$x+x1_len,
             y = ps[conf=="AFC" & conf_rank==7L]$y,
             yend = ps[conf=="AFC" & conf_rank==7L]$y) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==2L]$x+x1_len,
             xend = ps[conf=="AFC" & conf_rank==2L]$x+x1_len,
             y = ps[conf=="AFC" & conf_rank==2L]$y,
             yend = ps[conf=="AFC" & conf_rank==2L]$y-y1_len) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==2L]$x+x1_len,
             xend = ps[conf=="AFC" & conf_rank==2L]$x+x1_len*2,
             y = ps[conf=="AFC" & conf_rank==2L]$y-y1_len*0.5,
             yend = ps[conf=="AFC" & conf_rank==2L]$y-y1_len*0.5) +
    # annotate("text",
    #          x = ps[conf=="AFC" & conf_rank==2L]$x+x1_len*1.5,
    #          y = ps[conf=="AFC" & conf_rank==2L]$y-y1_len*0,
    #          label = "BUF") +

    # NFC week 1, rank 2 and 7 bracket
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==2L]$x,
             xend = ps[conf=="NFC" & conf_rank==2L]$x-x1_len,
             y = ps[conf=="NFC" & conf_rank==2L]$y,
             yend = ps[conf=="NFC" & conf_rank==2L]$y) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==7L]$x,
             xend = ps[conf=="NFC" & conf_rank==7L]$x-x1_len,
             y = ps[conf=="NFC" & conf_rank==7L]$y,
             yend = ps[conf=="NFC" & conf_rank==7L]$y) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==2L]$x-x1_len,
             xend = ps[conf=="NFC" & conf_rank==2L]$x-x1_len,
             y = ps[conf=="NFC" & conf_rank==2L]$y,
             yend = ps[conf=="NFC" & conf_rank==2L]$y-y1_len) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==2L]$x-x1_len,
             xend = ps[conf=="NFC" & conf_rank==2L]$x-x1_len*2,
             y = ps[conf=="NFC" & conf_rank==2L]$y-y1_len*0.5,
             yend = ps[conf=="NFC" & conf_rank==2L]$y-y1_len*0.5) +
#     # annotate("text",
#     #          x = ps[conf=="NFC" & conf_rank==2L]$x-x1_len*1.5,
#     #          y = ps[conf=="NFC" & conf_rank==2L]$y-y1_len*0,
#     #          label = "PHI") +

    # AFC week 1, rank 3 and 6 bracket
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==3L]$x,
             xend = ps[conf=="AFC" & conf_rank==3L]$x+x1_len,
             y = ps[conf=="AFC" & conf_rank==3L]$y,
             yend = ps[conf=="AFC" & conf_rank==3L]$y) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==6L]$x,
             xend = ps[conf=="AFC" & conf_rank==6L]$x+x1_len,
             y = ps[conf=="AFC" & conf_rank==6L]$y,
             yend = ps[conf=="AFC" & conf_rank==6L]$y) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len,
             xend = ps[conf=="AFC" & conf_rank==3L]$x+x1_len,
             y = ps[conf=="AFC" & conf_rank==3L]$y,
             yend = ps[conf=="AFC" & conf_rank==3L]$y-y1_len) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len,
             xend = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*2,
             y = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*0.5,
             yend = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*0.5) +
#     # annotate("text",
#     #          x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*1.5,
#     #          y = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*0,
#     #          label = "BAL") +

    # NFC week 1, rank 3 and 6 bracket
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==3L]$x,
             xend = ps[conf=="NFC" & conf_rank==3L]$x-x1_len,
             y = ps[conf=="NFC" & conf_rank==3L]$y,
             yend = ps[conf=="NFC" & conf_rank==3L]$y) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==6L]$x,
             xend = ps[conf=="NFC" & conf_rank==6L]$x-x1_len,
             y = ps[conf=="NFC" & conf_rank==6L]$y,
             yend = ps[conf=="NFC" & conf_rank==6L]$y) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==3L]$x-x1_len,
             xend = ps[conf=="NFC" & conf_rank==3L]$x-x1_len,
             y = ps[conf=="NFC" & conf_rank==3L]$y,
             yend = ps[conf=="NFC" & conf_rank==3L]$y-y1_len) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==3L]$x-x1_len,
             xend = ps[conf=="NFC" & conf_rank==3L]$x-x1_len*2,
             y = ps[conf=="NFC" & conf_rank==3L]$y-y1_len*0.5,
             yend = ps[conf=="NFC" & conf_rank==3L]$y-y1_len*0.5) +
#     # annotate("text",
#     #          x = ps[conf=="NFC" & conf_rank==3L]$x-x1_len*1.5,
#     #          y = ps[conf=="NFC" & conf_rank==3L]$y-y1_len*0,
#     #          label = "WAS") +

    # AFC week 1, rank 4 and 5 bracket
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==4L]$x,
             xend = ps[conf=="AFC" & conf_rank==4L]$x+x1_len,
             y = ps[conf=="AFC" & conf_rank==4L]$y,
             yend = ps[conf=="AFC" & conf_rank==4L]$y) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==5L]$x,
             xend = ps[conf=="AFC" & conf_rank==5L]$x+x1_len,
             y = ps[conf=="AFC" & conf_rank==5L]$y,
             yend = ps[conf=="AFC" & conf_rank==5L]$y) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==4L]$x+x1_len,
             xend = ps[conf=="AFC" & conf_rank==4L]$x+x1_len,
             y = ps[conf=="AFC" & conf_rank==4L]$y,
             yend = ps[conf=="AFC" & conf_rank==4L]$y-y1_len) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==4L]$x+x1_len,
             xend = ps[conf=="AFC" & conf_rank==4L]$x+x1_len*2,
             y = ps[conf=="AFC" & conf_rank==4L]$y-y1_len*0.5,
             yend = ps[conf=="AFC" & conf_rank==4L]$y-y1_len*0.5) +
#     # annotate("text",
#     #          x = ps[conf=="AFC" & conf_rank==4L]$x+x1_len*1.5,
#     #          y = ps[conf=="AFC" & conf_rank==4L]$y-y1_len*0,
#     #          label = "HOU") +

    # NFC week 1, rank 4 and 5 bracket
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==4L]$x,
             xend = ps[conf=="NFC" & conf_rank==4L]$x-x1_len,
             y = ps[conf=="NFC" & conf_rank==4L]$y,
             yend = ps[conf=="NFC" & conf_rank==4L]$y) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==5L]$x,
             xend = ps[conf=="NFC" & conf_rank==5L]$x-x1_len,
             y = ps[conf=="NFC" & conf_rank==5L]$y,
             yend = ps[conf=="NFC" & conf_rank==5L]$y) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==4L]$x-x1_len,
             xend = ps[conf=="NFC" & conf_rank==4L]$x-x1_len,
             y = ps[conf=="NFC" & conf_rank==4L]$y,
             yend = ps[conf=="NFC" & conf_rank==4L]$y-y1_len) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==4L]$x-x1_len,
             xend = ps[conf=="NFC" & conf_rank==4L]$x-x1_len*2,
             y = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*0.5,
             yend = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*0.5) +
#     # annotate("text",
#     #          x = ps[conf=="NFC" & conf_rank==4L]$x-x1_len*1.5,
#     #          y = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*0,
#     #          label = "LA") +

    # # AFC week 2, rank 1 to rank 4/5 extensions
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*2,
             xend = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*2,
             y = ps[conf=="AFC" & conf_rank==1L]$y,
             yend = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*2.5) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*2,
             xend = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4,
             y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*1.25,
             yend = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*1.25) +
#     # annotate("text",
#     #          x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*3,
#     #          y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*0.75,
#     #          label = "KC") +
#     #
#     # # NFC week 2, rank 1 to rank 4/5 extensions
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*2,
             xend = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*2,
             y = ps[conf=="NFC" & conf_rank==1L]$y,
             yend = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*2.5) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*2,
             xend = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*4,
             y = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*1.25,
             yend = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*1.25) +
#     # annotate("text",
#     #          x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*3,
#     #          y = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*0.75,
#     #          label = "WAS") +
#     #
#     # # AFC week 2, rank 3/6 to rank 2/7 extensions
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*2,
             xend = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*2,
             y = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*0.5,
             yend = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*3.5) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*2,
             xend = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*4,
             y = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*2,
             yend = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*2) +
#     # annotate("text",
#     #          x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*3,
#     #          y = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*1.5,
#     #          label = "BUF") +
#     #
#     # # NFC week 2, rank 3/6 to rank 2/7 extensions
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==3L]$x-x1_len*2,
             xend = ps[conf=="NFC" & conf_rank==3L]$x-x1_len*2,
             y = ps[conf=="NFC" & conf_rank==3L]$y-y1_len*0.5,
             yend = ps[conf=="NFC" & conf_rank==3L]$y-y1_len*3.5) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==3L]$x-x1_len*2,
             xend = ps[conf=="NFC" & conf_rank==3L]$x-x1_len*4,
             y = ps[conf=="NFC" & conf_rank==3L]$y-y1_len*2,
             yend = ps[conf=="NFC" & conf_rank==3L]$y-y1_len*2) +
#     # annotate("text",
#     #          x = ps[conf=="NFC" & conf_rank==4L]$x-x1_len*3,
#     #          y = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*1.5,
#     #          label = "PHI") +
#     #
#     # # AFC week 3, rank 1/4/5 to rank 2/7/3/6 extensions
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4,
             xend = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4,
             y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*1.25,
             yend = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*7) +
    annotate("segment",
             x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4,
             xend = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*5.5,
             y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*4,
             yend = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*4) +
#     # annotate("text",
#     #          x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4.75,
#     #          y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*3.5,
#     #          label = "KC") +
#     #
#     # # NFC week 3, rank 1/3/6 to rank 2/7/4/5 extensions
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*4,
             xend = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*4,
             y = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*1.25,
             yend = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*7) +
    annotate("segment",
             x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*4,
             xend = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*5.5,
             y = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*4,
             yend = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*4) +
#     # annotate("text",
#     #          x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*4.75,
#     #          y = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*3.5,
#     #          label = "PHI") +
#     #
#     # # Superbowl
    annotate("rect",
             xmin = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*5.5,
             xmax = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*6.5,
             ymin = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*4.5,
             ymax = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*3.5,
             alpha = 0.2) +
#     # annotate("text",
#     #          x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*6,
#     #          y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*4,
#     #          label = "PHI") +
    geom_label() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_rect(fill = "white", color = "black")
    )
}

p <- premake_bracket()

if(FALSE){
  ggsave(
    paste0("App/Data/bracket_gen",str_remove_all(str_sub(Sys.time(),1,19), ":"),".png"),
    plot = p,
    width = 2400,
    height = 1600,
    units = "px",
    dpi = 300
  )
}
