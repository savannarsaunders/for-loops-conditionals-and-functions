data <- read_csv("data/fish_psu_density.csv")
taxo <- read_csv("data/taxonomic.csv")

# 1

for (i in 1:25) {
  print(i)
}

# 2

for (spp in unique(data$SPECIES_CD)) {
  print(spp)
}

# 3 

for (i in 1:10) {
  print(i * i)
}

# 4 

for (i in 1:20) {
  if (i %% 2 != 0) {
    print(i)
  }
}

# 5

for (i in 1:10) {
  if (i %% 2 == 0){
    print(paste(i, "is even"))
  } else {
    print(paste(i, "is odd"))
  }
}


# 6

a <- c()

for (spp in unique(data$SPECIES_CD)) {
  a <- append(a, spp)
}


# 7

square_a_number <- function(x) {
  x * x
}

# 8

circle_stats <- function(radius) {
  circ <- 2 * pi * radius
  area <- pi * radius * radius
  
  return(list(circumfrence  = circ, area =  area))
}

# 9

sample_number_by_year_strat <- function(df) {
  a <- df %>% 
    group_by(YEAR, STRAT) %>% 
    summarise(n = length(unique(PRIMARY_SAMPLE_UNIT)))
  
  return(a)
}

# 10

plot_spp <- function(df, spp) {
  
  commonname <- str_to_title(pluck(filter(taxo, SPECIES_CD == spp)$COMNAME))
  d <- df %>% 
    filter(SPECIES_CD == spp) %>%
    group_by(YEAR, REGION, SPECIES_CD) %>% 
    summarise(mean_den = mean(density), stdev = sd(density), n = length(unique(PRIMARY_SAMPLE_UNIT)), SE = stdev / sqrt(n)) %>% 
    ggplot(aes(x=YEAR, y=mean_den, color = REGION, group = REGION)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = mean_den - SE, ymax = mean_den + SE), width = 0.25, size = 0.5) + 
    labs(title = paste("Cool Plot of",commonname, " ")) + 
    xlab("Year") + ylab("Density (SE)") +
    theme_minimal()+
    theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          panel.spacing.x = unit(8, "mm"),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(color = "grey50", 
                                      fill = NA, 
                                      size = 1),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(),
          axis.line.x = element_line(colour="grey50", size = 1),
          axis.line.y = element_line(colour="grey50", size = 1),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_line(colour="#f0f0f0"),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.5, "cm"),
          legend.margin = margin(0,0,0,0, "cm"),
          legend.title = element_text(face="italic"),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_blank(),
          strip.text = element_text(face="plain")
    )
  
  return(d)
}
