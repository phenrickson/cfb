# create custom theme

theme_phil <- function () { 
        
        theme_minimal() %+replace%
                theme(
                        axis.title = element_text(),
                       # strip.background = element_rect(fill="grey100"),
                        #strip.text = element_text(colour = 'black'),
                      #  strip.text.x = element_text(size = 7),
                      #  strip.text.y = element_text(size = 7),
                        legend.position = "top",
                      plot.subtitle = element_text(size = 10,
                                                   hjust = 0),
                      axis.text.x = element_text(size=rel(0.9),
                                                 vjust = 1),
                      axis.text.y = element_text(size=rel(0.9),
                                                 hjust = 1),
                      plot.caption = element_text(size=8,
                                                  hjust = 1,
                                                  vjust = 1,
                                                  margin = unit(c(6,0,0,0), "points")),
                        legend.title = element_blank(),
                        panel.grid.minor = element_blank(),
                       panel.spacing = unit(6.5, "mm"),
                #      strip.text.y = element_text(family = "sans", colour = "#3C3C3C", size = 8),
                 #     strip.text.x = element_text(family = "sans", colour = "#3C3C3C", size = 8),
                  #    strip.background = element_rect(fill="grey80"),
                      panel.grid = element_line(colour = "grey90")
                      
                      # remove spacing between facets
                      
                )
                
}

a=theme_phil()
a$plot.caption

