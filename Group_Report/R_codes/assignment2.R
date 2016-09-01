library(ggplot2)
library(grid)
library(gridExtra)

## Some initial settings
senic_path <- 'data/Senic.csv'   # Path to the data file
SENIC.ref <- "Source: Study on the Efficacy of Nosocomail Infection Control (SENIC)"
theme_set( theme_bw() )                 # ggplot appearance


senic_data <- read.csv2(senic_path)
senic_data$X7 <- factor(senic_data$X7, levels=c(1,2), labels=c('Yes', 'No'), ordered = TRUE)
senic_data$X8 <- factor(senic_data$X8, levels=c(1,2,3,4), labels=c('NE', 'NC', 'S', 'W'))

senic_column_desc <- list(Obs = 'Identification Number',
                          X1 = 'Length of Stay',
                          X2 = 'Age',
                          X3 = 'Infection Risk',
                          X4 = 'Routine Culturing Ratio',
                          X5 = 'Routine Chest X-ray Ratio',
                          X6 = 'Number of Beds',
                          X7 = 'Medical School Affiliation',
                          X8 = 'Region',
                          X9 = 'Average Daily Census',
                          X10 = 'Number of Nurses',
                          X11 = 'Available Facilities & Services')

## 2.2: Qualitative plots
plot_x7 <- ggplot(senic_data, aes(X7)) +
    geom_bar( stat = 'count' ) +
    geom_text( stat = 'count', aes(label=..count..), hjust = -0.5) +
    ggtitle('Number of Hospitals with and without Medical School Affiliation') +
    xlab('') +
    ylab('Hospital Count') +
    coord_flip() +
    scale_y_continuous(
        expand = c(0.04, 0),
        limits = c(0, 105),
        breaks = scales::pretty_breaks(n=10) ) +
    theme( aspect.ratio = 1/2 )

plot_x8 <- ggplot(senic_data, aes(reorder(X8, -table(X8)[X8]))) +
    geom_bar( stat = 'count' ) +
    geom_text( stat = 'count', aes(label=..count..), hjust = -0.5) +
    ggtitle('Number of Hospitals in Regions') +
    xlab('') +
    ylab('Count') +
    coord_flip() +
    scale_y_continuous(
        expand = c(0.02, 0),
        limits = c(0, 40),
        breaks = scales::pretty_breaks(n=10) ) +
    theme( aspect.ratio = 1/2 )


plot_quali_vars <- grid.arrange(
    plot_x7,
    plot_x8,
    nrow = 2,
    bottom = textGrob(SENIC.ref,
                      gp = gpar(fontface=1, fontsize=10),
                      hjust=1,
                      x=0.98)
)


## Export
## ggsave('senic_x7.pdf', plot_x7)
## ggsave('senic_x8.pdf', plot_x8)
ggsave('../qualitative_vars.pdf', plot_quali_vars)
