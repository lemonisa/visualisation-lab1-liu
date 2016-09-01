## Import librareis
library(MASS)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(fANCOVA)
library(ggrepel)
library(FField)

## Some initial settings
senic_path <- '../question/Senic.csv'   # Path to the data file
theme_set( theme_bw() )                 # ggplot appearance


##### Assignment 1
# Making raw plot from R, output to 'ass1_raw_plot.pdf'
df1=aggregate(Price~Type, data=Cars93, FUN=mean) # Cars93 is built-in in MASS
pdf('ass1_raw_plot.pdf')
barplot(df1$Price, names.arg=df1$Type)
dev.off()

##### Assignment 2
## Reading data
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


plot_quali_vars <- grid.arrange(plot_x7, plot_x8, nrow = 2)


## Export
ggsave('senic_x7.pdf', plot_x7)
ggsave('senic_x8.pdf', plot_x8)
ggsave('qualitative_vars.pdf', plot_quali_vars)


## 2.3: Quantitative plots

boxplot_aspectratio <- 2.4

make_quant_boxplot <- function (colname) {
    # Outliers position
    outliers.leftpos <- 0.60
    outliers.rightpos <- 1.16

    # Normalizatio factors before passing coordinates to orce field factor for FField library
    y.fact <- 70 / max(senic_data[ ,colname]) # 70 because y axis should have stronger field
    x.fact <- 100 / outliers.rightpos

    # Data frame for holding outliers labels data
    outliers.frame <-
        data.frame(
            outliers_mask =
                senic_data[ ,colname] < quantile(senic_data[ ,colname], 0.25) - 1.5 * IQR(senic_data[ ,colname])
                       | quantile(senic_data[ ,colname], 0.75) + 1.5 * IQR(senic_data[ ,colname]) < senic_data[ ,colname]
        )

    outliers.frame$outliers =
        ifelse(
            outliers.frame$outliers_mask,
            paste('#', senic_data$Obs, sep = ''),
            NA )

    n_outliers = nrow( outliers.frame[outliers.frame$outliers_mask, ] )

    outliers.frame$xcoord = rep(NA, nrow(outliers.frame))
    outliers.frame$xcoord.t = rep(NA, nrow(outliers.frame))
    outliers.frame$ycoord.t = rep(NA, nrow(outliers.frame))

    outliers.frame$xcoord[outliers.frame$outliers_mask] =
        c(
            matrix(c(rep(outliers.rightpos, floor(n_outliers / 2)),
                     rep(outliers.leftpos, floor(n_outliers / 2))),
                   2, byrow = T),
            rep(outliers.rightpos, n_outliers - floor(n_outliers / 2) * 2 )
        )

    outliers.frame[outliers.frame$outliers_mask, colname] <- senic_data[outliers.frame$outliers_mask, colname]

    outliers.frame$xcoord.t <- outliers.frame$xcoord * x.fact
    outliers.frame$ycoord.t[outliers.frame$outliers_mask] <- senic_data[outliers.frame$outliers_mask, colname] * y.fact

    outliers.frame <- cbind(
        outliers.frame,
        FFieldPtRep(
            coords = outliers.frame[ ,c("xcoord.t", "ycoord.t")],
            rep.fact = 43
        )
    )

    # Detect moved x-coordinate. Kind of a hacky way to posit the labels
    outliers.frame$x <- ifelse( outliers.frame$x < 1.01 * x.fact, outliers.leftpos, outliers.rightpos )
    outliers.frame$y <- outliers.frame$y / y.fact

    plot <- ggplot(senic_data, aes_string(x = shQuote(senic_column_desc[[colname]]), y=colname)) +
        geom_segment(
            data = outliers.frame,
            aes_string(x = "x",
                       xend = shQuote(senic_column_desc[[colname]]),
                       y = "y",
                       yend = colname),
            na.rm = TRUE,
            colour = "grey80"
        ) +
        geom_boxplot(  ) +
        ylab('') +
        xlab('') +
        geom_label(
            data = outliers.frame,
            aes(label = outliers, y = y, x = x),
            hjust = 0,
            na.rm = TRUE,
            colour = "grey30",
            show.legend = FALSE,
            inherit.aes = FALSE,
            label.size = 0,
            size = 2.8 ) +
        theme( aspect.ratio = boxplot_aspectratio )

    return( plot )
}

combine_plots <- function (plots.list) {
    return( do.call( grid.arrange, append(plots.list, list(ncol = 3))) )
}

quant.vars <- c("X1", "X2", "X3", "X4", "X5", "X6", "X9", "X10", "X11")
quant.plots <- lapply(quant.vars, make_quant_boxplot)
quant.combined <- combine_plots(quant.plots)

## Exporting with ggsave
mapply(
    function (plot, varname) {
        ggsave( tolower( paste('senic_', varname, '.pdf', sep='') ), plot )
    }, quant.plots, quant.vars
)
ggsave('quantitative_vars.pdf',
       quant.combined,
       width = 7,
       height = 9)


##### Assignment 3
## 3.1
least_nurse_hospital <- senic_data[which.min(senic_data$X10/senic_data$X6),]

## 3.2
plot_infection_risk <- ggplot(senic_data, aes(y = X3, x = Obs)) +
    geom_point() +
    geom_point(
        data = least_nurse_hospital,
        colour = 'red',
        size = 3) +     # Cheating: add a bigger point to cover the original
    ylab(senic_column_desc$X3) +
    xlab(senic_column_desc$Obs)
ggsave('infection_risk.pdf', plot_infection_risk)

## 3.3
mod <- loess.as(senic_data$Obs, senic_data$X3, criterion="gcv", degree=2)
result <- predict(mod, se=TRUE)

fit_data <- data.frame(
    Obs = senic_data$Obs,
    X3 = senic_data$X3,
    fit = result$fit,
    ci.upper = result$fit + 2 * result$se.fit,
    ci.lower = result$fit - 2 * result$se.fit)

## This inserts layers at the background, so that the ribbon doesn't
## cover up the dots. Reference: http://stackoverflow.com/a/20250185
plot_infection_risk$layers <- c(
    geom_ribbon(data = fit_data, aes(ymin = ci.lower, ymax = ci.upper), fill = "grey80"),
    geom_line(data = fit_data, aes(y = fit)),
    plot_infection_risk$layers
)

ggsave('infection_risk_ANCOVA.pdf', plot_infection_risk)
