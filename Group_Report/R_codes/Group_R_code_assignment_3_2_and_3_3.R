# Assignment 3.2 and 3.3

install.packages("fANCOVA")
library(fANCOVA)

data <- read.csv2(file = path_assignment_2,header=T)
mydata_2 <- data
#------------- Data structure

# Changing the column/Variable names
new_column_names <-
    c("ID","Length_of_stay","Age","Infection_Risk",
      "Routine_Culturing_Ratio", "Routine_Chest_Xray_Ratio",
      "nr_of_beds","Med_school_aff","Region","Ave_Daily_Cens",
      "nr_of_nurses","Avail_Fac_Ser")

colnames(mydata_2) <-new_column_names

# classifiying the variable into 'factors'

# Starting of with different labels (Will give better graph output)
region_labels <- c("1" = "NE", "2" = "NC", "3" = "S", "4" = "W")
med_school_aff_labels <- c("1" = "Yes", "2" = "No")

mydata_2[,"Region"] <- region_labels[mydata_2[,"Region"]]
mydata_2[,"Med_school_aff"] <- med_school_aff_labels[mydata_2[,"Med_school_aff"]]

# Classifiying
mydata_2[,c("ID","Med_school_aff","Region")] <-
    lapply(mydata_2[,c("ID","Med_school_aff","Region")], as.factor)
lapply(mydata_2,class)


mod <- loess.as(data$Obs, data$X3,criterion="gcv", degree=2)
result <- predict(mod, se=TRUE)

data$fitted_values <- result$fit
data$fitted_sd <- result$se.fit

ggplot(data = subset(mydata_2, ID!=107)) +
    geom_point( aes(y=Infection_Risk,
                    x = as.numeric(ID)))+
    geom_point(data = subset(mydata_2, ID == 107),
               aes(y=Infection_Risk, x = as.numeric(ID)),
               col = "red",
               size = 5 ) +
    geom_line(data = data, aes(y = fitted_values, x = Obs)) +
    geom_ribbon(data = data,
                aes(ymax = (fitted_values + 2*fitted_sd),
                    ymin = (fitted_values - 2*fitted_sd), x = Obs),
                fill = "red", alpha = 0.2) +
    xlab("Hospital ID") + ylab("Infection Risk in Percent") +
    scale_x_continuous(labels  = NULL, breaks = NULL ) +
    annotate("segment",
             x = 85 ,
             xend = 104,
             y = 0.8,
             yend = 1.3,
             arrow = arrow(length = unit(.3, "cm"),
                           type = "closed")) +
    annotate("text",
             x = 65,
             y = 0.8,
             label = "Hospital 107 (ID) has the lowest Nurses to Bed Ratio") +
    theme_bw()
