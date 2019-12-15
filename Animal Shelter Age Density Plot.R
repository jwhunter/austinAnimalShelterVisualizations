#austin animal shelter analysis and visualization
library(ggplot2)

setwd("/Users/Jacob/R/austinanimalshelter")

charAgeToYears <- function(v){
    output <- c()
    
    for(a in v){
        a <- unlist(strsplit(a, " "))
        
        if(length(a) == 2){
            if(grepl("year", a[[2]])){
                output <- c(output, as.numeric(a[[1]]) * 12)
            } else if(grepl("month", a[[2]])){
                output <- c(output, as.numeric(a[[1]]))
            } else if(grepl("week", a[[2]])){
                output <- c(output, as.numeric(a[[1]]) / 4)
            } else if(grepl("day", a[[2]])){
                output <- c(output, as.numeric(a[[1]]) / 30)
            } else {
                output <- c(output, -1)
            }
        } else {
            output <- c(output, -1)
        }
    }
    output
}

outcomes <- read_csv("~/R/austinanimalshelter/aac_shelter_outcomes.csv")

outcomes$age_upon_outcome_in_months <- charAgeToYears(outcomes$age_upon_outcome)

outcomes$age <- abs(difftime(outcomes$date_of_birth, Sys.Date()))/30

#how many of these animals are between 0 and 150 months old?
length(which(outcomes$animal_type %in% c("Cat", "Dog", "Livestock") & outcomes$age < 150))/length(which(outcomes$animal_type %in% c("Cat", "Dog", "Livestock")))

ggplot(subset(outcomes, animal_type %in% c("Dog", "Cat", "Livestock")), aes(age, fill = animal_type, colour = animal_type)) +
    geom_density(alpha = 0.3) +
    xlim(0, 150) +
    theme_bw() + 
    scale_fill_manual(values=c("#468966", 
                               "#fff0a5",
                               "#ffb03b",
                               "#b64926")) +
    scale_color_manual(values=c("#468966", 
                                "#fff0a5",
                                "#ffb03b",
                                "#b64926")) +
    theme(legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="black"),
          legend.position = c(0.8, 0.8)) +
    xlab("Age (months)") + 
    ylab("Density") +
    labs(title = "Ages of Animals at the Austin Animal Shelter",
         subtitle = "Separated by animal type and cutoff at 150 months, containing 93% of the animals",
         caption = "Source: the Austin Animal Shelter",
         fill = "Animal Type", color = "Animal Type")
    