source("4alt-determine_winner_aic.r")

# For the models in which wingspread was selected by AIC, what
# proportion of those models have a positive wingspread effect (which
# is what we would expect)? This is only use if
# 4alt-determine_winner_aic.r is ran.
if(!is.null(results$wing_spread_effect)) {
  frac_positive_wingspread_effect <- 
    sum(results$wing_spread_effect > 0, na.rm=T)/sum(!is.na(results$wing_spread_effect))
  print(paste("Fraction of wingspread effects that are positive =", 
              frac_positive_wingspread_effect))
}

# winners table (combine the two csv files in Excel to make an xlsx)
stock_winners <- spread(select(results, -mean_depth), Case, Winner)
stock_winners_counts <- with(results, table(Winner, Case))
write.csv(stock_winners, "output/stock_winners.csv", row.names = FALSE)
write.csv(stock_winners_counts, "output/stock_winners_counts.csv", row.names = TRUE)

my_case_names <- c("1" = "With Zeros and Fills",
                   "2" = "Without Zeros, With Fills",
                   "3" = "With Zeros, Without Fills",
                   "4" = "Without Zeros or Fills")

for (icase in 1:4){
  case_plot <- ggplot(filter(results, Case == icase), 
                      aes(x=mean_depth, y=reorder(Stock_short, mean_depth), color=Winner)) +
    geom_point() +
    scale_color_manual(values = c("blue", "red")) +
    xlab("Mean Depth (meters)") +
    ylab("Stock") +
    ggtitle(my_case_names[icase]) +
    theme_bw()
  
  print(case_plot)
  ggsave(paste0("output/case_plot_",icase,".png"))
}

# make the stock specific plots - this will get big!
make_pdf <- FALSE # set to TRUE to create the four pdf files

if (make_pdf == TRUE){
  
  # loop through the four cases (takes a while to run, about 1.5 hours on my machine)
  for (my_choice in 1:4){
    pdf(file = paste0("output/stock_specific_plots_case_", my_choice,".pdf"), onefile=TRUE)
    
    for (istock in 1:nstocks){
      
      this.winner <- filter(results, 
                            Stock_short == adios.data$Stock_short[istock],
                            Case == my_choice) %>%
        select(Winner) 
      
      subdata <- filter(withcatch, Stock_short == adios.data$Stock_short[istock])
      
      # create four cases: with/without zeros, with/without filled wing areas
      if (my_choice == 1) mycase <- subdata # with zeros, with filled wing areas
      if (my_choice == 2) mycase <- filter(subdata, CatchWt > 0) # without zeros, with filled wing areas
      if (my_choice == 3) mycase <- filter(subdata, orig_wing_spread_flag == 1) # with zeros, without filled wing areas
      if (my_choice == 4) mycase <- filter(subdata, (CatchWt > 0) & (orig_wing_spread_flag == 1)) # without zeros, without filled wing areas
      
      if (length(unique(mycase$Season)) >= 2){
        standard_lm <- lm(Cstandard ~ 
                            as.factor(Year) * as.factor(Season) * as.factor(Strata) + wing_area_swept, 
                          data=mycase)
        wingarea_lm <- lm(Cwingarea ~ 
                            as.factor(Year) * as.factor(Season) * as.factor(Strata) + wing_area_swept, 
                          data=mycase)
      }else{ # stocks with only one Season used
        standard_lm <- lm(Cstandard ~ 
                            as.factor(Year) * as.factor(Strata) + wing_area_swept, 
                          data=mycase)
        wingarea_lm <- lm(Cwingarea ~ 
                            as.factor(Year) * as.factor(Strata) + wing_area_swept, 
                          data=mycase)
      }
      
      mycase$predS <- predict(standard_lm)
      mycase$predA <- predict(wingarea_lm)
      
      years <- unique(mycase$Year)
      nyears <- length(years)
      
      for (iyear in 1:nyears){
        pp <- ggplot(filter(mycase, Year == years[iyear]), aes(x=wing_area_swept)) +
          geom_point(aes(y=Cstandard), color="blue") +
          geom_point(aes(y=Cwingarea), color="red") +
          facet_wrap(Season~Strata) +
          geom_line(aes(x=wing_area_swept, y=predS), color="blue") +
          geom_line(aes(x=wing_area_swept, y=predA), color="red") +
          geom_vline(xintercept = standard_wing_swept_area, linetype = 2) +
          ggtitle(paste(mycase$Stock_short[1],years[iyear]," Case",my_choice,
                        " (",my_case_names[my_choice],") Winner =",this.winner$Winner[1])) +
          ylab("Catch per Area (kg/km^2)") +
          theme_bw()
        
        print(pp)
        
      }
    }
    dev.off()
  }
}

#---------------------------------------------------
# example of how to look at one stock in more detail
istock <- 1            # from the adios_data.csv
my_choice <- 4         # which Case
my_year <- 2016        # which year to plot
my_season <- 'Spring'  # which season to plot
my_strata <- 1160      # which strata to plot

this.winner <- filter(results, 
                      Stock_short == adios.data$Stock_short[istock],
                      Case == my_choice) %>%
  select(Winner) 

subdata <- filter(withcatch, Stock_short == adios.data$Stock_short[istock])

# create four cases: with/without zeros, with/without filled wing areas
if (my_choice == 1) mycase <- subdata # with zeros, with filled wing areas
if (my_choice == 2) mycase <- filter(subdata, CatchWt > 0) # without zeros, with filled wing areas
if (my_choice == 3) mycase <- filter(subdata, orig_wing_spread_flag == 1) # with zeros, without filled wing areas
if (my_choice == 4) mycase <- filter(subdata, (CatchWt > 0) & (orig_wing_spread_flag == 1)) # without zeros, without filled wing areas

if (length(unique(mycase$Season)) >= 2){
  standard_lm <- lm(Cstandard ~ 
                      as.factor(Year) * as.factor(Season) * as.factor(Strata) + wing_area_swept, 
                    data=mycase)
  wingarea_lm <- lm(Cwingarea ~ 
                      as.factor(Year) * as.factor(Season) * as.factor(Strata) + wing_area_swept, 
                    data=mycase)
}else{ # stocks with only one Season used
  standard_lm <- lm(Cstandard ~ 
                      as.factor(Year) * as.factor(Strata) + wing_area_swept, 
                    data=mycase)
  wingarea_lm <- lm(Cwingarea ~ 
                      as.factor(Year) * as.factor(Strata) + wing_area_swept, 
                    data=mycase)
}

mycase$predS <- predict(standard_lm)
mycase$predA <- predict(wingarea_lm)

years <- unique(mycase$Year)

ppp <- ggplot(filter(mycase, Year == my_year & Season == my_season & Strata == my_strata), 
              aes(x=wing_area_swept)) +
  geom_point(aes(y=Cstandard), color="blue") +
  geom_point(aes(y=Cwingarea), color="red") +
  facet_wrap(Season~Strata) +
  geom_line(aes(x=wing_area_swept, y=predS), color="blue") +
  geom_line(aes(x=wing_area_swept, y=predA), color="red") +
  geom_vline(xintercept = standard_wing_swept_area, linetype = 2) +
  ggtitle(paste(mycase$Stock_short[1],years[my_year]," Case",my_choice,
                " (",my_case_names[my_choice],") Winner =",this.winner$Winner[1])) +
  ylab("Catch per Area (kg/km^2)") +
  theme_bw()

print(ppp)
ggsave("output/example_catch_per_area_with_regressions.png")
