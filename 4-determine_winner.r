source("3-add_stocks_data.r")

results <- data.frame(ID = integer(),
                      Stock_short = character(),
                      Case = integer(),
                      Winner = character(),
                      mean_depth = double())

for (istock in 1:nstocks){
  
  subdata <- filter(withcatch, Stock_short == adios.data$Stock_short[istock])

  for (icase in 1:4){
    # create four cases: with/without zeros, with/without filled wing areas
    if (icase == 1) mycase <- subdata # with zeros, with filled wing areas
    if (icase == 2) mycase <- filter(subdata, CatchWt > 0) # without zeros, with filled wing areas
    if (icase == 3) mycase <- filter(subdata, orig_wing_spread_flag == 1) # with zeros, without filled wing areas
    if (icase == 4) mycase <- filter(subdata, (CatchWt > 0) & (orig_wing_spread_flag == 1)) # without zeros, without filled wing areas
    
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
    
    standard_slope <- standard_lm$coefficients[names(standard_lm$coefficients) == "wing_area_swept"]
    wingarea_slope <- wingarea_lm$coefficients[names(wingarea_lm$coefficients) == "wing_area_swept"]
    
    winner = "WingSpread"
    if(abs(standard_slope) < abs(wingarea_slope)) winner <- "Standard"
    if(abs(standard_slope) == abs(wingarea_slope)) winner <- "Tie"
    
    # check for significant winner (outside plus minus 1.96 std devs)
    standard_slope_stderr <- coef(summary(standard_lm))["wing_area_swept", "Std. Error"]
    if (wingarea_slope < standard_slope - 1.96 * standard_slope_stderr |
        wingarea_slope > standard_slope + 1.96 * standard_slope_stderr) winner = paste(winner, "Significant")
    
    mean_depth = mean(mycase$Depth, na.rm = TRUE)

    this.result <- data.frame(ID = istock,
                              Stock_short = adios.data$Stock_short[istock],
                              Case = icase,
                              Winner = winner,
                              mean_depth = mean_depth)
    
    results <- rbind(results, this.result)
    
  }
}
results
