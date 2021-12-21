# NFL-Player-Evaluation
Scripts to reproduce Regularized Adjusted Expected Points Added

The .Rmd file explains the methodology and analysis, which have since been updated:
- Expected_Points_Model_Set.R: rebuilds the EPA model with some additional variables (I have since added an Expected Dropback model to predict the probability of a pass or run, and some propietary data)
- RAEPA_All.R: calculates the vanilla RAPM for QB, RB, WR, TE, Defense since 1999 - shoot me an email if you would like the data yearly
- RAEPA.R: calculates RAPM individually utilizing past season RAPM as weights for current season
- Additional_Functions.R: Mostly nflfastR functions with a couple extras added to shorten the script
