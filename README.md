# NFL-Player-Evaluation
Scripts to reproduce Regularized Adjusted Expected Points Added - among other things

The .Rmd file explains the methodology and analysis, which have since been updated:
- Expected_Points_Model_Set.R: rebuilds the EPA model with some additional variables (I have since added an Expected Dropback model to predict the probability of a pass or run, and some propietary data)
- EP_Models.R: Ben and Sebastian's EP models
- RAEPA_All.R: calculates RAPM individually utilizing past season RAPM as weights for current season - shoot me an email if you would like the data yearly or the Vanilla RAPM's since 1999
- Additional_Functions.R: Mostly nflfastR functions with a couple extras added to shorten the script
- NFL_Tracking_Data: SQL database of 2021 BDB Repo with Tom Bliss code for some starter metrics
- Mixed_Ratings: Using Mixed models to create player and coordinator ratings
- fastRConnection: creates SQL connection to nflfastR database and performs some joins to create a master roster
