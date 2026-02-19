This folder contains the supplementary material for the manuscript entitled 
"Using Computer Games to Explore Foraging-Predation Trade-offs and Spatial Learning in Humans".

It contains two raw datasets (csv files) produced by the two computer games used for the study, one dataset produced by the questionnaire the participants filled,
and three additional datasets derived from the former files (the directions for building them are presented and coded in the R files), 
three R files in which the analysis was coded, and an R project file.
R version 4.5.0 was used.

The files are:

1. open_field_data.csv: raw data produced from the open-field foraging game.
2. humen_maze_data.csv: raw data produced from the maze game.
3. general_data.csv: dataset produced by the questionnaire that the participants filled out, including only the sex and age of the participants.
4. open_field_visits_detail:  dataset summarizing the patch visits numbers and times in the open-field game. 
Directions for building the file are located in the "foraging_game_analysis.R" file.
5. game_sum.csv: dataset derived from the combination of the summaries of raw datasets, used for the correlation analysis.
6. foraging_game_analysis.R: R code for the analysis of the open-field foraging game.
7. Human_maze_analysis.R: R code for the analysis of the maze game.
8. r_code_for_game_analysis.Rproj: the RStudio project used for running the R files.
