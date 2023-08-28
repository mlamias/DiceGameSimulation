# Reset the random number generator
set.seed(34) 

# Set Parameters
DICE_FACES<-1:6
NUM_GAMES<-100000000
ROUNDS<-3
PLAYERS<-2
MATRIX_NAMES<-c("Game_Iteration", paste0("R_", rep(1:3,each=2), "_P_", c(1,2)))

# Run three rounds of games for each player
games<-sample(DICE_FACES, NUM_GAMES*ROUNDS*PLAYERS, replace = TRUE) %>% matrix(ncol=ROUNDS*PLAYERS)

game_details<-games %>% data.frame() %>% mutate(Game_Iteration=1:NUM_GAMES) %>% 
  select(Game_Iteration, everything()) %>% 
  set_names(MATRIX_NAMES) %>% tibble() %>% 
  mutate(R1_Won=(R_1_P_1==R_1_P_2),
         R2_Won=(R_2_P_1==R_2_P_2),
         R3_Won=(R_3_P_1==R_3_P_2),
         Game_Won=((R1_Won+R2_Won+R3_Won)==3)
  )

# Obtain round and game result details
wins<-game_details %>% summarize(
  across(
    ends_with(("Won")),
    list(sum, mean),
    .names = "{col}_{fn}")
  ) %>% 
  set_names(c(paste0("R", rep(1:3,each=2), c("_Win_Num", "_Win_Prob")), "Game_Win_Num", "Game_Win_Prob"))




# Obtain the probability of winning the game
p<-round(wins$Game_Win_Prob, 5)
theoretical_probability<-round(1/((6)^3), 5)

# Compute and print the odds of winning the entire game
odds<-p/(1-p)

glue::glue("\n
========================RESULTS========================
The theoretical result is: {theoretical_probability}.
The simulated result is: {p}.\n
The odds of the event is calculated to be:  {round(odds, 5)}.
=======================================================")

