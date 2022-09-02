library(rvest)
library(shiny)
library(tidyr)
require(dplyr)

CDFormula <-
  c(
    "Corners" = 0.05,
    "Crossing" = 0.01,
    "Dribbling" = 0.4,
    "Finishing" = 0.1,
    "First Touch" = 0.35,
    "Free Kicks" = 0.1,
    "Heading" = 0.55,
    "Long Shots" = 0.1,
    "Long Throws" = 0.05,
    "Marking" = 0.55,
    "Passing" = 0.55,
    "Penalty Taking" = 0.1,
    "Tackling" = 0.4,
    "Technique" = 0.35,
    "Aggression" = 0.4,
    "Anticipation" = 0.5,
    "Bravery" = 0.3,
    "Composure" = 0.8,
    "Concentration" = 0.5,
    "Decisions" = 0.5,
    "Determination" = 0.2,
    "Flair" = 0.1,
    "Leadership" = 0.1,
    "Off the ball" = 0.1,
    "Positioning" = 0.55,
    "Vision" = 0.5,
    "Work Rate" = 0.55,
    "Acceleration" = 0.9,
    "Agility" = 0.6,
    "Balance" = 0.35,
    "Jumping Reach" = 0.65,
    "Natural Fitness" = 0.1,
    "Pace" = 0.9,
    "Stamina" = 0.3,
    "Strength" = 0.5
  )

FBFormula <-
  c(
    "Corners" = 0.3,
    "Crossing" = 0.25,
    "Dribbling" = 0.5,
    "Finishing" = 0.1,
    "First Touch" = 0.3,
    "Free Kicks" = 0.1,
    "Heading" = 0.2,
    "Long Shots" = 0.1,
    "Long Throws" = 0.3,
    "Marking" = 0.45,
    "Passing" = 0.45,
    "Penalty Taking" = 0.1,
    "Tackling" = 0.5,
    "Technique" = 0.45,
    "Aggression" = 0.45,
    "Anticipation" = 0.45,
    "Bravery" = 0.2,
    "Composure" = 0.3,
    "Concentration" = 0.45,
    "Decisions" = 0.45,
    "Determination" = 0.2,
    "Flair" = 0.2,
    "Leadership" = 0.1,
    "Off the ball" = 0.7,
    "Positioning" = 0.3,
    "Vision" = 0.25,
    "Work Rate" = 0.9,
    "Acceleration" = 1.0,
    "Agility" = 0.6,
    "Balance" = 0.25,
    "Jumping Reach" = 0.4,
    "Natural Fitness" = 0.1,
    "Pace" = 0.9,
    "Stamina" = 1.0,
    "Strength" = 0.25
  )

DMFormula <-
  c(
    "Corners" = 0.1,
    "Crossing" = 0.1,
    "Dribbling" = 0.45,
    "Finishing" = 0.2,
    "First Touch" = 0.5,
    "Free Kicks" = 0.3,
    "Heading" = 0.1,
    "Long Shots" = 0.4,
    "Long Throws" = 0.05,
    "Marking" = 0.2,
    "Passing" = 0.65,
    "Penalty Taking" = 0.1,
    "Tackling" = 0.35,
    "Technique" = 0.50,
    "Aggression" = 0.5,
    "Anticipation" = 0.55,
    "Bravery" = 0.3,
    "Composure" = 0.6,
    "Concentration" = 0.5,
    "Decisions" = 0.65,
    "Determination" = 0.2,
    "Flair" = 0.5,
    "Leadership" = 0.1,
    "Off the ball" = 0.4,
    "Positioning" = 0.65,
    "Vision" = 0.55,
    "Work Rate" = 0.9,
    "Acceleration" = 0.65,
    "Agility" = 0.45,
    "Balance" = 0.35,
    "Jumping Reach" = 0.15,
    "Natural Fitness" = 0.1,
    "Pace" = 0.7,
    "Stamina" = 0.7,
    "Strength" = 0.35
  )

WFormula <-
  c(
    "Corners" = 0.3,
    "Crossing" = 0.65,
    "Dribbling" = 0.55,
    "Finishing" = 0.15,
    "First Touch" = 0.3,
    "Free Kicks" = 0.1,
    "Heading" = 0.1,
    "Long Shots" = 0.1,
    "Long Throws" = 0.3,
    "Marking" = 0.35,
    "Passing" = 0.5,
    "Penalty Taking" = 0.15,
    "Tackling" = 0.35,
    "Technique" = 0.50,
    "Aggression" = 0.35,
    "Anticipation" = 0.45,
    "Bravery" = 0.15,
    "Composure" = 0.3,
    "Concentration" = 0.35,
    "Decisions" = 0.35,
    "Determination" = 0.2,
    "Flair" = 0.2,
    "Leadership" = 0.1,
    "Off the ball" = 0.4,
    "Positioning" = 0.35,
    "Vision" = 0.35,
    "Work Rate" = 0.75,
    "Acceleration" = 1.0,
    "Agility" = 0.5,
    "Balance" = 0.15,
    "Jumping Reach" = 0.1,
    "Natural Fitness" = 0.1,
    "Pace" = 1.0,
    "Stamina" = 0.75,
    "Strength" = 0.3
  )

AMFormula <-
  c(
    "Corners" = 0.05,
    "Crossing" = 0.05,
    "Dribbling" = 0.65,
    "Finishing" = 0.65,
    "First Touch" = 0.4,
    "Free Kicks" = 0.3,
    "Heading" = 0.1,
    "Long Shots" = 0.2,
    "Long Throws" = 0.01,
    "Marking" = 0.05,
    "Passing" = 0.5,
    "Penalty Taking" = 0.15,
    "Tackling" = 0.15,
    "Technique" = 0.65,
    "Aggression" = 0.5,
    "Anticipation" = 0.7,
    "Bravery" = 0.2,
    "Composure" = 0.35,
    "Concentration" = 0.25,
    "Decisions" = 0.4,
    "Determination" = 0.2,
    "Flair" = 0.2,
    "Leadership" = 0.1,
    "Off the ball" = 0.35,
    "Positioning" = 0.1,
    "Vision" = 0.3,
    "Work Rate" = 0.8,
    "Acceleration" = 1.0,
    "Agility" = 0.3,
    "Balance" = 0.5,
    "Jumping Reach" = 0.1,
    "Natural Fitness" = 0.1,
    "Pace" = 0.8,
    "Stamina" = 0.8,
    "Strength" = 0.3
  )
STFormula <-
  c(
    "Corners" = 0.05,
    "Crossing" = 0.05,
    "Dribbling" = 0.75,
    "Finishing" = 0.80,
    "First Touch" = 0.5,
    "Free Kicks" = 0.05,
    "Heading" = 0.25,
    "Long Shots" = 0.25,
    "Long Throws" = 0.01,
    "Marking" = 0.01,
    "Passing" = 0.4,
    "Penalty Taking" = 0.2,
    "Tackling" = 0.05,
    "Technique" = 0.65,
    "Aggression" = 0.5,
    "Anticipation" = 0.5,
    "Bravery" = 0.2,
    "Composure" = 0.35,
    "Concentration" = 0.05,
    "Decisions" = 0.45,
    "Determination" = 0.2,
    "Flair" = 0.25,
    "Leadership" = 0.1,
    "Off the ball" = 0.45,
    "Positioning" = 0.05,
    "Vision" = 0.2,
    "Work Rate" = 0.6,
    "Acceleration" = 1.0,
    "Agility" = 0.3,
    "Balance" = 0.5,
    "Jumping Reach" = 0.2,
    "Natural Fitness" = 0.1,
    "Pace" = 0.7,
    "Stamina" = 0.65,
    "Strength" = 0.25
  )

createTeamDF <- function(full_squad) {
  
  names(full_squad)[5] <- "Nationality"
  team_df <- full_squad
  team_df$newPosition <-
    gsub('/', ', ', team_df$Position)
  team_df$newPosition <-
    gsub('( \\(\\w+\\)|, )', ' ', team_df$newPosition)
  team_df$newPosition <-
    gsub('  ', ' ', team_df$newPosition)
  team_df <-
    team_df %>% separate(newPosition, c('Pos1', 'Pos2', 'Pos3', 'Pos4'), sep = " ")
  team_df$GKBool <-
    with(team_df,
         Pos1 == "GK" |
           Pos2 == "GK" | Pos3 == "GK" | Pos4 == "GK")
  team_df$CDBool <-
    with(team_df, Pos1 == "D" |
           Pos2 == "D" | Pos3 == "D" | Pos4 == "D")
  team_df$FBBool <-
    with(team_df,
         Pos1 == "WB" |
           Pos2 == "WB" | Pos3 == "WB" | Pos4 == "WB")
  team_df$DMBool <-
    with(team_df,
         Pos1 == "DM" |
           Pos2 == "DM" | Pos3 == "DM" | Pos4 == "DM")
  team_df$MBool <-
    with(team_df,
         Pos1 == "M" |
           Pos2 == "M" | Pos3 == "M" | Pos4 == "M")
  team_df$AMBool <-
    with(team_df,
         Pos1 == "AM" |
           Pos2 == "AM" | Pos3 == "AM" | Pos4 == "AM")
  team_df$STBool <-
    with(team_df,
         Pos1 == "ST" |
           Pos2 == "ST" | Pos3 == "ST" | Pos4 == "ST")
  
  # print(((20 * as.double(STFormula["Corners"])) + (20 * as.double(STFormula["Crossing"])) + (20 *
  #                                                                                              as.double(STFormula["Dribbling"])) + (20 * as.double(STFormula["Finishing"])) + (20 * as.double(STFormula["First Touch"])) + (20 * as.double(STFormula["Free Kicks"])) + (20 * as.double(STFormula["Heading"])) + (20 * as.double(STFormula["Long Shots"])) + (20 * as.double(STFormula["Long Throws"])) + (20 * as.double(STFormula["Marking"])) + (20 * as.double(STFormula["Passing"])) + (20 * as.double(STFormula["Penalty Taking"])) + (20 * as.double(STFormula["Tackling"])) + (20 * as.double(STFormula["Technique"])) + (20 * as.double(STFormula["Aggression"])) + (20 * as.double(STFormula["Anticipation"])) + (20 * as.double(STFormula["Bravery"])) + (20 * as.double(STFormula["Composure"])) + (20 * as.double(STFormula["Concentration"])) + (20 * as.double(STFormula["Decisions"])) + (20 * as.double(STFormula["Determination"])) + (20 * as.double(STFormula["Flair"])) + (20 * as.double(STFormula["Leadership"])) + (20 * as.double(STFormula["Off the ball"])) + (20 * as.double(STFormula["Positioning"])) + (20 * as.double(STFormula["Leadership"])) + (20 * as.double(STFormula["Vision"])) + (20 * as.double(STFormula["Work Rate"])) + (20 *
  #                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       as.double(STFormula["Acceleration"])) + (20 * as.double(STFormula["Agility"])) + (20 * as.double(STFormula["Balance"])) + (20 * as.double(STFormula["Jumping Reach"])) + (20 * as.double(STFormula["Natural Fitness"])) + (20 * as.double(STFormula["Pace"])) + (20 * as.double(STFormula["Stamina"])) + (20 * as.double(STFormula["Strength"]))
  # ))
  
  team_df$GK <-
    ifelse(
      team_df$GKBool == TRUE,
      ((team_df$Aer * 0.6) + (team_df$Cmd * 0.4) + (team_df$Com *
                                                                    0.3) + (team_df$Ecc * 0.2) + (team_df$Fir * 0.3) + (team_df$Han * 0.5) + (team_df$Kic * 0.35) + (team_df$`1v1` * 0.45) + (team_df$Pas * 0.45) + (team_df$Ref * 0.8) + (team_df$TRO * 0.4) + (team_df$Thr * 0.3) + (team_df$Agg * 0.4) + (team_df$Ant * 0.4) + (team_df$Bra * 0.3) + (team_df$Cmp * 0.4) + (team_df$Cnt * 0.65) + (team_df$Dec * 0.5) + (team_df$Det * 0.2) + (team_df$Fla * 0.2) + (team_df$Ldr * 0.1) + (team_df$Pos * 0.4) + (team_df$Ldr * 0.1) + (team_df$Vis * 0.4) + (team_df$Wor * 0.1) + (team_df$Acc * 0.7) + (team_df$Agi) + (team_df$Bal * 0.2) + (team_df$Jum *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   0.45) + (team_df$Nat * 0.1) + (team_df$Pac * 0.5) + (team_df$Sta * 0.1) + (team_df$Str * 0.7)
      ) / 251 * 100,
      NA
    )
  team_df$CD <-
    ifelse(
      team_df$CDBool == TRUE,
      ((team_df$Cor * as.double(CDFormula["Corners"])) + (team_df$Cro * as.double(CDFormula["Crossing"])) + (team_df$Dri *
                                                                                                                             as.double(CDFormula["Dribbling"])) + (team_df$Fin * as.double(CDFormula["Finishing"])) + (team_df$Fir * as.double(CDFormula["First Touch"])) + (team_df$Fre * as.double(CDFormula["Free Kicks"])) + (team_df$Hea * as.double(CDFormula["Heading"])) + (team_df$Lon * as.double(CDFormula["Long Shots"])) + (team_df$`L Th` * as.double(CDFormula["Long Throws"])) + (team_df$Mar * as.double(CDFormula["Marking"])) + (team_df$Pas * as.double(CDFormula["Passing"])) + (team_df$Pen * as.double(CDFormula["Penalty Taking"])) + (team_df$Tck * as.double(CDFormula["Tackling"])) + (team_df$Tec * as.double(CDFormula["Technique"])) + (team_df$Agg * as.double(CDFormula["Aggression"])) + (team_df$Ant * as.double(CDFormula["Anticipation"])) + (team_df$Bra * as.double(CDFormula["Bravery"])) + (team_df$Cmp * as.double(CDFormula["Composure"])) + (team_df$Cnt * as.double(CDFormula["Concentration"])) + (team_df$Dec * as.double(CDFormula["Decisions"])) + (team_df$Det * as.double(CDFormula["Determination"])) + (team_df$Fla * as.double(CDFormula["Flair"])) + (team_df$Ldr * as.double(CDFormula["Leadership"])) + (team_df$OtB * as.double(CDFormula["Off the ball"])) + (team_df$Pos * as.double(CDFormula["Positioning"])) + (team_df$Ldr * as.double(CDFormula["Leadership"])) + (team_df$Vis * as.double(CDFormula["Vision"])) + (team_df$Wor * as.double(CDFormula["Work Rate"])) + (team_df$Acc *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         as.double(CDFormula["Acceleration"])) + (team_df$Agi * as.double(CDFormula["Agility"])) + (team_df$Bal * as.double(CDFormula["Balance"])) + (team_df$Jum * as.double(CDFormula["Jumping Reach"])) + (team_df$Nat * as.double(CDFormula["Natural Fitness"])) + (team_df$Pac * as.double(CDFormula["Pace"])) + (team_df$Sta * as.double(CDFormula["Stamina"])) + (team_df$Str * as.double(CDFormula["Strength"]))
      ) / 263.2 * 100,
      NA
    )
  team_df$FB <-
    ifelse(
      team_df$FBBool == TRUE,
      ((team_df$Cor * as.double(FBFormula["Corners"])) + (team_df$Cro * as.double(FBFormula["Crossing"])) + (team_df$Dri *
                                                                                                                             as.double(FBFormula["Dribbling"])) + (team_df$Fin * as.double(FBFormula["Finishing"])) + (team_df$Fir * as.double(FBFormula["First Touch"])) + (team_df$Fre * as.double(FBFormula["Free Kicks"])) + (team_df$Hea * as.double(FBFormula["Heading"])) + (team_df$Lon * as.double(FBFormula["Long Shots"])) + (team_df$`L Th` * as.double(FBFormula["Long Throws"])) + (team_df$Mar * as.double(FBFormula["Marking"])) + (team_df$Pas * as.double(FBFormula["Passing"])) + (team_df$Pen * as.double(FBFormula["Penalty Taking"])) + (team_df$Tck * as.double(FBFormula["Tackling"])) + (team_df$Tec * as.double(FBFormula["Technique"])) + (team_df$Agg * as.double(FBFormula["Aggression"])) + (team_df$Ant * as.double(FBFormula["Anticipation"])) + (team_df$Bra * as.double(FBFormula["Bravery"])) + (team_df$Cmp * as.double(FBFormula["Composure"])) + (team_df$Cnt * as.double(FBFormula["Concentration"])) + (team_df$Dec * as.double(FBFormula["Decisions"])) + (team_df$Det * as.double(FBFormula["Determination"])) + (team_df$Fla * as.double(FBFormula["Flair"])) + (team_df$Ldr * as.double(FBFormula["Leadership"])) + (team_df$OtB * as.double(FBFormula["Off the ball"])) + (team_df$Pos * as.double(FBFormula["Positioning"])) + (team_df$Ldr * as.double(FBFormula["Leadership"])) + (team_df$Vis * as.double(FBFormula["Vision"])) + (team_df$Wor * as.double(FBFormula["Work Rate"])) + (team_df$Acc *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         as.double(FBFormula["Acceleration"])) + (team_df$Agi * as.double(FBFormula["Agility"])) + (team_df$Bal * as.double(FBFormula["Balance"])) + (team_df$Jum * as.double(FBFormula["Jumping Reach"])) + (team_df$Nat * as.double(FBFormula["Natural Fitness"])) + (team_df$Pac * as.double(FBFormula["Pace"])) + (team_df$Sta * as.double(FBFormula["Stamina"])) + (team_df$Str * as.double(FBFormula["Strength"]))
      ) / 273 * 100,
      NA
    )
  team_df$DM <-
    ifelse(
      team_df$DMBool == TRUE,
      ((team_df$Cor * as.double(DMFormula["Corners"])) + (team_df$Cro * as.double(DMFormula["Crossing"])) + (team_df$Dri *
                                                                                                                             as.double(DMFormula["Dribbling"])) + (team_df$Fin * as.double(DMFormula["Finishing"])) + (team_df$Fir * as.double(DMFormula["First Touch"])) + (team_df$Fre * as.double(DMFormula["Free Kicks"])) + (team_df$Hea * as.double(DMFormula["Heading"])) + (team_df$Lon * as.double(DMFormula["Long Shots"])) + (team_df$`L Th` * as.double(DMFormula["Long Throws"])) + (team_df$Mar * as.double(DMFormula["Marking"])) + (team_df$Pas * as.double(DMFormula["Passing"])) + (team_df$Pen * as.double(DMFormula["Penalty Taking"])) + (team_df$Tck * as.double(DMFormula["Tackling"])) + (team_df$Tec * as.double(DMFormula["Technique"])) + (team_df$Agg * as.double(DMFormula["Aggression"])) + (team_df$Ant * as.double(DMFormula["Anticipation"])) + (team_df$Bra * as.double(DMFormula["Bravery"])) + (team_df$Cmp * as.double(DMFormula["Composure"])) + (team_df$Cnt * as.double(DMFormula["Concentration"])) + (team_df$Dec * as.double(DMFormula["Decisions"])) + (team_df$Det * as.double(DMFormula["Determination"])) + (team_df$Fla * as.double(DMFormula["Flair"])) + (team_df$Ldr * as.double(DMFormula["Leadership"])) + (team_df$OtB * as.double(DMFormula["Off the ball"])) + (team_df$Pos * as.double(DMFormula["Positioning"])) + (team_df$Ldr * as.double(DMFormula["Leadership"])) + (team_df$Vis * as.double(DMFormula["Vision"])) + (team_df$Wor * as.double(DMFormula["Work Rate"])) + (team_df$Acc *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         as.double(DMFormula["Acceleration"])) + (team_df$Agi * as.double(DMFormula["Agility"])) + (team_df$Bal * as.double(DMFormula["Balance"])) + (team_df$Jum * as.double(DMFormula["Jumping Reach"])) + (team_df$Nat * as.double(DMFormula["Natural Fitness"])) + (team_df$Pac * as.double(DMFormula["Pace"])) + (team_df$Sta * as.double(DMFormula["Stamina"])) + (team_df$Str * as.double(DMFormula["Strength"]))
      ) / 279 * 100,
      NA
    )
  team_df$M <-
    ifelse(team_df$MBool == TRUE,
           ((((team_df$Cor * as.double(AMFormula["Corners"])) + (team_df$Cro * as.double(AMFormula["Crossing"])) + (team_df$Dri *
                                                                                                                                    as.double(AMFormula["Dribbling"])) + (team_df$Fin * as.double(AMFormula["Finishing"])) + (team_df$Fir * as.double(AMFormula["First Touch"])) + (team_df$Fre * as.double(AMFormula["Free Kicks"])) + (team_df$Hea * as.double(AMFormula["Heading"])) + (team_df$Lon * as.double(AMFormula["Long Shots"])) + (team_df$`L Th` * as.double(AMFormula["Long Throws"])) + (team_df$Mar * as.double(AMFormula["Marking"])) + (team_df$Pas * as.double(AMFormula["Passing"])) + (team_df$Pen * as.double(AMFormula["Penalty Taking"])) + (team_df$Tck * as.double(AMFormula["Tackling"])) + (team_df$Tec * as.double(AMFormula["Technique"])) + (team_df$Agg * as.double(AMFormula["Aggression"])) + (team_df$Ant * as.double(AMFormula["Anticipation"])) + (team_df$Bra * as.double(AMFormula["Bravery"])) + (team_df$Cmp * as.double(AMFormula["Composure"])) + (team_df$Cnt * as.double(AMFormula["Concentration"])) + (team_df$Dec * as.double(AMFormula["Decisions"])) + (team_df$Det * as.double(AMFormula["Determination"])) + (team_df$Fla * as.double(AMFormula["Flair"])) + (team_df$Ldr * as.double(AMFormula["Leadership"])) + (team_df$OtB * as.double(AMFormula["Off the ball"])) + (team_df$Pos * as.double(AMFormula["Positioning"])) + (team_df$Ldr * as.double(AMFormula["Leadership"])) + (team_df$Vis * as.double(AMFormula["Vision"])) + (team_df$Wor * as.double(AMFormula["Work Rate"])) + (team_df$Acc *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                as.double(AMFormula["Acceleration"])) + (team_df$Agi * as.double(AMFormula["Agility"])) + (team_df$Bal * as.double(AMFormula["Balance"])) + (team_df$Jum * as.double(AMFormula["Jumping Reach"])) + (team_df$Nat * as.double(AMFormula["Natural Fitness"])) + (team_df$Pac * as.double(AMFormula["Pace"])) + (team_df$Sta * as.double(AMFormula["Stamina"])) + (team_df$Str * as.double(AMFormula["Strength"]))
           ) / 247 * 100
           ) + (((team_df$Cor * as.double(DMFormula["Corners"])) + (team_df$Cro * as.double(DMFormula["Crossing"])) + (team_df$Dri *
                                                                                                                                       as.double(DMFormula["Dribbling"])) + (team_df$Fin * as.double(DMFormula["Finishing"])) + (team_df$Fir * as.double(DMFormula["First Touch"])) + (team_df$Fre * as.double(DMFormula["Free Kicks"])) + (team_df$Hea * as.double(DMFormula["Heading"])) + (team_df$Lon * as.double(DMFormula["Long Shots"])) + (team_df$`L Th` * as.double(DMFormula["Long Throws"])) + (team_df$Mar * as.double(DMFormula["Marking"])) + (team_df$Pas * as.double(DMFormula["Passing"])) + (team_df$Pen * as.double(DMFormula["Penalty Taking"])) + (team_df$Tck * as.double(DMFormula["Tackling"])) + (team_df$Tec * as.double(DMFormula["Technique"])) + (team_df$Agg * as.double(DMFormula["Aggression"])) + (team_df$Ant * as.double(DMFormula["Anticipation"])) + (team_df$Bra * as.double(DMFormula["Bravery"])) + (team_df$Cmp * as.double(DMFormula["Composure"])) + (team_df$Cnt * as.double(DMFormula["Concentration"])) + (team_df$Dec * as.double(DMFormula["Decisions"])) + (team_df$Det * as.double(DMFormula["Determination"])) + (team_df$Fla * as.double(DMFormula["Flair"])) + (team_df$Ldr * as.double(DMFormula["Leadership"])) + (team_df$OtB * as.double(DMFormula["Off the ball"])) + (team_df$Pos * as.double(DMFormula["Positioning"])) + (team_df$Ldr * as.double(DMFormula["Leadership"])) + (team_df$Vis * as.double(DMFormula["Vision"])) + (team_df$Wor * as.double(DMFormula["Work Rate"])) + (team_df$Acc *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   as.double(DMFormula["Acceleration"])) + (team_df$Agi * as.double(DMFormula["Agility"])) + (team_df$Bal * as.double(DMFormula["Balance"])) + (team_df$Jum * as.double(DMFormula["Jumping Reach"])) + (team_df$Nat * as.double(DMFormula["Natural Fitness"])) + (team_df$Pac * as.double(DMFormula["Pace"])) + (team_df$Sta * as.double(DMFormula["Stamina"])) + (team_df$Str * as.double(DMFormula["Strength"]))
           ) / 279 * 100
           )) / 2,
           NA)
  team_df$W <-
    ifelse(
      team_df$AMBool == TRUE,
      ((team_df$Cor * as.double(WFormula["Corners"])) + (team_df$Cro * as.double(WFormula["Crossing"])) + (team_df$Dri *
                                                                                                                           as.double(WFormula["Dribbling"])) + (team_df$Fin * as.double(WFormula["Finishing"])) + (team_df$Fir * as.double(WFormula["First Touch"])) + (team_df$Fre * as.double(WFormula["Free Kicks"])) + (team_df$Hea * as.double(WFormula["Heading"])) + (team_df$Lon * as.double(WFormula["Long Shots"])) + (team_df$`L Th` * as.double(WFormula["Long Throws"])) + (team_df$Mar * as.double(WFormula["Marking"])) + (team_df$Pas * as.double(WFormula["Passing"])) + (team_df$Pen * as.double(WFormula["Penalty Taking"])) + (team_df$Tck * as.double(WFormula["Tackling"])) + (team_df$Tec * as.double(WFormula["Technique"])) + (team_df$Agg * as.double(WFormula["Aggression"])) + (team_df$Ant * as.double(WFormula["Anticipation"])) + (team_df$Bra * as.double(WFormula["Bravery"])) + (team_df$Cmp * as.double(WFormula["Composure"])) + (team_df$Cnt * as.double(WFormula["Concentration"])) + (team_df$Dec * as.double(WFormula["Decisions"])) + (team_df$Det * as.double(WFormula["Determination"])) + (team_df$Fla * as.double(WFormula["Flair"])) + (team_df$Ldr * as.double(WFormula["Leadership"])) + (team_df$OtB * as.double(WFormula["Off the ball"])) + (team_df$Pos * as.double(WFormula["Positioning"])) + (team_df$Ldr * as.double(WFormula["Leadership"])) + (team_df$Vis * as.double(WFormula["Vision"])) + (team_df$Wor * as.double(WFormula["Work Rate"])) + (team_df$Acc *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             as.double(WFormula["Acceleration"])) + (team_df$Agi * as.double(WFormula["Agility"])) + (team_df$Bal * as.double(WFormula["Balance"])) + (team_df$Jum * as.double(WFormula["Jumping Reach"])) + (team_df$Nat * as.double(WFormula["Natural Fitness"])) + (team_df$Pac * as.double(WFormula["Pace"])) + (team_df$Sta * as.double(WFormula["Stamina"])) + (team_df$Str * as.double(WFormula["Strength"]))
      ) / 254 * 100,
      NA
    )
  team_df$AM <-
    ifelse(
      team_df$AMBool == TRUE,
      ((team_df$Cor * as.double(AMFormula["Corners"])) + (team_df$Cro * as.double(AMFormula["Crossing"])) + (team_df$Dri *
                                                                                                                             as.double(AMFormula["Dribbling"])) + (team_df$Fin * as.double(AMFormula["Finishing"])) + (team_df$Fir * as.double(AMFormula["First Touch"])) + (team_df$Fre * as.double(AMFormula["Free Kicks"])) + (team_df$Hea * as.double(AMFormula["Heading"])) + (team_df$Lon * as.double(AMFormula["Long Shots"])) + (team_df$`L Th` * as.double(AMFormula["Long Throws"])) + (team_df$Mar * as.double(AMFormula["Marking"])) + (team_df$Pas * as.double(AMFormula["Passing"])) + (team_df$Pen * as.double(AMFormula["Penalty Taking"])) + (team_df$Tck * as.double(AMFormula["Tackling"])) + (team_df$Tec * as.double(AMFormula["Technique"])) + (team_df$Agg * as.double(AMFormula["Aggression"])) + (team_df$Ant * as.double(AMFormula["Anticipation"])) + (team_df$Bra * as.double(AMFormula["Bravery"])) + (team_df$Cmp * as.double(AMFormula["Composure"])) + (team_df$Cnt * as.double(AMFormula["Concentration"])) + (team_df$Dec * as.double(AMFormula["Decisions"])) + (team_df$Det * as.double(AMFormula["Determination"])) + (team_df$Fla * as.double(AMFormula["Flair"])) + (team_df$Ldr * as.double(AMFormula["Leadership"])) + (team_df$OtB * as.double(AMFormula["Off the ball"])) + (team_df$Pos * as.double(AMFormula["Positioning"])) + (team_df$Ldr * as.double(AMFormula["Leadership"])) + (team_df$Vis * as.double(AMFormula["Vision"])) + (team_df$Wor * as.double(AMFormula["Work Rate"])) + (team_df$Acc *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         as.double(AMFormula["Acceleration"])) + (team_df$Agi * as.double(AMFormula["Agility"])) + (team_df$Bal * as.double(AMFormula["Balance"])) + (team_df$Jum * as.double(AMFormula["Jumping Reach"])) + (team_df$Nat * as.double(AMFormula["Natural Fitness"])) + (team_df$Pac * as.double(AMFormula["Pace"])) + (team_df$Sta * as.double(AMFormula["Stamina"])) + (team_df$Str * as.double(AMFormula["Strength"]))
      ) / 247 * 100,
      NA
    )
  team_df$ST <-
    ifelse(
      team_df$STBool == TRUE,
      ((team_df$Cor * as.double(STFormula["Corners"])) + (team_df$Cro * as.double(STFormula["Crossing"])) + (team_df$Dri *
                                                                                                                             as.double(STFormula["Dribbling"])) + (team_df$Fin * as.double(STFormula["Finishing"])) + (team_df$Fir * as.double(STFormula["First Touch"])) + (team_df$Fre * as.double(STFormula["Free Kicks"])) + (team_df$Hea * as.double(STFormula["Heading"])) + (team_df$Lon * as.double(STFormula["Long Shots"])) + (team_df$`L Th` * as.double(STFormula["Long Throws"])) + (team_df$Mar * as.double(STFormula["Marking"])) + (team_df$Pas * as.double(STFormula["Passing"])) + (team_df$Pen * as.double(STFormula["Penalty Taking"])) + (team_df$Tck * as.double(STFormula["Tackling"])) + (team_df$Tec * as.double(STFormula["Technique"])) + (team_df$Agg * as.double(STFormula["Aggression"])) + (team_df$Ant * as.double(STFormula["Anticipation"])) + (team_df$Bra * as.double(STFormula["Bravery"])) + (team_df$Cmp * as.double(STFormula["Composure"])) + (team_df$Cnt * as.double(STFormula["Concentration"])) + (team_df$Dec * as.double(STFormula["Decisions"])) + (team_df$Det * as.double(STFormula["Determination"])) + (team_df$Fla * as.double(STFormula["Flair"])) + (team_df$Ldr * as.double(STFormula["Leadership"])) + (team_df$OtB * as.double(STFormula["Off the ball"])) + (team_df$Pos * as.double(STFormula["Positioning"])) + (team_df$Ldr * as.double(STFormula["Leadership"])) + (team_df$Vis * as.double(STFormula["Vision"])) + (team_df$Wor * as.double(STFormula["Work Rate"])) + (team_df$Acc *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         as.double(STFormula["Acceleration"])) + (team_df$Agi * as.double(STFormula["Agility"])) + (team_df$Bal * as.double(STFormula["Balance"])) + (team_df$Jum * as.double(STFormula["Jumping Reach"])) + (team_df$Nat * as.double(STFormula["Natural Fitness"])) + (team_df$Pac * as.double(STFormula["Pace"])) + (team_df$Sta * as.double(STFormula["Stamina"])) + (team_df$Str * as.double(STFormula["Strength"]))
      ) / 234 * 100,
      NA
    )
  team_df$Mental <- team_df$Bra + team_df$Cmp + team_df$Cnt + team_df$Det + team_df$Tea
  team_df <- team_df %>%                   # Using dplyr functions
    mutate_if(is.numeric,
              round,
              digits = 1)
  return(team_df)
}

tableCheck <- function(inputFile){
  if (exists("team_df")&&is.data.frame(get("team_df"))){
    return(team_df)
  }
  else {
    team_df <-
      read_html(inputFile$datapath, encoding = "UTF-8") %>%
      html_table() %>%
      .[[1]]
    team_df <- createTeamDF(team_df)
    
    return(team_df)
  }
}

GKRoles <- function(team_df){
  
}

roleTable <- function(team_df, position){
  team_df <-
    switch(
      input$positionPosRating,
      "GK" = team_df <-
        filter(team_df, GKBool == TRUE),
      "CD" = team_df <-
        filter(team_df, CDBool == TRUE),
      "FB" = team_df <-
        filter(team_df, FBBool == TRUE),
      "DM" = team_df <-
        filter(team_df, DMBool == TRUE),
      "M" = team_df <-
        filter(team_df, MBool == TRUE),
      "AM" = team_df <-
        filter(team_df, AMBool == TRUE),
      "W" = team_df <-
        filter(team_df, AMBool == TRUE),
      "ST" = team_df <- filter(team_df, STBool == TRUE)
    )
}

# full_squad <-
#   read_html("CurrentSquad.html", encoding = "UTF-8") %>%
#   html_table() %>%
#   .[[1]]
# createTeamDF(full_squad)