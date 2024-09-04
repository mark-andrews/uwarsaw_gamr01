GH <- function(PATH){
  stringr::str_c('https://raw.githubusercontent.com/mark-andrews/uwarsaw_gamr01/main/', PATH)
}

# height, weight etc of sample of US army
weight_df <- readr::read_csv(GH("data/weight.csv")) |>
  dplyr::select(weight, height, gender, age, race) |> 
  dplyr::mutate(across(c(race, gender), factor))

# British Lexicon Project lexical decision data: sagendermple
blp_df <- readr::read_csv(GH('data/blp_df.csv')) |>
  # remove very slow rts
  dplyr::filter(rt <= 2000) |> 
  dplyr::mutate(across(c(subject, item), factor))

eyefix_df <- readr::read_csv(GH("data/funct_theme_pts.csv"))

eyefix_df_avg <- dplyr::group_by(eyefix_df, Time, Object) |>
  dplyr::summarize(mean_fix = mean(meanFix), .groups = 'drop')

eyefix_df_avg_targ <- dplyr::filter(eyefix_df_avg, Object == 'Target')

DOC_df <- readr::read_csv(GH("data/DoctorVisits.csv"))
