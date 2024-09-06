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

gssvocab_df <- readr::read_csv(GH("data/gssvocab.csv"))

set.seed(1001)
eyefix_df_avg_targ2 <- 
  mutate(eyefix_df_avg_targ, 
         mean_fix = mean_fix + rnorm(n=n(), sd = 0.1))

eyefix_df_avg2 <- 
  mutate(eyefix_df_avg, 
         mean_fix = mean_fix + rnorm(n=n(), sd = 0.1),
         Object = factor(Object))

set.seed(101)
mono_df <- tibble(x = seq(-2, 2, length.out = 20),
                  y = 0.5 + 1.5 * x + rnorm(length(x))
)

golf_df <- readr::read_csv(GH("data/golf_putts.csv"))
eyetrackr_df <- readr::read_csv(GH("data/eyetrackr.csv")) %>% 
  mutate(across(c(subject, condition, target), as.factor))

meuse_df <- readr::read_csv(GH('data/meuse.csv'))

raneftanh_df <- readr::read_csv(GH("data/raneftanh.csv"))
