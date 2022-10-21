library(tidyverse)
library(mlbplotR)
library(ggrepel)

# install.packages("devtools")
# devtools::install_github("camdenk/mlbplotR")

# custom theme for plots
theme_scott <- function () {

   theme_minimal(base_size=9) %+replace%
      theme(
        panel.grid.minor = element_blank(),
        plot.background = element_rect(
           fill = "aliceblue", color = "aliceblue"
        ),
        axis.text.y = element_text(size = 10),
	axis.text.x = element_text(size = 10),
	   plot.margin = margin(10, 10, 20, 10) 
      )
}

# read in data from csv
df = read_csv("https://raw.githubusercontent.com/svanlent/run_differential/main/run_diff_22.csv", show_col_types = FALSE)

# load up team logos from the mlbplotR package
teams_colors_logos = load_mlb_teams()

# join the team logos dataframe with df object and create a new column 
# that has each club's win/loss record 
df = df %>% 
   left_join(., teams_colors_logos, by = 'team_name') %>%
   mutate(record = paste0("(",wins,"-", losses, ")"))

teams_to_label = c("KC", "WSH", "BOS", "OAK", "PIT", "COL", "LAD", "HOU", "NYY", "CLE", "NYM", "STL")

# make plot
df %>% 
   ggplot(., aes(x = rs, y =  ra, 
       label = ifelse(team_abbr %in% teams_to_label, record, ''))) +
   geom_mlb_logos(aes(team_abbr = team_abbr), width = 0.06) +
   scale_x_continuous(breaks = seq(450, 900, 50), limits=c(450, 900)) +
   scale_y_reverse(breaks = seq(450, 900, 50), limits=c(900, 450)) +
   geom_text_repel(
      min.segment.length = .1, 
      nudge_x = .15, 
      box.padding = 0.9, 
      max.overlaps = Inf, 
      alpha = .9
   ) +
   geom_hline(yintercept=670, linetype="dashed", size = .2) +
   geom_vline(xintercept=670, linetype="dashed", size = .2) +
   labs(
      title="2022 MLB Run Differentials by Club",
      x ="Runs Scored", 
      y = "Runs Scored Against") +
   theme_scott() +
   theme(
       legend.position = 'none', 
       plot.title.position = 'plot', 
       plot.title = element_text(
           face = 'bold', 
           size = 18, 
           color = "#CB4C4E", 
           hjust = 0.5
       ), 
       plot.subtitle = element_text(
           size = 12, 
           face = 'bold', 
           hjust = 0.5
       )
   )

# plot #2

# fit the model
mod = df %>% lm(wins ~ diff, .) 

# use the broom package to quickly get the fitted values
fit = broom::augment(mod) %>% select(o_wins = wins, x_wins = .fitted)

# bind the expected wins column back to the original dataframe, df, and
# calculate the residuals
df_fit = fit %>% 
   cbind(., df) %>% 
   mutate(win_d = round(o_wins - x_wins, 0)) %>%
   as_tibble

# get team colors vector
team_colors_long = c(
    'Texas Rangers' = '#002D72',
    'Detroit Tigers' = '#0C2340',
    'Los Angeles Dodgers' = '#002F6C',
    'Los Angeles Angels' = '#BA0021',
    'Minnesota Twins' ='#0C2340',
    'New York Mets' = '#002D72',
    'Arizona Diamondbacks' = '#A71930',
    'St. Louis Cardinals' = '#BA0C2F',
    'Cleveland Guardians' = '#D50032',
    'Baltimore Orioles' = '#FC4C02',
    'Toronto Blue Jays' = '#134A8E',
    'Houston Astros' = '#EB6E1F',
    'New York Yankees' = '#0C2340',
    'Atlanta Braves' = '#BA0C2F',
    'Colorado Rockies' = '#330072',
    'Seattle Mariners' = '#00685E',
    'Milwaukee Brewers' = '#13294B',
    'Kansas City Royals' = '#1A4784',
    'Pittsburgh Pirates' = '#FFC72C',
    'Washington Nationals' = '#BA122B',
    'Chicago Cubs' = '#002F6C',
    'San Francisco Giants' = '#FA4616',
    'San Diego Padres' = '#041E42',
    'Boston Red Sox' = '#C8102E',
    'Miami Marlins' = '#ED6F2E',
    'Cincinnati Reds' = '#D50032',
    'Oakland Athletics' = '#034638',
    'Tampa Bay Rays' = '#8FBCE6',
    'Philadelphia Phillies' = '#BA0C2F',
    'Chicago White Sox' = '#27251F'
)

# make plot
df_fit %>%
   mutate(o_wins = as.numeric(o_wins)) %>%
   mutate(team_abbr = paste0(team_abbr, " ", win_d)) %>%
   ggplot(., aes(x = o_wins, y =  x_wins, 
      label = team_abbr, fill = team_name)) +
   scale_x_continuous(breaks = seq(50, 120, 5), limits=c(50, 120)) +
   scale_y_continuous(breaks = seq(50, 120, 5), limits=c(50, 120)) +
   geom_point(size = 3) + 
   scale_fill_manual(values=team_colors_long) +
   geom_point(shape = 21, size = 5, colour = "black") +
   geom_text_repel(
      min.segment.length = .1, 
      nudge_x = .15, 
      box.padding = 0.9, 
      max.overlaps = Inf, 
      alpha = .9
   ) +
   theme_scott() +
   theme(
      legend.position = 'none', 
      plot.title.position = 'plot', 
      plot.title = element_text(
         face = 'bold', 
         size = 18, 
         color = "#CB4C4E", 
         hjust = 0.5
       ), 
      plot.subtitle = element_text(
         size = 12, 
         face = 'bold', 
         hjust = 0.5
       )
   ) +
   labs(
      title="2022 MLB Wins vs. Expected Wins",
      subtitle = "Expected Wins Based on Run Differential",
      x ="Actual Wins", 
      y = "Expected Wins") +
   geom_smooth(
      method="lm", 
      se=FALSE, 
      formula=y~x, 
      colour="red", 
      linetype = "dashed", 
      fill = NA
   )

# plot #3

# read in data from csv
df_all = read_csv("https://raw.githubusercontent.com/svanlent/run_differential/main/run_diff_20_22.csv", show_col_types = FALSE)

# join the team logos again
df_all = df_all %>% left_join(., teams_colors_logos, by = 'team_name')

# make the plot
df_all %>% 
   ggplot(., aes(x=year, y=diff, fill = team_name)) +
   geom_bar(stat="identity", color = "black", width = .8) +
   scale_y_continuous(
      breaks = seq(-350, 350, by = 50), 
      limits = c(-350, 350)) +
   scale_x_continuous(breaks = seq(2012, 2022, by = 1)) +
   scale_fill_manual(values=team_colors_long) +
   theme_scott() +
   theme(
      legend.position = 'none', 
      legend.title=element_blank(),
      plot.title.position = 'plot', 
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9, angle=65, vjust=0.6),
      plot.title = element_text(
         face = 'bold', 
         size = 18, 
         color = "#CB4C4E", hjust = 0.5), 
      plot.subtitle = element_text(
         size = 12, 
         face = 'bold', 
         hjust = 0.5),
      plot.margin = margin(8, 8, 8, 8),
      strip.text.x = element_blank(),
      axis.title.x=element_blank()
   ) +
   labs(title="MLB Club Run Differential, 2012-2022",
      subtitle = "",
      x ="", 
      y = "Run Differential") +
   facet_wrap(~team_abbr) +
   mlbplotR::geom_mlb_logos(aes(team_abbr = team_abbr), x = 2012, y = 275, width = 0.18)

