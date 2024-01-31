# Load data and dependencies ----------------------------------------------

library(ggplot2) # graphing
library(dplyr) # pipe and data manipulation
library(glue)

source('preprocess.R')

# dat <- preprocess('data/lichess_db_standard_rated_2014-07.pgn', writeIn = T,
#                   writeOut = T, outdir = 'data', nIter = 10)

dat <- read.csv('data/lichess_db_standard_rated_2014-07_processed.csv', stringsAsFactors = F)

# Clean data --------------------------------------------------------------

# remove draws and add elo groups
openings <- dat %>% 
  filter(WhiteWins != 0.5) %>% 
  mutate(RatingDiff = WhiteElo - BlackElo) # rating difference
openings$WhiteEloGroup <- cut(openings$WhiteElo, breaks = seq(0, 2000, by = 100), labels = F)
openings$BlackEloGroup <- cut(openings$BlackElo, breaks = seq(0, 2000, by = 100), labels = F)

# rating differences are not consistent across elos
openings %>% 
  ungroup() %>% 
  mutate(RatingDiff = WhiteElo - BlackElo) %>% 
  group_by(WhiteEloGroup) %>% 
  summarize(
    mean_diff = mean(RatingDiff)
  )

# Let's take relatively competitive games
summary(openings$RatingDiff)

# Let's cut out some outliers, let's just keep the main 95% of RatingDiff values
glue("Rows before cutting outliers: {nrow(openings)}")
lower_bound <- mean(openings$RatingDiff) - 1.96*sd(openings$RatingDiff)
upper_bound <- mean(openings$RatingDiff) + 1.96*sd(openings$RatingDiff)
openings <- openings %>% 
  filter(RatingDiff >= lower_bound & RatingDiff <= upper_bound)
glue("Rows after cutting outliers: {nrow(openings)}")

# retrieve top 10 openings
common_openings <- sort(table(dat$ECO), decreasing = TRUE)[1:10]

# number of rows from pre-openings-filtered data (for descriptives below)
nodraws <- nrow(openings)

# filter to top 10 openings
openings <- openings %>% 
  filter(ECO %in% rownames(common_openings)) # filter to top 10 openings

# Frequency of top 10 openings
openings %>% 
  group_by(ECO) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

# Let's look at the top 5, to keep analyses manageable
openings <- openings %>% 
  filter(ECO %in% rownames(common_openings)[1:5])

# Basic descriptives ------------------------------------------------------

# Information about data filtering process
glue("Original dataset length: {nrow(dat)}\n", 
     "Draws removed: {nrow(dat) - nodraws}\n",
     "The top five openings account for {nrow(openings)} games, which is {round((nrow(openings)/nodraws)*100)}% of the non-drawn games")

# raw probabiliities 
tapply(openings$WhiteWins, list(openings$ECO), mean, na.rm = T)

# dplyr
openings %>% 
  group_by(ECO) %>% 
  summarize(
    propW = mean(WhiteWins)
  )

# show elo ranges
all_elos <- data.frame(player = c(openings$White, openings$Black),
                       elo = c(openings$WhiteElo, openings$BlackElo))

all_elos %>% 
  ggplot(mapping = aes(x = elo)) +
  geom_histogram(colour = "black", fill = "black") +
  theme_bw() +
  labs(title = "Who's Playing?",
       x = "Elo", 
       y = "Count")
summary(all_elos)

# Win proportion by elo grouping ------------------------------------------

# overall win proportions, not accounting for rating
openings %>% 
  group_by(ECO) %>% 
  mutate(propW = sum(WhiteWins)/n()) %>% 
  ggplot(mapping = aes(x = ECO, y = propW)) +
  geom_point() +
  geom_hline(yintercept = 0.50, linetype = 2) +
  theme_bw() +
  labs(title = "Win Proportion for White by Opening across All Ratings",
       subtitle = "From 200,000+ lichess Games",
       x = "Opening ECO Code", 
       y = "Proportion of Games Won",
       caption = "ECO Codes: https://www.365chess.com/eco.php")

# calculate proportion of wins from white's perspective
wins <- openings %>% 
  group_by(WhiteEloGroup, ECO) %>% 
  mutate(propW = sum(WhiteWins)/n()) # with no draws, sum is number of games won by white, divide by total games

# win proportion by rating

wins %>% 
  ggplot(mapping = aes(x = WhiteEloGroup, y = propW)) +
  geom_point() +
  facet_wrap(~ECO, nrow = 5) +
  geom_hline(yintercept = 0.50, linetype = 2) +
  theme_bw() +
  labs(title = "Win Proportion by Opening by Rating",
       subtitle = "From 200,000+ lichess Games",
       x = "ELO",
       y = "Proportion of Games Won",
       caption = "ECO Codes: https://www.365chess.com/eco.php") +
  scale_x_continuous(breaks = seq(min(wins$WhiteEloGroup), max(wins$WhiteEloGroup), by = 1), #19 scale breaks
                     labels = c("1 - 100", "100 - 200", "200 - 300", "300 - 400", "400 - 500",
                                "500 - 600", "600 - 700", "700 - 800", "800 - 900", "900 - 1000",
                                "1000 - 1100", "1100 - 1200", "1200 - 1300", "1300 - 1400", "1400 - 1500",
                                "1500 - 1600", "1600 - 1700", "1700 - 1800", "1800 - 1900")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank())

# how does one graph look, if I want to toggle through them
wins %>% 
  filter(ECO == "A00") %>% 
  ggplot(mapping = aes(x = WhiteEloGroup, y = propW)) +
  geom_point() +
  geom_hline(yintercept = 0.50, linetype = 2) +
  facet_wrap(~ECO) +
  theme_bw() +
  labs(title = "Win Proportion by Opening by Rating",
       subtitle = "From 200,000+ lichess Games",
       x = "Elo",
       y = "Proportion of Games Won",
       caption = "ECO Codes: https://www.365chess.com/eco.php") +
  scale_x_continuous(breaks = seq(min(wins$WhiteEloGroup), max(wins$WhiteEloGroup), by = 1), #19 scale breaks
                     labels = c("1 - 100", "100 - 200", "200 - 300", "300 - 400", "400 - 500",
                                "500 - 600", "600 - 700", "700 - 800", "800 - 900", "900 - 1000",
                                "1000 - 1100", "1100 - 1200", "1200 - 1300", "1300 - 1400", "1400 - 1500",
                                "1500 - 1600", "1600 - 1700", "1700 - 1800", "1800 - 1900")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank())

# Note that not all elo ratings have the same number of games
wins %>% 
  group_by(ECO, WhiteEloGroup) %>% 
  summarize(n = n())

# Many of the 1500-and-up visualizations are based on only a few games, wouldn't trust them as much

# Table of Elo range, best white opening, best black opening (1-probW)
white_openings <- wins %>% 
  group_by(WhiteEloGroup, ECO) %>% 
  summarize(propW = mean(propW)) %>% 
  arrange(WhiteEloGroup, desc(propW)) %>% 
  filter(propW == max(propW))

# best black opening
black_openings <- wins %>% 
  group_by(WhiteEloGroup, ECO) %>% 
  summarize(propW = mean(propW)) %>% 
  arrange(WhiteEloGroup, desc(propW)) %>% 
  filter(propW == min(propW))

final <- left_join(white_openings, black_openings, by = "WhiteEloGroup", suffix = c(".white", ".black")) %>% 
  mutate(propW.black = 1 - propW.black)
final %>% 
  filter(WhiteEloGroup <= 14)

wins %>% 
  group_by(WhiteEloGroup, ECO) %>% 
  summarize(propW = mean(propW)) %>% 
  arrange(WhiteEloGroup, desc(propW)) %>% 
  mutate(propB = 1 - propW)

wins %>% 
  group_by(WhiteEloGroup, ECO) %>% 
  summarize(propW = mean(propW)) %>% 
  arrange(WhiteEloGroup, desc(propW)) %>% 
  mutate(propB = 1 - propW) %>% 
  filter(propW == max(propW) || propW == min(propW))