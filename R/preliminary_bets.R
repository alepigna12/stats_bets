library(dplyr)
data = read.csv("nba_betting_money_line.csv")
prob_data = data %>%
  na.omit() %>%
  mutate(prob1 = unlist(lapply(price1, function(x) implied_prob(create_odds_object(x)))),
         prob2 = unlist(lapply(price2, function(x) implied_prob(create_odds_object(x)))),
         prob1_novig = no_vig_prob(prob1, prob2)$home,
         prob2_novig = no_vig_prob(prob1, prob2)$away)
stats <- prob_data %>%
  group_by(game_id) %>%
  summarise(best_h = max(price1),
            best_a = max(price2),
            alpha = sum(prob1_novig),
            beta = sum(prob2_novig))
games = read.csv("nba_games_all.csv")
results = games %>% select(game_id, wl) %>%
  na.omit() %>%
  mutate(w = wl=="W")
data = merge(stats, games%>%select(game_id, w), by="game_id") %>%
  mutate(best_h = unlist(lapply(best_h, function(x) create_odds_object(x)$european)),
         best_a = unlist(lapply(best_a, function(x) create_odds_object(x)$european)))

#for arbitrage: a*best_h = (1-a)*best_a  > 1
# a(best_h+best_a) = best_a --> best_a/(best_h+best_a)

data %>% mutate(pstar = best_a/(best_h+best_a),
                estar = pstar * best_h - 1)%>%
  summarise(proportion_opportinity = mean(estar>0))

arbitrage_cases = data %>% mutate(pstar = best_a/(best_h+best_a),
                                  estar = pstar * best_h - 1)%>%
  filter(estar>0)

arbitrage_cases %>% summarise(mean(estar))
