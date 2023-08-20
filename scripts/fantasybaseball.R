library(tidyverse)
library(rvest)


link <- "https://www.fangraphs.com/projections?pos=2b&stats=bat&type=zips&statgroup=fantasy"


pg <- read_html(link) %>% html_table() %>% .[[16]]

colnames(pg)

pg <- pg %>% 
  mutate(`BB%` = parse_number(`BB%`),
         `K%` = parse_number(`K%`))


zips.proj <- pg %>% 
  mutate(bb_k = `BB%`/`K%`,
         X1B = H - `2B` - `3B` - HR,
         PTS = 0.5*R + 1.5*X1B + 3*`2B` + 4.5*`3B` +
           6*HR + 1*RBI + 2*SB - 2*CS + 1.5*BB - 1.5*SO,
         pos_adp = rank(ADP),
         pts_rank = rank(-PTS))

zips.proj %>% 
  ggplot(aes(pos_adp, pts_rank, label = ifelse(pts_rank < pos_adp, Name, ""))) +
  geom_point() +
  geom_text(aes(hjust = 0, vjust= 0)) +
  geom_smooth(method = "lm")






batting <- read_csv("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/Batting.csv")
pitching <- read_csv("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/Pitching.csv")
people <- read_csv("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/People.csv")
fielding <- read_csv("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/Fielding.csv")


people %>% 
  mutate(name = paste(nameFirst, nameLast, sep = " ")) %>% 
  select(playerID, name) -> people


fielding %>% 
  group_by(playerID, yearID) %>% 
  filter(InnOuts == max(InnOuts)) %>% 
  ungroup() %>% 
  left_join(people, by = "playerID") %>% 
  select(playerID, name, yearID, POS, E) -> fielding



batting %>% 
  left_join(fielding, by = c("playerID", "yearID")) -> batting


batting %>% 
  janitor::clean_names() %>% 
  mutate(x1b = h-x2b-x3b-hr,
         pts = 0.5*r + 1.5*x1b + 3*x2b + 4.5*x3b + 6*hr + rbi + 2*sb -
           2*cs + 1.5*bb + 1.5*hbp - 1.5*so - 2*e) -> batting



batting %>% 
  filter(year_id == 2022,
         ab > 250,
         pos == "OF") %>% 
  mutate(ppg = pts/g) %>% 
  ggplot(aes(g, ppg, label = ifelse(ppg>=2.5, name, ""))) +
  geom_point() +
  geom_text()



pitching %>%
  left_join(people, by = "playerID") %>% 
  filter(yearID == 2022,
         IPouts >= 380) %>% 
  mutate(IP = IPouts/3,
         ip_per_start = IP/GS) %>% 
  arrange(desc(ip_per_start)) %>% 
  select(name, ip_per_start, GS) %>% 
  knitr::kable()


pitching %>% 
  janitor::clean_names() %>% 
  mutate(pts = 4*W - 4*L + 8*CG + 8*SHO + 6*SV +
           0.5*IPouts - 1.5 )















predict_ks <- function(name, opp, hand){
  
  if(hand == "l"){
    lineup <- lefties %>% 
      janitor::clean_names() %>% 
      filter(tm == opp)
  }
  
  else{
    lineup <- righties %>% 
      janitor::clean_names() %>% 
      filter(tm == opp)
  }
  
  a <- pitchers_collect2023 %>% 
    filter(pitcher_name == name) %>% 
    .[1,] %>% 
    mutate(playername = name)
  
  a <- cbind(a, lineup)
  
  return(a)
}



lefties <- read_csv("/Users/matthewwankiewicz/Documents/predictors/predictors/Splits Leaderboard Data vs LHP (1).csv")
righties <- read_csv("/Users/matthewwankiewicz/Documents/predictors/predictors/Splits Leaderboard Data vs RHP (1).csv")

merTools::predictInterval(model_so, predict_ks("Kevin Gausman", "HOU", "r"))

set.seed(21)
sim <- c()
for(i in 1:1000){
sim[i] <- qnorm(runif(1), mean = as.numeric(x), sd = 2.88) + qnorm(runif(1), mean = as.numeric(x), sd = 2.88)*.136
}

min(sim)


pitching_stats %>% 
  filter(Opp == "NYM",
         Date > "2023-01-01") %>% pull(P_SO) %>% mean()


righties %>% 
  janitor::clean_names()
