library(worldfootballR)
library(ggplot2)
library(tidyverse)
library(ggsoccer)
library(extrafont)
library(showtext)
loadfonts(device = "win", quiet = TRUE)

#game id
ID <- fotmob_get_matches_by_date(20230331) %>%
  filter(primary_id==38) %>%
  select(match_id) 

# i is a number that represents which game's ID from a given day you want
ID <- as.numeric(ID$match_id)
ID <- ID[1]

#teams color, name and id

Team_info <- fotmob_get_season_stats(
  country = "AUT",
  league_name = "Bundesliga",
  season_name = "2022/2023",
  team_or_player = "team",
  stat_name = "Expected goals"
) %>%
   select(participant_name, team_id)

Team_info[Team_info=="SK Austria Klagenfurt"] = "Austria Klagenfurt"

Team_col <- c("Austria Klagenfurt"="#57184d", "WSG Tirol"="#009848", "Altach"="#000000", "Ried"="#dc4a4b",  
              "Wolfsberger AC"="#A4A4A4", "Hartberg"="#1888C0", "Austria Lustenau"="#445834", "Rapid Wien"="#01653D",  
              "LASK"="#F72CCB", "Austria Wien"="#484090", "Sturm Graz"="#474342", "Salzburg"="#0C2044")
Shape_Shots <- c("Gol"=19, "Pudło" =18, "Strzał obroniony" =15, "Obramowanie" =17)


#match data
match_det <- fotmob_get_match_details(ID) %>%
  filter(expected_goals!="NA", expected_goals>0.01)
match_shots <- match_det[15:41]
match_shots$H_A <- case_when(
  match_shots$team_id == match_det$home_team_id ~ "Home",
  TRUE ~ "Away")
match_shots$event_type[match_shots$event_type=="Goal"] <- "Gol"
match_shots$event_type[match_shots$event_type=="Miss"] <- "Pudło"
match_shots$event_type[match_shots$event_type=="AttemptSaved"] <- "Strzał obroniony"
match_shots$event_type[match_shots$event_type=="Post"] <- "Obramowanie"

#players xG
player_xG <- match_shots %>%
  select(player_name,
         expected_goals) %>%
  aggregate(expected_goals ~ player_name, sum) %>%
  filter(expected_goals>=0.01) %>%
  mutate(player_name = fct_reorder(player_name,expected_goals))

#creating a df with team id and team name
match_team_id <- match_shots %>%
 select(player_name,
        expected_goals,
        team_id) %>%
  filter(expected_goals>=0.01)
match_team_id$H_A <- case_when(
  match_team_id$team_id == match_det$home_team_id ~ "Home",
  TRUE ~ "Away")


match_team_id$dup <- duplicated(match_team_id$player_name)
match_team_id <- filter(match_team_id, dup=="FALSE")
match_team_id <- merge(match_team_id, Team_info, by="team_id", all=T)
match_team_id <- filter(match_team_id, player_name!="NA")


#merging 

player_xG <- merge(player_xG, match_team_id, by="player_name")
colnames(player_xG)[2] <- "expected_goals"
player_xG <- player_xG[-4:-5]
player_xG$team_id <- as.character(player_xG$team_id)


#players' xG bars

ggplot(data=player_xG, aes(x=expected_goals, y=player_name,fill= factor(participant_name))) +
  my_theme + theme(plot.title = element_text(hjust=.5, vjust=-0.5, family = "Roboto Slab", face = "bold", size=20)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=Team_col) +
  coord_cartesian(xlim=c(0,2.2),
                  expand=F,
                  clip="off") +
  guides(fill="none") +
  labs(title="",
       caption="źródło: FotMob | twórca: @AustriackaPilka") +
  xlab("xG") +
  ylab("")
#saving plot in Documents in "BundesReport" folder
DeLuciatoR::ggsave_fitmax(path="BundesReport", filename = "1xGBar.png", maxheight =8, 
                          maxwidth = 15)

#shotmap for home team

Home_Shots <- match_shots %>%
  filter(H_A=="Home",is_own_goal==F)

ggplot(Home_Shots) +
  annotate_pitch(dimensions = pitch_international, colour="black",
                 fill="#2B734C",
                 limits=F) +
  geom_point(Home_Shots, mapping = aes(x = x-5, y = y,
                                       color = expected_goals,
                                       shape = factor(event_type),
                                       size=7)) +
  my_theme + theme(panel.background = element_rect(fill="#DDC9B4"),
  ) + 
  theme_pitch() + theme(plot.margin = unit(c(0,0,0,0), "cm"),
                        plot.background = element_rect(fill="#DDC9B4"),
                        plot.caption = element_text(hjust=0.9,face="bold", family="Rubik", size=12)) +
  coord_flip(xlim = c(49, 101)) +
  scale_shape_manual(name = "Rezultat strzału", values = Shape_Shots) +
  guides(shape = guide_legend(override.aes = list(size = 4.5)),
         size="none") +
  scale_color_gradient(name="Gole spodziewane", low="#000066", high = "#ff0000", limits=c(0,1)) +
  labs(title=paste0("Mapa strzałów: ",filter(match_team_id,H_A=="Home")$participant_name),
       subtitle=paste0("Gole: ",sum(Home_Shots$event_type=="Gol"),
                       " | xG: ",round(sum(Home_Shots$expected_goals),digits=2)),
       caption = "źródło: FotMob | twórca: @AustriackaPilka",
       shape="Rezultat Strzału") + 
  annotate("text", x = 98, y = 5.5, label = paste0("Strzały: ",nrow(Home_Shots)), color = "#E6DADA", size=4.8) +
    annotate("text", x = 98, y = 61, label = paste0("xG/Strzał: ", round(sum(Home_Shots$expected_goals/nrow(Home_Shots)),digits=2)), 
           color = "#E6DADA", size=4.8)
DeLuciatoR::ggsave_fitmax(path="BundesReport", filename = "2HomeSM.png", maxheight =8, 
                          maxwidth = 15)

#shotmap for away team

Away_Shots <- match_shots %>%
  filter(H_A=="Away",is_own_goal==F)

ggplot(Away_Shots) +
  annotate_pitch(dimensions = pitch_international, colour="black",
                 fill="#2B734C",
                 limits=F) +
  geom_point(Away_Shots, mapping = aes(x = x-5, y = y,
                                       color = expected_goals,
                                       shape = factor(event_type),
                                       size=7)) +
  my_theme + theme(panel.background = element_rect(fill="#DDC9B4"),
  ) + 
  theme_pitch() + theme(plot.margin = unit(c(0,0,0,0), "cm"),
                        plot.background = element_rect(fill="#DDC9B4")) +
  coord_flip(xlim = c(49, 101)) +
  scale_color_gradient(name="Gole spodziewane", low="#000066", high = "#ff0000", limits=c(0,1)) +
  scale_shape_manual(name="Rezultat strzału", values = Shape_Shots) + 
  guides(shape = guide_legend(override.aes = list(size = 4.5)),
         size="none") +
  labs(title=paste0("Mapa strzałów: ", filter(match_team_id,H_A=="Away")$participant_name),
       subtitle=paste0("Bramki: ",sum(Away_Shots$event_type=="Gol"),
                       " | xG: ",round(sum(Away_Shots$expected_goals),digits=2)),
       caption = "źródło: FotMob | twórca: @AustriackaPilka") + 
  annotate("text", x = 98, y = 5.5, label = paste0("Strzały: ",nrow(Away_Shots)), color = "#E6DADA", size=4.8) +
  annotate("text", x = 98, y = 61, label = paste0("xG/Strzał: ", round(sum(Away_Shots$expected_goals/nrow(Away_Shots)),digits=2)), 
           color = "#E6DADA", size=4.8)
DeLuciatoR::ggsave_fitmax(path="BundesReport", filename = "3AwaySM.png", maxheight =8, 
                          maxwidth = 15)


#flowchart data organizing
flow_H <- Home_Shots %>%
  mutate(flow = cumsum(expected_goals),
         team = match_det$home_team[1]) %>%
  select(min,player_name,event_type,expected_goals,flow,team_id)
flow_H$team <- match_det$home_team[1]
H_r0 <- c(0,"none","none",0,0,match_det$home_team_id[1],match_det$home_team[1])
H_r1 <- c(92,"none","none",0,sum(flow_H$expected_goals),match_det$home_team_id[1],match_det$home_team[1])
flow_H <- rbind(flow_H,H_r0,H_r1)
flow_H$min <- as.numeric(flow_H$min)
flow_H$expected_goals <- as.numeric(flow_H$expected_goals)
flow_H$flow <- as.numeric(flow_H$flow)

flow_A <- Away_Shots %>%
  mutate(flow = cumsum(expected_goals),
         team = match_det$away_team[1]) %>%
  select(min,player_name,event_type,expected_goals,flow,team_id)
A_r0 <- c(0,"none","none",0,0,match_det$away_team_id[1],match_det$away_team[1])
A_r1 <- c(92,"none","none",0,sum(flow_A$expected_goals),match_det$away_team_id[1],match_det$away_team[1])
flow_A <- rbind(flow_A,A_r0,A_r1)
flow_A$min <- as.numeric(flow_A$min)
flow_A$expected_goals <- as.numeric(flow_A$expected_goals)
flow_A$flow <- as.numeric(flow_A$flow)


#flowchart graph
ggplot() +
  scale_color_manual(name="Drużyna",values =Team_col) +
  labs(title=paste0(filter(match_team_id,H_A=="Home")$participant_name, " vs ", filter(match_team_id,H_A=="Away")$participant_name, " flowchart ",
                    match_det$league_round_name[1]),
       subtitle=paste0("Zielona kropka = Gol"), caption="źródło: FotMob | twórca: @AustriackaPilka") +
  geom_step(data=flow_H, aes(x=min,y=flow, color=filter(match_team_id,H_A=="Home")$participant_name[1]),linewidth=1) +
  geom_point(data=flow_H, aes(x=min,y=flow), size=3.2) +
  geom_point(data=filter(flow_H,flow_H$event_type=="Gol"), 
             aes(x=min,y=flow), color="green", size=3.2) +
  geom_point(data=flow_H, aes(x=min,y=flow),shape=1, size=3.2, color="black") +
  geom_step(data=flow_A, aes(x=min,y=flow, color=filter(match_team_id,H_A=="Away")$participant_name[1]), linewidth=1) +
  geom_point(data=flow_A, aes(x=min,y=flow), size=3.2) +
  geom_point(data=filter(flow_A, flow_A$event_type=="Gol"), 
             aes(x=min,y=flow), color="green", size=3.2) +
  geom_point(data=flow_A, aes(x=min,y=flow),shape=1, size=3.2, color="black") +
  geom_vline(xintercept = 45, linewidth=1, color="black", linetype="dashed") +
  scale_x_continuous(breaks = seq(0,90,15)) +
  coord_cartesian(
    xlim=c(0,98),
    clip ="off",
    expand=F) +
  xlab("minuta") +
  ylab("xG") +
  my_theme + theme(legend.key.width = unit(1,"cm"), legend.position = c(0.1,1),
                   axis.title.x = element_text(hjust =0.485), axis.title.y = element_text(hjust =0.5),
                   )
DeLuciatoR::ggsave_fitmax(path="BundesReport", filename = "4flow.png", maxheight =8, 
                          maxwidth = 15)

