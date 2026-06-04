rm(list=ls())
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# install.packages(c("shiny", "ggplot2", "dplyr", "shinyWidgets",
#                    "jsonlite", "httr", "readr", "stringr",
#                    "tidyr", "tibble"))
library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(jsonlite)
library(httr)
library(sqldf)
library(readr)
library(stringr)
library(tidyr)
library(tibble)



#TRANSLATOR PENNSYLVANIA

CACHE_FILE <- "translation_cache.rds"
t_cache <- if (file.exists(CACHE_FILE)) readRDS(CACHE_FILE) else list()

translate <- function(text, lang) {
  if (lang == "" || is.na(text)) return(text)
  if (lang == "en" && !grepl("[^\\x00-\\x7F]", text, perl = TRUE)) return(text)
  key <- paste(text, lang)
  
  cached <- t_cache[[key]]
  if (!is.null(cached) && is.character(cached) && length(cached) == 1 && !is.na(cached)) {
    return(cached)
  }
  
  tryCatch({
    res <- GET("https://translate.googleapis.com/translate_a/single",
               query = list(client = "gtx", sl = "auto", tl = lang, dt = "t", q = text))
    out <- content(res)[[1]][[1]][[1]]
    
    if (!is.character(out) || length(out) != 1 || is.na(out)) return(text)
    
    t_cache[[key]] <<- out
    saveRDS(t_cache, CACHE_FILE)
    out
  }, error = function(e) text)
}

LanguageButtons <- div(style = "padding: 5px 15px; display: flex; align-items: center; gap: 5px;",
                    icon("globe", style = "font-size: 20px; margin-right: 5px;"),
                    actionButton("l_en", "English"),
                    actionButton("l_es", "Español"),
                    actionButton("l_ja", "日本語"),
                    actionButton("l_zh", "中文"),
                    actionButton("l_ko", "한국어")
)


#DATA LOADING PENNSYLVANIA

atbats <- read_csv("atbats_master.csv",
                   col_types = cols(pitcher_npb_id = col_character(),
                                    batter_npb_id  = col_character()))
atbats <- sqldf("select * from atbats where game_type = 'regular_season'")
players <- fromJSON("players.json")
logos_raw <- fromJSON("npb_logos.json")
league_standings <- read_csv("standings.csv")
SeasonBattingStats <- read_csv("batting_individual.csv")
SeasonPitchingStats <- read_csv("pitching_individual.csv")
playerinfo <- read_csv("rosters.csv")

SeasonPitchingStats <- SeasonPitchingStats %>%
  mutate(across(c(ip, era), ~ suppressWarnings(as.numeric(gsub("[^0-9.]", "", .x)))))

# Collapse duplicate stat rows (some players appear multiple times in source data)
SeasonBattingStats <- SeasonBattingStats %>%
  group_by(npb_id, player, team, league) %>%
  summarise(
    games   = sum(games,   na.rm = TRUE),
    pa      = sum(pa,      na.rm = TRUE),
    ab      = sum(ab,      na.rm = TRUE),
    runs    = sum(runs,    na.rm = TRUE),
    hits    = sum(hits,    na.rm = TRUE),
    doubles = sum(doubles, na.rm = TRUE),
    triples = sum(triples, na.rm = TRUE),
    hr      = sum(hr,      na.rm = TRUE),
    tb      = sum(tb,      na.rm = TRUE),
    rbi     = sum(rbi,     na.rm = TRUE),
    sb      = sum(sb,      na.rm = TRUE),
    cs      = sum(cs,      na.rm = TRUE),
    sh      = sum(sh,      na.rm = TRUE),
    sf      = sum(sf,      na.rm = TRUE),
    bb      = sum(bb,      na.rm = TRUE),
    ibb     = sum(ibb,     na.rm = TRUE),
    hbp     = sum(hbp,     na.rm = TRUE),
    so      = sum(so,      na.rm = TRUE),
    gidp    = sum(gidp,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    avg = ifelse(ab > 0, round(hits / ab, 3), 0),
    obp = ifelse((ab + bb + hbp + sf) > 0, round((hits + bb + hbp) / (ab + bb + hbp + sf), 3), 0),
    slg = ifelse(ab > 0, round(tb / ab, 3), 0)
  )

SeasonPitchingStats <- SeasonPitchingStats %>%
  group_by(npb_id, player, team, league) %>%
  summarise(
    games        = sum(games,        na.rm = TRUE),
    wins         = sum(wins,         na.rm = TRUE),
    losses       = sum(losses,       na.rm = TRUE),
    saves        = sum(saves,        na.rm = TRUE),
    holds        = sum(holds,        na.rm = TRUE),
    hold_points  = sum(hold_points,  na.rm = TRUE),
    cg           = sum(cg,           na.rm = TRUE),
    sho          = sum(sho,          na.rm = TRUE),
    no_bb_games  = sum(no_bb_games,  na.rm = TRUE),
    bf           = sum(bf,           na.rm = TRUE),
    ip           = sum(ip,           na.rm = TRUE),
    hits         = sum(hits,         na.rm = TRUE),
    hr           = sum(hr,           na.rm = TRUE),
    bb           = sum(bb,           na.rm = TRUE),
    ibb          = sum(ibb,          na.rm = TRUE),
    hbp          = sum(hbp,          na.rm = TRUE),
    so           = sum(so,           na.rm = TRUE),
    wp           = sum(wp,           na.rm = TRUE),
    bk           = sum(bk,           na.rm = TRUE),
    runs         = sum(runs,         na.rm = TRUE),
    er           = sum(er,           na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    win_pct = ifelse((wins + losses) > 0, round(wins / (wins + losses), 3), 0),
    era     = ifelse(ip > 0, round((er * 9) / ip, 2), 0)
  )



league_standings <- sqldf("select league, team_en, wins, losses, ties, case when games_behind = '--' then 0 else games_behind end games_behind from league_standings")


league_standings <- sqldf("select league, 

case when team_en = 'Hiroshima Toyo Carp' then 'Carp'  
      when team_en = 'Tokyo Yakult Swallows' then 'Swallows' 
         when team_en = 'Hanshin Tigers' then 'Tigers' 
             when team_en = 'Yomiuri Giants' then 'Giants' 
                 when team_en = 'Yokohama DeNA BayStars' then 'BayStars' 
                     when team_en = 'Chunichi Dragons' then 'Dragons' 
                         when team_en = 'Fukuoka SoftBank Hawks' then 'Hawks' 
                             when team_en = 'ORIX Buffaloes' then 'Buffaloes' 
                                 when team_en = 'Chiba Lotte Marines' then 'Marines' 
                                     when team_en = 'Tohoku Rakuten Golden Eagles' then 'Golden Eagles' 
                                         when team_en = 'Saitama Seibu Lions' then 'Lions' 
                                             when team_en = 'Hokkaido Nippon-Ham Fighters' then 'Fighters' else team_en end team_en,
wins, losses, ties, games_behind from league_standings
                                          ")

league_standings$games_behind <- as.numeric(league_standings$games_behind)


#League Logos PENNSYLVANIA
npb_logo     <- logos_raw$leagues$NPB$logo_uri
central_logo <- logos_raw$leagues$CentralLeague$logo_uri
pacific_logo <- logos_raw$leagues$PacificLeague$logo_uri

#Team Logos PENNSYLVANIA
teams <- list(
  DeNA         = list(logo = logos_raw$teams$DeNA$logo_uri),
  ヤクルト     = list(logo = logos_raw$teams$ヤクルト$logo_uri),
  中日         = list(logo = logos_raw$teams$中日$logo_uri),
  巨人         = list(logo = logos_raw$teams$巨人$logo_uri),
  広島         = list(logo = logos_raw$teams$広島$logo_uri),
  阪神         = list(logo = logos_raw$teams$阪神$logo_uri),
  オリックス   = list(logo = logos_raw$teams$オリックス$logo_uri),
  ソフトバンク = list(logo = logos_raw$teams$ソフトバンク$logo_uri),
  ロッテ       = list(logo = logos_raw$teams$ロッテ$logo_uri),
  日本ハム     = list(logo = logos_raw$teams$日本ハム$logo_uri),
  楽天         = list(logo = logos_raw$teams$楽天$logo_uri),
  西武         = list(logo = logos_raw$teams$西武$logo_uri)
)

#STuff for matching later PENNSYLVANIA
team_map <- c(
  "Yokohama_DeNA_BayStars"     = "DeNA",
  "Tokyo_Yakult_Swallows"      = "ヤクルト",
  "Chunichi_Dragons"           = "中日",
  "Yomiuri_Giants"             = "巨人",
  "Hiroshima_Toyo_Carp"        = "広島",
  "Hanshin_Tigers"             = "阪神",
  "ORIX_Buffaloes"             = "オリックス",
  "Fukuoka_SoftBank_Hawks"     = "ソフトバンク",
  "Chiba_Lotte_Marines"        = "ロッテ",
  "Hokkaido_NipponHam_Fighters"= "日本ハム",
  "Tohoku_Rakuten_Eagles"      = "楽天",
  "Saitama_Seibu_Lions"        = "西武"
)


team_to_full_jp <- c(
  "DeNA"         = "横浜DeNAベイスターズ",
  "ヤクルト"     = "東京ヤクルトスワローズ",
  "中日"         = "中日ドラゴンズ",
  "巨人"         = "読売ジャイアンツ",
  "広島"         = "広島東洋カープ",
  "阪神"         = "阪神タイガース",
  "オリックス"   = "オリックス・バファローズ",
  "ソフトバンク" = "福岡ソフトバンクホークス",
  "ロッテ"       = "千葉ロッテマリーンズ",
  "日本ハム"     = "北海道日本ハムファイターズ",
  "楽天"         = "東北楽天ゴールデンイーグルス",
  "西武"         = "埼玉西武ライオンズ"
)


# Join player photos PENNSYLVANIA
player_cols <- players %>% select(npb_id, name_en = name, name_ja, photo_b64, photo_url)

atbats <- atbats %>%
  left_join(player_cols, by = c("pitcher_npb_id" = "npb_id")) %>%
  rename(pitcher_name_en = name_en, pitcher_name_ja = name_ja,
         pitcher_photo_b64 = photo_b64, pitcher_photo_url = photo_url) %>%
  left_join(player_cols, by = c("batter_npb_id" = "npb_id")) %>%
  rename(batter_name_en = name_en, batter_name_ja = name_ja,
         batter_photo_b64 = photo_b64, batter_photo_url = photo_url)

# LOGOS INTO TABLE TO JOIN PENNSYLVANIA
logos <- logos_raw$teams %>%
  bind_rows(.id = "team_key") %>%
  select(team_key, team_name_en = name_en, league, logo_base64, logo_uri)

# Join logos for home and away teams PENNSYLVANIA
atbats <- atbats %>%
  left_join(logos, by = c("home_team" = "team_key")) %>%
  rename(home_logo_b64 = logo_base64, home_logo_uri = logo_uri,
         home_team_en = team_name_en, home_league = league) %>%
  left_join(logos, by = c("away_team" = "team_key")) %>%
  rename(away_logo_b64 = logo_base64, away_logo_uri = logo_uri,
         away_team_en = team_name_en, away_league = league)



#DATA MANIPULATION PENNSYLVANIA

atbats$speed_kph <- gsub('[km/h]', '', atbats$speed_kph)

atbats$pitcher_name <- gsub('#[0-9.]+', '', atbats$pitcher_name)

atbats$batter_name <- gsub('#[0-9.]+', '', atbats$batter_name)

atbats$ball_type <- factor(atbats$ball_type)

atbats$speed_kph <- as.numeric(atbats$speed_kph)


#Unit conversions for later PENNSYLVANIA

to_mph    <- function(kph) kph * 0.621371
ft_to_m   <- function(ft)  ft * 0.3048
cm_to_in  <- function(cm)  cm / 2.54
kg_to_lb  <- function(kg)  kg * 2.20462

convert_speed       <- function(kph, u) if (u == "imperial") round(to_mph(kph), 1) else round(kph, 1)
convert_distance_ft <- function(ft, u)  if (u == "metric")   round(ft_to_m(ft), 2) else round(ft, 2)

speed_label    <- function(u) if (u == "imperial") "mph" else "km/h"
distance_label <- function(u) if (u == "metric")   "m"   else "ft"
height_label   <- function(u) if (u == "imperial") "in"  else "cm"
weight_label   <- function(u) if (u == "imperial") "lbs" else "kg"

# Name display PENNSYLVANIA
get_display_name <- function(name_ja, lang) {
  if (lang == "ja") return(name_ja)
  en <- players$name[players$name_ja == name_ja][1]
  if (is.na(en)) name_ja else en
}

#Player names for dropdown selection PENNSYLVANIA

pitcher_choices <- atbats %>%
  filter(!is.na(pitcher_photo_b64)) %>%
  left_join(players %>% select(npb_id, team, number), by = c("pitcher_npb_id" = "npb_id")) %>%
  mutate(team = team_map[team], number = as.numeric(number)) %>%
  distinct(pitcher_name, pitcher_photo_b64, team, number) %>%
  arrange(number)

batter_choices <- atbats %>%
  filter(!is.na(batter_photo_b64)) %>%
  left_join(players %>% select(npb_id, team, number), by = c("batter_npb_id" = "npb_id")) %>%
  mutate(team = team_map[team], number = as.numeric(number)) %>%
  distinct(batter_name, batter_photo_b64, team, number) %>%
  arrange(number)

#Stats to display on card PENNSYLVANIA
pitchercard_stat_display <- c("era", "games", "wins", "losses", "saves", "holds",
                   "ip", "so", "bb", "hits", "hr")

battercard_stat_display <- c("games", "avg", "obp", "slg", "hits", "rbi", "sb", "bb",
                              "doubles", "triples", "hr")


#UI PENNSYLVANIA

ui <- uiOutput("main_ui")

#SERVER PENNSYLVANIA

server <- function(input, output, session) {
  

#Translation  PENNSYLVANIA
  lang <- reactiveVal("en")
  observeEvent(input$l_en, { lang("en") })
  observeEvent(input$l_es, { lang("es") })
  observeEvent(input$l_ja, { lang("ja") })
  observeEvent(input$l_zh, { lang("zh") })
  observeEvent(input$l_ko, { lang("ko") })
  
#Unit Conversion PENNSYLVANIA
  
  units <- reactiveVal("metric")
  observeEvent(input$unit_imperial, { units("imperial") })
  observeEvent(input$unit_metric,   { units("metric") })
  
#Tab selection  PENNSYLVANIA
  current_tab <- reactiveVal("tab1")
  observeEvent(input$tabs, { current_tab(input$tabs) })

#Team Filters PENNSYLVANIA 
  selected_teams <- reactiveVal(NULL)
  observeEvent(input$reset_filters, { selected_teams(NULL) })
  observeEvent(input$central_league, { selected_teams(c("DeNA", "ヤクルト", "中日", "巨人", "広島", "阪神")) })
  observeEvent(input$pacific_league, { selected_teams(c("オリックス", "ソフトバンク", "ロッテ", "日本ハム", "楽天", "西武")) })
  observeEvent(input$team_DeNA, { selected_teams("DeNA") })
  observeEvent(input$team_yakult, { selected_teams("ヤクルト") })
  observeEvent(input$team_chunichi, { selected_teams("中日") })
  observeEvent(input$team_giants, { selected_teams("巨人") })
  observeEvent(input$team_carp, { selected_teams("広島") })
  observeEvent(input$team_tigers, { selected_teams("阪神") })
  observeEvent(input$team_orix, { selected_teams("オリックス") })
  observeEvent(input$team_hawks, { selected_teams("ソフトバンク") })
  observeEvent(input$team_marines, { selected_teams("ロッテ") })
  observeEvent(input$team_fighters, { selected_teams("日本ハム") })
  observeEvent(input$team_eagles, { selected_teams("楽天") })
  observeEvent(input$team_lions, { selected_teams("西武") })
  
  # PITCH SELECTION PITCHER TAB PENNSYLVANIA
  observeEvent(input$p_pitch_all, {
    req(input$pitcher)
    pitches <- atbats %>%
      filter(pitcher_name == input$pitcher, !is.na(ball_type)) %>%
      pull(ball_type) %>% unique() %>% as.character()
    updateCheckboxGroupButtons(session, "p_pitch_select", selected = pitches)
  })
  
  observeEvent(input$p_pitch_none, {
    updateCheckboxGroupButtons(session, "p_pitch_select", selected = character(0))
  })
  
  # PITCH SELECTION BATTER TAB PENNSYLVANIA
  observeEvent(input$b_pitch_all, {
    req(input$batter)
    pitches <- atbats %>%
      filter(batter_name == input$batter, !is.na(ball_type)) %>%
      pull(ball_type) %>% unique() %>% as.character()
    updateCheckboxGroupButtons(session, "b_pitch_select", selected = pitches)
  })
  
  observeEvent(input$b_pitch_none, {
    updateCheckboxGroupButtons(session, "b_pitch_select", selected = character(0))
  })
  

  # PLAYER FILTER PENNSYLVANIA
  filtered_pitchers <- reactive({
    t <- selected_teams()
    if (is.null(t)) pitcher_choices else pitcher_choices[pitcher_choices$team %in% t, ]
  })
  
  filtered_batters <- reactive({
    t <- selected_teams()
    if (is.null(t)) batter_choices else batter_choices[batter_choices$team %in% t, ]
  })
  
 


  
    
  #Main UI building PENNSYLVANIA
  
  output$main_ui <- renderUI({
    l = lang()
    
    fp <- filtered_pitchers()
    p_display <- sapply(fp$pitcher_name, get_display_name, l)
    p_choices <- setNames(fp$pitcher_name, paste0("#", fp$number, " ", p_display))
    
    fb <- filtered_batters()
    b_display <- sapply(fb$batter_name, get_display_name, l)
    b_choices <- setNames(fb$batter_name, paste0("#", fb$number, " ", b_display))
    
    
    AppHeader <- div(
      style = "display:flex; align-items:center; gap:12px; padding:8px 16px;",
      
      div(style = "display:flex; flex-direction:column; gap:4px;",
          LanguageButtons,
          div(style = "display:flex; align-items:center; gap:6px;",
              actionButton("unit_imperial", translate("Imperial", l), icon = icon("ruler"), class = "btn-sm btn-default"),
              actionButton("unit_metric", translate("Metric", l), icon = icon("ruler"), class = "btn-sm btn-default")
          )
      ),
      
      actionButton("reset_filters",
                   label = tags$img(src = npb_logo, height = "100px"),
                   class = "btn-sm", style = "align-self:stretch; padding:4px;"),
      
      div(style = "display:flex; flex-direction:column; gap:4px;",
          div(style = "display:flex; align-items:center; gap:6px;",
              actionButton("central_league", label = tags$img(src = central_logo, height = "50px"), class = "btn-sm"),
              actionButton("team_DeNA", label = tags$img(src = teams$DeNA$logo, height = "50px"), class = "btn-sm"),
              actionButton("team_yakult", label = tags$img(src = teams$ヤクルト$logo, height = "50px"), class = "btn-sm"),
              actionButton("team_chunichi", label = tags$img(src = teams$中日$logo, height = "50px"), class = "btn-sm"),
              actionButton("team_giants", label = tags$img(src = teams$巨人$logo, height = "50px"), class = "btn-sm"),
              actionButton("team_carp", label = tags$img(src = teams$広島$logo, height = "50px"), class = "btn-sm"),
              actionButton("team_tigers", label = tags$img(src = teams$阪神$logo, height = "50px"), class = "btn-sm")
          ),
          div(style = "display:flex; align-items:center; gap:6px;",
              actionButton("pacific_league", label = tags$img(src = pacific_logo, height = "50px"), class = "btn-sm"),
              actionButton("team_orix", label = tags$img(src = teams$オリックス$logo, height = "50px"), class = "btn-sm"),
              actionButton("team_hawks", label = tags$img(src = teams$ソフトバンク$logo, height = "50px"), class = "btn-sm"),
              actionButton("team_marines", label = tags$img(src = teams$ロッテ$logo, height = "50px"), class = "btn-sm"),
              actionButton("team_fighters", label = tags$img(src = teams$日本ハム$logo, height = "50px"), class = "btn-sm"),
              actionButton("team_eagles", label = tags$img(src = teams$楽天$logo, height = "50px"), class = "btn-sm"),
              actionButton("team_lions", label = tags$img(src = teams$西武$logo, height = "50px"), class = "btn-sm")
          )
      )
    )  
    
    
    navbarPage(
      translate("Baseball App", l),
      id = "tabs", 
      selected = current_tab(),
      header = AppHeader,
      

      
#PAGE ONE PENNSYLVANIA
            
            
    tabPanel(translate("Home Page", l), value = "tab1",
             sidebarLayout(
               sidebarPanel(width = 3,
                            h2(translate("League Standings", l)),
                            h3(translate("Central League", l)),
                            tableOutput("central_table"),
                            h3(translate("Pacific League", l)),
                            tableOutput("pacific_table")
                            ),
               mainPanel(width = 9,
                         uiOutput("leaders_title"),
                         fluidRow(
                           column(2, uiOutput("ba_leader")),
                           column(2, uiOutput("hr_leader")),
                           column(2, uiOutput("rbi_leader")),
                           column(2, uiOutput("hit_leader")),
                           column(2, uiOutput("sb_leader")),
                         ),
                         fluidRow(
                           column(2, uiOutput("win_leader")),
                           column(2, uiOutput("era_leader")),
                           column(2, uiOutput("saves_leader")),
                           column(2, uiOutput("strikeout_leader")),
                           column(2, uiOutput("inning_leader")),
                         )
                         )
             )
             
             
             
             ),




#PAGE TWO PENNSYLVANIA



tabPanel(translate("Pitcher Statistics", l), value = "tab2",
         sidebarLayout(
           sidebarPanel(width = 2,
                        h3(translate("Pitcher Selection", l)),
                        selectInput("pitcher", translate("Pitcher", l), choices = p_choices),
                        hr(),
                        h4(translate("Filters", l)),
                        checkboxGroupButtons("p_batter_hand", translate("Batter Hand", l),
                                             choiceNames  = c(translate("vs RHB", l), translate("vs LHB", l)),
                                             choiceValues = c("右打", "左打"),
                                             selected     = c("右打", "左打"),
                                             individual = TRUE),
                        uiOutput("pitch_picker_ui")
           ),
           mainPanel(width = 9,
                     h4(translate("Pitcher Card", l)),
                     uiOutput("pitcher_card"),
                     fluidRow(style = "display: flex; align-items: flex-start;",
                              column(2, tableOutput("pitcher_pitch_table")),
                              column(10, plotOutput("pitcher_pitch_plot", height = "650px"))
                     )
           )
         )
),





#PAGE 3 PENNSYLVANIA




tabPanel(translate("Batter Statistics", l), value = "tab3",
         sidebarLayout(
           sidebarPanel(width = 2,
                        h3(translate("Batter Selection", l)),
                        selectInput("batter", translate("Batter", l), choices = b_choices),
                        hr(),
                        h4(translate("Filters", l)),
                        checkboxGroupButtons("b_pitcher_hand", translate("Pitcher Hand", l),
                                             choiceNames  = c(translate("vs RHP", l), translate("vs LHP", l)),
                                             choiceValues = c("右投", "左投"),
                                             selected     = c("右投", "左投"),
                                             individual = TRUE),
                        uiOutput("batter_pitch_picker_ui")
           ),
           mainPanel(width = 10,
                     h4(translate("Batter Card", l)),
                     uiOutput("batter_card"),
                     fluidRow(style = "display: flex; align-items: flex-start;",
                              column(2, tableOutput("batter_pitch_table")),
                              column(10, plotOutput("batter_heatmap", height = "650px"))
                     )
           )
         )
),




#PAGE 4 PENNSYLVANIA



tabPanel(translate("Application Information", l), value = "tab4",
         h2(translate("What is the purpose of this app?", l)),
         h4(translate("Data in baseball has exponentially risen in the past quarter century. 
                      This app tackles the overwhelming masses of data by putting information into a concise, 
                      visual format.", l)),
         
         h2(translate("How to Use", l)),
         h4(translate("Use the language buttons in the header to switch between English, Spanish, Japanese, Chinese, and Korean.", l)),
         h4(translate("Toggle Imperial or Metric to change units throughout the app.", l)),
         h4(translate("Click team logos in the header to filter players and stat leaders by team. Click the NPB logo to reset.", l)),
         h4(translate("Click league logos to filter to all 6 teams in the Central or Pacific League.", l)),
         
         h2(translate("Home Page", l)),
         h4(translate("Shows current league standings and the top performers in 10 statistical categories.", l)),
         
         h2(translate("Pitcher Statistics", l)),
         h4(translate("Select a pitcher to see their bio, season stats, pitch locations on the strike zone, and pitch type breakdown. Filter by batter handedness or pitch types in the sidebar.", l)),
         
         h2(translate("Batter Statistics", l)),
         h4(translate("Select a batter to see their bio, season stats, and a 13-zone batting average heatmap. Red zones are strengths, blue zones are weaknesses. The side table breaks down batting average by pitch type.", l)),
         
         h2(translate("Data Source", l)),
         h4(translate("Data scraped from Yahoo Japan NPB pitch-by-pitch records. Includes all 12 NPB teams across the Central and Pacific leagues.", l)),
         h4(translate("Last updated: May 1, 2026", l))
         
         
)
    )
  })
  
  
  
  
  
#PITCHER TAB ADDITIONS
  
  #PITCHERR CARD


  output$pitcher_card <- renderUI({
    req(input$pitcher)
    l <- lang()
    u <- units()
    
    bio   <- playerinfo[playerinfo$player == input$pitcher, ]
    photo <- players[players$name_ja == input$pitcher, ]
    stats <- SeasonPitchingStats[SeasonPitchingStats$player == input$pitcher, ]
    
    img_src   <- if (nrow(photo) > 0) photo$photo_url[1] else "https://via.placeholder.com/120x150"
    stat_cols <- intersect(pitchercard_stat_display, names(stats))
    
    fluidRow(
      column(2,
             tags$img(src = img_src, width = "120px"),
             tags$p(tags$strong(get_display_name(input$pitcher, l))),
             if (nrow(bio) > 0) tags$small(paste0("#", bio$number[1], " | ", translate(bio$team[1], l)))
      ),
      column(2,
             if (nrow(bio) > 0) {
               height_val <- if (u == "imperial") round(cm_to_in(bio$height_cm[1]), 1) else bio$height_cm[1]
               weight_val <- if (u == "imperial") round(kg_to_lb(bio$weight_kg[1]), 0) else bio$weight_kg[1]
               throws_full <- switch(bio$throws_en[1], "R" = "Right Handed", "L" = "Left Handed", bio$throws_en[1])
               bats_full   <- switch(bio$bats_en[1],   "R" = "Right Handed", "L" = "Left Handed", "S" = "Switch Hitter", bio$bats_en[1])
               tagList(
                 tags$p(tags$small(
                   tags$strong(translate("Age", l)), ": ", bio$age[1], tags$br(),
                   tags$strong(translate("Height", l)), ": ", height_val, " ", height_label(u), tags$br(),
                   tags$strong(translate("Weight", l)), ": ", weight_val, " ", weight_label(u), tags$br(),
                   tags$strong(translate("Throws", l)), ": ", translate(throws_full, l), tags$br(),
                   tags$strong(translate("Bats", l)), ": ", translate(bats_full, l), tags$br(),
                   tags$strong(translate("Position", l)), ": ", translate(bio$position_group_en[1], l)
                 ))
               )
             } else {
               tags$p(translate("No bio info available", l))
             }
      ),
      column(8,
             if (nrow(stats) > 0) {
               tagList(
                 tags$p(tags$small(tags$strong(translate("Season Pitching Stats", l)))),
                 fluidRow(
                   tagList(lapply(stat_cols, function(s) {
                     column(1,
                            tags$p(style = "margin-bottom: 0; font-size: 16px;", tags$strong(stats[[s]][1])),
                            tags$small(toupper(s))
                     )
                   }))
                 )
               )
             } else {
               tags$p(translate("No stats available", l))
             }
      )
    )
  })
  
  #PITCH SELECTIONS
  
  output$pitch_picker_ui <- renderUI({
    req(input$pitcher)
    l <- lang()
    pitches <- atbats %>%
      filter(pitcher_name == input$pitcher, !is.na(ball_type)) %>%
      count(ball_type, sort = TRUE) %>%
      pull(ball_type) %>% as.character()
    
    pitch_labels <- vapply(pitches, function(x) {
      out <- translate(x, l)
      if (!is.character(out) || length(out) != 1) x else out
    }, character(1))
    
    tagList(
      tags$label(translate("Pitch Types", l), style = "font-weight: bold; display: block; margin-bottom: 4px;"),
      div(style = "display: flex; gap: 4px; margin-bottom: 6px;",
          actionButton("p_pitch_all",  translate("All", l),  class = "btn-xs btn-default"),
          actionButton("p_pitch_none", translate("None", l), class = "btn-xs btn-default")
      ),
      checkboxGroupButtons("p_pitch_select", label = NULL,
                           choiceNames  = unname(pitch_labels),
                           choiceValues = pitches,
                           selected     = pitches,
                           direction    = "vertical",
                           individual   = TRUE,
                           checkIcon    = list(yes = icon("check")))
    )
  })
  
  
  
  pitcher_filtered <- reactive({
    if (is.null(input$pitcher) || is.null(input$p_pitch_select) || is.null(input$p_batter_hand)) {
      return(atbats[0, ])  # empty df with same columns
    }
    atbats %>%
      filter(pitcher_name == input$pitcher,
             ball_type    %in% input$p_pitch_select,
             batter_hand  %in% input$p_batter_hand,
             !is.na(pitch_result))
  })
  
  #PITCH PLOT PENNSYLVANIA
  
  output$pitcher_pitch_plot <- renderPlot({
    l <- lang()
    u <- units()
    d <- pitcher_filtered() %>% filter(!is.na(pitch_loc_x), !is.na(pitch_loc_z))
    d_n <- nrow(d)
    shiny::validate(shiny::need(is.numeric(d_n) && d_n > 0, translate("No pitches match.", l)))
    
    unique_pitches <- unique(as.character(d$ball_type))
    pitch_lookup <- vapply(unique_pitches, function(x) {
      out <- translate(x, l)
      if (!is.character(out) || length(out) != 1) x else out
    }, character(1))
    names(pitch_lookup) <- unique_pitches
    
    d$x <- convert_distance_ft(d$pitch_loc_x, u)
    d$z <- convert_distance_ft(d$pitch_loc_z, u)
    d$pitch_label <- as.character(pitch_lookup[as.character(d$ball_type)])
    
    # Strike zone
    sz_left   <- convert_distance_ft(-1.30, u)
    sz_right  <- convert_distance_ft( 1.30, u)
    sz_bottom <- convert_distance_ft( 1.50, u)
    sz_top    <- convert_distance_ft( 3.50, u)
    v_lines   <- convert_distance_ft(c(-0.43, 0.43), u)
    h_lines   <- convert_distance_ft(c( 2.17, 2.83), u)
    
    # Home plate 
    plate_y_back  <- convert_distance_ft(-0.30, u)
    plate_y_mid   <- convert_distance_ft(-0.10, u)
    plate_y_front <- convert_distance_ft( 0.05, u)
    plate <- data.frame(
      x = c(sz_left, sz_left, 0, sz_right, sz_right, sz_left),
      y = c(plate_y_back, plate_y_mid, plate_y_front, plate_y_mid, plate_y_back, plate_y_back)
    )
    

    xlim_v <- convert_distance_ft(c(-2.5, 2.5), u)
    ylim_v <- convert_distance_ft(c(-0.6, 5), u)
    
    ggplot(d, aes(x = x, y = z, color = pitch_label)) +
      annotate("rect", xmin = sz_left, xmax = sz_right,
               ymin = sz_bottom, ymax = sz_top,
               fill = NA, color = "black", linewidth = 1.2) +
      annotate("segment", x = v_lines, xend = v_lines,
               y = sz_bottom, yend = sz_top,
               linetype = "dashed", color = "gray60") +
      annotate("segment", x = sz_left, xend = sz_right,
               y = h_lines, yend = h_lines,
               linetype = "dashed", color = "gray60") +
      geom_polygon(data = plate, aes(x = x, y = y),
                   inherit.aes = FALSE,
                   fill = "white", color = "black", linewidth = 0.8) +
      geom_point(alpha = 0.7, size = 2.5) +
      coord_fixed(xlim = xlim_v, ylim = ylim_v) +
      labs(x = paste0(translate("Horizontal", l), " (", distance_label(u), ")"),
           y = paste0(translate("Height", l), " (", distance_label(u), ")"),
           color = translate("Pitch Type", l)) +
theme_minimal(base_size = 14) +
theme(legend.position = "bottom",
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 14, face = "bold"),
      legend.key.size = unit(1.2, "cm"),
      axis.title = element_text(size = 13),
      plot.margin = margin(0, 5, 5, 5))
  })
  
#PITCH USAGE TABLE
  
  output$pitcher_pitch_table <- renderTable({
    l <- lang()
    u <- units()
    d <- pitcher_filtered()
    d_n <- nrow(d)
    shiny::validate(shiny::need(is.numeric(d_n) && d_n > 0, translate("No pitches match.", l)))
    
    total <- nrow(d)
    
    tbl <- d %>%
      group_by(ball_type) %>%
      summarise(
        Usage  = round(n() / total * 100, 1),
        AvgSpd = round(convert_speed(mean(speed_kph, na.rm = TRUE), u), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(Usage))

    unique_pitches <- unique(as.character(tbl$ball_type))
    pitch_lookup <- vapply(unique_pitches, function(x) {
      out <- translate(x, l)
      if (!is.character(out) || length(out) != 1) x else out
    }, character(1))
    names(pitch_lookup) <- unique_pitches
    
    tbl$Pitch <- as.character(pitch_lookup[as.character(tbl$ball_type)])
    tbl <- tbl %>% select(Pitch, Usage, AvgSpd)
    
    new_names <- vapply(c("Pitch", "Usage %", paste("Avg", speed_label(u))),
                        function(x) {
                          out <- translate(x, l)
                          if (!is.character(out) || length(out) != 1) x else out
                        }, character(1))
    colnames(tbl) <- new_names
    tbl
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)

  
  
#BATTER PAGE STUFF
  
  #BATTER CARD
  
  output$batter_card <- renderUI({
    req(input$batter)
    l <- lang()
    u <- units()
    
    bio   <- playerinfo[playerinfo$player == input$batter, ]
    photo <- players[players$name_ja == input$batter, ]
    stats <- SeasonBattingStats[SeasonBattingStats$player == input$batter, ]
    
    img_src   <- if (nrow(photo) > 0) photo$photo_url[1] else "https://via.placeholder.com/120x150"
    stat_cols <- intersect(battercard_stat_display, names(stats))
    
    fluidRow(
      column(2,
             tags$img(src = img_src, width = "120px"),
             tags$p(tags$strong(get_display_name(input$batter, l))),
             if (nrow(bio) > 0) tags$small(paste0("#", bio$number[1], " | ", translate(bio$team[1], l)))
      ),
      column(2,
             if (nrow(bio) > 0) {
               height_val <- if (u == "imperial") round(cm_to_in(bio$height_cm[1]), 1) else bio$height_cm[1]
               weight_val <- if (u == "imperial") round(kg_to_lb(bio$weight_kg[1]), 0) else bio$weight_kg[1]
               throws_full <- switch(bio$throws_en[1], "R" = "Right Handed", "L" = "Left Handed", bio$throws_en[1])
               bats_full   <- switch(bio$bats_en[1],   "R" = "Right Handed", "L" = "Left Handed", "S" = "Switch Hitter", bio$bats_en[1])
               tagList(
                 tags$p(tags$small(
                   tags$strong(translate("Age", l)), ": ", bio$age[1], tags$br(),
                   tags$strong(translate("Height", l)), ": ", height_val, " ", height_label(u), tags$br(),
                   tags$strong(translate("Weight", l)), ": ", weight_val, " ", weight_label(u), tags$br(),
                   tags$strong(translate("Throws", l)), ": ", translate(throws_full, l), tags$br(),
                   tags$strong(translate("Bats", l)), ": ", translate(bats_full, l), tags$br(),
                   tags$strong(translate("Position", l)), ": ", translate(bio$position_group_en[1], l)
                 ))
               )
             } else {
               tags$p(translate("No bio info available", l))
             }
      ),
      column(8,
             if (nrow(stats) > 0) {
               tagList(
                 tags$p(tags$small(tags$strong(translate("Season Batting Stats", l)))),
                 fluidRow(
                   tagList(lapply(stat_cols, function(s) {
                     column(1,
                            tags$p(style = "margin-bottom: 0; font-size: 16px;", tags$strong(stats[[s]][1])),
                            tags$small(toupper(s))
                     )
                   }))
                 )
               )
             } else {
               tags$p(translate("No stats available", l))
             }
      )
    )
  })
  
  
#BATTER INFO BY PITCH TYPE
  
  output$batter_pitch_picker_ui <- renderUI({
    req(input$batter)
    l <- lang()
    pitches <- atbats %>%
      filter(batter_name == input$batter, !is.na(ball_type)) %>%
      count(ball_type, sort = TRUE) %>%
      pull(ball_type) %>% as.character()
    
    pitch_labels <- vapply(pitches, function(x) {
      out <- translate(x, l)
      if (!is.character(out) || length(out) != 1) x else out
    }, character(1))
    
    tagList(
      tags$label(translate("Pitch Types", l), style = "font-weight: bold; display: block; margin-bottom: 4px;"),
      div(style = "display: flex; gap: 4px; margin-bottom: 6px;",
          actionButton("b_pitch_all",  translate("All", l),  class = "btn-xs btn-default"),
          actionButton("b_pitch_none", translate("None", l), class = "btn-xs btn-default")
      ),
      checkboxGroupButtons("b_pitch_select", label = NULL,
                           choiceNames  = unname(pitch_labels),
                           choiceValues = pitches,
                           selected     = pitches,
                           direction    = "vertical",
                           individual   = TRUE,
                           checkIcon    = list(yes = icon("check")))
    )
  })
  
  batter_filtered <- reactive({
    if (is.null(input$batter) || is.null(input$b_pitch_select) || is.null(input$b_pitcher_hand)) {
      return(atbats[0, ])
    }
    atbats %>%
      filter(batter_name  == input$batter,
             ball_type    %in% input$b_pitch_select,
             pitcher_hand %in% input$b_pitcher_hand,
             !is.na(pitch_zone),
             !is.na(pitch_result),
             is_last_pitch == TRUE)
  })
  
  #BATTER SUCCESS HEATMAP
  
  output$batter_heatmap <- renderPlot({
    l <- lang()
    u <- units()
    d <- batter_filtered()
    d_n <- nrow(d)
    shiny::validate(shiny::need(is.numeric(d_n) && d_n >= 5, translate("Need at least 5 at-bats.", l)))
    
    zone_rects <- tibble::tribble(
      ~zone, ~xmin,  ~xmax, ~ymin, ~ymax,
      1L, -1.30, -0.43, 2.83, 3.50,   2L, -0.43, 0.43, 2.83, 3.50,
      3L,  0.43,  1.30, 2.83, 3.50,   4L, -1.30,-0.43, 2.17, 2.83,
      5L, -0.43,  0.43, 2.17, 2.83,   6L,  0.43, 1.30, 2.17, 2.83,
      7L, -1.30, -0.43, 1.50, 2.17,   8L, -0.43, 0.43, 1.50, 2.17,
      9L,  0.43,  1.30, 1.50, 2.17,  11L, -1.30, 1.30, 3.50, 4.50,
      12L,-2.50, -1.30, 1.50, 3.50,  13L,  1.30, 2.50, 1.50, 3.50,
      14L,-1.30,  1.30, 0.00, 1.50
    ) %>%
      mutate(xmin = convert_distance_ft(xmin, u),
             xmax = convert_distance_ft(xmax, u),
             ymin = convert_distance_ft(ymin, u),
             ymax = convert_distance_ft(ymax, u))
    
    sz_left   <- convert_distance_ft(-1.30, u)
    sz_right  <- convert_distance_ft( 1.30, u)
    sz_bottom <- convert_distance_ft( 1.50, u)
    sz_top    <- convert_distance_ft( 3.50, u)
    
    zone_stats <- d %>%
      mutate(
        ab_text = as.character(ab_result),
        is_hit = stringr::str_detect(ab_text, "安打|本塁打|塁打|ランニング本塁打"),
        not_ab = stringr::str_detect(ab_text, "四球|死球|犠打|犠飛|妨害|敬遠")
      ) %>%
      filter(!not_ab) %>%
      group_by(pitch_zone) %>%
      summarise(
        ab = n(),
        hits = sum(is_hit),
        ba = hits / ab,
        .groups = "drop"
      )
    
    plot_df <- zone_rects %>%
      left_join(zone_stats, by = c("zone" = "pitch_zone")) %>%
      tidyr::replace_na(list(ab = 0, hits = 0, ba = 0)) %>%
      mutate(label = ifelse(ab > 0,
                            sprintf("%.3f\n(%d/%d)", ba, hits, ab),
                            "—\n(0 AB)"))
    
    ggplot(plot_df) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ba),
                color = "black", linewidth = 0.6) +
      geom_text(aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = label),
                size = 4, fontface = "bold") +
      annotate("rect", xmin = sz_left, xmax = sz_right,
               ymin = sz_bottom, ymax = sz_top,
               fill = NA, color = "black", linewidth = 1.5) +
      scale_fill_gradient2(low = "#3B82F6", mid = "white", high = "#EF4444",
                           midpoint = 0.250,
                           name = translate("BA", l),
                           limits = c(0, 0.5),
                           labels = function(x) sprintf("%.3f", x)) +
      coord_fixed(xlim = convert_distance_ft(c(-3, 3), u),
                  ylim = convert_distance_ft(c(-0.5, 5), u)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13, face = "bold"),
            legend.key.size = unit(1.2, "cm"),
            plot.margin = margin(0, 5, 5, 5))
  })
  
  #BATTER PITCH TYPE SUCCESS RATE
  
  output$batter_pitch_table <- renderTable({
    l <- lang()
    d <- batter_filtered()
    d_n <- nrow(d)
    shiny::validate(shiny::need(is.numeric(d_n) && d_n > 0, translate("No data.", l)))
    
    tbl <- d %>%
      mutate(
        ab_text = as.character(ab_result),
        is_hit = stringr::str_detect(ab_text, "安打|本塁打|塁打|ランニング本塁打"),
        not_ab = stringr::str_detect(ab_text, "四球|死球|犠打|犠飛|妨害|敬遠")
      ) %>%
      filter(!not_ab) %>%
      group_by(ball_type) %>%
      summarise(
        AB = n(),
        H = sum(is_hit),
        BA = H / AB,
        .groups = "drop"
      ) %>%
      arrange(desc(AB))
    
    unique_pitches <- unique(as.character(tbl$ball_type))
    pitch_lookup <- vapply(unique_pitches, function(x) {
      out <- translate(x, l)
      if (!is.character(out) || length(out) != 1) x else out
    }, character(1))
    names(pitch_lookup) <- unique_pitches
    
    tbl$Pitch <- as.character(pitch_lookup[as.character(tbl$ball_type)])
    tbl <- tbl %>% 
      mutate(BA = sprintf("%.3f", BA)) %>%
      select(Pitch, AB, H, BA)
    
    new_names <- vapply(c("Pitch", "AB", "H", "BA"),
                        function(x) {
                          out <- translate(x, l)
                          if (!is.character(out) || length(out) != 1) x else out
                        }, character(1))
    colnames(tbl) <- new_names
    tbl
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
  
  
  

#PAGE ONE PENNSYLVANIA

#LEAGUE STANDINGS PENNSYLVANIA
  

output$central_table <- renderTable({
  l <- lang()
  df <- league_standings[league_standings$league == "Central", ]
  df <- df[order(-df$wins, df$losses), ]
  df$league <- NULL
  colnames(df) <- c(translate("Team", l), translate("Wins", l),
                    translate("Losses", l), translate("Ties", l),
                    translate("Games Behind", l))
  df
}, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE, digits = 0)

output$pacific_table <- renderTable({
  l <- lang()
  df <- league_standings[league_standings$league == "Pacific", ]
  df <- df[order(-df$wins, df$losses), ]
  df$league <- NULL
  colnames(df) <- c(translate("Team", l), translate("Wins", l),
                    translate("Losses", l), translate("Ties", l),
                    translate("Games Behind", l))
  df
}, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE, digits = 0)


#STAT LEADERS PENNSYLVANIA


output$leaders_title <- renderUI({
  l <- lang()
  teams <- selected_teams()
  
  # Map Japanese short names to English short names from league_standings
  team_jp_to_en <- c(
    "DeNA" = "BayStars", "ヤクルト" = "Swallows", "中日" = "Dragons",
    "巨人" = "Giants", "広島" = "Carp", "阪神" = "Tigers",
    "オリックス" = "Buffaloes", "ソフトバンク" = "Hawks", "ロッテ" = "Marines",
    "日本ハム" = "Fighters", "楽天" = "Golden Eagles", "西武" = "Lions"
  )
  
  if (is.null(teams)) {
    title <- translate("NPB Stat Leaders", l)
  } else if (length(teams) == 1) {
    team_name <- if (l == "ja") teams else team_jp_to_en[teams]
    title <- paste(translate(team_name, l), translate("Stat Leaders", l))
  } else if (length(teams) == 6) {
    if ("巨人" %in% teams) {
      title <- translate("Central League Stat Leaders", l)
    } else {
      title <- translate("Pacific League Stat Leaders", l)
    }
  } else {
    title <- translate("Stat Leaders", l)
  }
  
  h3(title)
})



output$ba_leader <- renderUI({
  l <- lang()
  df <- SeasonBattingStats
  teams <- selected_teams()
  
  if (!is.null(teams)) {
    full_names <- team_to_full_jp[teams]
    df <- df[df$team %in% full_names, ]
  }
  
  top <- df[which.max(df$avg), ]
  
  title <- translate("Batting Average", l)

  
  photo <- batter_choices$batter_photo_b64[batter_choices$batter_name == top$player][1]
  number <- batter_choices$number[batter_choices$batter_name == top$player][1]
  
  wellPanel(
    strong(translate("Batting Average", l)),
    div(
      style = "text-align: center; margin: 10px 0;",
      if (!is.na(photo) && !is.null(photo)) {
        tags$img(src = paste0("data:image/jpeg;base64,", photo),
                 height = 80, style = "border-radius: 50%;")
      }
    ),
    h4(paste0("#", number, " ", get_display_name(top$player, l))),
    h3(sprintf("%.3f", top$avg))
  )
})



output$hr_leader <- renderUI({
  l <- lang()
  df <- SeasonBattingStats
  teams <- selected_teams()
  
  if (!is.null(teams)) {
    full_names <- team_to_full_jp[teams]
    df <- df[df$team %in% full_names, ]
  }
  
  top <- df[which.max(df$hr), ]
  
  title <- translate("Home Runs", l)
  
  
  photo <- batter_choices$batter_photo_b64[batter_choices$batter_name == top$player][1]
  number <- batter_choices$number[batter_choices$batter_name == top$player][1]
  
  
  wellPanel(
    strong(title),
    div(
      style = "text-align: center; margin: 10px 0;",
      if (!is.na(photo) && !is.null(photo)) {
        tags$img(src = paste0("data:image/jpeg;base64,", photo),
                 height = 80, style = "border-radius: 50%;")
      }
      ),
    h4(paste0("#", number, " ", get_display_name(top$player, l))),
    h3(top$hr)
  )
})



output$rbi_leader <- renderUI({
  l <- lang()
  df <- SeasonBattingStats
  teams <- selected_teams()
  
  if (!is.null(teams)) {
    full_names <- team_to_full_jp[teams]
    df <- df[df$team %in% full_names, ]
  }
  
  top <- df[which.max(df$rbi), ]
  
  title <- translate("Runs Batted In", l)
  
  
  photo <- batter_choices$batter_photo_b64[batter_choices$batter_name == top$player][1]
  number <- batter_choices$number[batter_choices$batter_name == top$player][1]
  
  wellPanel(
    strong(title),
    div(
      style = "text-align: center; margin: 10px 0;",
      if (!is.na(photo) && !is.null(photo)) {
        tags$img(src = paste0("data:image/jpeg;base64,", photo),
                 height = 80, style = "border-radius: 50%;")
      }
    ),
    h4(paste0("#", number, " ", get_display_name(top$player, l))),
    h3(top$rbi)
  )
})
  


output$hit_leader <- renderUI({
  l <- lang()
  df <- SeasonBattingStats
  teams <- selected_teams()
  
  if (!is.null(teams)) {
    full_names <- team_to_full_jp[teams]
    df <- df[df$team %in% full_names, ]
  }
  
  top <- df[which.max(df$hits), ]
  
  title <- translate("Hits", l)
  
  
  photo <- batter_choices$batter_photo_b64[batter_choices$batter_name == top$player][1]
  number <- batter_choices$number[batter_choices$batter_name == top$player][1]
  
  wellPanel(
    strong(title),
    div(
      style = "text-align: center; margin: 10px 0;",
      if (!is.na(photo) && !is.null(photo)) {
        tags$img(src = paste0("data:image/jpeg;base64,", photo),
                 height = 80, style = "border-radius: 50%;")
      }
    ),
    h4(paste0("#", number, " ", get_display_name(top$player, l))),
    h3(top$hits)
  )
})



output$sb_leader <- renderUI({
  l <- lang()
  df <- SeasonBattingStats
  teams <- selected_teams()
  
  if (!is.null(teams)) {
    full_names <- team_to_full_jp[teams]
    df <- df[df$team %in% full_names, ]
  }
  
  top <- df[which.max(df$sb), ]
  
  title <- translate("Stolen Bases", l)
  
  
  photo <- batter_choices$batter_photo_b64[batter_choices$batter_name == top$player][1]
  number <- batter_choices$number[batter_choices$batter_name == top$player][1]
  
  wellPanel(
    strong(title),
    div(
      style = "text-align: center; margin: 10px 0;",
      if (!is.na(photo) && !is.null(photo)) {
        tags$img(src = paste0("data:image/jpeg;base64,", photo),
                 height = 80, style = "border-radius: 50%;")
      }
    ),
    h4(paste0("#", number, " ", get_display_name(top$player, l))),
    h3(top$sb)
  )
})


output$win_leader <- renderUI({
  l <- lang()
  df <- SeasonPitchingStats
  teams <- selected_teams()
  
  if (!is.null(teams)) {
    full_names <- team_to_full_jp[teams]
    df <- df[df$team %in% full_names, ]
  }
  
  top <- df[which.max(df$wins), ]
  
  title <- translate("Wins", l)
  
  
  photo <- pitcher_choices$pitcher_photo_b64[pitcher_choices$pitcher_name == top$player][1]
  number <- pitcher_choices$number[pitcher_choices$pitcher_name == top$player][1]
  
  wellPanel(
    strong(title),
    div(
      style = "text-align: center; margin: 10px 0;",
      if (!is.na(photo) && !is.null(photo)) {
        tags$img(src = paste0("data:image/jpeg;base64,", photo),
                 height = 80, style = "border-radius: 50%;")
      }
    ),
    h4(paste0("#", number, " ", get_display_name(top$player, l))),
    h3(top$wins)
  )
})


output$era_leader <- renderUI({
  l <- lang()
  df <- SeasonPitchingStats
  teams <- selected_teams()
  
  if (!is.null(teams)) {
    full_names <- team_to_full_jp[teams]
    df <- df[df$team %in% full_names, ]
  }
  
  top <- df[which.min(df$era), ]
  
  title <- translate("Earned Run Average", l)
  
  
  photo <- pitcher_choices$pitcher_photo_b64[pitcher_choices$pitcher_name == top$player][1]
  number <- pitcher_choices$number[pitcher_choices$pitcher_name == top$player][1]
  
  wellPanel(
    strong(title),
    div(
      style = "text-align: center; margin: 10px 0;",
      if (!is.na(photo) && !is.null(photo)) {
        tags$img(src = paste0("data:image/jpeg;base64,", photo),
                 height = 80, style = "border-radius: 50%;")
      }
    ),
    h4(paste0("#", number, " ", get_display_name(top$player, l))),
    h3(top$era)
  )
})



output$saves_leader <- renderUI({
  l <- lang()
  df <- SeasonPitchingStats
  teams <- selected_teams()
  
  if (!is.null(teams)) {
    full_names <- team_to_full_jp[teams]
    df <- df[df$team %in% full_names, ]
  }
  
  top <- df[which.max(df$saves), ]
  
  title <- translate("Saves", l)
  
  
  photo <- pitcher_choices$pitcher_photo_b64[pitcher_choices$pitcher_name == top$player][1]
  number <- pitcher_choices$number[pitcher_choices$pitcher_name == top$player][1]
  
  wellPanel(
    strong(title),
    div(
      style = "text-align: center; margin: 10px 0;",
      if (!is.na(photo) && !is.null(photo)) {
        tags$img(src = paste0("data:image/jpeg;base64,", photo),
                 height = 80, style = "border-radius: 50%;")
      }
    ),
    h4(paste0("#", number, " ", get_display_name(top$player, l))),
    h3(top$saves)
  )
})


output$strikeout_leader <- renderUI({
  l <- lang()
  df <- SeasonPitchingStats
  teams <- selected_teams()
  
  if (!is.null(teams)) {
    full_names <- team_to_full_jp[teams]
    df <- df[df$team %in% full_names, ]
  }
  
  top <- df[which.max(df$so), ]
  
  title <- translate("Strikeouts", l)
  
  
  photo <- pitcher_choices$pitcher_photo_b64[pitcher_choices$pitcher_name == top$player][1]
  number <- pitcher_choices$number[pitcher_choices$pitcher_name == top$player][1]
  
  wellPanel(
    strong(title),
    div(
      style = "text-align: center; margin: 10px 0;",
      if (!is.na(photo) && !is.null(photo)) {
        tags$img(src = paste0("data:image/jpeg;base64,", photo),
                 height = 80, style = "border-radius: 50%;")
      }
    ),
    h4(paste0("#", number, " ", get_display_name(top$player, l))),
    h3(top$so)
  )
})


output$inning_leader <- renderUI({
  l <- lang()
  df <- SeasonPitchingStats
  teams <- selected_teams()
  
  if (!is.null(teams)) {
    full_names <- team_to_full_jp[teams]
    df <- df[df$team %in% full_names, ]
  }
  
  top <- df[which.max(df$ip), ]
  
  title <- translate("Innings Pitched", l)
  
  
  photo <- pitcher_choices$pitcher_photo_b64[pitcher_choices$pitcher_name == top$player][1]
  number <- pitcher_choices$number[pitcher_choices$pitcher_name == top$player][1]
  
  wellPanel(
    strong(title),
    div(
      style = "text-align: center; margin: 10px 0;",
      if (!is.na(photo) && !is.null(photo)) {
        tags$img(src = paste0("data:image/jpeg;base64,", photo),
                 height = 80, style = "border-radius: 50%;")
      }
    ),
    h4(paste0("#", number, " ", get_display_name(top$player, l))),
    h3(top$ip)
  )
})


  
}

shinyApp(ui = ui, server = server)











