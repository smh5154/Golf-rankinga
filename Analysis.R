
# Packages/Libraries =====
install.packages("corrplot")
install.packages("gganimate")
install.packages("ggridges")
install.packages("tidygeocoder")
install.packages(c( "sfhotspot", "rnaturalearth", "rnaturalearthdata"))
install.packages("sf")
install.packages("tidyr")
install.packages("naniar")
install.packages("ggrepel")
install.packages("gifski")
install.packages("ggpointdensity")
install.packages("rnaturalearth")
install.packages(c("rnaturalearth", "rnaturalearthdata", "sf"))
install.packages("viridis")
library(cowplot)
library(viridis)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggpointdensity)
library(gifski)
library(ggrepel)
library(ggplot2)
library(naniar)
library(googlesheets4)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(gganimate)
library(ggridges)
library(tidygeocoder)
library(sf)
library(sfhotspot)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)


# Data import ====
data <- read_sheet("https://docs.google.com/spreadsheets/d/15IVe2_WEcwP6BUVWD9CAtAhV_jD6cXkmSl4ZSLyNklc/edit?gid=0#gid=0",
                   sheet = "data")
write_csv(data,file="data/dataOCT19.csv")

PGA_USopen <- read_sheet("https://docs.google.com/spreadsheets/d/1-pRyojgI0UGO7ro27vQt37R9HSb8EX1bvyEEObOpZFw/edit?gid=1486180131#gid=1486180131",
                         sheet = "PGA_USopen")

# Datasets =====
#only the 7 categories for 2025 N=200
GD2025categories <-  data %>% filter(!is.na(GD2025Rank)) %>% select(GD2025ShotOptions:GD2025Conditioning)

#2025 GD courses with all columns N=200
GD2025data <- data %>% filter(!is.na(GD2025Rank))

data <- data %>% mutate(ShotOptions25_23inc = (GD2025ShotOptions - GD2023ShotOptions) )
data <- data %>% mutate(LV25_23inc = (GD2025LayoutVariety - GD2023LayoutVariety) )
data <- data %>% mutate(Character25_23inc = (GD2025Character - GD2023Character) )
data <- data %>% mutate(Challenge25_23inc = (GD2025Challenge - GD2023Challenge) )
data <- data %>% mutate(Fun25_23inc = (GD2025Fun - GD2023Fun))
data <- data %>% mutate(Aesthetics25_23inc = (GD2025Aesthetics - GD2023Aesthetics) )
data <- data %>% mutate(Conditioning25_23inc = (GD2025Conditioning - GD2023Conditioning) )

data %>% select(GD2025Rank, Course, ShotOptions25_23inc, LV25_23inc, Fun25_23inc, Conditioning25_23inc, Character25_23inc, Challenge25_23inc, Aesthetics25_23inc) %>%
  pivot_longer(
    cols = c(ShotOptions25_23inc, LV25_23inc, Fun25_23inc, Conditioning25_23inc, Character25_23inc, Challenge25_23inc, Aesthetics25_23inc), 
    names_to = "Category",        # New column for original column names
    values_to = "Score",         # New column for original column values
  ) %>%  ggplot(aes(x = Score,  y = Category, fill = Category)) + geom_density_ridges(quantile_lines = TRUE) + theme_classic() + 
  scale_x_continuous(limits = c(-.4, .4), breaks = seq(-.4, .4, by = .1)) + geom_vline(xintercept = c(0.0),linetype = "dashed", color = "gray", linewidth = .2)


data$Fun25_23percdiff <- ((data$GD2025ShotOptions - data$GD2023ShotOptions) / data$GD2023ShotOptions) * 100
data$Aesthetics25_23percdiff <- ((data$GD2025ShotOptions - data$GD2023ShotOptions) / data$GD2023ShotOptions) * 100
data$Conditioning25_23percdiff <- ((data$GD2025ShotOptions - data$GD2023ShotOptions) / data$GD2023ShotOptions) * 100

GD2025long <- GD2025categories  %>% 
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Score")

GD2025cats_rank <- data %>% filter(!is.na(GD2025Rank)) %>% select(GD2025ShotOptions:GD2025Conditioning, GD2025Rank)
GD2025cats_ranklong <- GD2025cats_rank %>%  rename(Rank2025GD = GD2025Rank) %>%
  pivot_longer(
    cols = starts_with("GD2025"), # Select columns to pivot
    names_to = "Category",        # New column for original column names
    values_to = "Score")   %>%           # New column for the values
  mutate(Year = "2025") %>% rename(Rank = Rank2025GD)

GD2023cats_rank <- data %>% filter(!is.na(GD2023Rank)) %>% select(GD2023ShotOptions:GD2023Conditioning, GD2023Rank)
GD2023cats_ranklong <- GD2023cats_rank %>%  rename(Rank2023GD = GD2023Rank) %>%
  pivot_longer(
    cols = starts_with("GD2023"), # Select columns to pivot
    names_to = "Category",        # New column for original column names
    values_to = "Score")   %>%           # New column for the values
  mutate(Year = "2023") %>% rename(Rank = Rank2023GD)

GD2021cats_rank <- data %>% filter(!is.na(GD2021Rank)) %>% select(GD2021ShotOptions:GD2021Character, GD2021Rank)
GD2021cats_ranklong <- GD2021cats_rank %>%  rename(Rank2021GD = GD2021Rank) %>%
  pivot_longer(
    cols = starts_with("GD2021"), # Select columns to pivot
    names_to = "Category",        # New column for original column names
    values_to = "Score")   %>%           # New column for the values
  mutate(Year = "2021") %>% rename(Rank = Rank2021GD)

combined_yearstop200GD <- bind_rows(GD2021cats_ranklong, GD2023cats_ranklong, GD2025cats_ranklong)
combined_yearstop200GD$Category <- str_remove(combined_yearstop200GD$Category, "GD2021")
combined_yearstop200GD$Category <- str_remove(combined_yearstop200GD$Category, "GD2023")
combined_yearstop200GD$Category <- str_remove(combined_yearstop200GD$Category, "GD2025")

ggplot(combined_yearstop200GD) + geom_density_ridges(mapping = aes(x = Score, y = Year, fill = Year), color = "white") + 
  theme_classic() + facet_wrap(~ Category) + scale_x_continuous(breaks = seq(7, 9, by = .5)) +
  geom_vline(xintercept = c(7.5, 8.0, 8.5), linetype = "dashed", color = "gray", linewidth = .2)

ggplot(GD2025cats_ranklong, aes(x = Rank2025GD, y = Score, color = Category, group = Category)) +
  geom_smooth(se = FALSE) +
  labs(title = "Category Score (scale: 1-10)", x = "Rank", y = "Score") +
  theme_classic()  +
  scale_y_continuous(limits = c(7.00, 9.00), breaks = seq(7.00, 9.00, by = .25)) +
  scale_color_discrete(labels = c(
    "GD2025Aesthetics" = "Aesthetics",
    "GD2025Challenge" = "Challenge",
    "GD2025Character" = "Character",
    "GD2025Conditioning" = "Conditioning",
    "GD2025Fun" = "Fun",
    "GD2025LayoutVariety" = "Layout Variety",
    "GD2025ShotOptions" = "Shot Options"))

GD2025cats_ranklong %>% filter(Category == "GD2025ShotOptions" | Category == "GD2025LayoutVariety") %>%
  ggplot(aes(x = Rank2025GD, y = Score, color = Category, group = Category)) +
  geom_smooth(se = FALSE) +
  labs(title = "Category Score (scale: 1-10)", x = "Rank", y = "Score") +
  theme_classic()  +
  scale_y_continuous(limits = c(7.00, 9.00), breaks = seq(7.00, 9.00, by = .25)) +
  scale_color_discrete(labels = c(
    "GD2025LayoutVariety" = "Layout Variety",
    "GD2025ShotOptions" = "Shot Options"))

data %>% filter(!is.na(GD2025Rank))


# NOV 4, 2025 Top 25 Comparison of GD to GOLF Rankings====
courses25 <- c("Muirfield Village Golf Club", "Shadow Creek", "Pinehurst No. 2", "San Francisco Golf Club", "Shoreacres")
data %>% filter(GD2025Rank <= 25| GOLF2024Rank <=25) %>% ggplot(aes(x=GD2025Rank, y=GOLF2024Rank, label = ifelse(Course %in% courses25, Course, ""))) + 
  geom_point(color="green4", size=2.5) + theme_classic() +   geom_text_repel(point.padding = 0.2, size=3 ,nudge_y = .5, hjust = 0.5, segment.curvature = -1e-20, arrow = arrow(length = unit(0.015, "npc"))) + 
  geom_abline(intercept = 0, slope = 1, color = "darkgreen", linetype = "dashed") +
  annotate("text", x = 40, y = 75, label = "@ParAndRank", angle = 0, size = 5, color = "lightblue", alpha = 1) +
  labs(x="2025 Golf Digest Ranking", y="2024 GOLF Ranking") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10))



# NOV 6, 2025 Animation of 9 courses with most improvement in Golf Digest====

GDmostimp <- data %>%
  filter(Course %in% c(
    "Ballyneal Golf Club", "Old Town Club", "TPC Sawgrass (Players Stadium)",
    "Shoreacres", "Sleepy Hollow Country Club", "Myopia Hunt Club",
    "California Golf Club", "Rock Creek Cattle Company", "Essex County Club"
  )) %>%
  pivot_longer(cols = matches("^GD\\d{4} ?Rank$"),names_to = "RatedYear",values_to = "Rank" ) %>%
  mutate(RatedYear = as.numeric(gsub("GD|Rank|\\s", "", RatedYear))) %>%
  ggplot(aes(x = RatedYear, y = Rank, color = Course, group = Course)) +
  geom_line(linetype = "dotted", linewidth = 1.2) +
  geom_point(size = 3) +theme_classic(base_size = 14) + scale_color_manual(
    values = c("Ballyneal Golf Club" = "#000000", "Old Town Club" = "#999999", "TPC Sawgrass (Players Stadium)" = "#CC79A7",
     "Shoreacres" = "#D55E00", "Sleepy Hollow Country Club" = "#0072B2","Myopia Hunt Club" = "#F0E442", "Essex County Club" = "#009E73",
     "California Golf Club" = "#56B4E9", "Rock Creek Cattle Company" = "#E69F00")) +
  scale_y_continuous(limits = c(1, 120), breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100,110, 120)) +
  scale_x_continuous(limits = c(2018, 2025), breaks = c(2018, 2021, 2023, 2025)) +
  labs(x = "Year Rated",y = "Golf Digest Rank",title = "GOLF DIGEST'S MOST IMPROVED") +
  annotate("text", x = 2021, y = 10,label = "@ParAndRank",color = "lightblue", size = 5, alpha = 1) +
  transition_states(states = Course,transition_length = 2, state_length = 1, wrap = FALSE ) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),   # title font
    axis.title.x = element_text(size = 20),                # x-axis label
    axis.title.y = element_text(size = 20),                # y-axis label
    axis.text = element_text(size = 16),                   # tick labels
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 1) + enter_fade() +exit_fade() + ease_aes("sine-in-out")

# 🔹 Render animation
anim <- animate(GDmostimp, fps = 10,duration = 20, width = 900,height = 600,renderer = gifski_renderer())
anim_save("~/Desktop/GolfGraphics/golf_digest_most_improved.gif", animation = anim)





# Nov 9, 2025====
data <- data %>%
  mutate(
    GOLFrankdiff = {
      # combine into a rowwise vector
      ranksGOLF <- cbind(GOLF2020Rank, GOLF2022Rank, GOLF2024Rank)
      # count non-NA per row
      valid_counts <- rowSums(!is.na(ranksGOLF))
      # compute min-max diff only if ≥2 values exist
      GOLFdiffs <- abs(pmin(GOLF2020Rank, GOLF2022Rank, GOLF2024Rank, na.rm = TRUE) -
                     pmax(GOLF2020Rank, GOLF2022Rank, GOLF2024Rank, na.rm = TRUE))
      ifelse(valid_counts >= 2, GOLFdiffs, NA)
    }
  )

GOLFmostimp <- data %>%
  filter(Course %in% c(
    "Baltusrol G.C. (Lower)", "Oakland Hills Country Club (South)", "CapRock Ranch",
     "The Honors Course", "Old Town Club"
  )) %>%
  pivot_longer(cols = matches("^GOLF\\d{4} ?Rank$"),names_to = "RatedYear",values_to = "Rank" ) %>%
  mutate(RatedYear = as.numeric(gsub("GOLF|Rank|\\s", "", RatedYear))) %>%
  ggplot(aes(x = RatedYear, y = Rank, color = Course, group = Course)) +
  geom_line(linetype = "dotted", linewidth = 1.2) +
  geom_point(size = 4) +theme_classic(base_size = 14) + scale_color_manual(
    values = c("Baltusrol G.C. (Lower)" = "#000000", "Oakland Hills Country Club (South)" = "#D55E00", "CapRock Ranch" = "#CC79A7",
                "The Honors Course" = "#0072B2","Old Town Club" = "#F0E442")) +
  scale_y_continuous(limits = c(1, 100), breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_x_continuous(limits = c(2020, 2024), breaks = c(2020, 2022, 2024)) +
  labs(x = "Year Rated",y = "GOLF Rank",title = "GOLF'S MOST IMPROVED") +
  annotate("text", x = 2021, y = 10,label = "@ParAndRank",color = "lightblue", size = 6, alpha = 1) +
  transition_states(states = Course,transition_length = 2, state_length = 1, wrap = FALSE ) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),   # title font
    axis.title.x = element_text(size = 22),                # x-axis label
    axis.title.y = element_text(size = 22),                # y-axis label
    axis.text = element_text(size = 18),                   # tick labels
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  ) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 1) + enter_fade() +exit_fade() + ease_aes("sine-in-out")

# 🔹 Render animation
anim <- animate(GOLFmostimp, fps = 10,duration = 20, width = 900,height = 600,renderer = gifski_renderer())
anim_save("~/Desktop/GolfGraphics/GOLF_most_improved.gif", animation = anim)





# Nov 10, 2025 aggregating GD and GOLF ranks====

data <- data %>% mutate(AGGrank = (GD2025Rank + GOLF2024Rank)/2) %>%
  mutate(AGGRank1_N = row_number(AGGrank))


# Nov 11, 2025 GOLF + GD - by state comparison tally ====
sum(is.na(data$GD2025Rank))
data %>% filter(!is.na(GD2025Rank)) %>% count(Status)


data %>% filter(GOLF2024Rank <=100 | GD2025Rank <= 200) %>% count(State, sort = T) %>% head(19)
x <- data %>% filter(GOLF2024Rank <=100 | GD2025Rank <= 200) %>% group_by(State, Status) %>%tally() %>% spread(key = Status, value = n, fill = 0)
print(x, n=50) 

data %>% filter(GD2025PublicRank <=100) %>% count(State, sort = T) %>% head(19)



# Nov 12, 2025 Map of Top203 and GDtopPublic
data <- data %>%
  geocode(city = City, state = State, method = "osm")
us_states <- map_data("state")
ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "white", color = "darkslategray") +
  geom_jitter(data = data, aes(x = long, y = lat, color = Status), size = 1) +
  #scale_color_viridis_c() + # Example for coloring points based on a 'value'
  labs(title = "Points on US Map by City and State") +
  theme_void() +
  coord_map() # Ensures correct aspect ratio for the map

data %>% filter(!is.na(GD2025PublicRank)) %>% ggplot() + 
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "white", color = "darkslategray") +
  geom_point(aes(x = long, y = lat), size = 2) +
  #scale_color_viridis_c() + # Example for coloring points based on a 'value'
  labs(title = "Points on US Map by City and State") +
  theme_void() +
  coord_map() # Ensures correct aspect ratio for the map

us_states <- map_data("state")

# 2️⃣ Reposition Alaska & Hawaii manually
# scale/translate to fit near CONUS
us_states_shifted <- us_states %>%
  mutate(
    long = case_when(
      region == "alaska" ~ long * 0.35 + 50,
      region == "hawaii" ~ long + 85,
      TRUE ~ long
    ),
    lat = case_when(
      region == "alaska" ~ lat * 0.35 - 10,
      region == "hawaii" ~ lat - 5,
      TRUE ~ lat
    )
  )
data_filtered <- data %>%
  filter(!is.na(GD2025PublicRank), GD2025PublicRank <= 100)

# 4️⃣ Reposition points in AK/HI to match map shift
data_shifted <- data_filtered %>%
  mutate(
    long = case_when(
      long < -130 & lat > 50 ~ long * 0.35 + 50, # Alaska
      lat < 25 & long < -150 ~ long + 85,        # Hawaii
      TRUE ~ long
    ),
    lat = case_when(
      long < -130 & lat > 50 ~ lat * 0.35 - 10,
      lat < 25 & long < -150 ~ lat - 5,
      TRUE ~ lat
    )
  )
ggplot() +
  geom_polygon(
    data = us_states_shifted,
    aes(x = long, y = lat, group = group),
    fill = "gray100", color = "gray70"
  ) + geom_point(
    data = data_shifted,
    aes(x = long, y = lat, color = GD2025PublicRank),
    size = 3,
    shape = 21,
    stroke = 1.2,
    position = position_jitter(width = 0.3, height = 0.3)  # show duplicates
  ) +
  scale_color_gradient(
    low = "darkgreen", high = "white",
    limits = c(1, 100),
    breaks = c(1, 25, 50, 75, 100),
    name = "2025 Public Rank"
  ) +  coord_quickmap(xlim = c(-135, -50), ylim = c(18, 55)) +
  labs(title = "Golf Digest 2025 Public Course Rankings") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



# File: scripts/us_map_insets_fixed.R

library(ggplot2)
library(dplyr)
library(maps)
library(cowplot)
library(viridis)

# Get US states map
us_all <- map_data("state")

# Split regions
us_main <- us_all %>% filter(!region %in% c("alaska", "hawaii"))
us_ak   <- us_all %>% filter(region == "alaska")
us_hi   <- us_all %>% filter(region == "hawaii")

# Shift + scale Alaska and Hawaii polygons (without distorting shape)
us_ak_fixed <- us_ak %>%
  mutate(long = (long + 130) * 0.35 - 120,
         lat  = (lat - 55)  * 0.35 + 25)

us_hi_fixed <- us_hi %>%
  mutate(long = long + 55,  # move east
         lat  = lat - 10)   # move south

# Combine all for main plot
us_fixed <- bind_rows(us_main, us_ak_fixed, us_hi_fixed)

# Filter your course data
data_filtered <- data %>%
  filter(!is.na(GD2025PublicRank), GD2025PublicRank <= 100)

# Match the same coordinate transforms
data_shifted <- data_filtered %>%
  mutate(
    long = case_when(
      long < -130 & lat > 50 ~ (long + 130) * 0.35 - 120, # Alaska
      lat < 25 & long < -150 ~ long + 55,                  # Hawaii
      TRUE ~ long
    ),
    lat = case_when(
      long < -130 & lat > 50 ~ (lat - 55) * 0.35 + 25,
      lat < 25 & long < -150 ~ lat - 10,
      TRUE ~ lat
    )
  )

# Final map
ggplot() +
  geom_polygon(
    data = us_fixed,
    aes(x = long, y = lat, group = group),
    fill = "gray97", color = "gray70"
  ) +
  geom_point(
    data = data_shifted,
    aes(x = long, y = lat, color = GD2025PublicRank),
    size = 3, shape = 21, stroke = 0.5,
    position = position_jitter(width = 0.3, height = 0.3)
  ) +
  scale_color_gradient(
    low = "white", high = "#0072B2",
    limits = c(1, 100),
    breaks = c(1, 25, 50, 75, 100),
    name = "2025 Public Rank"
  ) +
  coord_quickmap(xlim = c(-130, -65), ylim = c(22, 52)) +
  theme_void(base_size = 14) +
  labs(title = "Golf Digest 2025 Public Course Rankings") +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )









# Calculate the correlation matrix ====
GD2025categories <-  data %>% filter(!is.na(GD2025ShotOptions)) %>%
  select(GD2025ShotOptions:GD2025Conditioning)
# Create a basic correlation plot
cor_matrixGD2025 <- cor(GD2025categories)
new_names <- c("Shot Options", "Character", "Challenge", "Layout Variety",
               "Fun", "Aesthetics", "Conditioning")

# slope vs difficulty
SlopevDifficultymodel5 <- lm(GD2025Challenge ~ Slope, data = data)
summary(SlopevDifficultymodel5)
SlopevDifficultymodel3 <- lm(GD2023Challenge ~ Slope, data = data)
summary(SlopevDifficultymodel3)
SlopevDifficultymodel1 <- lm(GD2021Challenge ~ Slope, data = data)
summary(SlopevDifficultymodel1)
ggplot(data = data, aes(x = GD2021Challenge,  y = Slope)) + 
  geom_point(size=1) + geom_smooth(method = "lm") + theme_classic()

ggplot(data = data, aes(x = GD2025Rank,  y = Slope, color=Status)) + 
  geom_point(size=1) + geom_smooth(method = "lm", se = FALSE) + theme_classic()
ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Challenge, color=Status)) + 
  geom_point(size=1) + geom_smooth(method = "lm", se = FALSE) + theme_classic()

ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Conditioning, group=Status, color=Status)) + 
  geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))

ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Challenge, group=Status, color=Status)) + 
  geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))

ggplot(data = data, aes(x = GD2025Rank,  y = GD2025ShotOptions, group=Status, color=Status)) + 
  geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))

ggplot(data = data, aes(x = GD2025Rank,  y = GD2025LayoutVariety, group=Status, color=Status)) + 
  geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))

ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Fun, group=Status, color=Status)) + 
  geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))

ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Aesthetics, group=Status, color=Status)) + 
  geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))

ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Character, group=Status, color=Status)) + 
  geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))

rownames(cor_matrixGD2025) <- new_names
colnames(cor_matrixGD2025) <- new_names
corrplot(cor_matrixGD2025, method = "number", diag = FALSE, type = 'upper', tl.srt = 45,
         tl.col = 'black', tl.cex = 0.8)

ggplot(data = data, aes(x = GD2025ShotOptions,  y = GD2025LayoutVariety)) + 
  geom_point(size=1) + geom_smooth(method = "lm") + theme_classic() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted", size = 1) +
  scale_x_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25)) +
  scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))+
  labs(x = "Shot Options", y = "Layout Variety", title = "Golf Digest 2025 Scores", subtitle = "Layout Variety vs Shot Options")

ggplot(GD2025long) +
  geom_density_ridges(mapping = aes(x = Score, 
                                    y = Category, 
                                    fill = Category), 
                      color = "white") + theme_classic()

GDtheme_classic()GD2025categories$SO_LVdiff <- GD2025categories$GD2025ShotOptions - GD2025categories$GD2025LayoutVariety
ggplot(data = GD2025categories, aes(x = GD2025ShotOptions, y = SO_LVdiff)) + geom_point() +
  theme_classic() + scale_y_continuous(limits = c(-0.35, 0.35))




# post mm/dd/yy ====

data <- data %>%
  mutate(differenceG2024_GD2025 = GD2025Rank - GOLF2024Rank)
data %>% filter(GD2025Rank <=100) %>% ggplot(aes(x=GD2025Rank, y=differenceG2024_GD2025)) + geom_point() + theme_classic() + 
  geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed") + 
  annotate("text", x = 80, y = 50, label = "@ParAndRank", angle = 0, size = 5, color = "grey", alpha = 1) +
  labs(x = "Golf Digest 2025 Rank",y = "Ranking Difference between \nGOLF and Golf Digest")







data %>% filter(GD2025Rank <= 100| GOLF2024Rank <=100) %>% ggplot(aes(x=GD2025Rank, y=GOLF2024Rank)) + geom_point() + theme_classic() + 
  geom_abline(intercept = 0, slope = 1, color = "darkgreen", linetype = "dashed") + geom_miss_point()  +
  annotate("text", x = 40, y = 100, label = "@ParAndRank", angle = 0, size = 5, color = "grey", alpha = 1)

GD2025data <- GD2025data %>% mutate(GD2025ShotOptionsChange = GD2025ShotOptions - lead(GD2025ShotOptions)) %>%
  mutate(GD2025CharacterChange = GD2025Character - lead(GD2025Character)) %>%
  mutate(GD2025LayoutVarietyChange = GD2025LayoutVariety - lead(GD2025LayoutVariety)) %>%
  mutate(GD2025FunChange = GD2025Fun - lead(GD2025Fun)) %>%
  mutate(GD2025ChallengeChange = GD2025Challenge - lead(GD2025Challenge))  %>%
  mutate(GD2025AesteticsChange = GD2025Aesthetics - lead(GD2025Aesthetics)) %>%
  mutate(GD2025ConditioningChange = GD2025Conditioning - lead(GD2025Conditioning)) 

data <- data %>% mutate(GDShotOptionsChange = GD2025ShotOptions - lead(GD2023ShotOptions)) %>%
  mutate(GDCharacterChange = GD2025Character - lead(GD2023Character)) %>%
  mutate(GDLayoutVarietyChange = GD2025LayoutVariety - lead(GD2023LayoutVariety)) %>%
  mutate(GDFunChange = GD2025Fun - lead(GD2023Fun)) %>%
  mutate(GDChallengeChange = GD2025Challenge - lead(GD2023Challenge))  %>%
  mutate(GDAesteticsChange = GD2025Aesthetics - lead(GD2023Aesthetics)) %>%
  mutate(GDConditioningChange = GD2025Conditioning - lead(GD2023Conditioning))

data <- data %>% mutate(GD2025Score = GD2025ShotOptions+GD2025ShotOptions+GD2025Character+GD2025Challenge+
                          GD2025LayoutVariety+GD2025LayoutVariety+GD2025Fun+GD2025Aesthetics+GD2025Conditioning)


ggplot(GD2025long, aes(x = Score, fill = Category)) +
  geom_density(alpha = 0.3) + # alpha controls transparency
  labs(title = "Overlayed Density Plots", x = "Score", y = "Density") +
  theme_minimal()


data <- data %>%
  geocode(city = City, state = State, method = "osm")
us_states <- map_data("state")
ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "white", color = "darkslategray") +
  geom_point(data = data, aes(x = long, y = lat, color = Status), size = 1) +
  #scale_color_viridis_c() + # Example for coloring points based on a 'value'
  labs(title = "Points on US Map by City and State") +
  theme_void() +
  coord_map() # Ensures correct aspect ratio for the map

data %>% filter(!is.na(GD2025PublicRank)) %>% ggplot() + 
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "white", color = "darkslategray") +
  geom_point(aes(x = long, y = lat), size = 2) +
  #scale_color_viridis_c() + # Example for coloring points based on a 'value'
  labs(title = "Points on US Map by City and State") +
  theme_void() +
  coord_map() # Ensures correct aspect ratio for the map

data <- data %>% arrange(GD2021Rank) %>%
  mutate(
    next_Value2021score = lead(GD2021Score), # Get the value from the row below
    Percent_Greater_Than_next2021score = ((GD2021Score - next_Value2021score) / next_Value2021score) * 100
  )
ggplot(data = data, aes(x = GD2021Rank,  y = Percent_Greater_Than_next2021score)) + 
  geom_point(size=1) + geom_smooth(method = "lm") + theme_classic() + 
  geom_text(aes(label = GD2021Rank), nudge_y = 0.2)

custom_labels <- c("2021", "2023", "2025")

data %>% arrange(desc(Fun25_23inc)) %>% slice_head(n = 9) %>% 
  select(Course, GD2025Fun, GD2023Fun) %>% 
  pivot_longer(
    cols = c(GD2025Fun, GD2023Fun), 
    names_to = "Year",        # New column for original column names
    values_to = "FunScore",         # New column for original column values
  ) %>% ggplot(aes(x = Year, y = FunScore)) + geom_point() + facet_wrap(~Course, nrow = 3) +
  theme_bw() + scale_x_discrete(labels = custom_labels) + geom_text(aes(label = FunScore), vjust = -0.5, size=2.5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .5))


data %>% filter(GD2025Rank <= 100) %>% 
  filter(!is.na(GD2025Rank) & !is.na(GOLF2024Rank)) %>%
  summarise(n = n())

data %>% filter(GD2025Rank <= 100) %>% ggplot(aes(x=GD2025Rank, y=GOLF2024Rank)) + geom_point() +
  theme_classic()


PGA_USopen %>% filter(Rater == "GolfDigest") %>%
  ggplot(aes(x=Year, y=RankGD, color= Tournament)) + geom_point(size=2.5) + theme_classic() + theme(legend.position = c(0.8, 0.8), legend.title = element_blank()) +
  theme(legend.box.background = element_rect(color = "gray", fill = NA, linewidth = 1)) + geom_line(linetype = "dotted") + ylab("Rank (Golf Digest)") +
  scale_color_manual(values = c("PGA Championship" = "gray50", "US Open" = "dodgerblue2"))


df <- data.frame(
  x = c(1, 2, 3, NA, 5, 6),
  y = c(10, NA, 30, 40, 50, 60),
  group = c("A", "B", "A", "C", "B", "A")
)

# Create a new column to identify NA points
df_na <- tibble(
  x = c(1, 2, NA, 4, 5, 6),
  y = c(1, 2, 3, 4, NA, 6),
  category = c("A", "B", "A", "C", "B", "C"),
  has_na = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
)

ggplot(df_na, aes(x = x, y = y)) +
  geom_point(aes(shape = has_na, color = has_na), size = 3) +
  scale_shape_manual(values = c(16, 4)) +
  scale_color_manual(values = c("black", "red")) +
  labs(
    title = "Scatter plot highlighting rows with NA values",
    x = "X-axis",
    y = "Y-axis",
    color = "Has NA?",
    shape = "Has NA?"
  )