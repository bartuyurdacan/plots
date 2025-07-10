





# -----------------------------------------------------------------------------
# Natural Gas Market Overview Sankey Bump Chart
# -----------------------------------------------------------------------------
# Source:
#   “Natural Gas Market 2024 Annual Sector Report” (EPDK)
#   https://www.epdk.gov.tr/Detay/Icerik/3-0-94/dogal-gazyillik-sektor-raporu
# -----------------------------------------------------------------------------

# 0) (Optional) Set working directory to Desktop
# setwd("C:/Users/bartu/Desktop")

# 1) Load required libraries
library(tibble)     # for tribble()
library(dplyr)      # for data manipulation
library(tidyr)      # for pivot_longer()
library(ggplot2)    # for plotting
library(ggsankey)   # for geom_sankey_bump()
library(shadowtext) # for geom_shadowtext()
library(scales)     # for label_number()

# 2) Create the data frame
#    Values are in million Sm³
natural_gas <- tribble(
  ~Year,  ~Import, ~Production, ~Export, ~Consumption, ~EndOfPeriod_Stock,
  2019, 45211.47,      473.87,   762.68,       45282.03,           3095.44,
  2020, 48125.51,      441.72,   577.52,       48266.54,           2852.00,
  2021, 58703.93,      394.44,   382.89,       59830.36,           1914.17,
  2022, 54661.67,      379.81,   581.43,       53200.55,           5334.46,
  2023, 50483.77,      807.28,   896.28,       50210.84,           5442.40,
  2024, 52213.12,     2259.14,  1778.05,       53225.55,           4950.86
)

# 3) Reshape to long format and compute proportions
gas_long <- natural_gas %>%
  pivot_longer(
    cols = -Year,
    names_to  = "Category",
    values_to = "Value"
  ) %>%
  group_by(Year) %>%
  mutate(
    Total      = sum(Value),
    Proportion = Value / Total
  ) %>%
  ungroup()

# 4) Build the base Sankey bump chart
base_plot <- ggplot() +
  geom_sankey_bump(
    data = gas_long,
    aes(
      x     = factor(Year),
      node  = Category,
      value = Proportion,
      fill  = Category,
      label = Category
    ),
    alpha = 0.8
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position     = "bottom",
    plot.background     = element_rect(fill = "grey99", color = NA),
    axis.title          = element_blank(),
    axis.text.y         = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank()
  ) +
  labs(
    title    = "Natural Gas Market Composition by Year",
    subtitle = "Proportional flow of each category within annual totals",
    caption  = "Source: EPDK Natural Gas Market 2024 Annual Sector Report"
  )

# 5) Extract the built data for label placement
sankey_data <- ggplot_build(base_plot)$data[[1]]

# 6) Prepare start-year labels
labels_start <- sankey_data %>%
  group_by(label) %>%
  filter(x == min(x)) %>%
  summarise(x = first(x), y = mean(y), .groups = "drop") %>%
  left_join(
    gas_long %>% filter(Year == min(Year)) %>% select(Category, Value),
    by = c("label" = "Category")
  )

# 7) Prepare end-year labels
labels_end <- sankey_data %>%
  group_by(label) %>%
  filter(x == max(x)) %>%
  summarise(x = first(x), y = mean(y), .groups = "drop") %>%
  left_join(
    gas_long %>% filter(Year == max(Year)) %>% select(Category, Value),
    by = c("label" = "Category")
  )

# 8) Define a formatter for numbers in Turkish style
fmt_millions <- label_number(
  big.mark     = ".",
  decimal.mark = ",",
  accuracy     = 0.01
)

# 9) Add shadowed labels showing absolute values with units
final_plot <- base_plot +
  geom_shadowtext(
    data    = labels_start,
    aes(
      x, y,
      label = paste0(label, " · ", fmt_millions(Value), " million Sm³")
    ),
    size     = 4,
    hjust    = 1,
    nudge_x  = -0.1,
    bg.color = "grey99",
    fontface = "bold"
  ) +
  geom_shadowtext(
    data    = labels_end,
    aes(
      x, y,
      label = paste0(label, " · ", fmt_millions(Value), " million Sm³")
    ),
    size     = 4,
    hjust    = 0,
    nudge_x  = 0.1,
    bg.color = "grey99"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# 10) Save the script to the notebook environment
file_path = "/mnt/data/natural_gas_sankey.R"
with open(file_path, "w", encoding="utf-8") as f:
  f.write(r_code)
file_path
