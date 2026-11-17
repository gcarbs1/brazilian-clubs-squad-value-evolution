# Squad value ranking of the 2026 Série A clubs, January 2020 vs 2026
#
# Reads `data/processed/brazilian_clubs_squad_values.csv` (Transfermarkt
# squad market values, € millions) and draws one landscape chart (16:10,
# 32 × 20 cm at 300 dpi) in `viz/`. Run from the project root:
#
#   Rscript R/squad-value-chart.R
#
# Every club is a ribbon of the same width linking its rank in 2020 to its
# rank in 2026, so the chart reads as a race of positions. The three biggest
# climbers and the three biggest fallers are coloured; the others stay grey
# as context. The values are printed next to each crest.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(ggtext)
  library(ggimage)
  library(marquee)
  library(systemfonts)
  library(scales)
})

if (!file.exists("R/squad-value-chart.R")) {
  stop("Run the script from the project root.")
}

author <- "Gabriel Carbinatto"
width_cm <- 32
height_cm <- 20
dir.create("viz", showWarnings = FALSE)


# 1. Fonts, palette and helpers ------------------------------------------------

font_dir <- file.path("assets", "fonts")
register_font(
  "Libre Baskerville",
  plain = file.path(font_dir, "LibreBaskerville-Regular.ttf"),
  bold = file.path(font_dir, "LibreBaskerville-Bold.ttf"),
  italic = file.path(font_dir, "LibreBaskerville-Italic.ttf")
)
serif <- "Libre Baskerville"

pal <- list(
  background = "#FFFFFF",
  ink = "#111111",
  muted = "#6B6B6B",
  context = "#E1E1E1",
  context_text = "#8A8A8A",
  up = "#0F3A7D",
  down = "#A33A3F"
)

euro <- function(x) str_glue("€{number(x, accuracy = 0.1, big.mark = ',')}m")
signed <- function(x) case_when(x > 0 ~ str_glue("+{x}"),
                                x < 0 ~ str_glue("−{abs(x)}"),
                                TRUE ~ "=")

# Marquee markup: `{#hex **text**}` colours a span in bold.
color_text <- function(text, color) str_glue("{{{color} **{text}**}}")

text_style <- function(align = "justified-left", lineheight = 1.35) {
  classic_style(body_font = serif, lineheight = lineheight) |>
    modify_style("p", align = align, lineheight = lineheight,
                 margin = trbl(0))
}


# 2. Data ----------------------------------------------------------------------

clubs <- read_csv("data/processed/brazilian_clubs_squad_values.csv",
                  show_col_types = FALSE) |>
  mutate(
    rank_2020 = rank(-value_2020, ties.method = "first"),
    rank_2026 = rank(-value_2026, ties.method = "first"),
    change = rank_2020 - rank_2026,
    logo = file.path("assets", "team-logos", logo_file)
  )

n_highlight <- 3
climbers <- slice_max(clubs, change, n = n_highlight, with_ties = FALSE)
fallers <- slice_min(clubs, change, n = n_highlight, with_ties = FALSE)

clubs <- clubs |>
  mutate(
    group = case_when(
      club %in% climbers$club ~ "up",
      club %in% fallers$club ~ "down",
      TRUE ~ "context"
    ),
    fill = c(up = pal$up, down = pal$down, context = pal$context)[group],
    label_color = c(up = pal$up, down = pal$down,
                    context = pal$context_text)[group]
  )

total_2020 <- sum(clubs$value_2020)
total_2026 <- sum(clubs$value_2026)
top <- slice_max(clubs, change, n = 1)
bottom <- slice_min(clubs, change, n = 1)


# 3. Ribbons -------------------------------------------------------------------

# Ranks run top to bottom, so y = -rank. Each ribbon is a band of constant
# height whose centre follows a smoothstep between x = 0 and x = 1.
band <- 0.74
ribbon_x <- c(0, 1)
smooth <- function(t) t^2 * (3 - 2 * t)

ribbons <- clubs |>
  select(club, group, fill, rank_2020, rank_2026) |>
  mutate(path = map2(rank_2020, rank_2026, function(r0, r1) {
    t <- seq(0, 1, length.out = 120)
    centre <- -(r0 + (r1 - r0) * smooth(t))
    tibble(
      x = c(t, rev(t)),
      y = c(centre + band / 2, rev(centre - band / 2))
    )
  })) |>
  unnest(path) |>
  # Grey ribbons first, coloured ribbons on top.
  mutate(layer = if_else(group == "context", 1, 2)) |>
  arrange(layer)


# 4. Labels at both ends -------------------------------------------------------

logo_gap <- 0.045
value_gap <- 0.095
name_gap <- 0.255
change_gap <- 0.61

left <- clubs |>
  transmute(club, logo, label_color, group, y = -rank_2020,
            rank = rank_2020, value = euro(value_2020))
right <- clubs |>
  transmute(club, logo, label_color, group, y = -rank_2026,
            rank = rank_2026, value = euro(value_2026),
            change = signed(change))

end_text <- function(data, x, label, hjust, bold_groups = TRUE,
                     size = 3.3) {
  geom_text(data = data,
            aes(x = x, y = y, label = {{ label }},
                color = label_color,
                fontface = if (bold_groups) {
                  if_else(group == "context", "plain", "bold")
                } else "plain"),
            hjust = hjust, family = serif, size = size)
}


# 5. Chart ---------------------------------------------------------------------

subtitle <- str_glue(
  "The 20 clubs of the 2026 Série A ranked by the market value of their ",
  "squads, according to Transfermarkt. Together the squads went from ",
  "**{euro(total_2020)}** on 1 January 2020 to **{euro(total_2026)}** in 2026, ",
  "and the order changed with it. ",
  "{color_text(top$club, pal$up)} climbed {top$change} places, from ",
  "{ordinal(top$rank_2020)} to {ordinal(top$rank_2026)}, while ",
  "{color_text(bottom$club, pal$down)} fell {abs(bottom$change)}, from ",
  "{ordinal(bottom$rank_2020)} to {ordinal(bottom$rank_2026)}. ",
  "Colours mark the three ",
  "{color_text('biggest climbers', pal$up)} and the three ",
  "{color_text('biggest fallers', pal$down)}."
)

footnote <- str_glue(
  "**Data**: Transfermarkt, squad market values in € millions\n\n",
  "**Note**: every ribbon has the same width and links a club's rank in ",
  "the two years. The right-hand column shows places gained or lost."
)
footer_y <- -22.6

header_y <- 0.15
x_limits <- c(-0.66, 1.66)

chart <- ggplot() +
  geom_polygon(data = ribbons,
               aes(x = x, y = y, group = club, fill = fill),
               color = pal$background, linewidth = 0.35) +
  # Column headers.
  annotate("text", x = 0, y = header_y, label = "1 January 2020",
           family = serif, fontface = "bold", size = 4.4, color = pal$ink,
           hjust = 0.5, vjust = 0) +
  annotate("text", x = 1, y = header_y, label = "2026",
           family = serif, fontface = "bold", size = 4.4, color = pal$ink,
           hjust = 0.5, vjust = 0) +
  annotate("text", x = 1 + change_gap, y = header_y, label = "places",
           family = serif, size = 3.1, color = pal$muted,
           hjust = 1, vjust = 0) +
  # Left end: name, value, crest.
  geom_image(data = left, aes(x = -logo_gap, y = y, image = logo),
             size = 0.022, asp = width_cm / height_cm) +
  end_text(left, -value_gap, value, hjust = 1, bold_groups = FALSE) +
  end_text(left, -name_gap, club, hjust = 1) +
  # Right end: crest, value, name, change.
  geom_image(data = right, aes(x = 1 + logo_gap, y = y, image = logo),
             size = 0.022, asp = width_cm / height_cm) +
  end_text(right, 1 + value_gap, value, hjust = 0, bold_groups = FALSE) +
  end_text(right, 1 + name_gap, club, hjust = 0) +
  end_text(right, 1 + change_gap, change, hjust = 1) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(limits = x_limits, expand = expansion(0)) +
  # Footer: source and note at the left, author at the right.
  annotate("marquee", x = x_limits[1], y = footer_y, label = footnote,
           hjust = 0, vjust = 0, size = 3, colour = pal$ink, family = serif,
           lineheight = 1, style = text_style("left", 1.3)) +
  annotate("text", x = x_limits[2], y = footer_y, label = author,
           hjust = 1, vjust = 0, family = serif, fontface = "bold",
           size = 3, color = pal$ink) +
  scale_y_continuous(expand = expansion(0)) +
  coord_cartesian(ylim = c(-20.6, header_y + 0.6), clip = "off") +
  labs(
    title = "Who climbed Brazilian football's squad value ranking",
    subtitle = subtitle
  ) +
  theme_void(base_family = serif) +
  theme(
    plot.background = element_rect(fill = pal$background, color = NA),
    plot.title = element_text(family = serif, size = 27, color = pal$ink,
                              margin = margin(b = 12)),
    plot.subtitle = element_marquee(
      family = serif, size = 12.5, colour = pal$muted, lineheight = 1,
      width = unit(1, "npc"), margin = margin(b = 22),
      style = text_style("justified-left", 1.5)
    ),
    plot.title.position = "plot",
    plot.margin = margin(32, 40, 50, 40)
  )

ggsave("viz/brazilian-clubs-squad-value-evolution.png", chart,
       width = width_cm, height = height_cm, units = "cm", dpi = 300,
       device = ragg::agg_png, bg = pal$background)
