# The scarves changed places
#
# Squad market value of the 20 clubs of the 2026 Série A on 1 January 2020
# and in 2026 (Transfermarkt, € millions), drawn as supporters' scarves.
# Each scarf runs from the club's rank in 2020 to its rank in 2026, is
# knitted in the club's colours and is as wide as the squad is valuable.
# The three biggest climbers and the three biggest fallers are in full
# colour; the other scarves are faded.
#
# One landscape chart (16:10, 32 × 20 cm at 300 dpi) in `viz/`. Run from
# the project root:
#
#   Rscript R/squad-value-chart.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(ggimage)
  library(ggtext)
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


# 1. Fonts and palette ---------------------------------------------------------

font_dir <- file.path("assets", "fonts")
font_file <- function(name) file.path(font_dir, name)
register_font("Barlow Condensed Black",
              plain = font_file("BarlowCondensed-Black.ttf"))
register_font("Barlow Condensed",
              plain = font_file("BarlowCondensed-Medium.ttf"),
              bold = font_file("BarlowCondensed-Bold.ttf"))
register_font("Barlow",
              plain = font_file("Barlow-Regular.ttf"),
              bold = font_file("Barlow-SemiBold.ttf"),
              italic = font_file("Barlow-Italic.ttf"))
display <- "Barlow Condensed Black"
condensed <- "Barlow Condensed"
body <- "Barlow"

ui <- list(
  paper = "#F3EEE4",
  ink = "#1A1A1A",
  muted = "#7A7268",
  faint = "#B9B0A3",
  up = "#1D5FA8",
  down = "#B3261E"
)

# Mixes a colour with the paper; `amount` = 1 returns the paper.
fade <- function(colour, amount) {
  from <- col2rgb(colour) / 255
  to <- col2rgb(ui$paper) / 255
  mixed <- from * (1 - amount) + to * amount
  rgb(mixed[1, ], mixed[2, ], mixed[3, ])
}

euro <- function(x) str_glue("€{number(x, accuracy = 0.1, big.mark = ',')}m")
signed <- function(x) case_when(x > 0 ~ str_glue("+{x}"),
                                x < 0 ~ str_glue("−{abs(x)}"),
                                TRUE ~ "=")


# 2. Club scarves --------------------------------------------------------------

# Colours repeat along each scarf in the order given, approximating the
# stripes of the club's kit.
white <- "#FBFAF7"
black <- "#161616"
scarves <- tribble(
  ~club,                  ~stripes,
  "Flamengo",             c("#C8102E", black),
  "Palmeiras",            c("#006437", "#006437", white),
  "Cruzeiro",             c("#0B3D91", "#0B3D91", white),
  "Bahia",                c("#0E3990", white, "#D3122C", white),
  "Fluminense",           c("#7A1530", white, "#00613C", white),
  "Corinthians",          c(white, white, black),
  "Vasco",                c(black, black, white),
  "Botafogo",             c(black, white),
  "Atlético Mineiro",     c(white, black, black),
  "Santos",               c(white, black, white, white),
  "RB Bragantino",        c(white, "#D7193A"),
  "Grêmio",               c("#0D80BF", black, "#0D80BF", white),
  "São Paulo",            c(white, "#E2231A", black, "#E2231A"),
  "Internacional",        c("#E30613", "#E30613", white),
  "Athletico Paranaense", c("#C8102E", black, black),
  "Vitória",              c("#E2001A", black, black),
  "Mirassol",             c("#F2D40C", "#00703C"),
  "Coritiba",             c(white, "#006B3F", "#006B3F"),
  "Remo",                 c("#13265C", "#13265C", white),
  "Chapecoense",          c("#00843D", white)
)


# 3. Data ----------------------------------------------------------------------

clubs <- read_csv("data/processed/brazilian_clubs_squad_values.csv",
                  show_col_types = FALSE) |>
  mutate(
    rank_2020 = rank(-value_2020, ties.method = "first"),
    rank_2026 = rank(-value_2026, ties.method = "first"),
    change = rank_2020 - rank_2026,
    logo = file.path("assets", "team-logos", logo_file)
  ) |>
  left_join(scarves, by = "club")

stopifnot(!any(map_lgl(clubs$stripes, is.null)))

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
    highlighted = group != "context"
  )

total_2020 <- sum(clubs$value_2020)
total_2026 <- sum(clubs$value_2026)


# 4. Scarf geometry ------------------------------------------------------------

# Ranks run top to bottom (y = -rank). The scarf's centre follows a
# smoothstep between x = 0 and x = 1 and its width moves linearly from the
# 2020 to the 2026 value. A row is one unit tall; the most valuable squad
# fills `max_width` of it.
max_width <- 0.86
width_scale <- max_width / max(c(clubs$value_2020, clubs$value_2026))
n_blocks <- 22
smooth <- function(t) t^2 * (3 - 2 * t)

centre_at <- function(t, r0, r1) -(r0 + (r1 - r0) * smooth(t))
width_at <- function(t, v0, v1) (v0 + (v1 - v0) * t) * width_scale

scarf_blocks <- function(club, rank_2020, rank_2026, value_2020, value_2026,
                         stripes, highlighted, ...) {
  breaks <- seq(0, 1, length.out = n_blocks + 1)
  map_dfr(seq_len(n_blocks), function(k) {
    t <- seq(breaks[k], breaks[k + 1], length.out = 8)
    centre <- centre_at(t, rank_2020, rank_2026)
    half <- width_at(t, value_2020, value_2026) / 2
    colour <- stripes[(k - 1) %% length(stripes) + 1]
    tibble(
      club = club, block = k,
      x = c(t, rev(t)),
      y = c(centre + half, rev(centre - half)),
      fill = if (highlighted) colour else fade(colour, 0.68)
    )
  })
}

scarf_outline <- function(club, rank_2020, rank_2026, value_2020, value_2026,
                          highlighted, ...) {
  t <- seq(0, 1, length.out = 200)
  centre <- centre_at(t, rank_2020, rank_2026)
  half <- width_at(t, value_2020, value_2026) / 2
  tibble(club = club, x = c(t, rev(t)),
         y = c(centre + half, rev(centre - half)),
         colour = if (highlighted) ui$ink else ui$faint)
}

# Fringe: short threads beyond each end, spaced across the scarf's width.
fringe_length <- 0.028
fringe <- function(club, rank_2020, rank_2026, value_2020, value_2026,
                   stripes, highlighted, ...) {
  ends <- tibble(
    side = c(-1, 1), x = c(0, 1),
    y = c(-rank_2020, -rank_2026),
    w = c(value_2020, value_2026) * width_scale,
    colour = stripes[c(1, (n_blocks - 1) %% length(stripes) + 1)]
  )
  pmap_dfr(ends, function(side, x, y, w, colour) {
    n <- max(1, round(w / 0.045))
    offsets <- if (n == 1) 0 else seq(-w / 2, w / 2, length.out = n) * 0.86
    tibble(club = club, x = x, xend = x + side * fringe_length,
           y = y + offsets, yend = y + offsets,
           colour = if (highlighted) colour else fade(colour, 0.68))
  })
}

# Knit ribbing: thin lengthwise lines across the highlighted scarves.
ribbing <- function(club, rank_2020, rank_2026, value_2020, value_2026,
                    highlighted, ...) {
  if (!highlighted) return(NULL)
  t <- seq(0, 1, length.out = 200)
  centre <- centre_at(t, rank_2020, rank_2026)
  half <- width_at(t, value_2020, value_2026) / 2
  map_dfr(c(-0.55, -0.18, 0.18, 0.55), function(k) {
    tibble(club = club, line = k, x = t, y = centre + k * half)
  })
}

draw_order <- clubs |> arrange(highlighted, desc(group))
blocks <- pmap_dfr(draw_order, scarf_blocks) |>
  mutate(id = str_glue("{club}-{block}"),
         layer = match(club, draw_order$club))
outlines <- pmap_dfr(draw_order, scarf_outline) |>
  mutate(layer = match(club, draw_order$club))
threads <- pmap_dfr(draw_order, fringe)
ribs <- pmap_dfr(draw_order, ribbing) |>
  mutate(layer = match(club, draw_order$club))

# Context first, highlighted scarves on top, each with its outline.
scarf_layers <- map(seq_len(nrow(draw_order)), function(i) {
  list(
    geom_polygon(data = filter(blocks, layer == i),
                 aes(x = x, y = y, group = id, fill = fill),
                 color = NA),
    geom_path(data = filter(ribs, layer == i),
              aes(x = x, y = y, group = interaction(club, line)),
              color = "#000000", alpha = 0.12, linewidth = 0.12),
    geom_polygon(data = filter(outlines, layer == i),
                 aes(x = x, y = y, group = club, color = colour),
                 fill = NA, linewidth = 0.18)
  )
})


# 5. Labels --------------------------------------------------------------------

crest_gap <- 0.06

left <- clubs |>
  transmute(club, logo, highlighted, group, y = -rank_2020,
            name = str_to_upper(club), value = euro(value_2020))
right <- clubs |>
  transmute(club, logo, highlighted, group, y = -rank_2026,
            name = str_to_upper(club), value = euro(value_2026),
            change = signed(change))

label_colour <- function(data) {
  case_when(data$group == "up" ~ ui$up, data$group == "down" ~ ui$down,
            TRUE ~ ui$muted)
}
left$colour <- label_colour(left)
right$colour <- label_colour(right)

# Fixed columns: value next to the crest, then the name, then (right
# side only) places gained or lost.
value_gap <- 0.092
name_gap <- 0.178
change_x <- 1.5

label_text <- function(data, column, x, hjust, family, fontface = "plain",
                       size = 3.1) {
  geom_text(data = data,
            aes(x = x, y = y, label = .data[[column]], color = colour),
            hjust = hjust, family = family, fontface = fontface, size = size)
}

# 6. Legend: what the width means ----------------------------------------------

legend_values <- c(50, 100, 200)
legend_x <- 1.12
legend_y <- 3.0
legend <- tibble(
  value = legend_values,
  x = legend_x + c(0, 0.13, 0.26),
  half = value * width_scale / 2,
  label = str_glue("€{value}m")
)


# 7. Text ----------------------------------------------------------------------

title <- "THE SCARVES CHANGED PLACES"
deck <- str_glue(
  "The squads of the 20 clubs in the 2026 Série A were worth ",
  "**{euro(total_2020)}** on 1 January 2020 and **{euro(total_2026)}** in ",
  "2026, according to Transfermarkt. Each scarf joins a club's rank in the ",
  "two years and is as wide as its squad is valuable. ",
  "<span style='color:{ui$up};'>**Cruzeiro, Bahia and Botafogo**</span> ",
  "climbed the most, ",
  "<span style='color:{ui$down};'>**São Paulo, Grêmio and Internacional**",
  "</span> fell the most."
)
footnote <- str_glue(
  "**Data** Transfermarkt squad market values, € millions. ",
  "Scarf colours follow each club's kit."
)


# 8. Chart ---------------------------------------------------------------------

x_limits <- c(-0.5, 1.5)
y_top <- 6.4
header_y <- 0.2

chart <- ggplot() +
  # Year headers.
  annotate("text", x = 0, y = header_y, label = "2020", family = display,
           size = 9, color = ui$ink, hjust = 0.5, vjust = 0) +
  annotate("text", x = 1, y = header_y, label = "2026", family = display,
           size = 9, color = ui$ink, hjust = 0.5, vjust = 0) +
  scarf_layers +
  geom_segment(data = threads,
               aes(x = x, xend = xend, y = y, yend = yend, color = colour),
               linewidth = 0.32, lineend = "round") +
  # Crests and labels.
  geom_image(data = left, aes(x = -crest_gap, y = y, image = logo),
             size = 0.019, asp = width_cm / height_cm) +
  geom_image(data = right, aes(x = 1 + crest_gap, y = y, image = logo),
             size = 0.019, asp = width_cm / height_cm) +
  label_text(left, "value", -value_gap, 1, body, size = 2.95) +
  label_text(left, "name", -name_gap, 1, condensed, "bold") +
  label_text(right, "value", 1 + value_gap, 0, body, size = 2.95) +
  label_text(right, "name", 1 + name_gap, 0, condensed, "bold") +
  label_text(right, "change", change_x, 1, body, size = 2.95) +
  # Title block.
  annotate("text", x = x_limits[1], y = y_top, label = title,
           family = display, size = 17, color = ui$ink, hjust = 0,
           vjust = 1) +
  annotate("textbox", x = x_limits[1], y = y_top - 2.75, label = deck,
           family = body, size = 3.7, color = ui$ink, hjust = 0, vjust = 1,
           width = unit(17.5, "cm"), fill = NA, box.color = NA,
           box.padding = unit(0, "pt"), lineheight = 1.35) +
  # Width legend.
  annotate("text", x = legend_x, y = legend_y + 1.15,
           label = "SCARF WIDTH = SQUAD VALUE", family = condensed,
           fontface = "bold", size = 3.1, color = ui$ink, hjust = 0) +
  geom_rect(data = legend,
            aes(xmin = x, xmax = x + 0.045, ymin = legend_y - half,
                ymax = legend_y + half),
            fill = ui$ink) +
  geom_text(data = legend,
            aes(x = x + 0.055, y = legend_y, label = label),
            family = body, size = 2.9, color = ui$ink, hjust = 0) +
  # Footer.
  annotate("richtext", x = x_limits[1], y = -21.15, label = footnote,
           family = body, size = 2.8, color = ui$muted, hjust = 0,
           vjust = 1, fill = NA, label.color = NA,
           label.padding = unit(0, "pt")) +
  annotate("text", x = x_limits[2], y = -21.15, label = str_to_upper(author),
           family = condensed, fontface = "bold", size = 3, color = ui$ink,
           hjust = 1, vjust = 1) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = expansion(0)) +
  scale_y_continuous(expand = expansion(0)) +
  coord_cartesian(xlim = x_limits, ylim = c(-21.7, y_top), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = ui$paper, color = NA),
    plot.margin = margin(30, 40, 22, 40)
  )

ggsave("viz/brazilian-clubs-squad-value-evolution.png", chart,
       width = width_cm, height = height_cm, units = "cm", dpi = 300,
       device = ragg::agg_png, bg = ui$paper)
