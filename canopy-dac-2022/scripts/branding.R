library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(glue)
library(here)
library(systemfonts)
## FONTS

if(!all(c("Open Sans", "Bebas Neue") %in% system_fonts()$family)) {
  ## Please download and install two fonts:
  ## Open Sans: https://fonts.google.com/specimen/Open+Sans
  ## Bebas Neue: https://fonts.google.com/specimen/Bebas+Neue
  ## You may need to restart your R session after installing the fonts
  ## for R to be able to see them.
  warning(
    "Please make sure both fonts Open Sans and Bebas Neue are installed!
    You may need to restart your R session after installation."
  )
}


## tag labels ####

# fix_accent_a = function(x) {
#   x = str_replace_all(x, "Ã", "à")
#   x = str_replace_all(x, "\\s+", " ")
#   x
# }

tag_labels = read_csv(here("data/Tags-SY2021-22 Tags.csv")) %>%
  select(tag = `Variable name`, label = `short name`) #%>%
  #mutate(across(everything(), fix_accent_a))

label_tags = function(x, capitalize = "none") {
  if(any(!x %in% tag_labels$tag)) warning("Missing tag label")
  labels = tag_labels$label[match(x, tag_labels$tag)]
  labels[is.na(labels)] = x[is.na(labels)]
  if(capitalize == "title") {
    labels = str_to_title(labels)
  }
  if(capitalize == "first") {
    labels = str_to_sentence(labels)
  }
  return(labels)
}

scale_x_tag = function(...) scale_x_discrete(labels = label_tags)
scale_y_tag = function(...) scale_y_discrete(labels = label_tags)


### Demographic labels ####

dem_labs = c(
  "Number of students" = "nces_total_enrollment",
  "% students of color" = "non_white_percent",
  "% Native American and\nAlaskan Native students" = "aian_percent",
  "% Asian students" = "asian_percent",
  "# Black students" = "black_count",
  "% Black students" = "black_percent",
  "# Hispanic students" = "hispanic_count",
  "% Hispanic students" = "hispanic_percent",
  "% Native Hawaiian and\nPacific Islander students" = "nhpi_percent",
  "% Multiracial students" = "multiple_percent",
  "% White students" = "white_percent",
  "% BIPOC students" = "bipoc_percent",
  "# FRPL eligible" = "FRPL_count",
  "% FRPL eligible" = "FRPL_percent",
  "% FRPL eligible" = "frpl_percent",
  "% English Language Learner" = "ell_percent",
  "% Limited English Proficiency" = "LEP_percent",
  "% Special Education" = "IDEA_percent",
  "% Students with Disabilities" = "swd_percent",
  "Charter schools" = "school_descriptor_charter",
  "District schools" = "school_descriptor_district",
  "Independent schools" = "school_descriptor_independent",
  "Elementary schools" = "level_elem",
  "Middle schools" = "level_middle", 
  "High schools" = "level_high",
  "Level" = "level_simple",
  "Urban schools" = "locale_urban",
  "Suburban schools" = "locale_suburban",
  "Rural schools" = "locale_rural",
  "Locale" = "locale"
)

dem_labs_rv = names(dem_labs)
names(dem_labs_rv) = dem_labs

label_dems = function(dems) coalesce(dem_labs_rv[dems], dems)
scale_x_demo = scale_x_discrete(labels = dem_labs_rv)

## Percent bin labels ####
label_percent_bins = function(breaks) {
  n = length(breaks)
  if(n < 3) stop("Need at least 3 breaks!")
  lo = glue("<{scales::percent_format()(breaks[2])}")
  hi = glue(">{scales::percent_format()(breaks[n - 1])}")
  med = NULL
  if(n > 3) {
    med = paste(
      scales::percent_format()(breaks[2:(n - 2)]),
      scales::percent_format()(breaks[3:(n - 1)]),
      sep = "-"
    )
  }
  return(c(lo, med, hi))
}

# Cluster labels ####
clust_details = c(
  "MR1" = "Educational justice and holistic student supports",
  "MR2" = "Postsecondary pathways and the world outside school",
  "MR4" = "Deeper learning for mastery",
  "MR5" = "Flexible and individualized learning pathways",
  "MR3" = "Blended learning"
)
clust_details_r = setNames(names(clust_details), clust_details)

clust_labels = c(
  `Educational justice and holistic student supports` = "Educational justice and\nholistic student supports", 
  `Postsecondary pathways and the world outside school` = "Postsecondary pathways\nand the world outside school", 
  `Deeper learning for mastery` = "Deeper learning for mastery",
  `Flexible and individualized learning pathways` = "Flexible and individualized\nlearning pathways", 
  `Blended learning` = "Blended learning"
)

label_clust = function(x) {
  if(x %in% names(clust_details)) {
    x = clust_details[x]
  }
  clust_labels[x]
}



# Colors ####

# main Transcend colors
transcend_cols = c(
  red = "#EF464B",
  blue = "#1A4C81",
  teal = "#59C3B4",
  `light blue` = "#ADE0EE"
)

transcend_cols_noname = function(col) {
  unname(transcend_cols[col])
}

# secondary Transcend colors when needed
transcend_cols2 = c(
  "#BC2582",
  "#FFA630",
  "#FFDE42",
  "#99C24D",
  "#218380",
  "#D3B7D7"
)

# Transcend shades of gray
# (dark to light)
transcend_grays = c(
  "#4D4D4F",
  "#9D9FA2",
  "#D1D3D4"
)

na_col = transcend_grays[2]

## ggplot themes ####
theme_transcend = theme_gdocs(base_size = 16, base_family = "Open Sans") +
  theme(
    plot.title = element_text(family = "Bebas Neue", color = "black"),
    plot.background = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    panel.border = element_rect(colour = transcend_grays[1]),
    strip.text = element_text(size = rel(0.8)),
    plot.margin = margin(10, 24, 10, 10, "pt")
  )

theme_transcend_sparse = theme_few(base_size = 16, base_family = "Open Sans") +
  theme(
    plot.title = element_text(family = "Bebas Neue", colour = "black"),
    axis.text = element_text(colour = "black")
  )

theme_set(theme_transcend)

## ggplot scales ####

locale_cols = transcend_cols[c("blue", "teal", "light blue")] %>% 
  setNames(c("Urban", "Suburban", "Rural"))
scale_fill_locale = scale_fill_manual(values = locale_cols, na.value = na_col)
scale_color_locale = scale_color_manual(values = locale_cols, na.value = na_col)

hs_cols = transcend_cols[c("red", "blue")] %>% 
  setNames(c("High School", "Not High School"))

level_cols = transcend_cols[c("light blue", "teal", "red")] %>% 
  setNames(c("Elementary", "Middle", "High"))
scale_fill_level = scale_fill_manual(values = level_cols, na.value = na_col)
scale_color_level = scale_color_manual(values = level_cols, na.value = na_col)

scale_fill_charter = scale_fill_manual(
  values = unname(transcend_cols[c("teal", "blue")]),
  labels = c("Yes" = "Charter", "No" = "Traditional"),
  na.value = na_col
)

scale_fill_type = scale_fill_manual(
  values = unname(transcend_cols[c("teal", "blue", "light_blue")]),
  labels = c("Charter", "District", "Independent"),
  na.value = na_col
)

bar_y_scale_count = 
  scale_y_continuous(
    labels = scales::comma_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1)),
    breaks = scales::breaks_extended(Q = c(1, 5, 2, 4, 3))
) 

bar_y_scale_percent = 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) 

bar_x_scale_percent = 
  scale_x_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) 

bar_x_scale_count = 
  scale_x_continuous(
    labels = scales::comma_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1)),
    breaks = scales::breaks_extended(Q = c(1, 5, 2, 4, 3))
  ) 


bar_theme = theme(panel.grid.major.x = element_blank())


scale_fill_gradient_transcend = scale_fill_gradient2(
      limits = c(-1, 1),
      expand = c(0, 0), 
      low = transcend_cols["blue"], 
      mid = "white",
      high = transcend_cols["light blue"],
      midpoint = 0
    )

geom_col = function(..., width = 0.6) ggplot2::geom_col(..., width = width)

#' Standard Transcend save of plots/data
#' 
#' This is a ggsave wrapper that will save (1) a PNG file, (2) a SVG file,
#' and (3) optionally a CSV file with the data from the plot so it can
#' be easily referenced.
#'
#' @param plot a ggplot object to save
#' @param file the file name
#' @param dir the directory to save in
#' @param fig_width width in inches, default 9
#' @param fig_height height in inches, default 7
#' @param write_data boolean indicating whether or not to write a CSV with data,
#' default is TRUE.
ggsave_transcend = function(plot, file, dir, fig_width = 9, fig_height = 7, write_data = TRUE) {
  for (ext in c("png", "svg")) {
    ggsave(filename = sprintf("%s/%s.%s", dir, file, ext),
           plot = plot,
           width = fig_width, height = fig_height)
  }
  if(write_data) write_csv(plot$data, file = sprintf("%s/%s_data.csv", dir, file))
}


## Additional ggplot helper functions #####

## Reordering within facets
## from: https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R

#' Reorder an x or y axis within facets
#'
#' Reorder a column before plotting with faceting, such that the values are ordered
#' within each facet. This requires two functions: \code{reorder_within} applied to
#' the column, then either \code{scale_x_reordered} or \code{scale_y_reordered} added
#' to the plot.
#' This is implemented as a bit of a hack: it appends ___ and then the facet
#' at the end of each string.
#'
#' @param x Vector to reorder.
#' @param by Vector of the same length, to use for reordering.
#' @param within Vector of the same length that will later be used for faceting
#' @param fun Function to perform within each subset to determine the resulting
#' ordering. By default, mean.
#' @param sep Separator to distinguish the two. You may want to set this manually
#' if ___ can exist within one of your labels.
#' @param ... In \code{reorder_within} arguments passed on to \code{\link{reorder}}.
#' In the scale functions, extra arguments passed on to
#' \code{\link[ggplot2]{scale_x_discrete}} or \code{\link[ggplot2]{scale_y_discrete}}.
#'
#' @source "Ordering categories within ggplot2 Facets" by Tyler Rinker:
#' \url{https://trinkerrstuff.wordpress.com/2016/12/23/ordering-categories-within-ggplot2-facets/}
#'
#' @examples
#'
#' library(tidyr)
#' library(ggplot2)
#'
#' iris_gathered <- gather(iris, metric, value, -Species)
#'
#' # reordering doesn't work within each facet (see Sepal.Width):
#' ggplot(iris_gathered, aes(reorder(Species, value), value)) +
#'   geom_boxplot() +
#'   facet_wrap(~ metric)
#'
#' # reorder_within and scale_x_reordered work.
#' # (Note that you need to set scales = "free_x" in the facet)
#' ggplot(iris_gathered, aes(reorder_within(Species, value, metric), value)) +
#'   geom_boxplot() +
#'   scale_x_reordered() +
#'   facet_wrap(~ metric, scales = "free_x")
#'
#' @export
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}


#' @rdname reorder_within
#' @export
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


#' @rdname reorder_within
#' @export
scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}


