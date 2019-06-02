#' Calculate positions for seed
#'
#' @param seed_shape possible seed shapes, character
#' @param seed_r seed radius, numeric
#'
#' @return a data frame with x and y positions of the seed polygon
#' @import dplyr tibble purrr tidyr zoo EnvStats ggplot2 rap
#' @importFrom stats runif
#' @export
#'
makedf_seed <- function(seed_shape, seed_r) {
  switch(seed_shape,
         "none" = {
           seed_df <- tibble(x = 0, y = 0, id = 1, type = "polygon")
         },
         "circle" = {
           seed_df <- tibble(angle = seq(0, 2*pi, length.out = 200), x = seed_r*cos(angle), y = seed_r*sin(angle), id = 1, type = "polygon")
         },
         "diamond" = {
           seed_df <- tibble(angle = seq(0, 2*pi, length.out = 5), x = seed_r*cos(angle), y = seed_r*sin(angle), id = 1, type = "polygon")
         },
         "square" = {
           seed_df <- tibble(angle = seq(pi/4, pi/4+2*pi, length.out = 5), x = seed_r*cos(angle), y = seed_r*sin(angle), id = 1, type = "polygon")
         }
  )
  return(seed_df)
}

#' Calculate x and y positions for a path
#'
#' @param shape the shape to draw, character
#' @param r the radius of the shape, numeric
#' @param line the type of line, character
#' @param width the width of the line, numeric
#'
#' @return a dataframe with x and y positions and associated parameters
#' @export
#'
makedf_line <- function(shape, r, line, width) {
  switch(shape,
         "none" = {
           df <- tibble(x = 0, y = 0, id = 1, type = "path", linetype = line, linewidth = width)
         },
         "circle" = {
           df <- tibble(angle = seq(0, 2*pi, length.out = 200), x = r*cos(angle), y = r*sin(angle), id = 1, r = r, type = "path", linetype = line, linewidth = width)
         },
         "diamond" = {
           df <- tibble(angle = seq(0, 2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "path", linetype = line, linewidth = width)
         },
         "square" = {
           df <- tibble(angle = seq(pi/4, pi/4+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "path", linetype = line, linewidth = width)
         },
         "square_left" = {
           df <- tibble(angle = seq((67.5*pi/180), (67.5*pi/180)+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "path", linetype = line, linewidth = width)
         },
         "square_right" = {
           df <- tibble(angle = seq((22.5*pi/180), (22.5*pi/180)+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "path", linetype = line, linewidth = width)
         }
  )
  return(df)
}

#' Calculate positions of inscribed planets
#'
#' @param shape the shape that the planets lie on, character
#' @param r the radius of the shape, numeric
#' @param size the size of the planets, numeric
#'
#' @return a dataframe with x and y positions and point size
#' @export
#'
makedf_inscribed_planets <- function(shape, r, size) {
  switch(shape,
         "none" = {
           df <- tibble(x = 0, y = 0, id = 1, type = "point", size = size)
         },
         "circle" = {
           df <- tibble(angle = seq(0, 2*pi, length.out = 200), x = r*cos(angle), y = r*sin(angle), id = 1, type = "point", size = size)
         },
         "diamond" = {
           df <- tibble(angle = seq(0, 2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "point", size = size)
         },
         "square" = {
           df <- tibble(angle = seq(pi/4, pi/4+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "point", size = size)
         },
         "square_left" = {
           df <- tibble(angle = seq((67.5*pi/180), (67.5*pi/180)+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "point", size = size)
         },
         "square_right" = {
           df <- tibble(angle = seq((22.5*pi/180), (22.5*pi/180)+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "point", size = size)
         }
  )
  return(df)
}

#' Make a dataframe of outline positions
#'
#' @param nlines the number of outlines, numeric
#' @param r the radius of the outlines, numeric
#' @param shapes the shape of the outlines, character
#' @param linetype the type of line for the outlines, character
#' @param width the width of the outlines, numeric
#'
#' @return a dataframe with x and y positions and associated parameters
#' @export
#'
makedf_outlines <- function(nlines, r, shapes, linetype, width) {
  df <- tibble(id = 1:nlines, r = r, shape = shapes, linetype = linetype, linewidth = width) %>%
    nest(r, shape, linetype, linewidth) %>%
    mutate(points = map(data, ~makedf_line(shape = .x$shape, r = .x$r, line = .x$linetype, width = .x$linewidth))) %>%
    unnest(points, .id = "parent")

  return(df)

}

#' Make a dataframe of orbit positions
#'
#' @param nlines number of orbits, numeric
#' @param r radius of orbits, numeric
#' @param linetype type of line for orbits, character
#' @param linewidth width of the orbits, numeric
#'
#' @return
#' @export
#'
makedf_orbits <- function(nlines, r, linetype, linewidth) {
  df <- tibble(id = 1:nlines, r = r, linetype = linetype, linewidth = linewidth) %>%
    nest(r, linetype, linewidth) %>%
    mutate(points = map(data, ~makedf_line(r = .x$r, shape = "circle", line = .x$linetype, width = .x$linewidth))) %>%
    unnest(points, .id = "parent")

  return(df)
}

#' Make a dataframe for planet positions
#'
#' @param cen_x the center x coordinate of each planet, numeric
#' @param cen_y the center y coordinate of each planet, numeric
#' @param r the radius of each planet, numeric
#'
#' @return
#' @export
#'
makedf_planets <- function(cen_x, cen_y, r) {
  tibble(angle = seq(0, 2*pi, length.out = 50), x = cen_x + r*cos(angle), y = cen_y + r*sin(angle))
}

#' Choose the number of inscribed planets
#'
#' @param i
#' @param probs
#'
#' @return
#' @export
#'
choose_inscribed <- function(i, probs) {
  rand <- sample(0:1, 1, prob = probs)

  if(rand > 0) {
    tibble(planet = 4, num = i)
  } else {
    tibble(planet = 2, num = i)
  }
}

#' Calculate the radius of inscribed shapes
#'
#' @param shape1 the 'outer' shape, character
#' @param shape2 the 'inner' shape which we will calculate the radius for, character
#' @param r1 the radius of the 'outer' shape, numeric
#'
#' @return the radius of the inner shape
#' @export
#'
calc_inscribed_r <- function(shape1, shape2, r1) {
  if(shape1 == "circle") {
    r <- r1
  }
  if(shape1 == "diamond") {
    #radius of circle inscribed in diamond
    #diameter is length of side of diamond
    #chord length (diamond side) is 2*r*sin(angle/2)
    r <- (2*r1*sin((pi/2)/2)/2)
  }
  if(shape1 == "square") {
    r <- 0.5*sqrt(2)*r1 #45 45 90 triangle given hyp is r1
  }
  return(r)
}

#' Title
#'
#' @param input
#' @param glitch_r_min
#' @param glitch_r_max
#'
#' @return

glitch_connected <- function(input, glitch_r_min = 0.5, glitch_r_max = 2)  {
  glitch_init <- sample(min(input$angle):max(input$angle), 1)
  glitch_bump <- abs(glitch_init + sample(seq(-1.5, 1.5, by = 0.05), 1))

  glitch_start <- min(glitch_init, glitch_bump)
  glitch_end <- max(glitch_init, glitch_bump)

  r_bump <- sample(seq(glitch_r_min, glitch_r_max, by = 0.01), 1)

  input %>%
    filter(angle > glitch_start & angle < glitch_end) %>%
    mutate(r = r*r_bump, x = r*cos(angle), y = r*sin(angle)) %>%
    right_join(input, by = c("id", "angle")) %>%
    mutate(x_new = ifelse(is.na(x.x), x.y, x.x), y_new = ifelse(is.na(y.x), y.y, y.x),
           r_new = ifelse(is.na(r.x), r.y, r.x)) %>%
    select(id, angle, x = x_new, y = y_new, r = r_new, linetype = linetype.y, linewidth = linewidth.y)
}

#' Title
#'
#' @param input
#' @param num
#' @param glitch_r_min
#' @param glitch_r_max
#'
#' @return

glitch_shattered <- function(input, num, glitch_r_min = 0.5, glitch_r_max = 2)  {
  glitch_init <- sample(min(input$angle):max(input$angle), 1)
  glitch_bump <- abs(glitch_init + sample(seq(-1.5, 1.5, by = 0.05), 1))

  glitch_start <- min(glitch_init, glitch_bump)
  glitch_end <- max(glitch_init, glitch_bump)

  r_bump <- sample(seq(glitch_r_min, glitch_r_max, by = 0.01), 1)

  input %>%
    filter(angle > glitch_start & angle < glitch_end) %>%
    mutate(r = r*r_bump, x = r*cos(angle), y = r*sin(angle), id2 = paste(id, "glitch", num, sep = "_")) %>%
    right_join(input, by = c("id", "angle")) %>%
    mutate(x_new = ifelse(is.na(x.x), x.y, x.x), y_new = ifelse(is.na(y.x), y.y, y.x),
           r_new = ifelse(is.na(r.x), r.y, r.x),
           id2_new = ifelse(!is.na(id2.x), id2.x, id2.y)) %>%
    select(id, id2 = id2_new, angle, x = x_new, y = y_new, r = r_new, linetype = linetype.y, linewidth = linewidth.y)
}

#' Title
#'
#' @param input
#' @param min_spikes
#' @param max_spikes
#' @param min_spike_r
#' @param max_spike_r
#' @param min_spike_jitter
#' @param max_spike_jitter
#'
#' @return
#'
glitch_spike <- function(input, min_spikes = 30, max_spikes = 60, min_spike_r = 0.5, max_spike_r = 2, min_spike_jitter = -0.2, max_spike_jitter = 0.2) {
  num_spikes <- sample(min_spikes:max_spikes, 1)
  r_bump <- sample(seq(min_spike_r, max_spike_r, by = 0.01), num_spikes, replace = TRUE)
  spikes <- sample(unique(input$angle), num_spikes)
  spike_jitter_x <- sample(seq(min_spike_jitter, max_spike_jitter, by = 0.01), num_spikes, replace = TRUE)
  spike_jitter_y <- sample(seq(min_spike_jitter, max_spike_jitter, by = 0.01), num_spikes, replace = TRUE)

  spike_glitch <-
    input %>%
    filter(angle %in% spikes) %>%
    mutate(r = r*r_bump, x = r*cos(angle + spike_jitter_x), y = r*sin(angle + spike_jitter_y)) %>%
    right_join(input, by = c("id", "angle")) %>%
    mutate(x_new = ifelse(is.na(x.x), x.y, x.x), y_new = ifelse(is.na(y.x), y.y, y.x),
           r_new = ifelse(is.na(r.x), r.y, r.x)) %>%
    select(id, angle, x = x_new, y = y_new, r = r_new, linetype = linetype.y, linewidth = linewidth.y)
}

'%!in%' <- function(x, y) {
  !('%in%'(x,y))
}


