#' Generate "glitched" orbital glyphs
#'
#' @param seed_probs probabilities of choosing each shape (none, circle,
#'   diamond, square), numeric vector of length 4
#' @param glitch_type the type of glitch you want to introduce (options:
#'   "spike", "connected", "shattered"), character
#' @param pareto2_prob probabilities of having a second set of pareto rings,
#'   numeric vector of length 2
#' @param glitch_params a list of parameters for the glitch options, all
#'   parameters must be specified, but only ones pertient to the glitch_type
#'   will be used
#'
#' @return a glitched orbital ggplot
#' @export
#'
orbit_glitch <- function(seed_probs = c(1, 0, 0, 0), glitch_type = "spike", pareto2_prob = c(0.2, 0.8),
                             glitch_params = list(num_glitches = 10, glitch_r_min = 0.5, glitch_r_max = 2,
                                                  min_spikes = 30, max_spikes = 60, min_spikes2 = 15,
                                                  max_spikes2 = 40, min_spike_jitter = -0.2, max_spike_jitter = 0.2)) {
  #make seed df
  seed_opts <- c("none", "circle", "diamond", "square")
  seed_probs <- seed_probs
  seed_shape <- sample(seed_opts, 1, prob = seed_probs)
  seed_r <- runif(1, 0.1, 0.25)
  seed <- makedf_seed(seed_shape, seed_r)

  #choose seed outlines
  num_seed_outlines <- sample(1:3, 1)
  seed_outline_bumps <- c(0.02, 0.03, 0.04)[1:num_seed_outlines]
  seed_outline_r <- rep(seed_r, times = num_seed_outlines) + seed_outline_bumps[1:num_seed_outlines]
  seed_outline_linetype <- sample(c("solid", "dotted"), num_seed_outlines, replace = TRUE, prob = c(0.8, 0.2))
  seed_outline_width <- c(0.2, 0.2, 0.18)[1:num_seed_outlines]
  seed_outline_shapes <- seed_shape
  seed_outlines <- makedf_outlines(num_seed_outlines, seed_outline_r, seed_outline_shapes, seed_outline_linetype, seed_outline_width)

  #choose orbits
  num_orbits <- sample(1:4, 1)
  orbit_bumps <- c(0, 0.03, 0.1, 0.13)[1:num_orbits]
  orbit_params <- tibble(num = 1:num_orbits, orbit_bumps)
  orbit_params$orbit_widths <- c(sample(seq(0.15, 0.2, by = 0.01), 1), sample(seq(0.2, 0.25, by = 0.01), 1), sample(seq(0.2, 0.25, by = 0.01), 1), sample(seq(0.15, 0.2, by = 0.01), 1))[1:num_orbits]
  orbit_params$orbit_linetype <- sample(c("solid", "dotted"), num_orbits, replace = TRUE, prob = c(0.8, 0.2))
  thick_outline <- sample(1:8, 1)

  #adjust orbits it one is thick
  if(thick_outline %in% orbit_params$num) {
    orbit_params$orbit_widths[thick_outline] <- sample(seq(0.7, 1.5, by = 0.01), 1)
    orbit_params$orbit_linetype[thick_outline] <- "solid"

    orbit_bump_big <-
      orbit_params %>%
      filter(num > thick_outline) %>%
      mutate(orbit_bumps = orbit_bumps + 0.02) %>%
      pull(orbit_bumps)

    orbit_bump_small <-
      orbit_params %>%
      filter(num < thick_outline) %>%
      mutate(orbit_bumps = orbit_bumps - 0.02) %>%
      pull(orbit_bumps)

    orbit_params$orbit_bumps <- c(orbit_bump_small, orbit_params$orbit_bumps[thick_outline], orbit_bump_big)
  }

  orbit_r <- rep(1, times = num_orbits) + orbit_params$orbit_bumps
  orbits <- makedf_orbits(num_orbits, orbit_r, orbit_params$orbit_linetype, orbit_params$orbit_widths)

  pareto_start <- sample(0.35:0.6, 1)
  num_paretos <- sample(3:50, 1)
  pareto_r <- rpareto(num_paretos, pareto_start, shape = sample(3:4, 1))
  pareto_linetype <- sample(c("solid", "dotted"), num_paretos, replace = TRUE, prob = c(0.9, 0.1))
  pareto_width <- sample(seq(0.1, 0.25, by = 0.01), num_paretos, replace = TRUE)

  pareto_orbits <- makedf_orbits(num_paretos, r = pareto_r, linetype = pareto_linetype, linewidth = pareto_width)

  pareto_2 <- sample(c(TRUE, FALSE), 1, prob = pareto2_prob)
  if(pareto_2) {
    pareto_start2 <- sample(1:1.5, 1)
    num_paretos2 <- sample(1:20, 1)
    pareto_r2 <- rpareto(num_paretos2, pareto_start2, shape = sample(3:4, 1))
    pareto_linetype2 <- sample(c("solid", "dotted"), num_paretos2, replace = TRUE, prob = c(0.9, 0.1))
    pareto_width2 <- sample(seq(0.1, 0.4, by = 0.01), num_paretos2, replace = TRUE)

    pareto_orbits2 <- makedf_orbits(num_paretos2, r = pareto_r2, linetype = pareto_linetype2, linewidth = pareto_width2)
  } else {
    pareto_orbits2 <- data.frame(x = 0, y = 0, id = 0, parent = 1, linewidth = 0, linetype = "solid")
  }

  switch(glitch_type,
         "connected" = {
           ###connected glitch
           glitch_seed <- glitch_connected(pareto_orbits, glitch_r_min = glitch_params$glitch_r_min, glitch_r_max = glitch_params$glitch_r_max)
           glitch_list <- list(glitch_seed)
           for(i in 2:glitch_params$num_glitches) {
             glitch_list[[i]] <- glitch_connected(glitch_list[[i - 1]], glitch_r_min = glitch_params$glitch_r_min, glitch_r_max = glitch_params$glitch_r_max)
           }
           pareto_glitch <- glitch_list[[glitch_params$num_glitches]]

           if(pareto_2) {
             glitch_seed_2 <- glitch_connected(pareto_orbits2, glitch_r_min = glitch_params$glitch_r_min, glitch_r_max = glitch_params$glitch_r_max)
             glitch_list_2 <- list(glitch_seed_2)
             for(i in 2:glitch_params$num_glitches) {
               glitch_list_2[[i]] <- glitch_connected(glitch_list_2[[i - 1]], glitch_r_min = glitch_params$glitch_r_min, glitch_r_max = glitch_params$glitch_r_max)
             }
             pareto_glitch2 <- glitch_list_2[[glitch_params$num_glitches]]
           } else {
             pareto_glitch2 <- pareto_orbits2
           }
         },
         "shattered" = {
           ### shattered glitch
           input <- pareto_orbits %>% mutate(id2 = NA)
           glitch_seed <- glitch_shattered(input, num = 1, glitch_r_min = glitch_params$glitch_r_min, glitch_r_max = glitch_params$glitch_r_max)
           glitch_list <- list(glitch_seed)
           for(i in 2:glitch_params$num_glitches) {
             glitch_list[[i]] <- glitch_shattered(glitch_list[[i - 1]], num = i, glitch_r_min = glitch_params$glitch_r_min, glitch_r_max = glitch_params$glitch_r_max)
           }
           pareto_glitch <- glitch_list[[glitch_params$num_glitches]] %>%
             mutate(id2 = ifelse(is.na(id2), id, id2),
                    id_lead = lead(id2),
                    switch = ifelse(id_lead != id2, paste0("switch_", row_number()), NA))

           if(is.na(pareto_glitch$switch[1])) {
             pareto_glitch$switch[1] <- pareto_glitch$id2[1]
           }

           pareto_glitch$switch2 <- zoo::na.locf(pareto_glitch$switch)

           switch3 <- c(pareto_glitch$switch2[1], pareto_glitch$switch2)
           switch3 <- switch3[1:nrow(pareto_glitch)]
           pareto_glitch$id <- switch3

           if(pareto_2) {
             input2 <- pareto_orbits2 %>% mutate(id2 = NA)
             glitch_seed2 <- glitch_shattered(input2, num = 1, glitch_r_min = glitch_params$glitch_r_min, glitch_r_max = glitch_params$glitch_r_max)
             glitch_list2 <- list(glitch_seed2)
             for(i in 2:glitch_params$num_glitches) {
               glitch_list2[[i]] <- glitch_shattered(glitch_list2[[i - 1]], num = i, glitch_r_min = glitch_params$glitch_r_min, glitch_r_max = glitch_params$glitch_r_max)
             }
             pareto_glitch2 <- glitch_list2[[glitch_params$num_glitches]] %>%
               mutate(id2 = ifelse(is.na(id2), id, id2),
                      id_lead = lead(id2),
                      switch = ifelse(id_lead != id2, paste0("switch_", row_number()), NA))

             if(is.na(pareto_glitch2$switch[1])) {
               pareto_glitch2$switch[1] <- pareto_glitch2$id2[1]
             }

             pareto_glitch2$switch2 <- zoo::na.locf(pareto_glitch2$switch)

             switch3_2 <- c(pareto_glitch2$switch2[1], pareto_glitch2$switch2)
             switch3_2 <- switch3_2[1:nrow(pareto_glitch2)]
             pareto_glitch2$id <- switch3_2
           } else {
             pareto_glitch2 <- pareto_orbits2
           }
         },
         "spike" = {
           ##spike glitch
           pareto_glitch <- glitch_spike(input = pareto_orbits, min_spikes = glitch_params$min_spikes, max_spikes = glitch_params$max_spikes,
                                         min_spike_r = glitch_params$glitch_r_min, max_spike_r = glitch_params$glitch_r_max,
                                         min_spike_jitter = glitch_params$min_spike_jitter, max_spike_jitter = glitch_params$max_spike_jitter)
           if(pareto_2) {
             pareto_glitch2 <- glitch_spike(input = pareto_orbits2, min_spikes = glitch_params$min_spikes2, max_spikes = glitch_params$max_spikes2,
                                            min_spike_r = glitch_params$glitch_r_min, max_spike_r = glitch_params$glitch_r_max,
                                            min_spike_jitter = glitch_params$min_spike_jitter, max_spike_jitter = glitch_params$max_spike_jitter)
           } else {
             pareto_glitch2 <- pareto_orbits2
           }
         }
  )
  #put it all in a list and plot
  final_dat <-
    list(seed = seed,
         seed_outlines = seed_outlines,
         orbits = orbits,
         pareto1 = pareto_glitch,
         pareto2 = pareto_glitch2
    )

  plot <-
    ggplot() +
    geom_polygon(data = final_dat[["seed"]], aes(x = x, y = y, group = id), fill = "white") +
    geom_path(data = final_dat[["seed_outlines"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["seed_outlines"]]$linetype, color = "white") +
    geom_path(data = final_dat[["pareto1"]], aes(x = x, y = y, group = id, size = linewidth), linetype = final_dat[["pareto1"]]$linetype, color = "white") +
    geom_path(data = final_dat[["pareto2"]], aes(x = x, y = y, group = id, size = linewidth), linetype = final_dat[["pareto2"]]$linetype, color = "white") +
    geom_path(data = final_dat[["orbits"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["orbits"]]$linetype, color = "white") +
    scale_size_identity() +
    scale_color_identity() +
    theme_void() +
    coord_equal() +
    theme(panel.background = element_rect(fill = "#141414"))


  suppressMessages(print(plot))

}

