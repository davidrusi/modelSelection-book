# MH lattice animation (random walk proposal)
# Run in R. Install packages if needed:
# install.packages(c("tidyverse","gganimate","ggforce","gifski"))

library(tidyverse)
library(gganimate)
library(ggforce)
library(gifski)

set.seed(2025)


animation_RWMH_lattice <- function(target, filename_gif, n_steps=200) {
  # Grid definition: 1..10 in both coordinates
  xs <- 1:10
  ys <- 1:10
  grid <- expand.grid(x = xs, y = ys) %>%
    arrange(x, y) %>%
    mutate(id = row_number())
  # Neighborhood: horizontal or vertical neighbors (|dx|=1 OR |dy|=1, not diagonal)
  get_neighbors <- function(x, y) {
    candidates <- tibble(
      x = c(x-1, x+1, x,   x),
      y = c(y,   y,   y-1, y+1)
    )
    # keep only those inside bounds
    candidates %>% filter(x %in% xs, y %in% ys)
  }
  # helper to map x,y -> id
  xy_to_id <- function(xv, yv, grid_df = grid) {
    grid_df %>% filter(x == xv, y == yv) %>% pull(id)
  }
  # Build neighbor counts for each node
  neighbors_list <- grid %>%
    rowwise() %>%
    mutate(
      neigh_df = list(get_neighbors(x, y)),
      n_neigh = nrow(get_neighbors(x, y))
    ) %>%
    ungroup() %>%
    select(x, y, n_neigh, neigh_df)
  #
  grid <- mutate(grid, pi= target(x, y, decay_rate=0.6))
  # Attach neighbor counts into main grid dataframe
  grid <- grid %>%
    left_join(neighbors_list %>% select(x, y, n_neigh), by = c("x", "y"))
  # Build edges (undirected) for plotting
  edges <- tibble()
  for(i in seq_len(nrow(grid))) {
    xi <- grid$x[i]; yi <- grid$y[i]; idi <- grid$id[i]
    neighs <- get_neighbors(xi, yi)
    for(r in seq_len(nrow(neighs))) {
      nid <- xy_to_id(neighs$x[r], neighs$y[r])
      if(nid > idi) {
        edges <- bind_rows(edges, tibble(x=xi, y=yi, xend=neighs$x[r], yend=neighs$y[r]))
      }
    }
  }   
  # --- Simulate Metropolis-Hastings with uniform neighbor proposal ---
  message("Running MH algorithm\n")
  # initialize randomly
  current_id <- sample(grid$id, 1)
  chain_ids <- integer(n_steps)
  chain_ids[1] <- current_id
  #   
  for(t in 2:n_steps) {
    cur_row <- grid %>% filter(id == current_id)
    neighs <- get_neighbors(cur_row$x, cur_row$y)
    prop_xy <- neighs %>% slice_sample(n = 1)
    prop_id <- xy_to_id(prop_xy$x, prop_xy$y)
    prop_row <- grid %>% filter(id == prop_id)
    # acceptance probability for uniform neighbor proposal:
    alpha <- min(1, (prop_row$pi / cur_row$pi) * (cur_row$n_neigh / prop_row$n_neigh))
    if(runif(1) < alpha) {
      current_id <- prop_id
    }
    chain_ids[t] <- current_id
  }
  #
  # Prepare data for animation: cumulative visits up to each step
  anim_df_nodes <- grid %>%
    select(id, x, y, pi, n_neigh)
  #   
  visits <- tibble(step = 1:n_steps, id = chain_ids) %>%
    group_by(step) %>%
    mutate() %>% ungroup()
  #   
  node_steps <- expand_grid(step = 1:n_steps, id = grid$id) %>%
    arrange(step, id) %>%
    left_join(visits %>% count(step, id, name = "hits_step"), by = c("step","id")) %>%
    mutate(hits_step = replace_na(hits_step, 0)) %>%
    group_by(id) %>%
    mutate(cum_hits = cumsum(hits_step)) %>%
    ungroup() %>%
    left_join(anim_df_nodes, by = "id")
  #   
  # Current position per step (for large marker)
  current_pos <- tibble(step = 1:n_steps, current_id = chain_ids) %>%
    left_join(grid, by = c("current_id" = "id")) %>%
    rename(cx = x, cy = y)
  #   
  # Merge for plotting
  plot_frames <- node_steps %>%
    left_join(current_pos, by = "step")
  #   
  # Scale cumulative visits for plotting (size) and compute a capped alpha (0..1)
  plot_frames <- plot_frames %>%
    group_by(step) %>%
    mutate(cum_hits_scaled = (cum_hits / max(1, max(cum_hits))) * 6) %>%
    ungroup() %>%
    group_by(id) %>%
    mutate(cum_alpha = ifelse(max(cum_hits) > 0, (cum_hits / max(cum_hits)) * 0.6, 0)) %>%
    ungroup()
  #   
  # --- Plot + animate ---
  # We'll use transition_manual so each frame corresponds exactly to a step
  p <- ggplot() +
    # edges
    geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend),
                 color = "grey80", linewidth = 0.4) +
    # base nodes colored by target s (blue -> red)
    geom_point(data = anim_df_nodes, aes(x = x, y = y, fill = pi),
               shape = 21, colour = "black", size = 4) +
    scale_fill_gradient(low = "blue", high = "red", name = "Posterior probability") +
    # overlay: cumulative visits up to this step (animated).
    # Draw stroked circles with no fill so underlying node color is preserved
    geom_point(data = plot_frames, aes(x = x, y = y, size = cum_hits_scaled, alpha = cum_alpha),
               shape = 21, color = "black", fill = NA, stroke = 0.8, show.legend = FALSE) +
    # prominent solid black circular outline around current state (solid, thick stroke)
    geom_point(data = plot_frames %>% distinct(step, cx, cy), 
               aes(x = cx, y = cy),
               shape = 21, color = "black", fill = NA, size = 9, stroke = 1.8) +
    # small filled dot inside current node to mark it (use white so it doesn't add dark shadow)
    geom_point(data = plot_frames %>% distinct(step, cx, cy), 
               aes(x = cx, y = cy), colour = "white", size = 2.5, stroke = 0.8) +
    coord_fixed() +
    theme_minimal(base_size = 14) +
    labs(title = "Metropolis-Hastings on 10×10 lattice (random walk proposal)",
         subtitle = "Iteration: {current_frame}",
         x = "", y = "") +
    theme(legend.position = "right") +
    transition_manual(frames = step) +
    # set plot-level variables for use in dynamic subtitle (gganimate allows frame variables)
    NULL
  # To provide the current coordinates to the subtitle, we attach helper vectors into the environment
  # gganimate will look up variables named like <name>_frame inside glue expressions:
  cx_frame <- plot_frames %>% distinct(step, cx) %>% arrange(step) %>% pull(cx)
  cy_frame <- plot_frames %>% distinct(step, cy) %>% arrange(step) %>% pull(cy)
  # Animate: nframes = n_steps, fps = 2 -> each frame = 0.5 seconds
  if (!missing(filename_gif)) {
    message("Preparing animation\n")
    anim <- animate(p, nframes = n_steps, fps = 2, width = 700, height = 700, renderer = gifski_renderer())
    anim_save(filename_gif, animation = anim)
  }
  # Summary
  vis_summary <- tibble(id = grid$id) %>%
    left_join(tibble(id = chain_ids) %>% count(id, name = "visits"), by = "id") %>%
    left_join(grid %>% select(id, x, y, pi), by = "id") %>%
    replace_na(list(visits = 0)) %>%
    arrange(desc(pi)) |>
    transform(pi= round(pi, 4), proportion_visits= visits/n_steps) |>
    select(x, y, pi, proportion_visits) 
  # Print summary
  return(vis_summary)
}



#Unimodal target function
unimodal_target <- function(x, y, decay_rate= 0.6) {
    dist_to_peak = sqrt((x - 10)^2 + (y - 10)^2)
    s_raw = exp(-decay_rate * dist_to_peak)
    s_peak_raw <- max(s_raw)
    scale_factor <- 0.5 / s_peak_raw
    s = s_raw * scale_factor
    pi = s/sum(s)
    return(pi)
}

#Bimodal target function
bimodal_target <- function(x, y, decay_rate= 0.6) {
    dist_to_peak1 = sqrt((x - 10)^2 + (y - 10)^2)
    dist_to_peak2 = sqrt((x - 1)^2 + (y - 1)^2)
    s_raw = 0.5 * exp(-decay_rate * dist_to_peak1) + 0.5 * exp(-decay_rate * dist_to_peak2)
    s_peak_raw <- max(s_raw)
    scale_factor <- 0.5 / s_peak_raw
    s = s_raw * scale_factor
    pi = s/sum(s)
    return(pi)
}

#Create several animations
visits_unimodal_200 <- animation_RWMH_lattice(unimodal_target, filename_gif= "mh_lattice_unimodal.gif", n_steps=200)    #unimodal target, save gif
visits_unimodal_2000 <- animation_RWMH_lattice(unimodal_target, n_steps=2000)   #unimodal target, don't save gif

visits_bimodal_200 <- animation_RWMH_lattice(bimodal_target, filename_gif= "mh_lattice_bimodal.gif", n_steps=200) #bimodal target, save gif
visits_bimodal_2000 <- animation_RWMH_lattice(bimodal_target, n_steps=2000)    # bimodal target, don't save gif

head(visits_unimodal_2000, 10)
head(visits_bimodal_2000, 10)

summarize(visits_unimodal_2000, MAE= mean(abs(pi - proportion_visits)))
summarize(visits_bimodal_2000, MAE= mean(abs(pi - proportion_visits)))


print(head(vis_summary, 10))
