# figures ----
# Function for making boxplots of bed tow catches
plot_catch_rate <- function(scal_catch, tows, YEAR){

  output_dir <- file.path("figs", YEAR)

  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }

  scal_catch %>%
    filter(Size == "small" | Size == "large") %>%
    select(tow_id, Size, N = count) %>%
    left_join(tows, by = "tow_id") %>%
    mutate(Bed=factor(Bed, levels = Bed_levels)) %>%
    ggplot(aes(Bed, N, fill = Size)) +
    geom_boxplot() +
    ylab("Scallops / Tow") + xlab("Bed") +
    stat_summary(fun.data = function(x) c(y = -25, label = length(x)),
                 geom ="text") +
    scale_fill_manual(values = c("white", 'lightgray'), name = "Size class") +
    theme(legend.justification=c(1,0), legend.position=c(.95,.7)) +
    scale_y_continuous(label=scales::comma) +
    coord_cartesian(ylim = c(-25,600)) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/catch_rates.png")), plot = x,
         width = 6.5, height = 4)

  x
}
# Function for plotting abundance (abundance = TRUE) or biomass estimates
plot_scal_est <- function(scal_est, abundance = TRUE, YEAR){

  scal_est %>%
    ungroup %>%
    filter(!(Size %in% c("clapper", "all")))	%>%
    mutate(Bed = factor(Bed, levels = Bed_levels)) %>%
    ggplot(aes(Bed, est / 1000000, color = Size)) +
    geom_point(size = 1.5, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin = l95 / 1000000, ymax = u95 / 1000000),
                  size = 0.5,
                  width = 0.4,
                  position = position_dodge(.5)) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("small" = "grey70", "large" = "black"),
                       name = "Size class") +
    labs(y = ifelse(abundance, "Abundance (millions)\n",
                    "Round weight Biomass (million lb)\n")) -> x

  if(abundance){
    ggsave(here::here(paste0("figs/", YEAR, "/abund_est.png")), x,
           width = 6.5, height = 4, units = "in")
  } else {
    ggsave(here::here(paste0("figs/", YEAR, "/biom_est.png")), x,
           width = 6.5, height = 4, units = "in")
  }

  x
}

# Function for plotting scallop size distributions (no clappers) using sampling weights
plot_size_dist <- function(scal_awl, scal_catch, tows, YEAR){

  scal_catch %>%
    ungroup %>%
    filter(Size != "clapper") %>%
    select(tow_id, N = count, size = Size) -> tow_count

  scal_awl %>%
    dplyr::select(tow_id, size, sh) %>%
    filter(!is.na(sh), sh > 0, !is.na(size)) %>%
    add_count(tow_id, size) %>% # add subsample sizes by size class within each tow
    left_join(tow_count, by = c("tow_id", "size")) %>%
    left_join(tows, by = "tow_id") %>%
    filter(n <= N, N * n != 0) %>%
    mutate(wt = (N / n) / area_swept,
           Bed = factor(Bed, levels = Bed_levels)) %>%
    add_count(Bed) %>%
    mutate(label = paste0(Bed, ": N = ", n)) %>%
    ggplot(aes(sh, weight=wt)) +
    geom_histogram(color = "black", fill = "darkgray", bins = 75) +
    facet_wrap(~Bed, ncol=2, dir = 'v', scales = 'free_y') +
    xlab("\nShell height (mm)") +
    scale_y_continuous("Weighted shell height counts\n", label = scales::comma) +
    theme(strip.background = element_blank()) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/size_dist.png")), plot = x, width = 6.5, height = 8)

  x
}

plot_size_dist2 <- function(scal_awl, scal_catch, tows, YEAR){

  scal_catch %>%
    filter(Size != "clapper") %>%
    select(tow_id, N = count, size = Size) -> tow_count

  scal_awl %>%
    dplyr::select(tow_id, size, sh) %>%
    filter(!is.na(sh), sh > 0, !is.na(size)) %>%
    add_count(tow_id, size) %>% # add subsample sizes by size class within each tow
    left_join(tow_count, by = c("tow_id", "size")) %>%
    left_join(tows, by = "tow_id") %>%
    filter(n <= N, N * n != 0) %>%
    mutate(wt = (N / n) / area_swept,
           Bed = factor(Bed, levels = Bed_levels)) %>%
    add_count(Bed) %>%
    mutate(label = paste0(Bed, ": N = ", n)) %>%
    ggplot(aes(sh, weight=wt)) +
    geom_histogram(color = "black", fill = "darkgray", bins = 75) +
    facet_wrap(~Bed, ncol=2, dir = 'v', scales = 'free_y') +
    xlab("\nShell height (mm)") +
    scale_y_continuous("Weighted shell height counts\n", label = scales::comma) +
    theme(strip.background = element_blank()) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/size_dist.png")), plot = x, width = 6.5, height = 8)

  x
}
# Functions for plotting mwt against rwt and sh, by District
plot_mw_rw <- function(scal_awl, tows, Districts, YEAR){

  scal_awl %>%
    filter(sh >= 100, !is.na(mwt_lb), mwt_lb * rwt_lb > 0, mwt_lb < rwt_lb) %>%
    left_join(tows, by = "tow_id") %>%
    left_join(Districts, by = "Bed") %>%
    ggplot(aes(rwt_lb, mwt_lb, color = District)) +
    geom_point(alpha = 0.12) +
    geom_smooth(se = F) +
    labs(y = "Meat weight (lb)", x = "Round weight (lb)") +
    scale_color_grey() +
    theme(legend.justification=c(1,0), legend.position=c(.2,.65)) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/mwt_rwt.png")),
         plot = x, width = 6.5, height = 4, units = "in")

  x

}
plot_mw_sh <- function(scal_awl, tows, Districts, YEAR){
  scal_awl %>%
    filter(sh >= 100, !is.na(mwt_lb), mwt_lb * rwt_lb > 0, mwt_lb < rwt_lb) %>%
    left_join(tows, by = "tow_id") %>%
    left_join(Districts, by = "Bed") %>%
    ggplot(aes(sh, mwt_lb,  color = District)) +
    geom_point(alpha = 0.1) +
    geom_smooth(se = F) +
    labs(y = "Meat weight (lb)", x = "Shell height (mm)") +
    scale_color_grey() +
    theme(legend.justification=c(1,0), legend.position=c(.2,.65)) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/mwt_sh.png")),
         plot = x, width = 6.5, height = 4, units = "in")

  x

}
