# this plot will show the difference in the median density
# downstream - upstream with year and location as factors
# the horizontal red line at zero shows no difference between locations
# above the red line means more downstream, below means more upstream
# the vertical blue line shows when winter sediment redistribution began
# note the y-axis is on a pseudo-log scale to be able to handle negative and 0

#get median macro density for each year, season, location 
mac.sum <- my.df |> 
  group_by(year, season, location) |> 
  summarize(median.loc = median(density)) |> 
  na.omit()

#pivot wider then subtract locations
mac.diff <- mac.sum |> 
  pivot_wider(names_from = location, values_from = median.loc) |> 
  mutate(median.diff = (Downstream - Upstream))

mac.diff |> 
  ggplot(aes(
    x = year,
    y = median.diff)) +
  geom_hline(yintercept = 0, color = "red", size = 2, alpha = 0.25) +
  geom_point(size = 3) +
  ylab("Median Difference in Density (#/m\u00b2)\n (Downstream - Upstream)") +
  xlab("Year") +
  theme_classic(base_size = 14) +
  facet_grid(cols = vars(season)) +
  scale_y_continuous(breaks = c(-10^(4:0), 0, 10^(0:4)),
                     transform = "pseudo_log",
                     limits = c(-4000, 4000)) +
  guides(y = guide_axis_logticks(negative_small = 1)) +
  geom_vline(xintercept=2.5, color = "blue", size = 2, alpha = 0.25)
