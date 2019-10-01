# Figures for SAFE report
# ben.williams@alaska.gov
# last updated 2018-2

# need to run the data_cleaning.R script prior to creating these figures

# load ----
source("code/helper.R")

# globals ----

YEAR = 2018

# data ----
sh <- read_csv(paste0("output/", YEAR, "/sh.csv"))
yak <- read_csv(paste0("output/",YEAR,"/yak.csv")) %>% 
  filter(bed!=6)
d16 <- read_csv(paste0("output/", YEAR, "/d16.csv"))
q <- read_csv(paste0("output/", YEAR, "/q.csv"))
o <- read_csv(paste0("output/", YEAR, "/o.csv"))
m <- read_csv(paste0("output/", YEAR, "/m.csv"))
ksw <- read_csv(paste0("output/", YEAR, "/ksw.csv"))
kne <- read_csv(paste0("output/", YEAR, "/kne.csv"))
ksh <- read_csv(paste0("output/", YEAR, "/ksh.csv"))



# figure function ----
f_safe <- function(x){
  y = deparse(substitute(x))
x %>% 
  left_join(sh) %>% 
  ggplot(aes(Shell_Height, fill = Rtnd_Disc, color = Rtnd_Disc)) + 
  geom_density(alpha = 0.2) + 
  facet_grid(FY~.) +
  scale_color_manual(values = c('D' = "#e41a1c", "R" = "#377eb8"), labels = c('discard', 'retained'), name = "") + 
  scale_fill_manual(values = c('D' = "#e41a1c", "R" = "#377eb8"), labels = c('discard', 'retained'), name = "") +
  theme(legend.key = element_blank(),
        legend.position = c(0.2, 0.2),
        legend.background = element_rect(fill = "#ffffffaa", color = NA),
        legend.key.height = unit(0.6, 'line'),
        strip.background = element_blank(),
        panel.border = element_blank()) +
  xlab("Shell height (mm)") + ylab("Density") -> x1

x %>% 
  left_join(sh) %>% 
  ggplot(aes(Shell_Height, fill = Rtnd_Disc, color = Rtnd_Disc)) + 
  geom_histogram(alpha = 0.2, bins = 60) + 
  facet_grid(FY~.) +
  scale_color_manual(values = c('D' = "#e41a1c", "R" = "#377eb8"), labels = c('discard', 'retained'), name = "") + 
  scale_fill_manual(values = c('D' = "#e41a1c", "R" = "#377eb8"), labels = c('discard', 'retained'), name = "") +
  theme(legend.key = element_blank(),
        legend.position = c(0.2, 0.2),
        legend.background = element_rect(fill = "#ffffffaa", color = NA),
        legend.key.height = unit(0.6, 'line'),
        strip.background = element_blank(),
        panel.border = element_blank()) +
  xlab("Shell height (mm)") + ylab("Count") +
  guides(fill = FALSE, color = FALSE) -> x2

grid.arrange(x1, x2, ncol = 2) -> g
ggsave(file = paste('safe_figs/safe_fig_', y, ".png"), g, dpi = 200, height = 8, width = 6.5, units = "in")
}

# figs ----

f_safe(yak)
f_safe(d16)
f_safe(q)
f_safe(o)
f_safe(m)
f_safe(ksw)
f_safe(kne)
f_safe(ksh)

# rolling 
