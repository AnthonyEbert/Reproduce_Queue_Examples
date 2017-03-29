

library(queuecomputer)
library(dplyr)
library(session)
library(ggplot2)

set.seed(1)

git_hash <- system('git rev-parse HEAD', intern = TRUE)
print(git_hash)

N = 101
theta1 = 0.3
theta2 = 0.9
theta3 = 1.0

service <- runif(N, theta1, theta2)

interarrivals <- rexp(N, 1/theta3) 

# Not sure whether this is rate or mean, not that it matters for theta = 1.  
# From correlation of theta3 and z_mean it's clear that theta3 is the mean. 


output <- lapply(1:50, function(i, ...) {
  
  arrivals <- cumsum(rexp(N, theta3))
  service <- runif(N, theta1, theta2)
  
  departures <- queue(arrivals, service, 1)
  
  interdepartures <- diff(departures)
  
  return(data.frame(
    interdepartures = interdepartures, 
    realisation = rep(i, N - 1))
    )

})
  

Original_50_datasets <- bind_rows(output)

Summary_stats <- Original_50_datasets %>% 
  group_by(realisation) %>%
  summarise(z_bar = mean(interdepartures), z_min = min(interdepartures), z_med = median(interdepartures))

# One-by-one variation ---------------

each_num = 50

theta_1_seq <- rep(seq(0.2, 0.4, by = 0.04), each = each_num)
theta_2_seq <- rep(seq(0.8, 1.04, by = 0.04), each = each_num)
theta_3_seq <- rep(seq(0.8, 1.3, by = 0.08), each = each_num)

## Changing theta1 ------------------------

output <- lapply(1:length(theta_1_seq), function(i, ...) {
  
  theta1_p <- theta_1_seq[i]
  
  arrivals <- cumsum(rexp(N, theta3))
  service <- runif(N, theta1_p, theta2)
  
  departures <- queue(arrivals, service, 1)
  
  interdepartures <- diff(departures)
  
  return(data.frame(
    interdepartures = interdepartures, 
    theta1_p = rep(theta1_p, N - 1))
  )
  
})

theta1_50_datasets <- bind_rows(output) %>% 
  mutate(
    realisation = rep(rep(seq(1, 50), each = N - 1),6)
  )

Summary_stats_theta1 <- theta1_50_datasets %>%
  group_by(theta1_p, realisation) %>%
  summarise(z_bar = mean(interdepartures), z_min = min(interdepartures), z_med = median(interdepartures))


plot(Summary_stats_theta1, main = git_hash)

hash_path <- file.path("output", git_hash)

data_path <- file.path("output", git_hash, "Heggland_Frigessi_2004")

dir.create(hash_path, showWarnings = FALSE)
dir.create(data_path, showWarnings = FALSE)

save.session(file = paste("output/", git_hash, 
"_sesh.Rda", sep = ""))

ggplot(Summary_stats_theta1) + aes(x = theta1_p, y = z_bar, group = realisation) + geom_line()

ggsave(paste("output/", git_hash, "_theta1_z_bar.pdf", sep = ""))

# ggsave(paste(data_path, "/theta1_z_bar.pdf", sep = ""))
# 
# ggplot(Summary_stats_theta1) + aes(x = theta1_p, y = z_min, group = realisation) + geom_line()
# 
# ggsave(paste(data_path, "/theta1_z_min.pdf", sep = ""))
# 
# ggplot(Summary_stats_theta1) + aes(x = theta1_p, y = z_med, group = realisation) + geom_line()
# 
# ggsave(paste(data_path, "/theta1_z_med.pdf", sep = ""))



