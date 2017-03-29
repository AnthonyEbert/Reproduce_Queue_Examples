

library(queuecomputer)
library(dplyr)
library(ggplot2)
library(reshape2)

set.seed(1)

compatability <- FALSE

if(compatability == FALSE){
  git_hash <- system('git rev-parse HEAD', intern = TRUE)
  print(git_hash)
  base_name <- system('basename `git rev-parse --show-toplevel`', intern = TRUE)
  git_tag <- paste(base_name, "/", git_hash, sep = "")
} else {
  git_tag <- ""
}

Ntot = 1e5
N = 101

theta <- c(0.3, 0.9, 1.0)
theta1 = theta[1]
theta2 = theta[2]
theta3 = theta[3]

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

theta_1_sseq <- seq(0.2, 0.4, by = 0.04)
theta_2_sseq <- seq(0.8, 1.04, by = 0.04)
theta_3_sseq <- seq(0.8, 1.3, by = 0.08)

theta_1_seq <- rep(theta_1_sseq, each = each_num)
theta_2_seq <- rep(theta_2_sseq, each = each_num)
theta_3_seq <- rep(theta_3_sseq, each = each_num)

## Changing theta1 ------------------------

output <- lapply(1:length(theta_1_seq), function(i, ...) {
  
  theta1 <- theta_1_seq[i]
  theta2 <- theta[2]
  theta3 <- theta[3]
  
  arrivals <- cumsum(rexp(Ntot, 1/theta3))
  service <- runif(Ntot, theta1, theta2)
  
  departures <- queue(arrivals, service, 1) %>% tail(N)
  
  interdepartures <- diff(departures)
  
  return(data_frame(
    interdepartures = interdepartures, 
    parameter_value = rep(theta1, N - 1), 
    parameter_name = "theta1")
  )
  
})

theta1_50_datasets <- bind_rows(output) %>% 
  mutate(
    realisation = rep(rep(seq(1, 50), each = N - 1),length(theta_1_sseq))
  )

Summary_stats_theta1 <- theta1_50_datasets %>%
  group_by(parameter_name, parameter_value, realisation) %>%
  summarise(z_bar = mean(interdepartures), z_min = min(interdepartures), z_med = median(interdepartures))

## Changing theta2 ------------------

output <- lapply(1:length(theta_2_seq), function(i, ...) {
  
  theta1 <- theta[1]
  theta2 <- theta_2_seq[i]
  theta3 <- theta[3]
  
  arrivals <- cumsum(rexp(Ntot, 1/theta3))
  service <- runif(Ntot, theta1, theta2)
  
  departures <- queue(arrivals, service, 1) %>% tail(N)
  
  interdepartures <- diff(departures)
  
  return(data_frame(
    interdepartures = interdepartures, 
    parameter_value = rep(theta2, N - 1), 
    parameter_name = "theta2")
  )
  
})

theta2_50_datasets <- bind_rows(output) %>% 
  mutate(
    realisation = rep(rep(seq(1, 50), each = N - 1),length(theta_2_sseq))
  )

Summary_stats_theta2 <- theta2_50_datasets %>%
  group_by(parameter_name, parameter_value, realisation) %>%
  summarise(z_bar = mean(interdepartures), z_min = min(interdepartures), z_med = median(interdepartures))

## Changing theta3 ------------------

output <- lapply(1:length(theta_3_seq), function(i, ...) {
  
  theta1 <- theta[1]
  theta2 <- theta[2]
  theta3 <- theta_3_seq[i]
  
  arrivals <- cumsum(rexp(Ntot, 1/theta3))
  service <- runif(Ntot, theta1, theta2)
  
  departures <- queue(arrivals, service, 1) %>% tail(N)
  
  interdepartures <- diff(departures)
  
  return(data_frame(
    interdepartures = interdepartures, 
    parameter_value = rep(theta3, N - 1), 
    parameter_name = "theta3")
  )
  
})

theta3_50_datasets <- bind_rows(output) %>% 
  mutate(
    realisation = rep(rep(seq(1, 50), each = N - 1),length(theta_3_sseq))
  )

Summary_stats_theta3 <- theta3_50_datasets %>%
  group_by(parameter_name, parameter_value, realisation) %>%
  summarise(z_bar = mean(interdepartures), z_min = min(interdepartures), z_med = median(interdepartures))

Summary_stats_overall <- bind_rows(
  Summary_stats_theta1, 
  Summary_stats_theta2, 
  Summary_stats_theta3
  )

melted_summary_overall <- melt(Summary_stats_overall, id.vars = c("parameter_name", "parameter_value", "realisation"))

names(melted_summary_overall)[5] <- "stat_value"

pdf(file = paste(git_tag, "Figure1_Heggland_Frigessi.pdf", sep = ""))

ggplot(melted_summary_overall) + aes(x = parameter_value, y = stat_value) + geom_point() + facet_grid(variable ~ parameter_name, scales = "free") + stat_smooth()

dev.off()

if(compatability == FALSE){print(git_hash)}




