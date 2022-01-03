library(broom)
source("scripts/ovb_temp_snail_funs.R")

envt <- tibble(
  sims = 1:100
) %>%
  mutate(sites = map(sims, ~make_environment()),
         site_lm = map(sites, ~lm(site_recruitment ~ site_temp, data = .)),
         rec_temp_coef = map_dbl(site_lm, ~tidy(.x) %>% 
                                    filter(term == "site_temp") %>% 
                                    pull(estimate))) 


ggplot(envt %>%
         unnest(sites),
       aes(x = site_temp, y = site_recruitment, group = sims)) +
  geom_point(alpha = 0.4, color = 'grey') +
  stat_smooth(method = "lm", size = 0.5, alpha = 0.5,
              fill = NA, color = "black")

ggplot(envt, aes(x = rec_temp_coef)) + geom_histogram(bins = 30)


envt <- tibble(
  sims = 1:100
) %>%
  mutate(sites = map(sims, ~make_environment(ocean_recruitment=-2)),
         site_lm = map(sites, ~lm(site_recruitment ~ site_temp, data = .)),
         rec_temp_coef = map_dbl(site_lm, ~tidy(.x) %>% 
                                   filter(term == "site_temp") %>% 
                                   pull(estimate))) 


plots_df <- envt %>%
  mutate(plots = map(sites, make_plots))




analysis_df <- plots_df %>%
  mutate(analysis = map(plots, analyze_plots)) %>%
  unnest(analysis)


analysis_df %>%
  unnest(temp_effect) %>%
  ggplot(aes(y = model_type, x = estimate)) +
  ggridges::stat_density_ridges()

analysis_df %>%
  unnest(temp_effect) %>%
  filter(model_type != "Naieve") %>%
  ggplot(aes(x = model_type, y = estimate)) +
  geom_jitter()

ggplot(plots_df %>%
         unnest(site_year),
       aes(x = plot_temp, y = snails, color = site)) +
  geom_point(alpha = 0.5)



analysis_df %>%
  unnest(temp_effect) %>%
  filter(model_type != "Naieve") %>%
  ggplot(aes(x = sims, y = estimate, 
             ymin = estimate - 2*std.error,
             ymax = estimate + 2*std.error)) +
  geom_pointrange() +
  facet_wrap(vars(model_type))


analysis_df %>%
  unnest(temp_effect) %>%
  group_by(model_type) %>%
  summarize(mean_estimate = mean(estimate),
            sd_estimate = sd(estimate))
