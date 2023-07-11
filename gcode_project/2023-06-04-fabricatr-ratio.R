
library(tidyverse)
library(fabricatr)
library(randomizr)

# 52 states

#
state_data <-
  fabricate(
    state = add_level(
      N = 52,
      GDP = runif(N, 20000, 50000), # gpd per of this state #
      W = runif(N, 0, 100) # weather 0 is bad, 100 is good
    )
  )
state_data
#

# each state has 5
city_data = fabricate(data = state_data, 
          city = add_level(
            N = 5,
            # city specific characteristics
            u_i = rnorm(N),
            #
            employment_rate = plogis(scale(GDP) + u_i),
            crime_rate = 1- plogis(scale(GDP) + scale(W) + u_i)
            )
          )
#

#
head(city_data)
city_data %>% group_by(state) %>% summarise(mean(employment_rate), var(employment_rate))
#

#
library("multigroup")
city_data %>% head()
# cool #
TBWvariance(Data = city_data[,c("crime_rate", "employment_rate")], Group = city_data[,"state"])
#

# each state has 20 schools
students_data = fabricate(data = city_data, 
                      students = add_level(
                        N = 10,
                        # student specific characteristics
                        U_i = rnorm(N), 
                        social_origin = scale(GDP) + employment_rate + crime_rate, 

                        y_i = U_i + social_origin
                        )
)
#

#
students_data %>% head()
#

TBWvariance(Data = data.frame(y = students_data[,c("y_i")]), Group = students_data[,"city"])
#

##########################################################################################
##########################################################################################


students_data %>% head()
#

library("DeclareDesign")
library("DesignLibrary")
#

# Divide observations into clusters
clusters = rep(1:5, 10)

# Default: unit variance within each cluster
cbind(clusters, draw_normal_icc(clusters = clusters, ICC = 0.5))
#

# Can specify total SD instead:
total_sd_draw = draw_normal_icc(clusters = clusters, ICC = 0.5, total_sd = 3)
sd(total_sd_draw)

# Verify that ICC generated is accurate
corr_draw = draw_normal_icc(clusters = clusters, ICC = 0.4)
summary(lm(corr_draw ~ as.factor(clusters)))$r.squared
#

#

#
N_blocks <- 1
N_clusters_in_block <- 10
N_i_in_cluster <- 5
n_clusters_in_block <- 3
n_i_in_cluster <- 10
icc <- 0.402

?draw_normal_icc(mean = 0, N = N, clusters = cluster, ICC = icc)
fixed_pop

fixed_pop <- declare_population(block = add_level(N = N_blocks), 
                                cluster = add_level(N = N_clusters_in_block), subject = add_level(N = N_i_in_cluster, 
                                                                                                  latent = draw_normal_icc(mean = 0, N = N, clusters = cluster, ICC = icc), 
                                                                                                  Y = draw_ordered(x = latent, breaks = qnorm(seq(0, 1, length.out = 8)))))()
#
fixed_pop
#
population <- declare_population(data = fixed_pop)
#
?declare_population
#
estimand <- declare_inquiry(mean(Y), label = "Ybar")
stage_1_sampling <- declare_sampling(S1 = strata_and_cluster_rs(strata = block, clusters = cluster, n = n_clusters_in_block), filter = S1 == 1)
stage_2_sampling <- declare_sampling(S2 = strata_rs(strata = cluster, n = n_i_in_cluster), filter = S2 == 1)
clustered_ses <- declare_estimator(Y ~ 1, model = lm_robust, clusters = cluster, inquiry = estimand, label = "Clustered Standard Errors")
cluster_sampling_design <- population + estimand + stage_1_sampling + stage_2_sampling + clustered_ses


##########################################################################################
##########################################################################################

# correlation matrix #
sigma <- matrix(
  c(1, 0.4, 0.2,
    0.4, 1, 0.8,
    0.2, 0.8, 1),
  ncol = 3, nrow = 3
)

sigma
#

#
adv_data <- fabricate(
  primary_schools = add_level(N = 20, ps_quality = runif(N, 1, 10)),
  secondary_schools = add_level(N = 15, ss_quality = runif(N, 1, 10), nest = FALSE),
  colleges = add_level(N = 50, c_quality = runif(N, 1, 10), nest = FALSE),
  students = link_levels(
    N = 1500,
    by = join_using(
      ps_quality, ss_quality, c_quality,
      sigma = sigma
    ),
    earning_potential = 20000 + (2000 * ps_quality) +
      (6000 * ss_quality) + (10000 * c_quality) +
      rnorm(N, 0, 5000)
  )
)
#

head(adv_data)
#

# correlation matrix #
sigma <- matrix(
  c(1, 0.4, 0.2,
    0.4, 1, 0.8,
    0.2, 0.8, 1),
  ncol = 3, nrow = 3
)
#

#
adv_data <- fabricate(
  continent = add_level(N = 5, cont_happiness = runif(N, 1, 10), nest = FALSE),
  nation = add_level(N = 35, nation_happiness = runif(N, 1, 10), nest = FALSE),
  city = add_level(N = 50, city_happiness = runif(N, 1, 10), nest = FALSE),
  individual = link_levels(
    N = 1500,
    by = join_using(
      cont_happiness, nation_happiness, city_happiness,
      sigma = sigma),
    life_quality_yi = (1 * cont_happiness) + (2 * nation_happiness) + (3 * city_happiness) + runif(N, 0, 10),
    happiness_yi = scale(life_quality_yi) + rnorm(N)
  )
)
#

# cool #
adv_data %>% arrange(continent, nation, city)
adv_data %>% head()

adv_data$continent = factor(adv_data$continent)
adv_data$nation = factor(adv_data$nation)
adv_data$city = factor(adv_data$city)

# rename this #
table(adv_data$nation)
count(adv_data, continent, nation)
#

plot(adv_data$life_quality_yi, adv_data$happiness_yi)
#

#
summary(lm(happiness_yi ~ factor(continent), data=adv_data) )
summary(lm(happiness_yi ~ factor(continent) + factor(nation), data=adv_data) )
#

library(lme4)
lm(happiness_yi ~ life_quality_yi, data = adv_data) %>% summary()
lmer(happiness_yi ~ life_quality_yi + (1|city), data = adv_data) %>% summary()
#

#
library("estimatr")
# robust standard errors
adv_data
#
tidy(lm(happiness_yi ~ life_quality_yi, data = adv_data))
tidy(lm_robust(happiness_yi ~ life_quality_yi, clusters = city, data = adv_data))
#
res_rob
#