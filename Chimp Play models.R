
library(brms)
library(rstanarm)

setwd("your_directory")

df1=read.csv("/dataset_model1.csv")

####################
##fitting models


#model 1a - social and ecological conditions impacting the play likelihood of adult chimpanzees with partners of all ages
m1priors <- c(
  prior(normal(0, 2.5), class = "Intercept"),
  prior(normal(0, 2.5), class = "b")
)

model1=brm(play_all ~ z.age+z.rank+group+sin.date+cos.date+z.food+enc+hunt*sex+z.party+oestrus+offset(log(observation.duration))+
                 (1+z.party+z.food+sin.date+cos.date||Focal) +
                 (1+z.age+z.rank+z.party||day.id),
	data=df1, family = bernoulli(link = 'logit'), control = list(adapt_delta = 0.9),
	prior = m1priors,
	warmup = 500,
	iter = 3000,
	cores = 5,
	chains = 4,
	seed = 123)
	


summary(model1,
        probs = c(0.05, 0.11, 0.5, 0.89, 0.95),
        digits = 2)


bayes_R2(model1)


#model 1b - social and ecological conditions impacting the play likelihood of adult chimpanzees with adult partners


model1b=brm(adult_adult_play ~ z.age+z.rank+group+sin.date+cos.date+z.food+enc+hunt+z.party+oestrus*sex+offset(log(observation.duration))+
                 (1+z.party+z.food+sin.date+cos.date||Focal) +
                 (1+z.age+z.rank+z.party||day.id),
    data=df1, family = bernoulli(link = 'logit'), control = list(adapt_delta = 0.9),
    prior = m1priors,
    warmup = 500,
    iter = 3000,
    cores = 5,
    chains = 4,
    seed = 123)



#model 2 - social play partner chioce in adult chimpanzees

df2=read.csv("/dataset_model2.csv")

model2 = stan_clogit(
  play ~ 1 +
	z.ddsi+
	prev.aggression+ 
    z.age.focal * z.age.partner+
    z.rank.focal * z.rank.partner+
    sex.focal * sex.partner+
	Group +
    (1|dyad)+
    (1|pot.partner),
  strata = bout.nr,
  data = df2,
  prior = normal(),
  QR = TRUE,
  algorithm = 'sampling',
  chains = 4,
  warmup = 500,
  iter = 3000,
  cores = 4,
  control = list(adapt_delta = 0.9),
  sparse = FALSE
)


summary(model2,
        probs = c(0.05, 0.11, 0.5, 0.89, 0.95),
        digits = 2)


mean(bayes_R2(model2))


  




#permutation procedures
set.seed(555)

#upload dataset that contains data on play behavior during intergroup encounter days
enc_obs=read.csv("/observed_encounter.csv")
#upload dataset that contains data on play behavior during non intergroup encounter days
enc_exp=read.csv("/expected_encounter.csv")

##how many days had play before the encounter
observed_statistic1 <- length(unique(enc_obs$day.id[enc_obs$before.enc==1]))/length(unique(enc_obs$day.id))#0.76

# Permutation procedure - keep the focal information constant between the two datasets
enc_exp=enc_exp[enc_exp$Focal%in%unique(enc_obs$Focal),]


num_permutations <- 1000

permuted_statistics1 <- numeric(num_permutations)

enc_exp$nums=1:nrow(enc_exp)
for (i in 1:num_permutations) {
  permuted_data <- enc_exp
  for(j in 1:length(unique(permuted_data$Focal))){
      sel=permuted_data[permuted_data$Focal==unique(permuted_data$Focal)[j],]
      focal=sel$Focal[1]
  sample_times<- sample(df4$time.encounter[enc_obs$Focal==focal], size=nrow(enc_exp[enc_exp$Focal==focal,]), replace=TRUE)
  permuted_data$time.encounter[permuted_data$nums%in%sel$nums]=sample_times
  
  }
  permuted_data$b=(permuted_data$time.encounter)-permuted_data$time_min
  permuted_data$before.enc=0
  permuted_data$before.enc[permuted_data$b>=0]=1
  permuted_statistics1[i] <- length(unique(permuted_data$day.id[permuted_data$before.enc==1]))/length(unique(permuted_data$day.id))
}

mean(permuted_statistics1)




##hunt permutations
#upload dataset that contains data on play behavior of males during hunting days
hunt_obs=read.csv("/observed_hunt.csv")
#upload dataset that contains data on play behavior of males during non-hunting days
hunt_exp=read.csv("/expected_hunt.csv")# includes only focals that have data on hunting days

##how many days had play before the encounter
observed_statistic2 <- length(unique(hunt_obs$day.id[hunt_obs$before.hunt==1]))/length(unique(hunt_obs$day.id))#0.82

num_permutations <- 1000

permuted_statistics2 <- numeric(num_permutations)

hunt_exp$nums=1:nrow(hunt_exp)
for (i in 1:num_permutations) {
  permuted_data <- hunt_exp
  for(j in 1:length(unique(permuted_data$Focal))){
      sel=permuted_data[permuted_data$Focal==unique(permuted_data$Focal)[j],]
      focal=sel$Focal[1]
  sample_times<- sample(hunt_obs$time.hunt[hunt_obs$Focal==focal], size=nrow(hunt_exp[hunt_exp$Focal==focal,]), replace=TRUE)
  permuted_data$time.hunt[permuted_data$nums%in%sel$nums]=sample_times
  
  }
  permuted_data$b=(permuted_data$time.hunt)-permuted_data$time_min
  permuted_data$before.hunt=0
  permuted_data$before.hunt[permuted_data$b>=0]=1
  permuted_statistics2[i] <- length(unique(permuted_data$day.id[permuted_data$before.hunt==1]))/length(unique(permuted_data$day.id))
}

mean(permuted_statistics2)

