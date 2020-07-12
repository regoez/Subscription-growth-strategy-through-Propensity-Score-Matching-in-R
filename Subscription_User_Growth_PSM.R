hn <- read.csv("/Users/filepath/Midterm/data.csv", header = TRUE)
head(hn)

#Pre-work: Installing and loading packages

#install.packages("corrplot")
#install.packages("ggExtra")
library(dplyr)             
library(psych)             
library(ggplot2)           
library(MatchIt)           
library(corrplot)          
library(gridExtra)         
library(ggExtra)           
library(car)               
options(scipen = 999)      

#Data cleaning
#1. data demension
dim(hn)
#2. missing value check
sum(is.na(hn))
#3. Variable type check
glimpse(hn)
hn <- hn[,-1]

head(hn)
#3. Subsetting
#1) adopter only df
ad <- hn %>% filter(adopter == 1)
ad <- ad[,-c(12)]
head(ad)

#2) non-adopter only df
nad <- hn %>% filter(adopter == 0)
nad <- nad[,-c(12)]
head(nad)

#Question 1: Descriptive statistics
#install.packages("pastecs")
library(pastecs)
stat.desc(ad)
stat.desc(nad)

#Descriptive Statistics by Group (0: nonsubscriber, 1:subscriber)
desc_stat <- describeBy(hn, hn$adopter)
print(desc_stat)

# Differences in adopter and nonadopter including IQR, examining the difference between the two groups
library(psych)
ad2<-describe(ad, IQR=TRUE)
nad2<-describe(nad, IQR=TRUE)
gap <- ad2-nad2
gap = gap[,-c(1,5,6,11)]
gap

#T-test for each variable by groups(adopter, non-adopter)
with(hn, t.test(age ~ adopter))
with(hn, t.test(male ~ adopter))
with(hn, t.test(friend_cnt ~ adopter))
with(hn, t.test(avg_friend_age ~ adopter))
with(hn, t.test(avg_friend_male ~ adopter))
with(hn, t.test(friend_country_cnt ~ adopter))
with(hn, t.test(subscriber_friend_cnt ~ adopter))
with(hn, t.test(songsListened ~ adopter))
with(hn, t.test(lovedTracks ~ adopter))
with(hn, t.test(playlists ~ adopter))
with(hn, t.test(shouts ~ adopter))
with(hn, t.test(tenure ~ adopter))
with(hn, t.test(good_country ~ adopter))

# Visualization
# hn$adopter <- as.factor(hn$adopter)

#general correlation
#normalize
norm <- c("age", "friend_cnt", "avg_friend_age", "friend_country_cnt", "songsListened", "lovedTracks", "posts", "shouts", "tenure")

library(RColorBrewer)
U <-cor(hn)
corrplot(U, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

ad_del <- ad[-12]
S <-cor(ad_del)
corrplot(S, type="lower", order="hclust", main = "Subscribers", mar=c(0,0,2,0),
         col=brewer.pal(n=8, name="Set3"))


nad_del <- nad[-12]
N <-cor(nad_del)
corrplot(N, type="lower", order="hclust", main = "Free Users", mar=c(0,0,2,0),
         col=brewer.pal(n=8, name="Set3"))


#1. Demographics
#adopter vs freeuser comparison
age <- ggplot(hn, aes(x = adopter, y = age)) + geom_boxplot() + labs(title = "Age") + theme(plot.title = element_text(hjust = 0.5))
age

male <- ggplot(hn,aes(x=adopter, group=male,fill=male))+
  geom_bar(position="dodge")+theme_classic() + labs(title = "Sex (Men 1, Women 0)") + theme(plot.title = element_text(hjust = 0.5))
male

#Good Country
GC <- ggplot(hn,aes(x=adopter,group=good_country,fill = factor(good_country)))+
       geom_bar(position="dodge")+theme_classic() + labs(title = "Good Country") + theme(plot.title = element_text(hjust = 0.5))
GC

#2. User Engagement

#adopter vs freeuser comparison

#3. Peer influence

#adopter vs freeuser comparison
#friend_cnt, Eliminating extreme values
hn$adopter <- as.factor(hn$adopter)
p <- ggplot(hn, aes(x=adopter, y=friend_cnt)) + 
      geom_boxplot() + ylim(0, 150) + labs(title = "Friend Count (~150)") +
  theme(plot.title = element_text(hjust = 0.5))
p #645 rows are removed

#subscriber_friend_cnt
sf <- ggplot(hn, aes(x = adopter, y = subscriber_friend_cnt)) + geom_boxplot() + labs(title = "No. of Subscriber friends") + theme(plot.title = element_text(hjust = 0.5)) + ylim(0,50)
sf

#friend age
FA <- ggplot(hn, aes(x=adopter, y=friend_country_cnt)) + geom_boxplot(aes(fill=adopter),outlier.colour="blue", outlier.shape=6,outlier.size=0.5) + labs(title = "Friends Age") + theme(legend.position="none") + theme(plot.title = element_text(hjust = 0.5))
FA

#Friend's Country
FC <- ggplot(hn, aes(x=adopter, y=avg_friend_age)) + geom_boxplot(aes(fill=adopter),outlier.colour="blue", outlier.shape=5,outlier.size=0.4) + labs(title = "Friends Country Count") + theme(legend.position="none") + theme(plot.title = element_text(hjust = 0.5))
FC

#User Engagement
#shouts x songs listened
ss <- ggplot(hn, aes(x=songsListened, y=shouts,shape=adopter, color=adopter)) + geom_point(size=1) + labs(title = "Shouts X Songs Listened") + xlim(0,350000) + ylim(0,7500) + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
ss

#loved track x posting
lp <- ggplot(hn, aes(x=lovedTracks, y=posts,shape=adopter, color=adopter)) + geom_point(size=1) + labs(title = "Loved Track X Posts")  + theme(plot.title = element_text(hjust = 0.5)) + theme_classic()
lp
xlim(0,350000) + ylim(0,7500) 
hn$adopter <- as.integer(hn$adopter)

#PSM
#creating treatment group
hn$adopter <- as.numeric(hn$adopter)
hn2 <- mutate(hn,treatment=ifelse(hn$subscriber_friend_cnt >=1,1,0))
hn2 %>%
  group_by(adopter) %>% summarise(mean_treatment = mean(treatment),users=n())
with(hn2, t.test(treatment ~ adopter))

hn2$adopter <- as.numeric(hn2$adopter)
#calculate the mean for each covariate by treatment status
hn_cov <- c('age', 'male', 'friend_cnt', 'avg_friend_age', 'avg_friend_male', 'friend_country_cnt','songsListened', 'lovedTracks', 'posts', 'playlists', 'shouts', 'adopter', 'tenure', 'good_country')              
hn2 %>%
  group_by(treatment) %>%
  select(one_of(hn_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

lapply(hn_cov, function(v) {
  t.test(hn2[, v] ~ hn2$treatment)
})

# Propensity score estimation
m_ps <- glm(treatment ~ age+male+friend_cnt+avg_friend_age+avg_friend_male+friend_country_cnt+songsListened+lovedTracks+posts+playlists+shouts+tenure+good_country,
            family = binomial(), data = hn2)
summary(m_ps)

vif(m_ps)

# With this model, propensity score for each user can be calculated :It is simply the user’s predicted probability of being Treated, given the estimates from the logit model.

propensity <- data.frame(pr_score = predict(m_ps, type = "response"),
                     treatment = m_ps$model$treatment)
propensity

# Examining the region of common support
# After estimating the propensity score, it is useful to plot histograms of the estimated propensity scores by treatment status

labs <- paste("Having subscriber friend:", c("0 friend", "1+ friends"))
propensity %>%
  mutate(treatment = ifelse(treatment == 0, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~treatment) +
  xlab("Probability of having subscriber friends") +
  theme_bw()

sum(is.na(hn2))

# The method we use below is to find pairs of observations that have very similar propensity scores, 
# but that differ in their treatment status. We use the package MatchIt for this. 
# This package estimates the propensity score in the background and then matches observations based 
# on the method of choice (“nearest” in this case).


mod_match <- matchit(treatment ~ age+male+friend_cnt+avg_friend_age+avg_friend_male+friend_country_cnt+songsListened+lovedTracks+posts+playlists+shouts+tenure+good_country,
                     method = "nearest", data = hn2)
summary(mod_match)
plot(mod_match)

# To create a dataframe containing only the matched observations, use the match.data() function
dta_m <- match.data(mod_match)
print("Dimensions of the matched df")
dim(dta_m)

# The final dataset contains a variable called distance, which is the propensity score.
# Examining covariate balance in the matched sample
# Visual Inspection

fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$treatment <- as.factor(dta$treatment)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = treatment)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

grid.arrange(
  fn_bal(dta_m, "age"),
  fn_bal(dta_m, "male") + theme(legend.position = "none"),
  fn_bal(dta_m, "friend_cnt"),
  fn_bal(dta_m, "avg_friend_age") + theme(legend.position = "none"),
  fn_bal(dta_m, "avg_friend_male"),
  fn_bal(dta_m, "songsListened") + theme(legend.position = "none"),
  fn_bal(dta_m, "lovedTracks"),
  fn_bal(dta_m, "posts") + theme(legend.position = "none"),
  fn_bal(dta_m, "playlists"),  
  fn_bal(dta_m, "shouts") + theme(legend.position = "none"),
  fn_bal(dta_m, "tenure"),
  fn_bal(dta_m, "good_country") + theme(legend.position = "none"),
  fn_bal(dta_m, "friend_country_cnt"),
  nrow = 7)

####
# Difference of means

dta_m %>%
  group_by(treatment) %>%
  select(one_of(hn_cov)) %>%
  summarise_all(funs(mean))

lapply(hn_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$treatment)
})
#####
# Estimating treatment effects by t-test
# Estimating the treatment effect is simple once we have a matched sample that we are happy with. 

with(dta_m, t.test(adopter ~ treatment))

## regression analysis###
#categorical variable, binary dependent variable: glm
#glm model with all variables
dta_m$adopter <- as.factor(dta_m$adopter)
glm1 <- glm(adopter ~ treatment+age+male+friend_cnt+avg_friend_age+avg_friend_male+friend_country_cnt+songsListened+lovedTracks+posts+playlists+shouts+tenure+good_country, data = dta_m,family='binomial')
summary(glm1)

#glm model only with selected important variables
dta_m$adopter <- as.integer(dta_m$adopter)
glm_imp <- glm(adopter ~ treatment+age+male+avg_friend_age+friend_country_cnt+songsListened+lovedTracks+playlists+tenure+good_country, data = dta_m)
summary(glm_imp)

#Odds ratio of the variables
exp(glm_imp$coefficients)


# reference code work: https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
