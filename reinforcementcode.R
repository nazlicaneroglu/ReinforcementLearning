### file path etc
#rm(list=ls())
options(scipen=6, digits=4)
##packages anf libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,contextual, tidyverse, tinytex, rmarkdown, glmnet, matlib, MASS,pdist, PMA, softImpute, dplyr,plotrix, kernlab,ranger, randomForest, knitr,
               ggplot2,
               png,
               tidyverse,
               rlist,
               contextual,
               lubridate,
               zoo,
               roll)
install.packages("ggplot2")
install.packages("FactoMineR")
install.packages("vcd")
install.packages("fpc")
install.packages("roll")
install.packages("factoextra")
install.packages("zoo")
install.packages("fastDummies")
install.packages("GGally")
install.packages("cluster")
install.packages("NbClust")
install.packages("magrittr")
install.packages("data.table")
install.packages("tidyverse")
install.packages("cowplot")
library(data.table)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(magrittr)
library(GGally)
library(fastDummies)
library(dplyr)
library(matlib)
library(glmnet, quietly = TRUE)
library(caTools)
library("PMA")
library("softImpute")
library(FactoMineR)
library(vcd)
library(factoextra)
library(cluster)
library(NbClust)
library(zoo)
library(readr)
library(contextual)

##Data & seed
set.seed(8913)
loan_data<- read_csv("loan_data_set.csv")
View(loan_data)
attach(loan_data)

#Check whether we have omitted variables
summary(loan_data)
sum(complete.cases(loan_data))

#create a category
summary(loan_data$LoanAmount)

loan_data$loancat <- NA

loan_data$loancat[LoanAmount >  168.0 ] <- 1
loan_data$loancat[LoanAmount >  128.0 & LoanAmount <= 168.0] <- 2
loan_data$loancat[LoanAmount >  100.0 & LoanAmount <=  128.0] <- 3
loan_data$loancat[LoanAmount <=  100.0] <- 4

loan_data_final<-loan_data[complete.cases(loan_data), ]
str(loan_data_final)
sum(complete.cases(loan_data_final))
loan_data_final <- mutate(loan_data_final, Loan_Status2 = ifelse(Loan_Status=="Y",1,0))
loan_data_final <- mutate(loan_data_final,  Married2 = ifelse(Married=="Yes",1,0))
loan_data_final <- mutate(loan_data_final,  Dependents2 = ifelse(Dependents=="3+",1,0))
loan_data_final$Dependents[loan_data_final$Dependents=="3+"]<- 3
loan_data_final$Dependents <- as.numeric(loan_data_final$Dependents)
loan_data_final$Credit_History <- as.numeric(loan_data_final$Credit_History)
loan_data_final <- mutate(loan_data_final,  Male = ifelse(Gender=="Male",1,0))
loan_data_final <- mutate(loan_data_final,  Edu = ifelse(Education=="Graduate",1,0))
loan_data_final <- mutate(loan_data_final,  Self_E = ifelse(Self_Employed=="Yes",1,0))

attach(loan_data_final)
##Some pre-analysis graphs

table1 <- table(loan_data_final$loancat, loan_data_final$Loan_Status2)
table1_prop <- prop.table(table1, margin = 1)
table1_prop <- cbind(sort(unique(loan_data_final$loancat)), table1_prop[,2])
colnames(table1_prop) <- c("loan_cat", "status")
table1_prop <- data.frame(table1_prop)

table1plot <- ggplot(table1_prop, aes(x=loan_cat, y=status, fill=as.numeric(loan_cat))) +
geom_bar(stat="identity", position=position_dodge(), show.legend =
        FALSE)+
labs(title="", x="Loan Category", y = "Status of the Loan")+
 geom_hline(yintercept=mean(table1_prop$status), linetype="dashed",
          color = "red")+
annotate("text", x = 0, y = mean(table1_prop$status)+0.05, label =
        "Mean", color = 'red')+
theme_minimal()


##############Thompson
ts_bandit<- OfflineReplayEvaluatorBandit$new(formula = Loan_Status2 ~ loancat , data = loan_data_final, randomize = FALSE)
agents  <- list(Agent$new(LinUCBDisjointOptimizedPolicy$new(0.01),  ts_bandit, "c = 0.01"),
                Agent$new(LinUCBDisjointOptimizedPolicy$new(0.1),  ts_bandit, "c = 0.1"),
                Agent$new(LinUCBDisjointOptimizedPolicy$new(1.0),  ts_bandit, "c = 1.0"),
                Agent$new(ThompsonSamplingPolicy$new(alpha = 1, beta = 1), ts_bandit, "TS"))

 
# simulate
size_sim=100000
n_sim=12
newsimulation <- Simulator$new(agents, horizon = size_sim, simulations = n_sim)

ts_history           <- newsimulation$run()


# gather results
df_ts_result <- ts_history$data%>%
  select(t, sim, choice, reward, agent) 


# check how many obs
max_obs_per_sim_ts = df_ts_result%>%
  group_by(sim, agent) %>%
  summarise(max_t = max(t))
max_obs_per_sim_ts

# Maximum number of observations
max_obs=1700

# data.frame aggregated for two versions: 20 and 40 arms
df_coins_agg_cumulative <- df_ts_result %>%
  group_by(agent, sim)%>% # group by number of arms, the sim
  mutate(cumulative_reward = cumsum(log(1 + reward)),
         rolling_reward = rollmean( reward, 252, na.pad=TRUE))%>%
  group_by(agent, t) %>% # group by number of arms, the t
  summarise(avg_cumulative_reward = mean(cumulative_reward),
            avg_rolling_reward = mean(rolling_reward),
            se_cumulative_reward = sd(cumulative_reward, na.rm=TRUE)/sqrt(n_sim),
            se_rolling_reward = sd(rolling_reward, na.rm=TRUE)/sqrt(n_sim)) %>%
  mutate(cumulative_reward_lower_CI =avg_cumulative_reward - 1.96*se_cumulative_reward,
         cumulative_reward_upper_CI =avg_cumulative_reward + 1.96*se_cumulative_reward,
         rolling_reward_lower_CI =avg_rolling_reward - 1.96*se_rolling_reward,
         rolling_reward_upper_CI =avg_rolling_reward + 1.96*se_rolling_reward)%>%
  filter(t <=max_obs)



# create ggplot object
ggplot(data=df_coins_agg_cumulative, aes(x=t, y=avg_cumulative_reward, color =agent))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymin=cumulative_reward_lower_CI , 
                  ymax=cumulative_reward_upper_CI,
                  fill = agent,
  ),
  alpha=0.1)+
  labs(x = 'Time', y='Cumulative Reward of trading', color ='Algorithm', fill='Algorithm')+
  theme_bw()+
  theme(text = element_text(size=16))

# plot number of selections
# gather results
df_TS <- df_ts_result %>%filter(agent=='TS')
df_UCB001 <- df_ts_result %>%filter(agent=='c = 0.01')
df_UCB01 <- df_ts_result %>%filter(agent=='c = 0.1')
df_UCB1 <- df_ts_result %>%filter(agent=='c = 1.0')

p001 <- ggplot(df_UCB001, aes(x=choice)) + 
  geom_bar(color = as.numeric(sort(unique(df_UCB001$choice))), 
           fill = as.numeric(sort(unique(df_UCB001$choice))))+
  labs(title="LinUCB (c=0.01)", x="Item id", y = 'Number of selections')+
  theme_minimal()
p001


p01 <- ggplot(df_UCB01, aes(x=choice)) + 
  geom_bar(color = as.numeric(sort(unique(df_UCB01$choice))), 
           fill = as.numeric(sort(unique(df_UCB01$choice))))+
  labs(title="LinUCB (c=0.1)", x="Item id", y = 'Number of selections')+
  theme_minimal()
p01

p1 <- ggplot(df_UCB1, aes(x=choice)) + 
  geom_bar(color = as.numeric(sort(unique(df_UCB1$choice))), 
           fill = as.numeric(sort(unique(df_UCB1$choice))))+
  labs(title="LinUCB (c=1.0)", x="Item id", y = 'Number of selections')+
  theme_minimal()
p1

p2 <- ggplot(df_TS, aes(x=choice)) + 
  geom_bar(color = as.numeric(sort(unique(df_TS$choice))), 
           fill = as.numeric(sort(unique(df_TS$choice))))+
  labs(title="TS", x="Item id", y = 'Number of selections')+
  theme_minimal()
p2

pall <- plot_grid(p2,p001,p01, p1)
pall


#reinforcement
coins_bandit_contextfull <- OfflineReplayEvaluatorBandit$new(formula =  Loan_Status2 ~ loancat |Edu + Self_E + Male + Dependents +Married2 + Loan_Amount_Term + CoapplicantIncome + ApplicantIncome , data = loan_data_final, randomize = FALSE, replacement = FALSE,)
coins_bandit_context <- OfflineReplayEvaluatorBandit$new(formula =  Loan_Status2 ~ loancat| Male + Dependents + Married2 + ApplicantIncome  , data = loan_data_final, randomize = FALSE, replacement = FALSE,)
ts_bandit<- OfflineReplayEvaluatorBandit$new(formula = Loan_Status2 ~ loancat , data = loan_data_final, randomize = FALSE)


alpha=0.1
linUCB <- LinUCBDisjointPolicy$new(alpha=alpha)
linUCBfull <- LinUCBDisjointPolicy$new(alpha=alpha)
ts<- ThompsonSamplingPolicy$new(alpha = 1, beta = 1)

agent_lin <- Agent$new(linUCB, coins_bandit_context, name='Linear UCB')
agent_full<- Agent$new(linUCBfull, coins_bandit_contextfull, name='Linear UCB with complete features')
agent_TS <-Agent$new(ts, ts_bandit, name='TS')
# simulate
size_sim=100000
n_sim=5
simulator          <- Simulator$new(list(agent_full, agent_lin, agent_TS), # set our agents
                                    horizon= size_sim, # set the sizeof each simulation
                                    do_parallel = TRUE, # run in parallel for speed
                                    simulations = n_sim, # simulate it n_sim times,
                                    
)


# run the simulator object  
history_coins           <- simulator$run()

# gather results
df_coins_result <- history_coins$data%>%
  select(t, sim, choice, reward, agent) 


# check how many obs
max_obs_per_sim = df_coins_result%>%
  group_by(sim, agent) %>%
  summarise(max_t = max(t))
max_obs_per_sim

# Maximum number of observations
max_obs=1700

# data.frame aggregated for two versions: 20 and 40 arms
df_coins_agg_cumulative <- df_coins_result %>%
  group_by(agent, sim)%>% # group by number of arms, the sim
  mutate(cumulative_reward = cumsum(log(1 + reward)),
         rolling_reward = rollmean( reward, 252, na.pad=TRUE))%>%
  group_by(agent, t) %>% # group by number of arms, the t
  summarise(avg_cumulative_reward = mean(cumulative_reward),
            avg_rolling_reward = mean(rolling_reward),
            se_cumulative_reward = sd(cumulative_reward, na.rm=TRUE)/sqrt(n_sim),
            se_rolling_reward = sd(rolling_reward, na.rm=TRUE)/sqrt(n_sim)) %>%
  mutate(cumulative_reward_lower_CI =avg_cumulative_reward - 1.96*se_cumulative_reward,
         cumulative_reward_upper_CI =avg_cumulative_reward + 1.96*se_cumulative_reward,
         rolling_reward_lower_CI =avg_rolling_reward - 1.96*se_rolling_reward,
         rolling_reward_upper_CI =avg_rolling_reward + 1.96*se_rolling_reward)%>%
  filter(t <=max_obs)



# create ggplot object
ggplot(data=df_coins_agg_cumulative, aes(x=t, y=avg_cumulative_reward, color =agent))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymin=cumulative_reward_lower_CI , 
                  ymax=cumulative_reward_upper_CI,
                  fill = agent,
  ),
  alpha=0.1)+
  labs(x = 'Time', y='Cumulative Reward of trading', color ='Algorithm', fill='Algorithm')+
  theme_bw()+
  theme(text = element_text(size=16))


# plot number of selections
# gather results
df_full <- df_coins_result %>%filter(agent=='Linear UCB with complete features')
df_lin <- df_coins_result %>%filter(agent=='Linear UCB')
df_thomp<- df_coins_result %>%filter(agent=='TS')

pfull <- ggplot(df_full, aes(x=choice)) + 
  geom_bar(color = as.numeric(sort(unique(df_full$choice))), 
           fill = as.numeric(sort(unique(df_full$choice))))+
  labs(title="LinUCB full", x="Item id", y = 'Number of selections')+
  theme_minimal()
pfull


pcontext <- ggplot(df_lin, aes(x=choice)) + 
  geom_bar(color = as.numeric(sort(unique(df_lin$choice))), 
           fill = as.numeric(sort(unique(df_lin$choice))))+
  labs(title="LinUCB rest", x="Item id", y = 'Number of selections')+
  theme_minimal()
pcontext

pTS<-  ggplot(df_thomp, aes(x=choice)) + 
  geom_bar(color = as.numeric(sort(unique(df_thomp$choice))), 
           fill = as.numeric(sort(unique(df_thomp$choice))))+
  labs(title="TS", x="Item id", y = 'Number of selections')+
  theme_minimal()
pTS

ptable <- plot_grid(pcontext,pfull, pTS)
ptable

