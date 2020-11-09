### Libraries
#### Be sure you have installed all these packages
#### (Work in a Rstudio that is installed in your pc to avoid problems with the package instalation)

library(stm)
library(dplyr)
library(tidytext)
library(bigrquery)
library(purrr)
library(tidyr)
library(ggplot2)
library(furrr)
library(stminsights)
library(quanteda)

#Step 1 out of 5: input
data = read.csv("C:/Users/s153832/Desktop/JADS/Master/Semester_1.1/Strategy_and_Business_Models/Assignment/total.csv", header = TRUE)
print(paste("number of observations:",length(data$my_id))) #9713

my_stopwords <-stopwords('english')
print(paste("number of stopwords:",length(my_stopwords))) #493
#Step 2 out of 5: Preparing the corpus
# table<-as.data.frame(data)
processed<- textProcessor(data$clean,metadata=data,
                          lowercase=FALSE, removestopwords=FALSE, removenumbers=FALSE,
                          removepunctuation=FALSE, stem=FALSE,
                          language="en",verbose=TRUE, onlycharacter= FALSE, striphtml=FALSE)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
# 
# #Step 3 out of 5: Finding the number of topics
# ###########
# # Warning #
# ###########
# #If your pc or laptop does not have a large RAM, is better to jump this step. 
# 
print(paste("First heuristic to determine the number of topics:",round(sqrt(length(data$clean)/2),0)))
# # 
# # 
# #you can use here the package future, and the package furrr for parallel processing
# plan(multiprocess)
# 
# many_models <- data_frame(K = c(5, 7, 10, 12, 15, 17, 20)) %>%
#   mutate(topic_model = future_map(K, ~stm(documents=docs, vocab=vocab, K = ., max.em.its = 10,
#                                           verbose = FALSE)))
# 
# heldout <- make.heldout(documents=docs, vocab=vocab)
# 
# k_result <- many_models %>%
#   mutate(exclusivity = map(topic_model, exclusivity),
#          semantic_coherence = map(topic_model, semanticCoherence, documents=docs),
#          eval_heldout = map(topic_model, eval.heldout, heldout$missing),
#          residual = map(topic_model, checkResiduals, documents=docs),
#          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
#          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
#          lbound = bound + lfact,
#          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
# 
# k_result %>%
#   transmute(K,
#             `Lower bound` = lbound,
#             Residuals = map_dbl(residual, "dispersion"),
#             `Semantic coherence` = map_dbl(semantic_coherence, mean),
#             `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
#   gather(Metric, Value, -K) %>%
#   ggplot(aes(K, Value, color = Metric)) +
#   geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
#   facet_wrap(~Metric, scales = "free_y") +
#   labs(x = "K (number of topics)",
#        y = NULL,
#        title = "Model diagnostics by number of topics",
#        subtitle = "These diagnostics indicate that a good number of topics would be a number before 100")
# 
# k_result %>%
#   select(K, exclusivity, semantic_coherence) %>%
#   filter(K %in% c(10,20,70,100)) %>%
#   unnest() %>%
#   mutate(K = as.factor(K)) %>%
#   ggplot(aes(semantic_coherence, exclusivity, color = K)) +
#   geom_point(size = 2, alpha = 0.7) +
#   labs(x = "Semantic coherence",
#        y = "Exclusivity",
#        title = "Comparing exclusivity and semantic coherence",
#        subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")
# 
# #Step 4 out of 5: running the algorithm
# 
STMfit <- stm(documents = docs, vocab = vocab, K = 30,data = meta,verbose=TRUE,
              #iterations before convergence:10, based on our previous test
              max.em.its = 10,
              #see a preview of the topics
              reportevery=5,

              prevalence = ~ s(Goal_USD) + s(Pledge_USD) + s(Number_Backers),

              seed=123456789, init.type = "Spectral"
)
# ctmFit <- stm(documents = docs, vocab = vocab, K = 15,
#              max.em.its = 150, data = out$meta, init.type = "Spectral", seed = 300)
# # 
# 
# #Step 5 out of 5: Topic Exploration
plot(STMfit, n=5,labeltype = "frex", topics = 1:15, type="summary")
plot(STMfit, n=5,labeltype = "frex", topics = 1:15, type="hist")
# 
# #### You can check other plots using ggplot, or plotly. 
# 
# #Beta
# # STMfit$beta
dim(STMfit$beta$logbeta[[1]])
# 
# #Words (observed words)
# # STMfit$vocab 
# 
# #with the function labelTopics we generate sets of words that represent each topic, using 4 different labelling algorithms. These measures are done from the Betas.
labelTopics(STMfit,n=10)
# 
# #one can compare the words often used by two topics
plot(STMfit, n=20,labeltype = "frex", topics = c(8,4),  type="perspectives")
# 
# #Sigma: covariance matrix. 
# # STMfit$sigma
plot(topicCorr(STMfit, method='huge'),cutoff=0.1)
# 
# #Theta: Topic proportions.
# #don't run# STMfit$theta
# barplot(STMfit$theta[1,],names.arg = paste("t",1:20)) #Example: the first document belongs to topic8 with a proportion greater than 0.35.
# 
# #We can add the Theta matrix into the original data. We can estimate new variables based on the theta values. 
# STM_DATA<-cbind(meta,STMfit$theta)
# #saveRDS(STM_DATA,paste0(direct,"DATA_STM.rds")) You can save this data for a future statistical analysis

# 
# # Estimate a regression using STM and covariates
# STMest <- estimateEffect(1:20 ~ first_author + channel + s(nb_days) + post_type +s(shares), STMfit,meta =meta)
# 
# save(out,STMfit,STMest,file=paste0(direct,"STM_elements.RData"))
# 
# ## STMinsights: Shiny Web app to visualize and analyze our topic modeling
# run_stminsights()
# 
# #in case you want to do the plots without the web app:
# tmp=get_effects(estimates=STMest,variable='first_author',type='pointestimate',ci=0.95)

