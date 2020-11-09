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

#Load the data
data = read.csv("C:/Users/s153832/Desktop/JADS/Master/Semester_1.1/Strategy_and_Business_Models/Assignment/final_clean_data.csv", header = TRUE)
print(paste("number of observations:",length(data$my_id))) #9713

#prepare the data.
#it is already lowercased, stemmed, without punctuation/URLs/emails/sparse terms, more than 50 words, and checked for language
#language checker is not great, so just check again here
processed<- textProcessor(data$clean,metadata=data,lowercase=FALSE, removestopwords=FALSE, removenumbers=FALSE, 
                          removepunctuation=FALSE, stem=FALSE,  
                          language="en",verbose=TRUE, onlycharacter= FALSE, striphtml=FALSE
                          ,customstopwords=FALSE)

#plot how many documents/words/tokens would be removed based on a threshold of amount of documents a words should appear in
plotRemoved(processed$documents, lower.thresh = seq(1,10, by = 1))

#for now I just used the threshold of 3 documents, but can be changed ofcourse
#prepare for STM
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 3)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#run the actual STM, for K topics, including characteristics for prevalence and/or content
stmFit <- stm(documents = out$documents, vocab = out$vocab, K=20, data = meta,verbose=TRUE,
               #max iterations
               max.em.its = 20,
               #see a preview of the topics every X iterations
               reportevery=5,
               #the prevalence characteristics
               #whether it is a rockstar or a flop, the goal, duration, update frequency based on literature, 
               #and amount of sparse terms that are removed (we assume this is a good way to measure amount of spelling mistakes)
               prevalence = ~ Goal_USD + duration + update_freq + sparse_terms,
               #the content characteristics
               content = ~ categorical,
               #leave the seed and init type
               seed=123456789, init.type = "Spectral"
               )


#this is the topic prevalence matrix in a dataframe
topic_prevalence <- data.frame(
  stmFit$theta)
#put this in a csv
#write.csv(topic_prevalence,"C:/Users/s153832/Desktop/JADS/Master/Semester_1.1/Strategy_and_Business_Models/Assignment/20_ellen_20.csv", row.names = FALSE)

#check the effects of the characteristics on determining each topic
#you can see if it is significant
#change formula to 1:#topics, and the variables to the variables you checked
effects <- estimateEffect(formula = 1:20 ~categorical + Goal_USD + duration + update_freq + sparse_terms, stmobj = stmFit,
               metadata = out$meta, uncertainty = "Global")
summary(effects)

#plot the topics and see whether they are more likely to be flop or rockstars
Result <- plot(
  effects,
  "categorical",
  method = "difference",
  cov.value1 = "3",
  cov.value2 = "1",
  verbose.labels = F,
  ylab = "Expected Difference in Topic Probability by Category (with 95% CI)",
  xlab = "More Likely Flop                           Not Significant                       More Likely Rockstar",
  main = "Effect of Rockstar/Flop on Topic Prevelance for Kickstarter Research",
  xlim = c(-0.1, 0.1)
)

# order this plot based on Expected Topic Proportion
rank = order(unlist(Result$means))
topicRnk <- topic[rank, ]

plot(
  effects,
  "categorical",
  method = "difference",
  cov.value1 = "3",
  cov.value2 = "1",
  verbose.labels = F,
  topics = topicRnk$TopicNumber,
  #labeltype = "custom",
  #custom.labels  = apply(topicNames$prob, 1, function(x) paste0(x, collapse = " + ")),
  ylab = "Expected Difference in Topic Probability by Category (with 95% CI)",
  xlab = "More Likely Flop                           Not Significant                       More Likely Rockstar",
  main = "Effect of Rockstar/Flop on Topic Prevelance for Kickstarter Research",
  xlim = c(-0.1, 0.1)
)


#INSPECT THE TOPICS
plot(stmFit, n=5,labeltype = "frex", topics = 1:15, type="summary")
plot(stmFit, n=5,labeltype = "frex", topics = 1:15, type="hist")
 
#with the function labelTopics we generate sets of words that represent each topic, using 4 different labelling algorithms. These measures are done from the Betas.
labelTopics(stmFit,n=10)
 
#one can compare the words often used by two topics
plot(stmFit, n=20,labeltype = "frex", topics = c(8,4),  type="perspectives")


#Sigma: covariance matrix. 
plot(topicCorr(stmFit, method='huge'),cutoff=0.1)

#Theta: Topic proportions.
barplot(stmFit$theta[1,],names.arg = paste("t",1:20)) #Example: the first document belongs to topic8 with a proportion greater than 0.35.

