# PACKAGES

# Structural topic modeling package
library(stm) 

# Network analysis and visualization package
library(igraph) 

# PREPROCESSING

# Read csv formatted data
data <- read.csv("C:/.../SSCI_data.csv") # Enter the datafile directory here

# Process data using function textProcessor()
processed <- textProcessor(data$ab, metadata = data) 

# Prepare data using function prepDocuments()
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 25)

# MODEL SELECTION

# Run diagnostic using function searchK()
kResult <- searchK(out$documents, out$vocab, K = c(5,10,15,20,25,30,35,40,45,50), init.type = "Spectral", prevalence =~ py + cy, data = out$meta)

# Plot diagnostic results using function plot()
plot(kResult)

# Semantic coherence-exclusivity plot using function plot()
plot(kResult$results$semcoh, kResult$results$exclus, xlab = "Semantic Coherence", ylab = "Exclusivity")

# Add labels to semantic coherence-exclusivity plot using function text()
text(kResult$results$semcoh, kResult$results$exclus, labels = paste("K", kResult$results$K), pos = 1)

# Semantic coherence-exclusivity table
knitr::kable(kResult$results)

# MODELING

# Specification for K topics using function stm()
model <- stm(out$documents, out$vocab, K = 24, max.em.its = 150, data = out$meta, init.type = "Spectral", prevalence =~ py + cy)

# Topic correlations using function topicCorr()
mod.out.coor <- topicCorr(model, method = "simple", cutoff = 0.01)

# Covariate effects using function estimateEffect() 
year_est <- estimateEffect(~ py, model, uncertainty = "None", metadata = out$meta)
citations_est <- estimateEffect(~ cy, model, uncertainty = "None", 
                                metadata = out$meta)

# Covariate effects summary using function summary()
summary(year_est)
summary(citations_est)

# POST-ESTIMATION DIAGNOTICS 

# Model summary using function summary()
summary(model)

# Find prototype documents using function findThoughts()
findThoughts(model, texts = out$meta$ti, topics = 21, n = 4)
findThoughts(model, texts = out$meta$ti, topics = 7, n = 4)
findThoughts(model, texts = out$meta$ti, topics = 14, n = 4)
findThoughts(model, texts = out$meta$ti, topics = 23, n = 4)
findThoughts(model, texts = out$meta$ti, topics = 11, n = 4)

# RESULTS

# Plot model results using function plot()
plot(model, type = "summary", labeltype = "frex", xlim = c(0,.20), 
     topic.names = c("CIVIL/HUMAN RIGHTS:", "LABOR MOVEMENT:",
                     "THREAT/REPRESSION:", "COLLECTIVE ACTION:", "MEDIA:", "POLITICAL OPPORTUNITY:", 
                     "WOMEN'S MOVEMENT:", "ENVIRONMENTAL MOVEMENT:",
                     "FRAMING/DISCOURSE:", "PLACE:", "THEORY/RESEARCH:", "MOVEMENT IDENTITY:", "POLICY/LAW:", "RACE/ETHNICITY:",
                     "METHODS:", "CORPORATE:", "PROTEST:",
                     "REPERTOIRES/DIFFUSION:", "PUBLIC SUPPORT:",
                     "ORGANIZATIONS/GROUPS:", "RECRUITMENT:",        
                     "DECISION-MAKING:", "POWER/STRUGGLE:",      
                     "MOBILIZATION:"))

# Plot model graph using function plot()
plot(mod.out.coor, vlabels = c("CIVIL/HUMAN RIGHTS", "LABOR MOVEMENT",
                               "THREAT/REPRESSION", "COLLECTIVE ACTION", 
                               "MEDIA", "POLITICAL OPPORTUNITY", 
                               "WOMEN'S MOVEMENT", "ENVIRONMENTAL MOVEMENT",    
                               "FRAMING/DISCOURSE", "PLACE", 
                               "THEORY/RESEARCH", "MOVEMENT IDENTITY", 
                               "POLICY/LAW", "RACE/ETHNICITY",
                               "METHODS", "CORPORATE", "PROTEST",
                               "REPERTOIRES/DIFFUSION", "PUBLIC SUPPORT",
                               "ORGANIZATIONS/GROUPS", "RECRUITMENT", 
                               "DECISION-MAKING", "POWER/STRUGGLE", 
                               "MOBILIZATION"))

# Plot covariate estimates using function plot()
plot(year_est, "py", method="continuous", topics=c(7,14), xlim=c(2005,2017), 
     xlab = "Year", labeltype = "custom", 
     custom.labels = c("Women's movement", "Race/ethnicity"))
plot(citations_est, "cy", method="continuous", topics=c(7,14), xlim=c(0,36), 
     xlab = "Citations Per Year", labeltype = "custom", 
     custom.labels = c("Women's movement", "Race/ethnicity"))
plot(citations_est, "cy", method="continuous", topics=c(2,21), xlim=c(0,36), 
     xlab = "Citations Per Year", labeltype = "custom", 
     custom.labels = c("Labor movement", "Recruitment"))

# CLEAN UP

rm(data)
rm(out)
rm(model)
rm(processed)
rm(kResult)
rm(year_est)
rm(citations_est)
rm(mod.out.coor)
