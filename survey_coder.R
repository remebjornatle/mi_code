####find path for R####
.libPaths("C:/R")

####load packages####
library(haven)
library(dplyr)
library(ggplot2)
library(corrplot)


#####load data####
df <- read_sav("C:/Users/t130054/Desktop/df17.sav")

setwd("C:/Users/t130054/Desktop/graphs")

View(df)

####Simple distribution####

#find all question columns
questions = df %>% 
  select(matches('Q0|hcountry')) %>% 
  select_if(is.numeric)


country.labels <- as.data.frame(attr(df$hcountry, "labels"))
questions$hcountry = factor(questions$hcountry, 
                            labels = rownames(country.labels))


names = colnames(questions)[grepl(pattern = "Q0",colnames(questions))]



for(i in 9:30) {
  
  temp = questions %>% 
    select(names[i])
  
  colnames(temp) = "temp_var"
  
  question.labels = attr(temp$temp_var,"label")
  
  options.labels = as.data.frame(attr(temp$temp_var,"labels")) 
  
  temp$temp_var = factor(temp$temp_var,ordered = TRUE, exclude = NULL, 
                             labels = rownames(options.labels))
  temp %>%
    ggplot(aes(temp_var)) +
    geom_bar() +
    coord_flip() +
    xlab(names[i]) +
    #scale_fill_discrete(breaks=rownames(options.labels)) +
    ggtitle(question.labels) +
    theme_minimal()
  
  ggsave(paste0(names[i],".pdf"))

  rm(temp)
    
}


####By group####


for(i in 9:30) {
  
  temp = questions %>% 
    select(c("hcountry",names[i]))
  
  colnames(temp)[2] = "temp_var"
  
  question.labels = attr(temp$temp_var,"label")
  
  options.labels = as.data.frame(attr(temp$temp_var,"labels")) 
  
  temp$temp_var = factor(temp$temp_var,ordered = TRUE, exclude = NULL, 
                         labels = rownames(options.labels))
  temp %>%
    ggplot(aes(temp_var)) +
    geom_bar() +
    coord_flip() +
    xlab(names[i]) +
    #scale_fill_discrete(breaks=rownames(options.labels)) +
    ggtitle(question.labels) +
    theme_minimal() +
    facet_wrap(~hcountry, ncol = 3)
  
  ggsave(paste0(names[i],"_facets",".pdf"))
  
  rm(temp)
  
}

####Correlations####
col3 <- colorRampPalette(c("red", "white", "blue")) 

questions %>% select_if(is.numeric) %>%
  select(contains("Q005")) %>% 
  cor(use = "complete.obs") %>%
  corrplot.mixed()
  corrplot(order = "hclust", addrect = 4, col = col3(20))
  #corrplot(order = "hclust", addrect = 4)


