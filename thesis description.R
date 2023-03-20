
library(na.tools)
library(readr)
library(Hmisc)
library(tigerstats)
library(text)
library(usethis)
library(reticulate)
library(factoextra)
library(epiDisplay)
library(forcats)
library(dplyr)
library(gitcreds)
library(corrplot)
library(lubridate)
reticulate::conda_list()
library(tibble)

Sys.setenv(RETICULATE_PYTHON = "C:\\Users\\desktop\\anaconda3\\envs\\textrpp_condaenv/python.exe")

create_github_token()
gitcreds_set()
use_git()
use_github()

loan <- read_csv("loan.csv")

theme_set(theme_bw())

set.seed(87031800)


loan$earliest_cr_line <- my(loan$earliest_cr_line)
loan$last_pymnt_d <- my(loan$last_pymnt_d)
loan$last_credit_pull_d <- my(loan$last_credit_pull_d)
loan$issue_d <- my(loan$issue_d)
loan$earliest_cr_line_year <- as.numeric(year(loan$earliest_cr_line))
loan$earliest_cr_line_month <- as.numeric(month(loan$earliest_cr_line))
loan$last_pymnt_d_year <- as.numeric(year(loan$last_pymnt_d))
loan$last_pymnt_d_month <- as.numeric(month(loan$last_pymnt_d))
loan$last_credit_pull_d_year <- as.numeric(year(loan$last_credit_pull_d))
loan$last_credit_pull_d_month <- as.numeric(month(loan$last_credit_pull_d))
loan$issue_d_year <- as.numeric(year(loan$issue_d))
loan$issue_d_month <- as.numeric(month(loan$issue_d))

loan <- subset(loan, select = -c(earliest_cr_line, last_pymnt_d, last_credit_pull_d, issue_d))

my_bootstrap <- function(data, index) {
  boot <- as.vector(unique(data[[index]]))
  boot <- boot[!is.na(boot)]
  for (j in 1:nrow(data)){
    if (is.na(data[[index]])[j]) {
      data[j,index] <- sample(boot, 1, replace = TRUE)
    }
  }
  return(data)
}


clean.data.by_bootstrap <- function(data, threshold){
  
  any_na_values <- 0
  full_na_values <- 0
  to_remove <- c()
  column_changed_mean <- c()
  
  for (i in 1:length(data)){
    if(any_na(data[, i])){
      if(all_na(data[, i])){
        full_na_values <- full_na_values + 1
        to_remove <- c(to_remove,i)
      }
      else {
        any_na_values <- any_na_values + 1
        if (sapply(data[,i], class) == "character") {
          data[is.na(data[,i]), i] <- "no information"
        }
        else if ((sum(is.na(data[,i]))/nrow(data)) > threshold){
          to_remove <- c(to_remove,i)
        }
        else {
          data <- my_bootstrap(data, i)
          column_changed_mean <- c(column_changed_mean, names(data)[i])
        }
      }
    }
    if (length(unique(data[[i]])) == 1) {
      to_remove <- c(to_remove,i)
    }
  }
  data_clean <- data[, -to_remove]
  
  print(c(any_na_values, full_na_values))
  
  return(as.data.frame(data_clean))
}


##################
## Preprocessing #
##################


loan$int_rate <- as.numeric(sub("%", "", loan$int_rate))
loan$revol_util <- as.numeric(sub("%", "", loan$revol_util))
loan <- loan %>% filter(loan_status != "Current")
new_data <- clean.data.by_bootstrap(loan, 0.5)


drop <- c("id", "member_id", "grade","sub_grade", "zip_code", "url")
new_data <- new_data[,!(names(new_data) %in% drop)]

 new_data$pub_rec_bankruptcies[new_data$pub_rec_bankruptcies == 2] <- 1

new_data$term <- as.factor(new_data$term)
new_data$emp_length <- as.factor(new_data$emp_length)
new_data$home_ownership <- as.factor(new_data$home_ownership)
new_data$verification_status <- as.factor(new_data$verification_status)
new_data$loan_status <- as.factor(new_data$loan_status)
new_data$addr_state <- as.factor(new_data$addr_state)
new_data$pub_rec <- as.factor(new_data$pub_rec)
new_data$total_acc <- as.factor(new_data$total_acc)
new_data$purpose <- as.factor(new_data$purpose)
new_data$pub_rec_bankruptcies <- as.factor(new_data$pub_rec_bankruptcies)
new_data$earliest_cr_line_year <- as.factor(new_data$earliest_cr_line_year)
new_data$earliest_cr_line_month <- as.factor(new_data$earliest_cr_line_month)
new_data$last_pymnt_d_year <- as.factor(new_data$last_pymnt_d_year)
new_data$last_pymnt_d_month <- as.factor(new_data$last_pymnt_d_month)
new_data$last_credit_pull_d_year <- as.factor(new_data$last_credit_pull_d_year)
new_data$last_credit_pull_d_month <- as.factor(new_data$last_credit_pull_d_month)
new_data$issue_d_year <- as.factor(new_data$issue_d_year)
new_data$issue_d_month <- as.factor(new_data$issue_d_month)

num_cols <- unlist(lapply(new_data, is.numeric))

data_correlations <- new_data[ , c(num_cols)]

corrplot(round(cor(data_correlations),1), method = "color",
         addCoef.col = "black",tl.col="black", tl.srt=45, insig = "blank")
drop <- c("funded_amnt", "funded_amnt_inv", "total_pymnt", "total_pymnt_inv",
          "total_rec_prncp", "total_rec_int", "installment", "collection_recovery_fee", "pub_rec", "total_acc")
data_correlations <- data_correlations[,!(names(data_correlations) %in% drop)]
corrplot(round(cor(data_correlations),1), method = "color",
         addCoef.col = "black",tl.col="black", tl.srt=45, insig = "blank")
new_data <- new_data[,!(names(new_data) %in% drop)]


levels(new_data$emp_length)[levels(new_data$emp_length)=="n/a"] <- "no information"

categ_cols <- unlist(lapply(new_data, is.factor))
data_categorical <- new_data[ , c(categ_cols)]

for(i in names(data_categorical)){
  data_categorical[i] <- sapply(data_categorical[i], as.numeric)
}

corrplot(round(cor(data_categorical),1), method = "color",
         addCoef.col = "black",tl.col="black", tl.srt=45, insig = "blank")

corrplot(round(cor(cbind(data_categorical, data_correlations)),1), method = "color",
         addCoef.col = "black",tl.col="black", tl.srt=45, insig = "blank")



###############
# Description #
###############


describe(new_data)

html(describe(new_data), size=85, tabular=TRUE,
     greek=TRUE, scroll=FALSE)

tab1(new_data$loan_status, sort.group = "decreasing", cum.percent = TRUE)

mean1 <- new_data %>% group_by(loan_status) %>% summarise(mean_loan=mean(int_rate))

ggplot(new_data, aes(x=loan_amnt, y=int_rate)) +
  geom_point(aes(color=loan_status, alpha = 0.2)) +
  geom_hline(data=mean1, aes(yintercept=mean_loan, col=loan_status), linewidth = 1.5)

mean1 <- new_data %>% group_by(loan_status) %>% summarise(mean_inc=mean(log(annual_inc)))

ggplot(new_data, aes(x=loan_amnt, y=log(annual_inc))) +
  geom_point(aes(color=loan_status)) +
  geom_hline(data=mean1, aes(yintercept=mean_inc, col=loan_status), linewidth = 1.5)

ggplot(new_data, aes(x = purpose, fill = loan_status)) + 
  geom_bar(position = 'fill') +
  geom_text(aes(x = purpose, 
                label = scales::percent(after_stat(count / tapply(count, x, sum)[x])), 
                group = loan_status), position = "fill", stat = "count") + labs(y = "proportion")+
  scale_fill_brewer(palette="Paired")


######################
# k-means emp_length #
######################


xtabs(~emp_length+loan_status, data = new_data)
tab <- as.data.frame(rowPerc(xtabs(~emp_length+loan_status, data = new_data)))
tab <- cbind(tab[tab$loan_status == "Charged Off",], tab[tab$loan_status == "Fully Paid",])
tab <- tab[, c(1,3,6)]
colnames(tab) <- c("emp_length", "Charged_Off", "Fully_Paid")


ggplot(tab, aes(x = Charged_Off, y = Fully_Paid)) + geom_point(aes(color = emp_length))

rownames(tab) <- tab$emp_length
tab <- tab[, -1]

fviz_nbclust(tab, kmeans, method = "silhouette", k.max = 8)
fviz_nbclust(tab, kmeans, method = "wss", k.max = 8)

km_emp_length <- kmeans(tab, 4, nstart = 25)

levels(new_data$emp_length) <- (as.data.frame(km_emp_length$cluster))[,1]

fviz_cluster(km_emp_length, tab, ggtheme = theme_bw(), labelsize = 15) + ggtitle(label="emp_length levels clustering") 



####################
## k-means purpose #
####################


tab <- as.data.frame(rowPerc(xtabs(~purpose+loan_status, data = new_data)))
tab <- cbind(tab[tab$loan_status == "Charged Off",], tab[tab$loan_status == "Fully Paid",])
tab <- tab[, c(1,3,6)]
colnames(tab) <- c("purpose", "Charged_Off", "Fully_Paid")


ggplot(tab, aes(x = Charged_Off, y = Fully_Paid)) + geom_point(aes(color = purpose))

rownames(tab) <- tab$purpose
tab <- tab[, -1]

fviz_nbclust(tab, kmeans, method = "silhouette", k.max = 8)
fviz_nbclust(tab, kmeans, method = "wss", k.max = 8)

km_purpose <- kmeans(tab, 3, nstart = 25)

km_purpose$size
km_purpose$cluster

levels(new_data$purpose) <- (as.data.frame(km_purpose$cluster))[,1]

fviz_cluster(km_purpose, tab, ggtheme = theme_bw()) + ggtitle(label="purpose levels clustering")


#################################
# k-means earliest_cr_line_year #
#################################


xtabs(~earliest_cr_line_year+loan_status, data = new_data)

tab <- as.data.frame(rowPerc(xtabs(~earliest_cr_line_year+loan_status, data = new_data)))
tab <- cbind(tab[tab$loan_status == "Charged Off",], tab[tab$loan_status == "Fully Paid",])
tab <- tab[, c(1,3,6)]
colnames(tab) <- c("earliest_cr_line_year", "Charged_Off", "Fully_Paid")


ggplot(tab, aes(x = Charged_Off, y = Fully_Paid)) + geom_point(aes(color = earliest_cr_line_year))

rownames(tab) <- tab$earliest_cr_line_year
tab <- tab[, -1]

fviz_nbclust(tab, kmeans, method = "silhouette", k.max = 8)
fviz_nbclust(tab, kmeans, method = "wss", k.max = 8)

km_earliest_cr_line_year <- kmeans(tab, 6, nstart = 25)

levels(new_data$earliest_cr_line_year) <- (as.data.frame(km_earliest_cr_line_year$cluster))[,1]
fviz_cluster(km_earliest_cr_line_year, tab, ggtheme = theme_bw())  + ggtitle(label="earliest_cr_line_year levels clustering")

######################
# k-means addr_state #
######################


xtabs(~addr_state+loan_status, data = new_data)

tab <- as.data.frame(rowPerc(xtabs(~addr_state+loan_status, data = new_data)))
tab
tab <- cbind(tab[tab$loan_status == "Charged Off",], tab[tab$loan_status == "Fully Paid",])
tab <- tab[, c(1,3,6)]
colnames(tab) <- c("addr_state", "Charged_Off", "Fully_Paid")


ggplot(tab, aes(x = Charged_Off, y = Fully_Paid)) + geom_point(aes(color = addr_state))

rownames(tab) <- tab$addr_state
tab <- tab[, -1]

fviz_nbclust(tab, kmeans, method = "silhouette", k.max = 8)
fviz_nbclust(tab, kmeans, method = "wss", k.max = 8)

km_addr_state<- kmeans(tab, 3, nstart = 25)
km_addr_state$size
km_addr_state$cluster

levels(new_data$addr_state) <- (as.data.frame(km_addr_state$cluster))[,1]

fviz_cluster(km_addr_state, tab, ggtheme = theme_bw()) + ggtitle(label="addr_state levels clustering")




####################
# clustering in HD #
####################


data_clustering <- new_data[,colnames(new_data)[grepl('factor|num',sapply(new_data,class))]]


data_clustering <- new_data[,colnames(new_data)[grepl('num',sapply(new_data,class))]]


var_standardization <- function(data,variable) {
  y <- paste("data$",sep = "",variable)
  x <- eval(parse(text=y))
  x <- (x-min(x))/(max(x)-min(x))
  
  name <- paste("Scaled_",sep = "", variable)
  
  data <- data[, ! names(data) %in% variable, drop = F]
  data <- data.frame(data,x)
  names(data)[names(data) == 'x'] <- name
  return(data)
}










#########################
## Exemple of embedding #
#########################


tittle <- as_tibble(new_data$title)

desc <- as_tibble(new_data$desc)

# word_embeddings <- textEmbed(
#   texts = tittle,
#   model = "bert-base-uncased",
#   layers = -2,
#   aggregation_from_tokens_to_texts = "mean",
#   aggregation_from_tokens_to_word_types = "mean",
#   keep_token_embeddings = FALSE)
# 
# desk <- function(desc) {
#   word_embeddings2 <- textEmbed(
#     texts = desc,
#     model = "bert-base-uncased",
#     layers = -2,
#     aggregation_from_tokens_to_texts = "mean",
#     aggregation_from_tokens_to_word_types = "mean",
#     keep_token_embeddings = FALSE)
#   
#   saveRDS(word_embeddings2, "desc_embeddings.rds")
#   
# }

# desk(desc)

dataforprojection <- new_data[, c(21, 7,17)]

dataforprojection$int_rate <- as.factor(dataforprojection$int_rate)
dataforprojection$loan_status <- as.factor(dataforprojection$loan_status)

text_embeddings_bert <- textEmbed(dataforprojection,
                                  aggregation_from_tokens_to_word_types = "mean",
                                  keep_token_embeddings = FALSE)

text_embeddings_bert

saveRDS(text_embeddings_bert, "text_embeddings_bert.rds")

df_for_plotting <- textProjection(dataforprojection$title, 
                                  text_embeddings_bert$texts$title,
                                  text_embeddings_bert$word_types,
                                  dataforprojection$int_rate, 
                                  dataforprojection$loan_status
)

plot_projection <- textProjectionPlot(
  word_data = df_for_plotting,
  y_axes = TRUE,
  p_alpha = 0.05,
  title_top = "Supervised Bicentroid Projection of Harmony in life words",
  x_axes_label = "Low vs. High HILS score",
  y_axes_label = "Low vs. High SWLS score",
  p_adjust_method = "bonferroni",
  points_without_words_size = 0.4,
  points_without_words_alpha = 0.4
)
plot_projection$final_plot


######################


titlle_embedding <- readRDS("title_embeddings.rds")




projection_results <- textProjection(
  words = new_data$title,
  word_embeddings = text_embeddings_bert$texts$value,
  word_types_embeddings = titlle_embedding$word_types,
  x = new_data$loan_amnt,
  y = new_data$total_amount
)

projection_results

plot_projection <- textProjectionPlot(
  word_data = projection_results,
  y_axes = TRUE,
  title_top = "Supervised Bicentroid Projection of loan in tittle",
  x_axes_label = "Low vs. High HILS score",
  y_axes_label = "Low vs. High SWLS score",
  points_without_words_size = 0.4,
  points_without_words_alpha = 0.4
)
plot_projection$final_plot

word_embeddings_bert <- textEmbed(Language_based_assessment_data_3_100,
                                  aggregation_from_tokens_to_word_types = "mean",
                                  keep_token_embeddings = FALSE)

df_for_plotting <- textProjection(Language_based_assessment_data_3_100$harmonywords, 
                                  word_embeddings_bert$text$harmonywords,
                                  word_embeddings_bert$word_types,
                                  Language_based_assessment_data_3_100$hilstotal, 
                                  Language_based_assessment_data_3_100$swlstotal
)

# Plot the data
plot_projection <- textProjectionPlot(
  word_data = df_for_plotting,
  y_axes = TRUE,
  p_alpha = 0.05,
  title_top = "mine",
  x_axes_label = "Low vs. High HILS score",
  y_axes_label = "Low vs. High SWLS score",
  p_adjust_method = "bonferroni",
  points_without_words_size = 0.4,
  points_without_words_alpha = 0.4
)
plot_projection$final_plot

ex <- Language_based_assessment_data_3_100

plot_projection <- textProjectionPlot(
  word_data = df_for_plotting,
  y_axes = TRUE,
  p_alpha = 0.05,
  title_top = "Supervised Bicentroid Projection of Harmony in life words",
  x_axes_label = "Low vs. High HILS score",
  y_axes_label = "Low vs. High SWLS score",
  p_adjust_method = "bonferroni",
  points_without_words_size = 0.4,
  points_without_words_alpha = 0.4
)
plot_projection$final_plot

options(ggrepel.max.overlaps = 1000)



########################################################################
