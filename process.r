library(dplyr)
library(neuralnet)
library(Metrics)

#find correlation between columns
cor_mat <- cor(g_df_m[,4:41],method="pearson")
cor_mat_cnames <- colnames(cor_mat)
col_rm <- c()
for(i in seq(1,37,1)){
  for(j in seq(i,37,1)){
    if(cor_mat[i,j]!=1 && cor_mat[i,j]>0.8){
      col_rm <- c(col_rm,cor_mat_cnames[j])
    }
  }
}
col_rm_u<-unique(col_rm)
col_new <- col_n[!col_n %in% col_rm_u]
g_df_aft_cor <- subset(g_df_m, select=col_new)

#PCA 
pca_mat <- g_df_aft_cor[,4:38]
pca_mat_norm <- g_df_aft_cor[,NULL]
for(x in pca_mat){
  x_norm <- normalize(x,method="standardize",range=c(0,1))
  pca_mat_norm <- cbind(pca_mat_norm,x_norm)
} 
prin_comp = prcomp(pca_mat_norm, scale. = T)
#plot(prin_comp, type = "l")
p_std_dev <- prin_comp$sdev
p_var <- p_std_dev^2
p_var_per <- p_var/sum(p_var)

p_sum <-0
ind<-0
for( p in p_var_per){
  if(p_sum<0.95){
    p_sum <- p_sum+p
    ind <- ind+1
  }
}

#neural-network
nn_data <- prin_comp$x[,1:ind]
nn_data <- cbind(nn_data,g_df_aft_cor$outcome_12hr)
colnames(nn_data)[ind+1] <- "outcome_12hr"

r_cnt <- nrow(nn_data)
n_train_recs <-as.integer(.8*nrow(nn_data))
n_test_recs <-as.integer(r_cnt-n_train_recs)

train_rec <- nn_data[1:n_train_recs,]
next_rec_no <- n_train_recs+1
test_rec <- nn_data[next_rec_no:r_cnt,]

set.seed(45)
NN = neuralnet(outcome_12hr ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29+PC30+PC31, train_rec, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
nn_test_values <- subset(test_rec, select = c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14","PC15","PC16","PC17","PC18","PC19","PC20","PC21","PC22","PC23","PC24","PC25","PC26","PC27","PC28","PC29","PC30","PC31"))
nn.results <- compute(NN, nn_test_values)
t_act <- test_rec[,32]
t_pred <- nn.results$net.result
results <- data.frame(actual = t_act, prediction = t_pred)
e_val <-rmse(t_act,t_pred)
print(e_val)

