# Library
library(ggforestplot)
library(tidyverse)

# Data
load("../Data/5groups_Young_3omcis_meta.RData")

# data 行是样本，列是特征和分组信息
forestplot3 <- function(data=NULL, y.var=NULL, x.var=NULL, co.var=NULL, log.trans=T, omic.name=NULL, p.cutoff=0.05) {
    if(is.null(data)){
      stop("Please imoport the data.")}
    if(is.null(y.var)){
      stop("Please provide the independent variable.")
    }
    if(is.null(x.var)){
      stop("Please provide the dependent variable.")
    }
    if(log.trans){
      y.dat <- log(data[,y.var]+0.001)
    }else{
      y.dat <- data[,y.var]
    }
    data.com <- cbind(data[c(x.var)], y.dat)
    
    lm.list <- list()
    for (i in y.var) {
      lm.list[[i]] <- parameters::parameters(lm(as.formula(paste0(i, " ~ ", x.var)), data = data.com))
    }
    lm.res <- do.call(rbind, lm.list)
    lm.df <- lm.res[grepl(paste0("^", x.var), lm.res$Parameter),]
    lm.df <- data.frame(feature = y.var, lm.df)
    lm.df$p.adjust <- p.adjust(lm.df$p, method = "fdr")
    lm.df$sig <- ifelse(lm.df$p < p.cutoff, "sig", "nosig")
    lm.df$sig <- factor(lm.df$sig, levels = c("nosig", "sig"))
    lm.df <- lm.df[match(y.var, lm.df$feature),]
    
    plot.df=lm.df
    
    for.plot<- ggforestplot::forestplot(
      df = plot.df,
      name = feature,
      estimate = Coefficient,
      se = SE,
      colour = sig
    ) +
      ggplot2::scale_color_manual(values=c("red","blue")) +
      ggplot2::theme(legend.position='none') +
      ggplot2::ggtitle( paste(groups,collapse = "_VS_") )
    
    dir.create("output" )
    for.plot
    ggsave(paste0("output/", omic.name, "_", paste0(groups, collapse = "_vs_"), "_pvaluelessthan_", p.cutoff, "_forestplot.pdf"), width = 5, height = nrow(plot.df)/2,limitsize = FALSE)
}





j="immune"
df <- get(j)
df <- df[complete.cases(df),]
data <- merge(meta.df, rownames_to_column(df, var = "sample_FACS"))
data <- data %>% dplyr::filter(Group=="Young"| Group=="NC") %>% dplyr::select(-Age,-Gender)
groups=c("Young", "NC")
data$Group <- factor(data$Group, levels =groups)
x.var = "Group"
p.cutoff = 0.05
forestplot3(data = data, y.var = colnames(df), x.var = "Group", log.trans = T, omic.name = j, p.cutoff = 0.05)



j="immune"
df <- get(j)
df <- df[complete.cases(df),]
data <- merge(meta.df, rownames_to_column(df, var = "sample_FACS"))
data <- data %>% dplyr::filter(Group=="Young"| Group=="SCD1") %>% dplyr::select(-Age,-Gender)
groups=c("Young", "SCD1")
data$Group <- factor(data$Group, levels =groups)
x.var = "Group"
p.cutoff = 0.05
forestplot3(data = data, y.var = colnames(df), x.var = "Group", log.trans = T, omic.name = j, p.cutoff = 0.05)




