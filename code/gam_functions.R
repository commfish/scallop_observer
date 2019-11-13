# notes ----
# functions used for GAM model diagnostics
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov
# date: 2019-11-11


# function for creating residual plots by explanatory variable
# argument x: gam or bam object created in mcgv

f_resid_plots <- function(x){
  vars <- names(x$model)[-1]
  x$model$Residuals <- resid(x)
  
  plots <- list()
  for(i in 1:length(vars)){
    if(is.factor(x$model[,vars[i]]) == T){
      as_tibble(x$model) %>%
        select(vars[i], Residuals) %>%
        ggplot()+
        geom_boxplot(aes_string(x = vars[i], y = "Residuals"))+
        geom_hline(yintercept = c(-1, 0, 1), linetype = 2, color = "red") -> plots[[i]]
    } 
    if(is.numeric(x$model[,vars[i]]) == T){
      as_tibble(x$model) %>%
        select(vars[i], Residuals) %>%
        ggplot()+
        geom_point(aes_string(x = vars[i], y = "Residuals"))+
        geom_hline(yintercept = c(-1, 0, 1), linetype = 2, color = "red") -> plots[[i]]
    }
  }
  x$model$Residuals <- NULL
  plots
}

# extract estimated degrees of freedom
# argument x: gam or bam object created in mcgv
f_edf <- function(x){sum(x$edf)}

# extract GCV score
# argument x: gam or bam object created in mcgv
f_gcv <- function(x){x$gcv}

# extract model paramaterization
# argument x: gam or bam object created in mcgv
f_gam_form <- function(x){as.character(x$formula)[3]}
