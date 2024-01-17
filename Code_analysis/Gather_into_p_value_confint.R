library(tidyverse)
library(readr)
options(scipen=100000)
basepath <- "K:\\Metode\\Challenge\\7_FKC\\age_cohort\\"






load_in <- function(base_path, filename_after_i){
  i_seq = 1:1
  for (i in i_seq){ 
    if(i==1){out <- read_delim(paste0(base_path, filename_after_i), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
    }else{out <- rbind(out, read_delim(paste0(base_path,filename_after_i), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE))}}
    return(out)
}


CD_all <- load_in(base_path = basepath, filename_after_i="CD_all_agg.csv")
CD_edu <- load_in(base_path = basepath, filename_after_i= "CD_edu_agg.csv")
CD_inc <- load_in(base_path = basepath, filename_after_i = "CD_inc_agg.csv")

CD_original_estimates <- read_delim(paste0(basepath,"CD_original_estimates_agg.csv"), 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

NCD_all <- load_in(base_path = basepath, filename_after_i="NCD_all_agg.csv")
NCD_edu <- load_in(base_path = basepath, filename_after_i="NCD_edu_agg.csv")
NCD_inc <- load_in(base_path = basepath, filename_after_i="NCD_inc_agg.csv")

NCD_original_estimates <- read_delim(paste0(basepath,"NCD_original_estimates_agg.csv"), 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

diags_all <- load_in(base_path = basepath, filename_after_i="diags_all_agg.csv")
diags_edu <- load_in(base_path = basepath, filename_after_i="diags_edu_agg.csv")
diags_inc <- load_in(base_path = basepath, filename_after_i="diags_inc_agg.csv")

diags_original_estimates <- read_delim(paste0(basepath,"original_estimates_diags_all.csv"), 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

diags_original_estimates %>% filter(estimator!= "asir") %>%  ggplot(aes(x = estimator, y=value, alpha = 0.1)) + geom_jitter(width = 0.3, height=0)
CD_original_estimates %>% filter(estimator!= "asir") %>%  ggplot(aes(x = estimator, y=value, alpha = 0.1)) + geom_jitter(width = 0.3, height=0)
NCD_original_estimates %>% filter(estimator!= "asir") %>%  ggplot(aes(x = estimator, y=value, alpha = 0.1)) + geom_jitter(width = 0.3, height=0)
firstrow_of_obs <- distinct(CD_all,CD, KOEN,SEP,estimator)

harmonic <- function(m){
  #calculates harmonic number for benjamini yekutieli procedure
  
  s <-0
  for (i in 1:m){
    s <- s+ 1/i
  }
  return(s)
}

normplot <- function(boot_tibble,disease_cat, disease_cat_quoted){
  diags <- as.list(distinct(select(boot_tibble, c(disease_cat_quoted))))[[1]]
  for (est in distinct(select(boot_tibble, c("estimator")))$estimator){
    for (k in distinct(select(boot_tibble, c("KOEN")))$KOEN){
      for(diag_i in (1:length(diags))){

        diag <- diags[[diag_i]]
        for(S in distinct(select(boot_tibble, c("SEP")))$SEP){
          print(est)
          print(diag)
          print(S)
          print(k)
          print(filter(boot_tibble, SEP =={{S}}, {{disease_cat}}=={{diag}}, KOEN=={{k}},estimator=={{est}}))
          data <- filter(boot_tibble, SEP =={{S}}, {{disease_cat}}=={{diag}}, KOEN=={{k}},estimator=={{est}})
          data <- filter(data,est != "asir")$value
          
          
          if (sum(!is.na(data))>0){
            #normed <- rnorm(10000, mean=mean(data), sd = sd(data))
            #qqplot(normed, data)
            qqnorm(data, main = paste(est,"Koen:",k,S,diag,sep = " - ", collapse = "..."))
            Sys.sleep(0.2)}
        }
        
      }
    }
  }
}

quant <- function(grouped_df,q){
  
  if (length(grouped_df) <= 1){return(NA)}

  grouped_df <- filter(tibble(value=grouped_df), !is.na(value))
  
  if (nrow(grouped_df) <= 1){return(NA)}

  grouped_df <- as_vector(grouped_df)

  return(approx(seq(from=0,to=1,length.out =length(grouped_df)),grouped_df,q)$y)
}


out_all <- CD_all  %>% 
  group_by(CD,KOEN,SEP,estimator) %>% arrange(value) %>% filter(CD=="Diarrheal diseases") %>% 
  summarise(sd_confint = sd(value), quantile_025 = quant(value, 0.025),quantile_975  = quant(value, 0.975)) %>% 
  ungroup()

confidence_intervals <- function(original_estimates, boot_all, boot_edu,boot_inc, disease_cat, disease_cat_quoted){
  
out_all <- boot_all  %>% 
  group_by({{disease_cat}},KOEN,SEP,estimator) %>% arrange(value) %>% 
  summarise(sd_confint = sd(value), quantile_025_empi = quant(value, 0.025),quantile_975_empi  = quant(value, 0.975)) %>% 
  ungroup()
out_EDU<- boot_edu  %>% 
                 filter((estimator!="asir") & str_detect(SEP,"udd")) %>%
                 group_by({{disease_cat}}, KOEN,SEP,estimator) %>% 
                 summarise(sd_h0_EDU = sd(value), mean_h0_EDU = mean(value),
                 quantile_H0_025_edu= quant(value, 0.025),
                 quantile_H0_975_edu= quant(value, 0.975)) %>% 
                 ungroup() %>% 
                 mutate(quantile_H0_025_norm_edu = qnorm(mean=mean_h0_EDU, sd=sd_h0_EDU,p=0.025),
                        quantile_H0_975_norm_edu= qnorm(mean=mean_h0_EDU , sd=sd_h0_EDU,p=0.975))

out_INC <- boot_inc %>% 
                 filter((estimator!="asir") & str_detect(SEP,"Q")) %>%
                 group_by({{disease_cat}}, KOEN,SEP,estimator) %>% 
                 summarise(sd_h0_INC = sd(value), mean_h0_inc = mean(value),
                 quantile_H0_025_inc= quant(value, 0.025),
                 quantile_H0_975_inc= quant(value, 0.975))%>% ungroup() %>% 
                 mutate(quantile_H0_025_norm_inc = qnorm(mean=mean_h0_inc, sd=sd_h0_INC,p=0.025),
                        quantile_H0_975_norm_inc= qnorm(mean=mean_h0_inc, sd=sd_h0_INC,p=0.975)
 )
                           
out_join <- left_join(out_all, out_EDU, by=c(disease_cat_quoted,"KOEN","SEP","estimator")) %>% 
  left_join(out_INC, by=c(disease_cat_quoted,"KOEN","SEP","estimator")) %>% 
  left_join(original_estimates %>% rename(original_estimate=value), by=c(disease_cat_quoted,"KOEN","SEP","estimator"))
out_join <- out_join %>% mutate(quantile_025_norm = qnorm(mean=original_estimate, sd=sd_confint,p=0.025),
                    quantile_975_norm = qnorm(mean=original_estimate, sd=sd_confint,p=0.975),
                    pvalue_EDU = (1-pnorm(abs(original_estimate), mean=0,sd=sd_h0_EDU))/2, #two-sided test
                    pvalue_INC = (1-pnorm(abs(original_estimate), mean=0,sd=sd_h0_INC))/2, #two-sided test
                    pvalue_both = ifelse(is.na(pvalue_EDU),pvalue_INC,pvalue_EDU),
                    quantile_H0_025_both_empi = ifelse(is.na(quantile_H0_025_edu),quantile_H0_025_inc,quantile_H0_025_edu),
                    quantile_H0_025_both_norm= ifelse(is.na(quantile_H0_025_norm_edu),quantile_H0_025_norm_inc,quantile_H0_025_norm_edu),
                    quantile_H0_975_both_empi = ifelse(is.na(quantile_H0_975_edu),quantile_H0_025_inc,quantile_H0_025_edu),
                    quantile_H0_975_both_norm= ifelse(is.na(quantile_H0_975_norm_edu),quantile_H0_975_norm_inc,quantile_H0_975_norm_edu),
                    H_0 = ifelse(is.na(pvalue_EDU),"inc","edu"),
                    original_rowid = 1:nrow(out_join)) #generate pvalues, rowid to get back after sorting to get q-values.
out_join <- out_join %>% arrange(pvalue_both) %>% 
  mutate(rank=1:nrow(out_join)) %>% 
  mutate(q_value = 0.05*rank/max(rank, na.rm=TRUE)) %>% #5% false discovery rate
  mutate(q_valueYekutieli = 0.05*rank/(max(rank, na.rm=TRUE)*harmonic(max(rank, na.rm=TRUE)))) %>% #5% false discovery rate
  mutate(pq = pvalue_both*max(rank,na.rm=TRUE)/rank) %>% #test - try to invert into q multiplication into a "p-value"
  mutate(accept = pvalue_both<= q_value) %>% 
  mutate(acceptYekutieli = pvalue_both<= q_valueYekutieli) %>% 
  mutate(FalseCoverageRate_025 = ifelse(accept & !is.na(accept),qnorm(mean=original_estimate, sd=sd_confint,p=(0.025)*sum(accept,na.rm = TRUE)/max(rank,na.rm=TRUE)),NA),
         FalseCoverageRate_975 = ifelse(accept & !is.na(accept),qnorm(mean=original_estimate, sd=sd_confint,p=(0.975)*sum(accept,na.rm = TRUE)/max(rank,na.rm=TRUE)),NA))
#(adjusted pvalue:significance level 0.05 (5%), lower tail, multiply 
#by number of selected (so here accepted) hypothesis, 
#divided by number of tested hypothesis (http://www.math.tau.ac.il/~ybenja/MyPapers/benjamini_yekutieli_JASA2005.pdf)


return(out_join)}




CD_confints <- confidence_intervals(CD_original_estimates,boot_all = CD_all,boot_edu = CD_edu, boot_inc= CD_inc, CD, "CD")
CD_confints_simple <- CD_confints %>% select(c(c(CD,KOEN,SEP,estimator,original_estimate,quantile_025_empi,quantile_975_empi,quantile_025_norm,quantile_975_norm, H_0, pvalue_both, q_valueYekutieli, acceptYekutieli, quantile_H0_025_both_empi,quantile_H0_975_both_empi,quantile_H0_025_both_norm,quantile_H0_975_both_norm)))

NCD_confints <- confidence_intervals(NCD_original_estimates,boot_all = NCD_all,boot_edu = NCD_edu, boot_inc= NCD_inc, NCD, "NCD")
NCD_confints_simple <- NCD_confints %>% select(c(c(NCD,KOEN,SEP,estimator,original_estimate,quantile_025_empi,quantile_975_empi,quantile_025_norm,quantile_975_norm, H_0, pvalue_both, q_valueYekutieli, acceptYekutieli, quantile_H0_025_both_empi,quantile_H0_975_both_empi,quantile_H0_025_both_norm,quantile_H0_975_both_norm)))

diags_confints <- confidence_intervals(diags_original_estimates,boot_all = diags_all,boot_edu = diags_edu, boot_inc= diags_inc, DIAG, "DIAG")
diags_confints_simple <- diags_confints %>% select(c(c(DIAG,KOEN,SEP,estimator,original_estimate,quantile_025_empi,quantile_975_empi,quantile_025_norm,quantile_975_norm, H_0, pvalue_both, q_valueYekutieli, acceptYekutieli, quantile_H0_025_both_empi,quantile_H0_975_both_empi,quantile_H0_025_both_norm,quantile_H0_975_both_norm)))

write_csv2(CD_confints, paste0(basepath,"CD_confints.csv"))
write_csv2(CD_confints_simple, paste0(basepath,"CD_confints_simple.csv"))

write_csv2(NCD_confints, paste0(basepath,"NCD_confints.csv"))
write_csv2(NCD_confints_simple, paste0(basepath,"NCD_confints_simple.csv"))

write_csv2(diags_confints, paste0(basepath,"diags_confints.csv"))
write_csv2(diags_confints_simple, paste0(basepath,"diags_confints_simple.csv"))

