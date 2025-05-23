#Eddy Dowle
#2025

#rates of movement and stigma contacted analyses

library(readxl)
library(tidyverse)
library(scales)
library(FSA)
library(readxl)

#bring in and tidy dataset

setwd('C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024/')

#Newest sheet from Brad
crops_together<-read.csv('Copy of Insect behaviour Data site added for Eddy.csv',quote="",row.names = NULL,header=T, check.names = FALSE)

colnames(crops_together)

#give each insect a unique name so can be counted as individuals during summary
#e.g. crop_fullsciename_insectno = individual_id_eddy

crops_together<-crops_together %>% mutate(Individual_id_eddy=paste0(Crop,"-",`Full scientific name`,"_",`Insect no.`))

#just renaming columns for consistency with previous work
names(crops_together)[names(crops_together) == 'stigmas flowers/inflorescence contacted'] <- 'Stigmas/Umblets/Inflorescence contacted'
names(crops_together)[names(crops_together) == 'Time on inflorescence/flower'] <- 'Time on inflorescence/umbel/flower (sec)'
names(crops_together)[names(crops_together) == 'Inflorescence'] <- 'Inflorescence/Flower'

crops_together %>% count(Crop)

#generate a summary table consisting of:
#summary table
#number of individuals 
#ave average duration followed 
#ave count flower unit (refered to as infloresence here just for brevity)
#ave visits successful stigma contact
#ave stigma visited
#ave flower unit visited per minute
#ave % visits involving stigma contact
#ave stigma contacts per flower unit

#note rate is per minute (60 seconds)
#for stigma turn into P/A and then just sum and turn that into proportion
crops_together_summarytable<-  crops_together %>% mutate(Stigmas_contacted_PA=case_when(`Stigmas/Umblets/Inflorescence contacted` > 0~1,`Stigmas/Umblets/Inflorescence contacted` == 0~0)) %>% group_by(Crop,`Full scientific name`,Individual_id_eddy)  %>% summarise(SumTimeFlower=sum(`Time on inflorescence/umbel/flower (sec)`),Countinfloresence=n(),CountVisitsSuccessStigma=sum(Stigmas_contacted_PA),SumStigmavisited=sum(`Stigmas/Umblets/Inflorescence contacted`))%>% mutate(time_diff=60/SumTimeFlower) %>% mutate(inflorescencePerMin=Countinfloresence*time_diff) %>% mutate(prop_visit_touch_stigma=CountVisitsSuccessStigma/Countinfloresence) %>% mutate(ave_num_stigmatouches_per_inflorsense=SumStigmavisited/Countinfloresence)

summary_table<-crops_together_summarytable %>% select(-Individual_id_eddy,-time_diff) %>% group_by(Crop,`Full scientific name`) %>% summarise_all(mean)

summary_table_count<-crops_together_summarytable %>% select(-Individual_id_eddy,-time_diff) %>% group_by(Crop,`Full scientific name`) %>% summarise(count_observations=n())

summary_table<-full_join(summary_table_count,summary_table)

#write out
#write.csv(summary_table,'Summary_table_inflorescence_visits_flies_2ndMay.csv',quote=F,row.names = F)

###############################
#analysing rates of movements##
###############################
#plot with brads colour scheme
#unique(deposition_data_all$Bee.species)
brads_col_2<-read_excel('Copy of Insect behaviour Data final.xlsx',sheet='Insect Colours')
head(brads_col_2)

brads_col_2$`Full scientific name`
unique(crops_together$`Full scientific name`)


colours_figs<-crops_together %>% select(`Full scientific name`) %>% unique() %>% left_join(.,brads_col_2,by=c('Full scientific name'='Full scientific name'))
brads_col<-colours_figs %>% arrange(`Insect ordering for figs`)
brads_col$species<-brads_col$`Full scientific name`

#relevel to brads order
brads_col$species <- factor(brads_col$species, levels=brads_col$species[order(brads_col$`Insect ordering for figs`)], ordered=TRUE)
#brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)
levels(brads_col$species)

#Standardising number of flower visited per 60 seconds (one minute)
crops_together_inflorescenceMin<-crops_together %>% group_by(Crop,`Full scientific name`,Individual_id_eddy) %>% summarise(SumTimeFlower=sum(`Time on inflorescence/umbel/flower (sec)`),Countinfloresence=n()) %>% mutate(time_diff=60/SumTimeFlower) %>% mutate(inflorescencePerMin=Countinfloresence*time_diff)

#relevel to brads order
crops_together_inflorescenceMin$`Full scientific name` <- forcats::fct_relevel(crops_together_inflorescenceMin$`Full scientific name` ,levels(brads_col$species))
levels(crops_together_inflorescenceMin$`Full scientific name` )

#plot Estimated inflorescence/umbel/flower visits per minute by species

crops_together_inflorescenceMin%>% 
  ggplot(aes(x=Crop,y=inflorescencePerMin,fill=`Full scientific name`)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  #scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Estimated inflorescence/umbel/flower visits per minute") + guides(fill=guide_legend(title="Species"))+
  coord_trans(y='log1p')

#plus dots
crops_together_inflorescenceMin%>% 
  ggplot(aes(x=Crop,y=inflorescencePerMin,fill=`Full scientific name`))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  labs(y= "Estimated inflorescence/umbel/flower visits per minute") + guides(fill=guide_legend(title="Species")) + 
  coord_trans(y='log1p')

#non parametric tests to work out significance between species within a crop
#doing a kruskal-wallis and Dunn test for each group
kruskal_table_out<-NULL
dunn_table_out<-NULL
crops<-unique(crops_together_inflorescenceMin$Crop)
for (item in crops) {
  print(item)
  subset_crop<-crops_together_inflorescenceMin %>% filter(Crop==item)
  result_kruskal<-kruskal.test(inflorescencePerMin ~`Full scientific name`,data=subset_crop)
  #note dunn.test (dunn.test package R) runs a one sided test dunnTest (FSA) runs a two sided test
  result_dunn<-dunnTest(inflorescencePerMin ~`Full scientific name`,data=subset_crop,method='bonferroni')
  test_kruskal<-data.frame(c(result_kruskal$statistic,result_kruskal$parameter,pvalue=result_kruskal$p.value))
  colnames(test_kruskal)[1] <- paste0(item,'_KruskalWallis')
  test_kruskal <- tibble::rownames_to_column(test_kruskal, "Stats")
  print(test_kruskal)
  dunn_table<-result_dunn$res
  dunn_table$P.unadj<-NULL
  names(dunn_table)[names(dunn_table) == 'Z'] <- paste0(item,'_Z')
  names(dunn_table)[names(dunn_table) == 'P.adj'] <- paste0(item,'_P.adj')
  print(dunn_table)
  if (is.null(kruskal_table_out)){
    kruskal_table_out<-test_kruskal
  }
  else{
    kruskal_table_out<-full_join(kruskal_table_out,test_kruskal)
  }
  if (is.null(dunn_table_out)){
    dunn_table_out<-dunn_table
  }
  else{
    dunn_table_out<-full_join(dunn_table_out,dunn_table,by='Comparison')
  }
  
}

#write out
#write.table(dunn_table_out,'Dunn_analysis_Dec2024_inflorescenceVisitPerMin_flies_2ndMay.csv',sep=',',quote=F,row.names = F)
#write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_inflorescenceVisitPerMin_flies_2ndMay.csv',sep=',',quote=F,row.names = F)

#############################
#proportion stigma contacted#
#############################


#relevel to brads order
head(crops_together_summarytable)
crops_together_summarytable$`Full scientific name` <- forcats::fct_relevel(crops_together_summarytable$`Full scientific name` ,levels(brads_col$species))
levels(crops_together_summarytable$`Full scientific name` )
#drop avocado, kiwifruit as flower morphology is different so test doesnt make sense
#brad will drop carrot as the data isnt actually for stigma but for umblet 

#boxplot
crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=`Full scientific name`)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Species"))

#boxplot with dots
crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=`Full scientific name`)) + 
  geom_boxplot(outliers = FALSE)+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Species"))

#There is one supper outlier in the onion data. We suspect that this is a typo in the data entry and that its 33 rather than 330 
#removing onion outlier
crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit') %>% filter(Individual_id_eddy!='Onion-Calliphora stygia_81') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=`Full scientific name`)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Species"))

#boxplot with dots
crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit') %>% filter(Individual_id_eddy!='Onion-Calliphora stygia_81') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=`Full scientific name`)) + 
  geom_boxplot(outliers = FALSE)+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Species"))

#testing significance between each species within a crop
#doing a kruskal-wallis and Dunn test for each group
kruskal_table_out<-NULL
dunn_table_out<-NULL
crops_together_summarytable_stigma<-crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit')
crops<-unique(crops_together_summarytable_stigma$Crop)
for (item in crops) {
  print(item)
  subset_crop<-crops_together_summarytable_stigma %>% filter(Crop==item)
  result_kruskal<-kruskal.test(ave_num_stigmatouches_per_inflorsense ~`Full scientific name`,data=subset_crop)
  #note dunn.test (dunn.test package R) runs a one sided test dunnTest (FSA) runs a two sided test
  result_dunn<-dunnTest(ave_num_stigmatouches_per_inflorsense ~`Full scientific name`,data=subset_crop,method='bonferroni')
  test_kruskal<-data.frame(c(result_kruskal$statistic,result_kruskal$parameter,pvalue=result_kruskal$p.value))
  colnames(test_kruskal)[1] <- paste0(item,'_KruskalWallis')
  test_kruskal <- tibble::rownames_to_column(test_kruskal, "Stats")
  print(test_kruskal)
  dunn_table<-result_dunn$res
  dunn_table$P.unadj<-NULL
  names(dunn_table)[names(dunn_table) == 'Z'] <- paste0(item,'_Z')
  names(dunn_table)[names(dunn_table) == 'P.adj'] <- paste0(item,'_P.adj')
  print(dunn_table)
  if (is.null(kruskal_table_out)){
    kruskal_table_out<-test_kruskal
  }
  else{
    kruskal_table_out<-full_join(kruskal_table_out,test_kruskal)
  }
  if (is.null(dunn_table_out)){
    dunn_table_out<-dunn_table
  }
  else{
    dunn_table_out<-full_join(dunn_table_out,dunn_table,by='Comparison')
  }
  
}

#write out
#write.table(dunn_table_out,'Dunn_analysis_Dec2024_averageNumStigTouchPerInflore_flies_may2nd.csv',sep=',',quote=F,row.names = F)
#write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_averageNumStigTouchPerInflore_flies_may2nd.csv',sep=',',quote=F,row.names = F)


########################################
########################################
#rerunning just on honey bees
#then testing within honey bees across sites
########################################
########################################
#understanding the impact of site on the data

#dropping all non honey bees

crops_together_honeybee<-crops_together %>% filter(`Full scientific name`=='Apis mellifera')

#naming issues in sites
crops_together_honeybee$Site   <- gsub("155 Takaka Hill Highway", "154 Takaka Hill Highway", crops_together_honeybee$Site)
crops_together_honeybee$Site   <- gsub("495 Lawn road", "494 Lawn road", crops_together_honeybee$Site)
crops_together_honeybee$Site   <- gsub("Munro orchard", "Munro Orchard", crops_together_honeybee$Site)
crops_together_honeybee$Site   <- gsub("31 Mcbrydie Road ", "31 McBrydie Road", crops_together_honeybee$Site)
crops_together_honeybee$Site   <- gsub("300 Tuapiro Rd- Katikati ", "300 Tuapiro Rd- Katikati", crops_together_honeybee$Site)
crops_together_honeybee$Site   <- gsub("Edgeworth- Bells Road- Ashburton ", "Edgeworth- Bells Road- Ashburton", crops_together_honeybee$Site)
crops_together_honeybee<-crops_together_honeybee %>% filter(Site!='')
crops_together_honeybee$Site   <- gsub("^ ", "", crops_together_honeybee$Site)
crops_together_honeybee$Site   <- gsub(" $", "", crops_together_honeybee$Site)
crops_together_honeybee$Site   <- gsub("  ", "", crops_together_honeybee$Site)
crops_together_honeybee$Site   <- gsub("31 McBrydie Road", "31 McBrydie road", crops_together_honeybee$Site)


as.data.frame(sort(unique(crops_together_honeybee$Site)))

#generating rate
#for stigma turn into P/A and then just sum and turn that into proportion
#using sites rather than species to join by
crops_together_honeybee_summarytable<-  crops_together_honeybee %>% mutate(Stigmas_contacted_PA=case_when(`Stigmas/Umblets/Inflorescence contacted` > 0~1,`Stigmas/Umblets/Inflorescence contacted` == 0~0)) %>% group_by(Crop,Site,Individual_id_eddy)  %>% summarise(SumTimeFlower=sum(`Time on inflorescence/umbel/flower (sec)`),Countinfloresence=n(),CountVisitsSuccessStigma=sum(Stigmas_contacted_PA),SumStigmavisited=sum(`Stigmas/Umblets/Inflorescence contacted`))%>% mutate(time_diff=60/SumTimeFlower) %>% mutate(inflorescencePerMin=Countinfloresence*time_diff) %>% mutate(prop_visit_touch_stigma=CountVisitsSuccessStigma/Countinfloresence) %>% mutate(ave_num_stigmatouches_per_inflorsense=SumStigmavisited/Countinfloresence)

summary_table_honeybee<-crops_together_honeybee_summarytable %>% select(-Individual_id_eddy,-time_diff) %>% group_by(Crop,Site) %>% summarise_all(mean)

summary_table_honeybee_count<-crops_together_honeybee_summarytable %>% select(-Individual_id_eddy,-time_diff) %>% group_by(Crop,Site) %>% summarise(count_observations=n())

summary_table_honeybee<-full_join(summary_table_honeybee_count,summary_table_honeybee)

#write.csv(summary_table_honeybee,'summary_table_honeybee_inflorescence_visits_sites_2ndMay.csv',quote=F,row.names = F)

###############################
#analysing rates of movements honeybee by site##
###############################
#is site a significant predictor of rates of movements in honeybees (only species we have enough data to test)

#Standardising number of flower visited per 60 seconds (one minute)
crops_together_honeybee_inflorescenceMin<-crops_together_honeybee %>% group_by(Crop,Site,Individual_id_eddy) %>% summarise(SumTimeFlower=sum(`Time on inflorescence/umbel/flower (sec)`),Countinfloresence=n()) %>% mutate(time_diff=60/SumTimeFlower) %>% mutate(inflorescencePerMin=Countinfloresence*time_diff)

crops_together_honeybee_inflorescenceMin%>% 
  ggplot(aes(x=Crop,y=inflorescencePerMin,fill=Site)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  #scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  # scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Estimated inflorescence/umbel/flower visits per minute") + guides(fill=guide_legend(title="Site"))+
  coord_trans(y='log1p')

#plus dots
crops_together_honeybee_inflorescenceMin%>% 
  ggplot(aes(x=Crop,y=inflorescencePerMin,fill=Site))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
  # scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  labs(y= "Estimated inflorescence/umbel/flower visits per minute") + guides(fill=guide_legend(title="Site")) + 
  coord_trans(y='log1p')

#doing a kruskal and dunn between sites
kruskal_table_out<-NULL
dunn_table_out<-NULL
crops<-unique(crops_together_honeybee_inflorescenceMin$Crop)
for (item in crops) {
  print(item)
  subset_crop<-crops_together_honeybee_inflorescenceMin %>% filter(Crop==item)
  result_kruskal<-kruskal.test(inflorescencePerMin ~Site,data=subset_crop)
  #note dunn.test (dunn.test package R) runs a one sided test dunnTest (FSA) runs a two sided test
  result_dunn<-dunnTest(inflorescencePerMin ~Site,data=subset_crop,method='bonferroni')
  test_kruskal<-data.frame(c(result_kruskal$statistic,result_kruskal$parameter,pvalue=result_kruskal$p.value))
  colnames(test_kruskal)[1] <- paste0(item,'_KruskalWallis')
  test_kruskal <- tibble::rownames_to_column(test_kruskal, "Stats")
  print(test_kruskal)
  dunn_table<-result_dunn$res
  dunn_table$P.unadj<-NULL
  names(dunn_table)[names(dunn_table) == 'Z'] <- paste0(item,'_Z')
  names(dunn_table)[names(dunn_table) == 'P.adj'] <- paste0(item,'_P.adj')
  print(dunn_table)
  if (is.null(kruskal_table_out)){
    kruskal_table_out<-test_kruskal
  }
  else{
    kruskal_table_out<-full_join(kruskal_table_out,test_kruskal)
  }
  if (is.null(dunn_table_out)){
    dunn_table_out<-dunn_table
  }
  else{
    dunn_table_out<-full_join(dunn_table_out,dunn_table,by='Comparison')
  }
  
}

#write.table(dunn_table_out,'Dunn_analysis_Dec2024_inflorescenceVisitPerMin_honeybees_2ndMay.csv',sep=',',quote=F,row.names = F)
#write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_inflorescenceVisitPerMin_honeybees_2ndMay.csv',sep=',',quote=F,row.names = F)
kruskal_table_out
#kruskals are significant for apple avo carrot pak choi kiwi raddish onion
#dunn test
dunn_table_out %>% filter(Apple_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Apple_P.adj) %>% nrow()
#apple 5/45 sig
dunn_table_out %>% filter(Avocado_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Avocado_P.adj) %>% nrow()
#avo 6/78 sig
dunn_table_out %>% filter(Carrot_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Carrot_P.adj) %>% nrow()
#carrot 4/45 sig
dunn_table_out %>% filter(Kiwifruit_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Kiwifruit_P.adj ) %>% nrow()
#kiwifruit 0/153 sig
dunn_table_out %>% filter(`Pak Choi_P.adj` <0.05) %>% nrow()
dunn_table_out %>% drop_na(`Pak Choi_P.adj`) %>% nrow()
#pak choi 0/120 sig
dunn_table_out %>% filter(Radish_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Radish_P.adj ) %>% nrow()
#radish 0/15 sig
dunn_table_out %>% filter(Onion_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Onion_P.adj ) %>% nrow()
#onion 1/66 sig
dunn_table_out %>% filter(Pear_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Pear_P.adj ) %>% nrow()
#pear 0/10 sig

#running a model to see if can drop site as a predictor

#modeling the impact of site on SVD
library(lme4)
library(MASS)
library(lmtest)
library(DHARMa)
library(glmmTMB)

#go back to the raw data 
head(crops_together_honeybee)
head(summary_table_honeybee)
head(crops_together_honeybee_summarytable)

#raw counts #inflorescence ~crop and offset log(sumtime)
hist(crops_together_honeybee_summarytable$Countinfloresence)
mod<-glm(Countinfloresence ~ Crop, data = crops_together_honeybee_summarytable, family = poisson, offset = log(SumTimeFlower))
summary(mod)
mod_site<-glm(Countinfloresence ~ Crop+Site, data = crops_together_honeybee_summarytable, family = poisson, offset = log(SumTimeFlower))
summary(mod_site)
lrtest(mod,mod_site)
simResids <- simulateResiduals(mod_site)
# Generate plots to compare the model residuals to expectations
plot(simResids)

#site does have a significant impact on behavior in honeybees
mod_site<-glm(Countinfloresence ~ Crop+Site, data = crops_together_honeybee_summarytable, family = poisson, offset = log(SumTimeFlower))
summary(mod_site)
mod_site_nb<-glm.nb(Countinfloresence ~ Crop+Site+offset(log(SumTimeFlower)), data = crops_together_honeybee_summarytable)
summary(mod_site_nb)
lrtest(mod_site,mod_site_nb)
simResids <- simulateResiduals(mod_site_nb)
# Generate plots to compare the model residuals to expectations
plot(simResids)

#having a look on all raw data
head(crops_together_summarytable)
mod<-glm.nb(Countinfloresence ~ Crop*`Full scientific name`+offset(log(SumTimeFlower)), data = crops_together_summarytable)
summary(mod)
simResids <- simulateResiduals(mod)
# Generate plots to compare the model residuals to expectations
plot(simResids)


#############################
#proportion stigma contacted honeybee by site#
#############################

#drop avocado, kiwifruit, 
#brad will drop carrot as the data isnt actually for stigma but for umblet or something (flowers isnt really my jam)

#boxplot
crops_together_honeybee_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit'&Crop!='Carrot') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=Site)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  # scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Site"))


#boxplot with dots
crops_together_honeybee_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit'&Crop!='Carrot') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=Site)) + 
  geom_boxplot(outliers = FALSE)+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
  #  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Site"))


#removing onion outlier
crops_together_honeybee_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit'&Crop!='Carrot') %>% filter(Individual_id_eddy!='Onion-Calliphora stygia_81') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=Site)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  # scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Site"))

#boxplot with dots
crops_together_honeybee_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit'&Crop!='Carrot') %>% filter(Individual_id_eddy!='Onion-Calliphora stygia_81') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=Site)) + 
  geom_boxplot(outliers = FALSE)+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1),colour='black',pch=21,alpha = 0.4)+
  #  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Site"))


#doing a kruskal-wallis and Dunn test for each group
kruskal_table_out<-NULL
dunn_table_out<-NULL
crops_together_honeybee_summarytable_stigma<-crops_together_honeybee_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit'&Crop!='Carrot')
crops<-unique(crops_together_honeybee_summarytable_stigma$Crop)
for (item in crops) {
  print(item)
  subset_crop<-crops_together_honeybee_summarytable_stigma %>% filter(Crop==item)
  result_kruskal<-kruskal.test(ave_num_stigmatouches_per_inflorsense ~Site,data=subset_crop)
  #note dunn.test (dunn.test package R) runs a one sided test dunnTest (FSA) runs a two sided test
  result_dunn<-dunnTest(ave_num_stigmatouches_per_inflorsense ~Site,data=subset_crop,method='bonferroni')
  test_kruskal<-data.frame(c(result_kruskal$statistic,result_kruskal$parameter,pvalue=result_kruskal$p.value))
  colnames(test_kruskal)[1] <- paste0(item,'_KruskalWallis')
  test_kruskal <- tibble::rownames_to_column(test_kruskal, "Stats")
  print(test_kruskal)
  dunn_table<-result_dunn$res
  dunn_table$P.unadj<-NULL
  names(dunn_table)[names(dunn_table) == 'Z'] <- paste0(item,'_Z')
  names(dunn_table)[names(dunn_table) == 'P.adj'] <- paste0(item,'_P.adj')
  print(dunn_table)
  if (is.null(kruskal_table_out)){
    kruskal_table_out<-test_kruskal
  }
  else{
    kruskal_table_out<-full_join(kruskal_table_out,test_kruskal)
  }
  if (is.null(dunn_table_out)){
    dunn_table_out<-dunn_table
  }
  else{
    dunn_table_out<-full_join(dunn_table_out,dunn_table,by='Comparison')
  }
  
}

#write.table(dunn_table_out,'Dunn_analysis_averageNumStigTouchPerInflore_honeybee_may2nd.csv',sep=',',quote=F,row.names = F)
#write.table(kruskal_table_out,'Kruskal_analysis_averageNumStigTouchPerInflore_honeybee_may2nd.csv',sep=',',quote=F,row.names = F)
kruskal_table_out
#kruskals are significant for apple pak choi raddish onion
#dunn test
dunn_table_out %>% filter(Apple_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Apple_P.adj) %>% nrow()
#apple 2/45 sig
dunn_table_out %>% filter(`Pak Choi_P.adj` <0.05) %>% nrow()
dunn_table_out %>% drop_na(`Pak Choi_P.adj`) %>% nrow()
#pak choi 1/120 sig
dunn_table_out %>% filter(Radish_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Radish_P.adj ) %>% nrow()
#radish 2/15 sig
dunn_table_out %>% filter(Onion_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Onion_P.adj ) %>% nrow()
#onion 0/66 sig
dunn_table_out %>% filter(Pear_P.adj <0.05) %>% nrow()
dunn_table_out %>% drop_na(Pear_P.adj ) %>% nrow()
#pear 0/10 sig

#model

colnames(crops_together_honeybee_summarytable)
head(crops_together_honeybee_summarytable)
head(crops_together_honeybee)

#raw counts sucessful stigma count ~crop and offset log(Countinfloresence)
hist(crops_together_honeybee_summarytable$CountVisitsSuccessStigma)

#tying poisson
mod<-glm(CountVisitsSuccessStigma~Crop, data = crops_together_honeybee_summarytable, family = poisson, offset = log(Countinfloresence))
summary(mod)
mod_site<-glm(CountVisitsSuccessStigma~Crop+Site, data = crops_together_honeybee_summarytable, family = poisson, offset = log(Countinfloresence))
summary(mod_site)
lrtest(mod,mod_site)
simResids <- simulateResiduals(mod_site)
# Generate plots to compare the model residuals to expectations
plot(simResids)
#site has significant impact on succesful stigma touch in honeybees

mod_site<-glm(CountVisitsSuccessStigma~Crop, data = crops_together_honeybee_summarytable, family = poisson, offset = log(Countinfloresence))
summary(mod_site)
mod_site_nb<-glm.nb(CountVisitsSuccessStigma~Crop+Site+offset(log(Countinfloresence)), data = crops_together_honeybee_summarytable)
summary(mod_site_nb)

#site does have a significant impact on successful stigma touch in honeybees

#having a look on all raw data
head(crops_together_summarytable)
mod<-lm(CountVisitsSuccessStigma~Crop*`Full scientific name`+offset(log(Countinfloresence)), data = crops_together_summarytable)
summary(mod)
simResids <- simulateResiduals(mod)
# Generate plots to compare the model residuals to expectations
plot(simResids)

