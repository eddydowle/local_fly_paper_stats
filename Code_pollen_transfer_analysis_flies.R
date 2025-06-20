#Eddy Dowle
#2025
#code for fly pollination paper
#code for running SVD analysis (generate boxplot, do kruskal/wallis etc)

############################################
#Analysing SVD for flies across eight crops#
############################################

#packages
library(tidyverse)
library(scales)
library(ggforce)
library(FSA)
library(readxl)

setwd('C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024/')


#########################################
#data wrangling#
#########################################
#skip down to analysis this is just for me to line up the 7 crops with kiwi


#deposition data for 7 crops
deposition_data<-read.csv('Copy of SVD data for Eddy with fields final.csv',header=T)

colnames(deposition_data)
head(deposition_data)
unique(deposition_data$Crop)
unique(deposition_data$Bee.species)
unique(deposition_data$Pollinator.species)

#kiwifruit data comes from melissa's already published dataset available from supplementary here https://pmc.ncbi.nlm.nih.gov/articles/PMC9188772/
deposition_data_kiwi<-read.csv('SVDs-2013-2015-Hayward.csv',header=T)
#columns of interest pollen count = male.pollen.rest.stigmas.est + male.pollen.first.stigma
unique(deposition_data_kiwi$tax)

#dropping non-honey bee bees from kiwi
deposition_data_kiwi_fix<-deposition_data_kiwi %>% dplyr::select(Crop,Site,Order,tax,Male.pollen.first.stigma,Male.pollen.rest.stigmas.est) %>% mutate(Pollen.deposition=Male.pollen.first.stigma+Male.pollen.rest.stigmas.est) %>% dplyr::select(-Male.pollen.first.stigma,-Male.pollen.rest.stigmas.est) %>% filter(tax!='Bombus terrestris') %>%  filter(tax!='Bombus ruderatus') %>% filter(tax!='Leioproctus spp.') %>% filter(tax!='Lasioglossum spp.') %>% filter(tax!='Calliprason pallidus') %>% filter(tax!='') %>% dplyr::select(-Order) %>% drop_na(Pollen.deposition)
unique(deposition_data_kiwi_fix$tax)

#fixing the names so that they match between kiwi and other crops
#need to switch Bibionidae to Dilophus nigrostigma
#exclude "Calliprason pallidus" not represented in the other crops and only 4 records
deposition_data_kiwi_fix$tax   <- gsub("Bibionidae", "Dilophus nigrostigma", deposition_data_kiwi_fix$tax)
unique(deposition_data_kiwi_fix$tax)

deposition_data_kiwi_fix<-deposition_data_kiwi_fix %>% mutate(Bee.species = str_replace(tax, "control", "Control")) %>% dplyr::select(-tax) 
unique(deposition_data_kiwi_fix$Bee.species)
unique(deposition_data_kiwi_fix$Crop)
deposition_data_kiwi_fix$Crop <- gsub("kiwifruit", "Kiwifruit", deposition_data_kiwi_fix$Crop)

colnames(deposition_data_kiwi_fix)
colnames(deposition_data)

#lining up so can bind kiwi on to the end of brads data
#remove kiwi from brads set as it is wongly transformed
names(deposition_data)[names(deposition_data) == 'Pollinator.species'] <- 'Bee.species'
deposition_data<-deposition_data %>% filter(Crop!='Kiwifruit')

colnames(deposition_data)
unique(deposition_data$Crop)
deposition_data_all<-bind_rows(deposition_data,deposition_data_kiwi_fix)

#save it out as merged file
#write.csv(deposition_data_all,'SVD_data_eddy_cleaned_flies_8crops.csv',row.names = F)

############################################
#analysis#
############################################

deposition_data_all<-read.csv('SVD_data_eddy_cleaned_flies_8crops.csv',header=T)

#making boxplots
#relevel to put controls at the end:
#please ignore Bee.species it just means species its just a lazy thing I havent switched over as its what brad calls the column in the datasheets
deposition_data_all$Bee.species<- forcats::fct_relevel(deposition_data_all$Bee.species,"Control", after = Inf)

#plot with brads colour scheme
unique(deposition_data_all$Bee.species)
brads_col_2<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Insect_ordering')
head(brads_col_2)
#brad cant spell so fix spelling for merge
deposition_data_all$Bee.species <- gsub("\\*", "", deposition_data_all$Bee.species)
brads_col_2$Species<- gsub("\\*", "", brads_col_2$Species)
deposition_data_all$Bee.species <- gsub("Apis mellfera", "Apis mellifera", deposition_data_all$Bee.species)
deposition_data_all$Bee.species <- gsub("Helophilus cingulata", "Helophilus cingulatus", deposition_data_all$Bee.species)
deposition_data_all$Bee.species <- gsub("Australophyra rostrata", "Hydrotaea rostrata", deposition_data_all$Bee.species)
#Protohystricia alcis*
unique(deposition_data_all$Bee.species)
deposition_data_all$Bee.species <- gsub("Protohystricia alcis", "Prohystricia alcis", deposition_data_all$Bee.species)

deposition_data_all$Bee.species <- gsub("Odontomyia cloris\\/atrovirens", "Odontomyia spp.", deposition_data_all$Bee.species)

#merge colurs in
colours_figs<-deposition_data_all %>% dplyr::select(Bee.species) %>% unique() %>% left_join(.,brads_col_2,by=c('Bee.species'='Species'))
brads_col<-colours_figs %>% arrange(Bee.species)
brads_col$species<-brads_col$Bee.species
brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)

#brad has a very specific order that he wants the species plottd in so change order up for plotting
unique(deposition_data_all$Bee.species)
deposition_data_all$Bee.species<- forcats::fct_relevel(deposition_data_all$Bee.species,"Control", after = Inf)
deposition_data_all$Bee.species<- forcats::fct_relevel(deposition_data_all$Bee.species,"Apis mellifera","Calliphora stygia","Calliphora vicina","Lucilia sericata","Pollenia pseudorudis","Pollenia spp.","Hydrotaea rostrata","Delia platura","Prohystricia alcis","Oxysarcodexia varia","Eristalis tenax","Helophilus hochstetteri","Helophilus cingulatus","Melangyna novaezelandiae","Melanostoma fasciatum","Allograpta dorsalis","Odontomyia spp.","Dilophus nigrostigma","Zorion guttigerum","Control")

brads_col$species<- forcats::fct_relevel(brads_col$species,"Apis mellifera","Calliphora stygia","Calliphora vicina","Lucilia sericata","Pollenia pseudorudis","Pollenia spp.","Hydrotaea rostrata","Delia platura","Prohystricia alcis","Oxysarcodexia varia","Eristalis tenax","Helophilus hochstetteri","Helophilus cingulatus","Melangyna novaezelandiae","Melanostoma fasciatum","Allograpta dorsalis","Odontomyia spp.","Dilophus nigrostigma","Zorion guttigerum","Control")

brads_col$species <- factor(brads_col$species, levels=brads_col$species[order(brads_col$Code)], ordered=TRUE)
brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)
levels(brads_col$species)
deposition_data_all$Bee.species <- forcats::fct_relevel(deposition_data_all$Bee.species,levels(brads_col$species))
levels(deposition_data_all$Bee.species)

#standard boxplot
ggplot(deposition_data_all,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
  coord_trans(y='log1p')

#plus dots
ggplot(deposition_data_all,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  #scale_color_manual(values=set_names(brads_col$Colour, brads_col$species))+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
  coord_trans(y='log1p')

#brad now wants honey bees in gold
brads_colv2<-brads_col
brads_colv2$Colour<-gsub("gray94", "gold", brads_colv2$Colour)

#plus dots
ggplot(deposition_data_all,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
  scale_fill_manual(values=with(brads_colv2,setNames(Colour,species)))+
  #scale_color_manual(values=set_names(brads_col$Colour, brads_col$species))+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
  coord_trans(y='log1p')

########################################
#kruskal-wallis test#
########################################


#just renaming columsn so I dont have to change my code below from a different paper
deposition_data_all$Pollen_deposition<-deposition_data_all$Pollen.deposition
deposition_data_all$Bee_species<-deposition_data_all$Bee.species

#doing a kruskal-wallis and Dunn test for each group
kruskal_table_out<-NULL
dunn_table_out<-NULL
crops<-unique(deposition_data_all$Crop)
for (item in crops) {
  print(item)
  subset_crop<-deposition_data_all %>% filter(Crop==item)
  #doesnt work for some reason in dunn test, it doesnt take into account levels so just going to rename control with a z to get it go at the end, doesnt really matter but keeps the Z values looking clean etc
  subset_crop$Bee_species <- gsub("Control", "ZControl", subset_crop$Bee_species)
  subset_crop$Bee_species<- forcats::fct_relevel(subset_crop$Bee_species,"ZControl", after = Inf)
  result_kruskal<-kruskal.test(Pollen_deposition ~ Bee_species,data=subset_crop)
  #note dunn.test (dunn.test package R) runs a one sided test dunnTest (FSA) runs a two sided test
  result_dunn<-dunnTest(Pollen_deposition ~ Bee_species,data=subset_crop,method='bonferroni')
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

#dumb reordering of control to get it as the last comparison
dunn_table_out<-dunn_table_out %>% arrange(Comparison)
dunn_table_out$Comparison<-gsub("ZControl", "Control", dunn_table_out$Comparison)

#write out

#write.table(dunn_table_out,'Dunn_analysis_Dec2024_8crops_flies_fixpakchoionion2ndMay.csv',sep=',',quote=F,row.names = F)
#write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_8crops_flies_fixpakchoionion2ndMay.csv',sep=',',quote=F,row.names = F)


#########################################
#testing the impact of site
#########################################

#we dont have enough data to test the impact of site on fly species or non-honey bee species so just looking within honey bees

#dropping all non honey bees
deposition_data_all_honeybee<-deposition_data_all %>% filter(Bee.species=='Apis mellifera')
as.data.frame(sort(unique(deposition_data_all_honeybee$Site)))

#standard boxplot
ggplot(deposition_data_all_honeybee,aes(x=Crop,y=Pollen.deposition,fill=Site))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  #scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
  coord_trans(y='log1p')

#plus dots
ggplot(deposition_data_all_honeybee,aes(x=Crop,y=Pollen.deposition,fill=Site))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
  # scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  #scale_color_manual(values=set_names(brads_col$Colour, brads_col$species))+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
  coord_trans(y='log1p')

###########################
#running KW and Dunn tests#
###########################

#doing a kruskal-wallis and Dunn test for each crop between sites
kruskal_table_out<-NULL
dunn_table_out<-NULL
crops<-unique(deposition_data_all_honeybee$Crop)
#pear only has one location so remove
crops<-crops[! crops %in% 'Pear']
for (item in crops) {
  print(item)
  subset_crop<-deposition_data_all_honeybee %>% filter(Crop==item)
  #doesnt work for some reason in dunn test, it doesnt take into account levels so just going to rename control with a z to get it go at the end, doesnt really matter but keeps the Z values clean etc
  # subset_crop$Bee_species <- gsub("Control", "ZControl", subset_crop$Site)
  #subset_crop$Bee_species<- forcats::fct_relevel(subset_crop$Bee_species,"ZControl", after = Inf)
  #  subset_crop$Bee_species<- forcats::fct_relevel(subset_crop$Bee_species,"Control", after = Inf)
  result_kruskal<-kruskal.test(Pollen_deposition ~ Site,data=subset_crop)
  #note dunn.test (dunn.test package R) runs a one sided test dunnTest (FSA) runs a two sided test
  result_dunn<-dunnTest(Pollen_deposition ~ Site,data=subset_crop,method='bonferroni')
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


#write.table(dunn_table_out,'Dunn_analysis_honeybeesSVD_across_sites.csv',sep=',',quote=F,row.names = F)
#write.table(kruskal_table_out,'Kruskal_analysis_honeybeesSVD_across_sites.csv',sep=',',quote=F,row.names = F)

#radish and kiwi have a significant impact of site
#radish only has two sites and that is the only significant dunn test. None of the kiwi comparisons are significant in the dunn test

#modeling the impact of site on SVD
library(lme4)
library(MASS)
library(lmtest)
library(DHARMa)
library(glmmTMB)

#so site is variable across crops (e.g. not same site in each crops), so use site as a predictor and see if we can drop it

#linear model#
mod<-lm(Pollen.deposition~Crop,data=deposition_data_all_honeybee)
summary(mod)
#site as a predictor
mod2<-lm(Pollen.deposition~Crop+Site,data=deposition_data_all_honeybee)
summary(mod2)
lrtest(mod,mod2)
AIC(mod,mod2)
#so additional predictors don't improve the model so can exclude site
full_mod1  <- glm(Pollen.deposition ~ Crop, data=deposition_data_all_honeybee)
summary(full_mod1)

#glm#
#but the data isnt normal so probably needs to be transformed into poisson or nb
hist(deposition_data_all_honeybee$Pollen.deposition)
#but can only do this on the whole count data (joy)
deposition_data_all_honeybee_wholes<-deposition_data_all_honeybee %>% filter(Crop!='Kiwifruit'&Crop!='Apple'&Crop!='Pear'&Crop!='Carrot')
hist(deposition_data_all_honeybee_wholes$Pollen.deposition)

full_mod1  <- glm(Pollen.deposition ~ Crop, family="poisson", data=deposition_data_all_honeybee_wholes)
summary(full_mod1)
full_mod1_nb  <- glm.nb(Pollen.deposition ~ Crop, data=deposition_data_all_honeybee_wholes)
summary(full_mod1_nb)
lrtest(full_mod1,full_mod1_nb)
#nb better fit
full_mod1_nb_site  <- glm.nb(Pollen.deposition ~ Crop+Site, data=deposition_data_all_honeybee_wholes)
summary(full_mod1_nb_site)
lrtest(full_mod1_nb,full_mod1_nb_site)
#no benefit to adding in the site data in model


#check model
# Simulate residuals 
simResids <- simulateResiduals(full_mod1_nb)
# Generate plots to compare the model residuals to expectations
plot(simResids)

#using glmmTMB to deal with dispersion issues ie. heteroscedastic data
full_modTMB_site<-glmmTMB(Pollen.deposition ~ Crop+Site, data=deposition_data_all_honeybee_wholes,family=nbinom2)
summary(full_modTMB_site)
full_modTMB<-glmmTMB(Pollen.deposition ~ Crop, data=deposition_data_all_honeybee_wholes,family=nbinom2)
summary(full_modTMB)
lrtest(full_modTMB_site,full_modTMB)

#check model
# Simulate residuals 
simResids <- simulateResiduals(full_modTMB)
# Generate plots to compare the model residuals to expectations
plot(simResids)

#better no longer over dispersed but within groups are all over the show still. Ins't SVD data just so fun.
full_modTMB<-glmmTMB(Pollen.deposition ~ Crop,data=deposition_data_all_honeybee_wholes,family=nbinom2)
summary(full_modTMB)
full_modTMB_d<-glmmTMB(Pollen.deposition ~ Crop, dispformula = ~Crop,data=deposition_data_all_honeybee_wholes,family=nbinom2)
summary(full_modTMB_d)
lrtest(full_modTMB,full_modTMB_d)

#check model
# Simulate residuals 
simResids <- simulateResiduals(full_modTMB_d)
# Generate plots to compare the model residuals to expectations
plot(simResids)

#bring the within group deviation down so test with and without sites as predictor
full_modTMB_d<-glmmTMB(Pollen.deposition ~ Crop, dispformula = ~Crop,data=deposition_data_all_honeybee_wholes,family=nbinom2)
summary(full_modTMB_d)
full_modTMB_d_site<-glmmTMB(Pollen.deposition ~ Crop+Site, dispformula = ~Crop,data=deposition_data_all_honeybee_wholes,family=nbinom2)
summary(full_modTMB_d)
lrtest(full_modTMB_d,full_modTMB_d_site)
#no significant difference

#Better but pvalues are possibly meaningless on glmmTMB with disp due to over inflation of type 1 error. But I think we can say that the model is not improved with the addition of site?

###################################################
#using raw count data to model site#
##################################################

#brad has now sent me the raw count data with stigma counts to model this.
#this means i can model site and add stigma count as the offset, meaning I can keep in carrot, pear and apple

#bring in and merging data etc

#fixed version of apple, pear and carrot using raw count, have added a collumn with stigma count
deposition_data_rawpearcarrotapple<-read.csv('Copy of Stigma counts SVD_rawcounts.csv',header=T)

#fitler original ones to ones that are counts for one stigma
deposition_data_all_wholecounts<-deposition_data_all %>% filter(Crop!='Apple'&Crop!='Pear'&Crop!='Carrot'&Crop!='Kiwifruit')
#add in column with stigma count as 1
deposition_data_all_wholecounts$Stigma_count<-1

#remake kiwi data with number of stigmas as a column
#dropping non-honey bee bees from kiwi
deposition_data_kiwi_fix_stigma<-deposition_data_kiwi %>% dplyr::select(Crop,Site,Order,tax,Male.pollen.first.stigma,Male.pollen.rest.stigmas.est,Number.of.stigmas) %>% mutate(Pollen.deposition=Male.pollen.first.stigma+Male.pollen.rest.stigmas.est) %>% dplyr::select(-Male.pollen.first.stigma,-Male.pollen.rest.stigmas.est) %>% filter(tax!='Bombus terrestris') %>%  filter(tax!='Bombus ruderatus') %>% filter(tax!='Leioproctus spp.') %>% filter(tax!='Lasioglossum spp.') %>% filter(tax!='Calliprason pallidus') %>% filter(tax!='') %>% dplyr::select(-Order) %>% drop_na(Pollen.deposition)
unique(deposition_data_kiwi_fix_stigma$tax)
#clean up kiwi data
#exclude "Calliprason pallidus" not represented in the other crops and only 4 records
deposition_data_kiwi_fix_stigma$tax   <- gsub("Bibionidae", "Dilophus nigrostigma", deposition_data_kiwi_fix_stigma$tax)
unique(deposition_data_kiwi_fix_stigma$tax)

deposition_data_kiwi_fix_stigma<-deposition_data_kiwi_fix_stigma %>% mutate(Bee.species = str_replace(tax, "control", "Control")) %>% dplyr::select(-tax) 
unique(deposition_data_kiwi_fix_stigma$Bee.species)
unique(deposition_data_kiwi_fix_stigma$Crop)
deposition_data_kiwi_fix_stigma$Crop <- gsub("kiwifruit", "Kiwifruit", deposition_data_kiwi_fix_stigma$Crop)

colnames(deposition_data_kiwi_fix_stigma)
colnames(deposition_data_rawpearcarrotapple)
colnames(deposition_data_all_wholecounts)

#lining up so can bind kiwi on to the end of brads data
#remove kiwi from brads set as it is wongly transformed
names(deposition_data_all_wholecounts)[names(deposition_data_all_wholecounts) == 'Pollinator.species'] <- 'Bee.species'
names(deposition_data_rawpearcarrotapple)[names(deposition_data_rawpearcarrotapple) == 'Pollinator.species'] <- 'Bee.species'
unique(deposition_data_all_wholecounts$Crop)
names(deposition_data_kiwi_fix_stigma)[names(deposition_data_kiwi_fix_stigma) == 'Number.of.stigmas'] <- 'Stigma_count'

colnames(deposition_data)
unique(deposition_data$Crop)
deposition_data_all_wholecounts_com<-bind_rows(deposition_data_all_wholecounts,deposition_data_kiwi_fix_stigma)
deposition_data_all_wholecounts_com<-bind_rows(deposition_data_all_wholecounts_com,deposition_data_rawpearcarrotapple)

deposition_data_all_wholecounts_com$Bee_species<-NULL
deposition_data_all_wholecounts_com$Pollen_deposition<-NULL

#there is 3 rows with 0 for stigma count, going to remove as not sure what is going on there
deposition_data_all_wholecounts_com <-deposition_data_all_wholecounts_com %>% filter(Stigma_count>0)

###########running models################
#dropping all non honey bees
deposition_data_all_wholecounts_com_honeybee<-deposition_data_all_wholecounts_com %>% filter(Bee.species=='Apis mellifera')
as.data.frame(sort(unique(deposition_data_all_wholecounts_com_honeybee$Site)))


#glm#
hist(deposition_data_all_wholecounts_com_honeybee$Pollen.deposition)

full_mod1  <- glm(Pollen.deposition ~ Crop, data=deposition_data_all_wholecounts_com_honeybee,family = poisson, offset = log(Stigma_count))
summary(full_mod1)
full_mod1_nb  <- glm.nb(Pollen.deposition ~ Crop+log(Stigma_count), data=deposition_data_all_wholecounts_com_honeybee)
summary(full_mod1_nb)
lrtest(full_mod1,full_mod1_nb)
#nb better fit

#the offset is chocking I think on the variables that are one or two observations for a site/crop so removign these three which have <3 records per site/crop
test<-deposition_data_all_wholecounts_com_honeybee %>% filter(Site!='Tallots Road'&Site!='Upton Downs'&Site!='Snodgrass road')

full_mod1_nb  <- glm.nb(Pollen.deposition ~ Crop+log(Stigma_count), data=test)
full_mod1_nb_site  <- glm.nb(Pollen.deposition ~ Crop+Site+log(Stigma_count), data=test)
summary(full_mod1_nb_site)
lrtest(full_mod1_nb,full_mod1_nb_site)
#no benefit to adding in the site data in model
simResids <- simulateResiduals(full_mod1_nb)
# Generate plots to compare the model residuals to expectations
plot(simResids)

#adding dispformula to deal with heteroscedastic data
full_modTMB<-glmmTMB(Pollen.deposition ~ Crop, dispformula = ~Crop,data=test,family=nbinom2,offset = log(Stigma_count))
summary(full_modTMB)
full_modTMB_site<-glmmTMB(Pollen.deposition ~ Crop+Site, dispformula = ~Crop,data=test,family=nbinom2,offset = log(Stigma_count))
summary(full_modTMB_site)
lrtest(full_modTMB,full_modTMB_site)
#no benefit to adding in the site data in model
simResids <- simulateResiduals(full_modTMB)
# Generate plots to compare the model residuals to expectations
plot(simResids)

#even though pvalue isnt useful on tmm models its fairly conclusive that site isnt a effect 

#just having a quick look across all crops in a SVD model with a offset
######################################################################
#raw counts of pollen and using stigma count as a offset in the model#
######################################################################
#model SVD across crops/species using stigma as a offset in the model
#tying poisson
head(deposition_data_all_wholecounts_com)
mod<-glm(Pollen.deposition~Crop, data = deposition_data_all_wholecounts_com, family = poisson, offset = log(Stigma_count))
summary(mod)
mod_nb  <- glm.nb(Pollen.deposition~Crop+log(Stigma_count), data = deposition_data_all_wholecounts_com )
summary(mod_nb)
lrtest(mod,mod_nb)

#with species in
mod<-glm(Pollen.deposition~Crop*Bee.species, data = deposition_data_all_wholecounts_com, family = poisson, offset = log(Stigma_count))
summary(mod)
mod_nb  <- glm.nb(Pollen.deposition~Crop*Bee.species+log(Stigma_count), data = deposition_data_all_wholecounts_com )
summary(mod_nb)
lrtest(mod,mod_nb)

#check model
# Simulate residuals 
simResids <- simulateResiduals(mod_nb)
# Generate plots to compare the model residuals to expectations
plot(simResids)

#adding dispformula to deal with heteroscedastic data
full_modTMB<-glmmTMB(Pollen.deposition ~ Crop, dispformula = ~Crop,data=deposition_data_all_wholecounts_com,family=nbinom2,offset = log(Stigma_count))
summary(full_modTMB)
full_modTMB_spe<-glmmTMB(Pollen.deposition ~ Crop*Bee.species, dispformula = ~Crop,data=deposition_data_all_wholecounts_com,family=nbinom2,offset = log(Stigma_count))
summary(full_modTMB_site)
lrtest(full_modTMB,full_modTMB_spe)

#######mucking about
############modeling from original data and ignoring fraction issue
#what if I ignore the warnings about the fractions
full_mod1  <- glm(Pollen.deposition ~ Crop, family="poisson", data=deposition_data_all_honeybee)
summary(full_mod1)
full_mod1_nb  <- glm.nb(Pollen.deposition ~ Crop, data=deposition_data_all_honeybee)
summary(full_mod1_nb)
lrtest(full_mod1,full_mod1_nb)
#nb
full_mod1_nb_site  <- glm.nb(Pollen.deposition ~ Crop+Site, data=deposition_data_all_honeybee)
summary(full_mod1_nb_site)
lrtest(full_mod1_nb_site,full_mod1_nb)
#no benefit to adding in the site data in model but its a bad model 

#check model
# Simulate residuals 
simResids <- simulateResiduals(full_mod1_nb)
# Generate plots to compare the model residuals to expectations
plot(simResids)

#modeling with TMB
full_modTMB_site<-glmmTMB(Pollen.deposition ~ Crop+Site, data=deposition_data_all_honeybee,family=nbinom2)
summary(full_modTMB_site)
full_modTMB<-glmmTMB(Pollen.deposition ~ Crop, data=deposition_data_all_honeybee,family=nbinom2)
summary(full_modTMB)
lrtest(full_modTMB_site,full_modTMB)

#check model
# Simulate residuals 
simResids <- simulateResiduals(full_modTMB)
# Generate plots to compare the model residuals to expectations
plot(simResids)

#adding dispformula to deal with heteroscedastic data
full_modTMB<-glmmTMB(Pollen.deposition ~ Crop, dispformula = ~Crop,data=deposition_data_all_honeybee,family=nbinom2)
summary(full_modTMB)
full_modTMB_site<-glmmTMB(Pollen.deposition ~ Crop+Site, dispformula = ~Crop,data=deposition_data_all_honeybee,family=nbinom2)
summary(full_modTMB_site)
lrtest(full_modTMB,full_modTMB_site)

#check model
# Simulate residuals 
simResids <- simulateResiduals(full_modTMB)
# Generate plots to compare the model residuals to expectations
plot(simResids)


