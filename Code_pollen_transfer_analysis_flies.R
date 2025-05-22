#Eddy Dowle
#2025
#code for fly pollination paper

############################################
#Analysing SVD for flies across eight crops#
############################################

#packages
library(tidyverse)
library(scales)
library(ggforce)
library(coin)
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
write.csv(deposition_data_all,'SVD_data_eddy_cleaned_flies_8crops.csv',row.names = F)

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
colours_figs<-deposition_data_all %>% select(Bee.species) %>% unique() %>% left_join(.,brads_col_2,by=c('Bee.species'='Species'))
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






