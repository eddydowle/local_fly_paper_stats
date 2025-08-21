library(tidyverse)
library(lme4)
library(readxl)
library(svglite)
library(vegan)



######################
#diversity statistics#
######################

#basic diversity stats + MDS
#means I need to deal with the naming issues on species ID

setwd("C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024")

apple<-read_excel('Daily insect counts all crops for Eddy_mod.xlsx',sheet='Apple')
pear<-read_excel('Daily insect counts all crops for Eddy_mod.xlsx',sheet='Pear')
avocado<-read_excel('Daily insect counts all crops for Eddy_mod.xlsx',sheet='Avocado')
kiwifruit<-read_excel('Daily insect counts all crops for Eddy_mod.xlsx',sheet='Kiwifruit')
pakchoi<-read_excel('Daily insect counts all crops for Eddy_mod.xlsx',sheet='Pak choi')
radish<-read_excel('Daily insect counts all crops for Eddy_mod.xlsx',sheet='Radish')
onion<-read_excel('Daily insect counts all crops for Eddy_mod.xlsx',sheet='Onion')
carrot<-read_excel('Daily insect counts all crops for Eddy_mod.xlsx',sheet='Carrot')

#apple contains apple and pear
unique(apple$Crop)
unique(pear$Crop)
unique(avocado$Crop)
unique(kiwifruit$Crop)
unique(pakchoi$Crop)
unique(radish$Crop)
unique(onion$Crop)
unique(carrot$Crop)

#check region
unique(apple$Region)
unique(pear$Region)
unique(avocado$Region)
unique(kiwifruit$Region)
unique(pakchoi$Region)
unique(radish$Region)
unique(onion$Region)
unique(carrot$Region)

#check location and season
unique(apple$`Location and season`)
unique(pear$`Location and season`)
unique(avocado$`Location and season`)
unique(kiwifruit$`Location and season`)
unique(pakchoi$`Location and season`)
unique(radish$`Location and season`)
unique(onion$`Location and season`)
unique(carrot$`Location and season`)

#brad wants them grouped on the column labeled 'Ordering'
unique(apple$Ordering)
unique(pear$Ordering)
unique(avocado$Ordering)
unique(kiwifruit$Ordering)
unique(pakchoi$Ordering)
unique(radish$Ordering)
unique(onion$Ordering)
unique(carrot$Ordering)

#brad wants them grouped on the column labeled 'Ordering'
#scientific name is wacky so stick to the ordering column as the grouping variable
#remove “Ephydridae spp.” from Pak choi and onion data
#as per brad is wasnt counted in the other crops so needs to be removed
onion<-onion %>% filter(`Scientific name`!='Ephydridae spp.')
pakchoi<-pakchoi %>% filter(`Scientific name`!='Ephydridae spp.')



brads_col_2<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Insect_ordering')
head(brads_col_2)


################
#data wrangling#
################

#sorting out daily counts as diversity statistics will be based on daily counts:
###apple
#day 1, 2, 3
apple_day1<-apple %>% select(-`Day 2`,-`Day 3`) %>% mutate(Day='Day_1') %>% filter(`Day 1`>0) %>% rename(raw_count = `Day 1`)
apple_day2<-apple %>% select(-`Day 1`,-`Day 3`) %>% mutate(Day='Day_2') %>% filter(`Day 2`>0) %>% rename(raw_count = `Day 2`)
apple_day3<-apple %>% select(-`Day 1`,-`Day 2`) %>% mutate(Day='Day_3') %>% filter(`Day 3`>0) %>% rename(raw_count = `Day 3`)

apple_day<-rbind(apple_day1,apple_day2,apple_day3)  

#just make sure everything is tidy tidy 
apple_fly<-apple_day%>% filter(Grouping!='Bees')%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Apple_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Ordering,Region,`Location and season`,Day,raw_count,Crop) %>% group_by(sample_ID,Ordering,Region,`Location and season`,Day,Crop) %>% summarise(raw_count=sum(raw_count)) %>% ungroup()

#some replicates have only 0 counts for non bees and are being dropped  so need to be added back in
samples<-apple_day%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Apple_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Region,`Location and season`,Day,Crop) %>% unique()

apple_fly<-full_join(apple_fly,samples)

###pear
#day 1, 2, 3
pear_day1<-pear %>% select(-`Day 2`,-`Day 3`) %>% mutate(Day='Day_1') %>% filter(`Day 1`>0) %>% rename(raw_count = `Day 1`)
pear_day2<-pear %>% select(-`Day 1`,-`Day 3`) %>% mutate(Day='Day_2') %>% filter(`Day 2`>0) %>% rename(raw_count = `Day 2`)
pear_day3<-pear %>% select(-`Day 1`,-`Day 2`) %>% mutate(Day='Day_3') %>% filter(`Day 3`>0) %>% rename(raw_count = `Day 3`)

pear_day<-rbind(pear_day1,pear_day2,pear_day3) 

#just make sure everything is tidy tidy 
pear_fly<-pear_day%>% filter(Grouping!='Bees')%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Pear_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Ordering,Region,`Location and season`,Day,raw_count,Crop) %>% group_by(sample_ID,Ordering,Region,`Location and season`,Day,Crop) %>% summarise(raw_count=sum(raw_count)) %>% ungroup()

#some replicates have only 0 counts for non bees and are being dropped  so need to be added back in
samples<-pear_day%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Pear_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Region,`Location and season`,Day,Crop) %>% unique()

pear_fly<-full_join(pear_fly,samples)


###avocado
#day 1, 2, 3
avocado_day1<-avocado %>% select(-`Day 2`,-`Day 3`) %>% mutate(Day='Day_1') %>% filter(`Day 1`>0) %>% rename(raw_count = `Day 1`)
avocado_day2<-avocado %>% select(-`Day 1`,-`Day 3`) %>% mutate(Day='Day_2') %>% filter(`Day 2`>0) %>% rename(raw_count = `Day 2`)
avocado_day3<-avocado %>% select(-`Day 1`,-`Day 2`) %>% mutate(Day='Day_3') %>% filter(`Day 3`>0) %>% rename(raw_count = `Day 3`)

avocado_day<-rbind(avocado_day1,avocado_day2,avocado_day3) 
#just make sure everything is tidy tidy 
avocado_fly<-avocado_day%>% filter(Grouping!='Bees')%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Avocado_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Ordering,Region,`Location and season`,Day,raw_count,Crop) %>% group_by(sample_ID,Ordering,Region,`Location and season`,Day,Crop) %>% summarise(raw_count=sum(raw_count)) %>% ungroup()

#some replicates have only 0 counts for non bees and are being dropped  so need to be added back in
samples<-avocado_day%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Avocado_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Region,`Location and season`,Day,Crop) %>% unique()

avocado_fly<-full_join(avocado_fly,samples)

###kiwifruit
#day 1, 2, 3
kiwifruit_day1<-kiwifruit %>% select(-`Day 2`,-`Day 3`) %>% mutate(Day='Day_1') %>% filter(`Day 1`>0) %>% rename(raw_count = `Day 1`)
kiwifruit_day2<-kiwifruit %>% select(-`Day 1`,-`Day 3`) %>% mutate(Day='Day_2') %>% filter(`Day 2`>0) %>% rename(raw_count = `Day 2`)
kiwifruit_day3<-kiwifruit %>% select(-`Day 1`,-`Day 2`) %>% mutate(Day='Day_3') %>% filter(`Day 3`>0) %>% rename(raw_count = `Day 3`)

kiwifruit_day<-rbind(kiwifruit_day1,kiwifruit_day2,kiwifruit_day3) 

#just make sure everything is tidy tidy 
kiwifruit_fly<-kiwifruit_day%>% filter(Grouping!='Bees')%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Kiwifruit_',Region,'_',`Location and season`,'_',Day)) %>% select(Crop,sample_ID,Ordering,Region,`Location and season`,Day,raw_count) %>% group_by(sample_ID,Ordering,Region,`Location and season`,Day,Crop) %>% summarise(raw_count=sum(raw_count)) %>% ungroup()

#some replicates have only 0 counts for non bees and are being dropped  so need to be added back in
samples<-kiwifruit_day%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Kiwifruit_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Region,`Location and season`,Day,Crop) %>% unique()

kiwifruit_fly<-full_join(kiwifruit_fly,samples)

###pakchoi
#day 1
pakchoi_day1<-pakchoi %>% mutate(Day='Day_1') %>% rename(raw_count = `Corrected Daily Count`)

pakchoi_day<-pakchoi_day1 

#just make sure everything is tidy tidy 
pakchoi_fly<-pakchoi_day%>% filter(Grouping!='Bees')%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Pakchoi_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Ordering,Region,`Location and season`,Day,raw_count,Crop) %>% group_by(sample_ID,Ordering,Region,`Location and season`,Day,Crop) %>% summarise(raw_count=sum(raw_count)) %>% ungroup()

#some replicates have only 0 counts for non bees and are being dropped  so need to be added back in
samples<-pakchoi_day%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Pakchoi_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Region,`Location and season`,Day,Crop) %>% unique()

pakchoi_fly<-full_join(pakchoi_fly,samples)


###radish
#day 1
radish_day1<-radish %>% mutate(Day='Day_1') %>% rename(raw_count = `Corrected Daily Count`)

radish_day<-radish_day1 

#just make sure everything is tidy tidy 
radish_fly<-radish_day%>% filter(Grouping!='Bees')%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Radish_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Ordering,Region,`Location and season`,Day,raw_count,Crop) %>% group_by(sample_ID,Ordering,Region,`Location and season`,Day,Crop) %>% summarise(raw_count=sum(raw_count)) %>% ungroup()

#some replicates have only 0 counts for non bees and are being dropped  so need to be added back in
samples<-radish_day%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Radish_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Region,`Location and season`,Day,Crop) %>% unique()

radish_fly<-full_join(radish_fly,samples)

###onion
#day 1
onion_day1<-onion %>% mutate(Day='Day_1') %>% rename(raw_count = `Corrected Daily Count`)

onion_day<-onion_day1 

#just make sure everything is tidy tidy 
onion_fly<-onion_day%>% filter(Grouping!='Bees')%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Onion_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Ordering,Region,`Location and season`,Day,raw_count,Crop) %>% group_by(sample_ID,Ordering,Region,`Location and season`,Day,Crop) %>% summarise(raw_count=sum(raw_count)) %>% ungroup()

#some replicates have only 0 counts for non bees and are being dropped  so need to be added back in
samples<-onion_day%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Onion_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Region,`Location and season`,Day,Crop) %>% unique()

onion_fly<-full_join(onion_fly,samples)

###carrot
#day 1 2 3
carrot_day1<-carrot %>% select(-`Day 2`,-`Day 3`) %>% mutate(Day='Day_1') %>% filter(`Day 1`>0) %>% rename(raw_count = `Day 1`)
carrot_day2<-carrot %>% select(-`Day 1`,-`Day 3`) %>% mutate(Day='Day_2') %>% filter(`Day 2`>0) %>% rename(raw_count = `Day 2`)
carrot_day3<-carrot %>% select(-`Day 1`,-`Day 2`) %>% mutate(Day='Day_3') %>% filter(`Day 3`>0) %>% rename(raw_count = `Day 3`)

carrot_day<-rbind(carrot_day1,carrot_day2,carrot_day3) 
#just make sure everything is tidy tidy 
carrot_fly<-carrot_day%>% filter(Grouping!='Bees')%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Carrot_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Ordering,Region,`Location and season`,Day,raw_count,Crop) %>% group_by(sample_ID,Ordering,Region,`Location and season`,Day,Crop) %>% summarise(raw_count=sum(raw_count)) %>% ungroup()

#some replicates have only 0 counts for non bees and are being dropped  so need to be added back in
samples<-carrot_day%>% mutate(Ordering = factor(Ordering)) %>% mutate(sample_ID = paste0('Carrot_',Region,'_',`Location and season`,'_',Day)) %>% select(sample_ID,Region,`Location and season`,Day,Crop) %>% unique()

carrot_fly<-full_join(carrot_fly,samples)


##############################
#generating tables and counts#
##############################

allcrops_per_r_fly<-rbind(apple_fly,pear_fly,avocado_fly,kiwifruit_fly,pakchoi_fly,radish_fly,onion_fly,carrot_fly)  

unique(allcrops_per_r_fly$Crop)

counts<-allcrops_per_r_fly %>% ungroup() %>% select(sample_ID,Ordering,raw_count)

#I need to fill up the NA's for the samples with 0 flies with some dummy 0 for ones species to keep them in the next stage - so this is just to add a 0 value in and I've chosed the insect '27' at random (cause i know it occurs in the dataset so Im not adding something not present)

#in '' as its a factor not a number
counts$Ordering[is.na(counts$Ordering)] <- '27'
counts$raw_count[is.na(counts$raw_count)] <- 0
#transform wide
counts_wide<-counts %>% pivot_wider(names_from=`Ordering`,values_from = `raw_count`)
counts_wide[is.na(counts_wide)] <- 0
counts_wide<-counts_wide %>% remove_rownames %>% column_to_rownames(var="sample_ID")

#count species per property
sppr <- specnumber(counts_wide)
head(sppr)
sppr
#zeros are in there now, just because of the merge above they are out of order

#anova against variables, species richness vs crops
meta_data<-allcrops_per_r_fly %>% ungroup() %>% select(sample_ID,Crop,Region,`Location and season`,Day) %>% unique()

#adding in: spring vs summer flowering to metadata table
head(meta_data)
meta_data<-meta_data %>% mutate(flowertime = case_when(Crop == 'Apple' ~ "Spring",Crop == 'Pear' ~ "Spring",Crop == 'Avocado' ~ "Spring",Crop == 'Kiwifruit' ~ "Spring",Crop == 'Carrot' ~ "Summer",Crop == 'Radish' ~ "Summer",Crop == 'Onion' ~ "Summer",Crop == 'Pak Choi' ~ "Summer"))

####################################################
#impact of crop, region and flowering time
###################################################

#crop
sppr_aov <-aov(sppr ~ Crop, data = meta_data)
summary(sppr_aov)
TukeyHSD(sppr_aov)

#impact of crop and region
sppr_aov <-aov(sppr ~ Crop+Region, data = meta_data)
summary(sppr_aov)
TukeyHSD(sppr_aov)

#impact of flowering time
sppr_aov <-aov(sppr ~ flowertime, data = meta_data)
summary(sppr_aov)
TukeyHSD(sppr_aov)

#impact of flowering time and crop
sppr_aov <-aov(sppr ~ flowertime+Crop, data = meta_data)
summary(sppr_aov)
TukeyHSD(sppr_aov)

#can plot that out
sppr_df <- sppr %>% 
  enframe() %>% 
  full_join(meta_data, by = c("name" = "sample_ID"))

#picking colour scheme
#devtools::install_github("Nowosad/rcartocolor")
library(rcartocolor)
#?carto_pal
col_bound<-rcartocolor::carto_pal(n=9,name='Prism')
#col_bound<-rcartocolor::carto_pal(n=9,name='Safe')
#col_bound<-rcartocolor::carto_pal(n=9,name='Vivid')
#col_bound<-rcartocolor::carto_pal(n=9,name='Bold')
#col_bound<-rcartocolor::carto_pal(n=9,name='Antique')

#personally prefer prism

#just crop
plot_sppr <- ggplot(sppr_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  scale_fill_manual(values=col_bound)+
  theme_bw()+
  labs(x = "Crop",
       y = "Number of species per sample",
       title = "Species richness")
plot_sppr

#other significant factor was region
plot_sppr <- ggplot(sppr_df, aes(x = Region, y = value, fill = Crop)) +
  geom_boxplot() +
  facet_wrap(~Crop, scales = "free_x")+
  theme_bw()+
  scale_fill_manual(values=col_bound)+
  labs(x = "Region",
       y = "Number of species per sample",
       title = "Species richness")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr

#other significant factor was flowertime
plot_sppr <- ggplot(sppr_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
 # facet_wrap(~flowertime, scales="free")+
  facet_wrap(~flowertime, scales = "free_x")+
#  facet_grid(.~flowertime,drop = TRUE) +
  theme_bw()+
  scale_fill_manual(values=col_bound)+
  labs(x = "Region",
       y = "Number of species per sample",
       title = "Species richness")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr

#########
#simpson#
#########

#using simpson which is less sensitive to rare species which should be more robust to the sampling biases in the data
simpsondiv<-diversity(counts_wide,index='simpson')
head(simpsondiv)
####################################################
#impact of crop, region and flowering time
###################################################

#crop
simpsondiv_aov <-aov(simpsondiv ~ Crop, data = meta_data)
summary(simpsondiv_aov)
TukeyHSD(simpsondiv_aov)

#impact of crop and region
simpsondiv_aov <-aov(simpsondiv ~ Crop+Region, data = meta_data)
summary(simpsondiv_aov)
#region not significant in simpson (this lines up with when we only use rep 1 (which is good))

#impact of flowering time
simpsondiv_aov <-aov(simpsondiv ~ flowertime, data = meta_data)
summary(simpsondiv_aov)
TukeyHSD(simpsondiv_aov)

#impact of flowering time and crop
simpsondiv_aov <-aov(simpsondiv ~ flowertime+Crop, data = meta_data)
summary(simpsondiv_aov)
TukeyHSD(simpsondiv_aov)

#can plot that out
simpsondiv_df <- simpsondiv %>% 
  enframe() %>% 
  full_join(meta_data, by = c("name" = "sample_ID"))

#just crop
plot_sppr <- ggplot(simpsondiv_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  scale_fill_manual(values=col_bound)+
  theme_bw()+
  labs(x = "Crop",
       y = "Simpson",
       title = "Species richness: simpson")
plot_sppr

#other significant factor was flowertime
plot_sppr <- ggplot(simpsondiv_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  facet_wrap(~flowertime, scales = "free_x")+
  theme_bw()+
  scale_fill_manual(values=col_bound)+
  labs(x = "Region",
       y = "Simpson",
       title = "Species richness: simpson")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr

#########
#shannon#
#########

#hmmmm I actually dont like that, when say apple has a bunch of 1's its getting a very high score as its perfectly even

#having a look at shannon
shannondiv<-diversity(counts_wide)
head(shannondiv)
#crop
shannondiv_aov <-aov(shannondiv ~ Crop, data = meta_data)
summary(shannondiv_aov)
TukeyHSD(shannondiv_aov)

#impact of crop and region
shannondiv_aov <-aov(shannondiv ~ Crop+Region, data = meta_data)
summary(shannondiv_aov)
TukeyHSD(shannondiv_aov)
#region significant in shannon

#impact of flowering time
shannondiv_aov <-aov(shannondiv ~ flowertime, data = meta_data)
summary(shannondiv_aov)
TukeyHSD(shannondiv_aov)

#impact of flowering time and crop
shannondiv_aov <-aov(shannondiv ~ flowertime+Crop, data = meta_data)
summary(shannondiv_aov)
TukeyHSD(shannondiv_aov)

#can plot that out
shannondiv_df <- shannondiv %>% 
  enframe() %>% 
  full_join(meta_data, by = c("name" = "sample_ID"))

#just crop
plot_sppr <- ggplot(shannondiv_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  scale_fill_manual(values=col_bound)+
  theme_bw()+
  labs(x = "Crop",
       y = "Shannon",
       title = "Species richness: shannon")
plot_sppr


#other significant factor was region
plot_sppr <- ggplot(shannondiv_df, aes(x = Region, y = value, fill = Crop)) +
  geom_boxplot() +
  facet_wrap(~Crop, scales = "free_x")+
  theme_bw()+
  scale_fill_manual(values=col_bound)+
  labs(x = "Region",
       y = "Shannon",
       title = "Species richness: shannon")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr


#other significant factor was flowertime
plot_sppr <- ggplot(shannondiv_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  facet_wrap(~flowertime, scales = "free_x")+
  theme_bw()+
  scale_fill_manual(values=col_bound)+
  labs(x = "Region",
       y = "Shannon",
       title = "Species richness: shannon")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr

################
#beta diversity#
################
#im not sure that this is useful as I have to remove the 0's (e.g. samples where there was no non-bee insects observed ~mostly apple/pear) but here it is:
#remove zero values:
counts_wide_no0 <- counts_wide[rowSums(counts_wide == 0) != ncol(counts_wide), ]

meta_data_no0<-meta_data %>% filter(sample_ID %in% row.names(counts_wide_no0))

#crop
perm<-adonis2(counts_wide_no0 ~ Crop,data=meta_data_no0)
perm
#flowertime
perm<-adonis2(counts_wide_no0 ~ flowertime,data=meta_data_no0)
perm
#region
perm<-adonis2(counts_wide_no0 ~ Region,data=meta_data_no0)
perm
#crop + flowertime
perm<-adonis2(counts_wide_no0 ~ Crop+flowertime,data=meta_data_no0)
perm
#crop+region
perm<-adonis2(counts_wide_no0 ~ Crop+Region,data=meta_data_no0)
perm
#crop+flowertime+region
perm<-adonis2(counts_wide_no0 ~ Crop+flowertime+Region,data=meta_data_no0)
perm


#MDS
pk_NMDS <- metaMDS(counts_wide_no0,trymax=200)
#doesnt converge - whats up?
pk_NMDS
#stress (ideally <0.2)

stressplot(pk_NMDS)

plot(pk_NMDS)

plot_df <- scores(pk_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(meta_data_no0, by = c("site" = "sample_ID"))

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Crop)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, linewidth = 0.5) +
  theme_bw()+
  labs(title = "NMDS")
plot_nmds

#################################################################
#setting a minimum count of 5 across at least 2 taxa#
#################################################################
#this is a little arbitary but means the MDS plot isnt trying to converge stuff with basically no data
#remove <5 counts:
counts_wide_no0 <- counts_wide[rowSums(counts_wide) >= 5, ]
#remove things that are only from a single species
counts_wide_no0 <-counts_wide_no0[which(rowSums(1*(counts_wide_no0!=0))>1),]

meta_data_no0<-meta_data %>% filter(sample_ID %in% row.names(counts_wide_no0))

#reading the beta as well
#crop
perm<-adonis2(counts_wide_no0 ~ Crop,data=meta_data_no0)
perm
#flowertime
perm<-adonis2(counts_wide_no0 ~ flowertime,data=meta_data_no0)
perm
#region
perm<-adonis2(counts_wide_no0 ~ Region,data=meta_data_no0)
perm
#crop + flowertime
perm<-adonis2(counts_wide_no0 ~ Crop+flowertime,data=meta_data_no0)
perm
#crop+region
perm<-adonis2(counts_wide_no0 ~ Crop+Region,data=meta_data_no0)
perm
#crop+flowertime+region
perm<-adonis2(counts_wide_no0 ~ Crop+flowertime+Region,data=meta_data_no0)
perm


#MDS
pk_NMDS <- metaMDS(counts_wide_no0,trymax=200)
#converges - yay - a little stressed but is a lot of data
pk_NMDS
#stress 
stressplot(pk_NMDS)

plot(pk_NMDS)

plot_df <- scores(pk_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(meta_data_no0, by = c("site" = "sample_ID"))

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Crop)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, linewidth = 0.5) +
  theme_bw()+
  scale_colour_manual(values=col_bound)+
  labs(title = "NMDS")
plot_nmds

########
#innext#
########
library(iNEXT.3D)

#generate a list of the unique sample locations
categories <- unique(meta_data$Crop)
categories

#create empty list
split_list <- list()
#fill empty list with a presence absence OTU from each location

head(allcrops_per_r_fly)

#I need to fill up the NA's for the samples with 0 flies with some dummy 0 for ones species to keep them in the next stage - so this is just to add a 0 value in and I've chosed the insect '27' at random (cause i know it occurs in the dataset so Im not adding something not present)

#in '' as its a factor not a number
allcrops_per_r_fly_innext<-allcrops_per_r_fly
allcrops_per_r_fly_innext$Ordering[is.na(allcrops_per_r_fly_innext$Ordering)] <- '27'
allcrops_per_r_fly_innext$raw_count[is.na(allcrops_per_r_fly_innext$raw_count)] <- 0

#create empty list
split_list <- list()

for (category in categories) {
 print(category)
   subset_data <- allcrops_per_r_fly_innext %>% filter(Crop == category)
#  subset_data <- allcrops_per_r_fly_innext %>% filter(Crop == "Carrot")
  test<-subset_data %>% ungroup() %>% dplyr::select(sample_ID,Ordering,raw_count)
  test   <- test %>%  spread(key = sample_ID, value = raw_count)
  test<-test %>% remove_rownames %>% column_to_rownames(var="Ordering")
  test[is.na(test)] <- 0
 # test <- test[rowSums(test == 0) != ncol(test), ]
  test<-test %>% mutate(across(everything(), ~ ifelse(. > 0, 1, 0)))
  split_list[[category]]<-as(test,'matrix')
} 

matrix_list <- list(data = list())
for (category in categories) {
  otu_table <- as(split_list[[category]], "matrix")
  matrix_list[["data"]][[category]] <- otu_table
}

#here Im setting the endpoint as NULL which is double the input sample size (this is probably most robust)
out.raw <- iNEXT3D(data = matrix_list$data, diversity = 'TD', q = c(0, 1, 2), datatype = 'incidence_raw', nboot = 50,endpoint=NULL)

#there is various ways to look at these plots:
#within a crop
ggiNEXT3D(out.raw, type = 1, facet.var = 'Assemblage') + facet_wrap(~Assemblage, nrow = 3)

#across crops
ggiNEXT3D(out.raw, type = 1, facet.var = "Order.q")


#running innext as abundance as well

#create empty list
split_list <- list()

for (category in categories) {
  print(category)
  subset_data <- allcrops_per_r_fly_innext %>% filter(Crop == category)
 # subset_data <- allcrops_per_r_fly_innext %>% filter(Crop == "Carrot")
  test<-subset_data %>% ungroup() %>% dplyr::select(sample_ID,Ordering,raw_count)
  test   <- test %>%  spread(key = sample_ID, value = raw_count)
  test<-test %>% remove_rownames %>% column_to_rownames(var="Ordering")
  test[is.na(test)] <- 0
  test<-as.data.frame(rowSums(test))
  colnames(test) <- c(paste0(category))
  # test <- test[rowSums(test == 0) != ncol(test), ]
 # test<-test %>% mutate(across(everything(), ~ ifelse(. > 0, 1, 0)))
  split_list[[category]]<-as(test,'matrix')
} 

matrix_list <- list(data = list())
for (category in categories) {
  otu_table <- as(split_list[[category]], "matrix")
  matrix_list[["data"]][[category]] <- otu_table
}

#here Im setting the endpoint as NULL which is double the input sample size (this is probably most robust)
out.raw <- iNEXT3D(data = matrix_list$data, diversity = 'TD', q = c(0, 1, 2), datatype = 'abundance', nboot = 50,endpoint=NULL)

#there is various ways to look at these plots:
#within a sample
ggiNEXT3D(out.raw, type = 1, facet.var = 'Assemblage') + facet_wrap(~Assemblage, nrow = 3)

#across samples
ggiNEXT3D(out.raw, type = 1, facet.var = "Order.q")

#yes well I think we can agree across samples works better for apple and pear
#but its interesting that avocado diversity depending on how you look at it can change quite a bit

######################################################################
#conservatively taking only Day 1 samples and analysing
######################################################################


#conservatively if we just look at the first day due to all the biases in the data
meta_data_day1<-meta_data %>% filter(Day=='Day_1')

counts_day1<-allcrops_per_r_fly %>% ungroup() %>% filter(Day=='Day_1') %>% select(sample_ID,Ordering,raw_count)

#I need to fill up the NA's for the samples with 0 flies with some dummy 0 for ones species to keep them in the next stage - so this is just to add a 0 value in and I've chosed the insect '27' at random (cause i know it occurs in the dataset so Im not adding something not present)

#in '' as its a factor not a number
counts_day1$Ordering[is.na(counts_day1$Ordering)] <- '27'
counts_day1$raw_count[is.na(counts_day1$raw_count)] <- 0
#transform wide
counts_day1_wide<-counts_day1 %>% pivot_wider(names_from=`Ordering`,values_from = `raw_count`)
counts_day1_wide[is.na(counts_day1_wide)] <- 0
counts_day1_wide<-counts_day1_wide %>% remove_rownames %>% column_to_rownames(var="sample_ID")

#count species per property
sppr <- specnumber(counts_day1_wide)
head(sppr)
sppr
#zeros are in there now, just because of the merge above they are out of order

####################################################
#impact of crop, region and flowering time
###################################################

#crop
sppr_aov <-aov(sppr ~ Crop, data = meta_data_day1)
summary(sppr_aov)
TukeyHSD(sppr_aov)

#impact of crop and region
sppr_aov <-aov(sppr ~ Crop+Region, data = meta_data_day1)
summary(sppr_aov)

#note region is no longer significant when we drop those other samples

#impact of flowering time
sppr_aov <-aov(sppr ~ flowertime, data = meta_data_day1)
summary(sppr_aov)
TukeyHSD(sppr_aov)

#impact of flowering time and crop
sppr_aov <-aov(sppr ~ flowertime+Crop, data = meta_data_day1)
summary(sppr_aov)
TukeyHSD(sppr_aov)

#can plot that out
sppr_df <- sppr %>% 
  enframe() %>% 
  full_join(meta_data_day1, by = c("name" = "sample_ID"))

#just crop
plot_sppr <- ggplot(sppr_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  scale_fill_manual(values=col_bound)+
  theme_bw()+
  labs(x = "Crop",
       y = "Number of species per sample",
       title = "Species richness")
plot_sppr

#other significant factor was flowertime
plot_sppr <- ggplot(sppr_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  facet_wrap(~flowertime, scales = "free_x")+
  theme_bw()+
  scale_fill_manual(values=col_bound)+
  labs(x = "Region",
       y = "Number of species per sample",
       title = "Species richness")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr

#using simpson which is less sensitive to rare species which should be more robust to the sampling biases in the data
simpsondiv<-diversity(counts_day1_wide,index='simpson')
head(simpsondiv)
simpsondiv
####################################################
#impact of crop, region and flowering time
###################################################

#crop
simpsondiv_aov <-aov(simpsondiv ~ Crop, data = meta_data_day1)
summary(simpsondiv_aov)
TukeyHSD(simpsondiv_aov)

#impact of crop and region
simpsondiv_aov <-aov(simpsondiv ~ Crop+Region, data = meta_data_day1)
summary(simpsondiv_aov)
#region not significant in simpson (this lines up with when we only use rep 1 (which is good))

#impact of flowering time
simpsondiv_aov <-aov(simpsondiv ~ flowertime, data = meta_data_day1)
summary(simpsondiv_aov)
TukeyHSD(simpsondiv_aov)

#impact of flowering time and crop
simpsondiv_aov <-aov(simpsondiv ~ flowertime+Crop, data = meta_data_day1)
summary(simpsondiv_aov)
TukeyHSD(simpsondiv_aov)

#can plot that out
simpsondiv_df <- simpsondiv %>% 
  enframe() %>% 
  full_join(meta_data_day1, by = c("name" = "sample_ID"))

#just crop
plot_sppr <- ggplot(simpsondiv_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  scale_fill_manual(values=col_bound)+
  theme_bw()+
  labs(x = "Crop",
       y = "Simpson",
       title = "Species richness: simpson")
plot_sppr

#other significant factor was flowertime
plot_sppr <- ggplot(simpsondiv_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  facet_wrap(~flowertime, scales = "free_x")+
  theme_bw()+
  scale_fill_manual(values=col_bound)+
  labs(x = "Region",
       y = "Simpson",
       title = "Species richness: simpson")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr


#########
#shannon#
#########

#hmmmm I actually dont like that, when say apple has a bunch of 1's its getting a very high score as its perfectly even

#having a look at shannon
shannondiv<-diversity(counts_day1_wide)
head(shannondiv)
#crop
shannondiv_aov <-aov(shannondiv ~ Crop, data = meta_data_day1)
summary(shannondiv_aov)
TukeyHSD(shannondiv_aov)

#impact of crop and region
shannondiv_aov <-aov(shannondiv ~ Crop+Region, data = meta_data_day1)
summary(shannondiv_aov)
TukeyHSD(shannondiv_aov)
#region significant in shannon

#impact of flowering time
shannondiv_aov <-aov(shannondiv ~ flowertime, data = meta_data_day1)
summary(shannondiv_aov)
TukeyHSD(shannondiv_aov)

#impact of flowering time and crop
shannondiv_aov <-aov(shannondiv ~ flowertime+Crop, data = meta_data_day1)
summary(shannondiv_aov)
TukeyHSD(shannondiv_aov)

#can plot that out
shannondiv_df <- shannondiv %>% 
  enframe() %>% 
  full_join(meta_data_day1, by = c("name" = "sample_ID"))

#just crop
plot_sppr <- ggplot(shannondiv_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  scale_fill_manual(values=col_bound)+
  theme_bw()+
  labs(x = "Crop",
       y = "Shannon",
       title = "Species richness: shannon")
plot_sppr

#other significant factor was region
plot_sppr <- ggplot(shannondiv_df, aes(x = Region, y = value, fill = Crop)) +
  geom_boxplot() +
  facet_wrap(~Crop, scales = "free_x")+
  theme_bw()+
  scale_fill_manual(values=col_bound)+
  labs(x = "Region",
       y = "Shannon",
       title = "Species richness: shannon")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr


#other significant factor was flowertime
plot_sppr <- ggplot(shannondiv_df, aes(x = Crop, y = value, fill = Crop)) +
  geom_boxplot() +
  facet_wrap(~flowertime, scales = "free_x")+
  theme_bw()+
  scale_fill_manual(values=col_bound)+
  labs(x = "Region",
       y = "Shannon",
       title = "Species richness: shannon")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr

################
#beta diversity#
################
#im not sure that this is useful as I have to remove the 0's (e.g. samples where there was no non-bee insects observed ~mostly apple/pear) but here it is:
#remove zero values:
counts_day1_wide_no0 <- counts_day1_wide[rowSums(counts_day1_wide == 0) != ncol(counts_day1_wide), ]

meta_data_day1_no0<-meta_data_day1 %>% filter(sample_ID %in% row.names(counts_day1_wide_no0))

#crop
perm<-adonis2(counts_day1_wide_no0 ~ Crop,data=meta_data_day1_no0)
perm
#flowertime
perm<-adonis2(counts_day1_wide_no0 ~ flowertime,data=meta_data_day1_no0)
perm
#region
perm<-adonis2(counts_day1_wide_no0 ~ Region,data=meta_data_day1_no0)
perm
#crop + flowertime
perm<-adonis2(counts_day1_wide_no0 ~ Crop+flowertime,data=meta_data_day1_no0)
perm
#crop+region
perm<-adonis2(counts_day1_wide_no0 ~ Crop+Region,data=meta_data_day1_no0)
perm
#crop+flowertime+region
perm<-adonis2(counts_day1_wide_no0 ~ Crop+flowertime+Region,data=meta_data_day1_no0)
perm


#MDS
pk_NMDS <- metaMDS(counts_day1_wide_no0,trymax=20)
#doesnt converge - whats up?
pk_NMDS
#stress (ideally <0.2)

stressplot(pk_NMDS)

plot(pk_NMDS)

plot_df <- scores(pk_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(meta_data_day1_no0, by = c("site" = "sample_ID"))

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Crop)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, linewidth = 0.5) +
  theme_bw()+
  labs(title = "NMDS")
plot_nmds

#################################################################
#setting a minimum count of 5 across at least 2 taxa#
#################################################################
#this is a little arbitary but means the MDS plot isnt trying to converge stuff with basically no data
#remove <5 counts:
counts_day1_wide_no0 <- counts_day1_wide[rowSums(counts_day1_wide) >= 5, ]
#remove things that are only from a single species
counts_day1_wide_no0 <-counts_day1_wide_no0[which(rowSums(1*(counts_day1_wide_no0!=0))>1),]

meta_data_day1_no0<-meta_data_day1 %>% filter(sample_ID %in% row.names(counts_day1_wide_no0))

#reading the beta as well
#crop
perm<-adonis2(counts_day1_wide_no0 ~ Crop,data=meta_data_day1_no0)
perm
#flowertime
perm<-adonis2(counts_day1_wide_no0 ~ flowertime,data=meta_data_day1_no0)
perm
#region
perm<-adonis2(counts_day1_wide_no0 ~ Region,data=meta_data_day1_no0)
perm
#crop + flowertime
perm<-adonis2(counts_day1_wide_no0 ~ Crop+flowertime,data=meta_data_day1a_no0)
perm
#crop+region
perm<-adonis2(counts_day1_wide_no0 ~ Crop+Region,data=meta_data_day1_no0)
perm
#crop+flowertime+region
perm<-adonis2(counts_day1_wide_no0 ~ Crop+flowertime+Region,data=meta_data_day1_no0)
perm

#####
#MDS#
#####

pk_NMDS <- metaMDS(counts_day1_wide_no0,trymax=500)
#converges - yay - a little stressed but is a lot of data
pk_NMDS
#stress 
stressplot(pk_NMDS)

plot(pk_NMDS)

plot_df <- scores(pk_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(meta_data_day1_no0, by = c("site" = "sample_ID"))

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Crop)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, linewidth = 0.5) +
  theme_bw()+
  scale_colour_manual(values=col_bound)+
  labs(title = "NMDS")
plot_nmds


#apple makes that look a bit dumb
#its only got a few samples which is why its so biased to that outlier. I'd just delete the cirle for apple on the previous as i think its too dodgy -

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Crop)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(aes(linetype = Crop)) +
  scale_linetype_manual(values = c(0,1,1,1,1,1,1,1)) +
  theme_bw()+
  scale_colour_manual(values=col_bound)+
  labs(title = "NMDS")
plot_nmds


# If you set the min at 6 it drops that outlier and wont draw a circle as there is to few points for apple 

counts_day1_wide_no0 <- counts_day1_wide[rowSums(counts_day1_wide) >= 6, ]
#remove things that are only from a single species
counts_day1_wide_no0 <-counts_day1_wide_no0[which(rowSums(1*(counts_day1_wide_no0!=0))>1),]

meta_data_day1_no0<-meta_data_day1 %>% filter(sample_ID %in% row.names(counts_day1_wide_no0))
#MDS
pk_NMDS <- metaMDS(counts_day1_wide_no0,trymax=200)
#converges - yay - a little stressed but is a lot of data
pk_NMDS
#stress 
stressplot(pk_NMDS)

plot(pk_NMDS)

plot_df <- scores(pk_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(meta_data_day1_no0, by = c("site" = "sample_ID"))

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Crop)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, linewidth = 0.5) +
  theme_bw()+
  scale_colour_manual(values=col_bound)+
  labs(title = "NMDS")
plot_nmds


########
#innext#
########
#innext by its nature should be robust for sampling bias but just for evenness sake Im rerunning on day 1 samples alone
library(iNEXT.3D)

#generate a list of the unique sample locations
categories <- unique(meta_data$Crop)
categories

#create empty list
split_list <- list()
#fill empty list with a presence absence OTU from each location

head(allcrops_per_r_fly)

#I need to fill up the NA's for the samples with 0 flies with some dummy 0 for ones species to keep them in the next stage - so this is just to add a 0 value in and I've chosed the insect '27' at random (cause i know it occurs in the dataset so Im not adding something not present)

#in '' as its a factor not a number
#filter to day 1 only
allcrops_per_r_fly_innext<-allcrops_per_r_fly %>% filter(Day=='Day_1')
allcrops_per_r_fly_innext$Ordering[is.na(allcrops_per_r_fly_innext$Ordering)] <- '27'
allcrops_per_r_fly_innext$raw_count[is.na(allcrops_per_r_fly_innext$raw_count)] <- 0

#create empty list
split_list <- list()

for (category in categories) {
  print(category)
  subset_data <- allcrops_per_r_fly_innext %>% filter(Crop == category)
  #  subset_data <- allcrops_per_r_fly_innext %>% filter(Crop == "Carrot")
  test<-subset_data %>% ungroup() %>% dplyr::select(sample_ID,Ordering,raw_count)
  test   <- test %>%  spread(key = sample_ID, value = raw_count)
  test<-test %>% remove_rownames %>% column_to_rownames(var="Ordering")
  test[is.na(test)] <- 0
  # test <- test[rowSums(test == 0) != ncol(test), ]
  test<-test %>% mutate(across(everything(), ~ ifelse(. > 0, 1, 0)))
  split_list[[category]]<-as(test,'matrix')
} 

matrix_list <- list(data = list())
for (category in categories) {
  otu_table <- as(split_list[[category]], "matrix")
  matrix_list[["data"]][[category]] <- otu_table
}

#here Im setting the endpoint as NULL which is double the input sample size (this is probably most robust)
out.raw <- iNEXT3D(data = matrix_list$data, diversity = 'TD', q = c(0, 1, 2), datatype = 'incidence_raw', nboot = 50,endpoint=NULL)

#there is various ways to look at these plots:
#within a crop
ggiNEXT3D(out.raw, type = 1, facet.var = 'Assemblage') + facet_wrap(~Assemblage, nrow = 3)

#across crops
ggiNEXT3D(out.raw, type = 1, facet.var = "Order.q")


#running innext as abundance as well

#create empty list
split_list <- list()

for (category in categories) {
  print(category)
  subset_data <- allcrops_per_r_fly_innext %>% filter(Crop == category)
  # subset_data <- allcrops_per_r_fly_innext %>% filter(Crop == "Carrot")
  test<-subset_data %>% ungroup() %>% dplyr::select(sample_ID,Ordering,raw_count)
  test   <- test %>%  spread(key = sample_ID, value = raw_count)
  test<-test %>% remove_rownames %>% column_to_rownames(var="Ordering")
  test[is.na(test)] <- 0
  test<-as.data.frame(rowSums(test))
  colnames(test) <- c(paste0(category))
  # test <- test[rowSums(test == 0) != ncol(test), ]
  # test<-test %>% mutate(across(everything(), ~ ifelse(. > 0, 1, 0)))
  split_list[[category]]<-as(test,'matrix')
} 

matrix_list <- list(data = list())
for (category in categories) {
  otu_table <- as(split_list[[category]], "matrix")
  matrix_list[["data"]][[category]] <- otu_table
}

#here Im setting the endpoint as NULL which is double the input sample size (this is probably most robust)
out.raw <- iNEXT3D(data = matrix_list$data, diversity = 'TD', q = c(0, 1, 2), datatype = 'abundance', nboot = 50,endpoint=NULL)

#there is various ways to look at these plots:
#within a sample
ggiNEXT3D(out.raw, type = 1, facet.var = 'Assemblage') + facet_wrap(~Assemblage, nrow = 3)

#across samples
ggiNEXT3D(out.raw, type = 1, facet.var = "Order.q")


