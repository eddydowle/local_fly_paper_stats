#making pie charts for brad

#need to make them resized based on abundance
#need to be editable for brad (SVG etc)
library(tidyverse)
library(lme4)
library(readxl)
library(svglite)

setwd("C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024")

apple<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Apple')
pear<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Pear')
avocado<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Avocado')
kiwifruit<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Kiwifruit')
pakchoi<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Pak choi')
radish<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Radish')
onion<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Onion')
carrot<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Carrot')

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
unique(apple$`Scientific name`)
unique(pear$`Scientific name`)
unique(avocado$`Scientific name`)
unique(kiwifruit$`Scientific name`)
unique(pakchoi$`Scientific name`)
unique(radish$`Scientific name`)
unique(onion$`Scientific name`)
unique(carrot$`Scientific name`)

#this column is also the one for the ordering on around pie chart based on the lettering
levels(as.factor(apple$Ordering))
#will default to correct ordering

#remove “Ephydridae spp.” from Pak choi and onion data
#as per brad is wasnt counted in the other crops so needs to be removed
onion<-onion %>% filter(`Scientific name`!='Ephydridae spp.')
pakchoi<-pakchoi %>% filter(`Scientific name`!='Ephydridae spp.')


#looks good 
unique(apple$Region)
#[1] "Hawkes Bay" "Motueka" 
mydf<-apple %>% filter(Region=='Hawkes Bay')
head(mydf)
totals<-mydf %>% select(`Location and season`,`Corrected Daily Count`) %>% group_by(`Location and season`) %>% summarise(total=sum(`Corrected Daily Count`))

mydf<-left_join(mydf,totals,by='Location and season')



#genus<-c("Apis","Bombus" ,"Hylaeus","Lasioglossum", "Leioproctus" , "Megachile")
#col_genus<-c('gold','gold3','firebrick4','firebrick1','firebrick','mediumorchid4')

#brads_col<-data.frame(genus,col_genus) %>% arrange(genus)

brads_col_2<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Insect_ordering')
head(brads_col_2)
apple 

#just bees vs flies to start with
#for this just turning everything into a percentage
test<-mydf%>% select(`Location and season`,`Corrected Daily Count`,Grouping) %>% group_by(`Location and season`,Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()

    test %>%   ggplot(aes(x = 1, y = per, fill = Grouping)) +
  geom_bar(stat = "identity" ,width=1) +
  facet_wrap(~ `Location and season` , strip.position = "bottom") +
  coord_polar("y", start = 0, direction = -1) +
  theme_bw(base_size = 12) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.title = element_text(size = 6), 
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 4))+
  scale_fill_manual(values=c('gold','dodgerblue'))+
 #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
  ggtitle(paste('Apple','Hawkes Bay'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))

  mydf %>% filter(Grouping=='Non_Bees')%>% ggplot(aes(x = total/2, y = `Corrected Daily Count`, fill = as.factor(Ordering), width = total)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~ `Location and season` , strip.position = "bottom") +
    coord_polar("y", start = 0, direction = -1) +
    theme_bw(base_size = 12) +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(size = 6), 
          strip.background = element_rect(fill = NA, colour = NA),
          strip.text = element_text(size = 4))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
    ggtitle(paste('Apple','Hawkes Bay'))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))
  
  
############################
  #actual figures#
  
  
#BEES VS FLIES
#decided on a barchart of bees vs flies to sit next to the pies

  #graph with the bees vs flies
  
  #just bees vs flies to start with
  #for this just turning everything into a percentage
  #then brad wants a little bar he can jiggle in next to his barchart
unique(apple$Region)
unique(pear$Region)
unique(avocado$Region)
unique(kiwifruit$Region)
unique(pakchoi$Region)
unique(radish$Region)
unique(onion$Region)
unique(carrot$Region)
  
test<-avocado%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()

  
  test %>%   ggplot(aes(x = `Location and season`, y = per, width=0.2, fill = Grouping)) +
    geom_bar(position='stack',stat='identity') +
    facet_grid(~Region, scales = "free", space = "free") +
    theme_bw(base_size = 12) +
    theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
    scale_fill_manual(values=c('lightgrey','black'))+
    ggtitle(paste('Avocado'))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))  
  
#maybe all as one on one figure?
  apple_per<-apple%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  apple_per$crop<-'Apple'
  
  pear_per<-pear%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  pear_per$crop<-'Pear'
  
  avocado_per<-avocado%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  avocado_per$crop<-'Avocado'
  
  kiwifruit_per<-kiwifruit%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  kiwifruit_per$crop<-'Kiwifruit'
  
  pakchoi_per<-pakchoi%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  pakchoi_per$crop<-'Pakchoi'
  
  radish_per<-radish%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  radish_per$crop<-'Radish'
  
  onion_per<-onion%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  onion_per$crop<-'Onion'
  
  carrot_per<-carrot%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  carrot_per$crop<-'Carrot'
allcrops_per<-rbind(apple_per,pear_per,avocado_per,kiwifruit_per,pakchoi_per,radish_per,onion_per,carrot_per)  

unique(allcrops_per$Grouping)

allcrops_per %>%   ggplot(aes(x = `Location and season`, y = per, fill = Grouping)) +
  geom_bar(position='stack',stat='identity',width=0.4) +
  facet_grid(crop~Region, scales = "free", space = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  scale_fill_manual(values=c('lightgrey','black'))+
  ggtitle(paste('Bees vs Flies'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))  


#doing an overview figure do it per crop by region and across crop
#by region
apple_per_r<-apple%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
apple_per_r$crop<-'Apple'

pear_per_r<-pear%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
pear_per_r$crop<-'Pear'

avocado_per_r<-avocado%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
avocado_per_r$crop<-'Avocado'

kiwifruit_per_r<-kiwifruit%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
kiwifruit_per_r$crop<-'Kiwifruit'

pakchoi_per_r<-pakchoi%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
pakchoi_per_r$crop<-'Pakchoi'

radish_per_r<-radish%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
radish_per_r$crop<-'Radish'

onion_per_r<-onion%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
onion_per_r$crop<-'Onion'

carrot_per_r<-carrot%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
carrot_per_r$crop<-'Carrot'
allcrops_per_r<-rbind(apple_per_r,pear_per_r,avocado_per_r,kiwifruit_per_r,pakchoi_per_r,radish_per_r,onion_per_r,carrot_per_r)  



allcrops_per_r %>%   ggplot(aes(x = Region, y = per, fill = Grouping)) +
  geom_bar(position='stack',stat='identity',width=0.9) +
  facet_grid(.~crop, scales = "free", space = "free") +
  #facet_wrap(~crop, scales = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  scale_fill_manual(values=c('lightgrey','black'))+
  ggtitle(paste('Bees vs Flies'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line")) 

#across all crops
apple_per_tot<-apple%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
apple_per_tot$crop<-'Apple'

pear_per_tot<-pear%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
pear_per_tot$crop<-'Pear'

avocado_per_tot<-avocado%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
avocado_per_tot$crop<-'Avocado'

kiwifruit_per_tot<-kiwifruit%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
kiwifruit_per_tot$crop<-'Kiwifruit'

pakchoi_per_tot<-pakchoi%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
pakchoi_per_tot$crop<-'Pakchoi'

radish_per_tot<-radish%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
radish_per_tot$crop<-'Radish'

onion_per_tot<-onion%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
onion_per_tot$crop<-'Onion'

carrot_per_tot<-carrot%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
carrot_per_tot$crop<-'Carrot'
allcrops_per_tot<-rbind(apple_per_tot,pear_per_tot,avocado_per_tot,kiwifruit_per_tot,pakchoi_per_tot,radish_per_tot,onion_per_tot,carrot_per_tot)  


allcrops_per_tot %>%   ggplot(aes(x = crop, y = per, fill = Grouping)) +
  geom_bar(position='stack',stat='identity',width=0.9) +
  #facet_grid(.~crop, scales = "free", space = "free") +
  #facet_wrap(~crop, scales = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  scale_fill_manual(values=c('lightgrey','black'))+
  ggtitle(paste('Bees vs Flies'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))

  
#############################
  #Pies of flies#
############################
  
unique(apple$Region)
unique(pear$Region)
unique(avocado$Region)
unique(kiwifruit$Region)
unique(pakchoi$Region)
unique(radish$Region)
unique(onion$Region)
unique(carrot$Region)

#just realising that the size wont be consistent between the sheets so lets do a facet grid and see if that solves it
#make it a loop
  cropsp<-'Carrot'
  crop_nonbees<-carrot %>% filter(Grouping=='Non_Bees')
  #apple looks dumb so doing a log+1 to see if that helps
  totals<-crop_nonbees %>% filter(Grouping=='Non_Bees') %>% select(`Location and season`,`Corrected Daily Count`,Region) %>% group_by(`Location and season`,Region) %>% summarise(total=sum(`Corrected Daily Count`))
  mydf<-left_join(crop_nonbees,totals,by=c('Location and season','Region'))
   image<-mydf %>% ggplot(aes(x = total/2, y = `Corrected Daily Count`, fill = as.factor(Ordering), width = total)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_grid( Region~`Location and season` ) +
    coord_polar("y", start = 0, direction = -1) +
    theme_bw(base_size = 12) +
    theme(axis.title = element_blank(),
          strip.text.x = element_text(angle = 90,size=4,hjust=0),
          strip.text.y = element_text(angle = 0,size=4,hjust=0),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(size = 6), 
          strip.background = element_rect(fill = NA, colour = NA))+
        #  strip.text = element_text(size = 4))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
    ggtitle(paste0(cropsp))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))
  image
   #This actually save the plot in a image
  #ggsave(file=paste0(cropsp,"_",'HawkesBay',".svg"), plot=image, width=15, height=12)
  ggsave(file=paste0(cropsp,"_","allareas_fix",".svg"), plot=image, width=30, height=12)
  ggsave(file=paste0(cropsp,"_","allareas_fix",".pdf"), plot=image, width=30, height=12)

  
#barcharts but log+1  
#apple looks dumb so doing a log+1 to see if that helps
#need to calculate the percentage of each insect:
crop_nonbees_per<-crop_nonbees %>% select(`Location and season`,`Corrected Daily Count`,Region,Ordering,`Scientific name`) %>% group_by(`Location and season`,Region,Ordering,`Scientific name`) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=total_count/sum(total_count)) %>% ungroup()
  
#log total
totals$total_log<-log(totals$total+1)
  
#total  x % - new fraction
mydf<-left_join(crop_nonbees_per,totals,by=c('Location and season'='Location and season','Region'='Region'))
  
mydf$total_count_scaled<-mydf$per*mydf$total_log
  
brads_col_2$Species <- forcats::fct_relevel(brads_col_2$Species ,levels(brads_col_2$Code))
levels(brads_col_2$Species)
  
  
image<-mydf %>% ggplot(aes(x = total_log/2, y = total_count_scaled, fill = as.factor(Ordering), width = total_log)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_grid( Region~`Location and season` ) +
    coord_polar("y", start = 0, direction = -1) +
    theme_bw(base_size = 12) +
    theme(axis.title = element_blank(),
          strip.text.x = element_text(angle = 90,size=4,hjust=0),
          strip.text.y = element_text(angle = 0,size=4,hjust=0),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(size = 6), 
          strip.background = element_rect(fill = NA, colour = NA))+
    #  strip.text = element_text(size = 4))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
    ggtitle(paste0(cropsp))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))
image  
ggsave(file=paste0(cropsp,"_","allareas_log1_fix",".svg"), plot=image, width=30, height=12)
  ggsave(file=paste0(cropsp,"_","allareas_log1_fix",".pdf"), plot=image, width=30, height=12)

  
  
########################################
##doing barcharts of fly diversity by crop##
########################################
  
  
#doing an overview figure do it per crop by region and across crop
  #by region
  #iniditally I did this by scientific name but thats a dodgy column, Ordering is going to be a better column to do it on
  apple_per_r_fly<-apple%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Region,Ordering)  %>% group_by(`Scientific name`,Region,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  apple_per_r_fly$crop<-'Apple'
  apple_reg_fe_n<-apple %>%  group_by(Region) %>%summarise(region_feild_count = n_distinct(`Location and season`))  
  apple_per_r_fly<-left_join(apple_per_r_fly,apple_reg_fe_n)
  apple_per_r_fly<-apple_per_r_fly %>% mutate(ave_feild_count=total_count/region_feild_count)
  
  pear_per_r_fly<-pear%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Region,Ordering)  %>% group_by(`Scientific name`,Region,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  pear_per_r_fly$crop<-'Pear'
  pear_reg_fe_n<-pear %>%  group_by(Region) %>%summarise(region_feild_count = n_distinct(`Location and season`))  
  pear_per_r_fly<-left_join(pear_per_r_fly,pear_reg_fe_n)
  pear_per_r_fly<-pear_per_r_fly %>% mutate(ave_feild_count=total_count/region_feild_count)
  
  avocado_per_r_fly<-avocado%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Region,Ordering)  %>% group_by(`Scientific name`,Region,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  avocado_per_r_fly$crop<-'Avocado'
  avocado_reg_fe_n<-avocado %>%  group_by(Region) %>%summarise(region_feild_count = n_distinct(`Location and season`))  
  avocado_per_r_fly<-left_join(avocado_per_r_fly,avocado_reg_fe_n)
  avocado_per_r_fly<-avocado_per_r_fly %>% mutate(ave_feild_count=total_count/region_feild_count)
  
  kiwifruit_per_r_fly<-kiwifruit%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Region,Ordering)  %>% group_by(`Scientific name`,Region,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  kiwifruit_per_r_fly$crop<-'Kiwifruit'
  kiwifruit_reg_fe_n<-kiwifruit %>%  group_by(Region) %>%summarise(region_feild_count = n_distinct(`Location and season`))  
  kiwifruit_per_r_fly<-left_join(kiwifruit_per_r_fly,kiwifruit_reg_fe_n)
  kiwifruit_per_r_fly<-kiwifruit_per_r_fly %>% mutate(ave_feild_count=total_count/region_feild_count)
  
  pakchoi_per_r_fly<-pakchoi%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Region,Ordering)  %>% group_by(`Scientific name`,Region,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  pakchoi_per_r_fly$crop<-'Pakchoi'
  pakchoi_reg_fe_n<-pakchoi %>%  group_by(Region) %>%summarise(region_feild_count = n_distinct(`Location and season`))  
  pakchoi_per_r_fly<-left_join(pakchoi_per_r_fly,pakchoi_reg_fe_n)
  pakchoi_per_r_fly<-pakchoi_per_r_fly %>% mutate(ave_feild_count=total_count/region_feild_count)
  
  radish_per_r_fly<-radish%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Region,Ordering)  %>% group_by(`Scientific name`,Region,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  radish_per_r_fly$crop<-'Radish'
  radish_reg_fe_n<-radish %>%  group_by(Region) %>%summarise(region_feild_count = n_distinct(`Location and season`))  
  radish_per_r_fly<-left_join(radish_per_r_fly,radish_reg_fe_n)
  radish_per_r_fly<-radish_per_r_fly %>% mutate(ave_feild_count=total_count/region_feild_count)
  
  onion_per_r_fly<-onion%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Region,Ordering)  %>% group_by(`Scientific name`,Region,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  onion_per_r_fly$crop<-'Onion'
  onion_reg_fe_n<-onion %>%  group_by(Region) %>%summarise(region_feild_count = n_distinct(`Location and season`))  
  onion_per_r_fly<-left_join(onion_per_r_fly,onion_reg_fe_n)
  onion_per_r_fly<-onion_per_r_fly %>% mutate(ave_feild_count=total_count/region_feild_count)
  
  carrot_per_r_fly<-carrot%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Region,Ordering)  %>% group_by(`Scientific name`,Region,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  carrot_per_r_fly$crop<-'Carrot'
carrot_reg_fe_n<-carrot %>%  group_by(Region) %>%summarise(region_feild_count = n_distinct(`Location and season`))  
carrot_per_r_fly<-left_join(carrot_per_r_fly,carrot_reg_fe_n)
carrot_per_r_fly<-carrot_per_r_fly %>% mutate(ave_feild_count=total_count/region_feild_count)

  
allcrops_per_r_fly<-rbind(apple_per_r_fly,pear_per_r_fly,avocado_per_r_fly,kiwifruit_per_r_fly,pakchoi_per_r_fly,radish_per_r_fly,onion_per_r_fly,carrot_per_r_fly)  

#  brads_col_2$Species <- forcats::fct_relevel(brads_col_2$Species ,levels(brads_col_2$Code))
levels(brads_col_2$Species)
allcrops_per_r_fly$`Scientific name` <- factor(allcrops_per_r_fly$`Scientific name`, levels=unique(allcrops_per_r_fly$`Scientific name`[order(allcrops_per_r_fly$Ordering)]), ordered=TRUE)
  #brads_col_2$Species <- forcats::fct_relevel(allcrops_per_r_fly$`Scientific name` ,levels(allcrops_per_r_fly$Ordering))
levels(brads_col_2$Species)
test<-unique(allcrops_per_r_fly$`Scientific name`)
test  
brads_col_2$Species
  
  allcrops_per_r_fly %>%   ggplot(aes(x = Region, y = per, fill = `Scientific name`)) +
    geom_bar(position='stack',stat='identity',width=0.9) +
    facet_grid(.~crop, scales = "free", space = "free") +
    #facet_wrap(~crop, scales = "free") +
    theme_bw(base_size = 12) +
    theme(strip.text.x = element_text(angle = 0,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
 #   scale_fill_manual(values=c('lightgrey','black'))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Species)))+
 #   scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    ggtitle(paste('Fly diversity'))+
    ylab('Abundance')+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line")) 

  
  #whave to be average counts not this, so need to use count average by feild count 
  allcrops_per_r_fly %>%   ggplot(aes(x = Region, y = ave_feild_count, fill = `Scientific name`)) +
    geom_bar(position='stack',stat='identity',width=0.9) +
    facet_grid(.~crop, scales = "free", space = "free") +
    #facet_wrap(~crop, scales = "free") +
    theme_bw(base_size = 12) +
    theme(strip.text.x = element_text(angle = 0,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Species)))+
    ggtitle(paste('Fly diversity'))+
    ylab('Ave counts per feild')+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))   
  
    
  #by crop
  apple_per_r_fly<-apple%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Ordering)  %>% group_by(`Scientific name`,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  apple_per_r_fly$crop<-'Apple'
  apple_reg_fe_n<-apple %>%  group_by(Crop) %>%summarise(crop_feild_count = n_distinct(`Location and season`))  
  apple_per_r_fly<-left_join(apple_per_r_fly,apple_reg_fe_n,by=c('crop'='Crop'))
  apple_per_r_fly<-apple_per_r_fly %>% mutate(ave_crop_count=total_count/crop_feild_count)
  
  pear_per_r_fly<-pear%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Ordering)  %>% group_by(`Scientific name`,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  pear_per_r_fly$crop<-'Pear'
  pear_reg_fe_n<-pear %>%  group_by(Crop) %>%summarise(crop_feild_count = n_distinct(`Location and season`))  
  pear_per_r_fly<-left_join(pear_per_r_fly,pear_reg_fe_n,by=c('crop'='Crop'))
  pear_per_r_fly<-pear_per_r_fly %>% mutate(ave_crop_count=total_count/crop_feild_count)
  
  avocado_per_r_fly<-avocado%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Ordering)  %>% group_by(`Scientific name`,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  avocado_per_r_fly$crop<-'Avocado'
  avocado_reg_fe_n<-avocado %>%  group_by(Crop) %>%summarise(crop_feild_count = n_distinct(`Location and season`))  
  avocado_per_r_fly<-left_join(avocado_per_r_fly,avocado_reg_fe_n,by=c('crop'='Crop'))
  avocado_per_r_fly<-avocado_per_r_fly %>% mutate(ave_crop_count=total_count/crop_feild_count)
  
  kiwifruit_per_r_fly<-kiwifruit%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Ordering)  %>% group_by(`Scientific name`,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  kiwifruit_per_r_fly$crop<-'Kiwifruit'
  kiwifruit_reg_fe_n<-kiwifruit %>%  group_by(Crop) %>%summarise(crop_feild_count = n_distinct(`Location and season`))  
  kiwifruit_per_r_fly<-left_join(kiwifruit_per_r_fly,kiwifruit_reg_fe_n,by=c('crop'='Crop'))
  kiwifruit_per_r_fly<-kiwifruit_per_r_fly %>% mutate(ave_crop_count=total_count/crop_feild_count)
  
  pakchoi_per_r_fly<-pakchoi%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Ordering)  %>% group_by(`Scientific name`,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  pakchoi_per_r_fly$crop<-'Pakchoi'
  pakchoi_reg_fe_n<-pakchoi %>%  group_by(Crop) %>%summarise(crop_feild_count = n_distinct(`Location and season`))  
  pakchoi_reg_fe_n$Crop <- sub("Pak Choi", "Pakchoi", pakchoi_reg_fe_n$Crop)
  pakchoi_per_r_fly<-left_join(pakchoi_per_r_fly,pakchoi_reg_fe_n,by=c('crop'='Crop'))
  pakchoi_per_r_fly
  pakchoi_per_r_fly<-pakchoi_per_r_fly %>% mutate(ave_crop_count=total_count/crop_feild_count)
  
  radish_per_r_fly<-radish%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Ordering)  %>% group_by(`Scientific name`,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  radish_per_r_fly$crop<-'Radish'
  radish_reg_fe_n<-radish %>%  group_by(Crop) %>%summarise(crop_feild_count = n_distinct(`Location and season`))  
  radish_per_r_fly<-left_join(radish_per_r_fly,radish_reg_fe_n,by=c('crop'='Crop'))
  radish_per_r_fly<-radish_per_r_fly %>% mutate(ave_crop_count=total_count/crop_feild_count)
  
  onion_per_r_fly<-onion%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Ordering)  %>% group_by(`Scientific name`,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  onion_per_r_fly$crop<-'Onion'
  onion_reg_fe_n<-onion %>%  group_by(Crop) %>%summarise(crop_feild_count = n_distinct(`Location and season`))  
  onion_per_r_fly<-left_join(onion_per_r_fly,onion_reg_fe_n,by=c('crop'='Crop'))
  onion_per_r_fly<-onion_per_r_fly %>% mutate(ave_crop_count=total_count/crop_feild_count)
  
  carrot_per_r_fly<-carrot%>% filter(Grouping!='Bees')%>% filter(`Scientific name`!='(blank)')%>% select(`Corrected Daily Count`,`Scientific name`,Ordering)  %>% group_by(`Scientific name`,Ordering) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  carrot_per_r_fly$crop<-'Carrot'
  carrot_reg_fe_n<-carrot %>%  group_by(Crop) %>%summarise(crop_feild_count = n_distinct(`Location and season`))  
  carrot_per_r_fly<-left_join(carrot_per_r_fly,carrot_reg_fe_n,by=c('crop'='Crop'))
  carrot_per_r_fly<-carrot_per_r_fly %>% mutate(ave_crop_count=total_count/crop_feild_count)
  
  allcrops_per_r_fly<-rbind(apple_per_r_fly,pear_per_r_fly,avocado_per_r_fly,kiwifruit_per_r_fly,pakchoi_per_r_fly,radish_per_r_fly,onion_per_r_fly,carrot_per_r_fly)  
  
  #  brads_col_2$Species <- forcats::fct_relevel(brads_col_2$Species ,levels(brads_col_2$Code))
  levels(brads_col_2$Species)
  allcrops_per_r_fly$`Scientific name` <- factor(allcrops_per_r_fly$`Scientific name`, levels=unique(allcrops_per_r_fly$`Scientific name`[order(allcrops_per_r_fly$Ordering)]), ordered=TRUE)
  #brads_col_2$Species <- forcats::fct_relevel(allcrops_per_r_fly$`Scientific name` ,levels(allcrops_per_r_fly$Ordering))
  levels(brads_col_2$Species)
  
  allcrops_per_r_fly %>%   ggplot(aes(x = crop, y = per, fill = `Scientific name`)) +
    geom_bar(position='stack',stat='identity',width=0.9) +
    facet_grid(.~crop, scales = "free", space = "free") +
    #facet_wrap(~crop, scales = "free") +
    theme_bw(base_size = 12) +
    theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
    #   scale_fill_manual(values=c('lightgrey','black'))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Species)))+
    #   scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    ggtitle(paste('Fly diversity'))+
    ylab('Abundance')+
    xlab('Crop')+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line")) 
  
  #whave to be average counts not this, so need to use count average by feild count 
  allcrops_per_r_fly %>%   ggplot(aes(x = crop, y = ave_crop_count, fill = `Scientific name`)) +
    geom_bar(position='stack',stat='identity',width=0.9) +
    facet_grid(.~crop, scales = "free", space = "free") +
    #facet_wrap(~crop, scales = "free") +
    theme_bw(base_size = 12) +
    theme(strip.text.x = element_text(angle = 0,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Species)))+
    ggtitle(paste('Fly diversity'))+
    ylab('Ave counts per feild')+
    xlab('Crop')+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))   
  

  
    
  
    
##############
#cant do this#
##############
  
#log +1
#logging straight doesnt work as total needs to logged and worked back (trust me ratios end up messed up)
  
crop_nonbees2<-crop_nonbees
crop_nonbees2$Corrected_Daily_Count_log1<-log(crop_nonbees2$`Corrected Daily Count`+1)
totals2<-crop_nonbees %>% filter(Grouping=='Non_Bees') %>% select(`Location and season`,Corrected_Daily_Count_log1,Region) %>% group_by(`Location and season`,Region) %>% summarise(total=sum(Corrected_Daily_Count_log1))
mydf2<-left_join(crop_nonbees2,totals2,by=c('Location and season','Region'))
 
mydf2 %>% ggplot(aes(x = total/2, y = `Corrected Daily Count`, fill = as.factor(Ordering), width = total)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_grid( Region~`Location and season` ) +
    coord_polar("y", start = 0, direction = -1) +
    theme_bw(base_size = 12) +
    theme(axis.title = element_blank(),
          strip.text.x = element_text(angle = 90,size=4,hjust=0),
          strip.text.y = element_text(angle = 0,size=4,hjust=0),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(size = 6), 
          strip.background = element_rect(fill = NA, colour = NA))+
    #  strip.text = element_text(size = 4))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
    ggtitle(paste0(cropsp))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))
  
#  ggsave(file=paste0(cropsp,"_","allareas_log1",".svg"), plot=image, width=30, height=12)
#  ggsave(file=paste0(cropsp,"_","allareas_log1",".pdf"), plot=image, width=30, height=12)

colnames(crop_nonbees)



######################################
#old code that looped through locations within a crop 
#unfortunelty it wont keep the size consistent between regions so shouldnt use it
###################################


#make it a loop
for (item in unique(apple$Region)){
  cropsp<-'Apple'
  print(item)
  mydf<-apple %>% filter(Region==item)
  #  mydf<-apple %>% filter(Region=='Hawkes Bay')
  totals<-mydf %>% select(`Location and season`,`Corrected Daily Count`) %>% group_by(`Location and season`) %>% summarise(total=sum(`Corrected Daily Count`))
  mydf<-left_join(mydf,totals,by='Location and season')
  image<-mydf %>% filter(Grouping=='Non_Bees')%>% ggplot(aes(x = total/2, y = `Corrected Daily Count`, fill = as.factor(Ordering), width = total)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~ `Location and season` , strip.position = "bottom") +
    coord_polar("y", start = 0, direction = -1) +
    theme_bw(base_size = 12) +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(size = 6), 
          strip.background = element_rect(fill = NA, colour = NA),
          strip.text = element_text(size = 4))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
    ggtitle(paste0(cropsp,": ",item))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))
  #This actually save the plot in a image
  #ggsave(file=paste0(cropsp,"_",'HawkesBay',".svg"), plot=image, width=15, height=12)
  ggsave(file=paste0(cropsp,"_",item,".svg"), plot=image, width=15, height=12)
  ggsave(file=paste0(cropsp,"_",item,".pdf"), plot=image, width=15, height=12)
}


#colours
insects_taxonomy<-read_excel('Insect groupings life histories Eddy_cp.xlsx', sheet = 'Taxonomic and life histories',na='NA')

