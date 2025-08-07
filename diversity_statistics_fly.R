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
sort(unique(apple$`Scientific name`))
sort(unique(pear$`Scientific name`)) #tipula spp.
sort(unique(avocado$`Scientific name`)) #Orthoprosopa bilineata 
sort(unique(kiwifruit$`Scientific name`)) #"Tipula. spp."
sort(unique(pakchoi$`Scientific name`))
sort(unique(radish$`Scientific name`))
sort(unique(onion$`Scientific name`)) #"Tipula sp."
sort(unique(carrot$`Scientific name`))

#remove “Ephydridae spp.” from Pak choi and onion data
#as per brad is wasnt counted in the other crops so needs to be removed
onion<-onion %>% filter(`Scientific name`!='Ephydridae spp.')
pakchoi<-pakchoi %>% filter(`Scientific name`!='Ephydridae spp.')


# Tipula sp. Tipula spp.

#Orthoprosopa bilineata Orthoprosopa bilineata.

#Other Heteroptera Other Heteroptera


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
