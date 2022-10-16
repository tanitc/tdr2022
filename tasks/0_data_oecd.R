library(OECD)
library(tidyverse)
library(ggrepel)
library(rvest)

datasets <- get_datasets()
datasets_plastic<-search_dataset("plastic", datasets)

get_datasets_list <- function(data_names){
  list_plastic <- list()
  list_meta_plastic <- list()
  for (i in as.character(data_names)){
    list_plastic[[i]] <- get_dataset(i)
    list_meta_plastic[[i]]<-data.frame(titolGrup="",titolBloc="",contingutBloc="")
    for (conti in blocs){
      a1<-conti %>% 
        html_elements(".MetadataGroupTitle") %>% 
        html_text2()
      a2<-conti %>% 
        html_elements(".MetadataItemTitle") %>% 
        html_text2()
      a3<-conti %>% 
        html_elements(".MetadataItemBlock") %>% 
        html_text2()
      list_meta_plastic[[i]] <- rbind(list_meta_plastic[[i]],data.frame(titolGrup=a1,titolBloc=a2,contingutBloc=a3))
    }
  }
  return(list(data=list_plastic,metadata=list_meta_plastic))
}

resultat <- get_datasets_list(datasets_plastic$id)


resultat$desc<- datasets_plastic
resultat$data[[1]]
resultat$meta[[1]]

resultat$config <-vector("list", length(resultat$data))
names(resultat$config)<- names(resultat$data)


###########   NO

i <- 1
(graph_name <- as.character(resultat$desc$id[i]))
as.character(resultat$desc$title[i])
head(resultat$data[[graph_name]])
colnames(resultat$data[[graph_name]])
browse_metadata(graph_name)
dim(resultat$data[[graph_name]])
# [1] "Plastic use in 2019"
#   LOCATION ObsValue    PLASTICS_APPLICATIONS PLASTICS_POLYMER


table(resultat$data[[graph_name]]$LOCATION)


df <- resultat$data[[graph_name]] %>% 
  filter(LOCATION!="TOTAL") %>% 
  mutate( ObsValue = as.numeric(ObsValue)
          ,Time=as.numeric(Time))
df %>% 
  ggplot(aes(x=Time,y=ObsValue, col=LOCATION))+
  geom_line()+
  geom_text_repel(data=df %>% filter(Time==2018), aes(x=Time,y=ObsValue, label=LOCATION), size=2)+
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )

############### SI

i <- 2

graph_name <- "PLASTIC_USE_7"

df <- resultat$data[[graph_name]] %>% 
  filter(PLASTIC_TYPES!="TOTAL") %>% 
  mutate( ObsValue = as.numeric(ObsValue)
          ,Time=as.numeric(Time))

df %>% 
  ggplot(aes(x=Time,y=ObsValue, col=PLASTIC_TYPES))+
  geom_line()+
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )

###########   SI

i <- 3
(graph_name <- as.character(resultat$desc$id[i]))
as.character(resultat$desc$title[i])
head(resultat$data[[graph_name]])
colnames(resultat$data[[graph_name]])
browse_metadata(graph_name)
dim(resultat$data[[graph_name]])

table(resultat$data[[graph_name]]$PLASTICS_POLYMER)

df <- resultat$data[[graph_name]] %>% 
  filter(PLASTICS_POLYMER!="TOTAL") %>% 
  mutate( ObsValue = as.numeric(ObsValue)
          ,Time=as.numeric(Time))
df %>% 
  ggplot(aes(x=Time,y=ObsValue, col=PLASTICS_POLYMER))+
  geom_line()+
  geom_text_repel(data=df %>% filter(Time==2018), aes(x=Time,y=ObsValue, label=PLASTICS_POLYMER), size=2)+
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )


###########   SI

i <- 4
(graph_name <- as.character(resultat$desc$id[i]))
as.character(resultat$desc$title[i])
head(resultat$data[[graph_name]])
colnames(resultat$data[[graph_name]])
browse_metadata(graph_name)
dim(resultat$data[[graph_name]])

table(resultat$data[[graph_name]]$LOCATION)

df <- resultat$data[[graph_name]] %>% 
  filter(LOCATION!="TOTAL") %>% 
  mutate( ObsValue = as.numeric(ObsValue)
          ,Time=as.numeric(Time))
df %>% 
  ggplot(aes(x=Time,y=ObsValue, col=LOCATION))+
  geom_line()+
  geom_text_repel(data=df %>% filter(Time==2018), aes(x=Time,y=ObsValue, label=LOCATION), size=2)+
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )


###########   SI

i <- 5
(graph_name <- as.character(resultat$desc$id[i]))
as.character(resultat$desc$title[i])
head(resultat$data[[graph_name]])
colnames(resultat$data[[graph_name]])
browse_metadata(graph_name)
dim(resultat$data[[graph_name]])

table(resultat$data[[graph_name]]$PLASTICS_APPLICATIONS)

df <- resultat$data[[graph_name]] %>% 
  filter(PLASTICS_APPLICATIONS!="TOTAL") %>% 
  mutate( ObsValue = as.numeric(ObsValue)
          ,Time=as.numeric(Time))
df %>% 
  ggplot(aes(x=Time,y=ObsValue, col=PLASTICS_APPLICATIONS))+
  geom_line()+
  geom_text_repel(data=df %>% filter(Time==2018), aes(x=Time,y=ObsValue, label=PLASTICS_APPLICATIONS), size=2)+
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )



###########   SI

i <- 6
(graph_name <- as.character(resultat$desc$id[i]))
as.character(resultat$desc$title[i])
head(resultat$data[[graph_name]])
colnames(resultat$data[[graph_name]])
browse_metadata(graph_name)
dim(resultat$data[[graph_name]])

table(resultat$data[[graph_name]]$LOCATION)
table(resultat$data[[graph_name]]$PLASTICS_EOLFATETYPES)

df <- resultat$data[[graph_name]] %>% 
  arrange(Time,PLASTICS_EOLFATETYPES) %>% 
  mutate( ObsValue = as.numeric(ObsValue)) %>% 
  filter(PLASTICS_EOLFATETYPES!="TOTAL"
         ,Time %in% c("1990","2000","2010","2019")) %>% 
  group_by(Time,PLASTICS_EOLFATETYPES) %>% 
  summarise(ObsValue = sum(ObsValue)) %>% 
  ungroup() %>% 
  group_by(Time) %>% 
  mutate( per_ObsValue = ObsValue / sum(ObsValue)
          , perc_label_y = cumsum(per_ObsValue)-per_ObsValue/2
          , label_y = cumsum(ObsValue)-ObsValue/2
  )
  
## %
df %>% 
  ggplot(aes(x=Time,y=per_ObsValue, fill=PLASTICS_EOLFATETYPES, group=Time))+
  #geom_bar(position="fill",stat = "identity")+
  geom_bar(position="stack", stat="identity")+
  geom_text_repel(aes(y = perc_label_y, label = paste0(round(ObsValue,1), "Mt (",round(100*per_ObsValue,1), "%)") ), size = 3) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )

df

# N
df %>% 
  ggplot(aes(x=Time,y=ObsValue, fill=PLASTICS_EOLFATETYPES, group=Time))+
  #geom_bar(position="fill",stat = "identity")+
  geom_bar(position="stack", stat="identity")+
  geom_text_repel(aes(y = label_y, label = paste0(round(ObsValue,1), "Mt (",round(100*per_ObsValue,1), "%)") ), size = 3) +
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )

df

###########   SI

i <- 7
(graph_name <- as.character(resultat$desc$id[i]))
as.character(resultat$desc$title[i])
head(resultat$data[[graph_name]])
colnames(resultat$data[[graph_name]])
browse_metadata(graph_name)
dim(resultat$data[[graph_name]])

table(resultat$data[[graph_name]]$LOCATION)


df <- resultat$data[[graph_name]] %>% 
  filter(LOCATION!="TOTAL") %>% 
  mutate( ObsValue = as.numeric(ObsValue)
          ,Time=as.numeric(Time))
df %>% 
  ggplot(aes(x=Time,y=ObsValue, col=LOCATION))+
  geom_line()+
  geom_text_repel(data=df %>% filter(Time==2018), aes(x=Time,y=ObsValue, label=LOCATION), size=2)+
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )


###########   NO

i <- 8
(graph_name <- as.character(resultat$desc$id[i]))
as.character(resultat$desc$title[i])
head(resultat$data[[graph_name]])
colnames(resultat$data[[graph_name]])
browse_metadata(graph_name)
dim(resultat$data[[graph_name]])
# [1] "Plastic waste in 2019"
#   LOCATION ObsValue    PLASTICS_APPLICATIONS PLASTICS_POLYMER


table(resultat$data[[graph_name]]$LOCATION)


df <- resultat$data[[graph_name]] %>% 
  filter(LOCATION!="TOTAL") %>% 
  mutate( ObsValue = as.numeric(ObsValue)
          ,Time=as.numeric(Time))
df %>% 
  ggplot(aes(x=Time,y=ObsValue, col=LOCATION))+
  geom_line()+
  geom_text_repel(data=df %>% filter(Time==2018), aes(x=Time,y=ObsValue, label=LOCATION), size=2)+
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )

###########   NO

i <- 9
(graph_name <- as.character(resultat$desc$id[i]))
as.character(resultat$desc$title[i])
head(resultat$data[[graph_name]])
colnames(resultat$data[[graph_name]])
browse_metadata(graph_name)
dim(resultat$data[[graph_name]])
# "Greenhouse gas emissions from plastics lifecycle"
# GREEN_HOUSE_GASES ObsValue PLASTIC_STAGE TIME_FORMAT Time 


table(resultat$data[[graph_name]]$GREEN_HOUSE_GASES)


df <- resultat$data[[graph_name]] %>% 
  filter(LOCATION!="TOTAL") %>% 
  mutate( ObsValue = as.numeric(ObsValue)
          ,Time=as.numeric(Time))
df %>% 
  ggplot(aes(x=Time,y=ObsValue, col=LOCATION))+
  geom_line()+
  geom_text_repel(data=df %>% filter(Time==2018), aes(x=Time,y=ObsValue, label=LOCATION), size=2)+
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )

###########   NO

i <- 10
(graph_name <- as.character(resultat$desc$id[i]))
as.character(resultat$desc$title[i])
head(resultat$data[[graph_name]])
colnames(resultat$data[[graph_name]])
browse_metadata(graph_name)
dim(resultat$data[[graph_name]])
# "Plastic leakage in 2019"
#LOCATION ObsValue    PLASTIC_CATEGORY PLASTIC_SOURCES   


unique(resultat$data[[graph_name]]$LOCATION)
table(resultat$data[[graph_name]]$PLASTIC_SOURCES,resultat$data[[graph_name]]$PLASTIC_CATEGORY)

resultat$data[[graph_name]] %>% 
  filter(PLASTIC_CATEGORY=="MACROPLASTICS") %>% data.frame()

df <- resultat$data[[graph_name]] %>% 
  filter(LOCATION!="TOTAL") %>% 
  mutate( ObsValue = as.numeric(ObsValue)
          ,Time=as.numeric(Time))
df %>% 
  ggplot(aes(x=Time,y=ObsValue, col=LOCATION))+
  geom_line()+
  geom_text_repel(data=df %>% filter(Time==2018), aes(x=Time,y=ObsValue, label=LOCATION), size=2)+
  theme(legend.position="bottom")+
  ggtitle(label =graph_name,subtitle = resultat$desc$title[resultat$desc$id==graph_name] )

