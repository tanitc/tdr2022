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
    url <- sprintf("https://stats.oecd.org/OECDStat_Metadata/ShowMetadata.ashx?Dataset=%s&Lang=en", i)
    simple <- read_html(url)
    blocs<-simple %>%
      html_element("body") %>%
      html_elements(".GroupDiv")
    
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

resultat$config <-vector("list", length(resultat$data))
names(resultat$config)<- names(resultat$data)

save(resultat,file="data/resultat.RData")

###########   NO
