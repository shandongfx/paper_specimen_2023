library("circlize")
plotNetwork = function(input_path,path_figureOutput,flag_self="NOTSET",version="old"){
  dir.create(path_figureOutput)
  df_continent_freq = readRDS(input_path)  
df_continent_freq = as.data.frame(df_continent_freq)
df_continent_freq$origin_geonames_Continent = gsub("_"," ",df_continent_freq$origin_geonames_Continent)
df_continent_freq$publishing_geonames_Continent = gsub("_"," ",df_continent_freq$publishing_geonames_Continent)
df_continent_freq = subset(df_continent_freq,origin_geonames_Continent!="Antarctica" & publishing_geonames_Continent!= "Antarctica")
include_self=T
if(!include_self){
df_continent_freq = subset(df_continent_freq,
                           as.character(origin_geonames_Continent)
                           !=
                             as.character(publishing_geonames_Continent) )
}
circos.clear()
outfile = gsub(".rds",paste0("network_continent","self_",flag_self,".pdf"),
               basename(input_path))
df_continent_freq$N = df_continent_freq$N/100000
df_continent_freq$origin_geonames_Continent = gsub(" ","_",df_continent_freq$origin_geonames_Continent)
df_continent_freq$publishing_geonames_Continent = gsub(" ","_",df_continent_freq$publishing_geonames_Continent)

if(version=="new2"){ 
  df_continent_freq$publishing_geonames_Continent = paste0(df_continent_freq$publishing_geonames_Continent,"1")  
  grid.col = c(South_America = "#3C5488FF",
               North_America ="#4DBBD5FF",
               Europe = "#E64B35FF", 
               Oceania = "#F39B7FFF",
               Asia = "#00A087FF", 
               Africa = "#8491B4FF",
               South_America1 = "#3C5488FF",
               North_America1 ="#4DBBD5FF",
               Europe1 = "#E64B35FF", 
               Oceania1 = "#F39B7FFF",
               Asia1 = "#00A087FF", 
               Africa1 = "#8491B4FF")  
  pdf(paste0(path_figureOutput,"_____",outfile),
      width = 5,height = 5)
  
  circos.clear()
  group = structure(c("1","1","1","1","1","1",
                      "2","2","2","2","2","2" ),
                    names = c( "South_America", "Oceania" ,"North_America", "Europe", "Asia", "Africa"  ,      
                               "South_America1","Oceania1","North_America1","Europe1","Asia1","Africa1" ))
  circos.par(start.degree = -5)
  chordDiagram( df_continent_freq,
                group = group,
                link.sort = TRUE, link.decreasing = TRUE,
                grid.col = grid.col,
                diffHeight =   mm_h(0),
                target.prop.height = mm_h(0),
                directional = 1, 
                transparency = 0.2,
                direction.type = c("diffHeight", "arrows"),
                link.arr.type = "big.arrow"
  )
  abline(h = 0, lty = 2, col = "#00000080")  
  dev.off()
  circos.clear()
} }

list_input = list.files("data_2020/3_matrix_20210422updated_v2_geovalid/",pattern="continent_df_media_.*rds$",full.names = T)
i=1
for(i in 1: length(list_input)){
  plotNetwork(input_path = list_input[i],
              path_figureOutput ="figure_2020/v20210630_geovalid/",
              version="new2")
}
list_input = list.files("data_2020/3_matrix_20210802_lift2Filter_full/",pattern="continent_df_media_.*rds$",full.names = T)
i=1
for(i in 1: length(list_input)){  
  plotNetwork(input_path = list_input[i],
              path_figureOutput ="figure_2020/v20210802_full/",
              version="new2")
}
list_input = list.files("data_2020/3_matrix_20211015_lift2Filter_full_fixMedia/",pattern="continent_df_media_.*rds$",full.names = T)
i=1
for(i in 1: length(list_input)){
  plotNetwork(input_path = list_input[i],
              path_figureOutput ="figure_2020/v20211015_full_fixMedia/",
              version="new2")
}