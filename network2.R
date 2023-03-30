library(data.table)
library(raster)
library(igraph)
library(ggplot2)
gadm = shapefile("../../GISDATA/DIVA_GISDATA/gadm36_levels.shp/gadm36_0.shp")
head(gadm@data)
names(gadm@data) = paste0("GADM_",names(gadm@data))
lv1 <- gadm
lv1_p = rgeos ::gCentroid(lv1,byid=TRUE)
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = lv1,
                               fill = "gray", color = "gray20",
                               size = 0.15)
plotMap = function(input_path,path_figureOutput){

if(T){  
  m2= readRDS(input_path)
  outfile = gsub(".rds",paste0("network_country","_v4",".tiff"),
                 basename(input_path))
  dir.create(path_figureOutput)  
  diag(m2) <- 0
  m2 = log10(m2)
  hist(  as.vector(m2) ,breaks=20 )
  m2[m2==-Inf] = 0
  temp_m2 = m2
  temp_m2 = temp_m2[temp_m2>0]
  my_thd = quantile(temp_m2,probs=0.9)   
  my_thd = sort(temp_m2,decreasing = T)[400] 
  cat("threshold",my_thd,"\n")
  m2[m2 < my_thd] = 0  
  net <- graph_from_adjacency_matrix(as.matrix(m2),
                                     weighted=TRUE)
  E(net)
}

if(T){
net_iso = V(net)$name 
length(net_iso)
net_label_df = data.frame(net_id = 1: length(net_iso),
                          net_iso = net_iso)
geonames <- fread("data/geonames/countryInfo.txt",sep = "\t",quote = "",
                  skip=50,
                  na.strings="",
                  select=c("Country","Continent","#ISO","ISO3"))
names(geonames) = paste0("geonames_",names(geonames))
geonames = as.data.frame(geonames)
net_label_df_updated = merge(net_label_df,geonames,
                             by.x="net_iso",by.y="geonames_#ISO",
                             all.x=TRUE)
net_label_df_updated = merge(net_label_df_updated,
                             gadm@data,
                             by.x="geonames_ISO3",by.y="GADM_GID_0",
                             all.x=TRUE)
net_label_df_updated= net_label_df_updated[ order(net_label_df_updated$net_id)  , ] 
identical( net_label_df_updated$net_id , 1: length(net_iso) )
identical( as.character(net_label_df_updated$net_iso) , net_iso )
V(net)$name = net_label_df_updated$geonames_Country
}

sum_inst_country <- colSums(m2) 
flag_layout <- "map"
if(flag_layout=="network"){
flag_manualBlue_red = FALSE
if(flag_manualBlue_red){
col_hot_num <- c(173,27,27)
col_cold_num <- c(8,47,145)

e_alpha <- E(net)$weight
e_alpha <- (e_alpha- min(e_alpha) ) / (max(e_alpha)-min(e_alpha))
e_alpha <- e_alpha* 255 
E(net)$color <- rgb(red=col_hot_num[1],
                    green=col_hot_num[2],
                    blue=col_hot_num[3],
                    alpha=e_alpha,
                    maxColorValue = 255) 
e_alpha <- (sum_inst_country+0.1) 
e_alpha <- (e_alpha- min(e_alpha) ) / (max(e_alpha)-min(e_alpha))
e_alpha <- e_alpha* 255 
V(net)$color <- rgb(red=col_hot_num[1],
                    green=col_hot_num[2],
                    blue=col_hot_num[3],
                    alpha=e_alpha,
                    maxColorValue = 255)  
}

l <- layout_randomly(net)
l <- layout_in_circle(net)
l <- layout_on_sphere(net)

plot(net,
     vertex.label.cex=sum_inst_country/100+1,
     edge.arrow.size=0.5,
     edge.width=E(net)$weight/2,
     edge.color=E(net)$color,
     vertex.frame.color="white",
     vertex.size=sum_inst_country/10+1,
     vertex.label.font=sum_inst_country/10+1,
     layout=l)
}

if(flag_layout=="map"){
lv1_p <- data.frame(lv1_p)
lv1_p$GADM_NAME_0 <- lv1$GADM_NAME_0
lv1_p$GADM_GID_0 <- lv1$GADM_GID_0
nrow(lv1_p) 
lv1_p_update = merge(lv1_p,net_label_df_updated,
                     by.x="GADM_GID_0",by.y="geonames_ISO3",
                     all.y=T,all.x=F)
nrow(lv1_p_update)
head(lv1_p_update)
lv1_p_update <- lv1_p_update[order(lv1_p_update$net_id),]

if(  identical( lv1_p_update$geonames_Country ,  V(net)$name )         ){
  print("good")
  
  lv1_p_update$x[lv1_p_update$GADM_GID_0=="USA"] = -98
  lv1_p_update$y[lv1_p_update$GADM_GID_0=="USA"] = 38
  
  my_layout <-   lv1_p_update[ c("x","y")] 
  
  edge_attributes = as_data_frame(net)
  edge_attributes$local_id = 1:nrow(edge_attributes)  
  edge_attributes = merge(edge_attributes,lv1_p_update,
                          by.x="to",by.y="geonames_Country",all.x=T) 
  edge_attributes = edge_attributes[order(edge_attributes$local_id),]
  edge_attributes$geonames_Continent[edge_attributes$geonames_Continent == "NA"]= "NA "  
  edge_attributes$edge_color = factor(edge_attributes$geonames_Continent)
  levels(edge_attributes$edge_color) = c("#8491B4FF",   #Africa 
                                         "#4DBBD5FF", #Asia   
                                         "#E64B35FF", #Europe       
                                         "#F39B7FFF", #N. America   
                                         "#00A087FF", #Australasia  # correct
                                         "#3C5488FF" #S. America  
                                         )  
  E(net)$geonames_Continent <- edge_attributes$geonames_Continent  
  my_layout <- ggraph::create_layout(net, layout = "manual",
                                     node.positions = my_layout, 
                                     circular = FALSE)
}
library(ggraph)
library(igraph)
  map_net <- ggraph(my_layout) + 
    country_shapes+ 
    theme(legend.position = c(0.1,0.7),
          legend.text=element_text(size=20),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),   # background while
          axis.line = element_line(colour = "white"))+
    geom_edge_hive(arrow = arrow(angle = 20,
                                 length = unit(0.2, 'cm'),
                                 type="open"), 
                   lineend="round",
                   aes(width = 10^weight,  
                       alpha = weight,
                       color=factor(geonames_Continent)                     
                   ) ,  
                   spread=1) + 
    scale_edge_width(range = c(0.5,2),guide = "none")+ 
    scale_edge_alpha(range=c(0.4,0.9),guide = "none")+ 
    scale_edge_color_manual( values = 
                               c("#8491B4FF", 
                                 "#00A087FF", 
                                 "#E64B35FF",  
                                 "#4DBBD5FF",
                                 "#F39B7FFF",
                                 "#3C5488FF",  
                                 "black"),
                             name=" "                             
    )+ 
    geom_node_point(color=NA,
                    size=1     )
  ggsave(plot = map_net,
         paste0(path_figureOutput,
                outfile),
         width = 50,height = 25,units = "cm",dpi = 300)
}}

list_input = list.files("data_2020/3_matrix_20210422updated_v2_geovalid/",pattern="collection_matrix_media_.*rds$",full.names = T)
i=1
for(i in c(3,7)  ){ 
  plotMap(input_path = list_input[i],
              path_figureOutput ="figure_2020/v20210630_geovalid/")
}
list_input = list.files("data_2020/3_matrix_20210422updated_v2_full/",pattern="collection_matrix_media_.*rds$",full.names = T)
i=1
for(i in c(3,7)  ){ 
  plotMap(input_path = list_input[i],
          path_figureOutput ="figure_2020/v20210630_full/")
}

list_input = list.files("data_2020/3_matrix_20210802_lift2Filter_full/",pattern="collection_matrix_media_.*rds$",full.names = T)
i=1
for(i in 1: length(list_input)){
  plotMap(input_path = list_input[i],
          path_figureOutput ="figure_2020/v20210802_full/")
}
