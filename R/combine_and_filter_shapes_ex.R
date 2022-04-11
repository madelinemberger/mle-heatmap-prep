## Join SeaSketch and Maptionnaire Shapes function 
## Maddie Berger
## December 2021


#### Summary
# Rbinds shapes from Seasketch and Maptionnaire into single feature classes per sector. These are then saved to a folder and exported. 
# note; this only works if the Seasketch and Maptionnaire individual files are saved with names that ensure matching sectors are in the
# same place when listed alphabetically. 

### Inputs: 
# List of file names to search for
# File paths to each folder where the individual shapes are stored 

## BUILD NOTES:
# need to fix the naming scheme so that you only get ONE per name 
# erase functio in workflow?


# create erase function

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

ss_list <- list.files(file.path(paste0(maldives_dir,"/outputs/shapefiles/seasketch/",
                                       report_date)),
                      pattern = ".shp",
                      full.names = T)

map_list <- list.files(file.path(paste0(maldives_dir,"/outputs/shapefiles/maptionnaire/",
                                        report_date)),
                       pattern = ".shp",
                       full.names = T)

# start function
join_ss_map_shapes <- function(ss_file_list,map_file_list, report_date){
  #report_date = "2022-03-16"
  #get files 
  
  
  # get clipping poly
  
  #maldives <- read_sf(file.path(maldives_dir,"inputs/mdv_osm_wgs84.shp")) %>% 
  #filter(FID %in% c(554433,555025,121546))
  #note this one needs to be transformed  
  #mdv_old <- read_sf(file.path("~/github/survey_results/maldives/inputs/shapefiles/static-spatial/AdministrativeIsland.shp")) %>% 
  #  st_transform(crs = 4326)
  #mdv_old_k <- mdv_old %>% 
  #filter(atoll %in% c("Kaafu","Daviyani"))
  #filter(admin2Name %in% c("Kaafu Atoll","Male")) %>% 
  #st_transform(crs = 4326)
  
  
  # create df of maptionnaire names 
  
  filter_list <- data.frame(
    filter_names = lapply(map_list, basename) %>% unlist()) %>%  
    filter(!str_detect(filter_names,"map"))
  #filter(!str_detect(filter_names,"tuna"))
  
  # check to make sure this is consistent since the file paths are dependent on this 
  print(report_date)
  
  dir.create(paste0(maldives_dir,"/outputs/shapefiles/combined_shps/",report_date))
  
  for (i in seq_along(filter_list$filter_names)){
    
    #i = 6
    #i = 2
    print(i)
    detect_name = filter_list$filter_names[i]
    
    #filter filepaths for the one you want
    
    ss_filepath <- ss_list[grepl(detect_name, ss_list)]
    map_filepath <- map_list[grepl(detect_name, map_list)]
    
    # read in the shape and select columns you need for heatmap
    ss_shape <- read_sf(file.path(ss_filepath)) %>% 
      dplyr::select(resp_id,weight,atoll,island,sector)
    # rbind shape 
    
    map_shape <- read_sf(file.path(map_filepath)) %>% 
      dplyr::select(resp_id,weight,atoll,island,sector)
    
    comb_shape <- rbind(ss_shape,map_shape) %>% 
      st_make_valid()
    
    #Grab K - think about how to loop this for atolls and islands
    
    # comb_shp_k <- comb_shape %>% 
    #   filter(atoll == "K")
    
    # save shapes without crop 
    st_write(comb_shape, 
             dsn = file.path(paste0(
               maldives_dir,"/outputs/shapefiles/combined_shps/",report_date,"/",substr(detect_name,1,9),".shp")),
             delete_dsn = T)
    
    
    
  }
  
  # figure out way here to filter out the rest of the ss shapes and process those
  
  ss_only_shapes <- setdiff(names_compare$ss_names, names_compare$map_names)
  
  print(ss_only_shapes)
  
  for (i in seq_along(ss_only_shapes)){
    
    #i = 11
    print(i)
    detect_name = ss_only_shapes[i]
    
    #filter filepaths for the one you want
    
    ss_filepath <- ss_list[grepl(detect_name, ss_list)]
    #map_filepath <- map_list[grepl(detect_name, map_list)]
    
    # read in the shape and select columns you need 
    ss_shape <- read_sf(file.path(ss_filepath)) %>% 
      dplyr::select(resp_id,weight,atoll,island, sector)
    
    # save shapes without crop 
    st_write(ss_shape, 
             dsn = file.path(paste0(
               maldives_dir,"/outputs/shapefiles/combined_shps/",report_date,"/",substr(detect_name,1,9),".shp")),
             delete_dsn = T)
    
  }
  
  
}
