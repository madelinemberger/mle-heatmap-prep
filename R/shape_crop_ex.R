# Loop to crop shapes to the Offshore

# Filter out shapes that are in the offshore area, save, and grab the resp_ids

for (i in seq_along(combined_shapes)){
  
  #i = 17
  #if (i == 9 | i == 11) next
  name <- substr(basename(combined_shapes[i]),1,nchar(basename(combined_shapes[i]))-4)
  
  shape <- read_sf(combined_shapes[i]) %>% 
    st_make_valid()
  
  # drop invalid shapes 
  shape_valid <- shape %>% 
    mutate(
      valid = st_is_valid(geometry)
    ) %>% 
    filter(valid != FALSE)
  
  n_drop <- nrow(shape) - nrow(shape_valid)
  cat(n_drop,"shapes were dropped due to invalid geometries")
  #shape_off <- st_overlaps(shape,offshore_area)
  
  # do they intersect? note, this determines if x and y share ANY space so presumably includes partial overlaps
  
  shape_test <- shape_valid %>% 
    mutate(offshore_tf = st_intersects(.,offshore_area, sparse = FALSE))
  
  # filter out 
  shape_offsh <- shape_test %>% 
    filter(offshore_tf == TRUE) %>% 
    mutate(
      sec = name
    )
  
  shape_insh <- shape_test %>% 
    filter(offshore_tf == FALSE)
  
  # ids 
  
  offshore_ids <- data.frame(ids = unique(shape_offsh$resp_id))
  
  offshore_sec_ids <- shape_offsh %>% 
    as.data.frame(
      dplyr::select(resp_id,sector)
    )
  
  inshore_ids <- data.frame(ids = unique(shape_insh$resp_id))
  
  # save shapes
  if(nrow(shape_offsh) != 0){
    
    st_write(shape_offsh,
             dsn = file.path(paste0(maldives_dir,"/outputs/shapefiles/combined_shps/offshore/",name,".shp")),
             delete_dsn = T)
    
  }else if(nrow(shape_insh) != 0){
    
    st_write(shape_insh,
             dsn = file.path(paste0(maldives_dir,"/outputs/shapefiles/combined_shps/inshore/",name,".shp")),
             delete_dsn = T)
  }else{
    print ("no shape here")
  }
  
  
  
  
  
  if(i == 1){
    offshore_ids_df = offshore_ids
    inshore_ids_df = inshore_ids
  }else{
    offshore_ids_df <- offshore_ids_df %>% 
      rbind(offshore_ids)
    
    inshore_ids_df <- inshore_ids_df %>% 
      rbind(inshore_ids)
  }
  
}
