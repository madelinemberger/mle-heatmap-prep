# Clean shapefiles from SeaSketch
## August 2021
## Maddie Berger


# This script will clean the shapefiles and join the respondent info with the SeaSketch shapes, as attributes. 
# It is a loop that contains two nested if statements

# Once cleaned, there should be a new folder in the output directory with the date, and within that folder shapefile for each sector with the following attributes:
## respondent ID
## name + facilitator name
## email
## number of people in response
## value

# This script also pulls out species and gear lists from the fishing shapefiles, which we use to create the weekly update reports. These are saved to 
# an "int" folder, short for interim

###########################################################

# get list of shapefile files

ss_raw_shps <- list.files(file.path(paste0(maldives_dir,"/inputs/shapefiles/seasketch/",report_date)),
                          pattern = ".shp", 
                          full.names = T)


# get respondent info to join to the shapes

ss_info_shapes<- read_csv(file.path(paste0(maldives_dir,"/outputs/int/ss_shapes_info_",report_date,".csv"))) # ok this needs to be all of them 

#create dated folder to save everything to 
dir.create(paste0(maldives_dir,"/outputs/shapefiles/seasketch/",report_date))


# loop through each shape

foreach(f = ss_raw_shps) %dopar% {
  
  #f = ss_raw_shps[17] # for testing 
  
  name = str_to_lower(str_extract(basename(f),"^[^_]*_[^_]*")) # subsets the basename up to the second underscore to create a text name for saving
  
  # read in shape

  shp <- read_sf(f) %>%
    clean_names() %>% 
    filter(updated_at > launch_date) %>% 
    left_join(., ss_info_shapes, by = "resp_id")
  

  # create test that skips empty shapes
    
  if (nrow(shp) != 0){
    
    shp_info <- shp %>% 
      filter(!is.na(practice)) %>% 
      #filter(date > "2021-11-30")%>%
      #filter(date < "2021-12-02") %>% 
      dplyr::select(
        -name.x,
        -id,
        #-user_id,
        -completed,
        -sc_id, 
        sector = sc_name,
        -fid,
        -p_attrs,
        -updated_at,
        -created_at, 
        -is_coll,
        -weight, # this field is old, from when the survey had the 100 pennies instead of slider
        -user_attrs,
        -ip_addr
      ) %>% 
      mutate(
        value = as.numeric(value),
        number_of_ppl = as.numeric(number_of_ppl)
      )
    
    # normalize weight: find total points used by one respondent, then divide each response by that amount. Multiply by number of people
    
    ### find total points used by one respondent 
    
    pts_total <- shp_info %>% 
      as.data.frame() %>%
      mutate(
        value = as.numeric(value) #need to change this once everything is called "value"
      ) %>%  
      group_by(resp_id) %>% 
      summarize(
        total_pts = sum(value)
      )
    
    ### join to shape, and then normalize and multiply 
    shp_weight_norm <- inner_join(shp_info, pts_total, by = "resp_id") %>% 
      mutate(
        value_norm = round((value / total_pts)*100, digits = 0), #normalize
        weight = value_norm * as.numeric(number_of_ppl) #multiply by the number of people if a group response. 
      )
    
    # create lists of gear to create mini loops in to make each type of shapefile
    #gear_comm_art <- c("")
    
    # clean up remaining columns, save separate
    
    if(str_detect(name, "tuna") == TRUE | str_detect(name, "artisanal") == TRUE){
      
      shp_clean <- shp_weight_norm %>% 
        #separate(species_tu, into = c("species_1","species_2"), sep = ",")# maybe not necessary
        rename(
          gear = starts_with("gear")
        ) %>% 
        mutate(
          species = str_trim(str_replace_all(species,"[^[:alpha:]]", " ")), #to filter out gear types, just use str_detect
          gear = str_trim(str_replace_all(gear,"[^[:alpha:]]", " "))
        ) %>% 
        dplyr::select(
          resp_id,
          weight,
          name = user_name,
          email = user_email,
          practice,
          facilitated,
          resp_name,
          facilitator_name,
          number_of_ppl,
          age,
          gender,
          atoll,
          island,
          date,
          sector
        )
      # filter into specific shapes and then save to new folder that a different script will draw from. save with new column specifying which shape it is for
      
      sp_list <- shp_weight_norm %>% 
        #separate(species_tu, into = c("species_1","species_2"), sep = ",")# maybe not necessary
        mutate(
          species = str_trim(str_replace_all(species,"[^[:alpha:]]", " ")), #to filter out gear types, just use str_detect
          #gear = str_trim(str_replace_all(gear,"[^[:alpha:]]", " "))
        ) %>% 
        as.data.frame() %>% 
        dplyr::select(
          resp_id,
          name = user_name,
          resp_name,
          facilitator_name,
          part_full_time,
          number_of_ppl,
          atoll,
          island,
          species,
          date
        )
      
      gear_list <- shp_weight_norm %>% 
        rename(
          gear = starts_with("gear")
        ) %>% 
        #separate(species_tu, into = c("species_1","species_2"), sep = ",")# maybe not necessary
        mutate(
          species = str_trim(str_replace_all(species,"[^[:alpha:]]", " ")), #to filter out gear types, just use str_detect
          gear = str_trim(str_replace_all(gear,"[^[:alpha:]]", " "))
        ) %>% 
        as.data.frame() %>% 
        dplyr::select(
          resp_id,
          name = user_name,
          resp_name,
          facilitator_name,
          part_full_time,
          atoll,
          island,
          gear,
          date
        )
      
      # save lists 
      
      write_csv(sp_list,
                file.path(paste0(maldives_dir,"/outputs/int/",name,"_sp_list.csv")))
      
      write_csv(gear_list,
                file.path(paste0(maldives_dir,"/outputs/int/",name,"_gear_list.csv")))
      
    }else if(str_detect(name, "recreational") ==TRUE | str_detect(name, "bait") == TRUE){
      
      shp_clean <- shp_weight_norm %>%
        mutate( #to filter out gear types, just use str_detect
          gear = str_trim(str_replace_all(gear,"[^[:alpha:]]", " "))
        ) %>% 
        dplyr::select(
          resp_id,
          weight,
          name = user_name,
          email = user_email,
          practice,
          facilitated,
          resp_name,
          facilitator_name,
          number_of_ppl,
          age,
          gender,
          atoll,
          island,
          date,
          sector
        )
      
      # select the id columns, count and save lists 
      
      list <- shp_weight_norm %>%
        mutate( #to filter out gear types, just use str_detect
          gear = str_trim(str_replace_all(gear,"[^[:alpha:]]", " "))
        ) %>% 
        as.data.frame() %>% 
        dplyr::select(
          resp_id,
          name = user_name,
          resp_name,
          facilitator_name,
          part_full_time,
          number_of_ppl,
          atoll,
          island,
          gear,
          date
        )
      
      # save list 
      
      write_csv(list,
                file.path(paste0(maldives_dir,"/outputs/int/",name,"_gear_list.csv")))
      
    }else if(str_detect(name, "boat") == TRUE){
      
      shp_clean <- shp_weight_norm %>% 
        mutate(
          type = str_trim(str_replace_all(charter_ty,"[^[:alpha:]]", " ")),
          length = str_trim(str_replace_all(charter_le,"[^[:alpha:]]", " "))
        ) %>% 
        dplyr::select(
          resp_id,
          weight,
          name = user_name,
          email = user_email,
          practice,
          facilitated,
          resp_name,
          facilitator_name,
          number_of_ppl,
          age,
          gender,
          atoll,
          island,
          date,
          sector
        )
      
      list <- shp_weight_norm %>% 
        mutate(
          type = str_trim(str_replace_all(charter_ty,"[^[:alpha:]]", " ")),
          length = str_trim(str_replace_all(charter_le,"[^[:alpha:]]", " "))
        ) %>% 
        as.data.frame() %>% 
        dplyr::select(
          resp_id,
          user_name,
          resp_name,
          facilitator_name,
          type,
          length,
          starts_with("vessel"),
          atoll,
          island,
          date
        ) %>% 
        mutate(
          vessel_id = ifelse(is.na(vessel_id.y), vessel_id.x,vessel_id.y)
        ) %>% 
        dplyr::select(-vessel_id.x,-vessel_id.y) %>% 
        distinct() %>% 
        mutate(
          length = case_when(
            str_detect(length,"Multi") == T ~ "Multi Day",
            TRUE ~ "Day Trips"
          )
        )
      
      write_csv(list,
                file.path(paste0(maldives_dir,"/outputs/int/ss_",name,"_vessel_list.csv")))
    }else{
      
      shp_clean <- shp_weight_norm %>%
        dplyr::select(
          resp_id,
          weight,
          name = user_name,
          email = user_email,
          practice,
          facilitated,
          resp_name,
          facilitator_name,
          number_of_ppl,
          age,
          gender,
          atoll,
          island,
          date,
          sector
        )
    }
    
    
    
    # save shapes into the date folder using the name we extracted at the very beginning
    
    st_write(shp_clean, 
             dsn = file.path(paste0(
               maldives_dir,"/outputs/shapefiles/seasketch/",report_date,"/",name,".shp")),
             delete_dsn = T)
    
    
  } else{
    print(paste("We have not gotten any responses for ",name," yet."))
  }
  
  #print(i)
  
}

# end loop 

# Read in fishing lists, commercial and recreational, and artisanal and assign "pelagic" or "reef" to each response

tuna_species <- read_csv(file.path(maldives_dir,"/outputs/int/tuna_fishing_sp_list.csv")) %>% 
  mutate(
    species = gsub("\\s+"," ",species)
  ) %>% 
  mutate(
    type = "Pelagic",
    sector_id = "Tuna"
  )


nontuna_species <- read_csv(file.path(maldives_dir,"/outputs/int/nontuna_fishing_sp_list.csv")) %>%
  #rbind(.,test_df, test_df_2) %>% 
  mutate(
    species = gsub("\\s+"," ",species),
    type = case_when(
      species %in% "Billfish" ~ "Pelagic",
      str_detect(species, "Billfish") == T ~ "Both",
      species == "Billfish Other" ~ "Pelagic",
      TRUE ~ "Reef"
    ),
    sector_id = "Non-Tuna"
  )


# Bind them all together

all_commercial_fishers <- rbind(tuna_species, nontuna_species) %>% 
  distinct()

write_csv(all_commercial_fishers,
          file.path(maldives_dir,"/outputs/int/ss_commercial_fishers.csv"))


# repeat for artisanal (giving it a type)

art_sp <- read_csv(file.path(maldives_dir,"outputs/int/artisanal_fishing_sp_list.csv")) %>% 
  mutate(
    type = case_when(
      str_detect(species, "Billfish ") == T | str_detect(species, " Neritic Tuna") == T  ~ "Both",
      species == "Billfish" | species == "Neritic Tuna" ~ "Pelagic",
      species == "Other" ~ "Other",
      TRUE ~ "Reef"
    ),
    sector_id = "Artisanal"
  ) %>% 
  mutate(
    type = case_when(
      species == "Billfish   Neritic Tuna" & type == "Both" ~ "Pelagic",
      species == "Billfish   Neritic Tuna   Other" ~ "Pelagic",
      TRUE ~ type
    )
  )

write_csv(art_sp,
          file.path(maldives_dir,"/outputs/int/ss_artisanal_fishers.csv"))

# END SCRIPT