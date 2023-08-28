# Zooniverse analysis-----------------------------------------------------------
rm(list=ls())

# Load libraries----------------------------------------------------------------
library(tidyverse)
library(lubridate)  
library(dplyr)

# Read in data------------------------------------------------------------------
zoo = data.table::fread("data/zoo_merged.csv", data.table = FALSE)

# Filter out data not useful to analyses----------------------------------------
## Filter out photos with 2+ animals
test = zoo %>% 
  select(expertID, photoName, subject_id) %>% 
  group_by(photoName, subject_id) %>% 
  count(expertID) %>% 
  select(-n)

UWI_db = zoo %>% 
  select(expertID, photoName, subject_id) %>% 
  group_by(photoName, subject_id) %>% 
  count(expertID) %>% 
  filter(length(photoName) >= 2) %>% 
  select(-n)

## These are photoNames which should be removed from zooniverse data because they have
## 2+ species in them and zooniverse does not distinguish these
photos_to_rm = UWI_db %>% 
  ungroup(.) %>% 
  distinct(photoName) 

total_photos = test %>% 
  ungroup(.) %>% 
  distinct(photoName)

(length(total_photos$photoName)-length(photos_to_rm$photoName))/length(total_photos$photoName)


# Remove from entire dataset 
zoo = zoo[-which(zoo$photoName %in% photos_to_rm$photoName),]

# Remove tags after n = 15
# n_seen = # of people who viewed each photo
zoo = zoo %>%
  group_by(subject_id) %>%
  summarise(n_seen = length(subject_id)) %>%
  left_join(., zoo)

zoo = zoo %>% 
  filter(n_seen <= 15)

# fix weights which got incorrectly joined, drop here first
zoo = zoo %>% 
  select(-animal_weight)

# Tidy up data------------------------------------------------------------------
# Match species naming between Zooniverse choice and experts
unique(zoo$choice)
zoo$choice <- trimws(zoo$choice)

unique(zoo$expertID)
zoo$expertID <- trimws(zoo$expertID)

# Collapse species groups
zoo <- zoo %>% mutate(expertID = fct_collapse(expertID, 
                                              "Rat spp." = c("Rat spp.", "Brown Rat"),
                                              "weasel" = c("Least weasel", "Long-tailed weasel"),
                                              "Bird" = c("Bird","American robin", "Mallard duck"),
                                              "Eastern gray squirrel" = c("Eastern gray squirrel", "melanistic gray squirrel"),
                                              "Human" = c("Golf cart", "Human")))

zoo <- zoo %>% mutate(choice = fct_collapse(choice, 
                                            "Empty" = c("nothinghere"),
                                            "Eastern cottontail rabbit" = c("rabbit"),
                                            "Human" = c("human", "mower"),
                                            "Domestic dog" = "dogdomestic",
                                            "Eastern gray squirrel" = c("squirrelmelanisticgray", "squirrelgray"),
                                            "Raccoon" = "raccoon",
                                            "Mouse" = "mouse",
                                            "White-tailed deer" = "deer",
                                            "Rat spp." = "rat",
                                            "Virginia opossum" = "opossum",
                                            "Bird" = c("bird"),
                                            "Fox squirrel" = "squirrelfox",
                                            "Woodchuck" = "woodchuckakagroundhog",
                                            "Gray fox" = "foxgray",
                                            "Domestic cat" = "catdomestic",
                                            "American mink" = "mink",
                                            "Red fox" = "foxred",
                                            "Striped Skunk" = "skunk",
                                            "North american beaver" = "beaver",
                                            "Eastern chipmunk" = "chipmunk",
                                            "Coyote" = "coyote",
                                            "Flying squirrel (cannot ID)" = "squirrelflying",
                                            "Squirrel (cannot ID)" = "treesquirrelbwimage"))
# Drop columns which don't match
zoo = zoo %>% 
  filter(expertID != "Unknown")

zoo = zoo %>% 
  filter(expertID != "Snake")

# Add correct model weight back in for modeling---------------------------------
animal_weight = read_csv("animal_weights.csv", col_names = TRUE)

zoo = left_join(zoo, animal_weight, by = "expertID", relationship = "many-to-many")

# Drop old "avid_user" classification and tighten to folks in 95 percentile

zoo = zoo %>% 
  select(-avid_user)

avid_user = zoo %>% 
  group_by(user_id) %>% 
  count(user_id) %>% 
  mutate(avid_user = ifelse(n >= quantile(.$n, probs = .95), 1, 0)) %>% 
  rename(., user_tag_count = n)

zoo = left_join(zoo, avid_user, by = "user_id")

# Add column of the number of species identified for each subject_id
zoo = zoo %>% 
  group_by(retired.id) %>%
  summarise(unique.choice = length(unique(choice))) %>% #unique.choice = # of species ppl tagged an image
  left_join(., zoo)

# UWI Zooniverse Retirement Rules-----------------------------------------------
# If subject is in the gold standard group (ID'ed by staff), don't retire it
# If 7 users have annotated the same animal, retire it (flagged)
# If anyone has annotated a human, retire it.
# If anyone has annotated the subject as ‘Report this photo’, retire it.
# If the first three users annotated the subject as blank, retire it (Empty)
# If 5 users annotated the subject as blank, retire it (nothing_here)
# If the classification count has reached 15, retire it.
# Else, don't retire it.

# Since we want to analyze the success of these retirement classes, we should remove tags that come after
# photos have been retired. We can do this by removing rows which have been created after 
# the retired.retired_at column

zoo = zoo %>% 
  mutate(after_retirement = ifelse(created_at > retired.retired_at, 1, 0))

# Let's make functions to recreate zooniverse's retirement classes--------------
zoo_list = split(zoo, factor(zoo$subject_id))

# Retire if first three tags are empty
first_3_empty <- function(sp_tag){
  if(sum(sp_tag[1:3] == "Empty") == 3){
    return(
      list(
        classification = "Empty", 
        retired = TRUE
      )
    )
  } else {
    return(
      list(
        classification = NA,
        retired = FALSE
      )
    )
  }
}

# Retire once 5 users tag empty 
five_empty <- function(sp_tag){
  unq_sp <- unique(sp_tag)
  finished_loc <- rep(NA, length(unq_sp))
  for(i in 1:length(unq_sp)){
    where_sp <- cumsum(
      sp_tag == unq_sp[i]
    )
    if(7 %in% where_sp & unq_sp[i] != "Empty"){
      finished_loc[i] <- which(where_sp == 7)[1]
    }
    if(5 %in% where_sp & unq_sp[i] == "Empty"){
      finished_loc[i] <- which(where_sp == 5)[1]
    }
  }
  if("Empty" %in% unq_sp){
    where_empty <- which(unq_sp == "Empty")
    if(!is.na(finished_loc[where_empty])){
      if(min(finished_loc, na.rm = TRUE) == finished_loc[where_empty]){
        return(
          list(
            classification = "Empty",
            retired = TRUE
          )
        )
      } else {
        return(
          list(
            classification = NA,
            retired = FALSE
          )
        )
      }
    } else {
      return(
        list(
          classification = NA,
          retired = FALSE
        )
      )
    }
    
  } else {
    return(
      list(
        classification = NA,
        retired = FALSE
      )
    )
  }
}



# Retire once 7 users tag the same species  
any_sp_7 <- function(sp_tag){
  unq_sp <- unique(sp_tag)
  tag_finished <- sapply(
    1:length(unq_sp),
    function(x){
      where_sp <- cumsum(
        sp_tag == unq_sp[x]
      )
      if(7 %in% where_sp){
        return(which(where_sp == 7)[1])
      } else {
        return(NA)
      }
    }
  )
  if(any(!is.na(tag_finished))){
    tag_finished[is.na(tag_finished)] <- 999
    to_return <- which(
      tag_finished == min(tag_finished)
    )
    return(
      list(
        classification = as.character(
          unq_sp
        )[to_return],
        retired = TRUE
      )
    )
  }else{
    return(
      list(
        classification = NA,
        retired = FALSE
      )
    )
  }
  
}

# Retire once 15 users tag a photo
class_count <- function(sp_tag, expert){
  my_count <- table(sp_tag)
  if(sum(my_count) >=15){
    my_count <- sort(
      my_count,
      decreasing = TRUE
    )
    if(sum(my_count == max(my_count)) == 1){
      sp <- names(my_count)[1]
    }else{
      sp <- expert
    }
    
    return(
      list(
        classification = sp,
        retired = TRUE
      )
    )
  } else {
    return(
      list(
        classification = NA,
        retired = FALSE
      )
    )
  }
}

# Retire if any user tags human
human_tag <- function(sp_tag){
  if("Human" %in% sp_tag){
    return(
      list(
        classification = "Human",
        retired = TRUE
      )
    )
  } else {
    return(
      list(
        classification = NA,
        retired = FALSE
      )
    )
  }
}

# Put these functions into a loop

pb <- txtProgressBar(max = length(zoo_list))
for(i in 1:length(zoo_list)){
  setTxtProgressBar(pb, i)
  # DO THE SORTING BY ID DATE
  zoo_list[[i]] = zoo_list[[i]] %>% 
    dplyr::arrange(retired.created_at)
  
  # human check   
  is_human <- human_tag(zoo_list[[i]]$choice)
  if(is_human$retired){
    zoo_list[[i]]$our_rt <- "Human"
    zoo_list[[i]]$final_tag <- paste0( 
      is_human$classification,
      collapse = ", "
    )
    next
  }
  
  # empty check
  is_empty <- first_3_empty(zoo_list[[i]]$choice)
  if(is_empty$retired){
    zoo_list[[i]]$our_rt <- "3_nothing_here"
    zoo_list[[i]]$final_tag <- is_empty$classification
    next
  }
  
  
  is_5_empty <- five_empty(zoo_list[[i]]$choice)
  if(is_5_empty$retired){
    zoo_list[[i]]$our_rt <- "5_nothing_here"
    zoo_list[[i]]$final_tag <- is_5_empty$classification
    next
  }
  
  # species check  
  sp_7_tags <- any_sp_7(zoo_list[[i]]$choice)
  if(sp_7_tags$retired){
    zoo_list[[i]]$our_rt <- "7_species"
    zoo_list[[i]]$final_tag <- paste0( 
      sp_7_tags$classification,
      collapse = ", "
    )
    next
  }
  
  # classification count check 
  class_count_15 <- class_count(zoo_list[[i]]$choice, unique(zoo_list[[i]]$expertID))
  if(class_count_15$retired){
    zoo_list[[i]]$our_rt <- "classification_count"
    zoo_list[[i]]$final_tag <- paste0( 
      class_count_15$classification,
      collapse = ", "
    )
    next
  }
  
}

# Unlist your data 
zoo = dplyr::bind_rows(zoo_list)

# Drop levels created earlier
zoo$choice <- as.character(zoo$choice)
zoo$expertID <- as.character(zoo$expertID)

# Add column for 'commonality' of species
zoo = zoo %>%
  ungroup() %>%
  group_by(expertID) %>%
  mutate(sp_count = length(unique(subject_id)))

# Now let's get the proportion of those species occurrence
concat <- zoo %>% 
  unite('concat', expertID:subject_id, remove = FALSE) %>% 
  select(c(expertID, subject_id, concat))

length(unique(concat$concat))

zoo = zoo %>%
  ungroup() %>%
  group_by(expertID) %>%
  mutate(sp_prop = length(unique(subject_id))/length(unique(concat$concat)))

# Update 'correct' column of general species categories------------------------- 
zoo = zoo %>% 
  mutate(correct = ifelse(expertID == choice, 1, 0)) %>% 
  mutate(correct = ifelse(expertID == "Fox squirrel" 
                          & choice == "Squirrel (cannot ID)", 1, as.numeric(correct))) %>% 
  mutate(correct = ifelse(expertID == "Eastern gray squirrel" 
                          & choice == "Squirrel (cannot ID)", 1, as.numeric(correct))) %>% 
  mutate(correct = ifelse(expertID == "American mink" 
                          & choice == "American mink", 1, as.numeric(correct))) %>% 
  mutate(correct = ifelse(expertID == "American mink" 
                          & choice == "weasel", 1, as.numeric(correct))) 


# Save this data to load into modeling scripts----------------------------------
write.csv(zoo, "E:/GitHub/LPZ Coordinator/Zooniverse/model_data.csv")


