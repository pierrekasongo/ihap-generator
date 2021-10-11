
convert <- function(){
  
    master <-read.csv ("data_for_datim.csv")
    Org <- read_csv("convertor/OU sites.csv")
    Mechs<-read.csv ("convertor/Mechanisms.csv")
    Data_f<-read.csv ("convertor/Data sets, elements and combos paramaterized - Facility .csv")
    Data_c<-read.csv ("convertor/Data sets, elements and combos paramaterized - Community.csv")
    
    
    # Dropping and renaming columns #
    
    Mechs2 <- Mechs %>%
      rename(AttributeOptionComboID = uid) %>%
      select(-c(code, primeid, startdate, enddate, ou))
    Org2 <- Org %>%
      rename(orgUnitID = orgunit_internal_id) %>%
      select(c(orgunit_level, orgunit_parent, orgunit_name, orgUnitID ))
    Data_f2 <- Data_f %>% 
      rename(categoryOptionComboID = categoryoptioncombouid,
             dataelementID = dataelementuid) %>%
      select(-c(dataelementdesc, code))
    Data_c2 <- Data_c %>% 
      rename(categoryOptionComboID = categoryoptioncombouid,
             dataelementID = dataelementuid) %>%
      select(-c(dataelementdesc, code))
    
    
    
    #### Merging ####
    
    merged <- left_join(master, Mechs2, by="AttributeOptionComboID") #anything from Mechs2 that does not merged is dropped
    
    
    merged2 <- left_join(merged, Org2, by="orgUnitID")
    
    
    sitedata <- bind_rows(Data_c2, Data_f2) 
    
    
    merged3 <- left_join(merged2, sitedata, by=c("dataelementID", "categoryOptionComboID"))

    
    
    #### to account for EMR_SITE = 'Yes' values ####
    # take out rows where Value = "true", create merged_emrsite
    # convert Value into numeric
    # run steps
    # convert Value in new dataset to factor to append to merged_emrsite
    
    
    merged_emrsite <- filter(merged3, Value == "true")
    merged_emrsite <- merged_emrsite %>%
      select(-c(Value, partner, agency, categoryoptioncombocode, dataelement, dataset)) %>%
      mutate(Value = 1)
    merged_emrsite$Value <- as.numeric(merged_emrsite$Value)
    
    
    merged4 <- filter(merged3, Value != "true")
    merged4$Value <- as.numeric(merged4$Value)
    
    merged5 <- merged4 %>%
      select(-c(partner, agency, categoryoptioncombocode, dataelement, dataset)) %>%
      arrange(orgunit_name, shortname, categoryoptioncombo, mechanism, Period) %>%
      group_by(Period, mechanism, orgUnitID, dataelementID, categoryOptionComboID, AttributeOptionComboID,
               orgunit_name, orgunit_parent, orgunit_level, shortname, categoryoptioncombo) %>%
      summarize(Value = sum(Value)) %>%
      ungroup() %>%
      replace(., is.na(.), "#N/A")
    # The group_by() and summarize() steps aggregates values from any rows with duplicative meta data #
    
    merged6 <- bind_rows(merged5, merged_emrsite)
    
    
    write.xlsx(merged6, "converted.xlsx")
    

}

loadConverted <- function(){
  if(file.exists("converted.xlsx")){
    conv <- read.xlsx("converted.xlsx")
    
    data <- conv %>% 
      select(-c(orgUnitID:AttributeOptionComboID,orgunit_level))
    
    return(data[1:100,])
  }else
    return(NULL)
}

