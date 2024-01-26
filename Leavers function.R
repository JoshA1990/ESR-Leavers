

all_leavers_function <- function() {
  ###################################Data wrangling###################################
  #Amend column names so the dots are replaced with spaces
  colnames(Raw_Data_y1) <- str_replace_all(colnames(Raw_Data_y1), " ", "_")
  colnames(Raw_Data_y2) <- str_replace_all(colnames(Raw_Data_y2), " ", "_")
  colnames(nationality) <- str_replace_all(colnames(nationality), " ", "_")
  
  #join NHS organisation codes and nationality
  Raw_Data_y1 <- full_join(Raw_Data_y1,NHS_orgs)
  Raw_Data_y1 <- full_join(Raw_Data_y1,nationality)
  Raw_Data_y2 <- full_join(Raw_Data_y2,NHS_orgs)
  Raw_Data_y2 <- full_join(Raw_Data_y2,nationality)
  
  #Adding sufixes to variables to separate variable names between datasets
  colnames(Raw_Data_y1) <- paste(colnames(Raw_Data_y1), "y1", sep = "_")
  colnames(Raw_Data_y2) <- paste(colnames(Raw_Data_y2), "y2", sep = "_")
  
  
  
  #Rename unique identifier to remove the suffix so it's easier to merge datasets later on
  Raw_Data_y1 <- rename(Raw_Data_y1, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y1)
  Raw_Data_y2 <- rename(Raw_Data_y2, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y2)
  
  #create flags for active nurses, on both datasets, will be important depending on whether looking at joiners or leavers later
  Raw_Data_y1 <- Raw_Data_y1 %>% 
    mutate(Staff_group_y1 = if_else(substr(Occupation_Code_y1, 1, 1) %in% c(0,1,2,8,9) ,"Medical", 
                                    if_else(substr(Occupation_Code_y1, 1, 1) %in% c("N","P"), "Nurse and Midwife", 
                                            if_else(substr(Occupation_Code_y1, 1, 1) %in% "A","Ambulance Staff",
                                                    if_else(substr(Occupation_Code_y1, 1, 1) %in% "G", "Admin and Support Staff",
                                                            if_else(substr(Occupation_Code_y1, 1, 1) %in% "H", "HCA and Support",
                                                                    if_else(substr(Occupation_Code_y1,1,1) %in% "S","Scientific, Therapeutic and Technical Staff","Other")))))))%>%
    mutate(Status_orig_y1 = Status_y1) %>%
    mutate(Status_y1 = if_else(Status_y1 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
    mutate(Asg_Type_Of_Contract_y1 = if_else(Asg_Type_Of_Contract_y1 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
    filter (Asg_Type_Of_Contract_y1=='Permanent/fixed term/locum') %>%
    mutate (Nationality_grouping_y1 =if_else(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')) %>%
    mutate (Nationality_grouping_y1_v2 = if_else(Nationality_grouping_y1 %in% c('ROW','EU'), 'IR',
                                                 if_else(Nationality_grouping_y1 %in% c('UK','Unknown'),'Domestic','Other')))
  
  Raw_Data_y2 <- Raw_Data_y2 %>%
    mutate(Staff_group_y2 = if_else(substr(Occupation_Code_y2, 1, 1) %in% c(0,1,2,8,9) ,"Medical", 
                                    if_else(substr(Occupation_Code_y2, 1, 1) %in% c("N","P"), "Nurse and Midwife", 
                                            if_else(substr(Occupation_Code_y2, 1, 1) %in% "A","Ambulance Staff",
                                                    if_else(substr(Occupation_Code_y2, 1, 1) %in% "G", "Admin and Support Staff",
                                                            if_else(substr(Occupation_Code_y2, 1, 1) %in% "H", "HCA and Support",
                                                                    if_else(substr(Occupation_Code_y2,1,1) %in% "S","Scientific, Therapeutic and Technical Staff","Other")))))))%>% 
    mutate(Status_orig_y2 = Status_y2) %>%
    mutate(Status_y2 = if_else(Status_y2 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
    mutate(Asg_Type_Of_Contract_y2 = if_else(Asg_Type_Of_Contract_y2 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
    filter (Asg_Type_Of_Contract_y2=='Permanent/fixed term/locum') %>%
    mutate (Nationality_grouping_y2 =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')) %>%
    mutate (Nationality_grouping_y2_v2 = if_else(Nationality_grouping_y2 %in% c('ROW','EU'), 'IR',
                                                 if_else(Nationality_grouping_y2 %in% c('UK','Unknown'),'Domestic','Other')))
  
  #ordering data by fte and then staff group
  Raw_Data_y1 <- Raw_Data_y1[order(Raw_Data_y1$Staff_group_y1,-Raw_Data_y1$Contracted_Wte_y1),]
  Raw_Data_y2 <- Raw_Data_y2[order(Raw_Data_y2$Staff_group_y2,-Raw_Data_y2$Contracted_Wte_y2),]
  
  
  #removing all duplications in Person_Unique_Nhs_Identifier so there's only one entry for each
  Raw_Data_y1_dedup <- Raw_Data_y1[ !duplicated(Raw_Data_y1$Unique_Nhs_Identifier), ]
  Raw_Data_y2_dedup <- Raw_Data_y2[ !duplicated(Raw_Data_y2$Unique_Nhs_Identifier), ]
  
  #Join datasets
  Data <- full_join(Raw_Data_y1_dedup, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier")
  
  #get rid of unsused data
  rm(Raw_Data_y1_dedup)
  
  #merge nationality into a single field and override NA nationality with Unknowns and NA NHS providers with 0s
  Data <- Data %>%
    mutate (Nationality = if_else(is.na(Nationality_y2) == FALSE, Nationality_y2, Nationality_y1)) %>%
    mutate (Nationality_grouping =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, Nationality_grouping_y1)) %>%
    mutate (Nationality_grouping = if_else(is.na(Nationality_grouping) == TRUE, 'Unknown',Nationality_grouping)) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('ROW','EU'), 'IR',
                                              if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic','Other'))) %>%
    mutate (NHSD_trust_or_CCG_y1 = if_else(is.na(NHSD_trust_or_CCG_y1) == FALSE, NHSD_trust_or_CCG_y1,0)) %>% 
    mutate (NHSD_trust_or_CCG_y2 = if_else(is.na(NHSD_trust_or_CCG_y2) == FALSE, NHSD_trust_or_CCG_y2,0))
  
  
  
  #joiner/ leaver flags
  Data <- Data %>%
    #joiner flags
    mutate(joiner = if_else(is.na(Staff_group_y1) == TRUE & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(non_active_to_active = if_else(is.na(Staff_group_y1) == FALSE & Status_y1 != "Active" & NHSD_trust_or_CCG_y1 == "1" & is.na(Staff_group_y2) == FALSE & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(other_joiner = if_else(is.na(Staff_group_y1) == FALSE & NHSD_trust_or_CCG_y1 == "0" & is.na(Staff_group_y2) == FALSE & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0))%>%
    #leaver flags
    mutate(leaver = if_else(is.na(Staff_group_y2) == TRUE & is.na(Staff_group_y1) == FALSE & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(active_to_non_active = if_else(is.na(Staff_group_y2) == FALSE & Status_y2 != "Active" & NHSD_trust_or_CCG_y2 == "1" & is.na(Staff_group_y1) == FALSE & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(other_leaver = if_else(is.na(Staff_group_y2) == FALSE & NHSD_trust_or_CCG_y2 == "0" & is.na(Staff_group_y1) == FALSE & Status_y1 == "Active" & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    #FTE change
    mutate(group_change = if_else(Staff_group_y1 != Staff_group_y2 & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1" & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(occ_change = if_else(Staff_group_y1 == Staff_group_y2 & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 =="1" & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1" & Occupation_Code_y1 != Occupation_Code_y2, Contracted_Wte_y2,0))%>%
    mutate(FTE_change = if_else(Staff_group_y2 == Staff_group_y1 & Status_y1 == "Active" & Status_y2 == "Active" & NHSD_trust_or_CCG_y1 == "1" & NHSD_trust_or_CCG_y2 == "1" & Contracted_Wte_y2 != Contracted_Wte_y1 , Contracted_Wte_y2-Contracted_Wte_y1, 0))
  
  
  #override NAs in joiner/ leaver flags
  Data <- Data %>%
    mutate (group_change = if_else(is.na(group_change)==FALSE,group_change,0)) %>%
    mutate (other_joiner = if_else(is.na(other_joiner)==FALSE,other_joiner,0)) %>%
    mutate (occ_change = if_else(is.na(occ_change)==FALSE,occ_change,0)) %>%
    mutate (other_leaver = if_else(is.na(other_leaver)==FALSE,other_leaver,0)) 
  
  #duplication check
  Data$Unique_Nhs_Identifier[duplicated(Data$Unique_Nhs_Identifier)]
  
  #Join datasets to overwrite Y1 nationality
  Raw_Data_y1 <- left_join(Raw_Data_y1, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier")
  
  #get rid of unsused data
  rm(Raw_Data_y2_dedup)
  
  #overwrite Y1 nationality
  Raw_Data_y1<- Raw_Data_y1 %>%
    mutate (Nationality_grouping_y1_v2_2=if_else(is.na(Nationality_grouping_y2_v2)==FALSE,Nationality_grouping_y2_v2,Nationality_grouping_y1_v2)) %>%
    mutate (Nationality_grouping_y1_v2=if_else(is.na(Nationality_grouping_y2)==FALSE,Nationality_grouping_y2,Nationality_grouping_y1))
  
  ###################################joiner/ leaver summaries###################################
  #Total joiners/ leavers
  summary <- Data %>%
    summarise (joiner=sum(joiner),
               non_active_to_active=sum(non_active_to_active),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               active_to_non_active=sum(active_to_non_active),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change),
               occ_change=sum(occ_change),
               group_change=sum(group_change)
    )
  
  #insert nationality column
  summary <- summary %>% mutate (Staff_group="All") %>% select (10,1:9)
  
  #Split by group
  summary_staff_group_y1 <- Data %>%
    group_by(Staff_group_y1) %>%
    summarise (joiner=sum(joiner),
               non_active_to_active=sum(non_active_to_active),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               active_to_non_active=sum(active_to_non_active),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change),
               occ_change=sum(occ_change),
               group_change=sum(group_change)
    )%>%
    select(1,3:10)
  
  summary_staff_group_y2 <- Data %>%
    group_by(Staff_group_y2) %>%
    summarise (joiner=sum(joiner),
               non_active_to_active=sum(non_active_to_active),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               active_to_non_active=sum(active_to_non_active),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change),
               occ_change=sum(occ_change),
               group_change=sum(group_change)
    )%>%
    select(1:2,10)
  
  #rename nationality grouping to match above summaries
  summary_staff_group_y1 <- summary_staff_group_y1 %>% rename (Staff_Group=Staff_group_y1)%>%
    rename(Group_Change_From = group_change)
  summary_staff_group_y2 <- summary_staff_group_y2 %>% rename (Staff_Group=Staff_group_y2)%>%
    rename(Group_Change_To = group_change)
  
  summary <- summary_staff_group_y2 %>% bind_cols(summary_staff_group_y1)%>%
    mutate(group_change_difference=Group_Change_To-Group_Change_From)%>%
    select(1:2,5:11,13)%>%
    rename(staff_group = Staff_Group...1)
  
  
  
  ###################################FTE summaries###################################
  #FTE - year 1
  #total
  FTE_y1 <- Raw_Data_y1 %>%
    filter(is.na(Staff_group_y1) == FALSE & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
    group_by(Staff_group_y1)%>%
    summarise (FTE_y1 = sum(Contracted_Wte_y1))
  
  
  #FTE - year 2
  #total
  FTE_y2 <- Raw_Data_y2 %>%
    filter(is.na(Staff_group_y2) == FALSE & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
    group_by(Staff_group_y2)%>%
    summarise (FTE_y2 = sum(Contracted_Wte_y2))
  
  
  
  ###################################Final combined summary output###################################
  #Create a variable to keep rows
  rows_to_keep <- c("Admin and Support Staff",
                    "Ambulance Staff",
                    "HCA and Support",
                    "Medical",
                    "Nurse and Midwife",
                    "Other",
                    "Scientific, Therapeutic and Technical Staff")
  
  #combine joiners/ leavers with FTE
  summary <- summary %>% filter(staff_group %in% rows_to_keep)%>%
    bind_cols(FTE_y1,FTE_y2) %>%
    select (1:10,12,14)%>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")))
  
  #leaver rates
  summary <- summary %>%
    mutate(leaver_rate = as.numeric(leaver)/as.numeric(FTE_y1))
  
  #pivot data into long format
  pivot <- pivot_longer(summary, c(2:13))
  
  Data <- Data %>% drop_na(Tm_Year_Month_y2)
  ###################################Pull joiner/ leaver period name###################################
  #extract joiner/ leaver period name
  pivot_final <- pivot
  colnames(pivot_final) <- c("Staff_grouping", "name", paste(substr(Data$Tm_Year_Month_y1,1,8)[1],"to",substr(Data$Tm_Year_Month_y2,1,8)[2]))
  
  
  ################################### Exports ###################################
  
  
  
  #export to folder area with the date of extract
  write.csv(pivot_final, paste0("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/Leavers/",substr(Data$Tm_Year_Month_y2,1,8)[1],".csv"))
  
  
  
}