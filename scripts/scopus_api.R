### Document coverage using the Scopus API

## Loading libraries
library(rscopus) # Scopus Database API interface
library(biblioverlap) # For document coverage analysis
library(dplyr) # For data manipulation
library(lubridate) # For datetime manipulation

#scopus_api_key <- '' #Put your API key here

set_api_key(scopus_api_key) #Setting api key for recovering records


#Function to get records from scopus API and format them as input for biblioverlap
#Using the default parameters, this function retrieves all articles published in 2022 with any affiliation to Universidade Federal do Rio de Janeiro (UFRJ - AF-ID = 60000036) from a given subject field
#Default number of entries downloaded per API call is set to 25 (which is currently the limit for free API calls). If you use the premium API, this value can be adjusted
get_scopus_records <- function(af_id = 60000036, pubyear = 2022, 
                        subj_area, doctype = 'AR', entries_count = 25) {
  formatted_query = sprintf('AF-ID(%s)
                 AND PUBYEAR = %s
                 AND SUBJAREA(%s)
                 AND DOCTYPE(%s)', af_id, pubyear, subj_area, doctype) #Generating query
  res <- scopus_search(query = formatted_query, count = entries_count) #Getting data from api
  dfs <- gen_entries_to_df(res$entries) #Converting api response to dataframe
  cleaned_df <- dfs$df %>% 
    rename('TI' = 'dc:title',
           'AU' = 'dc:creator',
           'SO' = 'prism:publicationName',
           'PY' = 'prism:coverDate',
           'DI' = 'prism:doi'
           ) %>%  #Renaming columns
    select(TI, AU, SO, PY, DI) %>% #Selecting relevant columns
    mutate(PY = year(PY)) #Converting date column to year-only column
  return(cleaned_df) #Returning dataframe for document coverage analysis
}

scopus_datasets <- list() #Creating the list that will hold all datasets

#We will recover datasets from three different subject areas:
# BIOC - Biochemistry, Genetics and Biological Sciences
# IMMU - Immunology and Microbiology
# NEUR - Neuroscience
  
scopus_datasets$BIOC <- get_scopus_records(subj_area = "BIOC")
scopus_datasets$IMMU <- get_scopus_records(subj_area = "IMMU")
scopus_datasets$NEUR <- get_scopus_records(subj_area = "NEUR")

saveRDS(scopus_datasets, file = "rds/scopus_api.rds") #Saving rds file with the data recovered from the API to avoid using the API each time.

scopus_datasets <- readRDS("rds/scopus_api.rds") #Loading data from rds file


#Setting up list of column names for the document matching procedure
column_names <- list(DI = 'DI', TI = 'TI',
                        PY = 'PY', SO = 'SO',
                        AU = 'AU')

#Running biblioverlap
scopus_results <- biblioverlap(scopus_datasets, matching_fields = column_names)

#Generating plots
matching_summary_plot(scopus_results$summary) #Matching summary plot
venn_plot(scopus_results$db_list) #Venn diagram
upset_plot(scopus_results$db_list) #UpSet plot
