### Document coverage using the Scopus API

## Loading libraries
library(rscopus) # Scopus Database API interface
library(biblioverlap) # For document coverage analysis
library(dplyr) # For data manipulation
library(lubridate) # For datetime manipulation

#scopus_api_key <- '' #Put your API key here

set_api_key(scopus_api_key)


#query <- 'AF-ID(60000036) AND PUBYEAR > 2012 AND SUBJAREA(MEDI) AND (LIMIT-TO(SUBJAREA,"BIOC") OR LIMIT-TO(SUBJAREA,"IMMU") OR LIMIT-TO(SUBJAREA,"NEUR"))'

#Function to get records from scopus

get_records <- function(af_id = 60000036, pubyear = 2022, 
                        subj_area, doctype = 'AR') {
  formatted_query = sprintf('AF-ID(%s)
                 AND PUBYEAR = %s
                 AND SUBJAREA(%s)
                 AND DOCTYPE(%s)', af_id, pubyear, subj_area, doctype)
  res <- scopus_search(query = formatted_query, count = 25) 
  dfs <- gen_entries_to_df(res$entries)
  cleaned_df <- dfs$df %>% 
    rename('TI' = 'dc:title',
           'AU' = 'dc:creator',
           'SO' = 'prism:publicationName',
           'PY' = 'prism:coverDate',
           'DI' = 'prism:doi'
           ) %>% 
    select(TI, AU, SO, PY, DI) %>% 
    mutate(PY = year(PY))
  return(cleaned_df)
}

scopus_search(quer)
  
bioc_dfs = get_records(subj_area = "BIOC")
immu_dfs = get_records(subj_area = "IMMU")
neur_dfs = get_records(subj_area = "NEUR")

#quais áreas pegar da Scopus?

#- Biochemistry, Genetics and Molecular Biology
#-Immunology and Microbiology
#-Neuroscience