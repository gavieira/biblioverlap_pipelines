### Document coverage using the OpenAlex API

## Loading libraries
library(openalexR) # OpenAlex Database API interface
library(biblioverlap) # For document coverage analysis
library(dplyr) # For data manipulation
library(purrr) # For data manipulation
library(lubridate) # For datetime manipulation


#Set your email for faster response times - more info at https://docs.openalex.org/how-to-use-the-api/rate-limits-and-authentication#the-polite-pool)
#options(openalexR.mailto = "") #Add your email here

#Function to get records from openalex API and format them as input for biblioverlap
#Using the default parameters, this function retrieves all journal articles published in 2022 with any affiliation to Universidade Federal do Rio de Janeiro (UFRJ - ROR = https://ror.org/03490as77) from a user-specified subject field (passed to the function as field ids - https://docs.google.com/spreadsheets/d/1v-MAq64x4YjhO7RWcB-yrKV5D_2vOOsxl4u6GBKEXY8/edit#gid=983250122)
get_openalex_records <- function(ror = 'https://ror.org/03490as77',
                                 pubyear = 2022, 
                                 type_crossref = 'journal-article',
                                 field_id) {
  records <- oa_fetch(
    entity = 'works',
    institutions.ror = ror,
    publication_year = pubyear,
    type_crossref = type_crossref,
    primary_topic.field.id = field_id,
    abstract = FALSE,
    options = list(select = c('doi', 'display_name', 'publication_year', 'authorships', 'primary_location')),
    verbose = TRUE 
    ) %>% #Recovers document data through the openalex API
    rename(DI = doi, TI = display_name, AU = author, SO = so, PY = publication_year) %>% #Renames fields
    select(DI, TI, AU, SO, PY) %>% #Selects fields
    mutate(AU = map(seq_along(AU), ~ na.omit(case_when(AU[[.x]]$author_position == 'first' ~ AU[[.x]]$au_display_name)) ) ) #Generates a column containing only the first authors
}



openalex_datasets <- list() #Creating a list that will hold all datasets

#We will recover datasets from three different fields. Their respective field_ids are also displayed:
#Biochemistry, Genetics and Biological Sciences: 13
#Immunology and Microbiology: 24
#Neuroscience: 11

openalex_datasets$BIOC <- get_openalex_records(field_id = 13)
openalex_datasets$IMMU <- get_openalex_records(field_id = 24)
openalex_datasets$NEUR <- get_openalex_records(field_id = 11)


saveRDS(openalex_datasets, file = "rds/openalex_api.rds") #Saving rds file with the data recovered from the API to avoid using the API each time.

openalex_datasets <- readRDS("rds/openalex_api.rds") #Loading data from rds file



#Setting up list of column names for the document matching procedure
column_names <- list(DI = 'DI', TI = 'TI',
                        PY = 'PY', SO = 'SO',
                        AU = 'AU')

#Running biblioverlap
openalex_results <- biblioverlap(openalex_datasets, matching_fields = column_names)

#Generating plots
matching_summary_plot(openalex_results$summary) #Matching summary plot
venn_plot(openalex_results$db_list) #Venn diagram
upset_plot(openalex_results$db_list) #UpSet plot