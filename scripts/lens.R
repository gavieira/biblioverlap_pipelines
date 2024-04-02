### Document coverage using Lens data

##Loading libraries
library(bibliometrix) # For importing Lens data into R
library(dplyr) # For data manipulation
library(biblioverlap) # For document coverage analysis

#In this script, we'll work the original data used to obtain biblioverlap's example dataset. While the data available in the package only covers January 2022, in here we will work with all papers indexed by Lens that were published in 2022 by Universidade Federal do Rio de Janeiro (UFRJ) in the four biological disciplines. Those are listed below alongside a link to a publicly available copy of the data that can be exported directly from Lens:

#Biochemistry (https://www.lens.org/lens/search/scholar/list?collectionId=212653)
#Genetics (https://www.lens.org/lens/search/scholar/list?collectionId=212658)
#Microbiology (https://www.lens.org/lens/search/scholar/list?collectionId=212657)
#Zoology (https://www.lens.org/lens/search/scholar/list?collectionId=212655)

##Importing datasets (with bibliometrix) - can take a while...
biochemistry <- convert2df(file = 'data/lens/biochemistry-br-22.csv', dbsource = 'lens', format = 'csv')
genetics <- convert2df(file = 'data/lens/genetics-br-22.csv', dbsource = 'lens', format = 'csv')
microbiology <- convert2df(file = 'data/lens/microbiology-br-22.csv', dbsource = 'lens', format = 'csv')
zoology <- convert2df(file = 'data/lens/zoology-br-22.csv', dbsource = 'lens', format = 'csv')


View(biochemistry) #Taking a look at the data modified by bibliometrix

#Note that the import through biblioverlap modifies some fields, which is particularly useful it normalizes fields across distinct bibliographic databases (though that is not the case here). A reference to bibliometrix's column names can be found at https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf.
#It also adds some fields, like the "29-Character Source Abbreviation" (J9), which we'll be using to compare the source information, since it's shorter and presumably less prone to misspellings.

#Adding the data to a named list
lens_datasets <- list(Biochemistry = biochemistry,
                      Genetics = genetics,
                      Microbiology = microbiology,
                      Zoology = zoology)


#We're going to select only the relevant fields for the downstream analysis. Additionaly, we're going to remove html tags from the Title field, which are relatively common for Lens. They probably wouldn't impact our analysis since we're using datasets from the same database, but to remove them is generally considered a best practice. 
lens_datasets <- lapply(lens_datasets, function(df) { 
  df %>%
    select(DI, TI, AU, PY, J9, DT, Citing.Works.Count, Is.Open.Access) %>% #Selecting relevant fields
    mutate(TI = gsub("<.*?>", "", TI)) #Removes html tags, common in the Title field)
})

#Defining the columns that will be used in the document matching process
column_names = list(DI = 'DI', TI = 'TI',
                    AU = 'AU', PY = 'PY',
                    SO = 'J9')


#Performing document coverage analysis
lens_results <- biblioverlap(lens_datasets, matching_fields = column_names)


#Generating plots
matching_summary_plot(lens_results$summary) #Matching summary plot
venn_plot(lens_results$db_list) #Venn diagram
upset_plot(lens_results$db_list) #UpSet plot

##Using other fields to analyze subsets of the data
#Documents in open access 
open_access <- lapply(lens_results$db_list, function(db) {
  filter(db, `Is.Open.Access` == 'TRUE')
})
venn_plot(open_access)

#Documents with 10 or more citations (i.e. docs that count for the i10 index)
i10_docs <- lapply(lens_results$db_list, function(db) {
  filter(db, `Citing.Works.Count` >= 10)
})
venn_plot(i10_docs)

#Preprints
preprints <- lapply(lens_results$db_list, function(db) {
  filter(db, `DT` == 'PREPRINT')
})
venn_plot(preprints)