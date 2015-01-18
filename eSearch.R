## eSearch.R
## PERFORM SIMPLE SEARCH (ESearch) ON PUBMED CENTRAL AND 
## RETRIEVE UID'S OUTPUT BY QUERY

##==================================================================
## INPUT ARGUMENTS: 
##  -query (string): keywords to be searched formatted according to ncbi
##    eutils guidelines
##  -nreturns (integer): maximum number of query records to retrieve,
##    up to a maximum of 10,000
##  -database (string): abbreviation for ncbi database to search (see ncbi
##    eutils documentation)
##  -sortby (string): method used to sort id's in the esearch output (see
##    ncbi eutils documentation)
## OUTPUT: a vector of uid's/pmcid's (pubmed central id's)
##
## EXAMPLE:
##  eSearch("term=(\"trastuzumab\"+OR+\"herceptin\")+AND+(\"tumor+growth\"\
##          +OR+\"tumor+volume\"+OR+\"tumor+size\"+OR+\"tumor+inhibition\"\
##          +OR+\"tumor+growth+inhibition\"+OR+\"tgi\"+OR+\"tumor+response\"\
##          +OR+\"tumor+regression\")"), 10, "pmc", "relevance")

##==================================================================
library("RCurl")
library("XML")
library("httr")
library("rvest")
library("stringr")

eSearch = function(query, nreturns=10, database="pmc", sortby="relevance") {
  
  ## NCBI DATABASE - BASE URL OF API AND GENERAL API OPTIONS
  ## base eutils url
  url.base = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  ## eSearch utility
  esearch =  "esearch.fcgi?"
    
  ## database to search (default is pubmed central to use with article scraper)
  db = paste0("db=",database)
  
  ## maximum number of uid's to be retrieved (max=100k)
  retmax = paste0("retmax=",nreturns)
  
  ## method used to sort uid's output by eSearch
  sortmethod = paste0("sort=",sortby)
  
  ## compose url for eSearch
  url.esearch = paste0(url.base, esearch, db, "&", retmax,
                       "&", sortmethod, "&", query)
  
  ## get and parse xml data returned by eSearch
  data.esearch = getURL(url.esearch)
  data.xml = xmlParse(data.esearch)
  
  ## get uid's/pmcid's
  uids = data.xml %>% xml_nodes("Id") %>% xml_text()
  return(uids)
}