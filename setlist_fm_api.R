#------------------------------------------------------------------------------------------------------
#  xml format (note: did not create a dataframe)
#------------------------------------------------------------------------------------------------------
  ##comment: Libraries needed:
library(httr)
library(tidyverse)
library(XML)
library(xml2)
library(RCurl)

# Function
xmlToDF = function(doc, xpath, isXML = TRUE, usewhich = TRUE, verbose = TRUE) {
  
  if (!isXML) 
    doc = xmlParse(doc)
  #### get the records for that form
  nodeset <- getNodeSet(doc, xpath)
  
  ## get the field names
  var.names <- lapply(nodeset, names)
  
  ## get the total fields that are in any record
  fields = unique(unlist(var.names))
  
  ## extract the values from all fields
  dl = lapply(fields, function(x) {
    if (verbose) 
      print(paste0("  ", x))
    xpathSApply(proc, paste0(xpath, "/", x), xmlValue)
  })
  
  ## make logical matrix whether each record had that field
  name.mat = t(sapply(var.names, function(x) fields %in% x))
  df = data.frame(matrix(NA, nrow = nrow(name.mat), ncol = ncol(name.mat)))
  names(df) = fields
  
  ## fill in that data.frame
  for (icol in 1:ncol(name.mat)) {
    rep.rows = name.mat[, icol]
    if (usewhich) 
      rep.rows = which(rep.rows)
    df[rep.rows, icol] = dl[[icol]]
  }
  
  return(df)
}

###
mbid <- 'e01646f2-2a04-450d-8bf2-0d993082e058'

##comment: First page only with this link, adjust as necessary or loop for multiple
xmlLocation <- paste('api.setlist.fm/rest/0.1/artist/',mbid,'/setlists.xml?p=1',sep='')

# read xml information from API
phish <- read_xml(x = getURL(xmlLocation))

# extract all setlist records
setlists <- xml_find_all(phish, "//setlist")


# extract info about setlists to use to query songs
sets <- xml_attrs(setlists) # attributes
labs <- trimws(xml_attr(setlists, 'id'))

xml_attrs(setlists[[1]])

# This creates a list where each element is a list of all info for a given show
shows <- as_list(phish)



# extract all sets
sets <- xml_find_all(phish, "//set") 


xml_find_all(sets[[2]], '//song/@name') %>%
  xml_text()

xml_attrs(setlists)

sets <- xml_find_all(phish, "//setlist") 

xml_find_all(sets[[2]], '//') %>%
  xml_text()







