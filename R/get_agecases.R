#' Data acquisition of cases age region from Epicentro ISS
#' 
#' Data acquisition of cases age from Epicentro ISS \href{https://www.epicentro.iss.it/}{(Link)} of summary information and age cases per region
#'
#'
#' @examples
#' \dontrun{
#' get_agecases(regNames)
#' }
#' 
#' @param regNames vector. Name of italian regions
#' @return list. Return a list of region containing a list of dataframe  
#' @export
#' 
get_agecases <- function(regNames) {
  
  files = "https://www.epicentro.iss.it/coronavirus/bollettino/Bolletino-sorveglianza-integrata-COVID-19_30-marzo-2020_appendix.pdf"

  #"https://www.epicentro.iss.it/coronavirus/bollettino/Bollettino-sorveglianza-integrata-COVID-19_30-marzo-2020.pdf"
  document <- lapply(files, pdftools::pdf_text)
  lapply(document, length) 
  alldocument_pages = document[[1]]
  
  
  condition_split = "Sintesi dei dati principali -"

  splitted = strsplit(alldocument_pages, condition_split)
  length(splitted)
  interested_pages = c()
  regNames_extend = c(regNames, c("Trento","Bolzano","Friuli-Venezia-Giulia","Valle D’Aosta"))
  
  for (i in 1:length(splitted))
  {
    splitted[[i]] = stringr::str_trim(splitted[[i]])
    splitted[[i]]= splitted[[i]][splitted[[i]]!=""]
    tmp = startsWith(splitted[[i]],regNames_extend)
    if(length(tmp[tmp ==T])>0)
    {
      filter_doc = splitted[[i]]
      interested_pages <- c(interested_pages, filter_doc)
    }
  }
  #Friuli
  
  pages_split =  stringr::str_split(interested_pages, "•")
  regions = c()
  age_lis = list()
  for (reg in 1:length(pages_split))
  {
    clean = stringr::str_trim(pages_split[[reg]])
    
    m = gregexpr('[0-9]+',clean[2:5])
    partition = regmatches(clean[2:5], m)
    
    regione = clean[1]
    casi_totali = as.integer(partition[[1]])
    eta_media = as.integer(partition[[2]][1])
    deceduti = as.integer(partition[[3]])
    operatori_sanitari = as.integer(partition[[4]][1])
    
    summary = data.frame(casi_totali,eta_media,deceduti,operatori_sanitari)
    
    m2 = gregexpr('[0-9]+-[0-9]+|[+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)',clean[5])
    part2 = regmatches(clean[5], m2)[[1]]
    
    age_cases = data.frame()
    
    for (i in seq(2,length(part2)-1, by = 3))
    {
      age_int = part2[i]
      cases = as.integer(part2[i+1])
      perc_cases =  as.numeric(part2[i+2])
      
      if(i == length(part2)-2)
      {
        age_int = "Not known"
        cases = as.integer(part2[i])
        perc_cases =  as.numeric(part2[i+1])
        
      }
      tmp =  data.frame(age_int,cases,perc_cases)
      age_cases = rbind(age_cases,tmp)
    }
    regions =  c(regions, regione)
    age_lis[[reg]] = list(summary, age_cases)
    names(age_lis[[reg]]) = c("summary","age_cases")
    
  }
  
  names(age_lis) = regions
  return(age_lis)
  
}