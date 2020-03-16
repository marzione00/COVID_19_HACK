#' Data acquisition regional insentive care capacity
#'
#' @return dataframe. Return a dataframe containing information about regional intensive care capacity
#'
#' @examples
#'   intensivecare_capacity = get_intensivecare_cap()
#' 
#' @param list. List of the Italy regional dataset
#' @export
get_intensivecare_cap <- function(regionTS) {
  
  readfile  <- read.csv(system.file("extdata", "Posti_TI.csv", package="covid19"),header=TRUE)
  colnames(readfile) = c("region","intensivecare_cap")
  readfile = readfile[order(readfile$region),]
  value =c()
  
  sortedreg = regionTS[order(names(regionTS))]
  for(index_region in 1:length(sortedreg)) 
  {
    
    
    vector = sortedreg[[index_region]]$terapia_intensiva  
    print(names(sortedreg))
    value[index_region] = tail(vector, n = 1)
    
  }
    
  
  df = data.frame(readfile,value)
  colnames(df) = c("region","intensivecare_cap","intensive_care_occupation")
  
  return(df)
}
