
# Notes: 
# These examples show how to run web extractions in parallel 
# They are simplified and timed examples from stackoverflow






#----- Not in parallel -----#

tmklist <- c(91136088, 73006073, 92023027, 45061064, 45061065, 45061066, 45061067, 45061068)

start_time <- Sys.time()
data1 <- c()
for (i in 1:length(tmklist)) {
  data1[i] <- read_html(paste0(paste0('http://gis.hicentral.com/pubwebsite/TMKDetails.aspx?tmk=', tmklist[[i]]),'&lyrLst=0|0|0|0|0|0|0|0|0|0|0|0|0|13|0|15|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|lblsaerial2008&unit=0000&address=')) %>% html_text()  
  
}
end_time <-Sys.time()
no_parallel_time <- end_time - start_time
no_parallel_time
object.size(data1)
# 226488 bytes
nchar(data1)
# 28172 28347 28286 28230 28252 28219 28202 28241






#----- In parallel -----# 
# From: https://stackoverflow.com/questions/38257287/reading-multiple-html-pages-with-rvest-in-parallel


require(rvest); require(foreach); require(doParallel)


# sample input values used to generate html
tmklist <- c(91136088, 73006073, 92023027, 45061064, 45061065, 45061066, 45061067, 45061068)

start_time <- Sys.time()
data1 <- c()
registerDoParallel(cores = 3)
data1 <- foreach(i = 1:length(tmklist), .export=c("data1"), .combine = c) %dopar% {
  library(rvest)
  read_html(paste0(paste0('http://gis.hicentral.com/pubwebsite/TMKDetails.aspx?tmk=', tmklist[[i]]),'&lyrLst=0|0|0|0|0|0|0|0|0|0|0|0|0|13|0|15|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|0|lblsaerial2008&unit=0000&address=')) %>% html_text()
  #return(data)
}
end_time <-Sys.time()
parallel_time <- end_time - start_time
parallel_time
# stopCluster(cl)
object.size(data1)
# 226488 bytes
nchar(data1)
# 28172 28347 28286 28230 28252 28219 28202 28241





































# From mclapply example here: 
# https://stackoverflow.com/questions/43863914/web-scraping-with-rvest-in-parallel

library(rvest)
library(dplyr)
library(httr)

LINKS <- read_html("https://stackoverflow.com/") %>% 
  html_nodes(".question-hyperlink") %>% 
  html_attr(name = "href") %>%
  paste("https://stackoverflow.com", ., sep = "")


Get_values <- function(x){
  
  RES <- read_html(x) %>% 
    html_nodes(".label-key") %>% 
    html_text()
}



DATA <- lapply(LINKS[1:10], Get_values) #works fine


# No parallel 
start_time_no_parallel <- Sys.time()
DATA <- lapply(LINKS[1:10], Get_values) #returns NULL
end_time_no_parallel <- Sys.time(); total_time_no_parallel <- end_time_no_parallel - start_time_no_parallel
total_time_no_parallel


# In parallel
library(parallel)
start_time_parallel <- Sys.time()
DATA <- mclapply(LINKS[1:10], Get_values, mc.cores = 2) # Use: mc.cores = parallel::detectCores() - 1 # Also note mcapply is also an available function
end_time_parallel <- Sys.time(); total_time_parallel <- end_time_parallel - start_time_parallel





# Getting around windows error
# Here: https://stackoverflow.com/questions/46343775/mc-cores-1-is-not-support-on-windows

# original
ll <- parallel::mclapply( waydf$geometry$coordinates , st_linestring,
                          mc.cores = 2  )

# replace the above with all of the below
library(parallel)
cl <- makeCluster(2) # detectCores() - 1
cl <- clusterEvalQ(cl, { library(sf) })  # you need to export packages as well
# cl <- clusterExport(cl, "st_linestring")  # each worker is a new environment, you will need to export variables/functions to
ll <- parallel::parLapply(cl, LINKS[1:10],  Get_values)    # if st_linestring takes arguments then st_linestring(x)
stopCluster(cl)












