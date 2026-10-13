args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=1L) stop('Usage: Rscript scripts/write_alignment_receipt.R OUTPUT.csv')

source('temporal_inputs.R')
data <- read.csv('one45.csv')
data$date <- as.Date(data$date,format='%m/%d/%Y')

# Mirror the original archived study window and the first-row removal after
# differencing, without loading plotting or Markov-switching dependencies.
data <- data[-(1:719),]
data <- data[-(770:1492),]
data <- data[-1,]
price <- 100*log(as.numeric(data$realprice))
receipt <- alignment_summary(price,data$date,1:4)
write.csv(receipt,args[[1L]],row.names=FALSE)
print(receipt)
