library(RColorBrewer)

### READ IN DATA
mix <- read.csv('mix-data-full.csv',
                stringsAsFactors = FALSE)

### CLEAN UP NUMERIC COLUMNS
numeric_columns <- c(1,4,7:36, 38:45, 48:50, 52:61, 63:73, 77, 79, 82, 83)

for (j in numeric_columns){
  # isolate column
  column <- mix[,j]
  # remove commas and percentage signs
  column <- gsub(',|%', '', column)
  mix[,j] <- as.numeric(column)
}

### DEFINE FUNCTION FOR SUBSETTING
sub_mix <- function(Region = 'Africa',
                    Year = NULL){
  # Subset region
  sub_data <- mix[which(mix$Region == Region),]
  
  # Subset for years
  if(!is.null(Year)){
    if(length(Year) < 2){
      Year <- rep(Year, 2)
    }
    sub_data <- sub_data[which(sub_data$Fiscal.Year >= Year[1] &
                                 sub_data$Fiscal.Year <= Year[2]),]
  }
  return(sub_data)
}


### DEFINE FUNCTION FOR VISUALIZING OUTCOMES OF INTEREST

# possibles <- gsub('[.]', ' ', names(mix)[numeric_columns])
# cat(paste0('"', possibles, '",\n'))

outcome <- function(var = 'Gross Loan Portfolio', 
                    data = mix){
  # Put in R space format
  var_formatted <- gsub(' ', '.', var)
  # Put in dataframe
  val <- data[,var_formatted]
  
  # Visualize
  h <- hist(val,
            breaks = 200,
            plot = FALSE)
  
  cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(h$mids))
  plot(h,
       col = adjustcolor(cols, alpha.f = 0.6),
       border = NA,
       main = var,
       xlab = var)
  quants <- round(quantile(val, na.rm = TRUE, probs = c(0.025, 0.975)), digits = 2)
  text(x = h$mids[round(0.75 * length(h$mids))],
       y = max(h$counts) * 0.8,
       labels = paste0('Mean: ', round(mean(val, na.rm = TRUE), digits = 2), '\n',
                       'Median: ', round(median(val, na.rm = TRUE), digits = 2), '\n',
                       '95% range: ', quants[1], ' - ', quants[2]))
}
outcome()
