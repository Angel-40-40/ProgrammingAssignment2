#
# |------------------------------------------------------------------------------------------|
# | I N I T I A L I Z A T I O N |
# |------------------------------------------------------------------------------------------|
a <- getwd()

c<-dir()

#
# |------------------------------------------------------------------------------------------|
# | I N T E R N A L F U N C T I O N S |
# |------------------------------------------------------------------------------------------|
complete <- function(directory, id = 1:332) {
        # --- Assert 'directory' is a character vector of length 1 indicating the
        # location of the CSV files.  'id' is an integer vector indicating the
        # monitor ID numbers to be used Return a data frame of the form: id nobs 1
        # 117 2 1041 ...  where 'id' is the monitor ID number and 'nobs' is the
        # number of complete cases
        
        # --- Assert create an empty vector
        nobsNum <- numeric(0)
        
        for (cid in id) {
                # --- Assert get data frame as ID
                cDfr <- getmonitor(cid, directory)
                
                # --- Assert count the number of complete cases and append to numeric
                # vector
                nobsNum <- c(nobsNum, nrow(na.omit(cDfr)))
        }
        
        # --- Assert return value is a data frame with TWO (2) columns
        data.frame(id = id, nobs = nobsNum)
}

getmonitor <- function(id, directory, summarize = FALSE) {
        # --- Assert 'id' is a vector of length 1 indicating the monitor ID
        # number. The user can specify 'id' as either an integer, a character, or
        # a numeric.  'directory' is a character vector of length 1 indicating the
        # location of the CSV files 'summarize' is a logical indicating whether a
        # summary of the data should be printed to the console; the default is
        # FALSE
        
        # --- Assert construct file name Directory is pre-appended to file name.
        # Use sprintf() to add leading zeroes.  E.g. 'specdata/001.csv'
        fileStr <- paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", 
                         sep = "")
        
        # --- Assert read csv
        rawDfr <- read.csv(fileStr)
        
        # --- Assert summary if true
        if (summarize) {
                print(summary(rawDfr))
        }
        
        # --- Return value is a data frame
        return(rawDfr)
}

#
# |------------------------------------------------------------------------------------------|
# | M A I N P R O C E D U R E |
# |------------------------------------------------------------------------------------------|

# --- Init set working directory


# --- Get data frame as ID = 1
complete(a, c(2, 4, 8, 10, 12))