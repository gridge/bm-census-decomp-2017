# Filter input Census BM survey data into a subset of variables
# Note: by default always also include special variables: weightnerds
# inputFile: input CSV file with Census results
# outputFile: output CSV file with filtered sample
# labelsList: vector of labels of categories that you want to extract (e.g. c('virgin') )
# refactorData: apply a set of hard-coded re-factoring of variables
# cleanData: only accept valid entries
# prescale: integer prescale to select only 1 out of N input data

filterCensusBMDataCSV <- function(inputFile, outputFile='', labelsList=c("gender", "residence","bmevents","virgin","agegr"), 
                                  refactorData=TRUE, cleanData=FALSE, prescale=1) {

    print(paste0("inputFile=", inputFile))
    print(paste0("outputFile=", outputFile))
    print(paste0("categories=",labelsList))
    print(paste0("refactorData=",refactorData))
    print(paste0("cleanDAta=",cleanData))
    print(paste0("prescale=",prescale))

    #Add default labelsList
    labelsList <- make.unique(append(labelsList, "weightnerds"))

    #Read input data    
    print('Loading input file')
    censusTable <- read.csv(inputFile, header=TRUE)
    
    #Refactor data, if requested
    if (refactorData == TRUE) {
      
    }

    #Clean data, if requested
    nEntries <- nrow(censusTable[,])
    validEntries <- rep(TRUE, nEntries)
    if (cleanData == TRUE) {
      print('Cleaning data')
      for (l in labelsList) {
        validEntries <- validEntries & (!is.na(censusTable[,l]))
      }
    }
    
    #Apply a prescale, if requested
    if (prescale > 1) {
      filterVec <- c(TRUE)
      filterVec <- append(filterVec, rep(FALSE, prescale-1))
      filterVec <- rep(filterVec, len=nEntries)
      validEntries <- validEntries & filterVec
    }

    # Print stats
    nValidEntries <- nrow(censusTable[validEntries,])
    print('Statistics:')
    print(paste(' Total entries: ', nEntries))
    print(paste(' Output entries: ', nValidEntries, '(', nValidEntries / nEntries * 100, '%)'))

    # Save data to a file
    if (! outputFile == '') {
        print(paste0('Saving filtered table to file ', outputFile))
        outTable <- censusTable[validEntries,labelsList]
        write.csv(outTable, file=outputFile)
    }
    print('All Done.')
}
