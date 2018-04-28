# Analysis macro for SF Decompression data
# BM Census 2018
# Gridge

#Utility function that return a string for pinting value and fraction
printValFrac <- function(numerator, denominator) {
    return(paste0(numerator, ' / ', denominator, ' (', signif(numerator / denominator * 100, digits=2), '%)'))
}

#Make density distribution with 95% poisson CL and weights
#TODO: wrapper
plotDensity <- function(x, y) {
    
}

filterData <- function(inputCSVFile='data/MergedData_DecomNov18.csv', outputCSVFile='',makePlots=TRUE) {

    rawMergedData <<- read.csv(inputCSVFile, header=TRUE)
    nEntries = nrow(rawMergedData)
    print(paste('Total entries: ', nEntries))

    # Check validity of data
    ## Fix Shift (1 invalid shift, all fieds are also empty -> will discard)
    isInvalidShift <- unlist(lapply(rawMergedData$Shift, is.na))
    print(paste('Number of invalid-shift cards:', printValFrac(nrow(rawMergedData[isInvalidShift,]), nEntries)))

    ## Fix Lane
    rawMergedData[rawMergedData$Lane == 'G','Lane'] <- 'g'
    rawMergedData$Lane <- as.character(rawMergedData$Lane)
    rawMergedData[rawMergedData$Lane == '','Lane'] <- 'NA'
    rawMergedData$Lane <- as.factor(rawMergedData$Lane)

    isValidClicker <- unlist(lapply(rawMergedData$Clicker, is.numeric))
    nInvalidClicker <- nrow(rawMergedData[!isValidClicker,])
    print(paste('Invalid clickers (not numeric): ', nInvalidClicker))

    VetoSkipValues <- factor(rawMergedData$Veto)
    isSkip <- (rawMergedData[,'Veto'] %in% c('s', 'S'))
    nSkips <- nrow(rawMergedData[isSkip,])
    print(paste('Number of skips: ', printValFrac(nSkips, nEntries)))

    isVeto <- (rawMergedData[,'Veto'] %in% c('a', 'A', 'c', 'C'))
    isChildVeto <- (rawMergedData[,'Veto'] %in% c('c', 'C'))
    nVetos = nrow(rawMergedData[isVeto,])
    nChildVetos = nrow(rawMergedData[isChildVeto,])
    print(paste('Vetos: ', printValFrac(nVetos, nEntries), ', out of which child-veto: ', printValFrac(nChildVetos, nVetos)))
    if (makePlots) {
        x11()
        with(rawMergedData[isVeto,], hist(Shift,main='Number of Vetos vs Shift')) #plot vetos per shift
    }
    #calculate correction factors for un-even distribution of vetos, will be stored as weightVetos
    vetoFractionNumerator <- as.vector(with(rawMergedData[isVeto,], summary(as.factor(Shift))))
    vetoFractionDenominator <- as.vector(table(rawMergedData$Shift))
    vetoFractions <- vetoFractionNumerator / vetoFractionDenominator
    if (makePlots) {
        x11()
        plot(vetoFractions, xlab='Shifts',ylab='Density',main='Fraction of Vetos per Shift',sub='1=2-5PM, 2=5-8PM, 3=8-11PM')
    }
    rawMergedData[rawMergedData$Veto == "", 'Veto'] <- 0 #Assign 0 to empty cells
    rawMergedData[rawMergedData$Veto == " ", 'Veto'] <- 0 #Assign 0 to empty cells    

    isValidX1 <- unlist(lapply(rawMergedData$X1, is.numeric))
    nInvalidX1 <- nrow(rawMergedData[!isValidX1,])    
    print(paste('Number of invalid year of birth  (not numeric): ', nInvalidX1))

    #Correct X1
    rawMergedData[is.na(rawMergedData$X1), 'X1'] = -1
    rawMergedData[rawMergedData$X1 == 1996, 'X1'] = 96  
    
    #Correct X2a
    rawMergedData$X2a <- as.character(rawMergedData$X2a)
    rawMergedData[rawMergedData$X2a == '3,4', 'X2a'] = 3
    rawMergedData[rawMergedData$X2a == '12', 'X2a'] = 2
    rawMergedData[rawMergedData$X2a == '23', 'X2a'] = 2
    rawMergedData[is.na(rawMergedData$X2a), 'X2a'] = 0
    rawMergedData[rawMergedData$X2a == '', 'X2a'] = 0
    rawMergedData$X2a <- as.factor(rawMergedData$X2a)    

    #Correct X2b, fixing different spellings as well
    rawMergedData$X2b <- as.character(rawMergedData$X2b)
    rawMergedData[rawMergedData$X2a == '1', 'X2b'] = 'San Francisco'
    rawMergedData[rawMergedData$X2b == '2', 'X2b'] = ''
    rawMergedData[rawMergedData$X2b == '0', 'X2b'] = ''
    rawMergedData[rawMergedData$X2b == 'La Grange, CA', 'X2b'] = 'La Grange'
    rawMergedData[rawMergedData$X2b == 'Stockton, CA', 'X2b'] = 'Stockton'
    rawMergedData[rawMergedData$X2b == 'San_Marcos', 'X2b'] = 'San Marcos'
    rawMergedData[rawMergedData$X2b == 'South_Bay/Sunnyvale', 'X2b'] = 'Sunnyvale'
    rawMergedData[rawMergedData$X2b == 'Santa_Cruz', 'X2b'] = 'Santa Cruz'
    rawMergedData[rawMergedData$X2b == 'San_Mateo', 'X2b'] = 'San Mateo'
    rawMergedData[rawMergedData$X2b == 'Oakland, CA', 'X2b'] = 'Oakland'
    rawMergedData[rawMergedData$X2b == 'LA', 'X2b'] = 'Los Angeles'
    rawMergedData[rawMergedData$X2b == 'Mountain  View', 'X2b'] = 'Mountain View'
    rawMergedData[rawMergedData$X2b == 'Oak', 'X2b'] = 'Oakland'
    rawMergedData[rawMergedData$X2b == 'El_Cerrito', 'X2b'] = 'El Cerrito'
    rawMergedData[rawMergedData$X2b == 'Palo_Alto', 'X2b'] = 'Palo Alto'
    rawMergedData[rawMergedData$X2b == 'San_Lorenzo', 'X2b'] = 'San Lorenzo'
    rawMergedData[rawMergedData$X2b == 'Pleasant Hill, CA', 'X2b'] = 'Pleasant Hill'
    rawMergedData[rawMergedData$X2b == 'San Jose/Sacramento', 'X2b'] = 'San Jose'
    rawMergedData[rawMergedData$X2b == 'San leandro', 'X2b'] = 'San Leandro'
    rawMergedData[rawMergedData$X2b == 'San_Jose', 'X2b'] = 'San Jose'
    rawMergedData[rawMergedData$X2b == 'San_Carlos', 'X2b'] = 'San Carlos'
    rawMergedData[rawMergedData$X2b == 'San_Rafael', 'X2b'] = 'San Rafael'
    rawMergedData$X2b <- as.factor(rawMergedData$X2b)        

    #Correct X3,4: Assign 0 to NAs
    rawMergedData[is.na(rawMergedData$X3), 'X3'] = 0
    rawMergedData[is.na(rawMergedData$X4), 'X4'] = 0        

    #Correct X5a
    spuriousX5a12 <- (rawMergedData[,'X5a'] == 12 & !is.na(rawMergedData[,'X5a']))
    rawMergedData[spuriousX5a12, 'X5a'] = 1
    spuriousX5a23 <- (rawMergedData[,'X5a'] == 23 & !is.na(rawMergedData[,'X5a']))
    rawMergedData[spuriousX5a23, 'X5a'] = 3
    rawMergedData[is.na(rawMergedData$X5a), 'X5a'] = 0
    rawMergedData[rawMergedData$X5a == 5, 'X5a'] = 1

    #Correct X5b, based on answers to X5a
    rawMergedData[rawMergedData$X5b == 'No','X5b'] = 0
    rawMergedData[rawMergedData$X5b == 'Friends','X5b'] = 0
    rawMergedData[rawMergedData$X5b == 'N/A','X5b'] = 0
    rawMergedData[rawMergedData$X5b == '','X5b'] = 0 
    

    #Finally filter out any of the above we don't want,
    mergedData <- rawMergedData[!isSkip & !isVeto & !isInvalidShift,]
    #set column types    
    ### mergedData$Shift <- as.factor(mergedData$Shift)    
    ### mergedData$X2a <- as.integer(mergedData$X2a)
    ### mergedData$X3 <- as.integer(mergedData$X3)
    ### mergedData$X4 <- as.integer(mergedData$X4)
    ### mergedData$X5a <- as.integer(mergedData$X5a)    
    ### mergedData$X5b <- as.integer(mergedData$X5b)
    #add the necessary variables,
    # weight for each entry to compensate vetos
    #  N/(N-V) = 1./(1-V/N) = 1./(1-vetoFraction)    
    weightVecVetos = 1.0 / (1.0 - vetoFractions)
    mergedData = within(mergedData, {
        weightVetos = ifelse(Shift == 2, weightVecVetos[1], ifelse(Shift == 5, weightVecVetos[2], ifelse(Shift == 8, weightVecVetos[3], 1.0)))
    })
    
    nFinalEntries = nrow(mergedData)
    print(paste('Total number of output entries:', printValFrac(nFinalEntries, nEntries)))
    print(paste(' # non-fatal Invalid entries for X1: ', printValFrac(nrow(mergedData[mergedData$X1 == 0,]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X2a: ', printValFrac(nrow(mergedData[mergedData$X2a == 0,]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X2b: ', printValFrac(nrow(mergedData[mergedData$X2b == '',]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X3: ', printValFrac(nrow(mergedData[mergedData$X3 == 0,]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X4: ', printValFrac(nrow(mergedData[mergedData$X4 == 0,]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X5a: ', printValFrac(nrow(mergedData[mergedData$X5a == 0,]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X5b: ', printValFrac(nrow(mergedData[mergedData$X5b == 0,]), nFinalEntries)))
    print(paste(' #non-fatal Invalid entries for X5b (for X5a==1,2): ', printValFrac(nrow(mergedData[(mergedData$X5b == 0) & (mergedData$X5a %in% c(1,2)),]), nrow(mergedData[(mergedData$X5a %in% c(1,2)),]))))
    print("Weights stored into 'weightVeto' to compensate vetos per-shift (1=2-5PM, 2=5-8PM, 3=8-11PM):")
    print(weightVecVetos)

    #if not empty, write to CSV file
    if (outputCSVFile != '') {
        write.csv(mergedData, outputCSVFile)
    }

    return(mergedData)
}

analyze <- function(inputCSVFile='data/MergedData_DecomNov18.csv') {
    #Filter data first
    mergedData <<- filterData(inputCSVFile, '', FALSE)

    # Simple density distributions
    ## Year

    ## Location

    ## ...

    # Correlations
}
