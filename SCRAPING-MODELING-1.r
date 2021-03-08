                    ########### Script Summary ############
                    
# PRUNIER Valentin - PALMA Sebastien - HENRY DE VILLENEUVE Erwan - ITURRIOZ Leo
                    
                    
                    # SETTING THE PRICE OF A RENT IN THE CITY OF TOULOUSE #
                  
             
#1. Introduction / Objectives
#2. Step 1 : Presets (Script 1)
#3. Step 2 : Scraping / Datasets Building (Script 1)
#4. Machine Learning Script for Predictions (Script 2)
#5. Conclusion


################################## 1. Introduction #####################################################################################
#1.1 CONTEXT
#We represent an investment fund company which want to create a real estate agency in Toulouse. With the current situation (Covid-19), a lot of
#owners are selling their real estate.

#1.2 PROBLEM
#The company doesn't have any data regarding the Toulouse area and wants to know what is the right price of rent for their last acquired real estate.

#1.3 SOLUTION
#The Business Analytics department designed a new tool which can analyze all the offers on the web in order to create a coherent and precise dataset to predict
#the price with the features of the real estate.

#1.4 METHO
#1.4.1 The Business Analytics department will scrap the different offers' URLs of a website and will store the offers' URLs into a dataframe.
#1.4.2 From these URLs, the department will be able to extract all the features of a real estate (Price, Living Space, Heating Features...)
#1.4.3 All these features will be store into a new dataset
#NB : It's easier to split the scrap of the "Furnished" real estate and "Unfurnished" real estate
#1.4.4 From the last dataset, it's possible to make a multiple linear regression to get the perfect renting price.

#######################################################################################################################################
#################################2. Step 1 : Presets to scrap #########################################################################
#######################################################################################################################################

#2.1  Packages
#All the packages we need to scrap

install.packages('htmltab')
install.packages('rvest')
install.packages('xml2')
install.packages("stringr")
library(rvest)
library(htmltab)
library(xml2)
library(stringr)

#################
#2.2 URL PRESETS#
#################

#To scrap the different offer URLs, we need to scrap the URL gathering all the results of research

# This URL look like this : 

# https://www.avendrealouer.fr/recherche.html?pageIndex={PAGE_INDEX}&sortPropertyName=ReleaseDate&sortDirection=Descending&searchTypeID=2&typeGroupCategoryID=6&localityIds=101-14281&typeGroupIds=47&housingIds=0&furnished=0&minimumPrice={minPrice}&hasAlert=false&hasMoreCriterias=true

#The most important parameters we need the change for the automation are {INDEX} and {MINIMUM_PRICE}.

# {PAGE_INDEX} because a results page only can store 15 offers (So only 15 offer URLs we can extract) - So, every 15 results, we have to
# change the index.

# {MINIMUM_PRICE} and {MAXIMUM_PRICE} are the limit of the website we can scrap, for instance it is limited to 25 page result. Then, we gonna split the scraping with different range
#of price (From 0 to 500, From 500 to 1000, ect).


#The domain name of the site we will scrap
DOMAIN_NAME <- 'https://www.avendrealouer.fr' 

# (1) First part of the URL until the {INDEX_PAGE}
URL_INDEX_OPENING = 'https://www.avendrealouer.fr/recherche.html?pageIndex='

# (2) All the parameters from the {INDEX_PAGE} until the {MINIMUM_PRICE}
URL_PARAMETERs_UNFURNISHED = '&sortPropertyName=ReleaseDate&sortDirection=Descending&searchTypeID=2&typeGroupCategoryID=6&localityIds=101-14281&typeGroupIds=47&housingIds=0&furnished=0&minimumPrice='
URL_PARAMETERs_FURNISHED = '&sortPropertyName=ReleaseDate&sortDirection=Descending&searchTypeID=2&typeGroupCategoryID=6&localityIds=101-14281&typeGroupIds=47&housingIds=1&furnished=1&minimumPrice='

# (3) Part of the URL between {MINIMUM_PRICE} and the last part of the URL
URL_MAX_PRICE = '&maximumPrice='

# (4) Last part of the URL
URL_INDEX_CLOSING = '&hasAlert=false&hasMoreCriterias=true'

#Finally, to extract the different offer URLs, we need to find the specific xPath.
# The generic Xpath of these offers is : '//*[@id="property-list-content-responsive"]/div/main/div[2]/div[{SEQ}]/div[1]/a'
# To keep automatizing the extraction, we need to determine all the xpath into the results page.
# With a quick observation, we can conclude that the offer appears every 2 'div' from the main content in the results page
# So, SEQ is the sequence of odds numbers from 1 to 50, to be sure to extract all the results

###################
#2.3 NODES_PRESETS#
###################

# (1) First part of the Xpath until the SEQ
NODES_Part_1 <- '//*[@id="property-list-content-responsive"]/div/main/div[2]/div['

# (2) Last part of the Xpath after the SEQ
NODES_Part_2 <- ']/div[1]/a'

#SEQUENCES FOR LOOPING xPAth
SEQ <- seq(from=1,to=50,by=2)

########################################################################################################################################
################################## STEP 2 : URL SCRAPING ###############################################################################
########################################################################################################################################

################################### PART 1 : UNFURNISHED REAL ESTATE URLs ################################################################################
#There are 3 loops to extract automatically the offer URLs from the results pages
# (1) While Loop : it's the loop that split the extraction with 4 range of prices (0-500 / 500-1000 / 1000-1500 / ...)
# (2) for Loop "j" : it's the loop that extract all the results pages with the limit of 25 pages in a row 
# (3) for Loop "i" : it's the loop that extract all the offers' URLs into a results page


{
  
  #########################################
  ##  2.1 - UNFURNISHED DATAFRAME PRESET ##
  #########################################
  
  UNFURNISHED <- as.data.frame('URL')
  PRICE_INDEX_MIN <- 0                            #PREsET_MINIMUM_PRICE
  PRICE_INDEX_MAX <- 500                          #PRESET_MAXIMUM_PRICE
  a=0                                             #PRESET while loop
  
  
  ##################################################################
  ## 2.2 - UNFURNISHED LOOP TO EXTRACT URLs FROM THE RESULTS PAGES ##
  ##################################################################
  
  while (a<4){
    for (j in 1:25){
      for (i in SEQ){
        xPath <- paste(NODES_Part_1,as.character(i),NODES_Part_2,sep='')        #Create the xPAth of the first offer of the first results
        URL <- paste(URL_INDEX_OPENING,as.character(j),URL_PARAMETERs_UNFURNISHED,PRICE_INDEX_MIN,URL_MAX_PRICE,PRICE_INDEX_MAX,URL_INDEX_CLOSING,sep='') #BUILDING the URL of the page results
        URL <- read_html(URL)
        URL <- html_node(URL,xpath = xPath)
        URL <- as.character(URL)
        URL <- substr(URL,10,84)                    #Cleaning step for just extracting the URL
        URL <- paste(DOMAIN_NAME,URL,sep='')
        URL <- gsub('\"','',URL)
        print(URL)
        UNFURNISHED <- rbind(UNFURNISHED,URL)       #Add the new URL to the dataset which store all of other URLs
      }
    }
    PRICE_INDEX_MIN <- PRICE_INDEX_MAX            #Change the MINIMUM_PRICE with the MAXIMUM_PRICE
    PRICE_INDEX_MAX <- PRICE_INDEX_MAX+500        #Change the MAXIMUM_PRICE +500 to scrap other offers
    print('changement')                           #A checking view into the console to verify that there is no error
    a <- a+1                                      #New step for the while loop when all the extraction are done
  }
  BACKUP <- UNFURNISHED                           #Creation of a backup dataframe for safety
  
  
  ##############################################
  ## 2.3 - EXTRACTION OF THE REFERENCE OFFERS ##
  ##############################################
  
  # It's important to store the references of the offers to rebuild the URL in a proper way (because, there are some extractions errors due to the HTML code)
  # The principle is pretty simple : We locate a key that refer to the offer references and we extract a string matching with this key (key = 'fd-')
  
  for (k in 2:length(t(UNFURNISHED))){
    Ref <- UNFURNISHED[k,1] 
    Ref_Location_start <- str_locate(Ref,'fd-')
    Ref_Location_start <- as.data.frame(Ref_Location_start)
    Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
    Ref_Location_end <- str_locate(Ref_Location_start,'.h')[,1]-1
    Reference <- substring(Ref_Location_start,0,Ref_Location_end)
    print(Reference)
    UNFURNISHED[k,2] <- Reference
  }
  
  #CLEAN NA FROM THE REFERENCE EXTRACTIONS
  UNFURNISHED <- na.omit(UNFURNISHED)
  
  
  ##################################################
  ## 2.4 - URL BUILDER FROM THE REFERENCES (LOOP) ##
  ##################################################
  
  #From this offer reference, we recompose the URL properly to avoid to get some error in the offer URLs (Error like error (500 : open connection)
  URL_BUILDER_LOCATION <- 'https://www.avendrealouer.fr/location/toulouse-31/b-appartement/loc-101-14281/'
  DOT_HTML <- '.html'
  
  for (L in 1:length(t(UNFURNISHED))){
    FINAL_URL <- paste(URL_BUILDER_LOCATION,UNFURNISHED[L,2],DOT_HTML,sep='')
    UNFURNISHED[L,'FINAL_URL'] <- FINAL_URL
    print(FINAL_URL)
  }
  
  
  #Clean NA FROM URL BUILDING & ERASE USELESS COLUMNS
  UNFURNISHED <- na.omit(UNFURNISHED)
  UNFURNISHED <- UNFURNISHED[,2:3]
  
  ############################
  ## 2.5 - Clean URL Errors ##
  ############################
  
  #This step is very useful to avoid the "error" URL 
  for (a in 1:length(t(UNFURNISHED))){
    Error_Checker <- try(read_html(UNFURNISHED[a,2]),silent = TRUE)
    Error_Checker
    if (Error_Checker[1] == "Error in open.connection(x, \"rb\") : HTTP error 500.\n") {
      UNFURNISHED[a,'Error_check'] <- 1
      UNFURNISHED <- UNFURNISHED[-(a),]
      print(1)
    } else {
      UNFURNISHED[a,'Error_check'] <- "clean"
      print(0)
    }
  }
  
  UNFURNISHED <- na.omit(UNFURNISHED)
  
  ################################
  ## 2.6 - FEATURES EXTRACTIONS ##
  ################################
  
  #All the loops below enable to extract the features of the offer
  #Every loops will open every URL to read the HTML code of the offers
  #In this code, we will select just the information we need thanks to "xPath code" of the objects like '.KHmco' for the prices
  #Secondly, every loops will clean the results of the extraction to just remain a actionable object for a future regression
  #The loops are quite similar and understandable 
  
  #PRICE EXTRACTION
  for (m in 1500:2100){
    URL <- UNFURNISHED[m,2]
    webpage <- read_html(URL)
    Price_html <- html_nodes(webpage,'.KHmco')
    Price <- html_text(Price_html)
    Price <- gsub(' ???','',Price)
    Price <- trimws(Price)
    print(Price)
    UNFURNISHED[m,'PRICE'] <- Price
  }
  
  
  #FEATURES EXTRACTION
  for (m in 1500:2100){
    URL <- UNFURNISHED[m,2]
    webpage <- read_html(URL)
    FEATURES_html <- html_nodes(webpage,'.cErVSh') #CVar = classics Variables
    FEATURES <- html_text(FEATURES_html)
    FEATURES <- paste(FEATURES, collapse = " ")
    FEATURES <- gsub('Chambres','Chambre',FEATURES) #Simplify the categorization of the room_type "Chambre"
    print(FEATURES)
    
    #LOOP FOR SURFACE
    for (k in 1500:2100){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,'Surface')
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
      Ref_Location_end <- str_locate(Ref_Location_start,'m²')[,1]-1
      Reference <- substring(Ref_Location_start,0,Ref_Location_end)
      Reference <- gsub('Surface ','',Reference)
      Surface <- Reference
      UNFURNISHED[m,'Surface'] <- Surface
    }
    
    #LOOP FOR PIECES   
    for (k in 1500:2100){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,'Pièces')
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
      Reference <- substring(Ref_Location_start,0,9)
      Reference <- trimws(Reference)
      Reference <- gsub('Pièces ','',Reference)
      Rooms <- Reference
      UNFURNISHED[m,'Rooms'] <- Rooms
    }
    
    #LOOP FOR CHAMBRES
    for (k in 1500:2100){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,'Chambre')
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
      Reference <- substring(Ref_Location_start,0,10)
      Reference <- trimws(Reference)
      Reference <- gsub('Chambre ','',Reference)
      Bedrooms <- Reference
      UNFURNISHED[m,'Bedrooms'] <- Bedrooms
    }
    
    #LOOP FOR FLOOR
    for (k in 1500:2100){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,'Étage')
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
      Reference <- substring(Ref_Location_start,0,8)
      Reference <- trimws(Reference)
      Reference <- gsub('Étage ','',Reference)
      Floor <- Reference
      UNFURNISHED[m,'FLOOR'] <- Floor
    }
    
    #LOOP FOR BUILDING YEAR
    for (k in 1500:2100){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,'Construction')
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
      Reference <- substring(Ref_Location_start,0,17)
      Reference <- trimws(Reference)
      Reference <- gsub('Construction ','',Reference)
      Building_year <- Reference
      UNFURNISHED[m,'BUILDTING_YEAR'] <- Building_year
    }
  }
  
  #LOCATION EXTRACTION
  
  for (m in 1177:2100){
    URL <- UNFURNISHED[m,2]
    webpage <- read_html(URL)
    Location_html <- html_nodes(webpage,'.cspDYs')
    Location <- html_text(Location_html)
    Location <- gsub('Toulouse ','',Location)
    Location <- gsub(')','',Location)
    Location <- substring(Location,2,7)
    print(Location)
    UNFURNISHED[m,'LOCATION'] <- Location
  }
  View(UNFURNISHED)
  
  #HEATING FEATURES EXTRACTION
  
  for (m in 2010:2100){
    URL <- UNFURNISHED[m,2]
    webpage <- read_html(URL)
    FEATURES_html <- html_nodes(webpage,'.bGqdzg') 
    FEATURES <- html_text(FEATURES_html)
    FEATURES <- paste(FEATURES, collapse = " ")
    FEATURES
    print(FEATURES)
    
    #LOOP FOR ELECTRICIty
    for (k in 2010:2100){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,' kWhEP')
      Ref_Location_start
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Reference <- substring(Ref,Ref_Location_start[,1]-2,Ref_Location_start[,1])
      Reference
      Reference <- gsub(' ','',Reference)
      Reference
      Electricity <- Reference
      UNFURNISHED[m,'ELECTRICITY_HEATING'] <- Electricity
    }
    
    #LOOP FOR GAS
    for (k in 2010:2100){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,' kg')
      Ref_Location_start
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Reference <- substring(Ref,Ref_Location_start[,1]-2,Ref_Location_start[,1])
      Reference
      Reference <- gsub(' ','',Reference)
      Reference
      Gas <- Reference
      UNFURNISHED[m,'GAS_HEATING'] <- Gas
    }
  }
  
  #Finally we will show the results of the dataframe with URLs, Refences and features of offers.
  View(UNFURNISHED)
  
  #The aim this to create a database, so knowing that, the offer are updated every day. It's really important to store all the results in a csv file. 
  #Every daily csv files will be concatenate into a large database to improve the model day by day.
  
  write.csv(UNFURNISHED,'UNFURNISHED.csv')
  
}


########################################################################################
##################################PART 2 : LOOP URLs FURNISHED ##########################
########################################################################################

#The loop is exactly the same that the UNFURNISHED algorithm so we don't need to explain it again. 
#Moreover, it's interesting to see the code without the comments.

{
  
  
  #######################################
  ##  2.1 - FURNISHED DATAFRAME PRESET ##
  #######################################
  
  PRICE_INDEX_MIN = 0
  PRICE_INDEX_MAX = 500
  FURNISHED <- as.data.frame('df')
  a=0
  
  
  ################################################################
  ## 2.2 - FURNISHED LOOP TO EXTRACT URL FROM THE RESULTS PAGES ##
  ################################################################
  
  while (a<2){
    for (j in 1:25){
      for (i in SEQ){
        xPath <- paste(NODES_Part_1,as.character(i),NODES_Part_2,sep='')
        URL <- paste(URL_INDEX_OPENING,as.character(j),URL_PARAMETERs_FURNISHED,PRICE_INDEX_MIN,URL_MAX_PRICE,PRICE_INDEX_MAX,URL_INDEX_CLOSING,sep='') 
        URL <- read_html(URL)
        URL <- html_node(URL,xpath = xPath)
        URL <- as.character(URL)
        URL <- substr(URL,10,84)
        URL <- paste(DOMAIN_NAME,URL,sep='')
        URL <- gsub('\"','',URL)
        print(URL)
        FURNISHED <- rbind(FURNISHED,URL)
      }
    }
    PRICE_INDEX_MIN <- PRICE_INDEX_MAX
    PRICE_INDEX_MAX <- PRICE_INDEX_MAX+500
    print('changement')
    a <- a+1 
  }
  
  BACKUP <- FURNISHED
  
  
  ##############################################
  ## 2.3 - EXTRACTION OF THE REFERENCE OFFERS ##
  ##############################################
  
  for (k in 2:length(t(FURNISHED))){
    Ref <- FURNISHED[k,1] 
    Ref_Location_start <- str_locate(Ref,'fd-')
    Ref_Location_start <- as.data.frame(Ref_Location_start)
    Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
    Ref_Location_end <- str_locate(Ref_Location_start,'.h')[,1]-1
    Reference <- substring(Ref_Location_start,0,Ref_Location_end)
    print(Reference)
    FURNISHED[k,2] <- Reference
  }
  
  #CLEAN NA FROM THE REFERENCE EXTRACTIONS
  FURNISHED <- na.omit(FURNISHED)
  
  
  ##################################################
  ## 2.4 - URL BUILDER FROM THE REFERENCES (LOOP) ##
  ##################################################
  
  #URL BUILDER FROM THE REFERENCES (LOOP)
  URL_BUILDER_LOCATION <- 'https://www.avendrealouer.fr/location/toulouse-31/b-appartement/loc-101-14281/'
  DOT_HTML <- '.html'
  
  for (L in 1:length(t(FURNISHED))){
    FINAL_URL <- paste(URL_BUILDER_LOCATION,FURNISHED[L,2],DOT_HTML,sep='')
    FURNISHED[L,'FINAL_URL'] <- FINAL_URL
    print(FINAL_URL)
  }
  
  #Clean NA FROM URL BUILDING & ERASE USELESS COLUMNS
  FURNISHED <- na.omit(FURNISHED)
  FURNISHED <- FURNISHED[,2:3]
  
  
  ############################
  ## 2.5 - Clean URL Errors ##
  ############################
  
  for (a in 1:length(t(FURNISHED))){
    Error_Checker <- try(read_html(FURNISHED[a,2]),silent = TRUE)
    Error_Checker
    if (Error_Checker[1] == "Error in open.connection(x, \"rb\") : HTTP error 500.\n") {
      FURNISHED[a,'Error_check'] <- 1
      FURNISHED <- FURNISHED[-(a),]
      print(1)
    } else {
      FURNISHED[a,'Error_check'] <- "clean"
      print(0)
    }
  }
  
  FURNISHED <- na.omit(FURNISHED)
  
  
  ################################
  ## 2.6 - FEATURES EXTRACTIONS ##
  ################################
  
  #PRICE EXTRACTION
  for (m in 467:1000){
    URL <- FURNISHED[m,2]
    webpage <- read_html(URL)
    Price_html <- html_nodes(webpage,'.KHmco')
    Price <- html_text(Price_html)
    Price <- gsub(' ???','',Price)
    Price <- trimws(Price)
    print(Price)
    FURNISHED[m,'PRICE'] <- Price
    
  }
  
  #FEATURES EXTRACTION
  for (m in 1:1000){
    URL <- FURNISHED[m,2]
    webpage <- read_html(URL)
    FEATURES_html <- html_nodes(webpage,'.cErVSh') #CVar = classics Variables
    FEATURES <- html_text(FEATURES_html)
    FEATURES <- paste(FEATURES, collapse = " ")
    FEATURES <- gsub('Chambres','Chambre',FEATURES) #Simplify the categorization of the room_type "Chambre"
    print(FEATURES)
    
    #LOOP FOR SURFACE
    for (k in 1:1000){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,'Surface')
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
      Ref_Location_end <- str_locate(Ref_Location_start,'m²')[,1]-1
      Reference <- substring(Ref_Location_start,0,Ref_Location_end)
      Reference <- gsub('Surface ','',Reference)
      Surface <- Reference
      FURNISHED[m,'Surface'] <- Surface
    }
    
    #LOOP FOR PIECES   
    for (k in 1:1000){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,'Pièces')
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
      Reference <- substring(Ref_Location_start,0,9)
      Reference <- trimws(Reference)
      Reference <- gsub('Pièces ','',Reference)
      Rooms <- Reference
      FURNISHED[m,'Rooms'] <- Rooms
    }
    
    #LOOP FOR CHAMBRES
    for (k in 1:1000){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,'Chambre')
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
      Reference <- substring(Ref_Location_start,0,10)
      Reference <- trimws(Reference)
      Reference <- gsub('Chambre ','',Reference)
      Bedrooms <- Reference
      FURNISHED[m,'Bedrooms'] <- Bedrooms
    }
    
    #LOOP FOR FLOOR
    for (k in 1:1000){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,'Étage')
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
      Reference <- substring(Ref_Location_start,0,8)
      Reference <- trimws(Reference)
      Reference <- gsub('Étage ','',Reference)
      Floor <- Reference
      FURNISHED[m,'FLOOR'] <- Floor
    }
    
    #LOOP FOR BUILDING YEAR
    for (k in 1:1000){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,'Construction')
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Ref_Location_start <- substring(Ref,Ref_Location_start[,1],Ref_Location_start[,1]+30)
      Reference <- substring(Ref_Location_start,0,17)
      Reference <- trimws(Reference)
      Reference <- gsub('Construction ','',Reference)
      Building_year <- Reference
      FURNISHED[m,'BUILDTING_YEAR'] <- Building_year
    }
  }
  
  #LOCATION EXTRACTION
  
  for (m in 1:1000){
    URL <- FURNISHED[m,2]
    webpage <- read_html(URL)
    Location_html <- html_nodes(webpage,'.cspDYs')
    Location <- html_text(Location_html)
    Location <- gsub('Toulouse ','',Location)
    Location <- gsub(')','',Location)
    Location <- substring(Location,2,7)
    print(Location)
    FURNISHED[m,'LOCATION'] <- Location
  }
  #HEATING FEATURES EXTRACTION
  
  for (m in 1:1000){
    URL <- FURNISHED[m,2]
    webpage <- read_html(URL)
    FEATURES_html <- html_nodes(webpage,'.bGqdzg') 
    FEATURES <- html_text(FEATURES_html)
    FEATURES <- paste(FEATURES, collapse = " ")
    FEATURES
    print(FEATURES)
    
    #LOOP FOR ELECTRICIty
    for (k in 1:1000){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,' kWhEP')
      Ref_Location_start
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Reference <- substring(Ref,Ref_Location_start[,1]-2,Ref_Location_start[,1])
      Reference
      Reference <- gsub(' ','',Reference)
      Reference
      Electricity <- Reference
      FURNISHED[m,'ELECTRICITY_HEATING'] <- Electricity
    }
    
    #LOOP FOR GAS
    for (k in 1:1000){
      Ref <- FEATURES
      Ref_Location_start <- str_locate(Ref,' kg')
      Ref_Location_start
      Ref_Location_start <- as.data.frame(Ref_Location_start)
      Reference <- substring(Ref,Ref_Location_start[,1]-2,Ref_Location_start[,1])
      Reference
      Reference <- gsub(' ','',Reference)
      Reference
      Gas <- Reference
      FURNISHED[m,'GAS_HEATING'] <- Gas
    }
  }
  
  View(FURNISHED)
  write.csv(FURNISHED,'FURNISHED.csv')
}

###########################################################
## 4 - MACHINE LEARNING SCRIPT FOR PREDICTION (Script 2) ##
###########################################################

##################################
## 4.1 - PACKAGE AND CSV IMPORTS ##
##################################

install.packages ("lmtest")
library(lmtest)
install.packages("car") 
library(car)
install.packages ("sandwich")
library(sandwich)
install.packages ("tseries")
library(tseries)

#KNOWING THAT THE METHOD/MODEL IS INCREMENTAL, WE USED THE EXAMPLE OF THE FIRST EXTRACTS TO DEMONSTRATE THE REGRESSION/PREDICTION
FURNISHED <- read.csv2("FURNISHED_Sample.csv",sep=",")
FURNISHED[,1] <- 1
FURNISHED <- na.omit(FURNISHED)

UNFURNISHED <- read.csv2("UNFURNISHED_Sample.csv",sep=",")
UNFURNISHED[,1] <- 2
UNFURNISHED <- na.omit(UNFURNISHED)


#######################################
## 4.2 - CLEANING/TRANSFORM DATASETS ##
#######################################

####################################
##4.2.1 -  MERGING THE 2 DATASETS ##
####################################

data <- rbind(FURNISHED,UNFURNISHED)
data <- na.omit(data)
data <- as.data.frame(data)

####################################
##4.2.2 -  DELETE USELESS DATA      ##
####################################

data <- cbind(data[,1],data[,5:13])
colnames(data) <- c('FU','PRICE','LIVING_SPACE','ROOMS','BEDROOMS','FLOOR','BUILDING_YEAR','LOCATION','ELECTRICITY','GAS')


###########################################
##4.2.3 -  RECODE PRICE ABOVE 1000 euros ##
###########################################

for (i in 1:length(t(data))){
  Longueur <- str_length(data[i,2])
  print(Longueur)
  if (Longueur>3){
    data[i,2] <- paste(substring(data[i,2],1,1),substring(data[i,2],3,5),sep='')
    print(data[i,2])
  }
}

###########################
##4.2.4 -  DATA CHECKING ##
###########################

str(data)
summary(data)


####################################
##4.2.5 - DATA VARIABLES RECODING ##
####################################

data$LIVING_SPACE <- as.integer(data$LIVING_SPACE)
data$PRICE <- as.integer(data$PRICE)
data$FU <- as.integer(data$FU)


##########################################################################
##4.3 - OBSERVATION OF EVERY RELATIONS BETWEEN PRICE AND OTHER VARIABLES ##
##########################################################################

############################
##4.3.1 - PRICE x SURFACE ##
############################

{reg<-lm(PRICE~LIVING_SPACE, data = data)
  summary(reg)
  coeff=coefficients(reg)
  reg$coefficients
  
  # Equation  
  eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
  
  # GRAPH
  plot(data$PRICE~data$LIVING_SPACE,main=eq)
  abline(reg, col="blue")}

##################################
##4.3.2 - PRICE x ALL VARIABLES ##
##################################

{reg<-lm(PRICE~FU+LIVING_SPACE + ROOMS + BEDROOMS + FLOOR
         + BUILDING_YEAR
         + LOCATION 
         + ELECTRICITY + GAS, data=data)
  
  # Equation  
  eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
  
  # GRAPH
  plot(reg,main=eq)
}

#####################################################
##4.4 - DATA MODELING (MULTIPLE LINEAR REGRESSION) ##
#####################################################

#########################################################################
##4.4.1 - IMPLEMENT THE REGRESSION AND CHECK THE GLOBAL SIGNIFICANCE ##
#########################################################################

#MLR = MULTIPLE LINEAR REGRESSION
MLR <- lm(PRICE~FU+LIVING_SPACE + ROOMS + BEDROOMS + FLOOR
          + BUILDING_YEAR
          + LOCATION 
          + ELECTRICITY + GAS, data=data)

summary(MLR)

#1st Step of the regression : check if the regression is globally significant
#To ensure that at least one of the coeffient of predictors is different that 0

#To do this we apply the ANOVA Test with the following parameters : 
#H0: all the coefficients are 0 
#H1: at least one coefficient is different from 0

#NB : Its possible to check the anova with the function 'summary'

#Conclusions : F-statistic: 352.6 on 9 and 337 DF,  p-value: < 2.2e-16
#Pvalue close to 0 so we reject H0
#Our regression is globally significant


###########################################################
##4.4.2 - WHICH VARIABLES ARE SIGNIFICANT IN THE MODEL ? ##
###########################################################

#Backward method.

MLR_2 <- lm(PRICE~LIVING_SPACE + FLOOR
            + BUILDING_YEAR, data=data)
summary(MLR_2)

#We have to remove all the predictors with a Pvalue above 0.05 (Or those without 3 stars)
#So we are keeping LIVING_SPACE,FLOOR AND BUILDING_YEAR
MLR <- MLR_2

############################################
##4.5 - EVALUATE THE QUALITY OF THE MODEL ##
############################################

#########################
##4.5.1 - R² ANALYSISL ##
#########################

#R²:  0.898 and Adjusted R²: 0.8971

#89.8% of the variance of Price is explained by the model, which is a good validation for the model
#But studying R² is not enough to completely validate the model, we have to check hypotheses on the residuals

#####################################################
##4.5.2 - STUDY OF THE HYPOTHESIS OF THE RESIDUALS ##
#####################################################

######################################################
##4.5.2.1 - FIRST HYPOTHESIS : IS RESIDUALS MEAN 0? ##
######################################################

summary(MLR$residuals)

#Results :
# Min.     1st Qu.  Median   Mean   3rd Qu.  Max. 
# -553.47  -69.44   -2.46    0.00   77.81    360.72 

#residuals mean is 0 so the first hypothesis is verified

#######################################################################
##4.5.2.2 - SECOND HYPOTHESIS : ARE RESIDUALS NORMALLY DISTRIBUTED ? ##
#######################################################################

#To verify the second hypothesis, we have to test the model with the Shapiro-Wilk Test and the Jarque-Bera test

#Shapiro Test:
#H0 is normally distributed
#H1 is not normally distributed

shapiro.test(MLR$residuals)

#Results :
#data:  MLR$residuals
#W = 0.92489, p-value = 3.452e-12

#pvalue <5%, we reject H0 so residuals are not normally distributed

jarque.bera.test(MLR$residuals)

#Results :
#data:  MLR$residuals
#X-squared = 229.13, df = 2, p-value < 2.2e-16

#So, our data are not normally distributed, in other words, the departure from normality,
#as measured by the test statistic, is statistically significant.
#We can observe the distribution with the gaussian curve to ensure that it's almost normally distributed

mean_residuals <- mean(MLR$residuals)
sd_residuals <- sd(MLR$residuals)
max <- max(MLR$residuals)
min <- min(MLR$residuals)
x <- seq(min,max,by=1)
y <- dnorm(x, mean = mean_residuals, sd = sd_residuals)
plot(x,y)

#With this plot, we can observe that it's almost normally distributed so, the hypothesis is acceptable


##############################################################
##4.5.2.3 - THIRD HYPOTHESIS : ARE RESIDUALS HOMOSKEDASTIC? ##
##############################################################

#To verify the third hypothesis, we have to test the model with the Breush Pagan Test

#Breush Pagan Test :  
#H0 : homoskedasticity 
#H1 : heteroskedasticity 

bptest(MLR,studentize=FALSE, data=data)

#data:  MLR
#BP = 55.086, df = 3, p-value = 6.581e-12
#pvalue <5%, we reject H0 so residuals are heteroskedastic


#4.5.2.3 (bis) Version White test that includes all regressors, their square and cross product
bptest(MLR,~I(FU)^2+I(LIVING_SPACE)^2 
       + I(ROOMS)^2 + I(BEDROOMS)^2 
       + I(FLOOR)^2 + I(BUILDING_YEAR)^2 
       + I(LOCATION)^2 
       + I(ELECTRICITY)^2 + I(GAS)^2, student=F,data=data)

#Conclusion remains the same


#Test for Heteroskedasticity
vcov2 <- vcov(MLR,type='HC1')
coeftest(MLR,vcov. = vcov2)

#Results :

#t test of coefficients:
  
  #Estimate Std. Error t value  Pr(>|t|)    
  #(Intercept)   932.74701  199.03141  4.6864 4.016e-06 ***
  #LIVING_SPACE   10.68653    0.21638 49.3876 < 2.2e-16 ***
  #FLOOR          31.99691    4.52645  7.0689 8.791e-12 ***
  #BUILDING_YEAR  -0.45193    0.10072 -4.4868 9.883e-06 ***
  #---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Conclusion of the test, the parameters are significant despite of the heteroskedasticity

################################################################
##4.5.2.4 - FOUTH HYPOTHESIS : ARE RESIDUALS AUTOCORRELATED ? ##
################################################################

#To verify the fourth hypothesis, we have to test the model with the Durbin Watson Test

#Durbin Watson Test
#H0 : residuals are not autocorrelated
#H1 : residuals are aucorrelated


durbinWatsonTest (MLR,max.lag=1)

#lag     Autocorrelation  D-W Statistic  P-value
#1       0.2929013        1.370211       0

#pvalue <5%, we reject H0 so residuals are autocorrelated

vcov4 <- NeweyWest(MLR)
coeftest(MLR,vcov. = vcov4)

#Results :
#t test of coefficients:
  
#  Estimate Std. Error t value  Pr(>|t|)    
#  (Intercept)  43.37073   19.78236  2.1924   0.02902 *  
#  LIVING_SPACE 10.63530    0.21474 49.5273 < 2.2e-16 ***
#  FLOOR        30.77958    6.29586  4.8889 1.559e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#We have to select only the parameters with the pvalue <5%

MLR_3 <- lm(PRICE~LIVING_SPACE + FLOOR, data=data)
summary(MLR_3)

MLR <- MLR_3

#This the final version of our regression


######################################
##4.6 - CHECK FOR MULTICOLLINEARITY ##
######################################

#To check the multicollinearity, just check that all the VIF are below 5

vif(MLR)
#all the VIF are <5, so there is no multicollinearity


###################
##5 - PREDICTION ##
###################

#JUST ENTER THE CARACTERISTIC OF AN APARTEMENT TO RENT IN TOULOUSE 
{
  {in.LIVING_SPACE <- readline(prompt="Living Space : ")
  in.LIVING_SPACE <- as.integer(in.LIVING_SPACE)}
  
  {in.ROOMS <- readline(prompt="Rooms: ")
    in.ROOMS <- as.integer(in.ROOMS)}
  
  {in.BEDROOMS <- readline(prompt="Bedrooms: ")
    in.BEDROOMS <- as.integer(in.BEDROOMS)}
  
  {in.FLOOR <- readline(prompt="Floor: ")
    in.FLOOR <- as.integer(in.FLOOR)}
  
  {in.BUILDING_YEAR <- readline(prompt="Building Year: ")
    in.BUILDING_YEAR <- as.integer(in.BUILDING_YEAR)}
  
  {in.LOCATION <- readline(prompt="Location: ")
    in.LOCATION <- as.integer(in.LOCATION)}
  
  {in.Elec <- readline(prompt="Elec: ")
    in.Elec <- as.integer(in.Elec)}
  
  {in.Gas <- readline(prompt="Gas: ")
    in.Gas <- as.integer(in.Gas)}
  
  predictors1 <- data.frame(LIVING_SPACE=in.LIVING_SPACE,ROOMS=in.ROOMS,BEDROOMS=in.BEDROOMS,FLOOR=in.FLOOR,BUILDING_YEAR=in.BUILDING_YEAR,
                            LOCATION=in.LOCATION,ELECTRICITY=in.Elec,GAS=in.Gas)
  predict(regmult1, predictors1, type="response",interval = "prediction", level = 0.95)
}

#Example with Valentin's apartment
"Living Space : 115
Rooms: 5
Bedrooms: 3
Floor: 1
Building Year: 1950
Location: 31000
Elec: 150
Gas: 15"

#   fit       lwr       upr
#   1312      1057      1567

#With our prediction, we can see a recommendation for the price of this apartment in function of its features
#With a precision of 95%

#Fit should be the best price but the model gives you the lower or upper price.

