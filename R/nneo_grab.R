#############################################################################################
#' @title  Access, download, and save NEON data products.

#' @author Josh Roberti \email{jroberti@BattelleEcology.org}\cr
#' Dave Durden\cr
#' Robert Lee

#' @description \code{grab.NEON}gathers neon data via the API and concatonates files per each
#' measurement level and/or variable.  These data can then be analyzed for commissioning
#' or other purposes using script by R Lee
#'
#' @param Site Parameter of class character. The NEON site to download data for.
#' @param time_bgn Parameter of class POSIX. Defines the start date of data to be downloaded.
#' @param time_end Parameter of class POSIX. Defines the end date of data to be downloaded.
#' @param time_agr Parameter of class numeric. The aggregation period for L1 DPs to be downloaded (e.g. `30` corresponds to a 30-minute data product).
#' @param data_var Parameter of class character. Identify the data product to be downloaded.
#' @param Pack Parameter of class character. A parameter to choose the basic or expanded NEON data product package.
#' @param dat_dir Parameter of class character. Directory where the file is written to.
#' @param useSpatial Logical. Searches NEON "As-Built" data files for number of replicate measurement levels for each measurement assembly. Defaults to FALSE.  If using for TIS data products and within the CommTis package it can be set to TRUE to acknowledge missing data levels.


#' @return Prints a .csv file in the Science Commissioning data archive folder with all the data variables for a data product at all locations in the soil plots and on the tower.
#'
#' @references
#' Terrestrial Instrumentation System (TIS) Science Commissioning Plan [NEON.DOC.003779]

#' @keywords process quality, key performance indicator (KPI), commissioning

#' @examples
#' Site <- "BART"
#' time_bgn <- as.POSIXct("2016-01-01 01:12:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
#' time_end <- as.POSIXct("2016-02-01 10:20:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
#' dat_dir <- "N:/Science/Science Commissioning Archive/SiteAndPayload/TisRadiationProcessQuality"
#' data_var <- "Photosynthetically active radiation (Quantum Line)"
#' time_agr <- 1
#' Pack <- "basic"
#' grabNEON(Site=Site, time_bgn=time_bgn, time_end=time_end, data_var="Photosynthetically active radiation (Quantum Line)", time_agr=30, Pack="basic", dat_dir=dat_dir)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti (2016-10-24)
#     original creation
#   Dave Durden (2016-11-21)
#     Applying standard style following Wiki and packaging
#   Josh Roberti (2016-12-09)
#     Amended code so full expanded data package is preserved.
#     Fixed regex for greping files from API
#     version updated to 0.0.2.
#   Josh Roberti (2017-01-09)
#     Updated code so it can be (better) used external of wrapper
#   Robert Lee (2017-04-07)
#     Changing formatting to confrom to rOpenSci
#   Josh Roberti (2017-04-20)
#     Amended code to submit to rOpenSci
##############################################################################################

grabNEON<-function(
    Site="BART",   #change to lowercase,
    time_start="2015-10-01",  #preset this to NULL or a date (if date, make only month span)
    time_end="2015-11-15",#preset this to NULL or a date (if date, make only month span)
    data_var= "active radiation",
      #"Photosynthetically active radiation (Quantum Line)",
    time_agr=30,
    pack_type="basic"   #change to lowercase
    #dat_dir=dirKpiApiOut   #write this out to tibble or df etc., not to a file:

    #useSpatial=FALSE  # will need to set to FALSE if running external of wrapper
){
    #outputthe available data products via nneo package and NEON API:
    #availableNEONDataProducts<<-as.data.frame(nneo::nneo_products())[which(as.data.frame(nneo::nneo_products())$productStatus=="ACTIVE"),c("productName","siteCodes")]

    #Rounding input time to nearest whole hour
   # time_bgn <- round(time_bgn, "hours")
    #time_end <- round(time_end, "hours")

    # if(is.null(WriteDir)==TRUE){
    #   stop("write directory must be specified")
    # }
    #options(warn=-1)
    #directory<-dat_dir
    #setwd(dat_dir)
    #Site<<-Site
    #load dependencies:
    #library(nneo)
    #library(lubridate)
    #---get site information----#
    #site_info<-lapply(Site, function(x) nneo::nneo_site(x))

    #valid_site<-grep(Site,nneo::nneo_sites()$siteCode)

################# ------------ START ---------------- ####################
    #grab site metadata:
    site_info<-nneo::nneo_site(Site)

    #get data product code(s) if valid:
    product_code<-site_info$dataProducts$dataProductCode[grep(tolower(gsub("\\(", "",data_var)),
                              gsub("\\(", "",tolower(site_info$dataProducts$dataProductTitle)))]
    #if empty:
    if(length(product_code)==0){stop(paste0("data product(s) not available for: ", Site))}

    #create year_month variable for nneo_data and nneo_file:
    year_month<-c(substr(time_start,0,7),substr(time_end,0,7))
    #check if it's same month - won't need to gather multiple monthly files:
    if(length(year_month)==1){year_month<-year_month[1]}

    #use nneo_data to get available file(s) via user input:
    var_data<-unlist(lapply(year_month, function(y) lapply(product_code,
                     function(x) nneo::nneo_data(product_code = x,
                                           site_code = Site,
                                           year_month = y,
                                           package=pack_type))),recursive = FALSE)

    #combine data filenames into one df:
    files_pack_type<-do.call("rbind",lapply(lapply(var_data, "[[", "data"),
                                            "[[", "files"))
    #get filenames that match user requested time_agr and sort:
    files_time_agr<-sort(files_pack_type$name[grep(paste0(time_agr,"min.csv"),
                                        files_pack_type$name)])
    #get the data using nneo_file:
    data_all<-lapply(year_month, function(y) lapply(unique(files_time_agr),
                                   function(x) nneo::nneo_file(product_code = substr(x,15,27),
                                                               site_code = Site,
                                                               year_month = y,
                                                               filename = x)))#,recursive = FALSE)
    #name first level of list:
    names(data_all)<-year_month
    #rbind dataframes of similar data:
    data_length<-unique(unlist(lapply(data_all, function(x) length(x))))
    crap<-list()
    for(i in 1:data_length){
      crap[[i]]<-do.call("rbind",lapply(data_all, "[[", i))
      names(crap[[i]])<-c(names(crap[[i]])[1:2],
                          paste0(names(crap[[i]][3:length(crap[[i]])]),
                                 substr(unique(files_time_agr),34,41)[i]))
    }
    #merge data:
    data_merge<-Reduce(function(x, y) merge(x, y, by=c("startDateTime","endDateTime")), crap)

    #final filter by date:
    date_filt_start<-min(grep(time_start,as.Date(substr(data_merge$startDateTime,0,10))))
    date_filt_end<-max(grep(time_end,as.Date(substr(data_merge$startDateTime,0,10))))
    data_final<-data_merge[date_filt_start:date_filt_end,]
    #output:
    return(data_final)
}


#     #
# #     for(i in 1:length(data_all)){
# #       first_two<-names(data_all[[i]][1:2])
# #       names(data_all[[i]])<-paste0(names(data_all[[i]]),
# #                                    substr(names(data_all[i]),
# #                                           nchar(names(data_all[i]))-11,
# #                                           nchar(names(data_all[i]))-4))
# #       #reset first two colnames to merge dfs:
# #       names(data_all[[i]])<-c(first_two,names(data_all[[i]][3:length(names(data_all[[i]]))]))
# #     }
#
#
#     #test<-do.call("rbind",lapply(data_all, `[`, 1))
#
#
#     # lapply(data_all, "[[", 1:length((lapply(data_all, `[`))))
#     #
#     # lapply(data_all, function(x) length(x))
#     #
#     #
#     # lapply(lapply(data_all, "[[", function(x)  x), setNames, nm = new_col_name)
#     #
#     # sapply(x, function(mat) split(data_all, col(mat))[1])
#
#     data_merge<-Reduce(function(x, y) merge(x, y, by=c("startDateTime","endDateTime")), data_all)
#
#     #merge the data based on startDateTime and endDateTime:
#     #data_merge<-Reduce(function(x, y) merge(x, y, by=c("startDateTime","endDateTime")), data_all)
#
#     #assign DP name to nested tibbles:
#     #names(data_all)<-rep(unique(substr(files_time_agr,0,45)),2)
#
#
#     # data_all<-lapply(files_time_agr,
#     #                  function(x) nneo::nneo_file(product_code = substr(x,15,27),
#     #                                              site_code = Site,
#     #                                        year_month = year_month[1],
#     #                                        filename = x))
#
#
#
#     #assign DP name to nested tibbles:
#     #names(data_all)<-substr(files_time_agr,0,45)
#     #assign spatial identifers to colnames of nested tibbles:
#     for(i in 1:length(data_all)){
#       first_two<-names(data_all[[i]][1:2])
#       names(data_all[[i]])<-paste0(names(data_all[[i]]),
#                                   substr(names(data_all[i]),
#                                          nchar(names(data_all[i]))-11,
#                                          nchar(names(data_all[i]))-4))
#       #reset first two colnames to merge dfs:
#       names(data_all[[i]])<-c(first_two,names(data_all[[i]][3:length(names(data_all[[i]]))]))
#     }
#     #merge the data based on startDateTime and endDateTime:
#     data_merge<-Reduce(function(x, y) merge(x, y, by=c("startDateTime","endDateTime")), data_all)
#
#
#     ################# ------------ END ---------------- ####################
#
#
#     #read NEON webpage that houses .csv file of current sites:
#     # master_site_page<-"http://www.neonscience.org/science-design/field-sites/export"
#     # all_sites <<- read.csv(master_site_page,header=TRUE,stringsAsFactors = FALSE)
#     # NEON_site<-subset(all_sites,Site.ID==Site)
#     # #check for valid site ID entry:
#     # if(length(NEON_site$Site.ID)== 0){
#     #     stop("invalid Site ID! Please enter a valid, 4-Letter NEON Site ID")
#     #     # select.list(all_sites[,"Site.ID"],title = "WARNING: invalid NEON SiteID! Please enter a valid, 4-letter, NEON Site ID; please choose a 4-Letter Site ID from below:")
#     #     # Site<<-readline(prompt="Selection:")
#     #     #
#     # }
#     #domain<-NEON_site$Domain.Number
#     #--------SITE---------#
#     #
#     #Split out the YYYY-MM
#     bgn_year<-substr(time_bgn,0,4)
#     bgn_mth<-substr(time_bgn,6,7)
#     end_year<-substr(time_end,0,4)
#     end_mth<-substr(time_end,6,7)
#
#     #-------BGN MTH-------#
#     #check for valid Month:
#     if(nchar(bgn_mth)!=2|(as.numeric(bgn_mth)<=12)==FALSE){
#         stop("invalid Begin Month! Please enter a valid begin month in 'mm' format")
#     }
#     #-------BGN MTH-------#
#     #
#     #-------END MTH-------#
#     #check for valid Month:
#     if(nchar(end_mth)!=2|(as.numeric(end_mth)<=12)==FALSE){
#         stop("invalid End Month! Please enter a valid end month in 'mm' format")
#     }
#     #-------END MTH-------#
#     #
#     #-------BGN YEAR------#
#     #check for valid Month:
#     if(nchar(bgn_year)!=4|(as.numeric(bgn_year)>=2012 & (as.numeric(bgn_year)<=as.numeric(format(Sys.Date(), "%Y")))==FALSE)){
#         stop("invalid Begin Year! Please enter a valid begin year in 'YYYY' format")
#     }
#     #-------BGN YEAR------#
#     #
#     #-------END YEAR------#
#     #check for valid Month:
#     if(nchar(end_year)!=4|(as.numeric(end_year)>=2012 & as.numeric(end_year)<=as.numeric(format(Sys.Date(), "%Y")) & as.numeric(end_year)>=as.numeric(bgn_year))==FALSE){
#         stop("invalid End Year! Please enter a valid end year in 'YYYY' format")
#     }
#     #-------END YEAR------#
#
#     #CONCATONATE DATES:
#     bgn_date<-substr(time_bgn,0,7)
#     end_date<-substr(time_end,0,7)
#
#     #Building the regularized time sequence
#     time_agrSec<-time_agr*60
#     time_rglr<-data.frame(POSIXseq=seq(time_bgn,time_end,by = time_agrSec))
#
#     #-------DATA VAR------#
#     #call NEON products using "nneo"
#     products_NEON<-as.data.frame(nneo_products())
#     #data_var2<-gsub(" \\s*\\([^\\)])","\\\\\"",data_var)
#     #data_var2<-gsub("\\s*\\("," \\(",data_var) #account for parentheses   \\s*\\([^\\)]+\\)
#     #exact match:
#     #data_var<-paste0("^",data_var2)
#     #if(grep(data_var)
#     #find which row has the DP of interest:
#     valid_product<-products_NEON[which(data_var==products_NEON$productName),c("productName","productCode","siteCodes")]
#     # products_NEON[grep(data_var,products_NEON$productName),c("productName","productCode","siteCodes")]
#     if(nrow(valid_product)==0){
#         stop("invalid 'data_var'! Please enter a valid, data_var. See 'nneo::nneo_products()$productName' for valid names.")
#     }
#     #check to see if data are available for the user-entered site:
#     if(Site%in%valid_product$siteCodes[[1]]$siteCode==FALSE){
#         stop(paste0("The specified 'data_var'is unavailable for ", Site," Please see 'nneo::nneo_products()' for a full list of variables per site.",collapse=" "))
#     }
#     #grab index of Site we want to grab data for:
#     site_index<-which(valid_product$siteCodes[[1]]$siteCode==Site)
#     #check to see if data are available for the user-entered site for the specified timeframe:
#     if(bgn_date%in%valid_product$siteCodes[[1]]$availableMonths[[site_index]]==FALSE){
#         stop("The specified start date is unavailable for the NEON site. The available dates for this site and data product are: ",paste(valid_product$siteCodes[[1]]$availableMonths[[site_index]],collapse=" "))
#     }
#     if(end_date%in%valid_product$siteCodes[[1]]$availableMonths[[site_index]]==FALSE){
#         stop("The specified end date is unavailable for the NEON site. The available dates for this site and data product are: ",paste(valid_product$siteCodes[[1]]$availableMonths[[site_index]],collapse=" "))
#     }
#     #date sequence to grab:
#     start_date_index<-grep(bgn_date,valid_product$siteCodes[[1]]$availableMonths[[site_index]])
#     end_dateIndex<-grep(end_date,valid_product$siteCodes[[1]]$availableMonths[[site_index]])
#     grab_these_dates<-valid_product$siteCodes[[1]]$availableMonths[[site_index]][start_date_index:end_dateIndex]
#     #grab data product ID to use in API:
#     data_prod_ID<-valid_product$productCode
#     #-------DATA VAR------#
#     #
#     #-------TIME AGR------#
#     validtime_agr<-c(1,2,5,10,30,60)
#     if((time_agr %in% validtime_agr)==FALSE){
#         stop("invalid time aggregation! Please enter a valid, aggregation period, e.g., 1 or 30")
#     }
#     #-------TIME AGR------#
#     #
#     #-------BUILD API REQUEST----#
#     #create vector of APIs to get:
#     find_API<-lapply(grab_these_dates, function(x) paste0("http://data.neonscience.org/api/v0/data/",data_prod_ID,"/",Site,"/",x,"?package=",Pack))
#     #find_API<-paste0("http://data.neonscience.org/api/v0/data/",data_prod_ID,"/",Site,"/",grab_these_dates[1],"?package=",Pack)  #single file test
#     linksAPI<-unlist(lapply(find_API, function(x)  unlist(strsplit(readLines(x),split="\""))))
#     #linksAPI<-unlist(strsplit(readLines(find_API),split="\"")) #left off here  #single link test
#     grep_API<- grep(paste0("_",time_agr,".*min.*.csv\\?package=",Pack),linksAPI)
#     useTheseLinks<-sort(unique(linksAPI[grep_API]))#sorting here or else the function mismatches DP names...
#     #what soil plot is the sensor in ? (0 = tower; >=1 soil plot <=5)
#     find_location<-unlist(strsplit(useTheseLinks,split="/"))
#     file_names<-(grep("NEON.",find_location,value=TRUE))
#     #locations and time:
#     location<-strsplit(substr(grep("[0-9]{3}.[0-9]{3}.",find_location,value=TRUE),34,41),",")
#     year_month<-strsplit(substr(grep("[0-9]{4}-[0-9]{2}",useTheseLinks,value=TRUE),63,69),",")
#
#     data<-lapply(useTheseLinks,function(x){read.csv(x,header=TRUE)})
#     names(data)<-file_names
#
#     #assign new names:
#     data2<-data
#     for(i in 1: length(data)){names(data2[[i]])<-paste0(names(data[[i]]),location[[i]])}
#     #convert to startTime to POSIX:
#     for(i in 1: length(data2)){data2[[i]]$POSIXstart<-as.POSIXct(gsub("T|Z", " ", data2[[i]][,1]),format="%Y-%m-%d %H:%M:%S", tz="UTC")}
#     #sort the list by name first before splitting:
#     # data4<-lapply(data2, function(df){
#     #     data2[order(df$enrichment),]
#     # })
#     #split the list by different measurement levels:
#
#     data3<-split(data2,sort(as.factor(unique(names(data2)))))
#     #create dynamic DFs based on how many different locations were downloaded
#     #newDFs<-paste0("df",seq(1,length(unique(names(data2))),1))
#     merge_these<-list()
#     for(i in 1:length(data3)){
#         df<-as.data.frame(do.call("rbind", data3[[i]]))
#         rownames(df)<-NULL #remove row names
#         merge_these[[i]]<-df #add dfs to list
#         assign(paste0("df",i),df) #assign individual names to Rbinded DFs
#     }
#     all_data<-Reduce(function(x, y) merge(x, y, all=TRUE, by="POSIXstart"), merge_these)
#     #find if and where (row index) the start and end dates are located in the file:
#     grep_start<-which(all_data$POSIXstart==as.POSIXlt(time_bgn))
#     if((length(grep_start)==0)==TRUE){grep_start<-1}
#     grep_end<-which(all_data$POSIXstart==as.POSIXlt(time_end))
#     if((length(grep_end)==0)==TRUE){grep_end<-nrow(all_data)}
#     #truncate data to start and end dates:
#     all_data_trunc<-all_data[grep_start:grep_end,]
#     #merge the truncated data with the "true Posix Times:"
#     final_data<- merge(time_rglr, all_data_trunc,by.x="POSIXseq",by.y="POSIXstart",all.x = TRUE)
#
#
#
#     #############--------------Run replicate checks--------------------#############
#     #DATA file needed as input
#     #find column matches within the datafile:
#
#     if(useSpatial==TRUE){
#         subset_group<-lapply(names(spatialLocationList), function(x) grep(paste0("^",x),colnames(final_data)))
#
#         #assign same names to this as the locationdata_var to keep traceability:
#         names(subset_group)<-names(spatialLocationList)
#
#         #Granulate down to the respective data_var Level
#         subset_group2<-subset_group[lapply(subset_group,length)>0]
#         #names(locationdata_var) %in% names(subset_group2)
#         myvars <- names(spatialLocationList) %in% names(subset_group2)
#         subsetdata_var <- spatialLocationList[myvars]
#         #Check to see if we need to grab Quality Metrics (QM) and finalQF columns separately:
#         if(length(subset_group2)==1){
#             #create patterns to grep:
#             toMatch <- c(paste0("^","finalQF"), "QM")
#             #look for "FinalQF" columns if only one DP is in the file:
#             subset_groupQF<-grep(paste(toMatch,collapse="|"),names(final_data))#paste0("^","finalQF")
#             #append the subset_groupQF group to the subset_group and then
#             subset_group2[[1]]<-sort(append(subset_group2[[1]],subset_groupQF))
#         }
#
#     #subsetdata_var<-lapply(names(subset_group2), function(x) grep(names(locationdata_var),x))
#     # locationdata_varSubset<-lapply(names(locationdata_var), function(x) grep(paste0("^",x),colnames(final_data)))
#     #     grep(paste0("^",names(subset_group2)),names(locationdata_var))
#     #actually subset the data into n dataframes within a list:
#
#
#     final_dataList<-list()
#     missing_level_list<-list()
#     for(i in 1:length(subset_group2)){
#         #create dataframes for each data_var
#         new_df<-final_data[, subset_group2[[i]]]
#         #keep ONLY levels that should be there for data_var as defined in locationdata_var
#         #check if correct measurement levels are in the file:
#         if(data_var=="Photosynthetically active radiation (Quantum Line)"){
#             should_be_there_QL<-list()
#             should_be_there_QL2<-list()
#             for(j in 1:(length(subsetdata_var[[i]])/3)){ #3 is number of QL replicates at a site
#                 should_be_there_QL[[j]]<-lapply(subsetdata_var[[i]][,j], function(x) x %in% str_sub(names(new_df),-8,-1))
#                 should_be_there_QL2[[j]]<-all(unlist(should_be_there_QL[[j]]))
#             }
#             #find which one is true:
#             should_be_there<-subsetdata_var[[i]][,which(should_be_there_QL2==TRUE)]
#             if(length(should_be_there)<3){stop("QL sensor(s) missing from unknown soil plot")}
#             keep_these<-as.numeric(sapply(should_be_there, function(x) grep(x,names(new_df))))
#         }
#         else{
#             should_be_there<-subsetdata_var[[i]]%in% str_sub(names(new_df),-8,-1)
#             keep_these<-as.numeric(sapply(subsetdata_var[[i]], function(x) grep(x,names(new_df))))
#         }
#         missing_level<-which(should_be_there==FALSE)
#         #output which levels are actually missing
#         missing_level_list[[i]]<-subsetdata_var[[i]][missing_level]
#         if(length(missing_level_list[[i]])>0){
#             #create missing values
#             missing_columns_mean<-lapply(missing_level_list[[1]],
#                                          function(x) paste0(names(subset_group2[i]),"Mean",x))
#             missing_columns_QF<-lapply(missing_level_list[[1]],
#                                        function(x) paste0(names(subset_group2[i]),"FinalQF",x))
#             missing.df<-data.frame(matrix(NA, nrow = nrow(final_data), ncol = length(missing_columns_mean)*2))
#             names(missing.df)<-c(missing_columns_mean,missing_columns_QF)
#         }
#
#         #OPTION 1: pass thru correct data if available in file:
#         if(length(keep_these)>0 & length(missing_level_list[[i]])==0){new_df2<-new_df[,keep_these]}
#         #OPTION 2: pass thru correct data if available and add NA columns for missing levels:
#         if(length(keep_these)>0 & length(missing_level_list[[i]])!=0){
#             new_df2<-new_df[,keep_these]
#             new_df2<-cbind(new_df2,missing.df)
#         }
#         #OPTION 3: Pass missing data only if NONE of the measurement levels are there:
#         if(length(keep_these)==0 & length(missing_level_list[[i]])!=0){new_df2<-missing.df}
#         final_dataList[[i]]<-new_df2
#     }
#     #add the POSIX times back to the dataframe:
#     final_dataList[[i+1]]<-data.frame("POSIXseq"=final_data[,"POSIXseq"])
#     #merge the dataframes back together for the respective data_var(s) of the KPI:
#     final_cleaned_df<-do.call("cbind", final_dataList)
#     #reorder columns so "POSIXseq is first"
#     final_cleaned_df<-final_cleaned_df[,c(length(final_cleaned_df),1:(length(final_cleaned_df)-1))]
#     #add names to the missing_level_list:
#     missing_level_list2<-missing_level_list[lapply(missing_level_list,length)>0]
#     if(length(missing_level_list2)>0){
#         for(i in 1: length(missing_level_list2)){
#             missing_level_list_out[i]<-lapply(names(subset_group2[i]),
#                                               function(x) paste0(x,missing_level_list2[[i]]))
#         }
#         #add names:
#         names(missing_level_list_out)<-names(subset_group2)
#         #missing levels (output to results file):
#         missingLvls<-paste(unlist(missing_level_list_out),collapse=" ")
#     }
#     if(length(missing_level_list2)==0){missingLvls<-NA}
# }else{
#     #remove multiple start/end time columns for each level.
#     remove_these<-grep("startDateTime.*|end_dateTime.*",names(final_data))
#     final_cleaned_df<-final_data[,-c(remove_these)]
# }
#
# #############--------------Run replicate checks--------------------#############
#
#
# #write the file out:
# file_name<-paste0(dat_dir,"/",substr(file_names[1],0,27),"_REQ_",as.Date(time_bgn),"_",as.Date(time_end),"_",time_agr,"min_",Pack,".csv",".gz")
# zipIt<-gzfile(file_name)
# write.csv(final_cleaned_df,zipIt,row.names = FALSE) ####final_cleaned_df used here after
# #-------BUILD API REQUEST----#
# if(useSpatial==TRUE){
#     return(missingLvls)
# }
# else{
#     print(paste0("Data downloaded and saved to: ", file_name))
# }
# }
