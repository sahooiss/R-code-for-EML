library(dplyr)
library(EML)
library(rmarkdown)
library(RPostgreSQL) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# I think we want to set up a R package or function for the EML
#data input from the website:

input_data<-function(infile) {
  
dt1<-read.csv(infile,header=F,na.strings = "NaN", 
               skip=2,sep=",",quot='"', 
               col.names=c(
                 "cruise",     
                 "ship",     
                 "ship_code",     
                 "order_occupied",     
                 "tow_type",     
                 "net_type",     
                 "tow_number",     
                 "net_location",     
                 "standard_haul_factor",     
                 "volume_sampled",     
                 "percent_sorted",     
                 "sample_quality",     
                 "latitude",     
                 "longitude",     
                 "line",     
                 "station",     
                 "time",     
                 "scientific_name",     
                 "common_name",     
                 "itis_tsn",     
                 "calcofi_species_code",     
                 "larvae_count",     
                 "larvae_10m2",     
                 "larvae_1000m3"),
                check.names=TRUE) 


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
# dt1<-as.data.frame(df1)
# 
# dt1$time<-strptime(paste0(substr(dt1$time,1,10)," ",substr(dt1$time,12,19)),format="%Y-%m-%d %H:%M:%S")

if (class(dt1$cruise)!="factor") dt1$cruise<- as.factor(dt1$cruise)
if (class(dt1$ship)!="factor") dt1$ship<- as.factor(dt1$ship)
if (class(dt1$ship_code)!="factor") dt1$ship_code<- as.factor(dt1$ship_code)
if (class(dt1$order_occupied)!="factor") dt1$order_occupied<- as.factor(dt1$order_occupied)
if (class(dt1$tow_type)!="factor") dt1$tow_type<- as.factor(dt1$tow_type)
if (class(dt1$net_type)!="factor") dt1$net_type<-as.factor(dt1$net_type)
if (class(dt1$tow_number)=="factor") dt1$tow_number<-as.numeric(levels(dt1$tow_number))[as.integer(dt1$tow_number) ]
if (class(dt1$net_location)!="factor") dt1$net_location<-as.factor(dt1$net_location)
if (class(dt1$standard_haul_factor)!="factor") dt1$standard_haul_factor <-as.factor(dt1$standard_haul_factor)
if (class(dt1$volume_sampled_cubic_meters)=="factor") dt1$volume_sampled_cubic_meters<- as.numeric(levels(dt1$volume_sampled_cubic_meters))[dt1$volume_sampled_cubic_meters]
if (class(dt1$percent_sorted)=="factor") dt1$percent_sorted<- as.numeric(levels(dt1$percent_sorted))[dt1$percent_sorted]
if (class(dt1$sample_quality)!="factor") dt1$sample_quality<- as.factor(dt1$sample_quality)
if (class(dt1$latitude_degrees_north)=="factor") dt1$latitude_degrees_north<- as.numeric(levels(dt1$latitude_degrees_north))[dt1$latitude_degrees_north]
if (class(dt1$longitude_degrees_east)=="factor") dt1$longitude_degrees_east<- as.numeric(levels(dt1$longitude_degrees_east))[dt1$longitude_degrees_east]
if (class(dt1$line)=="factor") dt1$line<- as.numeric(levels(dt1$line))[dt1$line]
if (class(dt1$station)=="factor") dt1$station<- as.numeric(levels(dt1$station))[dt1$station]
if (class(dt1$scientific_name)!="factor") dt1$scientific_name<- as.factor(dt1$scientific_name)
if (class(dt1$common_name)!="factor") dt1$common_name<- as.factor(dt1$common_name)
if (class(dt1$itis_tsn)=="factor") dt1$itis_tsn<- as.numeric(levels(dt1$itis_tsn))[dt1$itis_tsn]
if (class(dt1$calcofi_species_code)!="factor") dt1$calcofi_species_code<- as.factor(dt1$calcofi_species_code)
if (class(dt1$larvae_count)=="factor") dt1$larvae_count<- as.numeric(levels(dt1$larvae_count))[dt1$larvae_count]
if (class(dt1$larvae_10m2)=="factor") dt1$larvae_10m2<- as.numeric(levels(dt1$larvae_10m2))[dt1$larvae_10m2]
if (class(dt1$larvae_1000m3)=="factor") dt1$larvae_1000m3<- as.numeric(levels(dt1$larvae_1000m3))[dt1$larvae_1000m3]

return(dt1)

}

data1<-input_data("http://coastwatch.pfeg.noaa.gov/erddap/tabledap/erdCalCOFIlrvcntAtoAM.csv?cruise%2Cship%2Cship_code%2Corder_occupied%2Ctow_type%2Cnet_type%2Ctow_number%2Cnet_location%2Cstandard_haul_factor%2Cvolume_sampled%2Cpercent_sorted%2Csample_quality%2Clatitude%2Clongitude%2Cline%2Cstation%2Ctime%2Cscientific_name%2Ccommon_name%2Citis_tsn%2Ccalcofi_species_code%2Clarvae_count%2Clarvae_10m2%2Clarvae_1000m3&time%3E=2015-04-12T00%3A00%3A00Z&time%3C=2015-04-19T17%3A38%3A00Z")
data2<-input_data("http://coastwatch.pfeg.noaa.gov/erddap/tabledap/erdCalCOFIlrvcntANtoAR.csv?cruise%2Cship%2Cship_code%2Corder_occupied%2Ctow_type%2Cnet_type%2Ctow_number%2Cnet_location%2Cstandard_haul_factor%2Cvolume_sampled%2Cpercent_sorted%2Csample_quality%2Clatitude%2Clongitude%2Cline%2Cstation%2Ctime%2Cscientific_name%2Ccommon_name%2Citis_tsn%2Ccalcofi_species_code%2Clarvae_count%2Clarvae_10m2%2Clarvae_1000m3&time%3E=2015-04-12T00%3A00%3A00Z&time%3C=2015-04-19T17%3A38%3A00Z")
data3<-input_data("http://coastwatch.pfeg.noaa.gov/erddap/tabledap/erdCalCOFIlrvcntAStoBA.csv?cruise%2Cship%2Cship_code%2Corder_occupied%2Ctow_type%2Cnet_type%2Ctow_number%2Cnet_location%2Cstandard_haul_factor%2Cvolume_sampled%2Cpercent_sorted%2Csample_quality%2Clatitude%2Clongitude%2Cline%2Cstation%2Ctime%2Cscientific_name%2Ccommon_name%2Citis_tsn%2Ccalcofi_species_code%2Clarvae_count%2Clarvae_10m2%2Clarvae_1000m3&time%3E=2015-04-12T00%3A00%3A00Z&time%3C=2015-04-19T17%3A38%3A00Z")

# df1<-data1 %>%
#   select(line,station,time,volume_sampled_cubic_meters,scientific_name,common_name,itis_tsn,calcofi_species_code,larvae_count,larvae_10m2,latitude_degrees_north,longitude_degrees_east) %>%
#   rename(site_id=line,transect_id=station,date=time,volume=volume_sampled_cubic_meters,
#          auth_name=scientific_name,auth_taxon_id=itis_tsn,proj_taxon_id=calcofi_species_code,count=larvae_count,
#          density=larvae_10m2,latitude=latitude_degrees_north,longitude=longitude_degrees_east)

data<-rbind(data1,data2,data3)

write.csv(data,"CalCOFI_larvae_count_20170413.csv",row.names = F)
#-------------------------

# input postgreSQL table

pw <- "whatever"

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "bon_data_pkgs",
                 host = "rdb2", port = 5432,
                 user = "read_only_user", password = pw)
rm(pw) # removes the password

# check for the table
meta<-dbReadTable(con, c("mb2eml_r","vw_eml_attributes")) 
meta_unit<-dbReadTable(con, c("mb2eml_r","vw_custom_units")) 

dbDisconnect(con)

#----------------------
#input datatable

df <- read.csv("CalCOFI_larvae_count_20170413.csv", stringsAsFactors = T, header=TRUE)

df$time<-as.Date(df$time)
#set up the template for attribute metadata
cname<-colnames(df)
attributes_name<-data.frame(cname)
  
attributes<-attributes_name%>%
  rename(attributeName=cname) %>%
  left_join(meta,by="attributeName") %>%
  select(-AttributeID)

attributes$col_classes[attributes$measurementScale=="nominal"] <- "character"
attributes$col_classes[attributes$measurementScale=="ratio"] <- "numeric"  
attributes$col_classes[attributes$measurementScale=="dateTime"] <- "Date"  
attributes$col_classes[attributes$nonnumericDomain=="enumeratedDomain"] <- "factor" 

attributes$missingValueCode[attributes$attributeName=="larvae_1000m3"]<-"NAN"
attributes$missingValueCodeExplanation[attributes$attributeName=="larvae_1000m3"]<-"value not recorded or not available"


write.csv(attributes, "CalCOFI_larval_Metadata.csv", row.names = FALSE)


#-----------------------------
#define attributes

attributes <- read.csv("CalCOFI_larval_Metadata.csv", header = TRUE, sep = ",",na.strings = "",stringsAsFactors = FALSE)
#standardUnits <- get_unitList()
#View(standardUnits$units)

col_classes<-as.character(attributes[,"col_classes"])
attributes$col_classes=NULL

unitsList <- set_unitList(meta_unit)


attributeList <- set_attributes(attributes,col_class=col_classes)

#------------------------------
#access
principal1<-new("principal","uid=SBC,dc=ecoinformatics,dc=org")
principal2<-new("principal","public")
permission<-new("permission","all")

access1<-new("allow",principal=list(principal1),permission=permission)

access2<-new("allow",principal=list(principal2),permission=permission)

access <- new("access", 
          authSystem="https://pasta.lternet.edu/authentication",
          order="allowFirst",
          scope="document",
          system="https://pasta.lternet.edu",
          allow=c(access1,access2)
)



#physical attributes
filename<-"CALCOFI_larvae_count_20170413.csv"
url_head<- "http://sbc.lternet.edu/external/Collaborative_Research/SBCMBON_temp/Data/"


size0<-as.character(file.size("CALCOFI_larvae_count_20170413.csv"))
physical<-set_physical(filename,size=size0, sizeUnit="byte",url=paste0(url_head,filename),
                     numHeaderLines="1",recordDelimiter="\\r\\n",fieldDelimiter=",",quoteCharacter="\"")

row=nrow(df)
dataTable <- new("dataTable",
                 entityName = "SBCMBON CALCOFI larvae",
                 entityDescription = "CALCOFI larvae count",
                 physical = physical,
                 attributeList = attributeList,
                 numberOfRecords=as.character(row)
                 )


title <- "Integrated CALCOFI larval count"
pubDate <- "2017-04-13"


abstract <- as(set_TextType("5_SBCMBONAbstract.docx"), "abstract")

group <- c(as.person("Andrew Thompson <andrew.thompson@noaa.gov>"),
           as.person("Robert Miller <miller@msi.ucsb.edu >"))
           

creator <- as(group, "creator")


#publisher
NTL_address <- new("address",
                   deliveryPoint = "CalCOFI",
                   city = "La Jolla",
                   administrativeArea = "CA",
                   postalCode = "92037",
                   country = "USA")
publisher <- new("publisher",
                 organizationName = "CalCOFI",
                 address = NTL_address)
#contact

NTL_contact <- new("individualName",
                   givenName="Andrew",
                   surName="Thompson")

contact <- new("contact",individualName = NTL_contact,
               electronicMail = "andrew.thompson@noaa.gov",
               address = NTL_address,
               organizationName = "CalCOFI")

keywordSet <- new("keywordSet", keyword = c("fish","larvae"))

intellectualRights <- as(set_TextType("5_SBCMBONintellectualRights.docx"), "intellectualRights")


methods<-set_methods("5_SBCMBONMethod.docx")

begindate <- as.character(min(df$time,na.rm=T))
enddate <- as.character(max(df$time,na.rm=T))
wes<-min(df$longitude,na.rm=T)
eas<-max(df$longitude,na.rm=T)
nor<-max(df$latitude,na.rm=T)
sou<-min(df$latitude,na.rm=T)
geographicDescription <- "CalCOFI cruise lines along California coastal region"
coverage <- set_coverage(begin = begindate, end = enddate,
                         geographicDescription = geographicDescription,
                         west = wes, east = eas,
                         north = nor, south = sou,
                         altitudeMin = -500, altitudeMaximum = 0,
                         altitudeUnits = "meter")
#-------------------------------------------------------------------
#combine parameters

#put the dataset together 
dataset <- new("dataset",
               title = title,
               creator = creator,
               pubDate = pubDate,
               intellectualRights = intellectualRights,
               abstract = abstract,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               dataTable = dataTable
              )

#wrap it all into the eml tag and add additional metadata
eml <- new("eml",
           packageId = "CalCOFI20170413",
           system = "knb",
           schemaLocation="eml://ecoinformatics.org/eml-2.1.1 http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
           access=access,
           dataset = dataset,
           additionalMetadata = as(unitsList, "additionalMetadata"))
#eml_validate(eml)

write_eml(eml, "5_larval_XML.xml")
