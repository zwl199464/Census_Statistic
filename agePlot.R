
#' agePlot Plot the age by estimate plot
#'
#' @param county_name the county name in Florida, choice are "Alachua",
#'    "Baker",        "Bay",          "Bradford",
#'    "Brevard",      "Broward",      "Calhoun",
#'    "Charlotte",   "Citrus",       "Clay",
#'    "Collier",      "Columbia",    "DeSoto",
#'    "Dixie",        "Duval",        "Escambia",
#'    "Flagler",      "Franklin",     "Gadsden",
#'    "Gilchrist",   "Glades",       "Gulf",
#'    "Hamilton",     "Hardee",      "Hendry",
#'    "Hernando",     "Highlands",    "Hillsborough",
#'    "Holmes",       "Indian River", "Jackson",
#'    "Jefferson",   "Lafayette",    "Lake",
#'    "Lee",          "Leon",        "Levy",
#'    "Liberty",      "Madison",      "Manatee",
#'    "Marion",       "Martin",       "Miami-Dade",
#'    "Monroe",      "Nassau",       "Okaloosa",
#'    "Okeechobee",   "Orange",      "Osceola",
#'    "Palm Beach",   "Pasco",        "Pinellas",
#'    "Polk",         "Putnam",       "Santa Rosa",
#'    "Sarasota",    "Seminole",     "St. Johns",
#'    "St. Lucie",    "Sumter",      "Suwannee",
#'    "Taylor",       "Union",       "Volusia",
#'    "Wakulla",      "Walton",       "Washington"
#' @param sex Gender choice are "Female" and "Male"
#' @param ms Marital status choice are  "Never married","Married",
#'  "Married, spouse absent", "Married, other", "Widowed", "Divorced"
#'
#' @import stringr
#' @import acs
#' @return NULL
#' @export
#'
#' @examples agePlot("Miami-Dade","Male", "Never married")
#' agePlot("Alachua",  "Female", "Divorced")
#'
agePlot <- function(county_name,sex,ms) {

  # kluge to avoid note about use of internal dataset in build
  floridaCountyMarital <- NULL
  rm(floridaCountyMarital)
  utils::data(floridaCountyMarital, envir = environment())
  # Flip code book for all counties in FL

  countyNames<-levels(as.factor(floridaCountyMarital@geography$NAME))

  codebookColon<-str_locate(countyNames, "County")[,1]

  codebook<- data.frame(cbind(str_sub(countyNames, 0, codebookColon - 2),
                              floridaCountyMarital@geography$county))

  colnames(codebook)<-c("NAME","county")

  county <- as.character(codebook$county[codebook$NAME %in% county_name])
  #Pad the county code

  paddedCounty <- str_pad(county, 3, "left", pad = "0")

  recordNumber <- which(floridaCountyMarital@geography$county == paddedCounty)
  #Subset the data by county

  CT<-floridaCountyMarital[recordNumber]

  colors<- c( "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
              "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
              "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
              "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4",
              "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD",
              "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC",
              "#CCCCCC", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3",
              "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5",
              "#FFED6F")
#Start the magical logic to get the final dataset ready for ploting
  # "Never married",
  # "Married", "Married, spouse absent", "Married, other", "Widowed", "Divorced"
  # "Female", "Male"

  title <- paste0(sex," - ",ms)

  if (str_detect(ms,fixed("Married"))) {

    if(str_detect(ms,fixed("Married, spouse absent"))) {

      marry<-"Married, spouse absent: Separated: "

      s1<-str_detect( CT@acs.colnames, fixed(paste0(": ",marry)))
      s2<-str_detect( CT@acs.colnames, fixed("years"))
      s3<-str_detect( CT@acs.colnames, fixed((paste0(": ",sex))))
      fullNumber<-s1&s2&s3
      marital <- CT@estimate[fullNumber]

      colons <- str_locate_all(CT@acs.colnames[fullNumber], ":")[[1]]

      names(marital) <- str_sub(CT@acs.colnames[fullNumber], colons[5] +2, -1)

    }else if(str_detect(ms,fixed("Married, other"))) {

      marry<-"Married, spouse absent: Other:"

      s1<-str_detect( CT@acs.colnames, fixed(paste0(": ",marry)))
      s2<-str_detect( CT@acs.colnames, fixed("years"))
      s3<-str_detect( CT@acs.colnames, fixed((paste0(": ",sex))))
      fullNumber<-s1&s2&s3
      marital <- CT@estimate[fullNumber]

      colons <- str_locate_all(CT@acs.colnames[fullNumber], ":")[[1]]

      names(marital) <- str_sub(CT@acs.colnames[fullNumber], colons[5] +2, -1)

    }else  {
      marry<-"Married, spouse present:"
      s1<-str_detect( CT@acs.colnames, fixed(paste0(": ",marry)))
      s2<-str_detect( CT@acs.colnames, fixed("years"))
      s3<-str_detect( CT@acs.colnames, fixed((paste0(": ",sex))))
      fullNumber<-s1&s2&s3
      marital <- CT@estimate[fullNumber]

      colons <- str_locate_all(CT@acs.colnames[fullNumber], ":")[[1]]

      names(marital) <- str_sub(CT@acs.colnames[fullNumber], colons[4] +2, -1)
    }

  } else  {
    marry<-ms
    s1<-str_detect( CT@acs.colnames, fixed(paste0(": ",marry)))
    s2<-str_detect( CT@acs.colnames, fixed("years"))
    s3<-str_detect( CT@acs.colnames, fixed((paste0(": ",sex))))
    fullNumber<-s1&s2&s3
    marital <- CT@estimate[fullNumber]

    colons <- str_locate_all(CT@acs.colnames[fullNumber], ":")[[1]]

    names(marital) <- str_sub(CT@acs.colnames[fullNumber], colons[3] +2, -1)

  }





#make the plot


  make_bar(marital, colors, title)

}
