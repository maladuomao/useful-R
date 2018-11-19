### Useful R codes


# get data
setwd("Y:/Custom_Pricing_R/ECL_Pricing_Model_R")

source("Functions/Useful.R")
csv_name = "NY_Renewals_Dec2018"
rnw = fread(paste0("../Data/Renewals/", csv_name, ".csv"), integer64 = "character")


# select columns
rnw[, grep("Date", colnames(rnw)) := lapply(.SD, mdy), .SD = grep("Date", colnames(rnw), value = TRUE)]


# delete columns
cust_list[, c("Fix1", "Fix1_Term", "Fix2", "Fix2_Term") := NULL] # remove those temp price columns

# change data type
rnw[, Usage := as.numeric(Usage)]

# condition calculation
rnw[Commodity == "ELECTRIC" & ProductType == "Fixed", Price := Price * 100] # calculations all assume c/kWh
rnw[is.na(AnnualUsage) | Ct < 12, AnnualUsage := GBASS_Vol]


#  function
rnw = ref.assign_msi(rnw, "MarketParticipantIdentifier"
                     , inc_zone = (state %in% c("NY", "MA")) # join by zone if MA or NY
                     , z_colname = "Zone")                   # ref.msi_mapping need to be updated if new MarketStructureIdentifier is added

source("Functions/Calendarize.R")

# seperate NA/ non NA
rnw_nousage = rnw[is.na(Usage)]
rnw = rnw[!is.na(Usage)]
rnw = rbind(rnw, rnw_nousage)

# remove any thing with cal
rm(list=grep("^cal.", ls(), value = TRUE))


rnw[, ID := ContractOID] # set unique 'ID' column - needed for calendarization function
setkey(rnw, ID)

# not sure yet
rnw[,`:=` (ICAPEffectiveDate = NULL, NITSEffectiveDate = NULL)]

# set date
asofdate = Sys.Date()             # pull latest curves before this date
startdate = as.Date("2019-01-01") # cost start date


term = sort(unique(c(12, 24, 36, 48, 60, rnw[ProductType != "Variable", Term])))  # calculate costs for these terms



curve_list = c("CapacityRate", "CapacityScaleFactor") 


if (no_custom_costs == FALSE) 
  { curve_data = rq.pull_curves(odbc_archimedes, grep("PWR", msi_list, value = TRUE), curve_list, asofdate)}


save.image(paste0("../Output/Renewals/RData/", csv_name, "_Costs_Calculated.RData"))

Flat1.MP = ceiling(mean(Flat1_Margin_Pr, na.rm = TRUE))


## function
pr.fix = function(cust_list, prod_term, prod_margin, rate_floor, output_name) {
  
  perks_col = paste0(output_name, "_Perks")
  
  cust_list[, (term_col) := prod_term] # term

  ## assign prices
  cust_list[, (output_name) := {
    temp_fix = (get(paste0("TotalCTS_", prod_term)) * Ref_Cost_Mult # cost converted
                + prod_margin/Ref_Units_RCE * ifelse(Unit=="c/kWh", 100, 1) # margin
                + get(perks_col)/(AnnualUsage * prod_term/12) * ifelse(Unit=="c/kWh", 100, 1) # perks
    )/POR_GRT_Factor
    ; temp_fix_2 = round(temp_fix, 3) 
    ; pmax(temp_fix_2, Utility_Rate * (1 - rate_floor)) # make sure price doesn't fall below min
  }]
  
  # round prices to already set-up rate codes
  cust_list[Commodity == "ELECTRIC"
            , (output_name) := round(get(output_name) * 10, 0)/10 - 0.01]
  
  return(cust_list)
}















