
library(dplyr)
library(fixest)
options(scipen = 999)

library(censusapi)
years <- 2010:2019

demos <- data.frame()
ind_data <- data.frame()
job_demo_all <- data.frame()

# industry_vars <- c(
#   "B24050_001E",  # total employed
#   paste0("B24050_", sprintf("%03d", 3:43), "E"),  # Male
#   paste0("B24050_", sprintf("%03d", 44:84), "E"), # Female
#   "NAME"
# )

for (y in years) {
demos_l <- getCensus(
  name = "acs/acs1",
  vintage = y,
  vars = c("B19013_001E",
           "B15003_001E",
           "B15003_022E",
           "B15003_023E",
           "B15003_024E",
           "B15003_025E",
           "B01003_001E",
           "B02001_002E",  # White alone
           "B02001_003E",  # Black or African American alone
           "B02001_004E",  # American Indian and Alaska Native alone
           "B02001_005E",  # Asian alone
           "B02001_006E",  # Native Hawaiian and Other Pacific Islander alone
           "B02001_007E",  # Some other race alone
           "B02001_008E",  # Two or more races
           "B25064_001E"   # Median gross rent
  ),
  region = "metropolitan statistical area/micropolitan statistical area:*"

)


ind_data_l <- getCensus(
  name = "acs/acs1",
  vintage = y,
  vars = industry_codes <- c(
    # Agriculture, forestry, fishing and hunting, and mining
    "B24050_003E", "B24050_044E",
    # Construction
    "B24050_004E", "B24050_045E",
    # Manufacturing
    "B24050_005E", "B24050_046E",
    # Wholesale trade
    "B24050_006E", "B24050_047E",
    # Retail trade
    "B24050_007E", "B24050_048E",
    # Transportation and warehousing, and utilities
    "B24050_008E", "B24050_049E",
    # Information
    "B24050_009E", "B24050_050E",
    # Finance and insurance, and real estate and rental and leasing
    "B24050_010E", "B24050_051E",
    # Professional, scientific, and management, and administrative and waste management services
    "B24050_011E", "B24050_052E",
    # Educational services, and health care and social assistance
    "B24050_012E", "B24050_053E",
    # Arts, entertainment, and recreation, and accommodation and food services
    "B24050_013E", "B24050_054E",
    # Other services, except public administration
    "B24050_014E", "B24050_055E",
    # Public administration
    "B24050_015E", "B24050_056E"
  ),
  region = "metropolitan statistical area/micropolitan statistical area:*",
) 

job_demo_all_l <- getCensus(
  name = "acs/acs1",
  vintage = y,
  vars = "B24050_001E",
  region = "metropolitan statistical area/micropolitan statistical area:*",
)

job_demo_all_l$YEAR<-y
job_demo_all <- rbind(job_demo_all, job_demo_all_l)


demos_l$YEAR <- y

demos <- rbind(demos, demos_l)
ind_data_l$YEAR <- y

ind_data <- rbind(ind_data, ind_data_l)



}





job_data <- ind_data %>%
  mutate(
    agriculture = as.numeric(B24050_003E) + as.numeric(B24050_044E),
    construction = as.numeric(B24050_004E) + as.numeric(B24050_045E),
    manufacturing = as.numeric(B24050_005E) + as.numeric(B24050_046E),
    wholesale = as.numeric(B24050_006E) + as.numeric(B24050_047E),
    retail = as.numeric(B24050_007E) + as.numeric(B24050_048E),
    transport_util = as.numeric(B24050_008E) + as.numeric(B24050_049E),
    information = as.numeric(B24050_009E) + as.numeric(B24050_050E),
    finance_realestate = as.numeric(B24050_010E) + as.numeric(B24050_051E),
    professional = as.numeric(B24050_011E) + as.numeric(B24050_052E),
    education_health = as.numeric(B24050_012E) + as.numeric(B24050_053E),
    arts_accommodation = as.numeric(B24050_013E) + as.numeric(B24050_054E),
    other_services = as.numeric(B24050_014E) + as.numeric(B24050_055E),
    public_admin = as.numeric(B24050_015E) + as.numeric(B24050_056E)
  ) %>%
select(-starts_with("B24050_"))
colnames(job_data)[1] <- "AREA"
job_data <- job_data %>%
  select(AREA, YEAR, starts_with("agriculture"):public_admin) %>%
  pivot_longer(
    cols = -c(AREA, YEAR),
    names_to = "industry",
    values_to = "employment"
  )

job_data <- na.omit(job_data)
job_data <- job_data %>%
  mutate(industry_pool = case_when(
    industry %in% c("retail", "arts_accommodation", "other_services") ~ "consumer_services",
    industry %in% c("finance_realestate", "information", "professional")~ "business_services",
    industry %in% c("education_health", "public_admin") ~ "public_nonprofit",
    industry %in% c("construction", "manufacturing", "agriculture") ~ "goods_producing",
    industry %in% c("wholesale", "transport_util") ~ "logistics_util",
    TRUE ~ industry  
  ))

job_data <- aggregate(employment ~ industry_pool + AREA + YEAR, data = job_data, FUN = sum)




colnames(demos) <- c("AREA",
                     "inc",
                     "over25",
                     "bach",
                     "master",
                     "professional",
                     "doc",
                     "tot_pop",
                     "white",
                     "black",
                     "amerindian",
                     "asian",
                     "hawaiian",
                     "other",
                     "twomore",
                     "med_gross_rent",
                     "YEAR" )
demos$AREAYEAR <- paste0(demos$AREA, demos$YEAR)

demos <- demos %>%
  rowwise() %>%
  mutate(college = sum(c_across(c(bach,master,professional,doc))),
         college_pct = college/over25,
         over25_pct = over25/tot_pop,
         white_pct = white/tot_pop,
         black_pct = black/tot_pop,
         amerindian_pct = amerindian/tot_pop,
         asian_pct = asian/tot_pop,
         hawaiian_pct = hawaiian/tot_pop,
         other_pct = other/tot_pop,
         twomore_pct = twomore/tot_pop

  )

demos <- demos[, c("AREAYEAR",
                   "inc",
                   "tot_pop",
                   "college_pct",
                   "over25_pct",
                   "white_pct",
                   "black_pct",
                   "amerindian_pct",
                   "asian_pct",
                   "hawaiian_pct",
                   "other_pct",
                   "twomore_pct",
                   "med_gross_rent")]


groups   <- unique(job_data$industry_pool)

job_data$occ_code <- match(job_data$industry_pool, groups)
job_data$occ_code <- job_data$occ_code + 10
job_data$AREAYEAR <- paste0(job_data$AREA, job_data$YEAR)
job_data$ayo <- paste0(job_data$AREAYEAR, job_data$occ_code)


job_demo <- merge(job_data, demos, by = "AREAYEAR")




colnames(job_demo_all)[1] <- "AREA"

job_demo_all <- job_demo_all %>%
  rename(tot_emp = B24050_001E)
job_demo_all$AREAYEAR <- paste0(job_demo_all$AREA,job_demo_all$YEAR)
job_demo_all <- aggregate(tot_emp ~ AREAYEAR, data = job_demo_all, FUN = sum)
job_demo <- merge(job_demo, job_demo_all, by = "AREAYEAR")

##INDUSTRY SHARES IN t=0 (2016)
industry_shares <- job_demo[job_demo$YEAR == 2010,]
industry_shares$nat_share <- industry_shares$employment/industry_shares$tot_emp
industry_shares <- industry_shares[,c("AREA", "industry_pool", "nat_share")]

natl_shocks <- aggregate(employment ~ occ_code + YEAR , data = job_demo, FUN = sum)

library(plm)
natl_shocks <- pdata.frame(natl_shocks, index = c("occ_code", "YEAR"))
natl_shocks <- natl_shocks %>% arrange(occ_code, YEAR)
natl_shocks$nat_growth <- (natl_shocks$employment - plm::lag(natl_shocks$employment, 1)) / plm::lag(natl_shocks$employment, 1) 




nat_emp <- natl_shocks[, c("occ_code", "YEAR", "nat_growth")]
job_demo <- merge(job_demo, nat_emp, by=c("occ_code", "YEAR"))
ay_growth <- pdata.frame(aggregate(employment ~ AREA + YEAR, data = job_demo, FUN = sum), index = c("AREA","YEAR"))
ay_growth$true_growth <- (ay_growth$employment - plm::lag(ay_growth$employment, 1)) / plm::lag(ay_growth$employment,1)
ay_growth$AREAYEAR <- paste0(ay_growth$AREA,ay_growth$YEAR)
ay_growth <- ay_growth[,c("AREAYEAR", "true_growth")]
job_demo<- merge(job_demo, industry_shares, by=c("AREA", "industry_pool"))
job_demo$loc_share <- job_demo$employment/job_demo$tot_emp
job_demo$natl_shock_wt <- job_demo$nat_growth*job_demo$nat_share
job_demo <- merge(job_demo, ay_growth, by = "AREAYEAR")


housing_shocks <- data.frame("AREA" = job_demo$AREA, "YEAR" = job_demo$YEAR, "AREAYEAR" = job_demo$AREAYEAR, "med_gross_rent" = job_demo$med_gross_rent)
housing_shocks <- pdata.frame(housing_shocks, index = c("AREA", "YEAR"))
housing_shocks <- housing_shocks[!duplicated(housing_shocks$AREAYEAR),]
housing_shocks$hi_pctchange <- (housing_shocks$med_gross_rent- plm::lag(housing_shocks$med_gross_rent,1))/plm::lag(housing_shocks$med_gross_rent,1) 

housing_shocks <- data.frame("AREAYEAR" = housing_shocks$AREAYEAR, "hi_pctchange" = housing_shocks$hi_pctchange)
job_demo <- merge(job_demo, housing_shocks, by = "AREAYEAR")

job_demo <- na.omit(job_demo)

job_demo$industry_pool_ <- as.factor(job_demo$industry_pool)

table(table(job_demo$AREA)) 
length(unique(job_demo$AREA))
t <- table(job_demo$AREA)
lim <- job_demo$AREA %in% names(t)[t == 45]
job_demo <- job_demo[lim,]



iv_model <- feols(asinh(med_gross_rent) ~ i(industry_pool, ref = "public_nonprofit") + 
                    tot_pop + 
                    inc 
                    # college_pct + 
                    # over25_pct + 
                    # white_pct + 
                    # black_pct + 
                    # amerindian_pct + 
                    # asian_pct + 
                    # hawaiian_pct + 
                  | AREA + YEAR | 
                    i(industry_pool,true_growth) ~ 
                    i(industry_pool,natl_shock_wt), 
                  data = job_demo)

 summary(iv_model)


 table(job_demo$industry_pool[!duplicated(paste0(job_demo$industry_pool, "_", job_demo$YEAR, "_", job_demo$AREA))])

 
 
 