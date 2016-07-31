



library(readr)
library(plyr)
library(dplyr)
library(stringr)

pop <- read_csv('csv/vic_popn_by_suburb.csv') %>%
    mutate(SSC_CODE = as.integer(str_sub(region_id, 4))) %>%
    filter(SSC_CODE >= 20000, SSC_CODE < 30000) %>%
    dplyr::select(SSC_CODE, Population = Tot_P_P, Name, AreaSQKM)


roads <- read_csv('csv/road_length_by_suburb.csv') %>%
    filter(SSC_CODE >= 20000, SSC_CODE < 30000) %>%
    dplyr::select(SSC_CODE, roads = Sum_LENGTH) %>%
    mutate(roads.p = roads / max(.$roads, na.rm = TRUE))

rail <- read_csv('csv/rail_by_suburb.csv') %>%
    filter(SSC_CODE >= 20000, SSC_CODE < 30000) %>%
    dplyr::select(SSC_CODE, rail = Sum_LENGTH) %>%
    mutate(rail.p = rail / max(.$rail, na.rm = TRUE))

built <- read_csv('csv/builtup_area_by_suburb.csv') %>%
    filter(SSC_CODE >= 20000, SSC_CODE < 30000) %>%
    dplyr::select(SSC_CODE, built = Sum_AREA) %>%
    mutate(built.p = built / max(.$built, na.rm = TRUE))

parks <- read_csv('csv/public_land_by_suburb.csv') %>%
    filter(SSC_CODE >= 20000, SSC_CODE < 30000) %>%
    dplyr::select(SSC_CODE, parks = Sum_AREA) 

forest <- read_csv('csv/forest_by_suburb.csv') %>%
    filter(SSC_CODE >= 20000, SSC_CODE < 30000) %>%
    dplyr::select(SSC_CODE, forest = Sum_AREA) 

# food <- read_csv('csv/suburbs_zomato.csv') %>%
#     filter(SSC_CODE >= 20000, SSC_CODE < 30000) %>%
#     dplyr::select(SSC_CODE, food = ) 

sport <- read_csv('csv/sport_by_suburb.csv') %>%
    filter(SSC_CODE >= 20000, SSC_CODE < 30000) %>%
    dplyr::select(SSC_CODE, sport = Sport_Facilities) %>%
    mutate(sport.p = sport / max(.$sport, na.rm = TRUE))

community <- read_csv('csv/community_by_suburb.csv') %>%
    filter(SSC_CODE >= 20000, SSC_CODE < 30000) %>%
    mutate(community = `Self_Reporting_Health_as_Excellent_or_Very Good`
 + Feeling_Part_of_Community + Unemployment + Participation_in_Citizen_Engagement_Activities_Last_12_Months + Active_Community_Where_People_Get_Involved_In_Local_Issues
) %>%
    dplyr::select(SSC_CODE, community) %>%
    mutate(community.p = community / max(.$community, na.rm = TRUE))
    
    
df <- join_all(list(pop,
                    roads,
                    rail,
                    built,
                    parks,
                    forest,
                    community,
                    sport),
               by = 'SSC_CODE', match = 'first') %>%
    rowwise() %>%
    mutate(reserve = sum(parks, forest, na.rm=TRUE))
    
df[is.na(df)] <- 0
    
subs <- df %>%
    mutate(reserve.p = reserve / max(.$reserve, na.rm = TRUE))


## calculate env measure
calc_slider <- function(v, left, right, calc, df) {
    df[calc] <- ifelse(v == 0, df[left] + df[right],
                       ifelse(v < 0,
                              (df[left] * abs(v)) + (df[right] * (1 + v)),
                              (df[right] * v) + (df[left] * (1 - v))
                       )
    )
    return(df)
}              



                          