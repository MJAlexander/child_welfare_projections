## Dataframe of state abbreviation, name, fips, and division mapping ----
## Early years of MCOD files use NCHS-specific state codes.
nchs_state_codes <-
    list(
        "AL" = "01",
        "AK" = "02",
        "AZ" = "03",
        "AR" = "04",
        "CA" = "05",
        "CO" = "06",
        "CT" = "07",
        "DE" = "08",
        "DC" = "09",
        "FL" = "10",
        "GA" = "11",
        "HI" = "12",
        "ID" = "13",
        "IL" = "14",
        "IN" = "15",
        "IA" = "16",
        "KS" = "17",
        "KY" = "18",
        "LA" = "19",
        "ME" = "20",
        "MD" = "21",
        "MA" = "22",
        "MI" = "23",
        "MN" = "24",
        "MS" = "25",
        "MO" = "26",
        "MT" = "27",
        "NE" = "28",
        "NV" = "29",
        "NH" = "30",
        "NJ" = "31",
        "NM" = "32",
        "NY" = "33",
        "NC" = "34",
        "ND" = "35",
        "OH" = "36",
        "OK" = "37",
        "OR" = "38",
        "PA" = "39",
        "RI" = "40",
        "SC" = "41",
        "SD" = "42",
        "TN" = "43",
        "TX" = "44",
        "UT" = "45",
        "VT" = "46",
        "VA" = "47",
        "WA" = "48",
        "WV" = "49",
        "WI" = "50",
        "WY" = "51"
    )

st_info <- tibble(
    abbrev   = state.abb,
    division = as.character(state.division),
    st_lat   = state.center$y,
    st_lon   = state.center$x
) %>%
    ## We have to add DC because it's not a state
    add_row(
        abbrev = "DC",
        division = "South Atlantic",
        st_lat = 38.9072,
        st_lon = -77.0369
    ) %>%
    left_join(narcan::st_fips_map) %>%
    rename(st_fips = fips) %>%
    arrange(name) %>%
    left_join(tibble(
        abbrev = names(nchs_state_codes),
        nchs_fips = unlist(nchs_state_codes)
    ),
    by = "abbrev")

rm(nchs_state_codes)

categorize_division <- function(df) {
    df %>%
        mutate(division_cat = factor(
            division,
            levels = c(
                "US",
                "Pacific",
                "Mountain",
                "West North Central",
                "West South Central",
                "East North Central",
                "East South Central",
                "Middle Atlantic",
                "South Atlantic",
                "New England"
            ),
            labels = c(
                "Whole US",
                "Pacific",
                "Mountain",
                "WN Central",
                "WS Central",
                "EN Central",
                "ES Central",
                "Mid-Atlantic",
                "S Atlantic",
                "New England"
            ),
            ordered = TRUE
        ))
}

categorize_race <- function(df) {
    df %>%
        mutate(race = race_cat) %>%
        mutate(race_cat = factor(
            race,
            levels = c("total", "white", "black"),
            labels = c("Total", "White", "Black"),
            ordered = TRUE
        ))
}

categorize_decomp_type <- function(df) {
    df %>% 
        mutate(type_cat = factor(type, 
                                 levels = c("total", "non_opioid", "opioid"), 
                                 labels = c("Total", "Non-opioid", "Opioid"), 
                                 ordered = TRUE))
}


## Other helpers ----
return_st_info <- function() {
    st_info <- tibble(
        abbrev   = state.abb,
        division = as.character(state.division),
        st_lat   = state.center$y,
        st_lon   = state.center$x
    ) %>%
        ## We have to add DC because it's not a state
        add_row(
            abbrev = "DC",
            division = "South Atlantic",
            st_lat = 38.9072,
            st_lon = -77.0369
        ) %>%
        left_join(narcan::st_fips_map) %>%
        ## Add in whole US and NA
        add_row(
            abbrev = "US",
            name = "zzWhole US",
            division = "Whole US",
            st_lat = 0,
            st_lon = 200
        ) %>%
        add_row(
            abbrev = NA,
            name = "zzzUnknown State",
            division = "Unknown",
            st_lat = 0,
            st_lon = 199
        ) %>%
        rename(st_fips = fips) %>%
        arrange(st_lon) %>%
        mutate(lon_rank = dense_rank(st_lon),
               alpha_rank = dense_rank(name)) %>%
        mutate(name = gsub("zz|zzz", "", name))
    
    st_info %>%
        mutate(
            st_cat = factor(
                abbrev,
                levels = st_info %>%
                    arrange(lon_rank) %>%
                    pull(abbrev),
                ordered = TRUE
            ),
            name_cat = factor(
                name,
                levels = st_info %>%
                    arrange(lon_rank) %>%
                    pull(name),
                ordered = TRUE
            ),
            name_cat_alpha = factor(
                name,
                levels = st_info %>%
                    arrange(alpha_rank) %>%
                    pull(name),
                ordered = TRUE
            )
        )
}
