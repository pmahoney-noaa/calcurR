# Load Required Packages
pkgs <- c('tidyverse', 'lubridate', 'mapboxer')
sapply(pkgs, require, character = T)

# Load functions
build_flukebook_submission <- function (batch, df, imgdir, outdir, submitterID) {
  ids <- batch %>% pull(ID)
  dfo <- furrr::future_map_dfr(ids, function (i) {
    #print(i)
    bfi <- batch %>% filter(ID == i)
    dfi <- df %>% filter(CRC_ID == i)

    if (nrow(dfi) > 1) stop(paste("Too many photo records for ID:", i))

    if (nrow(dfi) == 1) {
      dfoi <- dfi %>%
        mutate(
          MarkedIndividual.individualID = i,
          Encounter.genus = "Eschrichtius",
          Encounter.specificEpithet = "robustus",
          Encounter.locationID = "Washington",
          Encounter.submitterID = submitterID,
          Encounter.decimalLatitude = bfi$lastLatitude,
          Encounter.decimalLongitude = bfi$lastLongitude,
          Encounter.year = year(bfi$lastDate),
          Encounter.month = month(bfi$lastDate),
          Encounter.day = day(bfi$lastDate),
          Encounter.researcherComments = paste0(i, ";", Information)
        ) %>%
        dplyr::select(
          Encounter.locationID,
          Encounter.decimalLatitude,
          Encounter.decimalLongitude,
          Encounter.year,
          Encounter.month,
          Encounter.day,
          Encounter.genus,
          Encounter.specificEpithet,
          Encounter.submitterID,
          Encounter.researcherComments
        )

      # When moving images, move in ALL CAPS
      if (dfi$Left_Image_Name == "") {
        fi <- data.frame(
          Encounter.mediaAsset0 = paste0(toupper(dfi$Right_Image_Name), ".JPG"),
          Encounter.mediaAsset1 = NA,
          location.mediaAsset0 = paste0(dfi$Right_Image_Name, ".JPG"),
          location.mediaAsset1 = NA
        )

        # Remove hyphens and underscores
        fi$Encounter.mediaAsset0 <- gsub("[-/&'()_ ]+", "", fi$Encounter.mediaAsset0)

      } else if (dfi$Right_Image_Name == "") {
        fi <- data.frame(
          Encounter.mediaAsset0 = paste0(toupper(dfi$Left_Image_Name), ".JPG"),
          Encounter.mediaAsset1 = NA,
          location.mediaAsset0 = paste0(dfi$Left_Image_Name, ".JPG"),
          location.mediaAsset1 = NA
        )

        # Remove hyphens and underscores
        fi$Encounter.mediaAsset0 <- gsub("[-/&'()_ ]+", "", fi$Encounter.mediaAsset0)

      } else {
        fi <- data.frame(
          Encounter.mediaAsset0 = paste0(toupper(dfi$Left_Image_Name), ".JPG"),
          Encounter.mediaAsset1 = paste0(toupper(dfi$Right_Image_Name), ".JPG"),
          location.mediaAsset0 = paste0(dfi$Left_Image_Name, ".JPG"),
          location.mediaAsset1 = paste0(dfi$Right_Image_Name, ".JPG")
        )

        # Remove hyphens and underscores
        fi$Encounter.mediaAsset0 <- gsub("[-/&'()_ ]+", "", fi$Encounter.mediaAsset0)
        fi$Encounter.mediaAsset1 <- gsub("[-/&'()_ ]+", "", fi$Encounter.mediaAsset1)
      }

      dfoi <- dfoi %>% add_column(fi, .before = 1)

      # if (ncol(fi) == 4) {
      #   dfo <- dfo %>%
      #     add_column(fi[1:2], .before = 1)
      # } else {
      #   dfo <- dfo %>%
      #     add_column(fi[1], .before = 1)
      # }

    } else {
      warning(paste("Record does not exist for ID:", i))
      dfoi <- NULL
    }

    return(dfoi)
  })

  ## copy images
  dir.create(paste0(outdir, "images/"), recursive = T)
  furrr::future_map(1:nrow(dfo), function (x) {
    if (!is.na(dfo$Encounter.mediaAsset0[x])) {
      file.copy(from = paste0(imgdir, dfo$location.mediaAsset0[x]),
                to = paste0(outdir, "images/", dfo$Encounter.mediaAsset0[x]),
                overwrite = T, copy.mode = T, copy.date = T)
    }

    if (!is.na(dfo$Encounter.mediaAsset1[x])) {
      file.copy(from = paste0(imgdir, dfo$location.mediaAsset1[x]),
                to = paste0(outdir, "images/", dfo$Encounter.mediaAsset1[x]),
                overwrite = T, copy.mode = T, copy.date = T)
    }
  })

  # Output data file
  write.csv(dfo %>% dplyr::select(-location.mediaAsset0, -location.mediaAsset1),
            paste0(outdir, "Batch_submission_file.csv"))

  openxlsx::write.xlsx(dfo %>% dplyr::select(-location.mediaAsset0, -location.mediaAsset1),
                       paste0(outdir, "Batch_submission_file.xlsx"))
}


# Load CSV version of CRC output
ER <- read.csv('./Data/ER-ID-forMML-through2023.csv') %>%
  dplyr::select(ID, ResearchGroup = Record.Source, Year, Date,
                Sreg = Sreg2, Reg = Reg3., Lat = Dec.Lat, Lon = Dec.Long) %>%
  mutate(Date = dmy(Date),
         Month = month(Date))

# Filter out a few 2021 data
#filter(Year <= 2020)


# Filter out missing months and specific regions
ERx <- ER %>%
  filter(!is.na(Month) & Reg %in% c(2:7, 10:12))


# Approximate boundaries
# 49.57 # Nootka
# 46.27 # Columbia River
# -124.7173
#
#
# 48.496 # Northern boundary MUA
# 48.039 # Southern boundary MUA
# -124.6386

#currentYear <- year(lubridate::now())
currentYear <- max(year(ER$Date), na.rm = T)
ERx <- ERx %>%
  filter(!is.na(Lat)) %>%
  mutate(
    obsMUA = ifelse(Lat >= 48.039 & Lat <= 48.496 & Lon <= -124.6386, 1, 0),
    #obsWA_SVI = ifelse(Reg %in% c(5:7, 10), 1, 0),
    obsWA_SVI = ifelse(Lat >= 46.27 & Lat <= 49.57 & Lon <= -123.9, 1, 0),
    juneNov = ifelse(Month >= 6 & Month <= 11, 1, 0),
    julyOct = ifelse(Month >= 7 & Month <= 10, 1, 0),
    enctAgeYrs = currentYear - Year
  )
write.csv(ERx %>%
            filter(Reg %in% c(5:7,10)) %>%
            filter(obsMUA == 1 | obsWA_SVI == 1),
          "./Data/crcIDs_encounters_forMatching_WNP_2023.csv", row.names = F)

sER <- ERx %>%
  group_by(ID) %>%
  mutate(
    juneNov_year = paste(juneNov, Year, sep = "_"),
    julyOct_year = paste(juneNov, Year, sep = "_"),
    MUA_juneNov_year = paste(obsMUA, juneNov_year, sep = "_"),
    WA_SVI_juneNov_year = paste(obsWA_SVI, juneNov_year, sep = "_"),
  ) %>%
  summarize(
    obsMUA = ifelse(any(obsMUA == 1, na.rm = T), 1, 0),
    nObsMUA = sum(obsMUA, na.rm = T),
    nYrsObsMUA_juneNov = sum(grepl("1_1_", unique(MUA_juneNov_year)), na.rm = T),
    nYrsObsMUA_decMay = sum(grepl("1_0_", unique(MUA_juneNov_year)), na.rm = T),

    obsWA_SVI = ifelse(any(obsWA_SVI == 1, na.rm = T), 1, 0),
    nObsWA_SVI = sum(obsWA_SVI, na.rm = T),
    nYrsObsWA_SVI_juneNov = sum(grepl("1_1_", unique(WA_SVI_juneNov_year)), na.rm = T),
    nYrsObsWA_SVI_decMay = sum(grepl("1_0_", unique(WA_SVI_juneNov_year)), na.rm = T),

    pcfg = ifelse(sum(grepl("1_", unique(juneNov_year))) > 1, 1, 0),
    firstDate = min(Date),
    lastDate = max(Date),
    lastLongitude = Lon[n()],
    lastLatitude = Lat[n()],
    numberOfEncts = n(),
    numberOfEnctYrs = length(unique(Year)),
    lastEnctAgeYrs = min(enctAgeYrs),
    monthsOfYearObs = paste(unique(Month)[order(unique(Month))], collapse = ","),
    ResearchGroup = paste(unique(ResearchGroup), collapse = ","),
    Region = paste(unique(Reg), collapse = ",")
  ) %>%
  arrange(-obsMUA, -nYrsObsMUA_juneNov, lastEnctAgeYrs) %>%
  filter(obsMUA == 1 | obsWA_SVI == 1)

write.csv(sER, "./Data/crcIDs_summary_forMatching_WNP_2023.csv", row.names = F)

#
## First batch of individuals
#
batch1 <- sER %>%
  filter(
    obsMUA == 1 &
      nYrsObsMUA_juneNov > 0 &
      lastEnctAgeYrs <=5 &
      pcfg == 1
  )
write.csv(batch1, "./Data/crcIDs_batch1_forMatching_WNP_since2018.csv", row.names = F)

# Plot locations
ERx %>%
  #filter(obsMUA == 1 | obsWA_SVI == 1) %>%
  filter(obsMUA == 1) %>%
  #filter(obsWA_SVI == 1) %>%
  mutate(
    color = factor(juneNov, labels = viridis::viridis(length(unique(juneNov)), option = "G")),
    color = gsub("FF", "", color)
  ) %>%
  as_mapbox_source(lng = "Lon", lat = "Lat") %>%
  mapboxer(
    center = c(-124.8, 48.2),
    zoom = 8
  ) %>%
  add_navigation_control() %>%
  add_circle_layer(
    circle_color = c("get", "color"),
    circle_radius = 3,
    popup = "ID: {{ID}} <br> {{ResearchGroup}} <br> {{Date}}"
  )


# CRC IDs with known western matches
mIDs <- c('1045', '809', '817', '825', '1040', '1042', '1326', '1405', '1694', '1564', '1626', '1488')



## Pulling Catalog for submission
batch1 <- batch1 %>% arrange(ID)
df <- read.csv("D:/PCFG Catalog/2023_Datasheets/PCFG_WebsiteCatalog_Datasheet_2023.csv")
imgdir <- "D:/PCFG Catalog/"
outdir <- "D:/FlukebookSubmissions/batch1/"
dir.create(outdir, recursive = T)

build_flukebook_submission(batch1, df, imgdir, outdir, submitterID = "pmahoney")






#
## Second batch of individuals
#
batch <- sER %>%
  filter(
    obsMUA == 1 &
      nYrsObsMUA_juneNov > 0 &
      lastEnctAgeYrs <=5 &
      pcfg == 0
  )
write.csv(batch, "./Data/crcIDs_batch2_forMatching_WNP_since2018.csv", row.names = F)

# CRC IDs with known western matches
mIDs <- c('1045', '809', '817', '825', '1040', '1042', '1326', '1405', '1694', '1564', '1626', '1488')



## Pulling Catalog for submission
batch <- batch %>% arrange(ID)
df <- read.csv("D:/PCFG Catalog/2023_Datasheets/PCFG_WebsiteCatalog_Datasheet_2023.csv")
imgdir <- "D:/PCFG Catalog/"
outdir <- "D:/FlukebookSubmissions/batch2/"
dir.create(outdir, recursive = T)

build_flukebook_submission(batch, df, imgdir, outdir, submitterID = "pmahoney")
