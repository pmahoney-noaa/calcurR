pkgs <- c("DBI", "RSQLite")
sapply(pkgs, require, character = T)

dbFile <- "E:/2023_PNW_data/ImageDatabase/digikam4.db"
mydb <- dbConnect(RSQLite::SQLite(), dbFile)

# List Tables
dbListTables(mydb)

# Stored in memory
head(dbGetQuery(mydb, 'SELECT * FROM Images'))
head(dbGetQuery(mydb, 'SELECT * FROM ImageTags'))
head(dbGetQuery(mydb, 'SELECT * FROM Tags'))

# For Large requstes
res <- dbSendQuery(mydb, "SELECT * FROM Albums")
dbFetch(res)
dbClearResult(res)



# Possible queries
query <-
"
SELECT
  Albums.relativePath,
  Images.name AS imageName, Images.modificationDate, Images.fileSize,
  ImageInformation.creationDate, ImageInformation.width, ImageInformation.height, ImageInformation.format,
  ImagePositions.latitudeNumber AS latitude, ImagePositions.longitudeNumber AS longitude, ImagePositions.altitude, ImagePositions.orientation, ImagePositions.tilt, ImagePositions.roll, ImagePositions.accuracy,
  duration, frameRate,
  modTags.tags
FROM
  Albums
INNER JOIN Images ON Albums.id = Images.album
LEFT JOIN ImageInformation ON Images.id = ImageInformation.imageid
LEFT JOIN ImagePositions ON Images.id = ImagePositions.imageid
LEFT JOIN VideoMetadata ON Images.id = VideoMetadata.imageid
LEFT JOIN (
    SELECT
      ImageTags.imageid,
      group_concat(Tags.name) AS tags
    FROM
      ImageTags
    INNER JOIN Tags ON ImageTags.tagid = Tags.id
    WHERE ImageTags.tagid > 25
    GROUP BY imageid
    ) AS modTags ON Images.id = modTags.imageid
"

test <- dbGetQuery(mydb, query)

query <-
"
SELECT
  ImageTags.imageid,
  group_concat(Tags.name) AS tags
FROM
  ImageTags
INNER JOIN Tags ON ImageTags.tagid = Tags.id
WHERE ImageTags.tagid > 25
GROUP BY imageid
"

dbGetQuery(mydb, query)




dbDisconnect(mydb)
