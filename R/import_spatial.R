


#' @title Import Points
#' @description Imports data from a sqlite database within a buffer distance D of point X, Y. The function currently assumes coordinate system GDA94 (EPSG:4283)
#' @param db character path and name of sqlite database
#' @param tbl character name of table to import
#' @param X numeric value of x coordinate
#' @param Y numeric value of y coordinate
#' @param D numeric buffer distance; defaults to 0.05 decimal degrees
#' @export
get.points <- function(db, tbl, X, Y, D = .05) {
    require(RSQLite)
    m <- dbDriver("SQLite")
    con <- dbConnect(m, dbname = db,
                     loadable.extensions = TRUE)
    sql <- "SELECT load_extension('mod_spatialite.dll')"
    rs <- dbGetQuery(con, sql)

    sql <- sprintf("
                   SELECT *,
                   X(Geometry) as longitude, Y(Geometry) as latitude,
                   Distance(MakePoint(%1$s, %2$s, 4283), Geometry, 1) AS distance
                   FROM %3$s AS tbl
                   WHERE PtDistWithin(tbl.geometry, MakePoint(%1$s, %2$s, 4283), %4$s)
                   AND PK_UID IN (
                       SELECT rowid
                       FROM SpatialIndex
                       WHERE f_table_name = '%3$s'
                       AND search_frame = BuildCircleMbr(%1$s, %2$s, %4$s)
                   )
                   ORDER BY distance;
                   ", X, Y, tbl, D)
    df <- dbGetQuery(con, sql)

    dbDisconnect(con)

    df <- df[,-grep('Geometry', colnames(df))]

    return(df)
}

#' @title Import Polygons
#' @description Imports data from a sqlite database within a buffer distance D of point X, Y. The function currently assumes coordinate system GDA94 (EPSG:4283)
#' @param db character path and name of sqlite database
#' @param tbl character name of table to import
#' @param X numeric value of x coordinate
#' @param Y numeric value of y coordinate
#' @param D numeric buffer distance; defaults to 0.05 decimal degrees
#' @export
get.poly <- function(db, tbl, X, Y, D = .05) {
    require(RSQLite)
    require(rgeos)
    require(rgdal)

    m <- dbDriver("SQLite")
    con <- dbConnect(m, dbname = db,
                     loadable.extensions = TRUE)
    sql.sp <- "SELECT load_extension('mod_spatialite.dll')"
    rs <- dbGetQuery(con, sql.sp)

    sql <- sprintf("
                   SELECT *,
                   Distance(MakePoint(%1$s, %2$s, 4283), Geometry, 1) AS distance,
                   MbrWithin(MakePoint(%1$s, %2$s, 4283), Geometry) AS within,
                   ST_AsText(geometry) AS wkt_geometry
                   FROM %3$s AS tbl
                   WHERE PtDistWithin(tbl.geometry, MakePoint(%1$s, %2$s, 4283), %4$s)
                   AND PK_UID IN (
                       SELECT rowid
                       FROM SpatialIndex
                       WHERE f_table_name = '%3$s'
                       AND search_frame = BuildCircleMbr(%1$s, %2$s, %4$s)
                   )
                   ORDER BY distance
                   ", X, Y, tbl, D)

    df = dbGetQuery(con, sql)

    # drop Geometry blob as it causes too many problem
    df = df[,-grep('Geometry', colnames(df))]

    # check if point is within a ploygon and set distance to 0
    df$distance <- ifelse(df$within == 1, -df$distance, df$distance)

    dbDisconnect(con)

    if (nrow(df) > 0) {

        row.names(df) <- df$id <- 1:nrow(df)

        # Create spatial polygons
        # http://www.r-bloggers.com/load-postgis-geometries-in-r-without-rgdal/
        SRID <- 4283
        p4s = CRS(sprintf("+init=epsg:%s", SRID))

        if (require(foreach) & require(doParallel)) {
            #setup parallel backend
            cl <- makeCluster(detectCores())
            registerDoParallel(cl)

            spTemp = foreach(i = 1:nrow(df), .combine = 'rbind') %dopar% {
                rgeos::readWKT(df$wkt_geometry[i], df$id[i], p4s)
            }

            stopCluster(cl)

        } else {

            for (i in seq(nrow(df))) {
                print(i)
                if (i == 1) {
                    spTemp = readWKT(df$wkt_geometry[i], df$id[i], p4s)
                }
                else {
                    spTemp = rbind(
                        spTemp, readWKT(df$wkt_geometry[i], df$id[i], p4s)
                    )
                }
            }

        }

        # Create SpatialPolygonsDataFrame, drop WKT field from attributes
        spdf <- SpatialPolygonsDataFrame(spTemp, df[-(length(df) - 1)])
        spdf@proj4string <- p4s

        return(spdf)

    } else {

        return(NULL)

    }

}








#' @title Import Lines
#' @description Imports data from a sqlite database within a buffer distance D of point X, Y. The function currently assumes coordinate system GDA94 (EPSG:4283)
#' @param db character path and name of sqlite database
#' @param tbl character name of table to import
#' @param X numeric value of x coordinate
#' @param Y numeric value of y coordinate
#' @param D numeric buffer distance; defaults to 0.05 decimal degrees
#' @export
get.lines <- function(db, tbl, X, Y, D = .05) {
    require(RSQLite)
    require(rgeos)
    require(rgdal)

    m <- dbDriver("SQLite")
    con <- dbConnect(m, dbname = db,
                     loadable.extensions = TRUE)
    sql.sp <- "SELECT load_extension('mod_spatialite.dll')"
    rs <- dbGetQuery(con, sql.sp)

    sql <- sprintf("
                   SELECT *,
                   Distance(MakePoint(%1$s, %2$s, 4283), Geometry, 1) AS distance,
                   ST_AsText(geometry) AS wkt_geometry
                   FROM %3$s AS tbl
                   WHERE PtDistWithin(tbl.geometry, MakePoint(%1$s, %2$s, 4283), %4$s)
                   AND PK_UID IN (
                       SELECT rowid
                       FROM SpatialIndex
                       WHERE f_table_name = '%3$s'
                       AND search_frame = BuildCircleMbr(%1$s, %2$s, %4$s)
                   )
                   ORDER BY distance
                   ", X, Y, tbl, D)

    df = dbGetQuery(con, sql)

    # drop Geometry blob as it causes too many problem
    df = df[,-grep('Geometry', colnames(df))]

    dbDisconnect(con)

    if (nrow(df) > 0) {

        row.names(df) <- df$id <- 1:nrow(df)

        # Create spatial polygons
        # http://www.r-bloggers.com/load-postgis-geometries-in-r-without-rgdal/
        SRID <- 4283
        EPSG = make_EPSG()
        p4s = EPSG[which(EPSG$code == SRID), "prj4"]


        if (require(foreach) & require(doParallel)) {
            #setup parallel backend
            cl <- makeCluster(detectCores())
            registerDoParallel(cl)

            spTemp = foreach(i = 1:nrow(df), .combine = 'rbind') %dopar% {
                rgeos::readWKT(df$wkt_geometry[i], df$id[i], p4s)
            }

            stopCluster(cl)

        } else {

            for (i in seq(nrow(df))) {
                print(i)
                if (i == 1) {
                    spTemp = readWKT(df$wkt_geometry[i], df$id[i], p4s)
                }
                else {
                    spTemp = rbind(
                        spTemp, readWKT(df$wkt_geometry[i], df$id[i], p4s)
                    )
                }
            }

        }

        # Create SpatialPolygonsDataFrame, drop WKT field from attributes
        spdf = SpatialLinesDataFrame(spTemp, df[-(length(df) - 1)])

        return(spdf)

    } else {

        return(NULL)

    }

}







#' Get unigue values From Database Field
#'
#' @description This function returns a single column dataframe of unique values in a database table
#' @param db character filepath to database
#' @param tbl character name of database table
#' @param field character name of table column
#'
#' @return data.frame
#' @export
#'
#' @examples
get.unique <- function(db, tbl, field) {
    require(RSQLite)
    m <- dbDriver("SQLite")
    con <- dbConnect(m, dbname = db)

    sql <- sprintf("
                   SELECT DISTINCT %s
                   FROM %s;
                   ", field, tbl)
    df <- dbGetQuery(con, sql)

    dbDisconnect(con)

    return(df)
}



