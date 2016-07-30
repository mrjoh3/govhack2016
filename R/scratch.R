
library(rgdal)
library(raster)

subs <- readOGR('data', 'SSC_2011_AUST')
roads <- readOGR('data/vmlite', 'vmlite_tr_road')

tmp <- intersect(subs, roads)


"
SELECT ssc_code, ssc_name, r.pk_uid, r.road_name, ST_Length(ST_Difference(r.geometry, s.geometry), 1) as r_dist
FROM ssc_2011_aust AS s, vmlite_tr_road AS r
WHERE ST_Intersects(s.geometry, r.geometry)
AND ssc_code >= 20000
AND ssc_code < 30000
AND s.PK_UID IN (
SELECT rowid
FROM SpatialIndex
WHERE f_table_name = 'ssc_2011_aust'
AND search_frame = Envelope(s.geometry)
);"




sp_join_line <- function(db, poly, ln) {
    
    require(RSQLite)
    m <- dbDriver("SQLite")
    con <- dbConnect(m, dbname = db,
                     loadable.extensions = TRUE)
    sql <- "SELECT load_extension('mod_spatialite.dll')"
    rs <- dbGetQuery(con, sql)
    
    sql <- sprintf("
                   SELECT ssc_code, ST_Length(ST_Intersection(ln.geometry, s.geometry), 1) as ln_dist
                   FROM ssc_2011_aust AS s, %s AS ln
                   WHERE ST_Intersects(s.geometry, ln.geometry)
                   AND ssc_code >= 20000
                   AND ssc_code < 30000
                   AND s.PK_UID IN (
                   SELECT rowid
                   FROM SpatialIndex
                   WHERE f_table_name = '%s'
                   AND search_frame = Envelope(ln.geometry)
                   );
                   ", ln, ln)
    
    df <- dbGetQuery(con, sql)
    
    dbDisconnect(con)
    
    return(df)
    
}




sp_join_poly <- function(db, poly, poly2) {
    
    require(RSQLite)
    m <- dbDriver("SQLite")
    con <- dbConnect(m, dbname = db,
                     loadable.extensions = TRUE)
    sql <- "SELECT load_extension('mod_spatialite.dll')"
    rs <- dbGetQuery(con, sql)
    
    sql <- sprintf("
                   SELECT ssc_code, ST_Area(ST_Intersection(ply.geometry, s.geometry), 1) as ply_dist
                   FROM ssc_2011_aust AS s, %s AS ply
                   WHERE ST_Intersects(s.geometry, ply.geometry)
                   AND ssc_code >= 20000
                   AND ssc_code < 30000
                   AND s.PK_UID IN (
                   SELECT rowid
                   FROM SpatialIndex
                   WHERE f_table_name = '%s'
                   AND search_frame = Envelope(ply.geometry)
                   );
                   ", poly2, poly2)
    
    df <- dbGetQuery(con, sql)
    
    dbDisconnect(con)
    
    return(df)
    
}
