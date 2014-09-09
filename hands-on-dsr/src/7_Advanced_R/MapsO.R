
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "Maps"
cat(paste0("\\newcommand{\\Module}{", Module, "}"))
RELOAD_MAPS <- FALSE


## ----setup, child="mycourse.Rnw"-----------------------------------------

## ----setup_options, include=FALSE----------------------------------------
library(knitr)
library(xtable)

opts_chunk$set(cache=FALSE)

opts_chunk$set(out.width='0.8\\textwidth')
opts_chunk$set(fig.align='center')

opts_chunk$set(src.top=NULL)
opts_chunk$set(src.bot=NULL)
opts_chunk$set(out.lines=4)
opts_chunk$set(out.truncate=80)

opts_chunk$set(fig.path=sprintf("figures/%s/", Module))
opts_chunk$set(cache.path=sprintf("cache/%s/", Module))
opts_chunk$set(bib.file=paste0(Module, ".bib"))

opts_chunk$set(background='#E7E7E7')

# Leave code as I have formatted it.

opts_chunk$set(tidy=FALSE)

# Hooks

# Allow auto crop of base graphics plots when crop=TRUE.

knit_hooks$set(crop=hook_pdfcrop)

# Truncate long lines and long output

hook_output <- knit_hooks$get("output")
hook_source <- knit_hooks$get("source")
knit_hooks$set(output=function(x, options) 
{
  if (options$results != "asis")
  {
    # Split string into separate lines.
    x <- unlist(stringr::str_split(x, "\n"))
    # Trim to the number of lines specified.
    if (!is.null(n <- options$out.lines)) 
    {
      if (length(x) > n) 
      {
        # Truncate the output.
        x <- c(head(x, n), "....\n")
      }
    }
    # Truncate each line to length specified.
    if (!is.null(m <- options$out.truncate))
    {
      len <- nchar(x)
      x[len>m] <- paste0(substr(x[len>m], 0, m-3), "...")
    }
    # Paste lines back together.
    x <- paste(x, collapse="\n")
    # Replace ' = ' with '=' - my preference. Hopefully won't 
    # affect things inappropriately.
    x <- gsub(" = ", "=", x)
  }
  hook_output(x, options)
},
source=function(x, options)
{
  # Split string into separate lines.
  x <- unlist(stringr::str_split(x, "\n"))
  # Trim to the number of lines specified.
  if (!is.null(n <- options$src.top)) 
  {
    if (length(x) > n) 
    {
      # Truncate the output.
      if (is.null(m <-options$src.bot)) m <- 0
      x <- c(head(x, n+1), "\n....\n", tail(x, m+2)) 
   }
  }
  # Paste lines back together.
  x <- paste(x, collapse="\n")
  hook_source(x, options)
})

# Optionally allow R Code chunks to be environments so we can refer to them.

knit_hooks$set(rcode=function(before, options, envir) 
{
  if (before)
    sprintf('\\begin{rcode}\\label{%s}\\hfill{}', options$label)
  else
    '\\end{rcode}'
})



## ----packages, message=FALSE---------------------------------------------
library(ggplot2)     # Plotting maps.
library(maps)        # Map data.
library(oz)          # Map data for Australia.
library(scales)      # Functions: alpha() comma()
library(ggmap)       # Google maps.


## ----echo=FALSE, message=FALSE-------------------------------------------
library(RgoogleMaps)
library(osmar)
library(OpenStreetMap)
library(rattle)


## ----child-demo, child="documentation.Rnw", eval=TRUE--------------------


## ----help_library, eval=FALSE, tidy=FALSE--------------------------------
## ?read.csv


## ----help_package, eval=FALSE--------------------------------------------
## library(help=rattle)


## ----record_start_time, echo=FALSE---------------------------------------
start.time <- proc.time()


## ----generate_bib, echo=FALSE, message=FALSE, warning=FALSE--------------
# Write all packages in the current session to a bib file
if (is.null(opts_chunk$get("bib.file"))) opts_chunk$set(bib.file="Course.bib")
write_bib(sub("^.*/", "", grep("^/", searchpaths(), value=TRUE)),
          file=opts_chunk$get("bib.file"))
system(paste("cat extra.bib >>", opts_chunk$get("bib.file")))
# Fix up specific issues.
# R-earth
system(paste("perl -pi -e 's|. Derived from .*$|},|'",
             opts_chunk$get("bib.file")))
# R-randomForest
system(paste("perl -pi -e 's|Fortran original by Leo Breiman",
             "and Adele Cutler and R port by|Leo Breiman and",
             "Adele Cutler and|'", opts_chunk$get("bib.file")))
# R-C50
system(paste("perl -pi -e 's|. C code for C5.0 by R. Quinlan|",
             " and J. Ross Quinlan|'", opts_chunk$get("bib.file")))
# R-caret
system(paste("perl -pi -e 's|. Contributions from|",
             " and|'", opts_chunk$get("bib.file")))
# Me
system(paste("perl -pi -e 's|Graham Williams|",
             "Graham J Williams|'", opts_chunk$get("bib.file")))




## ----cache_get_map, echo=FALSE-------------------------------------------
# Avoid downloading too much. Cache maps obtained using get_map() in maps/

if (! file.exists("maps")) dir.create("maps")

# Call cacheMap to cache the maps.

cacheMap <- function(name, reload=RELOAD_MAPS, ...)
{
  # Save all maps as "map"
  fname <- sprintf("maps/%s.RData", name)
  if (! reload & file.exists(fname))
  {
    load(fname)
  }
  else
  {
    map <- ggmap::get_map(...)
    save(map, file=fname)
  }
  invisible(map)
}

# So we can pretend to be using get_map()

get_map <- function(...) {invisible(map)}


## ----echo=FALSE----------------------------------------------------------
op <- options(digits=7)


## ----assumes_online_but_complains_about_sink, message=FALSE--------------
library(ggmap)
geocode("New York")
geocode("Qiushi Road, Shenzhen")
geocode("Canberra")
geocode("Gus' Cafe, Garema Place, Canberra, Australia")
geocode("9 Bunda Street, Canberra")
geocode("11 Bunda Street, Canberra")


## ----message=FALSE-------------------------------------------------------
(syd <- as.numeric(geocode("Sydney")))
(cbr <- as.numeric(geocode("Canberra")))
sydbb <- c(151.15, -33.88, 151.25, -33.84)


## ----echo=FALSE----------------------------------------------------------
options(op)


## ----echo=FALSE----------------------------------------------------------
opts_chunk$set(fig.width=12, out.width='\\textwidth')


## ----world_data, echo=FALSE----------------------------------------------
ds <- map_data("world")


## ----world_map, echo=FALSE-----------------------------------------------
p <- ggplot(ds, aes(x=long, y=lat, group=group))
p <- p + geom_polygon()
p


## ----world_data_show-----------------------------------------------------
ds <- map_data("world")
class(ds)
str(ds)
head(ds)


## ----world_map, eval=FALSE, ref.label="world_map"------------------------
## p <- ggplot(ds, aes(x=long, y=lat, group=group))
## p <- p + geom_polygon()
## p


## ----world_colour, echo=FALSE--------------------------------------------
p <- ggplot(ds, aes(x=long, y=lat, group=group, fill=region))
p <- p + geom_polygon()
p <- p + theme(legend.position = "none")
p


## ----world_colour_show, eval=FALSE, ref.label="world_colour"-------------
## NA


## ------------------------------------------------------------------------
length(unique(ds$region))


## ----echo=FALSE----------------------------------------------------------
opts_chunk$set(fig.width=9, out.width='\\textwidth')


## ----out.lines=NULL------------------------------------------------------
ds <- read.csv(file.path("data", "ozdata.csv"))
dim(ds)
head(ds, 2)
tail(ds, 2)
str(ds)
summary(ds)


## ----aus_map, echo=FALSE-------------------------------------------------
p <- ggplot(ds, aes(long, lat, fill=state))
p <- p + geom_polygon()
p <- p + coord_equal()
p <- p + ggtitle("States of Australia")
p


## ----aus_map_show, eval=FALSE, ref.label="aus_map"-----------------------
## NA


## ----aus_coast, echo=FALSE-----------------------------------------------
p <- ggplot(subset(ds, border=="coast"), aes(long, lat, fill=state))
p <- p + geom_path()
p <- p + coord_equal()
p <- p + ggtitle("Coastline of Australia")
p


## ----aus_coast_show, eval=FALSE, ref.label="aus_coast"-------------------
## NA


## ----nsw, echo=FALSE-----------------------------------------------------
p <- ggplot(subset(ds, state=="NSW"), aes(long, lat, fill=state))
p <- p + geom_polygon()
p <- p + coord_equal()
p <- p + ggtitle("Map of New South Wales")
p <- p + theme(legend.position="none")
p


## ----nsw_echo, eval=FALSE, ref.label="nsw"-------------------------------
## NA


## ----cache_sydney_google_map, echo=FALSE---------------------------------
map <- cacheMap("syd_google_roadmap",
                location="Sydney", zoom=14, maptype="roadmap", source="google")


## ----sydney_google_map, echo=FALSE---------------------------------------
map <- get_map(location="Sydney", zoom=14, maptype="roadmap", source="google")
p <- ggmap(map)
p


## ----sydney_google_map_echo, eval=FALSE, ref.label="sydney_google_map"----
## NA


## ------------------------------------------------------------------------
class(p)


## ----cache_sydney_google_map_geo, echo=FALSE-----------------------------
map <- cacheMap("syd_google_roadmap_geo",
                location=syd, zoom=14, maptype="roadmap", source="google")


## ----sydney_google_map_geo, echo=FALSE-----------------------------------
map <- get_map(location=syd, zoom=14, maptype="roadmap", source="google")
p <- ggmap(map)
p


## ----sydney_google_map_geo_echo, eval=FALSE, ref.label="sydney_google_map_geo"----
## NA


## ----message=FALSE-------------------------------------------------------
(syd <- as.numeric(geocode("Sydney")))


## ----sydney_google_map_device_panel, echo=FALSE--------------------------
map <- get_map(location="Sydney", zoom=14, maptype="roadmap", source="google")
p <- ggmap(map, extent="normal")
p


## ----sydney_google_map_device_panel_echo, eval=FALSE, ref.label="sydney_google_map_device_panel"----
## NA


## ----sydney_google_map_device_extent, echo=FALSE-------------------------
map <- get_map(location="Sydney", zoom=14, maptype="roadmap", source="google")
p <- ggmap(map, extent="device")
p


## ----sydney_google_map_device_extent_echo, eval=FALSE, ref.label="sydney_google_map_device_extent"----
## NA


## ----cache_sydney_google_map_terrain, echo=FALSE-------------------------
map <- cacheMap("syd_google_terrain", 
                location="Sydney", zoom=14, maptype="terrain", source="google")


## ----sydney_google_map_terrain, echo=FALSE-------------------------------
map <- get_map(location="Sydney", zoom=14, maptype="terrain", source="google")
p <- ggmap(map)
p


## ----sydney_google_map_terrain_echo, eval=FALSE, ref.label="sydney_google_map_terrain"----
## NA


## ----cache_sydney_google_map_satellite, echo=FALSE-----------------------
map <- cacheMap("syd_google_satellite", 
                location="Sydney", zoom=14, maptype="satellite", source="google")


## ----sydney_google_map_satellite, echo=FALSE-----------------------------
map <- get_map(location="Sydney", zoom=14, maptype="satellite", source="google")
p <- ggmap(map)
p


## ----sydney_google_map_satellite_echo, eval=FALSE, ref.label="sydney_google_map_satellite"----
## NA


## ----cache_sydney_google_map_hybrid, echo=FALSE--------------------------
map <- cacheMap("syd_google_hybrid", 
                location="Sydney", zoom=14, maptype="hybrid", source="google")


## ----sydney_google_map_hybrid, echo=FALSE--------------------------------
map <- get_map(location="Sydney", zoom=14, maptype="hybrid", source="google")
p <- ggmap(map)
p


## ----sydney_google_map_hybrid_echo, eval=FALSE, ref.label="sydney_google_map_hybrid"----
## NA


## ----cache_sydney_osm_map, echo=FALSE------------------------------------
map <- cacheMap("syd_osm", 
                location="Sydney", zoom=14, source="osm")


## ----sydney_osm_map, echo=FALSE------------------------------------------
map <- get_map(location="Sydney", zoom=14, source="osm")
p <- ggmap(map)
p


## ----sydney_osm_map_echo, eval=FALSE, ref.label="sydney_osm_map"---------
## NA


## ----cache_sydney_watercolour_map, echo=FALSE----------------------------
map <- cacheMap("syd_stamen_watercolour", 
                location="Sydney", zoom=14, maptype="watercolor", source="stamen")


## ----sydney_watercolour_map, echo=FALSE----------------------------------
map <- get_map(location="Sydney", zoom=14, maptype="watercolor", source="stamen")
p <- ggmap(map)
p


## ----sydney_watercolour_map, eval=FALSE, ref.label="sydney_watercolour_map"----
## map <- get_map(location="Sydney", zoom=14, maptype="watercolor", source="stamen")
## p <- ggmap(map)
## p


## ----cache_sydney_toner_map, echo=FALSE----------------------------------
map <- cacheMap("syd_stamen_toner", 
                location="Sydney", zoom=14, maptype="toner", source="stamen")


## ----sydney_toner, echo=FALSE--------------------------------------------
map <- get_map(location="Sydney", zoom=14, maptype="toner", source="stamen")
p <- ggmap(map)
p


## ----sydney_toner_echo, eval=FALSE, ref.label="sydney_toner"-------------
## NA


## ----cache_sydney_watercolor_bb_map, echo=FALSE--------------------------
map <- cacheMap("syd_stamen_watercolor_bb", 
                location=sydbb, maptype="watercolor", source="stamen")


## ----sydney_water_colour, echo=FALSE-------------------------------------
map <- get_map(location=sydbb, maptype="watercolor", source="stamen")
p <- ggmap(map)
p


## ----sydney_water_colour, eval=FALSE, ref.label="sydney_water_colour"----
## map <- get_map(location=sydbb, maptype="watercolor", source="stamen")
## p <- ggmap(map)
## p


## ----cache_canberra_google_map, echo=FALSE-------------------------------
map <- cacheMap("cbr_google_roadmap",
                location="Canberra", zoom=15, maptype="roadmap", source="google")


## ----canberra_google_map, echo=FALSE-------------------------------------
map <- get_map(location="Canberra", zoom=15, maptype="roadmap", source="google")
p <- ggmap(map)
p


## ----canberra_google_map_show, eval=FALSE, ref.label="canberra_google_map"----
## NA


## ----canberra_google_map_text, echo=FALSE--------------------------------
map <- get_map(location="Canberra", zoom=15, maptype="roadmap", source="google")
dflbl <- data.frame(lon=149.1230, lat=-35.2775, text="ANU")
p <- ggmap(map)
p <- p + geom_text(data=dflbl, aes(x=lon, y=lat, label=text),
                   size=10, colour="blue", fontface="bold")
p


## ----canberra_google_map_text_show, eval=FALSE, ref.label="canberra_google_map_text"----
## NA


## ----echo=FALSE----------------------------------------------------------
op <- options(digits=7)


## ----canberra_google_map_ann, echo=FALSE, message=FALSE------------------
landmarks <- c("Gus Cafe, Bunda Street, Canberra", "Canberra Centre, Canberra",
               "Canberra School of Music", "Jolimont Centre",
               "Australian National University")
lbls <- cbind(geocode(landmarks), text=landmarks)
p <- ggmap(map)
p <- p + geom_point(data=lbls, aes(x=lon, y=lat), size=5, colour="orange")
p <- p + geom_point(data=lbls, aes(x=lon, y=lat), size=3, colour="red")
p <- p + geom_text(data=lbls, aes(x=lon, y=lat, label=text), 
                   size=3, colour="blue", hjust=0, vjust=0)
p


## ----echo=FALSE----------------------------------------------------------
options(op)


## ----canberra_google_map_ann_show, eval=FALSE, ref.label="canberra_google_map_ann"----
## NA


## ----echo=FALSE, message=FALSE-------------------------------------------
map <- cacheMap("aus_google", location=as.numeric(geocode("Australia")), 
                zoom=4, source="google")


## ----aus_google, echo=FALSE, message=FALSE-------------------------------
map <- get_map(location=as.numeric(geocode("Australia")), 
               zoom=4, source="google")
p <- ggmap(map)
p


## ----aus_google_show, eval=FALSE, ref.label="aus_google"-----------------
## NA


## ----echo=FALSE----------------------------------------------------------
map <- cacheMap("aus_osm", location="Australia", zoom=4, source="osm")


## ----aus_osm, echo=FALSE-------------------------------------------------
map <- get_map(location="Australia", zoom=4, source="osm")
p <- ggmap(map)
p


## ----aus_osm_show, eval=FALSE, ref.label="aus_osm"-----------------------
## NA


## ----echo=FALSE----------------------------------------------------------
map <- cacheMap("europe", location="Europe", zoom=4)


## ----google_europe, echo=FALSE-------------------------------------------
map <- get_map(location="Europe", zoom=4)
p <- ggmap(map)
p


## ----google_europe_show, eval=FALSE, ref.label="google_europe"-----------
## NA


## ----echo=FALSE, message=FALSE-------------------------------------------
map <- cacheMap("ny_google", location=as.numeric(geocode("New York")), 
                zoom=14, source="google")


## ----google_new_york, echo=FALSE, message=FALSE--------------------------
map <- get_map(location=as.numeric(geocode("New York")), 
               zoom=14, source="google")
p <- ggmap(map)
p


## ----google_new_york_show, eval=FALSE, ref.label="google_new_york"-------
## NA


## ----echo=FALSE----------------------------------------------------------
map <- cacheMap("sz_google", location=as.numeric(geocode("Qiushi Road, Shenzhen")), 
                zoom=14, source="google")


## ----google_sz, echo=FALSE-----------------------------------------------
map <- get_map(location=as.numeric(geocode("Qiushi Road, Shenzhen")), 
               zoom=14, source="google")
p <- ggmap(map)
p


## ----google_sz-echo, eval=FALSE, ref.label="google_sz"-------------------
## NA


## ----echo=FALSE----------------------------------------------------------
map <- cacheMap("sz_google_satellite", 
                location=as.numeric(geocode("Qiushi Road, Shenzhen")), 
                zoom=14, maptype="satellite", source="google")


## ----goole_sz_sat, echo=FALSE--------------------------------------------
map <- get_map(location=as.numeric(geocode("Qiushi Road, Shenzhen")), 
               zoom=14, maptype="satellite", source="google")
p <- ggmap(map)
p


## ----goole_sz_sat_echo, eval=FALSE, ref.label="goole_sz_sat"-------------
## NA


## ----echo=FALSE, message=FALSE-------------------------------------------
addr <- "21 Bambridge Street, Weetangera, ACT, Australia"
loc <- as.numeric(geocode(addr))
map <- cacheMap("cbr_street_addr", location=loc, zoom=15, source="google")


## ----addr, echo=FALSE, message=FALSE-------------------------------------
addr <- "21 Bambridge Street, Weetangera, ACT, Australia"
loc <- as.numeric(geocode(addr))
lbl <- data.frame(lon=loc[1], lat=loc[2], text=addr)
map <- get_map(location=loc, zoom=15, source="google")
p <- ggmap(map)
p <- p + geom_point(data=lbl, aes(x=lon, y=lat), size=5, colour="orange")
p <- p + geom_point(data=lbl, aes(x=lon, y=lat), size=3, colour="red")
p <- p + geom_text(data=lbl, aes(x=lon, y=lat, label=text), 
                   size=5, colour="blue", hjust=0.5, vjust=5)
p


## ----addr, eval=FALSE, ref.label="addr"----------------------------------
## addr <- "21 Bambridge Street, Weetangera, ACT, Australia"
## loc <- as.numeric(geocode(addr))
## lbl <- data.frame(lon=loc[1], lat=loc[2], text=addr)
## map <- get_map(location=loc, zoom=15, source="google")
## p <- ggmap(map)
## p <- p + geom_point(data=lbl, aes(x=lon, y=lat), size=5, colour="orange")
## p <- p + geom_point(data=lbl, aes(x=lon, y=lat), size=3, colour="red")
## p <- p + geom_text(data=lbl, aes(x=lon, y=lat, label=text),
##                    size=5, colour="blue", hjust=0.5, vjust=5)
## p


## ----echo=FALSE, message=FALSE-------------------------------------------
addr <- "21 Bambridge Street, Weetangera, ACT, Australia"
loc <- as.numeric(geocode(addr))
map <- cacheMap("cbr_street_addr", location=loc, zoom=15, source="google")


## ----addr_circle, echo=FALSE, message=FALSE------------------------------
addr <- "21 Bambridge Street, Weetangera, ACT, Australia"
loc <- as.numeric(geocode(addr))
lbl <- data.frame(lon=loc[1], lat=loc[2], text=addr)
map <- get_map(location=loc, zoom=15, source="google")
p <- ggmap(map)
p <- p + geom_point(data=lbl, aes(x=lon, y=lat), size=5, 
                    shape=1, colour="red")
p <- p + geom_text(data=lbl, aes(x=lon, y=lat, label=text), 
                   size=5, colour="blue", hjust=0.5, vjust=5)
p


## ----addr_circle_show, eval=FALSE, ref.label="addr_circle"---------------
## NA


## ----echo=FALSE, message=FALSE-------------------------------------------
addr <- "21 Bambridge Street, Weetangera, ACT, Australia"
loc <- as.numeric(geocode(addr))
map <- cacheMap("cbr_street_addr_sat", location=loc, 
                maptype="hybrid", zoom=15, source="google")


## ----addr_sat, echo=FALSE, message=FALSE---------------------------------
addr <- "21 Bambridge Street, Weetangera, ACT, Australia"
loc <- as.numeric(geocode(addr))
lbl <- data.frame(lon=loc[1], lat=loc[2], text=addr)
map <- get_map(location=loc, zoom=15, maptype="hybrid", source="google")
p <- ggmap(map)
p <- p + geom_point(data=lbl, aes(x=lon, y=lat), 
                    alpha=I(0.5), size=I(5), colour="red")
p <- p + geom_text(data=lbl, aes(x=lon, y=lat, label=text), 
                   size=5, colour="white", hjust=0.5, vjust=5)
p


## ----addr_sat_show, eval=FALSE, ref.label="addr_sat"---------------------
## NA


## ------------------------------------------------------------------------
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))
head(arrests)


## ------------------------------------------------------------------------
states <- map_data("state")
head(states)
ds <- merge(states, arrests, sort=FALSE, by="region")
head(ds)
ds <- ds[order(ds$order), ]
head(ds)


## ----plot_usarrests_assaults_per_merder, fig.width=12, out.width='0.8\\textwidth'----
g <- ggplot(ds, aes(long, lat, group=group, fill=assault/murder))
g <- g + geom_polygon()
print(g)


## ----plot_usarrests_urbanpop, fig.width=12, out.width='0.8\\textwidth'----
g <- ggplot(ds, aes(long, lat, group=group, fill=urbanpop))
g <- g + geom_polygon()
print(g)


## ----osm_portland, echo=FALSE, message=FALSE-----------------------------
library(OpenStreetMap)
stores <- data.frame(name=c("Commercial","Union","Bedford"),
                     lon=c(-70.25042295455, -70.26050806045, -70.27726650238),
                     lat=c(43.657471302616, 43.65663299041, 43.66091757424))
lat <- c(43.68093, 43.64278)
lon <- c(-70.29548, -70.24097)
portland <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]), zoom=15, 'osm')
plot(portland, raster=TRUE)


## ----osm_portland_echo, eval=FALSE, ref.label="osm_portland"-------------
## NA


## ----portland_location, echo=FALSE---------------------------------------
stores <- data.frame(name=c("Commercial", "Union", "Bedford"),
                     lon=c(-70.25042295455, -70.26050806045, -70.27726650238),
                     lat=c( 43.65747130261,  43.65663299041,  43.66091757424))

portland <- c(-70.2954, 43.64278, -70.2350, 43.68093)


## ----echo=FALSE----------------------------------------------------------
map <- cacheMap("portland_osm", location=portland, source="osm")


## ----portland_annotate, echo=FALSE---------------------------------------
library(ggmap)
map <- get_map(location=portland, source="osm")

g <- ggmap(map)
g <- g + geom_point(data=stores, aes(x=lon, y=lat), size=5)
g <- g + geom_text(data=stores, aes(label=name, x=lon+.001, y=lat), hjust=0)
print(g)


## ----portland_location_show, eval=FALSE, ref.label="portland_location"----
## NA

## ----portland_annotate_show, eval=FALSE, ref.label="portland_annotate"----
## NA


## ----portland_annotate_highlight, echo=FALSE-----------------------------
g <- ggmap(map)
g <- g + geom_point(data=stores, aes(x=lon, y=lat), size=5)

theta <- seq(pi/16, 2*pi, length.out=32)
xo <- diff(portland[c(1,3)])/250
yo <- diff(portland[c(2,4)])/250

for(i in theta) 
  g <- g + geom_text(data=stores, bquote(aes(x=lon + .001 + .(cos(i) * xo), 
                       y=lat + .(sin(i) * yo), label = name)), 
                    size=5, colour='black', hjust=0)

g <- g + geom_text(data=stores, aes(x=lon+.001, y=lat, label=name), 
                   size=5, colour='white', hjust=0)
print(g)


## ----portland_annotate_highlight_show, eval=FALSE, ref.label="portland_annotate_highlight"----
## NA


## ----common_outtro, child="finale.Rnw", eval=TRUE------------------------


## ----syinfo, child="sysinfo.Rnw", eval=TRUE------------------------------

## ----echo=FALSE, message=FALSE-------------------------------------------
require(Hmisc)
pkg <- "knitr"
pkg.version <- installed.packages()[pkg, 'Version']
pkg.date <- installed.packages(fields="Date")[pkg, 'Date']
pkg.info <- paste(pkg, pkg.version, pkg.date)

rev <- system("bzr revno", intern=TRUE)
cpu <- system(paste("cat /proc/cpuinfo | grep 'model name' |",
                    "head -n 1 | cut -d':' -f2"), intern=TRUE)
ram <- system("cat /proc/meminfo | grep MemTotal: | awk '{print $2}'",
              intern=TRUE)
ram <- paste0(round(as.integer(ram)/1e6, 1), "GB")
user <- Sys.getenv("LOGNAME")
node <- Sys.info()[["nodename"]]
user.node <- paste0(user, "@", node)
gcc.version <- system("g++ -v 2>&1 | grep 'gcc version' | cut -d' ' -f1-3",
                      intern=TRUE)
os <- system("lsb_release -d | cut -d: -f2 | sed 's/^[ \t]*//'", intern=TRUE)





