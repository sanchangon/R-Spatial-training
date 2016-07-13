#-----------------------------------------------
# chingchai humhong
# 12/07/2016 - 13/07/2016
# chingchai.h@gmail.com
# GISTNU @ Naresuan University
# MIT@License
# github : https://github.com/sanchangon
#-----------------------------------------------------------------------------------------
# set work directory my project
setwd("C:/Users/sanchangon/Documents/GitHub/R-Spatial-training")
getwd()
library(maps)
library(mapdata)
map()
map('county', 'new jersey')
map('worldHires', 'Thailand', col="gray90", fill=TRUE)

map("worldHires", "Thailand", myborder = 0, col="gray90", fill=TRUE)
map.axes()	

#-----------------------------------------------------------------------------------------
windows(height=10,width=15)
map()	# low resolution map of the world
map('usa')	# national boundaries
map('county', 'new jersey')	# county map of New Jersey
map('state', region = c('new york', 'new jersey', 'penn'))	# map of three states
map("state", ".*dakota", myborder = 0)	# map of the dakotas
map.axes()				# show the effect of myborder = 0
if(require(mapproj))
  map('state', proj = 'bonne', param = 45)	# Bonne equal-area projection of states

# names of the San Juan islands in Washington state
map('county', 'washington,san', names = TRUE, plot = FALSE)

# national boundaries in one linetype, states in another
# (figure 5 in the reference)
map("state", interior = FALSE)
map("state", boundary = FALSE, lty = 2, add = TRUE)

# plot the ozone data on a base map
# (figure 4 in the reference)
data(ozone)
ozone
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
text(ozone$x, ozone$y, ozone$median)
box()
map.axes()

if(require(mapproj)) {	# mapproj is used for  projection="polyconic"
  # color US county map by 2009 unemployment rate
  # match counties to map using FIPS county codes
  # Based on J's solution to the "Choropleth Challenge"
  # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
  
  # load data
  # unemp includes data for some counties not on the "lower 48 states" county
  # map, such as those in Alaska, Hawaii, Puerto Rico, and some tiny Virginia
  #  cities
  # set windows
  windows(height=10,width=15)
  data(unemp)
  data(county.fips)
  
  # define color buckets
  colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
  unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
  leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
  
  # align data with map definitions by (partial) matching state,county
  # names, which include multiple polygons for some counties
  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                      county.fips$polyname)]
  colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]
  
  # draw map
  map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
      lty = 0, projection = "polyconic")
  map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
      projection="polyconic")
  title("unemployment by county, 2009")
  legend("topright", leg.txt, horiz = TRUE, fill = colors)
  
  # Choropleth Challenge example, based on J's solution, see:
  # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
  # To see the faint county boundaries, use RGui menu:  File/SaveAs/PDF
}

#-----------------------------------------------------------------------------------------



