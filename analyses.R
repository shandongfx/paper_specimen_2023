# housed species vs latitude
#-----------------#
df <- read.csv("housedVScollected_species.csv")
cor.test(abs(df4$centroid.lat), df4$housed_total)
# (r(243) = 0.26, p = 2.82e-05, 95% CI[0.14–0.38])
#-----------------#

# proportion of international specimens
#-----------------#
library(lsr)
df <- read.csv("herbarium_collection_proportions.csv")
# proportion of international physical collections
t.test(100-df$in_country_collections.p[which(df$colonzing_status == "colonizer")], 100-df$in_country_collections.p[which(df$colonzing_status == "colonized")])
cohensD(100-df$in_country_collections.p[which(df$colonzing_status == "colonizer")], 100-df$in_country_collections.p[which(df$colonzing_status == "colonized")])
# (t(59.10) = 3.58, p = 6.9e-04, d = 0.82, 95% CI[9.50, 33.54])

# proportion of international collections databased online
t.test(df$international_specimens_databased.p[which(df$colonzing_status == "colonizer")], df$international_specimens_databased.p[which(df$colonzing_status == "colonized")])
cohensD(df$international_specimens_databased.p[which(df$colonzing_status == "colonizer")], df$international_specimens_databased.p[which(df$colonzing_status == "colonized")])
# (t(56.01) = 2.81, p = 6.8e-03, d = 0.62, 95% CI[4.80, 28.62])

# proportion of international collections with digital images online
t.test(df$international_specimens_imaged.p[which(df$colonzing_status == "colonizer")], df$international_specimens_imaged.p[which(df$colonzing_status == "colonized")])
cohensD(df$international_specimens_imaged.p[which(df$colonzing_status == "colonizer")], df$international_specimens_imaged.p[which(df$colonzing_status == "colonized")])
# (t(35.82) = 2.61, p = 0.01, d = 0.65, 95% CI[5.14, 41.24])
#-----------------#

