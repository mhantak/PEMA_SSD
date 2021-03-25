allMamm <- read.csv("https://de.cyverse.org/dl/d/7B6941EE-1FED-4AAE-9D8A-8AC76DB0AC98/data.all.csv", header = TRUE)

pema <- allMamm[allMamm$scientificName == "Peromyscus maniculatus",]

write.csv(pema, "pema.csv")

#pema data extracted from the futres datastore
#all lifeStage are either juvenile, adult, or NS
#records without lifeStage are labeled as outlier or possibly good in measurementStatus
pema <- read.csv("https://de.cyverse.org/dl/d/ECECB910-C698-4B00-B275-82C473CC28A0/futres_pema.csv", header = TRUE)