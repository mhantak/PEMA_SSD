#load packages
require(dplyr)


#load data
allMamm <- read.csv("https://de.cyverse.org/dl/d/7B6941EE-1FED-4AAE-9D8A-8AC76DB0AC98/data.all.csv", header = TRUE)

#Peromyscus maniculatus
pema <- allMamm[allMamm$scientificName == "Peromyscus maniculatus",]

write.csv(pema, "pema.csv")

#pema data extracted from the futres datastore
#all lifeStage are either juvenile, adult, or NS
#records without lifeStage are labeled as outlier or possibly good in measurementStatus
pema <- read.csv("https://de.cyverse.org/dl/d/ECECB910-C698-4B00-B275-82C473CC28A0/futres_pema.csv", header = TRUE)

pema <- pema %>%
  drop_na(measurementType)

pema.trim <- pema[pema$lifeStage != "Juvenile" & pema$measurementStatus != "outlier",]

pema.trim$log.measurementValue <- log10(pema.trim$measurementValue)

pema.trim <- pema.trim[!is.infinite(pema.trim$log.measurementValue),]

#make short version
pema.mass <- pema.trim[pema.trim$measurementType == "mass",]
pema.total.length <- pema.trim[pema.trim$measurementType == "total.length",]
pema.tail <- pema.trim[pema.trim$measurementType == "tail.length",]

pema.length.trim <- pema.total.length %>%
  dplyr::select(individualID,
                measurementType,
                log.total.length = log.measurementValue,
                total.length = measurementValue) %>%
  as.data.frame()

pema.tail.trim <- pema.tail %>%
  dplyr::select(individualID,
                measurementType,
                log.tail.length = log.measurementValue,
                tail.length = measurementValue) %>%
  as.data.frame()

colnames(pema.mass)[colnames(pema.mass) == "log.measurementValue"] <- "log.mass"
colnames(pema.mass)[colnames(pema.mass) == "measurementValue"] <- "mass"
pema.mass.trim <- pema.mass[,c(1:42, 48:49)]

pema.short <- merge(pema.tail.trim, pema.length.trim, by = "individualID", all.x = FALSE, all.y = FALSE)
pema.short <- merge(pema.short, pema.mass.trim, by = "individualID", all.x = FALSE, all.y = FALSE)

pema.short$head.body.length <- pema.short$total.length - pema.short$tail.length
pema.short$log.head.body.length <- log10(pema.short$head.body.length)

pema.final <- pema.short[!is.infinite(pema.short$log.head.body.length),]

pema.final <- pema.final %>%
  drop_na(head.body.length, log.head.body.length)

write.csv(pema.final, "pema.for.SSD.csv")

pema.final <- read.csv("https://de.cyverse.org/dl/d/A4DC9C95-9E2E-4241-B7B9-F624CA5CD753/pema.for.SSD.csv", header = TRUE)

#cutoff from https://animaldiversity.org/accounts/Peromyscus_maniculatus/
pema.cutoff <- pema.final[pema.final$mass >= 10 & pema.final$mass <= 24,]
pema.cutoff <- pema.cutoff[pema.cutoff$total.length >= 119 & pema.cutoff$total.length <= 222,]

write.csv(pema.cutoff, "pema.trimmed.SSD.csv")

pema.trimmed <- read.csv("https://de.cyverse.org/dl/d/F2F2BD17-82AF-466E-8710-70F0F884C14E/pema.trimmed.SSD.csv", header = TRUE)

#Peromyscus leucopus
pele <- allMamm[allMamm$scientificName == "Peromyscus leucopus",]

write.csv(pele, "pele.csv")

pele <- read.csv("https://de.cyverse.org/dl/d/E476FB9B-9891-4AF0-A1D6-384BCC98D5A8/futres_pele.csv", header = TRUE)

pele <- pele %>%
  drop_na(measurementType)

pele.trim <- pele[pele$lifeStage != "Juvenile" & pele$measurementStatus != "outlier",]

pele.trim$log.measurementValue <- log10(pele.trim$measurementValue)

pele.trim <- pele.trim[!is.infinite(pele.trim$log.measurementValue),]

#make short version
pele.mass <- pele.trim[pele.trim$measurementType == "mass",]
pele.total.length <- pele.trim[pele.trim$measurementType == "total.length",]
pele.tail <- pele.trim[pele.trim$measurementType == "tail.length",]

pele.length.trim <- pele.total.length %>%
  dplyr::select(individualID,
                measurementType,
                log.total.length = log.measurementValue,
                total.length = measurementValue) %>%
  as.data.frame()

pele.tail.trim <- pele.tail %>%
  dplyr::select(individualID,
                measurementType,
                log.tail.length = log.measurementValue,
                tail.length = measurementValue) %>%
  as.data.frame()

colnames(pele.mass)[colnames(pele.mass) == "log.measurementValue"] <- "log.mass"
colnames(pele.mass)[colnames(pele.mass) == "measurementValue"] <- "mass"
pele.mass.trim <- pele.mass[,c(1:41, 47:48)]

pele.short <- merge(pele.tail.trim, pele.length.trim, by = "individualID", all.x = FALSE, all.y = FALSE)
pele.short <- merge(pele.short, pele.mass.trim, by = "individualID", all.x = FALSE, all.y = FALSE)

pele.short$head.body.length <- pele.short$total.length - pele.short$tail.length
pele.short$log.head.body.length <- log10(pele.short$head.body.length)

pele.final <- pele.short[!is.infinite(pele.short$log.head.body.length),]

pele.final <- pele.final %>%
  drop_na(head.body.length, log.head.body.length)

write.csv(pele.final, "pele.for.SSD.csv")

pele.final <- read.csv("https://de.cyverse.org/dl/d/316E8813-5F0B-49B4-A2DA-481436D3FAF8/pele.for.SSD.csv", header = TRUE)

#cutoff from https://animaldiversity.org/accounts/Peromyscus_leucopus/
pele.cutoff <- pele.final[pele.final$mass >= 15 & pele.final$mass <= 25,]
pele.cutoff <- pele.cutoff[pele.cutoff$total.length >= 150 & pele.cutoff$total.length <= 205,]

write.csv(pele.cutoff, "pele.trimmed.SSD.csv")

pele.trimmed <- read.csv("https://de.cyverse.org/dl/d/1541D138-9A6D-4A56-93A2-CD6C92E7EA07/pele.trimmed.SSD.csv", header = TRUE)

