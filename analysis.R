library(tidyverse) # metapackage with lots of helpful functions
library(dplyr)
library(RColorBrewer)

df_site = read.csv("neon-site_data.csv", header = TRUE)

###################################################                 ############################################################################
###################################################    NUMBER 4     ############################################################################
################################################################################################################################################

# get the State and SiteType columns and insert them into a new data-frame
state_sites_df <- select(df_site, State, SiteType) 

# count the total # of sites for each state
state_sites_freq <- table(state_sites_df) 

# convert to tibble to transform data for later
state_sites_tbl <- as_tibble(state_sites_freq) 

state_sites_tbl %>%
  spread(key = SiteType, value = n)

site_types <- spread(state_sites_tbl,key = SiteType, value = n) # 
names(site_types) <- gsub(" ", "_", names(site_types)) # remove white space in column name and replace it with an under-score
site_types_df <- as.data.frame(site_types) # convert to a data_frame

# Create a new data frame to hold all the aquatic and terrestrial data with a column for each
state_sites_df <- data.frame(matrix(ncol = 0, nrow = 25))

# append state names
state_sites_df$State <- site_types_df$State

# merge all aquatic and terrestrial data
state_sites_df$Aquatic <- site_types_df$Core_Aquatic + site_types_df$Relocatable_Aquatic
state_sites_df$Terrestrial <- site_types_df$Core_Terrestrial + site_types_df$Relocatable_Terrestrial

# Total number of NEON sites by state
state_sites_df$Total_sites <- state_sites_df$Aquatic + state_sites_df$Terrestrial

arrange(state_sites_df, desc(Aquatic))
arrange(state_sites_df, desc(Terrestrial) )
arrange(state_sites_df, desc(Total_sites) )

g <- ggplot(data = state_sites_df, aes(x = State, y = Total_sites, fill = Total_sites ))
g + geom_bar(stat="identity", position=position_dodge(), color="Black" ) +
  labs(title = "Total NEON sites by state")

g <- ggplot(data = state_sites_df, aes(x = State, y = Terrestrial, fill = Terrestrial ))
g_bar <- g + geom_bar(stat="identity", position=position_dodge(), color="Black" )
g_bar + scale_fill_gradient(low="black", high="red") +
  labs(title = "Total Terrestrial NEON sites by state")

g <- ggplot(data = state_sites_df, aes(x = State, y = Aquatic, fill = Aquatic ))
g_bar <- g + geom_bar(stat="identity", position=position_dodge(), color="Black" )
g_bar + labs(title = "Total Aquatic NEON sites by state")


g <- ggplot(data = state_sites_df, aes(x = Terrestrial, y = Aquatic), position = "jitter")
g_pt <- g + geom_point()
g_pt + geom_smooth() +
  labs(title = "Correlation between Aquatic NEON sites and Terrestrial NEON sites")


bird_df = read.csv("NEON_count-landbird/brd_countdata.csv", header = TRUE)

bird_dom_spe <- select(bird_df, domainID, scientificName)
bird_dom_spe <- group_by(bird_dom_spe,domainID,scientificName)
br_spec_count <- summarize(bird_dom_spe,n_distinct(scientificName))

domain_ID_list <- list("D01", "D02", "D03", "D04", "D05", "D06","D07","D08","D09","D10","D11","D12","D13","D14","D15","D16","D17","D18","D19","D20")
# names(numberOfSpecie) <- c("D01", "D02", "D03", "D04", "D05", "D06","D07","D08","D09","D10","D11","D12","D13","D14","D15","D16","D17","D18","D19","D20")
species_count <- vector() # numeric types better be a vector!

# lets create a df to append to
specie_count_df <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("domainID","species_count")
colnames(specie_count_df) <- x
i <- 1
for(id in domain_ID_list) {
  domain_x <- subset(br_spec_count,domainID==id)
  species_count[[i]] <- nrow(domain_x) - 1    # each row specifies a distinct specie. We subtract a row to remove unspecified observations from the species list.
  specie_count_df <- rbind(species_count, id = nrow(domain_x) - 1 )
  i <- i + 1
}
specie_count_df <- t(specie_count_df)

specie_count_df <- subset(specie_count_df, select = c(species_count)) # remove the broken id column 

# Get values for the column to be added "domainID" 
domainID = c("D01", "D02", "D03", "D04", "D05", "D06","D07","D08","D09","D10","D11","D12","D13","D14","D15","D16","D17","D18","D19","D20")

#Combine "domainID" column with existed df 
specie_count_matrix <- cbind(domainID, specie_count_df)

# Rename "new" column name

# convert matrix to df 
specie_count_df <- as.data.frame(specie_count_matrix)

# The Y axsis is broken!
ggplot(data=specie_count_df, aes(x=domainID, y = species_count
)) +
  geom_bar(stat="identity") + labs(title = "Total unique species by domainID")

###################################################                 ############################################################################
###################################################    NUMBER 5     ############################################################################
################################################################################################################################################

brd_native_status_df <- select(bird_df, domainID, nativeStatusCode)

native <- subset(brd_native_status_df, nativeStatusCode == "N") # get all non native results
dom_01 <- subset(native,domainID=="D01")
nrow(dom_01[dom_01$nativeStatusCode=="N",])

invasive <- subset(brd_native_status_df, nativeStatusCode == "I") # get all non native/invasive results
dom_01Inva <- subset(invasive,domainID=="D02")
nrow(dom_01Inva[dom_01Inva$nativeStatusCode=="I",])

native_count <- vector() # numeric types better be a vector!

# lets create a df to append to
status_df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("species_count")
colnames(status_df) <- x

i <- 1
for(id in domain_ID_list) {
  domain_x <- subset(native,domainID==id)
  native_count[[i]] <- nrow(domain_x)
  status_df <- cbind(native_count) # domainID will be replaced......cbind is column bind btw
  i <- i + 1
}
dim(status_df)

invasive_count <- vector() # numeric types better be a vector!

# lets create a df to append to
invasive_df <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("species_count")
colnames(invasive_df) <- x

i <- 1
for(id in domain_ID_list) {
  domain_x <- subset(invasive,domainID==id)
  invasive_count[[i]] <- nrow(domain_x)
  invasive_df <- cbind(invasive_count) # domainID will be replaced......cbind is column bind btw
  i <- i + 1
}
dim(invasive_df)

# Get values for the column to be added "domainID" 
domainID = c("D01", "D02", "D03", "D04", "D05", "D06","D07","D08","D09","D10","D11","D12","D13","D14","D15","D16","D17","D18","D19","D20")


# bind "invasive_count" into status matrix
status_matrix <- cbind(invasive_count, status_df)
status_df <- as.data.frame(status_matrix)
#bind "domainID" column into status_matrix
status_matrix <- cbind(domainID, status_df)
# convert matrix to df 
status_df <- as.data.frame(status_matrix)

# ******************* the data frame is broken as it does not display the correct number for invasive 

###################################################                 ############################################################################
###################################################    NUMBER 6     ############################################################################
################################################################################################################################################

taxo_df <- select(bird_df,domainID, siteID, family) # has data associated with the presense of families in each site
pt_site_df <- select(df_site,SiteID,precipitation,temperature,Latitude) # has data associated with the site's prec, temp, lat
pt_site_df
taxo_df

# merg the two tables into one df
taxo_pt_df <- left_join(taxo_df,pt_site_df, by = c("siteID" = "SiteID"))
colnames(taxo_pt_df)

# It's hard to see which family has the largest extent but we can see a few do cover both far ends of the spectrumes,
# while the majority center around the centeral areas of the plots.

# taxo vs lat
g <- ggplot(data = taxo_pt_df, aes(x = Latitude, y = reorder(family, desc(family))  ))
g_pt <- g + geom_bin2d(colour = "black") + labs(title = "Mosquito Gene vs Latitude") +
  labs(title = "total count of bird family vs Latitude") + ylab("Bird family")

# taxo vs temp
g <- ggplot(data = taxo_pt_df, aes(x = temperature, y = reorder(family, desc(family))  ))
g_pt <- g + geom_bin2d(colour = "black")
g_pt + labs(title = "total count of bird family vs temperature") + ylab("Bird family")

# taxo vs preci
g <- ggplot(data = taxo_pt_df, aes(x = precipitation, y = reorder(family, desc(family))  ))
g_pt <- g + geom_bin2d(colour = "black") + ylab("Bird family")
g_pt + labs(title = "total count of bird family vs precipitation")

###################################################                 ############################################################################
###################################################     PART II     ############################################################################
#################################################################################################################################################################

mosq_df = read.csv("NEON_count-mosquitoes/mos_expertTaxonomistIDProcessed.csv", header = TRUE)
# b_sub_fam_df <- select(beetle_df,subfamily)

bird_fam <- select(bird_df, family)

taxo_df <- select(mosq_df,domainID, siteID, genus) # has data associated with the prescence of families in each site
geo_df <- select(df_site,SiteID,State,DomainName,precipitation,temperature,Latitude) # has data associated with the site's prec, temp, lat

taxo_df <- left_join(taxo_df,geo_df, by = c("siteID" = "SiteID"))
colnames(taxo_df)

# Genus vs DomainName
hm.palette <- colorRampPalette(rev(brewer.pal(9, 'YlOrRd')), space='Lab')
g <- ggplot(data = taxo_df, aes(x = genus, y = DomainName ) )
g_pt <- g +  geom_bin2d(color="black") 
g_color <- g_pt + scale_fill_gradientn(colours = hm.palette(100))
g_fixed_label <- g_color + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Discovered Mosquito Gene count for NEON Domain")

# Genus vs State
hm.palette <- colorRampPalette(rev(brewer.pal(9, 'YlOrRd')), space='Lab')
g <- ggplot(data = taxo_df, aes(x = genus, y = State ) )
g_pt <- g +  geom_bin2d(color="black") 
g_color <- g_pt + scale_fill_gradientn(colours = hm.palette(100))
g_fixed_label <- g_color + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g_fixed_label + labs(title = "Discovered Mosquito Gene count for each State")

# Genus vs Latitude
hm.palette <- colorRampPalette(rev(brewer.pal(9, 'YlOrRd')), space='Lab')
g <- ggplot(data = taxo_df, aes(x = genus, y = Latitude ) )
g_pt <- g +  geom_bin2d(color="black") 
g_color <- g_pt + scale_fill_gradientn(colours = hm.palette(100))
g_fixed_label <- g_color + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g_fixed_label + labs(title = "Mosquito Gene vs Latitude")