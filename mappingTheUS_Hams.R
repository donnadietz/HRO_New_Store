#Joining 3 files for project
#https://cran.r-project.org/web/packages/usmap/vignettes/advanced-mapping.html

library(tidyverse)

library(usmap)
EN <- read_csv("EN.csv")  #FCC
#https://www.fcc.gov/uls/transactions/daily-weekly#fcc-uls-transaction-files-weekly  
#(l_amat/EN.dat)


ZipCounty <- read_csv("ZIP-COUNTY-FIPS_2017-06.csv")  #Kaggle
#https://www.kaggle.com/danofer/zipcodes-county-fips-crosswalk

EN <- EN %>% mutate(zip=substring(`'zip'`,2,6))

EN <- EN %>% mutate(zip2=as.numeric(zip))

mydata <- left_join(EN,ZipCounty, by=c("zip2"="ZIP"))

hamCount <- mydata %>% group_by(STCOUNTYFP) %>% count()

county_fips <- read_csv("county_fips_master.csv")
#https://github.com/kjhealy/fips-codes/blob/master/county_fips_master.csv




#library(usmap)
hamCount$fips <-hamCount$STCOUNTYFP
hamCount$countyFIPS <- as.numeric(hamCount$STCOUNTYFP)

hamCount2 <-left_join(county_fips, hamCount, by=c("fips"="countyFIPS"))

hamCount3 <- hamCount2 %>% mutate(n=replace_na(n,0))
#+
plot_usmap(regions=c("counties"), data=hamCount3, values="n")+theme(legend.position="right")+ 
  scale_fill_continuous(name="Hams per County", label=scales::comma)+
  labs(title="Amateur Radio Operators in the United States",
       subtitle="(California is REALLY messing up this graphic!)")
#+
hamCount4 <- hamCount3 %>% mutate(logdata = log(`n`+1))
plot_usmap(regions=c("counties"), data=hamCount4, values="logdata")+theme(legend.position="right")+ 
  scale_fill_continuous(name="ln(Hams per County)", label=scales::comma)+
  labs(title="log of (Amateur Radio Operators in the United States) ",
       subtitle="(Give the rest of us a chance, California!)")

#+
# NORTH EAST SECTION
#home <- tibble(lon=-77, lat=39)
#homeTrans<-usmap_transform(home)
#stores in NewCastle, Woodbridge, Salem
hroNE <- tibble(lon=c(-75.6,-77.3,-71.2), lat=c(39.7, 38.6, 42.8))
hroNE_Trans <- usmap_transform(hroNE)

plot_usmap(regions=c("counties"), 
           include=c("NJ","PA","DE", "MD","DC","VA","CT","NY","MA",
                     "WV","NH","VT","ME","OH","NC"), 
           data=hamCount, values="n")+ theme(legend.position="right")+ 
  scale_fill_continuous(name="Hams per County", label=scales::comma)+
  geom_point(data = hroNE_Trans, aes(x = lon.1, y = lat.1),
             color = "red", alpha = 0.8)+
  labs(title="Amateur Radio Operators in the Eastern United States",
       subtitle="HRO Stores: Salem-NH, New Castle-DE, Woodbridge-VA")

#+
#TEXAS ETC SECTION  #STORES in Denver, Phoenix, Plano
hroCO <- tibble(lon=c(-104.9,-112.2,-96.7), lat=c(39.7,33.6,33.0))
hroCO_Trans <- usmap_transform(hroCO)
plot_usmap(regions=c("counties"), 
           include=c("NM","CO","AZ","TX"), 
           na=0, data=hamCount3, values="n")+ theme(legend.position="right")+ 
  scale_fill_continuous(name="Hams per County", label=scales::comma)+
  geom_point(data = hroCO_Trans, aes(x = lon.1, y = lat.1),
             color = "red", alpha = 0.8)+
  labs(title="Amateur Radio Operators in Texas, Arizona, NM, CO",
       subtitle="HRO Stores: Denver-CO, Phoenix-AZ, Plano-TX")


#+
#Cali SECTION  #STORES in Anaheim, Oakland, SanDiego
hroCA <- tibble(lon=c(-117.9,-122.2,-117.1), lat=c(33.8,37.8,32.8))
hroCA_Trans <- usmap_transform(hroCA)
plot_usmap(regions=c("counties"), 
           include=c("CA"), 
           na=0, data=hamCount3, values="n")+ theme(legend.position="right")+ 
  scale_fill_continuous(name="Hams per County", label=scales::comma)+
  geom_point(data = hroCA_Trans, aes(x = lon.1, y = lat.1),
             color = "red", alpha = 0.8)+
  labs(title="Amateur Radio Operators in California",
       subtitle="HRO Stores: Oakland (San Fran), Anaheim (LA), San Diego")



#+
#Portland Oregon store and state
hroOR <- tibble(lon=c(-122.8), lat=c(45.4))
hroOR_Trans <- usmap_transform(hroOR)
plot_usmap(regions=c("counties"), 
           include=c("OR"), 
            data=hamCount3, values="n")+ theme(legend.position="right")+ 
  scale_fill_continuous(name="Hams per County", label=scales::comma)+
  geom_point(data = hroOR_Trans, aes(x = lon.1, y = lat.1),
             color = "red", alpha = 0.8)+
  labs(title="Amateur Radio Operators in Oregon", subtitle="HRO Store: Portland")



#+
#Milwaukee WI store
hroWI <- tibble(lon=c(-88.0), lat=c(43.1))
hroWI_Trans <- usmap_transform(hroWI)
plot_usmap(regions=c("counties"), 
           include=c("WI"), 
           data=hamCount3, values="n")+ theme(legend.position="right")+ 
  scale_fill_continuous(name="Hams per County", label=scales::comma)+
  geom_point(data = hroWI_Trans, aes(x = lon.1, y = lat.1),
             color = "red", alpha = 0.8)+
  labs(title="Amateur Radio Operators in Wisconsin",subtitle="HRO Store: Milwaukee")

#+
#Atlanta GA store
hroGA <- tibble(lon=c(-84.3), lat=c(33.9))
hroGA_Trans <- usmap_transform(hroGA)
plot_usmap(regions=c("counties"), 
           include=c("GA"), 
           data=hamCount3, values="n")+ theme(legend.position="right")+ 
  scale_fill_continuous(name="Hams per County", label=scales::comma)+
  geom_point(data = hroGA_Trans, aes(x = lon.1, y = lat.1),
             color = "red", alpha = 0.8)+
  labs(title="Amateur Radio Operators in Georgia",subtitle="HRO Store: Atlanta")

#knitr::spin("mappingTheUS_Hams.R",FALSE,format="Rmd")