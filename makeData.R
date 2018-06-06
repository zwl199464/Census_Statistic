library(acs)
library(devtools)
api.key.install("c69a35cc326304230b61737297d48214867c5b8e")
floridaCountyMarital <- acs.fetch(endyear = 2016,
                                  geography = geo.make(state = "FL", county = "*"),
                                  variable = acs.lookup(endyear = 2015,
                                                        table.number = "B12002",
                                                        dataset = "acs"),
                                  dataset = "acs",
                                  col.names = "pretty")

#devtools::use_data(floridaCountyMarital)
