# Add to standard datasets for cross-formatting
df.States$STFIPS_C   <- sprintf('%02d',df.States$STFIPS)
df.Counties$STFIPS_C <-  sprintf('%02d',df.Counties$STFIPS)
df.Counties$FIPS_C   <-  sprintf('%05d',df.Counties$FIPS)
df.Cities$STFIPS_C <-  sprintf('%02d',df.Cities$STFIPS)
df.Cities$FIPS_C   <-  sprintf('%05d',df.Cities$FIPS)


rownames(df.States) <- df.States$STFIPS_C
rownames(df.Counties) <- df.Counties$FIPS_C

save(df.States, file = file.path("Data","Input","Standard","df.States.RData"))
save(df.Counties, file = file.path("Data","Input","Standard","df.Counties.RData"))
save(df.Cities, file = file.path("Data","Input","Standard","df.Cities.RData"))