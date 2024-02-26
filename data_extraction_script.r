#____________________________________________________________________________________________________________

  library(RODBC)
  
  
  dat.pth <- "C:/GitRep/WPEA-data-visuals/Data/"
  
  
  myConn <- odbcDriverConnect("DSN=Visual FoxPro Database;UID=;PWD=;
                                  SourceDB=\\\\corp.spc.int\\shared\\FAME\\NC_NOU\\OFP\\db1\\tuna_dbs\\Log_dbs\\DBF\\logsheet.dbc;
                                  SourceType=DBC;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;")
  
  
  tp <- sqlTables(myConn) # to see all the tables in the observer database
  
  abest.tb <- sqlColumns(myConn, "a_best")
  
  dat <- sqlQuery(myConn, "SELECT * FROM
                  a_best WHERE flag_id IN ('ID', 'PH', 'VN')",
                  stringsAsFactors = FALSE)
  
  save(dat, file = paste0(dat.pth, "All_a-best_data.RData"))
  
  
#____________________________________________________________________________________________________________  
  
  library(RODBC)
  library(dplyr)
  
   dat.pth <- "C:/GitRep/WPEA-data-visuals/Data/"
  
   myConn <- odbcDriverConnect(connection="driver=SQL Server;server=nouSQL03;database=log_master;
                            Trusted_Connection=yes;")
  
  
  tp <- sqlTables(myConn) # to see all the tables in the observer database
  
  lf_master_tb <- sqlColumns(myConn, "len.LF_Master")
  
  dat_lf <- sqlQuery(myConn, "SELECT * FROM
                  len.LF_Master WHERE
                  FLAG_ID IN ('ID', 'PH', 'VN')",
                  stringsAsFactors = FALSE)
  
  
  dat_lf <- dat_lf %>% group_by(YR, GR, FLAG_ID, FLEET_ID, SP_ID, LEN) %>% summarise(FREQ = sum(FREQ))
  
  
  save(dat_lf, file = paste0(dat.pth, "All_lf-master-wpea_data.RData"))
  
  
  wf_master_tb <- sqlColumns(myConn, "wt.wt_Master")
  
  
  dat_wf <- sqlQuery(myConn, "SELECT * FROM
                  wt.wt_Master WHERE
                  FLAG_ID IN ('ID', 'PH', 'VN')",
                     stringsAsFactors = FALSE)
  
  dat_wf <- dat_wf %>% group_by(YR, GR, FLAG_ID, FLEET_ID, SP_ID, WT) %>% summarise(FREQ = sum(FREQ))
  
  save(dat_wf, file = paste0(dat.pth, "All_wf-master-wpea_data.RData"))
  
#____________________________________________________________________________________________________________

  library(RODBC)
  
  
  dat.pth <- "C:/GitRep/WPEA-data-visuals/Data/"
  
  
  myConn <- odbcDriverConnect(connection="driver=SQL Server;server=nouSQL03;database=log_master;
                            Trusted_Connection=yes;")
  
  
  tp <- sqlTables(myConn) # to see all the tables in the observer database
  
  abest.tb <- sqlColumns(myConn, "A_BEST_AGG")
  abest_cat.tb <- sqlColumns(myConn, "A_BEST_AGG_CATCH")
  tmp <- sqlColumns(myConn, "A_ACE_CATCH")
  
  
  dat1 <- sqlQuery(myConn, "SELECT *
		                       FROM	best.A_BEST_AGG",
                   stringsAsFactors = FALSE)
  
  dat2 <- sqlQuery(myConn, "SELECT *
		                       FROM	best.A_BEST_AGG_CATCH",
                   stringsAsFactors = FALSE)
  
  
  dat <- sqlQuery(myConn, "SELECT *
		                       FROM	best.A_BEST_AGG ab
                           left outer join best.A_BEST_AGG_CATCH ac on ab.A_BEST_ID = ac.A_BEST_ID",
                  stringsAsFactors = FALSE)
  
  
#____________________________________________________________________________________________________________  
  
  
  