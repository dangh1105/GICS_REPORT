# index_performance.R
# Purpose: Calculate weekly, monthly, quarterly, yearly returns for IFRC indices
# Author: Your Name

# Note: You may need to define or load some custom functions like:
# - My.Kable(), My.Kable.All()
# - DBL_CCPR_READRDS(), TRAINEE_MERGE_COL_FROM_REF()
# - CALCULATE_CHANGE_RT_VARPC(), SYSDATETIME(), etc.

# Paste your actual R function code below
# =================================================================================
CALCULATE_INDEX_PERFORMANCE = function (pData = udata, pFolder = '',
                                        pFile   = '', EndDate  = '', 
                                        ToAddRef = T, Remove_MAXABSRT = T )  {
  # ------------------------------------------------------------------------------------------------
  #updated: 2024-06-04 18:55
  
  if (nrow(pData ) > 1 ) 
  { 
    x = pData
    if ( !'close_adj' %in% colnames(pData) ) { x [, close_adj := close]}
    x [is.na(close_adj), close_adj := close]
    if ( !'code' %in% colnames(pData) ) { x [, code := codesource]}
  } else {
    if (pFile == 'ifrc_ccpr_investment_history.rds') {
      investment = readRDS(pFolder, pFile) [order (-date)]
      ipo = readRDS(data.table())
      investment = rbind (investment, ipo, fill=  T)
      investment [, rt:=((close/shift(close))-1), by='code']
      investment [, ':=' (cur = NA, close_adj = close)]
      x = investment
    } else { 
      x = readRDS(pFolder, pFile)
      if ( !'close_adj' %in% colnames(x) ) { x [, close_adj := close]}
      if ( !'code' %in% colnames(x) ) { x [, code := codesource]}
    }
  }
  
  # STEP 1: READ FILE PRICES HISTORY
  # cleanse raw data
  x = unique(x, by = c('code', 'date'))
  x = x [!is.na(close_adj)]
  
  if ( nrow(x [substr(code,1,3) == 'STK']) == nrow (x)) { pType = 'STK' } else { 
    if ( nrow(x [substr(code,1,3) == 'STK']) == 0) {pType = 'OTHER'} else {pType = ''}
  }
  if (Remove_MAXABSRT) { 
    switch (pType,
            'STK' = { 
              ToExclude = unique (x [abs(rt) > STK_MAXABSRT ], by = c('code','date'))
              if (length (ToExclude) >0) { x = x [!ToExclude, on =.(code, date) ]}
            },
            'OTHER' = {
              # ToExclude = unique (x [abs(rt) > IND_MAXABSRT & !grepl('VIX',code)]$date)
              ToExclude = unique (x [abs(rt) > STK_MAXABSRT & !grepl('VIX',code)], by = c('code','date'))
              
              if (length (ToExclude) >0) { x = x [!ToExclude, on =.(code, date) ]}
            },
            {
              ToExclude = unique (x [abs(rt) > STK_MAXABSRT & !grepl('VIX',code)], by = c('code','date'))
              if (length (ToExclude) >0) { x = x [!ToExclude, on =.(code, date) ]}
            })
  } 
  # rm(pType)
  
  
  # if (grepl ('STK',pFile)) { x [rt < STK_MAXABSRT]} else { x [rt <= IND_MAXABSRT]}
  
  # STEP 2: RE-CALCULATE DATA
  # Limit data maxdate
  exclude = try (setDT (fread (data.table()) ) )
  if (all(class(exclude)!='try-error')) {
    Ind_History = x  [date <= EndDate] [! code %in% exclude [active == 1]$code] 
  }
  
  str(Ind_History)
  #Calculate necessary column: change, varpc, yyyy, yyyymm, yyyyqn
  # Ind_History = Ind_History[order(code, date)] 
  Ind_History = CALCULATE_CHANGE_RT_VARPC(Ind_History)
  if (all (! c('change', 'varpc') %in% names (Ind_History))) {
    Ind_History[, ':='(change=close_adj-shift(close_adj), varpc=100*(close_adj/shift(close_adj)-1)), by='code']
  }
  
  # if (pType != 'STK'){
  #   Ind_History = CHECK_LAST_CHANGE_ZERO(Ind_History)
  # }
  Ind_History[,":="(yyyy=year(date),yyyymm=year(date)*100+month(date),yyyyqn=year(date)*100+floor((month(date)-1)/3)+1),]
  
  #Calculate rtm
  Ind_Month = unique(Ind_History[order(code, -date)], by=c('code', 'yyyymm'))[order(code, date)]
  Ind_Month[, rtm:=(close_adj/shift(close_adj))-1, by='code']
  
  My.Kable(Ind_History[, .(code, date, close, yyyy, yyyymm, yyyyqn)])
  #Calculate rtq
  Ind_Quarter = unique(Ind_History[order(code, -date)], by=c('code', 'yyyyqn'))[order(code, date)]
  Ind_Quarter[, rtq:=(close_adj/shift(close_adj))-1, by='code']
  
  #Calculate rty
  Ind_Year  = unique(Ind_History[order(code, -date)], by=c('code', 'yyyy'))[order(code, date)]
  Ind_Year[, rty:=(close_adj/shift(close_adj))-1, by='code']
  
  # Calculate wtd 
  today = as.Date(EndDate)
  friday_this_week = today - wday(today) + 6  
  friday_last_week = friday_this_week - 7         
  date_this_week = Ind_History[date <= friday_this_week, .SD[.N], by = code][, .(code, date_this = date, close_this = close_adj)]
  date_last_week = Ind_History[date <= friday_last_week, .SD[.N], by = code][, .(code, date_last = date, close_last = close_adj)]
  wtd_data = merge(date_this_week, date_last_week, by = "code")
  wtd_data[, wtd := round(100 * (close_this / close_last - 1), 3)]
  #Merge data: month, quarter, year
  Ind_Overview = unique(Ind_History[order(code, -date)], by=c('code')) #[, .(code, name, date, last=close_adj, last_change, last_varpc)]
  Ind_Overview = merge(Ind_Overview, Ind_Month[, .(code, date, mtd=round(100*rtm,12))], all.x = T, by=c('code', 'date'))
  Ind_Overview = merge(Ind_Overview, Ind_Quarter[, .(code, date, qtd=round(100*rtq,12))], all.x = T, by=c('code', 'date'))
  Ind_Overview = merge(Ind_Overview, Ind_Year[, .(code, date, ytd=round(100*rty,12))], all.x = T, by=c('code', 'date'))
  Ind_Overview = merge(Ind_Overview, wtd_data[, .(code, wtd)], all.x = T, by = "code")
  My.Kable.All(Ind_Overview[,.(code, date, close, yyyy, yyyymm, yyyyqn, mtd, qtd, ytd)])
  
  
  Ind_Month6M = Ind_Month[order(code, -date)][, nr:=seq.int(1, .N), by='code'][nr<=6][, .(code, date, rtm, nr)]
  My.Kable(setDT(spread(Ind_Month6M[, .(code, nr = paste0('M',nr), rtm=round(100*rtm,12))], key='nr', value='rtm'))[order(-M1)])
  Res_Month = setDT(spread(Ind_Month6M[, .(code, nr = paste0('M',nr), rtm=round(100*rtm,12))], key='nr', value='rtm'))[order(-M1)]
  
  Ind_Quarter6Y = Ind_Quarter[order(code, -date)][, nr:=seq.int(1, .N), by='code'][nr<=6][, .(code, date, rtq, nr)]
  Res_Quarter   = setDT(spread(Ind_Quarter6Y[, .(code, nr = paste0('Q',nr), rtq=round(100*rtq,12))], key='nr', value='rtq'))[order(-Q1)]
  My.Kable(Res_Quarter)
  
  Ind_Year6Y = Ind_Year[order(code, -date)][, nr:=seq.int(1, .N), by='code'][nr<=6][, .(code, date, rty, nr)]
  Res_Year   = setDT(spread(Ind_Year6Y[, .(code, nr = paste0('Y',nr), rty=round(100*rty,12))], key='nr', value='rty'))[order(-Y1)]
  My.Kable(Res_Year)
  
  
  
  Ind_All = merge(Ind_Overview,Res_Month, all.x = T, by = 'code' )
  Ind_All = merge(Ind_All,Res_Quarter, all.x = T, by = 'code' )
  Ind_All = merge(Ind_All,Res_Year, all.x = T, by = 'code' )
  My.Kable(Ind_All)
  
  # my_add   =  ins_ref [ code %in% Ind_All$code]
  # Ind_Data = merge (Ind_All, my_add [,.(code,  iso2, country, continent)], all.x = T, by = 'code')
  if (ToAddRef) {
    Ind_Data = TRAINEE_MERGE_COL_FROM_REF (pData = Ind_All, 
                                           List_Fields = c('name','cur','wgtg','prtr', 'category', 'size'))
  } else { Ind_Data = Ind_All}
  
  # if (!'name' %in% names (Ind_All)) {
  #   Ind_Data = merge (Ind_All, my_add [,.(code, name)], all.x = T, by = 'code')  
  # } else { Ind_Data = Ind_All}
  
  if (!'last' %in% names (Ind_Data)) {
    Ind_Data [, last:= close]
  }
  Ind_Data [, updated := SYS.TIME()]
  
  return(Ind_Data)
}

#Report============================================================================================
sec_ind = input data 
sec_ind = sec_ind[, CODE_CW := paste0("IND", CODE, "CWPRVND")]
x = sec_ind[level == 1]
y = sec_ind[level == 2]
list_level = c("10","15","20","25", "30", "35", "40", "45", "50", "55", "60")
Xlist = list()
for ( i in 1:length(list_level))
{
  # i = 4
  udata   = y %>% filter(substr(gics, 1, 2) == list_level[i])
  xdata   = udata %>% filter(nb_con == max(nb_con, na.rm = TRUE))
  Xlist[[i]] = xdata
}

data_level  = rbindlist(Xlist, fill = T)
data_get  = rbind(data_level, x, fill = T)[order(gics)]
data_get[, gics_new := ifelse(nchar(gics) > 2, as.numeric(substr(gics,1,2)) + 1, gics)]
data_get = data_get[order(gics_new)][,-c("gics_new")]
pdata = data.table()
dt_perf =  try(CALCULATE_INDEX_PERFORMANCE  (pData = pdata, pFolder = '', pFile   = '',
                                             EndDate  = SYSDATETIME(1),ToAddRef = F ) )
xdata = data_get$CODE_CW
x = dt_perf[code %in% xdata]
data_check = x[,.(code,date, wtd, mtd, qtd, ytd)]
data_test = setnames(data_check, "code", "CODE_CW")
data_final = merge(data_check, data_get[,.(gics_name, gics, CODE_CW)], by = "CODE_CW", all.x = T)
data_final[, gics_new := ifelse(nchar(gics) > 2, as.numeric(substring(gics,1,2)) + 1, gics)]
data_final = data_final[order(gics_new)][,-c("gics_new")]
data_final = data_final[,.(gics_level = gics, gics_name, CODE_CW, date, wtd, mtd = round(mtd,3), qtd = round(qtd,3), ytd = round(ytd,3))] 
data_level1 = data_final[nchar(gics_level) > 2]
data_all = data_level1[, gics_level := gsub(",", "", gics_level)]
data_all = data_all[order(-ytd)]
