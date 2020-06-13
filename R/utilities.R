basic_processing <- function(DT = NULL,
                             kg = keepgroup,
                             runlength = runlength,
                             look_forward = look_forward,
                             ...) {
  lookback <- (runlength - 1)
  DT[,.y := .numerator / .denominator]
  DT[.grpvar %chin% kg,flag := sign(.y - centre)]
  DT[flag != 0, rungroup := rleidv(flag), by = .grpvar
     ][flag != 0, cusum := cumsum(flag), by = list(.grpvar,rungroup)
       ][flag != 0, cusum_shift := shift(cusum,fill = cusum[.N], n = lookback, type = "lead"), by = .(.grpvar,rungroup)
         ][flag != 0, roll_centre := zoo::rollapply(.y, width = look_forward,
                                                    FUN = mean,
                                                    partial = TRUE,
                                                    align = "right"),
           by = list(.grpvar,rungroup)]
  
}



get_run_dates <- function(direct = direction,
                          DT = NULL,
                          target_vec = c("cusum_shift","cusum"),
                          compar_vec = flag_reset,
                          runlength = runlength,
                          ...) {
  flag_reset <- if (direct == "below") {
    runlength * -1
  } else {
    runlength
  }
  
  if (direct == "both") {
    res <- DT[abs(get(target_vec)) == abs(compar_vec),]
  } else {
    res <-  DT[get(target_vec) == compar_vec,]
    res[, .SD[1], by = .grpvar]
  }
}

get_runs_DT <- function(DT1 = NULL, #run_start
                        DT2 = NULL, # run_end
                        joinvar = ".grpvar",
                        instance = "first",
                        sdcols = c(".grpvar",".datecol","i..datecol","i.roll_centre"),
                        ... ){
  
  runs <- DT1[DT2, on = joinvar, mult = "first"][,.SD, .SDcols = sdcols][,.SD[1], by = joinvar][]
  setnames(runs,
           old = c(".datecol","i..datecol","i.roll_centre"),
           new = c("start_date","end_date","centre"))
  
}






# first pass
get_sustained <- function(DT1 = NULL,
                          DT2 = NULL, ...){
  
  sus <- get_runs_DT(DT1, DT2)
  sus <- sus[,c(".grpvar","centre","start_date","end_date"),]
  sus[,`:=`(run_type = 'sustained',
            rungroup = 1)][]
  return(sus)
  
}


#subsequent passes
get_sustained2 <- function(DT1 = NULL,
                           DT2 = NULL, ...){
  
  sus <- get_runs_DT(DT1, DT2)
  sus <- sus[,c(".grpvar","centre","start_date","i.date"),]
  setnames(sus,old = "i.date", new = "end_date")
  sus[,`:=`(run_type = 'sustained',
            rungroup = 1)][]
  return(sus)
  
}




#first pass
update_tempDT <- function(DT1 = NULL, # sustained
                          DT2 = NULL, # masterDT
                          joinvar = '.grpvar'
) {
  res <- DT1[DT2, on =  joinvar][.datecol >= start_date,][]
  res
}


#subsequent passes
update_tempDT2 <- function(DT1 = NULL, # sustained
                           DT2 = NULL, # masterDT
                           joinvar = '.grpvar'
) {
  res <- DT1[DT2, on =  joinvar][.datecol > end_date,][]
  res
}


get_process_centres <- function(DT,
                                look_forward = look_forward,
                                .numerator,
                                .denominator,
                                round_digits,
                                .grpvar,
                                plot_type,
                                ...) {
  
  temp_pc_rows <- DT[, utils::head(.SD, look_forward),  by = .grpvar]
  
  
  
  if (plot_type == "c") {
    temp_pc_rows[,centre := mean(.numerator, na.rm = TRUE), by = .grpvar]
  } else {
    
    temp_pc_rows[, centre := mean(.numerator / .denominator, na.rm = TRUE), by = .grpvar][]
    temp_pc_rows[, centre := round(centre,round_digits + 1), by = .grpvar][]
    
  }
  if (plot_type == "c") {
    
    temp_pc_rows[,std_dev := sqrt(centre), by = .grpvar]
    
  } else if (plot_type == 'p') {
    
    temp_pc_rows[,std_dev := sqrt(centre * (1 - centre) / .denominator) , by = .grpvar
                 ][,std_dev := round(std_dev, round_digits + 1)][]
    
  } else {
    
    temp_pc_rows[,std_dev := sqrt(centre  /  .denominator), by = .grpvar
                 ][,std_dev := round(std_dev, round_digits + 1)][]
  }
  
  
  temp_pc_rows[,`:=`(ucl = round(centre + 3 * std_dev,round_digits + 1),
                     uwl = round(centre + 2 * std_dev,round_digits + 1),
                     lwl = round(centre - 2 * std_dev,round_digits + 1),
                     lcl = round(centre - 3 * std_dev,round_digits + 1))][]
  
  
  
  temp_pc_rows[,lcl := data.table::fifelse(lcl < 0, 0,lcl)]
  temp_pc_rows[,lwl := data.table::fifelse(lwl < 0, 0,lwl)]
  
  temp_pc_rows[,`:=`(start_date = min(.datecol, na.rm = TRUE),
                     end_date = max(.datecol,na.rm = TRUE)), by = .grpvar]
  
  temp_pc_rows[,`:=`(run_type = "sustained", rungroup = 1), by = .grpvar]
  
}



