basic_processing <- function(DT = NULL,
                             kg = keepgroup,
                             runlength = runlength,
                             look_forward = look_forward,
                             by = by,
                             ...) {
  lookback <- (runlength - 1L)
  DT[,.y := .numerator / .denominator,]
  DT[,flag := sign(.y - centre), by]
  DT[flag != 0, rungroup := rleidv(flag)][flag != 0, cusum := cumsum(flag), by = rungroup
                                          ][flag != 0, cusum_shift := shift(cusum,fill = cusum[.N], n = lookback, type = "lead"), by = list(rungroup)
                                            ][flag != 0, roll_centre := zoo::rollapply(.y, width = look_forward,
                                                                                       FUN = mean,
                                                                                       partial = TRUE,
                                                                                       align = "right"),
                                              by = list(rungroup)]
  
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
    res[, .SD[1], by = by]
  }
}

get_runs_DT <- function(DT1 = NULL, #run_start
                        DT2 = NULL, # run_end
                        by = by,
                        sdcols = c(".datecol","i..datecol","i.roll_centre"),
                        ... ){
  
  runs <- DT1[DT2, on = by, mult = "first"][,.SD, .SDcols = sdcols,by][,.SD[1], by = by][]
  setnames(runs,
           old = c(".datecol","i..datecol","i.roll_centre"),
           new = c("start_date","end_date","centre"))
  
}






# first pass
get_sustained <- function(DT1 = NULL,
                          DT2 = NULL,
                          by = by,
                          ...){
  
  sus <- get_runs_DT(DT1, DT2, by = by)
  #sus[,.SD.,.SDcols = c("centre","start_date","end_date"),by = by]
  sus[,`:=`(run_type = 'sustained',
            rungroup = 1), by = by][]
  return(sus)
  
}




#first pass
update_tempDT <- function(DT1 = NULL, # sustained
                          DT2 = NULL, # masterDT
                          by = by) {
  res <- DT1[DT2, on = by][.datecol >= start_date,.SD, by = by][]
  res
}




get_process_centres <- function(DT,
                                look_forward = look_forward,
                                .numerator,
                                .denominator,
                                round_digits,
                                by,
                                plot_type,
                                ...) {
  
  temp_pc_rows <- DT[, utils::head(.SD, look_forward),  by = by]
  
  
  
  if (plot_type == "c") {
    temp_pc_rows[,centre := mean(.numerator, na.rm = TRUE), by = by]
  } else {
    
    temp_pc_rows[, centre := mean(.numerator / .denominator, na.rm = TRUE), by = by][]
    temp_pc_rows[, centre := signif(centre,round_digits + 2), by = by][]
    
  }
  if (plot_type == "c") {
    
    temp_pc_rows[,std_dev := sqrt(centre), by = by
                 ][,std_dev := signif(std_dev, round_digits + 2)][]
    
  } else if (plot_type == 'p') {
    
    temp_pc_rows[,std_dev := sqrt(centre * (1 - centre) / .denominator) , by = by
                 ][,std_dev := signif(std_dev, round_digits + 2)][]
    
  } else {
    
    temp_pc_rows[,std_dev := sqrt(centre  /  .denominator), by = by
                 ][,std_dev := signif(std_dev, round_digits + 2)][]
  }
  
  
  temp_pc_rows[,`:=`(ucl = signif(centre + 3 * std_dev,round_digits + 2),
                     uwl = signif(centre + 2 * std_dev,round_digits + 2),
                     lwl = signif(centre - 2 * std_dev,round_digits + 2),
                     lcl = signif(centre - 3 * std_dev,round_digits + 2))][]
  
  
  
  temp_pc_rows[,lcl := data.table::fifelse(lcl < 0, 0,lcl)]
  temp_pc_rows[,lwl := data.table::fifelse(lwl < 0, 0,lwl)]
  
  temp_pc_rows[,`:=`(start_date = min(.datecol, na.rm = TRUE),
                     end_date = max(.datecol,na.rm = TRUE)), by = by]
  
  temp_pc_rows[,`:=`(run_type = "sustained", rungroup = 1), by = by]
  
}



update_intermediate_rows <- function(DT,
                                     .numerator,
                                     .denominator,
                                     round_digits,
                                     by = by,
                                     plot_type) {
  
  # need to update these for plotting later
  if (plot_type == "c") {
    
    DT[,std_dev := sqrt(centre), by = by
       ][,std_dev := signif(std_dev, round_digits + 2)][]
    
  } else if (plot_type == 'p') {
    
    DT[,std_dev := sqrt(centre * (1 - centre) / .denominator) , by = by
       ][,std_dev := signif(std_dev, round_digits + 2)][]
    
  } else {
    
    DT[,std_dev := sqrt(centre  /  .denominator), by = by
       ][,std_dev := signif(std_dev, round_digits + 2)][]
  }
  
  
  DT[,`:=`(ucl = signif(centre + 3 * std_dev,round_digits + 2),
           uwl = signif(centre + 2 * std_dev,round_digits + 2),
           lwl = signif(centre - 2 * std_dev,round_digits + 2),
           lcl = signif(centre - 3 * std_dev,round_digits + 2))][]
  
  
  
  DT[,lcl := data.table::fifelse(lcl < 0, 0,lcl)][]
  DT[,lwl := data.table::fifelse(lwl < 0, 0,lwl)][]
  
  
}