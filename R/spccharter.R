#' spccharter
#'
#' Finds all runs of desired  length occurring on desired side of centre line.
#' Can also find runs occurring on both sides of the line, though this is of
#' limited use in terms of quality improvement.
#' Re-bases centre line each time a run is discovered.
#' The rebasing uses the points in the run, and a number of future points to ensure robust limits.
#'
#' Facets and axis limits are handled by ggplot, though x-axis breaks can be
#' specified using the appropriate character string e.g. "3 months"
#'
#'
#' @param df  data.frame or data table
#' @param numerator numeric value representing the number of defect(s)
#' @param denominator sample size
#' @param datecol name of date column
#' @param by  a single unquoted  variable or character vector oflength 2
#' indicating desired grouping variable(s) . You must supply a grouping variable .
#' @param plot_type 'c', 'p' or 'u' chart
#' @param initial_rows number of points to calculate initial baseline mean
#' @param look_forward number of rows to rebase limits on, including those in the sustained run
#' @param runlength length of desired run. Less than 8 may not be statisticall significant
#' @param direction should run occur "above", "below" or on "both" sides of the mean line


#' @param round_digits the number of decimal places to round the p / u values and limits to

#' @param multiplier to express results as rate per 1000, 10000 or for percentages
#'
#' @param facet_cols how many columns are required in the plot facets
#' @param facet_scales defaults to "fixed". Alternatively, "free_y"
#' @param chart_title title for the  final chart
#' @param chart_subtitle subtitle for chart
#' @param chart_caption caption for chart
#' @param chart_breaks character string defining desired x-axis date breaks
#' @param line_colr colour for basic chart lines
#' @param line_size thickness of connecting lines between run chart points
#' @param point_colr colour for basic chart points
#' @param point_size size of normal run chart points
#' @param centre_colr colour for solid and extended mean  lines
#' @param centre_line_size thickness of solid and extended mean  lines
#' @param highlight_fill fill colour for highlighting points in a sustained run
#' @param cl_fill geom_ribbon fill for upper  and lower control limits
#' @param cl_colr optional line colour for upper and lower control limits
#' @param wl_fill geom_ribbon fill for upper  and lower warning limits
#' @param wl_colr optional line colour for upper and lower warning limits
#' @param overwrite_theme set to FALSE if you want to amend the final plot
#' afterwards, in which case it returns the default ggplot2 theme, gridlines
#' and date labels. Leave at TRUE for theme_minimal, no gridlines and rotated
#' date labels.
#' @param outputs return both plot and data, plot only, data only
#'
#' @param ...  further arguments passed on to function
#'
#' @return (faceted) plot plus detailed data.tables showing improvement data.
#' These outputs can be used for further processing in external tools
#'
#' @import data.table
#' @importFrom stats median
#' @importFrom zoo rollapply
#' @importFrom ggplot2 aes ggplot geom_line geom_point geom_segment geom_ribbon
#' @importFrom ggplot2 theme element_text element_blank labs
#' @importFrom ggplot2 ggtitle facet_wrap vars scale_x_date scale_y_continuous
#' @importFrom scales label_percent
#' @export
#'
#'@examples
#'\donttest{
#'spccharter(testdata, numerator = defects, 
#'denominator = possible, datecol =  report_month,
#' by = testgroup, plot_type = 'p', 
#' direction = 'both', initial_rows = 13, 
#' look_forward = 13, chart_breaks = '3 months')
#' 
#' spccharter(testdata, numerator = defects, 
#'denominator = possible, datecol =  report_month,
#' by = testgroup, plot_type = 'c', 
#' direction = 'both', initial_rows = 13, 
#' look_forward = 13, chart_breaks = '3 months')
#' 
#' }
#'
#'
#'
spccharter <- function(df,
                       numerator,
                       denominator = NULL,
                       datecol = NULL,
                       by,
                       plot_type = c("c","p","u"),
                       runlength = 8,
                       initial_rows = 25,
                       look_forward = 25,
                       direction = c("above","below","both"),
                       round_digits = 2,
                       multiplier = c(100,1000,10000),
                       facet_cols = NULL,
                       facet_scales = "fixed",
                       chart_title = NULL,
                       chart_subtitle = NULL,
                       chart_caption = NULL,
                       chart_breaks = NULL,
                       line_colr =  "#005EB8",
                       line_size = 1.1,
                       point_colr = "#005EB8",
                       point_size = 2.7,
                       centre_colr = "#06425AFF",
                       centre_line_size = 1.05,
                       highlight_fill = "#B50A2AFF", #"#E48C2AFF", #"#DB1884",
                       cl_fill = "grey80", #"#67B9E9FF", #"#86C2DAFF",
                       cl_colr = NULL,
                       wl_fill = "grey90", #"#B7D9F2FF", #"#C0DDE1FF",
                       wl_colr = NULL,
                       overwrite_theme = TRUE,
                       outputs = c("both","plot","data"),
                       ...) {
  
  # initialise some variables
  
  start_date <- end_date  <-  NULL
  keepgroup <- NULL
  .numerator <- .denominator <- .datecol <- NULL
  ..numerator <- ..denominator <- ..datecol <- NULL
  .y <- flag <- rungroup <- ucl <- uwl <- lwl <- lcl <-  NULL
  med_lookup <- .extended <- cols_to_round <- extend_to <- extend_to2 <-  NULL
  
  # sdcols <- NULL
  
  
  flag_reset <- if (direction == "below") {
    runlength * -1
  } else {
    runlength
  }
  
  
  numerator <- deparse(substitute(numerator))
  
  denominator <- deparse(substitute(denominator))
  
  datecol <- deparse(substitute(datecol))
  
  temp_by <- deparse(substitute(by))
  
  if (length(temp_by == 1)) {
    by <- (as.character(substitute(by)))
  }
  
  if (length(temp_by > 1)) {
    by <- by[which(by != 'c')]
  }
  
  
  
  stopifnot(!is.null(numerator),
            !is.null(datecol),
            !is.null(df),
            !is.null(by),
            length(direction) == 1
  )
  
  plot_type <- match.arg(plot_type) #default to c chart
  .outputs <- match.arg(outputs) #default to c chart
  
  if (plot_type == 'u' &  length(multiplier) > 1) {
    stop('"Please specify the multiplier value for the u chart"')
  }
  
  
  
  .originalDT <- data.table::copy(df)
  data.table::setDT(.originalDT)
  
  
  .originalDT[,`:=`(.numerator = as.numeric(get(..numerator)), .datecol = get(..datecol))][]
  
  
  if (!is.null(by) & length(by) < 2) {
    .originalDT[,.facet1 := get(by)]
    .originalDT[,.fac_grp := get(by)][]
  }
  
  if (!is.null(by) & length(by) >= 2) {
    .originalDT[,.facet1 := get(by[1])]
    .originalDT[,.facet2 := get(by[2])]
    .originalDT[,.fac_grp := paste(.facet1,.facet2,sep = '_')][]
  }
  
  
  # is grpvar a factor
  factorcheck <- lapply(Filter(is.factor,.originalDT), levels)
  
  if (length(factorcheck)) {
    keeplevels <- levels(by)
  }
  
  
  if (plot_type == 'c') {
    .originalDT[,.denominator := 1L]
  } else if (!is.null(denominator)) {
    .originalDT[, .denominator := as.numeric(get(..denominator))]
  }
  
  
  
  
  keycols_temp <- by
  keycols <- c(keycols_temp,'.datecol')
  
  data.table::setkeyv(.originalDT, keycols)
  
  .originalDT <- .originalDT[!is.na(.datecol),]
  
  keepgroup <- .originalDT[,.N, by = by]
  
  lines_only <- keepgroup[N > 1,]
  lines_only <- unique(lines_only[[1]])
  lines_only <- unique(keepgroup[[1]])
  
  keeptest <- keepgroup[]
  keeptest[, compar := (initial_rows + runlength)][]
  keeptest[, result := (N >= compar)][]
  
  
  if (all(keeptest[["result"]] == FALSE)) {
    stop("None of the groups have enough rows of data beyond the specified baseline period, for the desired runlength.
        Please check the values of the initial_rows and runlength arguments.
        Currently they exceed the number of rows for each group")
  } else {
    keepgroup <-  .originalDT[,.N, by
                           ][N >= (initial_rows + runlength),.SD, .SDcols = "N", by]}
  
  if (all(keepgroup$N) == 0) {
    stop("None of the groups have enough rows of data beyond the specified baseline period, for the desired runlength.
        Please check the values of the initial_rows and runlength arguments.
        Currently they exceed the number of rows for each group")
  }
  
  
  process_centre_rows <- .originalDT[, utils::head(.SD, initial_rows),  by = by]
  
  if (plot_type == "c") {
    process_centre_rows[,centre := mean(.numerator,na.rm = TRUE), by = by]
  } else {
    
    process_centre_rows[,centre := mean(.numerator / .denominator, na.rm = TRUE), by = by][]
    process_centre_rows[, centre := signif(centre,round_digits + 2), by = by][]
    
  }
  
  
  #calculate standard deviation
  
  if (plot_type == "c") {
    
    process_centre_rows[,std_dev := sqrt(centre), by = by]
    
  } else if (plot_type == 'p') {
    
    process_centre_rows[,std_dev := sqrt(centre * (1 - centre) / .denominator) , by = by
                        ][,std_dev := signif(std_dev, round_digits + 2)][]
    
  } else {
    
    process_centre_rows[,std_dev := sqrt(centre  /  .denominator), by = by
                        ][,std_dev := signif(std_dev, round_digits + 2)][]
  }
  
  
  process_centre_rows[,`:=`(ucl = signif(centre + 3 * std_dev,round_digits + 2),
                            uwl = signif(centre + 2 * std_dev,round_digits + 2),
                            lwl = signif(centre - 2 * std_dev,round_digits + 2),
                            lcl = signif(centre - 3 * std_dev,round_digits + 2))][]
  
  process_centre_rows[,lcl := data.table::fifelse(lcl < 0, 0, lcl)]
  process_centre_rows[,lwl := data.table::fifelse(lwl < 0, 0, lwl)]
  
  process_centre_rows[,`:=`(start_date = min(.datecol, na.rm = TRUE),
                            end_date = max(.datecol,na.rm = TRUE)), by = by]
  
  centres <- process_centre_rows[,`:=`(run_type = "baseline", rungroup = 1)]
  
  
  med_lookup <- centres[,.SD, .SDcols = c('centre', 'end_date'), by]
  
  
  
  tempDT <- med_lookup[.originalDT, on = by, mult = 'first'][.datecol > end_date,.SD, by = by][]
  
  
  
  
  tempDT <- tempDT[,end_date := NULL][]
  
  
  
  # first time processing
  tempDT <- basic_processing(DT = tempDT, kg = keepgroup,runlength, look_forward, by = by)
  tempDT[,roll_centre := signif(roll_centre,round_digits + 2)][]
  run_start <- get_run_dates(direction,DT = tempDT, target_vec = "cusum_shift",
                             compar_vec = flag_reset, runlength, by = by)
  
  keepgroup <- run_start[,.N,by]
  keepgroup <- unique(keepgroup[[1]])
  
  if (length(keepgroup)) {
    
    run_end <- get_run_dates(direction,DT = tempDT, target_vec = "cusum",
                             compar_vec = flag_reset, runlength, by = by)
    
    
    sustained <- get_sustained(DT1 = run_start,
                               DT2 = run_end, by = by)
    
    setkeyv(sustained,keycols_temp)
    setkeyv(.originalDT,keycols_temp)
    tempDT <- update_tempDT(sustained,.originalDT)
    
    
    
    temp_centres <- get_process_centres(tempDT, look_forward, numerator,
                                        denominator, round_digits,
                                        by = by,
                                        plot_type = plot_type)
    
    tmpctrs <- temp_centres[,unique(.SD),.SDcols = c("centre"), by = by]
    sustained[, centre := NULL]
    sustained <- tmpctrs[sustained, on = keycols_temp]
    
    bindlist <- if (!exists("bindlist")) {
      bindlist <- list(centres, temp_centres)
    } else {
      bindlist <- c(bindlist,temp_centres)
    }
    
    centres <- data.table::rbindlist(bindlist, use.names = TRUE, fill = TRUE)
    
    
    med_lookup <-  sustained[,.SD, .SDcols = c('centre','end_date'), by]
    
    data.table::setkeyv(.originalDT, keycols)
    tempDT <- med_lookup[.originalDT, on = keycols_temp, mult = 'last'][.datecol > end_date,][]
    
    tempDT <- tempDT[,end_date := NULL][]
    
    
    # keepgroup <- tempDT[,.N,by][N >= (runlength),.SD,.SDcols = "N",by = by
    # ][,unique(by)]
    
    
    keepgroup <- tempDT[,.N, by = by]
    
    keepgroup <- keepgroup[N > 1,][,unique(keepgroup[[1]])]
    
    # if keepgroup > 0 , repeat, else
    
    while (length(keepgroup)) {
      tempDT <- basic_processing(DT = tempDT, kg = keepgroup, runlength, look_forward)
      run_start <- get_run_dates(direction, DT = tempDT, target_vec = "cusum_shift",
                                 compar_vec = flag_reset, runlength, by = by)
      keepgroup <- run_start[,.N,by]
      keepgroup <- keepgroup[N > 1,][,unique(keepgroup[[1]])]
      
      run_end <- get_run_dates(direction,DT = tempDT, target_vec = "cusum",
                               compar_vec = flag_reset, runlength, by = by)
      
      sustained <- get_runs_DT(DT1 = run_start, DT2 = run_end, by)
      sustained[,`:=`(run_type = 'sustained',rungroup = 1)][]
      
      setkeyv(sustained,keycols_temp)
      setkeyv(.originalDT,keycols_temp)
      
      tempDT <- update_tempDT(sustained,.originalDT)
      
      if (!length(keepgroup)) {break}
      temp_centres <- get_process_centres(tempDT, look_forward, numerator,
                                          denominator, round_digits,
                                          by = by,
                                          plot_type = plot_type)
      
      tmpctrs <- temp_centres[,unique(.SD),.SDcols = c("centre"), by = by]
      sustained[, centre := NULL]
      sustained <- tmpctrs[sustained, on = keycols_temp]
      
      
      
      bindlist <- list(centres,temp_centres)
      centres <- data.table::rbindlist(bindlist, use.names = TRUE, fill = TRUE)
      
      
      med_lookup <-  sustained[,.SD, .SDcols = c('centre','end_date'), by]
      
      data.table::setkeyv(.originalDT, keycols)
      tempDT <- med_lookup[.originalDT, on = keycols_temp, mult = 'last'][.datecol > end_date,][]
      
      tempDT <- tempDT[,end_date := NULL][]
      
    }
    
  }
  # modify the final results tables for plotting purposes
  
  centres[,extend_to := shift(start_date,type = "lead"), by = by]
  centres[,extend_to := ifelse(is.na(extend_to),
                               max(.originalDT[[".datecol"]]),extend_to), by = by]
  centre_rows <- centres[!is.na(end_date) & run_type == "baseline",]
  
  sustained_rows <- centres[!is.na(end_date) & run_type == "sustained",][]
  
  sus_obs <-  dim(sustained_rows)[1]
  
  if (sus_obs > 0L) {
  sustained_rows <- setDT(sustained_rows)[ ,.SD[which.max(start_date)],by = keycols][]
  sustained_rows[order(c(keycols_temp,'start_date'))]
  sustained_rows[,rungroup := NULL]
  sustained_rows[,rungroup := .GRP, by = c(keycols_temp,'start_date')][]
  sustained_rows <- sustained_rows[sustained_rows[, .I[start_date == max(start_date)], by = c('.datecol', keycols_temp)]$V1]
  }
  
  centres[order(.datecol),rungroup := .GRP, by = c(keycols_temp, .datecol, start_date)][]
  
  centres <- centres[centres[, .I[start_date == max(start_date)], by = c('.datecol', keycols_temp)]$V1]
  
  if (sus_obs > 0L) {
  
  highlights <-  sustained_rows[, utils::head(.SD, runlength),  by = c(keycols_temp, 'rungroup')]
  }
  
  # now we need to extend from the last improvement to the end of the data
  
  med_lookup <-  centres[,.SD, .SDcols = c('centre','end_date'), by]
  
  data.table::setkeyv(.originalDT, keycols)
  .extended <- med_lookup[.originalDT, on = keycols_temp,mult = 'last'][.datecol > end_date,][]
  
  if (dim(.extended)[1] > 0) {
    
    #calculate standard deviation
    
    
    if (plot_type == "c") {
      
      .extended[,std_dev := sqrt(centre), by = by]
      
    } else if (plot_type == 'p') {
      
      .extended[,std_dev := sqrt(centre * (1 - centre) / .denominator) , by = by
                ][,std_dev := signif(std_dev, round_digits + 2)][]
      
    } else {
      
      .extended[,std_dev := sqrt(centre  /  .denominator), by = by
                ][,std_dev := signif(std_dev, round_digits + 2)][]
    }
    
    
    .extended[,`:=`(ucl = signif(centre + 3 * std_dev,round_digits + 2),
                    uwl = signif(centre + 2 * std_dev,round_digits + 2),
                    lwl = signif(centre - 2 * std_dev,round_digits + 2),
                    lcl = signif(centre - 3 * std_dev,round_digits + 2))][]
    
    .extended[,lcl := data.table::fifelse(lcl < 0, 0, lcl)]
    .extended[,lwl := data.table::fifelse(lwl < 0, 0, lwl)]
    
    .extended[,`:=`(start_date = min(.datecol, na.rm = TRUE),
                    end_date = max(.datecol,na.rm = TRUE)), by = by]
    
    .extended[,`:=`(run_type = "extended", rungroup = .GRP), by = by]
    
    .extended[,`:=`(extend_to = end_date), by = by]
    
    bindlist <- list(centres,.extended)
    centres <- data.table::rbindlist(bindlist, use.names = TRUE, fill = TRUE)
    
  }
  
  
  susrowkeycols <- c(keycols_temp,'start_date','end_date')
  data.table::setkeyv(sustained_rows,susrowkeycols)
  
  
  .originalDT[,join_date := .datecol]
  centres[,join_date := .datecol]
  centres <- centres[centres[, .I[start_date == max(start_date)], by = c('.datecol', keycols_temp)]$V1]
  centres[,rungroup := .GRP, by = c('.fac_grp','start_date','end_date','centre')][]
  centres <- unique(centres)
  centres_extract <- centres[,.SD, .SDcols = c('join_date','centre'), by]
  
  
  keycols <- c(keycols_temp,'join_date')
  
  setkeyv(.originalDT,keycols)
  setkeyv(centres_extract,keycols)
  
  .originalDT <- centres_extract[.originalDT, roll = TRUE][]
  
  
  
  
  #  omit the factor level replacement for now
  #   if (length(factorcheck)) {
  #     .originalDT[,by := factor(by,levels = keeplevels,ordered = TRUE)]
  #     centres[,by := factor(by,levels = keeplevels,ordered = TRUE)]
  #     sustained_rows[,by := factor(by,levels = keeplevels,ordered = TRUE)]
  #     highlights[,by := factor(by,levels = keeplevels,ordered = TRUE)]
  #   }
  
  
  if (plot_type == "c") {
    
    .originalDT[,std_dev := sqrt(centre), by = by
       ][,std_dev := signif(std_dev, round_digits + 2)][]
    
  } else if (plot_type == 'p') {
    
    .originalDT[,std_dev := sqrt(centre * (1 - centre) / .denominator) , by = by
       ][,std_dev := signif(std_dev, round_digits + 2)][]
    
  } else {
    
    .originalDT[,std_dev := sqrt(centre  /  .denominator), by = by
       ][,std_dev := signif(std_dev, round_digits + 2)][]
  }
  
  
  .originalDT[,`:=`(ucl = signif(centre + 3 * std_dev,round_digits + 2),
           uwl = signif(centre + 2 * std_dev,round_digits + 2),
           lwl = signif(centre - 2 * std_dev,round_digits + 2),
           lcl = signif(centre - 3 * std_dev,round_digits + 2))][]
  
  
  
  .originalDT[,lcl := data.table::fifelse(lcl < 0, 0,lcl)][]
  .originalDT[,lwl := data.table::fifelse(lwl < 0, 0,lwl)][]

  if (plot_type == 'p') {
    .originalDT[,ucl := data.table::fifelse(ucl > 1, 1, ucl)]
    .originalDT[,uwl := data.table::fifelse(uwl > 1, 1, uwl)][]
  }
  
  
  .originalDT[, .y := (.numerator/.denominator)][]
  .originalDT[, .y := signif(.y,round_digits + 1)]
  
  
  if (.outputs != 'data') {
    
    # base plot
    
    spcchart <- ggplot2::ggplot(.originalDT,
                                ggplot2::aes(x = .datecol,
                                             y = .y,
                                             group = .fac_grp))
    
    
    
    # facet by one variable if required
    
    
    
    if (length(by) == 1) {
      spcchart <- spcchart + ggplot2::facet_wrap(vars(.facet1),
                                                 ncol = facet_cols,
                                                 scales = facet_scales)
    } else  {
      spcchart <- spcchart + ggplot2::facet_wrap(vars(.facet1, .facet2),
                                                 ncol = facet_cols,
                                                 scales = facet_scales)
    }
    
    
    
    # control limits with geom ribbon
    
    if (!is.null(cl_colr)) {
      spcchart <- spcchart +
        ggplot2::geom_ribbon(data = .originalDT,
                             aes(x = .datecol,
                                 ymin = lcl, ymax = ucl),
                             fill = cl_fill,
                             colour =  cl_colr,
                             size = centre_line_size)
    } else {
      
      spcchart <- spcchart +
        ggplot2::geom_ribbon(data = .originalDT,
                             aes(x = .datecol,
                                 ymin = lcl, ymax = ucl),
                             fill = cl_fill, size = centre_line_size)
    }
    
    if (!is.null(wl_colr)) {
      
      
      spcchart <- spcchart  +
        ggplot2::geom_ribbon(data = .originalDT,
                             aes(x = .datecol, ymin = lwl, ymax = uwl),
                             fill = wl_fill,
                             colour =  wl_colr,
                             size = centre_line_size)
    } else {
      
      spcchart <- spcchart  +
        ggplot2::geom_ribbon(data = .originalDT,
                             aes(x = .datecol, ymin = lwl, ymax = uwl),
                             fill = wl_fill,
                             size = centre_line_size)
    }
    
    
    
    
    # plot  points for all groups
    spcchart <- spcchart +
      ggplot2::geom_point(shape = 21 ,colour = point_colr,
                          fill = point_colr, size = point_size, na.rm = TRUE)
    
    #  amend plot theme elements unless user wants to over-write later
    if (overwrite_theme) {
      spcchart <- spcchart +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank()) +
        ggplot2::theme(legend.position = "bottom")
    }
    
    
    # exclude facets that don't have enough data points to require connecting lines
    
    # lines only
    
    lines_only <- .originalDT[.N > 1, .SD, by = by]
    lines_only[,.y := signif(.numerator/.denominator,round_digits + 1)]
    
    
    
    spcchart <- spcchart +
      ggplot2::geom_line(
        data = lines_only,
        na.rm = TRUE,
        ggplot2::aes(x = .datecol,
                     y = .y,
                     # group =  .facet1),
                     group =  .fac_grp),
        colour = line_colr,
        size = centre_line_size
      )
    
    # solid original mean line
    
    spcchart <- spcchart +
      ggplot2::geom_segment(data = centre_rows,na.rm = TRUE,
                            ggplot2::aes(x = start_date, xend = end_date,
                                         y = centre, yend = centre, group = rungroup),
                            colour = centre_colr,size = centre_line_size, linetype = 1)
    
    
    
    #  highlight sustained points
    if (sus_obs > 0L) {
      
      spcchart <- spcchart +
        ggplot2::geom_point(data = highlights,
                            ggplot2::aes(x = .datecol,
                                         y = signif((.numerator / .denominator),round_digits + 1),
                                         group = rungroup),
                            shape = 21,
                            colour = point_colr,
                            fill = highlight_fill,
                            size = point_size,
                            na.rm = TRUE)
    }
    
    # sustained mean  lines
    if (sus_obs > 0L) {
      spcchart <- spcchart +
        ggplot2::geom_segment(data = sustained_rows, na.rm = TRUE,
                              ggplot2::aes(x = start_date, xend = end_date,
                                           y = centre, yend = centre,
                                           group = rungroup), colour = centre_colr,
                              linetype = 1, size =  centre_line_size)
      
    }
    
    # title and captions
    spcchart <- spcchart  +
      ggplot2::ggtitle(label = chart_title, subtitle = chart_subtitle) +
      ggplot2::labs(x = "", y = "", caption = chart_caption)
    
    
    # extended baseline from last improvement date to next run or end
    spcchart <- spcchart  +
      ggplot2::geom_segment(data = centres, na.rm = TRUE,
                            ggplot2::aes(x = end_date,
                                         xend = extend_to,
                                         y = centre,
                                         yend = centre,
                                         group = rungroup),
                            colour = centre_colr,
                            linetype = 2,
                            size = 1.05)
    
    if (!is.null(chart_breaks)) {
      spcchart <- spcchart  + ggplot2::scale_x_date(breaks = chart_breaks)
    }
    
    if (plot_type == "p") {
      spcchart <- spcchart  + ggplot2::scale_y_continuous(labels = scales::label_percent())
    }
    
    if (plot_type == "u") {
      spcchart <- spcchart  + ggplot2::scale_y_continuous(labels = scales::label_percent(scale = multiplier, suffix = " "))
    }
  }
  
  
  if (.outputs == 'plot') {
    return(spcchart)
  }
  
  if (.outputs != 'plot') {
    # tidy up  - copy the results
    # otherwise the plot does not render due to internal ggplot2 checks
    
    return_centre <- copy(centre_rows)
    return_centre[,`:=`(.numerator = NULL, .denominator = NULL, .datecol = NULL)][]
    
    if (sus_obs > 0L) {
      return_sustained <- copy(sustained_rows)
      return_sustained[,`:=`(.numerator = NULL, .denominator = NULL, .datecol = NULL)][]
      
      setnames(return_sustained[,extend_to2 := extend_to][,extend_to := NULL],'extend_to2','extend_to')
    }
    
    remaining <- copy(.extended)
    remaining[,`:=`(.numerator = NULL, .denominator = NULL, .datecol = NULL)][]
    
    # round the output columns
    cols_to_round <- c('centre','std_dev','ucl','uwl','lwl', 'lcl')
    
    for (j in cols_to_round) {
      set(return_centre, i = NULL, j = j, value = signif(return_centre[[j]],round_digits))
    }
    
    if (dim(sustained_rows)[1] > 0L) {
      for (j in cols_to_round) {
        set(return_sustained, i = NULL, j = j, value = signif(return_sustained[[j]],round_digits))
      }
    }
    
    if (dim(remaining)[1] > 0L) {
      for (j in cols_to_round) {
        set(remaining, i = NULL, j = j, value = signif(remaining[[j]],round_digits))
      }
    }
    
  }
  
  
  if (.outputs == "data") {
    
    if (dim(sustained_rows)[1] > 0L) {
      results <- list( centres = centres,
                       centre_rows = return_centre,
                       sustained = return_sustained,
                       remaining = remaining)
    } else {
      
      results <- list( centres = centres,
                       centre_rows = return_centre,
                       remaining = remaining)
    }
    
    return(results)
    
    
  }
  
  
  
  
  
  
  if (dim(sustained_rows)[1] > 0L) {
    results <- list( spcchart = spcchart,
                     centres = centres,
                     centre_rows = return_centre,
                     sustained = return_sustained,
                     remaining = remaining)
  } else {
    
    results <- list( spcchart = spcchart,
                     centres = centres,
                     centre_rows = return_centre,
                     remaining = remaining)
  }
  
  return(results)
  
}
