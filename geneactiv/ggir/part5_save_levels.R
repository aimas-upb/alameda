library("arrow")

part5_save_levels <- function (datadir = c(), metadatadir = c(), f0 = c(), f1 = c(), 
                             params_sleep = c(), params_metrics = c(), params_247 = c(), 
                             params_phyact = c(), params_cleaning = c(), params_output = c(), 
                             params_general = c(), 
                             save_to_csv = FALSE, outfile_base = NULL, ...) 
{
  options(encoding = "UTF-8")
  Sys.setlocale("LC_TIME", "C")
  input = list(...)
  params = extract_params(params_sleep = params_sleep, params_metrics = params_metrics, 
                          params_247 = params_247, params_phyact = params_phyact, 
                          params_cleaning = params_cleaning, params_output = params_output, 
                          params_general = params_general, input = input, params2check = c("sleep", 
                                                                                           "metrics", "247", "phyact", "cleaning", "general", 
                                                                                           "output"))
  params_sleep = params$params_sleep
  params_metrics = params$params_metrics
  params_247 = params$params_247
  params_phyact = params$params_phyact
  params_cleaning = params$params_cleaning
  params_output = params$params_output
  params_general = params$params_general
  ms5.out = "/meta/ms5.out"
  if (!file.exists(paste(metadatadir, ms5.out, sep = ""))) {
    dir.create(file.path(metadatadir, ms5.out))
  }
  
  nightsummary = M = IMP = sib.cla.sum = c()
  fnames.ms3 = sort(dir(paste(metadatadir, "/meta/ms3.out", sep = "")))
  fnames.ms5 = sort(dir(paste(metadatadir, "/meta/ms5.out", sep = "")))
  sleeplogRDA = paste(metadatadir, "/meta/sleeplog.RData", sep = "")
  
  if (file.exists(sleeplogRDA) == TRUE) {
    sleeplog = logs_diaries = c()
    load(sleeplogRDA)
    if (length(logs_diaries) > 0) {
      if (is.list(logs_diaries)) {
        sleeplog = logs_diaries$sleeplog
      }
      else {
        sleeplog = logs_diaries
      }
    }
  }
  else {
    sleeplog = logs_diaries = c()
  }
  ffdone = fnames.ms5
  fnames.ms3 = sort(fnames.ms3)
  if (f1 > length(fnames.ms3)) 
    f1 = length(fnames.ms3)
  params_phyact[["boutdur.mvpa"]] = sort(params_phyact[["boutdur.mvpa"]], decreasing = TRUE)
  params_phyact[["boutdur.lig"]] = sort(params_phyact[["boutdur.lig"]], decreasing = TRUE)
  params_phyact[["boutdur.in"]] = sort(params_phyact[["boutdur.in"]], decreasing = TRUE)
  
  if (f0 > length(fnames.ms3)) 
    f0 = 1
  if (f1 == 0 | length(f1) == 0 | f1 > length(fnames.ms3)) 
    f1 = length(fnames.ms3)
  
  
  main_part5 = function(i, metadatadir = c(), f0 = c(), f1 = c(), 
                        params_sleep = c(), params_metrics = c(), params_247 = c(), 
                        params_phyact = c(), params_cleaning = c(), params_output = c(), 
                        params_general = c(), ms5.out, ms5.outraw, fnames.ms3, 
                        sleeplog, logs_diaries, extractfilenames, referencefnames, 
                        folderstructure, fullfilenames, foldernam) {
    fnames.ms1 = sort(dir(paste(metadatadir, "/meta/basic", sep = "")))
    fnames.ms2 = sort(dir(paste(metadatadir, "/meta/ms2.out", sep = "")))
    fnames.ms4 = sort(dir(paste(metadatadir, "/meta/ms4.out", sep = "")))
    ws3 = params_general[["windowsizes"]][1]
    
    di = 1
    DaCleanFile = c()
    
    # Create cumulative data frame for all levels from all files
    # The dataframe will actually contain the merger between the `ts` dataframe 
    # each `boutcount` DF and the overall `LEVELS` classification
    
    # 1. cols for: file ID, MRN, bout definition protocol (e.g. T5A5), and the light, medium and vigourous ACC metric levels
    out_df = data.frame(
                id = character(0),
                mrn = numeric(0),
                protocol.def = character(0),
                threshold.light = numeric(0),
                threshold.medium = numeric(0),
                threshold.vigorous = numeric(0)
              )
    
    # 2. add cols for time, ACC metric value, sib.detection, is.sleep, nonwear, activity.level
    out_df = cbind(out_df, data.frame(
                            timestamp = as.POSIXlt(character()),
                            timezone = character(0),
                            acc.metric.val = numeric(0),
                            sib.detection = numeric(0),
                            is.sleep = numeric(0),
                            is.nonwear = numeric(0),
                            activity.level = character(0)
            ))
    
    # 3. add cols for the general activity levels and each individual boutcount per selection threshold
    boutdur.mvpa.names = paste0("bout.mvpa.", params_phyact[["boutdur.mvpa"]])
    boutdur.mvpa.df = data.frame(matrix(ncol = length(params_phyact[["boutdur.mvpa"]]), nrow = 0))
    colnames(boutdur.mvpa.df) <- boutdur.mvpa.names
    boutdur.mvpa.df <- as.data.frame(lapply(boutdur.mvpa.df, as.numeric))
    out_df = cbind(out_df, boutdur.mvpa.df)
    
    boutdur.light.names = paste0("bout.light.", params_phyact[["boutdur.lig"]])
    boutdur.light.df = data.frame(matrix(ncol = length(params_phyact[["boutdur.lig"]]), nrow = 0))
    colnames(boutdur.light.df) <- boutdur.light.names
    boutdur.light.df <- as.data.frame(lapply(boutdur.light.df, as.numeric))
    out_df = cbind(out_df, boutdur.light.df)
    
    boutdur.in.names = paste0("bout.inactive.", params_phyact[["boutdur.in"]])
    boutdur.in.df = data.frame(matrix(ncol = length(params_phyact[["boutdur.in"]]), nrow = 0))
    colnames(boutdur.in.df) <- boutdur.in.names
    boutdur.in.df <- as.data.frame(lapply(boutdur.in.df, as.numeric))
    out_df = cbind(out_df, boutdur.in.df)
    
    
    if (length(params_cleaning[["data_cleaning_file"]]) > 0) {
      if (file.exists(params_cleaning[["data_cleaning_file"]])) 
        DaCleanFile = read.csv(params_cleaning[["data_cleaning_file"]])
    }
    if (length(ffdone) > 0) {
      if (length(which(ffdone == fnames.ms3[i])) > 0) {
        skip = 1
      }
      else {
        skip = 0
      }
    }
    else {
      skip = 0
    }
    
    if (params_general[["overwrite"]] == TRUE) 
      skip = 0
    selp = which(fnames.ms4 == fnames.ms3[i])
    if (length(selp) > 0) {
      if (file.exists(paste0(metadatadir, "/meta/ms4.out/", fnames.ms4[selp])) == FALSE) {
        skip = 1
      }
    }
    else {
      skip = 1
    }
    if (skip == 0) {
      selp = which(fnames.ms2 == fnames.ms3[i])
      load(file = paste0(metadatadir, "/meta/ms2.out/", fnames.ms2[selp]))
      selp = which(fnames.ms4 == fnames.ms3[i])
      load(file = paste0(metadatadir, "/meta/ms4.out/", fnames.ms4[selp]))
      summarysleep = nightsummary
      rm(nightsummary)
      
      idindex = which(summarysleep$filename == fnames.ms3[i])
      ID = summarysleep$ID[idindex[1]]
      MRN = NA
      
      if (is.na(as.numeric(ID))) {
        split_ID = strsplit(summarysleep$ID[idindex[1]], "-")
        MRN = as.integer(unlist(split_ID)[3])
      }
      
      ndays = nrow(summarysleep)
    
      di = 1
      fi = 1
      SPTE_end = c()
      
      if (length(idindex) > 0 & nrow(summarysleep) >= 1) {
        summarysleep_tmp = summarysleep
        selp = which(fnames.ms1 == paste0("meta_", fnames.ms3[i]))
        if (length(selp) != 1) {
          cat("Warning: Milestone data part 1 could not be retrieved")
        }
        load(paste0(metadatadir, "/meta/basic/", fnames.ms1[selp]))
        load(paste0(metadatadir, "/meta/ms3.out/", fnames.ms3[i]))
        ts = data.frame(time = IMP$metashort[, 1], 
                        ACC = IMP$metashort[, params_general[["acc.metric"]]] * 1000, 
                        guider = rep("unknown", nrow(IMP$metashort)), 
                        angle = as.numeric(as.matrix(IMP$metashort[, which(names(IMP$metashort) == "anglez")])))
        
        Nts = nrow(ts)
        if (length(which(names(IMP$metashort) == "anglez")) == 
            0) {
          cat("Warning: anglez not extracted. Please check that do.anglez == TRUE")
        }
        nonwear = IMP$rout[, 5]
        nonwear = rep(nonwear, each = (IMP$windowsizes[2]/IMP$windowsizes[1]))
        if (length(nonwear) > Nts) {
          nonwear = nonwear[1:Nts]
        }
        else if (length(nonwear) < Nts) {
          nonwear = c(nonwear, rep(0, (Nts - length(nonwear))))
        }
        ts$nonwear = 0
        ts$nonwear = nonwear
        lightpeak_available = "lightpeak" %in% colnames(M$metalong)
        if (lightpeak_available == TRUE) {
          luz = M$metalong$lightpeak
          if (length(params_247[["LUX_cal_constant"]]) > 
              0 & length(params_247[["LUX_cal_exponent"]]) > 
              0) {
            luz = params_247[["LUX_cal_constant"]] * 
              exp(params_247[["LUX_cal_exponent"]] * 
                    luz)
          }
          handle_luz_extremes = g.part5.handle_lux_extremes(luz)
          luz = handle_luz_extremes$lux
          correction_log = handle_luz_extremes$correction_log
          repeatvalues = function(x, windowsizes, Nts) {
            x = rep(x, each = (windowsizes[2]/windowsizes[1]))
            if (length(x) > Nts) {
              x = x[1:Nts]
            }
            else if (length(x) < Nts) {
              x = c(x, rep(0, (Nts - length(x))))
            }
            return(x)
          }
          luz = repeatvalues(x = luz, windowsizes = IMP$windowsizes, Nts)
          correction_log = repeatvalues(x = correction_log, 
                                        windowsizes = IMP$windowsizes, Nts)
          ts$lightpeak_imputationcode = ts$lightpeak = 0
          ts$lightpeak = luz
          ts$lightpeak_imputationcode = correction_log
        }
        rm(IMP, M, I)
        clock2numtime = function(x) {
          x2 = as.numeric(unlist(strsplit(x, ":")))/c(1, 
                                                      60, 3600)
          return(sum(x2))
        }
        Nepochsinhour = (60/ws3) * 60
        S = sib.cla.sum
        rm(sib.cla.sum)
        def = unique(S$definition)
        cut = which(S$fraction.night.invalid > 0.7 | 
                      S$nsib.periods == 0)
        if (length(cut) > 0) 
          S = S[-cut, ]
        if (params_general[["part5_agg2_60seconds"]] == TRUE) {
          ts_backup = ts
        }
        pko = which(summarysleep_tmp$sleeponset == 0 & 
                      summarysleep_tmp$wakeup == 0 & summarysleep_tmp$SptDuration == 0)
        if (length(pko) > 0) {
          summarysleep_tmp = summarysleep_tmp[-pko, ]
        }
        for (j in def) {
          ws3new = ws3
          if (params_general[["part5_agg2_60seconds"]] == TRUE) {
            ts = ts_backup
          }
          time_POSIX = iso8601chartime2POSIX(ts$time, 
                                             tz = params_general[["desiredtz"]])
          tempp = unclass(time_POSIX)
          if (is.na(tempp$sec[1]) == TRUE) {
            tempp = unclass(as.POSIXlt(ts$time, tz = params_general[["desiredtz"]]))
          }
          
          sec = tempp$sec
          min = tempp$min
          hour = tempp$hour
          if (params_general[["dayborder"]] == 0) {
            nightsi = which(sec == 0 & min == 0 & hour == 0)
          }
          else {
            nightsi = which(sec == 0 & min == (params_general[["dayborder"]] - 
                                                 floor(params_general[["dayborder"]])) * 
                              60 & hour == floor(params_general[["dayborder"]]))
          }
          
          summarysleep_tmp2 = summarysleep_tmp[which(summarysleep_tmp$sleepparam == j), ]
          S2 = S[S$definition == j, ]
          ts = g.part5.addsib(ts, ws3new, Nts, S2, params_general[["desiredtz"]], j, nightsi)
          summarysleep_tmp2 = g.part5.fixmissingnight(summarysleep_tmp2, 
                                                      sleeplog = sleeplog, ID)
          ts$diur = 0
          
          if (nrow(summarysleep_tmp2) > 0) {
            ts = g.part5.wakesleepwindows(ts, summarysleep_tmp2, 
                                          params_general[["desiredtz"]], nightsi, 
                                          sleeplog, ws3, Nts, ID, Nepochsinhour)
            
            if (length(nightsi) > 1) {
              ts = g.part5.addfirstwake(ts, summarysleep_tmp2, 
                                        nightsi, sleeplog, ID, Nepochsinhour, 
                                        Nts, SPTE_end, ws3)
            }
            if (params_general[["part5_agg2_60seconds"]] == TRUE) {
              ts$time_num = floor(as.numeric(iso8601chartime2POSIX(ts$time, 
                                                                   tz = params_general[["desiredtz"]]))/60) * 60
              if (lightpeak_available == TRUE) {
                ts = aggregate(ts[, c("ACC", "sibdetection", 
                                      "diur", "nonwear", "angle", "lightpeak", 
                                      "lightpeak_imputationcode")], by = list(ts$time_num), 
                               FUN = function(x) mean(x))
              }
              else {
                ts = aggregate(ts[, c("ACC", "sibdetection", 
                                      "diur", "nonwear", "angle")], by = list(ts$time_num), 
                               FUN = function(x) mean(x))
              }
              
              ts$sibdetection = round(ts$sibdetection)
              ts$diur = round(ts$diur)
              ts$nonwear = round(ts$nonwear)
              names(ts)[1] = "time"
              ts$time = as.POSIXlt(ts$time, origin = "1970-1-1", 
                                   tz = params_general[["desiredtz"]])
              ws3new = 60
              time_POSIX = ts$time
              tempp = unclass(time_POSIX)
              if (is.na(tempp$sec[1]) == TRUE) {
                tempp = unclass(as.POSIXlt(ts$time, 
                                           tz = params_general[["desiredtz"]]))
              }
              sec = tempp$sec
              min = tempp$min
              hour = tempp$hour
              if (params_general[["dayborder"]] == 0) {
                nightsi = which(sec == 0 & min == 0 & hour == 0)
              }
              else {
                nightsi = which(sec == 0 & min == (params_general[["dayborder"]] - 
                                                     floor(params_general[["dayborder"]])) * 
                                  60 & hour == floor(params_general[["dayborder"]]))
              }
              Nts = nrow(ts)
            }
            if ("angle" %in% colnames(ts)) {
              ts = ts[, -which(colnames(ts) == "angle")]
            }
            
            ts$window = 0
            for (TRLi in params_phyact[["threshold.lig"]]) {
              for (TRMi in params_phyact[["threshold.mod"]]) {
                for (TRVi in params_phyact[["threshold.vig"]]) {
                  levels = identify_levels(ts = ts, 
                                           TRLi = TRLi, TRMi = TRMi, TRVi = TRVi, 
                                           ws3 = ws3new, params_phyact = params_phyact)
                  LEVELS = levels$LEVELS
                  OLEVELS = levels$OLEVELS
                  Lnames = levels$Lnames
                  bc.mvpa = levels$bc.mvpa
                  bc.lig = levels$bc.lig
                  bc.in = levels$bc.in
                  ts = levels$ts
                  NNIGHTSSLEEP = length(unique(summarysleep_tmp2$calendar_date))
                  NNIGHTSACC = length(nightsi)
                  FM = which(diff(ts$diur) == -1)
                  
                  ## make output data frame 
                  df = data.frame(matrix(ncol = length(names(out_df)), nrow = length(ts$time)))
                  colnames(df) <- names(out_df)
                  
                  ## set the values from the `ts` dataframe
                  df$timestamp = ts$time
                  df$timezone = params_general[["desiredtz"]]
                  df$sib.detection = ts$sibdetection
                  df$is.sleep = ts$diur
                  df$is.nonwear = ts$nonwear
                  df$acc.metric.val = ts$ACC
                  
                  ## set the file id, protocol def and threshold values
                  df$id = ID
                  df$mrn = MRN
                  df$protocol.def = j
                  df$threshold.light = TRLi
                  df$threshold.medium = TRMi
                  df$threshold.vigorous = TRVi
                  
                  ## set the levels and boutcounts
                  df$activity.level = levels$Lnames[levels$LEVELS + 1]
                  
                  for (idx in 1:dim(bc.in)[1]) {
                    colname = paste0("bout.inactive.", params_phyact[["boutdur.in"]][idx])
                    df[colname] = bc.in[idx, ]
                  }
                  for (idx in 1:dim(bc.lig)[1]) {
                    colname = paste0("bout.light.", params_phyact[["boutdur.lig"]][idx])
                    df[colname] = bc.lig[idx, ]
                  }
                  for (idx in 1:dim(bc.mvpa)[1]) {
                    colname = paste0("bout.mvpa.", params_phyact[["boutdur.mvpa"]][idx])
                    df[colname] = bc.mvpa[idx, ]
                  }
                  
                  
                  ## rbind to cumulative out_df
                  if (dim(out_df)[1] == 0) {
                    out_df = data.frame(df)
                  }
                  else {
                    out_df = rbind(out_df, df)
                  }
                }
              }
            }
          }
        }
        
      }
    }
    
    return (out_df)
  }
  
  ## Here is the call to the main method
  activity_levels_overall <- data.frame()
  
  for (i in f0:f1) {
    cat(paste0(i, " "))
    
    tryCatch(
      expr = {
        levels_df <-  main_part5(i, metadatadir, f0, f1, params_sleep, 
                   params_metrics, params_247, params_phyact, params_cleaning, 
                   params_output, params_general, ms5.out, ms5.outraw, 
                   fnames.ms3, sleeplog, logs_diaries, extractfilenames, 
                   referencefnames, folderstructure, fullfilenames, 
                   foldername)
        
        if (dim(activity_levels_overall)[1] > 0) {
          activity_levels_overall = rbind(activity_levels_overall, levels_df)
        }
        else {
          activity_levels_overall = data.frame(levels_df)
        }
      },
      error = function(e) {
        print(paste0("Error processing file: ", fnames.ms3[i], ". Reason: "))
        print(e)
        print(paste0("Activity Levels analysis for: ", fnames.ms3[i], " will not be added to final results."))
      }
    )
  }
  
  # add a final date_processed column to inform on the date of processing in 
  # local time
  local_processing_time = Sys.time()
  tz_processing_time = as.POSIXlt(local_processing_time,
                                  tz=params_general[["desiredtz"]])
  processing_date_str = strftime(tz_processing_time, format = "%Y-%m-%d")
  activity_levels_overall$date_processed <- processing_date_str
  
  if (is.null(outfile_base)) {
    outfile_base = "activity_levels_overall"
  }
  
  # save(activity_levels_overall, file = file.path(metadatadir, "meta", "ms5.out", paste0(outfile_base, ".RData")))
  save(activity_levels_overall, file = file.path(metadatadir, paste0(outfile_base, ".RData")))
  
  if (save_to_csv) {
    # write.csv(activity_levels_overall,
    #           file.path(metadatadir, paste0(outfile_base, ".csv")),
    #           row.names = FALSE)
    
    # convert timestamp column to string
    activity_levels_overall$timestamp = strftime(activity_levels_overall$timestamp, 
                                                 format="%Y-%m-%d %H:%M:%S", 
                                                 tz = params_general[["desiredtz"]], 
                                                 usetz = FALSE)
    
    
    write_parquet(activity_levels_overall, 
                  file.path(metadatadir, 
                            paste0(outfile_base, ".parquet")))
    
  }
  
  # return (overall_df)
}
