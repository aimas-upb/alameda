library("arrow")

part4_save_sib <- function (datadir = c(), metadatadir = c(), f0 = f0, f1 = f1, 
          params_sleep = c(), params_metrics = c(), params_cleaning = c(), 
          params_output = c(), params_general = c(), ...) 
{
  input = list(...)
  params = extract_params(params_sleep = params_sleep, params_metrics = params_metrics, 
                          params_general = params_general, params_output = params_output, 
                          params_cleaning = params_cleaning, input = input, params2check = c("sleep", 
                                                                                             "metrics", "general", "output", "cleaning"))
  params_sleep = params$params_sleep
  params_metrics = params$params_metrics
  params_cleaning = params$params_cleaning
  params_output = params$params_output
  params_general = params$params_general
  if (exists("relyonsleeplog") == TRUE & exists("relyonguider") == FALSE) 
    relyonguider = params_sleep[["relyonsleeplog"]]
  nnpp = 40
  ms3.out = "/meta/ms3.out"
  if (file.exists(paste0(metadatadir, ms3.out))) {
  }
  else {
    cat("Warning: First run g.part3 (mode = 3) before running g.part4 (mode = 4)")
  }
  ms4.out = "/meta/ms4.out"
  if (file.exists(paste0(metadatadir, ms4.out))) {
  }
  else {
    dir.create(file.path(metadatadir, ms4.out))
  }
  meta.sleep.folder = paste0(metadatadir, "/meta/ms3.out")
  
  if (length(params_sleep[["loglocation"]]) > 0) {
    dolog = TRUE
  }
  else {
    dolog = FALSE
  }
  
  if (dolog == TRUE) {
    logs_diaries = g.loadlog(params_sleep[["loglocation"]], coln1 = params_sleep[["coln1"]], colid = params_sleep[["colid"]],
                             nnights = params_sleep[["nnights"]], sleeplogidnum= params_sleep[["sleeplogidnum"]],
                             sleeplogsep = params_sleep[["sleeplogsep"]], meta.sleep.folder = meta.sleep.folder, 
                             desiredtz = params_general[["desiredtz"]])
    sleeplog = logs_diaries$sleeplog
  }
  
  fnames = dir(meta.sleep.folder)
  if (f1 > length(fnames)) 
    f1 = length(fnames)
  if (f0 > length(fnames)) 
    f0 = 1
  if (f1 == 0 | length(f1) == 0 | f1 > length(fnames)) 
    f1 = length(fnames)
  cnt = 1
  idlabels = rep(0, nnpp)
  pagei = 1
  cnt67 = 1
  
  logdur = rep(0, length(fnames))
  ffdone = c()
  ms4.out = "/meta/ms4.out"
  fnames.ms4 = dir(paste0(metadatadir, ms4.out))
  fnames.ms4 = sort(fnames.ms4)
  ffdone = fnames.ms4
  fnames = sort(fnames)
  
  convertHRsinceprevMN2Clocktime = function(x) {
    if (x > 24) 
      x = x - 24
    HR = floor(x)
    MI = floor((x - HR) * 60)
    SE = round(((x - HR) - (MI/60)) * 3600)
    if (SE == 60) {
      MI = MI + 1
      SE = 0
    }
    if (MI == 60) {
      HR = HR + 1
      MI = 0
    }
    if (HR == 24) 
      HR = 0
    if (HR < 10) 
      HR = paste0("0", HR)
    if (MI < 10) 
      MI = paste0("0", MI)
    if (SE < 10) 
      SE = paste0("0", SE)
    return(paste0(HR, ":", MI, ":", SE))
  }
  if (length(params_cleaning[["data_cleaning_file"]]) > 0) {
    DaCleanFile = read.csv(params_cleaning[["data_cleaning_file"]])
  }
  
  ## Create an overall SPO dataframe that records the 
  ## SIB intervals relative to each "day" (day on which the sleep of a 
  ## person starts, goes over night and finishes with the wake up 
  ## on the next day)
  sib_overall = data.frame(
    id = numeric(0),
    nb = numeric(0), 
    start = numeric(0), 
    end = numeric(0), 
    is.sleep = numeric(0), 
    def = character(0),
    day.relative.to = character(0),
    timezone = character(0),
    sib.start = as.POSIXlt(character()),
    sib.end = as.POSIXlt(character())
  )

  for (i in f0:f1) {
    if (params_general[["overwrite"]] == TRUE) {
      skip = 0
    }
    else {
      skip = 0
      if (length(ffdone) > 0) {
        if (length(which(ffdone == fnames[i])) > 0) 
          skip = 1
      }
    }
    
    tryCatch(
      expr = {
        if (skip == 0) {
          cat(paste0(" ", i))
          
          
          sumi = 1
          ID = SPTE_end = SPTE_start = L5list = sib.cla.sum = longitudinal_axis = c()
          load(paste0(meta.sleep.folder, "/", fnames[i]))
          accid = c()
          if (length(ID) > 0) {
            if (!is.na(ID)) {
              accid = ID
            }
          }
          if (exists("SRI") == FALSE) 
            SRI = NA
          
          if (nrow(sib.cla.sum) != 0) {
            sib.cla.sum$sib.onset.time = iso8601chartime2POSIX(sib.cla.sum$sib.onset.time, 
                                                               tz = params_general[["desiredtz"]])
            sib.cla.sum$sib.end.time = iso8601chartime2POSIX(sib.cla.sum$sib.end.time, 
                                                             tz = params_general[["desiredtz"]])
            idwi = g.part4_extractid(params_general[["idloc"]], 
                                     fname = fnames[i], dolog, params_sleep[["sleeplogidnum"]], 
                                     sleeplog, accid = accid)
            accid = idwi$accid
            wi = idwi$matching_indices_sleeplog
            if (length(params_sleep[["nnights"]]) == 0) {
              nnightlist = 1:max(sib.cla.sum$night)
            }
            else {
              if (max(sib.cla.sum$night) < params_sleep[["nnights"]]) {
                nnightlist = 1:max(sib.cla.sum$night)
              }
              else {
                nnightlist = 1:params_sleep[["nnights"]]
              }
            }
            if (length(nnightlist) < length(wi)) {
              nnightlist = nnightlist[1:length(wi)]
            }
            nnights.list = nnightlist
            nnights.list = nnights.list[which(is.na(nnights.list) == 
                                                FALSE & nnights.list != 0)]
            if (params_cleaning[["excludefirstlast"]] == 
                TRUE & params_cleaning[["excludelast.part4"]] == 
                FALSE & params_cleaning[["excludefirst.part4"]] == FALSE) {
              if (length(nnights.list) >= 3) {
                nnights.list = nnights.list[2:(length(nnights.list) - 
                                                 1)]
              }
              else {
                nnights.list = c()
              }
            }
            else if (params_cleaning[["excludelast.part4"]] == 
                     FALSE & params_cleaning[["excludefirst.part4"]] == TRUE) {
              if (length(nnights.list) >= 2) {
                nnights.list = nnights.list[2:length(nnights.list)]
              }
              else {
                nnights.list = c()
              }
            }
            else if (params_cleaning[["excludelast.part4"]] == 
                     TRUE & params_cleaning[["excludefirst.part4"]] == FALSE) {
              if (length(nnights.list) >= 2) {
                nnights.list = nnights.list[1:(length(nnights.list) - 1)]
              }
              else {
                nnights.list = c()
              }
            }
            calendar_date = wdayname = rep("", length(nnights.list))
            daysleeper = rep(FALSE, length(nnights.list))
            nightj = 1
            guider.df = data.frame(matrix(NA, length(nnights.list), 
                                          5), stringsAsFactors = FALSE)
            names(guider.df) = c("ID", "night", "duration", 
                                 "sleeponset", "sleepwake")
            guider.df$night = nnights.list
            if (dolog == TRUE) {
              if (length(wi) > 0) {
                wi2 = wi[which(sleeplog$night[wi] %in% guider.df$night)]
                guider.df[which(guider.df$night %in% sleeplog$night[wi2]), 
                ] = sleeplog[wi2, ]
              }
            }
            for (j in nnights.list) {
              if (length(params_sleep[["def.noc.sleep"]]) == 
                  0 | length(SPTE_start) == 0) {
                guider = "notavailable"
                if (length(L5list) > 0) {
                  defaultSptOnset = L5list[j] - 6
                  defaultSptWake = L5list[j] + 6
                  guider = "L512"
                }
              }
              else if (length(params_sleep[["def.noc.sleep"]]) == 
                       1 | length(params_sleep[["loglocation"]]) != 
                       0 & length(SPTE_start) != 0) {
                defaultSptOnset = SPTE_start[j]
                defaultSptWake = SPTE_end[j]
                guider = "HDCZA"
                if (params_sleep[["sleepwindowType"]] == 
                    "TimeInBed" & params_general[["sensor.location"]] == 
                    "hip") {
                  guider = "HorAngle"
                }
                if (is.na(defaultSptOnset) == TRUE) {
                  availableestimate = which(is.na(SPTE_start) == FALSE)
                  cleaningcode = 6
                  if (length(availableestimate) > 0) {
                    defaultSptOnset = mean(SPTE_start[availableestimate])
                  }
                  else {
                    defaultSptOnset = L5list[j] - 6
                    guider = "L512"
                  }
                }
                if (is.na(defaultSptWake) == TRUE) {
                  availableestimate = which(is.na(SPTE_end) == 
                                              FALSE)
                  cleaningcode = 6
                  if (length(availableestimate) > 0) {
                    defaultSptWake = mean(SPTE_end[availableestimate])
                  }
                  else {
                    defaultSptWake = L5list[j] + 6
                    guider = "L512"
                  }
                }
              }
              else if (length(params_sleep[["def.noc.sleep"]]) == 2) {
                defaultSptOnset = params_sleep[["def.noc.sleep"]][1]
                defaultSptWake = params_sleep[["def.noc.sleep"]][2]
                guider = "setwindow"
              }
              if (defaultSptOnset >= 24) {
                defaultSptOnset = defaultSptOnset - 24
              }
              if (defaultSptWake >= 24) {
                defaultSptWake = defaultSptWake - 24
              }
              defaultdur = defaultSptWake - defaultSptOnset
              sleeplog_used = FALSE
              if (dolog == TRUE) {
                if (all(!is.na(guider.df[j, 4:5]))) {
                  sleeplog_used = TRUE
                }
              }
              if (sleeplog_used == FALSE) {
                guider.df[j, 1:5] = c(accid, j, defaultdur, 
                                      convertHRsinceprevMN2Clocktime(defaultSptOnset), 
                                      convertHRsinceprevMN2Clocktime(defaultSptWake))
                cleaningcode = 1
              }
              nightj = nightj + 1
              acc_available = TRUE
              spocum = data.frame(nb = numeric(0), start = numeric(0), 
                                  end = numeric(0), dur = numeric(0), def = character(0))
              spocumi = 1
              guider.df2 = guider.df[which(guider.df$night == j), ]
              tmp1 = as.character(guider.df2$sleeponset[1])
              tmp2 = unlist(strsplit(tmp1, ":"))
              SptOnset = as.numeric(tmp2[1]) + (as.numeric(tmp2[2])/60) + 
                (as.numeric(tmp2[3])/3600)
              tmp4 = as.character(guider.df2$sleepwake[1])
              tmp5 = unlist(strsplit(tmp4, ":"))
              SptWake = as.numeric(tmp5[1]) + (as.numeric(tmp5[2])/60) + 
                (as.numeric(tmp5[3])/3600)
              daysleeper[j] = FALSE
              if (is.na(SptOnset) == FALSE & is.na(SptWake) == 
                  FALSE & tmp1 != "" & tmp4 != "") {
                doubleDigitClocktime = function(x) {
                  x = unlist(strsplit(x, ":"))
                  xHR = as.numeric(x[1])
                  xMI = as.numeric(x[2])
                  xSE = as.numeric(x[3])
                  if (xHR < 10) 
                    xHR = paste0("0", xHR)
                  if (xMI < 10) 
                    xMI = paste0("0", xMI)
                  if (xSE < 10) 
                    xSE = paste0("0", xSE)
                  x = paste0(xHR, ":", xMI, ":", xSE)
                  return(x)
                }
                tmp1 = doubleDigitClocktime(tmp1)
                tmp4 = doubleDigitClocktime(tmp4)
                if (SptWake > 12 & SptOnset < 12) 
                  daysleeper[j] = TRUE
                if (SptWake > 12 & SptOnset > SptWake) 
                  daysleeper[j] = TRUE
                if (SptOnset < 12) 
                  SptOnset = SptOnset + 24
                if (SptWake <= 12) 
                  SptWake = SptWake + 24
                if (SptWake > 12 & SptWake < 18 & daysleeper[j] == TRUE) 
                  SptWake = SptWake + 24
                if (daysleeper[j] == TRUE) {
                  logdur[i] = SptOnset - SptWake
                }
                else {
                  logdur[i] = SptWake - SptOnset
                }
                if (sleeplog_used == TRUE) {
                  cleaningcode = 0
                  guider = "sleeplog"
                }
              }
              else {
                SptOnset = defaultSptOnset
                SptWake = defaultSptWake + 24
                logdur[i] = SptWake - SptOnset
                cleaningcode = 1
                sleeplog_used = FALSE
              }
              if (params_cleaning[["excludefirstlast"]] == 
                  FALSE) {
                if (daysleeper[j] == TRUE & j != max(nnights.list)) {
                  loaddays = 2
                }
                else {
                  loaddays = 1
                }
                if (daysleeper[j] == TRUE & j == max(nnights.list)) {
                  daysleeper[j] = FALSE
                  loaddays = 1
                  if (SptWake > 36) 
                    SptWake = 36
                  logdur[i] = SptWake - SptOnset
                }
              }
              else {
                if (daysleeper[j] == TRUE) {
                  loaddays = 2
                }
                else {
                  loaddays = 1
                }
              }
              dummyspo = data.frame(nb = numeric(1), start = numeric(1), 
                                    end = numeric(1), dur = numeric(1), def = character(1))
              dummyspo$nb[1] = 1
              spo_day = c()
              spo_day_exists = FALSE
              for (loaddaysi in 1:loaddays) {
                qq = sib.cla.sum
                sleepdet = qq[which(qq$night == (j + (loaddaysi - 1))), ]
                if (nrow(sleepdet) == 0) {
                  if (spocumi == 1) {
                    spocum = dummyspo
                  }
                  else {
                    spocum = rbind(spocum, dummyspo)
                  }
                  spocumi = spocumi + 1
                  cleaningcode = 3
                  acc_available = FALSE
                }
                else {
                  acc_available = TRUE
                }
                defs = unique(sleepdet$definition)
                for (k in defs) {
                  ki = which(sleepdet$definition == k)
                  sleepdet.t = sleepdet[ki, ]
                  if (loaddaysi == 1) 
                    remember_fraction_invalid_day1 = sleepdet.t$fraction.night.invalid[1]
                  nsp = length(unique(sleepdet.t$sib.period))
                  spo = data.frame(nb = numeric(nsp), start = numeric(nsp), 
                                   end = numeric(nsp), dur = numeric(nsp), 
                                   def = character(nsp))
                  if (nsp <= 1 & unique(sleepdet.t$sib.period)[1] == 
                      0) {
                    spo$nb[1] = 1
                    spo[1, c("start", "end", "dur")] = 0
                    spo$def[1] = k
                    if (daysleeper[j] == TRUE) {
                      tmpCmd = paste0("spo_day", k, "= c()")
                      eval(parse(text = tmpCmd))
                      spo_day_exists = TRUE
                    }
                  }
                  else {
                    DD = g.create.sp.mat(nsp, spo, sleepdet.t, 
                                         daysleep = daysleeper[j])
                    if (loaddaysi == 1) {
                      wdayname[j] = DD$wdayname
                      calendar_date[j] = DD$calendar_date
                    }
                    spo = DD$spo
                    reversetime2 = reversetime3 = c()
                    if (daysleeper[j] == TRUE) {
                      if (loaddaysi == 1) {
                        w1 = which(spo$end >= 18)
                        if (length(w1) > 0) {
                          spo = spo[w1, ]
                          if (nrow(spo) == 1) {
                            if (spo$start[1] <= 18) 
                              spo$start[1] = 18
                          }
                          else {
                            spo$start[which(spo$start <= 18)] = 18
                          }
                          tmpCmd = paste0("spo_day", k, 
                                          "= spo")
                          eval(parse(text = tmpCmd))
                          spo_day_exists = TRUE
                        }
                        else {
                          tmpCmd = paste0("spo_day", k, 
                                          "= c()")
                          eval(parse(text = tmpCmd))
                          spo_day_exists = TRUE
                        }
                      }
                      else if (loaddaysi == 2 & spo_day_exists == 
                               TRUE) {
                        w2 = which(spo$start < 18)
                        if (length(w2) > 0) {
                          spo = spo[w2, ]
                          if (ncol(spo) == 1) 
                            spo = t(spo)
                          if (nrow(spo) == 1) {
                            if (spo$end[1] > 18) 
                              spo$end[1] = 18
                          }
                          else {
                            spo$end[which(spo$end > 18)] = 18
                          }
                          spo[, c("start", "end")] = spo[, c("start", "end")] + 24
                          tmpCmd = paste0("spo_day2", k, "= spo")
                          eval(parse(text = tmpCmd))
                        }
                        else {
                          tmpCmd = paste0("spo_day2", k, "= c()")
                          eval(parse(text = tmpCmd))
                        }
                        name1 = paste0("spo_day", k)
                        name2 = paste0("spo_day2", k)
                        tmpCmd = paste0("spo = rbind(", name1, ",", name2, ")")
                        eval(parse(text = tmpCmd))
                      }
                    }
                    if (daysleeper[j] == TRUE) {
                      if (SptWake < 21 & SptWake > 12 & 
                          SptOnset > SptWake) {
                        SptWake = SptWake + 24
                      }
                    }
                    relyonguider_thisnight = FALSE
                    if (length(params_cleaning[["data_cleaning_file"]]) > 
                        0) {
                      if (length(which(DaCleanFile$relyonguider_part4 == 
                                       j & DaCleanFile$ID == accid)) > 
                          0) {
                        relyonguider_thisnight = TRUE
                      }
                    }
                    if (length(spo) == 0) {
                      spo = data.frame(nb = numeric(1), 
                                       start = numeric(1), end = numeric(1), 
                                       dur = numeric(1), def = character(1))
                      spo$nb[1] = 1
                      spo[1, 2:4] = 0
                      spo$def[1] = k
                    }
                    if (length(which(spo$start < SptWake & 
                                     spo$end > SptOnset)) == 0 | relyonguider_thisnight == 
                        TRUE) {
                      cleaningcode = 5
                      newlines = rbind(spo[1, ], spo[1, 
                      ])
                      newlines[1, 1:4] = c(nrow(spo) + 1, 
                                           SptOnset, SptOnset + 1/60, 1)
                      newlines[2, 1:4] = c(nrow(spo) + 1, 
                                           SptWake - 1/60, SptWake, 1)
                      spo = rbind(spo, newlines)
                      spo = spo[order(spo$start), ]
                      spo$nb = 1:nrow(spo)
                      relyonguider_thisnight = TRUE
                    }
                    for (evi in 1:nrow(spo)) {
                      if (spo$start[evi] < SptWake & spo$end[evi] > 
                          SptOnset) {
                        if (params_sleep[["sleepwindowType"]] == 
                            "TimeInBed") {
                          if (spo$end[evi] < SptWake & spo$start[evi] > 
                              SptOnset) {
                            spo$dur[evi] = 1
                          }
                        }
                        else {
                          spo$dur[evi] = 1
                        }
                        if (params_sleep[["relyonguider"]] == 
                            TRUE | relyonguider_thisnight == 
                            TRUE) {
                          if ((spo$start[evi] < SptWake & 
                               spo$end[evi] > SptWake) | (spo$start[evi] < 
                                                          SptWake & spo$end[evi] < spo$start[evi])) {
                            spo$end[evi] = SptWake
                          }
                          if ((spo$start[evi] < SptOnset & 
                               spo$end[evi] > SptOnset) | (spo$end[evi] > 
                                                           SptOnset & spo$end[evi] < spo$start[evi])) {
                            spo$start[evi] = SptOnset
                          }
                        }
                      }
                    }
                    if (daysleeper[j] == TRUE) {
                      reversetime2 = which(spo$start >= 
                                             36)
                      reversetime3 = which(spo$end >= 36)
                      if (length(reversetime2) > 0) 
                        spo$start[reversetime2] = spo$start[reversetime2] - 
                        24
                      if (length(reversetime3) > 0) 
                        spo$end[reversetime3] = spo$end[reversetime3] - 
                        24
                    }
                    spo$def = k
                    if (spocumi == 1) {
                      spocum = spo
                    }
                    else {
                      spocum = rbind(spocum, spo)
                    }
                    spocumi = spocumi + 1
                  }
                }
              }
              
              if (length(spocum) > 0) {
                NAvalues = which(is.na(spocum$def) == TRUE)
                if (length(NAvalues) > 0) {
                  spocum = spocum[-NAvalues, ]
                }
              }
              if (length(spocum) > 0 & class(spocum)[1] == 
                  "data.frame" & length(calendar_date) >= j) {
                if (nrow(spocum) > 1 & ncol(spocum) >= 5 & 
                    calendar_date[j] != "") {
                  undef = unique(spocum$def)
                  for (defi in undef) {
                    rowswithdefi = which(spocum$def == defi)
                    if (length(rowswithdefi) > 1) {
                      spocum.t = spocum[rowswithdefi, ]
                      correct01010pattern = function(x) {
                        x = as.numeric(x)
                        if (length(which(diff(x) == 1)) > 1) {
                          minone = which(diff(x) == -1) + 1
                          plusone = which(diff(x) == 1)
                          matchingvalue = which(minone %in% plusone == TRUE)
                          if (length(matchingvalue) > 0) 
                            x[minone[matchingvalue]] = 1
                        }
                        return(x)
                      }
                      delta_t1 = diff(as.numeric(spocum.t$end))
                      spocum.t$dur = correct01010pattern(spocum.t$dur)
                      spocum.t = spocum.t[!duplicated(spocum.t), ]
                      
                      spocum_nightj = data.frame(spocum.t)
                      names(spocum_nightj)[names(spocum_nightj) == "dur"] <- "is.sleep"
                      spocum_nightj$day.relative.to = chartime2iso8601(sib.cla.sum[which(sib.cla.sum$night == j), "start.time.day"][1], 
                                                                       tz = params_general[["desiredtz"]])
                      spocum_nightj$id = accid
                      spocum_nightj$timezone = params_general[["desiredtz"]]
                      
                      sleepdet_nightj = sleepdet.t[(which(sleepdet.t$night == j)), c("sib.period", "sib.onset.time", "sib.end.time")]
                      spocum_sleepdet_nightj = merge(spocum_nightj, sleepdet_nightj, by.x="nb", by.y="sib.period")
                      
                      spocum_nightj$sib.start = spocum_sleepdet_nightj$sib.onset.time
                      spocum_nightj$sib.end = spocum_sleepdet_nightj$sib.end.time
                      
                      sib_overall = rbind(sib_overall, spocum_nightj)
                    }
                  }
                }
              }
            }
          }
        }
      },
      
      error = function(e) {
        print(paste0("Error processing file: ", fnames.ms3[i], ". Reason: "))
        print(e)
        print(paste0("SIB analysis for: ", fnames.ms3[i], " will not be added to final results."))
      }
    )
  }
  
  # Save overall results
  local_processing_time = Sys.time()
  tz_processing_time = as.POSIXlt(local_processing_time,
                                  tz=params_general[["desiredtz"]])
  processing_date_str = strftime(tz_processing_time, format = "%Y-%m-%d")
  sib_overall$date_processed <- processing_date_str
  
  if (is.null(outfile_base)) {
    outfile_base = "sib_overall"
  }
  
  # return(sleep_levels_overall)
  save(sib_overall, file = file.path(metadatadir, paste0(outfile_base, ".RData")))
  
  sib_overall$sib.start = strftime(sib_overall$sib.start, 
                                               format="%Y-%m-%d %H:%M:%S", 
                                               tz = params_general[["desiredtz"]], 
                                               usetz = FALSE)
  sib_overall$sib.end = strftime(sib_overall$sib.end, 
                                   format="%Y-%m-%d %H:%M:%S", 
                                   tz = params_general[["desiredtz"]], 
                                   usetz = FALSE)
  
  write_parquet(sib_overall, 
                file.path(metadatadir, 
                          paste0(outfile_base, ".parquet")))
}
