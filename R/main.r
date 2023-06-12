#' @title Simulate ICU occupancy
#'
#' @description This function simulate ICU occupancy based on admission rate coefficients, number of beds ant fix or variable ICU bed approach.
#'
#' @param seed The seed number. FALSE if you don't want any seed parameter. A number if you want select a seed to reproduce the same outcome.
#' @param typedata Could be "synthetic" if you want a simulation of original data (admissionid, lengthofstay and urgency) or NA if ypu want to use your original data.
#' @param data It must be a data.frame contain an admissionid column, a lengthofstay column and a urgency column.
#' @param month They represent months over simulation is performed. Month are considered of 28 days.
#' @param nBedsStart It's the number of bed of the ICU. For variable ICU bed simulation this number varies over -0.125 and +0.125 of the selected one.
#' @param icutype It could be a "fix"bed ICU or a "variable"bed ICU.
#' @param nsimulation It's the number of simulation repeated.
#' @param election It's the relative frequency of election admission (value between 0 and 1). 1-election represents urgency admission.
#' @param cAR Cumulative admission ratio it's the hourly admission rate used in simulation. If you left NA it will be 0.20. cAR used in simulation it's 1.25 times of cAR selected considering ICU rejected patients.
#' @param coefHE It's a vector of four number which represent difference in admission rate thought daily solt time (6-12, 12-18, 18-24, 24-6) for elective patients. If yu left NA it would be c(1, 1, 1, 1). Sum must be 4.
#' @param coefHU It's a vector of four number which represent difference in admission rate thought daily solt time (6-12, 12-18, 18-24, 24-6) for urgent patients. If yu left NA it would be c(1, 1, 1, 1). Sum must be 4.
#' @param coefDE It's a vector of seven number which represent difference in admission rate thought days of the week for elective patients. If yu left NA it would be c(1, 1, 1, 1, 1, 1, 1). Sum must be 7.
#' @param coefDU It's a vector of seven number which represent difference in admission rate thought days of the week for urgent patients. If yu left NA it would be c(1, 1, 1, 1, 1, 1, 1). Sum must be 7.
#' @param coefME It's a vector of twelve number which represent difference in admission rate thought months of the year for elective patients. If yu left NA it would be c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1). Sum must be 12.
#' @param coefMU It's a vector of twelve number which represent difference in admission rate thought months of the year for urgent patients. If yu left NA it would be c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1). Sum must be 12.
#' @param coef It's a coefficient which permit to multiply all admission rate for a value simulating for example a disaster or an epidemic virus.
#' @param error Default=ult it's FALSE. If you decide TRUE you have to select a error.perc
#' @param error.perc It's the percentage value of the AR to simulate the admission rate from a normal distribution with mean the AR and sd the error.perc x AR.
#' @param losprediction "stochastic". Predictive algorithm it's not available in this version.
#' @param CO Represent the cut-off for variable model algorithm porbability to oper or close bed. Values could be between 0 and 1.
#' @param type Represent the type of return. It could be "analysis" for the analytic result or "result" for the origina return of the simulation
#'
#' @returns rejectedAdmission
#' @returns hoursRejectedAdmission
#' @returns timeToAdmissionRejected
#' @returns admittedAdmission
#' @returns bedMatrixList
#' @returns prob
#'
#' @returns plot1
#' @returns hourbedfreeday
#' @returns hourbedfreedaysd
#' @returns meanrejected
#' @returns sdrejected
#' @returns meanHourRejected
#' @returns sdHourRejected
#'
#' @export
#'
#' @examples
#' icubedsim.simulation(seed = 12345, month = 8, nBedsStart = 20, icutype = "fix", coefHE = c(1, 1.5, 1, 0.5), type = "analysis")
#'

icubedsim.simulation<-function(
                      seed = FALSE,
                      typedata = "synthetic", #or data frame contain a column admissionid and a column lenghtofstay
                      data = NULL,
                      month = 2,
                      nBedsStart = 40,
                      icutype = "fix", # Or variable
                      nsimulation = 2,
                      election = NA,
                      cAR = NA,
                      coefHE = NA,
                      coefHU = NA,
                      coefDE = NA,
                      coefDU = NA,
                      coefME = NA,
                      coefMU = NA,
                      coef = 1,
                      error = FALSE,
                      error.perc = NA,
                      losprediction = "stochastic", # stochastic Vs predictive
                      CO = 0.5,
                      type = "analysis" #or "result"
                      )
{

  ###admission rate function###
  admssionRateFunction<-function(month = month, election = election, cAR = cAR, coefHE = coefHE, coefHU = coefHU, coefDE = coefDE, coefDU = coefDU, coefME = coefME, coefMU = coefMU, coef = 1, error = FALSE, error.perc = NA){
    arrivingProb = c(election, 1-election)*cAR*1.25*coef
    # Inizio simulazione ore 6 di monday 1 Jan
    arrivingProbMatrix<-matrix(rep(arrivingProb, 4), ncol = 2, byrow = T)
    arrivingProbMatrix[,1]<-arrivingProbMatrix[,1]*coefHE
    arrivingProbMatrix[,2]<-arrivingProbMatrix[,2]*coefHU

    arrivingProbList = list()
    for(i in 1:12){
      arrivingProbMatrixH = arrivingProbMatrix
      arrivingProbMatrixH[,1]<-arrivingProbMatrixH[,1]*coefME[i]
      arrivingProbMatrixH[,2]<-arrivingProbMatrixH[,2]*coefMU[i]
      arrivingProbList[[i]] =  arrivingProbMatrixH
    }

    arrivingProbArrayList = list()
    for(i in 1:12){
      arrivingProbArrayList[[i]] <- array(arrivingProbList[[i]], c(4, 2, 7))
      arrivingProbArrayList[[i]][, 1,]<-arrivingProbArrayList[[i]][, 1,]*matrix(rep(coefDE, 4), ncol = 7, byrow = T)
      arrivingProbArrayList[[i]][, 2, ]<-arrivingProbArrayList[[i]][, 2, ]*matrix(rep(coefDU, 4), ncol = 7, byrow = T)
    }

    #Month of 28 days
    hour<-rep(c(rep(1,6), rep(2, 6), rep(3, 6), rep(4, 6)), 28)
    day<-rep(c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24), rep(6, 24), rep(7,24)), 4)

    dET<-c(rep(NA, length(hour)))
    dUT<-c(rep(NA, length(hour)))

    if(error%in% TRUE && error.perc > 0.00 && error.perc < 0.25){
      admissionRateMonth = list()
      for(m in 1:month){
        dET = c()
        dUT = c()
        if(m <= 12){
          for(i in 1:(28*24)){
            dET[i]<-rpois(1, lambda = rnorm(1, arrivingProbArrayList[[m]][hour[i], 1, day[i]], error.perc*arrivingProbArrayList[[m]][hour[i], 1, day[i]])) #rnorm add an error relative to lambda
            dUT[i]<-rpois(1, lambda = rnorm(1, arrivingProbArrayList[[m]][hour[i], 2, day[i]], error.perc*arrivingProbArrayList[[m]][hour[i], 2, day[i]])) #rnorm add an error relative to lambda (same error % to urgency or election)
          }
          admissionRateMonth[[m]] = list(dET, dUT)
        }else if(m> 12 && m <= 24){
          for(i in 1:(28*24)){
            dET[i]<-rpois(1, lambda = rnorm(1, arrivingProbArrayList[[m-12]][hour[i], 1, day[i]], error.perc*arrivingProbArrayList[[m-12]][hour[i], 1, day[i]])) #rnorm add an error relative to lambda
            dUT[i]<-rpois(1, lambda = rnorm(1, arrivingProbArrayList[[m-12]][hour[i], 2, day[i]], error.perc*arrivingProbArrayList[[m-12]][hour[i], 2, day[i]])) #rnorm add an error relative to lambda (same error % to urgency or election)
          }
          admissionRateMonth[[m]] = list(dET, dUT)
        }else{cat("Error. Month ", month, " are more than max. months allowed (24 months).")
          stop()}
      }
    }else if (error%in% FALSE && error.perc %in% NA) {
      admissionRateMonth = list()
      for(m in 1:month){
        dET = c()
        dUT = c()
        if(m <= 12){
          for(i in 1:(28*24)){
            dET[i]<-rpois(1, lambda = arrivingProbArrayList[[m]][hour[i], 1, day[i]])
            dUT[i]<-rpois(1, lambda = arrivingProbArrayList[[m]][hour[i], 2, day[i]])
          }
          admissionRateMonth[[m]] = list(dET, dUT)
        }else if(m> 12 && m <= 24){
          for(i in 1:(28*24)){
            dET[i]<-rpois(1, lambda = arrivingProbArrayList[[m-12]][hour[i], 1, day[i]])
            dUT[i]<-rpois(1, lambda = arrivingProbArrayList[[m-12]][hour[i], 2, day[i]])
          }
          admissionRateMonth[[m]] = list(dET, dUT)
        }else{print(paste0("Error. Month ", month, " are more than max. months allowed (24 months)."))}
      }
    }else{cat("Error in generate admission rate. No valid error was selected")
      stop()}

    admissionRate = data.table::rbindlist(admissionRateMonth)
    dET = admissionRate$V1
    dUT = admissionRate$V2

    dETS<-ifelse(dET>0, seq_along(dET), NA)
    dETS<-data.frame(dETS, dET)
    dETS<-dETS[complete.cases(dETS), ]
    names(dETS)<-c("hour", "patient")
    dUTS<-ifelse(dUT>0, seq_along(dUT), NA)
    dUTS<-data.frame(dUTS, dUT)
    dUTS<-dUTS[complete.cases(dUTS), ]
    names(dUTS)<-c("hour", "patient")
    admissionRate<-merge(dETS, dUTS, by = "hour", all.x = TRUE, all.y = TRUE)
    names(admissionRate)<-c("hour", "election", "urgency")
    admissionRate <- imputeTS::na_replace(admissionRate, 0)
    return(admissionRate)
  }


  ###type of simulation###
  if(icutype %in% "variable"){
    nBedsMin = round(nBedsStart* 0.875, digits = 0)
    nBedsMax = round(nBedsStart* 1.125, digits = 0)
  }else if(icutype %in% "fix"){
    nBedsMin = nBedsStart
    nBedsMax = nBedsStart
  }else{cat(icutype, " it's not a type of ICU simulation allowed. Allowed type of simulation are 'fix'and 'variable'", "\n")
    stop()}

  ###set.seed###
  if(!seed%in%FALSE){set.seed(seed)}

  ###synthetic data or controlled over data###
  if(typedata%in%"synthetic"){data = data.frame(c(1:20000), round(abs(rnorm(20000, 100, 30)), digits = 0))
                          names(data) = c("admissionid", "lengthofstay")
                          data$urgency = rbinom(20000, size = 1, prob = 0.5)
                          admission = data
                          admission$urgency = data$urgency
  }else{if(!sum(c("admissionid", "lengthofstay", "urgency")%in%colnames(data))==3){
   cat("Error in data: column admissionid or/and column lenghtofstay and/or column urgency it's not existing in data.", "\n")
    stop()}}

  admission = data

  ###control of admission rate simulation coefficients###
  election = ifelse(election %in% NA, 0.50, election)
  cAR = ifelse(cAR %in% NA, 0.20, cAR)
  if(sum(coefHE %in% NA)>0){coefHE=rep(ifelse(coefHE %in% NA, c(1), coefHE), 4)}
  if(sum(coefHU %in% NA)>0){coefHU=rep(ifelse(coefHU %in% NA, c(1), coefHE), 4)}
  if(sum(coefDE %in% NA)>0){coefDE=rep(ifelse(coefDE %in% NA, c(1), coefHE), 7)}
  if(sum(coefDU %in% NA)>0){coefDU=rep(ifelse(coefDU %in% NA, c(1), coefHE), 7)}
  if(sum(coefME %in% NA)>0){coefME=rep(ifelse(coefME %in% NA, c(1), coefHE), 12)}
  if(sum(coefMU %in% NA)>0){coefMU=rep(ifelse(coefMU %in% NA, c(1), coefHE), 12)}

  if(!losprediction %in% "stochastic"){cat("In this icubedsim version (0.1) only stochastic simulation is allowed.", "\n")
    stop()}

  if(CO>1 | CO<0){cat("CO values inserted ", CO, " is not allowed. Only value 0-1 are allowed.", "\n")
    stop()}

  ###Mask of start simulation###

  ###
  cat("ICUBedSIM v. 0.1 - ESICM Datathon 2023", "\n")
  cat("ICU simulation of ", month, " month.", "\n")
  cat("ICU simulation of ", nBedsStart, " ICU Beds.","\n")
  cat("Type of simulation: ", icutype, "\n")
  cat("Number of simulation: ", nsimulation, "\n")
  cat("Request output is ", type, " data.", "\n")
  cat("For more information visit our GitHub at https://github.com/querci-lorenzo/datathon2023", "\n")

  start.time <- Sys.time()
  ###
  monthS = month + 2
  hoursSim = monthS*28*24 #First 60 days will be deleted because ICU start empty and because varbed alghoritm start after one week after the first monthS. We cut first to monthS to represent real function of alghortim
  nBedsStart=nBedsMin
  rejectedAdmission<-list()
  hoursRejectedAdmission<-list()
  timeToAdmissionRejected<-list()
  admittedAdmission<-list()
  bedMatrixList<-list()
  prob<-list()
  for (s in 1:1){
    #print(paste("Simulation n°:", s))
    admissionRate<-admssionRateFunction(month = monthS, election = election, cAR = cAR, coefHE = coefHE, coefHU = coefHU, coefDE = coefDE, coefDU = coefDU, coefME = coefME, coefMU = coefMU, coef = coef, error = error, error.perc = error.perc)
    a = 1 #contatore pazienti che richiedono il posto in ICU rispetto alla matrice admissionRate
    j = c() #admissionid dei pazienti che vengono rigettati dalla ICU
    o = c() #hours to first free bad for rejected patients
    h = c() #orario (sequenziale) in cui i pazienti vengono rigettati
    x = c() #admissionid dei pazienti ammessi
    KC = NULL #control var. algh.
    CAdmRate = c() #write var. algh.
    CLosU = c() #write var. algh.
    CLosE = c() #write var. algh.
    CRatioU = c() #write var. algh.
    nBeds = matrix(rep(c(rep(1, nBedsStart), rep(NA, nBedsMax-nBedsStart)), hoursSim), ncol = nBedsMax, byrow = T) #1 = available bed
    hourBeds = c(rep(0, nBedsStart), rep(NA, nBedsMax-nBedsStart)) #hours to left a bed free
    idBeds<-rep(NA, nBedsMax) #raccoglie l'admission key dei pazienti ricoverati in quel momento (per il modello di predizione)
    hourClockBeds<-rep(NA, nBedsMax) #Conteggia i giorni di degenza attuali dei pazienti ricoverati
    bedMatrix<-matrix(ncol=nBedsMax, nrow = hoursSim) # Risultati
    PP = c() # Vettore che contiene le probabilitá di apertura
    for (i in 1:hoursSim){ # ore
      KC = 0
      hourBeds<-hourBeds-1
      hourBeds<-ifelse(hourBeds<0, 0, hourBeds)
      if(sum(is.na(nBeds[i, ]))==sum(is.na(nBeds[i-1, ]))){nBeds[i, ]<-ifelse(hourBeds==0, 1, nBeds[i-1, ])
      }else if(sum(is.na(nBeds[i-1, ]))>sum(is.na(nBeds[i, ]))){     ###Algoritmo di apertura posti letto
        TT = ifelse(hourBeds==0, 1, nBeds[i-1, !is.na(nBeds[i-1, ])])
        nBeds[i, !is.na(nBeds[i-1, ])]<- TT[!is.na(TT)]
        hourBeds = ifelse(!is.na(nBeds[i,])&hourBeds%in%NA, 0, hourBeds)
      }else if(sum(is.na(nBeds[i-1, ]))<sum(is.na(nBeds[i, ]))){     ###Algoritmo di chiusura posti letto
        if(sum(nBeds[i-1, which(nBeds[i, ]%in%NA)]%in%c(1, NA))==length(which(nBeds[i, ]%in%NA))){ # Queso funziona solo la TI era tutta aperta!! MODIFICA!!!
          nBeds[i, -which(nBeds[i, ]%in%NA)] = nBeds[i-1, -which(nBeds[i, ]%in%NA)]
          hourBeds = ifelse(is.na(nBeds[i,]), NA, hourBeds)
        }else{nBeds[i, ]<-ifelse(hourBeds==0, 1, nBeds[i-1, ])}
      }
      idBeds<-ifelse(hourBeds==0, NA, idBeds)
      hourClockBeds<-ifelse(hourBeds==0, NA, hourClockBeds+1)
      if(i == admissionRate$hour[a]){
        KC = 1
        #print(paste("Admission request at", i, "hours")) ###DA DISATTIVARE
        #print(paste(sum(admissionRate$election[a]+admissionRate$urgency[a], na.rm = T), "patient request. Election:", admissionRate$election[a], ". Urgency:", admissionRate$urgency[a])) ###DA DISATTIVARE
        pth<-as.factor(c(sample(data$admissionid[data$urgency %in% 1], admissionRate[a, 3]), sample(data$admissionid[data$urgency %in% 0], admissionRate[a, 2]))) #Urgent patient admitted preferencialy respect to election
        jj = ifelse(length(pth)-sum(nBeds[i, ], na.rm = T)>0, length(pth)-sum(nBeds[i, ], na.rm = T), 0) #number of rejected patients at each time (internal variable)
        a = ifelse(a==length(admissionRate$hour), a, a+1)
        if(sum(nBeds[i, ], na.rm = T)>=length(pth)){
          for (pt in 1:length(pth)){
            tp<-min(which(nBeds[i, ] == 1))
            #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]])) ###DA DISATTIVARE
           nBeds[i, tp] = 0
            hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
            idBeds[tp] = admission$admissionid[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
            hourClockBeds[tp] = 1
           x = c(x, as.character((pth[pt])))
          }
        }else if(sum(nBeds[i, ], na.rm = T)>0 & sum(nBeds[i, ], na.rm = T)<length(pth)){
          for (pt in 1:sum(nBeds[i, ], na.rm = T)){
            tp<-min(which(nBeds[i, ] == 1))
            #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]])) ###DA DISATTIVARE
            nBeds[i, tp] = 0
            hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
            idBeds[tp] = admission$admissionid[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
            hourClockBeds[tp] = 1
            x = c(x, as.character((pth[pt])))
          }
          j = c(j, as.character(pth[(length(pth)-jj+1):length(pth)])) #admissionid dei pazienti che vengono rigettati dalla ICU
          h = c(h, rep(i, jj)) #orario (sequenziale) in cui i pazienti vengono rigettati
          o = c(o, ifelse(rep(length(sort(hourBeds, decreasing = TRUE))>= jj, jj), tail(sort(hourBeds, decreasing = TRUE), jj), c(rep(max(hourBeds), jj-length(sort(hourBeds, decreasing = TRUE))), tail(sort(hourBeds, decreasing = TRUE), jj))))
          #print(paste("Not-available bed for", jj, " patient."))  ###DA DISATTIVARE
          }else{
            j = c(j, as.character(pth[(length(pth)-jj+1):length(pth)])) #admissionid dei pazienti che vengono rigettati dalla ICU
          h = c(h, rep(i, jj)) #orario (sequenziale) in cui i pazienti vengono rigettati
          o = c(o, ifelse(rep(length(sort(hourBeds, decreasing = TRUE))>= jj, jj), tail(sort(hourBeds, decreasing = TRUE), jj), c(rep(max(hourBeds), jj-length(sort(hourBeds, decreasing = TRUE))), tail(sort(hourBeds, decreasing = TRUE), jj))))
          #print(paste("Not-available bed for", jj, " patient."))  ###DA DISATTIVARE
          }
        }
        bedMatrix[i, ]<-nBeds[i, ]
        #Start IntellICU
        #Read
        CAdmRate<-c(CAdmRate, ifelse(KC == 1, length(pth), 0))
        if(KC==1){
          CLosU<-c(CLosU, mean(admission$lengthofstay[admission$admissionid%in%pth & admission$urgency%in%1], na.rm = T))
          CLosE<-c(CLosE, mean(admission$lengthofstay[admission$admissionid%in%pth & admission$urgency%in%0], na.rm = T))
          CRatioU<-c(CRatioU, table(admission$urgency[admission$admissionid%in%pth])[2])
        }
        #Write
        if(i>=28*24 && i%%24==0){
          PROB = rep(NA, 100)
          for(Y in 1:100){
            SNPat = rpois(24*7, lambda = (MASS::fitdistr(CAdmRate, "Poisson")$estimate + MASS::fitdistr(CAdmRate[(length(CAdmRate)-24*7):length(CAdmRate)], "Poisson")$estimate)/2)
            if(losprediction %in% "stochastic"){
              SNLos = rpois(sum(SNPat), lambda = (mean(CLosU, na.rm = T)*sum(CRatioU, na.rm = T)+mean(CLosE, na.rm = T)*(length(CAdmRate)-sum(CRatioU, na.rm = T)))/length(CAdmRate))
            }else if (losprediction %in% "predictive") {
           "In building"
            }else{print("Error in type of LOS prediction. Line 140")
            }
          degAtt = sum(rpois(sum(!is.na(hourClockBeds)), lambda = (mean(CLosU, na.rm = T)*sum(CRatioU, na.rm = T)+mean(CLosE, na.rm = T)*(length(CAdmRate)-sum(CRatioU, na.rm = T)))/length(CAdmRate)))
          PROB[Y] = ifelse(
          degAtt+
          sum(SNLos)-
          (24*7*(sum(!is.na(nBeds[i, ]))))-
          sum(SNLos[(length(SNLos)-sum(SNPat[(length(SNPat)-24):length(SNPat)])):length(SNLos)])+
          ifelse(max(h, na.rm = T)>(i-24*7), 72, 0)+
          ifelse(sum(ifelse(PP[((i/24)-3):((i/24)-1)]>CO, 100, 0))%in%NA, 0, sum(ifelse(PP[((i/24)-3):((i/24)-1)]>CO, 100, 0)))
          >0, 1, 0) #1 TI piena a 7 giorni
          }
          #Result
          #print(paste("Overbooking probability at 7 days: ", mean(PROB), ". Hour ", i))  ###DA DISATTIVARE
          PP=c(PP, mean(PROB))
          if(mean(PROB)>CO & i<(hoursSim-7*24)){
            if(sum(nBeds[i+(7*24-1),], na.rm = T)>nBedsStart){
              nBeds[(i+(7*24)):(i+(8*24-1)),]<-rep(1, length(nBeds[i+(24*7),]))
            }else if(sum(nBeds[i+(7*24-1),], na.rm = T)==nBedsStart){
              nBeds[(i+(7*24)):(i+(8*24-1)), (1:(nBedsStart+((dim(nBeds)[2]-nBedsStart)/2)))]<-rep(1)
            }else{print("ERROR: error in beds matrix. Line 170.")}
          }
        }
      #Finish IntellICU
      }
      hoursRejectedAdmission[[s]]<-h[h>(28*2*24)]-(28*2*24)
      rejectedAdmission[[s]]<-j[!is.na(j) & h>(28*2*24)]
      timeToAdmissionRejected[[s]]<-o[h>(28*2*24)]
      admittedAdmission[[s]]<-x[!is.na(x)]
      bedMatrixList[[s]]<-bedMatrix[((28*2*24)+1):hoursSim,]
      prob[[s]] <- PP
  }
  ###
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  cat("Estimated time to complete simulation is ", round(time.taken * nsimulation, digits = 0), " seconds", "\n")

  #Simulation

  month = month + 2
  hoursSim = month*28*24 #First 60 days will be deleted because ICU start empty and because varbed alghoritm start after one week after the first month. We cut first to month to represent real function of alghortim
  nBedsStart=nBedsMin
  rejectedAdmission<-list()
  hoursRejectedAdmission<-list()
  timeToAdmissionRejected<-list()
  admittedAdmission<-list()
  bedMatrixList<-list()
  prob<-list()
  for (s in 1:nsimulation){
    cat("Simulation n°:", s, " of ", nsimulation, "\n")
    admissionRate<-admssionRateFunction(month = month, election = election, cAR = cAR, coefHE = coefHE, coefHU = coefHU, coefDE = coefDE, coefDU = coefDU, coefME = coefME, coefMU = coefMU, coef = coef, error = error, error.perc = error.perc)
    a = 1 #contatore pazienti che richiedono il posto in ICU rispetto alla matrice admissionRate
    j = c() #admissionid dei pazienti che vengono rigettati dalla ICU
    o = c() #hours to first free bad for rejected patients
    h = c() #orario (sequenziale) in cui i pazienti vengono rigettati
    x = c() #admissionid dei pazienti ammessi
    KC = NULL #control var. algh.
    CAdmRate = c() #write var. algh.
    CLosU = c() #write var. algh.
    CLosE = c() #write var. algh.
    CRatioU = c() #write var. algh.
    nBeds = matrix(rep(c(rep(1, nBedsStart), rep(NA, nBedsMax-nBedsStart)), hoursSim), ncol = nBedsMax, byrow = T) #1 = available bed
    hourBeds = c(rep(0, nBedsStart), rep(NA, nBedsMax-nBedsStart)) #hours to left a bed free
    idBeds<-rep(NA, nBedsMax) #raccoglie l'admission key dei pazienti ricoverati in quel momento (per il modello di predizione)
    hourClockBeds<-rep(NA, nBedsMax) #Conteggia i giorni di degenza attuali dei pazienti ricoverati
    bedMatrix<-matrix(ncol=nBedsMax, nrow = hoursSim) # Risultati
    PP = c() # Vettore che contiene le probabilitá di apertura
    for (i in 1:hoursSim){ # ore
      KC = 0
      hourBeds<-hourBeds-1
      hourBeds<-ifelse(hourBeds<0, 0, hourBeds)
      if(sum(is.na(nBeds[i, ]))==sum(is.na(nBeds[i-1, ]))){nBeds[i, ]<-ifelse(hourBeds==0, 1, nBeds[i-1, ])
      }else if(sum(is.na(nBeds[i-1, ]))>sum(is.na(nBeds[i, ]))){     ###Algoritmo di apertura posti letto
        TT = ifelse(hourBeds==0, 1, nBeds[i-1, !is.na(nBeds[i-1, ])])
        nBeds[i, !is.na(nBeds[i-1, ])]<- TT[!is.na(TT)]
        hourBeds = ifelse(!is.na(nBeds[i,])&hourBeds%in%NA, 0, hourBeds)
      }else if(sum(is.na(nBeds[i-1, ]))<sum(is.na(nBeds[i, ]))){     ###Algoritmo di chiusura posti letto
        if(sum(nBeds[i-1, which(nBeds[i, ]%in%NA)]%in%c(1, NA))==length(which(nBeds[i, ]%in%NA))){ # Queso funziona solo la TI era tutta aperta!! MODIFICA!!!
          nBeds[i, -which(nBeds[i, ]%in%NA)] = nBeds[i-1, -which(nBeds[i, ]%in%NA)]
          hourBeds = ifelse(is.na(nBeds[i,]), NA, hourBeds)
        }else{nBeds[i, ]<-ifelse(hourBeds==0, 1, nBeds[i-1, ])}
      }
      idBeds<-ifelse(hourBeds==0, NA, idBeds)
      hourClockBeds<-ifelse(hourBeds==0, NA, hourClockBeds+1)
      if(i == admissionRate$hour[a]){
        KC = 1
        #print(paste("Admission request at", i, "hours")) ###DA DISATTIVARE
        #print(paste(sum(admissionRate$election[a]+admissionRate$urgency[a], na.rm = T), "patient request. Election:", admissionRate$election[a], ". Urgency:", admissionRate$urgency[a])) ###DA DISATTIVARE
        pth<-as.factor(c(sample(data$admissionid[data$urgency %in% 1], admissionRate[a, 3]), sample(data$admissionid[data$urgency %in% 0], admissionRate[a, 2]))) #Urgent patient admitted preferencialy respect to election
        jj = ifelse(length(pth)-sum(nBeds[i, ], na.rm = T)>0, length(pth)-sum(nBeds[i, ], na.rm = T), 0) #number of rejected patients at each time (internal variable)
        a = ifelse(a==length(admissionRate$hour), a, a+1)
        if(sum(nBeds[i, ], na.rm = T)>=length(pth)){
          for (pt in 1:length(pth)){
            tp<-min(which(nBeds[i, ] == 1))
            #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]])) ###DA DISATTIVARE
            nBeds[i, tp] = 0
            hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
            idBeds[tp] = admission$admissionid[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
            hourClockBeds[tp] = 1
            x = c(x, as.character((pth[pt])))
          }
        }else if(sum(nBeds[i, ], na.rm = T)>0 & sum(nBeds[i, ], na.rm = T)<length(pth)){
          for (pt in 1:sum(nBeds[i, ], na.rm = T)){
            tp<-min(which(nBeds[i, ] == 1))
            #print(paste("Admitted patient at", i, "hours at", tp, "bed. Urgency: ", data$urgency[data$admissionid %in% pth[pt]])) ###DA DISATTIVARE
            nBeds[i, tp] = 0
            hourBeds[tp]<-admission$lengthofstay[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
            idBeds[tp] = admission$admissionid[admission$admissionid%in%data$admissionid[data$admissionid%in%pth[pt]]]
            hourClockBeds[tp] = 1
            x = c(x, as.character((pth[pt])))
          }
          j = c(j, as.character(pth[(length(pth)-jj+1):length(pth)])) #admissionid dei pazienti che vengono rigettati dalla ICU
          h = c(h, rep(i, jj)) #orario (sequenziale) in cui i pazienti vengono rigettati
          o = c(o, ifelse(rep(length(sort(hourBeds, decreasing = TRUE))>= jj, jj), tail(sort(hourBeds, decreasing = TRUE), jj), c(rep(max(hourBeds), jj-length(sort(hourBeds, decreasing = TRUE))), tail(sort(hourBeds, decreasing = TRUE), jj))))
          #print(paste("Not-available bed for", jj, " patient."))  ###DA DISATTIVARE
        }else{
          j = c(j, as.character(pth[(length(pth)-jj+1):length(pth)])) #admissionid dei pazienti che vengono rigettati dalla ICU
          h = c(h, rep(i, jj)) #orario (sequenziale) in cui i pazienti vengono rigettati
          o = c(o, ifelse(rep(length(sort(hourBeds, decreasing = TRUE))>= jj, jj), tail(sort(hourBeds, decreasing = TRUE), jj), c(rep(max(hourBeds), jj-length(sort(hourBeds, decreasing = TRUE))), tail(sort(hourBeds, decreasing = TRUE), jj))))
          #print(paste("Not-available bed for", jj, " patient."))  ###DA DISATTIVARE
        }
      }
      bedMatrix[i, ]<-nBeds[i, ]
      #Start IntellICU
      #Read
      CAdmRate<-c(CAdmRate, ifelse(KC == 1, length(pth), 0))
      if(KC==1){
        CLosU<-c(CLosU, mean(admission$lengthofstay[admission$admissionid%in%pth & admission$urgency%in%1], na.rm = T))
        CLosE<-c(CLosE, mean(admission$lengthofstay[admission$admissionid%in%pth & admission$urgency%in%0], na.rm = T))
        CRatioU<-c(CRatioU, table(admission$urgency[admission$admissionid%in%pth])[2])
      }
      #Write
      if(i>=28*24 && i%%24==0){
        PROB = rep(NA, 100)
        for(Y in 1:100){
          SNPat = rpois(24*7, lambda = (MASS::fitdistr(CAdmRate, "Poisson")$estimate + MASS::fitdistr(CAdmRate[(length(CAdmRate)-24*7):length(CAdmRate)], "Poisson")$estimate)/2)
          if(losprediction %in% "stochastic"){
            SNLos = rpois(sum(SNPat), lambda = (mean(CLosU, na.rm = T)*sum(CRatioU, na.rm = T)+mean(CLosE, na.rm = T)*(length(CAdmRate)-sum(CRatioU, na.rm = T)))/length(CAdmRate))
          }else if (losprediction %in% "predictive") {
            "In building"
          }else{print("Error in type of LOS prediction. Line 140")
          }
          degAtt = sum(rpois(sum(!is.na(hourClockBeds)), lambda = (mean(CLosU, na.rm = T)*sum(CRatioU, na.rm = T)+mean(CLosE, na.rm = T)*(length(CAdmRate)-sum(CRatioU, na.rm = T)))/length(CAdmRate)))
          PROB[Y] = ifelse(
            degAtt+
              sum(SNLos)-
              (24*7*(sum(!is.na(nBeds[i, ]))))-
              sum(SNLos[(length(SNLos)-sum(SNPat[(length(SNPat)-24):length(SNPat)])):length(SNLos)])+
              ifelse(max(h, na.rm = T)>(i-24*7), 72, 0)+
              ifelse(sum(ifelse(PP[((i/24)-3):((i/24)-1)]>CO, 100, 0))%in%NA, 0, sum(ifelse(PP[((i/24)-3):((i/24)-1)]>CO, 100, 0)))
            >0, 1, 0) #1 TI piena a 7 giorni
        }
        #Result
        #print(paste("Overbooking probability at 7 days: ", mean(PROB), ". Hour ", i))  ###DA DISATTIVARE
        PP=c(PP, mean(PROB))
        if(mean(PROB)>CO & i<(hoursSim-7*24)){
          if(sum(nBeds[i+(7*24-1),], na.rm = T)>nBedsStart){
            nBeds[(i+(7*24)):(i+(8*24-1)),]<-rep(1, length(nBeds[i+(24*7),]))
          }else if(sum(nBeds[i+(7*24-1),], na.rm = T)==nBedsStart){
            nBeds[(i+(7*24)):(i+(8*24-1)), (1:(nBedsStart+((dim(nBeds)[2]-nBedsStart)/2)))]<-rep(1)
          }else{print("ERROR: error in beds matrix. Line 170.")}
        }
      }
      #Finish IntellICU
    }
    hoursRejectedAdmission[[s]]<-h[h>(28*2*24)]-(28*2*24)
    rejectedAdmission[[s]]<-j[!is.na(j) & h>(28*2*24)]
    timeToAdmissionRejected[[s]]<-o[h>(28*2*24)]
    admittedAdmission[[s]]<-x[!is.na(x)]
    bedMatrixList[[s]]<-bedMatrix[((28*2*24)+1):hoursSim,]
    prob[[s]] <- PP
  }

    if(type %in% "result"){return(list(rejectedAdmission = rejectedAdmission, hoursRejectedAdmission = hoursRejectedAdmission, timeToAdmissionRejected = timeToAdmissionRejected, admittedAdmission = admittedAdmission, bedMatrixList = bedMatrixList, prob = prob))
    }else if(type %in% "analysis" ){
      month = month - 2
      hoursSim = month*28*24
      ff = function(x){sum(x, na.rm = T)} #Percentuale letti liberi per ciascuna ora
      mat<-as.data.frame(matrix(rep(NA, hoursSim*nsimulation), ncol = nsimulation))
      for (f in 1:length(bedMatrixList)) {mat[, f]<-apply(bedMatrixList[[f]], 1, FUN = ff)}
      ff25 = function(x){quantile(x, 0.25, na.rm = T)}
      ff75 = function(x){quantile(x, 0.75, na.rm = T)}
      mat$Hours<-c(1:hoursSim)
      plot1<-ggplot(mat, aes(x = Hours)) +
        geom_line(aes(y = apply(mat[1:nsimulation], 1, mean)))+
        geom_ribbon(aes(
          ymax = apply(mat[1:nsimulation], 1, mean)+1.96*apply(mat[1:nsimulation], 1, sd),
          ymin = ifelse(apply(mat[1:nsimulation], 1, mean)-1.96*apply(mat[1:nsimulation], 1, sd)<0, 0, apply(mat[1:nsimulation], 1, mean)-1.96*apply(mat[1:nsimulation], 1, sd))), alpha = 0.3)+
        ylab("Total Hours Beds availables") +
        xlab("Hours of simulation") +
        ggtitle("% Beds available/hours simulation")+
        labs(caption = paste0("Simulation based on ", nBedsMin, " - ", nBedsMax, " beds-capacity and ", hoursSim, " hours of simulation. Simulation N = ", nsimulation)) +
        theme(plot.caption = element_text(hjust = 0))+
        theme_pubr()

      #Cost analysis
      freeBed<-data.frame(apply(mat[, 1:nsimulation], 2, sum)) #Totale ore libere nella simulazione per ciascuna simulazione
      hourbedfreeday<-apply(freeBed/(hoursSim/24), 2, mean)
      hourbedfreedaysd<-apply(freeBed/(hoursSim/24), 2, sd)

      #Analisi pazienti rejected
      meanrejected<-apply(as.data.frame(lapply(rejectedAdmission, length)), 1, mean)
      sdrejected<-apply(as.data.frame(lapply(rejectedAdmission, length)), 1, sd)

      #Analisi pazienti rejected hour
      meanHourRejected = mean(sapply(timeToAdmissionRejected, mean), na.rm = T)
      sdHourRejected = mean(sapply(timeToAdmissionRejected, sd), na.rm = T)

      #return
      return(
        list(
          plot1 = plot1,
          hourbedfreeday = hourbedfreeday,
          hourbedfreedaysd = hourbedfreedaysd,
          meanrejected = meanrejected/(hoursSim/24),
          sdrejected = sdrejected/(hoursSim/24),
          meanHourRejected = meanHourRejected,
          sdHourRejected = sdHourRejected
        )
      )
    }
}
