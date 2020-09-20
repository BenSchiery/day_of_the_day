rm(list = ls())

rand.latex <- function(max.layer = 4){
  n <- ceiling(max.layer * runif(1)^(1/3))
  
  con.lib <- c(pi, # pi
               exp(1), # e
               (1 + sqrt(5))/2, # golden ratio
               0.5772156649, # Euler-Mascheroni
               1,
               sqrt(2),
               log(2),
               0.915965594177, # Catalan
               4.6692016091, # Feigenbaum delta
               2.502907875096, # Feigenbaum alpha
               2.8077702420285, # Fransen-Robinson
               0.11494204485, # polygon inscribing
               1.78723165018, # Komornik-Loreti
               2.29558714939, # universal parabolic
               1.7052111401, # Niven
               0.2614972128476, # Merten
               1.45607494858, # Backhouse
               0.834626841674, # Gauss
               1.45136923488, # Soldner
               1.66168794963, # Somos
               0.5671432904, # omega
               3.35988566624, # reciprocal Fibonacci
               2.685452001065, # Khinchin's
               1.2824271291006, # Glaisher-Kinkelin
               1.20205690315959, # Apery's
               -1.74756459463318, # Madelung for 3D NaCl crystal
               0.660161815846869, # twin primes
               2.622057554292, # lemniscate
               0.764223653589 # Landau-Ramanujan
  )
  
  con.nom <- c("\\pi", 
               "e", 
               "\\phi", 
               "\\gamma", 
               "1",
               "\\sqrt{2}",
               "\\ln(2)",
               "K", # Catalan's
               "\\delta", # Feigenbaum's first
               "\\alpha", # Feigenbaum's second
               "F", # Fransen-Robinson
               "\\rho", # Kepler-Bouwkamp aka polygon inscribing
               "q", # Komornik-Loreti
               "P", # universal parabolic
               "C", # Niven's
               "M", # Merten's
               "B", # Backhouse's
               "G", # Gauss's
               "\\mu", # Soldner's
               "\\sigma", # Somos' quadratic recurrence
               "\\Omega", # Omega constant
               "\\psi", # Reciprocal Fibonacci
               "K_{0}", # Khinchin's
               "A", # Glaisher-Kinkelin
               "\\zeta(3)", # Apery's
               "M_{3}", # Madelung
               "\\Pi_{2}", # twin primes
               "L", # lemniscate
               "K_{LR}" # Landau-Ramanujan
  )
  
  f01 <- function(x){cos(x)}
  f02 <- function(x){sin(x)}
  f03 <- function(x){tan(x)}
  f04 <- function(x){cosh(x)}
  f05 <- function(x){sinh(x)}
  f06 <- function(x){tanh(x)}
  f07 <- function(x){acos(x)}
  f08 <- function(x){asin(x)}
  f09 <- function(x){atan(x)}
  f10 <- function(x){acosh(x)}
  f11 <- function(x){asinh(x)}
  f12 <- function(x){atanh(x)}
  f13 <- function(x){log(x)}
  f14 <- function(x, c){log(x, base = c)}
  f15 <- function(x){sqrt(x)}
  f16 <- function(x, c){x^(1/c)}
  f17 <- function(x, c){x + c}
  f18 <- function(x, c){x - c}
  f19 <- function(x, c){c - x}
  f20 <- function(x, c){c * x}
  f21 <- function(x, c){c / x}
  f22 <- function(x, c){x / c}
  f23 <- function(x){1/x}
  f24 <- function(x, c){x^c}
  f25 <- function(x, c){c^x}
  
  fun.bins <- list("trig" = c(1:12),
                   "log" = c(13, 14),
                   "rad" = c(15, 16),
                   "arith" = c(17:23),
                   "pow" = c(24, 25))
  
  con.bins <- list("common" = c(1:7),
                   "uncommon" = c(8:23))
  
  #### Loop ####
  
  r <- list(NaN, NA)
  while(is.nan(r[[1]]) | is.infinite(r[[1]])){
    bin.seq <- sample(c(1:length(fun.bins)), size = n, replace = T)
    fun.seq <- vector("numeric", length = n)
    for(i in 1:n){
      fun.seq[i] <- sample(fun.bins[[bin.seq[i]]], size = 1)
    }
    
    bin.seq <- sample(c(1:length(con.bins)), size = n, replace = T)
    con.seq <- vector("numeric", length = n)
    for(i in 1:n){
      con.seq[i] <- sample(con.bins[[bin.seq[i]]], size = 1)
    }
    
    fun.lib <- c(f01, f02, f03, f04, f05, 
                 f06, f07, f08, f09, f10, 
                 f11, f12, f13, f14, f15, 
                 f16, f17, f18, f19, f20,
                 f21, f22, f23, f24, f25) 
    
    fun.nom <- c("f01", "f02", "f03", "f04", "f05", 
                 "f06", "f07", "f08", "f09", "f10", 
                 "f11", "f12", "f13", "f14", "f15", 
                 "f16", "f17", "f18", "f19", "f20",
                 "f21", "f22", "f23", "f24", "f25")
    
    starter <- sample(c(1:length(con.lib)), size = 1)
    runner <- con.lib[starter]
    TeX <- con.nom[starter]
    
    for(i in 1:n){
      this.fun <- fun.nom[fun.seq[i]]
      this.con <- con.lib[con.seq[i]]
      this.con.nom <- con.nom[con.seq[i]]
      if(this.fun == "f01"){
        runner <- f01(runner)
        TeX <- paste("\\cos\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f02"){
        runner <- f02(runner)
        TeX <- paste("\\sin\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f03"){
        runner <- f03(runner)
        TeX <- paste("\\tan\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f04"){
        runner <- f04(runner)
        TeX <- paste("\\cosh\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f05"){
        runner <- f05(runner)
        TeX <- paste("\\sinh\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f06"){
        runner <- f06(runner)
        TeX <- paste("\\tanh\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f07"){
        runner <- f07(runner)
        TeX <- paste("\\cos^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f08"){
        runner <- f08(runner)
        TeX <- paste("\\sin^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f09"){
        runner <- f09(runner)
        TeX <- paste("\\tan^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f10"){
        runner <- f10(runner)
        TeX <- paste("\\cosh^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f11"){
        runner <- f11(runner)
        TeX <- paste("\\sinh^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f12"){
        runner <- f12(runner)
        TeX <- paste("\\tanh^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f13"){
        runner <- f13(runner)
        TeX <- paste("\\ln\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f14"){
        runner <- f14(x = runner, c = this.con)
        TeX <- paste("\\log_{", this.con.nom, "}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f15"){
        runner <- f15(runner)
        TeX <- paste("\\sqrt{", TeX, "}", sep = "")
      }else if(this.fun == "f16"){
        runner <- f16(x = runner, c = this.con)
        TeX <- paste("\\sqrt[", this.con.nom, "]{", TeX, "}", sep = "")
      }else if(this.fun == "f17"){
        runner <- f17(x = runner, c = this.con)
        TeX <- paste(TeX, "+", this.con.nom, sep = "")
      }else if(this.fun == "f18"){
        runner <- f18(x = runner, c = this.con)
        TeX <- paste(TeX, "-", this.con.nom, sep = "")
      }else if(this.fun == "f19"){
        runner <- f19(x = runner, c = this.con)
        TeX <- paste(this.con.nom, "-\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f20"){
        runner <- f20(x = runner, c = this.con)
        TeX <- paste(this.con.nom, TeX, sep = " ") # need 'sep = space' here
      }else if(this.fun == "f21"){
        runner <- f21(x = runner, c = this.con)
        TeX <- paste("\\frac{", this.con.nom, "}{", TeX, "}", sep = "")
      }else if(this.fun == "f22"){
        runner <- f22(x = runner, c = this.con)
        TeX <- paste("\\frac{", TeX, "}{", this.con.nom, "}", sep = "")
      }else if(this.fun == "f23"){
        runner <- f23(runner)
        TeX <- paste("\\frac{1}{", TeX, "}", sep = "")
      }else if(this.fun == "f24"){
        runner <- f24(x = runner, c = this.con)
        TeX <- paste("\\left(", TeX, "\\right)^{", this.con.nom, "}", sep = "")
      }else if(this.fun == "f25"){
        runner <- f25(x = runner, c = this.con)
        TeX <- paste(this.con.nom, "^{", TeX, "}", sep = "")
      }
    }
    r <- list(runner, TeX) 
  }
  
  if(r[[1]] < 0){
    r[[1]] <- -r[[1]]
    r[[2]] <- paste("-", "\\left(", r[[2]], "\\right)", sep = "")
  }
  
  return(r)
}

trunc.2 <- function(x){
  s <- x * 100
  floor(abs(s)) * sign(s) / 100
}

get.month <- function(x){
  floor(abs(x)) * sign(x)
}

get.day <- function(x){
  x <- trunc.2(abs(x))
  100 * (x - get.month(x))
}

#########################
#### User Parameters ####
#########################

n.try <- 1e4
max.layer <- 4
lenience <- 0.3 # a multiplier; each date may appear no more than [1 + lenience] * [its fair share of times]
write.to <- "./days_latex.csv"

#########################
#### Making holidays ####
#########################

clear <- paste(collapse = "", rep(" ", times = 15))
holidays <- as.data.frame(matrix(NA, ncol = 3, nrow = n.try))
colnames(holidays) <- c("month", "day", "LaTeX")
non.days <- c(2.3, 2.31, 4.31, 6.31, 9.31, 11.31)
for(i in 1:n.try){
  m <- 0
  d <- 0
  while(m > 12 | m < 1 | d > 31 | d < 1 | is.nan(m) | is.nan(d) | (m + d / 100) %in% non.days){
    date <- rand.latex(max.layer)
    m <- round(get.month(date[[1]]))
    d <- round(get.day(date[[1]]))
    if(!is.nan(m) & !is.nan(d) & i > 1){
      x <- as.numeric(holidays$month) + as.numeric(holidays$day) / 100
      x <- x[!is.na(x)]
      y <- m + d / 100
      z <- sum(x == y)
      if(z > (lenience + 1) * n.try / 365.25){ # start throwing days out if we get too many repeats
        m <- 69
        d <- 420
      }
    }
  }
  holidays[i,] <- c(m, round(d), date[[2]])
  cat("\r", clear, "\r", round(100 * i / n.try, digits = 2), "%", sep = "")
}

m <- as.numeric(holidays$month)
d <- as.numeric(holidays$day)
holidays <- holidays[order(m + 0.01 * d),]

write.csv(x = holidays, file = write.to, row.names = F)
