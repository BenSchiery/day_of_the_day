rm(list = ls())

mult.safe <- function(s){
  s.split <- strsplit(s, split = "")[[1]]
  plus.minus <- c("+", "-")
  openers <- c("(", "[", "{")
  closers <- c(")", "]", "}")
  
  first.position <- T
  a <- 0
  safe <- T
  for(i in s.split){
    if(i %in% plus.minus & a == 0 & !first.position){
      safe <- F
      return(safe)
    }
    first.position <- F
    if(i %in% openers){
      a <- a - 1
    }
    if(i %in% closers){
      a <- a + 1
    }
  }
  return(safe)
}

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
  
  fun.bins <- list("trig" = 1:12,
                   "log" = c(13, 14),
                   "rad" = c(15, 16),
                   "arith" = 17:23,
                   "pow" = c(24, 25))
  
  con.bins <- list("common" = 1:7,
                   "uncommon" = 8:23)
  
  #### Loop ####
  
  r <- list(NaN, NA)
  while(is.nan(r[[1]]) | is.infinite(r[[1]])){
    # random sequence of functions
    bin.seq <- sample(length(fun.bins), size = n, replace = T)
    fun.seq <- vector("numeric", length = n)
    for(i in 1:n){
      fun.seq[i] <- sample(fun.bins[[bin.seq[i]]], size = 1)
    }
    
    # random sequence of constants
    bin.seq <- sample(c(1:length(con.bins)), size = n, replace = T)
    con.seq <- vector("numeric", length = n)
    for(i in 1:n){
      con.seq[i] <- sample(con.bins[[bin.seq[i]]], size = 1)
    }
    
    starter <- sample(length(con.lib), size = 1)
    runner <- con.lib[starter]
    TeX <- con.nom[starter]
    
    for(i in 1:n){
      if(is.nan(runner)){
        break
      }
      this.fun <- fun.nom[fun.seq[i]]
      this.con <- con.lib[con.seq[i]]
      this.con.nom <- con.nom[con.seq[i]]
      if(this.fun == "f01"){ 
        # cosine
        runner <- f01(runner)
        TeX <- paste("\\cos\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f02"){
        # sine
        runner <- f02(runner)
        TeX <- paste("\\sin\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f03"){
        # tangent
        runner <- f03(runner)
        TeX <- paste("\\tan\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f04"){
        # hyperbolic cosine
        runner <- f04(runner)
        TeX <- paste("\\cosh\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f05"){
        # hyperbolic sine
        runner <- f05(runner)
        TeX <- paste("\\sinh\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f06"){
        # hyperbolic tangent
        runner <- f06(runner)
        TeX <- paste("\\tanh\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f07" & runner >= -1 & runner <= 1){
        # inverse cosine
        runner <- f07(runner)
        TeX <- paste("\\cos^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f08" & runner >= -1 & runner <= 1){
        # inverse sine
        runner <- f08(runner)
        TeX <- paste("\\sin^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f09"){
        # inverse tangent
        runner <- f09(runner)
        TeX <- paste("\\tan^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f10" & runner >= 1){
        # inverse hyperbolic cosine
        runner <- f10(runner)
        TeX <- paste("\\cosh^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f11"){
        # inverse hyperbolic sine
        runner <- f11(runner)
        TeX <- paste("\\sinh^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f12" & runner > -1 & runner < 1){
        # inverse hyperbolic tangent
        runner <- f12(runner)
        TeX <- paste("\\tanh^{-1}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f13" & runner > 0){
        # natural logarithm
        runner <- f13(runner)
        TeX <- paste("\\ln\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f14" & runner > 0){
        # base c logarithm
        runner <- f14(x = runner, c = this.con)
        TeX <- paste("\\log_{", this.con.nom, "}\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f15" & runner >= 0){
        # square root
        runner <- f15(runner)
        TeX <- paste("\\sqrt{", TeX, "}", sep = "")
      }else if(this.fun == "f16" & runner >= 0){
        # c root
        runner <- f16(x = runner, c = this.con)
        TeX <- paste("\\sqrt[", this.con.nom, "]{", TeX, "}", sep = "")
      }else if(this.fun == "f17"){
        # add c
        runner <- f17(x = runner, c = this.con)
        TeX <- paste(TeX, "+", this.con.nom, sep = "")
      }else if(this.fun == "f18"){
        # subtract c
        runner <- f18(x = runner, c = this.con)
        TeX <- paste(TeX, "-", this.con.nom, sep = "")
      }else if(this.fun == "f19"){
        # subtract from c
        runner <- f19(x = runner, c = this.con)
        TeX <- paste(this.con.nom, "-\\left(", TeX, "\\right)", sep = "")
      }else if(this.fun == "f20"){
        # multiply by c
        runner <- f20(x = runner, c = this.con)
        TeX.safe <- mult.safe(TeX)
        TeX.chars <- strsplit(TeX, split = "")[[1]]
        TeX <- if(!TeX.safe){
          paste(this.con.nom, "\\left(", TeX, "\\right)", sep = " ")
        }else{
          if(TeX.chars[1] == "-"){
            paste("-", this.con.nom, " ", paste(TeX.chars[-1], collapse = ""), sep = "")
          }else{
            paste(this.con.nom, TeX, sep = " ") # need 'sep = space' here
          }
        }
      }else if(this.fun == "f21" & runner != 0){
        # divide c by x
        runner <- f21(x = runner, c = this.con)
        TeX <- paste("\\frac{", this.con.nom, "}{", TeX, "}", sep = "")
      }else if(this.fun == "f22"){
        # divide by c
        runner <- f22(x = runner, c = this.con)
        TeX <- paste("\\frac{", TeX, "}{", this.con.nom, "}", sep = "")
      }else if(this.fun == "f23" & runner != 0){
        # reciprocal
        runner <- f23(runner)
        TeX <- paste("\\frac{1}{", TeX, "}", sep = "")
      }else if(this.fun == "f24" & runner >= 0){
        # to the power of c
        runner <- f24(x = runner, c = this.con)
        TeX <- paste("\\left(", TeX, "\\right)^{", this.con.nom, "}", sep = "")
      }else if(this.fun == "f25" & this.con >= 0){
        # c to the power of x
        runner <- f25(x = runner, c = this.con)
        TeX <- paste(this.con.nom, "^{", TeX, "}", sep = "")
      }
    }
    r <- list(runner, TeX) 
  }
  
  if(r[[1]] < 0){
    r[[1]] <- -r[[1]]
    
    TeX.chars <- strsplit(r[[2]], split = "")[[1]]
    r[[2]] <- if(mult.safe(r[[2]])){
      if(TeX.chars[1] == "-"){
        paste(TeX.chars[-1], collapse = "")
      }else{
        paste("-", r[[2]], sep = "")
      }
    }else{
      paste("-", "\\left(", r[[2]], "\\right)", sep = "")
    }
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
write.to <- "./days_latex_V3.csv"

#########################
#### Making holidays ####
#########################

clear <- "\r               \r"
holidays <- as.data.frame(matrix(NA, ncol = 4, nrow = n.try))
day.counter <- matrix(0, nrow = 12, ncol = 31)
colnames(holidays) <- c("month", "day", "decimal", "LaTeX")
non.days <- c(2.3, 2.31, 4.31, 6.31, 9.31, 11.31)
for(i in 1:n.try){
  m <- 0
  d <- 0
  while(m > 12 | m < 1 | d > 31 | d < 1 | is.nan(m) | is.nan(d) | (m + d / 100) %in% non.days){
    date <- rand.latex(max.layer)
    m <- round(get.month(date[[1]]))
    d <- round(get.day(date[[1]]))
    # if the date could be valid...
    if(!is.nan(m) & !is.nan(d)){
      # ...increment the day counter
      if(m <= 12 & m >= 1 & d <= 31 & d >= 1){
        day.counter[m, d] <- day.counter[m, d] + 1
        # start throwing days out if we get too many repeats
        if(day.counter[m, d] > (lenience + 1) * n.try / 365.25){ 
          m <- 69
          d <- 420
        }
      }
    }
  }
  holidays[i,] <- c(m, round(d), date[[1]], date[[2]])
  cat("\r", clear, "\r", round(100 * i / n.try, digits = 2), "%", sep = "")
}

m <- as.numeric(holidays$month)
d <- as.numeric(holidays$day)
holidays <- holidays[order(m + 0.01 * d),]

write.csv(x = holidays, file = write.to, row.names = F)
