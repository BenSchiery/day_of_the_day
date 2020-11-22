rm(list = ls())
graphics.off()
options(scipen = 999)

##########################
#### Constant Library ####
##########################

e <- exp(1) # e
phi <- (1 + sqrt(5))/2 # golden ratio
gamma <- 0.5772156649 # Euler-Mascheroni
sqrt2 <- sqrt(2)
ln2 <- log(2)
K <- 0.915965594177 # Catalan
delta <- 4.6692016091 # Feigenbaum delta
alpha <- 2.502907875096 # Feigenbaum alpha
F <- 2.8077702420285 # Fransen-Robinson
rho <- 0.11494204485 # polygon inscribing
q <- 1.78723165018 # Komornik-Loreti
P <- 2.29558714939 # universal parabolic
C <- 1.7052111401 # Niven
M <- 0.2614972128476 # Meissel-Mertens
B <- 1.45607494858 # Backhouse
G <- 0.834626841674 # Gauss
mu <- 1.45136923488 # Soldner
sigma <- 1.66168794963 # Somos
OMEGA <- 0.5671432904 # omega
psi <- 3.35988566624 # reciprocal Fibonacci
K0 <- 2.685452001065 # Khinchin's
A <- 1.2824271291006 # Glaisher-Kinkelin
zeta3 <- 1.20205690315959 # Apery's
M3 <- -1.74756459463318 # Madelung for 3D NaCl crystal
Pi2 <- 0.660161815846869 # twin primes
L <- 2.622057554292 # lemniscate
KLR <- 0.764223653589 # Landau-Ramanujan
eta <- 0.73733830336929 # Grossman
xi <- 1.187452351126501 # Foias
sigmap <- 0.850736188201867 # paper folding

delta * e - sigma * G
