#Disertatie 

#Ex. 1:
#Spatiul starilor este:

#S = {"M4", "M5", "M6", "M7"}

#Ex. 2:
#Matricea de trecere P:

Z<- matrix(data = c(0.8354, 0.1512, 0.0126, 0.0007, 0.0001, 0,
                    0.5552, 0.3710, 0.0649, 0.0057, 0.0022, 0.0010,
                    0.3163,	0.2316,	0.3131,	0.1246,	0.0144,	0.0000,
                    0.0629,	0.1572,	0.4214,	0.3019,	0.0503,	0.0063,
                    0.1111,	0.2593,	0.2222,	0.2593,	0.1481,	0.0000,
                    0.5000,	0.2500,	0.0000,	0.2500,	0.0000,	0.0000
), nrow = 6, ncol = 6, byrow = TRUE)
Z
stari<- c('M2','M3',"M4", "M5", "M6", "M7")  
stari
rownames(Z)<-stari
colnames(Z)<-stari
Z
Z**Z
#Calculam distributia stationara a lantului

stationary <- function(mat) {
  x = eigen(t(mat))$vectors[,1]
  as.double(x/sum(x))
}

stationary(Z)
#Rotunjim la 3 zecimale
round(stationary(Z), digits = 4)

#Verificam daca aceasta este distributia stationara

stationary(Z)%*%Z
round(stationary(Z)%*%Z, digits = 4)


round(stationary(Z), digits = 4)==round(stationary(Z)%*%Z, digits = 4)
distr_stationara<-stationary(Z)
distr_stationara
names(distr_stationara)<-stari
distr_stationara
round(distr_stationara, digits = 4)

medie_timp_cutremur_M7<-1/round(distr_stationara["M7"], digits = 4)
medie_timp_cutremur_M7

#Numarul mediu de perioade in care lantul revine in starea M7 este 3334.

#O perioada este egala cu nr. total al cutremurelor impartit la numarul de ani.
numar_cutremure <- 30504
numar_ani<-2021-1900+1
numar_ani
o_perioada<-numar_cutremure/numar_ani
o_perioada

#Numarul de ani in care se va produce urmatorul cutremur de magnitudine M7.
medie_ani_cutremur_M7<-medie_timp_cutremur_M7/o_perioada
medie_ani_cutremur_M7


#Exercitiul 15



stari<-c("M2", "M3", "M4", "M5", "M6", "M7")
init<-c(0.75,0.25,0,0,0,0)
init
names(init)<-stari
init


markov <- function(init,matrice,n,labels) {
  if (missing(labels)) labels <- 1:length(init)
  simlist <- numeric(n+1)
  states <- 1:length(init)
  simlist[1] <- sample(states,
                       1,
                       prob=init)
  for (i in 2:(n+1))
  { simlist[i] <- sample(states,
                         1,
                         prob=matrice[simlist[i-1],]) }
  labels[simlist] 
}

#generarea traiectoriei pentru 100 de pasi/tranzitii
traiectorie <- markov(init,Z,146,stari)
traiectorie
traiectorie[101]
length(traiectorie)
#interpretare: starea in care se afla lantul peste 100 zile daca pleaca din starea initiala generata de cod

#generare distrib de frecvente absolute
distr_frecv_abs <- table(traiectorie)
distr_frecv_abs

#generare distrib de frecvente relative
distr_frecv_rel <- distr_frecv_abs/length(traiectorie)
distr_frecv_rel


#10000 de simulari ale lantului Markov
stari<-c("M4", "M5", "M6", "M7", "M8")
init<-c(1,0,0,0,0)
init
names(init)<-stari
init

markov(init,Q,10,stari)


sim_total <- replicate(10000,markov(init,Q,10,stari))
sim_total


distr_frecv_abs <- table(sim_total)
distr_frecv_abs

distr_frecv_rel <- distr_frecv_abs/length(sim_total)
distr_frecv_rel

