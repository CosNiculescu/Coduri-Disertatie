#Disertatie 

#Ex. 1:
#Spatiul starilor este:

#S = {"M4", "M5", "M6", "M7"}

#Ex. 2:
#Matricea de trecere P:

Q<- matrix(data = c(0.8036, 0.1815, 0.0134, 0.0015, 0.0001, 
                    0.2599, 0.7206, 0.0176, 0.0017, 0.0001, 
                    0.0453, 0.6741, 0.2726, 0.0072, 0.0007, 
                    0.0459, 0.4984, 0.3672, 0.0869, 0.0016, 
                    0.0000, 0.2432, 0.4865, 0.1622, 0.1081
                    ), nrow = 5, ncol = 5, byrow = TRUE)
Q
stari<- c("M4", "M5", "M6", "M7", 'M8')  
stari
rownames(Q)<-stari
colnames(Q)<-stari
Q
Q**Q
#Calculam distributia stationara a lantului

stationary <- function(mat) {
  x = eigen(t(mat))$vectors[,1]
  as.double(x/sum(x))
}

stationary(Q)
#Rotunjim la 3 zecimale
round(stationary(Q), digits = 4)

#Verificam daca aceasta este distributia stationara

stationary(Q)%*%Q
round(stationary(Q)%*%Q, digits = 4)


round(stationary(Q), digits = 4)==round(stationary(Q)%*%Q, digits = 4)
distr_stationara<-stationary(Q)
distr_stationara
names(distr_stationara)<-stari
distr_stationara
round(distr_stationara, digits = 4)

medie_timp_cutremur_M7<-1/round(distr_stationara["M7"], digits = 4)
medie_timp_cutremur_M7

#Numarul mediu de perioade in care lantul revine in starea M7 este 526.

#O perioada este egala cu nr. total al cutremurelor impartit la numarul de ani.
numar_cutremure <- 363070
numar_ani<-2012-1900+1
numar_ani
o_perioada<-numar_cutremure/numar_ani
o_perioada

#Numarul de ani in care se va produce urmatorul cutremur de magnitudine M7.
medie_ani_cutremur_M7<-medie_timp_cutremur_M7/o_perioada
medie_ani_cutremur_M7


#Exercitiul 15



stari<-c("M4", "M5", "M6", "M7", "M8")
init<-c(1,0,0,0,0)
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
traiectorie <- markov(init,Q,329,stari)
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
