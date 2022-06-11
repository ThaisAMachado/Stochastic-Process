# 1) Implementar a simulação de um processo de poisson no R. Os argumentos de 
# entrada são a taxa de salto lambda e o tempo T até onde o processo será simulado. Ele
# deve retornar o número de saltos até o tempo T.

library(dplyr)
library(ggplot2)

ppoisson = function(lambda, T){
  # Vetor que armazenara os tempos entre os saltos:
  tempos_saltos = c(0)
  i = 2
  repeat{
    # Sorteio um valor da distribuicao exponencial de parametro lambda:
    tempos_saltos[i] = rexp(1, lambda)
    # O processo para quando a soma dos tempos entre os saltos for maior que T:
    if(sum(tempos_saltos) > T) break
    i = i+1
  }
  # O vetor "tempo" armazenara o tempo em que cada salto ocorreu:
  tempo = c()
  for(j in 1:length(tempos_saltos)){
    tempo[j] = sum(tempos_saltos[1:j])
  }
  # Retiro o tempo do ultimo salto para pegar o intervalo que nos interessa: [0,T]:
  cat("Numero de saltos ate o tempo", T, ":", length(tempo)-2)
  # Crio um data frame apenas para auxiliar a plotagem do gráfico:
  dataframe = data.frame(x = tempo[1:(length(tempo)-1)], y = 0:(length(tempo)-2), 
                         xend = c(tempo[2:(length(tempo)-1)], T),
                         yend = 0:(length(tempo)-2))
  # Grafico do Tempo x Estados do Processo:
  dataframe %>% ggplot(aes(x = x, y = y, yend = yend, xend = xend)) + geom_point() +
    geom_segment() + xlab("t") + ylab("N(t)") + 
    ggtitle("Tempo x Estados do Processo") + theme_light()
}

# Exemplos:

ppoisson(0.5,20)
ppoisson(0.15,40)

# 2) Implementar a simulação de um processo de poisson no R, cujos argumentos de entrada
# são a taxa de salto lambda e o número máximo de saltos N. Ele deve retornar o tempo
# gasto até observar N saltos.

poisson2 = function(lambda, N){
  # Vetor que armazenara os tempos entre os saltos:
  tempos_saltos = c(0)
  i = 2
  repeat{
    # Sorteio um valor da distribuicao exponencial de parametro lambda:
    tempos_saltos[i] = rexp(1, lambda)
    # O processo para quando a quantidade de saltos for igual a N:
    if(length(tempos_saltos) == (N+1)) break
    i = i+1
  }
  cat("Tempo gasto ate observar", N, "saltos:", round(sum(tempos_saltos),2))
  # O vetor "tempo" armazenara o tempo em que cada salto ocorreu:
  tempo = c()
  for(j in 1:length(tempos_saltos)){
    tempo[j] = sum(tempos_saltos[1:j])
  }
  # Crio um data frame apenas para auxiliar a plotagem do gráfico:
  dataframe = data.frame(x = tempo[1:length(tempo)], y = 0:(length(tempo)-1), 
                         xend = c(tempo[2:(length(tempo))], tempo[length(tempo)]),
                         yend = 0:(length(tempo)-1))
  # Grafico do Tempo x Estados do Processo:
  dataframe %>% ggplot(aes(x = x, y = y, yend = yend, xend = xend)) + geom_point() +
    geom_segment() + xlab("t") + ylab("N(t)") + 
    ggtitle("Tempo x Estados do Processo") + theme_light()
}

# Exemplos:

poisson2(0.6, 15)
poisson2(1.3, 20)

# 3) Agora definimos um conjunto finito {1,2,3} e tomamos um processo
# X_t, com X_0=1. Agora, a cada disparo, sorteamos um estado uniformemente, e o
# processo salta para aquele estado. Parece com Poisson, mas ao invés de somar +1 no 
# estado, saltamos aleatoriamente para outro estado.

# Os argumentos de entrada são a taxa de salto lambda e o número máximo de saltos N:
processo = function(lambda, N){
  # Vetor que armazenara os tempos entre os saltos:
  tempos_saltos = c(0)
  # Vetor que armazenara os estados:
  estados = c(1)
  i = 2
  repeat{
    # Sorteio um valor da distribuicao exponencial de parametro lambda:
    tempos_saltos[i] = rexp(1, lambda)
    # Sorteio o estado para qual o processo ira:
    estados[i] = sample(c(1,2,3), 1)
    # O processo para quando a quantidade de saltos for igual a N:
    if(length(tempos_saltos) == (N+1)) break
    i = i+1
  }
  # O vetor "tempo" armazenara o tempo em que cada salto ocorreu:
  tempo = c()
  for(j in 1:length(tempos_saltos)){
    tempo[j] = sum(tempos_saltos[1:j])
  }
  # Criando um data frame com os tempos dos saltos e os estados do processo:
  dataframe = data.frame("Tempos.dos.Saltos" = round(tempo,2), "Estados" = estados)
  print(dataframe)
  # Grafico do tempo x estados do processo:
  dataframe %>% ggplot(aes(x = Tempos.dos.Saltos, y = Estados, yend = Estados, 
                      xend = c(Tempos.dos.Saltos[2:nrow(dataframe)],
                               Tempos.dos.Saltos[nrow(dataframe)]))) + 
                      geom_point() + geom_segment() + xlab("Tempo") + ylab("Estados") + 
                      ggtitle("Tempo x Estados do Processo") + theme_light()
}

# Exemplos:

processo(0.5,12)
processo(2,30)

# 4) Por fim, imagine a mesma situação acima, porém, ao invés de escolher uniformemente 
# um estado, a escolha se dá de acordo com uma cadeia de Markov.

# Primeiramente construi uma funcao auxiliar que escolhe qual sera o proximo estado do 
# processo de acordo com o atual. As entradas sao a matriz de probabilidade de 
# transicao da cadeia de Markov e o estado em que se encontra o processo.

cadeia.markov = function(P, estado_anterior){
  # Matriz com as probabilidades de transicao acumuladas por linha:
  P[,2] = P[,2] + P[,1]
  P[,3] = P[,3] + P[,2]
  # Sorteio uma U(0,1):
  z = runif(1)
  # Comparo o numero sorteado com as probabilidades de transicao acumuladas:
  if(z <= P[estado_anterior,1]){
    novo_estado = 1
  } else{
    if(z > P[estado_anterior,1] & z <= P[estado_anterior,2]){
      novo_estado = 2
    } else {
      novo_estado = 3
    }
  }
  # Retorno o proximo estado do processo:
  return(novo_estado)
}

# Agora a funcao principal, cujos argumentos de entrada são a taxa de salto lambda, o
# número máximo de saltos N e a matriz de probabilidades de transicao P da cadeia de
# Markov.

processo2 = function(lambda, N, P){
  # Vetor que armazenara os tempos entre os saltos:
  tempos_saltos = c(0)
  # Vetor que armazenara os estados:
  estados = c(1)
  i = 2
  repeat{
    # Sorteio um valor da distribuicao exponencial de parametro \lambda:
    tempos_saltos[i] = rexp(1, lambda)
    # Chamo a funcao auxiliar para definir o estado para qual o processo ira:
    estados[i] = cadeia.markov(P, estados[i-1])
    # O processo para quando a quantidade de saltos for igual a N:
    if(length(tempos_saltos) == (N+1)) break
    i = i+1
  }
  # O vetor "tempo" armazenara o tempo em que cada salto ocorreu:
  tempo = c()
  for(j in 1:length(tempos_saltos)){
    tempo[j] = sum(tempos_saltos[1:j])
  }
  # Criando um data frame com os tempos dos saltos e os estados do processo:
  dataframe = data.frame("Tempos.dos.Saltos" = round(tempo,2), "Estados" = estados)
  print(dataframe)
  # Grafico do tempo x estados do processo:
  dataframe %>% ggplot(aes(x = Tempos.dos.Saltos, y = Estados, yend = Estados, 
                      xend = c(Tempos.dos.Saltos[2:nrow(dataframe)],
                               Tempos.dos.Saltos[nrow(dataframe)]))) + 
                      geom_point() + geom_segment() + xlab("Tempo") + ylab("Estados") + 
                      ggtitle("Tempo x Estados do Processo") + theme_light()
}

# Exemplos:

m = matrix(c(0.1,0.2,0.7,0.3,0.4,0.3,0.5,0.4,0.1), nrow = 3, ncol = 3, byrow = T)
processo2(1,8,m)

M = matrix(c(0.6,0.2,0.2,0.4,0.4,0.2,0.3,0.3,0.4), nrow = 3, ncol = 3, byrow = T)
processo2(0.8,15,M)
