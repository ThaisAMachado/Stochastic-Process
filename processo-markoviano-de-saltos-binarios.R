Xt = function(alfa0, alfa1, estadosXt, tempoXt){
  n = length(estadosXt)
  tempo_salto = rexp(1, rate = 1/ifelse(estadosXt[n] == 1, alfa1, alfa0))
  novo_estado = ifelse(estadosXt[n] == 1, 0, 1)
  return(c(novo_estado, tempo_salto+tempoXt))
}

Yt = function(lambda0, lambda1, estadosYt, tempoYt){
  n = length(estadosYt)
  tempo_salto = rexp(1, rate = 1/ifelse(estadosYt[n] == 1, lambda1, lambda0))
  novo_estado = ifelse(estadosYt[n] == 1, 0, 1)
  return(c(novo_estado, tempo_salto+tempoYt))
}

Zt = function(alfa0, alfa1, lambda0, lambda1, N){
  # "estados" armazena os estados do processo Zt
  # "qtd" armazena a quantidade de saltos que o processo Zt deu 
  qtd = 0
  processo_Zt = data.frame("Estados" = c(1), "Tempos.dos.Saltos" = c(0))
  processo_Xt = data.frame("Estados" = c(1), "Tempos.dos.Saltos" = c(0))
  processo_Yt = data.frame("Estados" = c(1), "Tempos.dos.Saltos" = c(0))
  i = 2
  Xtdisparou = TRUE
  Ytdisparou = TRUE
  nX = nrow(processo_Xt)
  nY = nrow(processo_Yt)
  repeat{
    if(Xtdisparou){
      processo_Xt[nX+1,] = Xt(alfa0, alfa1, processo_Xt$Estados, 
                          processo_Xt$Tempos.dos.Saltos[nX])
    }
    if(Ytdisparou){
      processo_Yt[nY+1,] = Yt(lambda0, lambda1, processo_Yt$Estados, 
                          processo_Yt$Tempos.dos.Saltos[nY])
    }
    nX = nrow(processo_Xt)
    nY = nrow(processo_Yt)
    print(processo_Xt)
    print(processo_Yt)
    # Se o tempo de disparo de Xt foi menor, Xt disparou primeiro
    if(processo_Xt[nX,2] < processo_Yt[nY,2]){
      processo_Zt[i,1] = processo_Xt[nX,1]*processo_Yt[nY-1,1]
      processo_Zt[i,2] = processo_Xt[nX,2]
      Xtdisparou = TRUE
      Ytdisparou = FALSE
    } else {
      # Tempo de disparo de Yt menor -> Yt disparou primeiro
      processo_Zt[i,1] = processo_Xt[nX-1,1]*processo_Yt[nY,1]
      processo_Zt[i,2] = processo_Yt[nY,2]
      Ytdisparou = TRUE
      Xtdisparou = FALSE
    }
    i = i+1
    qtd = qtd+1
    if(qtd == N) break
  }
  return(processo_Zt)
}

Zt(0.7, 1.3, 0.6, 1.5, 5)
