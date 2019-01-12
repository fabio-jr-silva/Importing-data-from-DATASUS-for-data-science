# ------------------------- SCRIP PARA LER INTERNAÇÕES HOSPITALARES DO BRASIL ---------------------
# Author: Fábio Jr Silva


# instalação dos pacotes requeridos
install.packages("read.dbc")
install.packages("stringr")
install.packages("lubridate")

# Importação das bibliotecas para dentro desse Script
library(read.dbc)
library(lubridate)
library(stringr)

# --------------------------- OBSERVAÇÃO --------------------------------------------------------------
# Antes de continuar é necessário entrar no site do DATASUS.
# link: http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1&acao=25.
# Baixar os arquivos das internações hospitalares por estado, ano, ou mês como está disponível.
# Descompactar os arquivos baixados em uma pasta no seu computador.
#------------------------------------------------------------------------------------------------------

# Caminho da pasta onde estão os arquivos de internações hospitalares baixados do DATASUS
setwd("C:/AIH")


# Deixe na lista abaixo apenas os estados que você baixou no siste do DATASUS, apague o resto
ufs = c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 
        'GO', 'MA', 'MT', 'MS', 'MG', 'PA', 'PB', 'PR', 
        'PE', 'PI', 'RJ', 'RN', 'RS', 'RO', 'RR', 'SC', 
        'SP', 'SE', 'TO')


# Estrutura de repetição para pegar todos os estados informados acima
for(uf in 1:NROW(ufs)){ # < -- DEIXE O CURSOSR EM CIMA DESSE for e aperte ctrl + Enter para executar
  
  rduf = paste('RD',ufs[uf], sep='') # Concatena "RD" com a sigla do estado

# ------------------------------------ CONFIGURAÇÃO NECESSÁRIA ---------------------------------------
  data_inicial = ymd("2009-01-01") # <-- Informe a data inicial no padrão americano yyyy-mm-dd
  data_final = ymd("2018-10-30") # <-- Informe a data final no padrão americano yyyy-mm-dd
# ---------------------------------------------------------------------------------------------------- 
  
  # VAriávei que vai avançar conforme se alteram os meses 
  data_corrente = data_inicial
  
  
  # Estrutura de repetição para percorrer o período
  while (data_corrente < data_final){
   
      # Concatena 0 com i para ter o mome do arquivo com o ano
      ano = paste(str_sub(as.character(year(data_corrente)), start = 3), sep='')
      
      
      # concatena 0 com j para ter o nome do arquivo com mes
      if (month(data_corrente) < 10){
        mes = paste('0', month(data_corrente), sep='')
      } else{
        mes = paste('', month(data_corrente), sep='')
      }
      
      # Formação do nome do arquivo no padrão RDPBAAMM.dbc
      nomeDateBase = paste(rduf, ano, mes, '.dbc', sep='')
      
      # log com o nome do arquivo que será lido
      print(nomeDateBase)
      
      # Lê um arquivo e cria uma base de dados
      df = read.dbc(nomeDateBase)

# ------------------------------------------------  CONFIGURAÇÃO -------------------------------------     
      # Informe o nome das colunas que você deseja selecionar para seu estudos
      newdf = df[c('MUNIC_RES', 'CEP', 'IDENT', 'ESPEC', 'INSTRU','SEXO', 'COD_IDADE', 'IDADE', 
                   'DT_INTER', 'DIAG_PRINC', 'DIAS_PERM','CAR_INT', 'PROC_SOLIC', 'RACA_COR')]
      
      # Informe o CID-10 que você deseja selecionar
      iPd = subset(newdf, DIAG_PRINC=='A90')
# ---------------------------------------------------------------------------------------------------
      
      if(data_corrente == data_inicial){
        iternacoes = iPd
        data_corrente = data_corrente + ddays(as.integer(days_in_month(data_corrente)))
        
      } else{
        # Junção de todas as internações por dengue
        iternacoes = rbind(iternacoes, iPd)
        data_corrente = data_corrente + ddays(as.integer(days_in_month(data_corrente)))
      }
    
 
 } # Fim da estrutura de repetição para o período
  
  # Mostra o número de internações por estado no período analizado
  print(paste(ufs[uf], ' Teve ', nrow(iternacoes), ' Internações', sep=''))
  
  if(uf == 1){
    # Inicializacao da variavel
    InterBR = iternacoes
  } else{
    # As internaçoes dos estados e período selecionado
    InterBR = rbind(InterBR, iternacoes) 
  }
  
  
} # Fim da estrutura de repetição que carrega os estados


# Escreve um arquivo .csv sem os nomes das colunas selecionadas acima
write.csv(baseTemp, file = "Internacoes_BR.csv", row.names=FALSE)
