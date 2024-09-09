library(httr)
library(jsonlite)

# Função para fazer login no Bluesky sem expor as variáveis no ambiente global
criacao_sessao_bluesky <- function() {
  # Verificar se a variável de ambiente "BearerBsky" já existe
  bearer_atual <- Sys.getenv("BearerBsky")
  
  if (bearer_atual != "") {
    # Se a variável já estiver definida, perguntar ao usuário se deseja continuar
    # Printar em vermelho usando o código ANSI
    cat("\033[31mJá existe um Bearer salvo. Deseja criar outro? (s/n): \033[0m")
    resposta <- readline()
    
    if (tolower(resposta) != "s") {
      cat("Operação cancelada.\n")
      return(invisible())  # Cancelar a operação sem retornar "NULL" visível
    }
  }
  
  # Solicitar as credenciais do usuário
  bluesky_handle <- readline(prompt = "Digite seu Bluesky Handle (ex: usuario.bsky.social): ")
  bluesky_senha <- readline(prompt = "Digite sua senha para App do Bluesky (ex. 1234-5678-9012-3456): ")
  
  # URL do endpoint
  url <- "https://bsky.social/xrpc/com.atproto.server.createSession"
  
  # Corpo da requisição (JSON)
  body <- toJSON(list(
    identifier = bluesky_handle,
    password = bluesky_senha
  ), auto_unbox = TRUE)
  
  # Fazer a requisição POST
  response <- POST(url,
                   add_headers(`Content-Type` = "application/json"),
                   body = body,
                   encode = "json")
  
  # Verificar o status da requisição
  if (status_code(response) == 200) {
    # Parsear o conteúdo JSON
    resposta_conteudo <- content(response, "text", encoding = "UTF-8")
    
    # Converter o JSON em uma lista
    dados_resposta <- fromJSON(resposta_conteudo)
    
    # Verificar se o campo 'accessJwt' está presente na resposta
    if (!is.null(dados_resposta$accessJwt)) {
      # Setar o accessJwt como variável de ambiente
      Sys.setenv(BearerBsky = dados_resposta$accessJwt)
      
      # Confirmar que a variável foi setada
      cat("Bearer foi salvo como variável de ambiente 'BearerBsky'.\n")
    } else {
      cat("Erro: Bearer não encontrado na resposta.\n")
    }
    
  } else {
    # Em caso de erro na resposta
    print(paste("Erro:", status_code(response)))
    print(content(response, "text"))
  }
}

# Chamar a função
criacao_sessao_bluesky()




##################################################################################
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)

# Função para buscar publicações com base em um termo e coletar todos os dados disponíveis para um intervalo de tempo
buscar_publicacoes <- function(termo, token, desde, ate, ordenacao = "top", limite = 100) {
  url <- "https://public.api.bsky.app/xrpc/app.bsky.feed.searchPosts"
  
  todas_publicacoes <- list()
  cursor <- NULL
  
  repeat {
    # Montando os parâmetros de requisição
    parametros_consulta <- list(
      q = termo,
      sort = ordenacao,  # Adicionar parâmetro de ordenação com valor default "top"
      since = desde,
      until = ate,
      limit = as.character(limite)
    )
    
    if (!is.null(cursor)) {
      parametros_consulta$cursor <- cursor  # Adicionar cursor se houver
    }
    
    # Fazer a requisição GET
    resposta <- tryCatch({
      GET(url, query = parametros_consulta, add_headers(Authorization = paste("Bearer", token)))
    }, error = function(e) {
      warning("Erro ao enviar solicitação para a API:", conditionMessage(e))
      return(NULL)
    })
    
    if (is.null(resposta) || resposta$status_code != 200) {
      warning("Erro ao buscar publicações com o termo:", termo, "Código de status:", resposta$status_code)
      break
    }
    
    # Parsear a resposta JSON
    conteudo_resposta <- content(resposta, as = "parsed", type = "application/json")
    
    publicacoes <- conteudo_resposta$posts
    
    if (length(publicacoes) == 0) {
      break
    }
    
    todas_publicacoes <- c(todas_publicacoes, publicacoes)
    
    cursor <- conteudo_resposta$cursor
    
    # Parar se for a última página
    if (is.null(cursor)) {
      break
    }
    
    Sys.sleep(1)
  }
  
  return(todas_publicacoes)
}

# Função principal para fazer requisições de hora em hora
buscar_publicacoes_por_hora <- function(termo, token, inicio, fim, ordenacao = "top", limite = 100) {
  # Criar a sequência de horas
  tempos <- seq(from = as.POSIXct(inicio, tz = "UTC"),
                to = as.POSIXct(fim, tz = "UTC"),
                by = "hour")
  
  # Lista para armazenar todas as publicações
  todas_publicacoes <- list()
  
  # Variável para armazenar o número total de publicações antes do loop atual
  publicacoes_anteriores <- 0
  
  # Loop para cada intervalo de uma hora
  for (i in 1:(length(tempos) - 1)) {
    desde <- tempos[i]
    ate <- tempos[i + 1]
    
    # Formatar os timestamps no formato ISO 8601 (YYYY-MM-DDTHH:MM:SSZ)
    desde_formatado <- format(desde, "%Y-%m-%dT%H:%M:%SZ")
    ate_formatado <- format(ate, "%Y-%m-%dT%H:%M:%SZ")
    
    # Printar apenas uma vez por intervalo de tempo
    cat("Fazendo requisição de", desde_formatado, "até", ate_formatado, "\n")
    
    # Chamar a função buscar_publicacoes para coletar todos os dados para esse intervalo de tempo
    publicacoes <- buscar_publicacoes(termo = termo, token = token, desde = desde_formatado, ate = ate_formatado, ordenacao = ordenacao, limite = limite)
    
    # Verificar se publicações foram retornadas e acumular
    if (!is.null(publicacoes) && length(publicacoes) > 0) {
      todas_publicacoes <- c(todas_publicacoes, publicacoes)
    }
    
    # Calcular o número de publicações coletadas nesse intervalo de tempo
    publicacoes_atual <- length(todas_publicacoes)
    publicacoes_coletadas <- publicacoes_atual - publicacoes_anteriores
    publicacoes_anteriores <- publicacoes_atual
    
    # Printar o número de publicações acumuladas e o número coletado nesse loop
    cat("Publicações acumuladas até agora:", publicacoes_atual, "( +", publicacoes_coletadas, " )\n")
    
    Sys.sleep(1)  # Pausa para evitar sobrecarregar a API
  }
  
  # Retornar todas as publicações coletadas
  return(todas_publicacoes)
}

# Parâmetros de exemplo
tipo_ordenacao <- "top"
termo <- "lula"
inicio <- "2024-09-08T00:00:00Z"
fim <- "2024-09-09T23:00:00Z"

# Fazer requisições de hora em hora e coletar todos os dados
resultados <- buscar_publicacoes_por_hora(termo = termo, token = Sys.getenv("BearerBsky"), inicio = inicio, fim = fim, ordenacao = tipo_ordenacao)






# Função recursiva para achatar uma lista de forma profunda, incluindo listas dentro de listas
flatten_lista <- function(lst, prefix = "") {
  dados <- list()
  
  for (nome in names(lst)) {
    novo_nome <- ifelse(prefix == "", nome, paste0(prefix, ".", nome))
    
    # Verifica se o campo é uma lista ou data.frame
    if (is.list(lst[[nome]]) && !is.data.frame(lst[[nome]])) {
      # Se for uma lista de elementos com mesmo comprimento, achatar como colunas adicionais
      if (all(sapply(lst[[nome]], length) == 1)) {
        sub_list <- unlist(lst[[nome]], recursive = FALSE)
        for (sub_nome in names(sub_list)) {
          dados[[paste0(novo_nome, ".", sub_nome)]] <- sub_list[[sub_nome]]
        }
      } else {
        dados <- c(dados, flatten_lista(lst[[nome]], novo_nome))
      }
    } else {
      dados[[novo_nome]] <- lst[[nome]]
    }
  }
  
  return(dados)
}


# Função para transformar a lista de posts em um data.frame achatado
flatten_posts <- function(posts) {
  # Achatar cada post e combiná-los em um data frame
  df_list <- lapply(posts, function(post) {
    flat_post <- flatten_lista(post)
    return(as.data.frame(flat_post, stringsAsFactors = FALSE))
  })
  
  # Combinar todos os data.frames em um único data.frame
  df_posts <- bind_rows(df_list)
  return(df_posts)
}

# Aplicar a função flatten_posts para a lista acumulada de posts
all_flat_posts <- flatten_posts(resultados)

all_flat_posts <- all_flat_posts %>%
  arrange(desc(likeCount))

glimpse(all_flat_posts)






