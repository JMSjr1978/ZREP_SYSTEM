*&---------------------------------------------------------------------*
*& Report ZCONTROLE_ACESSO_CASE
*&---------------------------------------------------------------------*
*& =========================================
*& com um exemplo prático: controle de acesso por cargo
*&
*& COMO USAR:
*& 1. Execute via transação: ZAUT (usuário normal)
*& 2. Execute via transação: ZAUTC (administrador)
*& 3. Execute via SE38: ZCONTROLE_ACESSO_CASE (desenvolvedor)
*&
*& AUTOR: Maurício Júnior
*& DATA: 02/02/2026
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& DECLARAÇÃO DO PROGRAMA
*&---------------------------------------------------------------------*
* REPORT: Define que isto é um programa ABAP
* O nome ZCONTROLE_... começa com Z porque é um programa customizado
REPORT ZCONTROLE_ACESSO_CASE.

*&---------------------------------------------------------------------*
*& 1. TIPOS DE DADOS - O "MOLDE" DAS INFORMAÇÕES
*&---------------------------------------------------------------------*
* TYPES: Cria um novo tipo de dados (como um modelo)
* BEGIN OF ... END OF: Define uma estrutura (vários campos juntos)

* Explicação:
* Imagine que queremos guardar informações sobre setores.
* Cada setor tem um NOME.
* ty_setor é o "formulário" em branco.
TYPES: BEGIN OF ty_setor,        " Começa a definição do tipo setor
         nome TYPE string,        " Campo: nome (tipo texto longo)
       END OF ty_setor.           " Termina a definição

* ty_t_setores é uma "lista" de formulários setor
* TABLE OF: Significa "tabela de" (uma lista)
TYPES: ty_t_setores TYPE TABLE OF ty_setor.

* Tipo para lista de cargos (para search help)
TYPES: BEGIN OF ty_cargo,
         codigo TYPE c LENGTH 1,
         descricao TYPE string,
       END OF ty_cargo.

TYPES: ty_t_cargos TYPE TABLE OF ty_cargo.

* Tipo para search help de setores
TYPES: BEGIN OF ty_setor_sh,
         setor TYPE char20,
       END OF ty_setor_sh.

TYPES: ty_t_setores_sh TYPE TABLE OF ty_setor_sh.

*&---------------------------------------------------------------------*
*& 2. VARIÁVEIS - ONDE GUARDAMOS OS DADOS
*&---------------------------------------------------------------------*
* DATA: Declara variáveis para usar no programa
* Variável = caixa que guarda informação

* lt_setores_permitidos: LISTA de setores permitidos
* lt = Local Table (tabela local)
* ls_setor: LINHA individual da tabela (work area)
* ls = Local Structure (estrutura local)
DATA: lt_setores_permitidos TYPE ty_t_setores,  " Lista de setores
      ls_setor              TYPE ty_setor,      " Um setor por vez
      lv_acesso_permitido   TYPE abap_bool,     " Sim/Não (booleano)
      lv_cargo              TYPE string.        " Cargo do usuário

*&---------------------------------------------------------------------*
*& 3. CONTROLE DAS TRANSAÇÕES
*&---------------------------------------------------------------------*
* Variáveis para saber QUAL transação executou o programa
* gv = Global Variable (variável global)

DATA: gv_modo_admin     TYPE abap_bool,     " Modo administrador (ZAUTC)
      gv_modo_debug     TYPE abap_bool,     " Modo debug (ZAUTD)
      gv_modo_relatorio TYPE abap_bool,     " Modo relatório (ZAUTR)
      gv_modo_export    TYPE abap_bool,     " Modo exportação (ZAUTE)
      gv_modo_normal    TYPE abap_bool.     " Modo normal (ZAUT)

*&---------------------------------------------------------------------*
*& 4. TELA DE SELEÇÃO - ONDE O USUÁRIO DIGITA
*&---------------------------------------------------------------------*
* SELECTION-SCREEN: Cria a tela de entrada do programa
* BLOCK: Agrupa campos visualmente
* WITH FRAME: Coloca uma moldura ao redor
* TITLE: Título do bloco

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  " PARAMETERS: Cria campo onde usuário digita
  " TYPE c: Caractere (letra/número)
  " LENGTH 1: Máximo 1 caractere
  " OBLIGATORY: Campo obrigatório (não pode ficar vazio)
  PARAMETERS: p_cargo  TYPE c LENGTH 1 OBLIGATORY.  " Ex: '1', '2', '3'
  " IMPORTANTE: Para search help funcionar com string, usamos char20
  PARAMETERS: p_setor  TYPE char20 OBLIGATORY.      " Ex: 'MARKETING'
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& 5. BLOCO SOMENTE PARA ADMINISTRADORES (ZAUTC)
*&---------------------------------------------------------------------*
* Este bloco só aparece se executado pela transação ZAUTC
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  " AS CHECKBOX: Cria uma caixa de seleção (✓)
  PARAMETERS: p_testar  AS CHECKBOX DEFAULT 'X' MODIF ID adm,  " Testar acesso
              p_log     AS CHECKBOX MODIF ID adm,              " Gravar log
              pv_todos  AS CHECKBOX MODIF ID adm.              " Ver todos setores
SELECTION-SCREEN END OF BLOCK b2.

*&---------------------------------------------------------------------*
*& 6. BLOCO PARA EXPORTAÇÃO (ZAUTE)
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
  PARAMETERS: p_arquiv TYPE string MODIF ID exp,            " Nome do arquivo
              p_format TYPE c LENGTH 3 DEFAULT 'CSV' MODIF ID exp. " CSV ou XLS
SELECTION-SCREEN END OF BLOCK b3.

*&---------------------------------------------------------------------*
*& 7. TEXTOS DO PROGRAMA (ELEMENTOS DE TEXTO)
*&---------------------------------------------------------------------*
* IMPORTANTE: Os textos TEXT-001, TEXT-002, TEXT-003 devem ser definidos
* na transação SE38 → Ir para → Elementos de texto → Textos de seleção
* Não podem ser atribuídos dinamicamente no código!
*
* Defina manualmente:
* TEXT-001: 'Dados do Usuário'
* TEXT-002: 'Opções Administrativas'
* TEXT-003: 'Opções de Exportação'

*&---------------------------------------------------------------------*
*& 8. INICIALIZAÇÃO - ANTES DA TELA APARECER
*&---------------------------------------------------------------------*
* INITIALIZATION: Executa ANTES da tela de seleção aparecer
* Aqui configuramos e detectamos qual transação chamou

INITIALIZATION.
  " SY-TCODE: Variável do sistema que guarda a transação atual
  " Ex: Se digitar 'ZAUT' no SAP, SY-TCODE = 'ZAUT'

  " Verifica qual transação executou o programa
  PERFORM f_detectar_transacao.

*&---------------------------------------------------------------------*
*& 9. AT SELECTION-SCREEN ON VALUE-REQUEST - SEARCH HELP (F4)
*&---------------------------------------------------------------------*
* Este evento é disparado quando usuário pressiona F4 no campo
* Aqui criamos nossa própria lista de valores (search help)

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_cargo.
  " Quando usuário pressiona F4 no campo Cargo
  PERFORM f_search_help_cargo.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_setor.
  " Quando usuário pressiona F4 no campo Setor
  PERFORM f_search_help_setor.

*&---------------------------------------------------------------------*
*& 10. VALIDAÇÃO DOS DADOS - ANTES DE PROCESSAR
*&---------------------------------------------------------------------*
* AT SELECTION-SCREEN: Valida os dados ANTES do START-OF-SELECTION
* Aqui verificamos se o usuário digitou dados corretos

AT SELECTION-SCREEN.
  " Chamamos rotina de validação
  PERFORM f_validar_dados.

*&---------------------------------------------------------------------*
*& 11. CONTROLE DE VISIBILIDADE DOS CAMPOS
*&---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT: Controla quais campos aparecem na tela
* Executado toda vez que a tela é exibida

AT SELECTION-SCREEN OUTPUT.
  " Oculta campos administrativos se não for modo admin
  IF gv_modo_admin NE abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'ADM'.
        screen-active = 0.  " 0 = invisível
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  " Oculta campos de exportação se não for modo export
  IF gv_modo_export NE abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'EXP'.
        screen-active = 0.  " 0 = invisível
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*&---------------------------------------------------------------------*
*& 12. PROCESSAMENTO PRINCIPAL - DEPOIS QUE O USUÁRIO CLICA EM EXECUTAR
*&---------------------------------------------------------------------*
* START-OF-SELECTION: Evento principal do programa
* Executado quando usuário clica em F8 (Executar)
START-OF-SELECTION.

  " Se for modo debug, ativa análise de performance
  IF gv_modo_debug = abap_true.
    PERFORM f_ativar_debug.
  ENDIF.

  " Se for modo administrador, verifica autorização
  IF gv_modo_admin = abap_true.
    PERFORM f_verificar_autorizacao.
  ENDIF.

  " Converte para MAIÚSCULAS (evita erro de 'marketing' vs 'MARKETING')
  TRANSLATE: p_cargo TO UPPER CASE,
             p_setor TO UPPER CASE.

  " CORREÇÃO: Atribui o valor de p_cargo para lv_cargo
  " Sem esta linha, lv_cargo fica vazio!
  lv_cargo = p_cargo.

  " PASSO 1: Define quais setores este cargo pode acessar
  PERFORM f_define_setores USING lv_cargo
                        CHANGING lt_setores_permitidos.

  " PASSO 2: Verifica se o setor está na lista permitida
  PERFORM f_verifica_acesso USING p_setor
                                  lt_setores_permitidos
                        CHANGING lv_acesso_permitido.

  " PASSO 3: Mostra o resultado na tela
  PERFORM f_exibe_resultado USING lv_cargo
                                  p_setor
                                  lv_acesso_permitido.

  " PASSO 4: Se acesso foi permitido, mostra dados do setor
  IF lv_acesso_permitido = abap_true.
    PERFORM f_exibir_dados_setor USING p_setor.
  ENDIF.

  " PASSO 5: Se for administrador, mostra informações extras
  IF gv_modo_admin = abap_true AND pv_todos = abap_true.
    PERFORM f_exibir_todos_setores USING lt_setores_permitidos.
  ENDIF.

  " PASSO 6: Se for modo exportação, gera arquivo
  IF gv_modo_export = abap_true.
    PERFORM f_exportar_dados USING lv_cargo
                                   p_setor
                                   lv_acesso_permitido.
  ENDIF.

*&---------------------------------------------------------------------*
*& 13. ROTINAS DO PROGRAMA (AS "FUNÇÕES")
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_DETECTAR_TRANSACAO
*&---------------------------------------------------------------------*
* Descrição: Detecta qual transação executou o programa
* Para iniciantes: Imagine que o programa é um restaurante
*  - ZAUT:  Entrada principal (todos entram por aqui)
*  - ZAUTC: Cozinha (só funcionários autorizados)
*  - ZAUTD: Escritório (só gerentes)
*----------------------------------------------------------------------*
FORM f_detectar_transacao.

  " SY-TCODE contém o código da transação atual
  CASE sy-tcode.

    " Transação: ZAUT - Modo normal para usuários comuns
    WHEN 'ZAUT'.
      gv_modo_normal = abap_true.     " Ativa modo normal

    " Transação: ZAUTC - Modo administrador
    WHEN 'ZAUTC'.
      gv_modo_admin = abap_true.      " Ativa modo admin

    " Transação: ZAUTD - Modo debug (para desenvolvedores)
    WHEN 'ZAUTD'.
      gv_modo_debug = abap_true.      " Ativa modo debug

    " Transação: ZAUTR - Modo relatório
    WHEN 'ZAUTR'.
      gv_modo_relatorio = abap_true.  " Ativa modo relatório

    " Transação: ZAUTE - Modo exportação
    WHEN 'ZAUTE'.
      gv_modo_export = abap_true.     " Ativa modo exportação

    " Quando transação está vazia (executado via SE38)
    WHEN ''.
      gv_modo_normal = abap_true.

    " Qualquer outra transação
    WHEN OTHERS.
      gv_modo_normal = abap_true.

  ENDCASE.

ENDFORM. " F_DETECTAR_TRANSACAO

*&---------------------------------------------------------------------*
*& Form F_SEARCH_HELP_CARGO
*&---------------------------------------------------------------------*
* Descrição: Mostra lista de cargos disponíveis quando usuário
*            pressiona F4 no campo Cargo
* Para iniciantes: É como um menu dropdown que aparece
*----------------------------------------------------------------------*
FORM f_search_help_cargo.

  " Tabelas internas para o search help
  DATA: BEGIN OF lt_cargos OCCURS 0,
          codigo TYPE c LENGTH 1,
          descricao TYPE char50,
        END OF lt_cargos.

  DATA: lt_return TYPE TABLE OF ddshretval,
        ls_return TYPE ddshretval.

  " Preenche lista de cargos disponíveis
  lt_cargos-codigo = '1'.
  lt_cargos-descricao = 'Diretor - Acesso Total'.
  APPEND lt_cargos.

  lt_cargos-codigo = '2'.
  lt_cargos-descricao = 'Gerente - Acesso Limitado'.
  APPEND lt_cargos.

  lt_cargos-codigo = '3'.
  lt_cargos-descricao = 'Analista - Acesso Restrito'.
  APPEND lt_cargos.

  " Chama função que mostra o popup de seleção
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CODIGO'        " Campo que retorna
      dynpprog        = sy-repid        " Programa atual
      dynpnr          = sy-dynnr        " Número da tela
      dynprofield     = 'P_CARGO'       " Campo de destino
      value_org       = 'S'             " S = estrutura
      window_title    = 'Selecione o Cargo'  " Título da janela
    TABLES
      value_tab       = lt_cargos       " Tabela com valores
      return_tab      = lt_return       " Tabela de retorno
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  " Se usuário selecionou algo, atualiza o campo
  IF sy-subrc = 0.
    READ TABLE lt_return INTO ls_return INDEX 1.
    IF sy-subrc = 0.
      p_cargo = ls_return-fieldval.
    ENDIF.
  ENDIF.

ENDFORM. " F_SEARCH_HELP_CARGO

*&---------------------------------------------------------------------*
*& Form F_SEARCH_HELP_SETOR
*&---------------------------------------------------------------------*
* Descrição: Mostra lista de setores disponíveis quando usuário
*            pressiona F4 no campo Setor
*----------------------------------------------------------------------*
FORM f_search_help_setor.

  " Tabelas internas para o search help
  DATA: BEGIN OF lt_setores OCCURS 0,
          setor TYPE char20,
        END OF lt_setores.

  DATA: lt_return TYPE TABLE OF ddshretval,
        ls_return TYPE ddshretval.

  " Preenche lista de todos os setores possíveis
  lt_setores-setor = 'FINANCEIRO'.
  APPEND lt_setores.

  lt_setores-setor = 'RH'.
  APPEND lt_setores.

  lt_setores-setor = 'MARKETING'.
  APPEND lt_setores.

  lt_setores-setor = 'TI'.
  APPEND lt_setores.

  lt_setores-setor = 'VENDAS'.
  APPEND lt_setores.

  lt_setores-setor = 'COMPRAS'.
  APPEND lt_setores.

  " Chama função que mostra o popup de seleção
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SETOR'         " Campo que retorna
      dynpprog        = sy-repid        " Programa atual
      dynpnr          = sy-dynnr        " Número da tela
      dynprofield     = 'P_SETOR'       " Campo de destino
      value_org       = 'S'             " S = estrutura
      window_title    = 'Selecione o Setor'  " Título da janela
    TABLES
      value_tab       = lt_setores      " Tabela com valores
      return_tab      = lt_return       " Tabela de retorno
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  " Se usuário selecionou algo, atualiza o campo
  IF sy-subrc = 0.
    READ TABLE lt_return INTO ls_return INDEX 1.
    IF sy-subrc = 0.
      p_setor = ls_return-fieldval.
    ENDIF.
  ENDIF.

ENDFORM. " F_SEARCH_HELP_SETOR

*&---------------------------------------------------------------------*
*& Form F_DEFINE_SETORES
*&---------------------------------------------------------------------*
* Descrição: Define quais setores cada cargo pode acessar
* Para iniciantes: É como uma lista de permissões
*  - Diretor (1) pode tudo
*  - Gerente (2) pode alguns
*  - Analista (3) pode poucos
*----------------------------------------------------------------------*
FORM f_define_setores USING iv_cargo TYPE string
                      CHANGING ct_setores TYPE ty_t_setores.

  " Limpa a lista antes de começar
  CLEAR: ct_setores.

  " CASE: Verifica o valor de iv_cargo
  " É como perguntar: "Qual é o cargo?"
  CASE iv_cargo.

    " Quando o cargo for '1' (Diretor)
    WHEN '1'.
      " Diretor pode acessar TODOS os setores

      " 1. Preenche a work area (ls_setor) com um setor
      ls_setor-nome = 'FINANCEIRO'.
      " 2. APPEND: Adiciona este setor à lista
      APPEND ls_setor TO ct_setores.

      " Repete para outros setores
      ls_setor-nome = 'RH'.
      APPEND ls_setor TO ct_setores.

      ls_setor-nome = 'MARKETING'.
      APPEND ls_setor TO ct_setores.

      ls_setor-nome = 'TI'.
      APPEND ls_setor TO ct_setores.

      ls_setor-nome = 'VENDAS'.
      APPEND ls_setor TO ct_setores.

      ls_setor-nome = 'COMPRAS'.
      APPEND ls_setor TO ct_setores.

      " Se for modo administrador, mostra mensagem
      IF gv_modo_admin = abap_true.
        WRITE: / 'ADMIN: Diretor tem acesso a TODOS os setores.'.
      ENDIF.

    " Quando o cargo for '2' (Gerente)
    WHEN '2'.
      " Gerente pode acessar alguns setores
      ls_setor-nome = 'FINANCEIRO'.
      APPEND ls_setor TO ct_setores.

      ls_setor-nome = 'MARKETING'.
      APPEND ls_setor TO ct_setores.

      ls_setor-nome = 'VENDAS'.
      APPEND ls_setor TO ct_setores.

    " Quando o cargo for '3' (Analista)
    WHEN '3'.
      " Analista só pode acessar Marketing
      ls_setor-nome = 'MARKETING'.
      APPEND ls_setor TO ct_setores.

    " Quando for qualquer outro cargo não esperado
    WHEN OTHERS.
      " MESSAGE: Mostra mensagem de erro
      WRITE: / 'ERRO: Cargo', iv_cargo, 'não reconhecido!' COLOR col_negative.
      " STOP: Para a execução do programa
      STOP.

  ENDCASE.

ENDFORM. " F_DEFINE_SETORES

*&---------------------------------------------------------------------*
*& Form F_VERIFICA_ACESSO
*&---------------------------------------------------------------------*
* Descrição: Verifica se o setor está na lista permitida
* Para iniciantes: É como um segurança conferindo uma lista
*  - Pega cada setor permitido
*  - Compara com o setor que o usuário quer
*  - Se encontrar, libera o acesso
*----------------------------------------------------------------------*
FORM f_verifica_acesso USING iv_setor     TYPE char20
                             it_setores   TYPE ty_t_setores
                       CHANGING cv_acesso TYPE abap_bool.

  " Inicializa como FALSO (acesso negado)
  " ABAP_FALSE = ' ' (vazio), ABAP_TRUE = 'X'
  cv_acesso = abap_false.

  " LOOP AT: Para CADA item na lista it_setores
  " INTO ls_setor: Guarda o item atual em ls_setor
  LOOP AT it_setores INTO ls_setor.

    " IF: Se o nome do setor atual for igual ao setor informado
    IF ls_setor-nome = iv_setor.
      " Acesso PERMITIDO!
      cv_acesso = abap_true.

      " EXIT: Sai do LOOP (já encontrou, não precisa continuar)
      EXIT.
    ENDIF.

  ENDLOOP. " Fim do loop

  " Se for modo debug, mostra detalhes
  IF gv_modo_debug = abap_true.
    WRITE: / 'DEBUG: Setor informado:', iv_setor.
    WRITE: / 'DEBUG: Total de setores permitidos:', lines( it_setores ).
    WRITE: / 'DEBUG: Acesso permitido?', cv_acesso.
  ENDIF.

ENDFORM. " F_VERIFICA_ACESSO

*&---------------------------------------------------------------------*
*& Form F_EXIBE_RESULTADO
*&---------------------------------------------------------------------*
* Descrição: Mostra o resultado na tela de forma bonita
* Para iniciantes: É como imprimir um relatório
*----------------------------------------------------------------------*
FORM f_exibe_resultado USING iv_cargo   TYPE string
                             iv_setor   TYPE char20
                             iv_acesso  TYPE abap_bool.

  " Se for modo relatório, formata como relatório
  IF gv_modo_relatorio = abap_true.
    PERFORM f_exibir_como_relatorio USING iv_cargo iv_setor iv_acesso.
    RETURN.
  ENDIF.

  " WRITE: Escreve na tela
  " /: Pula uma linha
  " COLOR: Muda a cor do texto

  WRITE: /.
  WRITE: / '═══════════════════════════════════════════════════'
          COLOR col_heading.
  WRITE: / '          RESULTADO DA VERIFICAÇÃO DE ACESSO'
          COLOR col_heading.
  WRITE: / '═══════════════════════════════════════════════════'
          COLOR col_heading.
  WRITE: /.

  " Mostra os dados informados
  WRITE: / 'Cargo informado:    ', iv_cargo
          COLOR col_key.
  WRITE: / 'Setor solicitado:   ', iv_setor
          COLOR col_key.
  WRITE: /.

  " Mostra o resultado
  IF iv_acesso = abap_true.
    " Acesso PERMITIDO - cor verde
    WRITE: / 'STATUS: ACESSO PERMITIDO'
            COLOR col_positive.
    WRITE: / '    O usuário tem autorização para acessar este setor.'.
  ELSE.
    " Acesso NEGADO - cor vermelha
    WRITE: / 'STATUS: ACESSO NEGADO'
            COLOR col_negative.
    WRITE: / 'O usuário NÃO tem autorização para acessar este setor.'.

    " Se for administrador, sugere ação
    IF gv_modo_admin = abap_true.
      WRITE: / '    ADMIN: Considere adicionar este setor ao cargo.'.
    ENDIF.
  ENDIF.

  WRITE: /.
  WRITE: / '═══════════════════════════════════════════════════'
          COLOR col_heading.

  " Se for modo debug, mostra informações técnicas
  IF gv_modo_debug = abap_true.
    WRITE: /.
    WRITE: / '--- INFORMAÇÕES TÉCNICAS (DEBUG) ---' COLOR col_total.
    WRITE: / 'Transação atual: ', sy-tcode.
    WRITE: / 'Usuário: ', sy-uname.
    WRITE: / 'Data: ', sy-datum, 'Hora: ', sy-uzeit.
  ENDIF.

ENDFORM. " F_EXIBE_RESULTADO

*&---------------------------------------------------------------------*
*& Form F_EXIBIR_DADOS_SETOR
*&---------------------------------------------------------------------*
* Descrição: Mostra informações detalhadas do setor
* Esta rotina é executada APENAS quando o acesso foi permitido
* Mostra dados financeiros, estatísticas e KPIs do setor
* MOEDA: EUR (Euro) conforme solicitado
*----------------------------------------------------------------------*
FORM f_exibir_dados_setor USING iv_setor TYPE char20.

  " Só mostra se acesso foi permitido
  " Esta verificação garante que dados sensíveis só apareçam
  " para quem tem autorização
  IF lv_acesso_permitido = abap_false.
    RETURN.
  ENDIF.

  WRITE: /.
  WRITE: / '═══════════════════════════════════════════════════'
          COLOR col_heading.
  WRITE: / '          DADOS DO SETOR:', iv_setor
          COLOR col_heading.
  WRITE: / '═══════════════════════════════════════════════════'
          COLOR col_heading.
  WRITE: /.

  " CASE para mostrar dados específicos de cada setor
  " Cada setor tem suas próprias métricas e KPIs relevantes
  CASE iv_setor.

    WHEN 'VENDAS'.
      WRITE: / 'Faturamento Mensal: € 1.250.000,00' COLOR col_positive.
      WRITE: / 'Meta do Trimestre:  € 3.500.000,00'.
      WRITE: / 'Atingimento:        89%' COLOR col_normal.
      WRITE: / 'Vendedores Ativos:  45'.
      WRITE: / 'Novos Clientes:     23'.
      WRITE: /.
      WRITE: / 'Top 3 Produtos Vendidos:'.
      WRITE: / '  1. Produto A - € 340.000,00'.
      WRITE: / '  2. Produto B - € 280.000,00'.
      WRITE: / '  3. Produto C - € 195.000,00'.

    WHEN 'FINANCEIRO'.
      WRITE: / 'Saldo em Caixa:     € 2.890.000,00' COLOR col_positive.
      WRITE: / 'Contas a Receber:   € 1.450.000,00'.
      WRITE: / 'Contas a Pagar:     €   780.000,00' COLOR col_negative.
      WRITE: / 'Lucro Líquido (Mês):€   520.000,00' COLOR col_positive.
      WRITE: / 'Margem de Lucro:    18.5%'.
      WRITE: /.
      WRITE: / 'Análise de Fluxo de Caixa:'.
      WRITE: / '  • Entradas:  € 2.230.000,00'.
      WRITE: / '  • Saídas:    € 1.710.000,00'.
      WRITE: / '  • Saldo:     €   520.000,00' COLOR col_positive.

    WHEN 'RH'.
      WRITE: / 'Total de Funcionários:    287'.
      WRITE: / 'Novas Contratações (Mês): 12'.
      WRITE: / 'Demissões (Mês):          5'.
      WRITE: / 'Taxa de Rotatividade:     4.2%'.
      WRITE: / 'Férias Programadas:       18'.
      WRITE: / 'Treinamentos em Andamento: 3'.
      WRITE: /.
      WRITE: / 'Folha de Pagamento:'.
      WRITE: / '  • Salários:         € 485.000,00'.
      WRITE: / '  • Benefícios:       €  92.000,00'.
      WRITE: / '  • Encargos:         € 145.500,00'.
      WRITE: / '  • Total:            € 722.500,00' COLOR col_total.

    WHEN 'MARKETING'.
      WRITE: / 'Campanhas Ativas:         7'.
      WRITE: / 'Investimento Mensal:      € 85.000,00'.
      WRITE: / 'Leads Gerados:            1.245'.
      WRITE: / 'Taxa de Conversão:        3.8%'.
      WRITE: / 'Engajamento Redes Sociais: +15%' COLOR col_positive.
      WRITE: /.
      WRITE: / 'ROI por Canal:'.
      WRITE: / '  • Google Ads:     287%' COLOR col_positive.
      WRITE: / '  • Facebook:       195%'.
      WRITE: / '  • Email Marketing: 412%' COLOR col_positive.
      WRITE: / '  • Instagram:      156%'.

    WHEN 'TI'.
      WRITE: / 'Chamados Abertos:         23'.
      WRITE: / 'Chamados Fechados (Mês):  156'.
      WRITE: / 'Tempo Médio Resolução:    2.5h'.
      WRITE: / 'Projetos em Andamento:    4'.
      WRITE: / 'Sistemas Críticos:        Todos OK' COLOR col_positive.
      WRITE: /.
      WRITE: / 'Infraestrutura:'.
      WRITE: / '  • Servidores:      24 (100% uptime)'.
      WRITE: / '  • Storage:         85% utilizado'.
      WRITE: / '  • Backup:          Diário (OK)' COLOR col_positive.
      WRITE: / '  • Segurança:       Sem incidentes'.
      WRITE: /.
      WRITE: / 'Custos Mensais:       € 45.000,00'.

    WHEN 'COMPRAS'.
      WRITE: / 'Pedidos Processados:      89'.
      WRITE: / 'Valor Total Compras:      € 450.000,00'.
      WRITE: / 'Fornecedores Ativos:      34'.
      WRITE: / 'Economia Negociações:     € 28.000,00' COLOR col_positive.
      WRITE: / 'Itens em Estoque Baixo:   12' COLOR col_total.
      WRITE: /.
      WRITE: / 'Principais Fornecedores:'.
      WRITE: / '  1. Fornecedor A - € 120.000,00'.
      WRITE: / '  2. Fornecedor B - €  98.000,00'.
      WRITE: / '  3. Fornecedor C - €  75.000,00'.
      WRITE: /.
      WRITE: / 'Prazo Médio Entrega:      7 dias'.
      WRITE: / 'Taxa de Devolução:        1.2%'.

    WHEN OTHERS.
      WRITE: / 'Dados não disponíveis para este setor.'.

  ENDCASE.

  WRITE: /.
  WRITE: / '═══════════════════════════════════════════════════'
          COLOR col_heading.
  WRITE: /.

  " Se for modo administrador, mostra observações adicionais
  IF gv_modo_admin = abap_true.
    WRITE: / '--- OBSERVAÇÕES ADMINISTRATIVAS ---' COLOR col_group.
    WRITE: / 'Última atualização: ', sy-datum, 'às', sy-uzeit.
    WRITE: / 'Dados baseados no período: ', sy-datum(6).
    WRITE: /.
  ENDIF.

ENDFORM. " F_EXIBIR_DADOS_SETOR

*&---------------------------------------------------------------------*
*& Form F_VALIDAR_DADOS
*&---------------------------------------------------------------------*
* Descrição: Valida os dados digitados pelo usuário
*----------------------------------------------------------------------*
FORM f_validar_dados.

  " Verifica se cargo é válido (só permite 1, 2, 3)
  " Usa CA (Contains Any) - verifica se p_cargo contém '1' OU '2' OU '3'
  IF NOT p_cargo CA '123'.
    " Mostra mensagem de erro
    MESSAGE 'Cargo inválido! Use: 1=Diretor, 2=Gerente, 3=Analista' TYPE 'E'.
  ENDIF.

  " Verifica se setor não está vazio
  IF p_setor IS INITIAL.
    MESSAGE 'Setor é obrigatório!' TYPE 'E'.
  ENDIF.

  " Verifica se setor existe na lista válida
  DATA: lv_setor_valido TYPE abap_bool.
  lv_setor_valido = abap_false.

  " Lista de setores válidos
  IF p_setor = 'FINANCEIRO' OR
     p_setor = 'RH' OR
     p_setor = 'MARKETING' OR
     p_setor = 'TI' OR
     p_setor = 'VENDAS' OR
     p_setor = 'COMPRAS'.
    lv_setor_valido = abap_true.
  ENDIF.

  IF lv_setor_valido = abap_false.
    MESSAGE 'Setor inválido! Use F4 para ver setores disponíveis.' TYPE 'E'.
  ENDIF.

ENDFORM. " F_VALIDAR_DADOS

*&---------------------------------------------------------------------*
*& Form F_VERIFICAR_AUTORIZACAO
*&---------------------------------------------------------------------*
* Descrição: Verifica se usuário tem autorização para transação admin
*----------------------------------------------------------------------*
FORM f_verificar_autorizacao.

  " AUTHORITY-CHECK: Verifica autorização no SAP
  " S_TCODE: Objeto de autorização para transações
  AUTHORITY-CHECK OBJECT 'S_TCODE'
    ID 'TCD' FIELD 'ZAUTC'.  " Verifica acesso à transação ZAUTC

  " SY-SUBRC: Código de retorno
  " 0 = Autorizado, ≠0 = Não autorizado
  IF sy-subrc <> 0.
    " Usuário NÃO tem autorização
    WRITE: / 'ERRO: Usuário', sy-uname, 'não autorizado para modo Admin!'
            COLOR col_negative.
    MESSAGE 'Acesso negado - sem autorização' TYPE 'E'.
  ENDIF.

ENDFORM. " F_VERIFICAR_AUTORIZACAO

*&---------------------------------------------------------------------*
*& Form F_EXIBIR_TODOS_SETORES
*&---------------------------------------------------------------------*
* Descrição: Mostra todos os setores que o cargo pode acessar
*----------------------------------------------------------------------*
FORM f_exibir_todos_setores USING it_setores TYPE ty_t_setores.

  WRITE: /.
  WRITE: / '--- TODOS OS SETORES PERMITIDOS PARA ESTE CARGO ---'
          COLOR col_group.
  WRITE: /.

  " LOOP AT com WRITE: Mostra cada setor
  LOOP AT it_setores INTO ls_setor.
    WRITE: / '   • ', ls_setor-nome COLOR col_normal.
  ENDLOOP.

  " LINES: Função que conta quantos itens tem na tabela
  WRITE: /.
  WRITE: / 'Total de setores permitidos: ',
          lines( it_setores ) COLOR col_total.

ENDFORM. " F_EXIBIR_TODOS_SETORES

*&---------------------------------------------------------------------*
*& Form F_EXIBIR_COMO_RELATORIO
*&---------------------------------------------------------------------*
* Descrição: Formata saída como relatório formal
*----------------------------------------------------------------------*
FORM f_exibir_como_relatorio USING iv_cargo TYPE string
                                    iv_setor TYPE char20
                                    iv_acesso TYPE abap_bool.

  DATA: lv_data TYPE string.

  " Formata data atual
  CONCATENATE sy-datum+6(2)  " Dia
              sy-datum+4(2)  " Mês
              sy-datum(4)    " Ano
         INTO lv_data SEPARATED BY '/'.

  WRITE: / 'Relatório de Controle de Acesso - SAP'.
  WRITE: / '======================================'.
  WRITE: /.
  WRITE: / 'Data:', lv_data, '          Hora:', sy-uzeit.
  WRITE: / 'Usuário:', sy-uname, '       Transação:', sy-tcode.
  WRITE: /.
  WRITE: / 'Cargo Analisado: ', iv_cargo.
  WRITE: / 'Setor Solicitado:', iv_setor.
  WRITE: /.

  IF iv_acesso = abap_true.
    WRITE: / 'DECISÃO: AUTORIZADO'.
    WRITE: / 'JUSTIFICATIVA: Cargo possui permissão para este setor.'.
  ELSE.
    WRITE: / 'DECISÃO: NÃO AUTORIZADO'.
    WRITE: / 'JUSTIFICATIVA: Cargo não possui permissão para este setor.'.
  ENDIF.

  WRITE: / '________________________________'.
  WRITE: / 'Assinatura Eletrônica do Sistema'.

ENDFORM. " F_EXIBIR_COMO_RELATORIO

*&---------------------------------------------------------------------*
*& Form F_ATIVAR_DEBUG
*&---------------------------------------------------------------------*
* Descrição: Ativa ferramentas de debug
*----------------------------------------------------------------------*
FORM f_ativar_debug.

  " SET RUN TIME: Ativa análise de performance
  SET RUN TIME ANALYZER ON.

  " Mostra mensagem informativa
  WRITE: / 'MODO DEBUG ATIVADO' COLOR col_group.
  WRITE: / 'Ferramentas de análise habilitadas.'.
  WRITE: /.

ENDFORM. " F_ATIVAR_DEBUG

*&---------------------------------------------------------------------*
*& Form F_EXPORTAR_DADOS
*&---------------------------------------------------------------------*
* Descrição: Exporta dados para arquivo (simulação)
*----------------------------------------------------------------------*
FORM f_exportar_dados USING iv_cargo TYPE string
                             iv_setor TYPE char20
                             iv_acesso TYPE abap_bool.

  DATA: lv_resultado TYPE string.

  " Define resultado como texto
  IF iv_acesso = abap_true.
    lv_resultado = 'PERMITIDO'.
  ELSE.
    lv_resultado = 'NEGADO'.
  ENDIF.

  WRITE: /.
  WRITE: / '═══════════════════════════════════════════════════'
          COLOR col_heading.
  WRITE: / 'EXPORTAÇÃO DE DADOS' COLOR col_heading.
  WRITE: / '═══════════════════════════════════════════════════'
          COLOR col_heading.
  WRITE: /.

  WRITE: / 'Arquivo gerado: ', p_arquiv.
  WRITE: / 'Formato: ', p_format.
  WRITE: /.
  WRITE: / 'Conteúdo exportado:'.
  WRITE: / 'Cargo;Setor;Resultado;Data;Usuário'.
  WRITE: / iv_cargo, ';', iv_setor, ';', lv_resultado, ';',
          sy-datum, ';', sy-uname.

  WRITE: /.
  WRITE: / 'Exportação concluída com sucesso!' COLOR col_positive.

ENDFORM. " F_EXPORTAR_DADOS
