" ****************************************************************
" * PROGRAMA     : ZREP_FUNCIONARIOS
" * DESCRIÇÃO    : Sistema de Gerenciamento de Funcionários
" * AUTOR        : Maurício Júnior - Desenvolvedor ABAP
" * DATA CRIAÇÃO : 15.01.2026
" ****************************************************************

REPORT ZREP_FUNCIONARIOS.

TABLES: zfuncionarios.

DATA: gt_funcionarios TYPE TABLE OF zfuncionarios,
      gs_funcionario  TYPE zfuncionarios,
      gv_total_func   TYPE i,
      gv_total_ganho  TYPE p DECIMALS 2,
      gv_linha        TYPE i VALUE 1,
      gv_opcao        TYPE c LENGTH 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:
  p_cons RADIOBUTTON GROUP opc DEFAULT 'X' USER-COMMAND cmd,
  p_incl RADIOBUTTON GROUP opc,
  p_edit RADIOBUTTON GROUP opc,
  p_del  RADIOBUTTON GROUP opc.
SELECTION-SCREEN END OF BLOCK b1.
SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS:
  s_id    FOR zfuncionarios-id_func  MODIF ID gr1,
  s_cargo FOR zfuncionarios-cargo    MODIF ID gr1,
  s_dtadm FOR zfuncionarios-data_adm MODIF ID gr1.
PARAMETERS:
  p_nome TYPE zfuncionarios-nome MODIF ID gr1.
SELECTION-SCREEN END OF BLOCK b2.
SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS:
  p_id_n   TYPE zfuncionarios-id_func  MODIF ID gr2,
  p_nome_n TYPE zfuncionarios-nome     MODIF ID gr2,
  p_nif_n  TYPE zfuncionarios-nif      MODIF ID gr2,
  p_cargo  TYPE zfuncionarios-cargo    MODIF ID gr2,
  p_ganho  TYPE zfuncionarios-ganho    MODIF ID gr2,
  p_data_n TYPE zfuncionarios-data_adm MODIF ID gr2.
SELECTION-SCREEN END OF BLOCK b3.
SKIP.

AT SELECTION-SCREEN OUTPUT.
  PERFORM controlar_visibilidade.

INITIALIZATION.


START-OF-SELECTION.
  CASE 'X'.
    WHEN p_cons.
      PERFORM consultar.
    WHEN p_incl.
      PERFORM incluir.
    WHEN p_edit.
      PERFORM editar.
    WHEN p_del.
      PERFORM excluir.
    WHEN OTHERS.
      " Nada selecionado - não faz nada
  ENDCASE.

FORM controlar_visibilidade.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'GR1'.
        screen-active = p_cons.
      WHEN 'GR2'.
        IF p_incl = 'X' OR p_edit = 'X' OR p_del = 'X'.
          screen-active = 'X'.
        ELSE.
          screen-active = ' '.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.

FORM consultar.
  DATA: lv_nome TYPE string.

  CLEAR: lv_nome.

  IF p_nome IS NOT INITIAL.
    lv_nome = |%{ condense( p_nome ) }%|.
  ENDIF.

  SELECT *
    FROM zfuncionarios
    WHERE id_func   IN @s_id
      AND cargo     IN @s_cargo
      AND data_adm  IN @s_dtadm
      AND ( @p_nome IS INITIAL OR nome LIKE @lv_nome )
    ORDER BY nome, id_func
    INTO TABLE @gt_funcionarios.

  IF sy-subrc <> 0.
    MESSAGE 'Nenhum funcionário encontrado!' TYPE 'I' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  DESCRIBE TABLE gt_funcionarios LINES gv_total_func.
  CLEAR gv_total_ganho.

  LOOP AT gt_funcionarios INTO gs_funcionario.
    ADD gs_funcionario-ganho TO gv_total_ganho.
  ENDLOOP.

  PERFORM exibir_relatorio.
ENDFORM.

FORM exibir_relatorio.
  WRITE: / '=== RELATÓRIO DE FUNCIONÁRIOS ==='.
  WRITE: /.
  WRITE: / 'Total:', gv_total_func, 'funcionários'.
  WRITE: / 'Total Ganhos:', gv_total_ganho, 'EUR'.
  WRITE: /.
  ULINE.
  LOOP AT gt_funcionarios INTO gs_funcionario.
    WRITE: / .
    WRITE: / 'ID:', gs_funcionario-id_func.
    WRITE: / 'Nome:', gs_funcionario-nome.
    WRITE: / 'NIF:', gs_funcionario-nif.
    WRITE: / 'Cargo:', gs_funcionario-cargo.
    WRITE: / 'Ganho:', gs_funcionario-ganho, 'EUR'.
    WRITE: / 'Data:', gs_funcionario-data_adm DD/MM/YYYY.
    WRITE: /.
  ENDLOOP.
ENDFORM.

FORM incluir.
  CLEAR gs_funcionario.
  PERFORM validar_campos_obrigatorios.

  SELECT SINGLE * FROM zfuncionarios
    INTO @gs_funcionario
    WHERE id_func = @p_id_n.

  IF sy-subrc = 0.
    MESSAGE 'ID já existe! Escolha outro ID.' TYPE 'E'.
  ELSEIF sy-subrc = 4.
    " OK - ID disponível
  ELSE.
    MESSAGE 'Erro ao verificar duplicidade de ID.' TYPE 'E'.
  ENDIF.

  gs_funcionario-id_func   = p_id_n.
  gs_funcionario-nome      = p_nome_n.
  gs_funcionario-nif       = p_nif_n.
  gs_funcionario-cargo     = p_cargo.
  gs_funcionario-ganho     = p_ganho.
  gs_funcionario-data_adm  = p_data_n.

  INSERT zfuncionarios FROM gs_funcionario.

  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE 'Funcionário incluído com sucesso!' TYPE 'S'.
    PERFORM limpar_campos_manutencao.
    CLEAR gs_funcionario.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Erro ao incluir funcionário!' TYPE 'E'.
  ENDIF.
ENDFORM.

FORM editar.
  CLEAR gs_funcionario.
  PERFORM validar_campos_obrigatorios.

  SELECT SINGLE * FROM zfuncionarios
    INTO @gs_funcionario
    WHERE id_func = @p_id_n.

  IF sy-subrc <> 0.
    MESSAGE 'Funcionário não encontrado! Verifique o ID.' TYPE 'E'.
  ENDIF.

  gs_funcionario-nome      = p_nome_n.
  gs_funcionario-nif       = p_nif_n.
  gs_funcionario-cargo     = p_cargo.
  gs_funcionario-ganho     = p_ganho.
  gs_funcionario-data_adm  = p_data_n.

  UPDATE zfuncionarios FROM gs_funcionario.

  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE 'Funcionário atualizado com sucesso!' TYPE 'S'.
    PERFORM limpar_campos_manutencao.
    CLEAR gs_funcionario.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Erro ao atualizar funcionário!' TYPE 'E'.
  ENDIF.
ENDFORM.

FORM excluir.
  DATA: lv_answer TYPE c VALUE ' '.

  CLEAR gs_funcionario.

  IF p_id_n IS INITIAL.
    MESSAGE 'Informe o ID do funcionário para excluir!' TYPE 'E'.
  ENDIF.

  SELECT SINGLE * FROM zfuncionarios
    INTO @gs_funcionario
    WHERE id_func = @p_id_n.

  IF sy-subrc <> 0.
    MESSAGE 'Funcionário não encontrado! Verifique o ID informado.' TYPE 'E'.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação de Exclusão'
      text_question         = 'Deseja excluir este funcionário?'
      text_button_1         = 'Sim'
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'Não'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = 'X'
    IMPORTING
      answer                = lv_answer.

  IF lv_answer = '1'.
    DELETE FROM zfuncionarios WHERE id_func = p_id_n.

    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE 'Funcionário excluído com sucesso!' TYPE 'S'.
      PERFORM limpar_campos_manutencao.
      CLEAR gs_funcionario.
    ELSE.
      ROLLBACK WORK.
      MESSAGE 'Erro ao excluir funcionário!' TYPE 'E'.
    ENDIF.
  ELSEIF lv_answer = '2'.
    MESSAGE 'Exclusão cancelada pelo usuário.' TYPE 'I'.
  ELSE.
    MESSAGE 'Operação cancelada.' TYPE 'I'.
  ENDIF.
ENDFORM.

FORM validar_campos_obrigatorios.
  IF p_id_n    IS INITIAL OR
     p_nome_n  IS INITIAL OR
     p_cargo   IS INITIAL OR
     p_ganho   IS INITIAL OR
     p_data_n  IS INITIAL.
    MESSAGE 'Preencha todos os campos obrigatórios!' TYPE 'E'.
  ENDIF.
ENDFORM.

FORM limpar_campos_manutencao.
  CLEAR: p_id_n, p_nome_n, p_nif_n, p_cargo, p_ganho, p_data_n.
ENDFORM.

FORM validar_data USING iv_data TYPE d.
  " Validação básica de data - implementação simples
  " Pode ser expandida conforme necessidade
  IF iv_data IS INITIAL.
    MESSAGE 'Data inválida!' TYPE 'E'.
  ENDIF.
ENDFORM.
