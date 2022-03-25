*----------------------------------------------------------------------*
*       CLASS ZCL_ABS_EDI_ALV_CTRL DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_ABS_EDI_ALV_CTRL definition
  public
  abstract
  create public .

public section.
  type-pools ABAP .

  constants:
    BEGIN OF c_popup_confirm_ret, "Retorno POP_UP_CONFIRM
        sim      TYPE char01 VALUE '1',
        nao      TYPE char01 VALUE '2',
        cancelar TYPE char01 VALUE 'A',
      END OF c_popup_confirm_ret .

  events ATUALIZAR_ALV .       "Atualizar AlV

  class-methods CONVERT_MSGSY_2_BAPIRET
    returning
      value(RS_RETURN) type BAPIRET2 .
*     "=>Carregar informações
  methods CARREGAR_INFO
  abstract
    exporting
      !ET_RETURN type BAPIRETTAB .
  methods CLEAR_ALL .
  methods CONSTRUCTOR .
  methods IS_ONLY_VIEW
  final
    returning
      value(RV_ONLY_VIEW) type ABAP_BOOL .
*      "=>Evento PAI
  methods ON_PAI
    importing
      !IV_UCOMM type SYUCOMM .
*     "=>Evento PBO
  methods ON_PBO
  abstract .
*     "=>Ao requisitar a atualização de dados da lista
  methods ON_REFRESH .
*     "=>Informa se será somente exibição
  methods SET_ONLY_VIEW
  final
    importing
      !IV_ONLY_VIEW type ABAP_BOOL .
protected section.

  types:
    BEGIN OF y_base_alv,
        cell_type TYPE lvc_t_styl,
      END OF y_base_alv .

  constants:
    BEGIN OF c_field_base,
        cell_type TYPE fieldname VALUE 'CELL_TYPE',
      END OF c_field_base .
  constants C_PERCENTAGE_FIX type I value 75 ##NO_TEXT.                             "Percentual fixo (progress gui)
  constants:
    BEGIN OF c_sel_mode_alv,  "Tipo de seleção linha ALV
        standard         TYPE lvc_libox VALUE IS INITIAL, "same as 'B'(standard)
        several_rows_btn TYPE lvc_libox VALUE 'A', "select several rows(via buttons on the left side) and several columns
        single_rows      TYPE lvc_libox VALUE 'B', "select single rows and several columns
        several_rows     TYPE lvc_libox VALUE 'C', "select several rows and several columns
        select_cells     TYPE lvc_libox VALUE 'D', "select cells as you like
      END OF c_sel_mode_alv .
  constants:
    BEGIN OF c_variant_alv,
        nenhum       TYPE char01 VALUE space, "Não será permitido mudança
        by_user      TYPE char01 VALUE 'U', "Somente Por usuário
        all_and_user TYPE char01 VALUE 'A', "para Todos e por usuário
        by_all       TYPE char01 VALUE 'X', "Somente Todos
      END OF c_variant_alv .
  data MO_ALV type ref to CL_ISSR_BASI_ALV_GRID .
  data MO_CONTAINER type ref to CL_GUI_CONTAINER .
  data MT_FCAT_ALV type LVC_T_FCAT .      "Catalog ALV
  data MT_FIELD_F4_CUSTOM type FIELDNAME_T .

*      "=>Refresh no ALV
  methods ATUALIZAR_DADOS_ALV .
  methods CARREGAR_FCAT
    importing
      !IO_STRUCT type ref to CL_ABAP_STRUCTDESCR .
*     "=>Exibir retorno de processamentos
  methods EXIBIR_BAPIRET
    importing
      !IT_RETURN type BAPIRETTAB .
*     "=>Formatar colunas ALV
  methods FORMATAR_FCAT
  abstract
    changing
      !CS_FCAT_ALV type LVC_S_FCAT .
  methods GET_REMOVE_FUNC_TOOLBAR
    returning
      value(RT_FUNCTIONS) type UI_FUNCTIONS .
  methods GET_SEL_MODE_LINE
    returning
      value(RV_SEL_MODE) type LVC_LIBOX .
  methods GET_TP_VARIANT_ALV
    returning
      value(RV_TP_VARIANT_ALV) type CHAR01 .
*     "=>Manipular evento de double clique
  methods ON_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO .
*     "=>Manipular evento para editar toolbar alv
  methods ON_HANDLE_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
*     "=>Manipular evento para user command ALV
  methods ON_HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
*    "=>Manipula evento ao clicar em um link (hotspot)
  methods ON_HOTSPOT_CLICK
  abstract
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID .
*      "=>Requisitar ALV
  methods REQUISITAR_ALV
    importing
      !IV_HANDLE type SLIS_HANDL
      !IO_CONTAINER type ref to CL_GUI_CONTAINER optional
      !IV_VARIANT type SLIS_VARI optional
    changing
      !CT_DATA_ALV type STANDARD TABLE .
  methods REQUISITAR_DOMAIN_F4
    importing
      !IV_VALUE type ANY .
*     "=>informar os manipuladores de evento
  methods SET_HANDLER_EVT_ALV .
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_ucomm,
        atualizar_lista TYPE syucomm VALUE 'REFRESH',
      END OF c_ucomm .
    DATA mv_only_view TYPE abap_bool VALUE abap_false.      "#EC NOTEXT
ENDCLASS.



CLASS ZCL_ABS_EDI_ALV_CTRL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->ATUALIZAR_DADOS_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD atualizar_dados_alv.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Atualizar dados do GUI ALV
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    DATA:
      ls_stbl TYPE lvc_s_stbl
      .

    IF me->mo_alv IS NOT BOUND.
      RETURN.

    ENDIF.
    ls_stbl-row = abap_true.
    ls_stbl-col = abap_true.

    me->mo_alv->refresh_table_display(
      is_stable = ls_stbl
*      i_soft_refresh = abap_true
    ).
  ENDMETHOD.                    "atualizar_dados_alv


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->CARREGAR_FCAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_STRUCT                      TYPE REF TO CL_ABAP_STRUCTDESCR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD carregar_fcat.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Carregar FCAT
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    DATA:
      lt_ddfields   TYPE cl_abap_structdescr=>component_table,

      lo_rtti_struc LIKE io_struct,
      lo_rtti_elemt TYPE REF TO cl_abap_elemdescr,

      ls_fcat_alv   LIKE LINE OF me->mt_fcat_alv,
      ls_dfies      TYPE dfies,

      lv_tabname    TYPE string
      .
    FIELD-SYMBOLS:
       <ls_ddfields> LIKE LINE OF lt_ddfields.

    CHECK io_struct IS BOUND.

    IF io_struct->is_ddic_type( ) EQ abap_true.
      lv_tabname = io_struct->get_relative_name( ).

    ENDIF.

    lt_ddfields = io_struct->get_components( ).


    LOOP AT lt_ddfields ASSIGNING <ls_ddfields>.
      CLEAR: ls_fcat_alv.

      IF <ls_ddfields>-as_include EQ abap_true
      OR <ls_ddfields>-type->kind EQ cl_abap_structdescr=>kind_struct.

        lo_rtti_struc ?= <ls_ddfields>-type.

        me->carregar_fcat( lo_rtti_struc ).

      ELSE.

        TRY .
            lo_rtti_elemt ?= <ls_ddfields>-type.
          CATCH cx_root.
            CONTINUE.

        ENDTRY.

        IF lo_rtti_elemt->is_ddic_type( ) EQ abap_true.
          ls_dfies = lo_rtti_elemt->get_ddic_field( p_langu = sy-langu ).

          MOVE-CORRESPONDING ls_dfies TO ls_fcat_alv.
          CLEAR ls_fcat_alv-tabname.
          ls_fcat_alv-domname   = lo_rtti_elemt->get_relative_name( ).
          ls_fcat_alv-fieldname = <ls_ddfields>-name.
          ls_fcat_alv-seltext   = ls_dfies-scrtext_l.
          ls_fcat_alv-scrtext_l = ls_dfies-scrtext_l.
          ls_fcat_alv-scrtext_m = ls_dfies-scrtext_m.
          ls_fcat_alv-scrtext_s = ls_dfies-scrtext_m.
          ls_fcat_alv-ref_table = ls_dfies-reftable.
          ls_fcat_alv-ref_field = ls_dfies-reffield.

          IF  ls_dfies-fieldtext  IS NOT INITIAL
          AND ls_fcat_alv-seltext IS INITIAL.
            ls_fcat_alv-seltext = ls_dfies-fieldtext.
            ls_fcat_alv-reptext = ls_dfies-fieldtext.
          ENDIF.

          IF ls_dfies-convexit IS NOT INITIAL.
            ls_fcat_alv-edit_mask = '==' && ls_dfies-convexit.
          ENDIF.
          ls_fcat_alv-f4availabl = abap_false.

          IF lv_tabname IS NOT INITIAL.
            ls_fcat_alv-ref_table = lv_tabname.
            ls_fcat_alv-ref_field = ls_fcat_alv-fieldname.
          ENDIF.

          IF   ls_fcat_alv-ref_table IS NOT INITIAL
          AND  ls_fcat_alv-ref_field IS NOT INITIAL.
            ls_fcat_alv-f4availabl = ls_dfies-f4availabl.

          ENDIF.
        ENDIF.

        me->formatar_fcat(
          CHANGING
            cs_fcat_alv = ls_fcat_alv
        ).

        APPEND ls_fcat_alv TO me->mt_fcat_alv.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "carregar_fcat


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABS_EDI_ALV_CTRL->CLEAR_ALL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD clear_all.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Requisitar manipulação de eventos ALV
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    IF me->mo_container IS BOUND.
      me->mo_container->free(
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
      ).
    ENDIF.

    IF me->mo_alv IS BOUND.
      me->mo_alv->free(
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
      ).
      IF sy-subrc IS INITIAL.
        CLEAR me->mo_alv.
      ENDIF.
    ENDIF.

    FREE:
      me->mo_alv,
      me->mv_only_view,
      me->mt_fcat_alv[],
      me->mt_field_f4_custom[]
      .
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABS_EDI_ALV_CTRL->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Construtor
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
  ENDMETHOD.                    "constructor


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABS_EDI_ALV_CTRL=>CONVERT_MSGSY_2_BAPIRET
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RS_RETURN                      TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_msgsy_2_bapiret.
    IF sy-msgid IS INITIAL.
      "Ocorreu um erro não mapeado.
      MESSAGE e008 INTO sy-msgli.
    ENDIF.

    IF sy-msgty IS INITIAL.
      sy-msgty = 'E'. "Erro

    ENDIF.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = sy-msgty
        cl     = sy-msgid
        number = sy-msgno
        par1   = sy-msgv1
        par2   = sy-msgv2
        par3   = sy-msgv3
        par4   = sy-msgv4
      IMPORTING
        return = rs_return.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->EXIBIR_BAPIRET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_RETURN                      TYPE        BAPIRETTAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD exibir_bapiret.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Exibir mensagens de retorno
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    DATA: lv_lines TYPE sydbcnt
            .
    FIELD-SYMBOLS:
          <ls_return> LIKE LINE OF it_return
          .
    IF it_return[] IS INITIAL.
      RETURN.

    ENDIF.

    lv_lines = lines( it_return ).
    IF lv_lines EQ 1.

      READ TABLE it_return
       ASSIGNING <ls_return>
           INDEX 1.

      MESSAGE
           ID <ls_return>-id
         TYPE 'I'
       NUMBER <ls_return>-number
         WITH <ls_return>-message_v1
              <ls_return>-message_v2
              <ls_return>-message_v3
              <ls_return>-message_v4
       DISPLAY LIKE <ls_return>-type.

    ELSE.
      CALL FUNCTION 'OXT_MESSAGE_TO_POPUP'
        EXPORTING
          it_message = it_return
        EXCEPTIONS
          bal_error  = 1
          OTHERS     = 2.
      IF sy-subrc IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "exibir_bapiret


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->GET_REMOVE_FUNC_TOOLBAR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_FUNCTIONS                   TYPE        UI_FUNCTIONS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_remove_func_toolbar.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Remover funções da toolbar
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    APPEND cl_gui_alv_grid=>mc_fc_graph TO rt_functions.
    APPEND cl_gui_alv_grid=>mc_fc_info  TO rt_functions.
    APPEND cl_gui_alv_grid=>mc_fg_edit  TO rt_functions.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->GET_SEL_MODE_LINE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SEL_MODE                    TYPE        LVC_LIBOX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_sel_mode_line.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Tipo de seleção da linha
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    rv_sel_mode = c_sel_mode_alv-several_rows_btn. "A
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->GET_TP_VARIANT_ALV
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_TP_VARIANT_ALV              TYPE        CHAR01
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_tp_variant_alv.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Obtem qual o tipo de variante
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    rv_tp_variant_alv = c_variant_alv-all_and_user.
*      "Caso tenha autorização somente por user....
*      lv_variant_alv = c_variant_alv-by_user.
*      "Caso não tenha autorização de modificar....
*      lv_variant_alv = c_variant_alv-nenhum.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABS_EDI_ALV_CTRL->IS_ONLY_VIEW
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_ONLY_VIEW                   TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_only_view.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Somente visualização?
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    rv_only_view = me->mv_only_view.

  ENDMETHOD.                    "set_only_view


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->ON_DOUBLE_CLICK
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_ROW                          LIKE
* | [--->] E_COLUMN                       LIKE
* | [--->] ES_ROW_NO                      LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_double_click.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Manipular evento "double click" no ALV
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    DATA: lv_ucomm TYPE syucomm.


    lv_ucomm = cl_gui_alv_grid=>mc_fc_detail.

    cl_gui_alv_grid=>transfer_fcode_lvc_to_slis(
      EXPORTING
        i_fcode_lvc    = lv_ucomm
      IMPORTING
        e_fcode_slis   = lv_ucomm
      EXCEPTIONS
        no_match_found = 1
   ).

    me->mo_alv->set_function_code(
      CHANGING
        c_ucomm = lv_ucomm
    ).
  ENDMETHOD.                    "on_double_click


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->ON_HANDLE_TOOLBAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_OBJECT                       LIKE
* | [--->] E_INTERACTIVE                  LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_handle_toolbar.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Manipular toolbar no ALV
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    DATA:
      ls_toolbar TYPE stb_button,
      lv_tabix   TYPE sytabix.

    ADD 1 TO lv_tabix.
    ls_toolbar-butn_type = '0'.   "normal button
    ls_toolbar-function  = c_ucomm-atualizar_lista.
    ls_toolbar-icon      = icon_refresh.
    ls_toolbar-quickinfo = 'Atualizar'(t06).
    INSERT ls_toolbar INTO e_object->mt_toolbar INDEX lv_tabix.

    ADD 1 TO lv_tabix.
    ls_toolbar-butn_type = '3'.   "separator
    INSERT ls_toolbar INTO e_object->mt_toolbar INDEX lv_tabix.
  ENDMETHOD.                    "on_handle_toolbar


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->ON_HANDLE_USER_COMMAND
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_UCOMM                        LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_handle_user_command.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Manipular eventos adicionados ao toolbar do ALV
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    CASE e_ucomm.
      WHEN c_ucomm-atualizar_lista.
        me->on_refresh( ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.                    "on_handle_user_command


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABS_EDI_ALV_CTRL->ON_PAI
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UCOMM                       TYPE        SYUCOMM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_pai.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Evento PAI
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    DATA:
      lv_ucomm LIKE iv_ucomm.

    lv_ucomm = iv_ucomm.

    IF me->mo_alv IS NOT BOUND.
      RETURN.

    ENDIF.

    CASE lv_ucomm.
      WHEN c_ucomm-atualizar_lista.
        me->on_refresh( ).

      WHEN OTHERS.
        me->mo_alv->set_function_code(
          CHANGING
            c_ucomm = lv_ucomm
        ).
    ENDCASE.
  ENDMETHOD.                    "on_pai


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABS_EDI_ALV_CTRL->ON_REFRESH
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_refresh.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Ao requisitar refresh dos dados ALV
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    DATA: lt_return TYPE bapirettab
          .

    me->carregar_info(
      IMPORTING
        et_return = lt_return
    ).
    me->atualizar_dados_alv( ).

    IF lt_return[] IS NOT INITIAL.
      me->exibir_bapiret( lt_return ).

    ENDIF.
  ENDMETHOD.                    "on_refresh


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->REQUISITAR_ALV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_HANDLE                      TYPE        SLIS_HANDL
* | [--->] IO_CONTAINER                   TYPE REF TO CL_GUI_CONTAINER(optional)
* | [--->] IV_VARIANT                     TYPE        SLIS_VARI(optional)
* | [<-->] CT_DATA_ALV                    TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD requisitar_alv.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Requisitar ALV para GUI
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    DATA:
      lt_rem_func    TYPE ui_functions,

      ls_variant     TYPE disvariant,
      ls_layout      TYPE lvc_s_layo,

      lo_rtti_struc  TYPE REF TO cl_abap_structdescr,
      lo_custom_cont TYPE REF TO cl_gui_custom_container,
      ld_w_data      TYPE REF TO data,

      lv_variant_alv TYPE char01
      .
    FIELD-SYMBOLS:
      <lt_data_alv> TYPE STANDARD TABLE,
      <ls_line>     TYPE any.

    IF me->mo_container IS NOT BOUND.

      IF io_container IS BOUND.
        me->mo_container = io_container.

      ELSE.

        CREATE OBJECT lo_custom_cont
          EXPORTING
            container_name              = iv_handle
          EXCEPTIONS
            cntl_error                  = 1
            cntl_system_error           = 2
            create_error                = 3
            lifetime_error              = 4
            lifetime_dynpro_dynpro_link = 5
            OTHERS                      = 6.
        IF sy-subrc <> 0.
          MESSAGE
               ID sy-msgid
             TYPE sy-msgty
           NUMBER sy-msgno
             WITH sy-msgv1
                  sy-msgv2
                  sy-msgv3
                  sy-msgv4.
          RETURN.
        ENDIF.

        me->mo_container ?= lo_custom_cont.

      ENDIF.


    ENDIF.

    IF me->mo_alv IS NOT BOUND.

      CREATE OBJECT me->mo_alv
        EXPORTING
          i_parent          = me->mo_container
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
      IF sy-subrc <> 0.
        MESSAGE
             ID sy-msgid
           TYPE sy-msgty
         NUMBER sy-msgno
           WITH sy-msgv1
                sy-msgv2
                sy-msgv3
                sy-msgv4.
        RETURN.
      ENDIF.

      ls_layout-zebra       = abap_true.
      ls_layout-sel_mode    = me->get_sel_mode_line( ).
      IF ls_layout-sel_mode NE c_sel_mode_alv-several_rows_btn.
        ls_layout-no_rowmark = abap_true.

      ENDIF.
      ls_layout-stylefname  = c_field_base-cell_type.
      ls_layout-cwidth_opt  = abap_true.

      "Nome da variante
      CLEAR ls_variant.
      ls_variant-report   = sy-cprog.
      ls_variant-handle   = iv_handle.
      ls_variant-username = sy-uname.
      IF iv_variant IS NOT INITIAL.
        ls_variant-variant = iv_variant.

      ENDIF.

      "Montagem e formatação dos campos para o ALV
      " Cria referência do tipo de linha da tabela
      CREATE DATA ld_w_data LIKE LINE OF ct_data_alv.
      IF ld_w_data IS BOUND.
        " Aponta ponteiro para o espeço da memória reservado
        ASSIGN ld_w_data->* TO <ls_line>.
      ENDIF.
      lo_rtti_struc ?= cl_abap_structdescr=>describe_by_data( <ls_line> ).
      me->carregar_fcat( lo_rtti_struc ).

      "Botões omitidos
      lt_rem_func = me->get_remove_func_toolbar( ).

      lv_variant_alv = me->get_tp_variant_alv( ).

      CALL METHOD me->mo_alv->set_table_for_first_display
        EXPORTING
          is_layout                     = ls_layout
          is_variant                    = ls_variant
          i_save                        = lv_variant_alv
          it_toolbar_excluding          = lt_rem_func
        CHANGING
          it_outtab                     = ct_data_alv
          it_fieldcatalog               = me->mt_fcat_alv
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE
             ID sy-msgid
           TYPE 'I'
         NUMBER sy-msgno
           WITH sy-msgv1
                sy-msgv2
                sy-msgv3
                sy-msgv4
        DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      me->set_handler_evt_alv( ).

    ENDIF.

    IF me->mv_only_view  EQ abap_true.
      CALL METHOD me->mo_alv->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.

    ENDIF.

  ENDMETHOD.                    "requisitar_alv


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->REQUISITAR_DOMAIN_F4
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD requisitar_domain_f4.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Requisitar F4 customizado (dominio)
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    TYPES:
      BEGIN OF tp_value_tab,
        domvalue_l TYPE dd07v-domvalue_l,
        ddtext     TYPE dd07v-ddtext,
      END OF tp_value_tab
      .
    DATA: lt_dd07      TYPE dd07v_tab,
          lt_value_tab TYPE STANDARD TABLE OF tp_value_tab,

          lo_elemdescr TYPE REF TO cl_abap_elemdescr,

          ls_value_tab LIKE LINE OF lt_value_tab,
          ls_dfies     TYPE dfies
          .
    FIELD-SYMBOLS:
         <ls_dd07> LIKE LINE OF lt_dd07
         .
    TRY .
        lo_elemdescr ?= cl_abap_elemdescr=>describe_by_data( iv_value ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    ls_dfies = lo_elemdescr->get_ddic_field( ).

    CALL FUNCTION 'DDUT_DOMVALUES_GET'
      EXPORTING
        name          = ls_dfies-domname
      TABLES
        dd07v_tab     = lt_dd07
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc IS NOT INITIAL.
      RETURN.

    ENDIF.

    LOOP AT lt_dd07 ASSIGNING <ls_dd07>.
      CLEAR ls_value_tab.
      MOVE-CORRESPONDING <ls_dd07> TO ls_value_tab.
      APPEND ls_value_tab TO lt_value_tab.
    ENDLOOP.

    IF lt_value_tab[] IS INITIAL.
      RETURN.

    ENDIF.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'DOMVALUE_L'
        value_org       = 'S'
      TABLES
        value_tab       = lt_value_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc IS NOT INITIAL.
      RETURN.

    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ABS_EDI_ALV_CTRL->SET_HANDLER_EVT_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_handler_evt_alv.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Requisitar manipulação de eventos ALV
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    SET HANDLER:
        me->on_hotspot_click              FOR me->mo_alv,
        me->on_double_click               FOR me->mo_alv,
        me->on_handle_toolbar             FOR me->mo_alv,
        me->on_handle_user_command        FOR me->mo_alv.

    me->mo_alv->set_toolbar_interactive( ).

    IF me->mv_only_view EQ abap_true.
      CALL METHOD me->mo_alv->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
    ELSE.
      CALL METHOD me->mo_alv->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ENDIF.

  ENDMETHOD.                    "set_handler_evt_alv


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABS_EDI_ALV_CTRL->SET_ONLY_VIEW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ONLY_VIEW                   TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_only_view.
***************************************************************************************************
*                                          
***************************************************************************************************

* Autor    : Max dos Anjos
* Data     : 05.03.2020
* Descrição:  Somente visualização?
***************************************************************************************************
* Alteração: XXX01 (XXX=sigla do desenvolvedor; 01=sequencial da alteração pelo desenvolvedor)
* Autor    :
* Data     : 00.00.0000
* Descrição:
***************************************************************************************************
    me->mv_only_view = iv_only_view.

  ENDMETHOD.                    "set_only_view
ENDCLASS.