CLASS zcl_max_cluster_view DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF y_key_nivel,
        nivel_key     TYPE tv_nodekey,
        nivel_descr   TYPE val_text,
        nivel_pai_key TYPE tv_nodekey,
      END OF y_key_nivel .
    TYPES:
      BEGIN OF y_key_item,
        item_key     TYPE char255,
        item_pai_key TYPE char255,
      END OF y_key_item .
    TYPES:
      y_t_key_item     TYPE STANDARD TABLE OF y_key_item .
    TYPES:
      y_t_key_item_sel TYPE HASHED TABLE OF char255
                            WITH UNIQUE KEY table_line .

    METHODS add_registro
      IMPORTING
        !iw_key_nivel TYPE y_key_nivel
        !it_key_data  TYPE y_t_key_item
        !it_data      TYPE STANDARD TABLE
        !iv_reset     TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_max_cluster_view .
    METHODS free .
    METHODS get_registro
      IMPORTING
        !iv_nivel_key TYPE tv_nodekey
      EXPORTING
        !et_data      TYPE STANDARD TABLE
      RAISING
        zcx_max_cluster_view .
    METHODS visualizar
      IMPORTING
        !ir_container TYPE REF TO cl_gui_container DEFAULT cl_gui_container=>default_screen
      RAISING
        zcx_max_cluster_view .
    METHODS set_ok_code_alv
      CHANGING
        !cv_ucomm TYPE syucomm .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF y_key_nivel_format.
        INCLUDE TYPE y_key_nivel.
    TYPES: o_struct TYPE REF TO cl_abap_structdescr,
           END OF y_key_nivel_format .

    DATA w_key_nivel_sel TYPE y_key_nivel_format .
    DATA t_fcat_grid TYPE lvc_t_fcat .
    DATA t_selected_rows TYPE lvc_t_row .
    DATA t_key_item_sel TYPE y_t_key_item_sel .
    DATA r_t_data_grid TYPE REF TO data .
    DATA r_o_alv_grid TYPE REF TO cl_gui_alv_grid .
    DATA v_variant_alv TYPE disvariant-variant .

    METHODS formatar_fcat_grid
      IMPORTING
        !iw_key_nivel_sel TYPE y_key_nivel_format .
    METHODS on_dbl_click_tree
      IMPORTING
        !iv_node_key TYPE tv_nodekey .
    METHODS get_data_by_key
      IMPORTING
        !iv_nivel_key TYPE tv_nodekey
      EXPORTING
        !et_data      TYPE REF TO data .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF y_key_item_hist,
        idx_nivel TYPE sytabix,
        item_key  TYPE char255,
      END OF y_key_item_hist .
    TYPES:
      y_t_key_item_hist TYPE SORTED TABLE OF y_key_item_hist
                         WITH UNIQUE KEY idx_nivel
                                         item_key .

    DATA r_o_alv_tree TYPE REF TO cl_gui_list_tree .
    DATA r_o_container_splt TYPE REF TO cl_gui_container .
    DATA r_o_splitter TYPE REF TO cl_gui_splitter_container .
    CONSTANTS c_data_level TYPE string VALUE 'DADOS' ##NO_TEXT.
    CONSTANTS c_estru_tree TYPE tabname VALUE 'MTREEITM' ##NO_TEXT.
    CONSTANTS c_field_level TYPE string VALUE 'NIVEL_KEY' ##NO_TEXT.
    CONSTANTS c_item_key TYPE string VALUE 'ITEM_KEY' ##NO_TEXT.
    CONSTANTS c_item_pai_key TYPE string VALUE 'ITEM_PAI_KEY' ##NO_TEXT.
    DATA r_o_struct_data TYPE REF TO cl_abap_structdescr .
    DATA r_t_data TYPE REF TO data .
    DATA t_key_item_hist TYPE y_t_key_item_hist .
    DATA:
      t_key_nivel TYPE SORTED TABLE OF y_key_nivel_format
                   WITH UNIQUE KEY nivel_pai_key
                                   nivel_key
                   WITH NON-UNIQUE SORTED KEY nivel
                    COMPONENTS nivel_key .

    METHODS carregar_fcat
      IMPORTING
        !ir_struct TYPE REF TO cl_abap_structdescr .
    METHODS add_nodes_tree
      RAISING
        zcx_max_cluster_view .
    METHODS build_node_and_item_table
      EXPORTING
        !et_itm_table  TYPE srmitemtab
        !et_node_table TYPE treev_ntab .
    METHODS get_components
      IMPORTING
        !ir_struct            TYPE REF TO cl_abap_structdescr
      RETURNING
        VALUE(rt_componentes) TYPE cl_abap_structdescr=>component_table .
    METHODS on_item_double_click
          FOR EVENT item_double_click OF cl_gui_list_tree
      IMPORTING
          !node_key .
    METHODS on_node_double_click
          FOR EVENT node_double_click OF cl_gui_list_tree
      IMPORTING
          !node_key .
    METHODS requisitar_grid
      IMPORTING
        !ir_container TYPE REF TO cl_gui_container
      RAISING
        zcx_max_cluster_view .
    METHODS requisitar_tree
      IMPORTING
        !ir_container TYPE REF TO cl_gui_container
      RAISING
        zcx_max_cluster_view .
ENDCLASS.



CLASS ZCL_MAX_CLUSTER_VIEW IMPLEMENTATION.


  METHOD add_nodes_tree.
    DATA:
      t_node_table TYPE treev_ntab,
      t_itm_table  TYPE srmitemtab
      .

    FIELD-SYMBOLS: <f_node_table> LIKE LINE OF t_node_table
                   .

    CHECK me->r_o_alv_tree IS BOUND.

    me->build_node_and_item_table( IMPORTING et_node_table = t_node_table
                                             et_itm_table  = t_itm_table ).

    CALL METHOD me->r_o_alv_tree->add_nodes_and_items
      EXPORTING
        node_table                     = t_node_table
        item_table                     = t_itm_table
        item_table_structure_name      = c_estru_tree
      EXCEPTIONS
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6.

    IF sy-subrc IS NOT INITIAL.
      "Erro ao gerar elementos de interface
      RAISE EXCEPTION TYPE zcx_max_cluster_view
        EXPORTING
          vm_msgv1 = CONV #( 'Erro ao gerar elementos de interface'(003) ).
    ENDIF.

    READ TABLE t_node_table
     ASSIGNING <f_node_table>
      INDEX 1.

    CHECK sy-subrc IS INITIAL.


    CALL METHOD me->r_o_alv_tree->expand_root_nodes
      EXCEPTIONS
        failed              = 1
        illegal_level_count = 2
        cntl_system_error   = 3.
    IF sy-subrc IS NOT INITIAL.
      "Erro ao gerar elementos de interface
      RAISE EXCEPTION TYPE zcx_max_cluster_view
        EXPORTING
          vm_msgv1 = CONV #( 'Erro ao gerar elementos de interface'(003) ).
    ENDIF.

    CALL METHOD me->r_o_alv_tree->node_set_style
      EXPORTING
        node_key          = <f_node_table>-node_key
        style             = cl_gui_list_tree=>style_emphasized
      EXCEPTIONS
        failed            = 1
        node_not_found    = 2
        cntl_system_error = 3.
    IF sy-subrc IS NOT INITIAL.
      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD add_registro.
    DATA: t_comp        TYPE cl_abap_structdescr=>component_table,
          t_comp_data   LIKE t_comp,
          t_comp_key    LIKE t_comp,
          t_key_new_tab TYPE abap_keydescr_tab,
          t_table_data  TYPE REF TO data
          .
    DATA: w_comp        LIKE LINE OF t_comp,
          w_key_item    TYPE y_key_item,
          w_key_new_tab LIKE LINE OF t_key_new_tab,
          w_key_format  TYPE y_key_nivel_format
          .
    DATA: w_work_area TYPE REF TO data,
          w_work_data TYPE REF TO data
          .
    DATA: r_o_table_data  TYPE REF TO cl_abap_tabledescr,
          r_o_struct_data TYPE REF TO cl_abap_structdescr,
          r_o_struct_key  TYPE REF TO cl_abap_structdescr,
          r_o_table_main  TYPE REF TO cl_abap_tabledescr
          .
    DATA: v_idx   TYPE sytabix
          .

    FIELD-SYMBOLS: <f_table_main> TYPE HASHED TABLE,
                   <f_table_data> TYPE STANDARD TABLE,
                   <f_wa_main>    TYPE any,
                   <f_wa_data>    TYPE any,
                   <f_old_data>   TYPE any,
                   <f_value>      TYPE any,
                   <f_key_data>   LIKE LINE OF it_key_data
                   .

    "Estrutura dos dados a serem inseridos
    r_o_table_data  ?= cl_abap_tabledescr=>describe_by_data( it_data ).

    r_o_struct_data ?= r_o_table_data->get_table_line_type( ).

    MOVE-CORRESPONDING iw_key_nivel TO w_key_format.
    w_key_format-o_struct = r_o_struct_data.

    INSERT w_key_format INTO TABLE me->t_key_nivel.

*  "Caso a chave j?? exista
    IF sy-subrc IS NOT INITIAL.

      IF iv_reset EQ abap_true.

        DELETE me->t_key_nivel
         WHERE nivel_pai_key = w_key_format-nivel_pai_key
           AND nivel_key     = w_key_format-nivel_key.

        INSERT w_key_format INTO TABLE me->t_key_nivel.
        IF sy-subrc IS NOT INITIAL.
          "Chave do n??vel informada j?? existe
          RAISE EXCEPTION TYPE zcx_max_cluster_view
            EXPORTING
              vm_msgv1 = CONV #( 'Chave do n??vel informada j?? existe'(001) ).
        ENDIF.

      ELSE.
        "Chave do n??vel informada j?? existe
        RAISE EXCEPTION TYPE zcx_max_cluster_view
          EXPORTING
            vm_msgv1 = CONV #( 'Chave do n??vel informada j?? existe'(001) ).
      ENDIF.
    ENDIF.

    t_comp_data = r_o_struct_data->get_components( ).

    CLEAR: r_o_struct_data,
           r_o_table_data,
           t_comp[]
           .

****  Criar novo tipo de tabela para os dados

    r_o_struct_key  ?= cl_abap_typedescr=>describe_by_data( w_key_item ).
    t_comp_key     = r_o_struct_key->get_components( ).
    INSERT LINES OF t_comp_key  INTO TABLE t_comp.

    INSERT LINES OF t_comp_data INTO TABLE t_comp.

    r_o_struct_data = cl_abap_structdescr=>create( p_components = t_comp ).

* Monta a estrutura da nova tabela para os dados com as chaves
    r_o_table_data = cl_abap_tabledescr=>create( p_line_type  = r_o_struct_data ).

    CREATE DATA t_table_data TYPE HANDLE r_o_table_data.
    ASSIGN t_table_data->* TO <f_table_data>.

    CREATE DATA w_work_data TYPE HANDLE r_o_struct_data.
    ASSIGN w_work_data->* TO <f_wa_data>.

    LOOP AT it_data ASSIGNING <f_old_data>.
      v_idx = sy-tabix.

      READ TABLE it_key_data
       ASSIGNING <f_key_data>
           INDEX v_idx.
      IF sy-subrc  IS NOT INITIAL.
        "Chave dos dados n??o corresponde com os dados
        RAISE EXCEPTION TYPE zcx_max_cluster_view
          EXPORTING
            vm_msgv1 = CONV #( TEXT-002 ).
      ENDIF.

      MOVE-CORRESPONDING <f_old_data> TO <f_wa_data>.
      MOVE-CORRESPONDING <f_key_data> TO <f_wa_data>.

      INSERT <f_wa_data> INTO TABLE <f_table_data>.


    ENDLOOP.

    CLEAR: t_comp[],
           t_comp_key[],
           t_comp_data[],
           t_key_new_tab[]
           .

***  Criar novo tipo de tabela para o nivel X dados
    IF me->r_t_data IS INITIAL.

*  "dados
      w_comp-name  = c_data_level.
      w_comp-type ?= cl_abap_datadescr=>describe_by_data( t_table_data ).
      APPEND w_comp TO t_comp.

*  "Chave dos dados
      w_comp-name  = c_field_level.
      w_comp-type ?= cl_abap_elemdescr=>describe_by_data( iw_key_nivel-nivel_key ).
      APPEND w_comp TO t_comp.


* Monta a nova estrutura com a jun????o dos dados com as chaves
      me->r_o_struct_data = cl_abap_structdescr=>create( p_components = t_comp ).

*  Chave da tabela interna.
      CLEAR: w_key_new_tab,
             t_key_new_tab[]
             .
      w_key_new_tab-name = c_field_level.
      INSERT w_key_new_tab INTO TABLE t_key_new_tab.


* Monta a estrutura da nova tabela para os dados com as chaves
      r_o_table_main = cl_abap_tabledescr=>create( p_line_type  = me->r_o_struct_data
                                                  p_table_kind = cl_abap_tabledescr=>tablekind_hashed
                                                  p_unique     = abap_true
                                                  p_key        = t_key_new_tab
                                                  ).



* Assimila a estrutura da tabela ao atributo REF TO DATA
      CREATE DATA me->r_t_data TYPE HANDLE r_o_table_main.

    ENDIF.

* Assimila o atributo a um field-symbol
    ASSIGN me->r_t_data->* TO <f_table_main>.

* Assimila a estrutura da workarea da tabela
    CREATE DATA w_work_area TYPE HANDLE me->r_o_struct_data.

* Assimila o REF DATA da workarea a um field-symbol
    ASSIGN w_work_area->* TO <f_wa_main>.

*   dados
    ASSIGN COMPONENT c_data_level
        OF STRUCTURE <f_wa_main>
                  TO <f_value>.
    IF <f_value> IS ASSIGNED.
      GET REFERENCE OF <f_table_data> INTO <f_value>.
    ENDIF.

*   Nivel de dados
    ASSIGN COMPONENT c_field_level
        OF STRUCTURE <f_wa_main>
                  TO <f_value>.
    IF <f_value> IS ASSIGNED.
      <f_value> = iw_key_nivel-nivel_key.
    ENDIF.

    INSERT <f_wa_main> INTO TABLE <f_table_main>.
    IF sy-subrc IS NOT INITIAL.
      IF iv_reset EQ abap_true.

        DATA(l_where_del) = |{ c_field_level } EQ iw_key_nivel-nivel_key|.
        DELETE <f_table_main>
         WHERE (l_where_del).

        INSERT <f_wa_main> INTO TABLE <f_table_main>.
        IF sy-subrc IS NOT INITIAL.
          "Chave do n??vel informada j?? existe
          RAISE EXCEPTION TYPE zcx_max_cluster_view
            EXPORTING
              vm_msgv1 = CONV #( 'Chave do n??vel informada j?? existe'(001) ).
        ENDIF.

      ELSE.
        "Chave do n??vel informada j?? existe
        RAISE EXCEPTION TYPE zcx_max_cluster_view
          EXPORTING
            vm_msgv1 = CONV #( 'Chave do n??vel informada j?? existe'(001) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD build_node_and_item_table.
    DATA: w_node LIKE LINE OF et_node_table,
          w_item LIKE LINE OF et_itm_table
          .

    DATA: v_idx_label TYPE sytabix
          .

    FIELD-SYMBOLS: <f_key_nivel> LIKE LINE OF me->t_key_nivel
                   .


    LOOP AT me->t_key_nivel ASSIGNING <f_key_nivel>.
      ADD 1 TO v_idx_label.

      CLEAR w_node.
      w_node-node_key   = <f_key_nivel>-nivel_key.

      w_node-isfolder   = abap_true.
      w_node-expander   = abap_false.
      READ TABLE me->t_key_nivel
       TRANSPORTING NO FIELDS
        WITH KEY nivel_pai_key = <f_key_nivel>-nivel_key
         BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        w_node-expander   = abap_true.

      ENDIF.


      IF <f_key_nivel>-nivel_pai_key IS NOT INITIAL.
        w_node-relatkey  = <f_key_nivel>-nivel_pai_key.
        w_node-relatship = cl_gui_list_tree=>relat_last_child.
      ENDIF.

      APPEND w_node TO et_node_table.

      CLEAR w_item.
      w_item-node_key   = w_node-node_key.
      w_item-item_name  = v_idx_label.
      w_item-class      = cl_gui_list_tree=>item_class_text.
      w_item-alignment  = cl_gui_list_tree=>align_auto.
      w_item-font       = cl_gui_list_tree=>item_font_prop.
      w_item-text       = <f_key_nivel>-nivel_descr.

      APPEND w_item TO et_itm_table.

    ENDLOOP.


  ENDMETHOD.


  METHOD carregar_fcat.
    DATA: t_ddfields TYPE cl_abap_structdescr=>component_table
      .
    DATA: r_o_rtti_elemt  TYPE REF TO cl_abap_elemdescr,
          r_o_rtti_struct TYPE REF TO cl_abap_structdescr
          .
    DATA: w_fcat  LIKE LINE OF me->t_fcat_grid,
          w_dfies TYPE dfies
          .
    FIELD-SYMBOLS: <f_ddfields>  LIKE LINE OF t_ddfields
                   .

    t_ddfields = ir_struct->get_components( ).

    LOOP AT t_ddfields ASSIGNING <f_ddfields>.
      IF <f_ddfields>-as_include EQ 'X'.

        r_o_rtti_struct ?= <f_ddfields>-type.

        me->carregar_fcat( r_o_rtti_struct ).
        CONTINUE.

      ENDIF.

      CLEAR w_fcat.
      r_o_rtti_elemt ?= <f_ddfields>-type.

      w_dfies = r_o_rtti_elemt->get_ddic_field( p_langu = sy-langu ).

      "Remove mandante
      IF w_dfies-datatype EQ 'CLNT'.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING w_dfies TO w_fcat.
      w_fcat-fieldname = <f_ddfields>-name.
      w_fcat-seltext   = w_dfies-scrtext_l.
      w_fcat-scrtext_l = w_dfies-scrtext_l.
      w_fcat-scrtext_m = w_dfies-scrtext_m.
      w_fcat-scrtext_s = w_dfies-scrtext_s.

      IF w_fcat-seltext IS INITIAL .

        w_fcat-coltext   = w_dfies-reptext.
        IF w_fcat-coltext IS INITIAL.
          w_fcat-coltext   = w_dfies-fieldtext.

        ENDIF.

      ENDIF.


      w_fcat-key       = w_dfies-keyflag.
      w_fcat-ref_table = w_dfies-reftable.
      w_fcat-ref_field = w_dfies-reffield.

      IF w_dfies-convexit IS NOT INITIAL.
        CONCATENATE '=='
                    w_dfies-convexit
               INTO w_fcat-edit_mask.
      ENDIF.
      APPEND w_fcat TO me->t_fcat_grid.
    ENDLOOP.
  ENDMETHOD.


  METHOD formatar_fcat_grid.
    CLEAR me->t_fcat_grid[].
    me->carregar_fcat( iw_key_nivel_sel-o_struct ).

  ENDMETHOD.


  METHOD free.
    IF me->r_o_splitter IS BOUND.
      CALL METHOD me->r_o_splitter->free
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.
      IF sy-subrc IS NOT INITIAL.
        FREE me->r_o_splitter.
      ENDIF.

    ENDIF.

    FREE: me->r_o_splitter,
          me->r_o_alv_tree,
          me->r_o_splitter,
          me->r_o_container_splt,
          me->t_key_nivel[],
          me->r_t_data,
          me->w_key_nivel_sel,
          me->t_fcat_grid[],
          me->r_o_struct_data,
          me->t_key_item_sel[],
          me->t_key_item_hist[],
          me->r_t_data_grid
          .
  ENDMETHOD.


  METHOD get_components.
    DATA: ol_struct_data LIKE ir_struct
          .
    DATA: tl_comp_inter LIKE rt_componentes,
          tl_comp_data  LIKE rt_componentes
          .
    FIELD-SYMBOLS: <fl_comp_data> LIKE LINE OF tl_comp_data
                   .

    CHECK ir_struct IS BOUND.

    tl_comp_data   = ir_struct->get_components( ).

    LOOP AT tl_comp_data ASSIGNING <fl_comp_data>.
      CLEAR: ol_struct_data.

      "Caso seja include, ser?? carregado os campos (recursividade)
      IF <fl_comp_data>-as_include EQ abap_true.
        ol_struct_data ?= <fl_comp_data>-type.

        tl_comp_inter = me->get_components( ol_struct_data ).

        INSERT LINES OF tl_comp_inter INTO TABLE rt_componentes.

      ELSE.

        INSERT <fl_comp_data> INTO TABLE rt_componentes.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_data_by_key.
    FIELD-SYMBOLS: <f_table_main> TYPE HASHED TABLE,
                   <f_ref_data>   TYPE REF TO data,
                   <f_wa_main>    TYPE any.


* Assimila o atributo a um field-symbol
    ASSIGN me->r_t_data->* TO <f_table_main>.

    READ TABLE <f_table_main>
     ASSIGNING <f_wa_main>
    WITH TABLE KEY (c_field_level) = iv_nivel_key.

    IF sy-subrc IS NOT INITIAL.
**** ERRO
      RETURN.
    ENDIF.

    ASSIGN COMPONENT c_data_level
        OF STRUCTURE <f_wa_main>
                  TO <f_ref_data>.
    IF sy-subrc IS NOT INITIAL.
**** ERRO
      RETURN.
    ENDIF.

    et_data = <f_ref_data>.
  ENDMETHOD.


  METHOD get_registro.
    DATA: t_table_data TYPE REF TO data
          .
    DATA: w_key_nivel_sel LIKE LINE OF me->t_key_nivel,
          w_data_out      TYPE REF TO data
          .

    FIELD-SYMBOLS: <f_table_data> TYPE STANDARD TABLE,
                   <f_wa_data>    TYPE any,
                   <f_wa_out>     TYPE any
                   .

    CLEAR et_data[].

** Muda nivel selecionado
    READ TABLE me->t_key_nivel
          INTO w_key_nivel_sel
      WITH TABLE KEY nivel
       COMPONENTS nivel_key = iv_nivel_key.

    IF sy-subrc IS NOT INITIAL.
      "Chave do n??vel inv??lida
      RAISE EXCEPTION TYPE zcx_max_cluster_view
        EXPORTING
          vm_msgv1 = CONV #( 'Chave do n??vel inv??lida'(010) ).

    ENDIF.

    IF me->r_o_alv_grid IS BOUND.
      me->r_o_alv_grid->check_changed_data( ).
    ENDIF.

    me->get_data_by_key( EXPORTING iv_nivel_key = w_key_nivel_sel-nivel_key
                         IMPORTING et_data      = t_table_data ).

    ASSIGN t_table_data->* TO <f_table_data>.

    CREATE DATA w_data_out TYPE HANDLE w_key_nivel_sel-o_struct.
    ASSIGN w_data_out->* TO <f_wa_out>.

    LOOP AT <f_table_data> ASSIGNING <f_wa_data>.
      MOVE-CORRESPONDING <f_wa_data> TO <f_wa_out>.

      INSERT <f_wa_out> INTO TABLE et_data.
    ENDLOOP.
  ENDMETHOD.


  METHOD on_dbl_click_tree.
    DATA: t_key_item_sel_old LIKE me->t_key_item_sel
          .
    DATA: w_nivel_sel_old LIKE me->w_key_nivel_sel,
          w_nivel_sel_new LIKE me->w_key_nivel_sel,
          w_key_item_hist LIKE LINE OF me->t_key_item_hist
          .
    DATA: v_idx_old        TYPE sytabix,
          v_idx_new        TYPE sytabix,
          v_idx_hist       TYPE sytabix,
          v_nivel_pai_hist LIKE me->w_key_nivel_sel-nivel_pai_key,
          v_valid          TYPE char01
          .

    FIELD-SYMBOLS: <f_table_data> TYPE STANDARD TABLE,
                   <f_wa_data>    TYPE any,
                   <f_value>      TYPE any,
                   <f_rows>       LIKE LINE OF me->t_selected_rows,
                   <f_key_nivel>  LIKE LINE OF me->t_key_nivel
                   .

    CLEAR me->t_selected_rows[].

    me->r_o_alv_grid->check_changed_data( IMPORTING e_valid = v_valid ).

    CHECK v_valid EQ abap_true.

    CHECK iv_node_key NE me->w_key_nivel_sel-nivel_key.

    CALL METHOD me->r_o_alv_grid->get_selected_rows
      IMPORTING
        et_index_rows = me->t_selected_rows.

    t_key_item_sel_old = me->t_key_item_sel.

    w_nivel_sel_old = me->w_key_nivel_sel.

** Muda nivel selecionado
    READ TABLE me->t_key_nivel
          INTO w_nivel_sel_new
      WITH TABLE KEY nivel
       COMPONENTS nivel_key = iv_node_key.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s001
          WITH 'Selecione uma linha v??lida'(004)
       DISPLAY LIKE 'E'.

      RETURN.
    ENDIF.

*** Indice do elemento novo
    v_idx_new = sy-tabix.

** Indice do elemento antigo
    READ TABLE me->t_key_nivel
          INTO w_nivel_sel_old
      WITH TABLE KEY nivel
       COMPONENTS nivel_key = w_nivel_sel_old-nivel_key.

    v_idx_old = sy-tabix.

*  "Caso seja selecionado o primeiro nivel (vazio)
    IF w_nivel_sel_new-nivel_pai_key IS NOT INITIAL.

      ASSIGN me->r_t_data_grid->* TO <f_table_data>.

*    "Quando estiver indo para o pr??ximo n??vel
      IF v_idx_old LT v_idx_new.

*     Caso seja do mesmo nivel do pai
        IF w_nivel_sel_new-nivel_pai_key EQ w_nivel_sel_old-nivel_pai_key.
          me->t_key_item_sel = t_key_item_sel_old.

        ELSE.

          CLEAR me->t_key_item_sel[].
          LOOP AT me->t_selected_rows ASSIGNING
                                   FIELD-SYMBOL(<f_index_sel>).

            READ TABLE <f_table_data>
             ASSIGNING <f_wa_data>
                 INDEX <f_index_sel>-index.

            IF sy-subrc IS NOT INITIAL.
              CONTINUE.
            ENDIF.

            ASSIGN COMPONENT c_item_key
                OF STRUCTURE <f_wa_data>
                          TO <f_value>.

*       Filtro de sele????o
            INSERT <f_value> INTO TABLE me->t_key_item_sel.
          ENDLOOP.

        ENDIF.

*       Hist??rico de sele????o
        LOOP AT me->t_key_item_sel ASSIGNING FIELD-SYMBOL(<f_key_item_sel>).
          CLEAR w_key_item_hist.
          w_key_item_hist-idx_nivel = v_idx_old.
          w_key_item_hist-item_key  = <f_key_item_sel>.

          INSERT w_key_item_hist INTO TABLE me->t_key_item_hist.
        ENDLOOP.

        "Nivel anterior
      ELSE.

        CLEAR me->t_key_item_sel[].

***      "Verifica se ele tem hist??rico
        v_idx_hist       = v_idx_new.
        v_nivel_pai_hist = w_nivel_sel_new-nivel_pai_key.

        DO.
          READ TABLE me->t_key_item_hist
          ASSIGNING FIELD-SYMBOL(<f_key_item_hist>)
            WITH KEY idx_nivel = v_idx_hist
              BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            v_idx_hist = <f_key_item_hist>-idx_nivel.

            "Encontrou...para a busca
            EXIT.

          ENDIF.

          "Se n??o tem pai..termina a busca
          IF v_nivel_pai_hist IS INITIAL.
            EXIT.

          ENDIF.

          "Ir?? buscar no elemento PAI
          READ TABLE me->t_key_nivel
          ASSIGNING FIELD-SYMBOL(<f_key_nivel_pai_hist>)
           WITH TABLE KEY nivel
           COMPONENTS nivel_key = v_nivel_pai_hist.
          IF sy-subrc IS INITIAL.
            "Atualiza para pr??xima busca
            v_idx_hist       = sy-tabix.
            v_nivel_pai_hist = <f_key_nivel_pai_hist>-nivel_pai_key.

          ENDIF.

        ENDDO.

        IF v_idx_hist IS NOT INITIAL.

          LOOP AT me->t_key_item_hist INTO w_key_item_hist
                                     WHERE idx_nivel EQ v_idx_hist.
            INSERT w_key_item_hist-item_key INTO TABLE me->t_key_item_sel.

          ENDLOOP.
        ENDIF.


*       Remove hist??rico posterior
        DELETE me->t_key_item_hist WHERE idx_nivel GT v_idx_new.

      ENDIF.

    ELSE.
      CLEAR: me->t_key_item_hist[],
             me->t_key_item_sel[].
    ENDIF.

    me->r_o_alv_tree->collapse_all_nodes( ).

    CALL METHOD me->r_o_alv_tree->expand_node
      EXPORTING
        node_key = iv_node_key.

* Remove a cor (destaque) dos outros niveis
    LOOP AT me->t_key_nivel ASSIGNING <f_key_nivel>.
      CALL METHOD me->r_o_alv_tree->node_set_style
        EXPORTING
          node_key = <f_key_nivel>-nivel_key
          style    = cl_gui_list_tree=>style_default.

    ENDLOOP.

* Mudar cor do nivel selecionado
    CALL METHOD me->r_o_alv_tree->node_set_style
      EXPORTING
        node_key = iv_node_key
        style    = cl_gui_list_tree=>style_emphasized.

    me->w_key_nivel_sel = w_nivel_sel_new.

*  "Limpa o grid, para montar novamente
    me->r_o_alv_grid->free( ).
    FREE me->r_o_alv_grid.

  ENDMETHOD.


  METHOD on_item_double_click.
    me->on_dbl_click_tree( node_key ).
  ENDMETHOD.


  METHOD on_node_double_click.
    me->on_dbl_click_tree( node_key ).
  ENDMETHOD.


  METHOD requisitar_grid.
    DATA: t_rows     TYPE lvc_t_row
            .
    DATA: w_variant TYPE disvariant,
          w_layout  TYPE lvc_s_layo,
          w_rows    LIKE LINE OF t_rows
          .

    DATA: t_table_data TYPE REF TO data
          .

    FIELD-SYMBOLS: <f_table_data> TYPE STANDARD TABLE,
                   <f_table_tmp>  TYPE STANDARD TABLE,
                   <f_wa_data>    TYPE any,
                   <f_key_item>   TYPE any
                   .

    IF me->w_key_nivel_sel IS INITIAL.
      "Selecione uma linha
      RAISE EXCEPTION TYPE zcx_max_cluster_view
        EXPORTING
          vm_msgv1 = CONV #( 'Selecione uma linha'(004) ).
    ENDIF.

    IF me->r_o_alv_grid IS NOT BOUND.

      me->get_data_by_key( EXPORTING iv_nivel_key = me->w_key_nivel_sel-nivel_key
                           IMPORTING et_data      = t_table_data ).

      ASSIGN t_table_data->* TO <f_table_tmp>.

* "Caso tenha itens selecionados
      IF me->t_key_item_sel[] IS NOT INITIAL.

        CREATE DATA me->r_t_data_grid LIKE <f_table_tmp>.

        ASSIGN me->r_t_data_grid->* TO <f_table_data>.

        CLEAR <f_table_data>[].

        LOOP AT <f_table_tmp> ASSIGNING <f_wa_data>.
          ASSIGN COMPONENT c_item_pai_key
              OF STRUCTURE <f_wa_data>
                        TO <f_key_item>.

          READ TABLE me->t_key_item_sel
           TRANSPORTING NO FIELDS
            WITH KEY table_line = <f_key_item>.

          IF sy-subrc IS INITIAL.
            INSERT <f_wa_data> INTO TABLE <f_table_data>.
          ENDIF.

        ENDLOOP.
      ELSE.

        ASSIGN <f_table_tmp> TO <f_table_data>.

        me->r_t_data_grid = t_table_data.
      ENDIF.

      CLEAR t_table_data.

      w_layout-sel_mode   = 'A'.
      w_layout-cwidth_opt = abap_true.
      w_layout-zebra      = abap_true.

      CREATE OBJECT me->r_o_alv_grid
        EXPORTING
          i_parent = ir_container.

      me->formatar_fcat_grid( me->w_key_nivel_sel ).

      "Nome da variante
      CLEAR w_variant.
      CONCATENATE sy-cprog
                  me->w_key_nivel_sel-nivel_key
             INTO w_variant-report.
      w_variant-username = sy-uname.

      IF me->v_variant_alv IS NOT INITIAL.
        w_variant-variant  = me->v_variant_alv.

      ENDIF.

      CALL METHOD me->r_o_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout                     = w_layout
          is_variant                    = w_variant
          i_save                        = 'U' "Por usu??rio
        CHANGING
          it_outtab                     = <f_table_data>
          it_fieldcatalog               = me->t_fcat_grid
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc IS NOT INITIAL.
        "Erro ao gerar elementos de interface
        RAISE EXCEPTION TYPE zcx_max_cluster_view
          EXPORTING
            vm_msgv1 = CONV #( 'Erro ao gerar elementos de interface'(003) ).
      ENDIF.

**    "Selecionar a primeira linha
*    w_rows-index = 1.
*    APPEND w_rows TO t_rows.
*    CALL METHOD me->r_o_alv_grid->set_selected_rows
*      EXPORTING
*        it_index_rows = t_rows.

    ELSE.
      me->r_o_alv_grid->refresh_table_display(
        EXPORTING
          i_soft_refresh = abap_true
          is_stable = VALUE #(
            col = abap_true
            row = abap_true
          )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD requisitar_tree.
    DATA: t_events TYPE cntl_simple_events.
    DATA: w_event  TYPE cntl_simple_event.


    CHECK me->r_o_alv_tree IS NOT BOUND.

* criar alv tree
    CREATE OBJECT me->r_o_alv_tree
      EXPORTING
        parent                      = ir_container
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = abap_true
        with_headers                = abap_false
      EXCEPTIONS
        cntl_system_error           = 1
        create_error                = 2
        failed                      = 3
        illegal_node_selection_mode = 4
        lifetime_error              = 5.

    IF sy-subrc IS NOT INITIAL.
      "Erro ao gerar elementos de interface
      RAISE EXCEPTION TYPE zcx_max_cluster_view
        EXPORTING
          vm_msgv1 = CONV #( 'Erro ao gerar elementos de interface'(003) ).
    ENDIF.

*  " Item double click
    w_event-eventid = cl_gui_list_tree=>eventid_item_double_click.
    w_event-appl_event = abap_true.
    APPEND w_event TO t_events.

*  " Item double click
    w_event-eventid = cl_gui_list_tree=>eventid_node_double_click.
    w_event-appl_event = abap_true.
    APPEND w_event TO t_events.

*  "N??o expandir filhos
    w_event-eventid = cl_gui_list_tree=>eventid_expand_no_children.
    w_event-appl_event = abap_true.
    APPEND w_event TO t_events.

    CALL METHOD me->r_o_alv_tree->set_registered_events
      EXPORTING
        events                    = t_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

    me->add_nodes_tree( ).


    SET HANDLER: me->on_node_double_click  FOR me->r_o_alv_tree,
                 me->on_item_double_click  FOR me->r_o_alv_tree.


  ENDMETHOD.


  METHOD set_ok_code_alv.
    IF me->r_o_alv_grid IS BOUND.
      me->r_o_alv_grid->set_function_code(
        CHANGING
          c_ucomm = cv_ucomm
      ).
    ENDIF.
  ENDMETHOD.


  METHOD visualizar.
    DATA: ol_cont_tree TYPE REF TO cl_gui_container,
          ol_cont_grid TYPE REF TO cl_gui_container
          .

    me->r_o_container_splt = ir_container.

    IF me->r_o_splitter IS NOT BOUND.

      CREATE OBJECT me->r_o_splitter
        EXPORTING
          parent            = me->r_o_container_splt
          rows              = 1
          columns           = 2
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2.
      IF sy-subrc IS NOT INITIAL.
        "Erro ao gerar elementos de interface
        RAISE EXCEPTION TYPE zcx_max_cluster_view
          EXPORTING
            vm_msgv1 = CONV #( 'Erro ao gerar elementos de interface'(003) ).
      ENDIF.

      "Tamanho de cada linha no divisor
      me->r_o_splitter->set_column_width( id = 1 width = 21 ).
      me->r_o_splitter->set_column_width( id = 2 width = 79 ).

*     Seleciona o primeiro nivel
      READ TABLE me->t_key_nivel
            INTO me->w_key_nivel_sel
           INDEX 1.
    ENDIF.

    IF me->r_o_splitter IS BOUND.
      "Tree
      CALL METHOD me->r_o_splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = ol_cont_tree.

      me->requisitar_tree( ol_cont_tree ).


*    "Grid
      CALL METHOD me->r_o_splitter->get_container
        EXPORTING
          row       = 1
          column    = 2
        RECEIVING
          container = ol_cont_grid.

      me->requisitar_grid( ol_cont_grid ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
