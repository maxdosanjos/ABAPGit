****************************************
* Report de teste para a cluster view
****************************************

REPORT zzmax_cluster_view_sample.

TABLES:
    scarr
    .

DATA: og_cluster_view TYPE REF TO zcl_max_cluster_view
      .


SELECTION-SCREEN BEGIN OF BLOCK bl1.
SELECT-OPTIONS: s_carrid FOR scarr-carrid.
SELECTION-SCREEN END OF BLOCK bl1.

START-OF-SELECTION.
  PERFORM f_executar.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  og_cluster_view->visualizar( ).

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_executar .
  DATA:
          tl_key_data_1  TYPE zcl_max_cluster_view=>y_t_key_item,
          tl_scarr       TYPE STANDARD TABLE OF scarr,

          wl_key_nivel    TYPE zcl_max_cluster_view=>y_key_nivel,
          wl_key_data_1 LIKE LINE OF tl_key_data_1,

          ol_x_cluster TYPE REF TO zcx_max_cluster_view,
          ol_dialog TYPE REF TO cl_gui_dialogbox_container
          .

  FIELD-SYMBOLS: <fl_scarr> LIKE LINE OF tl_scarr
                 .
  SELECT *
      UP TO 10 ROWS
    INTO TABLE tl_scarr
    FROM scarr
   WHERE carrid IN s_carrid.

  TRY.
      CREATE OBJECT og_cluster_view.

      wl_key_nivel-nivel_key     = '1'.
      wl_key_nivel-nivel_descr   = 'Teste nível 1'.
      wl_key_nivel-nivel_pai_key = space.

      LOOP AT tl_scarr ASSIGNING <fl_scarr>.


        wl_key_data_1-item_key = <fl_scarr>-carrid.

        INSERT wl_key_data_1 INTO TABLE tl_key_data_1.

      ENDLOOP.


      og_cluster_view->add_registro(
        iw_key_nivel = wl_key_nivel
        it_key_data  = tl_key_data_1
        it_data      = tl_scarr
      ).


      CALL SCREEN 9000.

    CATCH zcx_max_cluster_view INTO ol_x_cluster.

      MESSAGE ol_x_cluster
      TYPE 'S'
      DISPLAY LIKE 'E'.
  ENDTRY.
ENDFORM.                    " F_EXECUTAR
