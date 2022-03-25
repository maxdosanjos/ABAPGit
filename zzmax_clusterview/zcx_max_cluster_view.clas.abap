class ZCX_MAX_CLUSTER_VIEW definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_MAX_CLUSTER_VIEW,
      msgid type symsgid value 'ZCLUSTER_VIEW',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'VM_MSGV1',
      attr2 type scx_attrname value 'VM_MSGV2',
      attr3 type scx_attrname value 'VM_MSGV3',
      attr4 type scx_attrname value 'VM_MSGV4',
    end of ZCX_MAX_CLUSTER_VIEW .
  data VM_MSGV1 type SYMSGV .
  data VM_MSGV2 type SYMSGV .
  data VM_MSGV3 type SYMSGV .
  data VM_MSGV4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !VM_MSGV1 type SYMSGV optional
      !VM_MSGV2 type SYMSGV optional
      !VM_MSGV3 type SYMSGV optional
      !VM_MSGV4 type SYMSGV optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_MAX_CLUSTER_VIEW IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->VM_MSGV1 = VM_MSGV1 .
me->VM_MSGV2 = VM_MSGV2 .
me->VM_MSGV3 = VM_MSGV3 .
me->VM_MSGV4 = VM_MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_MAX_CLUSTER_VIEW .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
