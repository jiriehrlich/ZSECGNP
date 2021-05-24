*&---------------------------------------------------------------------*
*& Report ZRGNPUSERS_CONF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrgnpusers_conf.
data:
  lv_fm_name  type tdsfname,
  gt_gnpusers type standard table of zgnpusers.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
selection-screen begin of block global with frame title text-001.
parameters:
  p_dest  type rfcdest,
  p_arage type numc3,
  p_arthd type numc2,
  p_pwage type numc3,
  p_pwthd type numc2.
selection-screen end of block global.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
initialization.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
start-of-selection.
  perform select_data.
*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
end-of-selection.
  perform generate_report.
*&---------------------------------------------------------------------*
*& Form SELECT_DATA
*&---------------------------------------------------------------------*
form select_data.
  select * from zgnpusers into corresponding fields of table gt_gnpusers.
endform.
*&---------------------------------------------------------------------*
*& Form GENERATE_REPORT
*&---------------------------------------------------------------------*
form generate_report .
  call function 'SSF_FUNCTION_MODULE_NAME'
    exporting
      formname           = 'ZGNP_COMPREPORT_PWD'
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    importing
      fm_name            = lv_fm_name
    exceptions
      no_form            = 1
      no_function_module = 2
      others             = 3.
  if sy-subrc <> 0.
  endif.

  call function lv_fm_name
* EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
*   CONTROL_PARAMETERS         =
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
*   OUTPUT_OPTIONS             =
*   USER_SETTINGS              = 'X'
* IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      gnpusers         = gt_gnpusers
    exceptions
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      others           = 5.
  if sy-subrc <> 0.
  endif.
endform.
