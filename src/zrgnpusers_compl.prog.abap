*&---------------------------------------------------------------------*
*& Report ZRGNPUSERS_COMPL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrgnpusers_compl.
data:
  lv_fm_name  type tdsfname,
  gt_gnpusers type standard table of zgnpusers.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
selection-screen begin of block rep_type with frame title text-001.
parameters:
  p_uar radiobutton group rtyp,
  p_pwd radiobutton group rtyp default 'X',
  p_doc radiobutton group rtyp.
selection-screen end of block rep_type.
selection-screen begin of block download with frame title text-002.
parameters:
  p_data radiobutton group dwnl default 'X',
  p_form radiobutton group dwnl,
  p_open radiobutton group dwnl,
  p_wdir type string default 'C:\Temp'.
selection-screen end of block download.
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
