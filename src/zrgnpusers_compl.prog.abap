*&---------------------------------------------------------------------*
*& Report ZRGNPUSERS_COMPL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrgnpusers_compl.
data:
  gv_fname type string.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
selection-screen begin of block rep_type with frame title text-001.
parameters:
  p_ar  radiobutton group rtyp user-command act,
  p_pwd radiobutton group rtyp default 'X',
  p_doc radiobutton group rtyp.
selection-screen end of block rep_type.
selection-screen begin of block download with frame title text-002.
parameters:
  p_data  radiobutton group dwnl user-command act default 'X',
  p_templ radiobutton group dwnl,
  p_open  radiobutton group dwnl,
  p_dir   type string default 'C:\Temp',
  p_fname type string default ''.
selection-screen end of block download.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
initialization.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
at selection-screen output.
  perform get_filename.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
start-of-selection.
*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
end-of-selection.
  if p_data eq 'X'.
    perform download_data.
  elseif p_templ eq 'X'.
    perform dowload_template.
  elseif p_open eq 'X'.
    perform open_template.
  endif.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_DATA
form download_data.
  types:
    begin of gs_gnpusers.
      include type zgnpusers.
    types:
      ar_age(4)        type c,
      ar_status        type icon_d,
      pwd_status       type icon_d,
      pwd_age(4)       type c,
      docu             type icon_d,
      history          type icon_d,
      tlo_fullname     type text40,
      crstate_text(20) type c,
      cr_duration(4)   type c,
    end of gs_gnpusers.
  data:
    gt_gnpusers type standard table of gs_gnpusers.

  concatenate p_dir '\' p_fname into gv_fname.
  condense gv_fname.
  select * from zgnpusers into corresponding fields of table gt_gnpusers.

  call function 'GUI_DOWNLOAD'
    exporting
*     BIN_FILESIZE            =
      filename                = gv_fname
*     FILETYPE                = 'ASC'
*     APPEND                  = ' '
      write_field_separator   = 'X'
*     HEADER                  = '00'
*     TRUNC_TRAILING_BLANKS   = ' '
*     WRITE_LF                = 'X'
*     COL_SELECT              = ' '
*     COL_SELECT_MASK         = ' '
*     DAT_MODE                = ' '
*     CONFIRM_OVERWRITE       = ' '
*     NO_AUTH_CHECK           = ' '
*     CODEPAGE                = ' '
*     IGNORE_CERR             = ABAP_TRUE
*     REPLACEMENT             = '#'
*     WRITE_BOM               = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT            = ' '
*     WK1_N_SIZE              = ' '
*     WK1_T_FORMAT            = ' '
*     WK1_T_SIZE              = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS    = ABAP_TRUE
*     VIRUS_SCAN_PROFILE      = '/SCET/GUI_DOWNLOAD'
* IMPORTING
*     FILELENGTH              =
    tables
      data_tab                = gt_gnpusers
*     FIELDNAMES              =
    exceptions
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      others                  = 22.
  if sy-subrc ne 0.
    write: 'Error ', sy-subrc.
    skip.
  else.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form DOWLOAD_TEMPLATE
*&---------------------------------------------------------------------*
form dowload_template.

endform.
*&---------------------------------------------------------------------*
*& Form OPEN_TEPLATE
*&---------------------------------------------------------------------*
form open_template.

endform.
*&---------------------------------------------------------------------*
*& Form GET_FILENAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form get_filename .
  if p_data eq 'X'.
    p_fname = 'GNPACCOUNTS.txt'.
  else.
    if p_ar eq 'X'.
      p_fname = 'AR_Compliance.xlsx'.
    elseif p_pwd eq 'X'.
      p_fname = 'PWD_Compliance.xlsx'.
    else.
      p_fname = 'DOC_Compliance.xlsx'.
    endif.
  endif.
endform.
