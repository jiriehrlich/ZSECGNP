*&---------------------------------------------------------------------*
*& Report ZGNPUSERS_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrgnpusers_migration.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
parameters:
  p_down radiobutton group act default 'X',
  p_upl  radiobutton group act,
  p_file type string default 'C:\Temp\GNPACCOUNTS.txt'.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
initialization.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
start-of-selection.
  if p_down = 'X'.
    perform download_file.
  else.
    perform upload_file.
  endif.

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
end-of-selection.

*&---------------------------------------------------------------------*
*& Form UPLOAD_FILE
*&---------------------------------------------------------------------*
form upload_file .
  data:
    lt_users type standard table of zgnpusers,
    ls_users type zgnpusers,
    lv_file  type string.

  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = p_file
      mask             = ',*.txt.'
      mode             = 'O'
      title            = 'Upload File'(078)
    importing
      filename         = lv_file
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.

  call function 'GUI_UPLOAD'
    exporting
      filename                = lv_file
      has_field_separator     = 'X'
    tables
      data_tab                = lt_users
    exceptions
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      others                  = 17.
  if sy-subrc ne 0.
    write: 'Error ', sy-subrc.
    skip.
  else.
    delete from zgnpusers.
    delete lt_users index 1.
    loop at lt_users into ls_users.
      ls_users-mandt = sy-mandt.
      if ls_users-usruuid is initial.
        try.
            ls_users-usruuid = cl_system_uuid=>create_uuid_c32_static( ).
          catch cx_uuid_error.
        endtry.
      endif.
      modify lt_users from ls_users.
    endloop.
    modify zgnpusers from table lt_users.
  endif.

endform.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_FILE
*&---------------------------------------------------------------------*
form download_file .
  data:
    lt_users type standard table of zgnpusers,
    lv_file  type string.

  select * from zgnpusers into corresponding fields of table lt_users.
  call function 'GUI_DOWNLOAD'
    exporting
*     BIN_FILESIZE            =
      filename                = p_file
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
      data_tab                = lt_users
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
