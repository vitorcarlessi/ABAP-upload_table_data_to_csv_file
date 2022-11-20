*======================================================================*
*                                                                      *
*                       github.com/vitorcarlessi/                      *
*                                                                      *
*======================================================================*
* Program.....: ZR_UPLOAD_TABLE                                        *
* Include.....: ZR_UPLOAD_TABLE_C01                                    *
* Module......: ALL                                                    *
* Description.: Upload CSV to Table                                    *
*----------------------------------------------------------------------*
* Author......: Vitor Crepaldi Carlessi                                *
* Date........: 18.11.2022                                             *
*======================================================================*
*----------------------------------------------------------------------*
*   Classe GCL_UPLOAD_TABLE                                            *
*----------------------------------------------------------------------*
CLASS gcl_upload_table DEFINITION FINAL.

    *----------------------------------------------------------------------*
    *   Public Section GCL_UPLOAD_TABLE                                    *
    *----------------------------------------------------------------------*
      PUBLIC SECTION.
    
        "Atributos Classe - Seção Publica
        DATA: mt_table_header_fields TYPE truxs_t_text_data ##NEEDED.
    
        METHODS:
          "1)Constructor - Constructor Method
          constructor,
    
          "2)Full Process      -> Performs all processing
          full_process,
    
          "3)Get CSV File      -> Get CSV file locally
          get_csv_file,
    
          "4)Upload CSV        -> Upload CSV to internal table
          upload_csv CHANGING pt_csv_header      TYPE truxs_t_text_data
                              pt_converted_table TYPE truxs_t_text_data,
    
          "5)Check Fields       -> Check if we have the same table fields
          check_fields IMPORTING pt_csv_header TYPE truxs_t_text_data,
    
          "6)Update Table       -> Upload data into transparent table
          update_table IMPORTING pt_converted_table TYPE truxs_t_text_data,
    
          "7)Create BAL_LOG     -> Create BAL LOG method
          create_bal_log,
    
          "8)Add MSG to BAL_LOG -> Add mensage to BAL_LOG
          add_msg_to_bal_log CHANGING ps_msg_log TYPE bal_s_msg,
    
          "9)Show BAL_LOG       -> Show BAL_LOG mensage
          show_bal_log.
    
    ENDCLASS.
    
    CLASS gcl_upload_table IMPLEMENTATION.
    *----------------------------------------------------------------------*
    * 1) Classe GCL_UPLOAD_TABLE->CONSTRUCTOR                              *
    *----------------------------------------------------------------------*
      METHOD constructor ##NEEDED.
    
    *----------------------------------------------------------------*
    * Variáveis                                                      *
    *----------------------------------------------------------------*
        DATA: lv_srtfd TYPE indx-srtfd.
    
        "Program Name
        lv_srtfd = sy-repid.
    
        "Check if there's more than one instance running
        CALL FUNCTION 'ENQUEUE_ESINDX'
          EXPORTING
            srtfd          = lv_srtfd
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2.
    
        IF sy-subrc IS NOT INITIAL.
          "Programa em execução noutra instancia. Tente novamente mais tarde.
          MESSAGE text-t07 TYPE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.
    
        "Block Instance
        CALL FUNCTION 'ENQUEUE_E_TRDIR'
          EXPORTING
            name           = sy-repid
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
    
        IF sy-subrc IS NOT INITIAL.
          "Erro ao bloquear instância
          MESSAGE text-t03 TYPE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.
    
      ENDMETHOD.
    *----------------------------------------------------------------------*
    * 2) Classe GCL_UPLOAD_TABLE->FULL_PROCESS                             *
    *----------------------------------------------------------------------*
      METHOD full_process.
    
    *----------------------------------------------------------------*
    * Tables                                                         *
    *----------------------------------------------------------------*
        DATA: lt_csv_header      TYPE truxs_t_text_data,
              lt_converted_table TYPE truxs_t_text_data.
    
        "1)Upload CSV
        go_upload_table->upload_csv( CHANGING pt_csv_header         = lt_csv_header
                                              pt_converted_table    = lt_converted_table ).
    
        "2)Check Table Fields
        go_upload_table->check_fields( EXPORTING pt_csv_header      = lt_csv_header ).
    
        "3)BAL_LOG Create
        go_upload_table->create_bal_log( ).
    
        "4)Upload table data
        go_upload_table->update_table( EXPORTING pt_converted_table = lt_converted_table ).
    
        "5)BAL_LOG Show
        go_upload_table->show_bal_log( ).
    
      ENDMETHOD.
    *----------------------------------------------------------------------*
    * 3) Classe GCL_UPLOAD_TABLE->GET_CSV_FILE                             *
    *----------------------------------------------------------------------*
      METHOD get_csv_file.
    
    *----------------------------------------------------------------*
    * Variables                                                      *
    *----------------------------------------------------------------*
        DATA: lv_file TYPE localfile.
    
        "File manager support to locate file in a directory (on value request)
        CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
          EXPORTING
            program_name  = sy-repid
            dynpro_number = sy-dynnr
          CHANGING
            file_name     = lv_file
          EXCEPTIONS
            mask_too_long = 1
            OTHERS        = 2.
    
        "Check the function return
        IF sy-subrc IS INITIAL.
          p_csv = lv_file.
        ENDIF.
    
      ENDMETHOD.
    *----------------------------------------------------------------------*
    * 4) Classe GCL_UPLOAD_TABLE->UPLOAD_CSV                               *
    *----------------------------------------------------------------------*
      METHOD upload_csv.
    
    *----------------------------------------------------------------*
    * Constants                                                      *
    *----------------------------------------------------------------*
        CONSTANTS: lc_filetype  TYPE char10 VALUE 'ASC'.
    
    *----------------------------------------------------------------*
    * Variables                                                      *
    *----------------------------------------------------------------*
        DATA: lv_filename TYPE string.
    
        "Filename
        lv_filename = p_csv.
    
        "Upload CSV
        CALL FUNCTION 'GUI_UPLOAD'
          EXPORTING
            filename                = lv_filename
            filetype                = lc_filetype
          TABLES
            data_tab                = pt_converted_table
          EXCEPTIONS
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
            OTHERS                  = 17.
    
        "Check Errors
        IF sy-subrc IS NOT INITIAL.
          "Erro ao fazer upload do arquivo CSV
          MESSAGE text-t01 TYPE 'E'.
          RETURN.
        ENDIF.
    
        "Get Header
        READ TABLE pt_converted_table ASSIGNING FIELD-SYMBOL(<fs_converted_table>) INDEX 1.
        IF sy-subrc IS INITIAL.
          "Append Header
          APPEND INITIAL LINE TO pt_csv_header ASSIGNING FIELD-SYMBOL(<fs_csv_header>).
          <fs_csv_header> = <fs_converted_table>.
        ENDIF.
    
        "Remove Header from converted_table
        DELETE pt_converted_table INDEX 1.
    
      ENDMETHOD.
    *----------------------------------------------------------------------*
    * 5) Classe GCL_UPLOAD_TABLE->CHECK_FIELDS                             *
    *----------------------------------------------------------------------*
      METHOD check_fields.
    
    *----------------------------------------------------------------*
    * Constants                                                      *
    *----------------------------------------------------------------*
        CONSTANTS: lc_seperator TYPE char01 VALUE ';'.
    
    *----------------------------------------------------------------*
    * Classes                                                        *
    *----------------------------------------------------------------*
        DATA: lo_tabtype     TYPE REF TO cl_abap_tabledescr,
              lo_struct_type TYPE REF TO cl_abap_structdescr.
    
    *----------------------------------------------------------------*
    * Tables                                                         *
    *----------------------------------------------------------------*
        DATA: lt_comp_tab TYPE cl_abap_structdescr=>component_table.
    
    *----------------------------------------------------------------*
    * Variables                                                      *
    *----------------------------------------------------------------*
        DATA: lv_table_name   TYPE tabname.
    
        "Get Table Name
        lv_table_name = p_rsrd1.
    
        "Description of the type using relative/absolute names
        lo_struct_type ?= cl_abap_typedescr=>describe_by_name( lv_table_name ).
    
        "Table LT_COMP_TAB receive the value
        lt_comp_tab     = lo_struct_type->get_components( ).
    
        "Factory method for generating structure types without reuse
        lo_struct_type = cl_abap_structdescr=>create( lt_comp_tab ).
    
        "Factory method for generating structure types without reuse
        lo_tabtype     = cl_abap_tabledescr=>create( lo_struct_type ).
    
        "Get Name of Fields - CSV Header
        lo_struct_type ?= lo_tabtype->get_table_line_type( ).
    
        "Get CSV Header
        APPEND INITIAL LINE TO mt_table_header_fields ASSIGNING FIELD-SYMBOL(<fs_table_header_fields>).
    
        "LOOP -> Name of fields in table
        LOOP AT lo_struct_type->components ASSIGNING FIELD-SYMBOL(<fs_components>).
    
          AT LAST.
            "Last Line - Get only the last field name
            <fs_table_header_fields> = |{ <fs_table_header_fields> }{ <fs_components>-name }|.
            EXIT.
          ENDAT.
    
          "Get field name and separator
          <fs_table_header_fields> = |{ <fs_table_header_fields> }{ <fs_components>-name }{ lc_seperator }|.
    
        ENDLOOP.
    
        "Check we have the same columns fields
        READ TABLE pt_csv_header ASSIGNING FIELD-SYMBOL(<fs_csv_header>) INDEX 1.
    
        "Is CSV Header the same as Table Passed?
        IF <fs_csv_header> NE <fs_table_header_fields>.
          "As colunas de HRX e HDX para essa tabela não tem os campos idênticos
          MESSAGE text-t02 TYPE 'E'.
          RETURN.
        ENDIF.
    
      ENDMETHOD.
    *----------------------------------------------------------------------*
    * 6) Classe GCL_UPLOAD_TABLE->UPDATE_TABLE                             *
    *----------------------------------------------------------------------*
      METHOD update_table.
    
    *----------------------------------------------------------------*
    * Constants                                                      *
    *----------------------------------------------------------------*
        CONSTANTS: lc_seperator    TYPE char01          VALUE ';',
                   lc_colon        TYPE char01          VALUE ':',
                   lc_icon_success TYPE c               VALUE 'S',
                   lc_icon_error   TYPE c               VALUE 'N',
                   lc_error_msg    TYPE char50          VALUE 'Erro ao atualizar tabela'    ##NO_TEXT,
                   lc_success_msg  TYPE char50          VALUE 'Sucesso ao atualizar tabela' ##NO_TEXT,
                   lc_line         TYPE char05          VALUE 'Linha'                       ##NO_TEXT,
                   lc_msgid        TYPE bal_s_msg-msgid VALUE '00',
                   lc_msgno        TYPE bal_s_msg-msgno VALUE '001'.
    
    *----------------------------------------------------------------*
    * Classes                                                        *
    *----------------------------------------------------------------*
        DATA: lo_tabtype     TYPE REF TO cl_abap_tabledescr,
              lo_struct_type TYPE REF TO cl_abap_structdescr,
              lr_data        TYPE REF TO data.
    
    *----------------------------------------------------------------*
    * Tables                                                         *
    *----------------------------------------------------------------*
        DATA: lt_comp_tab TYPE cl_abap_structdescr=>component_table.
    
    *----------------------------------------------------------------*
    * Structures and Work Areas                                      *
    *----------------------------------------------------------------*
        DATA: ls_msg_log TYPE bal_s_msg.
    
    *----------------------------------------------------------------*
    * Variables                                                      *
    *----------------------------------------------------------------*
        DATA: lv_table_name TYPE tabname,
              lv_tabix      TYPE sy-tabix,
              lv_msgv1      TYPE bal_s_msg-msgv1,
              lv_msgty      TYPE bal_s_msg-msgty.
    
    *----------------------------------------------------------------*
    * Field-Symbols                                                  *
    *----------------------------------------------------------------*
        FIELD-SYMBOLS: <lt_table_values> TYPE STANDARD TABLE.
    
        "Get Table Name
        lv_table_name = p_rsrd1.
    
        "Description of the type using relative/absolute names
        lo_struct_type ?= cl_abap_typedescr=>describe_by_name( lv_table_name ).
    
        "Table LT_COMP_TAB receive the value
        lt_comp_tab     = lo_struct_type->get_components( ).
    
        "Factory method for generating structure types without reuse
        lo_struct_type = cl_abap_structdescr=>create( lt_comp_tab ).
    
        "Factory method for generating structure types without reuse
        lo_tabtype     = cl_abap_tabledescr=>create( lo_struct_type ).
    
        "Get Name of Fields - CSV Header
        lo_struct_type ?= lo_tabtype->get_table_line_type( ).
    
        "Dynamic Table Create
        CREATE DATA lr_data TYPE HANDLE lo_tabtype.
        ASSIGN lr_data->* TO <lt_table_values>.
    
        "Generate ITAB from CSV File
        CALL FUNCTION 'TEXT_CONVERT_TEX_TO_SAP'
          EXPORTING
            i_field_seperator    = lc_seperator
            i_tab_raw_data       = pt_converted_table
          TABLES
            i_tab_converted_data = <lt_table_values>
          EXCEPTIONS
            conversion_failed    = 1
            OTHERS               = 2.
    
        IF sy-subrc IS NOT INITIAL.
          "Erro ao fazer upload do arquivo CSV
          MESSAGE text-t01 TYPE 'E'.
          RETURN.
        ENDIF.
    
        "Create BAL_LOG
    
        "LOOP -> CSV Lines
        LOOP AT <lt_table_values> ASSIGNING FIELD-SYMBOL(<fs_table_values>).
    
          "Clear Variables/Structures
          CLEAR: lv_msgv1, lv_msgty, ls_msg_log.
    
          "Sy-tabix Count
          lv_tabix = sy-tabix.
    
          "Fill begin of message - Linha X:
          lv_msgv1 = |{ lc_line } { lv_tabix }{ lc_colon }|.
    
          "Modify Table
          MODIFY (lv_table_name) FROM <fs_table_values>.
    
          IF sy-subrc IS INITIAL.
            "Success!
            lv_msgv1 = |{ lv_msgv1 } { lc_success_msg }|.
            lv_msgty = lc_icon_success.
          ELSE.
            "Erro!
            lv_msgv1 = |{ lv_msgv1 } { lc_error_msg }|.
            lv_msgty = lc_icon_error.
          ENDIF.
    
          "Application Log: Log header data -> Mapping
          ls_msg_log-msgv1 = lv_msgv1.
          ls_msg_log-msgty = lv_msgty.
          ls_msg_log-msgid = lc_msgid.
          ls_msg_log-msgno = lc_msgno.
    
          "Add BAL_LOG msg
          go_upload_table->add_msg_to_bal_log( CHANGING ps_msg_log = ls_msg_log ).
    
        ENDLOOP.
    
      ENDMETHOD.
    *----------------------------------------------------------------------*
    * 7) Classe GCL_UPLOAD_TABLE->CREATE_BAL_LOG                           *
    *----------------------------------------------------------------------*
      METHOD create_bal_log.
    
    *----------------------------------------------------------------*
    * Estruturas e Work Areas                                        *
    *----------------------------------------------------------------*
        DATA: ls_log TYPE bal_s_log.
    
        "Application Log: External ID
        ls_log-extnumber = sy-cprog.
    
        "Application log: user name
        ls_log-aluser    = sy-uname.
    
        "Application log: Program name
        ls_log-alprog    = sy-repid.
    
        "BAL_LOG Create
        CALL FUNCTION 'BAL_LOG_CREATE'
          EXPORTING
            i_s_log = ls_log
          EXCEPTIONS
            OTHERS  = 1.
    
        "Check the return
        IF sy-subrc IS NOT INITIAL.
          "Erro ao criar o BAL_LOG
          MESSAGE text-t04 TYPE 'E'.
          RETURN.
        ENDIF.
    
      ENDMETHOD.
    *----------------------------------------------------------------------*
    * 8) Classe GCL_UPLOAD_TABLE->ADD_MSG_TO_BAL_LOG                       *
    *----------------------------------------------------------------------*
      METHOD add_msg_to_bal_log.
    
        "BAL_LOG msg add
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_s_msg       = ps_msg_log
          EXCEPTIONS
            log_not_found = 0
            OTHERS        = 1.
    
        "Check the return
        IF sy-subrc IS NOT INITIAL.
          "Erro ao adicionar mensagem ao BAL_LOG
          MESSAGE text-t05 TYPE 'E'.
          RETURN.
        ENDIF.
    
      ENDMETHOD.
    *----------------------------------------------------------------------*
    * 9) Classe GCL_UPLOAD_TABLE->SHOW_BAL_LOG                             *
    *----------------------------------------------------------------------*
      METHOD show_bal_log.
    
        "Show BAL_LOG
        CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
          EXCEPTIONS
            OTHERS = 1.
    
        "Check the return
        IF sy-subrc IS NOT INITIAL.
          "Erro ao exibir BAL_LOG
          MESSAGE text-t06 TYPE 'E'.
          RETURN.
        ENDIF.
    
      ENDMETHOD.
    
    ENDCLASS.