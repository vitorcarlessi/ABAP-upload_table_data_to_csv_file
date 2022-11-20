*======================================================================*
*                                                                      *
*                          InterCement                                 *
*                                                                      *
*======================================================================*
*                                                                      *
*                       github.com/vitorcarlessi/                      *
*                                                                      *
*======================================================================*
* Program.....: ZR_UPLOAD_TABLE                                        *
* Module......: ALL                                                    *
* Description.: Upload CSV to Table                                    *
*----------------------------------------------------------------------*
* Author......: Vitor Crepaldi Carlessi                                *
* Date........: 18.11.2022                                             *
*======================================================================*
REPORT zhr_upload_table.

INCLUDE zhr_upload_table_top.
INCLUDE zhr_upload_table_scr.
INCLUDE zhr_upload_table_c01.

INITIALIZATION.
  "Class instance
  CREATE OBJECT go_upload_table.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_csv.
  "Get CSV file locally
  go_upload_table->get_csv_file( ).

START-OF-SELECTION.
  "Start Full Process
  go_upload_table->full_process( ).