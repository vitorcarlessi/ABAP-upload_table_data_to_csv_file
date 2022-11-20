*======================================================================*
*                                                                      *
*                       github.com/vitorcarlessi/                      *
*                                                                      *
*======================================================================*
* Program.....: ZR_UPLOAD_TABLE                                        *
* Include.....: ZR_UPLOAD_TABLE_SCR                                    *
* Module......: ALL                                                    *
* Description.: Upload CSV to Table                                    *
*----------------------------------------------------------------------*
* Author......: Vitor Crepaldi Carlessi                                *
* Date........: 18.11.2022                                             *
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_rsrd1 TYPE rsrd1-tbma_val OBLIGATORY,
            p_csv   TYPE pathextern     OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.