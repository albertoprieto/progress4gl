/******************************************************************************/
/* # @(#)                 ID Programa: xxmd59cm.p                            >*/
/* # @(#) Descripcion: Consulta code_mstr cxrp_error_docum_aaaamm            >*/
/* # @(#)                                                                    >*/
/* # @(#)                                                                    >*/
/* # @(#) Parametros:                                                        >*/
/* # @(#)   Parametro   E|S|ES   Tipo   Longitud   Descripcion               >*/
/* # @(#)  -parametro- -E|S|ES- -tipo- -longitud- -descripcion-              >*/
/* # @(#)                                                                    >*/
/* # @(#) Fecha de Creacion: 11/Jun/21 HH:MM  jap                            >*/
/* # @(#) Autor:                                                             >*/
/* # @(#)                                                                    >*/
/*                                                                            */
/* # @(#) Fecha Ultima Modificacion:                                         >*/
/* # @(#) DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___    >*/
/* # @(#) DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___    >*/
/* # @(#) DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___    >*/
/*                                                                            */
/* Variables:                                                                 */
/*      <idvariable> <tipo> <longitud> <uso>                                  */
/*      <idvariable> <tipo> <longitud> <uso>                                  */
/*                                                                            */
/* Ide Modificaciones:                                                        */
/*                                                                            */
/*      <Id> <Usr>  <Fecha>    <Hora>     <Id> <Usr>  <Fecha>    <Hora>       */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--        */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--        */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--        */
/*                                                                            */
/******************************************************************************/
{mfdtitle.i}

run pxgblmgr.p persistent set global_gblmgr_handle.

Define Variable tmp_fldname As Character No-undo.
Define Variable tmp_year    As Character Format "9999" No-undo.
Define Variable tmp_month   As Character Format "99"   No-undo.
Define Variable wrk_auxdate As Date No-undo.
Define Variable wrk_iloop   As Integer No-undo.
Define Variable wrk_linea   As Character No-undo.

Define Frame a
tmp_fldname  Colon 21 Label "cxrp"
tmp_year     Colon 21 Label "Año"
tmp_month    Colon 21 Label "Mes"
With Frame a Side-Labels Width 80.

tmp_fldname = "cxrp_error_docum_".

Repeat:

    Display
        tmp_fldname Format "x(18)"
        tmp_year
        tmp_month
        Skip(1)
    With Frame a.
    
    Update
        tmp_year
        tmp_month     
    With Frame a No-error.
    
    If Length(Input tmp_year) <> 4 Then Do:
        Message "Año no válido, verifique."
        View-as Alert-box.
        Undo, Retry.
        Next-prompt
            tmp_year.
    End.
    
    If Length(Input tmp_month) <> 2 Then Do:
        Message "Mes no válido, verifique."
        View-as Alert-box.
        Undo, Retry.
        Next-prompt
            tmp_month.
    End.
    
    wrk_auxdate = Date("01" + "01" + String(tmp_year)) No-error.    
    If error-status:error Then Do:
        Message "Año no válido, verifique."
        View-as Alert-box.
        Undo, Retry.
        Next-prompt
            tmp_year.
    End.
    
    wrk_auxdate = Date("01" + String(tmp_month) + String(tmp_year)) No-error.
    If error-status:error Then Do:
        Message "Mes no válido, verifique."
        View-as Alert-box.
        Undo, Retry.
        Next-prompt
        tmp_month.
    End.
    
    Assign
        tmp_fldname = "cxrp_error_docum_" + 
                        String(tmp_year)  + 
                        String(tmp_month).

    {mfselprt.i "page" 132}
    {mfphead.i}

    Find First code_mstr Where code_fldname = tmp_fldname No-lock No-error.
    
    If Available code_mstr Then Run print_reporte1.
    {mfreset.i}
    
End.

Procedure print_reporte1.

    Display Substring(tmp_fldname,18,6) No-label Format "9999/99".
    Put
    "---------------------"
    "---------------------"
    "---------------------"
    .
    For Each code_mstr Where code_fldname = tmp_fldname
    No-lock:
    
        wrk_linea = code_cmmt.

        Do wrk_iloop = 1 To Num-entries(code_cmmt, " "):
            wrk_linea = Replace(wrk_linea,"  "," ").
            wrk_linea = Replace(wrk_linea,"~n ","~n").
            
            If Num-entries(wrk_linea) < 4 Then Do:
                Display
                    code_value No-label View-as Editor Size 30 By 2
                    wrk_linea  No-label View-as Editor Size 30 By 2
                    With Frame l1.
            End.
            
            If Num-entries(wrk_linea) >= 4 And Num-entries(wrk_linea) <= 8
            Then Do:
                Display
                    code_value No-label View-as Editor Size 30 By 3
                    wrk_linea  No-label View-as Editor Size 30 By 3
                    With Frame l2.
            End.
            
            
            If Num-entries(wrk_linea) >= 9 Then Do:
                Display
                    code_value No-label View-as Editor Size 30 By 7
                    wrk_linea  No-label View-as Editor Size 30 By 7
                    With Frame l3.
            End.
        End.
    End.
End Procedure.     
