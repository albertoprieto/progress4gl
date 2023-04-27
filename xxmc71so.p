/******************************************************************************/
/* # @(#)                 ID Programa:xxmc71so.p                             >*/
/* # @(#) Descripcion: Rep consulta anticipos (cliente, factura, pedido      >*/
/* # @(#)                                                                    >*/
/* # @(#)                                                                    >*/
/* # @(#) Parametros:                                                        >*/
/* # @(#)   Parametro   E|S|ES   Tipo   Longitud   Descripcion               >*/
/* # @(#)  -parametro- -E|S|ES- -tipo- -longitud- -descripcion-              >*/
/* # @(#)                                                                    >*/
/* # @(#) Fecha de Creacion: 21/jun/21 HH:MM  jlm                           >*/
/* # @(#) Autor:                                                             >*/
/* # @(#)                                                                    >*/
/* # @(#) Fecha Ultima Modificacion:                                         >*/
/* # @(#) DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___    >*/
/* # @(#) DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___    >*/
/* # @(#) DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___    >*/
/* Variables:                                                                 */
/*      <idvariable> <tipo> <longitud> <uso>                                  */
/*      <idvariable> <tipo> <longitud> <uso>                                  */
/*                                                                            */
/* Ide Modificaciones:                                                        */
/*      jap01 24-jun-21 Agregar seguridad por usuario-agente-cliente          */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--        */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--        */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--        */
/*                                                                            */
/******************************************************************************/

{mfdtitle.i}

run pxgblmgr.p persistent set global_gblmgr_handle.
Define var wrk_bill as char.
define var wrk_factura as char.
define var wrk_pedido as char.

define var wrk_nombre_cliente as char format "X(50)".

/*jap01-ini*/
Define Variable tmp_agente_error As Logical   No-Undo.
Define Variable tmp_msg_error    As Character No-Undo.
Define Variable tmp_existe_ped   As Logical   No-Undo.
/*jap01-fin*/

Form
wrk_bill    Column-Label "Cliente"
code_value  Column-Label "Factura"
xxar_eff    Column-Label "Fch fact"
xxar_curramt Column-Label "Impte USD"
code_cmmt   Column-Label "Pedidos"
Header  
 
"Cliente..:" wrk_bill Skip
 
"Nombre...:" wrk_nombre_cliente Skip
"Factura..:" wrk_Factura Skip
"pedidos..:" wrk_pedido Skip(1)
With Frame reporte Down Width 350.

Define Frame a
wrk_bill colon 21 Label "Cliente"
wrk_factura colon 21 label "factura"
wrk_pedido colon 21 label "Pedido"
With Width 80 Side-Labels.

Repeat:
    Update 
        wrk_bill
        wrk_factura
        wrk_pedido
        With Frame a.
    If wrk_bill = "" Then Do:
        Message "debe capturar el numero de cliente"
        View-As Alert-Box. 
        Next.
    End.
   
    /*jap01-ini*/
    Find first admstr where adaddr = wrk_bill No-Lock No-Error.
    If Not Available admstr Then Do:
        Message "Cliente no encontrado, verifique"
        View-As Alert-box.
        Undo, Retry.
    End.    
    Pause 0.
    tmp_msg_error = "Visualizar información".
    tmp_existe_ped = False.
    {gprun.i ""xxmi91so.p"" "(tmp_existe_ped,
                              wrk_bill,
                              wrk_bill,
                              tmp_msg_error,
                              Output tmp_agente_error)"} .
    If tmp_agente_error = True Then Do:
        Undo , Retry .
    End.   
    /*jap01-fin*/
      
    {mfselprt.i "page" 132}
    {mfphead.i}
   
    Run print_reporte1.
    {mfreset.i}
    Clear Frame Reporte.
   
End.

PROCEDURE print_reporte1.
   
    View Frame reporte.
    Find first admstr where adaddr = wrk_bill No-Lock No-Error. 
    wrk_nombre_cliente = adname.

    For Each code_mstr Where 
        code_fldname = "cliente_anticipo_" + wrk_bill
        And
        (code_value = wrk_Factura Or wrk_factura = "")
        And
        (Lookup(wrk_pedido,code_cmmt) >0 Or wrk_pedido = "")
        No-Lock: 
        Find First xxarmstr Where
                xxar_ardref = code_value And
                (xxar_arnbr Begins "Z" Or xxar_arnbr Begins "OV")
                No-Lock No-Error. 
        Display  
            wrk_bill
            code_value
            xxar_eff When Avail(xxarmstr)
            xxar_curramt When Avail(xxarmstr)
            code_cmmt
            With Frame reporte.
        Down With Frame reporte.
        {mfrpchk.i}
    End.    
End.
