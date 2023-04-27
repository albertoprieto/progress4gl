/*****************************************************************************/
/* # @(#)                                                                   >*/
/* # @(#) Descripcion: xxstp_mstr MNTO                                      >*/
/* # @(#)                                                                   >*/
/* # @(#)                                                                   >*/
/* # @(#) Parametros:                                                       >*/
/* # @(#)   Parametro   E|S|ES   Tipo   Longitud   Descripcion              >*/
/* # @(#)  -parametro- -E|S|ES- -tipo- -longitud- -descripcion-             >*/
/* # @(#)                                                                   >*/
/* # @(#) Fecha de Creacion: 14/May/21 HH:MM  jap                           >*/
/* # @(#) Autor:                                                            >*/
/* # @(#)                                                                   >*/
/* # @(#) Fecha Ultima Modificacion:                                        >*/
/* # @(#) DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   >*/
/* # @(#) DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   >*/
/* # @(#) DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   DD/MMM/AA HH:MM ___   >*/
/* Variables:                                                                */
/*      <idvariable> <tipo> <longitud> <uso>                                 */
/*      <idvariable> <tipo> <longitud> <uso>                                 */
/*                                                                           */
/* Ide Modificaciones:                                                       */
/*                                                                           */
/*      <Id> <Usr>  <Fecha>    <Hora>     <Id> <Usr>  <Fecha>    <Hora>      */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--       */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--       */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--       */
/******************************************************************************/

Define Stream straud.
Define Variable arch_salida_aud As Character.
Define Variable separador       As Character Format "X" Initial "!".
Define Variable tmp_campo       As Character.
Define Buffer buf_code_mstr For code_mstr.
Define Variable del-yn like mfc_logical Initial No Format "Si/No".
Define Variable batchdelete As Character Format "x(1)" No-undo.
{gpfieldv.i}      /* var defs for gpfield.i */
{mfdtitle.i}
  
Define Variable tmp_userid Like xxstp_userid No-undo. 
Define Variable tmp_siteo  Like xxstp_siteo  No-undo.
Define Variable tmp_loco   Like xxstp_loco   No-undo.
Define Variable tmp_sited  Like xxstp_sited  No-undo.               
Define Variable tmp_locd   Like xxstp_locd   No-undo.
Define Variable tmp_lineas Like xxstp_lineas Format "x(50)" No-undo.
Define Variable tmp_grupos Like xxstp_grupos Format "x(50)" No-undo.
Define Variable tmp_items  Like xxstp_items  Format "x(50)" No-undo.

Define Variable wrk_error As Character No-undo.
Define Variable i As Integer No-undo.

Form
    tmp_userid   Label "Usuario"
    tmp_siteo    Label "Almacen origen" 
    tmp_loco     Label "Ubicación origen"
    tmp_sited    Label "Almacen destino" 
    tmp_locd     Label "Ubicación destino"
    Skip(1)
    tmp_lineas   Label "Líneas"
    tmp_grupos   Label "Grupos"
    tmp_items    Label "Artículos"
With Frame a Side-Labels Width 80.

Repeat With Frame a:
    batchdelete = "".        
    Prompt-for
        tmp_userid  
        tmp_siteo
        tmp_loco
        tmp_sited               
        tmp_locd
        batchdelete no-label when (batchrun)

    Editing:
        If frame-field = "tmp_userid" Then Do:
            {mfnp.i xxstp_mstr tmp_userid xxstp_userid
                               tmp_userid xxstp_userid xxstp_unique}
        End.
        If frame-field = "tmp_siteo" Then Do:
            {mfnp.i xxstp_mstr tmp_siteo xxstp_siteo
                               tmp_siteo xxstp_siteo xxstp_unique}
        End.
        If frame-field = "tmp_loco" Then Do:
            {mfnp.i xxstp_mstr tmp_loco xxstp_loco
                               tmp_loco xxstp_loco xxstp_unique}
        End.
        If frame-field = "tmp_sited" Then Do:
        {mfnp.i xxstp_mstr tmp_sited xxstp_sited
                           tmp_sited xxstp_sited xxstp_unique}
        End.
        If frame-field = "tmp_locd" Then Do:
            {mfnp.i xxstp_mstr tmp_locd xxstp_locd
                               tmp_locd xxstp_locd xxstp_unique}
        End.
        If frame-field = "tmp_lineas" Then Do:
            {mfnp.i xxstp_mstr tmp_lineas xxstp_lineas
                               tmp_lineas xxstp_lineas xxstp_unique}
        End.
        If frame-field = "tmp_grupos" Then Do:
            {mfnp.i xxstp_mstr tmp_grupos xxstp_grupos
                               tmp_grupos xxstp_grupos xxstp_unique}
        End.
        If frame-field = "tmp_items" Then Do:
            {mfnp.i xxstp_mstr tmp_items xxstp_items
                               tmp_items xxstp_items xxstp_unique}
        End.
        
        If recno <> ? Then do:
      
            Display
                xxstp_userid @ tmp_userid        
                xxstp_siteo  @ tmp_siteo
                xxstp_loco   @ tmp_loco
                xxstp_sited  @ tmp_sited
                xxstp_locd   @ tmp_locd
                xxstp_lineas @ tmp_lineas
                xxstp_grupos @ tmp_grupos
                xxstp_items  @ tmp_items
                With Frame a 1 Col.
        End.
    End.             
    If Input tmp_userid  = "" Then Do:
        Message "Usuario requerido".
        Undo, Retry.
        Next-prompt tmp_userid.
    End. 

    If Input tmp_siteo  = "" Then Do:
        Message "Almacen origen requerido".
        Undo, Retry.
    Next-prompt tmp_siteo.
    End.
    If Input tmp_loco  = "" Then Do:
        Message "Ubicación origen requerida".
        Undo, Retry.
        Next-prompt tmp_loco.
    End.
    If Input tmp_sited  = "" Then Do:
        Message "Almacen destino requerido".
        Undo, Retry.
        Next-prompt tmp_sited.
    End.
    If Input tmp_locd  = "" Then Do:
        Message "Ubicación destino requerido".
        Undo, Retry.
        Next-prompt tmp_locd.
    End.

    Find First usr_mstr Where usr_userid = Input tmp_userid
    No-lock No-error.
    If Not Available usr_mstr Then Do:
        Message "Usuario no válido, verifique.".
        Undo,Retry.
    End.
    
    Find First loc_mstr Where loc_site = Input tmp_siteo
                         And  loc_loc  = Input tmp_loco
    No-lock No-error.
    If Not Available loc_mstr Then Do:
        Message "Almacen origen no válido en Ubicación origen, verifique.".
        Undo,Retry.
    End.
             
    Find First loc_mstr Where loc_loc = Input tmp_locd
                        And   loc_site = Input tmp_sited
    No-lock No-error.
    If Not Available loc_mstr Then Do:
        Message "Ubicación destino no válida, verifique".
        Undo,Retry.
    End.
                     
    Find First si_mstr Where si_site = Input tmp_sited
    No-lock No-error.
    If Not Available si_mstr Then Do:
        Message "Almacen destino no válido, verifique.".
        Undo,Retry.
    End.
                       
                       
    Find First xxstp_mstr Where xxstp_userid = Input tmp_userid
    And xxstp_siteo    = Input tmp_siteo
    And xxstp_loco     = Input tmp_loco
    And xxstp_sited    = Input tmp_sited
    And xxstp_locd     = Input tmp_locd
    No-error.
    
    If Not Available xxstp_mstr Then Do:
        
        Assign
            tmp_userid
            tmp_siteo
            tmp_loco
            tmp_sited
            tmp_locd.

        Clear Frame a.
        
        Display
            tmp_userid
            tmp_siteo
            tmp_loco
            tmp_sited
            tmp_locd
            Input tmp_lineas @ tmp_lineas
            Input tmp_grupos @ tmp_lineas 
            Input tmp_items  @ tmp_items.
            
        {pxmsg.i &MSGNUM=1 &ERRORLEVEL=1}
        Create xxstp_mstr.
            
        Assign
            xxstp_userid = Input tmp_userid
            xxstp_siteo  = Input tmp_siteo
            xxstp_loco   = Input tmp_loco
            xxstp_sited  = Input tmp_sited
            xxstp_locd   = Input tmp_locd
            xxstp_dateadd = today
            xxstp_timeadd = time
            xxstp_usradd  = global_userid.
              
        Prompt-for
            tmp_lineas 
            tmp_grupos
            tmp_items.
        
        Assign
            tmp_lineas
            tmp_grupos
            tmp_items.

        wrk_error = "".
        Do i = 1 To Num-entries(tmp_lineas):
            Find First pl_mstr Where
            pl_prod_line   = Entry(i,tmp_lineas) No-lock No-error.
            If Not Available(pl_mstr) Then Do:
                If  wrk_error = "" Then
                    wrk_error = Entry(i,tmp_lineas).
                Else
                    wrk_error = wrk_error + "," + Entry(i,tmp_lineas).
            End.
        End.
        If wrk_error <> "" Then Do:
            Message "Lineas capturadas son inválidas:" Skip
            wrk_error
            View-as Alert-box.
            Undo, Retry.
        End.
        
        wrk_error = "".
        Do i = 1 To Num-entries(tmp_grupos):
            Find First code_mstr Where 
            code_fldname = "pt_group" And
            code_value   = Entry(i,tmp_grupos) No-lock No-error.
            If Not Available(code_mstr) Then Do:
                If  wrk_error = "" Then
                    wrk_error = Entry(i,tmp_grupos).
                Else
                    wrk_error = wrk_error + "," + Entry(i,tmp_grupos).
            End.        
        End.
        If wrk_error <> ""  Then Do:
            Message "Estos grupos capturados son inválidos:" Skip
            wrk_error
            View-as Alert-box.
            Undo, Retry.
        End.    
                                                
        wrk_error = "".
        Do i = 1 To Num-entries(tmp_items):
            Find First pt_mstr Where
            pt_part   = Entry(i,tmp_items) No-lock No-error.
            If Not Available(pt_mstr) Then Do:
                If  wrk_error = "" Then
                    wrk_error = Entry(i,tmp_items).
                Else
            wrk_error = wrk_error + "," + Entry(i,tmp_items).
            End.
        End.
        If wrk_error <> "" Then Do:
            Message "Estos Artículos capturados son inválidos:" Skip
            wrk_error
            View-as Alert-box.
            Undo, Retry.
        End.
            
        Assign
            xxstp_lineas = Input tmp_lineas
            xxstp_grupos = Input tmp_grupos
            xxstp_items  = Input tmp_items.
     
        {xxmi33lb.i
        &id_file = "xxstp_mstr_mnto"
        &id_accion = "ALTA"
        &param01=xxstp_mstr.xxstp_userid
        &param02=xxstp_mstr.xxstp_siteo
        &param03=xxstp_mstr.xxstp_loco
        &param04=xxstp_mstr.xxstp_sited
        &param05=xxstp_mstr.xxstp_locd
        &param06=xxstp_mstr.xxstp_lineas
        &param07=xxstp_mstr.xxstp_grupos
        &param08=xxstp_mstr.xxstp_items
        }
        
        Display 
            xxstp_userid @ tmp_userid
            xxstp_siteo  @ tmp_siteo
            xxstp_loco   @ tmp_loco
            xxstp_sited  @ tmp_sited
            xxstp_locd   @ tmp_locd
            xxstp_lineas @ tmp_lineas
            xxstp_grupos @ tmp_grupos
            xxstp_items  @ tmp_items
            With Frame a.
        Pause.
        Clear Frame a.
        Message "Nuevo registro agregado".
        Pause.
        Clear Frame a.
        Next.
    End.
    
    If Available xxstp_mstr Then Do:
        {pxmsg.i
        &MSGTEXT="'Modificando registro.'"
        &ERRORLEVEL=1}
        
        {xxmi33lb.i
        &id_file = "xxstp_mstr_mnto"
        &id_accion = "MODIFICACION ANTES"
        &param01=xxstp_mstr.xxstp_userid
        &param02=xxstp_mstr.xxstp_siteo
        &param03=xxstp_mstr.xxstp_loco
        &param04=xxstp_mstr.xxstp_sited
        &param05=xxstp_mstr.xxstp_locd
        &param06=xxstp_mstr.xxstp_lineas
        &param07=xxstp_mstr.xxstp_grupos
        &param08=xxstp_mstr.xxstp_items
        }
        
        Prompt-for
            tmp_lineas
            tmp_grupos
            tmp_items
            Go-on(F5 CTRL-D).
        
        Assign 
            tmp_lineas
            tmp_grupos
            tmp_items.
      
        wrk_error = "".
        Do i = 1 To Num-entries(tmp_lineas):
            Find First pl_mstr Where
            pl_prod_line   = Entry(i,tmp_lineas) No-lock No-error.
            If Not Available(pl_mstr) Then Do:
                If  wrk_error = "" Then
                    wrk_error = Entry(i,tmp_lineas).
                Else
                    wrk_error = wrk_error + "," + Entry(i,tmp_lineas).
            End.
        End.
        If wrk_error <> "" Then Do:
            Assign tmp_lineas = "".
            Message " Lineas capturadas son inválidas:" Skip
            wrk_error
            View-as Alert-box.
            Undo,retry.
        End.
    
        wrk_error = "".
        Do i = 1 To Num-entries(tmp_grupos):
            Find First code_mstr Where
            code_mstr.code_fldname = "pt_group" And
            code_mstr.code_value   = Entry(i,tmp_grupos) No-lock No-error.
            If Not Available(code_mstr) Then Do:
                If  wrk_error = "" Then
                    wrk_error = Entry(i,tmp_grupos).
                Else wrk_error = wrk_error + "," + Entry(i,tmp_grupos).
            End.
        End.
        If wrk_error <> "" Then Do:
            Assign tmp_grupos = "".
            Message "Estos grupos capturados son inválidos:" Skip
            wrk_error
            View-as Alert-box.
            Undo,Retry.
        End.
    
        wrk_error = "".
        Do i = 1 To Num-entries(tmp_items):
            Find First pt_mstr Where
            pt_part   = Entry(i,tmp_items) No-lock No-error.
            If Not Available(pt_mstr) Then Do:
                If  wrk_error = "" Then
                    wrk_error = Entry(i,tmp_items).
                Else
                    wrk_error = wrk_error + "," + Entry(i,tmp_items).
            End.
        End.
        If wrk_error <> "" Then Do:
            Assign tmp_items = "".
            Message "Estos Artículos capturados son inválidos:" Skip
            wrk_error
            View-as Alert-box.
            Undo,Retry.
        End.
             
        Assign     
            xxstp_lineas = Input tmp_lineas
            xxstp_grupos = Input tmp_grupos
            xxstp_items  = Input tmp_items
            xxstp_datemod = today
            xxstp_timemod = time
            xxstp_usrmod  = global_userid. 
    End.
    
    If Lastkey = Keycode("F5") Or Lastkey = Keycode("CTRL-D")
        Or Input batchdelete = "x"then Do:
        del-yn = Yes.
        {pxmsg.i &MSGNUM=11 &ERRORLEVEL=1 &CONFIRM=del-yn}

        If del-yn then do:
      
            {xxmi33lb.i
            &id_file = "xxstp_mstr_mnto" 
            &id_accion = "BORRADO"
            &param01=xxstp_mstr.xxstp_userid
            &param02=xxstp_mstr.xxstp_siteo
            &param03=xxstp_mstr.xxstp_loco
            &param04=xxstp_mstr.xxstp_sited
            &param05=xxstp_mstr.xxstp_locd
            &param06=xxstp_mstr.xxstp_lineas
            &param07=xxstp_mstr.xxstp_grupos
            &param08=xxstp_mstr.xxstp_items
            }
                                   
            Delete xxstp_mstr.
            Clear Frame a.
            Message "Registro eliminado".
            Pause.
            Clear Frame a.
            Next.
        End.
    End.
    
    Else Do: 
        {xxmi33lb.i
        &id_file = "xxstp_mstr_mnto"
        &id_accion = "MODIFICACION DESPUES"
        &param01=xxstp_mstr.xxstp_userid
        &param02=xxstp_mstr.xxstp_siteo
        &param03=xxstp_mstr.xxstp_loco
        &param04=xxstp_mstr.xxstp_sited
        &param05=xxstp_mstr.xxstp_locd
        &param06=xxstp_mstr.xxstp_lineas
        &param07=xxstp_mstr.xxstp_grupos
        &param08=xxstp_mstr.xxstp_items
        }
    Next.
    End.
End.