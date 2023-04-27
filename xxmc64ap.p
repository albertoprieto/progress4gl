/******************************************************************************/
/* # @(#)                 Id Programa: xxmc64ap.p                            >*/
/* # @(#)                                                                    >*/
/* # @(#) Descripcion: Mnto Code_fldname = Rfc_prov_permite_modif            >*/
/* # @(#)                                                                    >*/
/* # @(#) Parametros:                                                        >*/
/* # @(#)  -parametro- -e|s|es- -tipo- -longitud- -descripcion-              >*/
/* # @(#)                                                                    >*/
/* # @(#) Fecha De Creacion: 07/jun/2021 jap                                 >*/
/* # @(#)                                                                    >*/
/* # @(#) Fecha Ultima Modificacion:                                         >*/
/* # @(#)                                                                    >*/
/* # @(#) dd/mmm/aa Hh:mm ___   dd/mmm/aa hh:mm ___   dd/mmm/aa hh:mm ___    >*/
/* # @(#) dd/mmm/aa Hh:mm ___   dd/mmm/aa hh:mm ___   dd/mmm/aa hh:mm ___    >*/
/* # @(#) dd/mmm/aa Hh:mm ___   dd/mmm/aa hh:mm ___   dd/mmm/aa hh:mm ___    >*/
/*                                                                            */
/* Variables:                                                                 */
/*      <idvariable> <tipo> <longitud> <uso>                                  */
/*      <idvariable> <tipo> <longitud> <uso>                                  */
/*                                                                            */
/* Ide Modificaciones:                                                        */
/*                                                                            */
/*      <id>  <usr>    <fecha>  <hora>   <descripcion>                        */
/*      ----- ------  --/---/--  --:--   -------------------------------      */
/*      ----- ------  --/---/--  --:--   -------------------------------      */
/*      ----- ------  --/---/--  --:--   -------------------------------      */
/*      ----- ------  --/---/--  --:--   -------------------------------      */
/*                                                                            */
/******************************************************************************/
/* Display Title */
{mfdtitle.i "2+ "}

Define Stream Straud.
Define Var Arch_salida_aud As Char.
Define Var Separador       As Char Format "x" Initial "!".
Define Var Tmp_campo       As Char.
Define Buffer Buf_code_mstr For Code_mstr.
Define Variable Del-yn Like Mfc_logical Initial No Format "si/no".
Define Variable Fieldlen As Integer Initial 0 No-undo.
Define Variable Fieldname Like Code_fldname No-undo.
Define Variable Vprefijo As Char No-undo.
Define Variable Vconsec  As Char No-undo.
Define Variable Batchdelete As Character Format "x(1)" No-undo.
{gpfieldv.i} 

Define Variable wrk_yn As Character View-as Combo-box
                                    List-items "yes","no".
Define Variable tmp_name Like ad_mstr.ad_name No-undo.

Form
    code_fldname   Colon 25 Label "fldname"   
    code_value     Colon 25 Label "Proveedor"
    tmp_name       Colon 25 Label "Nombre"    Skip(1) 
    wrk_yn         Colon 25 Label "Permitir cambio RFC"
With Frame A Side-labels Width 80 Attr-space.

Fieldname = "RFC_prov_permite_modif".
View Frame a.

Repeat With Frame a:
    
    Batchdelete = "".
    Find First code_mstr Where code_mstr.code_fldname = fieldname
    No-lock No-error.
        
    If Not Available code_mstr Then Do:
        Create code_mstr.
    
        Assign 
            code_mstr.code_fldname = fieldname.
    End.

    If Available code_mstr Then Do:
       Display 
            code_mstr.code_fldname
            With Frame a.
       Recno = Recid(code_mstr).
    End.

    {gpfield.i &field_name = Fieldname}
    {gpfldlen.i}

    Do On Error Undo, Retry:

        Prompt-for            
            code_mstr.code_value
            Batchdelete No-label When (batchrun)
            
        Editing:

            {mfnp05.i code_mstr code_fldval
            "code_fldname = Input code_fldname" code_value
            "Input code_value"}

            Find First ad_mstr Where ad_addr = code_value 
            And ad_type = "supplier" No-lock No-error.
            tmp_name = ad_name.
            
            If Available ad_mstr Then Do:
                Find First code_mstr Where 
                code_fldname = "RFC_prov_permite_modif" 
                And code_value = ad_addr No-lock No-error.
                wrk_yn = code_cmmt.
            End.
                
            If Recno <> ? Then Display 
            code_fldname 
            code_value
            tmp_name
            wrk_yn.
        
        End.
            
        If Trim(Input code_value) = "" 
        Or Length(Input code_value) < 2 Then Do:
            Message Input code_value 
            Skip "No existe proveedor, verifique"                  
            View-as Alert-box.
            Next-prompt 
            code_value 
            With Frame a.
            Undo, Retry.
        End.
    End. 

    Find code_mstr Where code_fldname = fieldname
                     And code_value   = Input code_value
    Exclusive-lock No-error.
    
    If Not Available code_mstr Then Do:
        Find First ad_mstr Where ad_addr = Input code_value 
        And ad_type = "supplier" No-lock No-error.
            
        If Not Available ad_mstr Then Do:
            Message Input code_value 
            Skip "No existe proveedor, verifique"                   
            View-as Alert-box.
            Undo, Retry.
        End.
        
        If Available ad_mstr Then Do:
            
            Create code_mstr.
            {pxmsg.i &MSGNUM=1 &ERRORLEVEL=1}
            
            Assign
                code_fldname = Fieldname
                code_value
                tmp_name = ad_name.
            
            Display
                code_fldname
                code_value
                tmp_name.
        End.
    End.

    If Available code_mstr Then Do: 
        Find First ad_mstr Where ad_addr = Input code_value 
        And ad_type = "supplier" No-lock No-error.
        tmp_name = ad_name.
            
        If Not Available ad_mstr Then Do:
            Message Input code_value 
            Skip "No existe proveedor, verifique"           
            View-as Alert-box.
            Undo, Retry.
        End.
        
        If Available ad_mstr Then Do:                            
            {xxmi33lb.i
            &id_file = "rfcmodif"
            &id_accion = "MODIFICACION ANTES"
            &param01=code_mstr.code_fldname
            &param02=code_mstr.code_value
            &param03=code_mstr.code_cmmt
            }

            Assign
                code_mstr.code_value.
        End.
        
        Display 
            code_mstr.code_fldname
            code_mstr.code_value
            tmp_name
            wrk_yn
        With Frame a.

        Ststatus = Stline[2].
        Status Input Ststatus.
    End.
        
    Prompt-for
        wrk_yn
        Go-on(f5 Ctrl-d).
    
    Assign
        wrk_yn.
    
    If wrk_yn = "Yes" Then Do:
                
        Find First ap_mstr Where ap_type = "VO" And 
        ap_vend = code_mstr.code_value 
        No-lock No-error.
        
        If available ap_mstr Then Do:
            Message 
            "No se puede setear proveedor para cambio de RFC,"
            Skip "existe voucher:" 
            Skip ap_ref
            View-as Alert-box.
            Undo, Retry.
        End.
    End.

    If wrk_yn  = "Yes" Then Do:
        
        Find First ap_mstr Where ap_type = "JL" And ap_vend =
        code_mstr.code_value
        No-lock No-error.
            
        If available ap_mstr Then Do:
            Message 
            "No se puede setear proveedor para cambio de RFC,
            Skip existe poliza JL:"
            Skip ap_ref
            View-as Alert-box.
            Undo, Retry.
        End.
    End.
    
    Assign
        wrk_yn
        code_mstr.code_cmmt = String(wrk_yn).
    
    {xxmi33lb.i
    &id_file = "rfcmodif"
    &id_accion = "MODIFICACION DESPUES"
    &param01=code_mstr.code_fldname
    &param02=code_mstr.code_value
    &param03=code_mstr.code_cmmt
    }
    
    If Lastkey = Keycode("f5") Or Lastkey = Keycode("ctrl-d")
    Or Input Batchdelete = "x" Then Do:
        Del-yn = Yes.
        {pxmsg.i &msgnum=11 &errorlevel=1 &confirm=del-yn}
    End.            
            
    If Del-yn Then Do:

        /* Registro Auditoria... */
        {xxmi33lb.i
        &id_file = "rfcmodif" 
        &id_accion = "BORRADO"
        &param01=code_mstr.code_fldname
        &param02=code_mstr.code_value
        &param03=code_mstr.code_cmmt
        }

        Delete code_mstr.
        Clear Frame a.
        Next.
    End.
            
    If New code_mstr Then Do:
        {xxmi33lb.i
        &id_file = "rfcmodif" 
        &id_accion = "ALTA"
        &param01=code_mstr.code_fldname
        &param02=code_mstr.code_value
        &param03=code_mstr.code_cmmt
        }
        Message "Nuevo registro agregado" tmp_name.
    End.
    
    Assign
        code_mstr.code_desc   = Global_userid + "|" 
                              + String(today) + "|" 
                              + String(time,"hh:mm:ss").
End.
Status Input.
