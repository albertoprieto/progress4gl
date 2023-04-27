/******************************************************************************/
/* # @(#)                                                                    >*/
/* # @(#) Descripcion: Rep VO datos de embarque                              >*/
/* # @(#)              Rep Destino de los fletes                             >*/
/* # @(#)                                                                    >*/
/* # @(#) Parametros:                                                        >*/
/* # @(#)   Parametro   E|S|ES   Tipo   Longitud   Descripcion               >*/
/* # @(#)  -parametro- -E|S|ES- -tipo- -longitud- -descripcion-              >*/
/* # @(#)                                                                    >*/
/* # @(#) Fecha de Creacion: 24/feb/21 HH:MM  jlm                            >*/
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
/*                                                                            */
/*      <Id> <Usr>  <Fecha>    <Hora>     <Id> <Usr>  <Fecha>    <Hora>       */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--        */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--        */
/*      ----  ---   --/---/--  --:--      ----  ---   --/---/--  --:--        */
/* jap01 24abr21 Agregar columnas xxcomp_serie, xxcomp_folio, xxcomp_fecha    */
/* adl01 28/Jun/21 Agregan guias tomando tabla xxtcsu_mstr                    */
/* adl02 28/Jun/21 Agregan guias tomando tabla xxflu_hist                     */
/* adl03 19/Jul/22 Agregan cheque y fecha de Cheque                           */
/******************************************************************************/

{mfdtitle.i}

run pxgblmgr.p persistent set global_gblmgr_handle.

 Define Var wrk_fileout As Char format "X(50)".
 Define var iCount as integer.
 define var desc_linea as char Format "X(35)".
 Define Var lineas   As  Char Format "X(30)".
 define var ix as integer.
 define var wrk_destino as char.
 define var wrk_fact_rangos as char.
 define var wrk_folini as integer.
 define var wrk_flag as logical.

 define var wsepara as char format "X" Initial "|".
 define var wtilde as char format "X" Initial "'".
def var wrk_ciclo as int.
def var wrk_dif as int.

Define Variable vii As Integer.    /*adl01 */
Def var soloFin As Logical.

   DEF VAR salto LIKE mfc_logical LABEL "Salto de Página" INIT FALSE NO-UNDO.
   
   DEF VAR sINIfile  AS CHAR FORMAT "X(50)" LABEL "Archivo a Imprimir" 
       NO-UNDO.
   
   DEF VAR linea     AS CHAR Format "x(200)" NO-UNDO.
   DEF STREAM entradaS.
   DEF VAR path-v    AS CHAR FORMAT "X(40)".
   DEF VAR  x        AS INT.

   Define Var lineasTot  As Integer.
   Define Var lineasCont As Integer.

 
define var wrk_titulos     as char extent 100 format "X(12)".
Define var i As Integer.

Define Stream ax.

Define Var wrk_limit as Integer.


Define Var tmp_date   Like glt_date No-Undo.
Define Var tmp_date1  Like glt_date No-Undo.

Define Var tmp_eff     Like glt_eff  No-Undo.
Define Var tmp_eff1    Like glt_eff  No-Undo.

def var tmp_vend as char.
def temp-table datrango
field datrango_folio as int.

def temp-table datos
Field dat_vend    Like ap_ref
Field dat_ref     Like ckd_ref
Field dat_voucher Like ckd_voucher
Field dat_amt     Like ckd_amt
Field dat_curr    Like ap_curr
Field dat_inv     Like vo_invoice
Field dat_seriefol Like xxcomp_Seriefol                 
/*jap01-ini*/
Field dat_serie   Like xxcomp_serie
Field dat_folio   Like xxcomp_folio
Field dat_fecha   Like xxcomp_fecha
/*jap01-fin*/
Field dat_ordent  Like oe_orden
Field dat_fchemb  As date
Field dat_uuid Like xxcomp_otuuid
Field dat_sub as dec
Field dat_iva as dec
Field dat_itotalImpTras Like xxcomp_itotalImpTras
field dat_RFCs as char
Field dat_destinos as char
field dat_facturas as char
Field dat_fchVO Like ap_eff
field dat_tot as dec 
field dat_guia as char
field dat_vo_fchfact as date
field dat_origen as char
field dat_concepto as char  /* adl01 */
field dat_factcpto as char  /* adl01 */
field dat_maniobra as char  /* adl02 */
field dat_cheque   as int format "99999999"  /* adl03 */
field dat_feccheq  as date                   /* adl03 */
field dat_tipo     as Char.                  /* adl03 */


Define Frame a
 tmp_vend  colon 21 label "prov"
 ad_name No-Label 
 tmp_eff   colon 21 label "Fch Pago"
 tmp_eff1  colon 50 label "A"
 tmp_date  colon 21 label "Fch.VO"
 tmp_date1 colon 50 label "A"
 
 
 /***wrk_fileout  colon 21 label "Archivo para Excel"***/
 
With Width 80 Side-Labels.

tmp_date  = 01/01/05.
tmp_date1 = Today.
tmp_eff   = date(Month(today),1,year(today)).
tmp_eff1  = Today.

tmp_Eff = 10/01/20.
tmp_Eff1 = 10/31/20.

tmp_Vend = "00060007".

Repeat:
   If tmp_date1  = hi_date Then tmp_date1 = ?.
   If tmp_date   = low_date Then tmp_date = ?.
   If tmp_eff1 = hi_date Then tmp_eff1 = ?.
   If tmp_eff  = low_date Then tmp_eff = ?.
   
   Empty Temp-table datos.             /* adl02 */
  
   Update tmp_vend With Frame a.
   
   Find First ad_mstr Where
        ad_type = "supplier" And
        ad_addr = tmp_vend No-Lock No-Error. 
   If Avail(ad_mstr) Then 
      Display ad_name With Frame a.
      
   Update 
          tmp_eff   tmp_eff1
          tmp_date  tmp_date1
          With Frame a.
   If tmp_date1  = ?  Then tmp_date1 = hi_date.
   If tmp_date   = ?  Then tmp_date  = low_date.
   If tmp_eff1    = ? Then tmp_eff1 = hi_date.
   If tmp_eff     = ? Then tmp_eff  = low_date.
   

   For Each datos:
      Delete datos.
   End.
 
   x = 1.
   
   /***Display wrk_fileout With Frame a.***/
   
   For Each datos:
      Delete datos.
   End.
   
   
   wrk_fileout = "tempo_" + global_userid + 
                            String(Random(50000,99999),"99999") + ".prn".
                            
   Output stream ax To value(wrk_fileout).
   
   /***
   Message "archivo para Excel" wrk_fileout View-As Alert-Box.
   ***/
   
   Run crea_datos.
   
   Run print_reporte1.

   Output stream ax Close.
   
   /***Display wrk_fileout With Frame a.**/
   
   /*begin:envia archivo a impresion*/
   IF x > 1 THEN LEAVE.
   
   x = 1 .
                    
   IF x = 1 THEN DO:
      x = x + 1.
           
      sINIFile = wrk_fileout.
             
      FILE-INFO:FILE-NAME = sinifile.
      IF FILE-INFO:FULL-PATHNAME = ? THEN sinifile = SEARCH(sinifile).

      Unix Silent Value("wc -l " + sinifile + " > numlineas_62ap.cnt").
      Input From numlineas_62ap.cnt.
        Import lineasTot.
      Input Close.

      {mfselprt.i "page" 132 }

         INPUT STREAM entradaS FROM VALUE(sinifile).
           lineasCont = 0.
           REPEAT:
              linea = "".
              IMPORT STREAM entradaS UNFORMATTED linea.
              lineasCont = lineasCont + 1.
              If soloFin And lineasCont <= lineasTot - 2998 Then Next.
              IF linea = "" THEN PUT SKIP(1).
                 ELSE PUT UNFORMATTED linea SKIP.
           END.
         
           /*
           IF salto THEN DO:
              PUT CHR(12) SKIP.
           END.
           */
               
         INPUT STREAM entradas CLOSE.

      {mfreset.i}
      {mfmsg.i 8 1}
      hide message.
   END.
   /*envia archivo a impresion*/
   unix silent rm value(wrk_fileout).
End.

Procedure crea_datos.
For Each ap_mstr Where
    ap_type = "CK" And
    ap_vend = tmp_vend And
    ap_eff >= tmp_eff And
    ap_eff <= tmp_eff1 And
    ap_amt <> 0 No-Lock:
    Find First ck_mstr Where
         ck_ref = ap_ref No-Lock No-Error. 
    If ck_status = "void" Then Next.
         
    For Each ckd_det Where
        ckd_ref = ap_ref No-Lock:
        Create datos.
        Assign
          dat_vend    = ap_vend
          dat_ref     = ckd_ref
          dat_voucher = ckd_voucher
          dat_amt     = ckd_amt
          dat_curr    = ap_curr
          dat_cheque  = ck_nbr       /* adl03 */
          dat_feccheq = ap_eff.      /* adl03 */
        Find First vo_mstr Where
             vo_ref = ckd_voucher No-Lock No-Error. 
        If Avail(vo_mstr) Then Do:     
           Assign
             dat_inv = vo_invoice
             dat_uuid = vo_user1.
        End.
    End.
End.
        
/*filtra por rango de fecha de VO*/
For Each datos:
    Find First ap_mstr Where
         ap_type = "VO" And
         ap_ref = dat_voucher No-Lock No-Error. 
    If Avail(ap_mstr) Then Do:
       dat_fchVO = ap_eff.
       If Not (ap_eff >= tmp_date And ap_eff <= tmp_date1) Then Do:
          Delete datos.
          Next.
       End.
    End.
    
    If dat_inv Begins "CFF-AP" Then Do:
       Delete datos.
       Next.
    End.

End.
    
For Each datos Where dat_uuid <> "":
    /* adl02 ==> */
    Find First xxcomp_mstr Where
            xxcomp_otuuid = dat_uuid No-Lock No-Error. 
    If Avail(xxcomp_mstr) Then Do:
       Find First xxcpto_mstr
               WHERE xxcpto_ArchivoXML = xxcomp_ArchivoXML
                     NO-LOCK NO-ERROR.
       IF AVAILABLE xxcpto_mstr Then            
             ASSIGN 
                 dat_concepto = CAPS(xxcpto_Descripcion)
                 dat_concepto = REPLACE(dat_concepto,CHR(10), " ")
                 dat_concepto = REPLACE(dat_concepto,CHR(13), " ") .
                           
          Do vii = 1 TO LENGTH(dat_concepto) :
             if substring(dat_concepto,vii,7) = "FACTURA" THEN DO:
                dat_factcpto = SUBSTRING(dat_concepto,viI , 18).
                vii = LENGTH(dat_concepto).
             END.
          END. 
          dat_factcpto = trim(REPLACE(dat_factcpto,"FACTURA",""))         .     
    END. 
          /* adl02 <= */    


    For Each embdatos Where
        oe__chr02 = dat_uuid No-Lock:
        If dat_ordent = "" Then
           dat_ordent = oe_orden.
        Else dat_ordent = dat_ordent + "," + oe_orden.
        
        If Lookup(oe_guia,dat_guia) = 0 Then Do:
           If dat_guia = "" Then
           dat_guia = oe_guia.
           Else dat_guia = dat_guia + "," + oe_guia. 
        End.

        For Each auxpedido Where
            ordent = oe_orden And
            factura <> "" No-Lock:
          
            If dat_facturas = "" Then 
               dat_facturas = factura.
            Else   
            dat_facturas = dat_facturas + "," + factura.
          
            Find First xxarmstr Where
                 xxar_ardref = factura No-Lock No-Error. 
            If Avail(xxarmstr) Then Do:
               Find First ad_mstr Where 
                    ad_type = "Customer" And
                    ad_addr = xxar_bill No-Lock No-Error. 
               If Avail(ad_mstr) Then Do:     
                  
                  If Lookup(ad_gst_id,dat_RFCs) = 0 Then Do:
                     If dat_RFCs = "" Then
                        dat_RFCs = ad_gst_id.
                     Else
                        dat_RFCs = dat_RFCs + "," + ad_gst_id.
                  End.
                  Find first ih_hist Where
                       ih_inv_nbr = factura And
                       (ih_nbr Begins "OV" Or ih_nbr Begins "Z")
                       No-Lock No-Error. 
                  If Avail(ih_hist) Then Do:
                     If ih_bill <> ih_ship Then Do:
                        Find First ad_mstr Where
                             ad_type = "ship-to" And
                             ad_addr = ih_ship No-Lock No-Error. 
                        If Not Avail(ad_mstr) Then Do:
                           Message "no se encontro direccion de entrega" skip
                           "para (ih_ship):" ih_ship
                           View-As Alert-Box. 
                        End.
                     End.
                     /*en este punto el buffer del ad_mstr puede corresponder*/
                     /*al _bill o direccion de entrega _ship */
                     If Avail(ad_mstr) Then Do:
                        Find First code_mstr Where
                             code_fldname = "ad_state " And
                             code_value = ad_state No-Lock No-Error. 
                        
                        wrk_destino = ad_city + " " + ad_state.
                        
                        If Lookup(wrk_destino,dat_destinos) = 0 Then Do:
                           If dat_destinos = "" Then
                           dat_destinos = wrk_destino.
                           Else
                           dat_destinos = dat_destinos + "," + wrk_destino.
                        End.
                        
                     End.
                  End.  
               End.
            End.
        End.         
    End.   
    
    /* adl01 ==> */   
    For each xxtcsu_mstr
             Where xxtcsu_uuid = dat_uuid No-Lock:

        If Lookup(xxtcsu_guia,dat_guia) = 0 Then Do:
           If dat_guia = ""  Then
              Assign dat_guia  = xxtcsu_guia.
           Else
              dat_guia = dat_guia + "," + xxtcsu_guia.
        End.
        
        If dat_facturas = "" Then Do:
           dat_facturas = xxtcsu_char01.
        End.   
        Else   
           dat_facturas = dat_facturas + "," + xxtcsu_char01.


        dat_facturas = Replace(dat_facturas,"|",",").
 
        Do vii = 1 To num-entries(xxtcsu_char01,"|") :
           Find First xxarmstr Where
                      xxar_ardref = entry(vii,xxtcsu_char01,"|") 
                      No-Lock No-Error. 
           If Avail(xxarmstr) Then Do:
              Find First ad_mstr Where 
                      ad_type = "Customer" And
                      ad_addr = xxar_bill No-Lock No-Error. 
              If Avail(ad_mstr) Then Do:     
                  
                 If Lookup(ad_gst_id,dat_RFCs) = 0 Then Do:
                    If dat_RFCs = "" Then
                       dat_RFCs = ad_gst_id.
                    Else
                       dat_RFCs = dat_RFCs + "," + ad_gst_id.
                 End.
              
                 Find first ih_hist Where
                         ih_inv_nbr = entry(vii,xxtcsu_char01,"|") And
                        (ih_nbr Begins "OV" Or ih_nbr Begins "Z")
                         No-Lock No-Error. 
                 If Avail(ih_hist) Then Do:
                     If ih_bill <> ih_ship Then Do:
                        Find First ad_mstr Where
                             ad_type = "ship-to" And
                             ad_addr = ih_ship No-Lock No-Error. 
                        If Not Avail(ad_mstr) Then Do:
                           Message "no se encontro direccion de entrega" skip
                           "para (ih_ship):" ih_ship
                           View-As Alert-Box. 
                        End.
                     End.
                     /*en este punto el buffer del ad_mstr puede corresponder*/
                     /*al _bill o direccion de entrega _ship */
                     If Avail(ad_mstr) Then Do:
                        Find First code_mstr Where
                             code_fldname = "ad_state " And
                             code_value = ad_state No-Lock No-Error. 
                        
                        wrk_destino = ad_city + " " + ad_state.
                        
                        If Lookup(wrk_destino,dat_destinos) = 0 Then Do:
                           If dat_destinos = "" Then
                           dat_destinos = wrk_destino.
                           Else
                           dat_destinos = dat_destinos + "," + wrk_destino.
                        End.
                        
                     End.
                 End.  
             End.
          End.
       End.
    End.
    /* adl01 <== */
    
    /* adl02 => */
    For each xxflu_hist
             Where xxflu_uuid = dat_uuid No-Lock:
        dat_maniobra = dat_concepto.   /* adl03 */
        dat_tipo     = CAPS(xxflu_cveaut).   /* adl03 */

        If Lookup(xxflu_guia,dat_guia) = 0 Then Do:
           If dat_guia = ""  Then
              Assign dat_guia  = xxflu_guia.
           Else
              dat_guia = dat_guia + "," + xxflu_guia.
        End.
      
        For Each embdatos Where
             oe_transp = xxflu_transp and
             oe_guia   = xxflu_guia   No-Lock:
       
            If dat_ordent = "" Then
               dat_ordent = oe_orden.
            Else dat_ordent = dat_ordent + "," + oe_orden.
        
            If Lookup(oe_guia,dat_guia) = 0 Then Do:
               If dat_guia = "" Then
                  dat_guia = oe_guia.
               Else dat_guia = dat_guia + "," + oe_guia. 
            End.

            For Each auxpedido Where
                ordent = oe_orden And
                factura <> "" No-Lock:
          
                If dat_facturas = "" Then 
                  dat_facturas = factura.
                Else   
                dat_facturas = dat_facturas + "," + factura.
          
                Find First xxarmstr Where
                     xxar_ardref = factura No-Lock No-Error. 
                If Avail(xxarmstr) Then Do:
                   Find First ad_mstr Where 
                     ad_type = "Customer" And
                     ad_addr = xxar_bill No-Lock No-Error. 
                   If Avail(ad_mstr) Then Do:     
                  
                      If Lookup(ad_gst_id,dat_RFCs) = 0 Then Do:
                         If dat_RFCs = "" Then
                            dat_RFCs = ad_gst_id.
                         Else
                            dat_RFCs = dat_RFCs + "," + ad_gst_id.
                      End.
                      Find first ih_hist Where
                          ih_inv_nbr = factura And
                         (ih_nbr Begins "OV" Or ih_nbr Begins "Z")
                          No-Lock No-Error. 
                      If Avail(ih_hist) Then Do:
                         If ih_bill <> ih_ship Then Do:
                            Find First ad_mstr Where
                                ad_type = "ship-to" And
                                ad_addr = ih_ship No-Lock No-Error. 
                            If Not Avail(ad_mstr) Then Do:
                               Message "no se encontro direccion de entrega" 
                                        skip
                               "para (ih_ship):" ih_ship
                               View-As Alert-Box. 
                            End.
                         End.
                     /*en este punto el buffer del ad_mstr puede corresponder*/
                     /*al _bill o direccion de entrega _ship */
                        If Avail(ad_mstr) Then Do:
                          Find First code_mstr Where
                               code_fldname = "ad_state " And
                               code_value = ad_state No-Lock No-Error. 
                        
                           wrk_destino = ad_city + " " + ad_state.
                        
                           If Lookup(wrk_destino,dat_destinos) = 0 Then Do:
                              If dat_destinos = "" Then
                                 dat_destinos = wrk_destino.
                              Else
                                 dat_destinos = dat_destinos + "," + 
                                               wrk_destino.
                           End.
                        
                        End.
                      End.  
                   End.
                End.
            End.         
        End.
        IF xxflu_guia = "" THEN DO:
           Find First xxarmstr Where
                     xxar_ardref = dat_factcpto No-Lock No-Error. 
                If Avail(xxarmstr) Then Do:
                   Find First ad_mstr Where 
                     ad_type = "Customer" And
                     ad_addr = xxar_bill No-Lock No-Error. 
                   If Avail(ad_mstr) Then Do:     
                      dat_factura = dat_factcpto. 
                      If Lookup(ad_gst_id,dat_RFCs) = 0 Then Do:
                         If dat_RFCs = "" Then
                            dat_RFCs = ad_gst_id.
                         Else
                            dat_RFCs = dat_RFCs + "," + ad_gst_id.
                      End.
                      Find first ih_hist Where
                          ih_inv_nbr = dat_factcpto And
                         (ih_nbr Begins "OV" Or ih_nbr Begins "Z")
                          No-Lock No-Error. 
                      If Avail(ih_hist) Then Do:
                         If ih_bill <> ih_ship Then Do:
                            Find First ad_mstr Where
                                ad_type = "ship-to" And
                                ad_addr = ih_ship No-Lock No-Error. 
                            If Not Avail(ad_mstr) Then Do:
                               Message "no se encontro direccion de entrega" 
                               skip
                               "para (ih_ship):" ih_ship
                               View-As Alert-Box. 
                            End.
                         End.
                     /*en este punto el buffer del ad_mstr puede corresponder*/
                     /*al _bill o direccion de entrega _ship */
                        If Avail(ad_mstr) Then Do:
                          Find First code_mstr Where
                               code_fldname = "ad_state " And
                               code_value = ad_state No-Lock No-Error. 
                        
                           wrk_destino = ad_city + " " + ad_state.
                        
                           If Lookup(wrk_destino,dat_destinos) = 0 Then Do:
                              If dat_destinos = "" Then
                                 dat_destinos = wrk_destino.
                              Else
                                 dat_destinos = dat_destinos + "," + 
                                               wrk_destino.
                           End.
                        
                        End.
                      End.  
                   End.
                End.
        END. 
          
    End.
    /* adl02 <==*/

  End.
End.

PROCEDURE print_reporte1.
   def var icuenta_destinos  as int.
   def var icuenta_rfcs      as int.
   def var icuenta_facturas  as int.
   
   For Each datos By dat_inv:
       If num-entries(dat_destinos) > icuenta_destinos Then
          icuenta_destinos = num-entries(dat_destinos).
       If num-entries(dat_destinos) > icuenta_destinos Then
          icuenta_destinos = num-entries(dat_destinos).
       If num-entries(dat_destinos) > icuenta_destinos Then
          icuenta_destinos = num-entries(dat_destinos).
   End.       

   /*convierte a rangos el campo dat_facturas */
   /***
   ***For Each datos By dat_inv:
   ***    Assign
   ***      wrk_fact_rangos = "".
   ***      
   ***    For Each datrango:
   ***        delete datrango.
   ***    end.
   ***    If num-entries(dat_facturas) > 1 Then Do:
   ***       
   ***       Do ix = 1 To num-entries(dat_facturas):
   ***          Create datrango.
   ***          Assign
   ***            datrango_folio = Int(Substr(Entry(ix,dat_facturas),3,6)).
   ***       End.
   ***       
   ***       wrk_fact_rangos = Entry(1,dat_facturas).
   ***       wrk_folini = Int(Substr(Entry(1,dat_facturas),3,6)).
   ***       
   ***       wrk_flag = FALSE.
   ***       wrk_dif = 0.
   ***       For Each datrango By datrango_folio:
   ***           If wrk_folini = datrango_folio Then Do:
   ***              wrk_folini =wrk_folini + 1.
   ***              wrk_dif = wrk_dif + 1.
   ***           End.
   ***           Else Do:
   ***                wrk_ciclo = wrk_ciclo + 1.
   ***                wrk_flag = TRUE.
   ***                wrk_dif = 0.
   ***                wrk_fact_rangos = wrk_fact_rangos + "-" + 
   ***                String(wrk_folini - 1) + "," + String(datrango_folio).
   ***                wrk_folini = datrango_folio + 1.
   ***                 
   ***           End.    
   ***       End.
   ***       If wrk_flag = FALSE Or wrk_dif <> 0 Then
   ***          wrk_fact_rangos = wrk_fact_rangos + "-" +
   ***          String(wrk_folini - 1).
   ***          
   ***       
   ***    End.    
   ***    Else 
   ***    wrk_fact_rangos = dat_facturas.
   ***    
   ***    dat_facturas = wrk_fact_rangos.
   ***End.
   ***/
          
   Run asigna_titulo_columnas.
   
   
   /*print titulos*/
   Put stream ax 
       "Voucher  |"  Space(2)  
       "Cheque   |"  Space(2)
       "Fecha Cheque  |"  Space(2)
       "    Subtotal|" Space(7)
       "    Iva|"      Space(5)  
       "      Total|"    
       "Funda                |"  
       "SerieFol             |"
       /*jap01-ini*/
       "Serie                    |"
       "Folio                                   |"
       "Fecha     |"
       /*jap01-fin*/
       "Guia      |" 
       "Tipo      |"
       "UUID                                     |" 
       "Proveed     |" 
       "Nombre Proveedor             |" 
       "Origen          |" 
       "Destinos|" Space(10)
       "Facturas|" Space(10)
       "RFCs|"  Space(10)
       "Concepto"
       .
   
   Put Stream ax Skip.

   For Each datos No-Lock By dat_inv:
       Find First xxcomp_mstr Where
            xxcomp_otuuid = dat_uuid No-Lock No-Error. 
       If Avail(xxcomp_mstr) Then Do:
          Assign
            dat_seriefol = xxcomp_seriefol
            /*jap01-ini*/
            dat_serie    = xxcomp_serie
            dat_folio    = xxcomp_folio
            dat_fecha    = xxcomp_fecha
            /*jap01-fin*/
            dat_sub      = xxcomp_sub
            dat_tot      = xxcomp_tot
            dat_iva      = dat_tot - dat_sub
            dat_itotalImpTras = xxcomp_itotalImpTras.

        
          If dat_iva <> dat_itotalImpTras Then Do:
             Message "Impuesto calculado difiere del impuesto retenido" 
             Skip
             "Impuesto calculado:" dat_iva Skip
             "Impuesto retenido:" dat_itotalImpTras Skip
             View-As Alert-Box. 
          End. 
       End.   
       Accumulate dat_sub (Total).
       Accumulate dat_iva (Total).
       Accumulate dat_tot (Total).

       /*print detalle*/
       Find First ad_mstr Where
            ad_addr = dat_vend No-Lock No-Error. 
            
       Put Stream ax 
         dat_voucher Space(1)    wsepara
         dat_ref     Space(1)    wsepara      /* adl03 */
         dat_feccheq Space(1)    wsepara      /* adl03 */
         space(4) dat_sub        wsepara
         space(4) dat_iva        wsepara
         space(6) dat_tot        wsepara
         dat_inv  space(1)       wsepara
         dat_seriefol Space(1)   wsepara
         /*jap01-ini*/
         dat_serie wsepara         
         dat_folio wsepara
         dat_fecha Space(2) wsepara
         /*jap01-fin*/
         wtilde dat_guia space(1)       wsepara
         dat_tipo space(1)       wsepara
         dat_uuid space(1)       wsepara
         wtilde dat_vend  Space(1) wsepara
         ad_name   space(1) wsepara
         "Guadalajara, Jal" wsepara.

       Do ix = 1 To num-entries(dat_destinos):
          Put Stream ax unformatted
          Entry(ix,dat_destinos)    ",".
       End.
       Put Stream ax wsepara.
       Do ix = 1 To num-entries(dat_facturas):
          Put Stream ax unformatted
              Entry(ix,dat_facturas) ",".
       End.

       Put Stream ax wsepara.
       
       Do ix = 1 To num-entries(dat_RFCs):
       Put Stream ax unformatted
       Entry(ix,dat_RFCs)    ",".
       End.
       
       
       Put Stream ax unformatte "|" dat_maniobra  Skip. /* adl02 */

               /*
       Put Stream ax Skip.                               /* adl02*/
                 */

  End.    

  /*print totales*/
   
   Put Stream ax Space(11).
   Put Stream ax  "-------------" space(2).
   Put Stream ax  "-------------" space(4).
   Put Stream ax  "-------------" .
   Put Stream ax Skip.
   
   Put Stream ax 
       Space(7)
       Accum Total dat_sub format "->,>>>,>>>,>>9.99"
       Accum Total dat_iva format "->>>,>>>,>>9.99"
       Accum Total dat_tot format "->,>>>,>>>,>>9.99".
   Put Stream ax Skip.

   
End.

Procedure asigna_titulo_columnas.
   def var wrk_espacios as integer.
End.

 