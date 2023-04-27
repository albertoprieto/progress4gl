/******************************************************************************/
/* # @(#)                                                                    >*/
/* # @(#) Descripcion: consulta xxstp_mstr                                   >*/
/* # @(#)                                                                    >*/
/* # @(#)                                                                    >*/
/* # @(#) Parametros:                                                        >*/
/* # @(#)   Parametro   E|S|ES   Tipo   Longitud   Descripcion               >*/
/* # @(#)  -parametro- -E|S|ES- -tipo- -longitud- -descripcion-              >*/
/* # @(#)                                                                    >*/
/* # @(#) Fecha de Creacion: 14/May/21 HH:MM  jap                            >*/
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

Define Variable sort-a As  Character Format "X(25)" Initial "by_userid"
View-As Combo-Box List-Item-Pairs
"Por userid","by_userid".

Define Variable tmp_userid   Like xxstp_mstr.xxstp_userid No-undo.
Define Variable tmp_userid1  Like xxstp_mstr.xxstp_userid No-undo.
Define Variable tmp_siteo    Like xxstp_mstr.xxstp_siteo  No-undo.
Define Variable tmp_siteo1   Like xxstp_mstr.xxstp_siteo  No-undo.
Define Variable tmp_loco     Like xxstp_mstr.xxstp_loco   No-undo.
Define Variable tmp_loco1    Like xxstp_mstr.xxstp_loco   No-undo.
Define Variable tmp_sited    Like xxstp_mstr.xxstp_sited  No-undo.
Define Variable tmp_sited1   Like xxstp_mstr.xxstp_sited  No-undo.
Define Variable tmp_locd     Like xxstp_mstr.xxstp_locd   No-undo.
Define Variable tmp_locd1    Like xxstp_mstr.xxstp_locd   No-undo.
Define Variable tmp_lineas   Like xxstp_mstr.xxstp_lineas No-undo.
Define Variable tmp_lineas1  Like xxstp_mstr.xxstp_lineas No-undo.
Define Variable tmp_grupos   Like xxstp_mstr.xxstp_grupos No-undo.
Define Variable tmp_grupos1  Like xxstp_mstr.xxstp_grupos No-undo.
Define Variable tmp_items    Like xxstp_mstr.xxstp_items  No-undo.
Define Variable tmp_items1   Like xxstp_mstr.xxstp_items  No-undo.

Define Frame a
tmp_userid    Colon 21 Label "Usuario"
tmp_siteo     Colon 21 Label "Almacen origen"
tmp_loco      Colon 21 Label "Ubicación origen"
tmp_sited     Colon 21 Label "Almacen destino"
tmp_locd      Colon 21 Label "Ubicación destino"
tmp_lineas    Colon 21 Label "Lineas"
tmp_grupos    Colon 21 Label "Grupos"
tmp_items     Colon 21 Label "Artículos"

Skip(1)
sort-a  colon 21 label "Ordenado por"
With Frame a Side-Labels Width 80.

ON Enter,Cursor-Up Of sort-a Do:
    Apply "TAB" To sort-a.
End.

Repeat:
    Assign
        tmp_userid = " "
        tmp_siteo  = " "
        tmp_loco   = " "
        tmp_sited  = " "
        tmp_locd   = " "
        tmp_lineas = " "
        tmp_grupos = " "
        tmp_items  = " "

        tmp_userid1 = " "
        tmp_siteo1  = " "
        tmp_loco1   = " "
        tmp_sited1  = " "
        tmp_locd1   = " "
        tmp_lineas1 = " "
        tmp_grupos1 = " "
        tmp_items1  = " ".
        
    Update
        tmp_userid  
        tmp_siteo
        tmp_loco   
        tmp_sited   
        tmp_locd    
        tmp_lineas  
        tmp_grupos 
        tmp_items 
        sort-a
    With Frame a.
    
    If tmp_userid <> "" Then tmp_userid1 = tmp_userid.
    If tmp_siteo  <> "" Then tmp_siteo1  = tmp_siteo.
    If tmp_loco   <> "" Then tmp_loco1   = tmp_loco.
    If tmp_sited  <> "" Then tmp_sited1  = tmp_sited.
    If tmp_locd   <> "" Then tmp_locd1   = tmp_locd.
    If tmp_lineas <> "" Then tmp_lineas1 = tmp_lineas.
    If tmp_grupos <> "" Then tmp_grupos1 = tmp_grupos.
    If tmp_items  <> "" Then tmp_items1  = tmp_items.
    
    If tmp_userid1 = "" Then tmp_userid1 = hi_char.
    If tmp_siteo1  = "" Then tmp_siteo1  = hi_char.
    If tmp_loco1   = "" Then tmp_loco1   = hi_char.
    If tmp_sited1  = "" Then tmp_sited1  = hi_char.
    If tmp_locd1   = "" Then tmp_locd1   = hi_char.
    If tmp_lineas1 = "" Then tmp_lineas1 = hi_char.
    If tmp_grupos1 = "" Then tmp_grupos1 = hi_char.
    If tmp_items1  = "" Then tmp_items1  = hi_char.
    
    {mfselprt.i "page" 132}
    {mfphead.i}
    
    If sort-a  = "by_userid"   Then Run print_reporte1.
    {mfreset.i}

End.

Procedure print_reporte1.
    
    For Each xxstp_mstr Where 
        (xxstp_userid >= tmp_userid  And xxstp_userid <= tmp_userid1 ) And 
        (xxstp_siteo  >= tmp_siteo   And xxstp_siteo  <= tmp_siteo1  ) And
        (xxstp_loco   >= tmp_loco    And xxstp_loco   <= tmp_loco1   ) And
        (xxstp_sited  >= tmp_sited   And xxstp_sited  <= tmp_sited1  ) And
        (xxstp_locd   >= tmp_locd    And xxstp_locd   <= tmp_locd1   ) And
        (xxstp_lineas >= tmp_lineas  And xxstp_lineas <= tmp_lineas1 ) And
        (xxstp_grupos >= tmp_grupos  And xxstp_grupos <= tmp_grupos1 ) And
        (xxstp_items  >= tmp_items   And xxstp_items  <= tmp_items1  )
        No-lock.     

        Display
            xxstp_userid @ tmp_userid Label "Usuario"
            xxstp_siteo  @ tmp_siteo  Column-Label "Almacen!origen"
            xxstp_loco   @ tmp_loco   Column-Label "Ubicación!origen"  
            xxstp_sited  @ tmp_sited  Column-Label "Almacen!destino"
            xxstp_locd   @ tmp_locd   Column-Label "Ubicación!destino" 
            xxstp_lineas @ tmp_lineas Column-Label "Líneas" 
            xxstp_grupos @ tmp_grupos Column-Label "Grupos"
            xxstp_items  @ tmp_items  Column-Label "Artículos"
       With Down.
    End.
End Procedure.
