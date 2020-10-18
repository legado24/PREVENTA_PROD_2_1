CREATE OR REPLACE
    PACKAGE BODY  "PREVENTA_PROD_2_1_1" AS


    FUNCTION  LOGINUSUARIO(
        usernamex in varchar2,
        passwordx in varchar2,
        codigox in varchar2,
        imeix in varchar2
    ) return types.ref_cursor is
        clogin types.ref_cursor;
        claveencriptada varchar2(100);
        n number;
        carac char;
        j number;
        carac1 char;
        v_query  VARCHAR2(2000);
        PCODIGOAPP VARCHAR2(17);
        MCODIGOAPP VARCHAR2(200);
        PCODUSUARIO NUMBER:=0;
        PCODPERFIL NUMBER:=0;
        COUNTCODAPP NUMBER:=0;
        COUNTCODPERF NUMBER:=0;
        PACTIVO NUMBER:=0;
        PTIENECLAVE NUMBER:=0;
        codperfilx VARCHAR2(10);
    BEGIN
        n:="LENGTH"(passwordx);


        SELECT COUNT(USUARIO) INTO PCODUSUARIO from SYSADM.USUARIOS WHERE USUARIO = usernamex;

        IF( PCODUSUARIO<> 0 ) then

            FOR i IN 1..n LOOP
                    carac:=SUBSTR(passwordx,i,1);
                    j:=ASCII(carac)-4;
                    carac1:=CHR(j);
                    claveencriptada:=claveencriptada||carac1;
                END LOOP;



            SELECT COUNT(USUARIO) INTO PCODUSUARIO from SYSADM.USUARIOS WHERE USUARIO = usernamex AND CLAVE = claveencriptada;

            IF(PCODUSUARIO <> 0) THEN  --EXISTE USUARIOS.

                SELECT COUNT(CODPERFIL) INTO PCODPERFIL from SYSADM.USUARIOS WHERE USUARIO = usernamex AND CLAVE = claveencriptada ;

                IF(PCODPERFIL <> 0) THEN  --EXISTE PERFIL
                    SELECT CODPERFIL INTO codperfilx
                    from SYSADM.USUARIOS WHERE USUARIO = usernamex AND CLAVE = claveencriptada ;

                    SELECT COUNT(USUARIO) INTO PACTIVO from SYSADM.USUARIOS WHERE USUARIO = usernamex AND CLAVE = claveencriptada AND ESTUSUARIO = 'H';

                    IF(PACTIVO <> 0) THEN
                        -- SELECT COUNT(USUARIO) INTO PTIENECLAVE from SYSADM.USUARIOS WHERE USUARIO = usernamex AND CLAVE = claveencriptada AND COD_APLICACION IS NOT NULL;

                        --IF(PTIENECLAVE <> 0) THEN

                        -- SELECT COUNT(COD_APLICACION) INTO COUNTCODAPP from SYSADM.USUARIOS WHERE USUARIO = usernamex AND CLAVE = claveencriptada AND COD_APLICACION = codigox;

                        VALI_REGIST_MAC(usernamex,claveencriptada,codigox,COUNTCODAPP,MCODIGOAPP);

                        IF(COUNTCODAPP=1 ) THEN


                            v_query:='SELECT DISTINCT US.USUARIO,US.CODPERFIL,US.COD_APLICACION AS CODIGO, UPPER (US.CLAVE) AS CLAVE,TO_CHAR(sysdate + 59/1440,''HH24:MI'') AS HORASERVIDOR,TC.CODCANAL,TC.DESCCANAL,TUM.CODEMPRESA,1 AS IGNOREGPS
						FROM SYSADM.USUARIOS US INNER JOIN SYSADM.TB_USUARIO_MESA TUM ON US.USUARIO=TUM.USUARIO
						INNER JOIN SYSADM.TB_CANAL TC ON TUM.CODCANAL=TC.CODCANAL
						WHERE  US.USUARIO =:username AND US.CLAVE =:claveencriptada';


                            OPEN clogin FOR v_query USING usernamex,claveencriptada;
                        ELSE
                            v_query:='SELECT * FROM (SELECT null as LLAVE,'''||MCODIGOAPP ||''' as DESC1,''E'' as USUARIO,''E'' as FF,
               ''D''  as CLAVE, ''E''  as CODCANAL, ''F''  as DESCCANAL,''G'' as CODEMPRESA ,0 AS IGNOREGPS  FROM dual) TB';
                            OPEN clogin FOR v_query;
                        END IF;
                        -- ELSE
                        --   v_query:='SELECT * FROM (SELECT null as LLAVE,''No tiene codigo registrado, comuniquese con el Area de Sistemas.'' as DESC1,''E'' as USUARIO,
                        --       ''D''  as CLAVE  FROM dual) TB';
                        --   OPEN clogin FOR v_query;
                        --END IF;
                    ELSE
                        v_query:='SELECT * FROM (SELECT null as LLAVE,''USUARIO INACTIVO.'' as DESC1,''E'' as USUARIO,''E'' as FF,
             ''D''  as CLAVE,''E''  as CODCANAL, ''F'' as DESCCANAL,''G'' as CODEMPRESA ,0 AS IGNOREGPS  FROM dual) TB';
                        OPEN clogin FOR v_query;
                    END IF;
                ELSE
                    v_query:='SELECT * FROM (SELECT null as LLAVE,''NO TIENE ASIGNADO UN PERFIL'' as DESC1,''E'' as USUARIO,''E'' as FF,
						''D''  as CLAVE,''E''  as CODCANAL, ''F''  as DESCCANAL,''G'' as CODEMPRESA,0 AS IGNOREGPS FROM dual) TB';
                    OPEN clogin FOR v_query;

                END IF;

            ELSE --CLAVE INCORRECTA.
                v_query:='SELECT * FROM (SELECT null as LLAVE,''CLAVE INCORRECTA'' as DESC1,''E'' as USUARIO,''E'' as FF,
          ''D''  as CLAVE,''E''  as CODCANAL, ''F''  as DESCCANAL,''G'' as CODEMPRESA,0 AS IGNOREGPS FROM dual) TB';
                OPEN clogin FOR v_query;

            END IF;

        ELSE
            v_query:='SELECT * FROM (SELECT null as LLAVE,''USUARIO NO ENCONTRADO'' as DESC1,''E'' as USUARIO,''E'' as FF,
         ''D''  as CLAVE,''E''  as CODCANAL, ''F''  as DESCCANAL,''G'' as CODEMPRESA,0 AS IGNOREGPS  FROM dual) TB';
            OPEN clogin FOR v_query;
        end if;
        dbms_output.put_line(v_query);
        return(clogin);

    end  LOGINUSUARIO;


    PROCEDURE VALI_REGIST_MAC(usernamex  IN VARCHAR2,passwordx IN VARCHAR2,macNewx	IN VARCHAR2,rptax  IN OUT    NUMBER,mensajex IN OUT    VARCHAR2)
        IS
        macBdx VARCHAR2(200);
        existeMacx NUMBER;
        ignoremacx CHAR(1);
        existeUsuariox VARCHAR2(100);
    BEGIN



        SELECT DISTINCT NVL(US.COD_APLICACION,'0'),nvl(US.IGNORE_MAC,'0') INTO macBdx,ignoremacx
        FROM SYSADM.USUARIOS US
                 INNER JOIN SYSADM.TB_USUARIO_MESA TUM ON US.USUARIO=TUM.USUARIO
        WHERE  US.USUARIO =usernamex AND US.CLAVE =passwordx;
        IF ignoremacx='0' then
            IF macBdX IS NOT NULL AND macBdX<>'0' THEN
                --IF macBdX<>'0' THEN
                IF macBdX=macNewx THEN
                    rptax:=1;
                ELSE
                    rptax:=0;
                    mensajex:='YA TIENE CODIGO REGISTRADO';
                END IF;
                --ELSE
                --			rptax:=0;
                --		mensajex:='SU EQUIPO NO DETECTA LA MAC';
                --END IF;

            ELSE

                SELECT COUNT(*) INTO existeMacx
                FROM SYSADM.USUARIOS WHERE  COD_APLICACION=macNewx;

                if existeMacx=0 then
                    UPDATE SYSADM.USUARIOS
                    SET COD_APLICACION=macNewx
                    WHERE USUARIO =usernamex AND CLAVE =passwordx;
                    rptax :=1;
                    mensajex :='SE REGISTRO LA MAC CORRECTAMENTE';
                    commit;
                ELSE
                    SELECT USUARIO INTO existeUsuariox
                    FROM SYSADM.USUARIOS WHERE  COD_APLICACION=macNewx AND ROWNUM<2;
                    rptax:=0;
                    mensajex:='CODIGO ' ||macNewx|| ' ESTA ASIGNADO TAMBIEN AL USUARIO:'||existeUsuariox;

                end if;

            END IF;
        ELSE
            rptax:=1;
        END IF;
    EXCEPTION
        WHEN OTHERS THEN
            rptax := -1;
            rollback;
            mensajex :='SE ENCONTRÓ UN ERROR: '||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE||macNewx;



    END VALI_REGIST_MAC;




    FUNCTION  RUTAS_DIA_USUARIO(usernamex IN VARCHAR2,diax IN VARCHAR2) return types.ref_cursor is
        rutasx types.ref_cursor;
    BEGIN

        open rutasx for
            SELECT DISTINCT R.CODRUTA,R.DESCRUTA
            FROM SYSADM.TB_RUTA_VENDEDOR RV
                     INNER JOIN SYSADM.TB_RUTA R ON RV.IDRUTA=R.IDRUTA
                     INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
            WHERE 1=1
              AND DIA=diax
              AND UM.USUARIO=usernamex;-- and R.CODRUTA='41';

        return rutasx;


    END RUTAS_DIA_USUARIO;




    FUNCTION  CLIENTES_BY_RUTA(codRutax in VARCHAR2) return types.ref_cursor is
        clientesx types.ref_cursor;
    BEGIN

        open clientesx for
            SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,LC.REF_DESPACHO
            FROM SYSADM.CLIENTE C
                     INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
            WHERE 1=1
              AND C.STATUS_CLIENTE='A'
              AND LC.RUTA_DESPACHO=codRutax
              AND LC.REF_DESPACHO IS NOT NULL and replace(LC.REF_DESPACHO,',','') is not null
              AND LC.ESTAD='A'  AND length(TRIM(TRANSLATE(replace(LC.REF_DESPACHO,',',''), ' +-.,0123456789', ' '))) is null ;

        return clientesx;


    END CLIENTES_BY_RUTA;




    FUNCTION  CLIENTES_BY_DIA(usernamex IN VARCHAR2,diax IN VARCHAR2) return types.ref_cursor is
        clientesx types.ref_cursor;
/*countjornadax number;
codclientex VARCHAR2(20),
descclientex VARCHAR2(200);
direccionx VARCHAR2(400);
coordenadaoldx VARCHAR2(400);
codrutax VARCHAR2(20),
descrutax VARCHAR2(200);*/
    BEGIN


        /*SELECT COUNT(*) into countjornadax
FROM SYSADM.TBL_USUARIO_CLIENTE
WHERE USUARIO=usernamex AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD');
--if  usernamex<>'ANGELAD'  then
IF countjornadax=0 then */
        open clientesx for
            SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                            CASE WHEN LC.REF_DESPACHO LIKE '-7%' THEN
                                     CONCAT( CONCAT( SUBSTR(LC.REF_DESPACHO, INSTR(LC.REF_DESPACHO, ',')+1), ',' ) ,
                                             SUBSTR(LC.REF_DESPACHO, 0, INSTR(LC.REF_DESPACHO, ',')-1) )
                                 ELSE
                                     LC.REF_DESPACHO
                                END AS REF_DESPACHO,
                            LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA,
                            (SELECT NVL(IGNORE_SECUENCIACION,0) FROM SYSADM.USUARIOS WHERE USUARIO=usernamex)AS IGNORESEC
            FROM SYSADM.CLIENTE C
                     INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                     INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                     INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                     INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                     INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
            WHERE 1=1
              AND RV.DIA=(select translate(SUBSTR(to_char(sysdate, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                          from dual)
              AND UM.USUARIO=usernamex
              AND C.STATUS_CLIENTE='A' --and rownum<10
              AND LC.REF_DESPACHO IS NOT NULL and replace(LC.REF_DESPACHO,',','') is not null
              AND LC.ESTAD='A'  AND length(TRIM(TRANSLATE(replace(LC.REF_DESPACHO,',',''), ' +-.,0123456789', ' '))) is null
            ORDER BY 5;

        /*LOOP
     FETCH clientesx INTO rutasx;
				EXIT WHEN clientesx%NOTFOUND;


    END LOOP;
    CLOSE clientesx;


ELSIF




end if;*/

/*ELSE

open clientesx for
SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
CASE WHEN LC.REF_DESPACHO LIKE '-7%' THEN
CONCAT( CONCAT( SUBSTR(LC.REF_DESPACHO, INSTR(LC.REF_DESPACHO, ',')+1), ',' ) ,
 SUBSTR(LC.REF_DESPACHO, 0, INSTR(LC.REF_DESPACHO, ',')-1) )
ELSE
LC.REF_DESPACHO
END AS REF_DESPACHO,
LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA
  FROM SYSADM.CLIENTE C
	INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
  INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
	INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
	 INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
WHERE 1=1
	AND RV.DIA=(select translate(SUBSTR(to_char(sysdate, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
from dual)
  AND UM.USUARIO=usernamex
	AND C.STATUS_CLIENTE='A' --AND ROWNUM<20
	AND LC.REF_DESPACHO IS NOT NULL and replace(LC.REF_DESPACHO,',','') is not null
	AND LC.ESTAD='A'  AND length(TRIM(TRANSLATE(replace(LC.REF_DESPACHO,',',''), ' +-.,0123456789', ' '))) is null
ORDER BY 5;

end if;*/

        return clientesx;

    END CLIENTES_BY_DIA;


    FUNCTION  DATOS_CLIENTE(codClientex IN VARCHAR2,codEmpresax IN VARCHAR2) return types.ref_cursor is
        datosx types.ref_cursor;
    BEGIN

        open datosx for

            SELECT
                (
                    SELECT LIMITE FROM (SELECT
                                            LIMITE
                                        FROM
                                            (
                                                SELECT
                                                    NVL(LIMIT_CREDI, 0) AS LIMITE
                                                FROM
                                                    XRAYADMIN.SGC_CLIENTE_LINEA_CREDITO
                                                WHERE
                                                        CODIG_EMPRE = codEmpresax
                                                  AND CODIG_CLIEN =codClientex
                                                  AND VIGEN = TO_CHAR(SYSDATE, 'MM-YYYY')
                                                  AND ESTAD = 'A'
                                                UNION SELECT 0 AS LIMITE FROM DUAL
                                            )
                                        ORDER BY LIMITE DESC
                                       )WHERE ROWNUM < 2) AS LIMITECRED,
                (SELECT NVL(SUM(MDCC_SALDO),0) --INTO vDeudaActua
                 FROM SYSADM.DOCUMENTO_CXC
                 WHERE CDCC_COMPANIA =codEmpresax
                   AND (CDCC_CLIENTE =codClientex
                     OR CDCC_CLIENTE IN (SELECT COD_CLIENTE FROM SYSADM.CLIENTE WHERE COD_CIA = '00' AND ACCIONISTA_PRIN = codClientex))
                   AND MDCC_SALDO > 0) AS DEUDAACT,
                (SELECT NVL(SUM(MONTO_NETO_GUIA),0)
                 FROM SYSADM.GUIAS_HEADER
                 WHERE COD_CIA = '00'
                   AND COMPANIA_VENTA_3 = codEmpresax
                   AND (
                             COD_CLIENTE =codClientex
                         OR COD_CLIENTE IN (
                         SELECT CLIEN.COD_CLIENTE FROM SYSADM.CLIENTE CLIEN
                         WHERE COD_CIA = '00' AND CLIEN.ACCIONISTA_PRIN = codClientex)
                     )
                   AND STATUS_GUIA = 'I') AS DEUDAFUT,
                (
                    SELECT CL.TIPO_CLIENTE ||'-'||(SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='006' AND LLAVE=CL.TIPO_CLIENTE)
                    FROM SYSADM.CLIENTE CL
                    WHERE  COD_CLIENTE=codClientex)TIPO
            FROM
                SYSADM.DUAL;

        return datosx;

    END DATOS_CLIENTE;

    FUNCTION  DATOS_USUARIO(usernamex IN VARCHAR2) return types.ref_cursor is
        datosx types.ref_cursor;
        codmesax VARCHAR2(10);
    BEGIN

        SELECT distinct CODMESA into codmesax
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usernamex AND TB_TIPO_CODIGO='R';


        open datosx for
            SELECT distinct UM.CODEMPRESA, codmesax,TM.DESCMESA,UM.TB_TIPO_CODIGO,
                            TB.DESC1,
                            UM.CODCANAL,TC.DESCCANAL,UM.CODVENDEDOR,TM.LOCALIDAD,UM.CODSEDE,CASE WHEN UM.TB_TIPO_CODIGO='R' THEN
                                                                                                     'REGULAR'
                                                                                                 WHEN UM.TB_TIPO_CODIGO='A' THEN
                                                                                                     'ARROZ'
                                                                                                 WHEN UM.TB_TIPO_CODIGO='C' THEN
                                                                                                     'CAMPAÑA'END AS TIPOPEDIDO
            FROM SYSADM.TB_USUARIO_MESA UM
                     INNER JOIN SYSADM.TB_CANAL TC ON UM.CODCANAL=TC.CODCANAL
                     INNER JOIN SYSADM.TB_MESA TM ON TM.CODLOCALIDAD=UM.CODMESA
                     INNER JOIN SYSADM.TB_ALMACEN AL ON UM.CODEMPRESA=AL.CODEMPRESA  AND UM.CODSEDE=AL.CODIG_SEDE  and TM.LOCALIDAD=AL.CODLOCALIDAD
                     INNER JOIN SYSADM.TABLAS TB ON TB.CATEGORIA='424' AND TB.LLAVE=UM.CODEMPRESA||TM.LOCALIDAD

            WHERE 1=1
              AND USUARIO=usernamex AND  AL.ESTALMACEN='H'
              AND UM.ESTADO_EN_MESA='A'
            ORDER BY 9;
        return datosx;

    END DATOS_USUARIO;

/*FUNCTION  OBTENER_CONDICIONES(codListax IN VARCHAR2,codVendedorx IN VARCHAR2,tieneDeudaX IN NUMBER) return types.ref_cursor is
condicionesx types.ref_cursor ;
permiteCreditox NUMBER;
queryx varchar2(700);
ppCodEmp varchar2(3):='';
countaux2 number;
 BEGIN

SELECT distinct CODEMPRESA into ppCodEmp
 FROM SYSADM.TB_USUARIO_MESA
WHERE CODVENDEDOR=codVendedorx AND TB_TIPO_CODIGO='R';


SELECT COUNT(DISTINCT UM.CODVENDEDOR) INTO permiteCreditox
FROM SYSADM.TB_USUARIO_MESA UM WHERE UM.PERMI_CREDI=1 AND UM.CODVENDEDOR=codVendedorx;


--CONTADOS
   queryx:=' SELECT DISTINCT LC.CONDICION,COND.DESC1,FLAG10  from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS
			WHERE CATEGORIA=''008'')COND  ON  LC.COMPANIA='''||ppCodEmp||''' AND LC.CONDICION=COND.LLAVE where  LISTAPRECIO='''||codListax||''' AND FLAG10 IN (3,2) AND ESTAD=''A'' ';


SELECT COUNT(*)  INTO countaux2
											FROM SYSADM.VENDEDOR_CAMPANIA
											WHERE CODVENDEDOR=codVendedorx
											AND STATUS_VENDEDOR='A';
---CREDITOS
if countaux2=0 then
 if (permiteCreditox<>0 AND tieneDeudaX=0)THEN
 queryx:=queryx||' UNION ';
 queryx:=queryx||' SELECT DISTINCT LC.CONDICION,COND.DESC1,FLAG10  from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA=''008'')COND  ON  LC.COMPANIA='''||ppCodEmp||''' AND LC.CONDICION=COND.LLAVE where LISTAPRECIO='''||codListax||''' AND FLAG10 IN (1) AND ESTAD=''A'' order by 3 desc' ;
 end if;
ELSE
 queryx:=queryx||' UNION ';
 queryx:=queryx||' SELECT DISTINCT LC.CONDICION,COND.DESC1,FLAG10  from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA=''008'')COND  ON  LC.COMPANIA='''||ppCodEmp||''' AND LC.CONDICION=COND.LLAVE where LISTAPRECIO='''||codListax||''' AND FLAG10 IN (1) AND ESTAD=''A'' order by 3 desc' ;

end if;
	dbms_output.put_line(queryx);
open condicionesx for queryx;
return condicionesx;

END OBTENER_CONDICIONES;*/
    FUNCTION  OBTENER_CONDICIONES(codListax IN VARCHAR2,codVendedorx IN VARCHAR2,tieneDeudaX IN NUMBER,codclientex IN VARCHAR2) return types.ref_cursor is
        condicionesx types.ref_cursor ;
        permiteCreditox NUMBER;
        queryx varchar2(700);
        ppCodEmp varchar2(3):='';
        countaux2 number;
    BEGIN

        SELECT distinct CODEMPRESA into ppCodEmp
        FROM SYSADM.TB_USUARIO_MESA
        WHERE CODVENDEDOR=codVendedorx AND TB_TIPO_CODIGO='R';

        if tieneDeudaX=1 then

--CONTADOS
            queryx:=' SELECT DISTINCT LC.CONDICION,COND.DESC1,FLAG10  from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS
			WHERE CATEGORIA=''008'')COND  ON  LC.COMPANIA='''||ppCodEmp||''' AND LC.CONDICION=COND.LLAVE where  LISTAPRECIO='''||codListax||''' AND FLAG10 IN (3,2) AND ESTAD=''A''';


        else


            SELECT COUNT(DISTINCT UM.CODVENDEDOR) INTO permiteCreditox
            FROM SYSADM.TB_USUARIO_MESA UM WHERE UM.PERMI_CREDI=1 AND UM.CODVENDEDOR=codVendedorx;


--CONTADOS
            queryx:=' SELECT DISTINCT LC.CONDICION,COND.DESC1,FLAG10  from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS
			WHERE CATEGORIA=''008'')COND  ON  LC.COMPANIA='''||ppCodEmp||''' AND LC.CONDICION=COND.LLAVE where  LISTAPRECIO='''||codListax||''' AND FLAG10 IN (3,2) AND ESTAD=''A''';


            SELECT COUNT(*)  INTO countaux2
            FROM SYSADM.VENDEDOR_CAMPANIA
            WHERE CODVENDEDOR=codVendedorx
              AND STATUS_VENDEDOR='A';
---CREDITOS
            if countaux2=0 then
                if (permiteCreditox<>0 AND tieneDeudaX=0)THEN
                    queryx:=queryx||' UNION ';
                    queryx:=queryx||' SELECT DISTINCT LC.CONDICION,COND.DESC1,FLAG10  from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA=''008'')COND  ON  LC.COMPANIA='''||ppCodEmp||''' AND LC.CONDICION=COND.LLAVE where LISTAPRECIO='''||codListax||''' AND FLAG10 IN (1) AND ESTAD=''A'' order by 3 desc' ;
                end if;
            ELSE
                queryx:=queryx||' UNION ';
                queryx:=queryx||' SELECT DISTINCT LC.CONDICION,COND.DESC1,FLAG10  from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA=''008'')COND  ON  LC.COMPANIA='''||ppCodEmp||''' AND LC.CONDICION=COND.LLAVE where LISTAPRECIO='''||codListax||''' AND ESTAD=''A'' AND FLAG10 IN (1) order by 3 desc' ;

            end if;


        end if;


        dbms_output.put_line(queryx);
        open condicionesx for queryx;
        return condicionesx;

    END OBTENER_CONDICIONES;

    FUNCTION  BUSCAR_ARTICULOS(codempresax in varchar2,codalmacenx in varchar2,codlistax in varchar2,condpagox in varchar2,codcanalx IN VARCHAR2,  filtrox in varchar2) return types.ref_cursor is
        articulosx types.ref_cursor;

        codsedex  VARCHAR2(5);
    BEGIN

        select DISTINCT CODIG_SEDE INTO codsedex
        from
            SYSADM.LP_CONDICION
        WHERE COMPANIA=codempresax
          AND LISTAPRECIO=codlistax;






        open articulosx FOR

            SELECT *
            FROM
                (
                    SELECT
                        RES.*, TB3.PRECIO_FINAL,NVL(TC.CANTIDAD,0)PISO,CASE WHEN (RES.saldo-NVL(TC.CANTIDAD,0))<0 THEN 0 ELSE (RES.saldo-NVL(TC.CANTIDAD,0)) END  DISPONIBLE, CASE WHEN RES.SALDO< NVL(TC.CANTIDAD,0) THEN 1 ELSE 0 END  RESTRINGIDO
                    FROM
                        (
                            SELECT
                                F.COD_ITEM,
                                F.DESC_ITEM,
                                F.UM_VENTA,
                                F.UM_CONTROL_STOCK,
                                F.PRECIO_BASE,
                                F.SALDO,
                                F.CANT,
                                F.COD_LISTA,
                                F.COMPANIA_VENTA_3
                            FROM
                                (
                                    SELECT
                                        ROWNUM CANT,
                                        ART.COD_ITEM,
                                        ART.DESC_ITEM,
                                        ART.UM_VENTA,
                                        ART.UM_CONTROL_STOCK,
                                        PD.PRECIO_BASE,
                                        /*(
						NVL (SA.QTY_FISICA, 0) - NVL (SA.QTY_COMPROMETIDA, 0) - NVL (SA.QTY_RESERVADA, 0) - NVL (SA.QTY_PROYECTOS, 0)
					) */
                                        case when fc.operacion='*' THEN
                                                 trunc((NVL (SA.qty_fisica, 0) - NVL (SA.qty_comprometida, 0) - NVL (SA.qty_reservada, 0) - NVL (SA.qty_proyectos, 0) )/fc.factor)
                                             ELSE
                                                 trunc((NVL (SA.qty_fisica, 0) - NVL (SA.qty_comprometida, 0) - NVL (SA.qty_reservada, 0) - NVL (SA.qty_proyectos, 0) )*fc.factor)

                                            end

                                               SALDO,
                                        PD.COD_LISTA,
                                        SA.COMPANIA_VENTA_3
                                    FROM
                                        SYSADM.ARTICULOS ART
                                            INNER JOIN SYSADM.SALDOS_ALMACEN SA ON ART.COD_ITEM = SA.COD_ITEM
                                            INNER JOIN SYSADM.PRECIOS_D PD ON PD.COD_ITEM = ART.COD_ITEM
                                            AND PD.UM_ITEM = ART.UM_VENTA
                                            AND SA.COMPANIA_VENTA_3 = PD.COMPANIA_VENTA

                                            INNER JOIN SYSADM.FACTORES_CONV FC ON FC.UM_ORIGEN=art.um_venta AND FC.UM_DESTINO=art.UM_CONTROL_STOCK

                                    WHERE
                                            ART.COD_CIA = '00'
                                      AND ART.COD_LINEA <> '99'
                                      AND ART.STATUS_ITEM = 'A'
                                      AND PD.MONEDA_LISTA = 'S/.'
                                      AND PD.COD_LISTA =codlistax
                                      AND SA.COMPANIA_VENTA_3 =codempresax-- AND art.PROVEEDOR_DEFAULT NOT IN('0150','0182','0163')
                                      AND SA.ALMACEN =codalmacenx
                                      AND ART.COD_ITEM || ART.DESC_ITEM LIKE '%' || TRANSLATE (
                                            filtrox,
                                            'áéíóúÁÉÍÓÚ',
                                            'aeiouAEIOU'
                                        ) || '%'
                                ) F
                            WHERE
                                    F.CANT < 12
                        ) RES
                            INNER JOIN (
                            SELECT
                                TB2.COMPANIA_VENTA,
                                TB2.COD_LISTA,
                                TB2.UM_ITEM,
                                TB2.COD_ITEM,
                                CASE WHEN PRECIO_FINAL IS NULL THEN
                                         PRECIO_BASE
                                     ELSE
                                         PRECIO_FINAL
                                    END AS PRECIO_FINAL
                            FROM
                                (
                                    SELECT
                                        PRECIO_BASE,
                                        CASE WHEN COMPANIA_VENTA IN ('05', '08') THEN
                                                 (
                                                         PRECIO_BASE - (
                                                             DSCTO_LISTA_PRECIO + DSCTO_CP_ART + DSCTO_PROM_TIPEADOS
                                                         )
                                                     )
                                             ELSE
                                                     (
                                                             PRECIO_BASE - (
                                                                 DSCTO_LISTA_PRECIO + DSCTO_CP_ART + DSCTO_PROM_TIPEADOS
                                                             )
                                                         ) * 1.18
                                            END AS PRECIO_FINAL,
                                        TB.COMPANIA_VENTA,
                                        TB.COD_LISTA,
                                        TB.UM_ITEM,
                                        TB.COD_ITEM
                                    FROM
                                        (
                                            SELECT
                                                PD.COMPANIA_VENTA,
                                                PD.COD_LISTA,
                                                PD.PRECIO_BASE,
                                                PD.UM_ITEM,
                                                D .COND_PAGO,
                                                (
                                                            PD.PRECIO_BASE * NVL (PC.DSCTO_CARACTERISTI, 0) / 100
                                                    ) AS DSCTO_LISTA_PRECIO,
                                                (
                                                    CASE
                                                        WHEN PD.F_DSCTO_PROM_FIN < SYSDATE THEN
                                                            0
                                                        ELSE
                                                            CASE
                                                                WHEN PD.DSCTO_PROMOCION = 0 THEN
                                                                    0
                                                                ELSE
                                                                    (
                                                                                PD.PRECIO_BASE * PD.DSCTO_PROMOCION / 100
                                                                        )
                                                                END
                                                        END
                                                    ) AS DSCTO_PROM_TIPEADOS,
                                                CASE
                                                    WHEN D .DSCTO_CP_ARTICULO = 0 THEN
                                                        0
                                                    ELSE
                                                        (
                                                                    PD.PRECIO_BASE * D .DSCTO_CP_ARTICULO / 100
                                                            )
                                                    END AS DSCTO_CP_ART,
                                                PD.COD_ITEM
                                            FROM
                                                SYSADM.PRECIOS_D PD
                                                    INNER JOIN SYSADM.PRECIOS_C PC ON (
                                                            PC.COD_CIA = PD.COD_CIA
                                                        AND PD.COMPANIA_VENTA = PC.COMPANIA_VENTA
                                                        AND PC.COD_LISTA = PD.COD_LISTA
                                                    )
                                                    LEFT JOIN SYSADM.DCTO_COND_PAGO_ART D ON (
                                                                                                         D .COD_CIA = PC.COD_CIA
                                                                                                     AND D .COD_LISTA = PC.COD_LISTA
                                                                                                     AND D .COMPANIA_VENTA = PC.COMPANIA_VENTA
                                                                                                     AND D .COD_ITEM = PD.COD_ITEM
                                                                                                     AND D .UM_ITEM = PD.UM_ITEM
                                                                                                 )
                                                    AND D .COND_PAGO =condpagox
                                            WHERE
                                                    PD.COMPANIA_VENTA = codempresax
                                              AND PD.MONEDA_LISTA = 'S/.'
                                              AND PD.COD_LISTA = codlistax
                                        ) TB
                                ) TB2
                        ) TB3 ON RES.COMPANIA_VENTA_3=TB3.COMPANIA_VENTA AND RES.COD_LISTA=TB3.COD_LISTA  AND RES.COD_ITEM = TB3.COD_ITEM
                            AND RES.UM_VENTA = TB3.UM_ITEM
                            LEFT JOIN
                        SYSADM.TOPE_CANAL TC ON RES.COD_ITEM=TC.COD_ITEM AND COD_CANAL=codcanalx AND CODSEDE=codsedex
                    WHERE RES.SALDO>0
                )B ;--WHERE B.RESTRINGIDO=0;



        return articulosx;


    end  BUSCAR_ARTICULOS;

    FUNCTION CONSULTAR_UBIGEO
    (
        eTipoEleme  IN VARCHAR,
        eCodigEleme IN VARCHAR
    )
        RETURN TYPES.REF_CURSOR IS sListaEleme TYPES.REF_CURSOR;
    vSente VARCHAR2(2000);
    vCondi CHAR(200);
    BEGIN
        IF eTipoEleme = 'DEPAR' THEN  		--DEPARTAMENTO
            vCondi := ' WHERE CATEGORIA = ''090''
								AND LLAVE <> ''00''			--NINGUNA
								AND LLAVE <> ''99''			--EXTRANJERO';
        ELSIF eTipoEleme = 'PROVI' THEN 	--PROVINCIA
            vCondi := ' WHERE CATEGORIA = ''132''
								AND LLAVE LIKE '''||eCodigEleme||'%''';
        ELSE 													--DISTRITO
            vCondi := 'WHERE CATEGORIA = ''035''
								AND LLAVE LIKE '''||eCodigEleme||'%''';
        END IF;
        vSente := 'SELECT LLAVE,DESC1
						 FROM SYSADM.TABLAS
						 '||vCondi||'
						 ORDER BY LLAVE';
        DBMS_OUTPUT.PUT_LINE(vSente);
        OPEN sListaEleme FOR (vSente);
        RETURN sListaEleme;
    END CONSULTAR_UBIGEO;






/*PROCEDURE "REGISTRARPEDIDOMOVIL" (TABLE_OBJ IN PEDIDO_PREVENTA_TABLE,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
 IS
correlativo number;
ubigeox varchar2(6);
direccionx varchar(300);
descclientex varchar(300);
tdocx varchar(300);
coodenada2x varchar2(300);
coderrorx CHAR(1);
descerrorx VARCHAR2(100);
bandx NUMBER;
rucx VARCHAR2(11);
countuidx number;
cantcondcredx NUMBER;
rptaaptox NUMBER;
mensajeaptox VARCHAR2(200);
BEGIN
						rucx:=null;

					SELECT 0 INTO	 bandx
					FROM SYSADM.DUAL;

						IF bandx=1 THEN
						coderrorx:='1';
						descerrorx:='.';
						else
						coderrorx:='0';
						descerrorx:=null;

						END IF;

						SELECT NEW_COORDENADAS INTO coodenada2x
						FROM SYSADM.LOCALES_CLIENTE
						WHERE COD_CLIENTE=TABLE_OBJ(1).codcliente
						AND COD_LOCAL=TABLE_OBJ(1).coddirecciondespacho;

					 --coodenada2x:='-6.7701908,-79.857111';--salaverry
	VALIDATEDISTANCIA(TABLE_OBJ(1).USUREGISTRA,to_char(TABLE_OBJ(1).coordenadas),to_char(coodenada2x),rptax,mensajex);
	IF rptax=1 THEN

								select ciudad,DIRECCION_CLIENTE,DESC_CLIENTE,CASE WHEN RUC is not NULL then
								'01-'||RUC
								ELSE
								'02-'||LIB_ELECTORAL END AS DOC INTO ubigeox,direccionx,descclientex ,tdocx
								from SYSADM.CLIENTE c where c.COD_CLIENTE=TABLE_OBJ(1).codcliente;
								IF SUBSTR(tdocx,0,2)='01' THEN
								rucx:=SUBSTR(tdocx,4);
								END IF;

								SELECT SEQUENCE1.NEXTVAL
								INTO correlativo
								FROM DUAL;

								select COUNT(*) INTO countuidx
								from SYSADM.LOG_PREVENTA
								where  "UID"=TABLE_OBJ(1).correlativo
								and USUARIOX=TABLE_OBJ(1).USUREGISTRA
								AND TO_CHAR(FECHA,'YYYY-MM-DD')="TO_CHAR"(SYSDATE,'YYYY-MM-DD');

								IF countuidx=0 THEN


							"APTOTOPESTOCK" (TABLE_OBJ,rptaaptox,mensajeaptox) ;

							IF rptaaptox=1 THEN

												IF TABLE_OBJ.COUNT > 0 THEN

														SELECT count(*) into cantcondcredx--
														FROM--
															SYSADM.LP_CONDICION LC--
														INNER JOIN (
															SELECT
																*
															FROM
																SYSADM.TABLAS
															WHERE
																CATEGORIA = '008'
														) COND ON LC.COMPANIA = TABLE_OBJ(1).codempresa--
														AND LC.CONDICION = COND.LLAVE--
														WHERE--
														1=1--
														AND	LC.CONDICION=TABLE_OBJ(1).codcondicion --
														AND  LISTAPRECIO = TABLE_OBJ(1).codlistaprecios--
														AND FLAG10 IN (1);--

														IF cantcondcredx=0 THEN--
															 INSERT INTO   SYSADM.LOG_PREVENTA("UID",sec_pedxtransferir,codvendedor,fecha  ,usuariox)
																VALUES (TABLE_OBJ(1).correlativo,correlativo,TABLE_OBJ(1).codvendedor,SYSDATE,TABLE_OBJ(1).USUREGISTRA);

																FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST
																LOOP
																 INSERT INTO SYSADM.PEDID_X_TRANSFERIR(CPXT_COMPANIA,NPXT_SECPEDIDO,CPXT_CLIENTE,XPXT_CLIENTE,NPXT_RUCCLIENTE,CPXT_TIPOPEDIDO,CPXT_TIPODESPACHO,CPXT_VENDEDOR,CPXT_CONDPAGO,CPXT_MONEDA,CPXT_LOCALIDAD,FPXT_PEDIDO,CPXT_ALMACEN,CPXT_LISTAPRECIO,CPXT_DIRDESPACHO,
																CPXT_ARTICULO,QPXT_PEDIDO,CPXT_UM,MPXT_PRECIOPEDIDO,MPXT_VALORVENTA,BPXT_BONIFICADO,CPXT_UBIGEOFACCLI,XPXT_DIRFACCLI,BPXT_PEDRECHAZADO,ROWVERSION,FECHA_SIS,TERMINAL,CODMESA,CODSEDE,CODCANAL,CODLOCAL,CODRUTA,COD_TIPODOC,USER_REG,xpxt_archivoerror)
																values (TABLE_OBJ(indx).codempresa,correlativo,TABLE_OBJ(indx).codcliente,descclientex,rucx,'01','0',TABLE_OBJ(indx).codvendedor,TABLE_OBJ(indx).codcondicion,'S/.',
																TABLE_OBJ(indx).codlocalidad,to_date(SUBSTR(sysdate,0,10)),TABLE_OBJ(indx).codalmacen,TABLE_OBJ(indx).codlistaprecios,
																TABLE_OBJ(indx).coddirecciondespacho,TABLE_OBJ(indx).codproducto,TABLE_OBJ(indx).cantidad,TABLE_OBJ(indx).um,
																TABLE_OBJ(indx).precio,TABLE_OBJ(indx).cantidad*TABLE_OBJ(indx).precio,'0',ubigeox,direccionx,coderrorx,'0',
																SYSDATE,'MOVIL',TABLE_OBJ(indx).codmesa,TABLE_OBJ(indx).codsede,
																TABLE_OBJ(indx).codcanal,TABLE_OBJ(indx).coddirecciondespacho,TABLE_OBJ(indx).CODRUTA,SUBSTR(tdocx,0,2),TABLE_OBJ(indx).USUREGISTRA,descerrorx);
																dbms_output.put_line('SE REGISTRÓ CORRECTAMENTE');
																END LOOP;

															 UPDATE SYSADM.TBL_USUARIO_JORNADA
																SET IDNOPEDIDO=NULL,NROPEDIDO=correlativo,COORDENADA_PEDIDO=TABLE_OBJ(1).coordenadas
																WHERE CODCLIENTE=TABLE_OBJ(1).codcliente AND USUARIO=TABLE_OBJ(1).USUREGISTRA AND CODSEDE=TABLE_OBJ(1).codsede
															  AND CODLOCAL=TABLE_OBJ(1).coddirecciondespacho AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD');
																 rptax := 1;
																mensajex :='SE REGISTRÓ PEDIDO CONTADO CORRECTAMENTE ';


														ELSE --
																APTOCREDITOCAMPANIA(TABLE_OBJ ,rptax ,mensajex);--
																IF rptax=1 THEN --
																	 INSERT INTO   SYSADM.LOG_PREVENTA("UID",sec_pedxtransferir,codvendedor,fecha  ,usuariox)
																  VALUES (TABLE_OBJ(1).correlativo,correlativo,TABLE_OBJ(1).codvendedor,SYSDATE,TABLE_OBJ(1).USUREGISTRA);

																	FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST--
																	LOOP--
																	 INSERT INTO SYSADM.PEDID_X_TRANSFERIR(CPXT_COMPANIA,NPXT_SECPEDIDO,CPXT_CLIENTE,XPXT_CLIENTE,NPXT_RUCCLIENTE,CPXT_TIPOPEDIDO,CPXT_TIPODESPACHO,CPXT_VENDEDOR,CPXT_CONDPAGO,CPXT_MONEDA,CPXT_LOCALIDAD,FPXT_PEDIDO,CPXT_ALMACEN,CPXT_LISTAPRECIO,CPXT_DIRDESPACHO,
																	CPXT_ARTICULO,QPXT_PEDIDO,CPXT_UM,MPXT_PRECIOPEDIDO,MPXT_VALORVENTA,BPXT_BONIFICADO,CPXT_UBIGEOFACCLI,XPXT_DIRFACCLI,BPXT_PEDRECHAZADO,ROWVERSION,FECHA_SIS,TERMINAL,CODMESA,CODSEDE,CODCANAL,CODLOCAL,CODRUTA,COD_TIPODOC,USER_REG,xpxt_archivoerror)
																	values (TABLE_OBJ(indx).codempresa,correlativo,TABLE_OBJ(indx).codcliente,descclientex,rucx,'01','0',TABLE_OBJ(indx).codvendedor,TABLE_OBJ(indx).codcondicion,'S/.',
																	TABLE_OBJ(indx).codlocalidad,to_date(SUBSTR(sysdate,0,10)),TABLE_OBJ(indx).codalmacen,TABLE_OBJ(indx).codlistaprecios,
																	TABLE_OBJ(indx).coddirecciondespacho,TABLE_OBJ(indx).codproducto,TABLE_OBJ(indx).cantidad,TABLE_OBJ(indx).um,
																	TABLE_OBJ(indx).precio,TABLE_OBJ(indx).cantidad*TABLE_OBJ(indx).precio,'0',ubigeox,direccionx,coderrorx,'0',
																	SYSDATE,'MOVIL',TABLE_OBJ(indx).codmesa,TABLE_OBJ(indx).codsede,
																	TABLE_OBJ(indx).codcanal,TABLE_OBJ(indx).coddirecciondespacho,TABLE_OBJ(indx).CODRUTA,SUBSTR(tdocx,0,2),TABLE_OBJ(indx).USUREGISTRA,descerrorx);
																	dbms_output.put_line('SE REGISTRÓ CORRECTAMENTE');
																	END LOOP;--

																  UPDATE SYSADM.TBL_USUARIO_JORNADA
																 	SET IDNOPEDIDO=NULL,NROPEDIDO=correlativo,COORDENADA_PEDIDO=TABLE_OBJ(1).coordenadas
																 	WHERE CODCLIENTE=TABLE_OBJ(1).codcliente AND USUARIO=TABLE_OBJ(1).USUREGISTRA
																	AND CODSEDE=TABLE_OBJ(1).codsede
																	 AND CODLOCAL=TABLE_OBJ(1).coddirecciondespacho AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD');
 																  rptax := 1;
																	 mensajex :='SE REGISTRÓ PANETON A CRÈDITO CORRECTAMENTE ';
																	commit;
																ELSE
																	rptax := -2;
																	--mensajex :='NO ESTA PERMITIDO PASAR CREDITOS!!';
																END IF;

														END IF;



												END IF;


												ELSE
														rptax:=rptaaptox;
														mensajex:=mensajeaptox;

												END IF;


								ELSE

								rptax := -2;
								 mensajex :='EL PEDIDO YA ESTA REGISTRADO,PORFAVOR VERIFICAR!!';
								END IF;



ELSE
rptax := -2;

END IF;


EXCEPTION

	WHEN OTHERS THEN
		rptax := -1;
		rollback;
   mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

end REGISTRARPEDIDOMOVIL;*/


    PROCEDURE "REGISTRARPEDIDOMOVIL" (TABLE_OBJ IN PEDIDO_PREVENTA_TABLE,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
        IS
        correlativo number;
        ubigeox varchar2(6);
        direccionx varchar(300);
        descclientex varchar(300);
        tdocx varchar(300);
        coodenada2x varchar2(300);
        coderrorx CHAR(1);
        descerrorx VARCHAR2(100);
        bandx NUMBER;
        rucx VARCHAR2(11);
        countuidx number;
        cantcondcredx NUMBER;
        rptaaptox NUMBER;
        mensajeaptox VARCHAR2(200);

        cursorpaqpedx TYPES.REF_CURSOR;
        codpaqped VARCHAR2(10);
        cursorartpaq  TYPES.REF_CURSOR;

        importbasesinigv NUMBER;
        importconigv NUMBER;
        newcantidadum number;
        coditemx VARCHAR2(10);
        vdctospaq TYPES.REF_CURSOR;


        vcuotamin NUMBER;
        vcuotamax NUMBER;
        vporcdcto NUMBER;
        vbandsalir number:=0;
        vporcdctopaq NUMBER:=0;
        vporcdctovol number:=0;

        cantitempaq NUMBER;
        cantexistvol NUMBER;

        vparticipacionpaq types.ref_cursor;
        vtipox CHAR(1);


        vcuotaminx  NUMBER;
        vcuotamaxx NUMBER;
        vcoditembonifx VARCHAR2(10);
        vumitembonifx VARCHAR2(8);
        vprioridadx NUMBER;
        vcantbonifx NUMBER;
        vmultiplox NUMBER;

        vnewcantidadbonif NUMBER;
        vismultiplox NUMBER;
        visrangox NUMBER;


        testex EXCEPTION;
        mensajetest varchar2(200);

        umaux VARCHAR2(20);

--
    BEGIN
        coderrorx:='0';


        select ciudad,DIRECCION_CLIENTE,DESC_CLIENTE,CASE WHEN RUC is not NULL then
                                                                  '01-'||RUC
                                                          ELSE
                                                                  '02-'||LIB_ELECTORAL END AS DOC INTO ubigeox,direccionx,descclientex ,tdocx
        from SYSADM.CLIENTE c where c.COD_CLIENTE=TABLE_OBJ(1).codcliente;
        IF SUBSTR(tdocx,0,2)='01' THEN
            rucx:=SUBSTR(tdocx,4);
        END IF;

        SELECT SEQUENCE1.NEXTVAL
        INTO correlativo
        FROM DUAL;


        INSERT_DCTO_TEMP(correlativo ,TABLE_OBJ );

        --PAQUETE DCTO BY MONTO
        OPEN cursorpaqpedx FOR
            SELECT DISTINCT COD_PAQ
            FROM  sysadm."TEMP_DCTO"
            WHERE ID_PEDIDO=correlativo
              AND MODO_PART=0;

        LOOP
            FETCH cursorpaqpedx  INTO codpaqped;
            EXIT WHEN cursorpaqpedx%NOTFOUND;

            --ITEMS BY PAQUETE DCTO BY MONTO;
            OPEN cursorartpaq FOR
                SELECT  COD_ITEM,CANTIDAD,UM_ITEM
                FROM SYSADM.TEMP_DCTO
                WHERE 1=1
                  AND 	ID_PEDIDO=correlativo
                  AND COD_PAQ=codpaqped
                  AND MODO_PART=0;

            importconigv:=0;
            importbasesinigv:=0;
            --SUMA IMPORTE DE TODOS LOS RTICULOS DEL PAQUETE CON SUS DESCURNTOS DIJOS
            LOOP
                FETCH cursorartpaq  INTO coditemx,newcantidadum,umaux;
                EXIT WHEN cursorartpaq%NOTFOUND;

                importconigv:=importconigv+XRAYADMIN.GETPRECIOXRAY(TABLE_OBJ(1).codempresa,coditemx,umaux,TABLE_OBJ(1).codlistaprecios,TABLE_OBJ(1).codcondicion,newcantidadum);
                importbasesinigv:=XRAYADMIN.GETPRECIOBASESINIGV(TABLE_OBJ(1).codempresa,coditemx,TABLE_OBJ(1).um,TABLE_OBJ(1).codlistaprecios,TABLE_OBJ(1).codcondicion,newcantidadum);


                UPDAtE SYSADM.TEMP_DCTO
                SET IMP_BASE_SIN_IGV=importbasesinigv
                WHERE ID_PEDIDO=correlativo
                  AND COD_ITEM=coditemx;


            end LOOP;
            CLOSE cursorartpaq;




            OPEN vdctospaq FOR
                SELECT DISTINCT

                    p4.cuota_inf, p4.cuota_sup, p4.descuento  --detalles
                FROM SYSADM.precios_c c,
                     SYSADM.paquete p1,
                     SYSADM.particip_paquete p2,
                     SYSADM.articulos a,
                     SYSADM.descuentos_paquete p4
                WHERE c.cod_cia = p1.cod_cia
                  AND c.compania_venta = p1.compania_venta
                  AND c.cod_lista = p1.cod_lista
                  AND p1.cod_cia = p2.cod_cia
                  AND p1.compania_venta = p2.compania_venta
                  AND p1.cod_lista = p2.cod_lista
                  AND p1.moneda_lista = p2.moneda_lista
                  AND p1.cod_paquete = p2.cod_paquete
                  AND p2.cod_cia = a.cod_cia
                  AND p2.cod_item = a.cod_item
                  AND p1.cod_cia = p4.cod_cia
                  AND p1.compania_venta = p4.compania_venta
                  AND p1.cod_lista = p4.cod_lista
                  AND p1.moneda_lista = p4.moneda_lista
                  AND p1.cod_paquete = p4.cod_paquete
                  AND c.compania_venta = TABLE_OBJ(1).codempresa
                  AND c.cod_localidad = TABLE_OBJ(1).codlocalidad
                  AND p1.cod_paquete=codpaqped
                  AND p1.moneda_lista = 'S/.'
                  AND c.cod_lista = TABLE_OBJ(1).codlistaprecios
                  AND c.tipo_lista = '01'
                  AND SYSDATE>=p4.fecha_ini_vig
                  AND SYSDATE<=p4.fecha_fin_vig
                ORDER BY p4.cuota_inf asc;

            vbandsalir:=0;

            LOOP
                FETCH vdctospaq INTO vcuotamin ,vcuotamax,vporcdcto ;--DETALLES
                EXIT WHEN vdctospaq%NOTFOUND ;--or vbandsalir=1;


                if vcuotamin<=importconigv and importconigv< vcuotamax THEN
                    vporcdctopaq:=vporcdcto;
                    --	vbandsalir:=1;
                END IF;

            END LOOP;
            CLOSE vdctospaq;


            UPDATE SYSADM.TEMP_DCTO
            SET PORC_DCTO_PAQ=vporcdctopaq,
                MONTO_DCTO_PAQ=ROUND(vporcdctopaq/100*IMP_BASE_SIN_IGV,2)
            WHERE ID_PEDIDO=correlativo
              AND COD_PAQ=codpaqped;





        END LOOP;
        CLOSE cursorpaqpedx;

        --- finaliza dcto paquete por mmonto



        --PAQUETE DCTO POR CANTIDAD

        OPEN cursorpaqpedx FOR
            SELECT DISTINCT COD_PAQ
            FROM  sysadm."TEMP_DCTO"
            WHERE ID_PEDIDO=correlativo
              AND MODO_PART=1;

        LOOP
            FETCH cursorpaqpedx  INTO codpaqped;
            EXIT WHEN cursorpaqpedx%NOTFOUND;

            --ITEMS BY PAQUETE DCTO BY CANTIDAD;
            OPEN cursorartpaq FOR
                SELECT  COD_ITEM,CANTIDAD
                FROM SYSADM.TEMP_DCTO
                WHERE 1=1
                  AND 	ID_PEDIDO=correlativo
                  AND COD_PAQ=codpaqped
                  AND MODO_PART=1;

            cantitempaq:=0;
            importbasesinigv:=0;
            --SUMA IMPORTE DE TODOS LOS RTICULOS DEL PAQUETE CON SUS DESCURNTOS DIJOS
            LOOP
                FETCH cursorartpaq  INTO coditemx,newcantidadum;
                EXIT WHEN cursorartpaq%NOTFOUND;

                cantitempaq:=cantitempaq+newcantidadum;


                importbasesinigv:=XRAYADMIN.GETPRECIOBASESINIGV(TABLE_OBJ(1).codempresa,coditemx,TABLE_OBJ(1).um,TABLE_OBJ(1).codlistaprecios,TABLE_OBJ(1).codcondicion,newcantidadum);


                UPDAtE SYSADM.TEMP_DCTO
                SET IMP_BASE_SIN_IGV=importbasesinigv
                WHERE ID_PEDIDO=correlativo
                  AND COD_ITEM=coditemx;


            end LOOP;
            CLOSE cursorartpaq;




            OPEN vdctospaq FOR
                SELECT DISTINCT

                    p4.cuota_inf, p4.cuota_sup, p4.descuento  --detalles
                FROM SYSADM.precios_c c,
                     SYSADM.paquete p1,
                     SYSADM.particip_paquete p2,
                     SYSADM.articulos a,
                     SYSADM.descuentos_paquete p4
                WHERE c.cod_cia = p1.cod_cia
                  AND c.compania_venta = p1.compania_venta
                  AND c.cod_lista = p1.cod_lista
                  AND p1.cod_cia = p2.cod_cia
                  AND p1.compania_venta = p2.compania_venta
                  AND p1.cod_lista = p2.cod_lista
                  AND p1.moneda_lista = p2.moneda_lista
                  AND p1.cod_paquete = p2.cod_paquete
                  AND p2.cod_cia = a.cod_cia
                  AND p2.cod_item = a.cod_item
                  AND p1.cod_cia = p4.cod_cia
                  AND p1.compania_venta = p4.compania_venta
                  AND p1.cod_lista = p4.cod_lista
                  AND p1.moneda_lista = p4.moneda_lista
                  AND p1.cod_paquete = p4.cod_paquete
                  AND c.compania_venta = TABLE_OBJ(1).codempresa
                  AND c.cod_localidad = TABLE_OBJ(1).codlocalidad
                  AND p1.cod_paquete=codpaqped
                  AND p1.moneda_lista = 'S/.'
                  AND c.cod_lista = TABLE_OBJ(1).codlistaprecios
                  AND c.tipo_lista = '01'
                  AND SYSDATE>=p4.fecha_ini_vig
                  AND SYSDATE<=p4.fecha_fin_vig
                ORDER BY p4.cuota_inf asc;


            vbandsalir:=0;
            LOOP
                FETCH vdctospaq INTO vcuotamin ,vcuotamax,vporcdcto ;--DETALLES
                EXIT WHEN vdctospaq%NOTFOUND or vbandsalir=1;



                if vcuotamin<=cantitempaq and cantitempaq< vcuotamax THEN
                    vporcdctopaq:=vporcdcto;
                    vbandsalir:=1;
                END IF;

            END LOOP;
            CLOSE vdctospaq;


            UPDATE SYSADM.TEMP_DCTO
            SET PORC_DCTO_PAQ=vporcdctopaq,
                MONTO_DCTO_PAQ=ROUND(vporcdctopaq/100*IMP_BASE_SIN_IGV,2)
            WHERE ID_PEDIDO=correlativo
              AND COD_PAQ=codpaqped;





        END LOOP;
        CLOSE cursorpaqpedx;

        --END DCTO POR CANTIDADAD










        FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST
            LOOP

                importbasesinigv:=XRAYADMIN.GETPRECIOBASESINIGV(TABLE_OBJ(1).codempresa,TABLE_OBJ(indx).																codproducto,TABLE_OBJ(indx).um,TABLE_OBJ(1).codlistaprecios,TABLE_OBJ(1).codcondicion,TABLE_OBJ(indx).cantidad);

                --descuento por volumen

                SELECT  COUNT(*) INTO cantexistvol
                FROM SYSADM.TEMP_DCTO
                WHERE ID_PEDIDO=correlativo
                  and cod_item=TABLE_OBJ(indx).codproducto;


                IF cantexistvol>0 THEN

                    SELECT nvl(porc_dcto_vol,0) ,NVL(IMP_BASE_SIN_IGV,importbasesinigv) INTO  vporcdctovol,importbasesinigv
                    FROM SYSADM.TEMP_DCTO
                    WHERE ID_PEDIDO=correlativo
                      and cod_item=TABLE_OBJ(indx).codproducto;


                    UPDATE SYSADM.TEMP_DCTO
                    SET MONTO_DCTO_VOL=(vporcdctovol/100)*importbasesinigv
                    WHERE ID_PEDIDO=correlativo
                      and cod_item=TABLE_OBJ(indx).codproducto;

                ELSE

                    INSERT INTO SYSADM.TEMP_DCTO(ID_PEDIDO,COD_ITEM,UM_ITEM,CANTIDAD)
                    VALUES(correlativo,TABLE_OBJ(indx).codproducto,TABLE_OBJ(indx).um,TABLE_OBJ(indx).cantidad);


                END IF;


                --end dcto por volumen--




            END LOOP;







        ---empieza bonificaciones

        INSERT_BONIF_TEMP(correlativo ,TABLE_OBJ );



        --PAQUETE BONIF BY MONTO
        OPEN cursorpaqpedx FOR
            SELECT DISTINCT COD_PAQ,
                            CASE WHEN IS_RANGO=1 THEN 'R' ELSE 'M' END TIPO
            FROM  sysadm."TEMP_BONIF"
            WHERE ID_PEDIDO=correlativo
              AND MODO_PART=0;

        LOOP
            FETCH cursorpaqpedx  INTO codpaqped,vtipox;
            EXIT WHEN cursorpaqpedx%NOTFOUND;

            --ITEMS BY PAQUETE BONIF BY MONTO;
            OPEN cursorartpaq FOR
                SELECT  COD_ITEM,CANTIDAD,UM_ITEM
                FROM SYSADM.TEMP_BONIF
                WHERE 1=1
                  AND 	ID_PEDIDO=correlativo
                  AND COD_PAQ=codpaqped
                  AND MODO_PART=0;

            importconigv:=0;
            importbasesinigv:=0;
            --SUMA IMPORTE DE TODOS LOS RTICULOS DEL PAQUETE
            LOOP
                FETCH cursorartpaq  INTO coditemx,newcantidadum,umaux;
                EXIT WHEN cursorartpaq%NOTFOUND;

                importconigv:=importconigv+XRAYADMIN.GETPRECIOXRAY(TABLE_OBJ(1).codempresa,coditemx,umaux,TABLE_OBJ(1).codlistaprecios,TABLE_OBJ(1).codcondicion,newcantidadum);
                importbasesinigv:=importbasesinigv+XRAYADMIN.GETPRECIOBASESINIGVBONIF(TABLE_OBJ(1).codempresa,coditemx,umaux,TABLE_OBJ(1).codlistaprecios,TABLE_OBJ(1).codcondicion,newcantidadum);


                UPDAtE SYSADM.TEMP_BONIF
                SET IMP_BASE_SIN_IGV=XRAYADMIN.GETPRECIOBASESINIGVBONIF(TABLE_OBJ(1).codempresa,coditemx,umaux,TABLE_OBJ(1).codlistaprecios,TABLE_OBJ(1).codcondicion,newcantidadum)--importbasesinigv
                WHERE ID_PEDIDO=correlativo
                  AND COD_ITEM=coditemx
                  AND COD_PAQ=codpaqped;


            end LOOP;
            CLOSE cursorartpaq;




            OPEN vparticipacionpaq FOR

                SELECT   p.cuota_min, p.cuota_max, p.cod_item_bonif, p.um_bonif,
                         p.prioridad, p.cantidad_bonif, p.multiplo
                FROM SYSADM.bonif_paq_p2 p, SYSADM.articulos a
                WHERE p.cod_cia = a.cod_cia
                  AND p.cod_item_bonif = a.cod_item
                  AND p.cod_cia = '00'
                  AND p.compania_venta = TABLE_OBJ(1).codempresa
                  AND p.cod_lista = TABLE_OBJ(1).codlistaprecios
                  AND p.moneda_lista = 'S/.'
                  AND p.cod_paquete =codpaqped
                  AND p.PRIORIDAD=1
                ORDER BY p.cod_paquete, p.cuota_min, p.cuota_max, p.cod_item_bonif;

            vbandsalir:=0;

            LOOP
                FETCH vparticipacionpaq INTO  vcuotaminx ,vcuotamaxx,vcoditembonifx,vumitembonifx,
                    vprioridadx,vcantbonifx,vmultiplox;
                EXIT WHEN vparticipacionpaq%NOTFOUND ;--or vbandsalir=1;


                if vcuotaminx<=importbasesinigv and importbasesinigv< vcuotamaxx THEN

                    if vtipox='M' THEN
                        vismultiplox:=1;
                        visrangox:=0;

                        vnewcantidadbonif:=trunc(importbasesinigv/vmultiplox)*vcantbonifx;

                    ELSIF vtipox='R' THEN
                        vismultiplox:=0;
                        visrangox:=1;

                        vnewcantidadbonif:=vcantbonifx;

                    END IF;

                    INSERT INTO SYSADM.TEMP_BONIF_DET(ID_PEDIDO,COD_ITEM_BONIF,UM_ITEM_BONIF,CANTIDAD,COD_PAQ,IS_RANGO,IS_MULTIPLO,MODO_PART)
                    VALUES(correlativo,vcoditembonifx,vumitembonifx,vnewcantidadbonif,codpaqped,visrangox,vismultiplox,0);




                    --vbandsalir:=1;
                END IF;

            END LOOP;
            CLOSE vparticipacionpaq;








        END LOOP;
        CLOSE cursorpaqpedx;







        ---PAQUETE  BONIFICACION POR CANTIDADES


        OPEN cursorpaqpedx FOR
            SELECT DISTINCT COD_PAQ,
                            CASE WHEN IS_RANGO=1 THEN 'R' ELSE 'M' END TIPO
            FROM  sysadm."TEMP_BONIF"
            WHERE ID_PEDIDO=correlativo
              AND MODO_PART=1;

        LOOP
            FETCH cursorpaqpedx  INTO codpaqped,vtipox;
            EXIT WHEN cursorpaqpedx%NOTFOUND;

            --ITEMS BY PAQUETE BONIF BY CANTIDAD;
            OPEN cursorartpaq FOR
                SELECT  COD_ITEM,CANTIDAD,UM_ITEM
                FROM SYSADM.TEMP_BONIF
                WHERE 1=1
                  AND 	ID_PEDIDO=correlativo
                  AND COD_PAQ=codpaqped
                  AND MODO_PART=1;

            cantitempaq:=0;
            importbasesinigv:=0;
            --SUMA IMPORTE DE TODOS LOS RTICULOS DEL PAQUETE
            LOOP
                FETCH cursorartpaq  INTO coditemx,newcantidadum,umaux;
                EXIT WHEN cursorartpaq%NOTFOUND;

                cantitempaq:=cantitempaq+newcantidadum;

                importbasesinigv:=XRAYADMIN.GETPRECIOBASESINIGVBONIF(TABLE_OBJ(1).codempresa,coditemx,umaux,TABLE_OBJ(1).codlistaprecios,TABLE_OBJ(1).codcondicion,newcantidadum);




            end LOOP;
            CLOSE cursorartpaq;






            OPEN vparticipacionpaq FOR

                SELECT   p.cuota_min, p.cuota_max, p.cod_item_bonif, p.um_bonif,
                         p.prioridad, p.cantidad_bonif, p.multiplo
                FROM SYSADM.bonif_paq_p2 p, SYSADM.articulos a
                WHERE p.cod_cia = a.cod_cia
                  AND p.cod_item_bonif = a.cod_item
                  AND p.cod_cia = '00'
                  AND p.compania_venta = TABLE_OBJ(1).codempresa
                  AND p.cod_lista = TABLE_OBJ(1).codlistaprecios
                  AND p.moneda_lista = 'S/.'
                  AND p.cod_paquete =codpaqped
                  AND p.PRIORIDAD=1
                ORDER BY p.cod_paquete, p.cuota_min, p.cuota_max, p.cod_item_bonif;

            vbandsalir:=0;

            LOOP
                FETCH vparticipacionpaq INTO  vcuotaminx ,vcuotamaxx,vcoditembonifx,vumitembonifx,
                    vprioridadx,vcantbonifx,vmultiplox;
                EXIT WHEN vparticipacionpaq%NOTFOUND;-- or vbandsalir=1;

                --	RAISE testex;

                if vcuotaminx<=cantitempaq and cantitempaq< vcuotamaxx THEN

                    if vtipox='M' THEN
                        vismultiplox:=1;
                        visrangox:=0;

                        vnewcantidadbonif:=trunc(cantitempaq/vmultiplox)*vcantbonifx;

                    ELSIF vtipox='R' THEN
                        vismultiplox:=0;
                        visrangox:=1;

                        vnewcantidadbonif:=vcantbonifx;

                    END IF;

                    INSERT INTO SYSADM.TEMP_BONIF_DET(ID_PEDIDO,COD_ITEM_BONIF,UM_ITEM_BONIF,CANTIDAD,COD_PAQ,IS_RANGO,IS_MULTIPLO,MODO_PART)
                    VALUES(correlativo,vcoditembonifx,vumitembonifx,vnewcantidadbonif,codpaqped,visrangox,vismultiplox,1);




                    --	vbandsalir:=1;
                END IF;

            END LOOP;
            CLOSE vparticipacionpaq;








        END LOOP;
        CLOSE cursorpaqpedx;




        ---- empieza bonificacin por item

        FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST
            LOOP

            --mensajetest :=coditemx||'--'||TABLE_OBJ(1).codcondicion||'--'||TABLE_OBJ(1).codempresa||'--'||TABLE_OBJ(indx).cantidad||'-'||TABLE_OBJ(indx).um;

            --	raise testex;
                importconigv:=importconigv+XRAYADMIN.GETPRECIOXRAY(TABLE_OBJ(1).codempresa,TABLE_OBJ(indx).codproducto,TABLE_OBJ(indx).um,TABLE_OBJ(1).codlistaprecios,TABLE_OBJ(1).codcondicion,TABLE_OBJ(indx).cantidad);--newcantidadum);
                importbasesinigv:=XRAYADMIN.GETPRECIOBASESINIGVBONIF(TABLE_OBJ(1).codempresa,TABLE_OBJ(indx).																codproducto,TABLE_OBJ(indx).um,TABLE_OBJ(1).codlistaprecios,TABLE_OBJ(1).codcondicion,TABLE_OBJ(indx).cantidad);


                OPEN vparticipacionpaq FOR

                    SELECT     b1.cuota_min,
                               b1.cuota_max, b1.cod_item_bonif, b1.um_bonif, b1.cantidad_bonif,
                               b1.multiplo ,B.tipo_bonif,b.flag_multiplo,b.flag_rangos

                    FROM sysadm.bonificacion_d1 b1, sysadm.bonificacion b
                    WHERE b.cod_cia = b1.cod_cia
                      AND b.compania_venta = b1.compania_venta
                      AND b.cod_lista = b1.cod_lista
                      AND b.moneda_lista = b1.moneda_lista
                      AND b.cod_bonif = b1.cod_bonif
                      AND b.flag_articulo = '1'
                      AND b.compania_venta = TABLE_OBJ(1).codempresa
                      AND b.cod_lista = TABLE_OBJ(1).codlistaprecios
                      and b1.prioridad=1
                      AND COD_ITEM=TABLE_OBJ(indx).codproducto
                      AND SYSDATE>= b.fecha_inicio
                      AND SYSDATE<=b.fecha_fin
                    ORDER BY b1.cuota_min asc;




                vbandsalir:=0;

                LOOP
                    FETCH vparticipacionpaq INTO  vcuotaminx ,vcuotamaxx,vcoditembonifx,vumitembonifx,
                        vcantbonifx,vmultiplox,vtipox,--SI ES UNIDAD DE MEDIA O MONTO
                        vismultiplox,visrangox;
                    EXIT WHEN vparticipacionpaq%NOTFOUND;-- or vbandsalir=1;

                    --	RAISE testex;
                    IF vtipox=0 THEN-- SI ES POR MONTO
                        if vcuotaminx<= importbasesinigv and importbasesinigv< vcuotamaxx THEN

                            if vismultiplox=1  THEN
                                visrangox:=0;

                                vnewcantidadbonif:=trunc(importbasesinigv/vmultiplox)*vcantbonifx;

                            ELSIF visrangox=1 THEN
                                vismultiplox:=0;

                                vnewcantidadbonif:=vcantbonifx;

                            END IF;

                            INSERT INTO SYSADM.TEMP_BONIF_DET(ID_PEDIDO,COD_ITEM_BONIF,UM_ITEM_BONIF,CANTIDAD,COD_PAQ,IS_RANGO,IS_MULTIPLO,MODO_PART)
                            VALUES(correlativo,vcoditembonifx,vumitembonifx,vnewcantidadbonif,codpaqped,visrangox,vismultiplox,0);--0 ES POR MONTO




                        --	vbandsalir:=1;
                        END IF;

                    ELSif vtipox=1 THEN --ES POR UNIDA DE MEDIDA

                        if vcuotaminx<= TABLE_OBJ(indx).cantidad and  TABLE_OBJ(indx).cantidad < vcuotamaxx THEN

                            if vismultiplox=1  THEN
                                visrangox:=0;

                                vnewcantidadbonif:=trunc(TABLE_OBJ(indx).cantidad/vmultiplox)*vcantbonifx;

                            ELSIF visrangox=1 THEN
                                vismultiplox:=0;

                                vnewcantidadbonif:=vcantbonifx;

                            END IF;

                            INSERT INTO SYSADM.TEMP_BONIF_DET(ID_PEDIDO,COD_ITEM_BONIF,UM_ITEM_BONIF,CANTIDAD,COD_PAQ,IS_RANGO,IS_MULTIPLO,MODO_PART)
                            VALUES(correlativo,vcoditembonifx,vumitembonifx,vnewcantidadbonif,NULL,visrangox,vismultiplox,1);--1 ES POR UNIDADD MEDIDA
                        END IF;
                    END IF;








                END LOOP;
                CLOSE vparticipacionpaq;



                --end bonif por item--




            END LOOP;











        registrar_pedido_xray( correlativo,TABLE_OBJ, rptax, mensajex);
        --rptax:=1;--borrar
--mensajex:='ok'; --borrar

    EXCEPTION
        WHEN testex THEN


            rptax:=-1;
            mensajex:=mensajetest;

        WHEN OTHERS THEN
            rptax := -1;
            rollback;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

    end REGISTRARPEDIDOMOVIL;






    PROCEDURE "REGISTRARPEDIDOMOVILOFFLINE" (TABLE_OBJ IN PEDIDO_PREVENTA_TABLE,montototalsugeridox IN NUMBER, rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
        IS
        correlativo number;
        ubigeox varchar2(6);
        direccionx varchar(300);
        descclientex varchar(300);
        tdocx varchar(300);
        coodenada2x varchar2(300);
        coderrorx CHAR(1);
        descerrorx VARCHAR2(100);
        bandx NUMBER;
        rucx VARCHAR2(11);
        countuidx number;
        cantcondcredx NUMBER;
        rptavalidacionmontox number;
        rptavalidacionhorax number;
        codigosartx varchar2(2000);
    BEGIN

        FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST--
            LOOP
                codigosartx:= codigosartx||TABLE_OBJ(indx).CODRUTA||',';

            END LOOP;

        rptavalidacionhorax:=XRAYADMIN.SGCSQL_VALIDAR_HORA_PEDIDO(TABLE_OBJ(1).codempresa,TABLE_OBJ(1).codsede,TABLE_OBJ(1).codmesa,TABLE_OBJ(1).codcanal,rtrim(codigosartx,','),TABLE_OBJ(1).USUREGISTRA,TABLE_OBJ(1).codvendedor,1);

        rptavalidacionmontox:=XRAYADMIN.SGCSQL_VALIDAR_MONTO_PEDIDO(TABLE_OBJ(1).codempresa,TABLE_OBJ(1).codsede,TABLE_OBJ(1).codmesa,TABLE_OBJ(1).codcanal,TABLE_OBJ(1).codcondicion,TABLE_OBJ(1).USUREGISTRA,TABLE_OBJ(1).codvendedor,montototalsugeridox,rtrim(codigosartx,','));

        IF rptavalidacionhorax=1 THEN

            IF rptavalidacionmontox=1 THEN

                rucx:=null;
                SELECT CASE WHEN TO_CHAR(SYSDATE,'HH24:mi:ss')<='04:00:00' THEN 1 ELSE 0 END INTO  bandx
                FROM SYSADM.DUAL;

                IF bandx=1 THEN
                    coderrorx:='1';
                    descerrorx:='.';
                else
                    coderrorx:='0';
                    descerrorx:=null;

                END IF;

                SELECT NEW_COORDENADAS INTO coodenada2x
                FROM SYSADM.LOCALES_CLIENTE
                WHERE COD_CLIENTE=TABLE_OBJ(1).codcliente
                  AND COD_LOCAL=TABLE_OBJ(1).coddirecciondespacho;

                VALIDATEDISTANCIA(TABLE_OBJ(1).USUREGISTRA,to_char(TABLE_OBJ(1).coordenadas),to_char(coodenada2x),rptax,mensajex);
                IF rptax=1 THEN

                    select ciudad,DIRECCION_CLIENTE,DESC_CLIENTE,CASE WHEN RUC is not NULL then
                                                                              '01-'||RUC
                                                                      ELSE
                                                                              '02-'||LIB_ELECTORAL END AS DOC INTO ubigeox,direccionx,descclientex ,tdocx
                    from SYSADM.CLIENTE c where c.COD_CLIENTE=TABLE_OBJ(1).codcliente;
                    IF SUBSTR(tdocx,0,2)='01' THEN
                        rucx:=SUBSTR(tdocx,4);
                    END IF;

                    SELECT SEQUENCE1.NEXTVAL
                    INTO correlativo
                    FROM DUAL;

                    select COUNT(*) INTO countuidx
                    from SYSADM.LOG_PREVENTA
                    where  "UID"=TABLE_OBJ(1).correlativo
                      and USUARIOX=TABLE_OBJ(1).USUREGISTRA
                      AND TO_CHAR(FECHA,'YYYY-MM-DD')="TO_CHAR"(SYSDATE,'YYYY-MM-DD');

                    IF countuidx=0 THEN

                        IF TABLE_OBJ.COUNT > 0 THEN

                            SELECT count(*) into cantcondcredx--
                            FROM--
                                SYSADM.LP_CONDICION LC--
                                    INNER JOIN (
                                    SELECT
                                        *
                                    FROM
                                        SYSADM.TABLAS
                                    WHERE
                                            CATEGORIA = '008'
                                ) COND ON LC.COMPANIA = TABLE_OBJ(1).codempresa--
                                    AND LC.CONDICION = COND.LLAVE--
                            WHERE--
                                    1=1--
                              AND	LC.CONDICION=TABLE_OBJ(1).codcondicion --
                              AND  LISTAPRECIO = TABLE_OBJ(1).codlistaprecios--
                              AND FLAG10 IN (1);--

                            IF cantcondcredx=0 THEN--
                                INSERT INTO   SYSADM.LOG_PREVENTA("UID",sec_pedxtransferir,codvendedor,fecha  ,usuariox)
                                VALUES (TABLE_OBJ(1).correlativo,correlativo,TABLE_OBJ(1).codvendedor,SYSDATE,TABLE_OBJ(1).USUREGISTRA);

                                FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST
                                    LOOP
                                        INSERT INTO SYSADM.PEDID_X_TRANSFERIR(CPXT_COMPANIA,NPXT_SECPEDIDO,CPXT_CLIENTE,XPXT_CLIENTE,NPXT_RUCCLIENTE,CPXT_TIPOPEDIDO,CPXT_TIPODESPACHO,CPXT_VENDEDOR,CPXT_CONDPAGO,CPXT_MONEDA,CPXT_LOCALIDAD,FPXT_PEDIDO,CPXT_ALMACEN,CPXT_LISTAPRECIO,CPXT_DIRDESPACHO,
                                                                              CPXT_ARTICULO,QPXT_PEDIDO,CPXT_UM,MPXT_PRECIOPEDIDO,MPXT_VALORVENTA,BPXT_BONIFICADO,CPXT_UBIGEOFACCLI,XPXT_DIRFACCLI,BPXT_PEDRECHAZADO,ROWVERSION,FECHA_SIS,TERMINAL,CODMESA,CODSEDE,CODCANAL,CODLOCAL,CODRUTA,COD_TIPODOC,USER_REG,xpxt_archivoerror)
                                        values (TABLE_OBJ(indx).codempresa,correlativo,TABLE_OBJ(indx).codcliente,descclientex,rucx,'01','0',TABLE_OBJ(indx).codvendedor,TABLE_OBJ(indx).codcondicion,'S/.',
                                                TABLE_OBJ(indx).codlocalidad,to_date(SUBSTR(sysdate,0,10)),TABLE_OBJ(indx).codalmacen,TABLE_OBJ(indx).codlistaprecios,
                                                TABLE_OBJ(indx).coddirecciondespacho,TABLE_OBJ(indx).codproducto,TABLE_OBJ(indx).cantidad,TABLE_OBJ(indx).um,
                                                TABLE_OBJ(indx).precio,TABLE_OBJ(indx).cantidad*TABLE_OBJ(indx).precio,'0',ubigeox,direccionx,coderrorx,'0',
                                                SYSDATE,'MOVIL',TABLE_OBJ(indx).codmesa,TABLE_OBJ(indx).codsede,
                                                TABLE_OBJ(indx).codcanal,TABLE_OBJ(indx).coddirecciondespacho,TABLE_OBJ(indx).CODRUTA,SUBSTR(tdocx,0,2),TABLE_OBJ(indx).USUREGISTRA,descerrorx);
                                        dbms_output.put_line('SE REGISTRÓ CORRECTAMENTE');
                                    END LOOP;

                                UPDATE SYSADM.TBL_USUARIO_JORNADA
                                SET IDNOPEDIDO=NULL,NROPEDIDO=correlativo,COORDENADA_PEDIDO=TABLE_OBJ(1).coordenadas
                                WHERE CODCLIENTE=TABLE_OBJ(1).codcliente AND USUARIO=TABLE_OBJ(1).USUREGISTRA
                                  AND CODSEDE=TABLE_OBJ(1).codsede
                                  AND CODLOCAL=TABLE_OBJ(1).coddirecciondespacho AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD');
                                rptax := 1;
                                mensajex :='SE REGISTRÓ PEDIDO CONTADO CORRECTAMENTE ';
                                commit;

                            ELSE --
                                APTOCREDITOCAMPANIA(TABLE_OBJ ,rptax ,mensajex);--
                                IF rptax=1 THEN --
                                    INSERT INTO   SYSADM.LOG_PREVENTA("UID",sec_pedxtransferir,codvendedor,fecha  ,usuariox)
                                    VALUES (TABLE_OBJ(1).correlativo,correlativo,TABLE_OBJ(1).codvendedor,SYSDATE,TABLE_OBJ(1).USUREGISTRA);

                                    FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST--
                                        LOOP--
                                    INSERT INTO SYSADM.PEDID_X_TRANSFERIR(CPXT_COMPANIA,NPXT_SECPEDIDO,CPXT_CLIENTE,XPXT_CLIENTE,NPXT_RUCCLIENTE,CPXT_TIPOPEDIDO,CPXT_TIPODESPACHO,CPXT_VENDEDOR,CPXT_CONDPAGO,CPXT_MONEDA,CPXT_LOCALIDAD,FPXT_PEDIDO,CPXT_ALMACEN,CPXT_LISTAPRECIO,CPXT_DIRDESPACHO,
                                                                          CPXT_ARTICULO,QPXT_PEDIDO,CPXT_UM,MPXT_PRECIOPEDIDO,MPXT_VALORVENTA,BPXT_BONIFICADO,CPXT_UBIGEOFACCLI,XPXT_DIRFACCLI,BPXT_PEDRECHAZADO,ROWVERSION,FECHA_SIS,TERMINAL,CODMESA,CODSEDE,CODCANAL,CODLOCAL,CODRUTA,COD_TIPODOC,USER_REG,xpxt_archivoerror)
                                    values (TABLE_OBJ(indx).codempresa,correlativo,TABLE_OBJ(indx).codcliente,descclientex,rucx,'01','0',TABLE_OBJ(indx).codvendedor,TABLE_OBJ(indx).codcondicion,'S/.',
                                            TABLE_OBJ(indx).codlocalidad,to_date(SUBSTR(sysdate,0,10)),TABLE_OBJ(indx).codalmacen,TABLE_OBJ(indx).codlistaprecios,
                                            TABLE_OBJ(indx).coddirecciondespacho,TABLE_OBJ(indx).codproducto,TABLE_OBJ(indx).cantidad,TABLE_OBJ(indx).um,
                                            TABLE_OBJ(indx).precio,TABLE_OBJ(indx).cantidad*TABLE_OBJ(indx).precio,'0',ubigeox,direccionx,coderrorx,'0',
                                            SYSDATE,'MOVIL',TABLE_OBJ(indx).codmesa,TABLE_OBJ(indx).codsede,
                                            TABLE_OBJ(indx).codcanal,TABLE_OBJ(indx).coddirecciondespacho,TABLE_OBJ(indx).CODRUTA,SUBSTR(tdocx,0,2),TABLE_OBJ(indx).USUREGISTRA,descerrorx);
                                    dbms_output.put_line('SE REGISTRÓ CORRECTAMENTE');
                                        END LOOP;--

                                    UPDATE SYSADM.TBL_USUARIO_JORNADA
                                    SET IDNOPEDIDO=NULL,NROPEDIDO=correlativo,COORDENADA_PEDIDO=TABLE_OBJ(1).coordenadas
                                    WHERE CODCLIENTE=TABLE_OBJ(1).codcliente AND USUARIO=TABLE_OBJ(1).USUREGISTRA
                                      AND CODSEDE=TABLE_OBJ(1).codsede
                                      AND CODLOCAL=TABLE_OBJ(1).coddirecciondespacho AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD');
                                    --dbms_output.put_line('SE REGISTRÓ CORRECTAMENTE');
                                    rptax := 1;
                                    mensajex :='SE REGISTRÓ CAMPAÑA A CRÈDITO CORRECTAMENTE ';
                                    commit;


                                ELSE
                                    rptax := -2;
                                    --mensajex :='NO ESTA PERMITIDO PASAR CREDITOS!!';
                                END IF;

                            END IF;



                        END IF;

                    ELSE

                        rptax := -2;
                        mensajex :='EL PEDIDO YA ESTA REGISTRADO,PORFAVOR VERIFICAR!!';
                    END IF;



                ELSE
                    rptax := -2;

                END IF;

            ELSE
                rptax := -2;
                mensajex :=TABLE_OBJ(1).codcliente||' NO SUPERA EL MONTO MINIMO DE '||rptavalidacionmontox||' SOLES!! ';

            END IF;

        ELSE

            rptax := -2;
            mensajex :=TABLE_OBJ(1).codcliente||' SINCRONIZACIÓN FUERA DEL PLAZO PERMITIDO!!';


        END IF;


    EXCEPTION

        WHEN OTHERS THEN
            rptax := -1;
            rollback;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

    end REGISTRARPEDIDOMOVILOFFLINE;















    FUNCTION LISTAR_TIPO_CLIENTE

        return types.ref_cursor is
        filas types.ref_cursor;
    BEGIN
        OPEN filas FOR
            select LLAVE,upper(DESC1),ESTADO from sysadm.TABLAS where categoria = '006' AND ESTADO = 'H' ORDER BY 2 asc;

        return (filas);
    END LISTAR_TIPO_CLIENTE;

    FUNCTION LISTAR_GIRO_CLIENTE

        return types.ref_cursor is
        filas types.ref_cursor;
    BEGIN
        OPEN filas FOR
            select LLAVE,upper(DESC1),ESTADO from sysadm.TABLAS where categoria = '329' ORDER BY 2 asc;

        return (filas);
    END LISTAR_GIRO_CLIENTE;



    PROCEDURE  DATOS_USUARIO_ALTA(
        usuariox IN VARCHAR2,
        csedesx OUT types.ref_cursor,
        ccanalx OUT types.ref_cursor,
        cempresasx OUT types.ref_cursor
    )
        IS
    BEGIN

        OPEN csedesx FOR
            SELECT DISTINCT UM.CODSEDE,UM.CODSEDE||'-'||TS.DESCSEDE
            FROM SYSADM.TB_USUARIO_MESA UM
                     INNER JOIN SYSADM.TB_SEDE TS ON UM.CODSEDE=TS.CODSEDE
            WHERE 1=1
              AND USUARIO=usuariox
              AND UM.ESTADO_EN_MESA='A';

        OPEN ccanalx FOR
            SELECT DISTINCT UM.CODCANAL,UM.CODCANAL||'-'||TC.DESCCANAL
            FROM SYSADM.TB_USUARIO_MESA UM
                     INNER JOIN SYSADM.TB_CANAL TC ON UM.CODCANAL=TC.CODCANAL
            WHERE 1=1
              AND USUARIO=usuariox
              AND UM.ESTADO_EN_MESA='A';

        OPEN cempresasx FOR
            SELECT DISTINCT UM.CODEMPRESA,UM.CODEMPRESA||'-'||EMP.DESC1
            FROM SYSADM.TB_USUARIO_MESA UM
                     INNER JOIN (SELECT LLAVE,DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='001' AND ESTADO='H')EMP ON UM.CODEMPRESA=EMP.LLAVE
            WHERE 1=1
              AND USUARIO=usuariox
              AND UM.ESTADO_EN_MESA='A';

    END DATOS_USUARIO_ALTA;



    PROCEDURE  DATOS_CLIENTE_PREVENTA(codClientex IN VARCHAR2,codEmpresax IN VARCHAR2,codListax IN VARCHAR2,limiteCreditox OUT NUMBER,deudaActx OUT NUMBER,tipoClientex OUT VARCHAR2,
                                      antiguedadx OUT VARCHAR2,cumpleañosx OUT VARCHAR2,velocidadPagox OUT NUMBER,countPedidosx OUT NUMBER,listaPreciox OUT VARCHAR2)
        IS
        montocobradox NUMBER;
    BEGIN

        SELECT LIMITE INTO limiteCreditox
        FROM (SELECT LIMITE FROM
            (
                SELECT
                    NVL(LIMIT_CREDI, 0) AS LIMITE
                FROM
                    XRAYADMIN.SGC_CLIENTE_LINEA_CREDITO
                WHERE
                        CODIG_EMPRE = codEmpresax
                  AND CODIG_CLIEN =codClientex
                  AND VIGEN = TO_CHAR(SYSDATE, 'MM-YYYY')
                  AND ESTAD = 'A'
                UNION SELECT 0 AS LIMITE FROM DUAL
            )
              ORDER BY LIMITE DESC
             )WHERE ROWNUM < 2;

        SELECT NVL(SUM(MONTO_COBRADO),0) into montocobradox
        FROM SYSADM.SGC_COBRANZA
        WHERE CODEMPRESA=codEmpresax AND CODCLIENTE=codClientex AND ESTADO_COBRANZA IN ('I','D');


        SELECT NVL(SUM(AA.MDCC_SALDO),0)-montocobradox INTO deudaActx
        FROM
            (
                SELECT  DISTINCT CXC.CDCC_SECUENCIA,CXC.mdcc_saldo
                FROM SYSADM.DOCUMENTO_CXC CXC
                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                WHERE CXC.CDCC_COMPANIA=codEmpresax
                  AND CXC.CDCC_CLIENTE=codClientex
                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                  AND CXC.MDCC_MONTO>0
            )AA ;

        /*SELECT  DISTINCT (CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0))DEUDAVENCIDA INTO deudaVencidax
								FROM SYSADM.DOCUMENTO_CXC CXC
								INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
								INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
								  LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
										FROM SYSADM.SGC_COBRANZA
										GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
								WHERE CXC.CDCC_COMPANIA=codEmpresax
								 AND CXC.CDCC_CLIENTE=codClientex
								AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
								and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
								AND CXC.MDCC_MONTO>0
								and (SYSDATE-CXC.FDCC_EMISION)>LPC.DIAS_PLAZO;


SELECT  DISTINCT (CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0))NOVENCIDA into deudaNoVencidax
								FROM SYSADM.DOCUMENTO_CXC CXC
								INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
								INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
								  LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
										FROM SYSADM.SGC_COBRANZA
										GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
								WHERE CXC.CDCC_COMPANIA=codEmpresax
								 AND CXC.CDCC_CLIENTE=codClientex
								AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
								and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
								AND CXC.MDCC_MONTO>0
							and (SYSDATE-CXC.FDCC_EMISION)<=LPC.DIAS_PLAZO;*/









        SELECT (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='006' AND LLAVE=CL.TIPO_CLIENTE) INTO  tipoClientex
        FROM SYSADM.CLIENTE CL
        WHERE  COD_CLIENTE=codClientex;

        SELECT  D1.AÑOS||' A-' ||D1.MESES||' M ' into antiguedadx --|| TRUNC((D1.DIAS2-D1.MESES)*24)||' D'
        FROM
            (
                SELECT D.AÑOS,TRUNC(((D.DIAS-D.AÑOS)*360/30)) MESES,((D.DIAS-D.AÑOS)*360/30)DIAS2
                FROM
                    (
                        select FECHA_CREACION,(TRUNC((trunc(sysdate) - FECHA_CREACION)/360)) AÑOS,((trunc(sysdate) - FECHA_CREACION)/360) DIAS
                        from SYSADM.CLIENTE WHERE COD_CLIENTE=codClientex
                    )D
            )D1;

        SELECT FECHA_CUMPLEANOS  INTO cumpleañosx
        FROM SYSADM.CLIENTE WHERE COD_CLIENTE=codClientex;


        SELECT DISTINCT TRUNC(NVL(SUM(CASE WHEN((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO)<0 then 0 else ((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO) END)/COUNT(CXC.CDCC_SECUENCIA),0)) INTO velocidadPagox
        FROM SYSADM.DOCUMENTO_CXC CXC
                 INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND ON CXC.CDCC_CONDVENTA=COND.LLAVE
                 INNER JOIN SYSADM.SPEDIDO_HEADER SPH ON CXC.CDCC_COMPANIA=SPH.COMPANIA_VENTA  AND  CXC.NDCC_PEDIDOOIH=SPH.NRO_PEDIDO
                 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION AND SPH.COD_LISTA_PRECIOS=LPC.LISTAPRECIO AND LPC.LOCALIDAD=SPH.DPTO_ORIGEN
        WHERE 1=1 AND SPH.COD_CIA='00' AND LPC.ESTAD='A'
          AND CXC.CDCC_CLIENTE=codClientex
          AND SDCC_STATUS='CA' AND COND.FLAG10=1
          AND MDCC_MONTO>0  ;



        SELECT COUNT(DISTINCT NRO_PEDIDO) INTO countPedidosx
        FROM SYSADM.SPEDIDO_HEADER WHERE COD_CLIENTE=codClientex AND STATUS_PEDIDO='G';


        SELECT DISTINCT  LP.COD_LISTA||'-'||LP.DESCRIPCION into listaPreciox
        FROM SYSADM.PRECIOS_C LP
                 INNER JOIN SYSADM.LP_CONDICION LP_CON ON LP_CON.LISTAPRECIO = LP.COD_LISTA
                 INNER JOIN SYSADM.TABLAS EM ON EM.LLAVE = LP.COMPANIA_VENTA
                 INNER JOIN SYSADM.TB_SEDE SE ON SE.CODSEDE = LP_CON.CODIG_SEDE
        WHERE LP.ESTAD_SGC = 'A' AND EM.CATEGORIA = '001'
          AND LP.COMPANIA_VENTA =codEmpresax
          AND LP.COD_LISTA =codListax;




    END DATOS_CLIENTE_PREVENTA;



    PROCEDURE  DATOS_VENDEDOR(usuariox IN VARCHAR2,operacionx IN NUMBER,fechaServerx OUT VARCHAR2,periodox OUT VARCHAR2,
                              pedidosEfectivosx OUT NUMBER,pedidosRechazadosx  OUT NUMBER,visitasnumx OUT NUMBER,visitassolesx OUT NUMBER, ticketdiariox OUT NUMBER,ticketmensualx OUT NUMBER,cursoravancex OUT types.ref_cursor)
        IS
        codempresax   VARCHAR2(2);
        codsedex  VARCHAR2(4);
        codmesax   VARCHAR2(4);
        codcanalx   VARCHAR2(4);
        codvendedorx VARCHAR2(4);
        anniox VARCHAR2(6);
        fechainix VARCHAR2(200);
        fechafinx VARCHAR2(200);
        fechax DATE;
        cantx number;
    BEGIN

        fechax:=SYSDATE;
        SELECT  DISTINCT CODEMPRESA,CODVENDEDOR,CODSEDE,CODMESA,CODCANAL INTO codempresax,codvendedorx,codsedex,codmesax,codcanalx
        FROM SYSADM.TB_USUARIO_MESA WHERE USUARIO=usuariox
                                      AND CODMESA<>'0185' AND TB_TIPO_CODIGO='R';


        SELECT count(*) INTO cantx
        FROM SYSADM.TIEMPO T INNER JOIN (SELECT TO_CHAR(SYSDATE,'YYYY/MM/DD')FECHA
                                         FROM SYSADM.DUAL)F ON T.FECHA=F.FECHA;

        if cantx<>0 THEN

            SELECT T.ANIO,T.PERIODO,to_Char(T.CODFECHA,'dd-mm-yyyy') INTO anniox,periodox,fechaServerx
            FROM SYSADM.TIEMPO T INNER JOIN (SELECT TO_CHAR(fechax,'YYYY/MM/DD')FECHA
                                             FROM SYSADM.DUAL)F ON T.FECHA=F.FECHA;

            SELECT TO_CHAR(MIN(T.CODFECHA),'DD-MM-YYYY') FMIN ,TO_CHAR(MAX(T.CODFECHA),'DD-MM-YYYY') FMAX INTO fechainix,fechafinx
            FROM SYSADM.TIEMPO T WHERE t.ANIO=anniox AND T.PERIODO=periodox;

        ELSE

            SELECT to_Char(SYSDATE,'dd-mm-yyyy'),'PERIODO' INTO fechaServerx,periodox
            FROM SYSADM.DUAL;
        end if;



        SELECT 0 INTO ticketdiariox FROM DUAL;
/*SELECT  NVL(ROUND(SUM(PH.MONTO_DESP_BONIF)/COUNT(PH.NRO_PEDIDO),2),0)  TICKETDIARIO INTO ticketdiariox
FROM SYSADM.SPEDIDO_HEADER PH
WHERE PH.COMPANIA_VENTA=codempresax
AND PH.COD_VENDEDOR=codvendedorx
AND TO_CHAR(PH.FECHA_PEDIDO,'DD-MM-YYYY')=TO_CHAR(fechax,'DD-MM-YYYY')--
AND PH.STATUS_PEDIDO='G';*/



        SELECT 0 INTO ticketmensualx FROM DUAL;
        /*SELECT NVL(ROUND(SUM(PH.MONTO_DESP_BONIF)/COUNT(PH.NRO_PEDIDO),2),0)  TICKETMENSUAL into ticketmensualx
FROM SYSADM.SPEDIDO_HEADER PH
WHERE PH.COMPANIA_VENTA=codempresax
AND PH.COD_VENDEDOR=codvendedorx
AND TO_CHAR(FECHA_PEDIDO,'MM-YYYY')=TO_CHAR(fechax,'MM-YYYY')
AND PH.STATUS_PEDIDO='G';*/

--cursoravancex:=SGCSQL_AVANCE_SUELDOS(codempresax,codsedex,codmesax,codcanalx,anniox,periodox,fechainix,fechafinx,codvendedorx,'1',NULL,operacionx);


        OPEN cursoravancex FOR
            SELECT   DISTINCT CODVENDEDOR,NOMBRE
            FROM SYSADM.TB_USUARIO_MESA UM INNER JOIN SYSADM.USUARIOS U ON UM.USUARIO=U.USUARIO
            WHERE UM.USUARIO=usuariox
              AND UM.CODMESA<>'0185' AND UM.TB_TIPO_CODIGO='R' AND ROWNUM<2;

        SELECT COUNT(DISTINCT NRO_PEDIDO) INTO visitasnumx
        FROM SYSADM.SPEDIDO_HEADER WHERE  STATUS_PEDIDO='G';

/*
SELECT DISTINCT PH.RUTA_DESPACHO,COUNT(PH.NRO_PEDIDO) PEDIDOSAPROB,(
SELECT DISTINCT COUNT(DISTINCT COD_CLIENTE)
FROM SYSADM.SPEDIDO_HEADER
WHERE COMPANIA_VENTA=codempresax
AND COD_VENDEDOR=codvendedorx
AND TO_CHAR(FECHA_PEDIDO,'MM-YYYY')='04-2019'
AND STATUS_PEDIDO='G' AND RUTA_DESPACHO=PH.RUTA_DESPACHO
GROUP BY RUTA_DESPACHO
) CLIENTESVISITA,
(SELECT COUNT(DISTINCT CL.COD_CLIENTE) FROM
 SYSADM.CLIENTE CL INNER JOIN SYSADM.LOCALES_CLIENTE LC ON CL.COD_CLIENTE=LC.COD_CLIENTE
 WHERE CL.STATUS_CLIENTE='A' AND LC.RUTA_DESPACHO=PH.RUTA_DESPACHO) CLIENTESRUTA
FROM SYSADM.SPEDIDO_HEADER PH
WHERE PH.COMPANIA_VENTA=codempresax
AND PH.COD_VENDEDOR=codvendedorx
AND TO_CHAR(FECHA_PEDIDO,'MM-YYYY')='04-2019'--TO_CHAR('03-2019','MM-YYYY')
AND PH.STATUS_PEDIDO='G'
GROUP BY RUTA_DESPACHO
ORDER BY 1 ASC;*/



    END DATOS_VENDEDOR;



    PROCEDURE DATOS_VENDEDOR_SUELDO(usuariox IN VARCHAR2,fechaServerx OUT VARCHAR2,periodox OUT VARCHAR2,
                                    pedidosEfectivosx OUT NUMBER,pedidosRechazadosx  OUT NUMBER,visitasnumx OUT NUMBER,visitassolesx OUT NUMBER, ticketdiariox OUT NUMBER,ticketmensualx OUT NUMBER,cursoravancex OUT types.ref_cursor,cabecerax OUT types.ref_cursor,politicasx OUT types.ref_cursor,adicionalx OUT types.ref_cursor,sueldovendedorx OUT types.ref_cursor,cadenarutasx OUT VARCHAR2,permiteofflinex OUT NUMBER)
        IS
        codempresax   VARCHAR2(2);
        codsedex  VARCHAR2(4);
        codmesax   VARCHAR2(4);
        codcanalx   VARCHAR2(4);
        codvendedorx VARCHAR2(4);
        anniox VARCHAR2(6);
        fechainix VARCHAR2(200);
        fechafinx VARCHAR2(200);
        fechax DATE;
        cantx number;



        idmesasueldox NUMBER;
        idperiodosueldox NUMBER;
--periodox VARCHAR2(100);
--fechaServerx VARCHAR2(200);
--fechainix VARCHAR2(200);
--fechafinx VARCHAR2(200);
        rutasx  types.ref_cursor;
        codrutax  VARCHAR2(10);
        descrutax VARCHAR2(100);
        cantrutasx NUMBER;

        countidperiodosueldox number;
        bandsincoberturax number;
        bandzonapeligrosax number;
    BEGIN

        fechax:=SYSDATE;
        SELECT  DISTINCT CODEMPRESA,CODVENDEDOR,CODSEDE,CODMESA,CODCANAL INTO codempresax,codvendedorx,codsedex,codmesax,codcanalx
        FROM SYSADM.TB_USUARIO_MESA WHERE USUARIO=usuariox
                                      AND CODMESA<>'0185' AND TB_TIPO_CODIGO='R';


        SELECT count(*) INTO cantx
        FROM SYSADM.TIEMPO T INNER JOIN (SELECT TO_CHAR(fechax,'YYYY/MM/DD')FECHA
                                         FROM SYSADM.DUAL)F ON T.FECHA=F.FECHA;

        if cantx<>0 THEN

            SELECT T.ANIO,T.PERIODO,to_Char(T.CODFECHA,'dd-mm-yyyy') INTO anniox,periodox,fechaServerx
            FROM SYSADM.TIEMPO T INNER JOIN (SELECT TO_CHAR(fechax,'YYYY/MM/DD')FECHA
                                             FROM SYSADM.DUAL)F ON T.FECHA=F.FECHA;



            SELECT TO_CHAR(MIN(T.CODFECHA),'DD-MM-YYYY') FMIN ,TO_CHAR(MAX(T.CODFECHA),'DD-MM-YYYY') FMAX INTO fechainix,fechafinx
            FROM SYSADM.TIEMPO T WHERE t.ANIO=anniox AND T.PERIODO=periodox;

        ELSE

            SELECT to_Char(fechax,'dd-mm-yyyy'),'PERIODO' INTO fechaServerx,periodox
            FROM SYSADM.DUAL;
        end if;


        SELECT 0 INTO ticketdiariox FROM SYSADM.DUAL;
/*SELECT  NVL(ROUND(SUM(PH.MONTO_DESP_BONIF)/COUNT(PH.NRO_PEDIDO),2),0)  TICKETDIARIO INTO ticketdiariox
FROM SYSADM.SPEDIDO_HEADER PH
WHERE PH.COMPANIA_VENTA=codempresax
AND PH.COD_VENDEDOR=codvendedorx
AND TO_CHAR(PH.FECHA_PEDIDO,'DD-MM-YYYY')=TO_CHAR(fechax,'DD-MM-YYYY')--
AND PH.STATUS_PEDIDO='G';*/


        SELECT 0 INTO ticketmensualx FROM SYSADM.DUAL;

/*
SELECT NVL(ROUND(SUM(PH.MONTO_DESP_BONIF)/COUNT(PH.NRO_PEDIDO),2),0)  TICKETMENSUAL into ticketmensualx
FROM SYSADM.SPEDIDO_HEADER PH
WHERE PH.COMPANIA_VENTA=codempresax
AND PH.COD_VENDEDOR=codvendedorx
AND TO_CHAR(FECHA_PEDIDO,'MM-YYYY')=TO_CHAR(fechax,'MM-YYYY')
AND PH.STATUS_PEDIDO='G';*/


        OPEN cursoravancex FOR
            SELECT   DISTINCT CODVENDEDOR,NOMBRE
            FROM SYSADM.TB_USUARIO_MESA UM INNER JOIN SYSADM.USUARIOS U ON UM.USUARIO=U.USUARIO
            WHERE UM.USUARIO=usuariox
              AND UM.CODMESA<>'0185' AND UM.TB_TIPO_CODIGO='R' AND ROWNUM<2;

        SELECT COUNT(DISTINCT NRO_PEDIDO) INTO visitasnumx
        FROM SYSADM.SPEDIDO_HEADER WHERE  STATUS_PEDIDO='G';




------------------


        SELECT IDMESASUELDO into idmesasueldox--,SGC_MESA_SUELDO.CODEMPRESA,EMPRESA.NOMEMPRESA,TB_SEDE.CODSEDE,DESCSEDE,SGC_MESA_SUELDO.CODMESA,DESCMESA,
        --TB_CANAL.CODCANAL,DESCCANAL
        FROM SYSADM.SGC_MESA_SUELDO
                 INNER JOIN (SELECT LLAVE,DESC1 AS NOMEMPRESA FROM SYSADM.TABLAS WHERE CATEGORIA = '001') EMPRESA
                            ON EMPRESA.LLAVE = SGC_MESA_SUELDO.CODEMPRESA
                 INNER JOIN SYSADM.TB_SEDE ON TB_SEDE.CODSEDE = SGC_MESA_SUELDO.CODSEDE
                 INNER JOIN SYSADM.TB_MESA ON TB_MESA.CODLOCALIDAD = SGC_MESA_SUELDO.CODMESA
                 INNER JOIN SYSADM.TB_CANAL ON TB_CANAL.CODCANAL = SGC_MESA_SUELDO.CODCANAL
        WHERE 1=1  AND SGC_MESA_SUELDO.CODEMPRESA = codempresax
          AND SGC_MESA_SUELDO.CODSEDE = codsedex
          AND SGC_MESA_SUELDO.CODMESA = codmesax
          AND SGC_MESA_SUELDO.CODCANAL = codcanalx ;

        SELECT T.ANIO,T.PERIODO,to_Char(T.CODFECHA,'dd-mm-yyyy')INTO anniox,periodox,fechaServerx
        FROM SYSADM.TIEMPO T INNER JOIN (SELECT TO_CHAR(fechax,'YYYY/MM/DD')FECHA
                                         FROM SYSADM.DUAL)F ON T.FECHA=F.FECHA;



        SELECT TO_CHAR(MIN(T.CODFECHA),'DD-MM-YYYY') FMIN ,TO_CHAR(MAX(T.CODFECHA),'DD-MM-YYYY') FMAX INTO fechainix,fechafinx
        FROM SYSADM.TIEMPO T WHERE t.ANIO=anniox AND T.PERIODO=periodox;



        SELECT count(IDPERIODOSUELDO) INTO countidperiodosueldox--,CODPERIODO,CODANIO,ESTPERIODOSUELDO
        FROM SYSADM.SGC_PERIODO_SUELDO
        WHERE 1=1  AND CODANIO =anniox AND CODPERIODO = periodox;


        IF  countidperiodosueldox>0 THEN
            SELECT IDPERIODOSUELDO INTO idperiodosueldox--,CODPERIODO,CODANIO,ESTPERIODOSUELDO
            FROM SYSADM.SGC_PERIODO_SUELDO
            WHERE 1=1  AND CODANIO =anniox AND CODPERIODO = periodox;
        ELSE

            idperiodosueldox:='0';
        END IF;

--cabecerax:=SGCSQL_CABECERA_MESASUELDO(null,idmesasueldox,1);
        OPEN cabecerax FOR
            SELECT IDMESASUELDOCAB,SGC_MESA_CABSUELDO.IDMESASUELDO,SGC_MESA_CABSUELDO.IDCABECERASUELDO,
                   DESCRIPCIONCAB,VALORCAB,TIPOCAB,ESTADOCAB,NROORDEN
            FROM SYSADM.SGC_MESA_CABSUELDO
                     INNER JOIN SYSADM.SGC_CABECERA_SUELDO ON SGC_CABECERA_SUELDO.IDCABECERASUELDO = SGC_MESA_CABSUELDO.IDCABECERASUELDO
            WHERE 1=1  AND SGC_MESA_CABSUELDO.IDMESASUELDO =idmesasueldox  order by 1 asc ;



--politicasx:=SGCSQL_POLITICA_ASIGNADA(null,idmesasueldox,idperiodosueldox,null,null,null,null,null,null,null,null,null,1);
        OPEN politicasx FOR
            SELECT IDPOLITICAASIGNADA,CODPOLITICA,NOMPOLITICA,DESCPOLITICA
            FROM SYSADM.SGC_POLITICA_ASIGNADA
            WHERE 1=1 --- AND CODPOLITICA<>'06'
              AND IDMESASUELDO =idmesasueldox  AND IDPERIODOSUELDO = idperiodosueldox  order by 2 desc ;


        adicionalx:=SGCSQL_PERIODO_ADICIONAL(null,idperiodosueldox,1);
        dbms_output.put_line(anniox||periodox);
        sueldovendedorx:=SGC_V4.SGCSQL_AVANCE_SUELDOS(codempresax,codsedex,codmesax,codcanalx,anniox,periodox,fechainix,fechafinx,codvendedorx,'5 desc',NULL,1);




        SELECT DISTINCT  COUNT(*) into cantrutasx
        FROM   SYSADM.TB_RUTA R
                   INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                   INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
        WHERE 1=1
          AND RV.DIA=(select translate(SUBSTR(to_char(SYSDATE, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                      from dual)
          AND UM.USUARIO=usuariox;

        permiteofflinex:=0;
        IF cantrutasx>0 THEN

            open rutasx FOR
                SELECT DISTINCT  R.CODRUTA,R.DESCRUTA,NVL(SIN_COBERTURA,0) SIN_COBERTURA,NVL(ZONA_PELIGROSA,0) ZONA_PELIGROSA
                FROM   SYSADM.TB_RUTA R
                           INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                           INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                WHERE 1=1
                  AND RV.DIA=(select translate(SUBSTR(to_char(SYSDATE, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                              from dual)
                  AND UM.USUARIO=usuariox;
            cadenarutasx:='RUTAS DE HOY:'||chr(10);
            LOOP
                FETCH rutasx INTO codrutax,descrutax,bandsincoberturax,bandzonapeligrosax;--,estadox;
                EXIT WHEN rutasx%NOTFOUND;
                IF (bandsincoberturax=1) or (bandzonapeligrosax=1) then
                    permiteofflinex:=permiteofflinex+1;
                end if;
                cadenarutasx:=cadenarutasx||codrutax||'-'||descrutax||chr(10);

            END LOOP;
            CLOSE rutasx;
        ELSE
            cadenarutasx:='NO TIENE RUTAS ASIGNADAS HOY, COMUNIQUESE CON SU SUPERVISOR ';

        END IF;

        if permiteofflinex>0 THEN
            permiteofflinex:=1;
        end if;



    END DATOS_VENDEDOR_SUELDO;


    FUNCTION LISTAR_LP(usuariox IN VARCHAR2)

        return types.ref_cursor is
        filas types.ref_cursor;
        codempx VARCHAR2(10);
        codsedex VARCHAR2(10);
    BEGIN

        SELECT distinct CODEMPRESA,CODSEDE into codempx,codsedex
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usuariox;

        OPEN filas for
            SELECT DISTINCT LP.COD_LISTA, LP.COD_LISTA||'-'||LP.DESCRIPCION
            FROM SYSADM.PRECIOS_C LP
                     INNER JOIN SYSADM.LP_CONDICION LP_CON ON LP_CON.LISTAPRECIO = LP.COD_LISTA
                     INNER JOIN SYSADM.TABLAS EM ON EM.LLAVE = LP.COMPANIA_VENTA
                     INNER JOIN SYSADM.TB_SEDE SE ON SE.CODSEDE = LP_CON.CODIG_SEDE
            WHERE LP.ESTAD_SGC = 'A' AND EM.CATEGORIA = '001'  AND LP.COMPANIA_VENTA =codempx  AND LP_CON.CODIG_SEDE =codsedex
            ORDER BY LP.COD_LISTA;
        return filas;

    END LISTAR_LP;


/*PROCEDURE  DATOS_CLIENTE_UPDATE(codClientex IN VARCHAR2,usuariox in varchar2,cdatosclientex OUT types.ref_cursor,clocalesclientex OUT types.ref_cursor)
IS
codempx VARCHAR2(10);
codsedex VARCHAR2(10);
BEGIN


SELECT CODEMPRESA,CODSEDE into codempx,codsedex
 FROM SYSADM.TB_USUARIO_MESA
WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R' AND ESTADO_EN_MESA='A';


OPEN cdatosclientex FOR

SELECT CL.DESC_CLIENTE,CL.DIRECCION_CLIENTE,CL.CIUDAD,CL.TELEFONO,CL.FAX,CASE WHEN CL.SEXO='V' THEN 'M' ELSE 'F' END SEXO,to_char(CL.FECHA_CUMPLEANOS,'YYYY-MM-DD'),CL.E_MAIL,CL.TIPO_CLIENTE,CL.GIRO_NEGOCIO,
TBDPTO.LLAVE,TBDPTO.DESC1,TBPROV.LLAVE,TBPROV.DESC1,TBDIST.LLAVE,TBDIST.DESC1,CLL.COD_LISTA,STATUS_CLIENTE,COMENTARIO_2
FROM SYSADM.CLIENTE CL
INNER JOIN SYSADM.TABLAS TBDPTO ON TBDPTO.CATEGORIA='090' AND TBDPTO.LLAVE=SUBSTR(CIUDAD, 0, 2)
INNER JOIN SYSADM.TABLAS TBPROV ON TBPROV.CATEGORIA='132' AND TBPROV.LLAVE=SUBSTR(CIUDAD, 0, 4)
INNER JOIN SYSADM.TABLAS TBDIST ON TBDIST.CATEGORIA='035' AND TBDIST.LLAVE=CIUDAD
INNER JOIN SYSADM.CLIENTE_LISTAS CLL ON CLL.COD_CLIENTE=CL.COD_CLIENTE AND CLL.TIPO_LISTA='01' AND CLL.COMPANIA_VENTA =codempx
WHERE CL.COD_CLIENTE=codClientex;
OPEN clocalesclientex FOR
SELECT LC.COD_LOCAL,LC.DIRECCION,LC.RUTA_DESPACHO ,R.DESCRUTA,REF_DESPACHO,LC.ESTAD,LC.OBSERVACIONES
FROM SYSADM.LOCALES_CLIENTE lc
INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
--INNER JOIN SYSADM.TB_SEDE S ON S.IDSEDE=R.IDSEDE
where LC.COD_CLIENTE=codClientex;-- AND S.CODSEDE='008';


END DATOS_CLIENTE_UPDATE;*/
    PROCEDURE  DATOS_CLIENTE_UPDATE(usuariox IN VARCHAR2,codClientex IN VARCHAR2,cdatosclientex OUT types.ref_cursor,clocalesclientex OUT types.ref_cursor,cgirosx OUT types.ref_cursor,ctiposx OUT types.ref_cursor,formatosx OUT types.ref_cursor,observacionesx   OUT types.ref_cursor)
        IS
    BEGIN

        OPEN cdatosclientex FOR

            SELECT CL.DESC_CLIENTE,CL.DIRECCION_CLIENTE,CL.CIUDAD,CL.TELEFONO,CL.FAX,CASE WHEN CL.SEXO='V' THEN 'M' ELSE 'F' END SEXO,to_char(CL.FECHA_CUMPLEANOS,'YYYY-MM-DD'),CL.E_MAIL,TIPO.TIPOCLIENTE,CL.GIRO_NEGOCIO,
                   TBDPTO.LLAVE,TBDPTO.DESC1,TBPROV.LLAVE,TBPROV.DESC1,TBDIST.LLAVE,TBDIST.DESC1,CLL.COD_LISTA,STATUS_CLIENTE,COMENTARIO_2,COD_OBSERVACION,(SELECT SUM(MDCC_SALDO) FROM SYSADM.DOCUMENTO_CXC WHERE CDCC_CLIENTE=CL.COD_CLIENTE) DEUDA,
                   (SELECT to_char(MAX(FDCC_EMISION),'yyyy-mm-dd') FROM SYSADM.DOCUMENTO_CXC WHERE CDCC_CLIENTE=CL.COD_CLIENTE AND SDCC_STATUS<>'AN') FECHAFACT,FORMATO_DIRECCION,NEW_DIRECCION
            FROM SYSADM.CLIENTE CL
                     INNER JOIN SYSADM.TABLAS TBDPTO ON TBDPTO.CATEGORIA='090' AND TBDPTO.LLAVE=SUBSTR(CIUDAD, 0, 2)
                     INNER JOIN SYSADM.TABLAS TBPROV ON TBPROV.CATEGORIA='132' AND TBPROV.LLAVE=SUBSTR(CIUDAD, 0, 4)
                     INNER JOIN SYSADM.TABLAS TBDIST ON TBDIST.CATEGORIA='035' AND TBDIST.LLAVE=CIUDAD
                     INNER JOIN SYSADM.CLIENTE_LISTAS CLL ON CLL.COD_CLIENTE=CL.COD_CLIENTE AND CLL.TIPO_LISTA='01' AND CLL.COMPANIA_VENTA IN ('01','05','08')
                     LEFT JOIN (select LLAVE GIRONEGOCIO,upper(DESC1),ESTADO from sysadm.TABLAS where categoria = '329'  AND NUM4=1)GIRO ON GIRO.GIRONEGOCIO=CL.GIRO_NEGOCIO
                     LEFT JOIN (select LLAVE TIPOCLIENTE,upper(DESC1),ESTADO,NUM4 from sysadm.TABLAS where categoria = '006' AND ESTADO = 'H' AND NUM4 IS NOT NULL)TIPO ON TIPO.TIPOCLIENTE=CL.TIPO_CLIENTE AND TIPO.NUM4=GIRO.GIRONEGOCIO

            WHERE CL.COD_CLIENTE=codClientex;

        OPEN clocalesclientex FOR
            SELECT LC.COD_LOCAL,LC.DIRECCION,LC.RUTA_DESPACHO ,R.DESCRUTA,REF_DESPACHO,LC.ESTAD,LC.OBSERVACIONES,LC.COD_OBSERVACION,LC.FORMATO_DIRECCION,LC.NEW_DIRECCION
            FROM SYSADM.LOCALES_CLIENTE lc
                     INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
--INNER JOIN SYSADM.TB_SEDE S ON S.IDSEDE=R.IDSEDE
            where LC.COD_CLIENTE=codClientex ;-- AND S.CODSEDE='008';



        open cgirosx FOR

            select LLAVE,upper(DESC1),ESTADO from sysadm.TABLAS where categoria = '329'  AND NUM4=1;

        OPEN ctiposx FOR

            select LLAVE,upper(DESC1),ESTADO from sysadm.TABLAS where categoria = '006' AND ESTADO = 'H' ;



        OPEN observacionesx FOR
            select
                CODIGO,DESCRIPCION
            from sysadm.OBS_AUD_CLIENTE
            where 1=1 AND STATUS = 'A' ;


        open formatosx for
            SELECT codigo,abrev,orden FROM SYSADM.FORM_DIR_CLIENTE;





    END DATOS_CLIENTE_UPDATE;





    PROCEDURE MANTCLIENTE(CLIENTE_OBJ IN CLIENTE_MOVIL_AUDITV2,localesx IN LOCAL_CLIEN_MOVIL_TABLE_UPDATE,operacionx IN NUMBER,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
        IS
        tipopersonax VARCHAR2(10);
        codempx VARCHAR2(10);
        countClientex NUMBER;
        statusx VARCHAR2(10);
        cantalmacenx NUMBER;
        codalmacenx VARCHAR2(10);
        sexox VARCHAR2(10);


        countjornadax number;
        codclientex VARCHAR2(20);
        descclientex VARCHAR2(200);
        direccionx VARCHAR2(400);
        coordenadaoldx VARCHAR2(400);
        codrutax VARCHAR2(20);
        descrutax VARCHAR2(200);
        codlocalx VARCHAR2(2);
        codlistax VARCHAR2(10);
        codempresax VARCHAR2(2);
        estadox VARCHAR2(2);
        diajornadax DATE;


--codempx VARCHAR2(10);
        codsedex VARCHAR2(10);
        codperfilx VARCHAR2(4);
        cantcreadosx NUMBER;
        auxtempx number;


        limitecantx number;
        canthuancx number;
        cantrutaaudix number;
        auxdescx number;
        excliente EXCEPTION;
        exsinusuario EXCEPTION;

        ubigeorutax VARCHAR2(10);
        distritox VARCHAR2(100);
        provinciax VARCHAR2(100);
        departamentox VARCHAR2(100);
        direccionfinalx VARCHAR2(900);
        cursorfindirx TYPEs.ref_cursor;
        cadenanewdirx VARCHAR2(900);
        tempodirx VARCHAR2(900);

        direccioncompletax VARCHAR2(900);
    BEGIN

        IF operacionx=1 then


            SELECT COUNT(*) INTO cantcreadosx
            FROM SYSADM.CLIENTE
            WHERE USUAR_REGIS=CLIENTE_OBJ.usuarioregistra
              AND TO_CHAR(FECHA_CREACION,'DD-MM-YYYY')=TO_CHAR(SYSDATE,'DD-MM-YYYY');


            SELECT COUNT(*)  INTO canthuancx -- huancayo podra crear 5 ckientes maximo
            FROM SYSADM.TB_USUARIO_MESA
            WHERE CODEMPRESA='01' AND CODSEDE='011'
              AND USUARIO=CLIENTE_OBJ.usuarioregistra;

            if canthuancx>0 THEN
                limitecantx:=10;
            else
                limitecantx:=10;--COLOQUE 10 TENGO Q REGRESAR A 2
            end if;

            /*SELECT CASE WHEN LENGTH(TRIM(localesx(1).desclocal)) IS NULL THEN 1 ELSE 0 END  into auxdescx
 FROM
  DUAL;


IF  auxdescx=1   THEN

 RAISE excliente;

END IF;*/



--if "LENGTH"(localesx(1).direccion)<>8   then

            SELECT COUNT(CODRUTA) into cantrutaaudix
            FROM SYSADM.TB_RUTA WHERE CODRUTA=localesx(1).codruta and CERRADA_BY_AUDIT=1;



            if cantrutaaudix=0 then


                IF cantcreadosx<limitecantx THEN

                    SELECT distinct CODEMPRESA,CODSEDE  into codempresax,codsedex
                    FROM SYSADM.TB_USUARIO_MESA
                    WHERE USUARIO=CLIENTE_OBJ.usuarioregistra AND TB_TIPO_CODIGO='R';










                    SELECT COUNT(COD_CLIENTE)INTO countClientex
                    FROM SYSADM.CLIENTE
                    WHERE COD_CLIENTE = CLIENTE_OBJ.codcliente ;

                    IF CLIENTE_OBJ.sexo='M'  THEN
                        sexox:='V';
                    ELSE
                        sexox:='M';
                    END IF;


                    IF countClientex=0 then

                        IF CLIENTE_OBJ.ruc IS NOT NULL  THEN
                            tipopersonax:='J';
                            direccioncompletax:=CLIENTE_OBJ.direccion;
                        ELSIF  CLIENTE_OBJ.dni IS NOT NULL  THEN
                            tipopersonax:='N';

                            SELECT UBIGEO INTO ubigeorutax
                            FROM SYSADM.TB_RUTA WHERE CODRUTA=localesx(1).codruta;

                            SELECT DESC1  INTO distritox
                            FROM SYSADM.TABLAS WHERE CATEGORIA='035' AND  LLAVE=ubigeorutax;


                            SELECT DESC1 into provinciax
                            FROM SYSADM.TABLAS WHERE CATEGORIA='132' AND LLAVE=substr(ubigeorutax,1,4);

                            SELECT DESC1 into departamentox
                            FROM SYSADM.TABLAS WHERE CATEGORIA='090' AND LLAVE <> '00'			--NINGUNA
                                                 AND LLAVE <> '99' AND LLAVE=substr(ubigeorutax,1,2);



                            open cursorfindirx FOR
                                select

                                        CASE WHEN  (select ABREV from SYSADM.FORM_DIR_CLIENTE  where codigo=bb.valb) IS NOT NULL THEN
                                                 (select ABREV from SYSADM.FORM_DIR_CLIENTE  where codigo=bb.valb)
                                             ELSE
                                                 bb.valb
                                            END  ||' '||aa.vala
                                FROM
                                    (
                                        select  a.orden,a.vala
                                        from
                                            (
                                                select rownum as orden,regexp_substr(localesx(1).newdireccion,'[^_]+', 1, level) VALa
                                                from dual
                                                connect by regexp_substr(localesx(1).newdireccion, '[^_]+', 1, level) is not null
                                            )a)aa  inner join (
                                        select b.orden,b.valb
                                        FROM
                                            (
                                                select rownum as orden, regexp_substr(CLIENTE_OBJ.formatodir,'[^_]+', 1, level) VALb
                                                from dual
                                                connect by regexp_substr(CLIENTE_OBJ.formatodir, '[^_]+', 1, level) is not null
                                            )b)bb on aa.orden=bb.orden WHERE  aa.vala<>'XXXX';

                            cadenanewdirx:='';

--cadenarutasx:='RUTAS DE HOY:'||chr(10);
                            LOOP
                                FETCH cursorfindirx INTO tempodirx;--,estadox;
                                EXIT WHEN cursorfindirx%NOTFOUND;
                                cadenanewdirx:=cadenanewdirx||tempodirx||' ';

                            END LOOP;
                            CLOSE cursorfindirx;


                            direccioncompletax:=cadenanewdirx||'-'||distritox||'-'||provinciax||'-'||departamentox;




                        END IF ;
                        INSERT INTO SYSADM.CLIENTE(
                            COD_CIA, COD_CLIENTE, RUC,   DESC_CLIENTE,
                            DIRECCION_CLIENTE, CIUDAD,
                            STATUS_CLIENTE, MONEDA_DEFAULT, VENDEDOR_DEFAULT, CP_DEFAULT,
                            ZONA_VENTAS, TIPO_CLIENTE, LIMITE_CREDITO,
                            FECHA_CREACION,
                            DCTO_VOLUMEN_FLAG, BACKORDER_FLAG,
                            RIESGO, CANAL, GIRO_NEGOCIO,
                            LIB_ELECTORAL, FLAG_RET_PED, FLAG_NO_RET_CRED,
                            NOM_COMERCIAL, FLAG_PRO_ALQ, GLOSA_CLIENTE, E_MAIL, FECHA_CUMPLEANOS,
                            N_EXTRANJERO,
                            SEXO, USUAR_REGIS, TIPO_PERSO,SIST_CREACION,
                            FORMATO_DIRECCION,NEW_DIRECCION
                        )
                        VALUES(
                                  '00', CLIENTE_OBJ.codcliente, CLIENTE_OBJ.ruc,replace(CLIENTE_OBJ.desccliente,'&Ntilde;','Ñ'),
                                  direccioncompletax, CLIENTE_OBJ.codubigeo,
                                  'P', 'S/.', '998', '21',
                                  '000000', CLIENTE_OBJ.codtipocliente, 100,
                                  TO_DATE(TO_CHAR(SYSDATE, 'YYYY-MM-DD HH24:MI:SS'), 'YYYY-MM-DD HH24:MI:SS'),
                                  '0', '0',
                                  '04', CLIENTE_OBJ.codcanal,CLIENTE_OBJ.codgironegocio,
                                  CLIENTE_OBJ.dni, '1', '0',
                                  CLIENTE_OBJ.desccliente, '0', CLIENTE_OBJ.referencia, CLIENTE_OBJ.email, TO_DATE(CLIENTE_OBJ.fechanacimiento,'YYYY-MM-DD'),
                                  '0',
                                  sexox, CLIENTE_OBJ.usuarioregistra, tipopersonax,'MOVIL',
                                  CLIENTE_OBJ.formatodir,localesx(1).newdireccion
                              );


                        SELECT DISTINCT CODEMPRESA into codempx
                        FROM SYSADM.TB_USUARIO_MESA WHERE USUARIO=CLIENTE_OBJ.usuarioregistra;

                        INSERT INTO SYSADM.CLIENTE_LISTAS(COD_CIA, COD_CLIENTE, COMPANIA_VENTA, TIPO_LISTA, COD_LISTA, USUARIO,ROWVERSION)
                        VALUES ('00', CLIENTE_OBJ.codcliente, codempx, '01', CLIENTE_OBJ.codlistaprecios, CLIENTE_OBJ.usuarioregistra,0);


                        INSERT INTO SYSADM.CXC_CLIENTE_STD(
                            COD_CIA, COD_CLIENTE, CIA_VENTA, LIMITE_CREDITO,
                            CREDITO_DISPONIBLE, CREDITO_UTILIZADO, QTY_CHECK_DEVUEL,
                            NRO_DOC_PENDIENTES, CANT_VENCIDOS, CANT_LETRA_PROTEST,
                            QTY_CHQDEV_ACUM, CANT_LETPROT_ACUM, DIASDEMORA_CANCEL,
                            MONTO_LAST_COMP, MONTO_ACUMULADO,ROWVERSION
                        )
                        VALUES(
                                  '00', CLIENTE_OBJ.codcliente, codempx, 100,
                                  100, 0, 0,
                                  0, 0, 0,
                                  0, 0, 0,
                                  0, 0,0
                              );


                        INSERT INTO SYSADM.CLIENTE_VENDEDOR(
                            COD_CIA, COD_CLIENTE, CIA_VENTA, COD_VENDEDOR,ROWVERSION
                        )
                        VALUES(
                                  '00', CLIENTE_OBJ.codcliente, codempx, '998',0
                              );


                        FOR indx IN  localesx.FIRST .. localesx.LAST
                            LOOP
                                INSERT INTO SYSADM.LOCALES_CLIENTE(
                                    COD_CIA, COD_CLIENTE, COD_LOCAL, DESC_LOCAL, DIRECCION, UBIGEO,
                                    RUTA_DESPACHO, REF_DESPACHO, OBSERVACIONES, COD_USUARIO,
                                    B_EXONERA_IGV, ESTAD, FECGEOLOCALIZACION, USUAR_GEOLO,ROWVERSION,
                                    FORMATO_DIRECCION,NEW_DIRECCION
                                )--replace(direccioncompletax,'&Ntilde;','Ñ')
                                VALUES('00', CLIENTE_OBJ.codcliente,localesx(indx).codlocal,replace(localesx(indx).desclocal,'&Ntilde;','Ñ') , direccioncompletax, localesx(indx).ubigeo,
                                       localesx(indx).codruta, localesx(indx).coordenadas, CLIENTE_OBJ.referencia, CLIENTE_OBJ.usuarioregistra,
                                       (SELECT NVL(FLAG3, '0') FROM SYSADM.TABLAS
                                        WHERE CATEGORIA = '035' AND LLAVE =  localesx(indx).ubigeo), 'A', SYSDATE,  CLIENTE_OBJ.usuarioregistra,0,
                                       CLIENTE_OBJ.formatodir,localesx(indx).newdireccion
                                      );

                            END LOOP;

                        SELECT COUNT(*) INTO cantalmacenx FROM SYSADM.CLIENTE_ALMACEN
                        WHERE COD_CIA = '00' AND COD_CLIENTE = CLIENTE_OBJ.codcliente AND CIA_VENTA =codempx;
                        IF cantalmacenx = 0 THEN
                            SELECT CODALMACEN INTO codalmacenx FROM SYSADM.TB_ALMACEN WHERE CODEMPRESA = codempx AND ROWNUM < 2;
                            INSERT INTO SYSADM.CLIENTE_ALMACEN(COD_CIA,COD_CLIENTE,CIA_VENTA,COD_ALMACEN,ROWVERSION)
                            VALUES('00',CLIENTE_OBJ.codcliente ,codempx,codalmacenx,0);
                        END IF;

                        SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                        LC.NEW_COORDENADAS,
                                        LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA INTO codclientex,descclientex,direccionx,coordenadaoldx,codrutax,descrutax,codlocalx,codlistax,codempresax
                        FROM SYSADM.CLIENTE C
                                 INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                                 INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                                 INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                                 INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                                 INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
                        WHERE 1=1
                          AND C.COD_CLIENTE=CLIENTE_OBJ.codcliente
                          AND C.STATUS_CLIENTE='P'
                          AND LC.ESTAD='A'  ;

/*
INSERT INTO SYSADM.TBL_USUARIO_JORNADA(USUARIO,FECHA,CODCLIENTE,DESCCLIENTE,CODLOCAL,CODRUTA,DESCRUTA,DIRECCION,
COORDENADA_ANT,CODLISTA,CODEMPRESA,ISNEW,ESTADO,FECHA_TRANS)
VALUES(CLIENTE_OBJ.usuarioregistra,to_date(SUBSTR(sysdate,0,10)),codclientex,descclientex,codlocalx,codrutax,descrutax,direccionx,
coordenadaoldx,codlistax,codempresax,'1','P',SYSDATE);*/


                        rptax := 1;
                        mensajex :='SE REGISTRÓ CORRECTAMENTE ';
                        --commit;

                    ELSE
                        SELECT STATUS_CLIENTE INTO statusx
                        FROM SYSADM.CLIENTE
                        WHERE COD_CLIENTE = CLIENTE_OBJ.codcliente ;

                        IF statusx IN ('A','P') THEN
                            rptax := -2;
                            mensajex :='CLIENTE SE ENCUENTRA REGISTRADO Y ESTA ACTIVO ';
                        ELSE
                            rptax := -3;
                            mensajex :='CLIENTE SE ENCUENTRA  REGISTRADO PERO ESTA INACTIVO ';
                        end if;

                    end if ;

                ELSE

                    rptax := -1;
                    mensajex :='SOLO SE PUEDEN CREAR DOS CLIENTES POR DIA( ORDEN DE GERENCIA )';

                END IF;
            ELSE
                rptax := -1;
                mensajex :='RUTA RESTRINGIDA PARA ALTAS, POR AUDITORIA!!';

            end if;

            --ELSE
--rptax := -1;
--mensajex :='ERROR EN LA DIRECCION DEL CLIENTE!!';

--end if;



        elsif operacionx=2 then

            /*SELECT distinct TM.CODEMPRESA,TM.CODSEDE,US.CODPERFIL into codempresax,codsedex,codperfilx
			 FROM SYSADM.TB_USUARIO_MESA TM
			INNER JOIN SYSADM.USUARIOS US ON TM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
			WHERE TM.USUARIO=CLIENTE_OBJ.usuarioregistra;

		if codperfilx='05' THEN

			UPDATE SYSADM.CLIENTE_LISTAS
				 SET COD_LISTA=CLIENTE_OBJ.codlistaprecios
				WHERE COMPANIA_VENTA='01' AND COD_CLIENTE=CLIENTE_OBJ.codcliente
				AND TIPO_LISTA=codempresax;


			ELSE*/
            UPDATE SYSADM.CLIENTE
            SET FECHA_CUMPLEANOS=TO_DATE(CLIENTE_OBJ.fechanacimiento,'YYYY-MM-DD')
            WHERE COD_CLIENTE=CLIENTE_OBJ.codcliente;
            --end if;


            rptax:=1;
            mensajex :='SE ACTUALIZÓ CORRECTAMENTE ';
            commit;

        elsif operacionx=3 then

            /*	SELECT COUNT(COD_CLIENTE)INTO countClientex
							 FROM SYSADM.CLIENTE
							WHERE COD_CLIENTE = CLIENTE_OBJ.codcliente ;*/


            IF CLIENTE_OBJ.sexo='M'  THEN
                sexox:='V';
            ELSE
                sexox:='M';
            END IF;



            UPDATE SYSADM.CLIENTE
            SET FAX=CLIENTE_OBJ.telefonofijo,
                TELEFONO=CLIENTE_OBJ.celular,
                TIPO_CLIENTE=CLIENTE_OBJ.codtipocliente  ,
                GIRO_NEGOCIO=CLIENTE_OBJ.codgironegocio,
                SEXO=sexox,
                E_MAIL=CLIENTE_OBJ.email,
--FECHA_CUMPLEANOS=TO_DATE(CLIENTE_OBJ.fechanacimiento,'YYYY-MM-DD'),
--STATUS_CLIENTE=CLIENTE_OBJ.estadocliente,
--FECHA_MODIFICACION=SYSDATE,
                COMENTARIO_2=CLIENTE_OBJ.referencia,
                USUARIO_AUDI=CLIENTE_OBJ.usuarioregistra,
--DIRECCION_CLIENTE=CLIENTE_OBJ.direccion,
                COD_OBSERVACION=CLIENTE_OBJ.observaciones,
                FORMATO_DIRECCION=CLIENTE_OBJ.formatodir,
                NEW_DIRECCION=CLIENTE_OBJ.newdireccion
            WHERE COD_CLIENTE=CLIENTE_OBJ.codcliente;


            SELECT distinct TM.CODEMPRESA,TM.CODSEDE,US.CODPERFIL into codempresax,codsedex,codperfilx
            FROM SYSADM.TB_USUARIO_MESA TM
                     INNER JOIN SYSADM.USUARIOS US ON TM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
            WHERE TM.USUARIO=CLIENTE_OBJ.usuarioregistra AND TM.ESTADO_EN_MESA='A';

            if codperfilx IN ('05','03') THEN

                UPDATE SYSADM.CLIENTE_LISTAS
                SET COD_LISTA=CLIENTE_OBJ.codlistaprecios,USUARIO=CLIENTE_OBJ.usuarioregistra, FECHA_CAMBIO=sysdate
                WHERE COMPANIA_VENTA=codempresax
                  AND COD_CLIENTE=CLIENTE_OBJ.codcliente
                  AND TIPO_LISTA='01';


            ELSE
                UPDATE SYSADM.CLIENTE
                SET FECHA_CUMPLEANOS=TO_DATE(CLIENTE_OBJ.fechanacimiento,'YYYY-MM-DD')
                WHERE COD_CLIENTE=CLIENTE_OBJ.codcliente;
            end if;





            rptax:=1;
            mensajex :='SE ACTUALIZÓ CORRECTAMENTE ';
            commit;
        elsif operacionx=4 then

            IF CLIENTE_OBJ.sexo='M'  THEN
                sexox:='V';
            ELSE
                sexox:='M';
            END IF;



            UPDATE SYSADM.CLIENTE
            SET FAX=CLIENTE_OBJ.telefonofijo,
                TELEFONO=CLIENTE_OBJ.celular,
                TIPO_CLIENTE=CLIENTE_OBJ.codtipocliente  ,
                GIRO_NEGOCIO=CLIENTE_OBJ.codgironegocio,
                SEXO=sexox,
                E_MAIL=CLIENTE_OBJ.email,FECHA_CUMPLEANOS=TO_DATE(CLIENTE_OBJ.fechanacimiento,'YYYY-MM-DD'),
                STATUS_CLIENTE=CLIENTE_OBJ.estadocliente,
                FECHA_MODIFICACION=SYSDATE,
                COMENTARIO_2=CLIENTE_OBJ.referencia,
                USUAR_MODIF=CLIENTE_OBJ.usuarioregistra,
                DIRECCION_CLIENTE=CLIENTE_OBJ.direccion
            WHERE COD_CLIENTE=CLIENTE_OBJ.codcliente;

            SELECT distinct TM.CODEMPRESA,TM.CODSEDE,US.CODPERFIL into codempresax,codsedex,codperfilx
            FROM SYSADM.TB_USUARIO_MESA TM
                     INNER JOIN SYSADM.USUARIOS US ON TM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
            WHERE TM.USUARIO=CLIENTE_OBJ.usuarioregistra AND TM.ESTADO_EN_MESA='A';
            UPDATE SYSADM.CLIENTE_LISTAS
            SET COD_LISTA=CLIENTE_OBJ.codlistaprecios,USUARIO=CLIENTE_OBJ.usuarioregistra, FECHA_CAMBIO=sysdate
            WHERE COMPANIA_VENTA=codempresax
              AND COD_CLIENTE=CLIENTE_OBJ.codcliente
              AND TIPO_LISTA='01';




            rptax:=1;
            mensajex :='SE ACTUALIZÓ CORRECTAMENTE ';
            commit;


        elsif operacionx=5 then  -- solo numero telefono

            IF CLIENTE_OBJ.usuarioregistra IS NOT NULL THEN

                insert into SYSADM.LOG_TELEFONO_CLIENTE(COD_CLIENTE,NRO_TELEFONO,USUARIO,FECHAREG)
                VALUES(CLIENTE_OBJ.codcliente,CLIENTE_OBJ.celular,CLIENTE_OBJ.usuarioregistra,to_date(SUBSTR(sysdate,0,10)));

            ELSE

                RAISE exsinusuario;

            END IF;

            UPDATE SYSADM.CLIENTE
            SET
                NEW_TELEFONO=CLIENTE_OBJ.celular
            WHERE COD_CLIENTE=CLIENTE_OBJ.codcliente;
            rptax:=1;
            mensajex :='SE REGISTRÓ NÚMERO TELEFÓNICO CORRECTAMENTE ';
            commit;
        end if;

    EXCEPTION
        WHEN exsinusuario THEN
            rptax:=-1;
            mensajex:='PROBLEMAS!!PORFAVOR ACTUALIZA  TU APLICACIÓN!!';
        WHEN excliente THEN
            rptax:=-1;
            mensajex:='DESCRIPCION VACIA!!';
        WHEN OTHERS THEN
            rptax := -1;
            rollback;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;



    END MANTCLIENTE;



    PROCEDURE  CARGAR_DATOS_ALTA(
        usuariox IN VARCHAR2,
        cdptosx OUT types.ref_cursor,
        cgirosx OUT types.ref_cursor,
        ctiposx OUT types.ref_cursor,
        clp OUT types.ref_cursor,
        crutasx OUT types.ref_cursor,
        formatosx OUT types.ref_cursor
    )
        IS
        codempx VARCHAR2(10);
        codsedex VARCHAR2(10);
        codperfilx VARCHAR2(4);
    BEGIN

        open cdptosx FOR
            SELECT LLAVE,DESC1
            FROM SYSADM.TABLAS
            WHERE CATEGORIA = '090' AND LLAVE <> '00'	AND LLAVE <> '99'
            ORDER BY LLAVE;




        open cgirosx FOR

            select LLAVE,upper(DESC1),ESTADO from sysadm.TABLAS where categoria = '329'  AND NUM4=1;

        OPEN ctiposx FOR

            select LLAVE,upper(DESC1),ESTADO from sysadm.TABLAS where categoria = '006' AND ESTADO = 'H' ;




        SELECT distinct TM.CODEMPRESA,TM.CODSEDE,US.CODPERFIL into codempx,codsedex,codperfilx
        FROM SYSADM.TB_USUARIO_MESA TM
                 INNER JOIN SYSADM.USUARIOS US ON TM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
        WHERE TM.USUARIO=usuariox  AND TM.ESTADO_EN_MESA='A';

        IF codperfilx IN ('05','03') THEN
            open crutasx FOR
                select CODRUTA,DESCRUTA
                from SYSADM.TB_RUTA TR INNER JOIN SYSADM.TB_SEDE TS ON TR.IDSEDE=TS.IDSEDE
                    AND TS.CODSEDE=codsedex
                where ESTADO='H';


        ELSE
            open crutasx FOR
                SELECT DISTINCT   R.CODRUTA,R.DESCRUTA
                FROM  SYSADM.TB_RUTA_VENDEDOR RV
                          INNER JOIN SYSADM.TB_RUTA R ON RV.IDRUTA=R.IDRUTA
                          INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                WHERE 1=1
                  AND RV.DIA=(select translate(SUBSTR(to_char(sysdate, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                              from dual)
                  AND UM.USUARIO=usuariox;

        END IF;





        OPEN clp FOR
            SELECT DISTINCT LP.COD_LISTA, LP.COD_LISTA||'-'||LP.DESCRIPCION
            FROM SYSADM.PRECIOS_C LP
                     INNER JOIN SYSADM.LP_CONDICION LP_CON ON LP_CON.LISTAPRECIO = LP.COD_LISTA
                     INNER JOIN SYSADM.TABLAS EM ON EM.LLAVE = LP.COMPANIA_VENTA
                     INNER JOIN SYSADM.TB_SEDE SE ON SE.CODSEDE = LP_CON.CODIG_SEDE
            WHERE LP.ESTAD_SGC = 'A' AND EM.CATEGORIA = '001'  AND LP.COMPANIA_VENTA =codempx  AND LP_CON.CODIG_SEDE =codsedex
            ORDER BY LP.COD_LISTA;



        open formatosx for
            SELECT codigo,abrev,orden FROM SYSADM.FORM_DIR_CLIENTE;



    END CARGAR_DATOS_ALTA;




    PROCEDURE  VALIDAR_DOCUMENTO(nrodocx IN VARCHAR2,rptax  IN OUT    NUMBER,mensajex IN OUT    VARCHAR2)
        IS
        countClientex NUMBER;
        statusx VARCHAR2(10);
        codClientex VARCHAR2(11);
    BEGIN

        IF LENGTH(TRIM(nrodocx))=11 THEN
            codClientex:=TRIM(nrodocx);
        ELSE
            codClientex:='DNI'||TRIM(nrodocx);
        END IF;

        SELECT COUNT(COD_CLIENTE)INTO countClientex
        FROM SYSADM.CLIENTE
        WHERE COD_CLIENTE = codClientex ;

        IF countClientex=0 then
            rptax := 1;
            mensajex :='OK';
        ELSE
            SELECT STATUS_CLIENTE INTO statusx
            FROM SYSADM.CLIENTE
            WHERE COD_CLIENTE = codClientex ;
            IF statusx IN ('A') THEN
                rptax := -2;
                mensajex :='CLIENTE SE ENCUENTRA REGISTRADO Y ESTA ACTIVO ';
            ELSIf statusx='I' then
                rptax := -3;
                mensajex :='CLIENTE SE ENCUENTRA  REGISTRADO PERO ESTA INACTIVO ';
            ELSIF statusx='P' THEN
                rptax := -4;
                mensajex :='CLIENTE SE ENCUENTRA  REGISTRADO CON ESTADO PENDIENTE ';
            end if;

        END IF;

    END VALIDAR_DOCUMENTO;




    PROCEDURE MANTLOCAL(codClientex IN VARCHAR2,usuariox IN VARCHAR2,operacionx IN VARCHAR2,LOCAL_OBJ IN LOCAL_CLIENTE_MOVIL_AUDIV2,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
        IS

        correlativox NUMBER;
        desclocalx VARCHAR2(100);
        codempresax VARCHAR2(2);
        codsedex VARCHAR2(3);

        ubigeorutax VARCHAR2(10);
        distritox VARCHAR2(100);
        provinciax VARCHAR2(100);
        departamentox VARCHAR2(100);
        direccionfinalx VARCHAR2(900);
        cursorfindirx TYPEs.ref_cursor;
        cadenanewdirx VARCHAR2(900);
        tempodirx VARCHAR2(900);
        statuslocalx VARCHAR2(2);
        geolocalizacionx VARCHAR2(900);
        direccioncompletax VARCHAR2(900);

--excetest EXCEPTION;
    BEGIN

        IF operacionx=1  THEN

            -- SE QUITO AGREGAR LOCALES
/*SELECT DESC_CLIENTE INTO desclocalx
FROM SYSADM.CLIENTE WHERE COD_CLIENTE=codClientex;

SELECT MAX(COD_LOCAL)+1 INTO correlativox
 FROM SYSADM.LOCALES_CLIENTE
WHERE COD_CLIENTE = codClientex ;

		INSERT INTO SYSADM.LOCALES_CLIENTE(
		 COD_CIA, COD_CLIENTE, COD_LOCAL, DESC_LOCAL, DIRECCION, UBIGEO,
		 RUTA_DESPACHO, REF_DESPACHO, OBSERVACIONES, COD_USUARIO,
		 B_EXONERA_IGV, ESTAD, FECGEOLOCALIZACION, USUAR_GEOLO,ROWVERSION
		)
		VALUES('00', codClientex,correlativox, desclocalx, LOCAL_OBJ.direccion, LOCAL_OBJ.ubigeo,
		  LOCAL_OBJ.codruta, LOCAL_OBJ.coordenadas, LOCAL_OBJ.observaciones, usuariox,
		  (SELECT NVL(FLAG3, '0') FROM SYSADM.TABLAS
			 WHERE CATEGORIA = '035' AND LLAVE =  LOCAL_OBJ.ubigeo), 'A', SYSDATE,  usuariox,0
		);

					rptax := 1;
					mensajex :='SE REGISTRÓ CORRECTAMENTE ';
					commit;*/
            rptax := -1;
            rollback;
            mensajex :='NO SE PODRAN REGISTRAN LOCALES (ORDEN DE GERENCIA)';

        ELSIF  operacionx=5 THEN

            UPDATE SYSADM.LOCALES_CLIENTE
            SET NEW_COORDENADAS=geolocalizacionx,
                FECHA_NEWGEO=SYSDATE,
                USUAR_GEOLO=usuariox
            WHERE COD_CLIENTE = codClientex AND COD_LOCAL=LOCAL_OBJ.codlocal;
            rptax := 1;
            COMMIT;
            mensajex :='SE ACTUALIZÓ LA COORDENADA';

        ELSIF  operacionx=2 THEN

            SELECT distinct CODEMPRESA,CODSEDE  into codempresax,codsedex
            FROM SYSADM.TB_USUARIO_MESA
            WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';




            IF LOCAL_OBJ.codruta is null OR LOCAL_OBJ.codruta='' THEN

                rptax := -1;
                mensajex :='COD RUTA VACIA';
            ELSE

                SELECT UBIGEO INTO ubigeorutax
                FROM SYSADM.TB_RUTA WHERE CODRUTA=LOCAL_OBJ.codruta;

                SELECT DESC1  INTO distritox
                FROM SYSADM.TABLAS WHERE CATEGORIA='035' AND  LLAVE=ubigeorutax;


                SELECT DESC1 into provinciax
                FROM SYSADM.TABLAS WHERE CATEGORIA='132' AND LLAVE=substr(ubigeorutax,1,4);

                SELECT DESC1 into departamentox
                FROM SYSADM.TABLAS WHERE CATEGORIA='090' AND LLAVE <> '00'			--NINGUNA
                                     AND LLAVE <> '99' AND LLAVE=substr(ubigeorutax,1,2);


                open cursorfindirx FOR
                    select

                            CASE WHEN  (select ABREV from SYSADM.FORM_DIR_CLIENTE  where codigo=bb.valb) IS NOT NULL THEN
                                     (select ABREV from SYSADM.FORM_DIR_CLIENTE  where codigo=bb.valb)
                                 ELSE
                                     bb.valb
                                END  ||' '||aa.vala
                    FROM
                        (
                            select  a.orden,a.vala
                            from
                                (
                                    select rownum as orden,regexp_substr(LOCAL_OBJ.newdireccion,'[^_]+', 1, level) VALa
                                    from dual
                                    connect by regexp_substr(LOCAL_OBJ.newdireccion, '[^_]+', 1, level) is not null
                                )a)aa  inner join (
                            select b.orden,b.valb
                            FROM
                                (
                                    select rownum as orden, regexp_substr(LOCAL_OBJ.formatodir,'[^_]+', 1, level) VALb
                                    from dual
                                    connect by regexp_substr(LOCAL_OBJ.formatodir, '[^_]+', 1, level) is not null
                                )b)bb on aa.orden=bb.orden WHERE  aa.vala<>'XXXX';

                cadenanewdirx:='';

--cadenarutasx:='RUTAS DE HOY:'||chr(10);
                LOOP
                    FETCH cursorfindirx INTO tempodirx;--,estadox;
                    EXIT WHEN cursorfindirx%NOTFOUND;
                    cadenanewdirx:=cadenanewdirx||tempodirx||' ';

                END LOOP;
                CLOSE cursorfindirx;

                IF cadenanewdirx is null THEN
                    direccioncompletax:=LOCAL_OBJ.DIRECCION;
                ELSE
                    direccioncompletax:=cadenanewdirx||'-'||distritox||'-'||provinciax||'-'||departamentox;
                END IF;
--RAISE excetest;

                UPDATE SYSADM.LOCALES_CLIENTE
                SET COORDENADAS_VENDEDOR_AUDI=geolocalizacionx,
                    VENDEDOR_AUDI=usuariox,
                    FECHA_AUDI=SYSDATE,
                    IS_AUDITADO=1,
                    DIRECCION=direccioncompletax,
                    FORMATO_DIRECCION=LOCAL_OBJ.formatodir,
                    NEW_DIRECCION=LOCAL_OBJ.newdireccion,
                    RUTA_DESPACHO=LOCAL_OBJ.codruta,
                    OBSERVACIONES=LOCAL_OBJ.observaciones
                WHERE COD_CLIENTE = codClientex AND COD_LOCAL=LOCAL_OBJ.codlocal;

                UPDATE SYSADM.TBL_USUARIO_JORNADA
                SET COORDENADA_NEW=LOCAL_OBJ.coordenadas
                WHERE USUARIO=usuariox AND CODSEDE=codsedex AND CODCLIENTE = codClientex AND CODLOCAL=LOCAL_OBJ.codlocal AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD') ;





                rptax := 1;
                mensajex :='SE REGISTRÓ CORRECTAMENTE ';
                commit;
            END IF;

        END IF;
    EXCEPTION
        --WHEN excetest THEN
        --	rptax := -1;
        -- mensajex :=direccioncompletax;

        WHEN OTHERS THEN
            rptax := -1;
            rollback;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;




    END MANTLOCAL;


    PROCEDURE REGISTRARNOPEDIDO(codClientex IN VARCHAR2,codEmpresax IN VARCHAR2,codLocalx in VARCHAR2, codRutax in VARCHAR2,descclientex in VARCHAR2,descmotivox in VARCHAR2,direccionx IN VARCHAR2,usuariox IN VARCHAR2,coordenadasx IN VARCHAR2,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2  )
        IS
        codvendx  VARCHAR2(10);
        codsedex VARCHAR2(10);
        correlativox NUMBER;
    BEGIN


        SELECT  DISTINCT CODVENDEDOR,CODSEDE INTO codvendx,codsedex
        FROM SYSADM.TB_USUARIO_MESA WHERE USUARIO=usuariox
                                      AND CODMESA<>'0185' AND TB_TIPO_CODIGO='R';

        SELECT SEQ_NOPEDIDO.NEXTVAL INTO correlativox
        FROM SYSADM.DUAL;

        INSERT INTO  SYSADM.TBL_NOPEDIDO
        (ID_NOPEDIDO,
         COD_CLIENTE,
         DESC_CLIENTE,
         COD_EMPRESA,
         USUARIO,
         COD_VENDEDOR,
         COD_SEDE,
         MOTIVO,
         COD_LOCAL,
         DIRECCION,
         COD_RUTA,
         FECHA_TRANSACCION)
        values (
                   correlativox,
                   codClientex,
                   descclientex,
                   codEmpresax,
                   usuariox,
                   codvendx,
                   codsedex,
                   descmotivox,
                   codLocalx,
                   direccionx,
                   codRutax,
                   sysdate);


        UPDATE SYSADM.TBL_USUARIO_JORNADA
        SET IDNOPEDIDO=correlativox,COORDENADA_PEDIDO=coordenadasx
        WHERE USUARIO=usuariox AND CODSEDE=codsedex
          AND CODCLIENTE=codClientex AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD')
          AND CODLOCAL=codLocalx;

        rptax := 1;
        mensajex :='SE REGISTRÓ CORRECTAMENTE ';
        commit;


    EXCEPTION

        WHEN OTHERS THEN
            rptax := -1;
            rollback;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

    END REGISTRARNOPEDIDO;

    FUNCTION "PEDIDOSPORUSUARIO"
    ( usuariox VARCHAR2,
      fechapedido varchar2,
      codlocalidadx varchar2,
      codalmacenx varchar2
    )
        return types.ref_cursor is
        Result types.ref_cursor;
        codempresax VARCHAR2(10);
        codvendx  VARCHAR2(10);
        codsedex VARCHAR2(10);
    begin

        SELECT  DISTINCT CODEMPRESA,CODVENDEDOR,CODSEDE INTO codempresax, codvendx,codsedex
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usuariox
          AND CODMESA<>'0185' AND TB_TIPO_CODIGO='R';

        open Result for SELECT DISTINCT sph.nro_pedido,  sph.cod_cliente,sph.desc_cliente,
                                        CASE WHEN sph.status_pedido='G' THEN
                                                 'APROBADO'
                                             WHEN sph.status_pedido='I' THEN
                                                 'INGRESADO'
                                             WHEN sph.status_pedido='A' THEN
                                                 'ANULADO'
                                            END STATUS, sph.monto_desp_bonif AS MONTO_FINAL,
                                        sph.cod_almacen,sph.compania_venta,SPH.CONDICION_PAGO,SPH.DPTO_ORIGEN,
                                        NVL((SELECT SUM (mdcc_monto) FROM SYSADM.documento_cxc WHERE cdcc_compania = sph.compania_venta
                                                                                                 AND ndcc_pedidooih = sph.nro_pedido
                                                                                                 AND sdcc_status IN ('AP', 'CA', 'PP')),0)as montofacturado
                        FROM SYSADM.spedido_header sph
                        WHERE sph.cod_cia = '00'  AND sph.COMPANIA_VENTA= codempresax AND SPH.CODSEDE=codsedex
                          and SPH.COD_ALMACEN=codalmacenx
                          AND sph.fecha_pedido BETWEEN TO_DATE(fechapedido,'DD-MM-YYYY') AND TO_DATE(fechapedido,'DD-MM-YYYY')
                          AND sph.cod_vendedor = codvendx AND sph.NRO_PEDIDO_REF IS NULL

                        UNION

                        SELECT DISTINCT NPXT_SECPEDIDO,CPXT_CLIENTE,XPXT_CLIENTE,
                                        CASE WHEN BPXT_PEDRECHAZADO =1 THEN'BATCHERA ERROR ' ELSE 'BATCHERA EN ESPERA' END  STATUS,0,
                                        CPXT_ALMACEN,CPXT_COMPANIA,CPXT_CONDPAGO,CPXT_LOCALIDAD,0

                        FROM SYSADM.PEDID_X_TRANSFERIR
                        WHERE 1=1 AND CPXT_COMPANIA=codempresax AND CODSEDE=codsedex
                          AND CPXT_VENDEDOR=codvendx AND CPXT_ALMACEN=codalmacenx
                          AND TO_CHAR(FECHA_SIS,'DD-MM-YYYY')=fechapedido


                        ORDER BY 4 DESC;

        return(Result);
    end PEDIDOSPORUSUARIO;


    FUNCTION DETALLESBYPEDIDO
    (codempresax VARCHAR2,
     codalmacenx VARCHAR2,
     nropedidox VARCHAR2
    )
        return types.ref_cursor is
        Result types.ref_cursor;
    begin
        open Result for
            SELECT a.cod_item, a.desc_item, spd.um_pedido,SUM (spf.qty_pedida) qty_pedida ,
                   spd.precio_item AS PRECIO_UNIT,spd.valor_venta AS valor_venta, spd.impuesto_igv AS IGV , spd.venta_neta AS venta_neta,
                   (SELECT   NVL (qty_fisica, 0)- NVL (qty_comprometida, 0)- NVL (qty_reservada, 0)- NVL (qty_proyectos, 0)
                    FROM SYSADM.saldos_almacen WHERE cod_cia = '00'
                                                 AND compania_venta_3 =codempresax AND cod_item =a.cod_item AND almacen = codalmacenx
--AND  NVL (qty_fisica, 0)- NVL (qty_comprometida, 0)- NVL (qty_reservada, 0)- NVL (qty_proyectos, 0)>0
                   )disponible
            FROM SYSADM.spedido_fecentrega spf, SYSADM.spedido_detalle spd, SYSADM.articulos a
            WHERE spd.cod_cia = spf.cod_cia
              AND spd.compania_venta = spf.compania_venta
              AND spd.nro_pedido = spf.nro_pedido
              AND spd.nro_back_order = spf.nro_back_order
              AND spd.cod_item = spf.cod_item
              AND spd.flag_bonificado = spf.flag_bonificado
              AND spd.cod_despacho = spf.cod_despacho
              AND spd.cod_cia = '00'
              AND spd.compania_venta = codempresax
              AND spd.nro_pedido = nropedidox
              AND a.cod_cia = spd.cod_cia
              AND a.cod_item = spd.cod_item
            GROUP BY spd.nro_pedido,spd.nro_back_order,spd.cod_despacho,spd.flag_bonificado,a.cod_item,a.desc_item,spd.um_pedido,spd.factor,spd.qty_pedida,spd.precio_item,spd.valor_venta,spd.impuesto_igv,
                     spd.impuesto_selectivo,spd.venta_neta,spd.porcentaje_cp,spd.porcentaje_cp_tc,spd.porcentaje_excep,spd.porcentaje_item,spd.porcentaje_monto,spd.porcentaje_paquete,spd.porcentaje_tc,
                     spd.porcentaje_vol,spf.fecha_entrega,spd.descuento_cp,spd.descuento_cp_tc,spd.descuento_excep,spd.descuento_item,spd.descuento_monto,spd.descuento_paquete,spd.descuento_tc,spd.descuento_vol;
        return(Result);
    end DETALLESBYPEDIDO;


    FUNCTION DOCUMENTOSDEUDACLIENTE(codclientex VARCHAR2,usuariox VARCHAR2)

        return types.ref_cursor is

        Result types.ref_cursor;
        codvendx VARCHAR2(10);
        codempx VARCHAR2(10);
--codsedex VARCHAR2(10);
    BEGIN

        SELECT distinct CODEMPRESA,CODVENDEDOR into codempx,codvendx
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';

/*OPEN Result for
SELECT  CDCC_SECUENCIA,ndcc_serie,
   ndcc_preimpreso,cdcc_tipodoc,mdcc_monto,mdcc_saldo,fdcc_emision,cdcc_condventa
   FROM sysadm.documento_cxc, sysadm.cliente c
   WHERE c.cod_cia = cdcc_clientecia
   AND c.cod_cliente = CDCC_CLIENTE
   AND cdcc_compania = codempx
		and  c.cod_cliente=codclientex
   and CDCC_VENDEDOR=codvendx
		and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION
	from SYSADM.LP_CONDICION LC
INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND
ON  LC.CONDICION=COND.LLAVE
where 1=1
 AND FLAG10 IN (1) )
   AND (sdcc_status = 'AP' OR sdcc_status = 'PP')
ORDER BY cdcc_tipodoc, cdcc_secuencia;*/




        OPEN Result for
            SELECT DISTINCT CXC.CDCC_SECUENCIA,
                            CXC.ndcc_serie,
                            CXC.ndcc_preimpreso,
                            CXC.cdcc_tipodoc,
                            CXC.mdcc_monto,
                            CXC.mdcc_saldo,
                            TO_CHAR(CXC.fdcc_emision,'YYYY-MM-DD'),
                            CXC.cdcc_condventa,
                            CXC.CDCC_VENDEDOR,
--CASE WHEN ((SYSDATE-CXC.FDCC_EMISION))>NVL(LPC.DIAS_PLAZO,0) THEN 'VENCIDO' ELSE 'NOVENCIDO' END ESTADO
                            CASE WHEN ((SYSDATE-CXC.FDCC_EMISION))>NVL(LPC.DIAS_PLAZO,0) THEN ROUND(((SYSDATE-CXC.FDCC_EMISION))-NVL(LPC.DIAS_PLAZO,0),0)|| ' DIA(S) DE VENCIDO' ELSE 'NO VENCIDO' END ESTADO,
                            (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='030' AND LLAVE=CXC.CDCC_VENDEDOR)
            FROM SYSADM.DOCUMENTO_CXC CXC
                     INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                     INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH
                     LEFT JOIN SYSADM.SGC_COBRANZA sc on CXC.CDCC_COMPANIA=sc.codempresa and cxc.cdcc_secuencia=sc.secuenciadoc and sc.tipodocumento=cxc.cdcc_tipodoc and cxc.codsede=SC.codsede AND sc.estado_cobranza<>'A'
            WHERE CXC.CDCC_COMPANIA=codempx
              AND CXC.CDCC_CLIENTE=codclientex
              AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
              and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
              AND MDCC_MONTO>0
              and (sc.estado_cobranza is null or TO_CHAR(sc.FECHA_cobranza,'YYYY-MM-DD')<TO_CHAR(SYSDATE,'YYYY-MM-DD'))
              --and sc.estado_cobranza is null
--and (SYSDATE-CXC.FDCC_EMISION)>LPC.DIAS_PLAZO
            ORDER BY 1;
        return Result;

    END DOCUMENTOSDEUDACLIENTE;

    PROCEDURE MANTCOBRANZA(COBRANZA_OBJ IN COBRANZA_MOVIL,documentosx IN LISTA_DOCDEUDA,operacionx IN NUMBER ,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
        IS
        codvendx VARCHAR2(10);
        codempx VARCHAR2(10);
        codsedex VARCHAR2(10);
        montox NUMBER(10,2);
        fechax VARCHAR2(20);

    BEGIN

        SELECT distinct CODEMPRESA,CODVENDEDOR,CODSEDE into codempx,codvendx,codsedex
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=COBRANZA_OBJ.usuario AND TB_TIPO_CODIGO='R';
        if operacionx=1 then


            FOR indx IN  documentosx.FIRST .. documentosx.LAST
                LOOP
                    INSERT INTO SYSADM.SGC_COBRANZA
                    (ID_COBRANZA,
                     CODVENDEDOR,
                     FECHA_REGISTRO,
                     FECHA_COBRANZA,
                     MONTO_COBRADO,
                     CODEMPRESA,
                     CODSEDE,
                     TIPODOCUMENTO,
                     SECUENCIADOC,
                     DESC_BANCO,
                     NUMERO_OPERACION,
                     ESTADO_COBRANZA,
                     CODCLIENTE,
                     EFECTIVO,
                     USUVENDEDOR,
                     TERMINAL
                    )
                    VALUES(
                              COBRANZA_SEQUENCE.NEXTVAL,
                              codvendx,
                              SYSDATE,
                              to_date(SUBSTR(sysdate,0,10)),
                              documentosx(indx).montopagado,
                              codempx,
                              codsedex,
                              documentosx(indx).tipodoc,
                              documentosx(indx).secuencia,
                              UPPER(COBRANZA_OBJ.banco),
                              UPPER(COBRANZA_OBJ.nroperacion),
                              documentosx(indx).statuspago,
                              documentosx(indx).codcliente,
                              documentosx(indx).efectivo,
                              COBRANZA_OBJ.usuario,
                              'MOVIL'

                          );

                END LOOP;

            rptax := 1;
            mensajex :='SE REGISTRÓ CORRECTAMENTE ';
            commit;

        ELsif operacionx=2 THEN -- ACTUALIZA ESTADO DEPOSITADO

            FOR indx IN  documentosx.FIRST .. documentosx.LAST
                LOOP
                    UPDATE SYSADM.SGC_COBRANZA
                    SET ESTADO_COBRANZA='D',NUMERO_OPERACION='OP:'||COBRANZA_OBJ.nroperacion,DESC_BANCO=UPPER(COBRANZA_OBJ.banco)
                    WHERE CODEMPRESA=codempx
                      and TIPODOCUMENTO=documentosx(indx).tipodoc
                      AND NUMERO_PLANILLA IS NULL
                      AND SECUENCIADOC=documentosx(indx).secuencia;

                END LOOP;
            rptax := 1;
            mensajex :='SE REGISTRÓ CORRECTAMENTE ';

        ELsif operacionx=3 THEN -- ACTUALIZA MONTO INGRESADO

            FOR indx IN  documentosx.FIRST .. documentosx.LAST
                LOOP
                    UPDATE SYSADM.SGC_COBRANZA
                    SET MONTO_COBRADO=documentosx(indx).montopagado
                    WHERE CODEMPRESA=codempx
                      and TIPODOCUMENTO=documentosx(indx).tipodoc
                      AND ESTADO_COBRANZA='I'
                      --AND NUMERO_PLANILLA IS NULL
                      AND SECUENCIADOC=documentosx(indx).secuencia;

                END LOOP;
            rptax := 1;
            mensajex :='SE ACTUALIZÓ CORRECTAMENTE ';
        ELsif operacionx=4 THEN -- DELETE MONTO INGRESADO
            FOR indx IN  documentosx.FIRST .. documentosx.LAST
                LOOP

                    SELECT CODSEDE,MONTO_COBRADO,CODVENDEDOR,to_char(FECHA_COBRANZA,'YYYY-MM-DD') INTO codsedex,montox,codvendx,fechax
                    FROM SYSADM.SGC_COBRANZA
                    WHERE CODEMPRESA=codempx
                      AND TIPODOCUMENTO=documentosx(indx).tipodoc
                      AND SECUENCIADOC=documentosx(indx).secuencia AND ESTADO_COBRANZA='I';

                    INSERT INTO SYSADM.TB_AUDI_COBRANZA
                    (CODEMPRESA,CODSEDE,MONTO_COBRADO,CODVENDEDOR,TIPODOCUMENTO,FECHA_COBRANZA,SECUENCIADOC,USER_ELIMINACION,FECHA_TRANSACCION)
                    VALUES(codempx,codsedex,montox,codvendx,documentosx(indx).tipodoc,TO_DATE(fechax,'YYYY-MM-DD'),documentosx(indx).secuencia,COBRANZA_OBJ.usuario,sysdate);




                    /*
DELETE SYSADM.SGC_COBRANZA
WHERE CODEMPRESA=codempx
AND TIPODOCUMENTO=documentosx(indx).tipodoc
AND SECUENCIADOC=documentosx(indx).secuencia
AND ESTADO_COBRANZA='I';*/







                END LOOP;
            --rptax := 1;
            --mensajex :='SE ELIMINÓ CORRECTAMENTE ';
            rptax := -1;
            mensajex :='NO ES POSIBLE ELIMINAR(ORDEN GERENCIA)  ';
        END IF;

    EXCEPTION

        WHEN OTHERS THEN
            rptax := -1;
            rollback;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

    END MANTCOBRANZA;


    FUNCTION COBRANZABYUSUARIO(usuariox VARCHAR2,fechax VARCHAR2)
        return types.ref_cursor is
        Result types.ref_cursor;
        codvendx VARCHAR2(10);
        codempx VARCHAR2(10);
        codsedex VARCHAR2(10);
    begin

        SELECT distinct CODEMPRESA,CODVENDEDOR,CODSEDE into codempx,codvendx,codsedex
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';

        OPEN Result FOR
            SELECT DISTINCT CXC.CDCC_SECUENCIA,CXC.ndcc_serie,CXC.ndcc_preimpreso,CXC.cdcc_tipodoc,CXC.mdcc_monto,CXC.mdcc_saldo,TO_CHAR(CXC.fdcc_emision,'YYYY-MM-DD'),CXC.cdcc_condventa,CXC.CDCC_VENDEDOR,nvl(sc.monto_cobrado,0),
--CASE WHEN ((SYSDATE-CXC.FDCC_EMISION))>NVL(LPC.DIAS_PLAZO,0) THEN 'VENCIDO' ELSE 'NOVENCIDO' END ESTADO
                            CASE WHEN ((SYSDATE-CXC.FDCC_EMISION))>NVL(LPC.DIAS_PLAZO,0) THEN ROUND(((SYSDATE-CXC.FDCC_EMISION))-NVL(LPC.DIAS_PLAZO,0),0)|| ' DIA(S) DE VENCIDO' ELSE 'NO VENCIDO' END ESTADO,
                            (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='030' AND LLAVE=CXC.CDCC_VENDEDOR)
            FROM SYSADM.DOCUMENTO_CXC CXC
                     INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                     INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH
                     LEFT JOIN SYSADM.SGC_COBRANZA sc on CXC.CDCC_COMPANIA=sc.codempresa and cxc.cdcc_secuencia=sc.secuenciadoc and sc.tipodocumento=cxc.cdcc_tipodoc and cxc.codsede=SC.codsede and SC.USUVENDEDOR=usuariox
            WHERE CXC.CDCC_COMPANIA=codempx
              AND SC.FECHA_COBRANZA=to_DATE(fechax,'DD-MM-YYYY')
              AND sc.estado_cobranza='I'
              AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
              and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
              AND MDCC_MONTO>0
--and (SYSDATE-CXC.FDCC_EMISION)>LPC.DIAS_PLAZO
            ORDER BY 1;

        return(Result);
    end COBRANZABYUSUARIO;


    FUNCTION  CLIENTE_SIN_COORDENADAS(usernamex IN VARCHAR2) return types.ref_cursor is
        clientesx types.ref_cursor;
    BEGIN

        open clientesx for
            SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                            LC.REF_DESPACHO,
                            LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA
            FROM SYSADM.CLIENTE C
                     INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                     INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                     INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                     INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                     INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
            WHERE 1=1
              AND RV.DIA=(select translate(SUBSTR(to_char(sysdate, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                          from dual)
              AND UM.USUARIO=usernamex
              AND C.STATUS_CLIENTE='A'  --and rownum<1
              AND LC.REF_DESPACHO IS  NULL  --AND C.COD_CLIENTE='DNI16615022'
              AND LC.ESTAD='A'
            ORDER BY 5;

        return clientesx;

    END CLIENTE_SIN_COORDENADAS;




    PROCEDURE  DATOS_CLIENTE_PREVENTAV1(usuariox IN VARCHAR2,codClientex IN VARCHAR2,codEmpresax IN VARCHAR2,codListax IN VARCHAR2,limiteCreditox OUT NUMBER,deudaVencidax OUT NUMBER,deudaNoVencidax OUT NUMBER,tipoClientex OUT VARCHAR2,
                                        antiguedadx OUT VARCHAR2,cumpleañosx OUT VARCHAR2,velocidadPagox OUT NUMBER,countPedidosx OUT NUMBER,listaPreciox OUT VARCHAR2,artsugeridosx out types.ref_cursor,artfocusx out types.ref_cursor,exotelefonox out number,nrotelefonox OUT VARCHAR2)
        IS
        montocobradox NUMBER;
        bandtelefonox NUMBER;
    BEGIN

        SELECT LIMITE INTO limiteCreditox
        FROM (SELECT LIMITE FROM
            (
                SELECT
                    NVL(LIMIT_CREDI, 0) AS LIMITE
                FROM
                    XRAYADMIN.SGC_CLIENTE_LINEA_CREDITO
                WHERE
                        CODIG_EMPRE = codEmpresax
                  AND CODIG_CLIEN =codClientex
                  AND VIGEN = TO_CHAR(SYSDATE, 'MM-YYYY')
                  AND ESTAD = 'A'
                UNION SELECT 0 AS LIMITE FROM DUAL
            )
              ORDER BY LIMITE DESC
             )WHERE ROWNUM < 2;



        SELECT NVL(SUM(VENC.DEUDAVENCIDA),0) INTO deudaVencidax
        FROM
            (
                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDAVENCIDA--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                FROM SYSADM.DOCUMENTO_CXC CXC
                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                    FROM SYSADM.SGC_COBRANZA
                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                WHERE CXC.CDCC_COMPANIA=codEmpresax
                  AND CXC.CDCC_CLIENTE=codClientex
                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                  AND CXC.MDCC_MONTO>0
                  and (SYSDATE-CXC.FDCC_EMISION)>NVL(LPC.DIAS_PLAZO,0)
            )VENC;



        SELECT NVL(SUM(VENC.DEUDANOVENCIDA),0) INTO deudaNoVencidax
        FROM
            (
                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDANOVENCIDA--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                FROM SYSADM.DOCUMENTO_CXC CXC
                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                    FROM SYSADM.SGC_COBRANZA
                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                WHERE CXC.CDCC_COMPANIA=codEmpresax
                  AND CXC.CDCC_CLIENTE=codClientex
                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                  AND CXC.MDCC_MONTO>0
                  and (SYSDATE-CXC.FDCC_EMISION)<=NVL(LPC.DIAS_PLAZO,0)
            )VENC;



        SELECT (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='006' AND LLAVE=CL.TIPO_CLIENTE) INTO  tipoClientex
        FROM SYSADM.CLIENTE CL
        WHERE  COD_CLIENTE=codClientex;

        SELECT  D1.AÑOS||' A-' ||D1.MESES||' M ' into antiguedadx --|| TRUNC((D1.DIAS2-D1.MESES)*24)||' D'
        FROM
            (
                SELECT D.AÑOS,TRUNC(((D.DIAS-D.AÑOS)*360/30)) MESES,((D.DIAS-D.AÑOS)*360/30)DIAS2
                FROM
                    (
                        select FECHA_CREACION,(TRUNC((trunc(sysdate) - FECHA_CREACION)/360)) AÑOS,((trunc(sysdate) - FECHA_CREACION)/360) DIAS
                        from SYSADM.CLIENTE WHERE COD_CLIENTE=codClientex
                    )D
            )D1;







        SELECT FECHA_CUMPLEANOS ,1 INTO cumpleañosx,exotelefonox
        FROM SYSADM.CLIENTE WHERE COD_CLIENTE=codClientex;

        SELECT COUNT(*) INTO bandtelefonox
        FROM SYSADM.CLIENTE WHERE COD_CLIENTE=codClientex AND NEW_TELEFONO IS NOT NULL;

        IF bandtelefonox>0 THEN
            exotelefonox:=1;
            SELECT NEW_TELEFONO INTO nrotelefonox
            FROM SYSADM.CLIENTE WHERE COD_CLIENTE=codClientex ;

        elsif exotelefonox=1 AND  bandtelefonox=0 THEN
            nrotelefonox:='Sin número de teléfono';
        END IF;


        SELECT DISTINCT TRUNC(NVL(SUM(CASE WHEN((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO)<0 then 0 else ((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO) END)/COUNT(CXC.CDCC_SECUENCIA),0)) INTO velocidadPagox
        FROM SYSADM.DOCUMENTO_CXC CXC
                 INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND ON CXC.CDCC_CONDVENTA=COND.LLAVE
                 INNER JOIN SYSADM.SPEDIDO_HEADER SPH ON CXC.CDCC_COMPANIA=SPH.COMPANIA_VENTA  AND  CXC.NDCC_PEDIDOOIH=SPH.NRO_PEDIDO
                 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION AND SPH.COD_LISTA_PRECIOS=LPC.LISTAPRECIO AND LPC.LOCALIDAD=SPH.DPTO_ORIGEN
        WHERE 1=1 AND SPH.COD_CIA='00' AND LPC.ESTAD='A'
          AND CXC.CDCC_CLIENTE=codClientex
          AND SDCC_STATUS='CA' AND COND.FLAG10=1
          AND MDCC_MONTO>0  ;



        SELECT COUNT(DISTINCT NRO_PEDIDO) INTO countPedidosx
        FROM SYSADM.SPEDIDO_HEADER WHERE COD_CLIENTE=codClientex AND STATUS_PEDIDO='G';


        SELECT DISTINCT  LP.COD_LISTA||'-'||LP.DESCRIPCION into listaPreciox
        FROM SYSADM.PRECIOS_C LP
                 INNER JOIN SYSADM.LP_CONDICION LP_CON ON LP_CON.LISTAPRECIO = LP.COD_LISTA
                 INNER JOIN SYSADM.TABLAS EM ON EM.LLAVE = LP.COMPANIA_VENTA
                 INNER JOIN SYSADM.TB_SEDE SE ON SE.CODSEDE = LP_CON.CODIG_SEDE
        WHERE LP.ESTAD_SGC = 'A' AND EM.CATEGORIA = '001'
          AND LP.COMPANIA_VENTA =codEmpresax
          AND LP.COD_LISTA =codListax;


        open artsugeridosx FOR
            SELECT B.COD_ITEM,B.DESC_ITEM,B.CANT_SUGERIDA,	'S',B.NRO_VECES
            FROM(
                    SELECT ROWNUM AS FILA,A.*
                    FROM (
                             SELECT PS.COD_CLIENTE,PS.COD_PROVEEDOR,PS.COD_ITEM,ART.DESC_ITEM,PS.CANT_SUGERIDA,PS.NRO_VECES
                             FROM
                                 (
                                     SELECT DISTINCT  CASE WHEN substr(TDM.C_PROVEEDOR,0,instr (TDM.C_PROVEEDOR, '-')-1) IS NULL THEN TDM.C_PROVEEDOR ELSE substr(TDM.C_PROVEEDOR,0,instr (TDM.C_PROVEEDOR, '-')-1) END AS codprov
                                     FROM SYSADM.TB_USUARIO_MESA TUM
                                              INNER JOIN SYSADM.USUARIOS US ON TUM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
                                              INNER JOIN SYSADM.TB_MESA TM ON TUM.CODEMPRESA=TM.CODEMPRESA AND TUM.CODMESA=TM.CODLOCALIDAD
                                              INNER JOIN SYSADM.TB_DETALLE_MESA TDM ON TM.IDMESA=  TDM.IDMESA
                                     WHERE US.USUARIO=usuariox AND TUM.TB_TIPO_CODIGO='R'
                                 )USUPROV
                                     INNER JOIN SYSADM.PEDIDO_SUGERIDO PS ON USUPROV.CODPROV=PS.COD_PROVEEDOR AND TO_CHAR(PS.FECHA,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD')
                                     INNER JOIN SYSADM.ARTICULOS ART ON PS.COD_ITEM=ART.COD_ITEM
                             WHERE 1=1 AND PS.COD_CLIENTE=codClientex
                             ORDER BY 6 DESC
                         )A
                )B where 1=1 AND B.FILA<4 ;


        OPEN artfocusx FOR
            SELECT AF.CODITEM,ART.DESC_ITEM,case when UPDF.CANT_SUGERIDA=0 then 1 else UPDF.CANT_SUGERIDA end ,'F'
                 ,AF.UNIDADMEDIDA,AF.CODEMPRESA,AF.CODSEDE,AF.CODALMACEN
            FROM SYSADM.ARTICULOS ART
                     INNER JOIN SYSADM.FOCUS_ARTICULO AF  ON ART.COD_ITEM=AF.CODITEM
                     INNER JOIN SYSADM.FOCUS_PARTICIPACION PF ON AF.IDFOCUSDET=PF.IDFOCUSDET
                     INNER JOIN SYSADM.FOCUS_USUARIO_PARTI UPF ON UPF.IDPARTICIPACION=PF.IDPARTICIPACION
                     INNER JOIN SYSADM.FOCUS_USU_PARTI_DET UPDF ON UPDF.IDUSUPARTIFOCUS=UPF.IDUSUPARTIFOCUS
            WHERE UPF.USUARIO=usuariox
              AND AF.ESTADO='P'
              AND TO_CHAR(UPDF.FECHA,'DD/MM/YYYY')<=TO_CHAR(SYSDATE,'DD/MM/YYYY');


    END DATOS_CLIENTE_PREVENTAV1;

    FUNCTION "ANULAR_PEDIDO_INGRESADO" (pedidox SPEDIDO_ANULAR) return number
    AS
        statusaprobx number;
        statusbdx varchar2(2);
    begin

        SELECT num4 into statusaprobx
        FROM "SYSADM"."TABLAS"
        where CATEGORIA='001' and llave=pedidox.codempresa;

        select STATUS_PEDIDO into statusbdx from SYSADM.spedido_header
        WHERE cod_cia = '00' AND compania_venta =pedidox.codempresa
          AND nro_pedido =pedidox.nropedido;

        if statusbdx='I' AND statusaprobx=0 THEN

            UPDATE SYSADM.spedido_header SET rowversion = 0, status_pedido = 'A',
                                             fecha_anulacion = SYSDATE, usuario_anulacion = pedidox.usuarioxray
            WHERE cod_cia = '00' AND compania_venta=pedidox.codempresa AND nro_pedido =pedidox.nropedido;

            UPDATE SYSADM.cxc_cliente_std
            SET  rowversion = 0,
                 credito_utilizado = NVL(credito_utilizado,0)-pedidox.montovta,
                 credito_disponible = NVL(credito_disponible,0) +pedidox.montovta
            WHERE cod_cia = '00' AND cod_cliente = pedidox.codcliente AND cia_venta =pedidox.codempresa;

            commit;
            return 1;
        ELSE
            return -1;
        END IF;

    exception
        WHEN NO_DATA_FOUND THEN
            return -1;
        WHEN OTHERS THEN
            return -1;
    end ANULAR_PEDIDO_INGRESADO;


    FUNCTION COBRANZA_BY_USUARIO(usuariox VARCHAR2,fechax VARCHAR2)return types.ref_cursor is
        cobranzax types.ref_cursor;
    BEGIN

        open cobranzax for
            SELECT CXC.NDCC_SERIE,CXC.NDCC_PREIMPRESO,to_char(CXC.FDCC_EMISION,'YYYY-MM-DD') FECHAEMISION,CXC.MDCC_SALDO,CXC.XDCC_CLIENTE,MONTO_COBRADO,
                   CASE WHEN SC.ESTADO_COBRANZA='D' THEN
                            'DEPOSITADO'
                        ELSE
                            'LIQUIDADO'
                       END STATUSCOBRANZA
            FROM SYSADM.DOCUMENTO_CXC CXC  INNER JOIN SYSADM.SGC_COBRANZA SC ON CXC.CDCC_COMPANIA=SC.CODEMPRESA AND CXC.CDCC_TIPODOC=SC.TIPODOCUMENTO AND CXC.CDCC_SECUENCIA=SC.SECUENCIADOC
                                           INNER JOIN SYSADM.CLIENTE CL ON SC.CODCLIENTE=CL.COD_CLIENTE
            WHERE SC.USUVENDEDOR=usuariox
              AND SC.ESTADO_COBRANZA IN ('D','A')
              AND TO_CHAR(SC.FECHA_COBRANZA,'dd-MM-YYYY')=fechax;

        return cobranzax;

    END COBRANZA_BY_USUARIO;



    FUNCTION  CLIENTES_OUT_RUTA(usernamex IN VARCHAR2,filtrox IN VARCHAR2) return types.ref_cursor is
        clientesx types.ref_cursor;
        codempx VARCHAR2(10);
        codvendx VARCHAR2(10);
        codsedex VARCHAR2(10);

    BEGIN

        SELECT distinct CODEMPRESA,CODVENDEDOR,CODSEDE into codempx,codvendx,codsedex
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usernamex AND TB_TIPO_CODIGO='R';

        open clientesx for
            SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                            LC.NEW_COORDENADAS,
                            LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,
                            codempx,'0' FLAESTADO,
                            CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                            case when C.STATUS_CLIENTE='A' then 'ACTIVO'  when C.STATUS_CLIENTE='P' then 'PENDIENTE' ELSE 'INACTIVO' END STATUS
            FROM SYSADM.CLIENTE C
                     INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                     INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                     INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND CL.COMPANIA_VENTA=codempx AND CL.TIpo_lista='01'
                     INNER JOIN SYSADM.PRECIOS_C PRE ON PRE.COMPANIA_VENTA = CL.COMPANIA_VENTA
                AND PRE.COD_CIA = CL.COD_CIA AND PRE.COD_LISTA = CL.COD_LISTA
                AND PRE.CODSEDE =codsedex
            WHERE 1=1
              AND c.cod_cia = '00'
-- AND c.STATUS_CLIENTE='A'
--AND LC.ESTAD='A'
              and c.cod_cliente||c.desc_cliente LIKE '%'||translate(filtrox, 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')||'%'  AND ROWNUM<11;

/*SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
LC.NEW_COORDENADAS,
LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,
codempx,'0' FLAESTADO,
CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
case when C.STATUS_CLIENTE='A' then 'ACTIVO' ELSE 'INACTIVO' END STATUS
   FROM SYSADM.CLIENTE C
	INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
  INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND CL.COMPANIA_VENTA=codempx AND CL.TIpo_lista='01'
INNER JOIN SYSADM.PRECIOS_C PRE ON PRE.COMPANIA_VENTA = CL.COMPANIA_VENTA
			AND PRE.COD_CIA = CL.COD_CIA AND PRE.COD_LISTA = CL.COD_LISTA
			AND PRE.CODSEDE =codsedex
WHERE 1=1
AND c.cod_cia = '00'
 AND R.CODRUTA
IN (SELECT CODRUTA
FROM SYSADM.TB_RUTA_VENDEDOR
INNER JOIN SYSADM.TB_RUTA ON TB_RUTA.IDRUTA = TB_RUTA_VENDEDOR.IDRUTA
WHERE 1=1
AND DIA  = (select translate(SUBSTR(to_char(to_date(SYSDATE,'DD/MM/YY'), 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
from dual) and TB_RUTA_VENDEDOR.codvendedor=codvendx)
and c.cod_cliente||c.desc_cliente LIKE '%'||translate(filtrox, 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')||'%' ;
*/
        return clientesx;

    END CLIENTES_OUT_RUTA;


    PROCEDURE  MENU_OUTRUTA_USUARIO(
        usuariox IN VARCHAR2,
        outrutax OUT NUMBER
    )
        IS
    BEGIN

        SELECT NVL(PERMITE_OUT_RUTA,0) INTO outrutax
        FROM SYSADM.USUARIOS
        WHERE USUARIO=usuariox;

    END MENU_OUTRUTA_USUARIO;


    PROCEDURE  DATOS_SUELDO_VENDEDOR(usuariox IN VARCHAR2,cabecerax OUT types.ref_cursor,politicasx OUT types.ref_cursor,adicionalx OUT types.ref_cursor,sueldovendedorx OUT types.ref_cursor,cadenarutasx OUT VARCHAR2)
        IS
        codempx   VARCHAR2(2);
        codsedex  VARCHAR2(4);
        codmesax   VARCHAR2(4);
        codcanalx   VARCHAR2(4);
        codvendx VARCHAR2(4);
        anniox VARCHAR2(6);
        idmesasueldox NUMBER;
        idperiodosueldox NUMBER;
        periodox VARCHAR2(100);
        fechaServerx VARCHAR2(200);
        fechainix VARCHAR2(200);
        fechafinx VARCHAR2(200);
        rutasx  types.ref_cursor;
        codrutax  VARCHAR2(10);
        descrutax VARCHAR2(100);
        cantrutasx NUMBER;
    BEGIN

        SELECT distinct CODEMPRESA,CODVENDEDOR,CODSEDE,CODMESA,CODCANAL  into codempx,codvendx,codsedex,codmesax,codcanalx
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';

        SELECT IDMESASUELDO into idmesasueldox--,SGC_MESA_SUELDO.CODEMPRESA,EMPRESA.NOMEMPRESA,TB_SEDE.CODSEDE,DESCSEDE,SGC_MESA_SUELDO.CODMESA,DESCMESA,
        --TB_CANAL.CODCANAL,DESCCANAL
        FROM SYSADM.SGC_MESA_SUELDO
                 INNER JOIN (SELECT LLAVE,DESC1 AS NOMEMPRESA FROM SYSADM.TABLAS WHERE CATEGORIA = '001') EMPRESA
                            ON EMPRESA.LLAVE = SGC_MESA_SUELDO.CODEMPRESA
                 INNER JOIN SYSADM.TB_SEDE ON TB_SEDE.CODSEDE = SGC_MESA_SUELDO.CODSEDE
                 INNER JOIN SYSADM.TB_MESA ON TB_MESA.CODLOCALIDAD = SGC_MESA_SUELDO.CODMESA
                 INNER JOIN SYSADM.TB_CANAL ON TB_CANAL.CODCANAL = SGC_MESA_SUELDO.CODCANAL
        WHERE 1=1  AND SGC_MESA_SUELDO.CODEMPRESA = codempx
          AND SGC_MESA_SUELDO.CODSEDE = codsedex
          AND SGC_MESA_SUELDO.CODMESA = codmesax
          AND SGC_MESA_SUELDO.CODCANAL = codcanalx ;

        SELECT T.ANIO,T.PERIODO,to_Char(T.CODFECHA,'dd-mm-yyyy')INTO anniox,periodox,fechaServerx
        FROM SYSADM.TIEMPO T INNER JOIN (SELECT TO_CHAR(SYSDATE,'YYYY/MM/DD')FECHA
                                         FROM SYSADM.DUAL)F ON T.FECHA=F.FECHA;



        SELECT TO_CHAR(MIN(T.CODFECHA),'DD-MM-YYYY') FMIN ,TO_CHAR(MAX(T.CODFECHA),'DD-MM-YYYY') FMAX INTO fechainix,fechafinx
        FROM SYSADM.TIEMPO T WHERE t.ANIO=anniox AND T.PERIODO=periodox;

        SELECT IDPERIODOSUELDO INTO idperiodosueldox--,CODPERIODO,CODANIO,ESTPERIODOSUELDO
        FROM SYSADM.SGC_PERIODO_SUELDO
        WHERE 1=1  AND CODANIO =anniox AND CODPERIODO = periodox;

--cabecerax:=SGCSQL_CABECERA_MESASUELDO(null,idmesasueldox,1);
        OPEN cabecerax FOR
            SELECT IDMESASUELDOCAB,SGC_MESA_CABSUELDO.IDMESASUELDO,SGC_MESA_CABSUELDO.IDCABECERASUELDO,
                   DESCRIPCIONCAB,VALORCAB,TIPOCAB,ESTADOCAB,NROORDEN
            FROM SYSADM.SGC_MESA_CABSUELDO
                     INNER JOIN SYSADM.SGC_CABECERA_SUELDO ON SGC_CABECERA_SUELDO.IDCABECERASUELDO = SGC_MESA_CABSUELDO.IDCABECERASUELDO
            WHERE 1=1  AND SGC_MESA_CABSUELDO.IDMESASUELDO =idmesasueldox  order by 1 asc ;



--politicasx:=SGCSQL_POLITICA_ASIGNADA(null,idmesasueldox,idperiodosueldox,null,null,null,null,null,null,null,null,null,1);
        OPEN politicasx FOR
            SELECT IDPOLITICAASIGNADA,CODPOLITICA,NOMPOLITICA,DESCPOLITICA
            FROM SYSADM.SGC_POLITICA_ASIGNADA
            WHERE 1=1 --- AND CODPOLITICA<>'06'
              AND IDMESASUELDO =idmesasueldox  AND IDPERIODOSUELDO = idperiodosueldox  order by 2 desc ;


        adicionalx:=SGCSQL_PERIODO_ADICIONAL(null,idperiodosueldox,1);
        sueldovendedorx:=SGC_V4.SGCSQL_AVANCE_SUELDOS(codempx,codsedex,codmesax,codcanalx,anniox,periodox,fechainix,fechafinx,codvendx,'5 desc',NULL,1);




        SELECT DISTINCT  COUNT(*) into cantrutasx
        FROM   SYSADM.TB_RUTA R
                   INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                   INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
        WHERE 1=1
          AND RV.DIA=(select translate(SUBSTR(to_char(SYSDATE, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                      from dual)
          AND UM.USUARIO=usuariox;

        IF cantrutasx>0 THEN

            open rutasx FOR
                SELECT DISTINCT  R.CODRUTA,R.DESCRUTA
                FROM   SYSADM.TB_RUTA R
                           INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                           INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                WHERE 1=1
                  AND RV.DIA=(select translate(SUBSTR(to_char(SYSDATE, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                              from dual)
                  AND UM.USUARIO=usuariox;
            cadenarutasx:='RUTAS DE HOY:'||chr(10);
            LOOP
                FETCH rutasx INTO codrutax,descrutax;--,estadox;
                EXIT WHEN rutasx%NOTFOUND;
                cadenarutasx:=cadenarutasx||codrutax||'-'||descrutax||chr(10);

            END LOOP;
            CLOSE rutasx;
        ELSE
            cadenarutasx:='NO TIENE RUTAS ASIGNADAS HOY, COMUNIQUESE CON SU SUPERVISOR ';

        END IF;





    END DATOS_SUELDO_VENDEDOR;






    FUNCTION PLANILLACOBRANZA(usuariox VARCHAR2)return types.ref_cursor is
        cobranzax types.ref_cursor;
        codempx VARCHAR2(10);
        codsedex VARCHAR2(10);
        codvendx VARCHAR2(10);
        codmesax VARCHAR2(10);
        codcanalx VARCHAR2(10);
    BEGIN
        SELECT distinct CODEMPRESA,CODVENDEDOR,CODSEDE,CODMESA,CODCANAL  into codempx,codvendx,codsedex,codmesax,codcanalx
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';
        open cobranzax for
/*SELECT DISTINCT CXC.CDCC_COMPANIA,
CXC.CDCC_TIPODOC,
CXC.NDCC_SERIE||'-'||CXC.NDCC_PREIMPRESO,
CXC.CDCC_CLIENTE,
CXC.XDCC_CLIENTE,
CXC.MDCC_MONTO,
CXC.MDCC_SALDO,
CXC.CDCC_VENDEDOR,
CXC.XDCC_DIRCLIENTE,
CASE WHEN ((SYSDATE-CXC.FDCC_EMISION))>NVL(LPC.DIAS_PLAZO,0) THEN 'VENCIDO' ELSE 'NOVENCIDO' END ESTADO
FROM SYSADM.DOCUMENTO_CXC CXC
INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH
LEFT JOIN SYSADM.SGC_COBRANZA sc on CXC.CDCC_COMPANIA=sc.codempresa and CXC.cdcc_secuencia=sc.secuenciadoc and sc.tipodocumento=CXC.cdcc_tipodoc and CXC.codsede=SC.codsede AND sc.estado_cobranza<>'A'
WHERE CXC.CDCC_COMPANIA = codempx
AND CXC.CODSEDE =codsedex
AND CXC.SDCC_STATUS IN ('AP','PP')
AND CXC.FDCC_VENCIMIENTO < SYSDATE
AND CXC.CDCC_CODIGO2 IN (SELECT CODRUTA FROM SYSADM.TB_RUTA_VENDEDOR
INNER JOIN SYSADM.TB_RUTA ON TB_RUTA.IDRUTA = TB_RUTA_VENDEDOR.IDRUTA
WHERE 1=1
AND DIA  = (select translate(SUBSTR(to_char(sysdate, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
from dual) and TB_RUTA_VENDEDOR.codvendedor=codvendx)
AND CXC.CDCC_CONDVENTA IN (SELECT LLAVE FROM SYSADM.TABLAS WHERE CATEGORIA = '008' AND RELLENO1 > 0)
AND CXC.MDCC_SALDO > 0;*/
            SELECT DISTINCT
                CXC.CDCC_COMPANIA,
                CXC.CDCC_TIPODOC,
                CXC.NDCC_SERIE||'-'||CXC.NDCC_PREIMPRESO,
                CXC.CDCC_CLIENTE,
                CXC.XDCC_CLIENTE,
                CXC.MDCC_MONTO,
                CXC.MDCC_SALDO,
                CXC.CDCC_VENDEDOR,
                CXC.XDCC_DIRCLIENTE,
--CASE WHEN ((SYSDATE-CXC.FDCC_EMISION))>NVL(LPC.DIAS_PLAZO,0) THEN 'VENCIDO' ELSE 'NOVENCIDO' END ESTADO
                CASE WHEN ((SYSDATE-CXC.FDCC_EMISION))>NVL(LPC.DIAS_PLAZO,0) THEN ROUND(((SYSDATE-CXC.FDCC_EMISION))-NVL(LPC.DIAS_PLAZO,0),0)|| ' DIA(S) DE VENCIDO' ELSE 'NO VENCIDO' END ESTADO,
                (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='030' AND LLAVE=CXC.CDCC_VENDEDOR)

            FROM SYSADM.DOCUMENTO_CXC CXC
                     INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                     INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH
                     LEFT JOIN SYSADM.SGC_COBRANZA sc on CXC.CDCC_COMPANIA=sc.codempresa and cxc.cdcc_secuencia=sc.secuenciadoc and sc.tipodocumento=cxc.cdcc_tipodoc and cxc.codsede=SC.codsede-- AND sc.estado_cobranza='A'
                AND SC.FECHA_COBRANZA=to_date(SYSDATE,'DD/MM/YY')
            WHERE CXC.CDCC_COMPANIA=codempx
              AND CXC.CDCC_VENDEDOR=codvendx
              AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
              AND CXC.CDCC_CODIGO2 IN (SELECT CODRUTA FROM SYSADM.TB_RUTA_VENDEDOR
                                                               INNER JOIN SYSADM.TB_RUTA ON TB_RUTA.IDRUTA = TB_RUTA_VENDEDOR.IDRUTA
                                       WHERE 1=1
                                         AND DIA  = (select translate(SUBSTR(to_char(to_date(SYSDATE,'DD/MM/YY'), 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                                     from dual) and TB_RUTA_VENDEDOR.codvendedor=codvendx)
              AND SC.MONTO_COBRADO IS NULL
              and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
              AND MDCC_MONTO>0 ORDER BY 10 DESC;


        return cobranzax;

    END PLANILLACOBRANZA;





    PROCEDURE  CLIENTES_BY_DIAV2(usernamex IN VARCHAR2,ignoresecx out number, clientesx OUT types.ref_cursor)
        IS
        countjornadax number;
        codclientex VARCHAR2(20);
        descclientex VARCHAR2(200);
        direccionx VARCHAR2(400);
        coordenadaoldx VARCHAR2(400);
        codrutax VARCHAR2(20);
        descrutax VARCHAR2(200);
        codlocalx VARCHAR2(2);
        codlistax VARCHAR2(10);
        codempresax VARCHAR2(2);
        estadox VARCHAR2(2);
        diajornadax DATE;
        countnewx number;
        countauxx number;

        codsedex VARCHAR2(3);

        diajornadavarcharx VARCHAR2(200);
        nrotelefonox VARCHAR2(200);
        nombresx VARCHAR2(300);
        apellidosx	 VARCHAR2(300);
        newdireccionx VARCHAR2(800);
        isauditadox NUMBER;
    BEGIN

        diajornadax:=SYSDATE;
        diajornadavarcharx:=TO_CHAR(SYSDATE,'YYYY-MM-DD');


        SELECT distinct CODEMPRESA,CODSEDE  into codempresax,codsedex
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usernamex AND TB_TIPO_CODIGO='R';


        SELECT COUNT(*) into countjornadax
        FROM SYSADM.TBL_USUARIO_JORNADA
        WHERE USUARIO=usernamex AND CODSEDE=codsedex
--AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD');
          AND FECHA=TO_date(diajornadavarcharx,'YYYY-MM-DD');

        --SELECT NVL(IGNORE_SECUENCIACION,0) into ignoresecx
--FROM SYSADM.USUARIOS WHERE USUARIO=usernamex;
        ignoresecx:=0;

        IF countjornadax=0 then
            open clientesx for
                SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                LC.NEW_COORDENADAS,
                                LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA,STATUS_CLIENTE AS ESTADO,C.NEW_TELEFONO,
                                C.NOMBRES,C.APELLIDOS,LC.NEW_DIRECCION,
                                NVL(LC.IS_AUDITADO,0) IS_AUDITADO
                FROM SYSADM.CLIENTE C
                         INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                         INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                         INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                         INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                         INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'

                         INNER JOIN SYSADM.PRECIOS_C PC ON PC.COMPANIA_VENTA=CL.COMPANIA_VENTA AND PC.COD_LISTA=CL.COD_LISTA AND PC.CODSEDE=codsedex -- se agrego para q solo aparezcan clientes con listas de la sede del vendedor


                WHERE 1=1
                  AND RV.DIA=(select translate(SUBSTR(to_char(diajornadax, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                              from dual)
                  AND UM.USUARIO=usernamex
                  AND C.STATUS_CLIENTE IN ('A','P')
                  AND LC.ESTAD='A'
                  AND nvl(C.USUAR_REGIS,'NONE')<>'EC';--EXCLUIR CLIENTES DIGITALES

            LOOP
                FETCH clientesx INTO codclientex,descclientex,direccionx,coordenadaoldx,codrutax,descrutax,codlocalx,codlistax,codempresax,estadox,nrotelefonox,nombresx,apellidosx,newdireccionx,isauditadox;
                EXIT WHEN clientesx%NOTFOUND;
                INSERT INTO SYSADM.TBL_USUARIO_JORNADA(USUARIO,CODSEDE,FECHA,CODCLIENTE,DESCCLIENTE,CODLOCAL,CODRUTA,DESCRUTA,DIRECCION,
                                                       COORDENADA_ANT,CODLISTA,CODEMPRESA,ESTADO,FECHA_TRANS,NEW_TELEFONO,NOMBRES,APELLIDOS,NEW_DIRECCION,IS_AUDITADO)
                VALUES(usernamex,codsedex,to_date(SUBSTR(diajornadax,0,10)),codclientex,descclientex,codlocalx,codrutax,descrutax,direccionx,
                       coordenadaoldx,codlistax,codempresax,estadox,SYSDATE,nrotelefonox,nombresx,apellidosx,newdireccionx,isauditadox);
            END LOOP;
            CLOSE clientesx;


            open clientesx for
                SELECT UC.CODCLIENTE,UC.DESCCLIENTE,UC.DIRECCION,LC.NEW_COORDENADAS,UC.CODRUTA,UC.DESCRUTA,UC.CODLOCAL,UC.CODLISTA,UC.CODEMPRESA,
                       CASE WHEN UC.IDNOPEDIDO IS NOT NULL THEN 'N'
                            WHEN UC.NROPEDIDO IS NOT NULL THEN 'P'
                            ELSE '0' END AS FLAGESTADO,
                       CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                       ESTADO ESTADOCLIENTE,TO_CHAR(FECHA,'YYYY-MM-DD') FECHA,uc.new_telefono,uc.nombres,uc.apellidos,uc.new_direccion,uc.is_auditado
                FROM SYSADM.TBL_USUARIO_JORNADA UC
                         LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UC.CODCLIENTE AND LC.COD_LOCAL=UC.CODLOCAL
                where USUARIO=usernamex  and CODSEDE=codsedex
--AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                  AND  FECHA =TO_date(diajornadavarcharx,'YYYY-MM-DD')
                  AND ESTADO IS NOT NULL ;-- SE AGREGO POR QUE QUITAN RUTAS A LOS USUARIOS /CLIENTES DE RUTA ANTIGUA VOLANDO

        ELSE

            SELECT COUNT(*)  INTO countnewx
            FROM(
                    SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                    LC.NEW_COORDENADAS,
                                    LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA,STATUS_CLIENTE AS ESTADO,c.new_telefono
                    FROM SYSADM.CLIENTE C
                             INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                             INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                             INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                             INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                             INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'

                             INNER JOIN SYSADM.PRECIOS_C PC ON PC.COMPANIA_VENTA=CL.COMPANIA_VENTA AND PC.COD_LISTA=CL.COD_LISTA AND PC.CODSEDE=codsedex -- se agrego para q solo aparezcan clientes con listas de la sede del vendedor



                    WHERE 1=1
                      AND RV.DIA=(select translate(SUBSTR(to_char(diajornadax, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                  from dual)
                      AND UM.USUARIO=usernamex
                      AND C.STATUS_CLIENTE IN ('A','P')
                      AND LC.ESTAD='A'
                      AND nvl(C.USUAR_REGIS,'NONE')<>'EC'  --EXCLUIR CLIENTES DIGITAL

                )A
                    LEFT JOIN
                (
                    SELECT UC.CODCLIENTE,UC.DESCCLIENTE,UC.DIRECCION,LC.NEW_COORDENADAS,UC.CODRUTA,UC.DESCRUTA,UC.CODLOCAL,UC.CODLISTA,UC.CODEMPRESA,
                           CASE WHEN UC.ISNEW IS NOT NULL THEN 'NEW'
                                WHEN UC.IDNOPEDIDO IS NOT NULL THEN 'N'
                                WHEN UC.NROPEDIDO IS NOT NULL THEN 'P'
                                ELSE '0' END AS FLAGESTADO,
                           CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                           ESTADO ESTADOCLIENTE,UC.NEW_TELEFONO,uc.nombres,uc.apellidos,uc.new_direccion,uc.is_auditado
                    FROM SYSADM.TBL_USUARIO_JORNADA UC
                             LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UC.CODCLIENTE AND LC.COD_LOCAL=UC.CODLOCAL
                    where USUARIO=usernamex and CODSEDE=codsedex
                      --AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                      AND FECHA=TO_date(diajornadavarcharx,'YYYY-MM-DD')
                )B ON A.COD_CLIENTE=B.CODCLIENTE AND A.COD_LOCAL=B.CODLOCAL AND A.COD_LISTA=B.CODLISTA AND A.ESTADO=B.ESTADOCLIENTE AND A.DIRECCION=B.DIRECCION AND A.NEW_TELEFONO=B.NEW_TELEFONO
            WHERE B.CODCLIENTE IS  NULL  OR A.NEW_TELEFONO<>NVL(B.NEW_TELEFONO,0);

            if countnewx=0 then
                open clientesx for
                    SELECT UC.CODCLIENTE,UC.DESCCLIENTE,UC.DIRECCION,LC.NEW_COORDENADAS,UC.CODRUTA,UC.DESCRUTA,UC.CODLOCAL,UC.CODLISTA,UC.CODEMPRESA,
                           CASE WHEN UC.ISNEW IS NOT NULL THEN 'NEW'
                                WHEN UC.IDNOPEDIDO IS NOT NULL THEN 'N'
                                WHEN UC.NROPEDIDO IS NOT NULL THEN 'P'
                                ELSE '0' END AS FLAGESTADO,
                           CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                           ESTADO ESTADOCLIENTE,TO_CHAR(FECHA,'YYYY-MM-DD') FECHA,UC.NEW_TELEFONO,uc.nombres,uc.apellidos,uc.new_direccion,uc.is_auditado
                    FROM SYSADM.TBL_USUARIO_JORNADA UC
                             LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UC.CODCLIENTE AND LC.COD_LOCAL=UC.CODLOCAL
                    where USUARIO=usernamex and CODSEDE=codsedex
                      --AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                      AND FECHA=TO_DATE(diajornadavarcharx,'YYYY-MM-DD')
                      AND ESTADO IS NOT NULL ;-- SE AGREGO POR QUE QUITAN RUTAS A LOS USUARIOS /CLIENTES DE RUTA ANTIGUA VOLANDO

            else

                OPEN clientesx FOR
                    SELECT A.*
                    FROM(
                            SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                            LC.NEW_COORDENADAS,
                                            LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA ,C.STATUS_CLIENTE as ESTADO,C.NEW_TELEFONO,C.NOMBRES,C.APELLIDOS,LC.NEW_DIRECCION,LC.IS_AUDITADO
                            FROM SYSADM.CLIENTE C
                                     INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                                     INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                                     INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                                     INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                                     INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'

                                     INNER JOIN SYSADM.PRECIOS_C PC ON PC.COMPANIA_VENTA=CL.COMPANIA_VENTA AND PC.COD_LISTA=CL.COD_LISTA AND PC.CODSEDE=codsedex -- se agrego para q solo aparezcan clientes con listas de la sede del vendedor



                            WHERE 1=1
                              AND RV.DIA=(select translate(SUBSTR(to_char(diajornadax, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                          from dual)
                              AND UM.USUARIO=usernamex
                              AND C.STATUS_CLIENTE IN ('A','P')
                              AND LC.ESTAD='A'

                              AND nvl(C.USUAR_REGIS,'NONE')<>'EC' --EXCLUIR CLIENTES DIGITAL
                        )A
                            LEFT JOIN
                        (
                            SELECT UC.CODCLIENTE,UC.DESCCLIENTE,UC.DIRECCION,LC.NEW_COORDENADAS,UC.CODRUTA,UC.DESCRUTA,UC.CODLOCAL,UC.CODLISTA,UC.CODEMPRESA,
                                   CASE WHEN UC.ISNEW IS NOT NULL THEN 'NEW'
                                        WHEN UC.IDNOPEDIDO IS NOT NULL THEN 'N'
                                        WHEN UC.NROPEDIDO IS NOT NULL THEN 'P'
                                        ELSE '0' END AS FLAGESTADO,
                                   CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                                   ESTADO ESTADOCLIENTE,UC.NEW_TELEFONO,UC.NOMBRES,UC.APELLIDOS,UC.NEW_DIRECCION,UC.IS_AUDITADO
                            FROM SYSADM.TBL_USUARIO_JORNADA UC
                                     LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UC.CODCLIENTE AND LC.COD_LOCAL=UC.CODLOCAL
                            where USUARIO=usernamex and CODSEDE=codsedex
                              --AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                              AND FECHA=TO_DATE(diajornadavarcharx,'YYYY-MM-DD')
                        )B ON A.COD_CLIENTE=B.CODCLIENTE AND A.COD_LOCAL=B.CODLOCAL AND A.COD_LISTA=B.CODLISTA AND A.ESTADO=B.ESTADOCLIENTE AND A.DIRECCION=B.DIRECCION AND A.NEW_TELEFONO=B.NEW_TELEFONO
                    WHERE B.CODCLIENTE IS  NULL OR A.NEW_TELEFONO<>NVL(B.NEW_TELEFONO,0);

                LOOP
                    FETCH clientesx INTO codclientex,descclientex,direccionx,coordenadaoldx,codrutax,descrutax,codlocalx,codlistax,codempresax,estadox,nrotelefonox,nombresx,apellidosx,newdireccionx,isauditadox;
                    EXIT WHEN clientesx%NOTFOUND;


                    SELECT COUNT(*) INTO countauxx
                    FROM SYSADM.TBL_USUARIO_JORNADA
                    WHERE 1=1
                      AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                      AND USUARIO=usernamex and CODSEDE=codsedex and CODLOCAL=codlocalx
                      AND CODCLIENTE=codclientex AND CODRUTA=codrutax ;--AND ESTADO=estadox;

                    IF   countauxx=0 THEN

                        INSERT INTO SYSADM.TBL_USUARIO_JORNADA(USUARIO,CODSEDE,FECHA,CODCLIENTE,DESCCLIENTE,CODLOCAL,CODRUTA,DESCRUTA,DIRECCION,
                                                               COORDENADA_ANT,CODLISTA,CODEMPRESA,ESTADO,FECHA_TRANS,NEW_TELEFONO,NOMBRES,APELLIDOS,NEW_DIRECCION,IS_AUDITADO)
                        VALUES(usernamex,codsedex,to_date(SUBSTR(diajornadax,0,10)),codclientex,descclientex,codlocalx,codrutax,descrutax,direccionx,
                               coordenadaoldx,codlistax,codempresax,estadox,SYSDATE,nrotelefonox,nombresx,apellidosx,newdireccionx,isauditadox);

                    ELSE
                        UPDATE SYSADM.TBL_USUARIO_JORNADA
                        SET CODLISTA=codlistax ,ESTADO=estadox,DIRECCION=direccionx ,NEW_TELEFONO=nrotelefonox
                        WHERE CODCLIENTE=codclientex AND CODSEDE=codsedex and CODLOCAL=codlocalx
                          --AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                          AND FECHA=TO_DATE(diajornadavarcharx,'YYYY-MM-DD')
                          AND USUARIO=usernamex;

                    END IF;

                END LOOP;
                CLOSE clientesx;

                OPEN clientesx FOR
                    SELECT UC.CODCLIENTE,UC.DESCCLIENTE,UC.DIRECCION,LC.NEW_COORDENADAS,UC.CODRUTA,UC.DESCRUTA,UC.CODLOCAL,UC.CODLISTA,UC.CODEMPRESA,
                           CASE WHEN UC.IDNOPEDIDO IS NOT NULL THEN 'N'
                                WHEN UC.NROPEDIDO IS NOT NULL THEN 'P'
                                ELSE '0' END AS FLAGESTADO,
                           CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                           ESTADO ESTADOCLIENTE,TO_CHAR(FECHA,'YYYY-MM-DD') FECHA,uc.new_telefono,uc.nombres,uc.apellidos,uc.new_direccion,uc.is_auditado
                    FROM SYSADM.TBL_USUARIO_JORNADA UC
                             LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UC.CODCLIENTE AND LC.COD_LOCAL=UC.CODLOCAL
                    where USUARIO=usernamex AND CODSEDE=codsedex
                      --AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                      AND FECHA=TO_DATE(diajornadavarcharx,'YYYY-MM-DD')
                      AND ESTADO IS NOT NULL  and rownum<10;-- SE AGREGO POR QUE QUITAN RUTAS A LOS USUARIOS /CLIENTES DE RUTA ANTIGUA VOLANDO


            end if;

        end if;
        --rptax :=1;
--mensajex :=' ';
        commit;
    EXCEPTION
        WHEN OTHERS THEN
            DBMS_OUTPUT.PUT_LINE('SE ENCONTRÓ UN ERROR: '||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE
                );
            --rptax := -1;
            rollback;
--mensajex :='SE ENCONTRÓ UN ERROR: '||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;


    END CLIENTES_BY_DIAV2;

    FUNCTION  "ALMACENESBYLOCALIDAD"
    (
        PCODEMPRESA varchar2,
        PCODLOCALIDAD varchar2
    ) return types.ref_cursor is
        Result types.ref_cursor;
    BEGIN
        --if (pcodempresa='05' and pcodlocalidad='40') then

        /*OPEN Result FOR
    SELECT CODALMACEN,DESCALMACEN FROM SYSADM.TB_ALMACEN
		WHERE CODEMPRESA = PCODEMPRESA AND CODLOCALIDAD in ( '40','45') ORDER BY 2 ASC;*/
/*else

  OPEN Result FOR
    SELECT CODALMACEN,DESCALMACEN FROM SYSADM.TB_ALMACEN
		WHERE CODEMPRESA = PCODEMPRESA AND CODLOCALIDAD = PCODLOCALIDAD ORDER BY 2 ASC;
end if;*/
        OPEN Result FOR
            SELECT CODALMACEN,DESCALMACEN FROM SYSADM.TB_ALMACEN
            WHERE CODEMPRESA = PCODEMPRESA AND CODLOCALIDAD = PCODLOCALIDAD ORDER BY 2 ASC;
        return(Result);
    end ALMACENESBYLOCALIDAD;




    PROCEDURE REGISTRARTOKEN(usuariox IN VARCHAR2, tokenx IN VARCHAR2,macx IN VARCHAR2,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2  )
        IS
        cantx NUMBER;
    BEGIN


        /*
select count(*) INTO cantx
from SYSADM."TBL_TOKEN_USUARIO_copy"
where MAC=macx;*/


        INSERT INTO  SYSADM."TBL_TOKEN_USUARIO_copy"
        (MAC,TOKEN,
         USUARIO)
        values (
                   macx,
                   tokenx,
                   usuariox);
/*else

UPDATE SYSADM."TBL_TOKEN_USUARIO_copy"
SET TOKEN=tokenx,USUARIO=usuariox
WHERE MAC=macx;

end if;*/

        rptax := 1;
        mensajex :='SE REGISTRÓ CORRECTAMENTE ';
        commit;


    EXCEPTION

        WHEN OTHERS THEN
            rptax := -1;
            rollback;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

    END REGISTRARTOKEN;



    FUNCTION "GETTOKENS"
    (usuariosx VALUE_ARRAY_STRING
    )
        return types.ref_cursor is
        Result types.ref_cursor;

    begin

        open Result for
            SELECT  TOKEN
            FROM SYSADM."TBL_TOKEN_USUARIO_copy"
            WHERE 1=1 AND USUARIO IN (select * from table(usuariosx));

        return(Result);
    end GETTOKENS;


    PROCEDURE VALIDATEDISTANCIA(usuariox IN VARCHAR2,coodenada1x IN VARCHAR2,coodenada2x IN VARCHAR2,rptax OUT NUMBER ,mensajex OUT VARCHAR2)
        is
        distanciax  NUMBER;
        limitemesax NUMBER;
        codmesax varchar2(10);
        latitudx1 varchar2(100);
        longitudx1 varchar2(100);
        latitudx2 varchar2(100);
        longitudx2 varchar2(100);
        limiteusuariox NUMBER;
        limitefinalx NUMBER;
        ignorerangox CHAR(1);
    begin

        select
            TO_NUMBER
                ((SELECT CC.CC
                  FROM(SELECT BB.IDCOL,BB.CC
                       FROM(select ROWNUM AS IDCOL, fechas AS CC
                            FROM(select regexp_substr(coodenada1x,'[^,]+', 1, level) as fechas
                                 from dual connect by regexp_substr(coodenada1x, '[^,]+', 1, level) is not null)AA )BB WHERE BB.IDCOL=1)CC
                 ),'99.9999999999999999999999999' ) AS LAT1,
            TO_NUMBER((SELECT CC.CC
                       FROM(SELECT BB.IDCOL,BB.CC
                            FROM(select ROWNUM AS IDCOL, fechas AS CC
                                 FROM(select regexp_substr(coodenada1x,'[^,]+', 1, level) as fechas from dual
                                      connect by regexp_substr(coodenada1x, '[^,]+', 1, level) is not null)AA )BB WHERE BB.IDCOL=2)CC
                      ),'99.9999999999999999999999999') AS LONG1  into latitudx1,longitudx1
        FROM DUAL;


        select
            TO_NUMBER((SELECT CC.CC
                       FROM(SELECT BB.IDCOL,BB.CC
                            FROM(select ROWNUM AS IDCOL, fechas AS CC
                                 FROM(select regexp_substr(coodenada2x,'[^,]+', 1, level) as fechas
                                      from dual connect by regexp_substr(coodenada2x, '[^,]+', 1, level) is not null)AA )BB WHERE BB.IDCOL=1)CC
                      ) ,'99.9999999999999999999999999')AS LAT2,
            TO_NUMBER((SELECT CC.CC
                       FROM(SELECT BB.IDCOL,BB.CC
                            FROM(select ROWNUM AS IDCOL, fechas AS CC
                                 FROM(select regexp_substr(coodenada2x,'[^,]+', 1, level) as fechas from dual
                                      connect by regexp_substr(coodenada2x, '[^,]+', 1, level) is not null)AA )BB WHERE BB.IDCOL=2)CC
                      ) ,'99.9999999999999999999999999')AS LONG2  into latitudx2,longitudx2
        FROM DUAL;

        SELECT distinct CODMESA into codmesax
        FROM SYSADM.TB_USUARIO_MESA WHERE USUARIO=usuariox and TB_TIPO_CODIGO='R';


        SELECT NVL(IGNORE_RANGO,0) INTO  ignorerangox
        FROM SYSADM.USUARIOS
        WHERE USUARIO=usuariox;

        IF ignorerangox=1 THEN
            rptax:= 1;
            mensajex:='CORRECTO';

        ELSE

            SELECT NVL(RANGO_METROS,0) INTO limiteusuariox
            FROM SYSADM.USUARIOS
            WHERE USUARIO=usuariox;
            IF limiteusuariox=0 THEN
                select rango_metros into  limitemesax
                from SYSADM.TB_MESA where CODLOCALIDAD=codmesax;

                limitefinalx:=limitemesax;
            ELSE
                limitefinalx:=limiteusuariox;
            END IF;



            SELECT ROUND(ACOS(COS((90-(latitudx1))*(3.141592654/180)) *COS((90-(latitudx2))*(3.141592654/180)) +SIN((90-latitudx1)*(3.141592654/180)) *SIN((90-latitudx2)*(3.141592654/180)) *COS((longitudx1-longitudx2)*(3.141592654/180)))*6371*1000,2) into distanciax
            FROM SYSADM.DUAL;

            if distanciax <=limitefinalx THEN
                rptax:=1;
                mensajex:='CORRECTO  '||distanciax||'m';

            ELSE
                rptax:= 0;
                mensajex:='DEBES ESTAR A '||limitemesax || 'm Y ESTAS A '||distanciax||'m';
            end if;

        END IF;



    END VALIDATEDISTANCIA;


---SGC PLANIFICADOR
    FUNCTION CANALESBYMESA(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2)return TYPES.ref_cursor
        is
        Result types.ref_cursor;
    BEGIN
        OPEN Result FOR
            select distinct C.CODCANAL,DESCCANAL
            from SYSADM.TB_ASIGNACION A
                     INNER JOIN SYSADM.TB_CANAL c --ON A.TB_COD_EMPRESA=codempx  AND A.TB_COD_SEDE=codsedex
                                ON A.TB_COD_MESA=codmesax AND A.TB_COD_CANAL=C.CODCANAL AND C.CODCANAL<>8;
/*select distinct UM.CODCANAL,DESCCANAL
 from SYSADM.TB_USUARIO_MESA um
INNER JOIN SYSADM.TB_CANAL c on UM.CODCANAL=c.CODCANAL
where UM.CODMESA=codmesax ;--and UM.CODCANAL<>2;*/

        return(Result);
    END CANALESBYMESA ;



    FUNCTION TIPOSCOLPLANIF return TYPES.ref_cursor
        is
        Result types.ref_cursor;
    BEGIN
        OPEN Result FOR
            select distinct  TCP.CODTIPOCOL,TCP.DESCTIPO
            from SYSADM.TIPO_COL_PLANIFICADOR TCP;
        return(Result);
    END TIPOSCOLPLANIF ;


    FUNCTION VALORESBYTIPOSCOL(codproveedorx IN VARCHAR2, codtipocolx IN VARCHAR2)return TYPES.ref_cursor
        is
        Result types.ref_cursor;
    BEGIN

        if codtipocolx='CAT' THEN
            OPEN Result FOR
                SELECT CODCATEGORIA,NOMCATEGORIA
                FROM SYSADM.TB_CATEGORIA_PROVEEDOR
                WHERE CODPROVEEDOR=codproveedorx;

        ELSIF codtipocolx='SKU'   THEN
            OPEN Result FOR
                SELECT ART.COD_ITEM,ART.DESC_ITEM
                FROM SYSADM.ARTICULOS ART
                WHERE 1=1
                  AND ART.STATUS_ITEM='A'
                  AND ART.COD_LINEA<>'99'
                  AND ART.PROVEEDOR_DEFAULT=codproveedorx;

        ELSIF codtipocolx='PRO'  THEN
            OPEN Result FOR
                SELECT C_PROVEEDOR,X_PROVEEDOR
                FROM SYSADM.PROVEEDOR
                WHERE  proveedor.C_proveedor=codproveedorx;
        END IF;
        return(Result);
    END VALORESBYTIPOSCOL ;




    PROCEDURE  MANTPLANIFICADOR(idplanificadorx IN NUMBER,codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2,codprovx IN VARCHAR2,codtipocolx IN VARCHAR2,codtipovalx IN VARCHAR2,
                                LIST_CODVALORES IN VALUE_ARRAY_STRING,LIST_VALORES IN VALUE_ARRAY_STRING,usuariox IN VARCHAR2,fechax IN VARCHAR2,operacionx in NUMBER,rptax        	IN OUT    NUMBER,mensajex IN OUT    VARCHAR2)
        IS

        provsegx VARCHAR2(100);
        countprovx  NUMBER;
    BEGIN


        IF operacionx=1 THEN
            FOR indx IN  LIST_VALORES.FIRST .. LIST_VALORES.LAST
                LOOP

                    INSERT INTO SYSADM.PLANIFICADOR(IDPLANIFICADOR,FECHA,DIA,CODEMPRESA,CODSEDE,CODMESA,CODCANAL,CODPROVEEDOR,CODTIPOCOL,CODTIPOVALOR,CODVALOR,VALORCOL,USUARIO,FECHATRANSACCION)
                    VALUES(XRAYADMIN.PLANIFICADOR_SEQUENCE.NEXTVAL,  TO_DATE(fechax,'dd/MM/yyyy'),
                           (select translate(SUBSTR(to_char(TO_DATE(fechax,'dd/MM/yyyy'), 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                            from dual)
                              ,codempx,codsedex,codmesax,codcanalx,codprovx,codtipocolx,codtipovalx,LIST_CODVALORES(indx),LIST_VALORES(indx),usuariox,SYSDATE);


                    select COUNT(DISTINCT PROVEEDORSEGUIM) INTO countprovx
                    from SYSADM.PLANIFICADOR
                    where CODEMPRESA=codempx
                      and codsede=codsedex
                      and codmesa=codmesax
                      and CODCANAL=codcanalx
                      and FECHA=TO_DATE(fechax,'dd/MM/yyyy')
                      and PROVEEDORSEGUIM is not null;

                    IF countprovx>0 then

                        select DISTINCT PROVEEDORSEGUIM INTO provsegx
                        from SYSADM.PLANIFICADOR
                        where CODEMPRESA=codempx
                          and codsede=codsedex
                          and codmesa=codmesax
                          and CODCANAL=codcanalx
                          and FECHA=TO_DATE(fechax,'dd/MM/yyyy')
                          and PROVEEDORSEGUIM is not null;


                        UPDATE SYSADM.PLANIFICADOR
                        SET PROVEEDORSEGUIM=provsegx
                        WHERE CODEMPRESA=codempx
                          and codsede=codsedex
                          and codmesa=codmesax
                          and CODCANAL=codcanalx
                          and FECHA=TO_DATE(fechax,'dd/MM/yyyy');

                    END IF;


                    rptax := 1;
                    mensajex :='SE REGISTRARON CORRECTAMENTE ' ;

                END LOOP;

            commit;
            rptax := 1;
            mensajex :='SE REGISTRARON CORRECTAMENTE ' ;
        ELSif operacionx=2 THEN
            FOR indx IN  LIST_VALORES.FIRST .. LIST_VALORES.LAST
                LOOP

                    UPDATE SYSADM.PLANIFICADOR
                    SET CODPROVEEDOR=codprovx,CODTIPOCOL=codtipocolx ,CODTIPOVALOR=codtipovalx,codvalor=LIST_CODVALORES(indx)
                    WHERE IDPLANIFICADOR=idplanificadorx AND CODEMPRESA=codempx AND CODSEDE=codsedex
                      AND CODMESA=codmesax and CODCANAL=codcanalx
                      and FECHA=TO_DATE(fechax,'dd/mm/yyyy');

                    rptax := 2;
                    mensajex :='SE ACTUALIZARON CORRECTAMENTE ' ;

                END LOOP;

            commit;
            rptax := 2;
            mensajex :='SE ACTUALIZARON CORRECTAMENTE ' ;


        ELSif operacionx=3 THEN

            FOR indx IN  LIST_VALORES.FIRST .. LIST_VALORES.LAST
                LOOP

                    DELETE SYSADM.PLANIFICADOR
                    WHERE IDPLANIFICADOR=idplanificadorx AND CODEMPRESA=codempx AND CODSEDE=codsedex
                      AND CODMESA=codmesax and CODCANAL=codcanalx
                      and CODPROVEEDOR=codprovx and CODTIPOCOL=codtipocolx
                      AND CODTIPOVALOR=codtipovalx and codvalor=LIST_CODVALORES(indx) and FECHA=TO_DATE(fechax,'dd/mm/yyyy');

                    rptax := 1;
                    mensajex :='SE ELIMINARON CORRECTAMENTE ' ;

                END LOOP;

            commit;
            rptax := 1;
            mensajex :='SE ELIMINARON CORRECTAMENTE ' ;
        END IF;

    EXCEPTION

        WHEN OTHERS THEN
            rptax := -1;
            rollback;
            mensajex :='SE ENCONTRÓ UN ERROR: '||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
    END MANTPLANIFICADOR;



    FUNCTION LISTPLANIF(fechax IN VARCHAR2, codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2)return TYPES.ref_cursor
        is
        Result types.ref_cursor;
    BEGIN
        open Result for
            SELECT   to_char(PL.FECHA,'DD/MM/YYYY'),PL.CODEMPRESA,PL.CODSEDE,PL.CODMESA,PL.CODCANAL,PL.CODPROVEEDOR,
                     PR.X_PROVEEDOR ,PL.CODTIPOCOL,TCP.DESCTIPO,PL.CODTIPOVALOR,PL.CODVALOR,PL.VALORCOL,PL.IDPLANIFICADOR
            FROM "SYSADM"."PLANIFICADOR" PL
                     INNER JOIN SYSADM.PROVEEDOR PR ON PL.CODPROVEEDOR=PR.C_PROVEEDOR
                     INNER JOIN SYSADM.TIPO_COL_PLANIFICADOR TCP ON TCP.CODTIPOCOL=PL.CODTIPOCOL
            WHERE 1=1
              AND PL.FECHA=TO_DATE(fechax,'DD/MM/YYYY')
              AND PL.CODEMPRESA=codempx
              AND PL.CODSEDE=codsedex
              AND PL.CODMESA=codmesax
              AND PL.CODCANAL=codcanalx;

        RETURN Result;
    END LISTPLANIF;


    FUNCTION LISTUSERSBYMESA( codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2)return TYPES.ref_cursor
        is
        Result types.ref_cursor;
    BEGIN
        open Result for
            SELECT   DISTINCT UM.USUARIO,US.NOMBRE
            FROM SYSADM.TB_USUARIO_MESA UM
                     INNER JOIN  SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO
            WHERE 1=1
              AND UM.CODEMPRESA=codempx
              AND UM.CODSEDE=codsedex
              AND ESTUSUARIO='H'
              AND CODMESA=codmesax
              AND CODCANAL=codcanalx
              AND UM.TB_TIPO_CODIGO='R';

        RETURN Result;
    END LISTUSERSBYMESA;



    PROCEDURE GENERARPLANIFICADOR(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2,usuariox IN VARCHAR2,fechax IN VARCHAR2,codproveedorx IN VARCHAR2,modox IN VARCHAR2,
                                  fijosx OUT types.ref_cursor,configplanx OUT types.ref_cursor,colmesesx OUT types.ref_cursor,colprovx OUT types.ref_cursor,rutasx OUT VARCHAR2 )
        IS
        codvendx VARCHAR2(50);
        empx VARCHAR2(10);
        sedex VARCHAR2(10);
        mesax VARCHAR2(10);
        canalx VARCHAR2(10);
        proveedorx VARCHAR2(10);
        fecx VARCHAR2(10);
        countatendx NUMBER;
        diasemanax VARCHAR2(100);

        countprovsegx NUMBER;
    BEGIN



        SELECT WM_CONCAT(A1.DESCRUTA) INTO rutasx
        FROM
            (
                SELECT distinct R.DESCRUTA
                FROM SYSADM.CLIENTE C
                         INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                         INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                         INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                         INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                         INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
                WHERE 1=1
                  AND RV.DIA=(select translate(SUBSTR(to_char(SYSDATE, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                              from dual)
                  AND UM.USUARIO=usuariox
                  AND C.STATUS_CLIENTE='A'
                  AND LC.ESTAD='A'
                ORDER BY 1
            )A1;




        SELECT distinct CODVENDEDOR  into codvendx
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';



        IF modox='M' THEN

            SELECT distinct CODEMPRESA,CODSEDE,CODMESA,CODCANAL  into empx,sedex,mesax,canalx
            FROM SYSADM.TB_USUARIO_MESA
            WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';

            fecx:=to_char(sysdate,'dd/mm/yyyy');

            select distinct PROVEEDORSEGUIM into proveedorx
            FROM SYSADM.PLANIFICADOR
            WHERE CODEMPRESA=empx AND CODSEDE=sedex AND CODMESA=mesax AND CODCANAL=canalx
              AND FECHA=to_date(fecx,'dd/mm/yyyy');



            SELECT
                COUNT(DISTINCT CXC.CDCC_CLIENTE) INTO countatendx
            FROM SYSADM.DOCUMENTO_CXC CXC
            WHERE CXC.CDCC_COMPANIA=empx
              AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
              AND CXC.CDCC_TIPODOC IN ('01','02','05')
              AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
              AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
              AND CXC.CDCC_VENDEDOR=codvendx
              AND CXC.CDCC_CODIGO2 IN (SELECT CODRUTA FROM SYSADM.TB_RUTA_VENDEDOR
                                                               INNER JOIN SYSADM.TB_RUTA ON TB_RUTA.IDRUTA = TB_RUTA_VENDEDOR.IDRUTA
                                       WHERE 1=1
                                         AND DIA  = (select translate(SUBSTR(to_char(to_date(SYSDATE,'DD/MM/YY'), 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                                     from dual) and TB_RUTA_VENDEDOR.codvendedor=codvendx)
              AND CXC.CDCC_FUNCION IS NOT NULL
              And CXC.CDCC_VENDEDOR Not Like '99%'
              AND CXC.SDCC_STATUS <> 'AN' ;

            select translate(to_char(to_date(SYSDATE,'DD/MM/YY'), 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU') INTO diasemanax
            from dual;

            rutasx:=rutasx||'|'||countatendx||'|'||diasemanax;


            --end if;


        ELSE
            empx:=codempx;
            sedex:=codsedex;
            mesax:=codmesax;
            canalx:=codcanalx;
            proveedorx:=codproveedorx;
            fecx:=fechax;

            UPDATE SYSADM.PLANIFICADOR SET PROVEEDORSEGUIM=codproveedorx
            WHERE CODEMPRESA=codempx AND
                    CODSEDE=codsedex AND
                    CODMESA=codmesax AND
                    CODCANAL=codcanalx AND
                    FECHA=to_date(fecx,'dd/mm/yyyy');

            SELECT
                COUNT(DISTINCT CXC.CDCC_CLIENTE) INTO countatendx
            FROM SYSADM.DOCUMENTO_CXC CXC
            WHERE CXC.CDCC_COMPANIA=empx
              AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
              AND CXC.CDCC_TIPODOC IN ('01','02','05')
              AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
              AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
              AND CXC.CDCC_VENDEDOR=codvendx
              AND CXC.CDCC_CODIGO2 IN (SELECT CODRUTA FROM SYSADM.TB_RUTA_VENDEDOR
                                                               INNER JOIN SYSADM.TB_RUTA ON TB_RUTA.IDRUTA = TB_RUTA_VENDEDOR.IDRUTA
                                       WHERE 1=1
                                         AND DIA  = (select translate(SUBSTR(to_char(to_date(SYSDATE,'DD/MM/YY'), 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                                     from dual) and TB_RUTA_VENDEDOR.codvendedor=codvendx)
              AND CXC.CDCC_FUNCION IS NOT NULL
              And CXC.CDCC_VENDEDOR Not Like '99%'
              AND CXC.SDCC_STATUS <> 'AN' ;

            select translate(to_char(to_date(SYSDATE,'DD/MM/YY'), 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU') INTO diasemanax
            from dual;

            rutasx:=rutasx||'|'||countatendx||'|'||diasemanax;

        END IF;


        open colmesesx for
            select (translate(to_char(add_months(SYSDATE,-3), 'MONTH', 'NLS_DATE_LANGUAGE=SPANISH'), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU'))A,(translate(to_char(add_months(SYSDATE,-2), 'MONTH', 'NLS_DATE_LANGUAGE=SPANISH'), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU'))B,(translate(to_char(add_months(SYSDATE,-1), 'MONTH', 'NLS_DATE_LANGUAGE=SPANISH'), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU'))C,(translate(to_char(sysdate, 'MONTH', 'NLS_DATE_LANGUAGE=SPANISH'), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU'))D
            from SYSADM.dual;


        open fijosx for
            SELECT A.COD_CLIENTE,A.DESC_CLIENTE,A.DIRECCION,A.GEOLOCALIZADO,
                   CASE WHEN (A.TIENEDEUDA=1) THEN
                            (
                                SELECT DISTINCT
                                    CASE WHEN (SUM(CASE WHEN ((SYSDATE-CXC.FDCC_EMISION))>NVL(LPC.DIAS_PLAZO,0) THEN 1 ELSE 0 END))>0 THEN 'V' ELSE 'N/V' END ESTADO
                                FROM SYSADM.DOCUMENTO_CXC CXC
                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH
                                         LEFT JOIN SYSADM.SGC_COBRANZA sc on CXC.CDCC_COMPANIA=sc.codempresa and cxc.cdcc_secuencia=sc.secuenciadoc and sc.tipodocumento=cxc.cdcc_tipodoc and cxc.codsede=SC.codsede AND sc.estado_cobranza<>'A'
                                WHERE CXC.CDCC_COMPANIA=empx
                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                  AND MDCC_MONTO>0
                                  AND CDCC_CLIENTE=A.COD_CLIENTE)


                        ELSE
                            'S/D'
                       END AS ESTADODEUDA,
                   NVL((SELECT SUM(DET.MDEMO_TOTAL-DET.MDEMO_IGV) MONTO
                        FROM SYSADM.DOCUMENTO_CXC CXC
                                 INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                                 INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                        WHERE 1=1
                          AND CXC.CDCC_COMPANIA=empx
                          AND CXC.CDCC_CLIENTE=A.COD_CLIENTE
                          AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR((SELECT add_months(SYSDATE,-3) FROM SYSADM.DUAL),'MM')
                          AND CXC.CDCC_TIPODOC IN ('01','02','05')
                          AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                          AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                          AND AM.CODPROVEEDORLOG=proveedorx
                          AND CXC.CDCC_FUNCION IS NOT NULL
                          AND DET.CDEMO_ITEMART <> '000000'
                          And CXC.CDCC_VENDEDOR Not Like '99%'
                          AND CXC.SDCC_STATUS <> 'AN'
                       ),0)A,
                   NVL((SELECT SUM(DET.MDEMO_TOTAL-DET.MDEMO_IGV) MONTO
                        FROM SYSADM.DOCUMENTO_CXC CXC
                                 INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                                 INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                        WHERE 1=1
                          AND CXC.CDCC_COMPANIA=empx
                          AND CXC.CDCC_CLIENTE=A.COD_CLIENTE
                          AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR((SELECT add_months(SYSDATE,-2) FROM SYSADM.DUAL),'MM')
                          AND CXC.CDCC_TIPODOC IN ('01','02','05')
                          AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                          AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                          AND AM.CODPROVEEDORLOG=proveedorx
                          AND CXC.CDCC_FUNCION IS NOT NULL
                          AND DET.CDEMO_ITEMART <> '000000'
                          And CXC.CDCC_VENDEDOR Not Like '99%'
                          AND CXC.SDCC_STATUS <> 'AN'
                       ),0)B,
                   NVL((SELECT SUM(DET.MDEMO_TOTAL-DET.MDEMO_IGV) MONTO
                        FROM SYSADM.DOCUMENTO_CXC CXC
                                 INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                                 INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                        WHERE 1=1
                          AND CXC.CDCC_COMPANIA=empx
                          AND CXC.CDCC_CLIENTE=A.COD_CLIENTE
                          AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR((SELECT add_months(SYSDATE,-1) FROM SYSADM.DUAL),'MM')
                          AND CXC.CDCC_TIPODOC IN ('01','02','05')
                          AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                          AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                          AND AM.CODPROVEEDORLOG=proveedorx
                          AND CXC.CDCC_FUNCION IS NOT NULL
                          AND DET.CDEMO_ITEMART <> '000000'
                          And CXC.CDCC_VENDEDOR Not Like '99%'
                          AND CXC.SDCC_STATUS <> 'AN'
                       ),0)C,
                   NVL((SELECT SUM(DET.MDEMO_TOTAL-DET.MDEMO_IGV) MONTO
                        FROM SYSADM.DOCUMENTO_CXC CXC
                                 INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                                 INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                        WHERE 1=1
                          AND CXC.CDCC_COMPANIA=empx
                          AND CXC.CDCC_CLIENTE=A.COD_CLIENTE
                          AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
                          AND CXC.CDCC_TIPODOC IN ('01','02','05')
                          AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                          AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                          AND AM.CODPROVEEDORLOG=proveedorx
                          AND CXC.CDCC_VENDEDOR=codvendx
                          AND CXC.CDCC_FUNCION IS NOT NULL
                          AND DET.CDEMO_ITEMART <> '000000'
                          And CXC.CDCC_VENDEDOR Not Like '99%'
                          AND CXC.SDCC_STATUS <> 'AN'
                       ),0) D

            FROM
                (
                    SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                    CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END GEOLOCALIZADO,
                                    (
                                        SELECT DISTINCT
                                            CASE WHEN SUM(MDCC_SALDO)>0 THEN 1 ELSE 0 END TIENEDEUDA
                                        FROM SYSADM.DOCUMENTO_CXC CXC
                                                 INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH
                                                 LEFT JOIN SYSADM.SGC_COBRANZA sc on CXC.CDCC_COMPANIA=sc.codempresa and cxc.cdcc_secuencia=sc.secuenciadoc and sc.tipodocumento=cxc.cdcc_tipodoc and cxc.codsede=SC.codsede AND sc.estado_cobranza<>'A'
                                        WHERE CXC.CDCC_COMPANIA=empx
                                          AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                          and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                          AND MDCC_MONTO>0
                                          AND CDCC_CLIENTE=C.COD_CLIENTE

                                    ) TIENEDEUDA
                    FROM SYSADM.CLIENTE C
                             INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                             INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                             INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                             INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                             INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
                    WHERE 1=1
                      AND RV.DIA=(select translate(SUBSTR(to_char(SYSDATE, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                  from dual)
                      AND UM.USUARIO=usuariox
                      AND C.STATUS_CLIENTE='A'
                      AND LC.ESTAD='A'
--AND C.COD_CLIENTE='DNI17526827'
                    ORDER BY 2
                )A;



        open configplanx for
            select  CODPROVEEDOR,CODTIPOCOL,CODTIPOVALOR,CODVALOR,VALORCOL --CODPROVEEDOR,CODTIPOCOL,CODTIPOVALOR,CODVALOR,VALORCOL
            FROM SYSADM.PLANIFICADOR
            WHERE CODEMPRESA=empx
              AND CODSEDE=sedex
              AND CODMESA=mesax
              AND CODCANAL=canalx AND FECHA=to_date(fecx,'dd/mm/yyyy') ORDER BY CODPROVEEDOR,CODTIPOCOL,VALORCOL  ASC;

        OPEN colprovx FOR
            select distinct  PL.CODPROVEEDOR,PR.X_PROVEEDOR
            FROM SYSADM.PLANIFICADOR PL INNER JOIN SYSADM.PROVEEDOR PR ON PL.CODPROVEEDOR=PR.C_PROVEEDOR
            WHERE PL.CODEMPRESA=empx
              AND PL.CODSEDE=sedex
              AND PL.CODMESA=mesax
              AND PL.CODCANAL=canalx
              AND PL.FECHA=to_date(fecx,'dd/mm/yyyy') order by PL.CODPROVEEDOR asc;


    END GENERARPLANIFICADOR;

    FUNCTION  "GETVALBYCLIENTE" (codempx in varchar2,tipocolx varchar2,tipovalx in varchar2,valorx in varchar2,usuariox IN VARCHAR2,codclientex IN VARCHAR2,modox IN VARCHAR2 ) return VARCHAR2 is
        --montox  NUMBER(10,2);
        montocadenax varchar2(100);
        codvendx VARCHAR2(10);
        countx NUMBER;
        empx VARCHAR2(10);
        montopedidocadx varchar2(100);
        cantpedidoaux number;
    begin

        SELECT distinct CODVENDEDOR  INTO codvendx
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';



        IF modox='M' THEN
            SELECT distinct CODEMPRESA  into empx
            FROM SYSADM.TB_USUARIO_MESA
            WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';

        ELSE
            empx:=codempx;
        END IF;


        IF tipocolx='CAT' THEN
            IF tipovalx='S' THEN

                SELECT count(*) INTO countx
                FROM SYSADM.DOCUMENTO_CXC CXC
                         INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                         INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                    AND CXC.CDCC_COMPANIA=empx
                    AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
                    AND CXC.CDCC_TIPODOC IN ('01','02','05')
                    AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                    AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                    AND AM.CODPROVEEDOR=valorx
                    AND CXC.CDCC_VENDEDOR=codvendx
                    AND CXC.CDCC_CLIENTE=codclientex
                    AND CXC.CDCC_FUNCION IS NOT NULL
                    AND DET.CDEMO_ITEMART <> '000000'
                    And CXC.CDCC_VENDEDOR Not Like '99%'
                    AND CXC.SDCC_STATUS <> 'AN' ;


                IF countx=0 THEN
                    montocadenax:='0';
                ELSE

                    SELECT  NVL(SUM(DET.MDEMO_TOTAL-DET.MDEMO_IGV),0) INTO montocadenax
                    FROM SYSADM.DOCUMENTO_CXC CXC
                             INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                             INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                        AND CXC.CDCC_COMPANIA=empx
                        AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
                        AND CXC.CDCC_TIPODOC IN ('01','02','05')
                        AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                        AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                        AND AM.CODPROVEEDOR=valorx
                        AND CXC.CDCC_VENDEDOR=codvendx
                        AND CXC.CDCC_CLIENTE=codclientex
                        AND CXC.CDCC_FUNCION IS NOT NULL
                        AND DET.CDEMO_ITEMART <> '000000'
                        And CXC.CDCC_VENDEDOR Not Like '99%'
                        AND CXC.SDCC_STATUS <> 'AN' ;


                    SELECT  count(*) into cantpedidoaux
                    FROM
                        SYSADM.SPEDIDO_HEADER SPH
                            INNER JOIN SYSADM.SPEDIDO_DETALLE SPD ON  SPH.COMPANIA_VENTA=SPD.COMPANIA_VENTA  AND SPH.NRO_PEDIDO=SPD.NRO_PEDIDO
                            INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=SPD.COD_ITEM
                    WHERE 1=1
                      AND SPH.COD_CIA='00'
                      AND SPH.COMPANIA_VENTA=empx
                      AND TO_CHAR(SPH.FECHA_PEDIDO,'yyyy-mm-dd')=TO_CHAR(SYSDATE,'yyyy-mm-dd')
                      AND  SPH.COD_VENDEDOR=codvendx
                      AND AM.CODPROVEEDOR=valorx
                      AND SPH.COD_CLIENTE=codclientex
                      AND SPD.COD_ITEM <> '000000'
                      And SPH.COD_VENDEDOR Not Like '99%'
                      AND SPH.STATUS_PEDIDO = 'G' ;


                    if cantpedidoaux>0 then

                        SELECT  NVL(SUM(SPD.VENTA_NETA-SPD.IMPUESTO_IGV),0)||'' into montopedidocadx
                        FROM
                            SYSADM.SPEDIDO_HEADER SPH
                                INNER JOIN SYSADM.SPEDIDO_DETALLE SPD ON SPH.COD_CIA=SPD.COD_CIA AND SPH.COMPANIA_VENTA=SPD.COMPANIA_VENTA  AND SPH.NRO_PEDIDO=SPD.NRO_PEDIDO
                                INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=SPD.COD_ITEM
                        WHERE 1=1
                          AND SPH.COD_CIA='00'
                          AND SPH.COMPANIA_VENTA=empx
                          AND TO_CHAR(SPH.FECHA_PEDIDO,'yyyy-mm-dd')=TO_CHAR(SYSDATE,'yyyy-mm-dd')
                          AND  SPH.COD_VENDEDOR=codvendx
                          AND AM.CODPROVEEDOR=valorx
                          AND SPH.COD_CLIENTE=codclientex
                          AND SPD.COD_ITEM <> '000000'
                          And SPH.COD_VENDEDOR Not Like '99%'
                          AND SPH.STATUS_PEDIDO = 'G'
                        GROUP BY SPH.COD_CLIENTE,AM.CODPROVEEDOR;
                        montocadenax:=montocadenax||'+'||montopedidocadx;
                    ELSE
                        montocadenax:=montocadenax;
                    end if;



                end if;

            ELSE
                SELECT count(DISTINCT NDCC_PEDIDOOIH)||'' INTO montocadenax
                FROM SYSADM.DOCUMENTO_CXC CXC
                         INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                         INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                    AND CXC.CDCC_COMPANIA=empx
                    AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
                    AND CXC.CDCC_TIPODOC IN ('01','02','05')
                    AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                    AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                    AND AM.CODPROVEEDOR=valorx
                    AND CXC.CDCC_VENDEDOR=codvendx
                    AND CXC.CDCC_CLIENTE=codclientex
                    AND CXC.CDCC_FUNCION IS NOT NULL
                    AND DET.CDEMO_ITEMART <> '000000'
                    And CXC.CDCC_VENDEDOR Not Like '99%'
                    AND CXC.SDCC_STATUS <> 'AN' ;

            END IF;
        ELSIF tipocolx='PRO' THEN
            IF tipovalx='S' THEN

                SELECT count(*) INTO countx
                FROM SYSADM.DOCUMENTO_CXC CXC
                         INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                         INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                    AND CXC.CDCC_COMPANIA=empx
                    AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
                    AND CXC.CDCC_TIPODOC IN ('01','02','05')
                    AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                    AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                    AND AM.CODPROVEEDORLOG=valorx
                    AND CXC.CDCC_VENDEDOR=codvendx
                    AND CXC.CDCC_CLIENTE=codclientex
                    AND CXC.CDCC_FUNCION IS NOT NULL
                    AND DET.CDEMO_ITEMART <> '000000'
                    And CXC.CDCC_VENDEDOR Not Like '99%'
                    AND CXC.SDCC_STATUS <> 'AN' ;


                IF countx=0 THEN
                    montocadenax:='0';
                ELSE

                    SELECT  NVL(SUM(DET.MDEMO_TOTAL-DET.MDEMO_IGV),0) INTO montocadenax
                    FROM SYSADM.DOCUMENTO_CXC CXC
                             INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                             INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                        AND CXC.CDCC_COMPANIA=empx
                        AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
                        AND CXC.CDCC_TIPODOC IN ('01','02','05')
                        AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                        AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                        AND AM.CODPROVEEDORLOG=valorx
                        AND CXC.CDCC_VENDEDOR=codvendx
                        AND CXC.CDCC_CLIENTE=codclientex
                        AND CXC.CDCC_FUNCION IS NOT NULL
                        AND DET.CDEMO_ITEMART <> '000000'
                        And CXC.CDCC_VENDEDOR Not Like '99%'
                        AND CXC.SDCC_STATUS <> 'AN' ;


                    SELECT  count(*) into cantpedidoaux
                    FROM
                        SYSADM.SPEDIDO_HEADER SPH
                            INNER JOIN SYSADM.SPEDIDO_DETALLE SPD ON  SPH.COMPANIA_VENTA=SPD.COMPANIA_VENTA  AND SPH.NRO_PEDIDO=SPD.NRO_PEDIDO
                            INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=SPD.COD_ITEM
                    WHERE 1=1
                      AND SPH.COD_CIA='00'
                      AND SPH.COMPANIA_VENTA=empx
                      AND TO_CHAR(SPH.FECHA_PEDIDO,'yyyy-mm-dd')=TO_CHAR(SYSDATE,'yyyy-mm-dd')
                      AND  SPH.COD_VENDEDOR=codvendx
                      AND AM.CODPROVEEDORLOG=valorx
                      AND SPH.COD_CLIENTE=codclientex
                      AND SPD.COD_ITEM <> '000000'
                      And SPH.COD_VENDEDOR Not Like '99%'
                      AND SPH.STATUS_PEDIDO = 'G' ;


                    if cantpedidoaux>0 then

                        SELECT  NVL(SUM(SPD.VENTA_NETA-SPD.IMPUESTO_IGV),0)||'' into montopedidocadx
                        FROM
                            SYSADM.SPEDIDO_HEADER SPH
                                INNER JOIN SYSADM.SPEDIDO_DETALLE SPD ON SPH.COD_CIA=SPD.COD_CIA AND SPH.COMPANIA_VENTA=SPD.COMPANIA_VENTA  AND SPH.NRO_PEDIDO=SPD.NRO_PEDIDO
                                INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=SPD.COD_ITEM
                        WHERE 1=1
                          AND SPH.COD_CIA='00'
                          AND SPH.COMPANIA_VENTA=empx
                          AND TO_CHAR(SPH.FECHA_PEDIDO,'yyyy-mm-dd')=TO_CHAR(SYSDATE,'yyyy-mm-dd')
                          AND  SPH.COD_VENDEDOR=codvendx
                          AND AM.CODPROVEEDORLOG=valorx
                          AND SPH.COD_CLIENTE=codclientex
                          AND SPD.COD_ITEM <> '000000'
                          And SPH.COD_VENDEDOR Not Like '99%'
                          AND SPH.STATUS_PEDIDO = 'G'
                        GROUP BY SPH.COD_CLIENTE,AM.CODPROVEEDOR;
                        montocadenax:=montocadenax||'+'||montopedidocadx;
                    ELSE
                        montocadenax:=montocadenax;
                    end if;



                end if;

            ELSE
                SELECT count(DISTINCT NDCC_PEDIDOOIH)||'' INTO montocadenax
                FROM SYSADM.DOCUMENTO_CXC CXC
                         INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                         INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                    AND CXC.CDCC_COMPANIA=empx
                    AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
                    AND CXC.CDCC_TIPODOC IN ('01','02','05')
                    AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                    AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                    AND AM.CODPROVEEDORLOG=valorx
                    AND CXC.CDCC_VENDEDOR=codvendx
                    AND CXC.CDCC_CLIENTE=codclientex
                    AND CXC.CDCC_FUNCION IS NOT NULL
                    AND DET.CDEMO_ITEMART <> '000000'
                    And CXC.CDCC_VENDEDOR Not Like '99%'
                    AND CXC.SDCC_STATUS <> 'AN' ;

            END IF;
        ELSIF tipocolx='SKU' THEN

            if tipovalx='S' THEN
                SELECT    count(*) INTO countx
                FROM SYSADM.DOCUMENTO_CXC CXC
                         INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                         INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                    AND CXC.CDCC_COMPANIA=empx
                    AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
                    AND CXC.CDCC_TIPODOC IN ('01','02','05')
                    AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                    AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                    AND CXC.CDCC_VENDEDOR=codvendx
                    AND DET.CDEMO_ITEMART=valorx
                    AND CXC.CDCC_CLIENTE=codclientex
                    AND CXC.CDCC_FUNCION IS NOT NULL
                    AND DET.CDEMO_ITEMART <> '000000'
                    And CXC.CDCC_VENDEDOR Not Like '99%'
                    AND CXC.SDCC_STATUS <> 'AN';

                if countx=0 THEN
                    montocadenax:='0';

                ELSE

                    SELECT    NVL(SUM(DET.MDEMO_TOTAL-DET.MDEMO_IGV),0) INTO montocadenax
                    FROM SYSADM.DOCUMENTO_CXC CXC
                             INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                             INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                        AND CXC.CDCC_COMPANIA=empx
                        AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
                        AND CXC.CDCC_TIPODOC IN ('01','02','05')
                        AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                        AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                        AND CXC.CDCC_VENDEDOR=codvendx
                        AND DET.CDEMO_ITEMART=valorx--'530279'
                        AND CXC.CDCC_CLIENTE=codclientex
                        AND CXC.CDCC_FUNCION IS NOT NULL
                        AND DET.CDEMO_ITEMART <> '000000'
                        And CXC.CDCC_VENDEDOR Not Like '99%'
                        AND CXC.SDCC_STATUS <> 'AN';
                    -- GROUP BY CXC.CDCC_CLIENTE,DET.CDEMO_ITEMART;

                end if;
            ELSE
                SELECT count(DISTINCT NDCC_PEDIDOOIH)||'' INTO montocadenax
                FROM SYSADM.DOCUMENTO_CXC CXC
                         INNER JOIN SYSADM.DOCCXC_MOTIVO DET ON CXC.CDCC_COMPANIA= DET.CDEMO_COMPANIADCC AND CXC.CDCC_TIPODOC=DET.CDEMO_TIPODOCDCC AND CXC.CDCC_SECUENCIA=DET.CDEMO_SECUENCIADCC
                         INNER JOIN SYSADM.ARTICULO_MYSQL AM ON AM.CODARTICULO=DET.CDEMO_ITEMART
                    AND CXC.CDCC_COMPANIA=empx
                    AND TO_CHAR(CXC.FDCC_EMISION,'MM') =TO_CHAR(SYSDATE,'MM')
                    AND CXC.CDCC_TIPODOC IN ('01','02','05')
                    AND CXC.CDCC_VENDEDOR NOT IN('193','P41','984')
                    AND CXC.CDCC_CLIENTE NOT IN('20103381402','20353607783','20271522950','20531321970','20353607864')
                    AND CXC.CDCC_VENDEDOR=codvendx
                    AND DET.CDEMO_ITEMART=valorx
                    AND CXC.CDCC_CLIENTE=codclientex
                    AND CXC.CDCC_FUNCION IS NOT NULL
                    AND DET.CDEMO_ITEMART <> '000000'
                    And CXC.CDCC_VENDEDOR Not Like '99%'
                    AND CXC.SDCC_STATUS <> 'AN' ;
            END if;
        END IF;


        return montocadenax;
    END GETVALBYCLIENTE;





    FUNCTION  GETTIPOCOLBYPROV(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2 ,codprovx IN VARCHAR2,fechax IN VARCHAR2,usuariox IN VARCHAR2,modox IN VARCHAR2) return TYPES.ref_cursor
        is
        Result types.ref_cursor;
        empx VARCHAR2(10);
        sedex VARCHAR2(10);
        mesax VARCHAR2(10);
        canalx VARCHAR2(10);
        proveedorx VARCHAR2(10);
        fecx VARCHAR2(10);

    BEGIN


        IF modox='M' THEN
            SELECT distinct CODEMPRESA,CODSEDE,CODMESA,CODCANAL  into empx,sedex,mesax,canalx
            FROM SYSADM.TB_USUARIO_MESA
            WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';
            fecx:=to_char(sysdate,'dd/mm/yyyy');

/*select distinct PROVEEDORSEGUIM into proveedorx
		FROM SYSADM.PLANIFICADOR
		WHERE CODEMPRESA=empx AND
		CODSEDE=sedex AND
		CODMESA=mesax AND
		CODCANAL=canalx AND
		 FECHA=to_date(fecx,'dd/mm/yyyy');*/



        ELSE
            empx:=codempx;
            sedex:=codsedex;
            mesax:=codmesax;
            canalx:=codcanalx;
--proveedorx:=codprovx;
            fecx:=fechax;

        END IF;


        open Result for
            select  DISTINCT  PL.CODTIPOCOL,CP.DESCTIPO
            FROM SYSADM.PLANIFICADOR PL inner join SYSADM.TIPO_COL_PLANIFICADOR CP ON PL.CODTIPOCOL=CP.CODTIPOCOL
            WHERE PL.CODEMPRESA=empx
              AND PL.CODSEDE=sedex
              AND PL.CODMESA=mesax
              AND PL.CODCANAL=canalx
              and PL.CODPROVEEDOR=codprovx
              AND PL.FECHA=to_date(fecx,'dd/mm/yyyy') ORDER BY PL.CODTIPOCOL ASC;


        return Result;

    END GETTIPOCOLBYPROV;





    FUNCTION  GETCOLBYTIPOCOL(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2 ,codprovx IN VARCHAR2,codtipocolx IN VARCHAR2,fechax IN VARCHAR2,usuariox IN VARCHAR2,modox in varchar2) return TYPES.ref_cursor
        is
        Result types.ref_cursor;
        empx VARCHAR2(10);
        sedex VARCHAR2(10);
        mesax VARCHAR2(10);
        canalx VARCHAR2(10);
        proveedorx VARCHAR2(10);
        fecx VARCHAR2(10);
    BEGIN

        IF modox='M' THEN
            SELECT distinct CODEMPRESA,CODSEDE,CODMESA,CODCANAL  into empx,sedex,mesax,canalx
            FROM SYSADM.TB_USUARIO_MESA
            WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';
            fecx:=to_char(sysdate,'dd/mm/yyyy');

/*select distinct PROVEEDORSEGUIM into proveedorx
		FROM SYSADM.PLANIFICADOR
		WHERE CODEMPRESA=empx AND
		CODSEDE=sedex AND
		CODMESA=mesax AND
		CODCANAL=canalx AND
		 FECHA=to_date(fecx,'dd/mm/yyyy');]*/



        ELSE
            empx:=codempx;
            sedex:=codsedex;
            mesax:=codmesax;
            canalx:=codcanalx;
--proveedorx:=codprovx;
            fecx:=fechax;

        END IF;


        open Result for
            select CODTIPOVALOR,CODVALOR,VALORCOL
            FROM SYSADM.PLANIFICADOR
            WHERE CODEMPRESA=empx
              AND CODSEDE=sedex
              AND CODMESA=mesax
              AND CODCANAL=canalx
              and CODPROVEEDOR=codprovx
              and CODTIPOCOL=codtipocolx
              AND FECHA=to_date(fecx,'dd/mm/yyyy') ORDER BY CODPROVEEDOR,VALORCOL ASC;

        return Result;

    END GETCOLBYTIPOCOL;



    PROCEDURE  VALIDARPLANIFICADOR(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,
                                   codcanalx IN VARCHAR2,usuarioX IN VARCHAR2,fechax IN VARCHAR2,modox IN VARCHAR2,rptavalidacionx OUT NUMBER )
        IS
        empx VARCHAR2(10);
        sedex VARCHAR2(10);
        mesax VARCHAR2(10);
        canalx VARCHAR2(10);
        fecx VARCHAR2(100);
        countprovsegx NUMBER;
    BEGIN

        IF modox='M' THEN

            SELECT distinct CODEMPRESA,CODSEDE,CODMESA,CODCANAL  into empx,sedex,mesax,canalx
            FROM SYSADM.TB_USUARIO_MESA
            WHERE USUARIO=usuariox AND TB_TIPO_CODIGO='R';

            fecx:=to_char(sysdate,'dd/mm/yyyy');

            SELECT COUNT(DISTINCT PROVEEDORSEGUIM) INTO 	countprovsegx
            FROM SYSADM.PLANIFICADOR
            WHERE CODEMPRESA=empx
              AND CODSEDE=sedex
              AND CODMESA=mesax
              AND CODCANAL=canalx
              AND FECHA=to_date(fecx,'dd/mm/yyyy');

            if countprovsegx>0 THEN
                rptavalidacionx:=1;
            else
                rptavalidacionx:=0;
            end if;

        ELSE
            empx:=codempx;
            sedex:=codsedex;
            mesax:=codmesax;
            canalx:=codcanalx;
            fecx:=fechax;

            SELECT COUNT(*) INTO 	countprovsegx
            FROM SYSADM.PLANIFICADOR
            WHERE CODEMPRESA=empx
              AND CODSEDE=sedex
              AND CODMESA=mesax
              AND CODCANAL=canalx
              AND FECHA=to_date(fecx,'dd/mm/yyyy');

            if countprovsegx>0 THEN
                rptavalidacionx:=1;
            else
                rptavalidacionx:=0;
            end if;

        END IF;

    END VALIDARPLANIFICADOR;

    FUNCTION  LISTARTBYPROOV(codprovx IN VARCHAR2,codempx 	IN VARCHAR2,codalmx IN VARCHAR2) return TYPES.ref_cursor
        is
        Result types.ref_cursor;
    BEGIN
        open Result FOR
            SELECT F.COD_ITEM,F.DESC_ITEM,f.SALDO
            FROM
                (
                    SELECT ROWNUM  cant,ART.COD_ITEM,ART.DESC_ITEM,(NVL(SA.qty_fisica,0)- NVL(SA.qty_comprometida,0)- NVL(SA.qty_reservada,0)- NVL(SA.qty_proyectos,0)) saldo
                    FROM SYSADM.ARTICULOS ART
                             INNER JOIN SYSADM.SALDOS_ALMACEN sa on  ART.COD_ITEM=SA.COD_ITEM
                    WHERE 1=1
                      AND ART.PROVEEDOR_DEFAULT=codprovx
                      AND STATUS_ITEM='A' AND SA.COMPANIA_VENTA_3=codempx AND SA.ALMACEN=codalmx
                )F WHERE F.SALDO>0;
/*SELECT F.COD_ITEM,F.DESC_ITEM
FROM
(
SELECT ROWNUM  cant,ART.COD_ITEM,ART.DESC_ITEM
FROM SYSADM.ARTICULOS ART
WHERE 1=1
AND ART.PROVEEDOR_DEFAULT=codprovx
AND STATUS_ITEM='A'
--AND  art.cod_item||art.desc_item LIKE '%'||translate(filtrox, 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')||'%'
)F ; */
        return Result;
    end LISTARTBYPROOV;

    PROCEDURE  DATOSMODALFOCUS(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR,codalmacenx IN VARCHAR2,codproveedorx IN VARCHAR2,coditemx IN VARCHAR2,cursorcanalesx OUT types.ref_cursor,cursorartx OUT types.ref_cursor)
        is

    BEGIN
        open cursorcanalesx FOR
            SELECT  UM.CODCANAL,TC.DESCCANAL,COUNT(UM.CODVENDEDOR) AS CANVENDEDORES
            FROM SYSADM.TB_USUARIO_MESA UM
                     INNER JOIN SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO
                     INNER JOIN SYSADM.TB_CANAL TC ON TC.CODCANAL=UM.CODCANAL
            WHERE CODEMPRESA=codempx
              AND CODSEDE=codsedex
              AND CODMESA=codmesax
              AND UM.TB_TIPO_CODIGO='R'
              AND US.ESTUSUARIO='H'
              AND UM.ESTADO_EN_MESA='A'
              AND CODPERFIL='06' AND TC.CODCANAL<>'8'
            GROUP BY UM.CODCANAL,TC.DESCCANAL;

        open cursorartx FOR
            SELECT ART.COD_ITEM,ART.DESC_ITEM,ART.UM_VENTA,ART.UM_CONTROL_STOCK,
                   (NVL(SA.qty_fisica,0)- NVL(SA.qty_comprometida,0)- NVL(SA.qty_reservada,0)- NVL(SA.qty_proyectos,0)) DISPONIBLE
            FROM SYSADM.ARTICULOS ART
                     INNER JOIN SYSADM.SALDOS_ALMACEN SA ON SA.COD_ITEM=ART.COD_ITEM
            WHERE 1=1
              AND SA.COMPANIA_VENTA_3=codempx
              AND ART.COD_ITEM=coditemx
              AND SA.ALMACEN=codalmacenx;



    END DATOSMODALFOCUS;




/*PROCEDURE "APTOCREDITOCAMPANIA" (TABLE_OBJ IN PEDIDO_PREVENTA_TABLE,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
 IS
 countaux number;
countaux2 number;
 permiteCreditox NUMBER;
deudavencidax NUMBER(10);
 codperfilx VARCHAR2(4);
BEGIN

countaux:=0;


 SELECT DISTINCT US.CODPERFIL  INTO codperfilx
				FROM SYSADM.TB_USUARIO_MESA UM inner join SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO
				WHERE 1=1 AND UM.CODVENDEDOR=TABLE_OBJ(1).codvendedor AND UM.TB_TIPO_CODIGO='R';



				SELECT COUNT(DISTINCT UM.CODVENDEDOR)  INTO permiteCreditox
				FROM SYSADM.TB_USUARIO_MESA UM
				WHERE 1=1 AND UM.CODVENDEDOR=TABLE_OBJ(1).codvendedor;

				SELECT NVL(SUM(VENC.DEUDAVENCIDA),0) INTO deudavencidax
				FROM
				(
				SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDAVENCIDA--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
												FROM SYSADM.DOCUMENTO_CXC CXC
												INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
												 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
												 LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
													FROM SYSADM.SGC_COBRANZA
													WHERE ESTADO_COBRANZA IN ('I','D')
												 GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
												WHERE CXC.CDCC_COMPANIA=TABLE_OBJ(1).codempresa
												 AND CXC.CDCC_CLIENTE=TABLE_OBJ(1).codcliente
												AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
												and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
												AND CXC.MDCC_MONTO>0
										 and (SYSDATE-CXC.FDCC_EMISION)>NVL(LPC.DIAS_PLAZO,0)
				)VENC;

				if deudavencidax=0 then
						if (permiteCreditox<>0)THEN
								rptax := 1;
								mensajex :='SE REGISTRO PEDIDO ,VENDEDOR APTO CREDITOS SIN DEUDA VENCIDA';

						else
											SELECT COUNT(*)  INTO countaux2
											FROM SYSADM.VENDEDOR_CAMPANIA
											WHERE CODVENDEDOR=TABLE_OBJ(1).codvendedor
											AND STATUS_VENDEDOR='A';

											IF countaux2>0 THEN

														FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST
														LOOP
															select count(*) INTO countaux
															from SYSADM.ARTICULO_CAMPANIA
															where COD_ITEM=TABLE_OBJ(indx).codproducto
															AND STATUS_ITEM='A';

														EXIT WHEN countaux=0;
														END LOOP;
											END IF;

											IF countaux>0 THEN
															 rptax := 1;
															 mensajex :='SE REGISTRO PEDIDO CREDITO POR SER CAMPAÑA';
											ELSE
																rptax := 0;
															 mensajex :='NO PUEDE PASAR CREDITO, NO TODOS SON ARTICULOS DE CAMPAÑA ';
											END IF;
						end if;

					ELSE
						IF codperfilx<>'06'  THEN
									rptax := 0;
									mensajex :='NO PUEDE PASAR CREDITO, CLIENTE TIENE DEUDA VENCIDA ';
								ELSE
									rptax := 1;
									mensajex :='REGISTRÓ PEDIDO MAYORISTA';
								END IF;

					end if;




EXCEPTION

	WHEN OTHERS THEN
		rptax := -1;
    mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

end APTOCREDITOCAMPANIA;*/

    PROCEDURE "APTOCREDITOCAMPANIA" (TABLE_OBJ IN PEDIDO_PREVENTA_TABLE,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
        IS
        countaux number;
        countaux2 number;
        permiteCreditox NUMBER;
        deudavencidax NUMBER(10);
        codperfilx VARCHAR2(4);

    BEGIN

        countaux:=0;



        SELECT DISTINCT US.CODPERFIL  INTO codperfilx
        FROM SYSADM.TB_USUARIO_MESA UM inner join SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO
        WHERE 1=1 AND UM.CODVENDEDOR=TABLE_OBJ(1).codvendedor AND UM.TB_TIPO_CODIGO='R';


        SELECT COUNT(DISTINCT UM.CODVENDEDOR)  INTO permiteCreditox
        FROM SYSADM.TB_USUARIO_MESA UM
        WHERE UM.PERMI_CREDI=1 AND UM.CODVENDEDOR=TABLE_OBJ(1).codvendedor;

        SELECT NVL(SUM(VENC.DEUDAVENCIDA),0) INTO deudavencidax
        FROM
            (
                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDAVENCIDA--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                FROM SYSADM.DOCUMENTO_CXC CXC
                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                    FROM SYSADM.SGC_COBRANZA
                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                WHERE CXC.CDCC_COMPANIA=TABLE_OBJ(1).codempresa
                  AND CXC.CDCC_CLIENTE=TABLE_OBJ(1).codcliente
                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                  AND CXC.MDCC_MONTO>0
                  and (SYSDATE-CXC.FDCC_EMISION)>NVL(LPC.DIAS_PLAZO,0)
            )VENC;

        if deudavencidax=0 then
            if (permiteCreditox<>0)THEN
                rptax := 1;
                mensajex :='SE REGISTRO PEDIDO ,VENDEDOR APTO CREDITOS SIN DEUDA VENCIDA';

            else
                SELECT COUNT(*)  INTO countaux2
                FROM SYSADM.VENDEDOR_CAMPANIA
                WHERE CODVENDEDOR=TABLE_OBJ(1).codvendedor
                  AND STATUS_VENDEDOR='A';

                IF countaux2>0 THEN

                    FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST
                        LOOP
                            select count(*) INTO countaux
                            from SYSADM.ARTICULO_CAMPANIA
                            where COD_ITEM=TABLE_OBJ(indx).codproducto
                              AND STATUS_ITEM='A';

                            EXIT WHEN countaux=0;
                        END LOOP;
                END IF;

                IF countaux>0 THEN
                    rptax := 1;
                    mensajex :='SE REGISTRO PEDIDO CREDITO POR SER CAMPAÑA';
                ELSE
                    rptax := 0;
                    mensajex :='NO PUEDE PASAR CREDITO, NO TODOS SON ARTICULOS DE CAMPAÑA ';
                END IF;
            end if;

        ELSE

            IF codperfilx<>'06'  THEN
                rptax := 0;
                mensajex :='NO PUEDE PASAR CREDITO, CLIENTE TIENE DEUDA VENCIDA ';
            ELSE
                rptax := 1;
                mensajex :='REGISTRÓ PEDIDO MAYORISTA';
            END IF;
        end if;




    EXCEPTION

        WHEN OTHERS THEN
            rptax := -1;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

    end APTOCREDITOCAMPANIA;


    PROCEDURE DISTRIBXCANAL(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2 ,codalmacenx IN VARCHAR2,cantffvvx IN NUMBER ,cantitemx IN NUMBER,cantxvendex IN NUMBER,desccanalx IN VARCHAR2,fechainiciox IN VARCHAR2,fechafinx IN VARCHAR2,coditemx IN VARCHAR2,umventax IN VARCHAR2,umstockx IN VARCHAR2,cantxmesax IN NUMBER,codproveedorx IN VARCHAR2,usuariox IN VARCHAR2,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
        IS
        cursorx TYPES.ref_cursor;
        usuariovendx VARCHAR2(30);
        promediox NUMBER(10,2);
        correlativo1 number;
        correlativo2 number;
        correlativo3 number;
        cantdiasinix number;
        cantfocusx NUMBER;
    BEGIN

        select trunc(to_date(fechafinx,'DD/MM/YYYY'))-trunc(to_date(fechainiciox,'DD/MM/YYYY')) into cantdiasinix
        from dual;


        SELECT COUNT(*)  INTO cantfocusx
        FROM SYSADM.FOCUS_ARTICULO
        WHERE CODEMPRESA=codempx
          AND CODSEDE=codsedex
          AND CODMESA=codmesax
          and CODITEM=coditemx
          AND UNIDADMEDIDA=umstockx
          and ESTADO='P';


        IF cantfocusx>0 THEN
            SELECT IDFOCUSDET  INTO correlativo1
            FROM SYSADM.FOCUS_ARTICULO
            WHERE CODEMPRESA=codempx
              AND CODSEDE=codsedex AND CODMESA=codmesax and CODITEM=coditemx
              AND UNIDADMEDIDA=umstockx and ESTADO='P';
        ELSE
            SELECT ARTICULO_FOCUS_SEQUENCE.NEXTVAL  INTO correlativo1
            FROM DUAL;


            INSERT INTO SYSADM.FOCUS_ARTICULO
            VALUES(correlativo1,codempx,codsedex,codmesax,coditemx,umstockx,codproveedorx,codalmacenx,cantxmesax,'P',to_date(fechainiciox,'DD/MM/YYYY'),to_date(fechafinx,'DD/MM/YYYY'),usuariox,SYSDATE);

        END IF;





        SELECT PARTICIPACION_FOCUS_SEQUENCE.NEXTVAL  INTO correlativo2
        FROM DUAL;






        INSERT INTO SYSADM.FOCUS_PARTICIPACION
        VALUES(correlativo2,correlativo1,codcanalx,desccanalx,cantffvvx,cantitemx,cantxvendex);


        open cursorx FOR
            SELECT  US.USUARIO,NVL(XRAYADMIN.GETPROMEDIOXVEND(UM.CODEMPRESA,UM.CODSEDE,UM.CODMESA,UM.CODCANAL,UM.CODVENDEDOR,UM.USUARIO),0)
            FROM SYSADM.TB_USUARIO_MESA UM
                     INNER JOIN SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO
                     INNER JOIN SYSADM.TB_CANAL TC ON TC.CODCANAL=UM.CODCANAL
            WHERE UM.CODEMPRESA=codempx
              AND UM.CODSEDE=codsedex
              AND UM.CODMESA=codmesax
              AND UM.CODCANAL=codcanalx
              AND UM.TB_TIPO_CODIGO='R'
              AND US.ESTUSUARIO='H' AND TC.CODCANAL<>'8'
              AND UM.ESTADO_EN_MESA='A'
              AND US.CODPERFIL='06';

        LOOP
            FETCH cursorx INTO usuariovendx,promediox;--,estadox;
            EXIT WHEN cursorx%NOTFOUND;

            SELECT USUARIO_PARTI_FOCUS_SEQUENCE.NEXTVAL  INTO correlativo3
            FROM DUAL;
            INSERT INTO "SYSADM"."FOCUS_USUARIO_PARTI"
            VALUES(correlativo3,usuariovendx,correlativo2,promediox,cantdiasinix,cantxvendex);

        END LOOP;
        CLOSE cursorx;




        rptax := 1;
        mensajex :='TODO CORRECTO ';

        commit;

    EXCEPTION

        WHEN OTHERS THEN
            rptax := -1;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;



    END DISTRIBXCANAL;


    PROCEDURE DISTRIBALL(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,fechainiciox IN VARCHAR2,fechafinx IN VARCHAR2,coditemx IN VARCHAR2,codalmacenx IN VARCHAR2,umventax IN VARCHAR2,umstockx IN VARCHAR2,codproveedorx IN VARCHAR2,cantxmesax IN NUMBER,cantffvvx IN VALUE_ARRAY_NUMBER,cantitemx IN VALUE_ARRAY_NUMBER,cantxvendex IN VALUE_ARRAY_NUMBER,codcanalx IN VALUE_ARRAY_STRING,desccanalx IN VALUE_ARRAY_STRING,usuariox IN VARCHAR2,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
        IS
        cursorx TYPES.ref_cursor;
        usuariovendx VARCHAR2(30);
        promediox NUMBER(10,2);
        correlativo1 number;
        correlativo2 number;
        correlativo3 number;
        cantdiasinix number;
        cantfocusx NUMBER;
        cantsugxdia 	NUMBER;
        cantsugxclientex number;
    BEGIN

        select trunc(to_date(fechafinx,'DD/MM/YYYY'))-trunc(to_date(fechainiciox,'DD/MM/YYYY'))+1 into cantdiasinix
        from dual;


        SELECT COUNT(*)  INTO cantfocusx
        FROM SYSADM.FOCUS_ARTICULO
        WHERE CODEMPRESA=codempx
          AND CODSEDE=codsedex AND CODMESA=codmesax and CODITEM=coditemx
          AND UNIDADMEDIDA=umstockx and ESTADO='P';


        IF cantfocusx>0 THEN
            SELECT IDFOCUSDET  INTO correlativo1
            FROM SYSADM.FOCUS_ARTICULO
            WHERE CODEMPRESA=codempx
              AND CODSEDE=codsedex AND CODMESA=codmesax and CODITEM=coditemx
              AND UNIDADMEDIDA=umstockx and ESTADO='P';
        ELSE
            SELECT ARTICULO_FOCUS_SEQUENCE.NEXTVAL  INTO correlativo1
            FROM DUAL;


            INSERT INTO SYSADM.FOCUS_ARTICULO
            VALUES(correlativo1,codempx,codsedex,codmesax,coditemx,umstockx,codproveedorx,codalmacenx,cantxmesax,'P',to_date(fechainiciox,'DD/MM/YYYY'),to_date(fechafinx,'DD/MM/YYYY'),usuariox,SYSDATE);

        END IF;

        for i in 1 .. codcanalx.COUNT loop


                SELECT PARTICIPACION_FOCUS_SEQUENCE.NEXTVAL  INTO correlativo2
                FROM DUAL;

                INSERT INTO SYSADM.FOCUS_PARTICIPACION
                VALUES(correlativo2,correlativo1,1,desccanalx(i),cantffvvx(i),cantitemx(i),cantxvendex(i));

                open cursorx FOR
                    SELECT  US.USUARIO,NVL(XRAYADMIN.GETPROMEDIOXVEND(UM.CODEMPRESA,UM.CODSEDE,UM.CODMESA,UM.CODCANAL,UM.CODVENDEDOR,UM.USUARIO),0)
                    FROM SYSADM.TB_USUARIO_MESA UM
                             INNER JOIN SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO
                             INNER JOIN SYSADM.TB_CANAL TC ON TC.CODCANAL=UM.CODCANAL
                    WHERE UM.CODEMPRESA=codempx
                      AND UM.CODSEDE=codsedex
                      AND UM.CODMESA=codmesax
                      AND UM.CODCANAL=codcanalx(i)
                      AND UM.TB_TIPO_CODIGO='R'
                      AND US.ESTUSUARIO='H' AND TC.CODCANAL<>'8'
                      AND UM.ESTADO_EN_MESA='A'
                      AND US.CODPERFIL='06';



                LOOP
                    FETCH cursorx INTO usuariovendx,promediox;--,estadox;
                    EXIT WHEN cursorx%NOTFOUND;

                    SELECT USUARIO_PARTI_FOCUS_SEQUENCE.NEXTVAL  INTO correlativo3
                    FROM DUAL;
                    cantsugxdia:=ROUND(cantxvendex(i)/cantdiasinix);

                    INSERT INTO "SYSADM"."FOCUS_USUARIO_PARTI"
                    VALUES(correlativo3,usuariovendx,correlativo2,promediox,cantdiasinix,cantsugxdia);

                    if promediox<>0 then
                        cantsugxclientex:=ROUND(cantsugxdia/promediox);
                    ELSE
                        cantsugxclientex:=0;
                    end if;

                    INSERT INTO SYSADM.FOCUS_USU_PARTI_DET
                    VALUES(USU_PARTI_DET_FOCUS_SEQUENCE.NEXTVAL,correlativo3,TO_DATE(fechainiciox,'DD/MM/YYYY'),cantsugxdia,cantsugxclientex,0,cantdiasinix,cantsugxdia);
                END LOOP;
                CLOSE cursorx;



            end loop;

        rptax := 1;
        mensajex :='TODO CORRECTO ';
        commit;
    EXCEPTION

        WHEN OTHERS THEN
            rollback;
            rptax := -1;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;



    END DISTRIBALL;


    FUNCTION CABECERAFOCUS(codempx IN VARCHAR2, codsedex IN VARCHAR2,codmesax IN VARCHAR2) return TYPES.ref_cursor
        is
        Result types.ref_cursor;
    BEGIN

        open Result for
            select TB.IDFOCUSDET,TB.CODEMPRESA,TB.CODSEDE,TB.CODMESA,TB.CODITEM,TB.UNIDADMEDIDA,TB.FECHA_INICIO,TB.FECHA_FIN,(TB.PLAZOFOCUS-TB.DIASRESTANTES) AS DIASPARTIC,
                   CASE WHEN TB.DIASRESTANTES<0 THEN
                            0
                        ELSE
                            TB.DIASRESTANTES
                       END AS DIASRESTANTES,TB.ESTADO

            FROM
                (
                    select IDFOCUSDET,CODEMPRESA,CODSEDE,CODMESA,CODITEM,UNIDADMEDIDA,FECHA_INICIO,FECHA_FIN,
                           (TRUNC(TO_DATE(FECHA_FIN,'DD/MM/YYYY'))-TRUNC(TO_DATE(FECHA_INICIO,'DD/MM/YYYY')))+1 PLAZOFOCUS,
                           (TRUNC(TO_DATE(FECHA_FIN,'DD/MM/YYYY'))-TRUNC(TO_DATE(SYSDATE,'DD/MM/YYYY')))+1 DIASRESTANTES,ESTADO
                    from SYSADM.FOCUS_ARTICULO
                    WHERE 1=1
                      AND CODEMPRESA=codempx
                      AND CODSEDE=codsedex
                      AND CODMESA=codmesax
                )TB;
        return Result;

    END CABECERAFOCUS;


    FUNCTION SUGERENCIAFOCUS(usuariox IN VARCHAR2) return TYPES.ref_cursor
        is
        Result types.ref_cursor;
    BEGIN

        open Result for
            SELECT AF.CODITEM,UPDF.CANT_SUGERIDA,AF.UNIDADMEDIDA,AF.CODEMPRESA,AF.CODSEDE,AF.CODALMACEN
            FROM SYSADM.FOCUS_ARTICULO AF
                     INNER JOIN SYSADM.FOCUS_PARTICIPACION PF ON AF.IDFOCUSDET=PF.IDFOCUSDET
                     INNER JOIN SYSADM.FOCUS_USUARIO_PARTI UPF ON UPF.IDPARTICIPACION=PF.IDPARTICIPACION
                     INNER JOIN SYSADM.FOCUS_USU_PARTI_DET UPDF ON UPDF.IDUSUPARTIFOCUS=UPF.IDUSUPARTIFOCUS
            WHERE UPF.USUARIO=usuariox
              AND AF.ESTADO='P'
              AND TO_CHAR(UPDF.FECHA,'DD/MM/YYYY')<=TO_CHAR(SYSDATE,'DD/MM/YYYY');
        return Result;

    END SUGERENCIAFOCUS;


    FUNCTION RUTAS_BY_USUARIO
    (
        usuariox  IN VARCHAR
    )
        RETURN TYPES.REF_CURSOR IS rptax TYPES.REF_CURSOR;
    codsedex VARCHAR2(10);
    BEGIN
        /*SELECT DISTINCT TB_COD_SEDE INTO codsedex
 FROM SYSADM.TB_ASIGNACION
WHERE TB_USUARIO=usuariox;*/

        OPEN rptax
            FOR
            SELECT CODRUTA,(CODRUTA||'-'||DESCRUTA)
            FROM SYSADM.TB_RUTA WHERE ESTADO='H' AND ROWNUM<50;


        RETURN rptax;
    END RUTAS_BY_USUARIO;

    FUNCTION  CLIENTES_OFFLINE(usernamex IN VARCHAR2) return types.ref_cursor is
        clientesx types.ref_cursor;
        countjornadax number;
        codclientex VARCHAR2(20);
        descclientex VARCHAR2(200);
        direccionx VARCHAR2(400);
        coordenadaoldx VARCHAR2(400);
        codrutax VARCHAR2(20);
        descrutax VARCHAR2(200);
        codlocalx VARCHAR2(2);
        codlistax VARCHAR2(10);
        codempresax VARCHAR2(2);
        estadox VARCHAR2(2);
        diajornadax DATE;
        countnewx number;
        countauxx number;
        ignoresecx number;

        fechacumplex VARCHAR2(10);
        tiponegociox VARCHAR2(200);
        cantpedidosx number;
        velocidadpagox number;
        antiguedadx VARCHAR2(200);
        deudavencidax number(10,2);
        deudanovencidax number(10,2);
        limitecreditox number(10,2);
        desclistapreciosx VARCHAR2(200);
        cantmaxx NUMBER ;
        limitecargax number;

        codsedex VARCHAR2(10);
        countauxrutaasigx number;

        nrotelefonox VARCHAR2(200);
        isauditadox NUMBER;

    BEGIN
        limitecargax:=10;
        diajornadax:=SYSDATE;

        SELECT distinct CODEMPRESA,CODSEDE  into codempresax,codsedex
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usernamex AND TB_TIPO_CODIGO='R';

        SELECT COUNT(*) into countjornadax
        FROM SYSADM.TBL_USUARIO_JORNADA
        WHERE USUARIO=usernamex
          AND CODSEDE=codsedex
          AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD');



        SELECT COUNT(COUNT(DISTINCT CL.COD_CLIENTE)) INTO countauxrutaasigx
        FROM SYSADM.USUARIOS US
                 INNER JOIN SYSADM.TB_USUARIO_MESA UM ON US.USUARIO=UM.USUARIO
                 INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON UM.CODVENDEDOR=RV.CODVENDEDOR
                 INNER JOIN SYSADM.TB_RUTA RU ON RV.IDRUTA=RU.IDRUTA
                 INNER JOIN SYSADM.TB_CANAL CA ON CA.CODCANAL=UM.CODCANAL
                 INNER JOIN SYSADM.LOCALES_CLIENTE LC ON LC.RUTA_DESPACHO=RU.CODRUTA  AND LC.ESTAD='A'
                 INNER JOIN SYSADM.CLIENTE CL  ON CL.COD_CLIENTE=LC.COD_CLIENTE AND CL.STATUS_CLIENTE='A'
        WHERE 1=1  and um.codempresa=codempresax
          AND UM.CODSEDE=codsedex AND US.USUARIO=usernamex
        GROUP BY UM.CODVENDEDOR,RV.DIA,CA.DESCCANAL
        HAVING  RV.DIA=(select translate(SUBSTR(to_char(SYSDATE, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                        from dual);




        if countauxrutaasigx<>0 then
            SELECT COUNT(DISTINCT CL.COD_CLIENTE) INTO cantmaxx
            FROM SYSADM.USUARIOS US
                     INNER JOIN SYSADM.TB_USUARIO_MESA UM ON US.USUARIO=UM.USUARIO
                     INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON UM.CODVENDEDOR=RV.CODVENDEDOR
                     INNER JOIN SYSADM.TB_RUTA RU ON RV.IDRUTA=RU.IDRUTA
                     INNER JOIN SYSADM.TB_CANAL CA ON CA.CODCANAL=UM.CODCANAL
                     INNER JOIN SYSADM.LOCALES_CLIENTE LC ON LC.RUTA_DESPACHO=RU.CODRUTA  AND LC.ESTAD='A'
                     INNER JOIN SYSADM.CLIENTE CL  ON CL.COD_CLIENTE=LC.COD_CLIENTE AND CL.STATUS_CLIENTE='A'
            WHERE 1=1  and um.codempresa=codempresax
              AND UM.CODSEDE=codsedex AND US.USUARIO=usernamex
              AND UM.TB_TIPO_CODIGO='R'
            GROUP BY UM.CODVENDEDOR,RV.DIA,CA.DESCCANAL
            HAVING  RV.DIA=(select translate(SUBSTR(to_char(SYSDATE, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                            from dual);

            IF cantmaxx<=limitecargax then



                --SELECT NVL(IGNORE_SECUENCIACION,0) into ignoresecx
                --FROM SYSADM.USUARIOS WHERE USUARIO=usernamex;
                ignoresecx:=0;

                IF countjornadax=0 then
                    open clientesx for
                        SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                        LC.NEW_COORDENADAS,
                                        LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA,C.STATUS_CLIENTE AS ESTADO,
                                        TO_CHAR(FECHA_CUMPLEANOS) FECHA_CUMPLEANOS,--FECHA CUMPLEAÑOS
                                        (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='006' AND LLAVE=C.TIPO_CLIENTE) TIPONEGOCIO, --TIPONEGOCIO

                                        (SELECT COUNT(DISTINCT NRO_PEDIDO)
                                         FROM SYSADM.SPEDIDO_HEADER WHERE COD_CLIENTE=C.COD_CLIENTE AND STATUS_PEDIDO='G') CANTPEDIDOS,
                                        (
                                            SELECT DISTINCT TRUNC(NVL(SUM(CASE WHEN((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO)<0 then 0 else ((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO) END)/COUNT(CXC.CDCC_SECUENCIA),0))
                                            FROM SYSADM.DOCUMENTO_CXC CXC
                                                     INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND ON CXC.CDCC_CONDVENTA=COND.LLAVE
                                                     INNER JOIN SYSADM.SPEDIDO_HEADER SPH ON CXC.CDCC_COMPANIA=SPH.COMPANIA_VENTA  AND  CXC.NDCC_PEDIDOOIH=SPH.NRO_PEDIDO
                                                     INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION AND SPH.COD_LISTA_PRECIOS=LPC.LISTAPRECIO AND LPC.LOCALIDAD=SPH.DPTO_ORIGEN
                                            WHERE 1=1 AND SPH.COD_CIA='00' AND LPC.ESTAD='A'
                                              AND CXC.CDCC_CLIENTE=C.COD_CLIENTE
                                              AND SDCC_STATUS='CA' AND COND.FLAG10=1
                                              AND MDCC_MONTO>0 ) VELOCIDADPAGO,--VELOCIDAD PAGO

                                        (
                                            SELECT   D1.AÑOS||' A-' ||D1.MESES||' M '
                                            FROM
                                                (
                                                    SELECT D.AÑOS,TRUNC(((D.DIAS-D.AÑOS)*360/30)) MESES,((D.DIAS-D.AÑOS)*360/30)DIAS2,D.COD_CLIENTE
                                                    FROM
                                                        (
                                                            select FECHA_CREACION,(TRUNC((trunc(sysdate) - FECHA_CREACION)/360)) AÑOS,((trunc(sysdate) - FECHA_CREACION)/360) DIAS,COD_CLIENTE
                                                            from SYSADM.CLIENTE
                                                        )D
                                                )D1 WHERE D1.COD_CLIENTE=C.COD_CLIENTE ) ANTIGUEDAD,--  ANTIGUEDAD
                                        (
                                            SELECT NVL(SUM(VENC.DEUDAVENCIDA),0)
                                            FROM
                                                (
                                                    SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDAVENCIDA,CDCC_COMPANIA,CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                    FROM SYSADM.DOCUMENTO_CXC CXC
                                                             INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                             INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                             LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                        FROM SYSADM.SGC_COBRANZA
                                                                        WHERE ESTADO_COBRANZA IN ('I','D')
                                                                        GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                    WHERE 1=1
                                                      AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                      and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                      AND CXC.MDCC_MONTO>0
                                                      and (SYSDATE-CXC.FDCC_EMISION)>NVL(LPC.DIAS_PLAZO,0)
                                                )VENC  WHERE 1=1	AND VENC.CDCC_COMPANIA=UM.CODEMPRESA AND VENC.CDCC_CLIENTE=C.COD_CLIENTE

                                        ) DEUDAVENCIDA,--DEUDA VENCIDA
                                        (
                                            SELECT NVL(SUM(NOVENC.DEUDANOVENCIDA),0)
                                            FROM
                                                (
                                                    SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDANOVENCIDA,CXC.CDCC_COMPANIA,CXC.CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                    FROM SYSADM.DOCUMENTO_CXC CXC
                                                             INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                             INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                             LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                        FROM SYSADM.SGC_COBRANZA
                                                                        WHERE ESTADO_COBRANZA IN ('I','D')
                                                                        GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                    WHERE 1=1
                                                      AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                      and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                      AND CXC.MDCC_MONTO>0
                                                      and (SYSDATE-CXC.FDCC_EMISION)<=NVL(LPC.DIAS_PLAZO,0)
                                                )NOVENC where 1=1 and NOVENC.CDCC_COMPANIA=UM.CODEMPRESA
                                                          AND NOVENC.CDCC_CLIENTE=C.COD_CLIENTE
                                        ) DEUDANOVENCIDA ,
                                        (SELECT 	nvl(sum(LIMIT_CREDI),0) AS LIMITE
                                         FROM
                                             XRAYADMIN.SGC_CLIENTE_LINEA_CREDITO
                                         WHERE 1=1  and CODIG_EMPRE = UM.CODEMPRESA
                                           AND CODIG_CLIEN =C.COD_CLIENTE
                                           AND VIGEN = TO_CHAR(SYSDATE, 'MM-YYYY')
                                           AND ESTAD = 'A') LIMITECREDITO,
                                        LP.DESCRIPCION,
                                        c.new_telefono,
                                        lc.is_auditado

                        FROM SYSADM.CLIENTE C
                                 INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                                 INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                                 INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                                 INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                                 INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
                                 INNER JOIN SYSADM.PRECIOS_C LP  ON LP.COD_LISTA = cl.COD_LISTA  AND LP.COMPANIA_VENTA =codempresax
                        WHERE 1=1
                          AND RV.DIA=(select translate(SUBSTR(to_char(diajornadax, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                      from dual)
                          AND UM.USUARIO=usernamex
                          AND C.STATUS_CLIENTE IN ('A','P')
                          AND LC.ESTAD='A'
                          AND nvl(C.USUAR_REGIS,'NONE')<>'EC';--EXCLUIR CLIENTES DIGITALES


                    LOOP
                        FETCH clientesx INTO codclientex,descclientex,direccionx,coordenadaoldx,codrutax,descrutax,codlocalx,codlistax,codempresax,estadox,fechacumplex,tiponegociox,cantpedidosx,velocidadpagox,antiguedadx,deudavencidax,deudanovencidax,limitecreditox,desclistapreciosx,nrotelefonox,isauditadox;
                        EXIT WHEN clientesx%NOTFOUND;
                        INSERT INTO SYSADM.TBL_USUARIO_JORNADA(USUARIO,CODSEDE,FECHA,CODCLIENTE,DESCCLIENTE,CODLOCAL,CODRUTA,DESCRUTA,DIRECCION,
                                                               COORDENADA_ANT,CODLISTA,DESCLISTA,CODEMPRESA,ESTADO,FECHA_CUMPLE,TIPO_NEGOCIO,CANT_PEDIDOS,VELOCIDAD_PAGO,ANTIGUEDAD,DEUDA_VENCIDA,DEUDA_NOVENCIDA,LIMITECREDITO,FECHA_TRANS,NEW_TELEFONO,IS_AUDITADO)
                        VALUES(usernamex,codsedex,to_date(SUBSTR(diajornadax,0,10)),codclientex,descclientex,codlocalx,codrutax,descrutax,direccionx,
                               coordenadaoldx,codlistax,desclistapreciosx,codempresax,estadox,to_date(fechacumplex),tiponegociox,cantpedidosx,velocidadpagox,antiguedadx,deudavencidax,deudanovencidax,limitecreditox,SYSDATE,nrotelefonox,isauditadox);
                    END LOOP;
                    CLOSE clientesx;


                    open clientesx for
                        SELECT UJ.CODCLIENTE,UJ.DESCCLIENTE,UJ.DIRECCION,LC.NEW_COORDENADAS,UJ.CODRUTA,UJ.DESCRUTA,UJ.CODLOCAL,UJ.CODLISTA,UJ.CODEMPRESA,
                               CASE WHEN UJ.IDNOPEDIDO IS NOT NULL THEN 'N'
                                    WHEN UJ.NROPEDIDO IS NOT NULL THEN 'P'
                                    ELSE '0' END AS FLAGESTADO,
                               CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                               ESTADO ESTADOCLIENTE,TO_CHAR(FECHA,'YYYY-MM-DD') FECHA,
                               TO_CHAR(FECHA_CUMPLE,'YYYY-MM-DD') FECHA_CUMPLE,
                               TIPO_NEGOCIO,
                               CANT_PEDIDOS,
                               VELOCIDAD_PAGO,
                               ANTIGUEDAD,
                               DEUDA_VENCIDA,
                               DEUDA_NOVENCIDA,
                               LIMITECREDITO,DESCLISTA,
                               UJ.NEW_TELEFONO,
                               UJ.IS_AUDITADO
                        FROM SYSADM.TBL_USUARIO_JORNADA UJ
                                 LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UJ.CODCLIENTE
                            AND LC.COD_LOCAL=UJ.CODLOCAL
                        where USUARIO=usernamex  and  CODSEDE=codsedex
                          AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD');

                ELSE

                    SELECT COUNT(*)  INTO countnewx
                    FROM(
                            SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                            LC.NEW_COORDENADAS,
                                            LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA,  STATUS_CLIENTE AS ESTADO,
                                            TO_CHAR(FECHA_CUMPLEANOS) FECHA_CUMPLEANOS,--FECHA CUMPLEAÑOS
                                            (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='006' AND LLAVE=C.TIPO_CLIENTE) TIPONEGOCIO, --TIPONEGOCIO

                                            (SELECT COUNT(DISTINCT NRO_PEDIDO)
                                             FROM SYSADM.SPEDIDO_HEADER WHERE COD_CLIENTE=C.COD_CLIENTE AND STATUS_PEDIDO='G') CANTPEDIDOS,
                                            (
                                                SELECT DISTINCT TRUNC(NVL(SUM(CASE WHEN((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO)<0 then 0 else ((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO) END)/COUNT(CXC.CDCC_SECUENCIA),0))
                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                         INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND ON CXC.CDCC_CONDVENTA=COND.LLAVE
                                                         INNER JOIN SYSADM.SPEDIDO_HEADER SPH ON CXC.CDCC_COMPANIA=SPH.COMPANIA_VENTA  AND  CXC.NDCC_PEDIDOOIH=SPH.NRO_PEDIDO
                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION AND SPH.COD_LISTA_PRECIOS=LPC.LISTAPRECIO AND LPC.LOCALIDAD=SPH.DPTO_ORIGEN
                                                WHERE 1=1 AND SPH.COD_CIA='00' AND LPC.ESTAD='A'
                                                  AND CXC.CDCC_CLIENTE=C.COD_CLIENTE
                                                  AND SDCC_STATUS='CA' AND COND.FLAG10=1
                                                  AND MDCC_MONTO>0 ) VELOCIDADPAGO,--VELOCIDAD PAGO

                                            (
                                                SELECT   D1.AÑOS||' A-' ||D1.MESES||' M '
                                                FROM
                                                    (
                                                        SELECT D.AÑOS,TRUNC(((D.DIAS-D.AÑOS)*360/30)) MESES,((D.DIAS-D.AÑOS)*360/30)DIAS2,D.COD_CLIENTE
                                                        FROM
                                                            (
                                                                select FECHA_CREACION,(TRUNC((trunc(sysdate) - FECHA_CREACION)/360)) AÑOS,((trunc(sysdate) - FECHA_CREACION)/360) DIAS,COD_CLIENTE
                                                                from SYSADM.CLIENTE
                                                            )D
                                                    )D1 WHERE D1.COD_CLIENTE=C.COD_CLIENTE ) ANTIGUEDAD,--  ANTIGUEDAD
                                            (
                                                SELECT NVL(SUM(VENC.DEUDAVENCIDA),0)
                                                FROM
                                                    (
                                                        SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDAVENCIDA,CDCC_COMPANIA,CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                        FROM SYSADM.DOCUMENTO_CXC CXC
                                                                 INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                                 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                                 LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                            FROM SYSADM.SGC_COBRANZA
                                                                            WHERE ESTADO_COBRANZA IN ('I','D')
                                                                            GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                        WHERE 1=1
                                                          AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                          and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                          AND CXC.MDCC_MONTO>0
                                                          and (SYSDATE-CXC.FDCC_EMISION)>NVL(LPC.DIAS_PLAZO,0)
                                                    )VENC  WHERE 1=1	AND VENC.CDCC_COMPANIA=UM.CODEMPRESA AND VENC.CDCC_CLIENTE=C.COD_CLIENTE

                                            ) DEUDAVENCIDA,--DEUDA VENCIDA
                                            (
                                                SELECT NVL(SUM(NOVENC.DEUDANOVENCIDA),0)
                                                FROM
                                                    (
                                                        SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDANOVENCIDA,CXC.CDCC_COMPANIA,CXC.CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                        FROM SYSADM.DOCUMENTO_CXC CXC
                                                                 INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                                 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                                 LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                            FROM SYSADM.SGC_COBRANZA
                                                                            WHERE ESTADO_COBRANZA IN ('I','D')
                                                                            GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                        WHERE 1=1
                                                          AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                          and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                          AND CXC.MDCC_MONTO>0
                                                          and (SYSDATE-CXC.FDCC_EMISION)<=NVL(LPC.DIAS_PLAZO,0)
                                                    )NOVENC where 1=1 and NOVENC.CDCC_COMPANIA=UM.CODEMPRESA
                                                              AND NOVENC.CDCC_CLIENTE=C.COD_CLIENTE
                                            ) DEUDANOVENCIDA ,
                                            (SELECT 	nvl(sum(LIMIT_CREDI),0) AS LIMITE
                                             FROM
                                                 XRAYADMIN.SGC_CLIENTE_LINEA_CREDITO
                                             WHERE 1=1  and CODIG_EMPRE = UM.CODEMPRESA
                                               AND CODIG_CLIEN =C.COD_CLIENTE
                                               AND VIGEN = TO_CHAR(SYSDATE, 'MM-YYYY')
                                               AND ESTAD = 'A') LIMITECREDITO,
                                            LP.DESCRIPCION

                            FROM SYSADM.CLIENTE C
                                     INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                                     INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                                     INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                                     INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                                     INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
                                     INNER JOIN SYSADM.PRECIOS_C LP  ON LP.COD_LISTA = cl.COD_LISTA  AND LP.COMPANIA_VENTA =codempresax
                            WHERE 1=1
                              AND RV.DIA=(select translate(SUBSTR(to_char(diajornadax, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                          from dual)
                              AND UM.USUARIO=usernamex
                              AND C.STATUS_CLIENTE IN ('A','P')
                              AND LC.ESTAD='A'
                              AND nvl(C.USUAR_REGIS,'NONE')<>'EC'--EXCLUIR CLIENTES DIGITALES
                        )A
                            LEFT JOIN
                        (
                            SELECT UJ.CODCLIENTE,UJ.DESCCLIENTE,UJ.DIRECCION,LC.NEW_COORDENADAS,UJ.CODRUTA,UJ.DESCRUTA,UJ.CODLOCAL,UJ.CODLISTA,UJ.CODEMPRESA,
                                   CASE WHEN UJ.ISNEW IS NOT NULL THEN 'NEW'
                                        WHEN UJ.IDNOPEDIDO IS NOT NULL THEN 'N'
                                        WHEN UJ.NROPEDIDO IS NOT NULL THEN 'P'
                                        ELSE '0' END AS FLAGESTADO,
                                   CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                                   ESTADO ESTADOCLIENTE,
                                   FECHA_CUMPLE,
                                   TIPO_NEGOCIO,
                                   CANT_PEDIDOS,
                                   VELOCIDAD_PAGO,
                                   ANTIGUEDAD,
                                   DEUDA_VENCIDA,
                                   DEUDA_NOVENCIDA,
                                   LIMITECREDITO
                            FROM SYSADM.TBL_USUARIO_JORNADA UJ
                                     LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UJ.CODCLIENTE AND LC.COD_LOCAL=UJ.CODLOCAL
                            where USUARIO=usernamex
                              AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                        )B ON A.COD_CLIENTE=B.CODCLIENTE AND A.COD_LOCAL=B.CODLOCAL AND A.COD_LISTA=B.CODLISTA AND A.ESTADO=B.ESTADOCLIENTE AND A.DIRECCION=B.DIRECCION
                            --AND A.FECHA_CUMPLEANOS=TO_CHAR(B.FECHA_CUMPLE,'DD-MM-YYYY')
                            AND A.TIPONEGOCIO=B.TIPO_NEGOCIO
                            AND A.CANTPEDIDOS=B.CANT_PEDIDOS
                            AND A.VELOCIDADPAGO=B.VELOCIDAD_PAGO
                            AND A.ANTIGUEDAD=B.ANTIGUEDAD
                            AND A.DEUDAVENCIDA=B.DEUDA_VENCIDA
                            AND A.DEUDANOVENCIDA=B.DEUDA_NOVENCIDA
                            AND A.LIMITECREDITO=B.LIMITECREDITO
                    WHERE B.CODCLIENTE IS  NULL;

                    if countnewx=0 then
                        open clientesx for
                            SELECT UJ.CODCLIENTE,UJ.DESCCLIENTE,UJ.DIRECCION,LC.NEW_COORDENADAS,UJ.CODRUTA,UJ.DESCRUTA,UJ.CODLOCAL,UJ.CODLISTA,UJ.CODEMPRESA,
                                   CASE WHEN UJ.ISNEW IS NOT NULL THEN 'NEW'
                                        WHEN UJ.IDNOPEDIDO IS NOT NULL THEN 'N'
                                        WHEN UJ.NROPEDIDO IS NOT NULL THEN 'P'
                                        ELSE '0' END AS FLAGESTADO,
                                   CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                                   ESTADO ESTADOCLIENTE,TO_CHAR(FECHA,'YYYY-MM-DD') FECHA,
                                   TO_CHAR(FECHA_CUMPLE,'YYYY-MM-DD') FECHA_CUMPLE,
                                   TIPO_NEGOCIO,
                                   CANT_PEDIDOS,
                                   VELOCIDAD_PAGO,
                                   ANTIGUEDAD,
                                   DEUDA_VENCIDA,
                                   DEUDA_NOVENCIDA,
                                   LIMITECREDITO,DESCLISTA,UJ.NEW_TELEFONO,UJ.IS_AUDITADO
                            FROM SYSADM.TBL_USUARIO_JORNADA UJ
                                     LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UJ.CODCLIENTE AND LC.COD_LOCAL=UJ.CODLOCAL
                            where USUARIO=usernamex AND uj.CODSEDE=codsedex
                              AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD') ;

                    else

                        OPEN clientesx FOR
                            SELECT A.*
                            FROM(
                                    SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                                    LC.NEW_COORDENADAS,
                                                    LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA ,C.STATUS_CLIENTE as ESTADO,
                                                    TO_CHAR(FECHA_CUMPLEANOS) FECHA_CUMPLEANOS,--FECHA CUMPLEAÑOS
                                                    (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='006' AND LLAVE=C.TIPO_CLIENTE) TIPONEGOCIO, --TIPONEGOCIO

                                                    (SELECT COUNT(DISTINCT NRO_PEDIDO)
                                                     FROM SYSADM.SPEDIDO_HEADER WHERE COD_CLIENTE=C.COD_CLIENTE AND STATUS_PEDIDO='G') CANTPEDIDOS,
                                                    (
                                                        SELECT DISTINCT TRUNC(NVL(SUM(CASE WHEN((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO)<0 then 0 else ((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO) END)/COUNT(CXC.CDCC_SECUENCIA),0))
                                                        FROM SYSADM.DOCUMENTO_CXC CXC
                                                                 INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND ON CXC.CDCC_CONDVENTA=COND.LLAVE
                                                                 INNER JOIN SYSADM.SPEDIDO_HEADER SPH ON CXC.CDCC_COMPANIA=SPH.COMPANIA_VENTA  AND  CXC.NDCC_PEDIDOOIH=SPH.NRO_PEDIDO
                                                                 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION AND SPH.COD_LISTA_PRECIOS=LPC.LISTAPRECIO AND LPC.LOCALIDAD=SPH.DPTO_ORIGEN
                                                        WHERE 1=1 AND SPH.COD_CIA='00' AND LPC.ESTAD='A'
                                                          AND CXC.CDCC_CLIENTE=C.COD_CLIENTE
                                                          AND SDCC_STATUS='CA' AND COND.FLAG10=1
                                                          AND MDCC_MONTO>0 ) VELOCIDADPAGO,--VELOCIDAD PAGO

                                                    (
                                                        SELECT   D1.AÑOS||' A-' ||D1.MESES||' M '
                                                        FROM
                                                            (
                                                                SELECT D.AÑOS,TRUNC(((D.DIAS-D.AÑOS)*360/30)) MESES,((D.DIAS-D.AÑOS)*360/30)DIAS2,D.COD_CLIENTE
                                                                FROM
                                                                    (
                                                                        select FECHA_CREACION,(TRUNC((trunc(sysdate) - FECHA_CREACION)/360)) AÑOS,((trunc(sysdate) - FECHA_CREACION)/360) DIAS,COD_CLIENTE
                                                                        from SYSADM.CLIENTE
                                                                    )D
                                                            )D1 WHERE D1.COD_CLIENTE=C.COD_CLIENTE ) ANTIGUEDAD,--  ANTIGUEDAD
                                                    (
                                                        SELECT NVL(SUM(VENC.DEUDAVENCIDA),0)
                                                        FROM
                                                            (
                                                                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDAVENCIDA,CDCC_COMPANIA,CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                                    FROM SYSADM.SGC_COBRANZA
                                                                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                                                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                                WHERE 1=1
                                                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                                  AND CXC.MDCC_MONTO>0
                                                                  and (SYSDATE-CXC.FDCC_EMISION)>NVL(LPC.DIAS_PLAZO,0)
                                                            )VENC  WHERE 1=1	AND VENC.CDCC_COMPANIA=UM.CODEMPRESA AND VENC.CDCC_CLIENTE=C.COD_CLIENTE

                                                    ) DEUDAVENCIDA,--DEUDA VENCIDA
                                                    (
                                                        SELECT NVL(SUM(NOVENC.DEUDANOVENCIDA),0)
                                                        FROM
                                                            (
                                                                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDANOVENCIDA,CXC.CDCC_COMPANIA,CXC.CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                                    FROM SYSADM.SGC_COBRANZA
                                                                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                                                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                                WHERE 1=1
                                                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                                  AND CXC.MDCC_MONTO>0
                                                                  and (SYSDATE-CXC.FDCC_EMISION)<=NVL(LPC.DIAS_PLAZO,0)
                                                            )NOVENC where 1=1 and NOVENC.CDCC_COMPANIA=UM.CODEMPRESA
                                                                      AND NOVENC.CDCC_CLIENTE=C.COD_CLIENTE
                                                    ) DEUDANOVENCIDA ,
                                                    (SELECT 	nvl(sum(LIMIT_CREDI),0) AS LIMITE
                                                     FROM
                                                         XRAYADMIN.SGC_CLIENTE_LINEA_CREDITO
                                                     WHERE 1=1  and CODIG_EMPRE = UM.CODEMPRESA
                                                       AND CODIG_CLIEN =C.COD_CLIENTE
                                                       AND VIGEN = TO_CHAR(SYSDATE, 'MM-YYYY')
                                                       AND ESTAD = 'A') LIMITECREDITO,
                                                    LP.DESCRIPCION,C.NEW_TELEFONO,LC.IS_AUDITADO
                                    FROM SYSADM.CLIENTE C
                                             INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                                             INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                                             INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                                             INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                                             INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
                                             INNER JOIN SYSADM.PRECIOS_C LP  ON LP.COD_LISTA = cl.COD_LISTA  AND LP.COMPANIA_VENTA =codempresax
                                    WHERE 1=1
                                      AND RV.DIA=(select translate(SUBSTR(to_char(diajornadax, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                                  from dual)
                                      AND UM.USUARIO=usernamex
                                      AND C.STATUS_CLIENTE IN ('A','P')
                                      AND LC.ESTAD='A'
                                      AND nvl(C.USUAR_REGIS,'NONE')<>'EC'--EXCLUIR CLIENTES DIGITALES
                                )A
                                    LEFT JOIN
                                (
                                    SELECT UJ.CODCLIENTE,UJ.DESCCLIENTE,UJ.DIRECCION,LC.NEW_COORDENADAS,UJ.CODRUTA,UJ.DESCRUTA,UJ.CODLOCAL,UJ.CODLISTA,UJ.CODEMPRESA,
                                           CASE WHEN UJ.ISNEW IS NOT NULL THEN 'NEW'
                                                WHEN UJ.IDNOPEDIDO IS NOT NULL THEN 'N'
                                                WHEN UJ.NROPEDIDO IS NOT NULL THEN 'P'
                                                ELSE '0' END AS FLAGESTADO,
                                           CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                                           ESTADO ESTADOCLIENTE,
                                           FECHA_CUMPLE,
                                           TIPO_NEGOCIO,
                                           CANT_PEDIDOS,
                                           VELOCIDAD_PAGO,
                                           ANTIGUEDAD,
                                           DEUDA_VENCIDA,
                                           DEUDA_NOVENCIDA,
                                           LIMITECREDITO
                                    FROM SYSADM.TBL_USUARIO_JORNADA UJ
                                             LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UJ.CODCLIENTE AND LC.COD_LOCAL=UJ.CODLOCAL
                                    where USUARIO=usernamex AND uj.CODSEDE=codsedex
                                      AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                                )B ON A.COD_CLIENTE=B.CODCLIENTE AND A.COD_LOCAL=B.CODLOCAL AND A.COD_LISTA=B.CODLISTA AND A.ESTADO=B.ESTADOCLIENTE AND A.DIRECCION=B.DIRECCION
                                    --AND A.FECHA_CUMPLEANOS=TO_CHAR(B.FECHA_CUMPLE,'DD-MM-YYYY')
                                    AND A.TIPONEGOCIO=B.TIPO_NEGOCIO
                                    AND A.CANTPEDIDOS=B.CANT_PEDIDOS
                                    AND A.VELOCIDADPAGO=B.VELOCIDAD_PAGO
                                    AND A.ANTIGUEDAD=B.ANTIGUEDAD
                                    AND A.DEUDAVENCIDA=B.DEUDA_VENCIDA
                                    AND A.DEUDANOVENCIDA=B.DEUDA_NOVENCIDA
                                    AND A.LIMITECREDITO=B.LIMITECREDITO
                            WHERE B.CODCLIENTE IS  NULL;

                        LOOP
                            FETCH clientesx INTO codclientex,descclientex,direccionx,coordenadaoldx,codrutax,descrutax,codlocalx,codlistax,codempresax,estadox,fechacumplex,tiponegociox,cantpedidosx,velocidadpagox,antiguedadx,deudavencidax,deudanovencidax,limitecreditox,desclistapreciosx,nrotelefonox,isauditadox;
                            EXIT WHEN clientesx%NOTFOUND;


                            SELECT COUNT(*) INTO countauxx
                            FROM SYSADM.TBL_USUARIO_JORNADA
                            WHERE 1=1
                              AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                              AND USUARIO=usernamex
                              and CODSEDE=codsedex
                              and CODLOCAL=codlocalx
                              AND CODCLIENTE=codclientex ;--AND CODRUTA=codrutax ;--AND ESTADO=estadox;

                            IF   countauxx=0 THEN

                                INSERT INTO SYSADM.TBL_USUARIO_JORNADA(USUARIO,CODSEDE,FECHA,CODCLIENTE,DESCCLIENTE,CODLOCAL,CODRUTA,DESCRUTA,DIRECCION,
                                                                       COORDENADA_ANT,CODLISTA,DESCLISTA,CODEMPRESA,ESTADO,FECHA_CUMPLE,TIPO_NEGOCIO,CANT_PEDIDOS,VELOCIDAD_PAGO,ANTIGUEDAD,DEUDA_VENCIDA,DEUDA_NOVENCIDA,LIMITECREDITO,FECHA_TRANS,NEW_TELEFONO,IS_AUDITADO)
                                VALUES(usernamex,codsedex,to_date(SUBSTR(diajornadax,0,10)),codclientex,descclientex,codlocalx,codrutax,descrutax,direccionx,
                                       coordenadaoldx,codlistax,desclistapreciosx,codempresax,estadox,to_date(fechacumplex),tiponegociox,cantpedidosx,velocidadpagox,antiguedadx,deudavencidax,deudanovencidax,limitecreditox,SYSDATE,nrotelefonox,isauditadox);

                            ELSE
                                UPDATE SYSADM.TBL_USUARIO_JORNADA
                                SET CODLISTA=codlistax ,ESTADO=estadox,DIRECCION=direccionx,CANT_PEDIDOS=cantpedidosx,
                                    LIMITECREDITO=limitecreditox,DEUDA_NOVENCIDA=deudanovencidax,DEUDA_VENCIDA=deudavencidax,TIPO_NEGOCIO=tiponegociox,DESCLISTA=desclistapreciosx,ANTIGUEDAD=antiguedadx,NEW_TELEFONO=nrotelefonox,IS_AUDITADO=isauditadox
                                WHERE CODCLIENTE=codclientex and CODLOCAL=codlocalx
                                  AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                                  AND USUARIO=usernamex and CODSEDE=codsedex;

                            END IF;

                        END LOOP;
                        CLOSE clientesx;

                        OPEN clientesx FOR
                            SELECT UJ.CODCLIENTE,UJ.DESCCLIENTE,UJ.DIRECCION,LC.NEW_COORDENADAS,UJ.CODRUTA,UJ.DESCRUTA,UJ.CODLOCAL,UJ.CODLISTA,UJ.CODEMPRESA,
                                   CASE WHEN UJ.IDNOPEDIDO IS NOT NULL THEN 'N'
                                        WHEN UJ.NROPEDIDO IS NOT NULL THEN 'P'
                                        ELSE '0' END AS FLAGESTADO,
                                   CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                                   ESTADO ESTADOCLIENTE,TO_CHAR(FECHA,'YYYY-MM-DD') FECHA,
                                   TO_CHAR(FECHA_CUMPLE,'YYYY-MM-DD') FECHA_CUMPLE,
                                   TIPO_NEGOCIO,
                                   CANT_PEDIDOS,
                                   VELOCIDAD_PAGO,
                                   ANTIGUEDAD,
                                   DEUDA_VENCIDA,
                                   DEUDA_NOVENCIDA,
                                   LIMITECREDITO,DESCLISTA,UJ.NEW_TELEFONO,UJ.IS_AUDITADO
                            FROM SYSADM.TBL_USUARIO_JORNADA UJ
                                     LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UJ.CODCLIENTE AND LC.COD_LOCAL=UJ.CODLOCAL
                            where USUARIO=usernamex and CODSEDE=codsedex
                              AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD') ;

                    end if;

                end if;

            ELSE




                open clientesx for
                    select 'X',' CARGANDO..!!','','','','','','','','',0 ignoregps FROM SYSADM.dual;


            end if;

        ELSE

            open clientesx for
                select '','' FROM SYSADM.dual where rownum<0;

        end if;




        return clientesx;

    END CLIENTES_OFFLINE;






    PROCEDURE  ONLY_CLIENTES_OFFLINE(usernamex IN VARCHAR2,clientesnewx in out types.ref_cursor,clientesoldx IN OUT types.ref_cursor)
        IS
        clientesx types.ref_cursor;
        clientesdelx types.ref_cursor;

        countjornadax number;
        codclientex VARCHAR2(20);
        codclientedelx VARCHAR2(20);
        codlocaldelx VARCHAR2(2);
        descclientex VARCHAR2(200);
        direccionx VARCHAR2(400);
        coordenadaoldx VARCHAR2(400);
        codrutax VARCHAR2(20);
        descrutax VARCHAR2(200);
        codlocalx VARCHAR2(2);
        codlistax VARCHAR2(10);
        codempresax VARCHAR2(2);
        codsedex VARCHAR2(3);
        estadox VARCHAR2(2);
        diajornadax DATE;
        countnewx number;
        countauxx number;
        ignoresecx number;

        fechacumplex VARCHAR2(10);
        tiponegociox VARCHAR2(200);
        cantpedidosx number;
        velocidadpagox number;
        antiguedadx VARCHAR2(200);
        deudavencidax number(10,2);
        deudanovencidax number(10,2);
        limitecreditox number(10,2);
        desclistapreciosx VARCHAR2(200);

    BEGIN

        diajornadax:=SYSDATE;

        SELECT distinct CODEMPRESA,CODSEDE  into codempresax,codsedex
        FROM SYSADM.TB_USUARIO_MESA
        WHERE USUARIO=usernamex AND TB_TIPO_CODIGO='R';


        OPEN clientesx FOR
            SELECT A.*
            FROM(
                    SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                    LC.NEW_COORDENADAS,
                                    LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA ,C.STATUS_CLIENTE as ESTADO,
                                    TO_CHAR(FECHA_CUMPLEANOS) FECHA_CUMPLEANOS,--FECHA CUMPLEAÑOS
                                    (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='006' AND LLAVE=C.TIPO_CLIENTE) TIPONEGOCIO, --TIPONEGOCIO

                                    (SELECT COUNT(DISTINCT NRO_PEDIDO)
                                     FROM SYSADM.SPEDIDO_HEADER WHERE COD_CLIENTE=C.COD_CLIENTE AND STATUS_PEDIDO='G') CANTPEDIDOS,
                                    (
                                        SELECT DISTINCT TRUNC(NVL(SUM(CASE WHEN((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO)<0 then 0 else ((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO) END)/COUNT(CXC.CDCC_SECUENCIA),0))
                                        FROM SYSADM.DOCUMENTO_CXC CXC
                                                 INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND ON CXC.CDCC_CONDVENTA=COND.LLAVE
                                                 INNER JOIN SYSADM.SPEDIDO_HEADER SPH ON CXC.CDCC_COMPANIA=SPH.COMPANIA_VENTA  AND  CXC.NDCC_PEDIDOOIH=SPH.NRO_PEDIDO
                                                 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION AND SPH.COD_LISTA_PRECIOS=LPC.LISTAPRECIO AND LPC.LOCALIDAD=SPH.DPTO_ORIGEN
                                        WHERE 1=1 AND SPH.COD_CIA='00' AND LPC.ESTAD='A'
                                          AND CXC.CDCC_CLIENTE=C.COD_CLIENTE
                                          AND SDCC_STATUS='CA' AND COND.FLAG10=1
                                          AND MDCC_MONTO>0 ) VELOCIDADPAGO,--VELOCIDAD PAGO

                                    (
                                        SELECT   D1.AÑOS||' A-' ||D1.MESES||' M '
                                        FROM
                                            (
                                                SELECT D.AÑOS,TRUNC(((D.DIAS-D.AÑOS)*360/30)) MESES,((D.DIAS-D.AÑOS)*360/30)DIAS2,D.COD_CLIENTE
                                                FROM
                                                    (
                                                        select FECHA_CREACION,(TRUNC((trunc(sysdate) - FECHA_CREACION)/360)) AÑOS,((trunc(sysdate) - FECHA_CREACION)/360) DIAS,COD_CLIENTE
                                                        from SYSADM.CLIENTE
                                                    )D
                                            )D1 WHERE D1.COD_CLIENTE=C.COD_CLIENTE ) ANTIGUEDAD,--  ANTIGUEDAD
                                    (
                                        SELECT NVL(SUM(VENC.DEUDAVENCIDA),0)
                                        FROM
                                            (
                                                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDAVENCIDA,CDCC_COMPANIA,CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                    FROM SYSADM.SGC_COBRANZA
                                                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                WHERE 1=1
                                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                  AND CXC.MDCC_MONTO>0
                                                  and (SYSDATE-CXC.FDCC_EMISION)>NVL(LPC.DIAS_PLAZO,0)
                                            )VENC  WHERE 1=1	AND VENC.CDCC_COMPANIA=UM.CODEMPRESA AND VENC.CDCC_CLIENTE=C.COD_CLIENTE

                                    ) DEUDAVENCIDA,--DEUDA VENCIDA
                                    (
                                        SELECT NVL(SUM(NOVENC.DEUDANOVENCIDA),0)
                                        FROM
                                            (
                                                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDANOVENCIDA,CXC.CDCC_COMPANIA,CXC.CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                    FROM SYSADM.SGC_COBRANZA
                                                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                WHERE 1=1
                                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                  AND CXC.MDCC_MONTO>0
                                                  and (SYSDATE-CXC.FDCC_EMISION)<=NVL(LPC.DIAS_PLAZO,0)
                                            )NOVENC where 1=1 and NOVENC.CDCC_COMPANIA=UM.CODEMPRESA
                                                      AND NOVENC.CDCC_CLIENTE=C.COD_CLIENTE
                                    ) DEUDANOVENCIDA ,
                                    (SELECT 	nvl(sum(LIMIT_CREDI),0) AS LIMITE
                                     FROM
                                         XRAYADMIN.SGC_CLIENTE_LINEA_CREDITO
                                     WHERE 1=1  and CODIG_EMPRE = UM.CODEMPRESA
                                       AND CODIG_CLIEN =C.COD_CLIENTE
                                       AND VIGEN = TO_CHAR(SYSDATE, 'MM-YYYY')
                                       AND ESTAD = 'A') LIMITECREDITO,
                                    LP.DESCRIPCION
                    FROM SYSADM.CLIENTE C
                             INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                             INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                             INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                             INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                             INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
                             INNER JOIN SYSADM.PRECIOS_C LP  ON LP.COD_LISTA = cl.COD_LISTA  AND LP.COMPANIA_VENTA =codempresax
                    WHERE 1=1
                      AND RV.DIA=(select translate(SUBSTR(to_char(diajornadax, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                  from dual)
                      AND UM.USUARIO=usernamex
                      AND C.STATUS_CLIENTE IN ('A','P')
                      AND LC.ESTAD='A' )A
                    LEFT JOIN
                (
                    SELECT UJ.CODCLIENTE,UJ.DESCCLIENTE,UJ.DIRECCION,LC.NEW_COORDENADAS,UJ.CODRUTA,UJ.DESCRUTA,UJ.CODLOCAL,UJ.CODLISTA,UJ.CODEMPRESA,
                           CASE WHEN UJ.ISNEW IS NOT NULL THEN 'NEW'
                                WHEN UJ.IDNOPEDIDO IS NOT NULL THEN 'N'
                                WHEN UJ.NROPEDIDO IS NOT NULL THEN 'P'
                                ELSE '0' END AS FLAGESTADO,
                           CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                           ESTADO ESTADOCLIENTE,
                           FECHA_CUMPLE,
                           TIPO_NEGOCIO,
                           CANT_PEDIDOS,
                           VELOCIDAD_PAGO,
                           ANTIGUEDAD,
                           DEUDA_VENCIDA,
                           DEUDA_NOVENCIDA,
                           LIMITECREDITO
                    FROM SYSADM.TBL_USUARIO_JORNADA UJ
                             LEFT JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UJ.CODCLIENTE AND LC.COD_LOCAL=UJ.CODLOCAL
                    where USUARIO=usernamex AND uj.CODSEDE=codsedex
                      AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                )B ON A.COD_CLIENTE=B.CODCLIENTE AND A.COD_LOCAL=B.CODLOCAL AND A.COD_LISTA=B.CODLISTA AND A.ESTADO=B.ESTADOCLIENTE AND A.DIRECCION=B.DIRECCION
                    --AND A.TIPONEGOCIO=B.TIPO_NEGOCIO
                    --AND A.CANTPEDIDOS=B.CANT_PEDIDOS
                    --AND A.VELOCIDADPAGO=B.VELOCIDAD_PAGO
                    --AND A.ANTIGUEDAD=B.ANTIGUEDAD
                    AND A.DEUDAVENCIDA=B.DEUDA_VENCIDA
                    AND A.DEUDANOVENCIDA=B.DEUDA_NOVENCIDA
                    AND A.LIMITECREDITO=B.LIMITECREDITO
            WHERE B.CODCLIENTE IS  NULL;



        OPEN clientesnewx FOR
            SELECT A.COD_CLIENTE,A.DESC_CLIENTE,A.DIRECCION ,
                   A.NEW_COORDENADAS,A.RUTA_DESPACHO,A.DESCRUTA,A.COD_LOCAL,A.COD_LISTA,A.CODEMPRESA,
                   NVL(B.FLAGESTADO,'0'),
                   CASE WHEN A.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                   A.ESTADO,
                   TO_CHAR(SYSDATE,'YYYY-MM-DD') FECHA,
                   A.FECHA_CUMPLEANOS,
                   A.TIPONEGOCIO,
                   A.CANTPEDIDOS,
                   A.VELOCIDADPAGO,
                   A.ANTIGUEDAD,
                   A.DEUDAVENCIDA,
                   A.DEUDANOVENCIDA,
                   A.LIMITECREDITO,A.DESCRIPCION
            FROM(
                    SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                    LC.NEW_COORDENADAS,
                                    LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA ,C.STATUS_CLIENTE as ESTADO,
                                    TO_CHAR(FECHA_CUMPLEANOS) FECHA_CUMPLEANOS,--FECHA CUMPLEAÑOS
                                    (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='006' AND LLAVE=C.TIPO_CLIENTE) TIPONEGOCIO, --TIPONEGOCIO

                                    (SELECT COUNT(DISTINCT NRO_PEDIDO)
                                     FROM SYSADM.SPEDIDO_HEADER WHERE COD_CLIENTE=C.COD_CLIENTE AND STATUS_PEDIDO='G') CANTPEDIDOS,
                                    (
                                        SELECT DISTINCT TRUNC(NVL(SUM(CASE WHEN((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO)<0 then 0 else ((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO) END)/COUNT(CXC.CDCC_SECUENCIA),0))
                                        FROM SYSADM.DOCUMENTO_CXC CXC
                                                 INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND ON CXC.CDCC_CONDVENTA=COND.LLAVE
                                                 INNER JOIN SYSADM.SPEDIDO_HEADER SPH ON CXC.CDCC_COMPANIA=SPH.COMPANIA_VENTA  AND  CXC.NDCC_PEDIDOOIH=SPH.NRO_PEDIDO
                                                 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION AND SPH.COD_LISTA_PRECIOS=LPC.LISTAPRECIO AND LPC.LOCALIDAD=SPH.DPTO_ORIGEN
                                        WHERE 1=1 AND SPH.COD_CIA='00' AND LPC.ESTAD='A'
                                          AND CXC.CDCC_CLIENTE=C.COD_CLIENTE
                                          AND SDCC_STATUS='CA' AND COND.FLAG10=1
                                          AND MDCC_MONTO>0 ) VELOCIDADPAGO,--VELOCIDAD PAGO

                                    (
                                        SELECT   D1.AÑOS||' A-' ||D1.MESES||' M '
                                        FROM
                                            (
                                                SELECT D.AÑOS,TRUNC(((D.DIAS-D.AÑOS)*360/30)) MESES,((D.DIAS-D.AÑOS)*360/30)DIAS2,D.COD_CLIENTE
                                                FROM
                                                    (
                                                        select FECHA_CREACION,(TRUNC((trunc(sysdate) - FECHA_CREACION)/360)) AÑOS,((trunc(sysdate) - FECHA_CREACION)/360) DIAS,COD_CLIENTE
                                                        from SYSADM.CLIENTE
                                                    )D
                                            )D1 WHERE D1.COD_CLIENTE=C.COD_CLIENTE ) ANTIGUEDAD,--  ANTIGUEDAD
                                    (
                                        SELECT NVL(SUM(VENC.DEUDAVENCIDA),0)
                                        FROM
                                            (
                                                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDAVENCIDA,CDCC_COMPANIA,CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                    FROM SYSADM.SGC_COBRANZA
                                                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                WHERE 1=1
                                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                  AND CXC.MDCC_MONTO>0
                                                  and (SYSDATE-CXC.FDCC_EMISION)>NVL(LPC.DIAS_PLAZO,0)
                                            )VENC  WHERE 1=1	AND VENC.CDCC_COMPANIA=UM.CODEMPRESA AND VENC.CDCC_CLIENTE=C.COD_CLIENTE

                                    ) DEUDAVENCIDA,--DEUDA VENCIDA
                                    (
                                        SELECT NVL(SUM(NOVENC.DEUDANOVENCIDA),0)
                                        FROM
                                            (
                                                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDANOVENCIDA,CXC.CDCC_COMPANIA,CXC.CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                    FROM SYSADM.SGC_COBRANZA
                                                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                WHERE 1=1
                                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                  AND CXC.MDCC_MONTO>0
                                                  and (SYSDATE-CXC.FDCC_EMISION)<=NVL(LPC.DIAS_PLAZO,0)
                                            )NOVENC where 1=1 and NOVENC.CDCC_COMPANIA=UM.CODEMPRESA
                                                      AND NOVENC.CDCC_CLIENTE=C.COD_CLIENTE
                                    ) DEUDANOVENCIDA ,
                                    (SELECT 	nvl(sum(LIMIT_CREDI),0) AS LIMITE
                                     FROM
                                         XRAYADMIN.SGC_CLIENTE_LINEA_CREDITO
                                     WHERE 1=1  and CODIG_EMPRE = UM.CODEMPRESA
                                       AND CODIG_CLIEN =C.COD_CLIENTE
                                       AND VIGEN = TO_CHAR(SYSDATE, 'MM-YYYY')
                                       AND ESTAD = 'A') LIMITECREDITO,
                                    LP.DESCRIPCION
                    FROM SYSADM.CLIENTE C
                             INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                             INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                             INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                             INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                             INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
                             INNER JOIN SYSADM.PRECIOS_C LP  ON LP.COD_LISTA = cl.COD_LISTA  AND LP.COMPANIA_VENTA =codempresax
                    WHERE 1=1
                      AND RV.DIA=(select translate(SUBSTR(to_char(diajornadax, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                  from dual)
                      AND UM.USUARIO=usernamex
                      AND C.STATUS_CLIENTE IN ('A','P')
                      AND LC.ESTAD='A' )A
                    left JOIN
                (
                    SELECT UJ.CODCLIENTE,UJ.DESCCLIENTE,UJ.DIRECCION,LC.NEW_COORDENADAS,UJ.CODRUTA,UJ.DESCRUTA,UJ.CODLOCAL,UJ.CODLISTA,UJ.CODEMPRESA,
                           CASE WHEN UJ.ISNEW IS NOT NULL THEN 'NEW'
                                WHEN UJ.IDNOPEDIDO IS NOT NULL THEN 'N'
                                WHEN UJ.NROPEDIDO IS NOT NULL THEN 'P'
                                ELSE '0' END AS FLAGESTADO,
                           CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                           ESTADO ESTADOCLIENTE,TO_CHAR(FECHA,'YYYY-MM-DD') FECHA,
                           FECHA_CUMPLE,
                           TIPO_NEGOCIO,
                           CANT_PEDIDOS,
                           VELOCIDAD_PAGO,
                           ANTIGUEDAD,
                           DEUDA_VENCIDA,
                           DEUDA_NOVENCIDA,
                           LIMITECREDITO,DESCLISTA
                    FROM SYSADM.TBL_USUARIO_JORNADA UJ
                             INNER JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UJ.CODCLIENTE AND LC.COD_LOCAL=UJ.CODLOCAL
                    where USUARIO=usernamex AND uj.CODSEDE=codsedex
                      AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                )B ON A.COD_CLIENTE=B.CODCLIENTE
                    AND A.COD_LOCAL=B.CODLOCAL
                    AND A.COD_LISTA=B.CODLISTA
                    -- AND A.RUTA_DESPACHO=B.CODRUTA
                    AND A.ESTADO=B.ESTADOCLIENTE
                    AND A.DIRECCION=B.DIRECCION
                --AND A.TIPONEGOCIO=B.TIPO_NEGOCIO
                --AND A.CANTPEDIDOS=B.CANT_PEDIDOS
                --AND A.VELOCIDADPAGO=B.VELOCIDAD_PAGO
                --AND A.ANTIGUEDAD=B.ANTIGUEDAD
                --AND A.DEUDAVENCIDA=B.DEUDA_VENCIDA
                --AND A.DEUDANOVENCIDA=B.DEUDA_NOVENCIDA
                --AND A.LIMITECREDITO=B.LIMITECREDITO
            WHERE B.CODCLIENTE IS  NULL;



        open clientesdelx FOR
            SELECT B.CODCLIENTE,B.CODLOCAL
            FROM(
                    SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                    LC.NEW_COORDENADAS,
                                    LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA ,C.STATUS_CLIENTE as ESTADO,
                                    TO_CHAR(FECHA_CUMPLEANOS) FECHA_CUMPLEANOS,--FECHA CUMPLEAÑOS
                                    (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='006' AND LLAVE=C.TIPO_CLIENTE) TIPONEGOCIO, --TIPONEGOCIO

                                    (SELECT COUNT(DISTINCT NRO_PEDIDO)
                                     FROM SYSADM.SPEDIDO_HEADER WHERE COD_CLIENTE=C.COD_CLIENTE AND STATUS_PEDIDO='G') CANTPEDIDOS,
                                    (
                                        SELECT DISTINCT TRUNC(NVL(SUM(CASE WHEN((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO)<0 then 0 else ((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO) END)/COUNT(CXC.CDCC_SECUENCIA),0))
                                        FROM SYSADM.DOCUMENTO_CXC CXC
                                                 INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND ON CXC.CDCC_CONDVENTA=COND.LLAVE
                                                 INNER JOIN SYSADM.SPEDIDO_HEADER SPH ON CXC.CDCC_COMPANIA=SPH.COMPANIA_VENTA  AND  CXC.NDCC_PEDIDOOIH=SPH.NRO_PEDIDO
                                                 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION AND SPH.COD_LISTA_PRECIOS=LPC.LISTAPRECIO AND LPC.LOCALIDAD=SPH.DPTO_ORIGEN
                                        WHERE 1=1 AND SPH.COD_CIA='00' AND LPC.ESTAD='A'
                                          AND CXC.CDCC_CLIENTE=C.COD_CLIENTE
                                          AND SDCC_STATUS='CA' AND COND.FLAG10=1
                                          AND MDCC_MONTO>0 ) VELOCIDADPAGO,--VELOCIDAD PAGO

                                    (
                                        SELECT   D1.AÑOS||' A-' ||D1.MESES||' M '
                                        FROM
                                            (
                                                SELECT D.AÑOS,TRUNC(((D.DIAS-D.AÑOS)*360/30)) MESES,((D.DIAS-D.AÑOS)*360/30)DIAS2,D.COD_CLIENTE
                                                FROM
                                                    (
                                                        select FECHA_CREACION,(TRUNC((trunc(sysdate) - FECHA_CREACION)/360)) AÑOS,((trunc(sysdate) - FECHA_CREACION)/360) DIAS,COD_CLIENTE
                                                        from SYSADM.CLIENTE
                                                    )D
                                            )D1 WHERE D1.COD_CLIENTE=C.COD_CLIENTE ) ANTIGUEDAD,--  ANTIGUEDAD
                                    (
                                        SELECT NVL(SUM(VENC.DEUDAVENCIDA),0)
                                        FROM
                                            (
                                                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDAVENCIDA,CDCC_COMPANIA,CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                    FROM SYSADM.SGC_COBRANZA
                                                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                WHERE 1=1
                                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                  AND CXC.MDCC_MONTO>0
                                                  and (SYSDATE-CXC.FDCC_EMISION)>NVL(LPC.DIAS_PLAZO,0)
                                            )VENC  WHERE 1=1	AND VENC.CDCC_COMPANIA=UM.CODEMPRESA AND VENC.CDCC_CLIENTE=C.COD_CLIENTE

                                    ) DEUDAVENCIDA,--DEUDA VENCIDA
                                    (
                                        SELECT NVL(SUM(NOVENC.DEUDANOVENCIDA),0)
                                        FROM
                                            (
                                                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDANOVENCIDA,CXC.CDCC_COMPANIA,CXC.CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                    FROM SYSADM.SGC_COBRANZA
                                                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                WHERE 1=1
                                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                  AND CXC.MDCC_MONTO>0
                                                  and (SYSDATE-CXC.FDCC_EMISION)<=NVL(LPC.DIAS_PLAZO,0)
                                            )NOVENC where 1=1 and NOVENC.CDCC_COMPANIA=UM.CODEMPRESA
                                                      AND NOVENC.CDCC_CLIENTE=C.COD_CLIENTE
                                    ) DEUDANOVENCIDA ,
                                    (SELECT 	nvl(sum(LIMIT_CREDI),0) AS LIMITE
                                     FROM
                                         XRAYADMIN.SGC_CLIENTE_LINEA_CREDITO
                                     WHERE 1=1  and CODIG_EMPRE = UM.CODEMPRESA
                                       AND CODIG_CLIEN =C.COD_CLIENTE
                                       AND VIGEN = TO_CHAR(SYSDATE, 'MM-YYYY')
                                       AND ESTAD = 'A') LIMITECREDITO,
                                    LP.DESCRIPCION
                    FROM SYSADM.CLIENTE C
                             INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                             INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                             INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                             INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                             INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
                             INNER JOIN SYSADM.PRECIOS_C LP  ON LP.COD_LISTA = cl.COD_LISTA  AND LP.COMPANIA_VENTA ='01'
                    WHERE 1=1
                      AND RV.DIA=(select translate(SUBSTR(to_char(diajornadax, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                  from dual)
                      AND UM.USUARIO=usernamex
                      AND C.STATUS_CLIENTE IN ('A','P')
                      AND LC.ESTAD='A' )A
                    RIGHT JOIN
                (
                    SELECT UJ.CODCLIENTE,UJ.DESCCLIENTE,UJ.DIRECCION,LC.NEW_COORDENADAS,UJ.CODRUTA,UJ.DESCRUTA,UJ.CODLOCAL,UJ.CODLISTA,UJ.CODEMPRESA,
                           CASE WHEN UJ.ISNEW IS NOT NULL THEN 'NEW'
                                WHEN UJ.IDNOPEDIDO IS NOT NULL THEN 'N'
                                WHEN UJ.NROPEDIDO IS NOT NULL THEN 'P'
                                ELSE '0' END AS FLAGESTADO,
                           CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                           ESTADO ESTADOCLIENTE,TO_CHAR(FECHA,'YYYY-MM-DD') FECHA,
                           FECHA_CUMPLE,
                           TIPO_NEGOCIO,
                           CANT_PEDIDOS,
                           VELOCIDAD_PAGO,
                           ANTIGUEDAD,
                           DEUDA_VENCIDA,
                           DEUDA_NOVENCIDA,
                           LIMITECREDITO,DESCLISTA
                    FROM SYSADM.TBL_USUARIO_JORNADA UJ
                             INNER JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UJ.CODCLIENTE AND LC.COD_LOCAL=UJ.CODLOCAL
                    where USUARIO=usernamex AND uj.CODSEDE=codsedex
                      AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                )B ON A.COD_CLIENTE=B.CODCLIENTE
                    AND A.COD_LOCAL=B.CODLOCAL
                    -- AND A.COD_LISTA=B.CODLISTA
                    --AND A.RUTA_DESPACHO=B.CODRUTA
                    AND A.ESTADO=B.ESTADOCLIENTE
                    AND A.DIRECCION=B.DIRECCION
                --AND A.TIPONEGOCIO=B.TIPO_NEGOCIO
                --AND A.CANTPEDIDOS=B.CANT_PEDIDOS
                --AND A.VELOCIDADPAGO=B.VELOCIDAD_PAGO
                --AND A.ANTIGUEDAD=B.ANTIGUEDAD
                --AND A.DEUDAVENCIDA=B.DEUDA_VENCIDA
                --AND A.DEUDANOVENCIDA=B.DEUDA_NOVENCIDA
                --AND A.LIMITECREDITO=B.LIMITECREDITO
            WHERE A.COD_CLIENTE IS  NULL;


        OPEN clientesoldx FOR
            SELECT B.CODCLIENTE,B.CODLOCAL
            FROM(
                    SELECT DISTINCT C.COD_CLIENTE,C.DESC_CLIENTE,LC.DIRECCION,
                                    LC.NEW_COORDENADAS,
                                    LC.RUTA_DESPACHO,R.DESCRUTA,LC.COD_LOCAL,CL.COD_LISTA,UM.CODEMPRESA ,C.STATUS_CLIENTE as ESTADO,
                                    TO_CHAR(FECHA_CUMPLEANOS) FECHA_CUMPLEANOS,--FECHA CUMPLEAÑOS
                                    (SELECT DESC1 FROM SYSADM.TABLAS WHERE CATEGORIA='006' AND LLAVE=C.TIPO_CLIENTE) TIPONEGOCIO, --TIPONEGOCIO

                                    (SELECT COUNT(DISTINCT NRO_PEDIDO)
                                     FROM SYSADM.SPEDIDO_HEADER WHERE COD_CLIENTE=C.COD_CLIENTE AND STATUS_PEDIDO='G') CANTPEDIDOS,
                                    (
                                        SELECT DISTINCT TRUNC(NVL(SUM(CASE WHEN((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO)<0 then 0 else ((CXC.FDCC_CANCELACION-CXC.FDCC_EMISION)-LPC.DIAS_PLAZO) END)/COUNT(CXC.CDCC_SECUENCIA),0))
                                        FROM SYSADM.DOCUMENTO_CXC CXC
                                                 INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND ON CXC.CDCC_CONDVENTA=COND.LLAVE
                                                 INNER JOIN SYSADM.SPEDIDO_HEADER SPH ON CXC.CDCC_COMPANIA=SPH.COMPANIA_VENTA  AND  CXC.NDCC_PEDIDOOIH=SPH.NRO_PEDIDO
                                                 INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION AND SPH.COD_LISTA_PRECIOS=LPC.LISTAPRECIO AND LPC.LOCALIDAD=SPH.DPTO_ORIGEN
                                        WHERE 1=1 AND SPH.COD_CIA='00' AND LPC.ESTAD='A'
                                          AND CXC.CDCC_CLIENTE=C.COD_CLIENTE
                                          AND SDCC_STATUS='CA' AND COND.FLAG10=1
                                          AND MDCC_MONTO>0 ) VELOCIDADPAGO,--VELOCIDAD PAGO

                                    (
                                        SELECT   D1.AÑOS||' A-' ||D1.MESES||' M '
                                        FROM
                                            (
                                                SELECT D.AÑOS,TRUNC(((D.DIAS-D.AÑOS)*360/30)) MESES,((D.DIAS-D.AÑOS)*360/30)DIAS2,D.COD_CLIENTE
                                                FROM
                                                    (
                                                        select FECHA_CREACION,(TRUNC((trunc(sysdate) - FECHA_CREACION)/360)) AÑOS,((trunc(sysdate) - FECHA_CREACION)/360) DIAS,COD_CLIENTE
                                                        from SYSADM.CLIENTE
                                                    )D
                                            )D1 WHERE D1.COD_CLIENTE=C.COD_CLIENTE ) ANTIGUEDAD,--  ANTIGUEDAD
                                    (
                                        SELECT NVL(SUM(VENC.DEUDAVENCIDA),0)
                                        FROM
                                            (
                                                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDAVENCIDA,CDCC_COMPANIA,CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                    FROM SYSADM.SGC_COBRANZA
                                                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                WHERE 1=1
                                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                  AND CXC.MDCC_MONTO>0
                                                  and (SYSDATE-CXC.FDCC_EMISION)>NVL(LPC.DIAS_PLAZO,0)
                                            )VENC  WHERE 1=1	AND VENC.CDCC_COMPANIA=UM.CODEMPRESA AND VENC.CDCC_CLIENTE=C.COD_CLIENTE

                                    ) DEUDAVENCIDA,--DEUDA VENCIDA
                                    (
                                        SELECT NVL(SUM(NOVENC.DEUDANOVENCIDA),0)
                                        FROM
                                            (
                                                SELECT  DISTINCT(CXC.CDCC_SECUENCIA),(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)) DEUDANOVENCIDA,CXC.CDCC_COMPANIA,CXC.CDCC_CLIENTE--,  nvl(sum(CXC.mdcc_saldo-NVL(CO.MONTOCOBRADO,0)),0) --INTO deudaVencidax
                                                FROM SYSADM.DOCUMENTO_CXC CXC
                                                         INNER JOIN SYSADM.CLIENTE CL ON CXC.CDCC_CLIENTECIA=CL.COD_CIA AND  CXC.CDCC_CLIENTE=CL.COD_CLIENTE
                                                         INNER JOIN (SELECT * FROM SYSADM.LP_CONDICION)LPC ON CXC.CDCC_COMPANIA=LPC.COMPANIA AND CXC.CDCC_CONDVENTA=LPC.CONDICION  AND LPC.LOCALIDAD=CXC.CDCC_ORIGPEDIDOOIH --AND LPC.CODIG_SEDE=CXC.CODSEDE
                                                         LEFT JOIN (SELECT CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC,SUM(MONTO_COBRADO) MONTOCOBRADO
                                                                    FROM SYSADM.SGC_COBRANZA
                                                                    WHERE ESTADO_COBRANZA IN ('I','D')
                                                                    GROUP BY CODEMPRESA,TIPODOCUMENTO,SECUENCIADOC)CO ON CO.CODEMPRESA=CXC.CDCC_COMPANIA AND CO.TIPODOCUMENTO=CXC.CDCC_TIPODOC AND CO.SECUENCIADOC=CXC.CDCC_SECUENCIA
                                                WHERE 1=1
                                                  AND (CXC.sdcc_status = 'AP' OR CXC.sdcc_status = 'PP')
                                                  and  CDCC_CONDVENTA in (SELECT DISTINCT LC.CONDICION from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS WHERE CATEGORIA='008')COND  ON  LC.CONDICION=COND.LLAVE where 1=1 AND FLAG10 IN (1) )
                                                  AND CXC.MDCC_MONTO>0
                                                  and (SYSDATE-CXC.FDCC_EMISION)<=NVL(LPC.DIAS_PLAZO,0)
                                            )NOVENC where 1=1 and NOVENC.CDCC_COMPANIA=UM.CODEMPRESA
                                                      AND NOVENC.CDCC_CLIENTE=C.COD_CLIENTE
                                    ) DEUDANOVENCIDA ,
                                    (SELECT 	nvl(sum(LIMIT_CREDI),0) AS LIMITE
                                     FROM
                                         XRAYADMIN.SGC_CLIENTE_LINEA_CREDITO
                                     WHERE 1=1  and CODIG_EMPRE = UM.CODEMPRESA
                                       AND CODIG_CLIEN =C.COD_CLIENTE
                                       AND VIGEN = TO_CHAR(SYSDATE, 'MM-YYYY')
                                       AND ESTAD = 'A') LIMITECREDITO,
                                    LP.DESCRIPCION
                    FROM SYSADM.CLIENTE C
                             INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                             INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                             INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                             INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                             INNER JOIN SYSADM.CLIENTE_LISTAS CL ON C.COD_CLIENTE=CL.COD_CLIENTE AND UM.CODEMPRESA=CL.COMPANIA_VENTA AND CL.TIpo_lista='01'
                             INNER JOIN SYSADM.PRECIOS_C LP  ON LP.COD_LISTA = cl.COD_LISTA  AND LP.COMPANIA_VENTA ='01'
                    WHERE 1=1
                      AND RV.DIA=(select translate(SUBSTR(to_char(diajornadax, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                  from dual)
                      AND UM.USUARIO=usernamex
                      AND C.STATUS_CLIENTE IN ('A','P')
                      AND LC.ESTAD='A' )A
                    RIGHT JOIN
                (
                    SELECT UJ.CODCLIENTE,UJ.DESCCLIENTE,UJ.DIRECCION,LC.NEW_COORDENADAS,UJ.CODRUTA,UJ.DESCRUTA,UJ.CODLOCAL,UJ.CODLISTA,UJ.CODEMPRESA,
                           CASE WHEN UJ.ISNEW IS NOT NULL THEN 'NEW'
                                WHEN UJ.IDNOPEDIDO IS NOT NULL THEN 'N'
                                WHEN UJ.NROPEDIDO IS NOT NULL THEN 'P'
                                ELSE '0' END AS FLAGESTADO,
                           CASE WHEN LC.NEW_COORDENADAS IS NULL THEN 0 ELSE 1 END ISGEOLOCALIZADO,
                           ESTADO ESTADOCLIENTE,TO_CHAR(FECHA,'YYYY-MM-DD') FECHA,
                           FECHA_CUMPLE,
                           TIPO_NEGOCIO,
                           CANT_PEDIDOS,
                           VELOCIDAD_PAGO,
                           ANTIGUEDAD,
                           DEUDA_VENCIDA,
                           DEUDA_NOVENCIDA,
                           LIMITECREDITO,DESCLISTA
                    FROM SYSADM.TBL_USUARIO_JORNADA UJ
                             INNER JOIN SYSADM.LOCALES_CLIENTE LC ON LC.COD_CLIENTE=UJ.CODCLIENTE AND LC.COD_LOCAL=UJ.CODLOCAL
                    where UJ.USUARIO=usernamex AND UJ.CODSEDE=codsedex
                      AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                )B ON A.COD_CLIENTE=B.CODCLIENTE
                    AND A.COD_LOCAL=B.CODLOCAL
                    --AND A.COD_LISTA=B.CODLISTA
                    --AND A.RUTA_DESPACHO=B.CODRUTA
                    AND A.ESTADO=B.ESTADOCLIENTE
                    AND A.DIRECCION=B.DIRECCION
                --AND A.TIPONEGOCIO=B.TIPO_NEGOCIO
                --AND A.CANTPEDIDOS=B.CANT_PEDIDOS
                --AND A.VELOCIDADPAGO=B.VELOCIDAD_PAGO
                --AND A.ANTIGUEDAD=B.ANTIGUEDAD
                --AND A.DEUDAVENCIDA=B.DEUDA_VENCIDA
                --AND A.DEUDANOVENCIDA=B.DEUDA_NOVENCIDA
                --AND A.LIMITECREDITO=B.LIMITECREDITO
            WHERE A.COD_CLIENTE IS  NULL;



        LOOP
            FETCH clientesdelx INTO codclientedelx,codlocaldelx;
            EXIT WHEN clientesdelx%NOTFOUND;
            --dbms_output.put_line(codclientedelx);

            DELETE FROM SYSADM.TBL_USUARIO_JORNADA
            WHERE  TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
              AND USUARIO=usernamex  AND CODSEDE=codsedex
              and CODLOCAL=codlocaldelx
              AND CODCLIENTE=codclientedelx;

        end loop;

        close clientesdelx;


        LOOP
            FETCH clientesx INTO codclientex,descclientex,direccionx,coordenadaoldx,codrutax,descrutax,codlocalx,codlistax,codempresax,estadox,fechacumplex,tiponegociox,cantpedidosx,velocidadpagox,antiguedadx,deudavencidax,deudanovencidax,limitecreditox,desclistapreciosx;
            EXIT WHEN clientesx%NOTFOUND;


            SELECT COUNT(*) INTO countauxx
            FROM SYSADM.TBL_USUARIO_JORNADA
            WHERE 1=1
              AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
              AND USUARIO=usernamex AND CODSEDE=codsedex
              and CODLOCAL=codlocalx
              AND CODCLIENTE=codclientex ;--AND CODRUTA=codrutax ;--AND ESTADO=estadox;

            IF   countauxx=0 THEN

                INSERT INTO SYSADM.TBL_USUARIO_JORNADA(USUARIO,CODSEDE,FECHA,CODCLIENTE,DESCCLIENTE,CODLOCAL,CODRUTA,DESCRUTA,DIRECCION,
                                                       COORDENADA_ANT,CODLISTA,DESCLISTA,CODEMPRESA,ESTADO,FECHA_CUMPLE,TIPO_NEGOCIO,CANT_PEDIDOS,VELOCIDAD_PAGO,ANTIGUEDAD,DEUDA_VENCIDA,DEUDA_NOVENCIDA,LIMITECREDITO,FECHA_TRANS)
                VALUES(usernamex,codsedex,to_date(SUBSTR(diajornadax,0,10)),codclientex,descclientex,codlocalx,codrutax,descrutax,direccionx,
                       coordenadaoldx,codlistax,desclistapreciosx,codempresax,estadox,to_date(fechacumplex),tiponegociox,cantpedidosx,velocidadpagox,antiguedadx,deudavencidax,deudanovencidax,limitecreditox,SYSDATE);

            ELSE
                UPDATE SYSADM.TBL_USUARIO_JORNADA
                SET CODLISTA=codlistax ,ESTADO=estadox,DIRECCION=direccionx,CANT_PEDIDOS=cantpedidosx,
                    LIMITECREDITO=limitecreditox,DEUDA_NOVENCIDA=deudanovencidax,DEUDA_VENCIDA=deudavencidax,TIPO_NEGOCIO=tiponegociox,DESCLISTA=desclistapreciosx,ANTIGUEDAD=antiguedadx
                WHERE CODCLIENTE=codclientex and CODLOCAL=codlocalx
                  AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(diajornadax,'YYYY-MM-DD')
                  AND USUARIO=usernamex AND CODSEDE=codsedex;

            END IF;

        END LOOP;
        CLOSE clientesx;


        COMMIT;
    EXCEPTION
        WHEN OTHERS THEN
            --rptax := -1;
            rollback;
        --mensajex :='SE ENCONTRÓ UN ERROR: '||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE||macNewx;


    END ONLY_CLIENTES_OFFLINE;








    PROCEDURE  DATOS_USUARIO_OFFLINE(usuariox IN VARCHAR2,cursordatosx IN OUT types.ref_cursor,cursorcondicionesx IN OUT types.ref_cursor)
        is
        codempx VARCHAR2(10);
        codsedex VARCHAR2(10);
        codmesax VARCHAR2(10);
    BEGIN


        SELECT distinct UM.CODEMPRESA,UM.CODSEDE,UM.CODMESA  INTO codempx,codsedex,codmesax
        FROM SYSADM.TB_USUARIO_MESA UM
                 INNER JOIN SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
        WHERE UM.USUARIO=usuariox AND UM.TB_TIPO_CODIGO='R';


        open cursordatosx for
            SELECT distinct UM.CODEMPRESA,codmesax,TM.DESCMESA,UM.TB_TIPO_CODIGO,TM.LOCALIDAD,
                            TB.DESC1,
                            UM.CODCANAL,TC.DESCCANAL,UM.CODVENDEDOR,AL.CODALMACEN,AL.DESCALMACEN,UM.CODSEDE,UM.PERMI_CREDI ,CASE WHEN VC.CODVENDEDOR IS NULL THEN 0 ELSE 1 END CAMPANIA
            FROM SYSADM.TB_USUARIO_MESA UM
                     INNER JOIN SYSADM.USUARIOS US ON US.USUARIO=UM.USUARIO
                     INNER JOIN SYSADM.TB_CANAL TC ON UM.CODCANAL=TC.CODCANAL
                     INNER JOIN SYSADM.TB_MESA TM ON TM.CODLOCALIDAD=UM.CODMESA
                     INNER JOIN SYSADM.TB_ALMACEN AL ON UM.CODEMPRESA=AL.CODEMPRESA  AND UM.CODSEDE=AL.CODIG_SEDE  and TM.LOCALIDAD=AL.CODLOCALIDAD
                     INNER JOIN SYSADM.TABLAS TB ON TB.CATEGORIA='424' AND TB.LLAVE=UM.CODEMPRESA||TM.LOCALIDAD
                     LEFT JOIN SYSADM.VENDEDOR_CAMPANIA VC ON VC.CODVENDEDOR=UM.CODVENDEDOR AND VC.STATUS_VENDEDOR='A'
            WHERE 1=1
              AND US.USUARIO=usuariox  AND US.ESTUSUARIO='H'
              AND  AL.ESTALMACEN='H'
              AND UM.ESTADO_EN_MESA='A'
            ORDER BY 9;





        open cursorcondicionesx for
            SELECT DISTINCT  LC.CONDICION,COND.DESC1,FLAG10,LISTAPRECIO
            from SYSADM.LP_CONDICION LC INNER JOIN (SELECT * FROM SYSADM.TABLAS
                                                    WHERE CATEGORIA='008')COND  ON  LC.COMPANIA=codempx
                AND LC.CONDICION=COND.LLAVE where
                    1=1
                                              AND LISTAPRECIO IN (
                    select DISTINCT(CODLISTA) from
                        SYSADM.TBL_USUARIO_JORNADA
                    WHERE USUARIO=usuariox AND CODSEDE=codsedex AND TO_CHAR(FECHA,'DD-MM-YYYY')=TO_CHAR(SYSDATE,'DD-MM-YYYY')
                )
                                              and LC.CODIG_SEDE=codsedex
                                              AND ESTAD='A';


    end DATOS_USUARIO_OFFLINE;


    PROCEDURE  ARTICULOS_OFFLINE(usernamex in varchar2,cursorarticulosx IN OUT types.ref_cursor,cursorsugeridox IN OUT types.ref_cursor,cursorfocusx IN OUT types.ref_cursor)
        is
        codempx VARCHAR2(10);
        codsedex VARCHAR2(10);
        codmesax VARCHAR2(10);
    BEGIN


        SELECT distinct UM.CODEMPRESA,UM.CODSEDE,UM.CODMESA  INTO codempx,codsedex,codmesax
        FROM SYSADM.TB_USUARIO_MESA UM
                 INNER JOIN SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
        WHERE UM.USUARIO=usernamex AND UM.TB_TIPO_CODIGO='R';





        OPEN cursorarticulosx FOR
/*	SELECT DISTINCT A.COD_ITEM,A.DESC_ITEM ,PD.UM_ITEM,PD.COD_LISTA,PD.PRECIO_BASE,SALDOS.SALDO,SALDOS.ALMACEN
		FROM SYSADM.ARTICULOS A
		INNER JOIN SYSADM.PRECIOS_D PD ON A.COD_ITEM=PD.COD_ITEM AND A.STATUS_ITEM='A' AND A.COD_LINEA <> '99'  AND PD.moneda_lista = 'S/.'
		INNER JOIN(
			SELECT distinct cod_item,(NVL(qty_fisica,0)- NVL(qty_comprometida,0)- NVL(qty_reservada,0)- NVL(qty_proyectos,0)) saldo,
			almacen,compania_venta_3 FROM SYSADM.saldos_almacen WHERE cod_cia = '00'
		)SALDOS ON SALDOS.cod_item=A.COD_ITEM AND SALDOS.compania_venta_3=PD.COMPANIA_VENTA
		WHERE 1=1 AND A.COD_CIA = '00'
		AND PD.COD_LISTA in (select DISTINCT LISTAPRECIO from SYSADM.LP_CONDICION where CODIG_SEDE
		IN (SELECT DISTINCT CODSEDE FROM SYSADM.TB_USUARIO_MESA
		WHERE USUARIO = usernamex))
		AND SALDOS.ALMACEN IN (
SELECT DISTINCT CODALMACEN
FROM SYSADM.TB_USUARIO_MESA UM
INNER JOIN SYSADM.USUARIOS US ON US.USUARIO=UM.USUARIO
 INNER JOIN SYSADM.TB_MESA TM ON TM.CODLOCALIDAD=UM.CODMESA
INNER JOIN SYSADM.TB_ALMACEN AL ON UM.CODEMPRESA=AL.CODEMPRESA
--AND UM.CODSEDE=AL.CODIG_SEDE  --LO HACE LENTO
and TM.LOCALIDAD=AL.CODLOCALIDAD AND AL.ESTALMACEN = 'H'
 WHERE US.USUARIO=usernamex AND US.ESTUSUARIO='H');
*/

            SELECT TB.COD_ITEM,TB.DESC_ITEM,TB.UM_ITEM,TB.COD_LISTA,TB.PRECIO_BASE,TB.SALDO,TB.ALMACEN,
                   tb.cond_pago,
                   CASE WHEN
                                CASE WHEN COMPANIA_VENTA IN ('05','08') THEN (PRECIO_BASE - (DSCTO_LISTA_PRECIO+ DSCTO_CP_ART +DSCTO_PROM_TIPEADOS)) ELSE
                                        (PRECIO_BASE - (DSCTO_LISTA_PRECIO+ DSCTO_CP_ART +DSCTO_PROM_TIPEADOS)) *1.18 END  IS NULL THEN
                            TB.PRECIO_BASE
                        ELSE
                            CASE WHEN COMPANIA_VENTA IN ('05','08') THEN (PRECIO_BASE - (DSCTO_LISTA_PRECIO+ DSCTO_CP_ART +DSCTO_PROM_TIPEADOS)) ELSE
                                    (PRECIO_BASE - (DSCTO_LISTA_PRECIO+ DSCTO_CP_ART +DSCTO_PROM_TIPEADOS)) *1.18 END

                       END AS  PRECIO_FINAL
            FROM
                (SELECT PD.COMPANIA_VENTA,ART.COD_ITEM,ART.DESC_ITEM,PD.UM_ITEM,PD.COD_LISTA,PD.PRECIO_BASE, (NVL(SA.qty_fisica,0)- NVL(SA.qty_comprometida,0)- NVL(SA.qty_reservada,0)- NVL(SA.qty_proyectos,0)) saldo,SA.ALMACEN,
                        d.cond_pago,
                        (PD.PRECIO_BASE*NVL(PC.DSCTO_CARACTERISTI,0)/100) AS DSCTO_LISTA_PRECIO,
                        (CASE WHEN PD.F_DSCTO_PROM_FIN < SYSDATE THEN 0 ELSE CASE WHEN PD.DSCTO_PROMOCION = 0 THEN 0 ELSE
                            (PD.PRECIO_BASE*PD.DSCTO_PROMOCION/100) END END) AS DSCTO_PROM_TIPEADOS,
                        CASE WHEN D.DSCTO_CP_ARTICULO = 0 THEN 0 ELSE (PD.PRECIO_BASE*D.DSCTO_CP_ARTICULO/100) END AS DSCTO_CP_ART
                 FROM SYSADM.ARTICULOS ART
                          INNER JOIN SYSADM.PRECIOS_D PD ON ART.COD_ITEM=PD.COD_ITEM AND pd.um_item =ART.UM_VENTA
                          INNER JOIN SYSADM.PRECIOS_C PC ON (PC.COD_CIA=PD.COD_CIA AND PD.COMPANIA_VENTA= PC.COMPANIA_VENTA AND PC.COD_LISTA = PD.COD_LISTA)
                          INNER JOIN SYSADM.DCTO_COND_PAGO_ART D ON (D.COD_CIA=PC.COD_CIA AND D.COD_LISTA = PC.COD_LISTA AND D.COMPANIA_VENTA = PC.COMPANIA_VENTA AND D.COD_ITEM = PD.COD_ITEM AND D.UM_ITEM=PD.UM_ITEM)
                     AND D.COND_PAGO IN(select distinct condicion
                                        from SYSADM.LP_CONDICION where compania=codempx and codig_sede=codsedex and ESTAD='A')--condiciones por sede
                          INNER JOIN SYSADM.SALDOS_ALMACEN sa on SA.COD_CIA=PD.COD_CIA AND  art.COD_ITEM=SA.COD_ITEM AND SA.compania_venta_3=PD.COMPANIA_VENTA
                 WHERE PD.COMPANIA_VENTA = codempx
                   AND art.cod_cia = '00'
                   AND art.status_item='A'
                   AND SA.ALMACEN IN (
                     SELECT DISTINCT CODALMACEN
                     FROM SYSADM.TB_USUARIO_MESA UM
                              INNER JOIN SYSADM.USUARIOS US ON US.USUARIO=UM.USUARIO
                              INNER JOIN SYSADM.TB_MESA TM ON TM.CODLOCALIDAD=UM.CODMESA
                              INNER JOIN SYSADM.TB_ALMACEN AL ON UM.CODEMPRESA=AL.CODEMPRESA  and TM.LOCALIDAD=AL.CODLOCALIDAD AND AL.ESTALMACEN = 'H'
                     WHERE US.USUARIO=usernamex AND US.ESTUSUARIO='H'
                 )
                   AND ARt.COD_LINEA<>'99'
                   AND PD.MONEDA_LISTA = 'S/.'
                   AND PD.COD_LISTA in (select DISTINCT CODLISTA from sysadm.TBL_USUARIO_JORNADA WHERE USUARIO=usernamex
                                                                                                   AND CODSEDE=codsedex AND TO_CHAR(FECHA,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD')
                 )
                ) TB ;




        open cursorsugeridox FOR
            SELECT B.COD_CLIENTE,B.COD_ITEM,B.CANT_SUGERIDA,'S',B.NRO_VECES
            FROM(
                    SELECT ROWNUM AS FILA,A.*
                    FROM (
                             SELECT PS.COD_CLIENTE,PS.COD_PROVEEDOR,PS.COD_ITEM,PS.CANT_SUGERIDA,PS.NRO_VECES
                             FROM
                                 (
                                     SELECT DISTINCT  CASE WHEN substr(TDM.C_PROVEEDOR,0,instr (TDM.C_PROVEEDOR, '-')-1) IS NULL THEN TDM.C_PROVEEDOR ELSE substr(TDM.C_PROVEEDOR,0,instr (TDM.C_PROVEEDOR, '-')-1) END AS codprov
                                     FROM SYSADM.TB_USUARIO_MESA TUM
                                              INNER JOIN SYSADM.USUARIOS US ON TUM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
                                              INNER JOIN SYSADM.TB_MESA TM ON TUM.CODEMPRESA=TM.CODEMPRESA AND TUM.CODMESA=TM.CODLOCALIDAD
                                              INNER JOIN SYSADM.TB_DETALLE_MESA TDM ON TM.IDMESA=  TDM.IDMESA
                                     WHERE US.USUARIO=usernamex AND TUM.TB_TIPO_CODIGO='R'
                                 )USUPROV
                                     INNER JOIN SYSADM.PEDIDO_SUGERIDO PS ON USUPROV.CODPROV=PS.COD_PROVEEDOR AND TO_CHAR(PS.FECHA,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD')
--INNER JOIN SYSADM.ARTICULOS ART ON PS.COD_ITEM=ART.COD_ITEM
                                     INNER JOIN (
                                     SELECT DISTINCT C.COD_CLIENTE
                                     FROM SYSADM.CLIENTE C
                                              INNER JOIN SYSADM.LOCALES_CLIENTE LC ON C.COD_CLIENTE=LC.COD_CLIENTE
                                              INNER JOIN SYSADM.TB_RUTA R ON LC.RUTA_DESPACHO=R.CODRUTA
                                              INNER JOIN SYSADM.TB_RUTA_VENDEDOR RV ON RV.IDRUTA=R.IDRUTA
                                              INNER JOIN SYSADM.TB_USUARIO_MESA UM ON RV.CODVENDEDOR=UM.CODVENDEDOR
                                     WHERE 1=1
                                       AND RV.DIA=(select translate(SUBSTR(to_char(SYSDATE, 'DAY', 'NLS_DATE_LANGUAGE=SPANISH'),0,2), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')
                                                   from dual)
                                       AND UM.USUARIO=usernamex
                                       AND C.STATUS_CLIENTE IN ('A','P')
                                       AND LC.ESTAD='A'
                                 )CLIENTESBYDIA ON PS.COD_CLIENTE=CLIENTESBYDIA.COD_CLIENTE
                             WHERE 1=1
--ORDER BY 5 DESC
                         )A
                )B ;



        open cursorfocusx FOR
            SELECT AF.CODITEM,ART.DESC_ITEM,CASE WHEN UPDF.CANT_SUGERIDA=0 THEN 1 ELSE UPDF.CANT_SUGERIDA END ,'F'
                 ,AF.UNIDADMEDIDA,AF.CODEMPRESA,AF.CODSEDE,AF.CODALMACEN
            FROM SYSADM.ARTICULOS ART
                     INNER JOIN SYSADM.FOCUS_ARTICULO AF  ON ART.COD_ITEM=AF.CODITEM
                     INNER JOIN SYSADM.FOCUS_PARTICIPACION PF ON AF.IDFOCUSDET=PF.IDFOCUSDET
                     INNER JOIN SYSADM.FOCUS_USUARIO_PARTI UPF ON UPF.IDPARTICIPACION=PF.IDPARTICIPACION
                     INNER JOIN SYSADM.FOCUS_USU_PARTI_DET UPDF ON UPDF.IDUSUPARTIFOCUS=UPF.IDUSUPARTIFOCUS
            WHERE UPF.USUARIO=usernamex
              AND AF.ESTADO='P'
              AND TO_CHAR(UPDF.FECHA,'DD/MM/YYYY')<=TO_CHAR(SYSDATE,'DD/MM/YYYY');




    end ARTICULOS_OFFLINE;





    FUNCTION  GET_ARTICULOS_FOCUS_SUGERIDOS(codempresax in varchar2,codalmacenx in varchar2,codlistax in varchar2,condpagox in varchar2,  codigosx VALUE_ARRAY_STRING) return types.ref_cursor is
        articulosx types.ref_cursor;
    BEGIN


        open articulosx FOR

            SELECT RES.*,TB3.PRECIO_FINAL,RES.compania_venta_3,RES.COD_LISTA
            FROM
                (SELECT  f.cod_item, f.desc_item,f.UM_VENTA ,f.UM_CONTROL_STOCK,f.PRECIO_BASE,f.saldo,f.cant,f.cod_lista,f.compania_venta_3
                 from
                     (
                         SELECT ROWNUM  cant,ART.cod_item, ART.desc_item,ART.UM_VENTA ,ART.UM_CONTROL_STOCK,PD.PRECIO_BASE,(NVL(SA.qty_fisica,0)- NVL(SA.qty_comprometida,0)- NVL(SA.qty_reservada,0)- NVL(SA.qty_proyectos,0)) saldo, pd.cod_lista,SA.compania_venta_3
                         FROM SYSADM.articulos art
                                  INNER JOIN SYSADM.SALDOS_ALMACEN sa on  art.COD_ITEM=SA.COD_ITEM
                                  INNER JOIN SYSADM.PRECIOS_D PD ON PD.cod_item=art.COD_ITEM AND pd.um_item =ART.UM_VENTA  and sa.compania_venta_3=pd.compania_venta
                         WHERE art.cod_cia = '00'
                           AND art.COD_LINEA <> '99'
                           AND art.status_item='A'
                           AND pd.moneda_lista = 'S/.'
                           AND pd.cod_lista=codlistax
                           AND SA.compania_venta_3 =codempresax
                           AND sa.almacen =codalmacenx
                           and art.cod_item in (select * from table(codigosx))
                     ) f
                 where f.cant<12
                )RES INNER JOIN
                (
                    SELECT tb2.COMPANIA_VENTA,tb2.COD_LISTA,tb2.UM_ITEM, tb2.COD_ITEM, CASE WHEN PRECIO_FINAL IS NULL THEN PRECIO_BASE ELSE PRECIO_FINAL END AS PRECIO_FINAL FROM
                        (SELECT PRECIO_BASE,COD_LISTA,
                                CASE WHEN COMPANIA_VENTA IN ('05','08') THEN (PRECIO_BASE - (DSCTO_LISTA_PRECIO+ DSCTO_CP_ART +DSCTO_PROM_TIPEADOS)) ELSE
                                        (PRECIO_BASE - (DSCTO_LISTA_PRECIO+ DSCTO_CP_ART +DSCTO_PROM_TIPEADOS)) *1.18 END AS PRECIO_FINAL,tb.COMPANIA_VENTA,tb.UM_ITEM,tb.COD_ITEM
                         FROM ( SELECT PD.COMPANIA_VENTA,PD.COD_LISTA, PD.PRECIO_BASE, PD.UM_ITEM,D.COND_PAGO,
                                       (PD.PRECIO_BASE*NVL(PC.DSCTO_CARACTERISTI,0)/100) AS DSCTO_LISTA_PRECIO,
                                       (CASE WHEN PD.F_DSCTO_PROM_FIN < SYSDATE THEN 0 ELSE CASE WHEN PD.DSCTO_PROMOCION = 0 THEN 0 ELSE
                                           (PD.PRECIO_BASE*PD.DSCTO_PROMOCION/100) END END) AS DSCTO_PROM_TIPEADOS,
                                       CASE WHEN D.DSCTO_CP_ARTICULO = 0 THEN 0 ELSE (PD.PRECIO_BASE*D.DSCTO_CP_ARTICULO/100) END AS DSCTO_CP_ART,PD.COD_ITEM
                                FROM SYSADM.PRECIOS_D PD
                                         INNER JOIN SYSADM.PRECIOS_C PC ON (PC.COD_CIA=PD.COD_CIA AND PD.COMPANIA_VENTA= PC.COMPANIA_VENTA AND PC.COD_LISTA = PD.COD_LISTA)
                                         LEFT JOIN SYSADM.DCTO_COND_PAGO_ART D ON (D.COD_CIA=PC.COD_CIA AND D.COD_LISTA = PC.COD_LISTA AND D.COMPANIA_VENTA = PC.COMPANIA_VENTA AND D.COD_ITEM = PD.COD_ITEM AND D.UM_ITEM=PD.UM_ITEM) AND D.COND_PAGO = condpagox
                                WHERE PD.COMPANIA_VENTA = codempresax
                                  AND PD.MONEDA_LISTA = 'S/.'
                                  AND PD.COD_LISTA = codlistax

                              ) TB ) TB2

                ) TB3 ON RES.compania_venta_3=TB3.COMPANIA_VENTA AND RES.COD_LISTA=TB3.COD_LISTA AND RES.COD_ITEM=TB3.COD_ITEM AND RES.UM_VENTA=TB3.UM_ITEM;


        return articulosx;


    end  GET_ARTICULOS_FOCUS_SUGERIDOS;


    FUNCTION BANDEJAINGRESOS(usuariox IN VARCHAR2) return types.ref_cursor is
        ingresosx types.ref_cursor;
        codempx varchar2(10);
        --	codsedex varchar2(10);

    BEGIN


        SELECT distinct UM.CODEMPRESA INTO codempx--,codsedex,codmesax
        FROM SYSADM.TB_USUARIO_MESA UM
                 INNER JOIN SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
        WHERE UM.USUARIO=usuariox AND UM.TB_TIPO_CODIGO='R';

        open ingresosx FOR
            SELECT ART.COD_ITEM,ART.DESC_ITEM,MOV.UM_ITEM_3,MOV.CANTIDAD,to_char(HORA_TRANSACCION,'hh24:mm:ss')
            FROM SYSADM.MOV_INVENTARIOS MOV
                     INNER JOIN SYSADM.ARTICULOS ART ON MOV.COD_ITEM_2=ART.COD_ITEM
            WHERE 1=1
              AND MOV.COMPANIA_VENTA_3=codempx
              AND MOV.TIPO_MOVIMIENTO='IC'
              AND TO_CHAR(FECHA_TRANSACCION,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD')
              AND MOV.PROVEEDOR IN (SELECT DISTINCT  CASE WHEN substr(TDM.C_PROVEEDOR,0,instr (TDM.C_PROVEEDOR, '-')-1) IS NULL THEN TDM.C_PROVEEDOR ELSE substr(TDM.C_PROVEEDOR,0,instr (TDM.C_PROVEEDOR, '-')-1) END AS codprov
                                    FROM SYSADM.TB_USUARIO_MESA TUM
                                             INNER JOIN SYSADM.USUARIOS US ON TUM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
                                             INNER JOIN SYSADM.TB_MESA TM ON TUM.CODEMPRESA=TM.CODEMPRESA AND TUM.CODMESA=TM.CODLOCALIDAD
                                             INNER JOIN SYSADM.TB_DETALLE_MESA TDM ON TM.IDMESA=  TDM.IDMESA
                                    WHERE US.USUARIO=usuariox AND TUM.TB_TIPO_CODIGO='R')
            ORDER BY HORA_TRANSACCION DESC;

        RETURN ingresosx;




    END ;



    FUNCTION BANDEJABONIFITEM(usuariox IN VARCHAR2) return types.ref_cursor is
        bonifx types.ref_cursor;
        codempx varchar2(10);
        codsedex varchar2(10);

    BEGIN


        SELECT distinct UM.CODEMPRESA,um.codsede INTO codempx,codsedex--,codmesax
        FROM SYSADM.TB_USUARIO_MESA UM
                 INNER JOIN SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
        WHERE UM.USUARIO=usuariox AND UM.TB_TIPO_CODIGO='R';

        open bonifx FOR
            SELECT
                DET1.COD_LISTA,
                DET1.COD_ITEM,
                ART.DESC_ITEM,
                DET1.UM_ITEM,
                DET1.CUOTA_MIN ,
                DET1.CUOTA_MAX,
                DET1.PRIORIDAD,
                DET1.COD_ITEM_BONIF,
                ART1.DESC_ITEM,
                DET1.UM_BONIF,DET1.CANTIDAD_BONIF,MULTIPLO
            from
                SYSADM.BONIFICACION CAB
                    INNER JOIN  SYSADM.BONIFICACION_D1 DET1 ON CAB.COD_CIA=DET1.COD_CIA AND CAB.compania_venta = det1.compania_venta
                    INNER JOIN SYSADM.ARTICULOS ART ON DET1.COD_ITEM=ART.COD_ITEM
                    INNER JOIN SYSADM.ARTICULOS ART1 ON DET1.COD_ITEM_BONIF=ART1.COD_ITEM
                    INNER JOIN SYSADM.BONIF_ITEM_NOTIF BIN ON BIN.COMPANIA_VENTA=CAB.compania_venta AND BIN.COD_LISTA=DET1.COD_LISTA AND BIN.COD_PAQUETE=CAB.COD_BONIF

                    AND cab.cod_lista = det1.cod_lista
                    AND cab.moneda_lista = det1.moneda_lista
                    AND cab.cod_bonif = det1.cod_bonif
            WHERE 1=1
              AND  cab.COMPANIA_VENTA=codempx
              AND CAB.COD_LISTA  IN (
                SELECT DISTINCT LP.COD_LISTA
                FROM SYSADM.PRECIOS_C LP
                         INNER JOIN SYSADM.LP_CONDICION LP_CON ON LP_CON.LISTAPRECIO = LP.COD_LISTA
                         INNER JOIN SYSADM.TABLAS EM ON EM.LLAVE = LP.COMPANIA_VENTA
                         INNER JOIN SYSADM.TB_SEDE SE ON SE.CODSEDE = LP_CON.CODIG_SEDE
                WHERE LP.ESTAD_SGC = 'A' AND EM.CATEGORIA = '001'
                  AND LP.COMPANIA_VENTA =codempx
                  AND LP_CON.CODIG_SEDE =codsedex

            )
              AND cab.flag_articulo = '1'
              AND to_char(CAB.FECHA_FIN,'YYYY-MM-DD')>=TO_CHAR(SYSDATE,'YYYY-MM-DD');



        RETURN bonifx;




    END ;



    PROCEDURE  BANDEJABONIFXPAQUETE(usuariox in varchar2,cursorparticipacionx IN OUT types.ref_cursor,cursorbonifx IN OUT types.ref_cursor)

        is

        codempx varchar2(10);
        codsedex varchar2(10);
    BEGIN


        SELECT distinct UM.CODEMPRESA,um.codsede INTO codempx,codsedex--,codmesax
        FROM SYSADM.TB_USUARIO_MESA UM
                 INNER JOIN SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
        WHERE UM.USUARIO=usuariox AND UM.TB_TIPO_CODIGO='R';


        OPEN cursorparticipacionx FOR
/*SELECT B.COD_LISTA,B.COD_PAQUETE,B.MODO_PARTICIPACION,B.DESCPAQUETE,B.UM_PAQUETE,B.COD_ITEM,B.DESC_ITEM,ROWNUM AS POS
FROM(
SELECT distinct --CAB.COMPANIA_VENTA,CAB.FECHA_CREACION,CAB.COD_LISTA,CAB.COD_BONIF,CAB.TIPO_BONIF ,
CAB.COD_LISTA,
BP.COD_PAQUETE,
BP.MODO_PARTICIPACION,-- 0 por monto  1 por CANTIDAD
BP.DESCRIPCION DESCPAQUETE,BP.UM_PAQUETE,
 PART.COD_ITEM,ART.DESC_ITEM ,BPN.FECHA_CREACION
from
SYSADM.BONIFICACION CAB
 INNER JOIN SYSADM.BONIF_PAQ BP ON CAB.COD_CIA=BP.COD_CIA AND CAB.COMPANIA_VENTA=BP.COMPANIA_VENTA AND CAB.COD_BONIF=BP.COD_BONIF AND CAB.COD_LISTA=BP.COD_LISTA
 INNER JOIN SYSADM.BONIF_PAQ_P1 PART ON BP.COD_CIA=PART.COD_CIA AND BP.COMPANIA_VENTA=PART.COMPANIA_VENTA AND BP.COD_LISTA=PART.COD_LISTA
 AND BP.COD_PAQUETE=PART.COD_PAQUETE AND BP.COD_BONIF=PART.COD_BONIF
 INNER JOIN SYSADM.ARTICULOS ART ON PART.COD_ITEM=ART.COD_ITEM
 INNER JOIN SYSADM.BONIF_PAQUETE_NOTIF BPN ON BPN.COMPANIA_VENTA=CAB.COMPANIA_VENTA AND BPN.COD_LISTA=CAB.COD_LISTA AND BPN.COD_PAQUETE=BP.COD_PAQUETE AND TO_CHAR(BPN.FECHA_CREACION,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD')
where CAB.compania_venta=codempx
  and CAB.cod_lista in  (
SELECT DISTINCT LP.COD_LISTA
		FROM SYSADM.PRECIOS_C LP
		INNER JOIN SYSADM.LP_CONDICION LP_CON ON LP_CON.LISTAPRECIO = LP.COD_LISTA
		INNER JOIN SYSADM.TABLAS EM ON EM.LLAVE = LP.COMPANIA_VENTA
		INNER JOIN SYSADM.TB_SEDE SE ON SE.CODSEDE = LP_CON.CODIG_SEDE
		WHERE LP.ESTAD_SGC = 'A' AND EM.CATEGORIA = '001'
AND LP.COMPANIA_VENTA =codempx
AND LP_CON.CODIG_SEDE =codsedex
)
  AND TO_CHAR(CAB.FECHA_FIN,'YYYY-MM-DD')>=TO_CHAR(SYSDATE,'YYYY-MM-DD') ORDER BY 8 DESC
)B;*/
            SELECT distinct --CAB.COMPANIA_VENTA,CAB.FECHA_CREACION,CAB.COD_LISTA,CAB.COD_BONIF,CAB.TIPO_BONIF ,
                            CAB.COD_LISTA,
                            BP.COD_PAQUETE,
                            BP.MODO_PARTICIPACION,-- 0 por monto  1 por CANTIDAD
                            BP.DESCRIPCION DESCPAQUETE,BP.UM_PAQUETE,
                            PART.COD_ITEM,ART.DESC_ITEM ,to_char(BPN.FECHA_CREACION,'HH24MMSS')
            from
                SYSADM.BONIFICACION CAB
                    INNER JOIN SYSADM.BONIF_PAQ BP ON CAB.COD_CIA=BP.COD_CIA AND CAB.COMPANIA_VENTA=BP.COMPANIA_VENTA AND CAB.COD_BONIF=BP.COD_BONIF AND CAB.COD_LISTA=BP.COD_LISTA
                    INNER JOIN SYSADM.BONIF_PAQ_P1 PART ON BP.COD_CIA=PART.COD_CIA AND BP.COMPANIA_VENTA=PART.COMPANIA_VENTA AND BP.COD_LISTA=PART.COD_LISTA
                    AND BP.COD_PAQUETE=PART.COD_PAQUETE AND BP.COD_BONIF=PART.COD_BONIF
                    INNER JOIN SYSADM.ARTICULOS ART ON PART.COD_ITEM=ART.COD_ITEM
                    INNER JOIN SYSADM.BONIF_PAQUETE_NOTIF BPN ON BPN.COMPANIA_VENTA=CAB.COMPANIA_VENTA AND BPN.COD_LISTA=CAB.COD_LISTA AND BPN.COD_PAQUETE=BP.COD_PAQUETE AND TO_CHAR(BPN.FECHA_CREACION,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD')
                    INNER JOIN(SELECT DISTINCT CASE WHEN substr(TDM.C_PROVEEDOR,0,instr (TDM.C_PROVEEDOR, '-')-1) IS NULL THEN  TDM.C_PROVEEDOR ELSE substr(TDM.C_PROVEEDOR,0,instr (TDM.C_PROVEEDOR, '-')-1) END CODPROV
                               FROM SYSADM.TB_USUARIO_MESA UM
                                        INNER JOIN SYSADM.USUARIOS US ON US.USUARIO=UM.USUARIO
                                        INNER JOIN SYSADM.TB_MESA TM ON TM.CODLOCALIDAD=UM.CODMESA
                                        INNER JOIN SYSADM.TB_DETALLE_MESA TDM ON TM.IDMESA=  TDM.IDMESA
                               WHERE 1=1
                                 AND UM.CODEMPRESA=codempx AND UM.CODSEDE=codsedex
                                 AND US.ESTUSUARIO='H'
                                 AND UM.ESTADO_EN_MESA='A' AND UM.USUARIO=usuariox)PROV ON PROV.CODPROV=ART.PROVEEDOR_DEFAULT

            where CAB.compania_venta=codempx
              and CAB.cod_lista in  (
                SELECT DISTINCT LP.COD_LISTA
                FROM SYSADM.PRECIOS_C LP
                         INNER JOIN SYSADM.LP_CONDICION LP_CON ON LP_CON.LISTAPRECIO = LP.COD_LISTA
                         INNER JOIN SYSADM.TABLAS EM ON EM.LLAVE = LP.COMPANIA_VENTA
                         INNER JOIN SYSADM.TB_SEDE SE ON SE.CODSEDE = LP_CON.CODIG_SEDE
                WHERE LP.ESTAD_SGC = 'A' AND EM.CATEGORIA = '001'
                  AND LP.COMPANIA_VENTA =codempx
                  AND LP_CON.CODIG_SEDE =codsedex
            )
              AND TO_CHAR(CAB.FECHA_FIN,'YYYY-MM-DD')>=TO_CHAR(SYSDATE,'YYYY-MM-DD') ORDER BY 8 DESC;




        open cursorbonifx for
            SELECT distinct
                CAB.COD_LISTA,
                BP.COD_PAQUETE,
                BP.MODO_PARTICIPACION,-- 0 por monto  1 por CANTIDAD
                BP.UM_PAQUETE,
                BON.COD_ITEM_BONIF,ART.DESC_ITEM ,
                BON.UM_BONIF,BON.PRIORIDAD AS ESTADO ,BON.CANTIDAD_BONIF ,BON.MULTIPLO,BON.CUOTA_MIN,BON.CUOTA_MAX,TO_CHAR(BPN.FECHA_CREACION,'HH24MMSS')
            from
                SYSADM.BONIFICACION CAB
                    INNER JOIN SYSADM.BONIF_PAQ BP ON CAB.COD_CIA=BP.COD_CIA AND CAB.COMPANIA_VENTA=BP.COMPANIA_VENTA AND CAB.COD_BONIF=BP.COD_BONIF AND CAB.COD_LISTA=BP.COD_LISTA
                    INNER JOIN SYSADM.BONIF_PAQ_P2 BON ON BP.COD_CIA=BON.COD_CIA AND BP.COMPANIA_VENTA=BON.COMPANIA_VENTA AND BP.COD_LISTA=BON.COD_LISTA
                    AND BP.COD_PAQUETE=BON.COD_PAQUETE AND BP.COD_BONIF=BON.COD_BONIF
                    INNER JOIN SYSADM.ARTICULOS ART ON BON.COD_ITEM_BONIF=ART.COD_ITEM
                    INNER JOIN SYSADM.BONIF_PAQUETE_NOTIF BPN ON BPN.COMPANIA_VENTA=CAB.COMPANIA_VENTA AND BPN.COD_LISTA=CAB.COD_LISTA AND BPN.COD_PAQUETE=BP.COD_PAQUETE AND TO_CHAR(BPN.FECHA_CREACION,'YYYY-MM-DD')=TO_CHAR(SYSDATE,'YYYY-MM-DD')
            where CAB.compania_venta=codempx
              and CAB.cod_lista in  (
                SELECT DISTINCT LP.COD_LISTA
                FROM SYSADM.PRECIOS_C LP
                         INNER JOIN SYSADM.LP_CONDICION LP_CON ON LP_CON.LISTAPRECIO = LP.COD_LISTA
                         INNER JOIN SYSADM.TABLAS EM ON EM.LLAVE = LP.COMPANIA_VENTA
                         INNER JOIN SYSADM.TB_SEDE SE ON SE.CODSEDE = LP_CON.CODIG_SEDE
                WHERE LP.ESTAD_SGC = 'A' AND EM.CATEGORIA = '001'
                  AND LP.COMPANIA_VENTA =codempx
                  AND LP_CON.CODIG_SEDE =codsedex
            )
              AND TO_CHAR(CAB.FECHA_FIN,'YYYY-MM-DD')>=TO_CHAR(SYSDATE,'YYYY-MM-DD') ;

--ORDER BY 13 DESC ;

    end BANDEJABONIFXPAQUETE;




    FUNCTION BANDEJAREPARTOVENTA(usuariox IN VARCHAR2) return types.ref_cursor is
        repartoventax types.ref_cursor;
        codempx varchar2(10);
        codsedex varchar2(10);

    BEGIN


        SELECT distinct UM.CODEMPRESA,um.codsede INTO codempx,codsedex--,codmesax
        FROM SYSADM.TB_USUARIO_MESA UM
                 INNER JOIN SYSADM.USUARIOS US ON UM.USUARIO=US.USUARIO AND US.ESTUSUARIO='H'
        WHERE UM.USUARIO=usuariox AND UM.TB_TIPO_CODIGO='R';

        open repartoventax FOR
            SELECT   DISTINCT SPH.NRO_PEDIDO,
                              CASE WHEN CXC.CDCC_TIPODOC='01' THEN
                                           'F'||CXC.NDCC_SERIE||'-'||CXC.NDCC_PREIMPRESO
                                   ELSE
                                           'B'||CXC.NDCC_SERIE||'-'||CXC.NDCC_PREIMPRESO
                                  END NRODOC,
                              SPH.DESC_CLIENTE,
                              sph.monto_desp_bonif,
                              LR.TIPO_CONDICION,
                              CASE WHEN LR.MOTIVO IS   NULL  THEN
                                       'ENTREGADO'
                                   ELSE
                                       'REBOTE'
                                  END  ESTADO,
                              FECHA_TRANS_COBRO,(SELECT DESC1 FROM SYSADM.TABLAS TB
                                                 WHERE CATEGORIA='412' AND LLAVE=LR.MOTIVO) MOTIVO
            FROM SYSADM.SPEDIDO_HEADER SPH
                     INNER JOIN SYSADM.LOG_REPARTO LR ON SPH.COMPANIA_VENTA=LR.CODEMPRESA AND SPH.NRO_PEDIDO=LR.NROPEDIDO
                     INNER JOIN SYSADM.DOCUMENTO_CXC CXC ON CXC.CDCC_COMPANIA=SPH.COMPANIA_VENTA AND CXC.CDCC_TIPODOC=LR.TIPO_DOC AND SPH.NRO_PEDIDO=CXC.NDCC_PEDIDOOIH
                AND CXC.NDCC_SERIE=LR.SERIE AND CXC.NDCC_PREIMPRESO=LR.PREIMPRESO
                     INNER JOIN SYSADM.TB_USUARIO_MESA UM  ON SPH.COMPANIA_VENTA=UM.CODEMPRESA AND SPH.COD_VENDEDOR=UM.CODVENDEDOR AND  UM.CODMESA<>'0185' AND UM.TB_TIPO_CODIGO='R'
                     INNER JOIN SYSADM.USUARIOS U ON UM.USUARIO=U.USUARIO   AND U.ESTUSUARIO='H'
            where 1=1
              AND SPH.COMPANIA_VENTA=codempx
              AND UM.USUARIO=usuariox
              AND TO_CHAR(FECHA_TRANS_COBRO,'DD-MM-YYYY')=TO_CHAR(SYSDATE,'DD-MM-YYYY')
            ORDER BY FECHA_TRANS_COBRO DESC;

        RETURN repartoventax;


    END BANDEJAREPARTOVENTA;




    PROCEDURE  VENDEDORES_BY_SUPERVISOR(usuariox IN VARCHAR2,filtrox IN VARCHAR2,cursorvendedoresx IN OUT types.ref_cursor)
        is

    BEGIN



        open cursorvendedoresx for
            SELECT US.USUARIO,US.NOMBRE, UM.CODEMPRESA, UM.CODSEDE,UM.CODMESA,UM.CODVENDEDOR,US.CODPERFIL,TC.CODCANAL,TC.DESCCANAL
            FROM SYSADM.TB_USUARIO_MESA UM INNER JOIN SYSADM.USUARIOS US ON  US.USUARIO=UM.USUARIO
                                           INNER JOIN SYSADM.TB_CANAL TC ON UM.CODCANAL=TC.CODCANAL
            WHERE US.ESTUSUARIO='H'
              AND UM.CODMESA IN (SELECT DISTINCT TUM.CODMESA--, UPPER (US.CLAVE) AS CLAVE,TO_CHAR(sysdate + 59/1440,''HH24:MI'') AS HORASERVIDOR,TC.CODCANAL,TC.DESCCANAL,TUM.CODEMPRESA
                                 FROM SYSADM.USUARIOS US INNER JOIN SYSADM.TB_USUARIO_MESA TUM ON US.USUARIO=TUM.USUARIO
                                                         INNER JOIN SYSADM.TB_CANAL TC ON TUM.CODCANAL=TC.CODCANAL
                                 WHERE  US.USUARIO =usuariox AND TUM.TB_TIPO_CODIGO='R'
            ) AND UM.TB_TIPO_CODIGO='R' AND UM.ESTADO_EN_MESA='A'
              and um.codvendedor||us.nombre LIKE '%'||translate(filtrox, 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU')||'%' ;




    end VENDEDORES_BY_SUPERVISOR;


--carga de precios

    PROCEDURE LOADFORMPRECIOS(operacionx in number,paramstringx in LISTPARAMETR0STRING,cursorrptax IN OUT types.ref_cursor)
        is
        codempresax VARCHAR2(10);
        codlocalidadx VARCHAR2(10);
        codzonax VARCHAR2(10);
        codlistax VARCHAR2(10);
        preciobasex VARCHAR2(200);



        porcentdctolpx NUMBER;

    BEGIN


        FOR indx IN  paramstringx.FIRST .. paramstringx.LAST
            LOOP

                IF paramstringx(indx).clave='codEmpresa' THEN
                    codempresax:=paramstringx(indx).valor;
                ELSIF paramstringx(indx).clave='codLocalidad' THEN
                    codlocalidadx:=paramstringx(indx).valor;
                ELSIF paramstringx(indx).clave='codZona' THEN
                    codzonax:=paramstringx(indx).valor;
                ELSIF paramstringx(indx).clave='codLista' THEN
                    codlistax:=paramstringx(indx).valor;
                ELSIF paramstringx(indx).clave='precioBase' THEN
                    preciobasex:=paramstringx(indx).valor;
                END IF;

            END LOOP;

        IF operacionx=1 THEN

            open cursorrptax for
                SELECT substr(LLAVE,3,4),upper(DESC1)
                FROM SYSADM.TABLAS
                WHERE CATEGORIA='424'
                  AND substr(LLAVE,1,2)=codempresax
                  AND ESTADO='A';

        ELSIF operacionx=2 THEN
            open cursorrptax for
                SELECT CODIGO,DESCRIPCION FROM SYSADM.ZONA_LOCALIDAD;

        ELSIF operacionx=3 THEN
            open cursorrptax for
                SELECT DISTINCT LPC.LISTAPRECIO,PC.DESCRIPCION
                FROM SYSADM.PRECIOS_C PC
                         INNER JOIN SYSADM.LP_CONDICION LPC ON PC.COMPANIA_VENTA=LPC.COMPANIA AND PC.COD_LOCALIDAD=LPC.LOCALIDAD
                    AND LPC.LISTAPRECIO=PC.COD_LISTA
                WHERE PC.COMPANIA_VENTA=codempresax AND LPC.LOCALIDAD=codlocalidadx AND LPC.ESTAD='A';

        ELSIF operacionx=4 THEN
            open cursorrptax for
                SELECT DISTINCT CODIGO, DESCRIPCION
                FROM SYSADM.ZONA_LOCALIDAD ZL
                WHERE ZL.CODIGO=codzonax ;


        ELSIF operacionx=5 THEN

            select nvl(DSCTO_CARACTERISTI,0)  into porcentdctolpx
            from SYSADM.PRECIOS_C
            where COMPANIA_VENTA=codempresax and COD_LISTA=codlistax;

            open cursorrptax for
                SELECT porcentdctolpx
                FROM SYSADM. DUAL;

        ELSIF  operacionx=6 THEN

            open cursorrptax for
                SELECT  CONDICION
                FROM
                    SYSADM.LP_CONDICION
                WHERE COMPANIA=codempresax
                  AND LOCALIDAD=codlocalidadx
                  AND LISTAPRECIO=codlistax
                  AND ESTAD='A';


        END IF;



    end;

    PROCEDURE "APTOTOPESTOCK" (TABLE_OBJ IN PEDIDO_PREVENTA_TABLE,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2)
        IS
        countaux number;
        coditemx VARCHAR2(8);
        extopex EXCEPTION;
        extope2x EXCEPTION;
        disponiblex NUMBER;
    BEGIN




        FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST
            LOOP

                SELECT COUNT(*) INTO countaux
                FROM
                    (
                        SELECT RES.COD_ITEM, CASE WHEN RES.SALDO< NVL(TC.CANTIDAD,0) THEN 1 ELSE 0 END  RESTRINGIDO
                        FROM
                            (
                                SELECT SA .COD_ITEM, (
                                        NVL (SA.QTY_FISICA, 0) - NVL (SA.QTY_COMPROMETIDA, 0) - NVL (SA.QTY_RESERVADA, 0) - NVL (SA.QTY_PROYECTOS, 0)
                                    ) SALDO
                                FROM SYSADM.SALDOS_ALMACEN SA

                                WHERE 1=1
                                  AND SA.COMPANIA_VENTA_3 =TABLE_OBJ(indx).codempresa
                                  AND SA.ALMACEN =TABLE_OBJ(indx).codalmacen
                                  AND SA.COD_ITEM=TABLE_OBJ(indx).codproducto
                            )RES LEFT JOIN
                            SYSADM.TOPE_CANAL TC ON RES.COD_ITEM=TC.COD_ITEM AND COD_CANAL=TABLE_OBJ(indx).codcanal AND CODSEDE=TABLE_OBJ(indx).codsede
                        WHERE RES.SALDO>0
                    )B
                WHERE 1=1 ;--and B.RESTRINGIDO=0;

                IF countaux>0 THEN

                    SELECT  B.DISPONIBLE  INTO disponiblex
                    FROM
                        (
                            SELECT   (RES.saldo-NVL(TC.CANTIDAD,0)) disponible  , CASE WHEN RES.SALDO< NVL(TC.CANTIDAD,0) THEN 1 ELSE 0 END  RESTRINGIDO
                            FROM
                                (
                                    SELECT SA .COD_ITEM,
                                           case when fc.operacion='*' THEN
                                                    trunc((NVL (SA.qty_fisica, 0) - NVL (SA.qty_comprometida, 0) - NVL (SA.qty_reservada, 0) - NVL (SA.qty_proyectos, 0) )/fc.factor)
                                                ELSE
                                                    trunc((NVL (SA.qty_fisica, 0) - NVL (SA.qty_comprometida, 0) - NVL (SA.qty_reservada, 0) - NVL (SA.qty_proyectos, 0) )*fc.factor)

                                               end
                                               SALDO
                                    FROM SYSADM.SALDOS_ALMACEN SA

                                             INNER JOIN SYSADM.ARTICULOS ART ON ART.COD_ITEM=SA.COD_ITEM
                                             INNER JOIN SYSADM.FACTORES_CONV FC ON FC.UM_ORIGEN=art.um_venta AND FC.UM_DESTINO=art.UM_CONTROL_STOCK


                                    WHERE 1=1
                                      AND SA.COMPANIA_VENTA_3 =TABLE_OBJ(indx).codempresa
                                      AND SA.ALMACEN =TABLE_OBJ(indx).codalmacen
                                      AND SA.COD_ITEM=TABLE_OBJ(indx).codproducto
                                )RES LEFT JOIN
                                SYSADM.TOPE_CANAL TC ON RES.COD_ITEM=TC.COD_ITEM AND TC.COD_CANAL=TABLE_OBJ(indx).codcanal AND TC.CODSEDE=TABLE_OBJ(indx).codsede
                            WHERE RES.SALDO>0
                        )B
                    WHERE 1=1;-- AND B.RESTRINGIDO=0;

                    IF TABLE_OBJ(indx).cantidad>disponiblex THEN

                        countaux:=-1;
                    END IF;
                END IF;


                coditemx:=TABLE_OBJ(indx).codproducto;

                EXIT WHEN (countaux=0 OR countaux=-1) ;
            END LOOP;

        if countaux=0 THEN
            RAISE extopex;

        end if;


        if countaux=-1 THEN
            RAISE extope2x;

        end if;


        rptax:=1;
        mensajex:='OK';

    EXCEPTION
        WHEN extope2x THEN
            rptax:=-1;
            mensajex:='LA CANTIDAD DEL ARTÍCULO '||coditemx||' EXCEDE EL STOCK DISPONIBLE PARA SU CANAL';

        WHEN extopex THEN
            rptax:=-1;
            mensajex:='EL ARTÍCULO '||coditemx||' NO TIENE EL STOCK DISPONIBLE';


        WHEN OTHERS THEN
            rptax := -1;
            mensajex :='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

    end APTOTOPESTOCK;


    PROCEDURE  VALIDAR_EDICION(usuariox IN VARCHAR2,estadox IN OUT NUMBER,mensajex IN OUT VARCHAR2)

        IS

    BEGIN

        estadox:=1;
        mensajex:='OK';



    END;

    FUNCTION TIPOS_BY_GIRO
    (
        codgirox IN VARCHAR2
    )
        RETURN TYPES.REF_CURSOR IS rptax TYPES.REF_CURSOR;
    codsedex VARCHAR2(10);
    BEGIN
        /*SELECT DISTINCT TB_COD_SEDE INTO codsedex
 FROM SYSADM.TB_ASIGNACION
WHERE TB_USUARIO=usuariox;*/

        OPEN rptax
            FOR
            select  LLAVE,upper(DESC1),ESTADO
            from sysadm.TABLAS where categoria = '006' AND ESTADO = 'H' and NUM4=codgirox
            ORDER BY 2 asc;


        RETURN rptax;
    END TIPOS_BY_GIRO;


    PROCEDURE INSERT_DCTO_TEMP(pcorrelativo IN NUMBER, TABLE_OBJ IN PEDIDO_PREVENTA_TABLE)
        IS
        vdctospaq TYPES.REF_CURSOR;
--DATOS CABECERA DCTO X PAQ
        vumpaq VARCHAR2(6);
        vcodpaq VARCHAR2(4);
        vdescpaq VARCHAR2(200);
        vmodopartpaq NUMBER;
--DETALLES DCTO PAQUETE
        vcuotamin NUMBER;
        vcuotamax NUMBER;
        vporcdcto NUMBER;
--

        vbandsalir number:=0;
        cantauxpaqx NUMBER;


        existeaux number;
--PARA DCTO VOLUMEN
        vdctosvol TYPES.REF_CURSOR;
    BEGIN

        FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST
            LOOP

                SELECT COUNT(*) INTO cantauxpaqx
                FROM SYSADM.precios_c c,
                     SYSADM.paquete p1,
                     SYSADM.particip_paquete p2,
                     SYSADM.articulos a,
                     SYSADM.descuentos_paquete p4
                WHERE c.cod_cia = p1.cod_cia
                  AND c.compania_venta = p1.compania_venta
                  AND c.cod_lista = p1.cod_lista
                  AND p1.cod_cia = p2.cod_cia
                  AND p1.compania_venta = p2.compania_venta
                  AND p1.cod_lista = p2.cod_lista
                  AND p1.moneda_lista = p2.moneda_lista
                  AND p1.cod_paquete = p2.cod_paquete
                  AND p2.cod_cia = a.cod_cia
                  AND p2.cod_item = a.cod_item
                  AND p1.cod_cia = p4.cod_cia
                  AND p1.compania_venta = p4.compania_venta
                  AND p1.cod_lista = p4.cod_lista
                  AND p1.moneda_lista = p4.moneda_lista
                  AND p1.cod_paquete = p4.cod_paquete
                  AND c.compania_venta = TABLE_OBJ(indx).codempresa
                  AND c.cod_localidad = TABLE_OBJ(indx).codlocalidad
                  and a.COD_ITEM=TABLE_OBJ(indx).codproducto
                  AND p1.moneda_lista = 'S/.'
                  AND c.cod_lista = TABLE_OBJ(indx).codlistaprecios
                  AND c.tipo_lista = '01'
                  AND SYSDATE>=p4.fecha_ini_vig
                  AND SYSDATE<=p4.fecha_fin_vig
                ORDER BY p4.cuota_inf asc;



                IF cantauxpaqx>0 THEN


                    SELECT DISTINCT
                        p1.cod_paquete, p1.descripcion,  p1.modo_participacion,  p1.um_paquete
                    INTO	vcodpaq,vdescpaq,vmodopartpaq,vumpaq

                    FROM SYSADM.precios_c c,
                         SYSADM.paquete p1,
                         SYSADM.particip_paquete p2,
                         SYSADM.articulos a,
                         SYSADM.descuentos_paquete p4
                    WHERE c.cod_cia = p1.cod_cia
                      AND c.compania_venta = p1.compania_venta
                      AND c.cod_lista = p1.cod_lista
                      AND p1.cod_cia = p2.cod_cia
                      AND p1.compania_venta = p2.compania_venta
                      AND p1.cod_lista = p2.cod_lista
                      AND p1.moneda_lista = p2.moneda_lista
                      AND p1.cod_paquete = p2.cod_paquete
                      AND p2.cod_cia = a.cod_cia
                      AND p2.cod_item = a.cod_item
                      AND p1.cod_cia = p4.cod_cia
                      AND p1.compania_venta = p4.compania_venta
                      AND p1.cod_lista = p4.cod_lista
                      AND p1.moneda_lista = p4.moneda_lista
                      AND p1.cod_paquete = p4.cod_paquete
                      AND c.compania_venta = TABLE_OBJ(indx).codempresa
                      AND c.cod_localidad = TABLE_OBJ(indx).codlocalidad
                      and a.COD_ITEM=TABLE_OBJ(indx).codproducto
                      AND p1.moneda_lista = 'S/.'
                      AND c.cod_lista = TABLE_OBJ(indx).codlistaprecios
                      AND c.tipo_lista = '01'
                      AND SYSDATE>=p4.fecha_ini_vig
                      AND SYSDATE<=p4.fecha_fin_vig ;



                    SELECT COUNT(*) INTO existeaux
                    FROM SYSADM.TEMP_DCTO
                    WHERE ID_PEDIDO=pcorrelativo
                      AND COD_ITEM=TABLE_OBJ(indx).codproducto;


                    if existeaux=0 THEN

                        INSERT INTO SYSADM.TEMP_DCTO(ID_PEDIDO,COD_ITEM,UM_ITEM,COD_PAQ,MODO_PART,CANTIDAD)
                        VALUES(pcorrelativo,TABLE_OBJ(indx).codproducto,TABLE_OBJ(indx).um,vcodpaq,vmodopartpaq,TABLE_OBJ(indx).cantidad);

                    ELSE

                        UPDATE SYSADM.TEMP_DCTO
                        SET MODO_PART=vmodopartpaq,COD_PAQ=vcodpaq
                        WHERE  ID_PEDIDO=pcorrelativo
                          AND COD_ITEM=TABLE_OBJ(indx).codproducto;


                    END IF;



                END IF;





                OPEN vdctosvol FOR
                    SELECT
                        d.cuota_inf, d.cuota_max,
                        d.dscto_volumen
                    FROM SYSADM.precios_c c, SYSADM.dcto_articulo_d d, SYSADM.articulos a
                    WHERE c.cod_cia = d.cod_cia
                      AND c.compania_venta = d.compania_venta
                      AND c.cod_lista = d.cod_lista
                      AND d.cod_cia = a.cod_cia
                      AND d.cod_item = a.cod_item
                      AND c.compania_venta =TABLE_OBJ(indx).codempresa
                      AND c.cod_localidad = TABLE_OBJ(indx).codlocalidad
                      AND d.moneda_lista = 'S/.'
                      AND d.cod_item=TABLE_OBJ(indx).codproducto
                      AND c.cod_lista =TABLE_OBJ(indx).codlistaprecios
                      AND c.tipo_lista = '01'
                      AND SYSDATE>=d.fecha_ini_vig
                      AND SYSDATE<=d.fecha_fin_vig
                    ORDER BY c.compania_venta,
                             c.cod_lista,
                             d.moneda_lista,
                             a.cod_item,
                             d.cuota_inf;

                vbandsalir:=0;
                LOOP
                    FETCH vdctosvol INTO	  vcuotamin ,vcuotamax,vporcdcto ;--DETALLES
                    EXIT WHEN vdctosvol%NOTFOUND or vbandsalir=1;

                    --DBMS_OUTPUT.PUT_LINE(vcuotamin);
                    if vcuotamin<=TABLE_OBJ(indx).cantidad and TABLE_OBJ(indx).cantidad < vcuotamax THEN
                        --vporcdctovolfinal:=vporcdcto;

                        SELECT COUNT(*) INTO existeaux
                        FROM SYSADM.TEMP_DCTO
                        WHERE ID_PEDIDO=pcorrelativo
                          AND COD_ITEM=TABLE_OBJ(indx).codproducto;


                        if existeaux=0 THEN

                            INSERT INTO SYSADM.TEMP_DCTO(ID_PEDIDO,COD_ITEM,UM_ITEM,CANTIDAD,PORC_DCTO_VOL)
                            VALUES(pcorrelativo,TABLE_OBJ(indx).codproducto,TABLE_OBJ(indx).um,TABLE_OBJ(indx).cantidad,vporcdcto);

                        ELSE

                            UPDATE SYSADM.TEMP_DCTO
                            SET PORC_DCTO_VOL=vporcdcto
                            WHERE  ID_PEDIDO=pcorrelativo
                              AND COD_ITEM=TABLE_OBJ(indx).codproducto;


                        END IF;

                        --
                        vbandsalir:=1;
                    END IF;
                end loop;
                close vdctosvol;






                --	dbms_output.put_line('SE REGISTRÓ CORRECTAMENTE');

            END LOOP;



    END;



    PROCEDURE registrar_pedido_xray(
        secpedxtrans in number,
        listobjpedtransfx  IN  PEDIDO_PREVENTA_TABLE,
        rptax              IN OUT  NUMBER,
        mensajex           IN OUT  VARCHAR2
    )
        is
        tipocambiox NUMBER;
        ubigeox VARCHAR2(8);
        direccionx VARCHAR2(900);
        descclientex  VARCHAR2(900);
        tdocx VARCHAR2(100);
        zonaventax VARCHAR2(8);
        rucx VARCHAR2(11);

        nropedidox VARCHAR2(12);
        correlativopedidox NUMBER;
        tipoclientex VARCHAR2(4);

        umorigenx VARCHAR2(8);
        umdestinox VARCHAR2(8);
        tieneigvx NUMBER;

        factorx NUMBER;
        operacionx CHAR(1);

        cantidadx NUMBER;

        pdctolpx NUMBER;
        mdctolpx NUMBER;

        pdctopromx NUMBER;
        mdctopromx NUMBER;


        pdctoexcepx NUMBER;
        mdctoexcepx NUMBER;

        countdctocpx number;
        pdctocpx NUMBER;
        mdctocpx NUMBER;

        pdctovolx number;
        mdctovolx number;

        pdctopaqx number;
        mdctopaqx number;

        canttempdctox number;

        pdctototalx NUMBER;
        mdctototalx NUMBER;

        isempresaexox number;
        montoigvx NUMBER;
        porcigvx NUMBER;

        importecabecerax NUMBER:=0;

        preciobasefletex number;
        secuenciax number;

        cursorbonifx types.ref_cursor;

        vcoditembonif VARCHAR2(10);
        vumitembonif VARCHAR2(10);
        vcantidadbonif NUMBER;

        preciobasebonifx number;
        countpreciobasex NUMBER;
        exceppreciobonifx exception;


    BEGIN


        SELECT
                correlativo + 1
        INTO nropedidox
        FROM
            sysadm.correlativo_com
        WHERE
                llave = 'P'
          AND compania = listobjpedtransfx(1).codempresa;

        correlativopedidox := nropedidox;
        nropedidox := 'P'||listobjpedtransfx(1).codvendedor
            || lpad(nropedidox, 6, '0');



        SELECT
            ciudad,
            direccion_cliente,
            desc_cliente,
            CASE
                WHEN ruc IS NOT NULL THEN
                        '01-' || ruc
                ELSE
                        '02-' || lib_electoral
                END AS doc,
            ZONA_VENTAS,TIPO_CLIENTE
        INTO
            ubigeox,
            direccionx,
            descclientex,
            tdocx,
            zonaventax,tipoclientex
        FROM
            sysadm.cliente c
        WHERE
                c.cod_cliente = listobjpedtransfx(1).codcliente;




        SELECT TB.tipo_cambio_banco   INTO tipocambiox
        FROM (
                 SELECT FECHA, tipo_cambio_banco
                 from SYSADM.TIPO_PARIDAD
                 where 1=1 AND moneda='S/.' and moneda_base='USD'
                   and fecha<=TO_DATE (SYSDATE, 'DD-MM-YY')
                 ORDER BY FECHA DESC )  TB
        WHERE ROWNUM <=1;


        IF substr(tdocx, 0, 2) = '01' THEN
            rucx := substr(tdocx, 4);
        END IF;

        INSERT INTO SYSADM.spedido_header(
            cod_cia,
            compania_venta,
            nro_pedido,
            nro_back_order,
            cod_cliente,
            moneda_pedido,
            status_pedido,
            cod_vendedor,
            tipo_orden,
            condicion_pago,
            fecha_pedido,
            usuario,
            cod_lista_precios,
            pos,
            facturacion_adel,
            dpto_origen,
            flag_modif,
            cod_almacen,
            flag_falta_valor,
            tipo_despacho,
            tipo_cliente,
            ruc,
            desc_cliente,
            dir_facturacion,
            zona_pedido,
            marca_cliente,
            distrito_despacho,
            fecha_prometida,
            fecha_creacion,
            orden_compra,
            glosa,
            mensaje_doc,
            urgente,
            flag_recogen,
            aprobado,
            f_sin_igv_tailoy,
            tipo_de_cambio,
            dias_diferidos,
            flag_letras_adel,
            doc_ref,
            nro_cotizacion,
            fecha_horareg,
            fecha_entregaoc,
            codmesa,
            codsede,
            codcanal,
            ruta_despacho,
            codlocal,
            nro_pedido_ref,
            cod_tipodoc,
            user_reg,
            impuestos,
            descuento_total,
            monto_desp_bonif,
            monto_neto,
            monto_pedido,
            rowversion,
            dscto_real,
            flete,
            igv_flete,
            porc_gasto_fin_ped,
            mont_gasto_fin_ped,
            igv_gasto_fin_ped,
            porc_otros_ped,
            monto_otros_ped,
            igv_otros_ped,
            porc_embalaje_ped,
            monto_embalaje_ped,
            igv_embalaje_ped,
            porc_seguro_ped,
            monto_seguro_ped,
            igv_seguro_ped,
            hold_code_credito,
            hold_code_general,
            hold_code_inv,
            hold_code_monto,
            hold_code_t_orden,
            hold_error_inv,
            monto_bonif_igv,
            monto_bonif,
            monto_bonif_isc
        ) VALUES (
                     '00',
                     listobjpedtransfx(1).codempresa,
                     nropedidox,
                     0,
                     listobjpedtransfx(1).codcliente,
                     'S/.',
                     'I',
                     listobjpedtransfx(1).codvendedor,
                     '01',
                     listobjpedtransfx(1).codcondicion,
                     to_date(to_char(sysdate, 'DD-MON-YY'), 'DD-MON-YY'),
                     'MOVIL',
                     listobjpedtransfx(1).codlistaprecios,
                     0,
                     0,
                     listobjpedtransfx(1).codlocalidad,
                     '1',
                     listobjpedtransfx(1).codalmacen,
                     '0',
                     '0',
                     tipoclientex,
                     rucx,
                     descclientex,
                     direccionx,
                     zonaventax,
                     NULL,
                     ubigeox,
                     to_date(to_char(sysdate + 1, 'DD-MON-YY'), 'DD-MON-YY'),
                     to_date(to_char(sysdate, 'DD-MON-YY'), 'DD-MON-YY'),
                     NULL,--coloque orden de compra null
                     NULL,
                     NULL,
                     0,
                     0,
                     '1',
                     '1',
                     tipocambiox,
                     0,
                     0,
                     secpedxtrans,--TIENE Q IR EL CORRELATIVO DE PEDID_X_tRANSFERIR
                     NULL,
                     sysdate,
                     NULL,	--FECHA_ENTREGAOC LO COLOCARÉ NULL
                     listobjpedtransfx(1).codmesa,
                     listobjpedtransfx(1).codsede,
                     listobjpedtransfx(1).codcanal,
                     listobjpedtransfx(1).codruta,
                     listobjpedtransfx(1).coddirecciondespacho,
                     NULL,
                     substr(tdocx, 0, 2),
                     listobjpedtransfx(1).usuregistra,
                     0,
                     0,
                     0,--VERIFICAR EL MONTO ACA TENGO Q RECALCULAR?
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0
                 );

        INSERT INTO SYSADM.spedido_despacho (
            cod_cia,
            nro_pedido,
            nro_back_order,
            compania_venta,
            cod_despacho,
            direccion,
            ubigeo,
            rowversion
        ) VALUES (
                     '00',
                     nropedidox,
                     0,
                     listobjpedtransfx(1).codempresa,
                     listobjpedtransfx(1).coddirecciondespacho,
                     direccionx,
                     ubigeox,
                     0
                 );

        FOR indx IN listobjpedtransfx.first..listobjpedtransfx.last LOOP

                SELECT
                    um_venta,
                    um_control_stock,
                    tax_igv
                INTO
                    umorigenx,
                    umdestinox,
                    tieneigvx
                FROM
                    sysadm.articulos
                WHERE
                        cod_item = listobjpedtransfx(indx).codproducto;

                SELECT
                    nvl(factor, 0),
                    operacion
                INTO
                    factorx,
                    operacionx
                FROM
                    sysadm.factores_conv
                WHERE
                        um_origen = umorigenx
                  AND um_destino = umdestinox;

                IF operacionx = '*' THEN
                    cantidadx := listobjpedtransfx(indx).cantidad * factorx;
                ELSIF operacionx = '/' THEN
                    cantidadx := listobjpedtransfx(indx).cantidad / factorx;
                END IF;

                -- OBTIENE DCTO LP
                SELECT
                    nvl(dscto_caracteristi, 0),
                    ( ( nvl(dscto_caracteristi, 0) * listobjpedtransfx(indx).precio ) / 100 ) * listobjpedtransfx(indx).cantidad
                INTO
                    pdctolpx,
                    mdctolpx
                FROM
                    sysadm.precios_c
                WHERE
                        compania_venta = listobjpedtransfx(indx).codempresa
                  AND cod_lista = listobjpedtransfx(indx).codlistaprecios;
                --end dcto lp


                --obtiene dcto condicion pago


                SELECT
                    COUNT(*)
                INTO countdctocpx
                FROM
                    sysadm.dcto_cond_pago_art
                WHERE
                        compania_venta = listobjpedtransfx(indx).codempresa
                  AND cod_item = listobjpedtransfx(indx).codproducto
                  AND cod_lista = listobjpedtransfx(indx).codlistaprecios
                  AND cond_pago = listobjpedtransfx(indx).codcondicion
                  AND um_item = listobjpedtransfx(indx).um;

                IF countdctocpx > 0 THEN
                    -- SELECT
                    --     dscto_cp_articulo,
                    --   ( dscto_cp_articulo * listobjpedtransfx(indx).precio / 100 ) * listobjpedtransfx(indx).cantidad
                    --PARA Q SAGA IGUAL AL XRAY DCTO SUCESIVO
                    SELECT DSCTO_CP_ARTICULO, ((DSCTO_CP_ARTICULO*(listobjpedtransfx(indx).precio-(mdctolpx/listobjpedtransfx(indx).cantidad)))/100)*listobjpedtransfx(indx).cantidad
                    INTO
                        pdctocpx,
                        mdctocpx
                    FROM
                        sysadm.dcto_cond_pago_art
                    WHERE
                            compania_venta = listobjpedtransfx(indx).codempresa
                      AND cod_item = listobjpedtransfx(indx).codproducto
                      AND cod_lista = listobjpedtransfx(indx).codlistaprecios
                      AND cond_pago = listobjpedtransfx(indx).codcondicion
                      AND um_item = listobjpedtransfx(indx).um;

                ELSE
                    pdctocpx := 0;
                    mdctocpx := 0;
                END IF;




                --end dcto condicion pago

                --FALTA COLOCAR D DCTO EXCEP
                pdctoexcepx:=0;
                mdctoexcepx:=0;
                --pdctopromx:=0;
                --mdctopromx:=0;

                select
                        round(CASE WHEN   SYSDATE>=F_DSCTO_PROM_INI  AND SYSDATE<=F_DSCTO_PROM_FIN THEN
                                       nvl(DSCTO_PROMOCION,0)
                                   ELSE
                                       0

                                  END,2)/100 DCTO_PROMO

                INTO   pdctopromx
                from SYSADM.PRECIOS_D
                where COMPANIA_VENTA=listobjpedtransfx(1).codempresa
                  AND  COD_LISTA=listobjpedtransfx(1).codlistaprecios  AND UM_ITEM=listobjpedtransfx(indx).um
                  AND COD_ITEM=listobjpedtransfx(indx).codproducto  AND MONEDA_LISTA='S/.';


                mdctopromx:=pdctopromx*(( listobjpedtransfx(indx).precio  * listobjpedtransfx(indx).cantidad)-(mdctolpx+mdctocpx));

                --end dctos excep  prmom

                --DCTO PAQUETE Y VOLUMEN
                SELECT COUNT(*) INTO canttempdctox
                FROM
                    SYSADM.TEMP_DCTO
                WHERE ID_PEDIDO=secpedxtrans
                  AND COD_ITEM=listobjpedtransfx(indx).codproducto;

                IF canttempdctox>0 THEN

                    SELECT ROUND(NVL(PORC_DCTO_PAQ,0),2)/100,ROUND(NVL(MONTO_DCTO_PAQ,0),2),ROUND(NVL(PORC_DCTO_VOL,0),2)/100,ROUND(NVL(MONTO_DCTO_VOL,0),2)
                    INTO

                        pdctopaqx,
                        mdctopaqx,
                        pdctovolx,
                        mdctovolx
                    FROM
                        SYSADM.TEMP_DCTO
                    WHERE ID_PEDIDO=secpedxtrans
                      AND COD_ITEM=listobjpedtransfx(indx).codproducto;
                ELSE

                    pdctopaqx:=0;
                    mdctopaqx:=0;
                    pdctovolx:=0;
                    mdctovolx:=0;


                END IF;

                --END DCTO PAQ Y VOL


                pdctototalx:=pdctolpx+pdctocpx+pdctoexcepx+pdctopromx+pdctovolx+pdctopaqx;
                mdctototalx:=mdctolpx+mdctocpx+mdctoexcepx+mdctopromx+mdctovolx+mdctopaqx;

                ---IGV


                SELECT FLAG2 INTO isempresaexox
                FROM SYSADM.TABLAS WHERE CATEGORIA='001'
                                     AND LLAVE=listobjpedtransfx(indx).codempresa;

                IF isempresaexox = 0 THEN
                    IF tieneigvx = 1 THEN
                        SELECT  NUM1 into porcigvx
                        FROM SYSADM.TABLAS WHERE CATEGORIA='051'
                                             AND LLAVE='03';
                    ELSE
                        porcigvx := 0;
                    END IF;


                    montoigvx := ( listobjpedtransfx(indx).precio * listobjpedtransfx(indx).cantidad - mdctototalx ) * porcigvx;

                ELSE
                    montoigvx := 0;
                END IF;
                --END IGV

                INSERT INTO SYSADM.spedido_detalle (
                    cod_cia,
                    compania_venta,
                    nro_pedido,
                    cod_item,
                    cod_lista_precios,
                    moneda_lista,
                    version_precios,
                    revision_precios,
                    status_linea,
                    almacen_venta,
                    tipo_de_cambio,
                    nro_back_order,
                    decimales,
                    multiplo,
                    um_menudeo,
                    cod_despacho,
                    secuencia,
                    flag_bonificado,
                    qty_pedida,
                    qty_aprobada,
                    qty_despachada,
                    um_pedido,
                    precio_item,
                    valor_venta,
                    cod_paquete,
                    porc_participacion,
                    monto_paquete,
                    factor,
                    venta_neta,
                    hold_code_precio,
                    hold_code_qty_min,
                    hold_code_qty_max,
                    hold_code_inv_det,
                    porcentaje_tc,
                    porcentaje_cp_tc,
                    porcentaje_cp,
                    porcentaje_monto,
                    porcentaje_vol,
                    porcentaje_excep,
                    porcentaje_paquete,
                    porcentaje_item,
                    descuento_tc,
                    descuento_cp,
                    descuento_cp_tc,
                    descuento_monto,
                    descuento_vol,
                    descuento_item,
                    descuento_excep,
                    descuento_paquete,
                    impuesto_selectivo,
                    impuesto_igv,
                    flete,
                    tasa_flete,
                    igv_flete,
                    rowversion
                ) VALUES (
                             '00',
                             listobjpedtransfx(indx).codempresa,
                             nropedidox,
                             listobjpedtransfx(indx).codproducto,
                             listobjpedtransfx(indx).codlistaprecios,
                             'S/.',
                             NULL,--versopmprecios
                             NULL,--revision_precios
                             NULL,--status_linea
                             listobjpedtransfx(indx).codalmacen,
                             1,--tipo_de_cambio
                             0,--nro_back_order
                             NULL,--decimales
                             NULL,--MULTIPLO
                             NULL,--um_menudeo
                             listobjpedtransfx(1).coddirecciondespacho,
                             indx,--secuencia
                             0,--flag bonificado
                             cantidadx,--qty pedida
                             cantidadx,--qty aprobada
                             cantidadx,--qty despachada
                             listobjpedtransfx(indx).um,
                             listobjpedtransfx(indx).precio,--PRECIO BASE
                             listobjpedtransfx(indx).precio * listobjpedtransfx(indx).cantidad,--valrventa
                             NULL,--cod_paquete
                             0,--porc_participacion
                             0,--monto_paquete
                             factorx,
                             (listobjpedtransfx(indx).precio * listobjpedtransfx(indx).cantidad)-mdctototalx+montoigvx,---VENTA NETA --ACA LO INTERESANTE
                             0,--hold_code_precio
                             0,--hold_code_qty_min
                             0,--hold_code_qty_max
                             0,--hold_code_cod_inv_det
                             ( pdctolpx / 100 ),--porcentaje_tc
                             ( pdctocpx / 100 ),--PORCENTAJE_CP_TC
                             0,--PORCENTAJE_CP
                             0,--PORCENTAJE_MONTO
                             pdctovolx,--PORCENTAJE_VOL
                             0,--PORCENTAJE_EXCEP  falta colocal
                             pdctopaqx, --PORCENTAJE_PAQUETE
                             pdctopromx,--porcentaje item falta olocar es el de promocion
                             mdctolpx,--DESCUENTO_TC
                             0,--DESCUENTO_CP
                             mdctocpx,--DESCUENTO_CP_TC
                             0,--DESCUENTO_MONTO
                             mdctovolx,--descuento_vol
                             mdctopromx,--DESCUENTO_ITEM
                             0,--descuento_excep  falta agregar
                             mdctopaqx,--descuento_paquete
                             0,
                             montoigvx,
                             0,
                             0,
                             0,
                             0
                         );

                INSERT INTO SYSADM.spedido_fecentrega (
                    cod_cia,
                    nro_pedido,
                    nro_back_order,
                    cod_item,
                    secuencia,
                    flag_bonificado,
                    cod_despacho,
                    compania_venta,
                    fecha_entrega,
                    qty_original,
                    qty_pedida,
                    qty_atendida,
                    qty_despachada,
                    qty_facturada,
                    qty_temporal,
                    rowversion
                ) VALUES (
                             '00',
                             nropedidox,
                             0,
                             listobjpedtransfx(indx).codproducto,
                             indx,
                             0,
                             listobjpedtransfx(1).coddirecciondespacho,
                             listobjpedtransfx(1).codempresa,
                             to_date(to_char(sysdate + 1, 'DD-MON-YY'), 'DD-MON-YY'),
                             cantidadx,
                             cantidadx,
                             0,
                             0,
                             0,
                             0,
                             0
                         );


                importecabecerax:=importecabecerax+(listobjpedtransfx(indx).precio * listobjpedtransfx(indx).cantidad)-mdctototalx+montoigvx;
                secuenciax := indx;


            END LOOP;


        --agregando flete

        SELECT
            um_venta,
            um_control_stock,
            tax_igv
        INTO
            umorigenx,
            umdestinox,
            tieneigvx
        FROM
            sysadm.articulos
        WHERE
                cod_item = '000000';

        SELECT
            nvl(factor, 0),
            operacion
        INTO
            factorx,
            operacionx
        FROM
            sysadm.factores_conv
        WHERE
                um_origen = umorigenx
          AND um_destino = umdestinox;

        IF operacionx = '*' THEN
            cantidadx := 1 * factorx;
        ELSIF operacionx = '/' THEN
            cantidadx := 1 / factorx;
        END IF;

        SELECT
            precio_base
        INTO preciobasefletex
        FROM
            sysadm.precios_d
        WHERE
                compania_venta = listobjpedtransfx(1).codempresa
          AND cod_lista = listobjpedtransfx(1).codlistaprecios
          AND um_item = umorigenx
          AND cod_item = '000000'  AND MONEDA_LISTA='S/.';

        SELECT
            nvl(dscto_caracteristi, 0),
            ( ( nvl(dscto_caracteristi, 0) * preciobasefletex ) / 100 ) * 1
        INTO
            pdctolpx,
            mdctolpx
        FROM
            sysadm.precios_c
        WHERE
                compania_venta = listobjpedtransfx(1).codempresa
          AND cod_lista = listobjpedtransfx(1).codlistaprecios;

        SELECT
            COUNT(*)
        INTO countdctocpx
        FROM
            sysadm.dcto_cond_pago_art
        WHERE
                compania_venta = listobjpedtransfx(1).codempresa
          AND cod_item = '000000' AND MONEDA_LISTA='S/.'
          AND cod_lista = listobjpedtransfx(1).codlistaprecios
          AND cond_pago = listobjpedtransfx(1).codcondicion
          AND um_item = umorigenx;

        IF countdctocpx > 0 THEN
            SELECT
                dscto_cp_articulo,
                ( dscto_cp_articulo * preciobasefletex / 100 ) * 1
            INTO
                pdctocpx,
                mdctocpx
            FROM
                sysadm.dcto_cond_pago_art
            WHERE
                    compania_venta = listobjpedtransfx(1).codempresa
              AND cod_item = '000000' AND MONEDA_LISTA='S/.'
              AND cod_lista =listobjpedtransfx(1).codlistaprecios
              AND cond_pago = listobjpedtransfx(1).codcondicion
              AND um_item = umorigenx;

        ELSE
            pdctocpx := 0;
            mdctocpx := 0;
        END IF;


        mdctototalx:=mdctolpx+mdctocpx;

        SELECT FLAG2 INTO isempresaexox
        FROM SYSADM.TABLAS WHERE CATEGORIA='001'
                             AND LLAVE=listobjpedtransfx(1).codempresa;

        IF isempresaexox = 0 THEN
            IF tieneigvx = 1 THEN
                SELECT  NUM1 into porcigvx
                FROM SYSADM.TABLAS WHERE CATEGORIA='051'
                                     AND LLAVE='03';
            ELSE
                porcigvx := 0;
            END IF;


            montoigvx := ( preciobasefletex * 1 - mdctototalx ) * porcigvx;

        ELSE
            montoigvx := 0;
        END IF;



        INSERT INTO SYSADM.spedido_detalle (
            cod_cia,
            compania_venta,
            nro_pedido,
            cod_item,
            cod_lista_precios,
            moneda_lista,
            version_precios,
            revision_precios,
            status_linea,
            almacen_venta,
            tipo_de_cambio,
            nro_back_order,
            decimales,
            multiplo,
            um_menudeo,
            cod_despacho,
            secuencia,
            flag_bonificado,
            qty_pedida,
            qty_aprobada,
            qty_despachada,
            um_pedido,
            precio_item,
            valor_venta,
            cod_paquete,
            porc_participacion,
            monto_paquete,
            factor,
            venta_neta,
            hold_code_precio,
            hold_code_qty_min,
            hold_code_qty_max,
            hold_code_inv_det,
            porcentaje_tc,
            porcentaje_cp_tc,
            porcentaje_cp,
            porcentaje_monto,
            porcentaje_vol,
            porcentaje_excep,
            porcentaje_paquete,
            porcentaje_item,
            descuento_tc,
            descuento_cp,
            descuento_cp_tc,
            descuento_monto,
            descuento_vol,
            descuento_item,
            descuento_excep,
            descuento_paquete,
            impuesto_selectivo,
            impuesto_igv,
            flete,
            tasa_flete,
            igv_flete,
            rowversion
        ) VALUES (
                     '00',
                     listobjpedtransfx(1).codempresa,
                     nropedidox,
                     '000000',
                     listobjpedtransfx(1).codlistaprecios,
                     'S/.',
                     NULL,
                     NULL,
                     NULL,
                     listobjpedtransfx(1).codalmacen,
                     1,
                     0,
                     NULL,
                     NULL,
                     NULL,
                     listobjpedtransfx(1).coddirecciondespacho,
                     secuenciax + 1,
                     0,
                     cantidadx,
                     cantidadx,
                     cantidadx,
                     umorigenx,
                     preciobasefletex,
                     preciobasefletex * 1,
                     NULL,
                     0,
                     0,
                     factorx,
                     ( ( preciobasefletex * 1 ) + montoigvx - mdctototalx ) * 1,
                     0,
                     0,
                     0,
                     0,
                     ( pdctolpx / 100 ),
                     ( pdctocpx / 100 ),
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     mdctolpx,
                     0,
                     mdctocpx,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     montoigvx,
                     0,
                     0,
                     0,
                     0
                 );

        UPDATE SYSADM.spedido_header
        SET
            monto_desp_bonif = monto_desp_bonif +importecabecerax +( ( preciobasefletex * 1 ) + montoigvx - mdctototalx )
        WHERE
                compania_venta = listobjpedtransfx(1).codempresa
          AND nro_pedido = nropedidox;

        INSERT INTO SYSADM.spedido_fecentrega (
            cod_cia,
            nro_pedido,
            nro_back_order,
            cod_item,
            secuencia,
            flag_bonificado,
            cod_despacho,
            compania_venta,
            fecha_entrega,
            qty_original,
            qty_pedida,
            qty_atendida,
            qty_despachada,
            qty_facturada,
            qty_temporal,
            rowversion
        ) VALUES (
                     '00',
                     nropedidox,
                     0,
                     '000000',
                     secuenciax + 1,
                     0,
                     listobjpedtransfx(1).coddirecciondespacho,
                     listobjpedtransfx(1).codempresa,
                     to_date(to_char(sysdate + 1, 'DD-MON-YY'), 'DD-MON-YY'),
                     1,
                     1,
                     0,
                     0,
                     0,
                     0,
                     0
                 );

        --end agregando flete

        secuenciax:= secuenciax + 1;

        --agregando bonificaciones


        OPEN cursorbonifx FOR
            SELECT  COD_ITEM_BONIF ,UM_ITEM_BONIF,SUM(CANTIDAD)
            FROM SYSADM.TEMP_BONIF_DET
            WHERE ID_PEDIDO=secpedxtrans
            GROUP BY COD_ITEM_BONIF ,UM_ITEM_BONIF;


        LOOP
            FETCH  cursorbonifx INTO  vcoditembonif,vumitembonif,vcantidadbonif;
            EXIT WHEN  cursorbonifx%NOTFOUND;

            secuenciax :=secuenciax +1;

            SELECT
                um_venta,
                um_control_stock,
                tax_igv
            INTO
                umorigenx,
                umdestinox,
                tieneigvx
            FROM
                sysadm.articulos
            WHERE
                    cod_item = vcoditembonif;



            SELECT
                nvl(factor, 0),
                operacion
            INTO
                factorx,
                operacionx
            FROM
                sysadm.factores_conv
            WHERE
                    um_origen = umorigenx
              AND um_destino = umdestinox;

            IF operacionx = '*' THEN
                cantidadx := 1 * factorx;
            ELSIF operacionx = '/' THEN
                cantidadx := 1 / factorx;

            END IF;


            SELECT
                precio_base
            INTO preciobasebonifx
            FROM
                sysadm.precios_d
            WHERE
                    compania_venta = listobjpedtransfx(1).codempresa
              AND cod_lista =listobjpedtransfx(1).codlistaprecios
              AND um_item = umorigenx
              AND cod_item =vcoditembonif;

            INSERT INTO SYSADM.spedido_detalle (
                cod_cia,
                compania_venta,
                nro_pedido,
                cod_item,
                cod_lista_precios,
                moneda_lista,
                version_precios,
                revision_precios,
                status_linea,
                almacen_venta,
                tipo_de_cambio,
                nro_back_order,
                decimales,
                multiplo,
                um_menudeo,
                cod_despacho,
                secuencia,
                flag_bonificado,
                qty_pedida,
                qty_aprobada,
                qty_despachada,
                um_pedido,
                precio_item,
                valor_venta,
                cod_paquete,
                porc_participacion,
                monto_paquete,
                factor,
                venta_neta,
                hold_code_precio,
                hold_code_qty_min,
                hold_code_qty_max,
                hold_code_inv_det,
                porcentaje_tc,
                porcentaje_cp_tc,
                porcentaje_cp,
                porcentaje_monto,
                porcentaje_vol,
                porcentaje_excep,
                porcentaje_paquete,
                porcentaje_item,
                descuento_tc,
                descuento_cp,
                descuento_cp_tc,
                descuento_monto,
                descuento_vol,
                descuento_item,
                descuento_excep,
                descuento_paquete,
                impuesto_selectivo,
                impuesto_igv,
                flete,
                tasa_flete,
                igv_flete,
                rowversion
            ) VALUES (
                         '00',--cod cia
                         listobjpedtransfx(1).codempresa,--codempresa
                         nropedidox,--nropedido
                         vcoditembonif,--coditem
                         listobjpedtransfx(1).codlistaprecios,
                         'S/.',
                         NULL,
                         NULL,
                         NULL,
                         listobjpedtransfx(1).codalmacen,
                         NULL,--tipo cambio
                         0,--nro back order
                         NULL,
                         NULL,
                         NULL,
                         listobjpedtransfx(1).coddirecciondespacho,
                         secuenciax ,
                         1,--flag bonificado
                         vcantidadbonif,
                         vcantidadbonif,
                         vcantidadbonif,
                         umorigenx,
                         preciobasebonifx,
                         preciobasebonifx * vcantidadbonif,---valor venta
                         NULL,
                         0,
                         0,
                         factorx,
                         0,--venta neta
                         0,
                         0,
                         0,
                         0,
                         0,
                         0,--porcentaje_lista_precios
                         0,
                         0,
                         0,
                         0,
                         0,
                         1,--porcentaje_item  100 %
                         0,--dcto lista precios
                         0,
                         0, --dcto condicion de pago
                         0,--dcto monto
                         0,--dcto vol
                         preciobasebonifx*vcantidadbonif,--dcto por item
                         0,--excepcional
                         0,--dcto paquete
                         0,--imp selectivo
                         0,--igv
                         0,--flete
                         0,--tasa flete
                         0,--igv flete
                         0 --rowversion
                     );



            INSERT INTO SYSADM.spedido_fecentrega (
                cod_cia,
                nro_pedido,
                nro_back_order,
                cod_item,
                secuencia,
                flag_bonificado,
                cod_despacho,
                compania_venta,
                fecha_entrega,
                qty_original,
                qty_pedida,
                qty_atendida,
                qty_despachada,
                qty_facturada,
                qty_temporal,
                rowversion
            ) VALUES (
                         '00',
                         nropedidox,
                         0,
                         vcoditembonif
                         ,
                         secuenciax,
                         1,
                         listobjpedtransfx(1).coddirecciondespacho,
                         listobjpedtransfx(1).codempresa,
                         to_date(to_char(sysdate + 1, 'DD-MON-YY'), 'DD-MON-YY'),
                         vcantidadbonif,
                         vcantidadbonif,
                         0,
                         0,
                         0,
                         0,
                         0
                     );




        END LOOP;
        CLOSE cursorbonifx;














        UPDATE sysadm.correlativo_com
        SET
            correlativo = correlativopedidox
        WHERE
                llave = 'P'
          AND compania = listobjpedtransfx(1).codempresa;







        rptax := 1;
        mensajex := 'OK';
    EXCEPTION
        WHEN OTHERS THEN
            rptax := -1;
            -- ROLLBACK;
            mensajex := 'SE ENCONTRÓ UN ERROR: '
                || ''
                || sqlerrm
                || '-'
                || dbms_utility.format_error_backtrace;





    END REGISTRAR_PEDIDO_XRAY;








    PROCEDURE INSERT_BONIF_TEMP(pcorrelativo IN NUMBER, TABLE_OBJ IN PEDIDO_PREVENTA_TABLE)
        IS

        vcantpaqx NUMBER;
        codpaquetex VARCHAR2(4);
        cantvigentepaqx number;
        vdescpaqx  VARCHAR2(200);
        vumpaqx VARCHAR2(8);
        vismultiplox NUMBER;
        visrangox  NUMBER;
        vmodopartx NUMBER;
    BEGIN

        FOR indx IN  TABLE_OBJ.FIRST .. TABLE_OBJ.LAST
            LOOP



                SELECT COUNT(*)  INTO  vcantpaqx
                FROM SYSADM.bonif_paq_p1 p, SYSADM.articulos a
                WHERE p.cod_cia = a.cod_cia
                  AND p.cod_item = a.cod_item
                  AND p.cod_cia = '00'
                  AND p.compania_venta = TABLE_OBJ(indx).codempresa
                  AND p.cod_lista = TABLE_OBJ(indx).codlistaprecios
                  AND p.moneda_lista = 'S/.'
                  AND P.COD_ITEM=TABLE_OBJ(indx).codproducto;


                IF vcantpaqx>0 THEN
                    --obtiene codigo del paquete

                    SELECT P.COD_PAQUETE INTO codpaquetex
                    FROM SYSADM.bonif_paq_p1 p, SYSADM.articulos a
                    WHERE p.cod_cia = a.cod_cia
                      AND p.cod_item = a.cod_item
                      AND p.cod_cia = '00'
                      AND p.compania_venta = TABLE_OBJ(indx).codempresa
                      AND p.cod_lista =TABLE_OBJ(indx).codlistaprecios
                      AND p.moneda_lista = 'S/.'
                      AND P.COD_ITEM=TABLE_OBJ(indx).codproducto;

                    --VERIFICAR SI EL PAQUETE ESTA VIGENTE


                    SELECT  COUNT(*) INTO cantvigentepaqx
                    FROM SYSADM.bonif_paq p, SYSADM.bonificacion b
                    WHERE b.cod_cia = p.cod_cia
                      AND b.compania_venta = p.compania_venta
                      AND b.cod_lista = p.cod_lista
                      AND b.moneda_lista = p.moneda_lista
                      AND b.cod_bonif = p.cod_bonif
                      AND b.flag_paquete = '1'
                      AND b.compania_venta = TABLE_OBJ(indx).codempresa
                      AND b.cod_lista = TABLE_OBJ(indx).codlistaprecios
                      --AND b.flag_rangos = '1'
                      AND p.cod_paquete=codpaquetex
                      AND SYSDATE>=b.fecha_inicio
                      AND SYSDATE<=b.fecha_fin ;


                    IF cantvigentepaqx>0 THEN

                        --OBTIENE DATOS DEL PAQUETE
                        --HE CONSIDERADO COD_BONIF=COD_PAQUETE
                        SELECT   		 p.descripcion,
                                       p.um_paquete,
                                       b.flag_multiplo,
                                       b.flag_rangos ,
                                       P.MODO_PARTICIPACION  --0 es monto 1 es unidad de medida

                        INTO vdescpaqx,vumpaqx,vismultiplox,visrangox,vmodopartx

                        FROM SYSADM.bonif_paq p, SYSADM.bonificacion b
                        WHERE b.cod_cia = p.cod_cia
                          AND b.compania_venta = p.compania_venta
                          AND b.cod_lista = p.cod_lista
                          AND b.moneda_lista = p.moneda_lista
                          AND b.cod_bonif = p.cod_bonif
                          AND b.flag_paquete = '1'
                          AND b.compania_venta = TABLE_OBJ(indx).codempresa
                          AND b.cod_lista =  TABLE_OBJ(indx).codlistaprecios
                          AND p.cod_paquete=codpaquetex;
                        --AND b.flag_rangos = '1'

                        INSERT INTO SYSADM.TEMP_BONIF(ID_PEDIDO,COD_ITEM,UM_ITEM,CANTIDAD,COD_PAQ,IS_RANGO,IS_MULTIPLO,MODO_PART)
                        VALUES(pcorrelativo,TABLE_OBJ(indx).codproducto,TABLE_OBJ(indx).um,TABLE_OBJ(indx).cantidad,codpaquetex,visrangox,vismultiplox,vmodopartx);




                    END IF;



                END IF;






            END LOOP;



    END;







    PROCEDURE CUSTOMERSBYROUTE(PCODEROUTE IN VARCHAR2,OESTADO IN OUT NUMBER,OMENSAJE IN OUT VARCHAR2,ORPTA  IN OUT TYPES.REF_CURSOR)
        IS


    BEGIN

        OPEN ORPTA FOR

            SELECT
                CLI.COD_CLIENTE,
                CLI.DESC_CLIENTE
            FROM
                SYSADM.CLIENTE CLI
                    INNER JOIN SYSADM.LOCALES_CLIENTE LCL ON CLI.COD_CLIENTE = LCL.COD_CLIENTE
            WHERE
                    LCL.RUTA_DESPACHO = PCODEROUTE;

        OESTADO:=1;
        OMENSAJE:='OK';
    EXCEPTION WHEN OTHERS THEN
        OESTADO := -1;
        OMENSAJE:='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.		FORMAT_ERROR_BACKTRACE;



    END CUSTOMERSBYROUTE;





    --VERSION 2

    FUNCTION F_PASSWORD(P_PASSWORD IN VARCHAR2,p_MODO IN VARCHAR2) RETURN VARCHAR2 IS
        v_CLAVEENCRIPTADA varchar2(100);
        v_DESENCRIPTADA VARCHAR2(100);
        n number;
        carac char;
        j number;
        carac1 char;
        v_CLAVEFINAL VARCHAR2(100);
    BEGIN

        n:="LENGTH"(p_PASSWORD);
        IF p_MODO='ENC' THEN
            FOR i IN 1..n LOOP
                    carac:=SUBSTR(p_PASSWORD,i,1);
                    j:=ASCII(carac)-4;
                    carac1:=CHR(j);
                    v_CLAVEENCRIPTADA:=v_CLAVEENCRIPTADA||carac1;
                END LOOP;
            v_CLAVEFINAL:=v_CLAVEENCRIPTADA;
        ELSIF p_MODO='DES' THEN
            FOR i IN 1..n LOOP
                    carac:=SUBSTR(p_PASSWORD,i,1);
                    j:=ASCII(carac)+4;
                    carac1:=CHR(j);
                    v_DESENCRIPTADA:=v_DESENCRIPTADA||carac1;
                END LOOP;
            v_CLAVEFINAL:=v_DESENCRIPTADA;

        END IF;

        RETURN v_CLAVEFINAL;

    END F_PASSWORD;



    PROCEDURE LOGIN_USER_EMAIL(p_USEREMAIL IN VARCHAR2,p_PASSWORD IN VARCHAR2, x_ESTADO OUT NUMBER,x_MENSAJE OUT VARCHAR2,x_RPTA OUT TYPES.REF_CURSOR)IS
        v_CLAVEENCRIPTADA varchar2(100);

        v_CANTUSER NUMBER;
        ex_EXISTEUSUARIO EXCEPTION;
        ex_CLAVE EXCEPTION;
    BEGIN

        SELECT COUNT(USUARIO) INTO v_CANTUSER
        from SYSADM.USUARIOS WHERE ( UPPER(USUARIO) = UPPER(p_USEREMAIL) OR UPPER(COD_PERFIL)=UPPER(p_USEREMAIL));

        IF( v_CANTUSER=0 ) then
            RAISE ex_EXISTEUSUARIO;
        END IF;

        v_CLAVEENCRIPTADA:=F_PASSWORD(p_PASSWORD,'ENC');

        SELECT COUNT(USUARIO) INTO v_CANTUSER
        from SYSADM.USUARIOS WHERE ( UPPER(USUARIO) = UPPER(p_USEREMAIL) OR UPPER(COD_PERFIL)=UPPER(p_USEREMAIL))
                               AND CLAVE=v_CLAVEENCRIPTADA;

        IF( v_CANTUSER=0 ) then
            RAISE ex_CLAVE;
        END IF;

        OPEN x_RPTA FOR
            SELECT   USUARIO,NOMBRE
            FROM SYSADM.USUARIOS
            WHERE ( UPPER(USUARIO) = UPPER(p_USEREMAIL) OR UPPER(COD_PERFIL)=UPPER(p_USEREMAIL))
              AND ESTUSUARIO='H' AND CLAVE=v_CLAVEENCRIPTADA;
        x_ESTADO := 1;
        x_MENSAJE:='OK';

    EXCEPTION
        WHEN ex_EXISTEUSUARIO THEN
            x_ESTADO := 0;
            x_MENSAJE:='USUARIO  INCORRECTO!!';
        WHEN ex_CLAVE THEN
            x_ESTADO := 0;
            x_MENSAJE:='CLAVE INCORRECTA!!';
        WHEN OTHERS THEN
            x_ESTADO := -1;
            x_MENSAJE:='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.			FORMAT_ERROR_BACKTRACE;


    END LOGIN_USER_EMAIL ;



    PROCEDURE SP_LIU_USER(p_PARAMETROS IN LISTPARAMETR0STRING,x_ESTADO OUT NUMBER,x_MENSAJE OUT VARCHAR2,x_RPTA OUT TYPES.REF_CURSOR)
        IS
        v_PROCEDURE VARCHAR2(40);
        v_DNI VARCHAR2(8);
        ex_NOTFOUND EXCEPTION;
        ex_EMAILNOTFOUND EXCEPTION;
        v_CANTUSER NUMBER;
        v_USERNAME VARCHAR2(20);
        v_PASSWORD VARCHAR2(20);
        v_CELULAR VARCHAR2(9);

        v_CLAVEENCRIPTADA VARCHAR2(100);

        --ex_TEST EXCEPTION;
        --v_TEST VARCHAR2(200);
    BEGIN
        --v_TEST:=p_PARAMETROS(1).CLAVE;
        --RAISE ex_TEST;

        IF p_PARAMETROS IS NOT NULL THEN
            FOR indx IN p_PARAMETROS.first..p_PARAMETROS.last LOOP

                    IF p_PARAMETROS(indx).CLAVE = 'v_PROCEDURE' THEN
                        v_PROCEDURE := p_PARAMETROS(indx).VALOR;
                    ELSIF p_PARAMETROS(indx).CLAVE = 'v_DNI' THEN
                        v_DNI := p_PARAMETROS(indx).VALOR;
                    ELSIF p_PARAMETROS(indx).CLAVE = 'v_USERNAME' THEN
                        v_USERNAME := p_PARAMETROS(indx).VALOR;
                    ELSIF p_PARAMETROS(indx).CLAVE = 'v_PASSWORD' THEN
                        v_PASSWORD := p_PARAMETROS(indx).VALOR;
                    ELSIF p_PARAMETROS(indx).CLAVE = 'v_CELULAR' THEN
                        v_CELULAR := p_PARAMETROS(indx).VALOR;

                    END IF;
                END LOOP;
        END IF;

        IF v_PROCEDURE='LI_USER_BY_DNI' THEN --LISTA USUARIOS POR DNI

            SELECT COUNT(USUARIO) INTO v_CANTUSER
            FROM SYSADM.USUARIOS WHERE IDENTITY_NUMBER=v_DNI;
            IF v_CANTUSER=0 THEN
                RAISE ex_NOTFOUND;
            END IF;
            SELECT COUNT(COD_PERFIL) INTO v_CANTUSER
            FROM SYSADM.USUARIOS WHERE IDENTITY_NUMBER=v_DNI;
            IF v_CANTUSER=0 THEN
                RAISE ex_EMAILNOTFOUND;
            END IF;



            OPEN x_RPTA FOR
                SELECT USUARIO,UPPER(NOMBRE)NOMBRE,UPPER(COD_PERFIL) EMAIL
                FROM SYSADM.USUARIOS WHERE IDENTITY_NUMBER=v_DNI;
            x_ESTADO:=1;
            x_MENSAJE:='OK';

        ELSIF  v_PROCEDURE='UP_USER_LOGIN' THEN--- UPDATE USER PARA EL SIGN UP
            v_CLAVEENCRIPTADA:=F_PASSWORD(v_PASSWORD,'ENC');
            UPDATE  SYSADM.USUARIOS
            SET CLAVE=v_CLAVEENCRIPTADA,MOBILE=v_CELULAR--,EMAIL=v_EMAIL
            WHERE USUARIO=v_USERNAME;
            x_ESTADO:=1;
            x_MENSAJE:='SE REGISTRÓ CORRECTAMENTE!!';
            COMMIT;
        END IF;

    EXCEPTION
        /*WHEN ex_TEST THEN
	x_ESTADO := 0;
		x_MENSAJE:=v_TEST;*/
        WHEN ex_NOTFOUND THEN
            x_ESTADO := 0;
            x_MENSAJE:='NO EXISTE EL USUARIO CON EL DNI INGRESADO!!';
        WHEN ex_EMAILNOTFOUND THEN
            x_ESTADO := 0;
            x_MENSAJE:='EL USUARIO NO TIENE UN EMAIL ASOCIADO!!';
        WHEN OTHERS THEN
            ROLLBACK;
            x_ESTADO := -1;
            x_MENSAJE:='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.			FORMAT_ERROR_BACKTRACE;


    END SP_LIU_USER;



    PROCEDURE SP_HOME_DATA(p_PARAMETROS IN LISTPARAMETR0STRING,x_ESTADO OUT NUMBER,x_MENSAJE OUT VARCHAR2,x_RPTA OUT TYPES.REF_CURSOR)
        IS
        v_PROCEDURE VARCHAR2(40);
        v_USERNAME VARCHAR2(20);
        v_PAGE NUMBER;
        v_LIMIT NUMBER;
    BEGIN

        IF p_PARAMETROS IS NOT NULL THEN
            FOR indx IN p_PARAMETROS.first..p_PARAMETROS.last LOOP

                    IF p_PARAMETROS(indx).CLAVE = 'v_PROCEDURE' THEN
                        v_PROCEDURE := p_PARAMETROS(indx).VALOR;
                    ELSIF p_PARAMETROS(indx).CLAVE = 'v_USERNAME' THEN
                        v_USERNAME := p_PARAMETROS(indx).VALOR;
                    ELSIF p_PARAMETROS(indx).CLAVE = 'v_PAGE' THEN
                        v_PAGE := CAST (p_PARAMETROS(indx).VALOR AS NUMBER);
                    ELSIF p_PARAMETROS(indx).CLAVE = 'v_LIMIT' THEN
                        v_LIMIT := CAST (p_PARAMETROS(indx).VALOR AS NUMBER);

                    END IF;
                END LOOP;
        END IF;

        IF v_PROCEDURE='LI_BONIF_ITEM' THEN --LISTA BONIFICACIONES POR ITEM
            OPEN x_RPTA FOR
                SELECT BONIF_ITEM.NRO ,BONIF_ITEM.COD_ITEM,BONIF_ITEM.DESC_ITEM,BONIF_ITEM.FOTO
                FROM
                    (
                        select ROWNUM NRO, COD_ITEM,DESC_ITEM,FOTO
                        from SYSADM.ARTICULOS
                        where foto is not null AND STATUS_ITEM='A'
                          AND FLAG_VTA_DIGITAL=1
                        /*SELECT ROWNUM NRO, ART_BON.COD_ITEM ,ART_BON.DESC_ITEM
						FROM SYSADM.BONIFICACION_D1 DET_BON
						INNER JOIN SYSADM.BONIFICACION BON ON
						BON.COD_CIA =DET_BON.COD_CIA
						AND BON.COMPANIA_VENTA=DET_BON.COMPANIA_VENTA
						AND BON.COD_LISTA = BON.COD_LISTA
						AND BON.MONEDA_LISTA = DET_BON.MONEDA_LISTA
						AND DET_BON.COD_BONIF=BON.COD_BONIF AND DET_BON.PRIORIDAD=1
						INNER JOIN   (select PC.COD_CIA,PC.COMPANIA_VENTA,PC.COD_LISTA
						from SYSADM.PRECIOS_C PC
						WHERE PC.COD_CIA='00'
						AND PC.COMPANIA_VENTA='01'
						AND PC.CODSEDE='001'
						AND PC.CODCANAL=1)DATOS_VEND ON
						DATOS_VEND.COD_CIA=BON.COD_CIA
						AND DATOS_VEND.COMPANIA_VENTA=BON.COMPANIA_VENTA
						AND DATOS_VEND.COD_LISTA=DET_BON.COD_LISTA
						AND DATOS_VEND.COD_LISTA=BON.COD_LISTA
						INNER JOIN SYSADM.ARTICULOS ART_VTA ON ART_VTA.COD_ITEM=DET_BON.COD_ITEM
						INNER JOIN SYSADM.ARTICULOS ART_BON ON ART_BON.COD_ITEM=DET_BON.COD_ITEM_BONIF
						WHERE 1=1
						AND SYSDATE >= BON.FECHA_INICIO
						AND SYSDATE <= BON.FECHA_FIN
						ORDER BY BON.FECHA_FIN DESC*/
                    )BONIF_ITEM WHERE NRO>=(v_PAGE-1)*v_LIMIT+1 AND  NRO<(((v_PAGE-1)*v_LIMIT)+v_LIMIT)+1;

            x_ESTADO:=1;
            x_MENSAJE:='OK';

        END IF;

    EXCEPTION


        WHEN OTHERS THEN
            ROLLBACK;
            x_ESTADO := -1;
            x_MENSAJE:='SE ENCONTRÓ UN ERROR: '||''||SQLERRM||'-'||DBMS_UTILITY.			FORMAT_ERROR_BACKTRACE;


    END SP_HOME_DATA;





END PREVENTA_PROD_2_1_1;
