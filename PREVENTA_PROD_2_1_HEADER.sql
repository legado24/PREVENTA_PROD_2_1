CREATE OR REPLACE
PACKAGE "PREVENTA_PROD_2_1" AS
FUNCTION  LOGINUSUARIO(usernamex in varchar2,passwordx in varchar2,codigox in varchar2,imeix in varchar2)return TYPES.ref_cursor;
PROCEDURE VALI_REGIST_MAC(usernamex  IN VARCHAR2,passwordx IN VARCHAR2,macNewx	IN VARCHAR2,rptax  IN OUT    NUMBER,mensajex IN OUT    VARCHAR2);
FUNCTION RUTAS_DIA_USUARIO(usernamex IN VARCHAR2,diax IN VARCHAR2)return TYPES.ref_cursor;
FUNCTION CLIENTES_BY_RUTA(codRutax in VARCHAR2) return TYPES.ref_cursor;
--
FUNCTION CLIENTES_BY_DIA(usernamex IN VARCHAR2,diax IN VARCHAR2) return TYPES.ref_cursor;
FUNCTION DATOS_CLIENTE(codClientex IN VARCHAR2,codEmpresax IN VARCHAR2) return TYPES.ref_cursor;--remplazado
FUNCTION DATOS_USUARIO(usernamex IN VARCHAR2) return TYPES.ref_cursor;
FUNCTION  OBTENER_CONDICIONES(codListax IN VARCHAR2,codVendedorx IN VARCHAR2,tieneDeudaX IN NUMBER,codclientex IN VARCHAR2) return types.ref_cursor ;
FUNCTION CONSULTAR_UBIGEO(eTipoEleme  IN VARCHAR,eCodigEleme IN VARCHAR)RETURN TYPES.REF_CURSOR;
FUNCTION BUSCAR_ARTICULOS(codempresax in varchar2,codalmacenx in varchar2,codlistax in varchar2,condpagox in varchar2,codcanalx IN VARCHAR2,  filtrox in varchar2)  return types.ref_cursor ;
PROCEDURE REGISTRARPEDIDOMOVIL(TABLE_OBJ IN PEDIDO_PREVENTA_TABLE,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2) ;
FUNCTION LISTAR_TIPO_CLIENTE return types.ref_cursor;
FUNCTION LISTAR_GIRO_CLIENTE return types.ref_cursor;
FUNCTION LISTAR_LP(usuariox IN VARCHAR2) RETURN TYPES.REF_CURSOR;
--FUNCTION REGISTRARCLIENTE (CLIENTE_OBJ IN CLIENTE_MOVIL) return number;
PROCEDURE MANTCLIENTE(CLIENTE_OBJ IN CLIENTE_MOVIL,localesx IN LOCAL_CLIENTE_MOVIL_TABLE_UPD,operacionx IN NUMBER ,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2);



PROCEDURE  DATOS_USUARIO_ALTA(usuariox IN VARCHAR2,csedesx OUT types.ref_cursor,ccanalx OUT types.ref_cursor,
   cempresasx OUT types.ref_cursor);
PROCEDURE  DATOS_CLIENTE_PREVENTA(codClientex IN VARCHAR2,codEmpresax IN VARCHAR2,codListax IN VARCHAR2,  limiteCreditox OUT NUMBER,deudaActx OUT NUMBER,tipoClientex OUT VARCHAR2,
antiguedadx OUT VARCHAR2,cumpleañosx OUT VARCHAR2,velocidadPagox OUT NUMBER,countPedidosx OUT NUMBER,listaPreciox OUT VARCHAR2);

PROCEDURE DATOS_VENDEDOR(usuariox IN VARCHAR2,operacionx IN NUMBER,fechaServerx OUT VARCHAR2,periodox OUT VARCHAR2,
pedidosEfectivosx OUT NUMBER,pedidosRechazadosx  OUT NUMBER,visitasnumx OUT NUMBER,visitassolesx OUT NUMBER, ticketdiariox OUT NUMBER,ticketmensualx OUT NUMBER,cursoravancex OUT types.ref_cursor);



PROCEDURE DATOS_VENDEDOR_SUELDO(usuariox IN VARCHAR2,fechaServerx OUT VARCHAR2,periodox OUT VARCHAR2,
pedidosEfectivosx OUT NUMBER,pedidosRechazadosx  OUT NUMBER,visitasnumx OUT NUMBER,visitassolesx OUT NUMBER, ticketdiariox OUT NUMBER,ticketmensualx OUT NUMBER,cursoravancex OUT types.ref_cursor,cabecerax OUT types.ref_cursor,politicasx OUT types.ref_cursor,adicionalx OUT types.ref_cursor,sueldovendedorx OUT types.ref_cursor,cadenarutasx OUT VARCHAR2,permiteofflinex OUT NUMBER);


PROCEDURE  CARGAR_DATOS_ALTA(usuariox IN VARCHAR2,cdptosx OUT types.ref_cursor,cgirosx OUT types.ref_cursor,ctiposx OUT types.ref_cursor,clp OUT types.ref_cursor,crutasx OUT types.ref_cursor );
PROCEDURE  VALIDAR_DOCUMENTO(nrodocx IN VARCHAR2,rptax  IN OUT    NUMBER,mensajex IN OUT    VARCHAR2);
PROCEDURE  DATOS_CLIENTE_UPDATE(codClientex IN VARCHAR2,usuariox IN VARCHAR2,cdatosclientex OUT types.ref_cursor,clocalesclientex OUT types.ref_cursor );
PROCEDURE MANTLOCAL(codClientex IN VARCHAR2,usuariox IN VARCHAR2,operacionx IN VARCHAR2,LOCAL_OBJ IN LOCAL_CLIENTE_MOVIL_UPD,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2);
PROCEDURE REGISTRARNOPEDIDO(codClientex IN VARCHAR2,codEmpresax IN VARCHAR2,codLocalx in VARCHAR2, codRutax in VARCHAR2,descclientex in VARCHAR2,descmotivox in VARCHAR2,direccionx IN VARCHAR2,usuariox IN VARCHAR2,coordenadasx IN VARCHAR2,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2  );
FUNCTION PEDIDOSPORUSUARIO(usuariox VARCHAR2,fechapedido varchar2,codlocalidadx varchar2,codalmacenx varchar2) RETURN TYPES.REF_CURSOR;
FUNCTION DETALLESBYPEDIDO(codempresax VARCHAR2,codalmacenx VARCHAR2,nropedidox VARCHAR2 ) RETURN TYPES.REF_CURSOR;
FUNCTION DOCUMENTOSDEUDACLIENTE(codclientex VARCHAR2,usuariox VARCHAR2)RETURN TYPES.REF_CURSOR;
PROCEDURE MANTCOBRANZA(COBRANZA_OBJ IN COBRANZA_MOVIL,documentosx IN LISTA_DOCDEUDA,operacionx IN NUMBER ,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2);
FUNCTION COBRANZABYUSUARIO(usuariox VARCHAR2,fechax VARCHAR2) RETURN TYPES.REF_CURSOR;
FUNCTION CLIENTE_SIN_COORDENADAS(usernamex IN VARCHAR2) return TYPES.ref_cursor;

PROCEDURE  DATOS_CLIENTE_PREVENTAV1(usuariox IN VARCHAR2,codClientex IN VARCHAR2,codEmpresax IN VARCHAR2,codListax IN VARCHAR2,  limiteCreditox OUT NUMBER,deudaVencidax OUT NUMBER,deudaNoVencidax OUT NUMBER,tipoClientex OUT VARCHAR2,
antiguedadx OUT VARCHAR2,cumpleañosx OUT VARCHAR2,velocidadPagox OUT NUMBER,countPedidosx OUT NUMBER,listaPreciox OUT VARCHAR2,artsugeridosx out types.ref_cursor,artfocusx out types.ref_cursor,exotelefonox out number,nrotelefonox OUT VARCHAR2);
FUNCTION    ANULAR_PEDIDO_INGRESADO(pedidox SPEDIDO_ANULAR)return number;
FUNCTION COBRANZA_BY_USUARIO(usuariox VARCHAR2,fechax VARCHAR2)return TYPES.ref_cursor;
FUNCTION CLIENTES_OUT_RUTA(usernamex IN VARCHAR2,filtrox IN VARCHAR2) return TYPES.ref_cursor;
PROCEDURE  MENU_OUTRUTA_USUARIO(usuariox IN VARCHAR2,outrutax OUT NUMBER );
PROCEDURE DATOS_SUELDO_VENDEDOR(usuariox IN VARCHAR2,cabecerax OUT types.ref_cursor,politicasx OUT types.ref_cursor,adicionalx OUT types.ref_cursor,sueldovendedorx OUT types.ref_cursor,cadenarutasx OUT VARCHAR2);
PROCEDURE  CLIENTES_BY_DIAV2(usernamex IN VARCHAR2,ignoresecx OUT NUMBER, clientesx OUT types.ref_cursor);

PROCEDURE REGISTRARTOKEN(usuariox IN VARCHAR2, tokenx IN VARCHAR2,macx IN VARCHAR2,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2);
FUNCTION GETTOKENS(usuariosx VALUE_ARRAY_STRING) return TYPES.ref_cursor;

PROCEDURE VALIDATEDISTANCIA(usuariox IN VARCHAR2,coodenada1x IN VARCHAR2,coodenada2x IN VARCHAR2,rptax OUT NUMBER ,mensajex OUT VARCHAR2);
FUNCTION ALMACENESBYLOCALIDAD(PCODEMPRESA varchar2,PCODLOCALIDAD varchar2) return types.ref_cursor;
FUNCTION PLANILLACOBRANZA(usuariox IN VARCHAR2)return TYPES.ref_cursor;


---- SGC PLANIF
PROCEDURE  VALIDARPLANIFICADOR(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,
codcanalx IN VARCHAR2,usuarioX IN VARCHAR2,fechax IN VARCHAR2,modox IN VARCHAR2,rptavalidacionx OUT NUMBER );


FUNCTION CANALESBYMESA(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2)return TYPES.ref_cursor;
FUNCTION TIPOSCOLPLANIF return TYPES.ref_cursor;
FUNCTION VALORESBYTIPOSCOL(codproveedorx IN VARCHAR2, codtipocolx IN VARCHAR2)return TYPES.ref_cursor;

PROCEDURE  MANTPLANIFICADOR(idplanificadorx IN NUMBER,codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2,codprovx IN VARCHAR2,codtipocolx IN VARCHAR2,codtipovalx IN VARCHAR2,
LIST_CODVALORES IN VALUE_ARRAY_STRING,LIST_VALORES IN VALUE_ARRAY_STRING,usuariox IN VARCHAR2,fechax IN VARCHAR2,operacionx in NUMBER,rptax        	IN OUT    NUMBER,mensajex IN OUT    VARCHAR2);
FUNCTION LISTPLANIF(fechax IN VARCHAR2, codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2)return TYPES.ref_cursor;
FUNCTION LISTUSERSBYMESA(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2)return TYPES.ref_cursor;
PROCEDURE GENERARPLANIFICADOR(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2,usuarioX IN VARCHAR2,fechax IN VARCHAR2,codproveedorx IN VARCHAR2,modox IN VARCHAR2
,fijosx OUT types.ref_cursor,configplanx OUT types.ref_cursor,colmesesx OUT types.ref_cursor,colprovx OUT types.ref_cursor,rutasx OUT VARCHAR2);
FUNCTION  GETVALBYCLIENTE (codempx in varchar2,tipocolx varchar2,tipovalx in varchar2,valorx in varchar2,usuariox IN VARCHAR2,codclientex IN VARCHAR2,modox IN VARCHAR2 ) return VARCHAR2 ;
FUNCTION  GETTIPOCOLBYPROV(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2 ,codprovx IN VARCHAR2,fechax IN VARCHAR2,usuariox IN VARCHAR2,modox in varchar2)return TYPES.ref_cursor ;
FUNCTION  GETCOLBYTIPOCOL(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2 ,codprovx IN VARCHAR2,codtipocolx IN VARCHAR2,fechax IN VARCHAR2,usuariox in varchar2,modox in varchar2) return TYPES.ref_cursor;
FUNCTION  LISTARTBYPROOV(codprovx IN VARCHAR2,codempx 	IN VARCHAR2,codalmx IN VARCHAR2) return TYPES.ref_cursor;
PROCEDURE  DATOSMODALFOCUS(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR,codalmacenx IN VARCHAR2,codproveedorx IN VARCHAR2,coditemx IN VARCHAR2,cursorcanalesx OUT types.ref_cursor,cursorartx OUT types.ref_cursor);

PROCEDURE DISTRIBXCANAL(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,codcanalx IN VARCHAR2 ,codalmacenx IN VARCHAR2,cantffvvx IN NUMBER ,cantitemx IN NUMBER,cantxvendex IN NUMBER,desccanalx IN VARCHAR2,fechainiciox IN VARCHAR2,fechafinx IN VARCHAR2,coditemx IN VARCHAR2,umventax IN VARCHAR2,umstockx IN VARCHAR2,cantxmesax IN NUMBER,codproveedorx IN VARCHAR2,usuariox IN VARCHAR2,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2);
PROCEDURE DISTRIBALL(codempx IN VARCHAR2,codsedex IN VARCHAR2,codmesax IN VARCHAR2,fechainiciox IN VARCHAR2,fechafinx IN VARCHAR2,coditemx IN VARCHAR2,codalmacenx IN VARCHAR2,umventax IN VARCHAR2,umstockx IN VARCHAR2,codproveedorx IN VARCHAR2,cantxmesax IN NUMBER,cantffvvx IN VALUE_ARRAY_NUMBER,cantitemx IN VALUE_ARRAY_NUMBER,cantxvendex IN VALUE_ARRAY_NUMBER,codcanalx IN VALUE_ARRAY_STRING,desccanalx IN VALUE_ARRAY_STRING,usuariox IN VARCHAR2,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2);
PROCEDURE APTOCREDITOCAMPANIA(TABLE_OBJ IN PEDIDO_PREVENTA_TABLE,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2);
FUNCTION CABECERAFOCUS(codempx IN VARCHAR2, codsedex IN VARCHAR2,codmesax IN VARCHAR2) RETURN TYPES.REF_CURSOR;

FUNCTION SUGERENCIAFOCUS(usuariox IN VARCHAR2) RETURN TYPES.REF_CURSOR;
--OFFLINE
FUNCTION RUTAS_BY_USUARIO(usuariox  IN VARCHAR)RETURN TYPES.REF_CURSOR ;
FUNCTION CLIENTES_OFFLINE(usernamex IN VARCHAR2) return TYPES.ref_cursor;
PROCEDURE ONLY_CLIENTES_OFFLINE(usernamex IN VARCHAR2,clientesnewx IN OUT types.ref_cursor,clientesoldx in OUT types.ref_cursor ) ;

PROCEDURE  DATOS_USUARIO_OFFLINE(usuariox IN VARCHAR2,cursordatosx IN OUT types.ref_cursor,cursorcondicionesx IN OUT types.ref_cursor);

PROCEDURE  ARTICULOS_OFFLINE(usernamex in varchar2,cursorarticulosx IN OUT types.ref_cursor,cursorsugeridox IN OUT types.ref_cursor,cursorfocusx in OUT types.ref_cursor);

PROCEDURE "REGISTRARPEDIDOMOVILOFFLINE" (TABLE_OBJ IN PEDIDO_PREVENTA_TABLE,montototalsugeridox IN NUMBER,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2) ;



FUNCTION GET_ARTICULOS_FOCUS_SUGERIDOS(codempresax in varchar2,codalmacenx in varchar2,codlistax in varchar2,condpagox in varchar2, codigosx VALUE_ARRAY_STRING)  return types.ref_cursor ;

FUNCTION BANDEJAINGRESOS(usuariox IN VARCHAR2) RETURN TYPES.REF_CURSOR;
FUNCTION BANDEJABONIFITEM(usuariox IN VARCHAR2) RETURN TYPES.REF_CURSOR;
--FUNCTION BANDEJABONIFXPAQUETE(usuariox IN VARCHAR2) RETURN TYPES.REF_CURSOR;


PROCEDURE  BANDEJABONIFXPAQUETE(usuariox in varchar2,cursorparticipacionx IN OUT types.ref_cursor,cursorbonifx IN OUT types.ref_cursor);
FUNCTION BANDEJAREPARTOVENTA(usuariox IN VARCHAR2) return types.ref_cursor ;
PROCEDURE  VENDEDORES_BY_SUPERVISOR(usuariox IN VARCHAR2,filtrox IN VARCHAR2,cursorvendedoresx IN OUT types.ref_cursor);


--CARGA DE PRECIOS
PROCEDURE LOADFORMPRECIOS(operacionx in number,paramstringx in LISTPARAMETR0STRING,cursorrptax IN OUT types.ref_cursor);

PROCEDURE "APTOTOPESTOCK" (TABLE_OBJ IN PEDIDO_PREVENTA_TABLE,rptax IN OUT NUMBER,mensajex IN OUT VARCHAR2) ;



PROCEDURE  VALIDAR_EDICION(usuariox IN VARCHAR2,estadox IN OUT NUMBER,mensajex IN OUT VARCHAR2);


END PREVENTA_PROD_2_1;




