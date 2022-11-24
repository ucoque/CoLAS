   -- sacamos los puntos de suministro del CT-pos
WITH lvs as(
       SELECT cod_meter, num_posicion
		FROM stg.stg_meter_cgp_ct
		WHERE cod_ct_stg = ", COD_CT,"
			AND num_posicion IN	", posicion, "
			AND TIP_METER = 'LVS'
   )
   , ps AS (
	SELECT COD_SERVICE_POINT AS cups
			, COD_METER, tip_meter
			, fec_baja_cn
			, nvl(FEC_INSTAL_EQUIPO ,FEC_ALTA_CN) FEC_INSTAL_EQUIPO
	FROM stg.stg_meter_cgp_ct
	WHERE nvl(COD_CT_SIC,COD_CT_STG) = ", COD_CT,"
		AND num_posicion IN ", posicion, "
		AND COD_SERVICE_POINT IS NOT NULL
		AND fec_baja_cn > TO_DATE('",toAux,"','dd/mm/yyyy')
		AND TIP_METER NOT IN ('SP','LVS')
)
-- Sacamos cuales son los contadores
, CNs AS (
	SELECT h.COD_SERVICE_POINT AS cups
			, h.COD_METER
			, NULL AS tip_meter
			, h.FEC_BAJA AS fec_baja_cn
			, NULL AS FEC_INSTAL_EQUIPO
	FROM stg.stg_meter_cgp_ct_his h
		JOIN ps p
			ON h.cod_service_point = cups
	WHERE fec_baja_cn > TO_DATE('",toAux,"','dd/mm/yyyy')
	UNION 
	SELECT * 
	FROM ps
)
-- la Curva de carga del supervisor
, LVS_LP AS (
	SELECT FH, CUPS, LVSLINE, AI, AE
		, AI-AE AS line_VAL_AI
	FROM stg.STG_SPV_S52 s52
		JOIN lvs lvs
			ON s52.LVSLINE = lvs.cod_meter
	WHERE FH >= TO_DATE('",fromAux,"','dd/mm/yyyy') -1/12
		AND FH < TO_DATE('",toAux,"','dd/mm/yyyy')
	)
-- sacamos por fecha la curva de los CN
, CN_LP AS (
	SELECT FH
		, cn.cups
		, meter_id
		, VAL_AI
		, VAL_AE
	FROM stg.STG_meter_read_lp mlp
		JOIN CNs cn
			ON mlp.meter_id = cn.cod_meter
	AND mlp.FH >= cn.fec_instal_equipo
	WHERE FH >= TO_DATE('",fromAux,"','dd/mm/yyyy') -1/12
		AND FH < TO_DATE('",toAux,"','dd/mm/yyyy')
)
--agrupamos por fecha
, gCN_LP AS (
	SELECT FH, SUM(VAL_AI-VAL_AE) AS CN_VAL_AI_AE
	FROM CN_LP
	GROUP BY fh
)
, gLVS_LP AS (
	SELECT FH, SUM(line_VAL_AI) AS line_VAL_AI
	FROM LVS_LP
	GROUP BY fh
)
/*
Juntamos gSPV_LP y gCN_LP y filtramos por la diferencia maxima aceptada para 
conseguir el listado de las fechas 'validas'.
*/
, FHs AS (
	SELECT s.FH
		, ROUND( (line_VAL_AI - CN_VAL_AI_AE) / (line_VAL_AI+0.001) ,3 ) AS loss
	FROM gCN_LP c
		JOIN gLVS_LP s
			ON c.FH = s.FH
	WHERE ",ifelse(allowNegDif ,"abs",""),"(line_VAL_AI - CN_VAL_AI_AE) / (line_VAL_AI+0.001) >= 0.005
		AND ",ifelse(allowNegDif ,"abs",""),"(line_VAL_AI - CN_VAL_AI_AE) / (line_VAL_AI+0.001) <= ", maxLoss, "
) 
-- calculamos las diferencias hora a hora de los SPVs
, LVS_lagged AS (
	SELECT s.FH, s.CUPS, s.LVSLINE 
		, line_VAL_AI - LAG(line_VAL_AI,1) OVER ( PARTITION BY LVSLINE ORDER BY s.FH ) AS dline_VAL_AI
		, EXTRACT(HOUR FROM f.FH - LAG(f.FH,1) OVER( PARTITION BY LVSLINE ORDER BY f.FH )) AS diffTime
	FROM LVS_LP s
		JOIN FHs f
			ON s.FH = f.FH
)
, CN_lagged AS (
	SELECT f.FH
		, cups
		, meter_id
		, (VAL_AI-VAL_AE) - LAG(VAL_AI-VAL_AE) OVER ( PARTITION BY cups ORDER BY cn.FH ) AS d_ai_ae
		, EXTRACT(HOUR FROM f.FH - LAG(f.FH,1) OVER ( PARTITION BY cups ORDER BY f.FH)) AS diffTime
	FROM CN_LP cn
		JOIN FHs f
			ON cn.FH = f.FH
)
SELECT FH, cups, meter_id, d_ai_ae
FROM CN_lagged
WHERE diffTime = 1
UNION
SELECT FH, 'lvs' as cups, LVSLINE AS meter_id, dline_VAL_AI AS d_ai_ae
FROM LVS_lagged
WHERE difftime = 1
