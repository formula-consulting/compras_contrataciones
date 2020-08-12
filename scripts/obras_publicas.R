obras_publicas_detalle <- readRDS("data/rds/obras_publicas_complemento.RDS")

obras_publicas_detalle <- obras_publicas_detalle %>%
  ungroup() %>% 
  left_join(obras_publicas, ., by = 'enlace_del_proceso') %>% 
  unnest(data) %>% 
  select(-codigo_unspsc2) %>% 
  mutate(
    codigo = str_extract(codigo_unspsc, "^[0-9]+"),
    categoria_gasto = ifelse(
      str_count(codigo_unspsc, "-") > 1,
      str_match(codigo_unspsc, "^([0-9]+) - .+ - (.+)$") %>% `[`(,3),
      str_remove(codigo_unspsc, "^([0-9]+) - ")
    )
  ) %>% 
  select(
    id_proceso, codigo_proceso, fecha_creacion, caratula, objeto_del_proceso,
    modalidad, unidad_compra,
    referencia, proveedor, cuenta_presupuestaria, codigo, categoria_gasto, 
    descripcion, cantidad, unidad, monto_estimado, monto_contratado, 
    precio_unitario_estimado, importe_estimado, cantidad_proveedores, enlace_del_proceso)

xlsx::write.xlsx(obras_publicas_detalle, "data/excel/obras_publicas.xlsx", showNA = FALSE)  

# 
# obras_publicas_detalle %>%
#   group_by(categoria_gasto) %>% 
#   summarise(
#     monto_gastado = sum(importe_estimado),
#     cantidad_comprada = sum(cantidad),
#     cantiad_compras = n()
#   ) %>% 
#   arrange(desc(monto_gastado)) %>% 
#   view()
# 
# obras_publicas_detalle %>% 
#   filter(is.na(categoria_gasto)) %>% 
#   view()
# 
# obras_publicas_detalle %>% 
#   filter(categoria_gasto == "Combustible diesel", cantidad > 1) %>% 
#   select(fecha_creacion, cantidad, proveedor, precio_unitario_estimado, importe_estimado, unidad) %>% 
#   mutate(year = lubridate::year(fecha_creacion)) %>% 
#   group_by(year) %>% 
#   summarise(
#     importe = sum(importe_estimado),
#     precio = mean(precio_unitario_estimado),
#     cantidad_galones = sum(cantidad)
#   )