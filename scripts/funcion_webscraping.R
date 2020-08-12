get_compra_data <- function(url) {
  html <- read_html(url)
  
  # Proveedor final
  proveedor <- html %>%
    html_nodes('.FltLightBackground~ .FltLightBackground .VortalSpan') %>% 
    html_text() %>% 
    paste(collapse = '; ')
  
  # monto estimado
  monto_estimado <- html %>%
    html_nodes("#cbxBasePriceValue") %>% 
    html_text() %>% 
    parse_number()
  
  # monto_contratado
  monto_contratado <- html %>%
    html_nodes('.FltContentTdAwardDetail .VortalNumericSpan') %>% 
    html_text() %>% 
    `[`(1) %>% parse_number()
  
  # detalles articulos comprados

  detalle_compra <- html %>%
    html_nodes(".PriceListLine") %>% 
    #html_nodes("td") %>%
    html_nodes('.PriceListLineTable') %>%
    html_table(fill = TRUE) %>%
    map(
      ~slice(.x, 1) %>% 
        mutate(across(everything(), as.character))
      ) %>% 
    bind_rows()
  
  if(ncol(detalle_compra) == 9) {
    detalle_compra <- setNames(detalle_compra,
      c('referencia', 'codigo_unspsc', "codigo_unspsc2", 'cuenta_presupuestaria',
        'descripcion', 'cantidad', 'unidad', 'precio_unitario_estimado',
        'importe_estimado'))  %>% 
            mutate(across(c(referencia, cantidad, precio_unitario_estimado, importe_estimado), as.character),
                   across(c(referencia, cantidad, precio_unitario_estimado, importe_estimado), parse_number))
  } else {
    detalle_compra <- detalle_compra %>% 
      setNames(
        c('referencia', 'codigo_unspsc', "codigo_unspsc2", 'cuenta_presupuestaria',
          'descripcion','cantidad_requerida', 'cantidad', 'unidad', 'precio_unitario_estimado',
          'precio_unitario', 'importe_moneda_original', 'itbis_descuento', 'descuento_mo', 'monto_gravado', 'itbis_pct',
          'itbis_mo', 'otros_impuestos_pct', 'otros_impuestos_mo', 'importe_estimado')
      )  %>% 
      mutate(across(c(referencia, cantidad, precio_unitario_estimado, importe_estimado), as.character),
             across(c(referencia, cantidad, precio_unitario_estimado, importe_estimado), parse_number)) %>% 
      select(referencia, codigo_unspsc, codigo_unspsc2, cuenta_presupuestaria, descripcion,
             cantidad, unidad, precio_unitario_estimado, importe_estimado)
    
  }
  

  data <- tibble(
    proveedor,
    monto_estimado,
    monto_contratado,
    enlace_del_proceso = url
  )
  
  data <- bind_cols(data, detalle_compra) %>%
    mutate(cantidad_proveedores = str_count(proveedor, ';') + 1) %>% #nueva variable
    group_by(proveedor, monto_estimado, monto_contratado, enlace_del_proceso) %>% 
    nest()
  
  return(data)
}
