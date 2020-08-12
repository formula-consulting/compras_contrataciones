library(here)
library(tidyverse)
library(readxl)
library(rvest)

files <- list.files('data') %>% str_subset(".xlsx$")

compras_2020 <- read_excel(here('data', files[1])) %>% 
  janitor::clean_names() %>% 
  mutate(across(fecha_inicio_recepcion_oferta:fecha_estimada_adjudicacion, parse_number),
         across(fecha_inicio_recepcion_oferta:fecha_estimada_adjudicacion, janitor::excel_numeric_to_date))

compras_2019 <- read_excel(here('data', files[3])) %>% 
  janitor::clean_names() %>% 
  mutate(across(fecha_inicio_recepcion_oferta:fecha_estimada_adjudicacion, parse_number),
         across(fecha_inicio_recepcion_oferta:fecha_estimada_adjudicacion, janitor::excel_numeric_to_date))

compras_2018 <- read_excel(here('data', files[2])) %>% 
  janitor::clean_names() %>% 
  mutate(across(fecha_inicio_recepcion_oferta:fecha_estimada_adjudicacion, lubridate::ymd))


compras <- bind_rows(compras_2020, compras_2019) %>% 
  bind_rows(compras_2018)

compras_adjudicadas <- compras %>% 
  filter(estado_proceso == "Proceso adjudicado y celebrado") %>% 
  select(id_proceso, codigo_proceso, fecha_creacion, caratula, objeto_del_proceso,
         modalidad, unidad_compra, enlace_del_proceso)


# Webscraping compras y contrataciones ------------------------------------

# Función para extraer la data
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
        mutate(across(everything(), as.character()))
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

# Obras públicas ----------------------------------------------------------

# obras_publicas <- compras_adjudicadas %>% 
#   filter(unidad_compra == "Ministerio de Obras Públicas y Comunicaciones")
# 
# obras_publicas_detalle <- vector(length = nrow(obras_publicas), mode = "list")

# for (proceso in 1:length(obras_publicas_detalle)) {
#   obras_publicas_detalle[[proceso]] <- get_compra_data(obras_publicas$enlace_del_proceso[proceso])
#   
#   print(paste0("Iteración ", proceso,'; ', 
#                round(proceso/length(obras_publicas_complemento), 2) * 100, "%"))
# }
# 
# obras_publicas_detalle <- obras_publicas_detalle %>% 
#   bind_rows()
# 
# saveRDS(obras_publicas_detalle, here::here("data", "rds", "obras_publicas_complemento.RDS"))


# MEPyD -------------------------------------------------------------------

mepyd <- compras_adjudicadas %>% 
  filter(unidad_compra == "Ministerio de Economía, Planificación y Desarrollo")

mepyd_detalle <- vector(length = nrow(mepyd), mode = "list")

for (proceso in 1:length(mepyd_detalle)) {
  mepyd_detalle[[proceso]] <- get_compra_data(mepyd$enlace_del_proceso[proceso])
  
  print(paste0("Iteración ", proceso,'; ', 
               round(proceso/length(mepyd_detalle), 2) * 100, "%"))
}

mepyd_detalle <- mepyd_detalle %>% 
  bind_rows()

saveRDS(mepyd_detalle, here::here("data", "rds", "mepyd_complemento.RDS"))

# Senasa ------------------------------------------------------------------

senasa <- compras_adjudicadas %>% 
  filter(unidad_compra == "Seguro Nacional de Salud (SENASA)")

senasa_detalle <- vector(length = nrow(senasa), mode = "list")

for (proceso in 146:length(senasa_detalle)) {
  senasa_detalle[[proceso]] <- get_compra_data(senasa$enlace_del_proceso[proceso])
  
  print(paste0("Iteración ", proceso,'; ', 
               round(proceso/length(senasa_detalle), 2) * 100, "%"))
}

senasa_detalle <- senasa_detalle %>% 
  bind_rows()

saveRDS(senasa_detalle, here::here("data", "rds", "senasa_complemento.RDS"))


# ministerio_agricultura --------------------------------------------------

ministerio_agricultura <- compras_adjudicadas %>% 
  filter(unidad_compra == "MINISTERIO DE AGRICULTURA")

ministerio_agricultura_detalle <- vector(length = nrow(ministerio_agricultura), mode = "list")

for (proceso in 1:length(ministerio_agricultura_detalle)) {
  ministerio_agricultura_detalle[[proceso]] <- get_compra_data(ministerio_agricultura$enlace_del_proceso[proceso])
  
  print(paste0("Iteración ", proceso,'; ', 
               round(proceso/length(ministerio_agricultura_detalle), 2) * 100, "%"))
}

ministerio_agricultura_detalle <- ministerio_agricultura_detalle %>% 
  bind_rows()

saveRDS(ministerio_agricultura_detalle, here::here("data", "rds", "ministerio_agricultura_complemento.RDS"))


# interior_policia --------------------------------------------------------

interior_policia <- compras_adjudicadas %>% 
  filter(unidad_compra == "Ministerio de Interior y Policía")

interior_policia_detalle <- vector(length = nrow(interior_policia), mode = "list")

for (proceso in 1:length(interior_policia_detalle)) {
  interior_policia_detalle[[proceso]] <- get_compra_data(interior_policia$enlace_del_proceso[proceso])
  
  print(paste0("Iteración ", proceso,'; ', 
               round(proceso/length(interior_policia_detalle), 2) * 100, "%"))
}

interior_policia_detalle <- interior_policia_detalle %>% 
  bind_rows()

saveRDS(interior_policia_detalle, here::here("data", "rds", "interior_policia_complemento.RDS"))


# defensa -----------------------------------------------------------------


defensa <- compras_adjudicadas %>% 
  filter(unidad_compra == "Ministerio de Defensa")

defensa_detalle <- vector(length = nrow(defensa), mode = "list")

for (proceso in 1:length(defensa_detalle)) {
  defensa_detalle[[proceso]] <- get_compra_data(defensa$enlace_del_proceso[proceso])
  
  print(paste0("Iteración ", proceso,'; ', 
               round(proceso/length(defensa_detalle), 2) * 100, "%"))
}

defensa_detalle <- defensa_detalle %>% 
  bind_rows()

saveRDS(defensa_detalle, here::here("data", "rds", "defensa_complemento.RDS"))


# micm --------------------------------------------------------------------

micm <- compras_adjudicadas %>% 
  filter(unidad_compra == "Ministerio de Industria, Comercio y Mipymes")

micm_detalle <- vector(length = nrow(micm), mode = "list")

for (proceso in 1:length(micm_detalle)) {
  micm_detalle[[proceso]] <- get_compra_data(micm$enlace_del_proceso[proceso])
  
  print(paste0("Iteración ", proceso,'; ', 
               round(proceso/length(micm_detalle), 2) * 100, "%"))
}

micm_detalle <- micm_detalle %>% 
  bind_rows()

saveRDS(micm_detalle, here::here("data", "rds", "micm_complemento.RDS"))


# dgii --------------------------------------------------------------------


dgii <- compras_adjudicadas %>% 
  filter(unidad_compra == "Dirección General Impuestos Internos")

dgii_detalle <- vector(length = nrow(dgii), mode = "list")

for (proceso in 1:length(dgii_detalle)) {
  dgii_detalle[[proceso]] <- get_compra_data(dgii$enlace_del_proceso[proceso])
  
  print(paste0("Iteración ", proceso,'; ', 
               round(proceso/length(dgii_detalle), 2) * 100, "%"))
}

dgii_detalle <- dgii_detalle %>% 
  bind_rows()

saveRDS(dgii_detalle, here::here("data", "rds", "dgii_complemento.RDS"))






