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


# Obras públicas ----------------------------------------------------------

obras_publicas <- compras_adjudicadas %>% 
  filter(unidad_compra == "Ministerio de Obras Públicas y Comunicaciones")

obras_publicas_detalle <- vector(length = nrow(obras_publicas), mode = "list")

for (proceso in 1:length(obras_publicas_detalle)) {
  obras_publicas_detalle[[proceso]] <- get_compra_data(obras_publicas$enlace_del_proceso[proceso])
  
  print(paste0("Iteración ", proceso,'; ', 
               round(proceso/length(obras_publicas_complemento), 2) * 100, "%"))
}

obras_publicas_detalle <- obras_publicas_detalle %>% 
  bind_rows()

saveRDS(obras_publicas_detalle, here::here("data", "rds", "obras_publicas_complemento.RDS"))


# MEPyD -------------------------------------------------------------------

mepyd <- compras_adjudicadas %>% 
 filter(unidad_compra == "Ministerio de Economía, Planificación y Desarrollo")

mepyd_detalle <- vector(length = nrow(mepyd), mode = "list")

for (proceso in 67:length(mepyd_detalle)) {
  mepyd_detalle[[proceso]] <- get_compra_data(mepyd$enlace_del_proceso[proceso])
  
  print(paste0("Iteración ", proceso,'; ', 
               round(proceso/length(mepyd_detalle), 2) * 100, "%"))
}

mepyd_detalle <- mepyd_detalle %>% 
  bind_rows()

saveRDS(mepyd_detalle, here::here("data", "rds", "mepyd_complemento.RDS"))


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
        set_names(c('referencia', 'codigo_unspsc', "codigo_unspsc2", 'cuenta_presupuestaria',
                    'descripcion', #'descripcion2', 
                    'cantidad', 'unidad', 'precio_unitario_estimado',
                    'importe_estimado')) %>% 
        mutate(across(c(referencia, cantidad, precio_unitario_estimado, importe_estimado), as.character),
               across(c(referencia, cantidad, precio_unitario_estimado, importe_estimado), parse_number))
    ) %>% 
    bind_rows()
  
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






# library(rvest)
# url <- "https://comunidad.comprasdominicana.gob.do//Public/Tendering/OpportunityDetail/Index?noticeUID=DO1.NTC.700904"
# 
# #read the page
# page <- read_html(url) 
# 
# #collect the business cards
# businesscards <- page %>% html_nodes("table.VortalGrid[id=grdBiddersList_tbl]") %>% 
#   html_table(fill=TRUE)
# 
# supplier <- businesscards[[1]]$Supplier
# 
# #Extract the URL from the lines
# supplier <- gsub(".+\\(\'(.*)\', .+", "\\1", supplier)
# #remove blank URL
# supplier <- supplier[nchar(supplier) >10 & !is.na(supplier)]
# 
# # attempt to read the suppliers
# bc1 <-read_html(supplier[2])
# bc1 %>% html_node("table") %>% html_table()
#most additional information is stored as attributes


# # Proveedor
# read_html(enlace1) %>% 
#   html_nodes('.FltLightBackground~ .FltLightBackground .VortalSpan') %>% 
#   html_text() %>% 
#   paste(collapse = '; ')
# 
# # detalles articulos comprados
# read_html(enlace1) %>%
#   html_nodes(".PriceListLine") %>% 
#   #html_nodes("td") %>%
#   html_nodes('.PriceListLineTable') %>%
#   html_table(fill = TRUE) %>%
#   map(
#     ~slice(.x, 1) %>% 
#       set_names(c('referencia', 'codigo_unspsc', 'cuenta_presupuestaria',
#                   'descripcion', 'descripcion2', 'cantidad', 'unidad', 'precio_unitario_estimado',
#                   'importe_estimado'))
#     ) %>% 
#   bind_rows() 
# 
# # monto_contratado
# read_html(enlace1) %>%
#   html_nodes('.FltContentTdAwardDetail .VortalNumericSpan') %>% 
#   html_text() %>% 
#   `[`(1) %>% parse_number()
# 
# # monto estimado
# read_html(enlace1) %>%
#   html_nodes("#cbxBasePriceValue") %>% 
#   html_text() %>% 
#   parse_number()
# 
# 
# 
# 
# 
# 
# 
