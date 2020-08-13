library(tidyverse)
library(readxl)
library(lubridate)

bases_articulos <- list.files('data/articulos')

clasificador <- map(c('Class', 'Family'), 
    ~read_excel('data/unspsc.xlsx', sheet = .x) %>% 
      mutate(across(everything(), as.character))
    )

compras_adjudicadas <- readRDS('data/compras_adjudicadas.RDS')

compras_articulos <- map(
  bases_articulos,
  ~read_excel(
    here::here('data', 'articulos', .x)
  )
) %>% 
  bind_rows() %>% 
  janitor::clean_names()

compras_articulos <- compras_adjudicadas %>% 
  select(enlace_del_proceso, modalidad, objeto_del_proceso) %>% 
  left_join(compras_articulos, .)
 
compras_articulos <- compras_articulos %>% 
  mutate(
    year = year(fecha_creacion),
    mes = month(fecha_creacion),
    codigo_unspcs = as.character(codigo_unspcs),
    class = str_replace(codigo_unspcs, '..$', '00'),
    family = str_replace(codigo_unspcs, '....$', '0000')
    ) %>% 
  left_join(clasificador[[1]]) %>% 
  left_join(clasificador[[2]])

glimpse(compras_articulos)



resumen_institcion_detalle <- compras_articulos %>% 
  group_by(unidad_compra, descripcion) %>% 
  summarise(
    importe = sum(precio_unitario * cantidad)/1000000,
    cantidad = sum(cantidad),
    precio = mean(precio_unitario)
  )


# Análisis Yani -----------------------------------------------------------

instituciones_yani <- c('Dirección General Impuestos Internos', 'Ministerio de Obras Públicas y Comunicaciones', 
  'Ministerio de Industria, Comercio y Mipymes', 'Seguro Nacional de Salud (SENASA)', 
  'Ministerio de Interior y Policía', 'Ministerio de Defensa', 'Contraloría Gral de la República',
  'Ministerio de la Mujer', 'MINISTERIO DE AGRICULTURA', 'Plan Asistencia Social de la Presidencia')

yani_by_family <- compras_articulos %>%
  filter(unidad_compra %in% instituciones_yani) %>% 
  group_by(unidad_compra, family_desc) %>% 
  summarise(
    importe = sum(precio_unitario * cantidad)/1000000,
    cantidad = sum(cantidad),
    precio = mean(precio_unitario)
  ) 

yani_by_family %>% 
  slice_max(order_by = importe, n = 10) %>% 
  ungroup() %>% 
  mutate(family_desc = str_wrap(family_desc, 30)) %>% 
  filter(!is.na(family_desc)) %>% 
  ggplot(aes(x = importe, y = fct_reorder(family_desc, importe))) +
  geom_col() +
  facet_wrap(~unidad_compra, scales = "free")







compras_articulos %>% filter(descripcion == 'Goma de mascar') %>% 
  select(unidad_compra, precio_unitario, cantidad, descripcion, descripcion_por_usuario) %>% 
  mutate( importe = (precio_unitario * cantidad)/1000) %>% view()

compras_articulos %>% 
  filter(is.na(class_desc)) %>% 
  select(unidad_compra, descripcion, codigo_unspcs, class) %>% 
  view()



















