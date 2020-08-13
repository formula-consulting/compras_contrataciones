library(tidyverse)

bases <- list.files(here::here('data', 'rds'))
compras_adjudicadas <- readRDS('data/compras_adjudicadas.RDS')

compras_detalladas <- map(
  bases,
  ~readRDS(here::here('data', 'rds', .x))
) %>% 
  setNames(str_remove(bases, '_complemento.rds')) %>% 
  bind_rows() %>%
  right_join(compras_adjudicadas, ., by = 'enlace_del_proceso') %>% 
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
  rename(institucion = unidad_compra)

compras_detalladas %>%
  filter(str_detect(institucion, 'SENASA', negate = TRUE)) %>% 
  group_by(institucion,  categoria_gasto) %>% 
  summarise(
    importe = sum(importe_estimado)/1000000,
    precio_unitario = mean(precio_unitario_estimado),
    cantidad = importe/precio_unitario
  ) %>% view()

  slice_max(order_by = importe, n = 10) %>% 
  ungroup() %>% 
  mutate(categoria_gasto = str_wrap(categoria_gasto, 30)) %>% 
  ggplot(aes(x = importe, y = fct_reorder(categoria_gasto, importe))) +
  geom_col() +
  facet_wrap(~institucion, scales = 'free')

compras_detalladas %>% 
  group_by(institucion,  proveedor) %>% 
  summarise(
    importe = sum(importe_estimado)/1000000,
    precio_unitario = mean(precio_unitario_estimado),
    cantidad = importe/precio_unitario
  )

