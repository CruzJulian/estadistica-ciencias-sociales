
# libs --------------------------------------------------------------------

library("readxl")
library("dplyr")
library("tidyr")
library("purrr")
library("stringr")
library("readr")

# functions ---------------------------------------------------------------

fn_row_to_qmd <- function(tb_cap){
  
  # browser()
  tb_cap |> 
    mutate(
      id = row_number(),
      citas = if_else(
        !is.na(citas), 
        map_chr(
          str_split(str_remove_all(citas, "\\s"), ","),
          \(s_cita) str_c(str_glue("[@{citas_sep}]", citas_sep = str_split(s_cita, ",")), collapse = ", ")),
        ""
      ),
      subsecciones = if_else(is.na(subsecciones), character(1), str_glue("### {subsecciones}\n\n")),
      seccion = if_else(is.na(seccion), character(1), str_glue("## {seccion}\n\n")),
      cont_text = case_when(
        tipo == "párrafo" ~ str_glue("{texto} {citas}\n\n"),
        tipo == "ítem" ~ str_glue(" - {texto}\n\n"),
        tipo == "fórmula" ~ str_glue("::: {{.callout-warning}}\n## Fórmula\n\n$${texto}$$\n\n:::\n\n"),
        tipo == "ejemplo" ~ str_glue("::: {{.callout-tip}}\n## Ejemplo\n\n{texto}\n\n:::\n\n"),
        # tipo == "Gráfico" & str_detect(texto, "\\s") != 0 ~ str_glue("\n\n```{{r, results='asis'}}\n\n {texto}\n\n```\n\n"),
        # tipo == "Gráfico" & str_detect(texto, "\\s") == 0 ~ str_glue("\n\n```{{r, results='asis', fig.cap = {texto}_caption}}\n\n {texto}\n\n```\n\n"),
        # tipo == "tabla" ~ str_glue("\n\n```{{r, results='asis'}}\n\n {texto}\n\n```\n\n"),
        .default = str_glue("{texto}\n\n")
      )
    ) |> # glimpse()
    group_by(seccion, subsecciones) |> 
    summarise(
      cont_text = str_c(cont_text, collapse = "\n\n"),
      id = mean(id)
    ) |> 
    arrange(id) |> 
    mutate(
      subseccion_text = str_glue("{subsecciones}\n\n{cont_text}\n\n")
    ) |>
    group_by(seccion) |> 
    summarise(
      seccion_text = str_c(subseccion_text, collapse = "\n\n"),
      id = mean(id)
    ) |> 
    arrange(id) |> 
    mutate(
      seccion_text = str_glue("{seccion}\n\n{seccion_text}\n\n")
    ) |>
    arrange(id) |> 
    pull(seccion_text) |> 
    str_c(collapse = "\n\n") -> s_cap_text
    
    
}


# lectura -----------------------------------------------------------------

read_excel("libro-estadistica-ciencias-sociales.xlsx", sheet = "contenido") -> tb_contenido

tb_contenido |> 
  select(modulos, capitulo, seccion) |> 
  distinct() -> tb_estructura



tb_contenido |> 
  filter(modulos %in% c("Preliminares", "Estadística inferencial", "Estadística univariada", "Estadística bivariada")) |> 
  mutate(id = row_number()) |> 
  group_by(modulos, capitulo) |> 
  mutate(id = mean(id)) |> 
  nest(caps = -c(id, modulos, capitulo)) |> 
  mutate(
    qmd_cap_body = map_chr(caps, fn_row_to_qmd),
    qmd_cap = str_glue("---\ntitle: \"{capitulo}\"\nlang: \"es-co\"\n---\n\n{qmd_cap_body}")
  ) -> tb_contenido_01


tb_contenido_01 |> 
  ungroup() |> 
  mutate(
    f_path = str_glue("caps/C_{str_pad(row_number(), 2, pad = 0)}.qmd"),
    escritura = walk2(qmd_cap, f_path, \(s_, f_path_) write_lines(s_, f_path_))
  ) 



