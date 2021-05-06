#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions 
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script defines custom functions
#-------- script defines custom functions
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#general========================================================================
#reads all csvs using data.table::fread()
#removes zero variance columns
#and cleans names
read_csv_allFiles <- function(file_list, extra_path) {
  data_list =
    file_list %>%
    paste0(here(), extra_path, .) %>%
    map(~data.table::fread(.x) %>%
          na_if("NULL") %>%
          janitor::remove_empty("cols") %>%
          janitor::clean_names()
    )
  names(data_list) = file_list
  data_list
}

#helper to make floor divides
#generally used to make bins
floor_divide = function(value, floor){
  (value %/% floor)*floor
}

#plusEqual operator
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

#evaluate string functions
#eg works if var is "input$something > num" in shiny
strg_fun_eval = function(text){
  eval(rlang::parse_expr(text))  
}

#takes quick counts for columns and their items
quick_value_count = function(df, rows, column, filter = NA){
  if(is.na(filter)){
  df[rows,] %>% 
    select(all_of(column)) %>% 
    nrow()
  } else {
    df[rows,] %>% 
      select(all_of(column)) %>% 
      filter(!!as.symbol(column) == filter) %>% 
      nrow()
  }
}

#cleans df using common operations
quick_clean = function(df, na_marker){
  df %>%  
    na_if(na_marker) %>% 
    janitor::clean_names() %>%  
    janitor::remove_empty(c("cols", "rows"))
}

pretty_char = function(col){
  col %>% 
    stringr::str_replace_all(., "_", " ") %>% 
    stringr::str_to_title(.)
}

dgt2 = function(x){
  round(x, 2)
}

#corrects column to start with zero 
crrct0 = function(x){
  x-x[1]
}

#function: changes negative to zero
lmt0 = function(x){
  ifelse(x<0, 0, x)
}



#shiny specific=================================================================
list = list(closable = F, 
            collapsed = F, 
            collapsible = T, 
            width = "100%", 
            solidHeader = T, 
            status = "primary")

quick_bs = function(id, title, text, trigger = "hover"){
  tagList(
    bsButton(inputId = id, label = "Info", icon = icon("question"), style = "info", size = "small"),
    bsPopover(id = id, title = title,
              content = text,
              placement = "right", 
              trigger = trigger)
  )
}


#takes master shiny input list and extracts list elements by name match 
#string can take "|" operator 
get_list_items = function(input_list, suffix = NA, string, purrr = T){
  if (purrr){
    input_list[names(input_list)[(str_detect(names(input_list) , paste0("_", suffix))+str_detect(names(input_list), string))==2]]
  } else {
    input_list[names(input_list)[str_detect(names(input_list), string)]]
  }
}

#makes common box that works for most purposes 
#objects need to be in a list
boxPlus_common = function(title = NA, object_in_box = NA, collapsed = F){
  boxPlus(title = title, 
          closable = F, 
          collapsed = collapsed, 
          collapsible = T, 
          width = "100%", 
          solidHeader = T, 
          status = "primary", 
          object_in_box)
}

#creates an empty row of a given height
#for shiny usage
spacer_row = function(size){
  fluidRow(box(height = size))
}

#creates function for modals 
modal = function(trigger, msg){
  observeEvent(trigger, {
    showModal(modalDialog(
      msg,
      size = "l",
      easyClose = TRUE
    ))
  })
}

#DT specific====================================================================
#creats DT readable icons 
icon_quick = function(name){
  as.character(icon(name))
}

#creates simple onclick columns
onclick_quick = function(column){
  str_glue('<a href="#" onclick="alert(\'{`column`}\');">Click for Description</a>')
}

# checkbox_quick = function(name){
#   str_glue('<input type="checkbox" name="name" value="{1:nrow(.)}"><br>')
# }


#dt style
dt_font_opts = DT::JS(
  "function(settings, json) {",
  "$('body').css({'font-family': 'Calibri'});",
  "}"
)

#gets values for checkbox items in DT
checkbox_grab = function(column){
  tags$script(HTML(paste0('$(document).on("click", "input", function () {
                       var checkboxes = document.getElementsByName("', column, '");
                       var checkboxesChecked = [];
                       for (var i=0; i<checkboxes.length; i++) {
                       if (checkboxes[i].checked) {
                       checkboxesChecked.push(checkboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("', column, '",checkboxesChecked);  })')))
}

#makes column names human readable
pretty_col_names = function(df){
  df %>% 
    rename_all(.funs = list(~stringr::str_replace_all(., "_", " ") %>% 
                              stringr::str_to_title(.))) 
}

#makes standard DT with a few inputs
dt_common = function(df, y = NA, pl = 10, dom = "Blftipr", sel = "none", but = c("copy", "csv"), edit = NA){
  #https://datatables.net/reference/option/dom
  
  df %>% 
    pretty_col_names() %>% 
    DT::datatable(escape = F,
                  rownames = F,
                  fillContainer = T, 
                  selection = sel,
                  editable = edit, 
                  extensions = c('Buttons'),
                  options = list(
                    pageLength = pl,
                    scrollY = y,
                    dom = dom,
                    buttons = but,
                    initComplete = dt_font_opts
                  ))
}


#end
