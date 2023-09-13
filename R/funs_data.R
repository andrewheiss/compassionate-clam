library(glue)
library(rvest)
suppressPackageStartupMessages(library(lubridate))

load_clean_chinafile <- function() {
  chinafile <- read_html(glue("https://web.archive.org/web/20220922181643/",
                              "https://jessicachinafile.github.io/index_RO_table.html")) |> 
    html_element(css = "#filter") |>
    html_table() |>
    select(org_name_cn = `Organization Name (Chinese)`,
           org_name_en = `Organization Name (English)`) |>
    group_by(org_name_cn) |>
    slice(1)
  
  return(chinafile)
}

province_cn_to_en <- function() {
  province_name <- tribble(
    ~province_cn, ~province_en,      ~province_code,
    "北京",      "Beijing",        "BJ",
    "上海",      "Shanghai",       "SH",
    "天津",      "Tianjin",        "TJ",
    "重庆",      "Chongqing",      "CQ",
    "河北",      "Hebei",          "HE",
    "山西",      "Beijing",        "SX",
    "内蒙",      "Inner Mongolia", "NM",
    "辽宁",      "Liaoning",       "LN",
    "吉林",      "Jilin",          "JL",
    "黑龙",      "Heilongjiang",   "HL",
    "江苏",      "Jiangsu",        "JS",
    "浙江",      "Zhejiang",       "ZJ",
    "安徽",      "Anhui",          "AH",
    "福建",      "Fujian",         "FJ",
    "江西",      "Jiangxi",        "JX",
    "山东",      "Shandong",       "SD",
    "河南",      "Henan",          "HA",
    "湖北",      "Hubei",          "HB",
    "湖南",      "Hunan",          "HN",
    "广东",      "Guangdong",      "GD",
    "广西",      "Guangxi",        "GX",
    "海南",      "Hainan",         "HI",
    "四川",      "Sichuan",        "SC",
    "贵州",      "Guizhou",        "GZ",
    "云南",      "Yunnan",         "YN",
    "西藏",      "Tibet",          "XZ",
    "陕西",      "Shaanxi",        "SN",
    "甘肃",      "Gansu",          "GS",
    "青海",      "Qinghai",        "QH",
    "宁夏",      "Ningxia",        "NX",
    "新疆",      "Xinjiang",       "XJ",
    "兵团",      "Bingtuan",       "BT"
  )
  
  return(province_name)
}


clean_map_data <- function(ongo) {
  suppressPackageStartupMessages(library(sf))
  suppressPackageStartupMessages(library(mapchina))
  
  # Clean up the geographic data
  suppressMessages({
    sf_use_s2(FALSE)  # https://github.com/xmc811/mapchina/issues/7#issuecomment-1028792066
    
    sf_china <- china |>
      group_by(Name_Province) |>
      summarise(geometry = st_union(geometry)) |> 
      mutate(province_cn = str_sub(Name_Province, end = 2))
  })
  
  # Get a count of ONGOs per province
  province_count <- ongo |>
    group_by(province_cn) |>
    summarise(ro_count = n())
  
  # Join ONGO count to map
  mapdata <- sf_china |>  
    left_join(province_count, by = "province_cn") |> 
    mutate(ro_count = ifelse(is.na(ro_count), 0, ro_count))
  
  return(mapdata)
}


clean_ongo_data <- function(manual, chinafile, province_name) {
  ongo <- read_csv(manual, show_col_types = FALSE) |>
    mutate(province_cn = str_sub(address, end = 2)) |>   # Province name from address
    left_join(province_name, by = "province_cn") |>  # Add English province name
    mutate(registration_date = ymd(registration_date), 
           still_active = ifelse(is.na(still_active), TRUE, still_active),
           geo_scope_num = str_count(geo_scope, ",") + 1) |>
    mutate(geo_scope_num = ifelse(geo_scope == "中国境内", # recode "all over China"
                                  max(geo_scope_num, na.rm = TRUE), geo_scope_num)) |>
    # reformat ro_name to merge with Chinafile data
    mutate(org_name_cn = ifelse(province_cn == "内蒙", str_sub(ro_name, end = -7), str_sub(ro_name, end = -6))) |>
    mutate(org_name_cn = ifelse(home == "South Korea", org_name_cn, str_remove(org_name_cn, "\\（.*\\）"))) |>
    left_join(chinafile, by = "org_name_cn") |> 
    # Derive local connection variable
    mutate(cn_background = case_when(work_field_code2 == "Chinese background" ~ 1, 
                                     TRUE ~ 0),
           local_aim = ifelse(!is.na(local_aim), 1, 0),
           local_org_name = ifelse(!is.na(local_org_name), 1, 0),
           local_connect = ifelse(cn_background + local_aim + local_org_name == 0, FALSE, TRUE)) |> 
    # Create different versions of the outcome variable
    mutate(province_count = geo_scope_num,
           province_pct = province_count / 32,
           province_cat = case_when(
             province_count == 1 ~ "Single province",
             province_count > 1 & province_count < 32 ~ "Multiple provinces",
             province_count == 32 ~ "All provinces"
           )) |> 
    mutate(province_cat = factor(province_cat, 
                                 levels = c("Single province", "Multiple provinces", "All provinces"),
                                 ordered = TRUE)) |> 
    mutate(time_since_law_passed = interval(ymd("2017-01-01"), registration_date),
           days_since_law = time_since_law_passed / days(1),
           months_since_law = time_since_law_passed / months(1),
           years_since_law = time_since_law_passed / years(1)) |> 
    mutate(year_registered = year(registration_date))
  
  return(ongo)
}

