library(stringr)
library(SnowballC)

# 定义增强版标准化函数
standardize_keyword <- function(kw) {
  # 阶段1：符号统一
  kw <- gsub("[-_/]", " ", kw)          # 替换分隔符为空格
  kw <- gsub("([a-z])([A-Z])", "\\1 \\2", kw) # 拆分驼峰式
  
  # 阶段2：格式标准化
  kw <- tolower(kw)
  kw <- gsub("\\s{2,}", " ", kw)        # 合并连续空格
  kw <- trimws(kw)
  
  # 阶段3：词形还原（增强版）
  kw <- wordStem(kw, language = "en")   # 基础词干提取
  
  # 自定义不规则复数处理
  irregular_map <- c(
    mice = "mouse",
    feet = "foot",
    geese = "goose",
    teeth = "tooth",
    children = "child",
    people = "person"
  )
  
  if (kw %in% names(irregular_map)) {
    return(irregular_map[[kw]])
  }
  return(kw)
}

# 主处理函数
process_wos_file <- function(input_path, output_path, synonym_path) {
  # 读取文件
  content <- readLines(input_path, encoding = "UTF-8")
  
  # 分割记录（增强容错性）
  records <- unlist(strsplit(paste(content, collapse="\n"), "\nER\\s*\n"))
  
  # 提取关键词函数
  extract_keywords <- function(record) {
    fields <- c("DE", "ID")
    unlist(lapply(fields, function(f) {
      matches <- str_match_all(record, paste0("\n", f, "\\s+(.*?)(?=\\n\\w+|$)"))[[1]][,2]
      kws <- unlist(strsplit(matches, ";\\s*"))
      trimws(kws[nzchar(kws)])
    }))
  }
  
  # 收集所有关键词
  all_keywords <- unique(unlist(lapply(records, extract_keywords)))
  
  # 构建同义词映射表
  keyword_map <- list()
  synonym_list <- list()
  
  # 首阶段：建立标准形式
  for (kw in all_keywords) {
    std_kw <- standardize_keyword(kw)
    if (!std_kw %in% names(keyword_map)) {
      keyword_map[[std_kw]] <- kw
      synonym_list[[std_kw]] <- kw
    } else {
      synonym_list[[std_kw]] <- c(synonym_list[[std_kw]], kw)
    }
  }
  
  # 生成替换对照表
  replacement_df <- do.call(rbind, lapply(names(synonym_list), function(std) {
    variants <- setdiff(synonym_list[[std]], keyword_map[[std]])
    data.frame(
      Standard_Form = keyword_map[[std]],
      Synonyms = ifelse(length(variants) > 0, paste(variants, collapse = "|"), NA)
    )
  }))
  write.csv(replacement_df[!is.na(replacement_df$Synonyms), ], synonym_path, row.names = FALSE)
  
  # 替换处理函数
  replace_keywords <- function(record) {
    process_field <- function(field) {
      str_replace_all(record, 
                      paste0("(?<=\\n", field, "\\s{2})(.*?)(?=\\n\\w+|$)"),
                      function(m) {
                        kws <- strsplit(m, ";\\s*")[[1]]
                        replaced <- sapply(kws, function(kw) {
                          std_kw <- standardize_keyword(kw)
                          ifelse(std_kw %in% names(keyword_map), keyword_map[[std_kw]], kw)
                        })
                        paste(replaced, collapse = "; ")
                      })
    }
    process_field("DE")
    process_field("ID")
    return(record)
  }
  
  # 处理所有记录
  processed <- sapply(records, replace_keywords)
  
  # 输出文件
  writeLines(paste(processed, collapse = "\nER\n"), output_path, useBytes = TRUE)
}

# 使用示例
process_wos_file(
  input_path = "4.txt",
  output_path = "4_output.txt",
  synonym_path = "4_mapping.csv"
)