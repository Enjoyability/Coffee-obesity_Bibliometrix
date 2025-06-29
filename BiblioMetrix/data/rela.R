# ================================================================
# 交互式主题相关性分析脚本
# ================================================================
# 安装依赖（首次运行取消注释）：
# install.packages(c("stringr","dplyr","tidyr","pheatmap","purrr"))

library(stringr); library(dplyr); library(tidyr); library(pheatmap); library(purrr)

# ---------- 0. 基本设置 ----------
file_path <- "processed_output.txt"   # <<< 如需修改请手动改路径

# ---------- 1. 用户输入：K ----------
k_input <- readline(prompt =
                      "请输入要提取的高频关键词个数 K （默认 10）： ")
K <- ifelse(k_input == "" | !grepl("^\\d+$", k_input), 10, as.integer(k_input))
cat(">> 将提取出现次数排名前", K, "的关键词\n")

# ---------- 2. 读取并切分记录 ----------
txt <- readLines(file_path, encoding = "UTF-8")
records <- str_split_1(paste(txt, collapse = "\n"), "\nER\\s*\n")

# ---------- 3. 提取关键词 ----------
extract_kw <- function(rec){
  segs <- str_match_all(
    rec, "\\n(?:DE|ID)\\s+(.*?)(?=\\n[A-Z]{2}\\s+|$)"
  )[[1]][,2]
  if (length(segs) == 0) return(character(0))
  segs %>% str_replace_all("\\n", " ") %>% str_split(";") %>%
    unlist() %>% str_trim() %>% tolower() %>% discard(~.x=="")
}
kw_lists <- lapply(records, extract_kw)

# ---------- 4. 高频关键词统计 ----------
top_kw <- kw_lists %>% unlist() %>% table() %>%
  sort(decreasing = TRUE) %>% head(K)
top_vec <- names(top_kw)

cat("\n【Step 1】当前高频关键词（按出现次数降序）\n")
print(top_kw)

# ---------- 5. 允许用户剔除部分关键词 ----------
rem <- readline(
  "\n如需移除某些关键词，请输入要剔除的词（用逗号分隔）。直接回车则不移除： ")
if (trimws(rem) != ""){
  to_remove <- str_split(rem, ",")[[1]] %>% str_trim() %>% tolower()
  top_vec   <- setdiff(top_vec, to_remove)
  cat(">> 已移除：", paste(to_remove, collapse = ", "), "\n")
}
if (length(top_vec) < 2){
  stop("剩余关键词不足以做后续分析。")
}

cat("\n保留的关键词：\n", paste(top_vec, collapse = ", "), "\n")

# ---------- 6. 用户主题划分 ----------
cat("\n【Step 2】为以上关键词划分主题。\n",
    "操作说明：\n",
    "  1) 先输入主题数 N。\n",
    "  2) 稍后依次输入每个主题包含的关键词（用逗号分隔）。\n",
    "     每个关键词必须来自保留列表，且不能重复归类。\n", sep="")

# 6.1 输入主题数
n_input <- readline("请输入主题数 N： ")
while(!grepl("^\\d+$", n_input) || as.integer(n_input) < 1){
  n_input <- readline("请重新输入正整数 N： ")
}
N <- as.integer(n_input)

# 6.2 逐主题输入关键词
themes <- vector("list", N)
assigned <- character(0)
for (i in seq_len(N)){
  repeat{
    msg <- paste0("主题", i, " 关键词（逗号分隔）： ")
    kws <- readline(msg) %>% str_split(",") %>% .[[1]] %>%
      str_trim() %>% tolower() %>% discard(~.x=="")
    if (all(kws %in% top_vec) && length(intersect(kws, assigned))==0 && length(kws)>0){
      themes[[i]] <- kws
      assigned <- c(assigned, kws)
      break
    } else {
      cat(">>> 输入有误：必须来源于保留列表且不重复，请重新输入。\n")
    }
  }
}
names(themes) <- paste0("Theme", seq_len(N))
cat("\n主题划分结果：\n")
print(themes)

# ---------- 7. 构造 文献 × 主题 0/1 矩阵 ----------
doc_theme <- matrix(0L, nrow = length(kw_lists), ncol = N,
                    dimnames = list(NULL, names(themes)))

for (d in seq_along(kw_lists)){
  for (t in seq_len(N)){
    if (length(intersect(kw_lists[[d]], themes[[t]])) > 0)
      doc_theme[d, t] <- 1L
  }
}
doc_theme <- as.data.frame(doc_theme)

# ---------- 8. 计算主题相似度（Jaccard，范围 0~1） ----------
jaccard <- function(x, y){
  a <- sum(x & y)
  b <- sum(x | y)
  if (b == 0) return(0)
  a / b
}
sim_mat <- outer(1:N, 1:N,
                 Vectorize(function(i,j) jaccard(doc_theme[[i]], doc_theme[[j]])))
dimnames(sim_mat) <- list(names(themes), names(themes))

# ---------- 9. 保存 & 可视化 ----------
write.csv(round(sim_mat, 3), "theme_similarity.csv", row.names = TRUE)
cat("\n>>> 已保存主题相似度矩阵：theme_similarity.csv\n")

pheatmap(sim_mat,
         filename = "theme_similarity.png",
         main      = "Theme Similarity (Jaccard)",
         display_numbers = TRUE,
         number_format   = "%.2f",
         border_color    = NA)
cat(">>> 已生成热图：theme_similarity.png\n")
