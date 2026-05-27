test_that("venny usage example", {
    # if (FALSE) {
    #     # Quick start
    #     lst <- LGL23$DEGs
    #     venny(lst)
    #     ggsave("example_00_venn.jpg", path = "./man/figures")
    # 
    #     # Usage example
    #     ## Prerequisites
    #     if (!require(venny)) install.packages("venny")
    #     if (!require(tidyverse)) install.packages("tidyverse")
    #     if (!require(ggtext)) install.packages("ggtext")
    #     if (!require(BiocManager)) install.packages("BiocManager")
    #     if (!require(clusterProfiler)) BiocManager::install("clusterProfiler")
    #     if (!require(org.At.tair.db)) BiocManager::install("org.At.tair.db")
    # 
    #     library(venny)
    #     library(tidyverse)
    #     library(ggtext)
    #     library(clusterProfiler)
    #     library(org.At.tair.db)
    # 
    #     intersect <- venny::intersect
    #     setdiff <- venny::setdiff
    #     union <- venny::union
    # 
    #     ## Background information
    # 
    #     lst <- LGL23$DEGs
    #     subset_names <- names(subset_label_default(length(lst)))
    #     print(names(lst))
    #     print(subset_names)
    # 
    # 
    #     ## Loss-of-function (Set C)
    # 
    #     select_subsets <- c("C", "BC", "CD", "ABC", "BCD", "AC", "ABCD", "ACD")
    #     font_color <- sapply(subset_names, \(x) if (x %in% select_subsets) "black" else "white")
    # 
    #     out <- venny(
    #         data = lst,
    #         detail = TRUE,
    #         set.label.position = set_label_position(hjust = c(0.2, -0.5, 0.5, -0.2)),
    #         subset.label.font = subset_label_font(color = font_color),
    #         subset.count.font = subset_count_font(color = font_color),
    #         subset.percentage.font = subset_percentage_font(color = font_color)
    #     )
    # 
    #     venn <- out$venn
    #     setops <- out$ellipse_path$`WT_mock vs KO_mock`
    # 
    #     highlight(venn, setops, linetype = "solid", color = "black") +
    #         coord_cartesian(xlim = c(-3, 5)) +
    #         annotate("richtext",
    #             x = 3.2, y = -1.6,
    #             hjust = 0,
    #             size = 5,
    #             label = paste(
    #                 "<b>KO:</b> Knock-out",
    #                 "<b>WT:</b> Wild-type",
    #                 "<b>OE:</b> Overexpression",
    #                 "<b>mock:</b> 0 nM treatment",
    #                 "<b>low:</b> 1 nM treatment",
    #                 "<b>high:</b> 5 nM treatment",
    #                 sep = "<br>"
    #             ),
    #         )
    #     ggsave("example_01_venn.jpg", path = "./man/figures")
    # 
    #     GO <- clusterProfiler::enrichGO(
    #         gene = LGL23$DEGs$`WT_mock vs KO_mock`,
    #         OrgDb = org.At.tair.db,
    #         keyType = "TAIR",
    #         ont = "BP"
    #     )
    # 
    #     GO@result |>
    #         slice_max(RichFactor, n = 20) |>
    #         ggplot(aes(RichFactor, fct_reorder(Description, RichFactor))) +
    #         theme_bw() +
    #         geom_point(aes(size = Count, color = FoldEnrichment)) +
    #         theme(axis.title.y = element_blank())
    #     ggsave("example_01_GO.jpg", path = "./man/figures")
    # 
    # 
    #     ## High-dosage recovery (Subset BCD)
    # 
    #     select_subsets <- "BCD"
    #     font_color <- sapply(subset_names, \(x) if (x %in% select_subsets) "black" else "white")
    # 
    #     BCD <- venny(
    #         data = lst,
    #         detail = TRUE,
    #         set.label.position = set_label_position(hjust = c(0.2, -0.5, 0.5, -0.2)),
    #         subset.label = list(CD = "", ABCD = ""),
    #         subset.label.font = subset_label_font(color = font_color),
    #         subset.count.position = subset_count_position(hide = c("CD", "ABCD")),
    #         subset.count.font = subset_count_font(color = font_color),
    #         subset.percentage.position = subset_percentage_position(hide = c("CD", "ABCD")),
    #         subset.percentage.font = subset_percentage_font(color = font_color)
    #     )
    # 
    #     venn <- BCD$venn
    #     ep <- BCD$ellipse_path
    #     setops <- ep$`KO_high vs KO_mock` |>
    #         intersect(ep$`WT_mock vs KO_mock`, ep$`OE_mock vs KO_mock`) |>
    #         setdiff(ep$`KO_low vs KO_mock`)
    # 
    #     highlight(venn, setops, linetype = "solid", color = "black")
    #     ggsave("example_02_venn.jpg", path = "./man/figures")
    # 
    #     GO <- clusterProfiler::enrichGO(
    #         gene = BCD$subset_elements$BCD,
    #         OrgDb = org.At.tair.db,
    #         keyType = "TAIR",
    #         ont = "BP"
    #     )
    # 
    #     GO@result |>
    #         slice_max(RichFactor, n = 20) |>
    #         ggplot(aes(RichFactor, fct_reorder(Description, RichFactor))) +
    #         theme_bw() +
    #         geom_point(aes(size = Count, color = FoldEnrichment)) +
    #         theme(axis.title.y = element_blank())
    #     ggsave("example_02_GO.jpg", path = "./man/figures")
    # 
    # }
    expect_equal(TRUE, TRUE)
})
