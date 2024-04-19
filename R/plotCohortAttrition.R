attritionChart <- function(x) {
  formatNum <- function(col) {
    if_else(
      !is.na(as.numeric(col)),
      gsub(" ", "", format(as.integer(col), big.mark=",")),
      col
    )
  }

  xn <- x %>%
    arrange(reason_id) %>%
    mutate(
      number_subjects = formatNum(number_subjects),
      number_records = formatNum(number_records),
      excluded_subjects = formatNum(excluded_subjects),
      excluded_records = formatNum(excluded_records),
      label = paste0(
        "N subjects = ", number_subjects, "\nN records = ", number_records
      )
    )
  if (nrow(xn) == 1) {
    xn <- xn %>%
      mutate(label = paste0("Qualifying events", "\n", label)) %>%
      select(label)
  } else {
    att <- xn %>%
      filter(reason_id > min(reason_id)) %>%
      mutate(
        label = paste0(
          "N subjects = ", excluded_subjects, "\nN records = ", excluded_records
        )
      ) %>%
      select(reason, label)
    xn <- xn %>%
      mutate(
        label = if_else(
          reason_id == min(reason_id),
          paste0("Initial events", "\n", label),
          if_else(
            reason_id == max(reason_id),
            paste0("Final events", "\n", label),
            label
          )
        )
      ) %>%
      select(label)
  }
  n <- nrow(x)
  xg <- create_graph()

  for (k in seq_len(n)) {
    xg <- xg %>%
      add_node(
        label = xn$label[k],
        node_aes = node_aes(
          shape = "box",
          x = 1,
          width = 1.4,
          y = n + 1 - k + ifelse(k == 1, 0.1, 0) + ifelse(k == n, -0.1, 0),
          height = ifelse(k == 1 | k == n, 0.6, 0.4),
          fontsize = 10, fontcolor = "black", penwidth = ifelse(k == 1 | k == n, 2, 1), color = "black"
        )
      )
    if (k > 1) {
      xg <- xg %>%
        add_edge(from = k - 1, to = k, edge_aes = edge_aes(color = "black"))
    }
  }
  if (n > 1) {
    for (k in seq_len(nrow(att))) {
      xg <- xg %>%
        add_node(
          label = att$label[k],
          node_aes = node_aes(
            shape = "box", x = 3, width = 1.2, y = n + 0.5 - k, height = 0.4,
            fontsize = 8, fillcolor = "grey", fontcolor = "black", color = "black"
          )
        ) %>%
        add_node(
          label = att$reason[k],
          node_aes = node_aes(
            shape = "box", x = 1, width = 2, y = n + 0.5 - k, height = 0.2, fillcolor = "white", color = "black", fontcolor = "back"
          )
        ) %>%
        add_edge(
          from = 2*k + n, to = 2*k + n -1, edge_aes = edge_aes(color = "black")
        )
    }
  }

  return(xg)
}
