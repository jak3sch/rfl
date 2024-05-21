draft_start <- as.numeric(as.POSIXct("2024-05-10 20:00:00 MEZ"))
#draft_end

rfl_draft_trades <- trades %>%
  dplyr::filter(timestamp >= draft_start) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
    dplyr::mutate(
      asset_type = ifelse(grepl("DP_", asset_id), "pick", "player"),
    ) %>%
    dplyr::group_by(trade_id) %>%
    dplyr::mutate(
      asset_types = paste(asset_type, collapse = ","),
      asset_ids = paste(asset_id, collapse = ","),
      franchise_ids = paste(franchise_id, collapse = ",")
    ) %>%
  dplyr::filter(franchise_id == "0007")

rfl_draft_trades_sends <- rfl_draft_trades %>%
  dplyr::filter(trade_side == "franchise_1") %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::summarise(
    date = dplyr::first(date),
    asset_ids = dplyr::first(asset_ids),
    asset_types = dplyr::first(asset_types),
    franchise_ids = dplyr::first(franchise_ids),
    asset_names = paste(asset_name, collapse = "\n"),
    franchise_name = dplyr::first(franchise_name),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    asset_names = paste0(franchise_name, " sends\n", asset_names)
  )
  dplyr::select(trade_id, date, trade_side, asset_names) %>%
  tidyr::spread(trade_side, asset_names)

rfl_draft_trades_gets <- rfl_draft_trades %>%
  dplyr::filter(trade_side == "franchise_2") %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::summarise(
    date = dplyr::first(date),
    asset_ids = dplyr::first(asset_ids),
    asset_types = dplyr::first(asset_types),
    franchise_ids = dplyr::first(franchise_ids),
    asset_get = paste(asset_name, collapse = "\n"),
    franchise_name = dplyr::first(franchise_name),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    asset_get = paste0(franchise_name, " gets\n", asset_get)
  )

rfl_draft_trades_combined <- rfl_draft_trades_sends %>%
  dplyr::select(franchise_id, franchise_name, asset_send) %>%
  dplyr::left_join(
    rfl_draft_trades_gets %>%
      dplyr::select(franchise_id, asset_get),
    by = c("franchise_id")
  )
