latest_league_data <- jsonlite::read_json(paste0(v_mfl_api_base, "export?TYPE=league&L=", v_mfl_league_id, "&JSON=1"))$league
