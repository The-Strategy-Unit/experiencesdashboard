pool <- pool::dbPool(drv = odbc::odbc(),
                     driver = "Maria DB",
                     server = Sys.getenv("HOST_NAME"),
                     UID = Sys.getenv("DB_USER"),
                     PWD = Sys.getenv("MYSQL_PASSWORD"),
                     database = "TEXT_MINING",
                     Port = 3306)
