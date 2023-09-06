#' Create/Reset the Database Table for Trust with same schema as `phase_2_demo` table
#'
#' @description
#' Use this to create a data table for a new trust (or reset an existing trust table) to ensure 
#' it has same schema as a `phase_2_demo` table because this is the table schema that the
#' phase 2 dashboard implementation is designed to work with
#' 
#' @param pool the database connection
#' @param set_trust_id the name of the trust table, should be same as `get_golem_config('trust_name')`
#' @param drop_table if the trust table already exist, should it be dropped and recreated or an error be thrown. 
#'
#' @return zero if operation is successful
#' @examples create_trust_table(pool, set_trust_id = 'trust_a_bk')
#' @noRd
create_trust_table <-  function(pool, set_trust_id, drop_table=FALSE){
  
  if (drop_table)  {
    DBI::dbExecute(pool, paste0('DROP TABLE IF EXISTS ', set_trust_id))
  } else{
    stop('Table already exist')
  }
  
  query <- paste0("CREATE TABLE ", set_trust_id, " AS (SELECT * FROM phase_2_demo WHERE 1=2)")
  DBI::dbExecute(pool, query)
  DBI::dbExecute(pool, paste0("ALTER TABLE ", set_trust_id, " ADD PRIMARY KEY (comment_id)"))
  
  query <- paste0("ALTER TABLE ", set_trust_id, " MODIFY COLUMN
                  `comment_id` INT(10) UNSIGNED AUTO_INCREMENT FIRST")
  DBI::dbExecute(pool, query)
}

#' create the api job table (api_jobs) in the database
#'
#' @param conn database connection
#' @noRd
create_job_table <- function(conn){
  
  query = "CREATE TABLE api_jobs (
      job_id int NOT NULL AUTO_INCREMENT,
      date datetime NOT NULL,
      url text NOT NULL,
      trust_id tinytext NOT NULL,
      user tinytext NOT NULL,
      email tinytext,
      status tinytext NOT NULL CHECK (status IN ('submitted', 'completed', 'failed')),
      PRIMARY KEY (job_id)
  )"
  
  DBI::dbExecute(conn, query)
}