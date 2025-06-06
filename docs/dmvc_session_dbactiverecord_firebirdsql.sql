CREATE TABLE dmvc_sessions (
  session_id varchar(100) NOT NULL,
  session_data blob SUB_TYPE text NOT NULL,
  session_expiration timestamp,
  session_created_at timestamp DEFAULT current_timestamp,
  PRIMARY KEY (session_id)
);
