-- mydb.sessions definition

CREATE TABLE `dmvc_sessions` (
  `session_id` varchar(100) NOT NULL,
  `session_data` longtext NOT NULL,
  `session_expiration` timestamp,
  `session_created_at` timestamp NOT NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`session_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;