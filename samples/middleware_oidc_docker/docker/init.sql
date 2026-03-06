-- PostgreSQL initialization for OIDC Docker Sample
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS schema_migrations (
    version INTEGER PRIMARY KEY,
    description VARCHAR(255) NOT NULL,
    applied_at TIMESTAMP NOT NULL DEFAULT NOW()
);
