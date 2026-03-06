-- V001: Create users table for OIDC user management

CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    oidc_subject VARCHAR(255) UNIQUE NOT NULL,
    email VARCHAR(255),
    display_name VARCHAR(255),
    role VARCHAR(20) NOT NULL DEFAULT 'viewer',
    last_login_at TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT NOW()
);
