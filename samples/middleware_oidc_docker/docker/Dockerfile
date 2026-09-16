FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    ca-certificates \
    libpq5 \
    && rm -rf /var/lib/apt/lists/* \
    && ln -s /usr/lib/x86_64-linux-gnu/libpq.so.5 /usr/lib/x86_64-linux-gnu/libpq.so

WORKDIR /app

COPY bin/OIDCDockerSample /app/OIDCDockerSample
COPY templates/ /app/templates/
COPY Migrations/ /app/Migrations/

RUN chmod +x /app/OIDCDockerSample

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=10s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

CMD ["/app/OIDCDockerSample"]
