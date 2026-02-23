FROM rust:1.88-bookworm AS builder
WORKDIR /build
COPY Cargo.toml Cargo.lock ./
COPY src/ src/
COPY migrations/ migrations/
COPY templates/ templates/
RUN cargo build --release

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y ca-certificates && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY --from=builder /build/target/release/sema-pkg /usr/local/bin/
COPY templates/ templates/
COPY static/ static/
COPY migrations/ migrations/
EXPOSE 3000
VOLUME ["/app/data"]
CMD ["sema-pkg"]
