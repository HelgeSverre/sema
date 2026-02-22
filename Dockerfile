# Stage 1: Build Sema
FROM rust:bookworm AS builder
WORKDIR /build
COPY . .
RUN cargo build --release

# Stage 2: Runtime
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/target/release/sema /usr/local/bin/sema
COPY examples/ /app/examples/

WORKDIR /app
EXPOSE 3000

ENTRYPOINT ["sema"]
CMD ["examples/web-server.sema"]
