# Stage 1: Build Sema (Alpine = musl = fully static binary)
FROM rust:alpine AS builder
RUN apk add --no-cache musl-dev
WORKDIR /build
COPY . .
RUN cargo build --release --locked -p sema-lang && strip target/release/sema

# Stage 2: Runtime (scratch = zero overhead, just the binary)
FROM scratch
COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/
COPY --from=builder /build/target/release/sema /usr/local/bin/sema
COPY examples/ /app/examples/

WORKDIR /app
EXPOSE 3000

ENTRYPOINT ["sema"]
CMD ["examples/web-server.sema"]
