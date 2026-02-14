FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    sbcl \
    chezscheme \
    guile-3.0 \
    chicken-bin \
    gambc \
    ecl \
    newlisp \
    picolisp \
    emacs-nox \
    luajit \
    default-jdk \
    curl ca-certificates python3 \
    && rm -rf /var/lib/apt/lists/*

# Chicken SRFI-69 (hash tables)
RUN chicken-install srfi-69

# Clojure CLI
RUN curl -fsSL https://download.clojure.org/install/linux-install-1.12.0.1530.sh | bash
RUN clojure -e '(println "ready")' 2>/dev/null || true

# Janet (build from source)
RUN apt-get update && apt-get install -y --no-install-recommends build-essential git && \
    git clone --depth 1 --branch v1.37.1 https://github.com/janet-lang/janet.git /tmp/janet && \
    cd /tmp/janet && make -j$(nproc) && make install && ldconfig && \
    rm -rf /tmp/janet && apt-get purge -y build-essential git && apt-get autoremove -y && rm -rf /var/lib/apt/lists/*

# Fennel
RUN curl -fsSL https://fennel-lang.org/downloads/fennel-1.5.1 -o /usr/local/bin/fennel && chmod +x /usr/local/bin/fennel
RUN ln -sf /usr/bin/luajit /usr/local/bin/lua

# Gauche (not in bookworm, install from source)
RUN apt-get update && apt-get install -y --no-install-recommends build-essential libgdbm-dev zlib1g-dev && \
    curl -fsSL https://github.com/shirok/Gauche/releases/download/release0_9_15/Gauche-0.9.15.tgz | tar xz -C /tmp && \
    cd /tmp/Gauche-0.9.15 && ./configure --prefix=/usr/local && make -j$(nproc) && make install && \
    rm -rf /tmp/Gauche-0.9.15 && apt-get purge -y build-essential && apt-get autoremove -y && rm -rf /var/lib/apt/lists/*

# Kawa (download jar)
RUN curl -fsSL https://repo1.maven.org/maven2/com/github/arvyy/kawa/3.1.1/kawa-3.1.1.jar -o /usr/local/lib/kawa.jar && \
    printf '#!/bin/sh\nexec java -jar /usr/local/lib/kawa.jar "$@"\n' > /usr/local/bin/kawa && \
    chmod +x /usr/local/bin/kawa

# Racket BC (bytecode backend avoids Chez Scheme boot file conflicts)
RUN curl -fsSL https://download.racket-lang.org/installers/8.15/racket-8.15-x86_64-linux-bc.sh -o /tmp/racket.sh && \
    chmod +x /tmp/racket.sh && \
    echo "yes\n1\n" | /tmp/racket.sh --in-place --dest /opt/racket 2>/dev/null; \
    rm -f /tmp/racket.sh
ENV PATH="/opt/racket/bin:$PATH"
RUN racket -e '(displayln "ready")' || echo "Racket install needs fixup"

WORKDIR /bench
COPY . .
RUN chmod +x run-benchmarks.sh
ENTRYPOINT ["/bench/run-benchmarks.sh"]
