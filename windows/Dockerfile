FROM jeffreybbrown/hode:latest AS builder

WORKDIR /opt/skg
COPY Cargo.toml Cargo.lock /opt/skg/
COPY server /opt/skg/server
COPY tools/introspect/code-map /opt/skg/tools/introspect/code-map
COPY tools/introspect/call-graph /opt/skg/tools/introspect/call-graph
COPY tools/introspect/change-forest /opt/skg/tools/introspect/change-forest
COPY tools/scope-reducer /opt/skg/tools/scope-reducer
COPY tools/rust-field-tags /opt/skg/tools/rust-field-tags
RUN cargo build --release --bin skg

FROM jeffreybbrown/hode:latest

WORKDIR /opt/skg
COPY --from=builder /opt/skg/target/release/skg /usr/local/bin/skg
COPY windows/container/start-servers.sh /opt/skg/windows/container/start-servers.sh
COPY docs /opt/skg/docs
COPY LICENSE.md /opt/skg/LICENSE.md
RUN chmod +x /usr/local/bin/skg /opt/skg/windows/container/start-servers.sh

ENV SKG_PROJECT_ROOT=/opt/skg
ENV PATH="/usr/local/bin:${PATH}"
EXPOSE 1731/tcp

CMD ["sleep", "infinity"]
